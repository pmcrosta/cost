stop("you don't want to do this")
## Code for cost paper 
## Peter M Crosta (pmcrosta@gmail.com)

## working on chagall
# .libPaths('~/R/library/')
library(Hmisc)
library(foreign)
library(parallel)
options(scipen=3)
lunique <- function(x) length(unique(x))
pdensity <- function(x) plot(density(x, na.rm=T))
r2 <- function(x) round(x,2)
r3 <- function(x) round(x,3)

setwd('~/projects/CBD/JBL/cohorts/')

NCinst <-  c(102, 103, 104, 105, 106)
NCnames <- c("Guilford", "CP", "Davidson", "Martin", "Wake")
names(NCinst) <- NCnames

#### STUDENT DATA ##########
## load in course taking data that was modified to fix NC course numbering issues.
#code that is loaded in had stata code to fix course numbering. this has been r-ified below
#use R code if ever need to use JBL raw files instead of SWC data files
courses <- read.dta("CourseEnrollments.dta")
courses <- subset(courses, subset=cntInstitution %in% NCinst[2] & CohortId %in% c(2005) & AcademicYearId %in% 2005:2010)
lencc = length(courses$CourseNumber)
first3 = substr(courses$CourseNumber, 1, 3)
abeflag <- let4 <- let7 <- thenorm <- rep(NA, length(lencc))
abeflag[first3 %in% c("ABE", "GED", "ABL", "HSD", "CED", "TST", "HRD")] <- 1
let4[substr(courses$CourseNumber, 4, 4)=="C"] <- 1
let7[substr(courses$CourseNumber, 7, 7)=="A"] <- 1
thenorm[is.na(let7) & is.na(abeflag) & is.na(let4)] <- substr(courses$CourseNumber[is.na(let7) & is.na(abeflag) & is.na(let4)], 1, 6)
thenorm[!is.na(let7) & is.na(abeflag)] <- substr(courses$CourseNumber[!is.na(let7) & is.na(abeflag)], 1, 7)
thenorm[!is.na(abeflag)] <- substr(courses$CourseNumber[!is.na(abeflag)], 1, 7)
thenorm[!is.na(let4)] <- substr(courses$CourseNumber[!is.na(let4)], 1, 8)
thenorm[unlist(regexec("^[A-Z][A-Z][A-Z][34567]", thenorm))!=-1] <- substr(courses$CourseNumber[unlist(regexec("^[A-Z][A-Z][A-Z][34567]", thenorm))!=-1], 1, 7)
courses$CourseNumber2 <- thenorm
coursedat <- courses
coursedat$CourseNumber <- coursedat$CourseNumber2
coursedat$CourseNumber2 <- courses$CourseNumber
rm(lencc, first3, abeflag, let7, let4, thenorm)

coursedat$occext <- nchar(coursedat$CourseNumber)==8 | (nchar(coursedat$CourseNumber)==7 & substring(coursedat$CourseNumber, 4, 4) %in% as.character(3:9))
coursedat$yearterm <- paste(coursedat$AcademicYearId, coursedat$TermId, sep=".")
coursedat$idterm <- paste(coursedat$StudentIdEncrypted, coursedat$yearterm, sep=".")

## load in student data - may or may not use FTIC. edit code below.
students <- read.dta("/projects/CBD/JBL/cohorts/cho/february_pull/y05_ALL.dta")
students <- subset(students, subset=cntInstitution %in% NCinst[2])
students$yearterm <- paste(students$AcademicYearId, students$TermId, sep=".")
students$idterm <- paste(students$StudentIdEncrypted, students$yearterm, sep=".")
students$FTIC <-  !(students$enrolledbefore2005==1 | students$BAlessthan3yrs==1 | students$NumColCred>0)
students$FTIC[is.na(students$FTIC)] <- TRUE
students <- subset(students, subset=FTIC)

## read in data containing major field
studfields <- read.dta("/projects/CBD/crosta/data/major_field.dta")
studfields <- studfields[studfields$cntInstitution==NCinst[2],]
if ("Stu" %in% colnames(students)) {
  colnames(students)[which(colnames(students)=="Stu")] <- "StudentIdEncrypted"
  students$StudentIdEncrypted <- substring(students$StudentIdEncrypted, first=4) 
}

students$firstmajor_field <- studfields$firstmajor_field[match(students$StudentIdEncrypted, studfields$StudentIdEncrypted)]
students$fmlas_detail <- studfields$fmlas_detail[match(students$StudentIdEncrypted, studfields$StudentIdEncrypted)]
students$lastmajor_field <- studfields$lastmajor_field[match(students$StudentIdEncrypted, studfields$StudentIdEncrypted)]
students$lmlas_detail <- studfields$lmlas_detail[match(students$StudentIdEncrypted, studfields$StudentIdEncrypted)]

### need to count all awards as output, so need progress file
progress <- read.dta("StudentProgress.dta")
progress <- subset(progress, subset=cntInstitution %in% NCinst[2] & CohortId %in% c(2005))
## keep only students who have any completions
progress <- subset(progress, subset=StudentIdEncrypted %in% students$StudentIdEncrypted & (Complete1!=0 | Complete2!=0))
progress$yearterm <- paste(progress$AcademicYearId, progress$TermId, sep="")
progress <- subset(progress, select=c("yearterm", "StudentIdEncrypted", "Complete1", "Complete2"))
progress <- subset(progress, subset=!(yearterm %in% c("20101", "20103", "20106", "20111")))
progress$Complete1[progress$Complete1 %in% c(0,8)] <- NA
progress$Complete2[progress$Complete2 %in% c(0,8)] <- NA
progwide <- reshape(progress, direction="wide", v.names=c("Complete1", "Complete2"), timevar="yearterm", idvar="StudentIdEncrypted")
progwide <- progwide[,sort(colnames(progwide)) ]
progwide$ncreds <- rowSums(!is.na(progwide))-1
progwide$creds <- gsub("NA", "", gsub(".NA", "", gsub("NA.", "", do.call(paste, c(progwide[colnames(progwide)[1:28]], sep = ".")))))

students$credstring <- progwide$creds[match(students$StudentIdEncrypted, progwide$StudentIdEncrypted)]
#table(students$outcome, students$credstring)

## find the average number of credits for students in each credstring category; isolate 1,2,4,5, and 6
avgcreds <- tapply(students$degcredits[as.numeric(students$start) %in% c(7,8,9)], students$credstring[as.numeric(students$start) %in% c(7,8,9)], mean, na.rm=T)
avgcreds <- avgcreds[c("1", "2", "4", "5", "6")]
## weight output by AA
outputweight <- avgcreds/avgcreds["4"]
students$output <- sapply(strsplit(students$credstring, "\\."), function(x) sum(outputweight[x]))
## for those who transfer without credential, let's give them some output cred
students$output[students$outcome=="Transfer_4-Year_NoCred" & is.na(students$output)] <- mean(students$degcredits[students$outcome=="Transfer_4-Year_NoCred"])/avgcreds["4"]
## zero output for everyone else
students$output[is.na(students$output)] <- 0 

########## COST DATA #################
## read in CPC data ###
setwd('~/projects/CBD/crosta/data/')
cpc07080910 <- read.csv("CostsPerCredit0708-0910.csv", header=T)
cpc07080910$Course <- as.character(cpc07080910$Course)

## assign 05 and 06 values from 07-08, assign 2010-11 values from 09-010
cpc <- with(cpc07080910, data.frame(Course, UnitId, UnitDescription, CPC07.08, CPC07.08, CPC07.08, CPC08.09, CPC09.10, CPC09.10), stringsAsFactors=FALSE)

years <- sort(unique(coursedat$AcademicYearId))
CPClist <- paste("CPC", years, sep="")
cpclist <- paste("cpc", years, sep="")
colnames(cpc) <- c("Course", "UnitId", "UnitDescription", CPClist)

## need to match on 6-digit course codes
## exclude occ ext
coursedat$digit6[!coursedat$occext] <- substr(coursedat$CourseNumber[!coursedat$occext], 1, 6)
cpc$digit6 <- substr(cpc$Course, 1, 6)

# table(is.na(match(unique(cpc$digit6), unique(coursedat$digit6))))  
# table(is.na(match(unique(coursedat$digit6), unique(cpc$digit6)))) 

## need to match on avg of 3-digit course codes if 6-digit code did not match up exactly
## exclude occ ext
coursedat$digit3[!coursedat$occext] <- substr(coursedat$CourseNumber[!coursedat$occext], 1, 3)
cpc$digit3 <- substr(cpc$Course, 1, 3)

## put cost data from 6-digit matches onto transcript file; gets curriculum courses
for (ii in 1:length(cpclist)) {
  coursedat[,cpclist[ii]] <-  get(CPClist[ii], cpc)[match(coursedat$digit6, cpc$digit6)]
}
 
## compute average over 3-digit prefixes for imputation [ACC shows up in accounting and in office tech systems.
## Realization here that course prefixes do not fit nicely into GL Units.. COE is another. oh well for now.]
cpc.dig3.list <- list()
for (ii in 1:length(cpclist)) {
  cpc.dig3.list[[ii]] <- tapply(cpc[,CPClist[ii]], cpc$digit3, mean, na.rm=T)
}

#lunique(coursedat$digit3[is.na(coursedat$cpc0405)]) #161 3digit codes without cost data

## if cost data is missing, use 3-digit code imputation
for (ii in 1:length(cpclist)) {
  coursedat[is.na(coursedat[,cpclist[ii]]), cpclist[ii]] <- cpc.dig3.list[[ii]][match(coursedat$digit3, names(cpc.dig3.list[[ii]]))][is.na(coursedat[,cpclist[ii]])]
}

#lunique(coursedat$digit3[is.na(coursedat$cpc0405)]) #72 3digit codes without cost data

## still left with 3digits in 0405 and 0708 that do not have cost per credits
## we'll have to estimate these

##let's use average for imputation here.
avglist <- sapply(CPClist, function(x) mean(get(x, cpc), na.rm=T))
avgnames <- paste("avg", years, sep="")

## impute with average
for (ii in 1:length(cpclist)) {
  coursedat[,avgnames[ii]] <-  get(cpclist[ii], coursedat)
  coursedat[is.na(get(cpclist[ii], coursedat)),avgnames[ii]] <-  avglist[ii]
}

## or just pop $100 in (impute with $100).
i100list <-  paste("i100.", years, sep="") 
for (ii in 1:length(cpclist)) {
  coursedat[,i100list[ii]] <-  get(cpclist[ii], coursedat)
  coursedat[is.na(get(cpclist[ii], coursedat)),i100list[ii]] <-  100
}

## now compute transcript cost assuming different cost bases
# This will also depend on numerous assumptions relating to which courses to count,
# currently, we assume costs for all courses except withdrawal (see p32 of CBD data requirements)
# Also considered the number of credits attempted

## create a cost assumption that matches when the course was taken to the avg CPC that year
coursedat$avg_match <- rep(NA, dim(coursedat)[1])
coursedat$i100_match <- rep(NA, dim(coursedat)[1])
for (ii in years) {
  coursedat$avg_match[coursedat$AcademicYearId==ii] <- coursedat[coursedat$AcademicYearId==ii, paste("avg", ii, sep="")]
  coursedat$i100_match[coursedat$AcademicYearId==ii] <- coursedat[coursedat$AcademicYearId==ii, paste("i100.", ii, sep="")]
}

costassum <- c(avgnames, "avg_match", i100list, "i100_match")
costlist <- vector("list", length(costassum))

######## Withdraw flag ####
### rebuild cost vectors from here

############ LOOK!!!!!!
## ignoring withdrawal in calculating transcript cost using withdrawflag
withdrawflag <- rep(TRUE, dim(coursedat)[1]) ##include all courses
#withdrawflag <- coursedat$Grade != -3 ##only include non-withdrawal courses

for (ii in seq_along(costassum)) {
  curvar <- with(coursedat, get(costassum[ii]))
  costlist[[ii]] <- tapply(curvar[withdrawflag]*coursedat$NumCreditsAttempt[withdrawflag], coursedat$StudentIdEncrypted[withdrawflag], FUN=sum, na.rm=T)
}
names(costlist) <- costassum

## bring in the ipeds data.
ipeds <- read.dta("/projects/delta_cost_project/delta_public_release_99_09.dta")
unitids <- c(198622, 198260, 198367, 198905, 199856)
ipeds <- subset(ipeds, subset=ipeds$unitid %in% unitids[2])
ipeds$year <- ipeds$academicyear-1
## Last year of IPEDS is 2008-09. Need IPEDS 09-10, 10-11 data. In calculations below, we use simple regression
## model to predict last two years.
## determine the proportion of noninstructional costs to attribute to the cohort each year by using
## the total enrollment numbers as reported by IPEDS and the undergraduate credits delivered as reported by ipeds.

cbdenroll <- tapply(coursedat$StudentIdEncrypted[!coursedat$occext], coursedat$AcademicYearId[!coursedat$occext], lunique)
ipedsenroll <- c(ipeds$total_enrollment[ipeds$year %in% years], exp(predict(with(ipeds, lm(log(total_enrollment)~year)), newdata=data.frame(year=c(2009, 2010)))))
cbdenroll_share <- cbdenroll/ipedsenroll

cbdcreds <- tapply(coursedat$NumCreditsAttempt[!coursedat$occext], coursedat$AcademicYearId[!coursedat$occext], sum)
ipedscreds <- c(ipeds$credhoursug[ipeds$year %in% years], exp(predict(with(ipeds, lm(log(credhoursug)~year)), newdata=data.frame(year=c(2009, 2010)))))
cbdcreds_share <- cbdcreds/ipedscreds

## proportions taken from CAPSEE data
cbdenroll_share <- c(0.416105382368719, 0.203433271741074, 0.139297848244621, 0.0985873517315372, 0.0765333492281099, 0.0633605531382249)
cbdcreds_share <- c(0.380344171207682, 0.249807172627673, 0.153887004411129, 0.0966013831516176, 0.0673406740374023, 0.0507732865642492)
names(cbdcreds_share) <- names(cbdenroll_share) <- years

## now we know what proportion of costs each year are to be spread out among our cohort.
## We get non-instructional costs from IPEDS: instsupp01, acadsupp01, studserv01, opermain01.
## We are assuming that instruction01 costs are being computed from transcript cost.
## We are ignoring grants01 as well. All of these sum to eandg01

## More assumptions: academic support and operations will be allocated across credits
##                   student services and institutional support will be allocated across individuals

## first get missing years using predictions from simple linear model
ipedsacad <- c(ipeds$acadsupp01[ipeds$year %in% years], predict(with(ipeds, lm(acadsupp01~year)), newdata=data.frame(year=c(2009, 2010))))
ipedsoper <- c(ipeds$opermain01[ipeds$year %in% years], predict(with(ipeds, lm(opermain01~year)), newdata=data.frame(year=c(2009, 2010))))
ipedsstudserv <- c(ipeds$studserv01[ipeds$year %in% years], predict(with(ipeds, lm(studserv01~year)), newdata=data.frame(year=c(2009, 2010))))
ipedsinstsupp <- c(ipeds$instsupp01[ipeds$year %in% years], predict(with(ipeds, lm(instsupp01~year)), newdata=data.frame(year=c(2009, 2010))))
## these are the ones we are not using
ipedsinstruct <- c(ipeds$instruction01[ipeds$year %in% years], predict(with(ipeds, lm(instruction01~year)), newdata=data.frame(year=c(2009, 2010))))
ipedsinstruct02 <- c(ipeds$instruction02[ipeds$year %in% years], predict(with(ipeds, lm(instruction02~year)), newdata=data.frame(year=c(2009, 2010))))
ipedsnonclassroom <- ipedsinstruct-ipedsinstruct02

## bring in Terri's data
noninst <- read.csv("/projects/CBD/crosta/data/noninstruct07-10.csv", header=T)
## keep grand totals
noninst.gt <- noninst[noninst$X=="Grand Total",]
#instruct.gt <- noninst[noninst$X=="Grand Total", c("Instruction", "FallYear")]
terri.pres <- c(predict(with(noninst.gt, lm(President~FallYear)), newdata=data.frame(FallYear=c(2005, 2006))), noninst.gt$President)
terri.admin <- c(predict(with(noninst.gt, lm(Administrative.Services~FallYear)), newdata=data.frame(FallYear=c(2005, 2006))), noninst.gt$Administrative.Services)
terri.instruction <- c(predict(with(noninst.gt, lm(Instruction~FallYear)), newdata=data.frame(FallYear=c(2005, 2006))), noninst.gt$Instruction)
terri.edsupport <- c(predict(with(noninst.gt, lm(Education.Support.Services~FallYear)), newdata=data.frame(FallYear=c(2005, 2006))), noninst.gt$Education.Support.Services)
terri.finance <- c(predict(with(noninst.gt, lm(Finance...Administrative.Services~FallYear)), newdata=data.frame(FallYear=c(2005, 2006))), noninst.gt$Finance...Administrative.Services)
terri.dct <- data.frame(dct=c(28321364, 33431172, 38473622), FallYear=c(2007:2009))
dct.pred <- predict(with(terri.dct, lm(dct~FallYear)), newdata=data.frame(FallYear=c(2005, 2006, 2010)))
names(dct.pred) <- c(2005, 2006, 2010)
newdct <- c(terri.dct$dct, dct.pred)
names(newdct) <- c(terri.dct$FallYear, names(dct.pred))
newdct <- newdct[order(newdct)]
nonclassroom <- (1-newdct/terri.instruction)*terri.instruction

######## WHICH SET OF COSTS DO WE WANT TO USE
IPEDS=FALSE #False means not ipeds
if (IPEDS) {
  ## now determine how much of these four costs should be distributed across this cohort in each year
  nonclassshare <- ipedsnonclassroom*cbdcreds_share
  acadshare <- ipedsacad*cbdcreds_share
  opershare <- ipedsoper*cbdcreds_share
  studservshare <- ipedsstudserv*cbdenroll_share 
  instsuppshare <- ipedsinstsupp*cbdenroll_share
} else {
  nonclassshare <- nonclassroom*cbdcreds_share
  acadshare <- terri.admin*cbdcreds_share
  opershare <- terri.edsupport*cbdcreds_share
  studservshare <- terri.finance*cbdenroll_share 
  instsuppshare <- terri.pres*cbdenroll_share  
}

names(nonclassshare) <- names(acadshare) <- names(opershare) <- names(studservshare) <- names(instsuppshare) <- years

#sum(nonclassshare+acadshare+opershare+studservshare+instsuppshare)

## since these are per person, just compute for each year

## the loop below computes non-instructional costs each year for each student enrolled
## also run here for withdraw flag changes, through revenue model
noninstlist <- vector("list", length(costassum))
for (ii in seq_along(costassum)) {
  curvar <- with(coursedat, get(costassum[ii]))
  noninstlist[[ii]] <- sapply(years, function(jj) {
    sset <- withdrawflag & coursedat$AcademicYearId==jj 
    1/(lunique(coursedat$StudentIdEncrypted[sset]))*(studservshare[as.character(jj)]+instsuppshare[as.character(jj)]) + 
      prop.table(tapply(curvar[sset]*coursedat$NumCreditsAttempt[sset], coursedat$StudentIdEncrypted[sset], FUN=sum, na.rm=T))*(acadshare[as.character(jj)]+opershare[as.character(jj)])
    })
}
names(noninstlist) <- costassum

## the loop below computes non-classroom instructional costs each year for each student enrolled
nonclasslist <- vector("list", length(costassum))
for (ii in seq_along(costassum)) {
  curvar <- with(coursedat, get(costassum[ii]))
  nonclasslist[[ii]] <- sapply(years, function(jj) { 
    prop.table(tapply(curvar[withdrawflag & coursedat$AcademicYearId==jj]*coursedat$NumCreditsAttempt[withdrawflag & coursedat$AcademicYearId==jj], coursedat$StudentIdEncrypted[withdrawflag & coursedat$AcademicYearId==jj], FUN=sum, na.rm=T))*(nonclassshare[as.character(jj)])
  })
}
names(nonclasslist) <- costassum

## Here we compute the pathway-non-instructional cost under our various cost assumptions (sum the years together in noninstlist)
noninstcost <- vector("list", length(costassum))
for (ii in seq_along(noninstlist)) {
  noninstcost[[ii]] <- tapply(unlist(noninstlist[[ii]]), names(unlist(noninstlist[[ii]])), sum, na.rm=T)
}
names(noninstcost) <- costassum

## Here we compute the pathway-non-classroom instructional cost under our various cost assumptions
nonclasscost <- vector("list", length(costassum))
for (ii in seq_along(noninstlist)) {
  nonclasscost[[ii]] <- tapply(unlist(nonclasslist[[ii]]), names(unlist(nonclasslist[[ii]])), sum, na.rm=T)
}
names(nonclasscost) <- costassum


## now compute total pathway cost by summing instructional and non-instructional costs
totcost <- vector("list", length(costassum))
for (ii in seq_along(costassum)) totcost[[ii]] <- rowSums(cbind(costlist[[ii]],noninstcost[[ii]],nonclasscost[[ii]]), na.rm=T)
#for (ii in seq_along(costassum)) totcost[[ii]] <- costlist[[ii]]+noninstcost[[ii]]+nonclasscost[[ii]]
names(totcost) <- costassum
############## END OF COST COMPUTATIONS ####################


################ STUDENT LEVEL DATA b##########################

## match cost data to FTIC data
for (ii in names(costlist)) {
  assign(paste(ii, "IC", sep='_'), costlist[[ii]][match(students$StudentIdEncrypted, names(costlist[[ii]]))])
  assign(paste(ii, "NC", sep='_'), noninstcost[[ii]][match(students$StudentIdEncrypted, names(noninstcost[[ii]]))])
  assign(paste(ii, "TC", sep='_'), totcost[[ii]][match(students$StudentIdEncrypted, names(totcost[[ii]]))])
  assign(paste(ii, "NCC", sep='_'), nonclasscost[[ii]][match(students$StudentIdEncrypted, names(nonclasscost[[ii]]))])
}

## assign revenue to 'students' sample 
# assign("revenue_sum", revbystud[match(students$StudentIdEncrypted, names(revbystud))])
# assign("tuition_sum", tuitbystud[match(students$StudentIdEncrypted, names(tuitbystud))])
# assign("fee_sum", feesbystud[match(students$StudentIdEncrypted, names(feesbystud))])

## revenue determination relies on aggregate cost must equal aggregate revenue
## using avg_match as cost, figure out how much is left that we can allocate by credit.
aggcost <- sum(avg_match_TC, na.rm=T)
totcreds <- tapply(coursedat$NumCreditsAttempt[withdrawflag], coursedat$StudentIdEncrypted[withdrawflag], FUN=sum, na.rm=T)


########### REVENUE MODEL ###########################
## In July 2012, I learned that tuition and fees go to the state, not the college, and they are redistributed from there.
## Ultimately, revenue from the state is disbursed on an FTE basis.
## However, there is also county revenue among other sources.
## In December 2012, we realized that we need another way to do revenues, since just assuming R=E and allocating 
## Rev by credit during each simulation is not really sensible.
##
## County funds from fiscal year 05-06 to 09-10: c(20635963, 23474542, 25174542, 26974542, 24492884)
# BUDGET FTE  COUNTY CURRENT BUDGET  COUNTY CURRENT SUPPORT PER BUDGETED FTE	STATE AID CURRENT STATE & FEDERAL BUDGET	STATE AID CURRENT STATE & FEDERAL SUPPORT PER BUDGET FTE	TOTAL SUPPORT
# 2009-10	14,917	24,492,884	1,642	76,247,611	5,111	100,740,495
# 2008-09	13,959	26,974,542	1,932	72,501,825	5,194	99,476,367
# 2007-08	12,669	25,174,542	1,987	64,302,057	5,076	89,476,599
#http://www.nccommunitycolleges.edu/Program_Audit_Services/docs/FTE%20Printable%20Version/FTE%20Form%202012-2013.pdf

########### END OF REVENUE MODEL ############################
## Let's assume a separate constant revenue per budget fte for curriculum and occext courses. (rev per credit) They will be in a fixed ratio 
## first, determine budget fte. This is easier for any course with a positive number of credits, 
## but we will have to use a different value for 0 creds attempt. Assume 1 credit for courses based on contact hours. not sure how right this is
coursedat$budgfte <- coursedat$NumCreditsAttempt
coursedat$budgfte[coursedat$budgfte==0] <- 1

credsbytype <- tapply(coursedat$budgfte[coursedat$StudentIdEncrypted %in% students$StudentIdEncrypted], coursedat$occext[coursedat$StudentIdEncrypted %in% students$StudentIdEncrypted], sum)
currrevpc <- aggcost*1.345/(credsbytype["TRUE"]+1.345*credsbytype["FALSE"])
coursedat$budgval[!coursedat$occext] <- coursedat$budgfte[!coursedat$occext]*currrevpc
coursedat$budgval[coursedat$occext] <- coursedat$budgfte[coursedat$occext]*(currrevpc/1.345)
## budgval approximates the revenue per course, assuming $152 for curriculum courses and $113 for noncurriculum and occext courses. 152/113=1.345
## this only really takes into account state funding.
studrev <- tapply(coursedat$budgval[withdrawflag], coursedat$StudentIdEncrypted[withdrawflag], FUN=sum, na.rm=T)
revbal <- studrev[names(totcreds) %in% students$StudentIdEncrypted]

## OLD revbal ###
## still a bunch of "0" credit people in here because of stuff like AHD, GED.
#revbal <- aggcost*prop.table(totcreds[names(totcreds) %in% students$StudentIdEncrypted])
assign("revbal_tot", revbal[match(students$StudentIdEncrypted, names(revbal))])
assign("totcreds_ftic", totcreds[match(students$StudentIdEncrypted, names(totcreds))])
## some redundancy in here

## OK so now revbal or revbal_tot is basically a stream of revenue associated with each student

################ STUDENT LEVEL DATA e##########################
save(coursedat, students, avg_match_IC, avg_match_NC, avg_match_NCC, avg_match_TC, revbal_tot, totcreds_ftic, withdrawflag, totcreds, file="/projects/CBD/crosta/data/ncsim_glimmer.Rdata")

TCsum <- function(cp, cost, pvar, sset) {
  costopts <- c("_IC", "_NCC", "_NC", "_TC")
  sseti <- sset & students$cntInstitution==cp & !is.na(get(paste(cost, "_NC", sep='')))
  curvar <- get(pvar, students)
  simcat_pctab <- sapply(costopts, function(xx) bystats(get(paste(cost, xx, sep='')), curvar, fun=function(x) c(Mean=mean(x, na.rm=T)), subset=sseti)[,c("Mean")])
  N_tab <- table(curvar[sseti])
  Q_tab <- tapply(students$output[sseti], curvar[sseti], sum, na.rm=T)
  CandQ <- merge(cbind(N_tab, Q_tab), simcat_pctab, all.y=T, by="row.names")
  rownames(CandQ) <- CandQ$Row.names
  colnames(CandQ) <- c("", "N", "Output", "Classroom Costs", "Non-classroom Instructional Costs", "Noninstructional Costs", "Total Costs")
  if (!is.null(levels(curvar))) {
    write.table(na.omit(CandQ[match(levels(curvar), rownames(CandQ)),]), file=whereto, append=T, quote=F, sep=',', eol='\n') 
  } else {
    write.table(na.omit(CandQ), file=whereto, append=T, quote=F, sep=',', eol='\n') 
  }
  write.table("", file=whereto, append=T, quote=F, row.names=F, col.names=F)
}

## choose one at a time: select a costassum and append _IC, _NC, _TC for instructional, non-instructional, total cost
cost <- costassum[7]
whereto <- paste('~/projects/CBD/crosta/logs/tcsum_', format(Sys.time(), "%Y_%b_%d_%H_%M_%S"), ifelse(IPEDS, "IPEDS", "Terri"), '.log', sep='')
TCsum(cp=103, cost=costassum[7], pvar="start", sset=TRUE)  
TCsum(cp=103, cost=costassum[7], pvar="assessment", sset=as.numeric(students$start)==7 | as.numeric(students$start)==8)  
TCsum(cp=103, cost=costassum[7], pvar="MathPlacement", sset=(as.numeric(students$start)==7 | as.numeric(students$start)==8) & as.numeric(students$MathExam==1))  
TCsum(cp=103, cost=costassum[7], pvar="EngPlacement", sset=(as.numeric(students$start)==7 | as.numeric(students$start)==8) & as.numeric(students$EngExam==1))  
TCsum(cp=103, cost=costassum[7], pvar="ReadPlacement", sset=(as.numeric(students$start)==7 | as.numeric(students$start)==8) & as.numeric(students$ReadExam==1))  
sset <- as.numeric(students$start)>=7 & as.numeric(students$start)<=9
for (ii in c("status", "agecat", "somegrant", "somepell", "outcome", "conc_compare", "max_program_swc", "firstmajor_field", "lastmajor_field")) {
  TCsum(cp=103, cost=costassum[7], pvar=ii, sset=sset)  
}

##let's do a revenue summary

Revsum <- function(cp, pvar, sset) {
  revopts <- c("revbal_tot")
  sseti <- sset & students$cntInstitution==cp & !is.na(revbal_tot)
  curvar <- get(pvar, students)
  simcat_pctab <- sapply(revopts, function(xx) bystats(get(xx), curvar, fun=function(x) c(Mean=mean(x, na.rm=T)), subset=sseti)[,c("Mean")])
  N_tab <- table(curvar[sseti])
  CandQ <- merge(data.frame(N_tab), simcat_pctab, by.x="Var1", by.y="row.names", sort=F)
  rownames(CandQ) <- CandQ$Var1
  colnames(CandQ) <- c("", "N", "Revenue")
  if (!is.null(levels(curvar))) {
    write.table(na.omit(CandQ[match(levels(curvar), rownames(CandQ)),]), file=whereto, append=T, quote=F, sep=',', eol='\n') 
  } else {
    write.table(na.omit(CandQ), file=whereto, append=T, quote=F, sep=',', eol='\n') 
  }
  write.table("", file=whereto, append=T, quote=F, row.names=F, col.names=F)
}

whereto <- paste('~/projects/CBD/crosta/logs/revsum_', format(Sys.time(), "%Y_%b_%d_%H_%M_%S"), ifelse(IPEDS, "IPEDS", "Terri"), '.log', sep='')
Revsum(cp=103, pvar="start", sset=TRUE)  
Revsum(cp=103, pvar="assessment",sset=as.numeric(students$start)==7 | as.numeric(students$start)==8)  
Revsum(cp=103, pvar="MathPlacement", sset=(as.numeric(students$start)==7 | as.numeric(students$start)==8) & as.numeric(students$MathExam==1))  
Revsum(cp=103, pvar="EngPlacement", sset=(as.numeric(students$start)==7 | as.numeric(students$start)==8) & as.numeric(students$EngExam==1))  
Revsum(cp=103, pvar="ReadPlacement", sset=(as.numeric(students$start)==7 | as.numeric(students$start)==8) & as.numeric(students$ReadExam==1))  
sset <- as.numeric(students$start)>=7 & as.numeric(students$start)<=9
for (ii in c("status", "agecat", "somegrant", "somepell", "outcome", "conc_compare", "max_program_swc", "firstmajor_field", "lastmajor_field")) {
  Revsum(cp=103, pvar=ii, sset=sset)  
}


######## Cost simulations ################
## here is the strategy:
## We will adjust changes in several KPIs (key performance indicators)
## Increase or decrease baseline rates by 20%
students$FirstMajor <- students$firstmajor_field
levels(students$FirstMajor) <- c(rep("LAS", 4), rep("CTE", 2), "Bus/Market", 
                                 rep("CTE",7), "Education", rep("Health",2), 
                                  rep("CTE", 2), "Mechanics", rep("CTE",3), "Undeclared")
students$FirstMajor[is.na(students$FirstMajor)] <- "Undeclared"

CostSim <- function(cp, cost, sseta, from, to, amt, N.sim=1000, replacer=F, n.cores=2, stratify=NULL) {
  sseti <- sseta & students$cntInstitution==cp & !is.na(get(paste(cost, "_NC", sep='')))
  
  ## little data set created to help with stratification code.
  studset  <- subset(students, subset=sseti, select=c("StudentIdEncrypted", "race", "agecat", "gender_new","FirstMajor"))
   
  ## baseline cost computation
  costopts <- c("_IC", "_NCC", "_NC", "_TC")
  baseline_cost <- sapply(costopts, function(xx) mean(get(paste(cost, xx, sep=''))[sseti], na.rm=T))
  baseline_N <- sum(sseti, na.rm=T)
  baseline_Q <- sum(students$output[sseti], na.rm=T)
  CandQ <- c(baseline_N, baseline_Q, baseline_cost, baseline_cost["_TC"]*baseline_N)
  names(CandQ) <- c("N", "Q", "IC", "NCC", "NC", "TC", "C.N")
  CandQ <- c(CandQ, CandQ["C.N"]/CandQ["Q"])
  names(CandQ)[length(names(CandQ))] <- "CpQ" 

      #   ## baseline revenue computation
      #   revopts <- c("revbal_tot")
      #   baseline_revenue <- sapply(revopts, function(xx) sum(get(xx)[sseti], na.rm=T))
      # THIS IS THE ORIG CODE. BELOW SETS baseline R=E
  
  credsbytype.ss <- tapply(coursedat$budgfte[coursedat$StudentIdEncrypted %in% students$StudentIdEncrypted[sseti]], coursedat$occext[coursedat$StudentIdEncrypted %in% students$StudentIdEncrypted[sseti]], sum)
  currrevpc.ss <- baseline_cost["_TC"]*baseline_N*1.345/(credsbytype.ss["TRUE"]+1.345*credsbytype.ss["FALSE"])
  coursedat$budgval.ss[!coursedat$occext] <- coursedat$budgfte[!coursedat$occext]*currrevpc.ss
  coursedat$budgval.ss[coursedat$occext] <- coursedat$budgfte[coursedat$occext]*(currrevpc.ss/1.345)
  studrev.ss <- tapply(coursedat$budgval.ss[withdrawflag], coursedat$StudentIdEncrypted[withdrawflag], FUN=sum, na.rm=T)
  revbal.ss <- studrev.ss[names(totcreds) %in% students$StudentIdEncrypted[sseti]]
  baseline_revenue <- sum(revbal.ss, na.rm=T)
  #assign("revbal_tot", revbal[match(students$StudentIdEncrypted, names(revbal.ss))])
  
  
  ## (S-R)/Q - sort of net cost per output, spending minus revenue over output
  ## i think this should be (R-S)/Q for net revenue per output
  SRQ <- (CandQ["C.N"]-baseline_revenue)/CandQ["Q"]
  names(SRQ) <- "SRQ"
  RSQ <- (baseline_revenue-CandQ["C.N"])/CandQ["Q"]
  names(RSQ) <- "RSQ"
  
  ## simulation info - identify who is in the from and to groups
  from_flag <- eval(parse(text=from))[sseti]
  to_flag <- eval(parse(text=to))[sseti]
  
  ## the number in the to group (which is being incremented), and the number to increment
  N_to <- sum(to_flag, na.rm=T)
  N_from <- sum(from_flag, na.rm=T)
    
  ## under replacer flag, we want to replace amt% of students in the from group with students in the to group
  to_inc <- ifelse(replacer, N_from*amt, as.integer(N_to*amt))
  
  doSim <- function(x) {
    ## vector/matrix for outcomes and costs for the subset
    outcome.sim <- students$output[sseti]
    cost.meas.sim <- sapply(costopts, function(xx) get(paste(cost, xx, sep=''))[sseti])
    totcred.ftic <- totcreds_ftic #this one is not for the subset sseti

    ## select students in the from_grp that will be replaced with those in the to_grp
    from_grp <- sample(which(from_flag), to_inc)
    from_id <- names(sseti[sseti])[from_grp]
    
    if (is.null(stratify)) {
      to_grp <- sample(which(to_flag), to_inc, replace=T)         
      to_id <- names(sseti[sseti])[to_grp]
    } else if (stratify=="racesex") {
      studset <- studset[order(studset$race, studset$gender_new),]
      to_poss <- students$StudentIdEncrypted[sseti][which(to_flag)]

      ## figure out how many students from group that was removed fit into each race-gender cell.
      ## also add adjustments for when stratifications are not possible
      sta <- with(studset[studset$StudentIdEncrypted %in% from_id,], t(table(race, gender_new) * to_inc/length(from_id)))
      sta2 <- with(studset[studset$StudentIdEncrypted %in% to_poss,], t(table(race, gender_new) * to_inc/length(to_poss)))
      adju <- which(sta==0 & sta2!=0, arr.ind=T)
      sta[adju] <- 1
      adju2 <- which(sta!=0 & sta2==0, arr.ind=T)
      sta[adju2] <- 0
      sta <- sta[which(sta!=0)]
      
      ## sample with replacement from the candidate students so we match race-gender of from group
      s <- try(sampling::strata(studset[studset$StudentIdEncrypted %in% to_poss,],c("race","gender_new"),size=sta, method="srswr"),silent=TRUE)
      if (class(s)=="try-error") return(NULL)
      
      ## vector of IDs and numbers for stratitifed replacement sample
      to_id <- sampling::getdata(studset[studset$StudentIdEncrypted %in% to_poss,], s)$StudentIdEncrypted
      to_grp <- match(to_id,  students$StudentIdEncrypted[sseti])
      
      while (length(from_grp) != length(to_grp)) {
      if (length(from_grp) > length(to_grp)) {
        from_grp <- from_grp[-1]
        from_id <- names(sseti[sseti])[from_grp]
      } else if (length(from_grp) < length(to_grp)) {
        to_id <- to_id[-1]
        to_grp <- match(to_id,  students$StudentIdEncrypted[sseti])
      }
      }
    } else if (stratify=="field") {
      studset <- studset[order(studset$FirstMajor),]
      to_poss <- students$StudentIdEncrypted[sseti][which(to_flag)]
      
      ## figure out how many students from group that was removed fit into each race-gender cell.
      ## also add adjustments for when stratifications are not possible
      sta <- with(studset[studset$StudentIdEncrypted %in% from_id,], table(FirstMajor) * to_inc/length(from_id))
      sta2 <- with(studset[studset$StudentIdEncrypted %in% to_poss,], table(FirstMajor) * to_inc/length(to_poss))
      adju <- which(sta==0 & sta2!=0, arr.ind=T)
      sta[adju] <- 1
      adju2 <- which(sta!=0 & sta2==0, arr.ind=T)
      sta[adju2] <- 0
      sta <- sta[which(sta!=0)]
      
      ## sample with replacement from the candidate students so we match race-gender of from group
      s <- try(sampling::strata(studset[studset$StudentIdEncrypted %in% to_poss,],c("FirstMajor"),size=sta, method="srswr"),silent=TRUE)
      if (class(s)=="try-error") return(NULL)
      
      ## vector of IDs and numbers for stratitifed replacement sample
      to_id <- sampling::getdata(studset[studset$StudentIdEncrypted %in% to_poss,], s)$StudentIdEncrypted
      to_grp <- match(to_id,  students$StudentIdEncrypted[sseti])
      
      while (length(from_grp) != length(to_grp)) {
        if (length(from_grp) > length(to_grp)) {
          from_grp <- from_grp[-1]
          from_id <- names(sseti[sseti])[from_grp]
        } else if (length(from_grp) < length(to_grp)) {
          to_id <- to_id[-1]
          to_grp <- match(to_id,  students$StudentIdEncrypted[sseti])
        }
      }
    }
    #if (length(from_grp) != length(to_grp)) return(NULL)
    ## make replacements in outcome and cost vector/matrix
    outcome.sim[from_grp]  <- outcome.sim[to_grp]
    cost.meas.sim[from_grp,]  <- cost.meas.sim[to_grp,]
    totcred.ftic[from_id] <- totcred.ftic[to_id]
    
    ## recompute revenue using entire new sample, and then subset sseti after distributing by credit
    ## CHANGE 12/12/12: just use revbal for simulation group for newrevbal (revenue as stream attached to each kid)
    #aggcost.sim <- get(paste(cost, "_TC", sep=''))
    #aggcost.sim[from_id] <- aggcost.sim[to_id]
    #newrevbal <- sum(aggcost.sim, na.rm=T)*prop.table(totcred.ftic)
    #rev.meas.sim <- newrevbal[sseti]
      #newrevbal <- revbal_tot
      #newrevbal[from_id] <- newrevbal[to_id]
      rev.meas.sim <- revbal.ss
      #rev.meas.sim <- revbal_tot[sseti]
      rev.meas.sim[from_id] <- rev.meas.sim[to_id]
    #revpercredit <- sum(aggcost.sim, na.rm=T) / sum(totcred.ftic, na.rm=T)
    creds.sset <- sum(totcred.ftic[sseti], na.rm=T)
      #revpercredit <- sum(newrevbal, na.rm=T) / sum(totcred.ftic, na.rm=T)
    revpercredit <- sum(rev.meas.sim, na.rm=T) / sum(totcred.ftic[sseti], na.rm=T)
    
    ## new cost and revenue computations
    new_cost <- colMeans(cost.meas.sim,na.rm=T) 
    new_N <- baseline_N
    new_Q <-sum(outcome.sim, na.rm=T)
    newCandQ <- c(new_N, new_Q, new_cost, new_cost["_TC"]*new_N)
    names(newCandQ) <- c("N", "Q", "IC", "NCC", "NC", "TC", "C.N")
    newCandQ <- c(newCandQ, newCandQ["C.N"]/newCandQ["Q"])
    names(newCandQ)[length(names(newCandQ))] <- "CpQ" 
    new_rev <- sum(rev.meas.sim,na.rm=T) 
    names(new_rev) <- "revenue"
    names(revpercredit) <- "revpercredit"
    names(creds.sset) <- "creds.sset"
    c(newCandQ, new_rev, revpercredit, creds.sset, nsimed=length(to_grp))
  }
  
  ## set up parallel environment
  cl <- makeCluster(getOption("cl.cores", n.cores))  #revopts used to be in export
  clusterExport(cl, c("students", "studset", "stratify", "sseti", "cost", "costopts", paste(cost, costopts, sep=''), "revbal.ss", "totcreds_ftic", "from_flag", "to_flag", "to_inc", "baseline_N"), envir=environment())
  ## run simulations. 
  sim.list <- parLapply(cl, 1:N.sim, fun=doSim)
  stopCluster(cl)
  
  sim.df <- data.frame(do.call(rbind, sim.list))
  sim.df$SRQ <- (sim.df$C.N-sim.df$revenue)/sim.df$Q
  sim.df$RSQ <- (sim.df$revenue-sim.df$C.N)/sim.df$Q
  sim.df$outputrat <- sim.df$Q/baseline_Q-1
  sim.df$totcostrat <- sim.df$C.N/CandQ["C.N"]-1
  sim.df$tcperqrat <- sim.df$CpQ/CandQ["CpQ"]-1
  sim.df$totrevrat <- sim.df$revenue/baseline_revenue-1
  return(list(sim.df=sim.df, from=from, to=to, amt=amt, replacer=replacer, baseline_cost=CandQ, baseline_revenue=baseline_revenue, baselineSRQ=SRQ, baselineRSQ=RSQ))
}



### Simulations
n.cores <-7
## baseline
sim00 <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=0, from="students$concyear %in% c(0,2,3,4,5)", to="students$concyear %in% c(1)",
                 N.sim=2, n.cores=2)
sim00$from <- sim00$to <- "baseline"

## Decrease the number of students coming directly from high school who place below college level
## and increase the number of college-ready students.
## Replace students in dev referral with those in college-ready for recent HS grads (2005) 
sim01 <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$start %in% c('Dev_Referral') & students$HsDipYr==2005", 
                 to="students$start %in% c('College-Ready') & students$HsDipYr==2005", N.sim=1000, replacer=T, n.cores=n.cores)

## Students starting below college level and completing referred dev ed within first year
## Increase the number of students who compelte dev ed within first yaer. Replace students who
## don't complete dev ed within first year with students completing dev ed within first year
## CANNOT DO DUE TO DATA LIMITATIONS
##sim02 <- NULL

## Students passing required entry-level math and English within first year and first two years on first attempt
## identify students who pass entry level math/english courses within 1/2 years
matcourse <- c("MAT101", "MAT110", "MAT115", "MAT121", "MAT122", "MAT140", "MAT141", "MAT155", "MAT161", "MAT171")
engcourse <- c("ENG110", "ENG111")
passmat1 <- unique(coursedat$StudentIdEncrypted[coursedat$CourseNumber %in% matcourse & coursedat$Grade %in% c(99,1,2,3,4) & coursedat$AcademicYearId %in% c(2005)])
passmat2 <- unique(coursedat$StudentIdEncrypted[coursedat$CourseNumber %in% matcourse & coursedat$Grade %in% c(99,1,2,3,4) & coursedat$AcademicYearId %in% c(2005,2006)])
passeng1 <- unique(coursedat$StudentIdEncrypted[coursedat$CourseNumber %in% engcourse & coursedat$Grade %in% c(99,1,2,3,4) & coursedat$AcademicYearId %in% c(2005)])
passeng2 <- unique(coursedat$StudentIdEncrypted[coursedat$CourseNumber %in% engcourse & coursedat$Grade %in% c(99,1,2,3,4) & coursedat$AcademicYearId %in% c(2005,2006)])

# math first year
sim03a <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% passmat1) & students$MathPlacement %in% c(1,2,3,4)', 
                  to='students$StudentIdEncrypted %in% passmat1 & students$MathPlacement %in% c(1,2,3,4)', N.sim=1000, n.cores=n.cores)
# English first year
sim03b <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% passeng1) & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', 
                  to='students$StudentIdEncrypted %in% passeng1 & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', N.sim=1000, n.cores=n.cores)
# math two years
sim03c <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% passmat2) & students$MathPlacement %in% c(1,2,3,4)', 
                  to='students$StudentIdEncrypted %in% passmat2 & students$MathPlacement %in% c(1,2,3,4)', N.sim=1000, n.cores=n.cores)
# English two years
sim03d <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% passeng2) & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', 
                  to='students$StudentIdEncrypted %in% passeng2 & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', N.sim=1000, n.cores=n.cores)

## Students persisting from year 1 to year 2
## Increase the number of students persisting from year 1 to year 2. Replace 
## students not persisting from year 1 to year 2 with students persisting

persisters <- unique(coursedat$StudentIdEncrypted[coursedat$AcademicYearId==2006])
sim04 <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% persisters)', 
                  to='students$StudentIdEncrypted %in% persisters', N.sim=100, n.cores=n.cores)

## Students earning 12 college credist in first year
## Increase the number of students earning 12 college credits in first year. Replace
## students who do not earn 12 credits with those who do, in first year
atleast12 <- names(which(with(coursedat[coursedat$AcademicYearId %in% c(2005),], tapply(NumCreditsComplete,StudentIdEncrypted,sum,na.rm=T))>=12))
sim05 <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from='!(students$StudentIdEncrypted %in% atleast12)', to='students$StudentIdEncrypted %in% atleast12', 
                 N.sim=1000, n.cores=n.cores)

## Students earning 24 college credist in first two years
## Increase the number of students earning 24 college credits in first year. Replace
## students who do not earn 24 credits with those who do, in first year
atleast24 <- names(which(with(coursedat[coursedat$AcademicYearId %in% c(2005,2006),], tapply(NumCreditsComplete,StudentIdEncrypted,sum,na.rm=T))>=24))
sim06 <- CostSim(cp=103, cost=costassum[7],sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from='!(students$StudentIdEncrypted %in% atleast24)', to='students$StudentIdEncrypted %in% atleast24', 
                 N.sim=1000, n.cores=n.cores)

## Concentrators
## Students who concentrated in first year: Increase the number of students who concentrate within first year
sim07a <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$concyear %in% c(0,2,3,4,5)", to="students$concyear %in% c(1)", N.sim=1000, n.cores=n.cores)

sim07b <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from="students$concyear %in% c(0,3,4,5)", to="students$concyear %in% c(1,2)", N.sim=1000, n.cores=n.cores)


## KPIs for highest educational outcome
## 20% inc in xfers with credential pulled from xfers without credential; 20% dec in xfrer without credential
sim08a <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$outcome %in% c('Transfer_4-Year_NoCred')", 
                 to="students$outcome %in% c('Transferred_4-Year_Award')", N.sim=1000, n.cores=n.cores, replacer=T)


## replace 20% of lingerers with associate degree students (20% dec in lingerers)
sim08b <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$outcome %in% c('Enrolled_Year_5_with_30+_College_Credits')", 
                 to='students$outcome %in% c("Associate")', N.sim=1000, n.cores=n.cores,  replacer=T)

## Report simulation results
simReport1 <- function(simarg) {
  if (!exists(simarg)) return
  simres <- get(simarg)
  cat(simres$from, simres$to, simres$amt, simres$replacer, 
      r3(mean(simres$sim.df$N)), r2(mean(simres$sim.df$Q)),
      r3(mean(simres$sim.df$outputrat)), r3(mean(simres$sim.df$totcostrat)),
      r3(mean(simres$sim.df$tcperqrat)), r3(mean(simres$sim.df$totrevrat)), sep=":")
  cat('\n')
}

## Report simulation results
simReport2 <- function(simarg) {
  if (!exists(simarg)) return
  simres <- get(simarg)
  cat(simres$from, simres$to, r3(mean(simres$sim.df$N)), r2(mean(simres$sim.df$Q)), r2(sd(simres$sim.df$Q)),
      r3(mean(simres$sim.df$C.N)), r3(sd(simres$sim.df$C.N)),
      r3(mean(simres$sim.df$revenue)), r3(sd(simres$sim.df$revenue)),
      r3(mean(simres$sim.df$CpQ)), r3(sd(simres$sim.df$CpQ)),
      r3(mean(simres$sim.df$SRQ)), r3(sd(simres$sim.df$SRQ)),
      r3(mean(simres$sim.df$RSQ)), r3(sd(simres$sim.df$RSQ)), r2(mean(simres$sim.df$creds.sset)), r3(mean(simres$sim.df$revpercredit)),
      sep=":")
  cat('\n')
}

#### simulations with stratification by race and gender ####
cbind(Persisters=prop.table(table(students$race[students$StudentIdEncrypted %in% persisters & sseti])), NonPersisters=prop.table(table(students$race[!(students$StudentIdEncrypted %in% persisters) & sseti])))
cbind(Persisters=prop.table(table(students$gender_new[students$StudentIdEncrypted %in% persisters & sseti])), NonPersisters=prop.table(table(students$gender_new[!(students$StudentIdEncrypted %in% persisters) & sseti])))

cbind(GT12=prop.table(table(students$race[students$StudentIdEncrypted %in% atleast12 & sseti])), LT12=prop.table(table(students$race[!(students$StudentIdEncrypted %in% atleast12) & sseti])))
cbind(GT12=prop.table(table(students$gender_new[students$StudentIdEncrypted %in% atleast12 & sseti])), LT12=prop.table(table(students$gender_new[!(students$StudentIdEncrypted %in% atleast12) & sseti])))

cbind(GT24=prop.table(table(students$race[students$StudentIdEncrypted %in% atleast24 & sseti])), LT24=prop.table(table(students$race[!(students$StudentIdEncrypted %in% atleast24) & sseti])))
cbind(GT24=prop.table(table(students$gender_new[students$StudentIdEncrypted %in% atleast24 & sseti])), LT24=prop.table(table(students$gender_new[!(students$StudentIdEncrypted %in% atleast24) & sseti])))

cbind(linger=prop.table(table(students$race[students$outcome %in% c('Enrolled_Year_5_with_30+_College_Credits') & sseti])), assoc=prop.table(table(students$race[students$outcome %in% c("Associate") & sseti])))
cbind(linger=prop.table(table(students$gender_new[students$outcome %in% c('Enrolled_Year_5_with_30+_College_Credits') & sseti])), assoc=prop.table(table(students$gender_new[students$outcome %in% c("Associate") & sseti])))

prop.table(table(students$race[students$StudentIdEncrypted %in% persisters & sseti], students$gender_new[students$StudentIdEncrypted %in% persisters & sseti]))
prop.table(table(students$race[!(students$StudentIdEncrypted %in% persisters) & sseti], students$gender_new[!(students$StudentIdEncrypted %in% persisters) & sseti]))


cbind(Persisters=prop.table(table(students$FirstMajor[students$StudentIdEncrypted %in% persisters & sseti])), NonPersisters=prop.table(table(students$FirstMajor[!(students$StudentIdEncrypted %in% persisters) & sseti])))
cbind(GT24=prop.table(table(students$FirstMajor[students$StudentIdEncrypted %in% atleast24 & sseti])), LT24=prop.table(table(students$FirstMajor[!(students$StudentIdEncrypted %in% atleast24) & sseti])))


stratcat <-"racesex"
stratcat <- "field"

sim01_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$start %in% c('Dev_Referral') & students$HsDipYr==2005", 
                 to="students$start %in% c('College-Ready') & students$HsDipYr==2005", N.sim=1000, replacer=T, n.cores=n.cores, stratify=stratcat)
# math first year
sim03a_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% passmat1) & students$MathPlacement %in% c(1,2,3,4)', 
                  to='students$StudentIdEncrypted %in% passmat1 & students$MathPlacement %in% c(1,2,3,4)', N.sim=1000, n.cores=n.cores, stratify=stratcat)
# English first year
sim03b_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% passeng1) & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', 
                  to='students$StudentIdEncrypted %in% passeng1 & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', N.sim=1000, n.cores=n.cores, stratify=stratcat)
# math two years
sim03c_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% passmat2) & students$MathPlacement %in% c(1,2,3,4)', 
                  to='students$StudentIdEncrypted %in% passmat2 & students$MathPlacement %in% c(1,2,3,4)', N.sim=1000, n.cores=n.cores, stratify=stratcat)
# English two years
sim03d_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from='!(students$StudentIdEncrypted %in% passeng2) & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', 
                  to='students$StudentIdEncrypted %in% passeng2 & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', N.sim=1000, n.cores=n.cores, stratify=stratcat)

sim04_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                       amt=.2, from='!(students$StudentIdEncrypted %in% persisters)', 
                       to='students$StudentIdEncrypted %in% persisters', N.sim=1000, n.cores=n.cores, stratify=stratcat)

sim05_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from='!(students$StudentIdEncrypted %in% atleast12)', to='students$StudentIdEncrypted %in% atleast12', 
                 N.sim=1000, n.cores=n.cores, stratify=stratcat)

sim06_strat <- CostSim(cp=103, cost="avg_match",sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from='!(students$StudentIdEncrypted %in% atleast24)', to='students$StudentIdEncrypted %in% atleast24', 
                 N.sim=1000, n.cores=n.cores, stratify=stratcat)

sim07a_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from="students$concyear %in% c(0,2,3,4,5)", to="students$concyear %in% c(1)", N.sim=1000, n.cores=n.cores, stratify=stratcat)

sim07b_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from="students$concyear %in% c(0,3,4,5)", to="students$concyear %in% c(1,2)", N.sim=1000, n.cores=n.cores, stratify=stratcat)


sim08a_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from="students$outcome %in% c('Transfer_4-Year_NoCred')", 
                  to="students$outcome %in% c('Transferred_4-Year_Award')", N.sim=1000, n.cores=n.cores, replacer=T, stratify=stratcat)

sim08b_strat <- CostSim(cp=103, cost="avg_match", sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                  amt=.2, from="students$outcome %in% c('Enrolled_Year_5_with_30+_College_Credits')", 
                  to='students$outcome %in% c("Associate")', N.sim=1000, n.cores=n.cores,  replacer=T, stratify=stratcat)




curr.sims <- ls()[grep(pattern="sim[0-9]",ls())]
#curr.sims <- paste("sim1",5:9,sep="")

cat("From", "To", "%Change", "Replaced", "N", "Output", "Output %Change", "Cost %Change", "Cost/Output %Change", "Revenue %Change", "\n", sep=":")
invisible(sapply(curr.sims, simReport1))

cat("From", "To", "N", "Output", "SD", "Spending", "SD", "Revenue", "SD", "Spending per Output", "SD", "Net cost per output", "SD", "Net revenue per output", "SD", "\n", sep=":")
invisible(sapply(curr.sims, simReport2))


## sims over a range of amt values.
amts <- seq(0.05, 0.75, 0.05)
sim03list <- vector(mode="list", length=length(amts))
names(sim03list) <- as.character(amts)
for (listdex in amts) {
	sim03list[[as.character(listdex)]] <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), amt=listdex, from='!(students$StudentIdEncrypted %in% passmat1) & students$MathPlacement %in% c(1,2,3,4)', to='students$StudentIdEncrypted %in% passmat1 & students$MathPlacement %in% c(1,2,3,4)', N.sim=1000, n.cores=n.cores)
}


amts <- seq(0.05, 0.75, 0.05)
sim04list <- vector(mode="list", length=length(amts))
names(sim04list) <- as.character(amts)
for (listdex in amts) {
	sim04list[[as.character(listdex)]] <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), amt=listdex, from='!(students$StudentIdEncrypted %in% persisters)', to='students$StudentIdEncrypted %in% persisters', N.sim=1000, n.cores=n.cores)
}

amts <- seq(0.05, 0.75, 0.05)
sim08list <- vector(mode="list", length=length(amts))
names(sim08list) <- as.character(amts)
for (listdex in amts) {
	sim08list[[as.character(listdex)]] <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=listdex, from="students$outcome %in% c('Enrolled_Year_5_with_30+_College_Credits')", 
                 to='students$outcome %in% c("Associate")', N.sim=1000, n.cores=n.cores,  replacer=T)
}

simReport3 <- function(simarg) {
  simres <- simarg
  cat(simres$from, simres$to, simres$amt, simres$replacer, 
      r3(mean(simres$sim.df$N)), r2(mean(simres$sim.df$Q)),
      r3(mean(simres$sim.df$outputrat)), r3(mean(simres$sim.df$totcostrat)),
      r3(mean(simres$sim.df$tcperqrat)), r3(mean(simres$sim.df$totrevrat)), sep=":")
  cat('\n')
}

## Report simulation results
simReport4 <- function(simarg) {
  simres <- simarg
  cat(simres$from, simres$to, r3(mean(simres$sim.df$N)), r2(mean(simres$sim.df$Q)), r2(sd(simres$sim.df$Q)),
      r3(mean(simres$sim.df$C.N)), r3(sd(simres$sim.df$C.N)),
      r3(mean(simres$sim.df$revenue)), r3(sd(simres$sim.df$revenue)),
      r3(mean(simres$sim.df$CpQ)), r3(sd(simres$sim.df$CpQ)),
      r3(mean(simres$sim.df$SRQ)), r3(sd(simres$sim.df$SRQ)),
      r3(mean(simres$sim.df$RSQ)), r3(sd(simres$sim.df$RSQ)), r2(mean(simres$sim.df$creds.sset)), r3(mean(simres$sim.df$revpercredit)),
      sep=":")
  cat('\n')
}


cat("From", "To", "%Change", "Replaced", "N", "Output", "Output %Change", "Cost %Change", "Cost/Output %Change", "Revenue %Change", "\n", sep=":")
invisible(sapply(sim03list, simReport3))

cat("From", "To", "N", "Output", "SD", "Spending", "SD", "Revenue", "SD", "Spending per Output", "SD", "Net cost per output", "SD", "Net revenue per output", "SD", "\n", sep=":")
invisible(sapply(sim03list, simReport4))

cat("From", "To", "%Change", "Replaced", "N", "Output", "Output %Change", "Cost %Change", "Cost/Output %Change", "Revenue %Change", "\n", sep=":")
invisible(sapply(sim04list, simReport3))

cat("From", "To", "N", "Output", "SD", "Spending", "SD", "Revenue", "SD", "Spending per Output", "SD", "Net cost per output", "SD", "Net revenue per output", "SD", "\n", sep=":")
invisible(sapply(sim04list, simReport4))

cat("From", "To", "%Change", "Replaced", "N", "Output", "Output %Change", "Cost %Change", "Cost/Output %Change", "Revenue %Change", "\n", sep=":")
invisible(sapply(sim08list, simReport3))

cat("From", "To", "N", "Output", "SD", "Spending", "SD", "Revenue", "SD", "Spending per Output", "SD", "Net cost per output", "SD", "Net revenue per output", "SD", "\n", sep=":")
invisible(sapply(sim08list, simReport4))






###### OTHER SIMS                   

## 20% inc in associates, pulled from lingerers
sim05 <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$outcome %in% c('Enrolled_Year_5_with_30+_College_Credits')", 
                 to='students$outcome %in% c("Associate")', N.sim=1000)
## take all students more than 1 level below college and replace with students 1-level below and above
sim06 <- CostSim(cp=103, cost=costassum[7], sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=1, from='students$MathPlacement %in% c(2,3,4)', to='students$MathPlacement %in% c(1,0)', N.sim=1000, 
                 replacer=T, n.cores=n.cores)


#### specific simulations for JFF policy institute talk ####
## increase students starting college ready by 20%; replace 20% of students needing dev ed with those who are college ready
sim15 <- CostSim(cp=103, cost=costassum[7], 
                 sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$start %in% c('Dev_Referral')", 
                 to="students$start %in% c('College-Ready')", 
                 N.sim=1000, replacer=T, n.cores=n.cores)

## increase program concentration rates by 20%; replace 20% of non-concs with concs
sim16 <- CostSim(cp=103, cost=costassum[7], 
                 sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from='students$conc_compare %in% c("Non-Conce_(Attempted)")', 
                 to='students$conc_compare %in% c("LAS_Concentrators","CTE_Concentrators")', 
                 N.sim=1000, replacer=T, n.cores=n.cores)

## increase program concentrator completion by 20%; replaced 20% of concs without awards with concs with awards
sim17 <- CostSim(cp=103, cost=costassum[7],  
                 sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from='students$conc_compare %in% c("LAS_Concentrators","CTE_Concentrators") & !(students$outcome %in% c("Associate"))', 
                 to='students$conc_compare %in% c("LAS_Concentrators","CTE_Concentrators") & students$outcome %in% c("Associate")', 
                 N.sim=1000, replacer=T, n.cores=n.cores)

## increase lingerer completion by 20%; replace 20% of lingerers with associate students
sim18 <- CostSim(cp=103, cost=costassum[7], 
                 sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$outcome %in% c('Enrolled_Year_5_with_30+_College_Credits')", 
                 to='students$outcome %in% c("Associate")', 
                 N.sim=1000, replacer=T, n.cores=n.cores)

## among transfers, increase AA completion by 20%; replace 20% of xfer without credential to xfer with credential
sim19 <- CostSim(cp=103, cost=costassum[7], 
                 sseta=students$start %in% c("Dev_Referral", "College-Ready", "No_Info"), 
                 amt=.2, from="students$outcome %in% c('Transfer_4-Year_NoCred')", 
                 to="students$outcome %in% c('Transferred_4-Year_Award')", 
                 N.sim=1000, replacer=T, n.cores=n.cores)



###################################################################################################
###################################################################################################
################################# IPEDS - Delta Cost b##############################################
###################################################################################################
###################################################################################################

setwd("~/projects/CBD/deltacost/")
deltacost <- read.dta("delta_public_release_99_09.dta")
deltacost <- subset(deltacost, subset=state=="NC" & academicyear>=2005 & sector_revised==4)

unitids <- c(198622, 198260, 198376, 198905, 199856)
names(unitids) <- NCnames

## instructional expenditures are instruction01, instruction02
## the first is comprehensive; the second only looks at salaries and wages
## credhoursug are the instruction hour based on credit for undergraduates

## should be able to calculate cost per credit hour:
with(deltacost[deltacost$unitid%in%unitids,], tapply(instruction01/credhoursug, instname, mean))
with(deltacost[deltacost$unitid%in%unitids,], tapply(instruction02/credhoursug, instname, mean))

cp0509 <- with(deltacost[deltacost$unitid==unitids["CP"],], data.frame(instname, academicyear, r2(instruction01/1000), r2(instruction02/1000), credhoursug, r2(instruction01/credhoursug), r2(instruction02/credhoursug)))
cp0509adj  <- with(deltacost[deltacost$unitid==unitids["CP"],], data.frame(instname, academicyear, r2(instruction01/hepi_scalar_2009/1000), r2(instruction02/hepi_scalar_2009/1000), credhoursug, r2(instruction01/hepi_scalar_2009/credhoursug), r2(instruction02/hepi_scalar_2009/credhoursug)))
colnames(cp0509) <- colnames(cp0509adj) <- c("instname", "year", "instruction1", "instruction2", "credithours", "CPCH1", "CPCH2")

## plots of instructional expenditures and CPCH for Central Pied
par(mar = c(5, 4, 4, 5))
plot(cp0509$year, cp0509$instruction1, type="l", lwd=2, xlab="Academic Year", ylab="Instructional Expenditure in $1000", ylim=c(min(cp0509$instruction1, cp0509$instruction2),max(cp0509$instruction1, cp0509$instruction2)), col=1, lty=1)
lines(cp0509$year, cp0509$instruction2, type="l", lwd=2, lty=2, col=2)
par(new=T)
plot(cp0509$year, cp0509$CPCH1, type="l", lty=3, col=3, lwd=2, ylab="", xlab="", axes=FALSE, ylim=c(min(cp0509$CPCH1, cp0509$CPCH2),max(cp0509$CPCH1, cp0509$CPCH2)))
lines(cp0509$year, cp0509$CPCH2, type="l", lty=4, lwd=2, col=4)
axis(4, at=pretty(c(cp0509$CPCH1, cp0509$CPCH2)))
mtext(4, text="Cost per Credit Hour", line=3)
legend("center", legend=c("Instruct1", "Instruct2", "CPCH1", "CPCH2"), col=1:4, lty=1:4, cex=1, bty="n")


par(mar = c(5, 4, 4, 5))
plot(cp0509adj$year, cp0509adj$instruction1, type="l", lwd=2, xlab="Academic Year", ylab="Instructional Expenditure in $1000", ylim=c(min(cp0509adj$instruction1, cp0509adj$instruction2),max(cp0509adj$instruction1, cp0509adj$instruction2)), col=1, lty=1)
lines(cp0509adj$year, cp0509adj$instruction2, type="l", lty=2, lwd=2, col=2)
par(new=T)
plot(cp0509adj$year, cp0509adj$CPCH1, type="l", lty=3, col=3, lwd=2, ylab="", xlab="", axes=FALSE, ylim=c(min(cp0509adj$CPCH1, cp0509adj$CPCH2),max(cp0509adj$CPCH1, cp0509adj$CPCH2)))
lines(cp0509adj$year, cp0509adj$CPCH2, type="l", lty=4, col=4, lwd=2)
axis(4, at=pretty(c(cp0509adj$CPCH1, cp0509adj$CPCH2)))
mtext(4, text="Cost per Credit Hour", line=3)
legend("center", legend=c("Instruct1", "Instruct2", "CPCH1", "CPCH2"), col=1:4, lty=1:4, cex=1, bty="n")

###################################################################################################
###################################################################################################
################################# IPEDS - Delta Cost e##############################################
###################################################################################################
###################################################################################################


