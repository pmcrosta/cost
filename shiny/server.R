# library(Hmisc)
library(parallel)
library(ggplot2)
load("./data/ncsim_glimmer.Rdata")

options(scipen=3)
lunique <- function(x) length(unique(x))
pdensity <- function(x) plot(density(x, na.rm=T))
r2 <- function(x) round(x,2)
r3 <- function(x) round(x,3)

NCinst <-  c(102, 103, 104, 105, 106)
NCnames <- c("Guilford", "CP", "Davidson", "Martin", "Wake")
names(NCinst) <- NCnames

CostSim <- function(cp, cost, sseta, from, to, amt, N.sim=1000, replacer=F, n.cores=2, coursedat2=coursedat) {
  sseti <- sseta & students$cntInstitution==cp & !is.na(get(paste(cost, "_NC", sep='')))
  
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
  credsbytype.ss <- tapply(coursedat2$budgfte[coursedat2$StudentIdEncrypted %in% students$StudentIdEncrypted[sseti]], coursedat2$occext[coursedat2$StudentIdEncrypted %in% students$StudentIdEncrypted[sseti]], sum)
  currrevpc.ss <- baseline_cost["_TC"]*baseline_N*1.345/(credsbytype.ss["TRUE"]+1.345*credsbytype.ss["FALSE"])
  coursedat2$budgval.ss <- NA
  coursedat2$budgval.ss[!coursedat2$occext] <- coursedat2$budgfte[!coursedat2$occext]*currrevpc.ss
  coursedat2$budgval.ss[coursedat2$occext] <- coursedat2$budgfte[coursedat2$occext]*(currrevpc.ss/1.345)
  studrev.ss <- tapply(coursedat2$budgval.ss[withdrawflag], coursedat2$StudentIdEncrypted[withdrawflag], FUN=sum, na.rm=T)
  revbal.ss <- studrev.ss[students$StudentIdEncrypted[sseti]]
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
    to_grp <- sample(which(to_flag), to_inc, replace=T)
    
    from_id <- names(sseti[sseti])[from_grp]
    to_id <- names(sseti[sseti])[to_grp]
    
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
    c(newCandQ, new_rev, revpercredit, creds.sset)
  }
  
  ## set up parallel environment
  cl <- makeCluster(getOption("cl.cores", n.cores))  #revopts used to be in export
  clusterExport(cl, c("students", "sseti", "cost", "costopts", paste(cost, costopts, sep=''), "revbal.ss", "totcreds_ftic", "from_flag", "to_flag", "to_inc", "baseline_N"), envir=environment())
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
  return(list(sim.df=sim.df, from=from, to=to, amt=amt, replacer=replacer, baseline_cost=CandQ, baseline_revenue=baseline_revenue, baselineSRQ=SRQ, baselineRSQ=RSQ, naffect=to_inc))
}

simReport1 <- function(simarg) {
  simres <- simarg
  simret <- c(simres$from, simres$to, simres$replacer, 100*simres$amt,
      r3(mean(simres$sim.df$N)), round(mean(simres$sim.df$Q)),
      100*r3(mean(simres$sim.df$outputrat)), 100*r3(mean(simres$sim.df$totcostrat)),
    100*r3(mean(simres$sim.df$tcperqrat)), 100*r3(mean(simres$sim.df$totrevrat)),
    r3(mean(simres$sim.df$C.N)), r3(mean(simres$sim.df$revenue)),  r3(mean(simres$sim.df$CpQ)))
  names(simret) <- c("From", "To", "Replaced", "Percent Change", "N", "Output", "Output Percent Change", "Cost Percent Change", "Cost/Output Percent Change", "Revenue Percent Change", "Cost", "Revenue", "Cost/Output")
  return(simret)
}


ssetcred <- students$start %in% c("Dev_Referral", "College-Ready", "No_Info")
persisters <- unique(coursedat$StudentIdEncrypted[coursedat$AcademicYearId==2006])
matcourse <- c("MAT101", "MAT110", "MAT115", "MAT121", "MAT122", "MAT140", "MAT141", "MAT155", "MAT161", "MAT171")
engcourse <- c("ENG110", "ENG111")
passmat1 <- unique(coursedat$StudentIdEncrypted[coursedat$CourseNumber %in% matcourse & coursedat$Grade %in% c(99,1,2,3,4) & coursedat$AcademicYearId %in% c(2005)])
passmat2 <- unique(coursedat$StudentIdEncrypted[coursedat$CourseNumber %in% matcourse & coursedat$Grade %in% c(99,1,2,3,4) & coursedat$AcademicYearId %in% c(2005,2006)])
passeng1 <- unique(coursedat$StudentIdEncrypted[coursedat$CourseNumber %in% engcourse & coursedat$Grade %in% c(99,1,2,3,4) & coursedat$AcademicYearId %in% c(2005)])
passeng2 <- unique(coursedat$StudentIdEncrypted[coursedat$CourseNumber %in% engcourse & coursedat$Grade %in% c(99,1,2,3,4) & coursedat$AcademicYearId %in% c(2005,2006)])
atleast12 <- names(which(with(coursedat[coursedat$AcademicYearId %in% c(2005),], tapply(NumCreditsComplete,StudentIdEncrypted,sum,na.rm=T))>=12))
atleast24 <- names(which(with(coursedat[coursedat$AcademicYearId %in% c(2005,2006),], tapply(NumCreditsComplete,StudentIdEncrypted,sum,na.rm=T))>=24))

nsims <- 500
ncores <- 6

shinyServer(function(input, output) {
  
  output$baseline <- reactiveTable(function() {
    sim00 <- CostSim(cp=NCinst[input$college], cost="avg_match", sseta=ssetcred, 
                     amt=0, from="students$concyear %in% c(0,2,3,4,5)", to="students$concyear %in% c(1)",
                     N.sim=4, n.cores=4)
    sim00$from <- sim00$to <- "baseline"
    simbase2.df <<- out.df <- data.frame(t(simReport1(sim00)), stringsAsFactors=F)
    baseout <- t(matrix(c(out.df$N, out.df$Output,  paste0("$",formatC(as.numeric(out.df[,c("Cost", "Revenue", "Cost.Output")]), big.mark=",", mode="integer")))))
    dimnames(baseout) <- list(c("Baseline"), c("N", "Output", "Cost", "Revenue", "Cost per Output"))
    baseout
    })
  
  ## switch gets from group, to group, labels?
  output$sim <- reactiveTable(function() {
    simargs <- switch(input$simchoice,
           baseline=NULL,
           collegeready=c("students$start %in% c('Dev_Referral') & students$HsDipYr==2005", "students$start %in% c('College-Ready') & students$HsDipYr==2005"),
           mathpass1yr=c('!(students$StudentIdEncrypted %in% passmat1) & students$MathPlacement %in% c(1,2,3,4)', 'students$StudentIdEncrypted %in% passmat1 & students$MathPlacement %in% c(1,2,3,4)'),
           engpass1yr=c('!(students$StudentIdEncrypted %in% passeng1) & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', 'students$StudentIdEncrypted %in% passeng1 & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))'),
           mathpass2yr=c('!(students$StudentIdEncrypted %in% passmat2) & students$MathPlacement %in% c(1,2,3,4)', 'students$StudentIdEncrypted %in% passmat2 & students$MathPlacement %in% c(1,2,3,4)'),
           engpass2yr=c('!(students$StudentIdEncrypted %in% passeng2) & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))', 'students$StudentIdEncrypted %in% passeng2 & (students$EngPlacement %in% c(1,2,3,4) | students$ReadPlacement %in% c(1,2,3,4))'),
           persist=c('!(students$StudentIdEncrypted %in% persisters)', 'students$StudentIdEncrypted %in% persisters'),
           earn12=c('!(students$StudentIdEncrypted %in% atleast12)', 'students$StudentIdEncrypted %in% atleast12'),
           earn24=c('!(students$StudentIdEncrypted %in% atleast24)', 'students$StudentIdEncrypted %in% atleast24'),
           conc1yr=c("students$concyear %in% c(0,2,3,4,5)", "students$concyear %in% c(1)"),
           conc2yr=c("students$concyear %in% c(0,3,4,5)", "students$concyear %in% c(1,2)"),
           xfercred=c("students$outcome %in% c('Transfer_4-Year_NoCred')", "students$outcome %in% c('Transferred_4-Year_Award')"),
           linger=c("students$outcome %in% c('Enrolled_Year_5_with_30+_College_Credits')", 'students$outcome %in% c("Associate")'))
   
    if (!is.null(simargs)) {
      sim04 <- CostSim(cp=NCinst[input$college], cost="avg_match", sseta=ssetcred, 
                       amt=input$amount/100, from=simargs[1], to=simargs[2], 
                       N.sim=nsims, n.cores=ncores,replacer=input$replacersim)
      naffect <<- sim04$naffect
      sim42.df <<- out.df <- data.frame(t(simReport1(sim04)), stringsAsFactors=F)
      simout <- t(matrix(c(out.df$N, out.df$Output, out.df$Output.Percent.Change, out.df$Cost.Percent.Change, out.df$Cost.Output.Percent.Change, out.df$Revenue.Percent.Change,  paste0("$",formatC(as.numeric(out.df[,c("Cost", "Revenue", "Cost.Output")]), big.mark=",", mode="integer")))))
      dimnames(simout) <- list(c("Simulation"), c("N", "Output", "Output percent change", "Cost percent change", "Cost per Output percent change", "Revenue percent change", "Cost", "Revenue", "Cost per Output"))
      simout      
    }
  })

  
  output$graphsoutput <- reactivePlot(function() {
    if (input$simchoice!="baseline") {
      simcomb <- rbind(simbase2.df, sim42.df)
      rownames(simcomb) <- c("Baseline", "Simulation")
      simcomb$type <- rownames(simcomb)
      a <- ggplot(simcomb, aes(y=as.numeric(Output), x=type))
      gplot <- a+geom_bar(stat="identity", fill=c("#a93730","#3c5a98")) + xlab("") +ylab("Output") + coord_flip() + ggtitle("Baseline and Simulated Output")           
      print(gplot)
    } #else {
      #barplot(as.numeric(simbase2.df$Output), main="Baseline Output",ylab="Output", names.arg="Baseline")
    #}
  })
  
  output$graphsexpend <- reactivePlot(function() {
    if (input$simchoice!="baseline") {
      simcomb <- rbind(simbase2.df, sim42.df)
      rownames(simcomb) <- c("Baseline", "Simulation")
      simcomb$type <- rownames(simcomb)
      a <- ggplot(simcomb, aes(y=as.numeric(Cost), x=type))
      gplot <- a+geom_bar(stat="identity", fill=c("#a93730","#3c5a98")) + xlab("") +ylab("Expenditure") + coord_flip() + ggtitle("Baseline and Simulated Expenditure")           
      print(gplot)
    } #else {
      #barplot(as.numeric(simbase2.df$Cost), main="Baseline Spending",ylab="Spending", names.arg="Baseline")
    #}
  })
  
  output$graphseffic <- reactivePlot(function() {
    if (input$simchoice!="baseline") {
      simcomb <- rbind(simbase2.df, sim42.df)
      rownames(simcomb) <- c("Baseline", "Simulation")
      simcomb$type <- rownames(simcomb)
      a <- ggplot(simcomb, aes(y=as.numeric(Cost.Output), x=type))
      gplot <- a+geom_bar(stat="identity", fill=c("#a93730","#3c5a98")) + xlab("") +ylab("Expenditure per output") + coord_flip() + ggtitle("Baseline and Simulated Expenditure per Output")           
      print(gplot)
    } #else {
      #barplot(as.numeric(simbase2.df$Cost.Output), main="Baseline Spending per Output",ylab="Spending", names.arg="Baseline")
    #}
  })
  

  output$summary <- reactiveText(function() {    
    simdesc <- switch(input$simchoice,
                      baseline=NULL,
                      collegeready=c("recent high school graduates placed into developmental education",
                                     "recent high school graduated deemed college-ready"),
                      mathpass1yr=c("students who did not pass college-level math in year 1", 
                                    "students who passed college-level math in year 1"),
                      engpass1yr=c("students who did not pass college-level English in year 1", 
                                   "students who passed college-level English in year 1"),
                      mathpass2yr=c("students who did not pass college-level math within 2 years", 
                                    "students who passed college-level math within 2 years"),
                      engpass2yr=c("students who did not pass college-level English within 2 years", 
                                   "students who passed college-level English within 2 years"),
                      persist=c("students who did not persist from year 1 to year 2", 
                                "students who persisted from year 1 to year 2"),
                      earn12=c("students who did not earn at least 12 credits in first year", 
                               "students who earned at least 12 credits in first year"),
                      earn24=c("students who did not earn at least 24 credits within first 2 years", 
                               "students who earned at least 24 credits within first 2 years"),
                      conc1yr=c("students who did not concentrate in first year", 
                                "students who concentrated in first year"),
                      conc2yr=c("students who did not concentrate within first 2 years", 
                                "students who concentrated within first 2 years"),
                      xfercred=c("students who did not transfer to a 4-year school without an associate degree", 
                                 "students who transferred to a 4-year school without an associate degree"),
                      linger=c("students who lingered with 30 or more credits after 5 years", 
                               "students who earned an associate degree"))
    if (!is.null(simdesc)) {
      if (input$replacersim==FALSE) {
        desctext <- paste("This simulation increased ", simdesc[2], " by ", input$amount, "%", " by taking them from ", simdesc[1], ". ", sep='')
      } else {
        desctext <- paste("This simulation took ", input$amount, "% of ", simdesc[1], " and replaced them with ", simdesc[2], ". ", sep='')
      }
      desctext <- paste(desctext, "The number of students involved in the simulation was ", naffect, ".", sep='')
      desctext
    }
    else {
      desctext <- "Please run a simulation."
      desctext
    }
  })
  
})
