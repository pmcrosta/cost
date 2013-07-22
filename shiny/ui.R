NCinst <-  c(102, 103, 104, 105, 106)
NCnames <- c("Guilford", "CP", "Davidson", "Martin", "Wake")
names(NCinst) <- NCnames

shinyUI(pageWithSidebar(
  
  headerPanel("Cost Simulation Tool Prototype - version 0.9"),
  
  sidebarPanel(
    
  selectInput(inputId = "college",
              label = "Select college:",
              choices = NCnames[2],
              selected = "CP"),
    
  selectInput(inputId = "simchoice",
              label = "Select KPI simulation:",
              selected="baseline",
              choices = c("Baseline only"="baseline",
                          "Increase college-ready recent HS grads"="collegeready",
                          "Increase in passing college math in first year"="mathpass1yr",
                          "Increase in passing college English in first year"="engpass1yr",
                          "Increase in passing college math within two years"="mathpass2yr",
                          "Increase in passing college English within two years"="engpass2yr",
                          "Increase in persisting from year 1 to year 2"="persist",
                          "Increase in earning at least 12 credits by first year"="earn12",
                          "Increase in earning at least 24 credits within two years"="earn24",
                          "Increase in first-year concentrators"="conc1yr",
                          "Increase in concentrators within first two years"="conc2yr",
                          "Increase in transfer with credential"="xfercred",
                          "Increase in AA recipients from 30-credit lingerers"="linger")
              ),
  
  
  sliderInput(inputId = "amount",
              label = "Amount of simulation (X% change)",
              min = 1, max = 100, value = 20, step = 1),
  
  br(),
  
  checkboxInput(inputId = "replacersim",
                label = strong("Replace initial group?"),
                value = FALSE),
  
  helpText("Note: Default simulation increases target group by X%.",
           "If 'Replace initial group' is checked, simulation instead replaces X% of initial group with target group (i.e. decreases initial group by X%.)"),
  
  conditionalPanel(condition="$('div#baseline').text()=='' || $('html').hasClass('shiny-busy')", img(src="loading.gif")),
  conditionalPanel(condition="!($('div#baseline').text()=='' || $('html').hasClass('shiny-busy'))", br()),
  submitButton("Run simulation")
  #, div(class="progress progress-striped active", div(class="bar", style="width: 100%;"))
  ),
  
  mainPanel(
  
  tabsetPanel(
    tabPanel("Table", h2("Baseline"),
             tableOutput(outputId="baseline"),
             p("Note: Dollars are estimated amounts that reflect five years of cohort costs."),
             #br(),
             h2("Simulation Results"),
             textOutput("summary"), br(),
       tableOutput("sim")), 
    
    tabPanel("Graphs", conditionalPanel(condition="!($('div#summary').text()=='Please run a simulation.')",
             h3("Output"), 
             plotOutput(outputId = "graphsoutput", height = "300px"), 
             h3("Expenditure"), plotOutput(outputId = "graphsexpend", height = "300px"), 
             h3("Efficiency"), plotOutput(outputId = "graphseffic", height = "300px"))), 
    
    tabPanel("Model Details", 
             h2("About"),
             p("This application is a companion to the paper: ",
               em("Can Community Colleges Afford to Improve Completion?  Measuring the Costs and Efficiency Effects of Recent Completion Reforms."), 
               "by Clive Belfield, Peter Crosta, and Davis Jenkins of the", 
               a(href="http://ccrc.tc.columbia.edu", "Community College Research Center,"), 
               "Teachers College, Columbia University."),
             p("This work is funded by the Bill & Melinda Gates Foundation through the Completion by Design initiative."),
             
             h2("Simulation Overview"),
             p("We simulate the economic effects of increasing key performance indicators (KPI) relative to the baseline."),
             p("Choosing 20 percent improvements for illustration purposes only, the simulations for each KPI are as follows. First, we 'move' 20% more students from the baseline sample of students into the desired category. Second, we calculate the economic consequences of having a different composition of students. Finally, we report these consequences relative to the baseline."),
             p("For example, the first progress KPI target is to improve persistence from year one to year two by 20%. We randomly remove students from the baseline sample who were not persisting and we randomly add more students who were persisting. Keeping the total number of students constant, we make these replacements until the persistence rate is 20% higher. This changes the collegeâ€™s expenditures because the pathway costs of the removed students differ from the pathway costs of the added students. We then calculate the new completion rates, expenditures, revenue, net revenues, and efficiency consequences. We perform this simulation 1,000 times and take the average of the 1,000 simulations."), 
             p("We simulate the economic effects on the assumption that these KPIs have been met, i.e. by calculating them based on the new sample created by the simulations. However, we do not calculate how much strategies to meet KPI goals would cost to implement. Therefore it is not appropriate to compare reforms directly. Clearly, these costs should be factored into the decision-making process but they will depend on which strategies are selected."),
             
             h2("Definitions and Equations"),
             h3("Completions"), p(HTML("Q = &alpha;*AA + &beta;*CS + &delta;*CL + &rho;*TR1 + &epsilon;*TR2 + &gamma;*TR3 + &lambda;*Z")),
             p("AA: Associate degrees"), p(HTML("CS: Certificate &lt;1 year")), 
             p("CL: Certificate 1+ year"), p("TR1: Transfer with award to 4-year college"),
             p("TR2: Transfer with no award to 4-year college"),
             p("TR3: Bachelorâ€™s degree at other college"),
             p("Z: No longer enrolled, no credential, no transfer, still enrolled after five years, certificate at other 2-year college"),
             p(HTML("&alpha; =1.00, &lambda;=0.00, &beta; &delta; &rho; &epsilon; &gamma; vary per college")),
             h3("Expenditures (E)"),
             p("E = DI + II + NI + K"),
             p("DI: Direct Instructional Expenses, Wages of FT and PT faculty in the classroom (including their SS payments)"),
             p("II: Indirect Instructional Expenses, Equipment, materials and other expenses"),
             p("NI: Non-instructional Expenses from sample xollege general ledger, (Presidential, Administrative, Instructional, Educational support, Finance/administrative)"),
             p("K:  Capital Expenses"),
             p("Allocation formulae: Per enrollee, per credit, multiples of DI"),
             h3("Revenues (R)"),
             p("R = F + T + G"),
             p("F: Fees paid by students"),
             p("T: Tuition charges per course"),
             p("G: Government subsidy per student based on state funding formula"),
             p("Allocation formulae: Per enrollee, per credit")       
             
  ))))
)
