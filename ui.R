
#written by Samuel Nelson and Daniel Hadley in 2016-2017
#
library(shiny)
library(scales)
library(plotly)
library(shinythemes)
# Define UI for random distribution application 



fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("RecidiViz: Analyzing Reincarceration"),


  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  fluidRow(
    column(3,
      sliderInput("recid_rate", 
                  "5-yr Recidivism Rate:", 
                  value = 55,
                  min = 0, 
                  max = 99,
                  post = "%"),
      
      br(),
      
      sliderInput("prison_time_served", 
                  "Average Prison Sentence (months):", 
                  value = 37,
                  min = 1, 
                  max = 120),
      
      br(),
      
      sliderInput("cost_per_yr", 
                  "Cost / Prisoner / Yr:", 
                  value = 30000,
                  min = 0, 
                  max = 60000,
                  pre = "$"),
      
      
      
      br(),
      
      actionButton("goButton", "Re-Run the Model"),
      p("This simulates 1,000 prisoners over 60 months, so it may take a few seconds.")
    ),
    
      
    # of the generated distribution
    column(
      width = 8,
      fluidRow(htmlOutput("graph1"),
               plotlyOutput("plot")
      ),
      fluidRow(htmlOutput("costData")
      ),
      fluidRow(htmlOutput("graph2"),
               plotlyOutput("plot2")
      ),
      fluidRow(htmlOutput("graph3"),
               plotlyOutput("plot3")
      ),
      fluidRow(
        h3("Methodology"),
        p("Recidivism is one of society's most persistent yet misunderstood problems. Everyone from politicians to", a("Supreme Court Justices", href= "https://www.themarshallproject.org/2014/12/04/the-misleading-math-of-recidivism#.AQSpHMFig"), "seem to get it wrong. This model uses data from the Bureau of Justice Statistics to simulate 1,000 parolees released on the same day."),
        h4("Recidivism Rate"),
        p("The recidivism rate is here defined as the percent of parolees who return to prison at least once within the specified time frame. The default for this is set to 55%, with a curve that matches national trends described in the following BJS report:", a("Recidivism Of Prisoners Released In 30 States In 2005.", href= "https://www.bjs.gov/content/pub/pdf/rprts05p0510.pdf"), "The data comes from the Filename:", a(" rprts05p0510f01", href = "https://www.bjs.gov/index.cfm?ty=pbdetail&iid=4986"),". This is used to estimate a parolee's odds of recidivating in a given month. The conditional probability of continued freedom is given by:"),
        withMathJax("$$P_i =\\frac{f_i - r_i}{f_i}$$"),
        p("where f is the number of people free but at risk of recidivating in the ith interval, and r is the number who recidivated in the same period."),
        p("As described above, someone right out of prison is more likely to recidivate than someone who has been free for several months. This probability is recalculated each month for all parolees who are out of prison."),
        h4("Prison Sentences"),
        p("The prison time parameter is used to build a distribution of possible prison sentences. Data comes from", a("Federal Justice Statistics, 2013 - Statistical Tables", href = "https://www.bjs.gov/index.cfm?ty=pbdetail&iid=5874"), ". Specifically, the average prison sentence is listed as 37.5 months in the file", a("fj13stt7.11.csv", href = "https://www.bjs.gov/content/pub/sheets/fjs13st.zip"), ". It is important to note that this data comes from *all* prisoners, not just parolees. This is significant because parolees generally serve shorter average sentences than first-time prisoners. They are more likely to return for less serious issues, such as a parole violation. Therefore, it is advisable to try different (lower) values for the average sentence length, testing how changes to this parameter affect the simulation outcome."),
        p("This application uses an exponential distribution to model the prison sentence that a parolee serves if he recidivates."),
        withMathJax("$$X \\sim \\exp(\\lambda)$$"),
        p("where X is the prison sentence and is drawn randomly from an exponential distribution that is parameterized by lamda, which is 1/mean."),
        p("The reason for selecting an exponential distribution (as opposed to, say, a normal distribution) is that it better captures the difference between the average prison sentence and the median, 25.3. In many prison systems, the average is much longer because of outliers, i.e., those who commit serious crimes and go to prison for life. The median is more representative of a typical outcome. Nevertheless, we do not want to leave out the possibility of someone receiving a very long sentence."),
        h4("Costs"),
        p("The Vera Institute reports that the average per-inmate cost was $31,286 in FY2010", a("(Source)", href = "https://www.vera.org/publications/the-price-of-prisons-what-incarceration-costs-taxpayers"), ". This varies widely by state, but could be used as good starting point for calculating total costs. In order to calculate the potential benefits of reducing recidivism, we strongly recomend using", a("marginal costs.", href = "http://archive.vera.org/sites/default/files/resources/downloads/marginal-costs-guide.pdf")),
        p("We hope that analysts will be able to use this to approximately judge how prison time changes as a function of the average prison sentence in conjunction with the recidivism rate. We are available to answer questions, and help communities that wish to measure the impact of recidivism using local data."),
        p(a("- The Sorenson Impact Data Team", href = "http://sorensonimpact.com/team"))
        )
      )
    )
  )

