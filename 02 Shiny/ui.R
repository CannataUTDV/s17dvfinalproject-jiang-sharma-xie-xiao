#ui.R
library(plotly)
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Barchart", tabName = "barchart1", icon = icon("dashboard")),
      menuItem("Boxplot", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("Histogram", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Scatter Plot", tabName = "scatterplot", icon = icon("dashboard")),
      menuItem("Crosstabs,KPIS,Parameters", tabName = "crosstab", icon = icon("dashboard"))
    )
  ),
  
  
  dashboardBody(    
    tabItems(
      # Begin Barchart1 tab content.
      tabItem(tabName = "barchart1",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb2", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click2",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("barchartData1")
                ),
                tabPanel("Barchart", "Black = Violent Crimes Done, Red = Average Number of Violent Crimes, and  Blue = (ViolentCrimes per State - Average Number of Crimes per State Over Times)", plotOutput("barchartPlot1", height=15000))
              )
      ),
      # End Barchart1 tab content.
      
      # Begin Boxplot tab content.
      tabItem(tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("regions3"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Boxplot",plotlyOutput("boxplotPlot1", height=500))
              )
      ),
      # End Boxplot tab content.
      
    
      
      # Begin Histogram tab content.
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb4", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("regions4"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("histogramdata1")
                ),
                tabPanel("Histogram",plotlyOutput("histogramPlot1", height=2500))
              )
      ),
      # End Histogram tab content.
      
      # Begin Scatter Plot tab content.
      tabItem(tabName = "scatterplot",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb5", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("regions5"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("scatterplotdata1")
                ),
                tabPanel("Scatter Plot",plotlyOutput("scatterPlot1", height=3000))
              )
      ),
      #End Scatter Plot tab Content
      tabItem(tabName = "crosstab",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb6", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         
                         sliderInput("KPI1", "Low_CrimetoEmployment:", 
                                     min = 0, max = .009,  value = .009),
                         sliderInput("KPI2", "Medium_CrimetoEmployment:", 
                                     min = .009, max = .017,  value = .017),
                         actionButton(inputId = "click6",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("plotdata1")
                ),
                tabPanel("CrossTab ",plotOutput("plot1", height=1500))
              )
      )
      
      
      
      
      
      
    
  ))
)


