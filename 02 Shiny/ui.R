#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualization One", tabName = "barchart1", icon = icon("dashboard")),
      menuItem("Visualization Two", tabName = "barchart2", icon = icon("dashboard")),
      menuItem("Visualization Three", tabName = "barchart3", icon = icon("dashboard"))
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
                tabPanel("Barchart", "Black = Sum of Sales per Region, Red = Average Sum of Sales per Category, and  Blue = (Sum of Sales per Region - Average Sum of Sales per Category)", plotOutput("barchartPlot1", height=15000))
              )
      ),
      # End Barchart1 tab content.
      
      # Begin Barchart2 tab content.
      tabItem(tabName = "barchart2",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("regions3"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("barchartData2")
                ),
                tabPanel("Barchart", "Map of United States of America and GSP Values in 2015", plotOutput("barchartPlot2", height=500))
              )
      ),
      # End Barchart2 tab content.
      
      # Begin Barchart3 tab content.
      tabItem(tabName = "barchart3",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb4", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("regions4"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("barchartData3")
                ),
                tabPanel("Barchart", "Map of the United States of America and Income/Compensation Ratios in 2015", plotOutput("barchartPlot3", height=500))
              )
      )
      # End Barchart3 tab content.
      
      
      
      
    )
  )
)


