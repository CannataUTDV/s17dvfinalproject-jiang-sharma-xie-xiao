# server.R
library(plotly)
require(ggplot2)
require(dplyr)
require(plotly)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)

conn <- data.world(token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmFzaHJvb20xNCIsImlzcyI6ImFnZW50OmFzaHJvb20xNDo6YzU2MWMyYTYtNzZhYS00ZTNmLWI5MjAtOTBlZmQzYjczNDdlIiwiaWF0IjoxNDg0ODY3OTI3LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.hEBz_EXKG5iOLa6xRvGhL-M8TqWuVa4AoZ0SbDd4kULe86diYRHjE8EYV72uzwnp-oEQISCp9NPAG13QHfj0yQ")

shinyServer(function(input, output) { 
    
    # These widgets are for the visualizations 
    online2 = reactive({input$rb2})
    online3 = reactive({input$rb3})
    online4 = reactive({input$rb4})
    online5 = reactive({input$rb5})
    
    #these widgets are for the crosstab sliders
    KPI_Low = reactive({input$KPI1})     
    KPI_Medium = reactive({input$KPI2})
 
    
    # Begin Barchart Tab ------------------------------------------------------------------
    df2 <- eventReactive(input$click2, {
        print("Getting from data.world")
        tdf = query(conn,
                    dataset="colinjianingxie/s-17-dv-final-project", type="sql",
                    query="select `FinalCrimeData.csv/FinalCrimeData`.`Violent crime total` as ViolentCrime,`FinalCrimeData.csv/FinalCrimeData`.State as state_name,`FinalCrimeData.csv/FinalCrimeData`.Year as year
from FinalCrimeData left inner join employment on `FinalCrimeData.csv/FinalCrimeData`.State = `employment.csv/employment`.state_name
                    where `employment.csv/employment`.year = `FinalCrimeData.csv/FinalCrimeData`.Year
                    group by `FinalCrimeData.csv/FinalCrimeData`.State ,`FinalCrimeData.csv/FinalCrimeData`.Year 
                    order by `FinalCrimeData.csv/FinalCrimeData`.State ,`FinalCrimeData.csv/FinalCrimeData`.Year  "    
                    ) 
        
        # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
        tdf2 = tdf %>% group_by(state_name) %>% summarize(window_avg_crime = mean(ViolentCrime))
        dplyr::inner_join(tdf, tdf2, by = "state_name") 
    })
    
    
    output$barchartData1 <- renderDataTable({DT::datatable(df2(),
                                                           rownames = FALSE,
                                                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$barchartPlot1 <- renderPlot({ggplot(df2(), aes(x=as.character(year), y=ViolentCrime)) +
            scale_y_continuous(labels = scales::comma) + # no scientific notation
            theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
            theme(axis.text.y=element_text(size=12, hjust=0.5)) +
            geom_bar(stat = "identity") + 
            facet_wrap(~state_name, ncol=1) + 
            coord_flip() + 
            # Add sum_sales, and (sum_sales - window_avg_sales) label.
            geom_text(mapping=aes(x=as.character(year), y=ViolentCrime, label=round(ViolentCrime)),colour="black", hjust=-.75) +
            geom_text(mapping=aes(x=as.character(year), y=ViolentCrime, label=round(ViolentCrime - window_avg_crime)),colour="blue", hjust=-2.5) +
            # Add reference line with a label.
            geom_hline(aes(yintercept = round(window_avg_crime)), color="red") +
            geom_text(aes( -1, window_avg_crime, label = window_avg_crime, vjust = -.5, hjust = -.25), color="red")
    })

    # End Barchart1 Tab ___________________________________________________________
    
    # Begin Boxplot Tab ------------------------------------------------------------------
    df3 <- eventReactive(input$click3, {
      print("Getting from data.world")
      tdf3 = query(conn,dataset="colinjianingxie/s-17-dv-final-project",type="sql",
                   query="Select `employment.csv/employment`.`Gross.State.Product` as GSP, `employment.csv/employment`.state_name as State, `employment.csv/employment`.year as Year
from employment 
                   group by `employment.csv/employment`.`Gross.State.Product` , `employment.csv/employment`.state_name , `employment.csv/employment`.year
                   order by `employment.csv/employment`.`Gross.State.Product` , `employment.csv/employment`.state_name , `employment.csv/employment`.year")
      
      
    })
    
    
    output$boxplotData1 <- renderDataTable({DT::datatable(df3(),
                                                           rownames = FALSE,
                                                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$boxplotPlot1 <- renderPlotly(
      
      {
        
        p <-ggplot(df3(),(aes(x=State,y=GSP,text=Year)))+geom_boxplot()+geom_point()+
          theme(axis.text.x=element_text(angle=90,size=10,vjust=0.5))+
          ggtitle("Boxplot of State vs GSP over 2010-2014")
          
        ggplotly(p)
        
      
        
        
      })
    
    # End Boxplot Tab ___________________________________________________________
    
    # Begin Histogram Tab ------------------------------------------------------------------
    df4 <- eventReactive(input$click4, {
      print("Getting from data.world")
      tdf3 = query(conn,dataset="colinjianingxie/s-17-dv-final-project",type="sql",
                   query="select `employment.csv/employment`.Employment,`employment.csv/employment`.state_name as state,`employment.csv/employment`.year
                   from employment
                   group by `employment.csv/employment`.Employment,`employment.csv/employment`.state_name ,`employment.csv/employment`.year
                   order by `employment.csv/employment`.Employment,`employment.csv/employment`.state_name ,`employment.csv/employment`.year")
                   
      
    })
    
    
    output$histogramdata1 <- renderDataTable({DT::datatable(df4(),
                                                           rownames = FALSE,
                                                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$histogramPlot1 <- renderPlotly({
      p <-ggplot(df4(),aes(Employment))+geom_histogram(breaks=c(seq(280000,17400000,200000))) +
      scale_x_continuous(labels = scales::comma) + 
        theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
        facet_wrap(~year, ncol=1)+
        ggtitle("Histogram of Employment from 2010-2014 with bin sizes of 200000")
      ggplotly(p)})
    
    # End Histogram Tab ___________________________________________________________
    
    
    # Begin ScatterPlot Tab ------------------------------------------------------------------
    df5 <- eventReactive(input$click5, {
      print("Getting from data.world")
      tdf3 = query(conn,dataset="colinjianingxie/s-17-dv-final-project",type="sql",
                   query="select `employment.csv/employment`.`Food.Insecure`as FoodInsecurity, `FinalCrimeData.csv/FinalCrimeData`.`Violent Crime rate` as ViolentCrimeRate, `FinalCrimeData.csv/FinalCrimeData`.State, `FinalCrimeData.csv/FinalCrimeData`.Year
from FinalCrimeData left inner join employment on `FinalCrimeData.csv/FinalCrimeData`.State = `employment.csv/employment`.state_name
                   where `employment.csv/employment`.year = `FinalCrimeData.csv/FinalCrimeData`.Year 
                   group by  `employment.csv/employment`.`Food.Insecure`, `FinalCrimeData.csv/FinalCrimeData`.`Violent Crime rate` , `FinalCrimeData.csv/FinalCrimeData`.State, `FinalCrimeData.csv/FinalCrimeData`.Year
                   order by  `employment.csv/employment`.`Food.Insecure`, `FinalCrimeData.csv/FinalCrimeData`.`Violent Crime rate` , `FinalCrimeData.csv/FinalCrimeData`.State, `FinalCrimeData.csv/FinalCrimeData`.Year
                   ")
      
      
    })
    
    
    output$scatterplotdata1 <- renderDataTable({DT::datatable(df5(),
                                                            rownames = FALSE,
                                                            extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$scatterPlot1 <- renderPlotly({p <- ggplot(df5()) + 
      labs(x="Violent Crime Rate",y="Food Insecurity Rate")+
        theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
        theme(axis.text.y=element_text(size=16, hjust=0.5)) +
        geom_point(aes(x=ViolentCrimeRate, y=FoodInsecurity, colour=State), size=2)+
        facet_wrap(~Year,ncol=1)+
        ggtitle("ScatterPlot of Violent crime rate vs Food Insecurity Rate over 2010-2014")
        ggplotly(p)}
      
      )
      
    
    # End Scatterplot Tab ___________________________________________________________
    
    
    # Begin Cross Tab ------------------------------------------------------------------
    df6 <- eventReactive(input$click6, {
      print("Getting from data.world")
      tdf3 = query(conn,dataset="colinjianingxie/s-17-dv-final-project",type="sql",
                   query="select `FinalCrimeData.csv/FinalCrimeData`.Year ,`FinalCrimeData.csv/FinalCrimeData`.State, sum(`FinalCrimeData.csv/FinalCrimeData`.`Violent crime total`)/sum(`employment.csv/employment`.`Employment`) as CrimetoEmploymentRatio, sum(`employment.csv/employment`.`Employment`) as Employment,sum(`FinalCrimeData.csv/FinalCrimeData`.`Violent crime total`) as ViolentCrime,

                   case
                   when sum(`FinalCrimeData.csv/FinalCrimeData`.`Violent crime total`)/sum(`employment.csv/employment`.`Employment`) < ? then '03 Low'
                   when sum(`FinalCrimeData.csv/FinalCrimeData`.`Violent crime total`)/sum(`employment.csv/employment`.`Employment`) < ? then '02 Medium'
                   else '01 High'
                   end AS CrimetoEmploymentKPI                
                   
                   from  FinalCrimeData left inner join employment on `FinalCrimeData.csv/FinalCrimeData`.State = `employment.csv/employment`.state_name
                   where `employment.csv/employment`.year = `FinalCrimeData.csv/FinalCrimeData`.Year
                   group by `FinalCrimeData.csv/FinalCrimeData`.Year ,`FinalCrimeData.csv/FinalCrimeData`.State,`FinalCrimeData.csv/FinalCrimeData`.`Violent crime total`,`employment.csv/employment`.`Employment` 
                   order by `FinalCrimeData.csv/FinalCrimeData`.Year ,`FinalCrimeData.csv/FinalCrimeData`.State,`FinalCrimeData.csv/FinalCrimeData`.`Violent crime total`,`employment.csv/employment`.`Employment` ",
                   queryParameters = list(KPI_Low(), KPI_Medium())
                   
                   )
      
      
    })
    
    
    output$plotdata1 <- renderDataTable({DT::datatable(df6(),
                                                              rownames = FALSE,
                                                              extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$plot1 <- renderPlot({ggplot(df6()) + 
        theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
        theme(axis.text.y=element_text(size=16, hjust=0.5)) +
        geom_text(aes(x=Year, y=State, label=ViolentCrime), size=6) +
        geom_tile(aes(x=Year, y=State, fill=CrimetoEmploymentKPI), alpha=0.50)+
        ggtitle("CrossTab of ViolentCrime/Employment")
      })
    
    
    # End Cross Tab ___________________________________________________________
    
    
    
})
