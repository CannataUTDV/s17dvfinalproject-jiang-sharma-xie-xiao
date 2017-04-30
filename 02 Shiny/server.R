# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)

conn <- data.world(token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmFzaHJvb20xNCIsImlzcyI6ImFnZW50OmFzaHJvb20xNDo6YzU2MWMyYTYtNzZhYS00ZTNmLWI5MjAtOTBlZmQzYjczNDdlIiwiaWF0IjoxNDg0ODY3OTI3LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.hEBz_EXKG5iOLa6xRvGhL-M8TqWuVa4AoZ0SbDd4kULe86diYRHjE8EYV72uzwnp-oEQISCp9NPAG13QHfj0yQ")

shinyServer(function(input, output) { 
    
    # These widgets are for the Barcharts tab.
    online2 = reactive({input$rb2})
    online3 = reactive({input$rb3})
    online4 = reactive({input$rb4})
    
    # Begin Barchart1 Tab ------------------------------------------------------------------
    df2 <- eventReactive(input$click2, {
        print("Getting from data.world")
        tdf = query(conn,
                    dataset="colinjianingxie/s-17-dv-project-6", type="sql",
                    query="select state_name, year, sum(Employment) sum_employment 
                           from `US Census Data` left inner join clean on `US Census Data`.State = clean.state_name
                           group by state_name, year
                           order by state_name, year"    
                    ) 
        
        # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
        tdf2 = tdf %>% group_by(state_name) %>% summarize(window_avg_employment = mean(sum_employment))
        dplyr::inner_join(tdf, tdf2, by = "state_name") %>% dplyr::filter(year %in% c(1980, 1985, 1990, 1995, 2000))
    })
    
    
    output$barchartData1 <- renderDataTable({DT::datatable(df2(),
                                                           rownames = FALSE,
                                                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$barchartPlot1 <- renderPlot({ggplot(df2(), aes(x=as.character(year), y=sum_employment)) +
            scale_y_continuous(labels = scales::comma) + # no scientific notation
            theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
            theme(axis.text.y=element_text(size=12, hjust=0.5)) +
            geom_bar(stat = "identity") + 
            facet_wrap(~state_name, ncol=1) + 
            coord_flip() + 
            # Add sum_sales, and (sum_sales - window_avg_sales) label.
            geom_text(mapping=aes(x=as.character(year), y=sum_employment, label=round(sum_employment)),colour="black", hjust=-.5) +
            geom_text(mapping=aes(x=as.character(year), y=sum_employment, label=round(sum_employment - window_avg_employment)),colour="blue", hjust=-2) +
            # Add reference line with a label.
            geom_hline(aes(yintercept = round(window_avg_employment)), color="red") +
            geom_text(aes( -1, window_avg_employment, label = window_avg_employment, vjust = -.5, hjust = -.25), color="red")
    })

    # End Barchart1 Tab ___________________________________________________________
    
    # Begin Barchart2 Tab ------------------------------------------------------------------
    df3 <- eventReactive(input$click3, {
      print("Getting from data.world")
      tdf = query(conn,
                  dataset="colinjianingxie/s-17-dv-project-6", type="sql",
                  query="select state_name, `clean.csv/clean`.`Gross.State.Product` as GSP,`clean.csv/clean`.Population as Population
                  from `US Census Data` left inner join clean on `US Census Data`.State = clean.state_name
                  where clean.year = 2015
                  group by state_name,`clean.csv/clean`.`Gross.State.Product`,`clean.csv/clean`.Population
                  order by state_name,`clean.csv/clean`.`Gross.State.Product`,`clean.csv/clean`.`Gross.State.Product`,`clean.csv/clean`.Population"    
      ) 
      
      # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
      tdf$state_name <-(state.name[match(tdf$state_name,state.abb)])
      tdf%>% dplyr::filter(state_name %in% c( "Alabama"     ,   "Alaska"       ,  "Arizona"  ,"Arkansas"     ,  "California"   ,  "Colorado" ,     
                                              "Connecticut"   , "Delaware"     ,  "Florida"   ,    
                                              "Georgia"    ,    "Hawaii"      ,   "Idaho"    ,     
                                              "Illinois"   ,    "Indiana"   ,     "Iowa"    ,      
                                              "Kansas"     ,    "Kentucky"  ,     "Louisiana"   ,  
                                              "Maine"       ,   "Maryland"     ,  "Massachusetts" ,
                                              "Michigan"    ,   "Minnesota"    ,  "Mississippi" ,  
                                              "Missouri"    ,   "Montana"   ,     "Nebraska",      
                                              "Nevada"     ,    "New Hampshire",  "New Jersey"   , 
                                              "New Mexico"   ,  "New York"    ,   "North Carolina",
                                              "North Dakota"  , "Ohio"      ,     "Oklahoma"  ,    
                                              "Oregon"      ,   "Pennsylvania",   "Rhode Island"  ,
                                              "South Carolina", "South Dakota",   "Tennessee"    , 
                                              "Texas"      ,    "Utah"        ,   "Vermont"     ,                                                                                                                                       "Virginia"  ,     "Washington"   ,  "West Virginia" ,
                                              "Wisconsin"  ,    "Wyoming" ))
      
      
      
    })
    
    
    output$barchartData2 <- renderDataTable({DT::datatable(df3(),
                                                           rownames = FALSE,
                                                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$barchartPlot2 <- renderPlot(
      
      {
        
        ggplot(data=df3(),aes(map_id= tolower(state_name)))+
          geom_map(aes(fill=GSP),map=states_map,color="black")+
          expand_limits(x=states_map$long,y=states_map$lat)+
          coord_fixed()+
          theme(legend.position = "bottom",
                axis.ticks = element_blank(), 
                axis.title = element_blank(), 
                axis.text =  element_blank()) +
          scale_fill_gradient(low="white", high="green") +
          guides(fill = guide_colorbar(barwidth = 20, barheight = .5)) + 
          ggtitle("GSP Ratios")
        
        
      })
    
    # End Barchart2 Tab ___________________________________________________________
    
    # Begin Barchart3 Tab ------------------------------------------------------------------
    df4 <- eventReactive(input$click4, {
      print("Getting from data.world")
      tdf = query(conn,
                  dataset="colinjianingxie/s-17-dv-project-6", type="sql",
                  query="select state_name, `clean.csv/clean`.`Workers..compensation` as compensation,`US Census Data.csv/US Census Data`.`Household Income` as income, `clean.csv/clean`.`Workers..compensation`/`US Census Data.csv/US Census Data`.`Household Income` as IncomeCompensationRatio
                from `US Census Data` left inner join clean on `US Census Data`.State = clean.state_name
                  where clean.year= 2015
                  group by state_name, `clean.csv/clean`.`Workers..compensation`,`US Census Data.csv/US Census Data`.`Household Income`,`clean.csv/clean`.`Workers..compensation`/`US Census Data.csv/US Census Data`.`Household Income`
                  order by state_name, `clean.csv/clean`.`Workers..compensation`,`US Census Data.csv/US Census Data`.`Household Income`,`clean.csv/clean`.`Workers..compensation`/`US Census Data.csv/US Census Data`.`Household Income`"    
      ) 
      
      # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
      tdf$state_name <-(state.name[match(tdf$state_name,state.abb)])
      tdf%>% dplyr::filter(state_name %in% c( "Alabama"     ,   "Alaska"       ,  "Arizona"  ,"Arkansas"     ,  "California"   ,  "Colorado" ,     
                                              "Connecticut"   , "Delaware"     ,  "Florida"   ,    
                                              "Georgia"    ,    "Hawaii"      ,   "Idaho"    ,     
                                              "Illinois"   ,    "Indiana"   ,     "Iowa"    ,      
                                              "Kansas"     ,    "Kentucky"  ,     "Louisiana"   ,  
                                              "Maine"       ,   "Maryland"     ,  "Massachusetts" ,
                                              "Michigan"    ,   "Minnesota"    ,  "Mississippi" ,  
                                              "Missouri"    ,   "Montana"   ,     "Nebraska",      
                                              "Nevada"     ,    "New Hampshire",  "New Jersey"   , 
                                              "New Mexico"   ,  "New York"    ,   "North Carolina",
                                              "North Dakota"  , "Ohio"      ,     "Oklahoma"  ,    
                                              "Oregon"      ,   "Pennsylvania",   "Rhode Island"  ,
                                              "South Carolina", "South Dakota",   "Tennessee"    , 
                                              "Texas"      ,    "Utah"        ,   "Vermont"     ,                                                                                                                                       "Virginia"  ,     "Washington"   ,  "West Virginia" ,
                                              "Wisconsin"  ,    "Wyoming" ))
      
      
      
    })
    
    
    output$barchartData3 <- renderDataTable({DT::datatable(df4(),
                                                           rownames = FALSE,
                                                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$barchartPlot3 <- renderPlot(
      
      {
        
        ggplot(data=df4(),aes(map_id= tolower(state_name)))+
          geom_map(aes(fill=IncomeCompensationRatio),map=states_map,color="black")+
          expand_limits(x=states_map$long,y=states_map$lat)+
          coord_fixed()+
          theme(legend.position = "bottom",
                axis.ticks = element_blank(), 
                axis.title = element_blank(), 
                axis.text =  element_blank()) +
          scale_fill_gradient(low="white", high="red") +
          guides(fill = guide_colorbar(barwidth = 20, barheight = .5)) + 
          ggtitle("Income/Compensation Ratios")
        
      })
    
    # End Barchart3 Tab ___________________________________________________________
    
})
