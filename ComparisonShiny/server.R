#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  
  # Reactive expression for the data subsetted to what the user selected
   filteredDataW <- reactive({
    MapMaster[MapMaster$Type=="Infection" & MapMaster$Week ==input$week,]
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = mapWorld) %>% addTiles() %>%
      addPolygons(weight = 1, fillColor = "white", color="grey") %>%
      fitBounds(~min(MapMaster$Lon), ~min(MapMaster$Lat), ~max(MapMaster$Lon), ~max(MapMaster$Lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    leafletProxy("map", data = filteredDataW()) %>%
      addCircleMarkers(~Lon, ~Lat, popup=content, radius= ~ifelse(UnitSize,6,10), color = ~palInf(Type), stroke=F, fillOpacity = 0.1) 
  })
  
  
  output$myplot1 <- renderPlotly({
    pM<-qplot(Type, geom=c("bar"), data=MapMaster[(MapMaster$Week)==input$week,], 
              fill=Type, main="Veterinary Action")+
      theme_void()
    ggM<-ggplotly(pM)
    layout(ggM, dragmode = "pan")
    
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filteredDataD <- reactive({
    MapMaster[MapMaster$Type=="Infection" & MapMaster$Day ==input$Day,]
  })
  
  output$map2 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = mapWorld) %>% addTiles() %>%
      addPolygons(weight = 1, fillColor = "white", color="grey") %>%
      fitBounds(~min(MapMaster$Lon), ~min(MapMaster$Lat), ~max(MapMaster$Lon), ~max(MapMaster$Lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    leafletProxy("map2", data = filteredDataD()) %>%
      addCircleMarkers(~Lon, ~Lat, popup=content, radius= ~ifelse(UnitSize,6,10), color = ~palInf(Type), stroke=F, fillOpacity = 0.1) 
  })
  
  output$myplot3 <- renderPlotly({

    
    EPI<-ggplot(EPICURVE[(EPICURVE$Scenario) %in% input$scenario_variable,],aes(x=day, y=sumInfnU, group=Scenario, colour=Scenario))+
      geom_line(size=1)+
      labs(title="Epidemic Curve",x="Time (days)", y = "New Infected Units")
    
    gg<-ggplotly(EPI)
    layout(gg, dragmode = "pan")
    
  })
  
  
  output$myplot4 <- renderPlotly({
        p1<-qplot(Scenario, diseaseDuration, geom=c("boxplot"), data=OUTBREAKDUR[(OUTBREAKDUR$Scenario)%in% input$scenario_variable,],
              fill=Scenario, main="Outbreak duration",
              xlab="", ylab="Duration (days)")
    gg1<-ggplotly(p1)
    layout(gg1, dragmode = "pan")
    
    
    
  })
  
  
  
  output$myplot5 <- renderPlotly({
       #Total Farms in outbreak
    p4<-qplot(Scenario, infcU, geom=c("boxplot"), data=TFO[(TFO$Scenario)%in% input$scenario_variable,],
              fill=Scenario, main="Farms in outbreak",
              xlab="", ylab="Units")
    gg2<-ggplotly(p4)
    layout(gg2, dragmode = "pan")
    
  })
  
  
  
  output$myplot6 <- renderPlotly({
        #Farms depopulated
    p6<-qplot(Scenario, descU, geom=c("boxplot"), data=FD[(FD$Scenario)%in% input$scenario_variable,],
              fill=Scenario, main="Farms depopulated",
              xlab="", ylab="Units")
    gg3<-ggplotly(p6)
    layout(gg3, dragmode = "pan")
    
  })
  
  output$myplot6.5 <- renderPlotly({
    
    p6.5<-qplot(Scenario, vaccU, geom=c("boxplot"), data=VaccUnit[(VaccUnit$Scenario)%in% input$scenario_variable,],
        fill=Scenario, main="Units Vaccinated",
        xlab="", ylab="Farms vaccinated")
    
    gg4<-ggplotly(p6.5)
    layout(gg4, dragmode = "pan")
  
  })
  
  output$myplot7 <- renderPlot({
    
    bp<-ggplot(Ty[(Ty$Scenario)%in% input$scenario_variable,], aes(x="", y=Freq, fill=Veterinary_action))+
      geom_bar(width = 1, stat = "identity")+
      facet_grid(facets=.~Scenario)+
      xlab("") 
    bp
    
    
  })
  
  
  output$myplot8 <- renderPlot({
    
    
    
   par(mfrow=c(1,4), mar=c(0,0,5,0))


    plot(net2,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main="Early")
    plot(net1,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main="Late")
    plot(net4,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main="Vx Early")
    plot(net3,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main="Vx Late")
    

  })
  
  
  # Downloadable report ----
 
  
   output$Report <- downloadHandler(
     filename = function () {
       switch(input$format, PDF= 'pdf',Word='docx', HTML= 'html')
         },
     content = function(file) {
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path("~/CEAH/ADSM_Missy/ADSM_Shiny/ADSM_3.6.18", "Report.Rmd")
       file.copy("Report.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(scenario_variable = input$scenario_variable, 
                      Day = input$Day, 
                      week = input$week)
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
}
     
  #   filename = function() {
  #     switch(
  #       input$format, PDF= 'pdf',Word='docx', HTML= 'html')
  #   },
  #   content = function(file) {
  #     src<-normalizePath('Report.Rmd')
  # 
  # # temporarily switch to the temp dir, in case you do not have write
  # # permission to the current working directory
  #     owd <- setwd("~/CEAH/ADSM_Missy/ADSM_Shiny/ADSM_3.6.18")#tempdir())
  #     on.exit(setwd(owd))
  #     #browser()
  #     file.copy(src, 'Report.Rmd', overwrite = TRUE)
  #     
  #     params<-list(scenario_variable=input$scenario_variable, 
  #                    Day=input$Day, week=input$week)
  # 
  #  
  #     out <- rmarkdown::render('Report.Rmd', 
  #                   params=params,
  #                   switch(input$format,
  #                          PDF = pdf_document(), Word = word_document(), HTML = html_document()
  #                   )
  #     )
  #           file.rename(out, file)
  #         }
  
