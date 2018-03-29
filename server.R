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
  
  output$myplot.5 <- renderPlot({
    #Merging data for each scenario (as each iteration comes in a different CSV file)
    # COMEBACK: Need to correct the "scenario_variable" problem in the DState data (make match the user input scenario)
    p <- ggplot()
    p <- p + geom_polygon( data=Subset_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
    pDS <- p + geom_point( data=DState[(DState$day)==input$days & (DState$Scenario)%in%input$scenario_variable,c(1:7)],  #position=position_jitter(width=1, height=1),
                              aes(x=long, y=lat, colour=Dstatus,fill=Dstatus, size=.03),show.legend=T)
    
    
    pDS 
  })
  
  output$myplot.55 <- renderPlot({
    #Merging data for each scenario (as each iteration comes in a different CSV file)
    
    p <- ggplot()
    p <- p + geom_polygon( data=Subset_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
    pDS1 <- p + geom_point( data=MapMaster[(MapMaster$Week)==input$week,],  
                           aes(x=Lon, y=Lat, colour=Status,fill=Status, size=.03),show.legend=T)
    
    pDS1 
    
  })
  
  output$myplot.56 <- renderPlot({
    #Merging data for each scenario (as each iteration comes in a different CSV file)
    
    p <- ggplot()
    p <- p + geom_polygon( data=Subset_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
    pDS2 <- p + geom_point( data=MapMaster[(MapMaster$Day)==input$day,],  
                            aes(x=Lon, y=Lat, colour=Status,fill=Status, size=.03),show.legend=T)
    
    pDS2
    
  })

  output$myplot1 <- renderPlot({
    #Merging data for each scenario (as each iteration comes in a different CSV file)
    
    p <- ggplot()
    p <- p + geom_polygon( data=Subset_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
    pLate <- p + geom_jitter( data=daily_dat$exposures[(daily_dat$exposures$Scenario)%in% input$scenario_variable,], position=position_jitter(width=1, height=1), 
                              aes(x=S_Lon, y=S_Lat, colour=S_ProductionType))
    
    pLate 
  })
  
  
  output$myplot2 <- renderPlot({
    #Merging data for each scenario (as each iteration comes in a different CSV file)
    
    p <- ggplot()
    p <- p + geom_polygon( data=Subset_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
    pLate <- p + geom_jitter( data=daily_dat$exposures[(daily_dat$exposures$Scenario)%in% input$scenario_variable,], position=position_jitter(width=1, height=1), 
                              aes(x=R_Lon, y=R_Lat, colour=R_ProductionType))
    
    pLate 
  })
  
  
  output$myplot3 <- renderPlotly({

    
    EPI<-ggplot(dbtable$epiCur[(dbtable$epiCur$Scenario) %in% input$scenario_variable,],aes(x=day, y=SuminfnU, group=Scenario, colour=Scenario))+
      geom_line(size=1)+
      labs(title="Epidemic Curve",x="Time (days)", y = "New Infected Units")
    
    gg<-ggplotly(EPI)
    layout(gg, dragmode = "pan")
    
  })
  
  
  output$myplot4 <- renderPlotly({
        p1<-qplot(Scenario, diseaseDuration, geom=c("boxplot"), data=dbtable$outDur[(dbtable$outDur$Scenario)%in% input$scenario_variable,],
              fill=Scenario, main="Outbreak duration",
              xlab="", ylab="Duration (days)")
    gg1<-ggplotly(p1)
    layout(gg1, dragmode = "pan")
    
    
    
  })
  
  
  
  output$myplot5 <- renderPlotly({
       #Total Farms in outbreak
    p4<-qplot(Scenario, infcU, geom=c("boxplot"), data=dbtable$tFarmOut[(dbtable$tFarmOut$Scenario)%in% input$scenario_variable,],
              fill=Scenario, main="Farms in outbreak",
              xlab="", ylab="Units")
    gg2<-ggplotly(p4)
    layout(gg2, dragmode = "pan")
    
  })
  
  
  
  output$myplot6 <- renderPlotly({
        #Farms depopulated
    p6<-qplot(Scenario, descU, geom=c("boxplot"), data=dbtable$farmDepo[(dbtable$farmDepo$Scenario)%in% input$scenario_variable,],
              fill=Scenario, main="Farms depopulated",
              xlab="", ylab="Units")
    gg3<-ggplotly(p6)
    layout(gg3, dragmode = "pan")
    
  })
  
  # output$myplot6.5 <- renderPlotly({
  #   
  #   p6.5<-qplot(Scenario, vaccU, geom=c("boxplot"), data=VaccUnit[(VaccUnit$Scenario)%in% input$scenario_variable,],
  #       fill=Scenario, main="Units Vaccinated",
  #       xlab="", ylab="Farms vaccinated")
  #   
  #   gg4<-ggplotly(p6.5)
  #   layout(gg4, dragmode = "pan")
  # 
  # })
  
  output$myplot7 <- renderPlot({
    
    bp<-ggplot(daily_compare[(daily_compare$Scenario)%in% input$scenario_variable,], aes(x="", y=Freq, fill=Veterinary_action))+
      geom_bar(width = 1, stat = "identity")+
      facet_grid(facets=.~Scenario)+
      xlab("") 
    bp
    
    
  })
  
  
  output$myplot8 <- renderPlot({
    
    
    
   par(mfrow=c(1,4), mar=c(0,0,5,0))

    for (i in 1:length(daily_graphdat)){
      plot(daily_graphdat[[i]],layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main=names(dirpath)[i])
    }
    
    # plot(net2,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main="Early")
    # plot(net1,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main="Late")
    # plot(net4,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main="Vx Early")
    # plot(net3,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main="Vx Late")
    

  })
  
  
  # Downloadable report ----
 
  
  #  output$downloadReport <- downloadHandler(
  #    
  #   filename = function() {
  #     paste(input$scenario_variable, sep = '.', switch(
  #       input$format, PDF= 'pdf',Word='docx'))
  #   },
  #   content = function(file) {
  #     src<-normalizePath('report.Rmd')
  # 
  # # temporarily switch to the temp dir, in case you do not have write
  # # permission to the current working directory
  #     owd <- setwd("~/CEAH/ADSM_Missy/ADSM_Shiny")#tempdir())
  #     on.exit(setwd(owd))
  #     #browser()
  #     file.copy(src, 'report.Rmd', overwrite = TRUE)
  # 
  #     library(rmarkdown)
  #     out <- render('report.Rmd', switch(
  #       input$format,
  #       PDF = pdf_document(), Word = word_document()
  #     ))
  #     file.rename(out, file)
  #     
  #   }
  # )
  
  
}

