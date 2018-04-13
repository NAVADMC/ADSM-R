
#Define UI for application

ui <- fluidPage(
  
  # Application title
  titlePanel(title=div(img(src="USDA_ht100px.jpg"),"ADSM: Scenario Comparison")),
  
      sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("scenario_variable", "Scenarios to compare:", c(names(dirpath))),
      tags$style(type="text/css", "textarea {width:100%}") ,
      tags$textarea(id="my_textarea", rows=10,placeholder =  "Comment...", value=""),
      
      # Button
      radioButtons('format', 'Document format', c('Word', 'HTML'), inline = TRUE),
      
      downloadButton('Report', "Generate report")
    ),
    
    mainPanel( 
      
      tabsetPanel(
        
        tabPanel("Outbreak Characteristics", 
                  
            plotlyOutput('myplot3'),plotlyOutput('myplot4'),
                          plotlyOutput('myplot5'),plotlyOutput('myplot6'),plotlyOutput('myplot6.5')
            ),
            
    
        tabPanel("Epidemic Spread",
        leafletOutput('map'),br(),leafletOutput('map2'),
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 100, left = 20, right = 20, bottom = 20,
                      width = 400, height = 20,
                      sliderInput("week",
                                  "Weeks in epidemic (Upper map):",
                                  min = min(MapMaster$Week),
                                  max = max(MapMaster$Week),
                                  value = 1,
                                  animate=animationOptions())),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 100, left = 20, right = 20, bottom = 20,
                                    width = 400, height = 20,
                      plotlyOutput('myplot1', height= 350, width = 400)),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 100, left = 20, right = 20, bottom = 20,
                      width = 400, height = 20,
                        sliderInput("Day",
                                  "Days in epidemic (Lower map):",
                                  min = min(MapMaster$Day),
                                  max = max(MapMaster$Day),
                                  value = 1,
                                  animate=animationOptions()))
        ),
        
                 
             
        tabPanel("Herd Level Analyses", 
                 plotOutput('myplot7'),
                 plotOutput('myplot7.5'),
                 plotOutput('myplot8')

                 )
            )
        )
      )
)


  
