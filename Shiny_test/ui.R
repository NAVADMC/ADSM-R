#
#Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ADSM: Scenario Comparison"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
            checkboxGroupInput("scenario_variable", "Scenarios to compare:", c(names(dirpath))),
      
     # checkboxGroupInput("output_analyses", "Output to compare:", c( "Epidemic curve","Outbreak duration","Farms in outbreak","Farms depopulated","Farm type distribution","Veterinary action","Transmission network")),
      
     
      
      tags$style(type="text/css", "textarea {width:100%}") ,
      tags$textarea(id="my_textarea", rows=10,placeholder =  "Comment...", value="")
      
      # # Button
      # radioButtons('format', 'Document format', c('PDF', 'Word'),inline = TRUE),
      #              
      # downloadButton('downloadReport')
      
      #downloadButton("downloadData", "Download")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel( column(width = 12,
      
      # Output: Table summarizing the values entered ---
      tabsetPanel(
        # tabPanel("Outbreak characteristics", plotlyOutput('myplot3'),plotlyOutput('myplot4'),
        #          plotlyOutput('myplot5'),plotlyOutput('myplot6'),plotlyOutput('myplot6.5')),
        
        tabPanel("Outbreak characteristics", plotlyOutput('myplot3'),plotlyOutput('myplot4'),
                 plotlyOutput('myplot5'),plotlyOutput('myplot6')),
        
        tabPanel("Spread of Epidemic", plotOutput("myplot.5"), 
                 sidebarPanel(
          sliderInput("days",
                    "Days in epidemic:",
                    min = 1,
                    max = 7,
                    value = 1,
                    animate=animationOptions()))),
        
        tabPanel("Spread of Epidemic1", plotOutput("myplot.55"), plotOutput("myplot.56"), 
                 sidebarPanel(
        sliderInput("week",
                    "Weeks in epidemic:",
                    min = 1,
                    max = 15,
                    value = 1,
                    animate=animationOptions()),
        sliderInput("day",
                    "Days in epidemic:",
                    min = 0,
                    max = 111,
                    value = 1,
                    animate=animationOptions()))),
        
        
        tabPanel("Spatial analyses", plotOutput('myplot1'),plotOutput('myplot2')),
        tabPanel("Herd level analyses", plotOutput('myplot7'),plotOutput('myplot8')))
      
    ))))
