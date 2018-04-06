# To run the shiny app you need to install the following pacakges
# install.packages(c("tidyverse","shiny","sp","rgdal","RColorBrewer"))

library(shiny)
rm(list=ls())
source("HelperFunctions.R")
yorkshireCrime2 <- getData()
yorkshireCrime2 <- yorkshireCrime2 %>% filter(nrTown >10)
yorkshireCrime2 <- countCrimes(yorkshireCrime2)

territories <-unique(yorkshireCrime2$town)
crime_type <- unique(yorkshireCrime2$Crime.type)
LOC <- unique(yorkshireCrime2$Last.outcome.category)


border <- getBorder()

ui <- fluidPage(
  # Application title
  titlePanel("Crime data analysis"), # comma before the next element (sidebarLayout())
  
  # Sidebar with two controls ("widgets"): interval type, and confidence/prediction level
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "territories", 
                  label = "Territory", 
                  choices = territories,
                  selected ="Sheffield"
      ),
      selectInput(inputId = "loc", 
                  label = "Last Outome Category", 
                  choices = 
                    c( "All",
                       LOC
                    ),selected = "All"
      ),
      selectInput(inputId = "crime_type", 
                  label = "Crime Type:", 
                  choices = 
                    c( "All",
                       crime_type
                    ), selected = "All"
                  
      ),
      actionButton(inputId = "action",
                   label = "Search"), 
      # Second control: a widget to specify the interval value
      tags$hr(),
      numericInput(inputId = "toplist", 
                   label = "Nr top locations to display.", 
                   min = 10, max = 100,
                   value = 20, # the default value
                   step = 10),
      tags$hr(),
      tags$h3("To update the data:"),
      tags$p("To update the data please replace the",
"file 2017-11-south-yorkshire-street.csv"
,"with a file with the same format and name containing the new data.",
"Then do restart the application.")
    ), 
    
    # Results to be displayed in the app
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table",
                           h4("Crime hotspots list"),
                           verbatimTextOutput("AllVars"),# Subtitle to appear 
                           tableOutput("topLocationTable") # Display the correlation matrix
                  ),
                  tabPanel("Map",
                           plotOutput('mapdefault',width = "100%", height="700px")
                           )
      )                  
    )
    
  )
)

# Define the "server": where the computations are done/plots generated etc.
# given the input values (input$interval, input$lvl, input$method) from the user interface
server <- function(input, output, session) {
  observe({
    territories <- input$territories
    loc <- input$loc
    updateSelectInput(session, "crime_type",
                      label = paste("Crime Types available for LOC :",loc),
                      choices = c("All",getAvailableCrimeTypes(yorkshireCrime2,territories,loc)),
                      selected = "All"
    )
  })
  
  buttonaction <- eventReactive(input$action, {
    c(territories=input$territories,loc=input$loc,crime_type=input$crime_type, nrRow = input$toplist)
  })
  
  output$AllVars <- renderText(
    paste(buttonaction())
  )
  
  output$topLocationTable <- renderTable({
    xx <- buttonaction()
    getTable(xx["territories"],xx["crime_type"],xx["loc"],xx["nrRow"] )
  })
  
  output$mapdefault <- renderPlot({
    xx <- buttonaction()
    tmp <- filterTheData(yorkshireCrime2, xx["territories"],xx["loc"],xx["crime_type"] )
    makeMap(tmp, border, xx["territories"] )
  })
  
  getTable <- function(town, crime_type, loc, nrRow){
    tmp <- filterTheData(yorkshireCrime2, townSearch = town, loc =loc, crime_type = crime_type)
    tmp <- head(tmp,n= as.integer(nrRow))
    return(data.frame(nr = 1:nrow(tmp),tmp))
  }
}
#
# Run the application 
shinyApp(ui = ui, server = server)


