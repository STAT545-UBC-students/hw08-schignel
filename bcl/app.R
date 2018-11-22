# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Bring in the data
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

# Define the user interface
ui <- fluidPage(theme = "bootstrap.css", # Add CSS Styling
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      h2("Filters"),
      "Use the options below to filter the data and find your preffered drink!
      You can download the filtered dataset by pressing the 'Download Data' button.",
      br(), br(),
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      checkboxGroupInput("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      sliderInput("sweetnessInput", "Product sweetness", 0, 10, c(0, 10)),
      uiOutput("countryOutput"),
      br(), br(),
      h4("Note:"), 
      "This app is an augmented version of the app produced by Dean Attali (https://deanattali.com), and public domain data from OpenDataBC.ca. All images used are in the public domain."
    ),
    mainPanel(
      downloadButton("downloadData", "Download Data"),
      img(src='BCL_big.png', align = "right"),
      plotOutput("coolplot"),
      br(), br(), # Adding line breaks
      br(), br(),
      DTOutput("results"), # use DT alias function to avoid conflict with shiny::DataTableOutput()
      br(), br(),
      img(src='liquor_bar.png', align = "right")
    )
  )
)
# Define the server settings
server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  # Set up data set to be filtered based on widget
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput,
             Sweetness == input$sweetnessInput
      )
  })
  # Set up output plot
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram(binwidth = 0.2, color = "dark blue") +
      ylab("Count") +
      xlab("Alcohol Content") +
      theme_bw()
  })
  # Set up output table
  output$results <- renderDT({ # use DT alias function to avoid conflict with shiny::renderDataTable()
    filtered()
  })
  # Set up download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
