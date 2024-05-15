#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#devtools::install_git("https://github.com/probably-jaden/Pricer", force = TRUE, upgrade = "always")

library(shiny)
library(plotly)
library(Pricer)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Duopoly"),

    # Sidebar layout with input and output definitions ----
    #fluidRow(
    sidebarLayout(
      # Sidebar panel for inputs ----
      #column(4,
      sidebarPanel(

        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),

        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),

        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
      ),
      mainPanel(
        dataTableOutput("contents")
      )
    ),
    fluidRow(
      column(4,
             wellPanel(
               selectInput("wtpProd1", "Product 1 Willingness to Pay Column", choices = c("FILE NEEDS TO BE UPLOADED")),
               selectInput("wtpProd2", "Product 2 Willingness to Pay Column", choices = c("FILE NEEDS TO BE UPLOADED")),
               selectInput("regressType", "Regression Transformation", choices = c("Linear", "Exponential")),
               numericInput("pop", "Customer Population Size", min = 0, value = 1000)
            )
      ),
      column(8,
             plotlyOutput("demand3D_plot"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  userData <- reactive({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    df
  })

  output$contents <- renderDataTable(userData(), options = list(pageLength = 5))

  observeEvent(input$file1, {
    updateSelectInput(inputId = "wtpProd1", choices = names(userData()))
  })

  userWTP1 <- reactive({
    if(input$wtpProd1 == "FILE NEEDS TO BE UPLOADED"){
      return(NULL)
    } else {
      return(userData()[[input$wtpProd1]])
    }
  })

  observeEvent(input$wtpProd1, {
    if(input$wtpProd1 == "FILE NEEDS TO BE UPLOADED"){
      return(NULL)
    } else {
      updateSelectInput(inputId = "wtpProd2", choices = setdiff(names(userData()), input$wtpProd1))
    }
  })

  userWTP2 <- reactive({
    if(input$wtpProd2 == "FILE NEEDS TO BE UPLOADED"){
      return(NULL)
    } else {
      return(userData()[[input$wtpProd2]])
    }
  })

  userCleanData <- reactive({
    if(is.null(userWTP2())){
      return(NULL)
    }
    quantityCreation_duo(userData(), input$wtpProd1, input$wtpProd2)
  })

  output$cleanContents <- renderDataTable(userCleanData(), options = list(pageLength = 5))

  output$demand3D_plot <- renderPlotly({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandPlot3D(data = userCleanData(), type = input$regressType, first_or_second = 1, population = input$pop, sample = nrow(userCleanData()))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

