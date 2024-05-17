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
library(DT)


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
               numericInput("pop", "Customer Population Size", min = 0, value = 1000),
               sliderInput("cPrice", "Competitor Price", min = 0, max = 10, value = 3, step = .25),
            )
      ),
      column(4,
             #plotlyOutput("demand3D_plot")
             plotOutput("demand_plot")
      ),
      column(4,
             verbatimTextOutput("lm_summary")
             #plotlyOutput("demand3D_plot")
             )
    ),
    #fluidRow(
    #  column(4,
    #         wellPanel(
    #           sliderInput("cPrice", "Competitor Price", min = 0, max = 10, value = 3, step = .25),
    #         )
    #  ),
    #  column(8,
    #         plotOutput("demand_plot")
    #         )
    #),
    fluidRow(
      column(4,
             wellPanel(
               sliderInput("price", "Price", min = 0, max = 10, value = 3, step = .25),
               numericInput("var1", "Variable Cost of Product 1", min = 0, value = 10),
               numericInput("fix1", "Fixed Cost (Overhead) of Product 1", min = 0, value = 1000),
               numericInput("var2", "Variable Cost of Product 2", min = 0, value = 10),
               numericInput("fix2", "Fixed Cost (Overhead) of Product 2", min = 0, value = 1000)
             )
      ),
      column(4,
             plotOutput("profit_plot"),
      ),
      column(4,
             verbatimTextOutput("nash_output")
             #plotlyOutput("profit_opt_line")
             )
    ),
    #fluidRow(
    #  column(4,
    #         wellPanel(
    #           numericInput("var2", "Variable Cost of Product 1", min = 0, value = 10),
    #           numericInput("fix2", "Fixed Cost (Overhead) of Product 1", min = 0, value = 1000)
    #         )
    #  ),
    #  column(4,
    #         #plotlyOutput("nash_plot")
    #  )
    #),

    #fluidRow(
    #  column(12,
             #verbatimTextOutput("nash_output")
    #         )
    #)
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

  observeEvent(input$wtpProd1, {
    if(input$wtpProd1 == "FILE NEEDS TO BE UPLOADED"){
      return(NULL)
    } else {
      updateSelectInput(inputId = "wtpProd2", choices = setdiff(names(userData()), input$wtpProd1))
    }
  })

  userCleanData <- reactive({
    if(is.null(userWTP2())){
      return(NULL)
    }
    quantityCreation_duo(userData(), input$wtpProd1, input$wtpProd2)
  })

  output$cleanContents <- renderDataTable(userCleanData(), options = list(pageLength = 5))

  userType <- reactive(input$regressType)
  userPop <- reactive(input$pop)
  userSample <- reactive(nrow(userCleanData()))

  output$demand3D_plot <- renderPlotly({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandPlot3D(data = userCleanData(), type = userType(), first_or_second = 1, population = userPop(), sample = userSample())
  })

  observeEvent(input$wtpProd2, {
    #browser()
    if(is.null(userWTP2())){
      return(NULL)
    }
    updateSliderInput(inputId = "cPrice", label = paste(input$wtpProd2, "(competitor) price"),  min = min(userWTP2()), max = max(userWTP2()), value = mean(userWTP2()))
  })

  userCPrice <- reactive(input$cPrice)

  output$demand_plot <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandPlotDuo(competitor_price = userCPrice(), data = userCleanData(), type = userType(), first_or_second = 1, population = userPop(), sample = userSample())
  })

  output$lm_summary <- renderPrint({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandSummaryDuo(data = userCleanData(), type = userType(), first_or_second = 1)
  })


  userPrice <- reactive(input$price)
  userVar1 <- reactive(input$var1)
  userFix1 <- reactive(input$fix1)

  observeEvent(input$wtpProd1, {
    #browser()
    if(is.null(userWTP1())){
      return(NULL)
    }
    updateSliderInput(inputId = "price", label = paste(input$wtpProd1, "price"),  min = min(userWTP1()), max = max(userWTP1()), value = median(userWTP1()))
  })

  observeEvent(input$wtpProd1, {
    #browser()
    if(is.null(userWTP1())){
      return(NULL)
    }
    updateNumericInput(inputId = "var1", label = paste(input$wtpProd1, "Variable Cost"),  min = 0, value = roundLog(median(userWTP1()) * .4))
    updateNumericInput(inputId = "fix1", label = paste(input$wtpProd1, "Fixed Cost"),  min = 0, value = roundLog(median(userWTP1()) * userPop() * .1))
  })

  output$profit_plot <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitPlotDuo(price1 = userPrice(), price2 = userCPrice(), data = userCleanData(), type = userType(), first_or_second = 1,  var = userVar1(), fix = userFix1(), population = userPop(), sample = userSample())
  })

  output$profit_opt_line <- renderPlotly({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitOptLine(data = userCleanData(), type = userType(), first_or_second = 1, var = userVar1(), fix = userFix1(), population = userPop(), sample = userSample())
  })

  userVar2 <- reactive(input$var2)
  userFix2 <- reactive(input$fix2)

  observeEvent(input$wtpProd2, {
    #browser()
    if(is.null(userWTP2())){
      return(NULL)
    }
    updateNumericInput(inputId = "var2", label = paste(input$wtpProd2, "Variable Cost"),  min = 0, value = roundLog(median(userWTP2()) * .4))
    updateNumericInput(inputId = "fix2", label = paste(input$wtpProd2, "Fixed Cost"),  min = 0, value = roundLog(median(userWTP2()) * userPop() * .1))
  })

  output$nash_plot <- renderPlotly({
    if(is.null(userCleanData())){
      return(NULL)
    }
    nash(data = userCleanData(), type = userType(), var1 = userVar1(), fix1 = userFix1(), var2 = userVar2(), fix2 = userFix2(), population = userPop(), sample = userSample())
  })

  output$nash_output <- renderPrint({
    if(is.null(userCleanData())){
      return(NULL)
    }
    competitionSolve(data = userCleanData(), type = userType(), first_or_second = 1, variable1 = userVar1(), fixed1 = userFix1(), variable2 = userVar2(), fixed2 = userFix2(), population = userPop(), sample = userSample())
  })

}

# Run the application
shinyApp(ui = ui, server = server)

#cRock <- quantityCreation_duo(rock, "area", "peri")
#profitOptLine(data = cRock, type = "Linear", 1, 1, 1, 1, 1)
#nash(data = cRock, type = "Linear", 1, 1, 1, 1, 1, 1)

# variable should be 40% of median price?
# fixed should be median price



