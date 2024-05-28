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
      radioButtons("quote", "Quoxte",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      ),
    mainPanel(
      dataTableOutput("contents")
      )
    ),

  titlePanel("Demand Estimation"),
  fluidRow(
    column(4,
           wellPanel(
             column(6, selectInput("wtpProd1", "Product 1 WTP Column", choices = c(""))),
             column(6, selectInput("wtpProd2", "Product 2 WTP Column", choices = c(""))),
             selectInput("regressType", "Regression Transformation", choices = c("Linear", "Exponential")),
             numericInput("pop", "Customer Population Size", min = 0, value = 1000)
             ),
           wellPanel(
             sliderInput("cPrice", "Competitor Price", min = 0, max = 10, value = 3, step = .25)
           )
           ),
    column(8,
           column(6,
           tabsetPanel(
             tabPanel("Demand",
                      plotOutput("demand_plot"),
                      ),
             tabPanel("Advanced 3D",
                      plotlyOutput("demand3D_plot"),
                      )
             )
           ),
           column(6,
                  tabsetPanel(
                    tabPanel("Summary",
                             verbatimTextOutput("lm_summary")
                    ),
                    tabPanel("Interpretations",  align = "center",
                             br(), br(), br(),
                             uiOutput("rSq_text"),
                             div(style = "font-size: 14px;",
                                 uiOutput("rSq_interpretation")
                             ),
                            # br(), br(), br(),
                            # uiOutput("demand_math_formula"),
                            # div(style = "font-size: 14px;",
                            #     uiOutput("intercept_interpretation")
                            # ),
                            # div(style = "font-size: 14px;",
                            #     uiOutput("slope_interpretation")
                            # )
                    )

                  )
                  )
           )
    ),

  fluidRow(
    column(4,
           wellPanel(
             sliderInput("price", "Price", min = 0, max = 10, value = 3, step = .25)
             ),
           column(6,
                  wellPanel(
                    numericInput("var1", "Variable Cost of Product 1", min = 0, value = 10),
                    numericInput("fix1", "Fixed Cost of Product 1", min = 0, value = 1000)
                  )
           ),
           column(6,
                  wellPanel(
                    numericInput("var2", "Variable Cost of Product 2", min = 0, value = 10),
                    numericInput("fix2", "Fixed Cost of Product 2", min = 0, value = 1000)
                    )
                  )
           ),
    column(4,
           tabsetPanel(
             tabPanel("Profit",
                      plotOutput("profit_plot")
                      ),
             tabPanel("Advanced 3D",
                      plotlyOutput("profit_opt_line")
                      )
             )
    ),
    column(4,
           tabsetPanel(
             tabPanel("optimized profit", align = "center",
                      br(), br(),
                      div(style = "font-size: 20px;",
                          uiOutput("opt_price")
                      ),
                      br(),
                      div(style = "font-size: 20px;",
                          uiOutput("opt_profit")
                          )
                      )
             )
           )
    ),

  fluidRow(
    column(4,
           ),
    column(4,
           tabsetPanel(
             tabPanel("Reaction Functions",
                      plotOutput("nash_plot2D")
                      ),
             tabPanel("Advanced 3D",
                     plotlyOutput("nash_plot3D")
                     )
             )
           ),
    column(4,
           tabsetPanel(
             tabPanel("Nash Equilibrium", align = "center",
                      br(), br(),
                      div(style = "font-size: 20px;",
                          uiOutput("nash_prod1")
                      ),
                      br(),
                      div(style = "font-size: 20px;",
                          uiOutput("nash_prod2")
                          )
                      )
             )
           )
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

  output$contents <- renderDataTable(userData(), options = list(pageLength = 8))

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
    demandPlotDuo(competitor_price = userCPrice(), price = userPrice(), data = userCleanData(), type = userType(), first_or_second = 1, population = userPop(), sample = userSample())
  })

  output$lm_summary <- renderPrint({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandSummaryDuo(data = userCleanData(), type = userType(), first_or_second = 1)
  })

  output$rSq_text  <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    rSq <- round(rSquaredDuo(data = userCleanData(), userType(), 1), 3)
    withMathJax(
      helpText(paste0("$$\\Large{R^2 \\ = \\ ", rSq, "}$$"))
    )
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
    profitPlotDuo(price1 = userPrice(), price2 = userCPrice(),
                  data = userCleanData(), type = userType(), first_or_second = 1,
                  var = userVar1(), fix = userFix1(),
                  population = userPop(), sample = userSample())
  })

  output$profit_opt_line <- renderPlotly({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitOptLine(data = userCleanData(), type = userType(),
                  first_or_second = 1, var = userVar1(), fix = userFix1(),
                  population = userPop(), sample = userSample())
  })

  output$opt_price <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }

    price <- optimizeProfitDuo(competitorPrice = userCPrice(), data = userCleanData(),
                               type = userType(), first_or_second = 1,
                               var = userVar1(), fix = userFix1(),
                               population = userPop(), sample = userSample())[[1]]

    show_Price <- paste0("<b>Price</b>: $", conNum_short(round(price, 2)))

    HTML(show_Price)
  })

  output$opt_profit <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profit <- optimizeProfitDuo(competitorPrice = userCPrice(), data = userCleanData(),
                                type = userType(), first_or_second = 1,
                                var = userVar1(), fix = userFix1(),
                                population = userPop(), sample = userSample())[[2]]

    show_Profit <- paste0("<b>Optimal Profit</b> $", format(round(profit, 2), big.mark = ","))

    HTML(show_Profit)
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

  output$nash_plot3D <- renderPlotly({
    if(is.null(userCleanData())){
      return(NULL)
    }
    nash3D(data = userCleanData(), type = userType(),
           var1 = userVar1(), fix1 = userFix1(),
           var2 = userVar2(), fix2 = userFix2(),
           population = userPop(), sample = userSample())
  })

  output$nash_output <- renderPrint({
    if(is.null(userCleanData())){
      return(NULL)
    }
    competitionSolve(data = userCleanData(), type = userType(), first_or_second = 1,
                     variable1 = userVar1(), fixed1 = userFix1(),
                     variable2 = userVar2(), fixed2 = userFix2(),
                     population = userPop(), sample = userSample())
  })

  output$nash_plot2D <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    nash2D(data = userCleanData(), type = userType(),
           var1 = userVar1(), fix1 = userFix1(),
           var2 = userVar2(), fix2 = userFix2(),
           population = userPop(), sample = userSample())
  })

  #nashPoints <- reactive({
  #  if(is.null(userCleanData())){
  #    return(NULL)
  #  }
  #  competitionSolve(data = userCleanData(), type = userType(), first_or_second = 1,
  #                   variable1 = userVar1(), fixed1 = userFix1(),
  #                   variable2 = userVar2(), fixed2 = userFix2(),
  #                   population = userPop(), sample = userSample())

#  })

  output$nash_prod1 <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }

    cols <- whichColumns(1, userCleanData())

    nashPoints <- binary_Optim(data = userCleanData(), type = userType(),
                               cols[[1]], cols[[2]], cols[[3]], cols[[4]],
                               var1 = userVar1(), fix1 = userFix1(),
                               var2 = userVar2(), fix2 = userFix2(),
                               population = userPop(), sample = userSample())

    price <- nashPoints[[1]]
    profit <- fPi_m(userCleanData(), userType(), cols[[1]], cols[[2]], cols[[3]], userVar1(), userFix1(), userPop(), userSample())(price, nashPoints[[2]])

    show_Price<- paste0("<b>", cols[[1]], "</b> Price: $", conNum_short(round(price, 2)), ", <b>Profit</b>: $", format(round(profit, 2), big.mark = ","))
    #show_Profit <- paste0("<b>Profit</b>: $", format(round(profit, 2), big.mark = ","))

    HTML(show_Price)
  })

  output$nash_prod2 <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    cols <- whichColumns(2, userCleanData())

    nashPoints <- binary_Optim(data = userCleanData(), type = userType(),
                               cols[[1]], cols[[2]], cols[[3]], cols[[4]],
                               var1 = userVar1(), fix1 = userFix1(),
                               var2 = userVar2(), fix2 = userFix2(),
                               population = userPop(), sample = userSample())

    price <- nashPoints[[1]]
    profit <- fPi_m(userCleanData(), userType(), cols[[1]], cols[[2]], cols[[3]], userVar2(), userFix2(), userPop(), userSample())(price, nashPoints[[2]])

    show_Price <- paste0("<b>", cols[[1]], "</b> Price: $", conNum_short(round(price, 2)), ", <b>Profit</b>: $", format(round(profit, 2), big.mark = ","))
    #show_Profit <- paste0("<b>Profit</b>: $", format(round(profit, 2), big.mark = ","))

    HTML(show_Price)
  })
}



# Run the application
shinyApp(ui = ui, server = server)

#cRock <- quantityCreation_duo(rock, "area", "peri")
#profitOptLine(data = cRock, type = "Linear", 1, 1, 1, 1, 1)
#nash(data = cRock, type = "Linear", 1, 1, 1, 1, 1, 1)

# variable should be 40% of median price?
# fixed should be median price

#library(tidyverse)
#cp <- read_csv("~/Desktop/CupcakesTest.csv")
#cpM <- quantityCreation_duo(cp, "cupcakes", "donuts")
#rSquaredDuo(cpM, "Exponential", 1)
