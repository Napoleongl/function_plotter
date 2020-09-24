# UI

library(ggplot2)
library(magrittr)
library(shiny)
require(data.table)
source("helpers.R")
source("graph.R")
xmin <- -2
xmax <- 2
xres <- 20
x <- seq(from = xmin, to = xmax, length.out = xres)
fill_under <- FALSE

ui <- fluidPage(
  
  titlePanel("Function definitions"),
  sidebarLayout(
    sidebarPanel(
      #actionButton("add_btn", "Add function"),
      #actionButton("rm_btn", "Remove function"),
      #textOutput("counter"),
      #uiOutput("textbox_ui"),
      textInput("f1", "Function 1", width = "150px", placeholder = "2+4*x", value = "5x^2+x-4"),
      textInput("f2", "Function 2", width = "150px", placeholder = "2+4*x", value = "3x-2"),
      textInput("f3", "Function 3", width = "150px", placeholder = "2+4*x", value = "2*x-1"),
      actionButton("plot_funs", "Plot functions", icon("chart-area"))
      ),
    mainPanel(
      plotOutput("function_plot")
      )
    ))

server <- function(input, output){
  function_strings <- eventReactive(input$plot_funs, {
    c(input$f1, input$f2, input$f3)%>%
      correct_missing_star() %>%
      correct_decimal_comma() 
    })
  fun_text <- reactive({paste(function_strings(), collapse = ",")})
  output$fun_text<- eventReactive(input$plot_funs, { fun_text() })
  observeEvent(input$plot_funs,{
    output$function_plot <- renderPlot(
        function_strings() %>%
          evaluate_functions(x = x) %>%
          graph_function()
    )
  })
}

shinyApp(ui, server)
