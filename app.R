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
  
  titlePanel("Simple function plotter"),
  sidebarLayout(
    sidebarPanel(
      #actionButton("add_btn", "Add function"),
      #actionButton("rm_btn", "Remove function"),
      #textOutput("counter"),
      #uiOutput("textbox_ui"),
      fluidRow(
        shinydashboard::box(width = 11,title = "Functions",
            textInput("f1", NULL, width = "100%", placeholder = "2+4*x", value = "5x^2+x-4"),
            textInput("f2", NULL, width = "100%", placeholder = "2+4*x", value = "3x-2"),
            textInput("f3", NULL, width = "100%", placeholder = "2+4*x", value = "2*x-1"),
            textInput("f4", NULL, width = "100%", placeholder = "2+4*x", value = "x*sin(x*5)-exp(x)/2")
            )
      ),
      fluidRow(
        shinydashboard::box(width = 11,title = "X-axis", 
            splitLayout(
              numericInput("xmin", "x-min", value = -1, step =0.1),
              numericInput("xmax", "x-max", value = 1, step =0.1),
              numericInput("xres", "points", value = 100L, min = 3L, max = 1000L, step =1L)
            )
        )
      ),
      actionButton("plot_funs", "Plot functions", icon("line-chart"))
      ),
    mainPanel(
      plotOutput("function_plot")
      )
    ))

server <- function(input, output){
  function_strings <- eventReactive(input$plot_funs, {
    c(input$f1, input$f2, input$f3, input$f4) %>%
      correct_missing_star() %>%
      correct_decimal_comma() %>% 
      remove_empty()
    })
  xseq <- eventReactive(input$plot_funs, {seq(from = input$xmin, to = input$xmax, length.out = input$xres)})
  observeEvent(input$plot_funs,{
    output$function_plot <- renderPlot(
        function_strings() %>%
          evaluate_functions(x = xseq()) %>%
          graph_function()
    )
  })
}

shinyApp(ui, server, , options = list(height = 1080))
