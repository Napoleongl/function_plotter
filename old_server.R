# Server

server <- function(input, output) {
  library(magrittr)
  source("helpers.R")
  source("graph.R")
  
  # Track the number of input boxes to render
  counter <- reactiveValues(n = 1)
  funstr <- reactiveValues()
  # Add or remove functions based on button clicks
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })
  
  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn, {
    if (counter$n > 1) counter$n <- counter$n - 1
  })

  textboxes <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("textin", i),
                    label = paste0("Function ", i), 
                    value = AllInputs()[[paste0("textin", i)]])
        })
      })
    }
    
  })
  
  output$textbox_ui <- renderUI({ textboxes() })
  
  verbboxes <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      isolate({
        funstr$x <- sapply(seq_len(n), function(i) {AllInputs()[[paste0("textin", i)]]})
        lapply(seq_len(n), function(i) {AllInputs()[[paste0("textin", i)]]
        })
      })
    }
    
  })
  output$verbbox_ui <- renderUI({ verbboxes() })
  
  observeEvent(input$plot_funs, {
    counter$n <- counter$n
    output$function_strings <- renderText({paste(funstr$x ,collapse = "-")})
  })
  #output$function_strings <- renderText({paste(funstr$x ,collapse = " ")})
  #output$function_strings <- renderText({paste(fun_strings, collapse = "--")})
  # output$function_plot <- renderPlot(function(){
  #   function_strings %>% 
  #     correct_missing_star() %>% 
  #     correct_decimal_comma() %>% 
  #     evaluate_functions(x = x) %>% 
  #     graph_function()
  # })
}
