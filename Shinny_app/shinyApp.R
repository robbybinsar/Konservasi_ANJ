library(shiny)
shinyApp(
    ui = fluidPage(
        selectInput(
            "select", 
            label = h3("Select box"), 
            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
            selected = 1
        ),
        hr(),
        fluidRow(column(3, verbatimTextOutput("value")))
    ),
    server = function(input, output) {
        # You can access the value of the widget with input$select, e.g.
        output$value <- renderPrint({ input$select })
    }
)
