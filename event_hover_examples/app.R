#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("plotly_event problems"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("vis_type", "Select type of visualisation",
                    choices = c(
                      "x and y the same (ok)" = "x_y_same",
                      "x and y same step but diff length (ok)"= "x_y_same_step",
                      "x and y same step but diff length 2(fail)"= "x_y_same_step_2",
                      
                      "x and y diff step but same length (fail)" = "x_y_diff_step",
                      "x and y diff step but same length 2 (ok)" = "x_y_diff_step_2"
                      
                    ),
                    selected = "x_y_same"
                    )
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("examples"),
         verbatimTextOutput("hover_output")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$examples <- renderPlotly({
     
      if(input$vis_type == "x_y_same"){
        x <- seq(0, 1, by = 0.01)
        y <- seq(0, 1, by = 0.01)
        z <- outer(X = x, Y = y)
        cat(file = stderr(), 
            "vis_type: ", input$vis_type, 
            ". len_x: ", length(x), 
            ";len_y: ", length(y),
            "; dim_z: ", dim(z), "\n")
        p <- plot_ly(z = ~z) %>%
          add_surface()
        
      } else if (input$vis_type == "x_y_same_step") {
        x <- seq(0, 2, by = 0.01)
        y <- seq(0, 1, by = 0.01)
        z <- outer(X = x, Y = y)
        
        cat(file = stderr(), 
            "vis_type: ", input$vis_type, 
            ". len_x: ", length(x), 
            ";len_y: ", length(y),
            "; dim_z: ", dim(z), "\n")
        
        p <- plot_ly(z = ~z) %>%
          add_surface()
        
      } else if (input$vis_type == "x_y_diff_step"){
        x <- seq(0,1,by= 0.02)
        y <- seq(0,1, by = 0.01)
        z <- outer(X = x, Y = y)
        cat(file = stderr(), 
            "vis_type: ", input$vis_type, 
            ". len_x: ", length(x), 
            ";len_y: ", length(y),
            "; dim_z: ", dim(z), "\n")
        
        
        p <- plot_ly(z = ~z) %>% 
          add_surface()
      } else if (input$vis_type == "x_y_same_step_2") {
        x <- seq(0,1, by = 0.01)
        y <- seq(0, 2, by = 0.01)
        z <- outer(X = x, Y = y)
        cat(file = stderr(), 
            "vis_type: ", input$vis_type, 
            ". len_x: ", length(x), 
            ";len_y: ", length(y),
            "; dim_z: ", dim(z), "\n")
        
        p <- plot_ly(z = ~z) %>% 
          add_surface()
      } else if (input$vis_type == "x_y_diff_step_2"){
        x <- seq(0, 1, by = 0.01)
        y <- seq(0, 1, by = 0.02)
        z <- outer(X = x, Y = y)
        cat(file = stderr(), 
            "vis_type: ", input$vis_type, 
            ". len_x: ", length(x), 
            ";len_y: ", length(y),
            "; dim_z: ", dim(z), "\n")
        
        
        p <- plot_ly(z = ~z) %>% 
          add_surface()
      }
     
     return (p)
   })
   
   output$hover_output <- renderPrint({
     s <- event_data("plotly_hover")
     if (length(s) == 0){
       "Move around!"
     } else {
       as.list(s)
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

