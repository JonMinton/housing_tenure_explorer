#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

pal <- colorRampPalette(brewer.pal(11, "Paired"))(200)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$heatmap <- renderPlotly({
    
    range_x <- range(dta$year)
    range_y <- range(dta$age)
    num_years <- range_x[2] - range_x[1]
    num_ages <- range_y[2] - range_y[1]
    
    dta %>% 
      filter(region %in% input$region, tenure %in% input$tenure) -> dta_ss
    dta_ss %>% 
      ggplot(aes(x = year, y = age, fill = proportion)) + 
      geom_tile() + 
      facet_grid(region ~ tenure) +
      guides(fill = FALSE) -> p
    
    p %>% ggplotly(
      width = length(unique(dta_ss$tenure)) * (num_years * 10) + 150, 
      height = length(unique(dta_ss$region)) * (num_ages * 10) + 50
      
    ) -> p
    
    k <- length(p$x$data)

    p %>% 
      style(
        colorscale = "Rainbow",
        showscale = F,
        zauto = F,
        zmin = 0, zmax = 1,
        traces = 1:(k),
        hoverinfo = "x+y+z"
      )  -> p1
    
    p1
    
  })
  

  
})
