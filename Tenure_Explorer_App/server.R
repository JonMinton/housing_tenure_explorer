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
  output$heatmap_diff_region <- renderPlotly({
    
    range_x <- range(dta$year)
    range_y <- range(dta$age)
    num_years <- range_x[2] - range_x[1]
    num_ages <- range_y[2] - range_y[1]
    
    dta %>% 
      filter(region ==  input$region_filter) -> dta_ss
    
    dta_first_tenure <- dta_ss %>% 
      filter(tenure == input$tenure_1)
    
    dta_second_tenure <- dta_ss %>% 
      filter(tenure == input$tenure_2)
    
    
    dta_diffs <- dta_first_tenure %>% 
      select(year, age, tenure, proportion) %>% 
      bind_rows(
        dta_second_tenure %>% 
          select(year, age, tenure, proportion)
      ) %>% 
      group_by(year, age) %>% 
      filter(n() == 2) %>% 
      summarise(
        diff_prop = proportion[tenure == !!input$tenure_2] - proportion[tenure == !!input$tenure_1]
      ) %>% 
      ungroup()

    p <- dta_diffs %>% 
      ggplot(aes(x = year, y = age, fill = diff_prop)) + 
      geom_tile() +
      scale_fill_distiller(palette = "Paired") 
    
    p %>% ggplotly(
      width =  (num_years * 15) + 50, 
      height = (num_ages * 10) + 25
    ) -> p
    

    p
    
  })
  
  output$heatmap_diff_tenure <- renderPlotly({
    
    range_x <- range(dta$year)
    range_y <- range(dta$age)
    num_years <- range_x[2] - range_x[1]
    num_ages <- range_y[2] - range_y[1]
    
    dta %>% 
      filter(tenure ==  input$tenure_filter) -> dta_ss
    
    dta_first_region <- dta_ss %>% 
      filter(region == input$region_1)
    
    dta_second_region <- dta_ss %>% 
      filter(region == input$region_2)
    
    
    dta_diffs <- dta_first_region %>% 
      select(year, age, region, proportion) %>% 
      bind_rows(
        dta_second_region %>% 
          select(year, age, region, proportion)
      ) %>% 
      group_by(year, age) %>% 
      filter(n() == 2) %>% 
      summarise(
        diff_prop = proportion[region == !!input$region_2] - proportion[region == !!input$region_1]
      ) %>% 
      ungroup()
    
    p <- dta_diffs %>% 
      ggplot(aes(x = year, y = age, fill = diff_prop)) + 
      geom_tile() +
      scale_fill_distiller(palette = "Paired") 
    
    p %>% ggplotly(
      width =  (num_years * 15) + 50, 
      height = (num_ages * 10) + 25
    ) -> p
    
    
    p
    
  })

  output$`3d_surface` <- renderPlotly({

    
    matrixify <- function(X, colname){
      tmp <- X %>% 
        select(year, age, !!colname)
      tmp %>% spread(age, !!colname) -> tmp
      years <- pull(tmp, year)
      tmp <- tmp %>% select(-year)
      ages <- names(tmp)
      mtrx <- as.matrix(tmp)
      return(list(ages = ages, years = years, vals = mtrx))
    }
    
    
    dta_ss <- dta %>% 
      filter(region == input$surface_region) %>% 
      filter(tenure == input$surface_tenure) %>% 
      select(year, age, proportion)
    

    prop_mtrx <- matrixify(dta_ss, "proportion")
    
    n_years <- length(prop_mtrx$years)
    n_ages <- length(prop_mtrx$ages)
    
    plot_ly() %>% 
      add_surface(
        x = ~prop_mtrx$ages, y = ~prop_mtrx$years, z = prop_mtrx$vals
      ) %>% 
      layout(
        scene = list(
          aspectratio = list(
            x = n_ages / n_years, y = 1, z = 0.5
          ),
          xaxis = list(
            title = "Age in years"
          ),
          yaxis = list(
            title = "Year"
          ),
          zaxis = list(
            title = "Proportion"
          )
        )        
      )

  })
  
  output$`3d_surface_composition` <- renderPlotly({
    # Start with a fixed example 
    
    matrixify <- function(X, colname){
      tmp <- X %>% 
        select(year, age, !!colname)
      tmp %>% spread(age, !!colname) -> tmp
      years <- pull(tmp, year)
      tmp <- tmp %>% select(-year)
      ages <- names(tmp)
      mtrx <- as.matrix(tmp)
      return(list(ages = ages, years = years, vals = mtrx))
    }
    
    
    dta_ss <- dta %>% 
      filter(region == input$surface_composition_region) %>% 
      select(year, age, tenure, proportion) %>% 
      group_by(year, age) %>% 
      mutate(cumulative_proportion = cumsum(proportion)) %>% 
      ungroup()
    
    surface_oo <- dta_ss %>% 
      filter(tenure == "Owner occupier") %>% 
      matrixify("cumulative_proportion")
    
    surface_sr <- dta_ss %>% 
      filter(tenure == "Social rent") %>% 
      matrixify("cumulative_proportion")
    
    surface_pr <- dta_ss %>% 
      filter(tenure == "Private rent") %>% 
      matrixify("cumulative_proportion")

    surface_rf <- dta_ss %>% 
      filter(tenure == "Care of/rent free") %>% 
      matrixify("cumulative_proportion")
    
    
    tooltip_oo <- dta_ss %>% 
      filter(tenure == "Owner occupier") %>% 
      matrixify("proportion")
    
    tooltip_sr <- dta_ss %>% 
      filter(tenure == "Social rent") %>% 
      matrixify("proportion")
    
    tooltip_pr <- dta_ss %>% 
      filter(tenure == "Private rent") %>% 
      matrixify("proportion")
    
    tooltip_rf <- dta_ss %>% 
      filter(tenure == "Care of/rent free") %>% 
      matrixify("proportion")
    
    custom_text <- paste0(
      "Year: ", rep(tooltip_oo$years, times = length(tooltip_oo$ages)), "\t",
      "Age: ", rep(tooltip_oo$ages, each = length(tooltip_oo$years)), "\n",
      "Composition: ", 
      "OO: ", round(tooltip_oo$vals, 2), "; ",
      "SR: ", round(tooltip_sr$vals, 2), "; ",
      "PR: ", round(tooltip_pr$vals, 2), "; ",
      "Other: ", round(tooltip_rf$vals, 2)
    ) %>% 
      matrix(length(tooltip_oo$years), length(tooltip_oo$ages))
    
    custom_oo <- paste0(
      "Owner occupation: ", 100 * round(tooltip_oo$vals, 3), " percent\n",
      custom_text
    ) %>% 
      matrix(length(tooltip_oo$years), length(tooltip_oo$ages))

    custom_sr <- paste0(
      "Social rented: ", 100 * round(tooltip_sr$vals, 3), " percent\n",
      custom_text
    ) %>% 
      matrix(length(tooltip_oo$years), length(tooltip_oo$ages))

    custom_pr <- paste0(
      "Private rented: ", 100 * round(tooltip_pr$vals, 3), " percent\n",
      custom_text
    ) %>% 
      matrix(length(tooltip_oo$years), length(tooltip_oo$ages))
    
    custom_rf <- paste0(
      "Other: ", 100 * round(tooltip_rf$vals, 3), " percent\n",
      custom_text
    ) %>% 
      matrix(length(tooltip_oo$years), length(tooltip_oo$ages))
    
    n_years <- length(surface_oo$years)
    n_ages <- length(surface_oo$ages)
    
    plot_ly(
      showscale = F
    ) %>% 
      add_surface(
        x = ~surface_oo$ages, y = ~surface_oo$years, z = surface_oo$vals,
        name = "Owner Occupiers",
        opacity = 0.7,
        colorscale = list(
          c(0,1),
          c('rgb(255,255,0)' , 'rgb(255,255,0)')
        ),
        hoverinfo = "text",
        text = custom_oo

      ) %>% 
      add_surface(
        x = ~surface_sr$ages, y = ~surface_sr$years, z = surface_sr$vals,
        name = "Social renters",
        opacity = 0.7,
        colorscale = list(
          c(0,1),
          c('rgb(255,0,0)' , 'rgb(255,0,0)')
        ),
        hoverinfo = "text",
        text = custom_sr
        
      ) %>% 
      add_surface(
        x = ~surface_pr$ages, y = ~surface_pr$years, z = surface_pr$vals,
        name = "Private renters",
        opacity = 0.7,
        colorscale = list(
          c(0,1),
          c('rgb(0,255,0)' , 'rgb(0,255,0)')
        ),
        hoverinfo = "text",
        text = custom_pr
        
      ) %>% 
      add_surface(
        x = ~surface_rf$ages, y = ~surface_rf$years, z = surface_rf$vals,
        name = "Other",
        opacity = 0.7,
        colorscale = list(
          c(0,1),
          c('rgb(0,0,255)' , 'rgb(0,0,255)')
        ),
        hoverinfo = "text",
        text = custom_rf
        
        
      ) %>% 
      layout(
        scene = list(
          aspectratio = list(
            x = n_ages / n_years, y = 1, z = 0.5
          ),
          xaxis = list(
            title = "Age in years"
          ),
          yaxis = list(
            title = "Year"
          ),
          zaxis = list(
            title = "Cumulative Proportion"
          ),
          showlegend = FALSE
        )      )
    
  })
  
})
