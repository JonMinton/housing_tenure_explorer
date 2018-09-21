#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(RColorBrewer)
library(plotly)

source("scripts/palette_set.R")

read_csv("data/FRS HBAI - tables v1.csv") %>% 
  select(
    region = regname, year = yearcode, age = age2, tenure = tenurename, n = N_ten4s, N = N_all2
    ) -> dta 

regions <- unique(dta$region)

tenure_types <- unique(dta$tenure)

colorscales <- c(
  'Blackbody',
    'Bluered',
    'Blues',
    'Earth',
    'Electric',
    'Greens',
    'Greys',
    'Hot',
    'Jet',
    'Picnic',
    'Portland',
    'Rainbow',
    'RdBu',
    'Reds',
    'Viridis',
    'YlGnBu',
    'YlOrRd'
) 

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Housing Tenure Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(fluid = T,
    sidebarPanel(width = 3,
                 
                 # LATTICE TABSET
      conditionalPanel(
        condition = "input.tabset_1 == 'Lattice'",
        selectInput("region", label = "Select region",
                    choices = regions,
                    selected = "UK",
                    multiple = TRUE
          )        
      ),
      conditionalPanel(
        condition = "input.tabset_1 == 'Lattice'",
        selectInput("tenure", label = "Select tenure type",
                    choices = tenure_types,
                    selected = "Owner occupier",
                    multiple = TRUE
        )
      ), 
      


                 # DIFFERENCE: REGION
    # Filter on tenure, difference by region
    conditionalPanel(
      condition = "input.tabset_1 == 'Difference between regions'",
      selectInput("region_filter", label = "Select region of interest",
                  choices = regions,
                  selected = "UK", 
                  multiple = FALSE
                  )
    ),
    conditionalPanel(
      condition = "input.tabset_1 == 'Difference between regions'",
      selectInput("tenure_1", label = "Select first tenure type",
                  choices = tenure_types,
                  selected = "Owner occupier",
                  multiple = FALSE
                  )
    ),
    conditionalPanel(
      condition = "input.tabset_1 == 'Difference between regions'",
      selectInput("tenure_2", label = "Select second tenure type",
                  choices = tenure_types,
                  selected = "Private rent",
                  multiple = FALSE
        )
      ),
    # DIFFERENCE: TENURE
    # Filter on region, difference by tenure
    conditionalPanel(
      condition = "input.tabset_1 == 'Difference between tenures'",
      selectInput("tenure_filter", label = "Select tenure of interest",
                  choices = tenure_types,
                  selected = "Owner occupier", 
                  multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = "input.tabset_1 == 'Difference between tenures'",
      selectInput("region_1", label = "Select first region",
                  choices = regions,
                  selected = "London",
                  multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = "input.tabset_1 == 'Difference between tenures'",
      selectInput("region_2", label = "Select second region",
                  choices = regions,
                  selected = "North/Wales",
                  multiple = FALSE
      )
    ),
    
    # 3D surface - single tenure within single region
    conditionalPanel(
      condition = "input.tabset_1 == '3D surface plot'",
      selectInput("surface_region", label = "Select region to show 3D surface",
                  choices = regions,
                  selected = "UK",
                  multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = "input.tabset_1 == '3D surface plot'",
      selectInput("surface_tenure", label = "Select tenure type to show 3D surface",
                  choices = tenure_types,
                  selected = "Owner occupier",
                  multiple = FALSE
      )
    ),
    # 3D surface composition - single region, show composition within region
    conditionalPanel(
      condition = "input.tabset_1 == '3D surface composition' | input.tabset_1 == '3D surface overlaid'",
      selectInput("surface_composition_region", label = "Select region to show 3D surface",
                  choices = regions,
                  selected = "UK",
                  multiple = FALSE
      )
    ),
    
    conditionalPanel(
      condition = "input.tabset_1 == 'Lattice' || input.tabset_1 == '3D surface plot'",
      selectInput("pal_type", label = "Select colour palette",
                  choices = palette_options,
                  selected = "adjusted_paired"
      )
    )
    
    

    ),
    mainPanel(width = 9,
      tabsetPanel(id = "tabset_1", type = "tab",
        tabPanel(
          title = "Lattice",
          plotlyOutput("heatmap")
        ),
        tabPanel(
          title = "Difference between tenures",
          plotlyOutput("heatmap_diff_region")
        ),
        tabPanel(
          title = "Difference between regions",
          plotlyOutput("heatmap_diff_tenure")
        ),
        tabPanel(
          title = "3D surface plot",
          plotlyOutput("3d_surface", height = 800)
        ),
        tabPanel(
          title = "3D surface composition",
          plotlyOutput("3d_surface_composition", height = 450),
          plotlyOutput(("cumulative_slice"))
        ),
        tabPanel(
          title = "3D surface overlaid",
          plotlyOutput("3d_surface_overlaid", height = 450),
          plotlyOutput("slice")
          )
          

        )
      )
    )
  )
)

