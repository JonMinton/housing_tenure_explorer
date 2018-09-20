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

read_csv("data/FRS HBAI - tables v1.csv") %>% 
  select(
    region = regname, year = yearcode, age = age2, tenure = tenurename, n = N_ten4s, N = N_all2
    ) -> dta 

regions <- unique(dta$region)

tenure_types <- unique(dta$tenure)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Housing Tenure Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(fluid = T,
    sidebarPanel(width = 3,
      selectInput("region", label = "Select region",
        choices = regions,
        selected = "UK",
        multiple = TRUE
                  ),
      selectInput("tenure", label = "Select tenure type",
        choices = tenure_types,
        selected = "Owner occupier",
        multiple = TRUE
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(width = 9,
       plotlyOutput("heatmap")
    )
  )
))
