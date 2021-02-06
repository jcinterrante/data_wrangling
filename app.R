library(tidyverse)
library(sf)
library(spData)
library(scales)
library(lubridate)
library(shiny)
library(plotly)

ui <- fluidPage(
  fluidRow(
    column(width = 3)
  ),
  fluidRow(
    column(width = 6,
           plotlyOutput("service_request")
    ),
    column(width = 6,
           plotlyOutput("building_violation")
    )
  )
  
  
  
  output$services <- renderPlot({
    ggplot() +
      geom_sf(data = chicago_shape_data, aes(fill = request_wait_mean)) + 
      theme_void()+
      scale_fill_viridis_c(option = "inferno") +
      labs(title = "Duration of Outstanding Service Request",
           fill = paste0("Wait time (", unit, ")"))
  })
  ggsave("Service_Requests.png")
  
  
  ggplot() +
    geom_sf(data = chicago_shape_data, aes(fill = violation_wait_mean)) + 
    theme_void()+
    scale_fill_viridis_c(option = "inferno") +
    labs(title = "Duration of Outstanding Building Violation", 
         fill = paste0("Wait time (", unit, ")"))
  ggsave("Building_Violations.png")
  }

shinyApp(ui = ui, server = server)
  server <- function(input, output){