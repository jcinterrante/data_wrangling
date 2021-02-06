library(tidyverse)
library(sf)
library(spData)
library(scales)
library(lubridate)
library(shiny)
library(plotly)

ui <- fluidPage(
  fluidRow(
    column(width = 3,
      tags$img(src = "CPR.png", height = 72, width = 72)
    ),
    column(width = 9,
      tags$h1("Chicago Urban Neglect Explorer"),
      tags$hr()
    )
  ),
  fluidRow(
    column(width = 3,
           checkboxInput("streets", "Show Streets", value = FALSE)
    )
  ),
  fluidRow(
    column(width = 6,
      tags$h2("Service Request Wait Times"),
      plotlyOutput("services")
    ),
    column(width = 6,
      tags$h2("Building Violation Duration"),
      plotlyOutput("violations")
    )
  )
)
  
  
server<-function(input, output){
  departments = c("CDOT - Department of Transportation")
  timezone = "America/Chicago"
  unit = "days"
  date_range = c(ymd("2020,1,1"), ymd("2021,1,1"))
  
  services <- read_csv(".\\Chicago Data\\service_subset.csv")
  violations <- read_csv(".\\Chicago Data\\violations_subset.csv")
  chicago_neglect <- st_read(".\\Chicago Maps\\chicago neglect.shp")
  street_map <- st_read(".\\Chicago Maps\\Major_Streets\\Major_Streets.shp")
  
  output$services <- renderPlotly({
    plt_1 <- ggplot() +
      geom_sf(data = chicago_neglect, aes(fill = rqst_w_)) + 
      theme_void()+
      scale_fill_viridis_c(option = "inferno") +
      labs(fill = paste0("Wait time (", unit, ")"))
    if(input$streets){
      plt_1 <-plt_1 +
        geom_sf(data = street_map)  
    }
    ggplotly(plt_1)
  })
  
  output$violations <- renderPlotly({
    plt_2 <- ggplot() +
      geom_sf(data = chicago_neglect, aes(fill = vltn_w_)) + 
      theme_void()+
      scale_fill_viridis_c(option = "inferno") +
      labs(fill = paste0("Wait time (", unit, ")"))
    if(input$streets){
      plt_2 <-plt_2 +
        geom_sf(data = street_map) 
    }
    ggplotly(plt_2)
  })
}

shinyApp(ui = ui, server = server)