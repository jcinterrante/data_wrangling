library(tidyverse)
library(sf)
library(spData)
library(scales)
library(lubridate)
library(shiny)
library(plotly)

ui <- fluidPage(
  fluidRow(
    tags$h1("Chicago Urban Neglect Explorer"),
    tags$hr()
  ),
  fluidRow(
    column(width = 6,
      plotlyOutput("services")
    ),
    column(width = 6,
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
  
  output$services <- renderPlotly({
    plt_1 <- ggplot() +
      geom_sf(data = chicago_neglect, aes(fill = rqst_w_)) + 
      theme_void()+
      scale_fill_viridis_c(option = "inferno") +
      labs(title = "Duration of Outstanding Service Request",
           fill = paste0("Wait time (", unit, ")"))
    ggplotly(plt_1)
  })
  
  output$violations <- renderPlotly({
    plt_2 <- ggplot() +
      geom_sf(data = chicago_neglect, aes(fill = vltn_w_)) + 
      theme_void()+
      scale_fill_viridis_c(option = "inferno") +
      labs(title = "Duration of Outstanding Building Violation", 
           fill = paste0("Wait time (", unit, ")"))
    ggplotly(plt_2)
  })
}

shinyApp(ui = ui, server = server)