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
           checkboxInput("streets", h3("Show Streets"), value = FALSE)
    ),
    column(width = 3,
           dateRangeInput("dates", h3("Date range"))
    )
  ),
  fluidRow(
    column(width = 6,
      tags$h2("Service Request Wait Times"),
      checkboxGroupInput("service_type", h3("Types of Services"),
                         choices = list("Street Light Problems" = "Alley Light Out Complaint",
                                        "Potholes" = c("Alley Light Out Complaint", "Alley Pothole Complaint"),
                                        "Bike and Scooter Issues" = 3,
                                        "Sidewalk Problems" = 4,
                                        "Snow Problems" = 5,
                                        "Signage and Stoplight Problems" = 6)

      )
    )
  ),
  fluidRow(
    column(width = 6,
      
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
  
  services_data <- read_csv(".\\Chicago Data\\service_subset.csv")
  violations_data <- read_csv(".\\Chicago Data\\violations_subset.csv")
  chicago_neglect_base <- st_read(".\\Chicago Maps\\chicago neglect.shp")
  street_map <- st_read(".\\Chicago Maps\\Major_Streets\\Major_Streets.shp")
  
  services <- reactive({
    services_data%>%filter(SR_TYPE %in% input$service_type)%>%
      group_by(COMMUNITY_AREA) %>%
      summarize(request_wait_mean = mean(wait_time, na.rm=TRUE))
  })
  

  building_violations_points <-st_transform(violations_data, 32616)
  
  building_violations_comm_points <- st_join(building_violations_points, chicago_shape["area_num_1"])
  
  st_geometry(building_violations_comm_points) <- NULL
  building_violation_means <- building_violations_comm_points %>%
    group_by(area_num_1) %>%
    summarize(violation_wait_mean = mean(wait_time))
  
  chicago_neglect <- chicago_neglect_base %>%
    left_join(services, by = c("area_num_1" = "COMMUNITY_AREA"))%>%
    left_join(building_violation_means, by = c("area_num_1" ))
  
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

