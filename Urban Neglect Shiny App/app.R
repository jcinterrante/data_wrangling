library(tidyverse)
library(sf)
library(spData)
library(scales)
library(lubridate)
library(shiny)
library(plotly)



ui <- fluidPage(
  fluidRow(
    column(
      width = 2,
      tags$img(src = "CPR.png", height = 72, width = 72)
    ),
    column(
      width = 10,
      tags$h1("Chicago Urban Neglect Explorer"),
      tags$hr()
    )
  ),
  fluidRow(
    mainPanel(
      width = 12, "Public wellbeing in cities relies on a combination of civic services and 
      private stewardship. What parts of Chicago are being neglected by the public 
      and private sectors? These maps display the amount of time it takes building 
      owners to remedy building code violations, an indicator of private sector neglect,
      and long 311 response times, an indicator of civic neglect. By plotting these 
      side by side, we can study patterns in the geography of neglect.",
      tags$hr()
    ),
  ),
  fluidRow(
    column(
      width = 4,
      dateRangeInput("dates", "Date range", start = "2020,1,1", end = "2021,1,1")
    ),
    column(
      width = 3,
      checkboxInput("streets", "Show Streets", value = FALSE)
    )
  ),

  fluidRow(
    column(
      width = 6,
      tags$h2("Service Request Wait Times"),
      plotlyOutput("services_map")
    ),
    column(
      width = 6,
      tags$h2("Building Violation Duration"),
      plotlyOutput("violations_map")
    )
  ),

  fluidRow(
    column(
      width = 6,
      checkboxGroupInput("service_type", h3("Types of Requests"),
        choices = list(
          "Street Light Out" = "Street Light Out Complaint",
          "Potholes" = "Pothole in Street Complaint",
          "Bike Requests" = "Bicycle Request/Complaint",
          "Sidewalk Problems" = "Sidewalk Inspection Request",
          "Signage and Stoplight Problems" = "Sign Repair Request - All Other Signs"
        ),
        selected = c(
          "Street Light Out Complaint",
          "Pothole in Street Complaint",
          "Bicycle Request/Complaint",
          "Sidewalk Inspection Request",
          "Sign Repair Request - All Other Signs"
        )
      )
    ),
    column(
      width = 6,
      checkboxGroupInput("inspection_category", h3("Source of Violation"),
        choices = list(
          "Complaint" = "COMPLAINT",
          "Periodic Inspection" = "PERIODIC",
          "Permit Inspection" = "PERMIT"
        ),
        selected = c(
          "COMPLAINT",
          "PERIODIC",
          "PERMIT"
        )
      )
    )
  )
)


server <- function(input, output) {
  timezone <- "America/Chicago"
  unit <- "days"
  services_data <- read_csv("service_subset.csv") # %>%
  # mutate(CREATED_DATE = mdy_hms(CREATED_DATE, tz = timezone),
  #      LAST_MODIFIED_DATE = mdy_hms(LAST_MODIFIED_DATE, tz = timezone))
  violations_data <- read_csv("violations_subset.csv") # %>%
  # mutate(VIOLATION.DATE = mdy(VIOLATION.DATE, tz = timezone),
  # VIOLATION.LAST.MODIFIED.DATE = mdy(VIOLATION.LAST.MODIFIED.DATE, tz = timezone))
  chicago_neglect_data <- st_read("chicago neglect.shp") %>%
    mutate(area_num_1 = as.numeric(are_nmb))
  street_map <- st_read("Major_Streets.shp")

  date_range <- reactive({
    interval(ymd(input$dates[1]), ymd(input$dates[2]), tzone = timezone)
  })
  
  services <- reactive({
    services_filtered <- services_data %>%
      filter(
        SR_TYPE %in% input$service_type,
        CREATED_DATE %within% date_range()
      ) %>%
      group_by(COMMUNITY_AREA) %>%
      summarize(request_wait_mean = mean(wait_time, na.rm = TRUE))

    chicago_neglect_data %>%
      left_join(services_filtered, by = c("area_num_1" = "COMMUNITY_AREA"))
  })

  violations <- reactive({
    violations_filtered <- violations_data %>%
      filter(
        INSPECTION.CATEGORY %in% input$inspection_category,
        VIOLATION.DATE %within% date_range()
      ) %>%
      group_by(area_num_1) %>%
      summarize(violation_wait_mean = mean(wait_time))

    chicago_neglect_data %>%
      left_join(violations_filtered, by = c("area_num_1" = "area_num_1"))
  })



  output$services_map <- renderPlotly({
    plt_1 <- ggplot() +
      geom_sf(data = services(), aes(fill = request_wait_mean)) +
      theme_void() +
      scale_fill_viridis_c(option = "inferno") +
      labs(fill = paste0("Wait time (", unit, ")"))
    if (input$streets) {
      plt_1 <- plt_1 +
        geom_sf(data = street_map)
    }
    ggplotly(plt_1)
  })

  output$violations_map <- renderPlotly({
    plt_2 <- ggplot() +
      geom_sf(data = violations(), aes(fill = violation_wait_mean)) +
      theme_void() +
      scale_fill_viridis_c(option = "inferno") +
      labs(fill = paste0("Wait time (", unit, ")"))
    if (input$streets) {
      plt_2 <- plt_2 +
        geom_sf(data = street_map)
    }
    ggplotly(plt_2)
  })
}

shinyApp(ui = ui, server = server)

