library(tidyverse)
library(sf)
library(spData)
library(scales)
library(ggmap)
library(lubridate)

# Public wellbeing in cities rely on a combination of civic services and 
# private stewardship. My choropleth answers the question: What parts of Chicago
# are being neglected by the public and private sectors? To do so, it uses persistent
# building code violations as an indicator of private sector neglect
# and long 311 response times as an indicator of civic neglect.

initialize_service_requests <- function(data, department_list, timezone = "America/Chicago", unit = "days"){
  data %>%
    filter(OWNER_DEPARTMENT %in% department_list) %>%
    mutate(CREATED_DATE = mdy_hms(CREATED_DATE, tz = timezone),
           LAST_MODIFIED_DATE = mdy_hms(LAST_MODIFIED_DATE, tz = timezone),
           CLOSED_DATE = mdy_hms(CLOSED_DATE, tz = timezone),
           "wait_time" = as.double(difftime(CLOSED_DATE,CREATED_DATE, unit = unit)))
}

initialize_building_violations <- function(data,timezone = "America/Chicago", 
                                           unit = "days", 
                                           date_range = c(ymd("2011,1,1"), today())){
  period <- interval(date_range[1], date_range[2])
  
  data%>%
    filter(!(is.na(LATITUDE)|is.na(LONGITUDE)))%>%
    mutate(VIOLATION.DATE = mdy(VIOLATION.DATE, tz = timezone),
           VIOLATION.LAST.MODIFIED.DATE = mdy(VIOLATION.LAST.MODIFIED.DATE, tz = timezone),
           "wait_time" = as.double(difftime(VIOLATION.LAST.MODIFIED.DATE, VIOLATION.DATE, unit = unit)))%>%
  filter(VIOLATION.DATE %within% period)

  }
if (!exists("chicago_shape")){
  path <- ".\\Chicago Maps\\"
  chicago_shape <- st_read(paste0(path,"CommAreas.shp"))%>% 
    mutate(area_num_1 = as.numeric(area_num_1))
  
  path <- ".\\Chicago Data\\"
  service_requests <- read.csv(paste0(path, "311_Service_requests.csv"))
  building_violations_data <- read.csv(paste0(path, "Building_Violations.csv"))
}

departments = c("CDOT - Department of Transportation")
timezone = "America/Chicago"
unit = "days"

service_requests_cdot <- initialize_service_requests(service_requests, departments)
building_violations <- initialize_building_violations(building_violations_data, unit = unit)

service_requests_cdot_summary <- service_requests_cdot %>%
  group_by(COMMUNITY_AREA) %>%
  summarize(request_wait_mean = mean(wait_time, na.rm=TRUE))

#https://spatialanalysis.github.io/lab_tutorials/1_R_Spatial_Data_Handling.html
building_violations_points <- st_as_sf(building_violations, 
                                      coords = c("LONGITUDE", "LATITUDE"), 
                                      crs = 4326, agr = "constant")

chicago_shape <- st_transform(chicago_shape, 32616)
building_violations_points <-st_transform(building_violations_points, 32616)

building_violations_comm_points <- st_join(building_violations_points, chicago_shape["area_num_1"])

st_geometry(building_violations_comm_points) <- NULL
building_violation_means <- building_violations_comm_points %>%
                              group_by(area_num_1) %>%
                              summarize(violation_wait_mean = mean(wait_time))
            
chicago_shape_data <- chicago_shape %>%
  left_join(service_requests_cdot_summary, by = c("area_num_1" = "COMMUNITY_AREA"))%>%
  left_join(building_violation_means, by = c("area_num_1" ))


ggplot() +
  geom_sf(data = chicago_shape_data, aes(fill = request_wait_mean)) + 
  theme_void()+
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Duration of Outstanding Service Request",
       fill = paste0("Wait time (", unit, ")"))

ggplot() +
  geom_sf(data = chicago_shape_data, aes(fill = violation_wait_mean)) + 
  theme_void()+
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Duration of Outstanding Building Violation", 
       fill = paste0("Wait time (", unit, ")"))

       