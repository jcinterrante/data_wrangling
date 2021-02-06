library(tidyverse)
library(sf)
library(spData)
library(scales)
library(lubridate)

# Public wellbeing in cities rely on a combination of civic services and 
# private stewardship. My choropleth answers the question: What parts of Chicago
# are being neglected by the public and private sectors? To do so, it uses persistent
# building code violations as an indicator of private sector neglect
# and long 311 response times as an indicator of civic neglect.

initialize_service_requests <- function(data, department_list, 
                                        timezone = "America/Chicago",
                                        unit = "days",
                                        date_range = c(ymd("2020,1,1"), today(timezone))){
  period <- interval(date_range[1], date_range[2])
  data %>%
    mutate(CREATED_DATE = mdy_hms(CREATED_DATE, tz = timezone),
           LAST_MODIFIED_DATE = mdy_hms(LAST_MODIFIED_DATE, tz = timezone),
           CLOSED_DATE = mdy_hms(CLOSED_DATE, tz = timezone),
           "wait_time" = as.double(if_else(is.na(CLOSED_DATE),
                                           difftime(today(timezone), CREATED_DATE, unit = unit), 
                                           difftime(CLOSED_DATE, CREATED_DATE, unit = unit))
                                   )
          ) %>%
  filter(OWNER_DEPARTMENT %in% department_list, CREATED_DATE %within% period, DUPLICATE == "false") %>%
  select(SR_NUMBER, SR_TYPE, OWNER_DEPARTMENT, STATUS, CREATED_DATE, 
         LAST_MODIFIED_DATE, CLOSED_DATE, COMMUNITY_AREA, LATITUDE, LONGITUDE,
         LOCATION, wait_time)
}

initialize_building_violations <- function(data,timezone = "America/Chicago", 
                                           unit = "days", 
                                           date_range = c(ymd("2020,1,1"), today(timezone)),
                                           category = c("COMPLAINT")){
  period <- interval(date_range[1], date_range[2])
  
  data%>%
    filter(!(is.na(LATITUDE)|is.na(LONGITUDE)))%>%
    mutate(VIOLATION.DATE = mdy(VIOLATION.DATE, tz = timezone),
           VIOLATION.LAST.MODIFIED.DATE = mdy(VIOLATION.LAST.MODIFIED.DATE, tz = timezone),
           "wait_time" = as.double(if_else(VIOLATION.STATUS == "OPEN", 
                                           difftime(today(timezone), VIOLATION.DATE, unit = unit), 
                                           difftime(VIOLATION.LAST.MODIFIED.DATE, VIOLATION.DATE, unit = unit))
                                   )
           )%>%
    select(ID, VIOLATION.LAST.MODIFIED.DATE, VIOLATION.DATE, VIOLATION.STATUS, 
           VIOLATION.STATUS.DATE,VIOLATION.DESCRIPTION, INSPECTION.STATUS, 
           INSPECTION.CATEGORY, DEPARTMENT.BUREAU, LATITUDE, LONGITUDE, LOCATION, 
           wait_time) %>%
    filter(VIOLATION.DATE %within% period)
}
# Customize Defaults
departments = c("CDOT - Department of Transportation")
timezone = "America/Chicago"
unit = "days"
date_range = c(ymd("2020,1,1"), ymd("2021,1,1"))

# write determines whether we use previously generated subsets of the original csvs, or create new subsets
# (that way we don't have to load and re-filter the original csvs every run -- too time consuming)
# FALSE: Read the previously created subsets (service_subset and violations_subset)
# TRUE: reload original service request and building violation csvs, change filters
# and write the changes to service_subset and violations_subset
write = FALSE

if (!exists("chicago_shape")){
  path <- ".\\Chicago Maps\\"
  chicago_shape <- st_read(paste0(path,"CommAreas.shp"))%>% 
    mutate(area_num_1 = as.numeric(area_num_1))
}

path <- ".\\Chicago Data\\"
if(write){
  service_requests <- read.csv(paste0(path, "311_Service_requests.csv"))
  service_requests_cdot <- initialize_service_requests(service_requests, departments, date_range = date_range)
  write.csv(service_requests_cdot, file = paste0(path, "service_subset.csv"))
  
  building_violations_data <- read.csv(paste0(path, "Building_Violations.csv"))
  building_violations <- initialize_building_violations(building_violations_data, unit = unit, date_range = date_range)
  write.csv(building_violations, file = paste0(path, "violations_subset.csv"))
} else{
  service_requests_cdot <- read.csv(paste0(path, "service_subset.csv"))
  building_violations <- read.csv(paste0(path, "violations_subset.csv"))
}

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

chicago_neglect <- chicago_shape %>%
  left_join(service_requests_cdot_summary, by = c("area_num_1" = "COMMUNITY_AREA"))%>%
  left_join(building_violation_means, by = c("area_num_1" ))

st_write(chicago_neglect, ".\\Chicago Maps\\chicago neglect.shp", delete_layer = TRUE)

ggplot() +
  geom_sf(data = chicago_neglect, aes(fill = rqst_w_)) + 
  theme_void()+
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Duration of Outstanding Service Request",
       fill = paste0("Wait time (", unit, ")"))

ggplot() +
  geom_sf(data = chicago_neglect, aes(fill = vltn_w_)) + 
  theme_void()+
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Duration of Outstanding Building Violation", 
       fill = paste0("Wait time (", unit, ")"))
