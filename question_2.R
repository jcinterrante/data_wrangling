# 1 late token used

# Nice job setting a relative file path

# Lines 127-129 throw an error because the variable names in the by() statement are incorrect
# -3

library(tidyverse)
library(sf)
library(spData)
library(scales)
library(lubridate)

# Public wellbeing in cities relies on a combination of civic services and
# private stewardship. My choropleths answer the question: What parts of Chicago
# are being neglected by the public and private sectors? To do so, it uses the
# amount of time it takes building owners to remedy building code violations
# as an indicator of private sector neglect and long 311 response times as an
# indicator of civic neglect. By plotting these side by side, we can study
# patterns in the spatial variation of these types of neglect.

initialize_service_requests <- function(data, department_list,
                                        timezone = "America/Chicago",
                                        unit = "days",
                                        date_range = c(ymd("2020,1,1"), today(timezone))) {
  period <- interval(date_range[1], date_range[2], tzone = timezone)
  data %>%
    mutate(
      CREATED_DATE = mdy_hms(CREATED_DATE, tz = timezone),
      LAST_MODIFIED_DATE = mdy_hms(LAST_MODIFIED_DATE, tz = timezone),
      CLOSED_DATE = mdy_hms(CLOSED_DATE, tz = timezone),
      "wait_time" = as.double(if_else(is.na(CLOSED_DATE),
        difftime(today(timezone), CREATED_DATE, unit = unit),
        difftime(CLOSED_DATE, CREATED_DATE, unit = unit)
      ))
    ) %>%
    filter(OWNER_DEPARTMENT %in% department_list, CREATED_DATE %within% period, DUPLICATE == "false") %>%
    select(
      SR_NUMBER, SR_TYPE, OWNER_DEPARTMENT, STATUS, CREATED_DATE,
      LAST_MODIFIED_DATE, CLOSED_DATE, COMMUNITY_AREA, LATITUDE, LONGITUDE,
      LOCATION, wait_time
    )
}

initialize_building_violations <- function(data, timezone = "America/Chicago",
                                           unit = "days",
                                           date_range = c(ymd("2020,1,1"), today(timezone)),
                                           category = c("COMPLAINT")) {
  period <- interval(date_range[1], date_range[2], tz = timezone)

  data <- data %>%
    filter(!(is.na(LATITUDE) | is.na(LONGITUDE))) %>%
    mutate(
      VIOLATION.DATE = mdy(VIOLATION.DATE, tz = timezone),
      VIOLATION.LAST.MODIFIED.DATE = mdy(VIOLATION.LAST.MODIFIED.DATE, tz = timezone),
      "wait_time" = as.double(if_else(VIOLATION.STATUS == "OPEN",
        difftime(today(timezone), VIOLATION.DATE, unit = unit),
        difftime(VIOLATION.LAST.MODIFIED.DATE, VIOLATION.DATE, unit = unit)
      ))
    ) %>%
    select(
      ID, VIOLATION.LAST.MODIFIED.DATE, VIOLATION.DATE, VIOLATION.STATUS,
      VIOLATION.STATUS.DATE, VIOLATION.DESCRIPTION, INSPECTION.STATUS,
      INSPECTION.CATEGORY, DEPARTMENT.BUREAU, LATITUDE, LONGITUDE, LOCATION,
      wait_time
    ) %>%
    filter(VIOLATION.DATE %within% period)

  # The Chicago Building Violation Data doesn't have a column for community area. I create one.
  # https://spatialanalysis.github.io/lab_tutorials/1_R_Spatial_Data_Handling.html
  data <- st_as_sf(data,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326, agr = "constant"
  )

  chicago_shape <- st_transform(chicago_shape, 32616)
  data <- st_transform(data, 32616)

  data <- data %>% st_join(chicago_shape["area_num_1"])

  st_geometry(data) <- NULL

  return(data)
}
# Customize Defaults
departments <- c("CDOT - Department of Transportation")
timezone <- "America/Chicago"
unit <- "days"
date_range <- c(ymd("2020,1,1"), ymd("2021,1,1"))

# write_csvs determines whether we use previously generated subsets of the original csvs, or create new subsets
# (that way we don't have to load and re-filter the original csvs every run -- too time consuming)
# FALSE: Read the previously created subsets (service_subset and violations_subset)
# TRUE: reload original service request and building violation csvs, change filters
# and write the changes to service_subset and violations_subset
write_csvs <- FALSE

if (!exists("chicago_shape")) {
  path <- "./Chicago Maps/"
  chicago_shape <- st_read(paste0(path, "CommAreas.shp")) %>%
    mutate(area_num_1 = as.numeric(area_num_1))
}

path <- "./Chicago Data/"
if (write_csvs) {
  service_requests_data <- read.csv(paste0(path, "311_Service_requests.csv"))
  service_requests <- initialize_service_requests(service_requests_data, departments, date_range = date_range)
  write.csv(service_requests, file = paste0(path, "service_subset.csv"))

  building_violations_data <- read.csv(paste0(path, "Building_Violations.csv"))
  building_violations <- initialize_building_violations(building_violations_data, unit = unit, date_range = date_range)
  write.csv(building_violations, file = paste0(path, "violations_subset.csv"))
  # st_write(building_violations, paste0(path,"building_violations.shp"), driver = "ESRI Shapefile", delete_layer = TRUE)
} else {
  service_requests <- read.csv(paste0(path, "service_subset.csv"))
  building_violations <- read.csv(paste0(path, "violations_subset.csv"))
}

service_requests_summary <- service_requests %>%
  group_by(COMMUNITY_AREA) %>%
  summarize(request_wait_mean = mean(wait_time, na.rm = TRUE))

building_violation_means <- building_violations %>%
  group_by(area_num_1) %>%
  summarize(violation_wait_mean = mean(wait_time))

chicago_neglect <- chicago_shape %>%
  left_join(service_requests_summary, by = c("ar_nm_1" = "COMMUNITY_AREA")) %>%
  left_join(building_violation_means, by = c("ar_nm_1" = "area_num_1"))

st_write(chicago_neglect, ".\\Chicago Maps\\chicago neglect.shp", delete_layer = TRUE)

ggplot() +
  geom_sf(data = chicago_neglect, aes(fill = request_wait_mean)) +
  theme_void() +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    title = "Duration of Outstanding Service Request",
    fill = paste0("Wait time (", unit, ")")
  )

ggplot() +
  geom_sf(data = chicago_neglect, aes(fill = violation_wait_mean)) +
  theme_void() +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    title = "Duration of Outstanding Building Violation",
    fill = paste0("Wait time (", unit, ")")
  )


service_types <- service_requests %>%
  group_by(SR_TYPE) %>%
  summarize(n())
