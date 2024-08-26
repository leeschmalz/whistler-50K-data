library(xml2)
library(dplyr)
library(httr)

gpx_file <- read_xml('./whistler_50K_official.gpx')

ns <- xml_ns(gpx_file)

# Extract track points
trkpts <- xml_find_all(gpx_file, ".//d1:trkpt", ns)

# Extract latitude, longitude, and elevation
lat <- xml_attr(trkpts, "lat")
lon <- xml_attr(trkpts, "lon")

# Create a data frame
gpx_df <- data.frame(
  latitude = as.numeric(lat),
  longitude = as.numeric(lon)
)

elevation_function <- function(lat, lon) {
  url <- 'https://api.open-elevation.com/api/v1/lookup?'
  params <- list(locations = paste(lat, lon, sep = ","))
  response <- GET(url, query = params)
  
  if (status_code(response) == 200) {
    content <- content(response, "parsed")
    return(content$results[[1]]$elevation)
  } else {
    return(NA)
  }
}

haversine_horizontal <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # Earth's radius in kilometers
  delta_lat <- (lat2 - lat1) * pi / 180
  delta_lon <- (lon2 - lon1) * pi / 180
  a <- sin(delta_lat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  distance <- R * c
  return(distance)
}

haversine_with_elevation <- function(lat1, lon1, lat2, lon2, ele1, ele2) {
  R <- 6371 # Earth's radius in kilometers
  delta_lat <- (lat2 - lat1) * pi / 180
  delta_lon <- (lon2 - lon1) * pi / 180
  a <- sin(delta_lat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  horizontal_distance <- R * c
  
  # Calculate the total distance including elevation
  vertical_distance <- abs(ele1 - ele2) / 1000 # Convert elevation from meters to kilometers
  total_distance <- sqrt(horizontal_distance^2 + vertical_distance^2)
  return(total_distance)
}

gpx_df <- gpx_df %>%
  rowwise() %>%
  mutate(elevation = elevation_function(latitude, longitude))

sys.sleep(2)

for (i in 1:nrow(gpx_df)){
  if(is.na(gpx_df$elevation[i])){
    elevation <- elevation_function(gpx_df$latitude[i], gpx_df$longitude[i])
    message(elevation)
    gpx_df$elevation[i] <- elevation
    Sys.sleep(0.3)
  }
}


gpx_df <- gpx_df %>% mutate(horizontal_distance_rel = NA)
gpx_df <- gpx_df %>% mutate(actual_distance_rel = NA)

for(i in 2:nrow(gpx_df)){
  prev_lat <- gpx_df$latitude[i-1]
  prev_lon <- gpx_df$longitude[i-1]
  prev_ele <- gpx_df$elevation[i-1]
  lat <- gpx_df$latitude[i]
  lon <- gpx_df$longitude[i]
  ele <- gpx_df$elevation[i]
  
  horiz_distance <- haversine_horizontal(prev_lat, prev_lon, lat, lon)
  actual_distance <- haversine_with_elevation(prev_lat, prev_lon, lat, lon, prev_ele, ele)
  
  gpx_df$horizontal_distance_rel[i] <- horiz_distance
  gpx_df$actual_distance_rel[i] <- actual_distance
}
gpx_df <- gpx_df %>% filter( !is.na(horizontal_distance_rel) ) # remove first row NA
gpx_df <- gpx_df %>% ungroup()
gpx_df <- gpx_df %>% mutate(actual_distance = cumsum(actual_distance_rel))
gpx_df <- gpx_df %>% mutate(horizontal_distance = cumsum(horizontal_distance_rel))

gpx_df %>% data.table::fwrite('~/whistler_gpx_analysis/whistler_elevation_data_official.csv')
