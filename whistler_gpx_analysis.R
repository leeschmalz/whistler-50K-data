library(dplyr)
library(ggplot2)
library(plotly)
options(scipen = 999)
source('./helper.R')

gpx_df <- data.table::fread('~/whistler_gpx_analysis/whistler_elevation_data_official.csv') %>% 
  rename(elevation_raw = elevation) %>%
  mutate(elevation_raw = elevation_raw * 3.281) # m to ft

# smooth out the inaccuracies in elevation api
loess_fit <- loess(elevation_raw ~ horizontal_distance, data = gpx_df, span = 0.0175)
gpx_df$elevation <- predict(loess_fit)
gpx_df <- gpx_df %>% select(horizontal_distance, elevation)
gpx_df <- interp(gpx_df) # interpolate to add density data

gpx_df$grade <- NA
grade_window <- 0.375 # in kms
for (i in 2:(nrow(gpx_df)-1)){
  for (j in 1:nrow(gpx_df)){
    if( (gpx_df$horizontal_distance[i] - gpx_df$horizontal_distance[i-j]>grade_window) | (i-j)==1 ){
      back_index <- i-j
      break
    }
  }
  
  for (k in 1:nrow(gpx_df)){
    if(gpx_df$horizontal_distance[i+k] - gpx_df$horizontal_distance[i]>grade_window | (i+k)==nrow(gpx_df)){
      forward_index <- i+k
      break
    }
  }
  
  rise <- (gpx_df$elevation[forward_index]-gpx_df$elevation[back_index])
  run <- (gpx_df$horizontal_distance[forward_index]-gpx_df$horizontal_distance[back_index]) * 3280.84 # km to feet
  
  gpx_df$grade[i] <- (rise / run) * 100
}

waypoints <- rbind(
  gpx_df %>% filter(horizontal_distance < 2) %>% arrange(elevation) %>% head(1) %>% mutate(name = 'NW Passage', type = 'base'),
  gpx_df %>% filter(horizontal_distance > 0 & horizontal_distance < 5) %>% arrange(-elevation) %>% head(1) %>% mutate(name = 'NW Passage', type = 'peak'),
  gpx_df %>% filter(horizontal_distance > 2 & horizontal_distance < 5) %>% arrange(elevation) %>% head(1) %>% mutate(name = 'Roundhouse Aid Station', type = 'base'),
  gpx_df %>% filter(horizontal_distance > 10 & horizontal_distance < 15) %>% arrange(-elevation) %>% head(1) %>% mutate(name = 'Roundhouse Aid Station', type = 'peak'),
  gpx_df %>% filter(horizontal_distance > 15 & horizontal_distance < 20) %>% arrange(elevation) %>% head(1) %>% mutate(name = 'Piccolo Summit', type = 'base'),
  gpx_df %>% filter(horizontal_distance > 15 & horizontal_distance < 20) %>% arrange(-elevation) %>% head(1) %>% mutate(name = 'Piccolo Summit', type = 'peak'),
  gpx_df %>% filter(horizontal_distance > 20 & horizontal_distance < 25) %>% arrange(elevation) %>% head(1) %>% mutate(name = 'Whistler Summit', type = 'base'),
  gpx_df %>% filter(horizontal_distance > 25 & horizontal_distance < 30) %>% arrange(-elevation) %>% head(1) %>% mutate(name = 'Whistler Summit', type = 'peak')
) %>% select(-grade)

climbs <- waypoints %>%
  group_by(name) %>%
  summarise(avg_grade = 100*(max(elevation) - min(elevation)) / (3280.84*(max(horizontal_distance) - min(horizontal_distance))),
            horizontal_distance_start = min(horizontal_distance),
            horizontal_distance_end = max(horizontal_distance),
            elevation_gain_ft = max(elevation) - min(elevation),
            peak_elevation_ft = max(elevation),
            distance_km = max(horizontal_distance) - min(horizontal_distance)
            )

p <- ggplot(gpx_df, aes(x=horizontal_distance)) +
  geom_point(aes(y=elevation), size=0.2) +
  geom_line(aes(y=elevation)) +
  labs(y = "Altitude (ft)", x = "Horizontal Distance (km)") +
  geom_segment(aes(x=horizontal_distance, xend = horizontal_distance, y=elevation, yend = min(elevation), color = grade)) + 
  scale_color_gradient2(low = 'darkgreen', mid = 'gold', high = 'darkred', midpoint = 0, space = "Lab", limits = c(-32, 32)) +
  theme_bw() + theme(panel.border = element_blank())

p <- p +
  geom_segment(data = climbs, 
               aes(x = horizontal_distance_start, 
                   xend = horizontal_distance_end, 
                   y = peak_elevation_ft+50, 
                   yend = peak_elevation_ft+50), 
               color = "black", inherit.aes = FALSE) +
  geom_text(data = climbs, 
            aes(x = (horizontal_distance_start + horizontal_distance_end) / 2, 
                y = peak_elevation_ft - 150, 
                label = paste0(round(distance_km, 1), 'km, ', round(elevation_gain_ft), " ft Climb\nAvg grade: ", round(avg_grade, 1), "%")), 
            color = "black", size = 3.5, inherit.aes = FALSE) +
  geom_text(data = climbs, 
            aes(x = (horizontal_distance_start + horizontal_distance_end) / 2, 
                y = peak_elevation_ft + 150, 
                label = name), 
            color = "darkblue", size = 3.5, inherit.aes = FALSE)

ggsave('~/whistler_gpx_analysis/grade_map_official.png', width = 20, height = 6)


### Total elevation gain
gpx_df <- gpx_df %>% 
  mutate(elevation_gain = elevation-lag(elevation)) %>%
  mutate(elevation_gain = if_else(elevation_gain<0,0,elevation_gain))

# total elevation gain
sum(gpx_df$elevation_gain,na.rm=T)

#ggplotly(p)

