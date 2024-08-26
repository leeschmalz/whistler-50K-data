interp <- function(gpx_df){
  new_distance <- c()
  new_elevation <- c()
  
  # Loop through each pair of points
  for (i in 1:(nrow(gpx_df) - 1)) {
    # Calculate the gap between consecutive points
    gap <- gpx_df$horizontal_distance[i + 1] - gpx_df$horizontal_distance[i]
    
    # If the gap is larger than 0.0025, interpolate points
    if (gap > 0.0025) {
      # Create sequence of new points between the current and next point
      new_distances <- seq(gpx_df$horizontal_distance[i], gpx_df$horizontal_distance[i + 1], by = 0.0025)[-1]
      
      # Use the LOESS model to predict the corresponding elevation values
      new_elevations <- predict(loess_fit, new_distances)
      
      # Store the new points
      new_distance <- c(new_distance, new_distances)
      new_elevation <- c(new_elevation, new_elevations)
    }
  }
  
  # Combine original and new points
  all_distances <- c(gpx_df$horizontal_distance, new_distance)
  all_elevations <- c(gpx_df$elevation, new_elevation)
  
  # Create a data frame and sort by horizontal distance
  interpolated_df <- data.frame(horizontal_distance = all_distances, elevation = all_elevations)
  interpolated_df <- interpolated_df[order(interpolated_df$horizontal_distance), ]
  
  interpolated_df <- interpolated_df %>% arrange(horizontal_distance)
  rownames(gpx_df) <- seq(length = nrow(gpx_df))
  
  return(interpolated_df)
}
