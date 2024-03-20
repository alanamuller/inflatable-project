# Used to be part of navStress_unityData_processing_script.R
# Probably need to run that script for anything in this script to work
# But I'm working on another way to analyze this data so it may become obsolete


######################## Calculate overlapping grids ###########################

library(sp)
library(raster)
library(sf)

# define the extent of the area you want to cover
xmin <- -400
xmax <- 400
ymin <- -400
ymax <- 400

# define the number of cells in the x and y directions
ncellx <- 37 # 37
ncelly <- 37 # 37

cellsize <- ((xmax - xmin)/ ncellx)

# create a SpatialPolygons object to represent the area
area_poly <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmax, xmax, xmin), c(ymin, ymin, ymax, ymax)))), ID = "1")))

# create a SpatialGrid object to represent the grid over the area
grid <- GridTopology(c(xmin + cellsize/2, ymin + cellsize/2), c(cellsize, cellsize), c(ncellx, ncelly))
grid_sp <- SpatialGrid(grid)

################### For navigation test trials ###################

# outer path actual (active learning phase)
outerPath_df <- data.frame(x = outer_active_df_list[[length(outer_active_df_list)]]$pos_X, y = outer_active_df_list[[length(outer_active_df_list)]]$pos_Z)
outerPath_sp <- SpatialPoints(outerPath_df)

# inner path actual (active learning phase)
innerPath_df <- data.frame(x = inner_active_df_list[[length(inner_active_df_list)]]$pos_X, y = inner_active_df_list[[length(inner_active_df_list)]]$pos_Z)
innerPath_sp <- SpatialPoints(innerPath_df)

# use the "over()" function to find which grids contain the x-y coordinates
outerPath_grid <- over(outerPath_sp, grid_sp)
innerPath_grid <- over(innerPath_sp, grid_sp)

# find the unique numbers for each of the paths representing the total grids the path uses
unique_outerPath_grids <- unique(outerPath_grid)
unique_innerPath_grids <- unique(innerPath_grid)

# find the total number of grids each path uses
grid_total_outer <- length(unique_outerPath_grids)
grid_total_inner <- length(unique_innerPath_grids)

# plot the grid and the x-y coordinates within the area
plot(grid_sp)
plot(outerPath_sp, add = TRUE, col = "red", cex = 0.1)

plot(grid_sp)
plot(innerPath_sp, add = TRUE, col = "red", cex = 0.1)




##################################################### just testing some stuff #######################################

grid_poly <- as(grid_sp, "SpatialPolygons")

# Define the group of indices you want to color red
indices_to_color_red <- c(5, 10, 15)  # Example: indices 5, 10, and 15

# Extract polygons corresponding to the selected indices
selected_polygons <- grid_poly[indices_to_color_red]

# Plot the grid
plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE)
plot(grid_poly, add = TRUE)

# Plot the selected polygons in red
plot(selected_polygons, col = "red", add = TRUE)





#####################################################################################################################


# Get the indices of the overlapping grids of the inner and outer paths
inner_outer_overlap <- intersect(outerPath_grid, innerPath_grid)

# Count the number of overlapping segments for outer and inner path - should be 0
num_inner_outer_overlap <- length(inner_outer_overlap)

# Create a dataframe to put the data in
overlap_counts_df <- data.frame(overlap_outer = numeric(), overlap_inner = numeric(), nonoverlapping_grid_num = numeric(), total_grids_trial = numeric(), stringsAsFactors = FALSE)

# do a loop to count the overlaps and put it in a dataframe

for (i in 1:length(navTest_trials_df_list)) {
  
  # traveled path (Navigation Test 24 trials)
  navTestTrial_df <- data.frame(x = navTest_trials_df_list[[i]]$pos_X, y = navTest_trials_df_list[[i]]$pos_Z)
  navTestTrial_sp <- SpatialPoints(navTestTrial_df)
  
  # use the "over()" function to find which grids contain the x-y coordinates
  navTestTrial_grid <- over(navTestTrial_sp, grid_sp)
  
  # find the unique numbers for each of the paths representing the total grids the path uses
  unique_navTestTrial_grids <- unique(navTestTrial_grid)
  
  # find the total number of grids each path uses
  grid_total_trial <- length(unique_navTestTrial_grids)
  
  # Get the indices of the overlapping grids with outer and inner paths
  overlap_outer_indices <- intersect(outerPath_grid, navTestTrial_grid)
  overlap_inner_indices <- intersect(innerPath_grid, navTestTrial_grid)
  
  # Count the number of overlapping and non-overlapping grids
  num_overlapping_outer <- length(overlap_outer_indices)
  num_overlapping_inner <- length(overlap_inner_indices)
  non_overlapping <- grid_total_trial - (length(overlap_outer_indices) + length(overlap_inner_indices))
  
  # Add data to the dataframe
  overlap_counts_df <- rbind(overlap_counts_df, data.frame(overlap_outer = num_overlapping_outer, overlap_inner = num_overlapping_inner, 
                                                           nonoverlapping_grid_num = non_overlapping, total_grids_trial = grid_total_trial))
}

pilot_data_processed <- cbind(log_data, overlap_counts_df)

# write dataframe to an excel file
file_name <- paste(subject_num, "_navTestTrials.xlsx", sep = "")
write.xlsx(pilot_data_processed, file_name, rowNames = FALSE)

################### For navInOrder learning trials ###################

# Create a dataframe to put the data in
overlap_navInOrder_df <- data.frame(trial = character(), overlap_outer = numeric(), overlap_inner = numeric(), total_navInOrder_grids = numeric(), total_actual_path_grids = numeric(), stringsAsFactors = FALSE)

# define outer path
outer_navInOrder <- data.frame(x = outer_navInOrder_all_dfs$pos_X, y = outer_navInOrder_all_dfs$pos_Z)
outer_navInOrder_sp <- SpatialPoints(outer_navInOrder)

# define inner path
inner_navInOrder <- data.frame(x = inner_navInOrder_all_dfs$pos_X, y = inner_navInOrder_all_dfs$pos_Z)
inner_navInOrder_sp <- SpatialPoints(inner_navInOrder)

# use the "over()" function to find which grids contain the x-y coordinates
outer_navInOrder_grid <- over(outer_navInOrder_sp, grid_sp)
inner_navInOrder_grid <- over(inner_navInOrder_sp, grid_sp)

# find the unique numbers for each of the paths representing the total grids the path uses
unique_outer_navInOrder_grids <- unique(outer_navInOrder_grid)
unique_inner_navInOrder_grids <- unique(inner_navInOrder_grid)

# find the total number of grids each path uses
grid_total_outer_navInOrder <- length(unique_outer_navInOrder_grids)
grid_total_inner_navInOrder <- length(unique_inner_navInOrder_grids)

# Get the indices of the overlapping grids of the outer and inner paths
outer_navInOrder_overlap <- intersect(outer_navInOrder_grid, outerPath_grid)
inner_navInOrder_overlap <- intersect(inner_navInOrder_grid, innerPath_grid)

# Count the number of overlapping segments for outer and inner path - should be 0
num_outer_navInOrder_overlap <- length(outer_navInOrder_overlap)
num_inner_navInOrder_overlap <- length(inner_navInOrder_overlap)

# Add data to the dataframe
overlap_navInOrder_df <- rbind(overlap_navInOrder_df, data.frame(trial = "outer", overlap_outer = num_outer_navInOrder_overlap, overlap_inner = "N/A", 
                                                                 total_navInOrder_grids = grid_total_outer_navInOrder, total_actual_path_grids = grid_total_outer))
overlap_navInOrder_df <- rbind(overlap_navInOrder_df, data.frame(trial = "inner", overlap_outer = "N/A", overlap_inner = num_inner_navInOrder_overlap, 
                                                                 total_navInOrder_grids = grid_total_inner_navInOrder, total_actual_path_grids = grid_total_inner))

# Add a column for subject ID
overlap_navInOrder_df$subjectID <- subject_num

# Make the subjectID column the first column
overlap_navInOrder_df <- overlap_navInOrder_df[c(ncol(overlap_navInOrder_df), 1:ncol(overlap_navInOrder_df)-1)]

# write dataframe to an excel file
file_name <- paste(subject_num, "_overlapLearningTrials.xlsx", sep = "")
write.xlsx(overlap_navInOrder_df, file_name, rowNames = FALSE)


##################### Getting the closest points to separate the whole active path into four segments #####################

# Define the points to search for (these are the store coordinates)
search_points <- data.frame(x = c(207.3, 145.79, -130.43, -249.37), y = c(99.9, -231.68, -112.92, 279.16))

########### OUTER PATH ###########

# Find the closest point to each search point
outer_closest_points <- lapply(1:nrow(search_points), function(i) {
  
  # Calculate distances to all points in data frame
  distances <- sqrt((outer_active_df_list[[length(outer_active_df_list)]]$pos_X - search_points[i, "x"])^2 + (outer_active_df_list[[length(outer_active_df_list)]]$pos_Z - search_points[i, "y"])^2)
  
  # Get index of minimum distance
  min_index <- which.min(distances)
  
  # Return x-y values and index of closest point
  return(data.frame(x = outer_active_df_list[[length(outer_active_df_list)]]$pos_X[min_index], 
                    y = outer_active_df_list[[length(outer_active_df_list)]]$pos_Z[min_index], 
                    index = min_index,
                    time = outer_active_df_list[[length(outer_active_df_list)]]$time_sec[min_index]))
})

# Combine the closest points into a single data frame
outer_closest_points_df <- do.call(rbind, outer_closest_points)

# Print the closest points
print(outer_closest_points_df)

### Make a list of dataframes to segment one path into four paths

# Initialize the dataframe
outer_active_seg_list <- list()

# Store 1 to store 2
outer_active_seg_list[[1]] <- outer_active_df_list[[length(outer_active_df_list)]] %>%
  filter(time_sec <= outer_closest_points_df$time[1])
# Store 2 to store 3
outer_active_seg_list[[2]] <- outer_active_df_list[[length(outer_active_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[1] & time_sec <= outer_closest_points_df$time[2])
# Store 3 to store 4
outer_active_seg_list[[3]] <- outer_active_df_list[[length(outer_active_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[2] & time_sec <= outer_closest_points_df$time[3])
# Store 4 to store 1
outer_active_seg_list[[4]] <- outer_active_df_list[[length(outer_active_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[3])

# Make an empty dataframe to put the path segment distance values into
outer_actual_seg_dist <- data.frame(segment_distance = numeric(), stringsAsFactors = FALSE)

# Fill dataframe with actual path distance values for each segment
for (i in seq_along(outer_active_seg_list)) {
  actual_seg_dist <- totDist(outer_active_seg_list[[i]]$pos_X, outer_active_seg_list[[i]]$pos_Z)
  
  # put info in dataframe
  outer_actual_seg_dist <- rbind(outer_actual_seg_dist, data.frame(segment_distance = actual_seg_dist))
  
}

########### INNER PATH ###########

# Find the closest point to each search point
inner_closest_points <- lapply(1:nrow(search_points), function(i) {
  
  # Calculate distances to all points in data frame
  distances <- sqrt((inner_active_df_list[[length(inner_active_df_list)]]$pos_X - search_points[i, "x"])^2 + (inner_active_df_list[[length(inner_active_df_list)]]$pos_Z - search_points[i, "y"])^2)
  
  # Get index of minimum distance
  min_index <- which.min(distances)
  
  # Return x-y values and index of closest point
  return(data.frame(x = inner_active_df_list[[length(inner_active_df_list)]]$pos_X[min_index], 
                    y = inner_active_df_list[[length(inner_active_df_list)]]$pos_Z[min_index], 
                    index = min_index,
                    time = inner_active_df_list[[length(inner_active_df_list)]]$time_sec[min_index]))
})

# Combine the closest points into a single data frame
inner_closest_points_df <- do.call(rbind, inner_closest_points)

# Print the closest points
print(inner_closest_points_df)

### Make a list of dataframes to segment one path into four paths

# Initialize the dataframe
inner_active_seg_list <- list()

# Store 1 to store 2
inner_active_seg_list[[1]] <- inner_active_df_list[[length(inner_active_df_list)]] %>%
  filter(time_sec <= inner_closest_points_df$time[1])
# Store 2 to store 3
inner_active_seg_list[[2]] <- inner_active_df_list[[length(inner_active_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[1] & time_sec <= inner_closest_points_df$time[2])
# Store 3 to store 4
inner_active_seg_list[[3]] <- inner_active_df_list[[length(inner_active_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[2] & time_sec <= inner_closest_points_df$time[3])
# Store 4 to store 1
inner_active_seg_list[[4]] <- inner_active_df_list[[length(inner_active_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[3])

# Make an empty dataframe to put the path segment distance values into
inner_actual_seg_dist <- data.frame(segment_distance = numeric(), stringsAsFactors = FALSE)

# Fill dataframe with actual path distance values for each segment
for (i in seq_along(inner_active_seg_list)) {
  actual_seg_dist <- totDist(inner_active_seg_list[[i]]$pos_X, inner_active_seg_list[[i]]$pos_Z)
  
  # put info in dataframe
  inner_actual_seg_dist <- rbind(inner_actual_seg_dist, data.frame(segment_distance = actual_seg_dist))
  
}
