##### You need to run the stress_processing_script.R before running this script

library(sp)
library(raster)

# define the extent of the area you want to cover
xmin <- -400
xmax <- 400
ymin <- -400
ymax <- 400

# define the number of cells in the x and y directions
ncellx <- 10 # 37
ncelly <- 10 # 37

cellsize <- ((xmax - xmin)/ ncellx)

# create a SpatialPolygons object to represent the area
area_poly <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmax, xmax, xmin), c(ymin, ymin, ymax, ymax)))), ID = "1")))

# create a SpatialGrid object to represent the grid over the area
grid <- GridTopology(c(xmin + cellsize/2, ymin + cellsize/2), c(cellsize, cellsize), c(ncellx, ncelly))
grid_sp <- SpatialGrid(grid)

# outer path actual (active learning phase)
points1_df <- data.frame(x = outer_active_df_list[[length(outer_active_df_list)]]$pos_X, y = outer_active_df_list[[length(outer_active_df_list)]]$pos_Z)
points1_sp <- SpatialPoints(points1_df)

# outer path traveled (outer navInOrder part)
points2_df <- data.frame(x = outer_navInOrder_all_dfs$pos_X, y = outer_navInOrder_all_dfs$pos_Z)
points2_sp <- SpatialPoints(points2_df)

# use the "over()" function to find which grid cells contain the x-y coordinates
points1_grid <- over(points1_sp, grid_sp)
points2_grid <- over(points2_sp, grid_sp)

# find the unique numbers for each of the paths representing the total grids the path uses
unique_path1_grids <- unique(points1_grid)
unique_path2_grids <- unique(points2_grid)

# plot the grid and the x-y coordinates within the area
plot(grid_sp)
plot(points1_sp, add = TRUE, col = "red")

plot(grid_sp)
plot(points2_sp, add = TRUE, col = "red")

# Get the indices of the overlapping grid cells
overlapping_indices <- intersect(points1_grid, points2_grid)

# Get the indices of the non-overlapping grid cells for points1
# aka grids that exist in path1 (actual) but not path2 (traveled)
non_overlapping_indices_1 <- setdiff(points1_grid, union(overlapping_indices, points2_grid))

# Get the indices of the non-overlapping grid cells for points2
# aka grids that exist in path2 (traveled) but not path1 (actual)
non_overlapping_indices_2 <- setdiff(points2_grid, union(overlapping_indices, points1_grid))

# Count the number of overlapping and non-overlapping grid cells
num_overlapping <- length(overlapping_indices)
num_non_overlapping_1 <- length(non_overlapping_indices_1)
num_non_overlapping_2 <- length(non_overlapping_indices_2)

# Print the results
cat("Number of overlapping grid cells: ", num_overlapping, "\n")
cat("Number of non-overlapping grid cells for points1: ", num_non_overlapping_1, "\n")
cat("Number of non-overlapping grid cells for points2: ", num_non_overlapping_2, "\n")

################ make a plot shading in the cells that 

# create a matrix of zeros with ncellx and ncelly
grid_matrix <- matrix(0, nrow = ncelly, ncol = ncellx)

# define the indices to color
color_indices_path1 <- unique_path1_grids
color_indices_path2 <- unique_path2_grids

# set the values at the color indices to 1
path1_matrix <- grid_matrix[color_indices_path1] <- 1
path2_matrix <- grid_matrix[color_indices_path2] <- 1

# define the color palette
colors <- c("white", "gray")

# plot the image with flipped y-axis
path1_image <- image(1:ncellx, 1:ncelly, grid_matrix, col = colors, ylim = c(ncelly + .5, 0.5))
path2_image <- image(1:ncellx, 1:ncelly, grid_matrix, col = colors, ylim = c(ncelly + .5, 0.5))


########################################################################################################




