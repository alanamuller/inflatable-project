library(sp)

# define the extent of the area you want to cover
xmin <- 0
xmax <- 10
ymin <- 0
ymax <- 10

# define the number of cells in the x and y directions
ncellx <- 3
ncelly <- 3

cellsize <- ((xmax - xmin)/ ncellx)

# create a SpatialPolygons object to represent the area
area_poly <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmax, xmax, xmin), c(ymin, ymin, ymax, ymax)))), ID = "1")))

# create a SpatialGrid object to represent the grid over the area
grid <- GridTopology(c(xmin + cellsize/2, ymin + cellsize/2), c(cellsize, cellsize), c(ncellx, ncelly))
grid_sp <- SpatialGrid(grid)

# generate path1 x-y coordinates within the area
points1_df <- data.frame(x = c(1,2,10), y = c(1,2,3))
points1_sp <- SpatialPoints(points1_df)

# generate path2 x-y coordinates within the area
points2_df <- data.frame(x = c(10,9,8,7,6,5,4,3,2,1,0), y = c(0,1,2,3,4,5,6,7,8,9,10))
points2_sp <- SpatialPoints(points2_df)

# use the "over()" function to find which grid cells contain the x-y coordinates
points1_grid <- over(points1_sp, grid_sp)
points2_grid <- over(points2_sp, grid_sp)

# plot the grid and the x-y coordinates within the area
plot(grid_sp)
plot(points1_sp, add = TRUE, col = "red")

plot(grid_sp)
plot(points2_sp, add = TRUE, col = "red")

# Get the indices of the overlapping grid cells
overlapping_indices <- intersect(points1_grid, points2_grid)

# Get the indices of the non-overlapping grid cells for points1
# aka grids that exist in path1 but not path2
non_overlapping_indices_1 <- setdiff(points1_grid, union(overlapping_indices, points2_grid))

# Get the indices of the non-overlapping grid cells for points2
# aka grids that exist in path2 but not path1
non_overlapping_indices_2 <- setdiff(points2_grid, union(overlapping_indices, points1_grid))

# Count the number of overlapping and non-overlapping grid cells
num_overlapping <- length(overlapping_indices)
num_non_overlapping_1 <- length(non_overlapping_indices_1)
num_non_overlapping_2 <- length(non_overlapping_indices_2)

# Print the results
cat("Number of overlapping grid cells: ", num_overlapping, "\n")
cat("Number of non-overlapping grid cells for points1: ", num_non_overlapping_1, "\n")
cat("Number of non-overlapping grid cells for points2: ", num_non_overlapping_2, "\n")







