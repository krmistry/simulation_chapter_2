##### Create simulation scenarios

# Loading libraries
library(spatstat) ## for calculating nearest neighbor distances

#### Scenarios:
## 5 clustering scenarios, 2 ways of moving through 30 years of time, and
## 2 levels of sampling detection with harvest occurring every other year, for
## a total of 20 simulation scenarios

#### Setting up environment parameters
## set (x,y) limits for grid size
x_max <- 500
y_max <- 500
## number of fish
n_fish <- 100

# testing push

### Random, one cluster loose, one cluster tight, two clusters loose, four 
### clusters tight at time 0
set.seed(818)

# List to hold all cluster scenarios at time 0
clusters <- list()
# Random
clusters$rdm <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$rdm) <- c("x_loc", "y_loc", "nnd")
clusters$rdm$x_loc <- runif(n_fish, 0, x_max)
clusters$rdm$y_loc <- runif(n_fish, 0, y_max)
clusters$rdm$nnd <- nndist(clusters$rdm$x_loc, clusters$rdm$y_loc)
# Cluster A
clusters$cluster_A <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$cluster_A) <- c("x_loc", "y_loc", "nnd")
clusters$cluster_A$x_loc <- rnorm(n_fish, 250, 100)
clusters$cluster_A$y_loc <- rnorm(n_fish, 250, 100)
clusters$cluster_A$nnd <- nndist(clusters$cluster_A$x_loc, 
                                 clusters$cluster_A$y_loc)
# Cluster B
clusters$cluster_B <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$cluster_B) <- c("x_loc", "y_loc", "nnd")
clusters$cluster_B$x_loc <- rnorm(n_fish, 250, 20)
clusters$cluster_B$y_loc <- rnorm(n_fish, 250, 20)
clusters$cluster_B$nnd <- nndist(clusters$cluster_B$x_loc, 
                                 clusters$cluster_B$y_loc)
# Cluster C
clusters$cluster_C <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$cluster_C) <- c("x_loc", "y_loc", "nnd")
clusters$cluster_C$x_loc <- c(rnorm(n_fish/2, 100, 50), 
                              rnorm(n_fish/2, 300, 50))
clusters$cluster_C$y_loc <- c(rnorm(n_fish/2, 100, 50), 
                              rnorm(n_fish/2, 300, 50))
clusters$cluster_C$nnd <- nndist(clusters$cluster_C$x_loc, 
                                 clusters$cluster_C$y_loc)
# Cluster D
clusters$cluster_D <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$cluster_D) <- c("x_loc", "y_loc", "nnd")
clusters$cluster_D$x_loc <- c(rnorm(n_fish/4, 100, 20), rnorm(n_fish/4, 200, 20),
                              rnorm(n_fish/4, 300, 20), rnorm(n_fish/4, 400, 20))
clusters$cluster_D$y_loc <- c(rnorm(n_fish/4, 100, 20), rnorm(n_fish/4, 200, 20),
                              rnorm(n_fish/4, 300, 20), rnorm(n_fish/4, 400, 20))
clusters$cluster_D$nnd <- nndist(clusters$cluster_D$x_loc, 
                                 clusters$cluster_D$y_loc)



#### Setting up timeseries of 30 years, with 2 versions: random (white noise)
#### and directional shift (south and west)

## Parameters
# Number of years
years <- 30
# Sigma for error in random walk
sigma_rdm <- 5
### Directional shift parameters
alpha_int <- c(-5, -5) 
# x and y values; positive means moving north or east, negative means 
# moving south or west
# Do I want this to have some white noise attached, or for it to be set to 
# the same value throughout the timeseries? I think for clarity, it makes 
# sense to initially have it as a set value rather than drawn from a 
# distribution

# Could use the same sigma as in the random walk, but for now set it 
# separately 
sigma_ds <- 5


### Create timeseries
## List to contain both types of timeseries
timeseries <- list()

# List to contain each clusters' timeseries with random movement
timeseries$rdm <- list()
# Loop to create timeseries for each cluster with random movement
for (clust_type in 1:length(clusters)) {
  timeseries$rdm[[clust_type]] <- list()
  # setting up first year with time 0 locations in cluster list
  timeseries$rdm[[clust_type]][[1]] <- clusters[[clust_type]]
  # Adding fish identifiers for individuals (for plotting)
  timeseries$rdm[[clust_type]][[1]]$fish <- paste0("fish_", c(1:nrow(clusters[[clust_type]])))
  for(year in 2:years) {
    x_year_error <- rnorm(n_fish, 0, sigma_rdm)
    y_year_error <- rnorm(n_fish, 0, sigma_rdm)
    x_loc <- timeseries$rdm[[clust_type]][[year - 1]][, 1] + x_year_error
    y_loc <- timeseries$rdm[[clust_type]][[year - 1]][, 2] + y_year_error
    nnd <- nndist(x_loc, y_loc)
    timeseries$rdm[[clust_type]][[year]] <- as.data.frame(cbind(x_loc, y_loc, nnd))
    timeseries$rdm[[clust_type]][[year]]$fish <- timeseries$rdm[[clust_type]][[1]]$fish
  }
}
names(timeseries$rdm) <- names(clusters)



# List to contain each clusters' timeseries with directional shift movement
timeseries$dir <- list()
# Loop to create timeseries for each cluster with random movement
for (clust_type in 1:length(clusters)) {
  timeseries$dir[[clust_type]] <- list()
  # setting up first year with time 0 locations in cluster list
  timeseries$dir[[clust_type]][[1]] <- clusters[[clust_type]]
  # Adding fish identifiers for individuals (for plotting)
  timeseries$dir[[clust_type]][[1]]$fish <- paste0("fish_", c(1:nrow(clusters[[clust_type]])))
  for(year in 2:years) {
    x_year_error <- rnorm(n_fish, 0, sigma_ds)
    y_year_error <- rnorm(n_fish, 0, sigma_ds)
    x_loc <- timeseries$dir[[clust_type]][[year - 1]][, 1] + alpha_int[1] + x_year_error
    y_loc <- timeseries$dir[[clust_type]][[year - 1]][, 2] + alpha_int[2] + y_year_error
    nnd <- nndist(x_loc, y_loc)
    timeseries$dir[[clust_type]][[year]] <- as.data.frame(cbind(x_loc, y_loc, nnd))
    timeseries$dir[[clust_type]][[year]]$fish <- timeseries$dir[[clust_type]][[1]]$fish
  }
}
names(timeseries$dir) <- names(clusters)




#### Sample timeseries every other year with 2 levels of detection
## Below code needs work; copied from what Mark gave me and doesn't seem
## to work yet


## assumes "catches" occur in 50x50 blocks, 
# Scenarios are: 80% and 20% coverage of the space (with perfect detection 
# in those areas)

### Originally tried 60%, keeping the code just in case I go back to this
#################################################################################
# 60% version: 64 out of 100 50x50 blocks
x_breaks <- sort(sample(seq(0, 450, 50), 8))
y_breaks <- sort(sample(seq(0, 450, 50), 8))

# Using breaks above, get vectors of starting and ending x and y points
x_start <- x_breaks
x_end <- c(x_breaks+50)
x_blocks <- as.data.frame(cbind(x_start, x_end))
y_start <- y_breaks
y_end <- c(y_breaks+50)
y_blocks <- as.data.frame(cbind(y_start, y_end))
#coverage <- as.data.frame(expand.grid(x_breaks, y_breaks))

# Plot of 60% coverage example
plot <- ggplot() +
  geom_point(data = timeseries$rdm$rdm[[1]], aes(x = x_loc, y = y_loc)) +
  annotate("rect", fill = "red", alpha = 0.3, 
             xmin = coverage$x_start[1], xmax = coverage$x_end[1],
             ymin = coverage$y_start[1], ymax = coverage$y_end[5]) +
  annotate("rect", fill = "red", alpha = 0.3, 
           xmin = coverage$x_start[1], xmax = coverage$x_end[1],
           ymin = coverage$y_start[6], ymax = coverage$y_end[7]) +
  annotate("rect", fill = "red", alpha = 0.3, 
           xmin = coverage$x_start[1], xmax = coverage$x_end[1],
           ymin = coverage$y_start[8], ymax = coverage$y_end[8]) +
  
  annotate("rect", fill = "red", alpha = 0.3, 
           xmin = coverage$x_start[2], xmax = coverage$x_end[3],
           ymin = coverage$y_start[1], ymax = coverage$y_end[5]) +
  annotate("rect", fill = "red", alpha = 0.3, 
           xmin = coverage$x_start[2], xmax = coverage$x_end[3],
           ymin = coverage$y_start[6], ymax = coverage$y_end[7]) +
  annotate("rect", fill = "red", alpha = 0.3, 
           xmin = coverage$x_start[2], xmax = coverage$x_end[3],
           ymin = coverage$y_start[8], ymax = coverage$y_end[8]) +
  
  annotate("rect", fill = "red", alpha = 0.3, 
           xmin = coverage$x_start[4], xmax = coverage$x_end[8],
           ymin = coverage$y_start[1], ymax = coverage$y_end[5]) +
  annotate("rect", fill = "red", alpha = 0.3, 
           xmin = coverage$x_start[4], xmax = coverage$x_end[8],
           ymin = coverage$y_start[6], ymax = coverage$y_end[7]) +
  annotate("rect", fill = "red", alpha = 0.3, 
           xmin = coverage$x_start[4], xmax = coverage$x_end[8],
           ymin = coverage$y_start[8], ymax = coverage$y_end[8])

##########################################################################################


## Function for calculating catch 
# in resulting catches matrix, columns are y, rows are x
catch_fun <- function(fish_data, 
                      num_x_blocks, 
                      num_y_blocks,
                      haul_num) {
  # Creating x and y start and end values for surveyed blocks
  x_blocks <- as.data.frame(sort(sample(seq(0, 450, 50), num_x_blocks)))
  colnames(x_blocks) <- "x_start"
  x_blocks$x_end <- x_blocks$x_start + 50
  y_blocks <- as.data.frame(sort(sample(seq(0, 450, 50), num_y_blocks)))
  colnames(y_blocks) <- "y_start"
  y_blocks$y_end <- y_blocks$y_start + 50
  
  # Setting up looping vectors
  x_dims <- nrow(x_blocks)
  y_dims <- nrow(y_blocks)

  ## empty matrix for recording catches
  catches <- matrix(NA, x_dims, y_dims, dimnames = list(paste0(x_blocks$x_start, "-", x_blocks$x_end),
                                                        paste0(y_blocks$y_start, "-", y_blocks$y_end)))
  # empty vectors for recording catch locations
  catches_x_loc <- vector()
  catches_y_loc <- vector()
  
  ## Setting up the total number of hauls & zero catch dataframe
  #(fixed for now , could be randomly drawn in each year)
  total_hauls <- haul_num
  
  ## loop over grid cells for catches
  for(i in 1:x_dims) {
    for(j in 1:y_dims) {
      ## fish within x-limits
      xc <- fish_data$x_loc > x_blocks[i, 1] & fish_data$x_loc <= x_blocks[i, 2]
      ## fish within y-limits
      yc <- fish_data$y_loc > y_blocks[j, 1] & fish_data$y_loc <= y_blocks[j, 2]
      ## fish within both limits
      catches[i, j] <- sum(xc & yc)
      # record fish locations of the catches
      loc_ind <- which(xc == TRUE & yc == TRUE)
      catches_x_loc <- c(catches_x_loc, fish_data$x_loc[loc_ind])
      catches_y_loc <- c(catches_y_loc, fish_data$y_loc[loc_ind])
    }
  }
  # Creating catch location matrix
  catches_loc <- cbind(catches_x_loc, catches_y_loc)
  colnames(catches_loc) <- c("x_loc", "y_loc")
  # Randomly drawing locations that aren't in catches_loc for the rest of
  # the hauls, which will automatically have zero catch
  zero_num <- total_hauls - nrow(catches_loc)
  zero_catches <- as.data.frame(matrix(NA, nrow = zero_num, ncol = 2))
  colnames(zero_catches) <- c("x_loc", "y_loc")
  zero_catches$x_loc <- runif(zero_num, 0, 500)
  zero_catches$y_loc <- runif(zero_num, 0, 500)
  
  return(list(catches_matrix = catches, 
              total_catch = sum(catches),
              catch_locs = catches_loc,
              zero_catches_locs = zero_catches))
}

test <- catch_fun(timeseries$rdm$cluster_A[[1]], 9, 9, 300)
x <- rbind(test$catch_locs[,c(1:2)], test$zero_catches_locs)




catch_eighty <- list()
catch_twenty <- list()
for(year in 1:years) {
  catch_eighty[[year]] <- catch_fun(timeseries$rdm$cluster_A[[year]], 9, 9, 300)
  catch_twenty[[year]] <- catch_fun(timeseries$rdm$cluster_A[[year]], 2, 1, 300)
}
catch_list <- list(catch_eighty, catch_twenty)


