##### Create simulation scenarios

# Loading libraries
library(spatstat) ## for calculating nearest neighbor distances
library(reshape2)
library(ggplot2)
library(dplyr)

#### Scenarios:
## 5 clustering scenarios, 2 ways of moving through 30 years of time, and
## 2 levels of sampling detection with harvest occurring every other year, for
## a total of 20 simulation scenarios

#### Setting up environment parameters
## set (x,y) limits for grid size
x_max <- 250 # 500
y_max <- 250 # 500
## number of fish
n_fish <- 100
## sigmas for clustering
sigma_a <- 50 # 100
sigma_b <- 10 # 20
sigma_c <- 25 # 50
sigma_d <- sigma_b

## number of cluster scenarios & cluster labels
n_clusters <- 5
cluster_names <- c("rdm", paste0("cluster_", c("A", "B", "C", "D")))

## Levels of catch coverage - 80% and 20%
coverage_list <- c("catch_eighty", "catch_twenty")
# Number of total hauls (roughly 3 times the number of fish)
total_hauls <- 300

## Movement types
movement_type <- c("rdm", "dir")

## Parameters
# Number of years
years <- 30
# Setting arbitrary years (for dataframe labeling)
all_years <- paste0("X", c((2020-29):2020))
## survey years (every other year in timeseries)
survey_yrs <- all_years[seq_along(all_years) %% 2 > 0]
names(survey_yrs) <- survey_yrs # so lapply will produced named lists
# Sigma for error to generate stochastic # of total fish in each year
sigma_ts <- 5

# Sigma for error in random walk
sigma_rdm <- 5
# Could use the same sigma as in the random walk, but for now set it 
# separately 
sigma_ds <- 5
### Directional shift parameter
alpha_int <- c(-5, -5) 


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
clusters$rdm$Biomass <- rlnorm(n_fish, 1, 2)*100
# Cluster A
clusters$cluster_A <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$cluster_A) <- c("x_loc", "y_loc", "nnd")
clusters$cluster_A$x_loc <- rnorm(n_fish, x_max/2, sigma_a)
clusters$cluster_A$y_loc <- rnorm(n_fish, y_max/2, sigma_a)
clusters$cluster_A$nnd <- nndist(clusters$cluster_A$x_loc, 
                                 clusters$cluster_A$y_loc)
clusters$cluster_A$Biomass <- rlnorm(n_fish, 1, 2)*100
# Cluster B
clusters$cluster_B <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$cluster_B) <- c("x_loc", "y_loc", "nnd")
clusters$cluster_B$x_loc <- rnorm(n_fish, x_max/2, sigma_b)
clusters$cluster_B$y_loc <- rnorm(n_fish, y_max/2, sigma_b)
clusters$cluster_B$nnd <- nndist(clusters$cluster_B$x_loc, 
                                 clusters$cluster_B$y_loc)
clusters$cluster_B$Biomass <- rlnorm(n_fish, 1, 2)*100
# Cluster C
clusters$cluster_C <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$cluster_C) <- c("x_loc", "y_loc", "nnd")
clusters$cluster_C$x_loc <- c(rnorm(n_fish/2, x_max/3, sigma_c), 
                              rnorm(n_fish/2, 2*(x_max/3), sigma_c))
clusters$cluster_C$y_loc <- c(rnorm(n_fish/2, y_max/3, sigma_c), 
                              rnorm(n_fish/2, 2*(y_max/3), sigma_c))
clusters$cluster_C$nnd <- nndist(clusters$cluster_C$x_loc, 
                                 clusters$cluster_C$y_loc)
clusters$cluster_C$Biomass <- rlnorm(n_fish, 1, 2)*100
# Cluster D
clusters$cluster_D <- as.data.frame(matrix(NA, nrow = n_fish, ncol = 3))
colnames(clusters$cluster_D) <- c("x_loc", "y_loc", "nnd")
clusters$cluster_D$x_loc <- c(rnorm(n_fish/4, x_max/5, sigma_d), rnorm(n_fish/4, 2*(x_max/5), sigma_d),
                              rnorm(n_fish/4, 3*(x_max/5), sigma_d), rnorm(n_fish/4, 4*(x_max/5), sigma_d))
clusters$cluster_D$y_loc <- c(rnorm(n_fish/4, y_max/5, sigma_d), rnorm(n_fish/4, 2*(x_max/5), sigma_d),
                              rnorm(n_fish/4, 3*(y_max/5), sigma_d), rnorm(n_fish/4, 4*(y_max/5), sigma_d))
clusters$cluster_D$nnd <- nndist(clusters$cluster_D$x_loc, 
                                 clusters$cluster_D$y_loc)
clusters$cluster_D$Biomass <- rlnorm(n_fish, 1, 2)*100


#### Setting up timeseries of 30 years, with 2 versions: random (white noise)
#### and directional shift (south and west)


# x and y values; positive means moving north or east, negative means 
# moving south or west
# Do I want this to have some white noise attached, or for it to be set to 
# the same value throughout the timeseries? I think for clarity, it makes 
# sense to initially have it as a set value rather than drawn from a 
# distribution


### Create timeseries
## List to contain both types of timeseries
timeseries <- list()

for(movement in movement_type) {
  # List to contain each clusters' timeseries 
  timeseries[[movement]] <- list()
  # Loop to create timeseries for each cluster 
  for (clust_type in cluster_names) {
    timeseries[[movement]][[clust_type]] <- list()
    # setting up first year with time 0 locations in cluster list
    timeseries[[movement]][[clust_type]][[1]] <- clusters[[clust_type]]
    # Adding fish identifiers for individuals (for plotting)
    #timeseries[[movement]][[clust_type]][[1]]$fish <- paste0("fish_", c(1:nrow(clusters[[clust_type]])))
    if(movement == "rdm") {
      for(year in 2:years) {
        # Generate a new number of fish for each year
        n_fish_new <- round(rnorm(1, n_fish, sigma_ts))
        # Generate new error to move fish with a random walk in each year
        x_year_error <- rnorm(n_fish_new, 0, sigma_rdm)
        y_year_error <- rnorm(n_fish_new, 0, sigma_rdm)
        # If more than fish are drawn than exist in the previous year, create new 
        # starting locations based on existing fish locations, and otherwise the 
        # appropriate sample from the previous year
        if(n_fish_new > nrow(timeseries[[movement]][[clust_type]][[year - 1]])) {
          new_fish <- sample_n(timeseries[[movement]][[clust_type]][[year - 1]], n_fish_new - nrow(timeseries[[movement]][[clust_type]][[year - 1]]))
          timeseries[[movement]][[clust_type]][[year - 1]] <- rbind(timeseries[[movement]][[clust_type]][[year - 1]],
                                                                    new_fish)
        } else if (n_fish_new <= nrow(timeseries[[movement]][[clust_type]][[year - 1]])) { # if less than 100 fish are drawn, sample that many from previous year
          timeseries[[movement]][[clust_type]][[year - 1]] <- sample_n(timeseries[[movement]][[clust_type]][[year - 1]], n_fish_new)
        }
        # Create new locations for each year
        x_loc <- timeseries[[movement]][[clust_type]][[year - 1]][, 1] + x_year_error
        y_loc <- timeseries[[movement]][[clust_type]][[year - 1]][, 2] + y_year_error
        # Calculate nearest neighbor distance for all points
        nnd <- nndist(x_loc, y_loc)
        timeseries[[movement]][[clust_type]][[year]] <- as.data.frame(cbind(x_loc, y_loc, nnd))
        # timeseries[[movement]][[clust_type]][[year]]$fish <- timeseries[[movement]][[clust_type]][[1]]$fish
        timeseries[[movement]][[clust_type]][[year]]$Biomass <- rlnorm(n_fish_new, 1, 2)*100
      }
    } else if (movement == "dir") {
        for(year in 2:years) {# Generate a new number of fish for each year
          n_fish_new <- round(rnorm(1, n_fish, sigma_ts))
          # Generate new error to move fish with with stochasticity while shifting directionally with alpha_int in each year
          x_year_error <- rnorm(n_fish_new, 0, sigma_ds)
          y_year_error <- rnorm(n_fish_new, 0, sigma_ds)
          # If more than fish are drawn than exist in the previous year, create new 
          # starting locations based on existing fish locations, and otherwise the 
          # appropriate sample from the previous year
          if(n_fish_new > nrow(timeseries[[movement]][[clust_type]][[year - 1]])) {
            new_fish <- sample_n(timeseries[[movement]][[clust_type]][[year - 1]], n_fish_new - nrow(timeseries[[movement]][[clust_type]][[year - 1]]))
            timeseries[[movement]][[clust_type]][[year - 1]] <- rbind(timeseries[[movement]][[clust_type]][[year - 1]],
                                                                      new_fish)
          } else if (n_fish_new <= nrow(timeseries[[movement]][[clust_type]][[year - 1]])) { # if less than 100 fish are drawn, sample that many from previous year
            timeseries[[movement]][[clust_type]][[year - 1]] <- sample_n(timeseries[[movement]][[clust_type]][[year - 1]], n_fish_new)
          }
          # Create new locations for each year
          x_loc <- timeseries[[movement]][[clust_type]][[year - 1]][, 1] + alpha_int[1] + x_year_error
          y_loc <- timeseries[[movement]][[clust_type]][[year - 1]][, 2] + alpha_int[2] + y_year_error
          # Calculate nearest neighbor distance for all points
          nnd <- nndist(x_loc, y_loc)
          # Combine locations into dataframe
          timeseries[[movement]][[clust_type]][[year]] <- as.data.frame(cbind(x_loc, y_loc, nnd))
          # timeseries$dir[[clust_type]][[year]]$fish <- timeseries$dir[[clust_type]][[1]]$fish
          # Simulate abundance at each location with log normal distribution
          timeseries[[movement]][[clust_type]][[year]]$Biomass <- rlnorm(n_fish_new, 1, 2)*100
        }
    }
    
    names(timeseries[[movement]][[clust_type]]) <- all_years
  }
}



#### Sample timeseries every other year with 2 levels of detection
## assumes "catches" occur in 50x50 blocks, 
# Scenarios are: 80% and 20% coverage of the space (with perfect detection 
# in the areas where "catch" occurs)



## Function for calculating catch 
# in resulting catches matrix, columns are y, rows are x
############ *** CURRENTLY SAMPLING EVERY YEAR, FIX THIS AT SOME POINT *** 
catch_fun <- function(fish_data, 
                      num_x_blocks, 
                      num_y_blocks,
                      haul_num) {
  # Creating x and y start and end values for surveyed blocks
  x_blocks <- as.data.frame(sort(sample(seq(0, 9*(x_max/10), x_max/10), num_x_blocks)))
  colnames(x_blocks) <- "x_start"
  x_blocks$x_end <- x_blocks$x_start + x_max/10
  y_blocks <- as.data.frame(sort(sample(seq(0, 9*(y_max/10), y_max/10), num_y_blocks)))
  colnames(y_blocks) <- "y_start"
  y_blocks$y_end <- y_blocks$y_start + y_max/10
  
  # Setting up looping vectors
  x_dims <- nrow(x_blocks)
  y_dims <- nrow(y_blocks)

  ## empty matrix for recording catches
  catches <- matrix(NA, x_dims, y_dims, dimnames = list(paste0(x_blocks$x_start, "-", x_blocks$x_end),
                                                        paste0(y_blocks$y_start, "-", y_blocks$y_end)))
  # empty vectors for recording catch locations & biomass
  catches_x_loc <- vector()
  catches_y_loc <- vector()
  catches_biomass <- vector()
  
  ## Setting up the total number of hauls (needed for 0 haul calculation)
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
      catches_biomass <- c(catches_biomass, fish_data$Biomass[loc_ind])
    }
  }
  # Creating catch location matrix
  catches_loc <- as.data.frame(cbind(catches_x_loc, catches_y_loc, catches_biomass))
  colnames(catches_loc) <- c("x_loc", "y_loc", "Biomass")
  # Randomly drawing locations within the whole space for the rest of
  # the hauls, which will automatically have zero catch
  zero_num <- total_hauls - nrow(catches_loc)
  zero_catches <- as.data.frame(matrix(NA, nrow = zero_num, ncol = 3))
  colnames(zero_catches) <- c("x_loc", "y_loc", "Biomass")
  zero_catches$x_loc <- runif(zero_num, 0, x_max)
  zero_catches$y_loc <- runif(zero_num, 0, y_max)
  zero_catches$Biomass <- 0
  # Not currently double checking to be sure I don't have a 0 catch where fish
  # actually are, but they could be missed in the real world anyway, right? So 
  # I'll leave it like this for now
  
  return(list(catches_matrix = catches, 
              total_catch = c("total_biomass" = sum(catches_loc$Biomass),
                              "num_locations" = nrow(catches_loc)),
              all_catch = rbind(catches_loc, zero_catches)))
}

test <- catch_fun(timeseries$dir$cluster_A$X1991, 9, 9, total_hauls)

# Creating catch data for all scenario timeseries in nested list
catchs_fun_outputs_all <- list()
for(movement in movement_type) {
  catchs_fun_outputs_all[[movement]] <- list()
  for(coverage in coverage_list) {
    catchs_fun_outputs_all[[movement]][[coverage]] <- list()
    for(cluster in cluster_names) {
      catchs_fun_outputs_all[[movement]][[coverage]][[cluster]] <- list()
      for(year in survey_yrs) {
        catchs_fun_outputs_all[[movement]]$catch_eighty[[cluster]][[year]] <- catch_fun(timeseries[[movement]][[cluster]][[year]], 9, 9, total_hauls)
        catchs_fun_outputs_all[[movement]]$catch_twenty[[cluster]][[year]] <- catch_fun(timeseries[[movement]][[cluster]][[year]], 2, 1, total_hauls)
      }
    }
  }
}


## Function to format catch data into full timeseries'
catch_formatting_fun <- function(catch_fun_output) {
  catches <- catch_fun_output$all_catch
  return(catches)
}

# Testing with 1 year in 1 scenario
# test <- catch_formatting_fun(catch_dir_list$catch_twenty$rdm$X1991)

# Creating catch timeseries for all scenarios in nested list
survey_timeseries <- list()
for(movement in movement_type) {
  survey_timeseries[[movement]] <- list()
  for(coverage in coverage_list) {
    survey_timeseries[[movement]][[coverage]] <- list()
    for(cluster in cluster_names) {
      survey_timeseries[[movement]][[coverage]][[cluster]] <- list()
      for(year in survey_yrs) {
        survey_timeseries[[movement]][[coverage]][[cluster]][[year]] <- catch_formatting_fun(catchs_fun_outputs_all[[movement]][[coverage]][[cluster]][[year]])
      }
    }
  }
}


########## Save simulation data & variable vectors ############

simulated_data <- list()
simulated_data$timeseries_data <- list("real_timeseries" = timeseries, 
                        "survey_timeseries" = survey_timeseries)

simulated_data$loop_vectors <- list("all_years" = all_years,
                              "survey_yrs" = survey_yrs,
                              "movement_type" = movement_type,
                              "coverage_list" = coverage_list,
                              "cluster_names" = cluster_names,
                              "x_y_dims" = c(x_max, y_max))

saveRDS(simulated_data, file = "data/simulated_data.rds")


#######################################################################################
## Checking how many total catches were made in each year for each scenario
## (may need to adjust the catch coverage if there are too many years with 0s)

total_catches <- list()
for(movement in movement_type) {
  total_catches[[movement]] <- list()
  for(coverage in coverage_list) {
    total_catches[[movement]][[coverage]] <- list()
    for(cluster in cluster_names) {
      catches <- as.data.frame(matrix(NA, nrow = length(survey_yrs), ncol = 2))
      colnames(catches) <- c("Year", "total_catch")
      catches$Year <- gsub("X", "", survey_yrs)
      for(year in 1:length(survey_yrs)) {
        catches$total_catch[year] <-  catchs_fun_outputs_all[[movement]][[coverage]][[cluster]][[survey_yrs[year]]]$total_catch
      }
      total_catches[[movement]][[coverage]][[cluster]] <- catches
    }
  }
}




# For plot labels
movement <- c("Random", "Directional")
coverages <- c("80%", "20%")

catches_check_fun <- function(total_catch_data,
                              movement_type,
                              coverage_type) {
  clusters_catches <- melt(total_catch_data, id.vars = colnames(total_catch_data$rdm))
  colnames(clusters_catches)[3] <- "cluster_type"
  plot <- ggplot(clusters_catches) +
    geom_point(aes(x = Year, y = total_catch, color = cluster_type)) +
    geom_path(aes(x = Year, y = total_catch, group = cluster_type, 
                  color = cluster_type)) +
    labs(title = paste(movement_type, "movement &", 
                       coverage_type, "coverage")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "bottom")
  return(plot)
}

catches_check_plots <- list()
catches_check_plots[[1]] <- catches_check_fun(total_catches$rdm$catch_eighty,
                                             movement[1],
                                             coverages[1])
catches_check_plots[[2]] <- catches_check_fun(total_catches$rdm$catch_twenty,
                                             movement[1],
                                             coverages[2])
catches_check_plots[[3]] <- catches_check_fun(total_catches$dir$catch_eighty,
                                              movement[2],
                                              coverages[1])
catches_check_plots[[4]] <- catches_check_fun(total_catches$dir$catch_twenty,
                                              movement[2],
                                              coverages[2])
  
saveRDS(catches_check_plots, file = "checking_total_catches_plots.rds")
## Having 20% coverage, at least with how I'm coding it above, produces quite a lot 
## of 0s for total catch per year, for most of the cluster scenarios (even 
## random). Might have to try 40% just to get not quite so many 0s, since my
## real data only ever had 1 year with 0 for 1 subregion. Moving forward with this
## for now though, and I'll come back to this after I get the models running




#################################################################################

### Originally tried 60% catch, keeping the code just in case I need anything 
## in here (like the plot, which colored in the boxes that were "caught", 
## although all manually)
# # 60% version: 64 out of 100 50x50 blocks
# x_breaks <- sort(sample(seq(0, 450, 50), 8))
# y_breaks <- sort(sample(seq(0, 450, 50), 8))
# 
# # Using breaks above, get vectors of starting and ending x and y points
# x_start <- x_breaks
# x_end <- c(x_breaks+50)
# x_blocks <- as.data.frame(cbind(x_start, x_end))
# y_start <- y_breaks
# y_end <- c(y_breaks+50)
# y_blocks <- as.data.frame(cbind(y_start, y_end))
# #coverage <- as.data.frame(expand.grid(x_breaks, y_breaks))
# 
# # Number of total hauls (roughly 3 times the number of fish)
# total_hauls <- 300
# 
# # Plot of 60% coverage example
# plot <- ggplot() +
#   geom_point(data = timeseries$rdm$rdm[[1]], aes(x = x_loc, y = y_loc)) +
#   annotate("rect", fill = "red", alpha = 0.3, 
#              xmin = coverage$x_start[1], xmax = coverage$x_end[1],
#              ymin = coverage$y_start[1], ymax = coverage$y_end[5]) +
#   annotate("rect", fill = "red", alpha = 0.3, 
#            xmin = coverage$x_start[1], xmax = coverage$x_end[1],
#            ymin = coverage$y_start[6], ymax = coverage$y_end[7]) +
#   annotate("rect", fill = "red", alpha = 0.3, 
#            xmin = coverage$x_start[1], xmax = coverage$x_end[1],
#            ymin = coverage$y_start[8], ymax = coverage$y_end[8]) +
#   
#   annotate("rect", fill = "red", alpha = 0.3, 
#            xmin = coverage$x_start[2], xmax = coverage$x_end[3],
#            ymin = coverage$y_start[1], ymax = coverage$y_end[5]) +
#   annotate("rect", fill = "red", alpha = 0.3, 
#            xmin = coverage$x_start[2], xmax = coverage$x_end[3],
#            ymin = coverage$y_start[6], ymax = coverage$y_end[7]) +
#   annotate("rect", fill = "red", alpha = 0.3, 
#            xmin = coverage$x_start[2], xmax = coverage$x_end[3],
#            ymin = coverage$y_start[8], ymax = coverage$y_end[8]) +
#   
#   annotate("rect", fill = "red", alpha = 0.3, 
#            xmin = coverage$x_start[4], xmax = coverage$x_end[8],
#            ymin = coverage$y_start[1], ymax = coverage$y_end[5]) +
#   annotate("rect", fill = "red", alpha = 0.3, 
#            xmin = coverage$x_start[4], xmax = coverage$x_end[8],
#            ymin = coverage$y_start[6], ymax = coverage$y_end[7]) +
#   annotate("rect", fill = "red", alpha = 0.3, 
#            xmin = coverage$x_start[4], xmax = coverage$x_end[8],
#            ymin = coverage$y_start[8], ymax = coverage$y_end[8])

##########################################################################################



## Alternate catch, where the timeseries data is used directly, just sampled
## every other year

x <- seq(1, 30, 2)
alt_years <- all_years[x]

alt_catch <- list()
for(movement in movement_type){
  alt_catch[[movement]] <- list()
  for (cluster in cluster_names) {
    alt_catch[[movement]][[cluster]] <- list()
    for(year in alt_years) {
      alt_catch[[movement]][[cluster]][[year]] <- timeseries[[movement]][[cluster]][[year]]
    }
  }
}





