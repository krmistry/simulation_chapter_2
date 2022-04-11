###### Format simulated data for each model, RE & VAST ########


### Initially using the full timeseries' (no catch sampling) just to check 
### that it will run in the models properly - will eventually replace this
### with the catch timeseries

# Importing functions from RE_model_v2 project
source("/Users/kellymistry/Desktop/Graduate Work/RE_model_v2/scripts/functions.R")

# Setting up subregions (4 evenly spaced vertical sections)
subregion_dims <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))
colnames(subregion_dims) <- c("x_start", "x_end", "y_start", "y_end", "subregion")
subregion_dims$x_start <- c(0, x_max/4, (x_max/4)*2, (x_max/4)*3)
subregion_dims$x_end <- c(x_max/4, (x_max/4)*2, (x_max/4)*3, x_max)
subregion_dims$y_start <- rep(0, 4)
subregion_dims$y_end <- rep(y_max, 4)
subregion_dims$subregion <- c("one", "two", "three", "four")

# Function to classify each location by subregion
subregion_label_fun <- function(data) {
  for(i in 1:nrow(data)) {
    if(data$x_loc[i] >= subregion_dims$x_start[1] & data$x_loc[i] < subregion_dims$x_end[1]) {
      data$Subregion[i] <- subregion_dims$subregion[1]
    } else if(data$x_loc[i] >= subregion_dims$x_start[2] & data$x_loc[i] < subregion_dims$x_end[2]) {
      data$Subregion[i] <- subregion_dims$subregion[2]
    } else if(data$x_loc[i] >= subregion_dims$x_start[3] & data$x_loc[i] < subregion_dims$x_end[3]) {
      data$Subregion[i] <- subregion_dims$subregion[3]
    } else if(data$x_loc[i] >= subregion_dims$x_start[4] & data$x_loc[i] <= subregion_dims$x_end[4]) {
      data$Subregion[i] <- subregion_dims$subregion[4]
    } else if(data$x_loc[i] < 0 | data$x_loc[i] > 500) {
      data$Subregion[i] <- "outside_region"
    }
  }
  return(data)
}


# Classifying catch data by subregion 
modified_timeseries <- list()
modified_timeseries$rdm <- list()
modified_timeseries$dir <- list()
for (cluster in 1:n_clusters) {
  cluster_rdm_new_list <- list()
  cluster_dir_new_list <- list()
  for(year in 1:years) {
    cluster_rdm_new_list[[year]] <- subregion_label_fun(timeseries$rdm[[cluster]][[year]])
    # Creating 1 metric ton for each location
    cluster_rdm_new_list[[year]]$metric_tons <- 1
    cluster_dir_new_list[[year]] <- subregion_label_fun(timeseries$dir[[cluster]][[year]])
    # Creating 1 metric ton for each location
    cluster_dir_new_list[[year]]$metric_tons <- 1
  }
  modified_timeseries$rdm[[cluster]] <- cluster_rdm_new_list
  names(modified_timeseries$rdm[[cluster]]) <- all_years
  modified_timeseries$dir[[cluster]] <- cluster_dir_new_list
  names(modified_timeseries$dir[[cluster]]) <- all_years
}
names(modified_timeseries$rdm) <- names(clusters)
names(modified_timeseries$dir) <- names(clusters)

## Checking how many fish emigrate, when there are just a static 100 fish 
for(cluster in 1:n_clusters) {
  for(year in 1:years) {
    x <- sum(modified_timeseries$rdm[[cluster]][[year]]$Subregion == "outside_region")
    print(paste0("Cluster ", names(clusters)[cluster], ", ", all_years[year], " - ", x))
  }
}

# Not that many - interestingly, there are more that emigrate in the random 
# scenario than any of the clustered ones (I guess because I centered the 
# clusters in the space initially)


# Calculating subregion estimated totals, means and variances for each year



# Formatting simulation data for RE input - column names required by re.dat_inputs_fun
# is: YEAR, AREA_BIOMASS and BIOMASS_CV
