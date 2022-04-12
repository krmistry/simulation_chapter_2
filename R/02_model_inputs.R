###### Format simulated data for each model, RE & VAST ########

library(R2admb)

# Importing functions from RE_model_v2 project
source("/Users/kellymistry/Desktop/Graduate Work/RE_model_v2/scripts/functions.R")

# Setting subregion labels
subregion_labels <- c("one", "two", "three", "four")

# Setting up subregions limits (4 evenly spaced vertical sections)
subregion_dims <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))
colnames(subregion_dims) <- c("x_start", "x_end", "y_start", "y_end", "subregion")
subregion_dims$x_start <- c(0, x_max/4, (x_max/4)*2, (x_max/4)*3)
subregion_dims$x_end <- c(x_max/4, (x_max/4)*2, (x_max/4)*3, x_max)
subregion_dims$y_start <- rep(0, 4)
subregion_dims$y_end <- rep(y_max, 4)
subregion_dims$subregion <- subregion_labels

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


subregion_label_fun(survey_timeseries$rdm$catch_eighty$rdm$X1991)
### Hierarchy of survey_timeseries list:
# - movement direction (random (rdm) or directional (dir))
# - catch coverage (80% (catch_eighty) or 20% (catch_twenty))
# - cluster types (random (rdm), and cluster A - D)
# - year (every other year starting in 1991 and ending in 2019 (total of 15 years))

# Classifying survey data by subregion 
modified_survey_ts <- list()
modified_survey_ts$rdm <- list()
modified_survey_ts$dir <- list()
for(coverage in 1:2) {
  modified_survey_ts$rdm[[coverage_list[coverage]]] <- list()
  modified_survey_ts$dir[[coverage_list[coverage]]] <- list()
  for (cluster in cluster_names) {
    cluster_rdm_new_list <- list()
    cluster_dir_new_list <- list()
    for(year in survey_yrs) {
      cluster_rdm_new_list[[year]] <- subregion_label_fun(survey_timeseries$rdm[[coverage]][[cluster]][[year]])
      cluster_dir_new_list[[year]] <- subregion_label_fun(survey_timeseries$dir[[coverage]][[cluster]][[year]])
    }
    modified_survey_ts$rdm[[coverage]][[cluster]] <- cluster_rdm_new_list
    modified_survey_ts$dir[[coverage]][[cluster]] <- cluster_dir_new_list
  }
}


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

## Melting years data into a single dataframe to make it easier to do subregion calculations
melted_years_list <- list()
for(movement in movement_type) {
  melted_years_list[[movement]] <- list()
  for(coverage in coverage_list) {
    melted_years_list[[movement]][[coverage]] <- list()
    for(cluster in cluster_names) {
      melted_years_list[[movement]][[coverage]][[cluster]] <- melt(modified_survey_ts[[movement]][[coverage]][[cluster]], id.vars = colnames(modified_survey_ts[[movement]][[coverage]][[cluster]][[1]]))
      colnames(melted_years_list[[movement]][[coverage]][[cluster]])[5] <- "Year"
    }
  }
}

## Calculating subregion estimated totals, means and variances for each year
# Equation is: (catch within subregion/area surveyed within subregion)*
# total subregion area

# Input for this function will be melted years dataframe
sub_calculations_fun <- function(data, 
                                     subregions,
                                     survey_years) {
  # All subregions are the same area, which is:
  subregion_area <- (x_max/4)*y_max
  # Create list to hold each subregions estimates
  sub_estimates <- list()
  # Create list to hold subregion & year row indices
  sub_year_ind <- list()
  # Separate out index for one subregion in one year at a time
  for(sub in subregions) {
    sub_year_ind[[sub]] <- lapply(survey_years,  function(x) {
      ind <- which(data$Year == x & data$Subregion == sub) 
      })
    # Creating dataframe to hold yearly estimates for each subregion
    estimates_df <- as.data.frame(matrix(NA, nrow = length(survey_years), ncol = 3))
    colnames(estimates_df) <- c("YEAR", "AREA_BIOMASS", "BIOMASS_CV")
    estimates_df$YEAR <- as.numeric(gsub("X", "", survey_years))
    for(year in 1:length(survey_years)) {
      # Separate data by subregion & year
      sub_year_data <- data[sub_year_ind[[sub]][[year]],]
      # Separate out catch from that subregion
      sub_year_catch <- sub_year_data$metric_tons
      # Calculate area surveyed, assuming 0.3 km^2 covered in each haul
      area_surveyed <- nrow(sub_year_data)*0.3
      # Calculate estimated biomass for subregion
      estimates_df$AREA_BIOMASS[year] <- (sum(sub_year_catch)/area_surveyed)*subregion_area
      # Changing 0s to 0.1 (because RE model breaks with 0 biomass)
      if(estimates_df$AREA_BIOMASS[year] == 0) {
        estimates_df$AREA_BIOMASS[year] <- 0.1
      }
      # Calculating mean and variance for catches in this subregion
      data_mean <- mean(sub_year_data$metric_tons)
      data_var <- var(sub_year_data$metric_tons)
      # Checking for 0s that will produce NAs for CV
      if(data_mean == 0) {
        data_mean <- 0.1
      }
      if(data_var == 0) {
        data_var <- 1e-04
      }
      # Calculating CV
      estimates_df$BIOMASS_CV[year] <- sqrt(data_var)/mean(data_mean)
      sub_estimates[[sub]] <- estimates_df
    }
  }
  return(sub_estimates)
}


# Creating subregion calculations required for RE model
sub_calc_data <- list()
for(movement in movement_type) {
  sub_calc_data[[movement]] <- list()
  for(coverage in coverage_list) {
    sub_calc_data[[movement]][[coverage]] <- list()
    for(cluster in cluster_names) {
      sub_calc_data[[movement]][[coverage]][[cluster]] <- sub_calculations_fun(melted_years_list[[movement]][[coverage]][[cluster]], 
                           subregion_labels,
                           survey_yrs)
    }
  }
}

# Setting up list of all results folders
results_folders <- list()
for(movement in movement_type) {
  results_folders[[movement]] <- list()
  for(coverage in coverage_list) {
    results_folders[[movement]][[coverage]] <- list()
    for(cluster in cluster_names) {
      results_folders[[movement]][[coverage]][[cluster]] <- sapply(subregion_labels,
                                                                   function(x) {
                                                                     paste0(here("results/"), 
                                                                            movement, "/",
                                                                            coverage, "/",
                                                                            cluster, "/",
                                                                            x)
                                                                   })
      for(folder in 1:length(subregion_labels)) {
        if(!dir.exists(results_folders[[movement]][[coverage]][[cluster]][folder])) {
          dir.create(results_folders[[movement]][[coverage]][[cluster]][folder], recursive = TRUE)
        } else {
          print("Directories exist")
        }
      }
    }
  }
}


# Copy re.tpl file from main project folder into all results folders
folders <- unlist(results_folders)
for(folder in 1:length(folders)) {
  file.copy(from = here("re.tpl"), 
            to = folders[folder], 
            overwrite = TRUE)
}


# ADMB output objects that are getting saved into the main project folder instead of the 
# appropriate results folder and therefore need to be moved
result_files <- c("rwout.rep", 
                  "admodel.cov", 
                  "admodel.dep", 
                  "admodel.hes", 
                  "hesscheck", 
                  "hessian.bin", 
                  "fmin.log")

# Formatting simulation data for RE input & running RE model
# - column names required by re.dat_inputs_fun is: 
#   -- YEAR, AREA_BIOMASS and BIOMASS_CV

# Set start and end year variables
start_yr <- as.numeric(gsub("X", "", survey_yrs[1]))
end_yr <- as.numeric(gsub("X", "", survey_yrs[length(survey_yrs)]))

for(movement in movement_type) {
  for(coverage in coverage_list) {
    for(cluster in cluster_names) {
      for(sub in 1:length(subregion_labels)) {
        # Creating re.dat object
        re.dat <- re.dat_inputs_fun(sub_calc_data[[movement]][[coverage]][[cluster]][[sub]], 
                                    start_yr, end_yr)
        # Putting results folder name in preferred format for write_dat()
        folder <- unname(gsub("/Users/kellymistry/Desktop/Graduate Work/Chapter_2/", 
                              "", results_folders[[movement]][[coverage]][[cluster]][sub]))
        # Saving as re.dat into appropriate results folder
        write_dat(paste0(folder, "/re"), 
                  L = re.dat)
      }
    }
  }
}



# Importing functions from VAST project
source("/Users/kellymistry/Desktop/Graduate Work/groundfish_VAST/scripts/01_functions.R")

