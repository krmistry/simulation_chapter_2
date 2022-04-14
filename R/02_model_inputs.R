###### Format simulated data for each model, RE & VAST ########
library(R2admb)
library(here)
library(VAST)

# Import simulated data & creating objects with shorter names for easier coding
simulated_data <- readRDS("data/simulated_data.rds")
# Vectors for looping
all_years <- simulated_data$loop_vectors$all_years
survey_yrs <- simulated_data$loop_vectors$survey_yrs
movement_type <- simulated_data$loop_vectors$movement_type
coverage_list <- simulated_data$loop_vectors$coverage_list
cluster_names <- simulated_data$loop_vectors$cluster_names
model_type <- c("RE", "VAST")

# Data objects
timeseries <- simulated_data$timeseries_data$real_timeseries
survey_timeseries <- simulated_data$timeseries_data$survey_timeseries
### Hierarchy of survey_timeseries list: (timeseries is the same, but without catch coverage)
# - movement direction (random (rdm) or directional (dir))
# - catch coverage (80% (catch_eighty) or 20% (catch_twenty))
# - cluster types (random (rdm), and cluster A - D)
# - year (every other year starting in 1991 and ending in 2019 (total of 15 years))


# Parameters
x_max <- simulated_data$loop_vectors$x_y_dims[1]
y_max <- simulated_data$loop_vectors$x_y_dims[2]

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

# Setting up list of all results folders
results_folders <- list()
for(model in model_type) {
  results_folders[[model]] <- list()
  for(movement in movement_type) {
    results_folders[[model]][[movement]] <- list()
    for(coverage in coverage_list) {
      results_folders[[model]][[movement]][[coverage]] <- list()
      for(cluster in cluster_names) {
        if(model == "RE") {
          results_folders[[model]][[movement]][[coverage]][[cluster]] <- sapply(subregion_labels,
                                                                                function(x) {
                                                                                  paste0(here("results/"),
                                                                                         model, "/",
                                                                                         movement, "/",
                                                                                         coverage, "/",
                                                                                         cluster, "/",
                                                                                         x)
                                                                                })
          
        } else if(model == "VAST") {
          results_folders[[model]][[movement]][[coverage]][[cluster]] <- paste0(here("results/"),
                                                                                model, "/",
                                                                                movement, "/",
                                                                                coverage, "/",
                                                                                cluster)
        }
        # Check if directories already exist; if they do, don't overwrite
        for(folder in 1:length(results_folders[[model]][[movement]][[coverage]][[cluster]])) {
          if(!dir.exists(results_folders[[model]][[movement]][[coverage]][[cluster]][folder])) {
            dir.create(results_folders[[model]][[movement]][[coverage]][[cluster]][folder], recursive = TRUE)
          } else {
            print("Directories exist")
          }
        }
      }
    }
  }
}



# Classifying survey data by subregion 
modified_survey_ts <- list()
for(movement in movement_type) {
  modified_survey_ts[[movement]] <- list()
  for(coverage in coverage_list) {
    modified_survey_ts[[movement]][[coverage]] <- list()
    for (cluster in cluster_names) {
      modified_survey_ts[[movement]][[coverage]][[cluster]] <- list()
      for(year in survey_yrs) {
        modified_survey_ts[[movement]][[coverage]][[cluster]][[year]] <- subregion_label_fun(survey_timeseries[[movement]][[coverage]][[cluster]][[year]])
      }
    }
  }
}


# Below would require modifying the original timeseries, so would need to add
# that to run this part again
# ## Checking how many fish emigrate, when there are just a static 100 fish 
# for(cluster in 1:n_clusters) {
#   for(year in 1:years) {
#     x <- sum(modified_timeseries$rdm[[cluster]][[year]]$Subregion == "outside_region")
#     print(paste0("Cluster ", names(clusters)[cluster], ", ", all_years[year], " - ", x))
#   }
# }

# Not that many - interestingly, there are more that emigrate in the random 
# scenario than any of the clustered ones (I guess because I centered the 
# clusters in the space initially)

## Melting years data into a single dataframe to make it easier to do subregion 
# calculations, and making years numeric
melted_yrs_survey_ts <- list()
for(movement in movement_type) {
  melted_yrs_survey_ts[[movement]] <- list()
  for(coverage in coverage_list) {
    melted_yrs_survey_ts[[movement]][[coverage]] <- list()
    for(cluster in cluster_names) {
      melted_yrs_survey_ts[[movement]][[coverage]][[cluster]] <- melt(modified_survey_ts[[movement]][[coverage]][[cluster]], id.vars = colnames(modified_survey_ts[[movement]][[coverage]][[cluster]][[1]]))
      colnames(melted_yrs_survey_ts[[movement]][[coverage]][[cluster]])[5] <- "Year"
      melted_yrs_survey_ts[[movement]][[coverage]][[cluster]]$Year <- as.numeric(gsub("X", "", 
                                                                                      melted_yrs_survey_ts[[movement]][[coverage]][[cluster]]$Year))
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
  # Create vector with numeric years
  years <- as.numeric(gsub("X", "", survey_years))
  # Separate out index for one subregion in one year at a time
  for(sub in subregions) {
    sub_year_ind[[sub]] <- lapply(years,  function(x) {
      ind <- which(data$Year == x & data$Subregion == sub) 
      })
    # Creating dataframe to hold yearly estimates for each subregion
    estimates_df <- as.data.frame(matrix(NA, nrow = length(survey_years), ncol = 3))
    colnames(estimates_df) <- c("YEAR", "AREA_BIOMASS", "BIOMASS_CV")
    estimates_df$YEAR <- years
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



# Copy re.tpl file from main project folder into all RE results folders
folders <- unlist(results_folders$RE)
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

# Creating re.dat objects for all scenarios & putting the in appropriate folders
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

# Importing custom extrapolation grid
user_region <- readRDS("data/user_region.rds")
input_grid <- cbind(Lat = user_region$Lat,
                    Lon = user_region$Lon,
                    Area_km2 = user_region$Area_km2)  # Extrapolation grid area is in 
# m^2 & is converted to km^2
gc() 

# min(user_region$Lon)
# [1] -179.9997
# max(user_region$Lon)
# [1] -175.5126
# min(user_region$Lat)
# [1] 0.009034906
# max(user_region$Lat)
# [1] 4.496422

# Creating subregion longitude borders
subregion_borders <- list()
subregion_borders$west <- c(-Inf, (-180 + (x_max/4)/111), (-180 + 2*(x_max/4)/111), (-180 + 3*(x_max/4)/111))
subregion_borders$east <- c((-180 + (x_max/4)/111), (-180 + 2*(x_max/4)/111), (-180 + 3*(x_max/4)/111), Inf)

# Construct subregion strata limits object required for VAST
strata.limits <- data.frame(STRATA = as.factor(subregion_labels),
                            west_border = subregion_borders$west,
                            east_border = subregion_borders$east)

## Spatial & spatiotemp random effects settings - ***I think these stay the same...
FieldConfig <- matrix( c("IID","IID",
                         "IID","IID",
                         "IID","IID"), #"IID" = independent identical distribution
                       ncol=2, nrow=3, 
                       dimnames=list(c("Omega","Epsilon","Beta"), 
                                     c("Component_1","Component_2")) ) 


# Function to convert the original (x,y) coordinates into latitude and longitude
# where: 
# (0, 0) = (-180, 0), 
# (500, 500) = (-175.5, 4.5)
# (0, 500) = (-180, 4.5)
# (500, 0) = (-175.5, 0)
# AND the conversion of degrees to km is: 111 km = 1 degree

lat_long_conversion <- function(data) {
  data$long <- -180 + data$x_loc/111
  data$lat = 0 + data$y_loc/111
  return(data)
}

## Starting with a test run with 1 scenario (random movement, 80% catch,
# random clustering)
test_scenario <- melt(survey_timeseries$rdm$catch_eighty$rdm, id.vars = colnames(survey_timeseries$rdm$catch_eighty$rdm[[1]]))
colnames(test_scenario)[4] <- "Year"
test_scenario$Year <- as.numeric(gsub("X", "", test_scenario$Year))
test_scenario <- lat_long_conversion(test_scenario)

# Create the VAST input Data_Geostat for the test scenario
Data_Geostat <- test_scenario[,-c(1:2)]
colnames(Data_Geostat) <- c("Catch_KG", "Year", "Lon", "Lat")
Data_Geostat$AreaSwept_km2 <- 0.3

# Create RhoConfig object
RhoConfig  <- c("Beta1" = 2, "Beta2" = 2, 
                "Epsilon1" = 2, "Epsilon2" = 2)

# Create VAST settings object - ***BIAS CORRECTING TURNED OFF, TURN BACK ON TO RUN FOR REAL
settings = make_settings(Version = "VAST_v12_0_0", #.cpp version, not software #e.g., "VAST_v12_0_0"
                         n_x = 500, #knots aka spatial resolution of our estimates
                         Region = "User", #Region = "gulf_of_alaska" , go to ?make_settings for other built in extrapolation grids
                         purpose = "index2", #changes default settings
                         ObsModel= c(2, 1),
                         ## everything after this is default if you use purpose = "index2"##
                         FieldConfig = FieldConfig, #spatial & spatiotemp random effects 
                         RhoConfig = RhoConfig, #temporal settings; default is all 0s, but if I specify this it will be changed here
                         strata.limits = strata.limits, #define area that you're producing index for
                         "knot_method" = "grid", #knots in proportion to spatial domain #other option is knot_method="samples"
                         fine_scale = TRUE, #changes the type of interpolation in your extrapolation area
                         bias.correct = FALSE, #corrects the index for nonlinear transformation; I want this for the final version, but I can turn it off while I'm messing with the model so it will run faster
                         use_anisotropy = TRUE) ##correlations decline depend on direction if this argument is TRUE




