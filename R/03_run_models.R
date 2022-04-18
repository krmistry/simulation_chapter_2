
################################################################################
### Run RE model 

# with all scenarios with all data
for(movement in movement_type) {
  for(coverage in coverage_list) {
    for(cluster in cluster_names) {
      for(sub in 1:length(subregion_labels)) {
        # Putting results folder name in preferred format for write_dat()
        folder <- unname(gsub("/Users/kellymistry/Desktop/Graduate Work/Chapter_2/", 
                              "", results_folders[[movement]][[coverage]][[cluster]][sub]))
        # Compile re.tpl file
        compile_admb(fn = paste0(folder, "/re"), re = TRUE, verbose=TRUE)
        # Run model
        run_admb(fn = paste0(folder, "/re"), verbose = FALSE)
        # Several of the results files are going into the project directory rather than the appropriate subregion/species folder, so this moves them into the appropriate directories
        for(file in 1:length(result_files)) {
          file.rename(here(result_files[file]), paste0(folder, "/", result_files[file]))
        }
      }
    }
  }
}



################################################################################
### Run VAST model

# with test scenario
test_folder <- paste0(results_folders$VAST$rdm$catch_eighty$rdm, "/")

# It was taking an extremely long time to run, so trying it out with epsilons = 0 just
# to be sure that it isn't messing up without giving an error message
settings$RhoConfig[3:4] <- c(0, 0)

fit <- fit_model( "settings"= settings, #all of the settings we set up above
                  "Lat_i"= Data_Geostat[,'Lat'], #latitude of observation
                  "Lon_i"= Data_Geostat[,'Lon'],  #longitude of observation
                  "t_i"= Data_Geostat[,'Year'], #time for each observation
                  "c_i"= rep(0,nrow(Data_Geostat)), #categories for multivariate analyses; don't actually use this, could comment it out if it doesn't have a fit
                  "b_i"= Data_Geostat[,'Catch_KG'], #in kg, raw catch or in CPUE per tow
                  "a_i"= Data_Geostat[,'AreaSwept_km2'], #sampled area for each observation
                  #                 "v_i"= Data_Geostat[,'Vessel'], #ok to leave in because it's all "missing" in data, so no vessel effects
                  "input_grid"= input_grid, #only needed if you have a user input extrapolation grid (which I do)
                  "optimize_args" =list("lower"=-Inf,"upper"=Inf), #TMB argument (?fit_tmb)
                  "working_dir" = test_folder,
                  "run_model" = TRUE)

## Plot results, save in plots folder
plot(fit, working_dir = paste0(test_folder, "Plots/"))

## ##
## Save the VAST model in the results folder
saveRDS(fit, file =  paste0(test_folder,"VASTfit.RDS"))

