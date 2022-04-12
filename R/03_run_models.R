

### Run 

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
