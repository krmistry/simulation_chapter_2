

library(reshape2)

### plotting fish paths through time (sigma = 5 for both random & directional shift)
# random movement with cluster B 
melted_fish_B_rdm <- melt(timeseries$rdm$cluster_B, id.vars = colnames(timeseries$rdm$cluster_B[[1]]))
colnames(melted_fish_B_rdm)[5] <- "Year"

ggplot(melted_fish_B_rdm, aes(x = x_loc, y = y_loc, color = as.factor(fish))) +
  geom_point() +
  geom_path() +
  theme(legend.position = "none")

# directional shift with cluster B
melted_fish_B_dir <- melt(timeseries$dir$cluster_B, id.vars = colnames(timeseries$rdm$cluster_B[[1]]))
colnames(melted_fish_B_dir)[5] <- "Year"

ggplot(melted_fish_B_dir, aes(x = x_loc, y = y_loc, color = as.factor(fish))) +
  geom_point() +
  geom_path() +
  theme(legend.position = "none")

