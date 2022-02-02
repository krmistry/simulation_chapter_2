## This script simulates various 2D spatial distributions of fish
## for comparing simple random walks and spatio-temporal models
## of catch probability and abundance.

#### setup ####

set.seed(666)

## set (x,y) limits for grid size; min = (0,0)
x_max <- 100
y_max <- 100

## number of fish/clusters
n_fish <- 100

## for calculating nearest neighbor distances
library(spatstat)


#### case 1: random ####

## assumes nearest neighbor distances ~ Poisson(lambda)

## locations of fish/clusters
rdm_x_loc <- runif(n_fish, 0, x_max)
rdm_y_loc <- runif(n_fish, 0, y_max)

## quick plot of locations
plot(rdm_x_loc, rdm_y_loc, pch = 16, xlim = c(0, 100), ylim = c(0, 100))

## calculate nearest neighbor distances
rdm_nnd <- nndist(rdm_x_loc, rdm_y_loc)
hist(rdm_nnd)


#### case 2a: aggregated ####

## assumes nearest neighbor distances ~ NegBin(size, mu)

## locations of fish/clusters as log-normal
agg_x_loc_a <- rlnorm(n_fish, 3, 0.7)
agg_y_loc_a <- rlnorm(n_fish, 3, 0.7)

## quick plot of locations
plot(agg_x_loc_a, agg_y_loc_a, pch = 16, xlim = c(0, 100), ylim = c(0, 100))

## calculate nearest neighbor distances
agg_nnd_a <- nndist(agg_x_loc_a, agg_y_loc_a)
hist(nnd_a)


#### case 2b: aggregated ####

## assumes nearest neighbor distances ~ NegBin(size, mu)

## locations of fish/clusters as normal
agg_x_loc_b <- rnorm(n_fish, 50, 20)
agg_y_loc_b <- rnorm(n_fish, 50, 20)

## quick plot of locations
plot(agg_x_loc_b, agg_y_loc_b, pch = 16, xlim = c(0, 100), ylim = c(0, 100))

## calculate nearest neighbor distances
agg_nnd_b <- nndist(agg_x_loc_b, agg_y_loc_b)
hist(agg_nnd_b)



