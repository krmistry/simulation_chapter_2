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
abline(h = seq(20, 80, 20), lty = "dashed")
abline(v = seq(20, 80, 20), lty = "dashed")

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
abline(h = seq(20, 80, 20), lty = "dashed")
abline(v = seq(20, 80, 20), lty = "dashed")

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
abline(h = seq(20, 80, 20), lty = "dashed")
abline(v = seq(20, 80, 20), lty = "dashed")

## calculate nearest neighbor distances
agg_nnd_b <- nndist(agg_x_loc_b, agg_y_loc_b)
hist(agg_nnd_b)


#### sample/harvest fish ####

## assumes "catches" occur in 20x20 blocks
x_breaks <- seq(0, 100, 20)
y_breaks <- seq(0, 100, 20)

x_dims <- length(x_breaks) - 1
y_dims <- length(y_breaks) - 1

## empty matrix for recording catches
catches <- matrix(NA, x_dims, y_dims)

## loop over grid cells
for(i in 1:x_dims) {
  for(j in 1:y_dims) {
    ## fish within x-limits
    xc <- agg_x_loc_b > x_breaks[i] & agg_x_loc_b <= x_breaks[i + 1]
    ## fish within y-limits
    yc <- agg_y_loc_b > y_breaks[j] & agg_y_loc_b <= y_breaks[j + 1]
    ## fish within both limits
    catches[i, length(x_breaks) - j] <- sum(xc & yc)
  }
}

## examine catches
t(catches)

## check total to see if some fish were excluded
sum(catches)

