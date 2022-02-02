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


#### case 1: random ####

## assumes nearest neighbor distances ~ Poisson(lambda)

## locations of fish/clusters
rx_loc <- runif(n_fish, 0, x_max)
ry_loc <- runif(n_fish, 0, y_max)


plot(rx_loc, ry_loc, pch = 16, xlim = c(0, 100), ylim = c(0, 100))


#### case 2: aggregated ####

## assumes nearest neighbor distances ~ NegBin(size, mu)

## locations of fish/clusters
ax_loc <- rlnorm(n_fish, 3, 0.7)
ay_loc <- rlnorm(n_fish, 3, 0.7)


plot(ax_loc, ay_loc, pch = 16, xlim = c(0, 100), ylim = c(0, 100))

