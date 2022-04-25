## this script contains functions for simulating log-normal catches
## across a user specified grid & then assigning them pseudo geo-locations


## better rounding function
about <- function(x, fun = "round", prec = 1) {
  ## `fun` can be "round", "floor", or "ceiling"
  ## `prec` is nearest value (eg, 0.1 means to nearest tenth); default 1 gives normal behavior
  if(prec <= 0) { stop("\"prec\" cannot be less than or equal to 0") }
  do.call(map, list(x / prec)) * prec
}


## calculate number of schools per grid cell
catch_loc <- function(data, x_res = 25, y_res = x_res) {
  ## x locations (longitude)
  x_loc <- about(data$x_loc, map = "ceiling", prec = x_res) / x_res
  ## y locations (latitude)
  y_loc <- about(data$y_loc, map = "ceiling", prec = y_res) / y_res
  ## schools per grid cell
  tbl <- apply(t(table(data.frame(x_loc, y_loc))), 2, rev)
  return(tbl)
}


## calculate catches/biomass over space
catch_size <- function(data, sd_log = 2) {
  cc <- ceiling(data * apply(data, c(1,2), rlnorm, n = 1, sdlog = sd_log))
  return(cc)
}


## test: random
plot(clusters$rdm$x_loc, clusters$rdm$y_loc, pch = 16, xlim = c(0, 250), ylim = c(0, 250))

c1 <- catch_loc(clusters$rdm)
c1
b1 <- catch_size(c1)
b1
# hist(b1)


## test: cluster A
plot(clusters$cluster_A$x_loc, clusters$cluster_A$y_loc, pch = 16, xlim = c(0, 250), ylim = c(0, 250))

c2 <- catch_loc(clusters$cluster_A)
c2
b2 <- catch_size(c2)
b2
# hist(b2)


## assign pseudo-geolocations to catches
geo_loc <- function(data,
                    lat_min = 55, lat_max = 57, lon_min = 145, lon_max = 147,
                    jitter = TRUE, prec = 20) {
  nx <- ncol(data)
  ny <- nrow(data)
  gg <- matrix(NA, nrow = nx * ny, ncol = 3)
  colnames(gg) <- c("lat", "lon", "catch")
  gg[,"lat"] <- rep(seq(lat_max, lat_min, length.out = ny), times = nx)
  gg[,"lon"] <- rep(seq(lon_max, lon_min, length.out = nx), each = ny)
  gg[,"catch"] <- c(data)
  if(jitter) {
    gg[,"lat"] <- gg[,"lat"] + runif(nx * ny, -1, 1) / prec
    gg[,"lon"] <- gg[,"lon"] + runif(nx * ny, -1, 1) / prec
  }
  return(gg)
}

## example of pseudo-locations
g1 <- geo_loc(b1)
plot(g1[,"lon"], g1[,"lat"], pch = 16)

