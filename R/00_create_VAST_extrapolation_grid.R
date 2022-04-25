# Creating extrapolation grid 
library(sp) 
library(sf) 

##### In order to VAST to use the simulated data, it needs to have real latitude
##### and longitude values. So I have to convert everything - should I do it
##### before or after. 

## Latitude conversion isn't too bad, it's the same everywhere:
# 500 km = 1.64e+6 ft, and 1 degree = 364,000 ft, 1 minute = 6,068 ft and
# 1 second = 101 ft; so my grid should be approximately:
# 4 degrees, 30 minutes and 19.40594 seconds, or 4.505495 degrees

## Longitude conversion is trickier, because it changes closer to the poles.
## For 60 degrees N, according to https://www.nhc.noaa.gov/gccalc.shtml, 
# 500 km = 9 degrees exactly

# So, if I use the random cluster at time 0, and convert the x and y to long
# and lat, but keeping it a 500 x 500 km square, the corners of the square
# would be: 
# long_points <- c(60, 60, 64.5, 64.5)
# lat_points <- c(-156, -147, -147, -157.5)

# Never mind, I don't know why, but for some reason the longitude is getting
# messed up, so I'm just going to use the equator where I don't have to worry
# about a gradient of change, and which direction it should be going in
long_points <- c(0, 0, 4.5, 4.5)
lat_points <- c(0, 4.5, 4.5, 0)
LL <- list("x" = lat_points,
           "y" = long_points)

# Use this to draw points around your data
# plot(timeseries$rdm$rdm$X1991$x_loc,
#      timeseries$rdm$rdm$X1991$y_loc)
# LL <- locator() - use this to point to parts of a plot to get their coordinates

saveRDS(LL, 'data/extent_LL.rds')
# Read coordinates for area back in
LL <- readRDS('data/extent_LL.rds')
region_extent <- data.frame(long=LL$x, lat=LL$y)
str(region_extent)

#### Turn it into a spatial polygon object
## Need to duplicate a point so that it is connected
region_extent <- rbind(region_extent, region_extent[1,])
## https://www.maths.lancs.ac.uk/~rowlings/Teaching/Sheffield2013/cheatsheet.html
poly <- Polygon(region_extent)
polys <- Polygons(list(poly), ID='all')
sps <- SpatialPolygons(list(polys))
## I think the F_AREA could be dropped here
sps <- SpatialPolygonsDataFrame(sps, data.frame(Id=factor('all'), F_AREA=1, row.names='all'))
proj4string(sps)<- CRS("+proj=longlat +datum=WGS84")
sps <- spTransform(sps, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
### Get UTM zone for conversion to UTM projection
## retrieves spatial bounding box from spatial data [,1] is
## longitude
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL

## Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

### Construct the extroplation grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
# cell_size <- 2000 # creates a 4 x 4 km grid; may have too many boxes for my area
# cell_size <- 4000 # creates a 16 x 16 km grid (GOA grid was usually 13 km^2)
cell_size <- 3500 # model wouldn't converge with 16 km^2, so trying ~12 km^2
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)

## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, Id,
                             Area_km2=( (cell_size/1000)^2),
                             row=1:nrow(region_grid_LL)))
## Filter out the grid that does not overlap (outside extent)
region <- subset(region_df, !is.na(Id))
## This is the final file needed.
str(region)
## > 'data.frame':	106654 obs. of  5 variables:
##  $ Lon     : num  -166 -166 -166 -166 -166 ...
##  $ Lat     : num  53.9 53.9 54 53.9 53.9 ...
##  $ Id      : Factor w/ 1 level "all": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Area_km2: num  4 4 4 4 4 4 4 4 4 4 ...
##  $ row     : int  401 402 975 976 977 978 1549 1550 1551 1552 ...

### Save it to be read in and passed to VAST later.
saveRDS(region, file = "data/user_region.rds")


##########################################################################
### Version of user region for 250x250 square area
long_points <- c(0, 0, 2.25, 2.25)
lat_points <- c(0, 2.25, 2.25, 0)
LL <- list("x" = lat_points,
           "y" = long_points)

saveRDS(LL, 'data/extent_LL_v2.rds')
# Read coordinates for area back in
LL <- readRDS('data/extent_LL_v2.rds')
region_extent <- data.frame(long=LL$x, lat=LL$y)
str(region_extent)

#### Turn it into a spatial polygon object
## Need to duplicate a point so that it is connected
region_extent <- rbind(region_extent, region_extent[1,])
## https://www.maths.lancs.ac.uk/~rowlings/Teaching/Sheffield2013/cheatsheet.html
poly <- Polygon(region_extent)
polys <- Polygons(list(poly), ID='all')
sps <- SpatialPolygons(list(polys))
## I think the F_AREA could be dropped here
sps <- SpatialPolygonsDataFrame(sps, data.frame(Id=factor('all'), F_AREA=1, row.names='all'))
proj4string(sps)<- CRS("+proj=longlat +datum=WGS84")
sps <- spTransform(sps, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
### Get UTM zone for conversion to UTM projection
## retrieves spatial bounding box from spatial data [,1] is
## longitude
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL

## Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

### Construct the extroplation grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
# cell_size <- 2000 # creates a 4 x 4 km grid; may have too many boxes for my area
# cell_size <- 4000 # creates a 16 x 16 km grid (GOA grid was usually 13 km^2)
cell_size <- 3500 # model wouldn't converge with 16 km^2, so trying ~12 km^2
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)

## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, Id,
                             Area_km2=( (cell_size/1000)^2),
                             row=1:nrow(region_grid_LL)))
## Filter out the grid that does not overlap (outside extent)
region <- subset(region_df, !is.na(Id))
## This is the final file needed.
str(region)
## > 'data.frame':	106654 obs. of  5 variables:
##  $ Lon     : num  -166 -166 -166 -166 -166 ...
##  $ Lat     : num  53.9 53.9 54 53.9 53.9 ...
##  $ Id      : Factor w/ 1 level "all": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Area_km2: num  4 4 4 4 4 4 4 4 4 4 ...
##  $ row     : int  401 402 975 976 977 978 1549 1550 1551 1552 ...

### Save it to be read in and passed to VAST later.
saveRDS(region, file = "data/user_region_v2.rds")

