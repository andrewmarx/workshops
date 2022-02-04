
# First, make sure the working directory is set to the location of this file.
getwd()


# Load the relevant libraries
library("raster")
library("rgdal")
library("rgeos")

#
# Loading shapefiles, plotting, viewing data, manipulating data
#
flma <- readOGR(dsn = "./data/flma", layer = "flma")
flma
plot(flma)

# We can treat the vector data like a data frame
View(flma)
flma$maname
flma$mgrinst
property_list <- flma@data[, c("maname", "mgrinst")]
View(property_list)

plot(flma, col = flma$mgrinst)


# We can change the data
flma$area
flma$area <- "Wait, this isn't area..."
flma$area
flma$area <- area(flma)
flma$area


# We can add new columns
flma$new_column <- "This is a new column"
View(flma)

# We can delete columns
flma <- flma[ ,!(names(flma) %in% c("new_column", "coowners"))]
View(flma)

# We can subset columns
flma <- flma[ , c("area", "perimeter", "maname", "mgrinst")]
View(flma)
plot(flma)


# We can also subset polygons based on the data
flma$mgrinst == "FL DEP Division of Recreation and Parks"
state_parks <- flma[flma$mgrinst == "FL DEP Division of Recreation and Parks", ]
View(state_parks)
plot(state_parks)

state_parks$area/state_parks$perimeter
state_parks$area/state_parks$perimeter>50
state_parks <- state_parks[state_parks$area/state_parks$perimeter>50, ]
View(state_parks)
plot(state_parks)

# We can merge dataframes to the data
site_data <- read.csv("./data/site_data.csv", stringsAsFactors = FALSE)
state_parks <- merge(state_parks, site_data, by.x = "maname", by.y = "site")
View(state_parks)

# Save our modified data to a shapefile
writeOGR(obj = state_parks, dsn = "./output/state_parks", layer = "state_parks", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#
# Loading csv coordinates
#

samples <- read.csv("./data/samples.csv", stringsAsFactors = FALSE)
View(samples)
plot(samples$long, samples$lat, asp = 1)

# Convert to spatial object
coordinates(samples)=~long+lat
View(samples)

# Like above, we can treat the data like a dataframe. Lets merge some external data
sample_data <- read.csv("./data/sample_data.csv", stringsAsFactors = FALSE)
samples <- merge(samples, sample_data, by.x = "sample", by.y = "sample")
View(samples)

# Let's delete the default id column
samples$id <- NULL
View(samples)

# we can save it different ways
write.csv(samples, "./output/samples.csv", row.names = FALSE, quote = FALSE)
writeOGR(obj = samples, dsn = "./output/samples", layer = "samples", driver = "ESRI Shapefile", overwrite_layer = TRUE)



#
# Loading a raster file
#

dem <- raster("./data/dem.tif")
dem
plot(dem)
barplot(table(round(getValues(dem))))

# Get/set specific pixels
dem[3,5]
temp <- dem[3,5]
dem[3,5] <- NA
dem[3,5]
dem[3,5] <- temp
dem[3,5]

# Get a vector of pixel values
dem_vals <- getValues(dem)
dem_vals

# Basic stats
cellStats(dem, stat = 'mean', na.rm = TRUE)
mean(dem_vals)

cellStats(dem, stat = 'max', na.rm = TRUE)
max(dem_vals)

dem <- dem + 10
cellStats(dem, stat = 'max', na.rm = TRUE)

dem <- sqrt(dem)
plot(dem)

dem <- dem^2 - 10
plot(dem)

# Aggregate data
dem_agg <- aggregate(dem, fact=10, fun=mean)
dem
dem_agg
plot(dem_agg)

dem_agg <- aggregate(dem, fact=10, fun=max)
plot(dem_agg)

# Save our modified raster
writeRaster(dem_agg, "./output/dem_agg.tif", format = "GTiff")




#
# Loading multiple raster files
#

bioclim_files <- list.files("./data/worldclim", full.names = TRUE)
bioclim_files

bioclim <- stack(bioclim_files)
bioclim
plot(bioclim)
plot(bioclim[[5]])

# Get values of specific pixel
bioclim[50,50]
bioclim[[5]][50,50]

# Get values at specific coordinate
extract(bioclim, data.frame(x = c(560000), y = c(650000)))

# Get information for all rasters
cellStats(bioclim, stat = 'mean', na.rm = TRUE)

# Aggregate data
bioclim_agg <- aggregate(bioclim, fact=10, fun=mean)
plot(bioclim)
plot(bioclim_agg)

#
# Coordinate reference systems
#

crs(flma)
crs(state_parks)
crs(samples) # Also, look at missing prj file in output
crs(dem)
crs(dem_agg)
crs(bioclim)

# Assigning a missing coordinate system (or replacing an incorrect one)
crs(samples) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(samples)
crs(samples) <- CRS('+init=epsg:4326')
crs(samples)


# Transforming vector data to a new CRS
plot(state_parks)
points(samples) # plotted off screen

state_parks # look at extents
samples # look at extents

temp_samples <- spTransform(samples, CRS('+init=epsg:3086'))
temp_samples
temp_samples <- spTransform(samples, crs(state_parks))
temp_samples
samples

plot(state_parks)
points(temp_samples)

# Transforming raster data
plot(bioclim[[5]])
plot(dem, add = TRUE)

temp_dem <- projectRaster(dem, crs = crs(bioclim[[5]]))

plot(bioclim[[5]])
plot(temp_dem, add = TRUE)

# Be careful when transforming raster data with discrete or categorical values
occ <- raster("./data/occ.tif")
plot(occ)
hist(occ)

temp_occ <- projectRaster(occ, crs = crs(state_parks))
plot(temp_occ)
plot(state_parks, add = TRUE)
hist(temp_occ)

temp_occ <- projectRaster(occ, crs = crs(state_parks), method = "ngb")
plot(temp_occ)
plot(state_parks, add = TRUE)
hist(temp_occ)

writeRaster(temp_occ, "./output/occ_utm.tif", format = "GTiff")

# It might even be an issue with continuous data
temp_dem <- projectRaster(dem, crs = crs(state_parks))
writeRaster(temp_dem, "./output/dem_utm.tif", format = "GTiff")


# Getting everything lined up
clc <- readOGR(dsn = "./data/clc", layer = "clc")


target_crs <- CRS('+init=epsg:3513')


state_parks <- spTransform(state_parks, target_crs)
samples <- spTransform(samples, target_crs)
clc <- spTransform(clc, target_crs)

occ <- projectRaster(occ, crs = target_crs, method = "ngb")
dem <- projectRaster(dem, crs = target_crs)
bioclim <- projectRaster(bioclim, crs = target_crs)

plot(dem)
plot(clc, add = TRUE)
plot(state_parks, add = TRUE, col = state_parks$maname)
points(samples)


#
# Extract data for points
#

extract(dem, samples)
extract(occ, samples)


#
# Get only sites where we have samples
#

sample_loc <- over(samples, state_parks)
sample_loc <- unique(as.character(sample_loc$maname))

sites <- state_parks[state_parks$maname %in% sample_loc, ]
plot(sites)


# Clipping raster to polygon

dem_crop <- crop(dem, sites)
plot(dem_crop)
plot(sites, add=TRUE)

temp_mask <- mask(dem_crop, sites)
plot(temp_mask)
plot(sites, add=TRUE)

sites_buffer <- gBuffer(sites, width = 300, byid = FALSE)
plot(sites)
plot(sites_buffer, add = TRUE, border = "red")

dem_crop <- crop(dem, sites_buffer)
plot(dem_crop)
plot(sites_buffer, add=TRUE)

temp_mask <- mask(dem_crop, sites_buffer)
plot(temp_mask)
plot(sites_buffer, add=TRUE)
plot(sites, add=TRUE, border = "red")

# Can also be done with raster stack
bioclim_crop <- crop(bioclim, sites_buffer)
bioclim_mask <- mask(bioclim_crop, sites_buffer)
plot(bioclim_mask)

plot(bioclim_mask[[5]])
plot(sites_buffer, add=TRUE)
plot(sites, add=TRUE, border = 'red')
