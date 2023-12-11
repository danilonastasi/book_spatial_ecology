
##### paragraph 2.3.3  #####

# We will first use simulated data to illustrate some ways in which we can alter 
# the scale of raster data and interpret summaries based on these changes.

# We will then illustrate a simple, multiscale analysis of reptile response to the 
# amount of forest cover in the Southeast USA. These data come from drift fence 
# arrays at 78 forested sites in Alabama, Georgia, and Florida, USA.


######                                    ######
###### let's start with the first example ######
######                                    ######

# raster is a package dedicated to the use of raster (or grid) GIS layers and 
# allows for many types of summaries, analyses, and visualizations.

# First, we create a toy landscape. To do so, we set up an empty raster layer 
# and then populate the empty cells with randomly generated values taken from a 
# Poisson distribution. The Poisson distribution is a discrete probability 
# distribution (i.e., a probability mass distribution) that is relevant for 
# count-based (integer) data, where the data, y, can take on the values of 0, 1, 2, etc. 
# It assumes that the mean equals the variance. By using a Poisson distribution, 
# we will set values of the cells to non-negative integers, which is a common 
# format for storing land-cover information.

library(raster)
set.seed(16) # sets random number seed for repeatability. This allows users to be 
             # able to replicate analyses where random number generators are used
toy <- raster(ncol=6, nrow=6, xmn=1, xmx=6, ymn=1, ymx=6) # create a 6 × 6 raster, 
                                          # specifying the numbers of rows (nrow) 
                                          # and columns (ncol), as well as the minimum 
                                          # and maximum coordinates
values(toy) <- rpois(ncell(toy), lambda=3) # populate the raster by taking random 
                                           # draws (i.e., random deviates) from the 
                                           # Poisson distribution with the rpois function.
ncell(toy)
plot(toy)
text(toy, digits=2)

# In the above code, we generate 36 values (ncell(toy) = 36) from the Poisson distribution, 
# where the mean value = 3.

# Note that when raster populates the raster layer with the Poisson data (rpois), it will 
# start from the top left of the layer and populate right and then down:

ncell(toy)
toy2 <- toy
values(toy2) <- 1:ncell(toy) # create numeric sequence 1-36
plot(toy2)
text(toy2, digits=2)

# Altering the grain of a raster layer is straightforward. We can increase the grain size 
# using the aggregate function (Fig. 2.5). Two common approaches are to: (1) take the mean 
# value of the cells being aggregated; or (2) use a “majority rule,” where we take the most 
# frequent value in the cells being aggregated. We can illustrate each of these approaches as:

toy_mean <- aggregate(toy, fact=2, fun=mean) #mean value
toy_maj <- aggregate(toy, fact=2, fun=modal) #majority rule

# Note that these rules are helpful for different situations. For categorical data 
# (e.g., vegetation types), a majority rule might be helpful because it will aggregate cells 
# based on the most frequent category. For continuous data (e.g., canopy cover), taking the 
# mean value might be more helpful than a majority rule (which would take the mode of the values). 
# In addition, the modal function can be limited when ties are common (no majority value), 
# which will occur more frequently when aggregating fewer cells. 



cellStats(toy, mean)
cellStats(toy, var)
cellStats(toy_mean, mean)
cellStats(toy_mean, var)

toy_dis2 <- disaggregate(toy, fact=2)
toy_dis2_bilinear <- disaggregate(toy, fact=2,
method='bilinear')

e <- extent(2, 4, 2, 4) #first create new, smaller extent
toy_crop <- crop(toy, e)
plot(toy_crop)
text(toy_crop)

#increase the extent
e <- extent(0, 7, 0, 7) #first create new, larger extent
toy_big <- extend(toy, e)
plot(toy_big)
text(toy_big)


##### paragraph 2.3.4 #####

setwd("C:/Users/danil/Documents/UniBo/Spatial Ecology/ebook/Fletcher_Fortin-2018-Supporting_Files/data")   
nlcd <- raster("nlcd2011SE")

proj4string(nlcd) 
# or
st_crs(nlcd) # from sf package, there is a difference

nlcd_proj <- projection(nlcd) 
# or
nlcd_proj <- st.crs(nlcd) # from sf package, tehre is a difference

res(nlcd)

nlcd <- as.factor(nlcd) #convert to factor
levels(nlcd)

# library(rgdal) --> not working anymore, let's change function
library(sf)
library(terra)
# sites <- readOGR("reptiledata") we change readOGR function with read_sf function from package sf
sites <- read_sf("reptiledata") # from package sf
# sites <- vect("reptiledata") # from package terra
class(sites)

summary(sites)

# proj4string(sites) <- nlcd_proj #set projection   # it does not work
st_crs(sites) <- nlcd_proj  # from sf package

head(sites, 2)
sites <- subset(sites, management!="Corn")

x.min <- min(sites$coords_x1)-10000
x.max <- max(sites$coords_x1)+10000
y.min <- min(sites$coords_x2)-10000
y.max <- max(sites$coords_x2)+10000
extent.new <- extent(x.min, x.max, y.min, y.max)
nlcd <- crop(nlcd, extent.new)

#create a new forest layer
forest <- nlcd
values(forest) <- 0
forest[nlcd==41 | nlcd==42 | nlcd==43] <- 1 #forest categorie

#reclassification vector
reclass <- c(rep(0,7), rep(1,3), rep(0,6))

#create reclassification matrix
reclass.mat <- cbind(levels(nlcd)[[1]], reclass)
head(reclass.mat, 3)

forest <- reclassify(nlcd, reclass.mat)

#buffer sites
buf1km <- 1000
buf5km <- 5000
#buffer only first site
# buffer.site1.1km <- buffer(sites[1,], width=buf1km) # it does not work, let's change with st_buffer function 
                                                      # and argument 'dist' instead of 'width'
buffer.site1.1km <- st_buffer(sites[1,], dist=buf1km)
# buffer.site1.5km <- buffer(sites[1,], width=buf5km)
buffer.site1.5km <- st_buffer(sites[1,], dist=buf5km)

#zoom into area for viewing
zoom(nlcd,buffer.site1.5km)
plot(buffer.site1.5km, border="red", lwd = 5, add=T)
plot(buffer.site1.1km, border="red", lwd = 3, add=T)
points(sites[1,], pch=19, cex=2, add=T)

buffer.forest1.1km <- crop(forest, buffer.site1.1km)
buffer.forest1.1km <- mask(forest, buffer.site1.1km)

#area of each cell, in ha
grainarea <- res(forest)[[1]]^2/10000
#area of 1km buffer
bufferarea <- (3.14159*buf1km^2)/10000
#calculation of forest cover and % cover
forestcover1km <- cellStats(buffer.forest1.1km, 'sum') * grainarea
percentforest1km <- forestcover1km / bufferarea * 100

BufferCover <- function(coords, size, landcover, grain){
bufferarea.i <- pi*size^2/10000
coords.i <- SpatialPoints(cbind(coords[i, 1], coords[i, 2]))
buffer.i <- gBuffer(coords.i, width=size)
crop.i <- crop(landcover, buffer.i)
crop.NA <- setValues(crop.i, NA) #for the rasterization
buffer.r <- rasterize(buffer.i, crop.NA) # rasterize buffer
land.buffer <- mask(x=crop.i, mask=buffer.r)
coveramount <- cellStats(land.buffer, 'sum')*grain
percentcover <- 100*(coveramount/bufferarea.i)
return(percentcover)
}

#create empty vector for storing output first
f1km <- vector(NA, length = nrow(sites))
f2km <- vector(NA, length = nrow(sites))
















