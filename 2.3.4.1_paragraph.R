
##### paragraph 2.3.4.1 #####

##### All data and R code can be downloaded at the first author’s website #####
# ( http://​www.​fletcherlab.​com under “Products”) 
# and at the University of Florida’s Institutional Repository 
# ( http://​ufdc.​ufl.​edu/​ufirg )

# setting working directory where I stored data from:
# http://​www.​fletcherlab.​com under “Products”
setwd("C:/Users/danil/Documents/UniBo/Spatial Ecology/ebook/Fletcher_Fortin-2018-Supporting_Files/data")   

# We can read the land-cover data with the raster function:
library(raster)
nlcd <- raster("nlcd2011SE") # nlcd2011SE is a folder

proj4string(nlcd) 
# or
library(sf)
st_crs(nlcd) # from sf package, there is a difference
# This projection contains lots of information. Most importantly, aea refers to 
# Albers Equal Area projection. We define the projection so that we can make sure 
# the transect data are considered to be in the same projection as the land-cover data

nlcd_proj <- projection(nlcd) # we assign the projection to nlcd_proj object
# or
# nlcd_proj <- st_crs(nlcd) # from sf package, there is a difference

# We can inspect other aspects of the raster layer as well, including the resolution 
# (grain size) , extent, and number of cells, with the res, extent, and ncell functions. 
# For example:
res(nlcd)
extent(nlcd)
ncell(nlcd)

# As expected, we find that the grain of the land-cover data is 30 × 30 m. Note 
# that R does not consider the land-cover data as categorical, which can be shown 
# with is.factor(nlcd). So, we convert the layer to be considered categorical with as.factor 
# and inspect the number of categories of land cover with levels 
# (note output is suppressed here):
nlcd <- as.factor(nlcd) #convert to factor
levels(nlcd)
# There are 16 categories of land cover, with labels reflecting the NLCD IDs. For example, 
# deciduous forest is ID = 41.
# We read in the transect data (a SpatialPointsDataFrame object, which can be determined 
# with the class function) with the readOGR function (from rgdal package, is not working anymore)

# library(rgdal) --> not working anymore, let's change package and function
# library(terra)
# sites <- readOGR("reptiledata") we change readOGR function with read_sf function from package sf
sites <- read_sf("reptiledata") # from sf package, sites does not have any projection
# or
# sites <- vect("reptiledata") # from terra package
class(sites)

summary(sites)
# The summary function provides a lot of relevant information. First, it provides the extent 
# of the layer. Notice that this data set includes data collected in eight different land-uses, 
# seven of which are different types of conifer forest, whereas one is corn. It also shows that 
# there are 85 sites (points) and that it does not know what the projection is for the layer. 

# proj4string(sites) <- nlcd_proj #set projection   # it does not work
st_crs(sites) <- nlcd_proj  # from sf package, we assign projection to sites

# We can call the SpatialPointsDataFrame using the generic functions. For example:
head(sites, 2)

# We use the subset function that is often used on data frames to remove the corn land use:
sites <- subset(sites, management!="Corn")

# With this subset, we work with 78 sites (nrow(sites)). Next, we crop the nlcd layer 
# to make the extent only be 10 km beyond the sampling transects to increase computing speed:
# define reduced extent
x.min <- min(sites$coords_x1)-10000
x.max <- max(sites$coords_x1)+10000
y.min <- min(sites$coords_x2)-10000
y.max <- max(sites$coords_x2)+10000
extent.new <- extent(x.min, x.max, y.min, y.max)
nlcd <- crop(nlcd, extent.new)

# To simplify our consideration of scaling issues, we reclassify the nlcd layer into a binary 
# forest/non-forest layer. This task can be accomplished in at least two ways. First, we could 
# reclassify land cover categories (pooling different forest land-cover types) using some 
# generic R commands to create a new layer that captures the forest cover across the 
# study region. To do so, we create a map of the same grain and extent, and then we can reset 
# values of the map. In this case, we want to pool land-cover categories 41, 42, and 43 
# (Deciduous, Evergreen, and Mixed Forest, respectively):
# create a new forest layer
forest <- nlcd # assign nlcd raster to forest with all the same values
values(forest) <- 0 # we assign zero to all values
forest[nlcd==41 | nlcd==42 | nlcd==43] <- 1 # forest categories | == or

# Note how with the raster package we can easily reclassify land-cover using simple operations 
# in R similar to other operations for vectors and matrices (Appendix). In this situation, 
# we are populating our new forest raster, which is initially set to all 0 values, as 1 if the 
# nlcd layer at a location is either 41, 42, or 43 (using the OR statement, | ). The result 
# is a new raster where forest values are 1 and all other values are 0. 

# Alternatively, we can use the reclassify function in the raster package to do the same 
# operation, which is much quicker computationally. This function requires creating a matrix 
# where the first column is the original land-cover categories and the second provides 
# information on the reclassification categories. In this case we need to make sure that 
# in the second column, there are all zeros except for the rows representing nlcd 
# categories 41, 42, and 43 (conifer, deciduous and mixed forests). In levels(nlcd)[[1]], 
# we find that IDs 41–43 are the 8th–10th values in the vector. Consequently, we can 
# create a reclassification vector as: 
# reclassification vector
reclass <- c(rep(0,7), rep(1,3), rep(0,6))
# We then create a reclassification matrix and reclassify the nlcd layer with the 
# reclassify function:
# create reclassification matrix
reclass.mat <- cbind(levels(nlcd)[[1]], reclass)
head(reclass.mat, 3)

forest <- reclassify(nlcd, reclass.mat)

# We then take point coordinates of sample locations and calculate the amount of forest 
# that surrounds each sampling location at different extents. To do so, we set the local 
# extents to 1000 and 5000 m (Fig. 2.8). We can then use the buffer function to create 
# circular buffers of different extents surrounding the sites. We consider the first site 
# (sites[1, ]) to illustrate and then use the same logic to apply to all sites:
# buffer sites
buf1km <- 1000
buf5km <- 5000
#buffer only first site
# buffer.site1.1km <- buffer(sites[1,], width=buf1km) # it does not work, let's change with st_buffer function 
                                                      # and argument 'dist' instead of 'width'
buffer.site1.1km <- st_buffer(sites[1,], dist=buf1km)
# buffer.site1.5km <- buffer(sites[1,], width=buf5km)
buffer.site1.5km <- st_buffer(sites[1,], dist=buf5km)

# The raster package has a useful function for viewing portions of raster layers. Here we use 
# the zoom function to zoom into the buffer we just created:
#zoom into area for viewing
zoom(nlcd,buffer.site1.5km)
plot(buffer.site1.5km, border="red", lwd = 5, add=T) # zoom function first, plot function after
                                                     # they go together
plot(buffer.site1.1km, border="red", lwd = 3, add=T)
# points(sites[1,], pch=19, cex=2, add=T) # error message, we have to use terra package
library(terra)
points(sites[1,], pch=19, cex=2, add=T) 

# How can we extract appropriate information at different scales? Let us focus on this 
# first site. Once we can capture the information we need for one point, we then repeat 
# for all sites. There are several ways to accomplish this task. The simplest way is to 
# take the buffered layer we just created and use the crop and mask functions:
buffer.forest1.1km <- crop(forest, buffer.site1.1km)
buffer.forest1.1km <- mask(forest, buffer.site1.1km)

# Extracting the forest area is straightforward from here. Given that the map is a binary 
# map of forest cover, we can use the cellStats function in the raster package to sum the 
# amount of forest cover (sum the 1’s for each cell to obtain the total number of forest cells). 
# We can then multiple this number by the grain to get the forest area and divide by the buffer 
# size to get the proportion of forest:
#area of each cell, in ha
grainarea <- res(forest)[[1]]^2/10000
#area of 1km buffer
bufferarea <- (3.14159*buf1km^2)/10000
#calculation of forest cover and % cover
forestcover1km <- cellStats(buffer.forest1.1km, 'sum') * grainarea
percentforest1km <- forestcover1km / bufferarea * 100

# That’s it! Now to repeat this for all of the points, we use a for loop, where we iteratively 
# go through all points, calculating buffers, extracting forest area for each buffer, and then 
# append the proportion of forest area for each point (and/or buffer size) for each sample 
# location. To do this more efficiently for so many points, we will use the rasterize function 
# to convert the buffer into a raster layer, which can be computationally quicker than not 
# rasterizing the buffer. We make a generic function that can then be used to automate all 
# of the steps for a given point. In this function, we first crop the layer to the buffer so 
# that we can work on a smaller extent, then we create an empty raster that we use for 
# rasterizing the buffer. With that new layer, we can use the mask function to create a new 
# raster that only includes forest cover within the buffer:
# let's create a function:
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
# So this function requires x–y locations of a point, the buffer distance of interest (size), 
# a binary land cover raster layer, and the grain area of the map (note the latter could build 
# into the function, but it would recalculate grain area each iteration, which is not necessary 
# for this example). We use this function, nesting it in a for loop, to calculate forest cover 
# for all the points:

#create empty vector for storing output first
f1km <- vector(NA, length = nrow(sites))
f2km <- vector(NA, length = nrow(sites))
