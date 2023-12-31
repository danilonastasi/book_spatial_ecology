
#####  R. version: 4.3.2 2023/10/31  #####
#####        paragraph 2.3.4.1       #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited #####


#####    code tested on 12/12/2023   #####


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
# crs(nlcd)
# This projection contains lots of information. Most importantly, aea refers to 
# Albers Equal Area projection. We define the projection so that we can make sure 
# the transect data are considered to be in the same projection as the land-cover data

nlcd_proj <- projection(nlcd) # we assign the projection to nlcd_proj object
# or
# nlcd_proj <- st_crs(nlcd) # from sf package, there is a difference
# nlcd_proj_terra <- crs(nlcd) # different from the first

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
# sites <- readOGR("reptiledata") we change readOGR function with read_sf function from package sf
# sites <- read_sf("reptiledata") # from sf package, sites does not have any projection
# or
library(terra)
sites <- vect("reptiledata") # from terra package but result is different from 
                               # readOGR function, it will be SpatVector
sites <- as(sites, "Spatial") # convert the object in a SpatialPointsDataFrame because
                              # there was an error message in the third row of the function
class(sites)

summary(sites)
# The summary function provides a lot of relevant information. First, it provides the extent 
# of the layer. Notice that this data set includes data collected in eight different land-uses, 
# seven of which are different types of conifer forest, whereas one is corn. It also shows that 
# there are 85 sites (points) and that it does not know what the projection is for the layer. 

proj4string(sites) <- nlcd_proj # set projection, we assign projection to sites
# st_crs(sites) <- nlcd_proj  # from sf package, we assign projection to sites

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
buffer.site1.1km <- buffer(sites[1,], width=buf1km) 
# buffer.site1.1km <- st_buffer(sites[1,], dist=buf1km)
buffer.site1.5km <- buffer(sites[1,], width=buf5km)
# buffer.site1.5km <- st_buffer(sites[1,], dist=buf5km)

# The raster package has a useful function for viewing portions of raster layers. Here we use 
# the zoom function to zoom into the buffer we just created:
#zoom into area for viewing
zoom(nlcd,buffer.site1.5km)
plot(buffer.site1.5km, border="red", lwd = 5, add=T) # zoom function first, plot function after
                                                     # they go together
plot(buffer.site1.1km, border="red", lwd = 3, add=T)
# points(sites[1,], pch=19, cex=2, add=T) # error message, we have to use terra package
points(sites[1,], pch=19, cex=2, add=T) # warning message but it works?

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
coords.i <- SpatialPoints(cbind(coords[i, 1], coords[i, 2])) # see above, row 174
# buffer.i <- gBuffer(coords.i, width=size) # does not work, let's change:
buffer.i <- buffer(coords.i, width=size)  # buffer function from terra package
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

# create empty vector for storing output first
# f1km <- vector(NA, length = nrow(sites)) # error message, let's change with:
f1km <- vector(length = nrow(sites))
# f2km <- vector(NA, length = nrow(sites)) # same error message
f2km <- vector(length = nrow(sites))

for(i in 1:nrow(sites)) {
   f1km[i] <- BufferCover(coords = sites, size = 1000, landcover = forest, grain = grainarea)
   # f1km[i] <- BufferCover(coords = sites, size = 2000, landcover = forest, grain = grainarea) # f1km I think is an error
   f2km[i] <- BufferCover(coords = sites, size = 2000, landcover = forest, grain = grainarea)
   print(i) #print iteration in for loop
}
# make data frame with associated site data
forest.scale <- data.frame(site = sites$site, x = sites$coords_x1, y = sites$coords_x2, 
f1km = f1km, f2km = f2km)

# We then use the above function to calculate the proportion of forest cover at different 
# buffer sizes for all of the points. The above code shows calculations for 1000 and 2000 m, 
# but we also ran 500, 3000, 4000, and 5000 m. In doing so, we find that the percent of forest 
# cover at different scales tends to be highly correlated (Fig. 2.9). This is not surprising, 
# given that calculations at a larger buffer size include area considered at smaller buffer 
# sizes. However, this correlation has implications for the interpretation of scale effects 
# (see below). Note that in R (and in other GIS), as the buffer increases, the computation 
# time also increases. See a recent package in R, spatialEco (Evans 2017), for similar 
# functionality regarding calculating landscape metrics surrounding points.

# We can also repeat this process for different grains by using the aggregate function to 
# coarsen the map. Why might we want to do this? A primary reason is to translate the map 
# to a resolution of data being collected in the field that we are using for making inferences. 
# In this case, we are considering data collected along two, 200 × 100 m transects within 
# forest patches, or 4 ha. If we wish to make predictions of species–environment relationships, 
# we may want our map grain to reflect the sampling grain. Consequently, we would want the 
# map to have an approximate 200 × 200 m grain. We can do this as:
# changing the grain
forest200 <- aggregate(forest, fact=7, fun=modal)


#####                                #####
#####        paragraph 2.3.4.2       #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited #####

#####     code tested on 12/14/2023  #####

# first load the reptile data on southeastern five-lined skinks (FLSK) and merge it to our 
# summaries of forest cover calculated at different scales:
flsk <- read.csv("reptiles_flsk.csv", header=T)
flsk <- merge(flsk, forest.scale, by = "site", all = F)

# Generalized linear models, like logistic regression, can be then be implemented in R 
# with the glm function . For instance, a logistic regression model with forest cover 
# calculated at the 1 km scale can be fit as:
pres.1km <- glm(pres ~ f1km, family = "binomial", data = flsk)
logLik(pres.1km)

nll <- function(par, cov, y) {
   alpha <- par[1]
   beta <- par[2]
   lp <- alpha + beta*cov #linear predictor
   p <- plogis(lp) #back-transform
   loglike <- -sum(y*log(p) + (1-y)*log(1-p)) #negative ∣∣
   return(loglike)
}

#fit logistic model
lr.buffer <- optim(par = c(0, 0), fn = nll, cov = flsk$f2km, y = flsk$pres, hessian = T)
lr.buffer$par

lr.buffer.vc <- solve(lr.buffer$hessian) #var−cov matrix
lr.buffer.se <- sqrt(diag(lr.buffer.vc)) #SE
lr.buffer.se

pres.2km <- glm(pres ~ f2km, family = "binomial", data = flsk)

summary(pres.2km)$coefficients

nll.kernel <- function(par, D, cov, y) {
   sig <- exp(par[1]) #ensures sig > 0
   alpha <- par[2]
   beta <- par[3]
   cov.w <- apply(D, 1, function(x) {
     w0 <- exp(-x^2 / (2 * sig^2)) #Gaussian kernel
     w0[w0==1] <- 0 #for truncated data
     w <- w0/sum(w0) #kernel weights; sums to 1
     sum(cov * w) #weighted average of raster
     })
   lp <- alpha + beta * cov.w #linear predictor
   p <- plogis(lp) #back-transform
   loglike <- -sum(y*log(p) + (1-y)*log(1-p)) #nll
   return(loglike)
}

for200.df <- data.frame(rasterToPoints(forest200))
library(fields)
D <- rdist(as.matrix(flsk[,c("x","y")]), as.matrix(for200.df[,c("x","y")]))

library(Matrix)
D <- D/1000 #in km
D[D > 10] <- 0 #truncate to only consider max dist
D <- Matrix(D, sparse = TRUE)

cov.subset <- which(colSums(D)!=0, arr.ind = T)
D <- D[, cov.subset]

lr.kernel <- optim(fn = nll.kernel, hessian = T, par = c(0,-6,8), D = D, 
   cov = for200.df$layer[cov.subset], y = flsk$pres)
lr.kernel$par

AICkernel <- 2 * lr.kernel$value + 2 * length(lr.kernel$par)
AICbuffer <- 2 * lr.buffer$value + 2 * length(lr.buffer$par)
c(AICkernel, AICbuffer)

