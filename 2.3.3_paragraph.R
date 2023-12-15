
#####  R. version: 4.3.2 2023/10/31  #####
#####        paragraph 2.3.3      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

#####    code tested on 12/11/2023   #####


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
# format for storing land-cover information:
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

# For these toy landscapes, we can formally ask whether the means and variances change as we 
# increase grain size, as described above. It is straightforward to do mathematical operations 
# on values of raster layers. Here, we contrast means and variances of the original raster to 
# that of the raster where we used the mean values to increase grain size using the cellStats 
# function (results not shown):
cellStats(toy, mean)
cellStats(toy, var)
cellStats(toy_mean, mean)
cellStats(toy_mean, var)
# In this situation, the mean value remains identical (3.417), whereas the variance decreases 
# as we increase the grain size (from 2.82 to 0.86).

# We can reduce the grain size by resampling the data (Fig. 2.6). This can be accomplished with 
# the disaggregate function. Two common approaches are to use a simple disaggregation, 
# which simply replicates values, or using bilinear interpolation, which is based on a 
# distance-weighted average of values in both the x and y directions (hence “bi'” linear):
toy_dis2 <- disaggregate(toy, fact=2)
toy_dis2_bilinear <- disaggregate(toy, fact=2, method='bilinear')
# Bilinear interpolation can be useful when working with continuous data but would not be 
# helpful if data were based on land-cover categories.

# Altering the extent is also straightforward. We can reduce the extent of the map by use 
# of the crop function. To do so, we need to create a new extent for cropping. This can 
# be a simply rectangle of coordinates or we could use a polygon file (e.g., a shapefile). 
# In contrast, the extent can be increased using the extent function. For this toy example, 
# we use new coordinates for changing the extent:
e <- extent(2, 4, 2, 4) #first create new, smaller extent
toy_crop <- crop(toy, e)
plot(toy_crop)
text(toy_crop) # now we have a zoom in of toy in these coordinates (extent xmin: 2 xmax: 4
               # ymin: 2 ymax: 4)

 #increase the extent:
e <- extent(0, 11, 0, 11) #first create new, larger extent
toy_big <- extend(toy, e)
plot(toy_big)
text(toy_big) # it is showed data in toy, in a small window but in a larger raster
# In this case, increasing the extent is not helpful unless we also populate the data in 
# the new extent.

# Simple examples like this one can be generally helpful when starting a new analysis 
# problem in spatial ecology because they can provide a tractable means of understanding 
# what different functions and models do.



















