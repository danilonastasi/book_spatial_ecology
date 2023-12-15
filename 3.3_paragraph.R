
#####  R. version: 4.3.2 2023/10/31  #####
#####        paragraph 3.3.3       #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

#####    code tested on _________   #####

# We will start with some simple examples using the raster package (for raster data) 
# (Hijmans and Van Etten 2012) and the rgeos package (for vector-based data) 
# (Bivand and Rundall 2017). SDMTools (VanDerWal et al. 2010) provides some more advanced 
# metrics for land-cover quantification that are largely based off of the program Fragstats 
# (McGarigal et al. 2002). lulcc is a package in R that focuses on quantifying LULC
# (Land Use Land Cover) change (Moulds et al. 2015). At the time of publication, 
# landscapemetrics was released for calculating a wide variety of landscape metrics 
#(Hesselbarth et al. 2018), which should also be considered, but it is not covered here.

# To illustrate land-cover variation across landscapes, we return to the land-cover data 
# used in Chap. 2 on scale. Land-cover data comes from the 2011 National Land Cover Database 
# (NLCD) (Homer et al. 2015). We focus primarily on one landscape considered in Chap. 2 
# so that we can more readily visualize and interpret land-cover variation.

# We first load the raster layer being considered and take a look at its attributes, 
# including the thematic resolution being considered, the grain, and extent. In this 
# situation, we reclassified the NLCD layer to simplify the thematic resolution down to 
# six categories (Fig. 3.4): forest, developed, agriculture (rowcrops), grassland, open, 
# and wetlands.


# let's start with the code:
library(raster)
# install.packages("SDMTools") # package not available for this version of R anymore
# library(SDMTools) # we can't use this package

# setting working directory where I stored data from:
# http://​www.​fletcherlab.​com under “Products”
setwd("C:/Users/danil/Documents/UniBo/Spatial Ecology/ebook/Fletcher_Fortin-2018-Supporting_Files/data")   
nlcd <- raster("nlcd2011gv2sr")

#grain and extent
res(nlcd)
extent(nlcd)

#nlcd thematic resolution
levels(nlcd) # it will be NULL

# The resolution is 30 × 30 m and the extent covers approximately 4 × 4 km. With the 
# levels function, we find that initially R did not treat the land-cover data as factors, 
# so we reformat the raster layer to a factor.

# convert land-cover integers to factors
nlcd <- as.factor(nlcd)
levels(nlcd) # now we have attributes ID from 1 to 6

# R is now treating the land-cover categories as factors, but they are only labeled as 
# integer values. For mapping we may want to label the integers based on the land-cover 
# type classifications. We can provide labels and plot the map. To appropriately label 
# the legend, the rasterVis package (Lamigueiro and Hijmans 2018) provides a 
# straightforward approach with the levelplot function:

# add names of land-cover categories to raster
land_cover <- levels(nlcd)[[1]]  # we associate ID numbers
# let's add a column with a vector we create
land_cover[,"landcover"] <- c("forest", "developed", "agriculture", "grassland", "open", "wetland")
levels(nlcd) <- land_cover # we add the new column to nlcd
# plot with custom color scheme

# install.packages("rasterVis")
library(rasterVis)
land_col <- c("green", "orange", "yellow", "brown", "white", "blue")
plot(nlcd, legend = T, col = land_col)
levelplot(nlcd, col.regions = land_col, xlab = "", ylab = "")



