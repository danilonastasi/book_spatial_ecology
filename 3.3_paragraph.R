
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

##### we need some objects we created in the code 2.3.4_paragraph.R #####
##### let's run the R code 2.3.4_paragraph.R totally #####


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
library(rasterVis)  ##### I got some problems and finally I fixed it but I am not sure how ####
land_col <- c("green", "orange", "yellow", "brown", "white", "blue")
plot(nlcd, legend = T, col = land_col)

levelplot(nlcd, col.regions = land_col, xlab = "", ylab = "")  # we associate the new level in legend with the region
##### I got problems with levelplot function, I fixed them but I did not undertand how ####


#####        paragraph 3.3.3.1      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

# To quantify characteristics of patches, the first step is to delineate the patches 
# themselves. This step is not trivial and can have important impacts on the conclusions 
# regarding the effects of patch variation on ecological patterns and processes. For vector 
# maps, typically patches are delineated by the user (e.g., hand digitizing aerial 
# photographs). However, for raster-based maps, we typically automate patch delineation, 
# using one of two common rules: the four-neighbor rule (also known as the “rook’s rule”) 
# and the eight-neighbor rule (also known as the “Queen’s rule”) (Fig. 3.5). Using the 
# four-neighbor rule will invariably result in a greater number of patches and smaller 
# patches than using the eight-neighbor rule. Note in some situations we might want to 
# use a 16-neighbor rule, such as if we would like for patch delineation to account for 
# the potential for gap-crossing by organisms (Bowman and Fahrig 2002), although in 
# practice this is rarely done.

# We focus on identifying patches of forest and interpreting their variation. First, 
# we delineate patches, then we use these delineations to calculate metrics for the 
# patches that describe variation in patch structure. 

# We reclassify the NLCD layer to create a binary layer of forest, akin to the island 
# model of land-cover variation. To do so, we create a reclassification matrix, similar 
# to the approach shown in Chap. 2. In this matrix, the first column is the original 
# land-cover categories and the second column is the new categories:
#create a reclassification matrix
nlcd.cat <- unique(nlcd)
nlcd.cat.for <- c(1, 0, 0, 0, 0, 0)
reclass.mat <- cbind(nlcd.cat, nlcd.cat.for) # new matrix where we associate 1 to ID 1, 
                                             # forest and 0 to the others
#forest binary layer from reclassification matrix
nlcd.forest <- reclassify(nlcd, reclass.mat)  # binary, set value 1 to forest, zero to others
plot(nlcd.forest)

# With this new binary forest layer (Fig. 3.6a) we can delineate forest patches. 
# Both the raster and SDMTools packages have a means to do this, but currently the 
# SDMTools package only allows patch delineation based on an eight-neighbor rule 
# (Fig. 3.5b). Consequently, we will use the raster package to have more flexibility 
# in accomplishing this task using the clump function:

#create patchIDs using clump from raster for 8-neighbor rule
forest.patchID <- clump(nlcd.forest, directions = 8) # error message, we need to install:
# install.packages("igraph")
plot(forest.patchID)

# Note that, similar to cell IDs (see Chap. 2), this function labels patches based 
# on integer values, starting in the northwestern (top left) portion of the map and 
# working down (Fig. 3.5). With this new patch ID layer, we can calculate a variety 
# of patch-based metrics using the PatchStats function in SDMTools :

# for.pstat <- PatchStat(forest.patchID, cellsize = res(nlcd.forest)[[1]]) # error message,
                                                  # we need the SMDTools package but it
                                                # cannot be installed
#### after downloading Rtools43 for Windows:
#### https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html
#### installing it, has been possible to insstall old packages
#### I got the SDMTools old package from the cran archive:
#### https://cran.r-project.org/src/contrib/Archive/SDMTools/
#### downloading the version SDMTools_1.1-221.2.tar.gz 
#### and installing it

###### Important: read the file DESCRIPTION in the zip file and check if 
###### the package needs other packages before installation.
###### The version SDMTools_1.1-221.2.tar.gz is the one it works for my System and R

# install.packages("C:/Users/danil/Downloads/SDMTools_1.1-221.2.tar.gz")
library(SDMTools)
for.pstat <- PatchStat(forest.patchID, cellsize = res(nlcd.forest)[[1]])
# In this function, we pass the length of cells into the cellsize argument to allow for 
# proper calculation of area and length measurements. These calculations are in the units 
# passed to the function; for instance, in the above code, we pass cellsize based on 
# meters, such that area is in m2 and edge is in m. This function automatically calculates 
# many patch-based metrics (Table 3.2) and returns a data frame, where each row is a patch 
# and each column is a metric.

names(for.pstat)

# Summaries of patch metrics can be derived using functions on the data frame. For example, 
# we calculate the number of patches on the map, mean of patch metrics and the standard 
# deviation (SD) of those metrics with simple R commands:

#number of patches
nrow(for.pstat)

#mean patch metrics
for.pstat.mean <- colMeans(for.pstat[,2:ncol(for.pstat)])

#SD of patch metrics
for.pstat.sd <- apply(for.pstat[,2:ncol(for.pstat)], 2, sd)

# The apply function is very flexible in this way. Here it applies functions to the columns 
# of the data based on the second argument in the function (2; note for applying 
# calculations on rows of the data, one would pass 1). Similarly, we can visualize the 
# variation or heterogeneity in metrics, such as the log of patch area. Patch area is often 
# transformed to a log scale for practical reasons: biologically, we expect a change in 
# 10 ha to be more important when contrasting a 5 ha to a 15 ha patch than when contrasting 
# a 1000 ha to 1010 ha patch. For example, a histogram of the distribution of patch areas 
# is straightforward to implement (Fig. 3.6b).

hist(log(for.pstat$area))

# While each of these patch-level metrics captures subtly different aspects of patch 
# structure, many of these metrics are highly correlated (Fig. 3.7). Note that some 
# summaries of patch metrics are also provided when doing a class-level analysis.


#####        paragraph 3.3.3.2      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

# We can also easily quantify patterns of land cover at the class-level. In this case, 
# we can focus on metrics that do not require delineating patches, such as forest area, 
# or we can get summary, patch-based metrics for the entire landscape (such as the 
# standard deviation of patch sizes, as shown above). In any class-level metric, 
# the metrics describe a focal land-cover class and most do not explicitly account for 
# other land-cover types (see landscape-level metrics below). Some exceptions include 
# class-level metrics that focus on edge contrast and interspersion, both of which account 
# for variation in other land-cover types to quantify pattern of a focal land-cover type 
# (see below).

# To calculate class-based metrics, we use the ClassStat function from SDMTools in a 
# similar way as we calculated patch-based metrics:

#calculation based on forest layer
for.cstat <- ClassStat(nlcd.forest, cellsize = res(forest)[[1]])
#calculation based on nlcd layer (all land-cover types)
nlcd.cstat <- ClassStat(nlcd, cellsize = res(nlcd)[[1]])

# When looking over these metrics, several metrics provided are summary statistics 
# for patch-level metrics (e.g., mean, minimum, maximum, and standard deviation of 
# patch size), while others are unique to pattern at the class level (Table 3.3). 
# Looking at the correlations of these metrics across classes in the landscape can help 
# interpret the extent to which metrics are capturing similar elements of class-level 
# structure (Fig. 3.8).

# We can check that these metrics are consistent with calculations based on the data 
# frame provided with the PatchStat function:

#mean patch size
for.cstat[for.cstat$class == 1, "mean.patch.area"]
for.pstat.mean["area"]

#standard deviation of patch shape
for.cstat[for.cstat$class == 1, "sd.shape.index"]
for.pstat.sd["shape.index"]

# The above calculations illustrate how some class-level metrics can be derived directly 
# from patch-level metrics. In summary, the SDMTools package provides several metrics 
# for quantifying land-cover patterns at the patch and class-level, similar to the 
# popular program Fragstats (McGarigal et al. 2002).

# While SDMTools does not calculate core area and isolation metrics, such metrics can be 
# calculated in a straightforward way using other packages in R. First, consider the 
# calculating core areas based on a distance of 100 m. To approach this problem, we 
# convert our raster map to a vector map with the rasterToPolygons function and then 
# buffer within patch polygons using the rgeos package (Fig. 3.9a):

#create polygon layer

#### we need the archived package rgeos ####
# install.packages("https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz")
library(rgeos)
forest.poly <- rasterToPolygons(forest.patchID, dissolve = T) # it works also without rgeos package
#create core polygons and calculate their area
core.poly <- gBuffer(forest.poly, width = -100, byid = T) 
core.area <- gArea(core.poly, byid = T)

