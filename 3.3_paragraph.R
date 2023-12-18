
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

##### we need the object "forest" we created in the code 2.3.4_paragraph.R #####
##### let's run the R code 2.3.4_paragraph.R until row: 127 #####


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

#### we need the archived package rgeos, we install it thanks to Rtools43 ####
# install.packages("https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz")
library(rgeos)
forest.poly <- rasterToPolygons(forest.patchID, dissolve = T) # it works also without rgeos package
#create core polygons and calculate their area
core.poly <- gBuffer(forest.poly, width = -100, byid = T) 
core.area <- gArea(core.poly, byid = T)

# This general approach is useful for delineating core areas of different sizes; however, 
# it may be computationally slow for large landscapes. In addition, in many situations we 
# may want to map more detail in edge effects , such as using the “effective area model” 
# (Sisk et al. 1997). The effective area model maps edge response functions, such as 
# variation in abundance as a function of distance from edge, for different edge types. 
# Such models can be implemented by creating raster layers based on distances to boundaries. 
# For instance, we can use the distance function in the raster package to create a new 
# raster layer that shows the distance to edge. To implement this approach, we reformat 
# our forest layer, such that forest is NA. This function will then calculate the nearest 
# distance from each non-NA pixel to each NA pixel:

#re-format raster
nlcd.forestNA <- nlcd.forest
nlcd.forestNA[nlcd.forestNA == 1] <- NA
#create a distance to edge raster for forest land cover
forest.dist <- raster::distance(nlcd.forestNA)

# Much more information is provided with this new raster layer (Fig. 3.9b). Note that in 
# the above code, we specified the raster package in the call of the distance function 
# (raster::distance), because the SDMTools package also has a different distance function.

# Isolation-related patch metrics can be quantified using distances based on patch centroids 
# or edge–edge distances. Both of these types of distance metrics can be calculated with 
# the rgeos package:

#centroids of polygons
forest.centroid <- gCentroid(forest.poly, byid = T)
#edge-edge distance matrix
edge.dist <- gDistance(forest.poly, byid = T)
#centroid-centroid distance matrix
cent.dist <- gDistance(forest.centroid, byid = T)

# With these distance matrices, we can use the apply function to calculate the nearest 
# neighbor distances, or the minimum distance from one patch to any other patch in on the 
# map. To do so, we first make the diagonal of the distance matrix NA, so that we ignore 
# the diagonal (the focal patch, for which distance = 0) when summarizing information 
# regarding other patches on the map. We can then use the apply function to identify the 
# minimum distance to another patch:

#patch-level nearest-neighbor distance
diag(cent.dist) <- NA
diag(edge.dist) <- NA
nnd.cent <- apply(cent.dist, 1, min, na.rm = T)
nnd.edge <- apply(edge.dist, 1, min, na.rm = T)

# Note here that using edge–edge (nnd.edge) versus centroid–centroid (nnd.cent) distances 
# provides different results and these distances are not correlated (r = 0.02). The distance 
# matrix can also be used to derive other patch-level and class-level summary statistics 
# (e.g., mean distance, SD distance) in a straightforward way.

# The proximity index incorporates both area and the distance matrix and is frequently used 
# as a metric of patch isolation (Gustafson and Parker 1992, 1994). 
# This metric shares some similarity with metapopulation metrics for patch isolation 
# (see Chap. 10). Note that some formulations of this metric, like that described above, 
# only consider distances from the focal patch to all other patches in the neighborhood 
# while others consider distances/linkages between non-focal patches within the neighborhood 
# as well. We can calculate the proximity index by first creating a vector of patch area. 
# Then we need to alter the distance matrix to only consider patches within a neighborhood 
# of the patch, say 1000 m. Finally, we divide area by the distance with the sweep function 
# and sum across all j patches to quantify the proximity index for patch i:

#patch area
patch.area <- data.frame(id=for.pstat$patchID, area=for.pstat$area)
#neighborhood for proximity index to be calculated
h <- edge.dist
h2 <- 1 / h^2
h2[edge.dist>1000] <- 0
diag(h2) <- 0
#calculate proximity index
patch.prox <- rowSums(sweep(h2, 2, patch.area$area, "*"))

# Note that in this approach, we do not want the diagonal of the distance matrix to be NA. 
# Rather, we use the h2 matrix as an indicator matrix for only summing elements where 
# distances are <1000 (not including the diagonal). With these results, we find that the 
# patch proximity metric is weakly correlated with both patch area (r = 0.09) and nearest 
# neighbor distance based on edge–edge distances (r = −0.16).


#####        paragraph 3.3.3.3      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

# Landscape-level metrics are not considered in SDMTools , unfortunately. Here we provide 
# code for some prominent landscape-level metrics (Table 3.4). Some common landscape-level 
# metrics can be gleaned from summaries of class-level metrics in SDMTools, whereas others 
# require writing new functions . We illustrate both approaches below.

# Landscape metrics that can readily be derived from class-level metrics include the number 
# of patches (NP), patch density (PD), largest patch index (LPI), total edge (TE), edge 
# density (ED), and aggregation index (AI). At the landscape-level, these metrics are 
# typically summing values for class-level metrics (e.g., NP, PD, TE), or taking the 
# maximum value (LPI). Some examples include:

land.NP <- sum(nlcd.cstat$n.patches)
land.PD <- sum(nlcd.cstat$patch.density)
land.LPI <- max(nlcd.cstat$largest.patch.index)
land.TE <- sum(nlcd.cstat$total.edge)/ 2
land.ED <- sum(nlcd.cstat$edge.density)/ 2
land.AI <- sum(nlcd.cstat$prop.landscape * nlcd.cstat$aggregation.index)

# Here, we divide the edge metrics by 2 because each edge segment will be counted twice 
# when summing across class-level metrics (i.e., an edge will be counted once for each 
# land-cover type in the adjacency). Also, the aggregation index is simply a weighted 
# mean of the class-level metrics for aggregation. Note that SDMTools scales some of 
# these metrics in a slightly different way than the Fragstats program when considering 
# area/length, where SDMTools uses the square of the units provided in the layer (e.g., m2), 
# while Fragstats uses hectares (ha).

# Land-cover richness and diversity are frequently considered. Land-cover richness is 
# simply the number of land-cover types in an area of interest. So, if we are only 
# interested in one or a few landscapes, then this is straightforward to calculate 
# with simple output from the raster package (or with output from SDMTools) . For example, 
# we can calculate land cover richness for our landscape as:

richness <- length(unique(values(nlcd)))

# If we would like to calculate land-cover richness repeatedly for neighborhood, 
# like when using a moving window analysis (see below), we can create a function 
# to call for each neighborhood, x, such as:

richness <- function(x) (length(unique(na.omit(x))))
                           
# Shannon’s diversity , D, and evenness , E, indices are other popular measures, 
# defined as:   D ...... and E.......

# For an entire landscape, it is straightforward to calculate D and E using output 
# from the table function.          

table(values(nlcd)))
# This function returns the number of cells for each land-cover type on the map. 
We can then use this information to calculate diversity and evenness:

C <- table(values(nlcd))
P <- C / sum(C)
D <- -sum(P * log(P))
E <- D / log(length(C))

# Note that in R, log defaults to calculating the natural log (i.e., ln(x)).

Other landscape-level metrics that require new functions for their quantification 
# (i.e., they cannot appropriately be summarized from class-level metrics) focus 
# primarily on aggregation-related metrics. Aggregation-related metrics can capture 
# several related concepts, including dispersion and interspersion. Dispersion indices 
# focus on spatial mixing of a class type (ignoring other class types), while interspersion 
# metrics focus on spatial mixing of different class types (ignoring dispersion of a 
# specific class type) (Table 3.1). One prominent metric is contagion, which is an 
# intuitive, landscape-level metric that captures both dispersion and interspersion. 
# Contagion has been quantified in subtly different ways. A common formulation of 
# contagion is (Li and Reynolds 1993, Riitters et al. 1996): Contagion .......

# For this metric, we multiply the probability of a land-cover type by the conditional 
# probability of that type being adjacent to a different land-cover type j and then sum 
# this expression. Note the similarity of the contagion index to that of Shannon’s Evenness 
# index , E. The matrix N taken from the elements Nij is a commonly used summary statistic 
# in several landscape-level metrics (Turner and Gardner 2015). Some other relevant 
# measures that can be derived from N include the percentage of like adjacencies and the 
# aggregation index (Table 3.4). Note that calculating N requires using a patch-definition 
# rule (e.g., Fragstats uses a four-neighbor rule).

# One way to calculate this measure is to take advantage of the adjacent function in the 
# raster package to calculate Nij:

#identify adjacent cells
adj <- adjacent(nlcd, 1:ncell(nlcd), directions = 4, pairs = T, include = T)
head(adj, 2)
# This function identifies all of the pairwise combinations of adjacencies on the map, 
# including like adjacencies (i.e., two cells of the same land-cover type) with the term 
# include = T. This information can be summarized to get N with the table function, 
# which counts the values on the nlcd map based on the identified adjacencies:

N <- table(nlcd[adj[,1]], nlcd[adj[,2]])

# From there, the remaining terms are straightforward to calculate. A function for 
# calculating contagion using the formula of Riitters et al. (1996) is:

contagion <- function(r){
  adj <- adjacent(r, 1:ncell(r), directions = 4)
  Nij <- table(r[adj[,1]], r[adj[,2]])
  Nij <- unclass(Nij) #convert table format to matrix format
  Ni <- rowSums(Nij)
  Pj_i <- as.matrix(Nij / Ni)
  Pi <- as.vector(unclass(table(values(r))) / ncell(r))
  Pij <- Pi * Pj_i
  n <- length(Pi)
  #Ritters et al. 1996 formula
  contagion <- 1 + sum(Pij * log(Pij),na.rm = T)/(log(n^2 + n) - log(2))
  return(contagion)
}

# The above function breaks the steps of calculating contagion into its parts. We first 
# calculate the Nij. Note that a rate-limiting step here is the construction of N using the 
# table function. Scaling this function to larger landscapes would require using faster 
# alternatives, such as the data.table function. Then Pi and Pj/i are calculated. 
# Finally, we put this together using the approach of Riitters et al. (1996), wherein a 
# slight modification of the denominator is used in calculating contagion.

# The general approach for calculating Nij can be used to also calculate the percentage of 
# like adjacencies, PLADJ, at the landscape-level. This metric quantifies the degree of 
# dispersion of land-cover types. As this metric gets larger, the land-cover types are 
# more aggregated. It is defined as:  PLADJ

# This metric can be calculated in R with the following function:

PLADJ <- function(r){
  adj <- adjacent(r, 1:ncell(r), directions = 4)
  Nij <- table(r[adj[,1]], r[adj[,2]])
  Nij <- unclass(Nij)
  PLADJ <- sum(diag(Nij)) / sum(Nij) * 100
  return(PLADJ)
}

# To provide context for these landscape-level metrics, we contrast the landscape used so 
# far (Fig. 3.4) with two other landscapes that were sampled in Chap. 2 (Fig. 3.10). For 
# each landscape, we apply these functions to interpret landscape-level variation. One 
# landscape is dominated by forest (Fig. 3.10b), whereas the other appears to be highly 
# fragmented (Fig. 3.10c). It is notable that the forest-dominated landscape has generally 
# similar landscape-level metric values to our original landscape except those related to 
# landscape diversity and evenness. This similarity is driven by the fact that non-forest 
# land cover is generally configured in small patches with a large proportion of edge. The 
# landscape that appears fragmented (Fig. 3.10c) does have more patches, more edge, and 
# less aggregation than the other landscapes. Note that these numbers can vary subtly 
# with other programs, such as Fragstats, based largely on the underlying assumptions of 
# the calculations (patch delineation rules, how boundaries are considered, etc.).

# Taken together, these analyses illustrate how several landscape-level metrics can be 
# calculated in R. It also illustrates how the use of landscape-level metrics can sometimes 
# be more difficult to interpret than for patch or class-level metrics, because typically 
# landscape-level metrics are pooling or summarizing information across all land-cover 
# types on the map (compare metrics for landscapes a and b in Fig. 3.10). This pooling 
# makes the metrics more difficult to interpret biologically than with other types of 
# metrics. Nonetheless, in some situations, we expect biologically that landscape-level 
# metrics should better describe key issues of relevance to biodiversity, such as questions 
# regarding the role of landscape heterogeneity in agricultural landscapes 
# (Fahrig et al. 2011; Reynolds et al. 2018), and the importance of “countryside” 
# biogeography (Brosi et al. 2008; Mendenhall et al. 2014), where interest lies in 
# understanding the value of biodiversity across human dominated land uses.


#####        paragraph 3.3.3.4      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

# Each of the above approaches is often applied to replicated landscapes. In Chap. 2, for 
# example, we calculated the proportion of forest cover surrounding different locations 
# (a “patch-landscape,” or “focal-patch” sampling design) (Fahrig 2003). Another approach 
# to landscape quantification is using a moving-window analysis, which is akin to a 
# “neighborhood” analysis and loosely captures some ideas of an “ecological neighborhood” 
# (Addicott et al. 1987).

# In a moving window analysis, for each pixel on a map we quantify land cover in a 
# surrounding neighborhood. The result is a new map that visualizes the neighborhood 
# variation in land-cover properties. These maps can then be used for sampling or for 
# making predictive maps, such as maps of predicted species distribution (see Chap. 7).

# The raster package provides a means to implement a moving-window analysis in a 
# straightforward way with the focal function. Moving windows can be based on different 
# shaped windows, such as rectangles or circles . We first create a weight matrix with 
# the focalWeight function that defines the window size and shape:

#focal buffer matrix for moving windows
buffer.radius <- 100
fw.100m <- focalWeight(nlcd, buffer.radius, type = 'circle')
#re-scale weight matrix to 1/0 for calculations
fw.100m <- ifelse(fw.100m > 0, 1, 0)
fw.100m

# This is a square matrix where a circle is approximated based on the radius considered. 
# Note that the cells in this matrix reflect the grain of the map being considered and 
# raster creates the size of the matrix to match the length of the radius/grain. The 
# focalWeight function can also be used to consider Gaussian kernels, as discussed in 
# Chap. 2, by specifying type = 'Gauss' and setting the value for sigma 
# (the smoothing parameter; see Fig. 2.​10). For example, a Gaussian kernel with 
# sigma = 50 would be quantified as:

focalWeight(nlcd, c(50, 100), type = 'Gauss')

# Here, the two numbers for d reflect sigma and the window size to be considered 
# (100 m; same as above). The use of a Gaussian kernel allows for the weighting scheme 
# to decline with distance (Fig. 2.​10).

With this weight matrix, we can then use the focal function to run a moving window 
# analysis. For each pixel, this function will multiple the focalWeight matrix by the 
# raster. If the matrix is a series of 0’s and 1’s, in effect this will mask all values 
# outside the neighborhood (by multiplying those values by 0).

We illustrate two examples. First, we calculate the proportion of forest cover surrounding 
# each pixel . To do so, we use the sum function to sum the total forest cover within 
# each window (Fig. 3.11). Note that below we illustrate this process in a couple of 
# steps for clarity, but it could be streamlined by using a weighted average instead 
# (see Chap. 2). Second, we can call our own defined functions of land-cover pattern. 
# Here, we call our own function to calculate land-cover richness in surrounding each pixel 
# to illustrate.

#forest cover moving window; number of cells
forest.100m <- focal(nlcd.forest, w = fw.100m, fun = "sum", na.rm=T)
#proportion
forest.prop.100m <- forest.100m / sum(fw.100m)
#richness moving window
richness.100m <- focal(nlcd, fw.100m, fun = richness)

# Some metrics at the neighborhood scale can be more difficult to calculate in an 
# efficient way. For instance, calculating Shannon’s diversity is less straightforward, 
# because the calculation described above would take too long (the table function used in 
# the above description is relatively slow). A much quicker way is to create individual 
# maps that describe the proportion of each land cover category with a moving window and 
# then use raster algebra across maps to derive a new map of diversity at the neighborhood 
# scale. A function to accomplish this for Shannon’s diversity is:

diversity <- function(landcover, radius) {
  n <- length(unique(landcover))
  #Create focal weights matrix
  fw.i <- focalWeight(landcover, radius, "circle")
  #create new layer for diversity
  D <- landcover
  values(D) <- 0
  #function for log(p)*p
  log.i <- function(x) ifelse(x == 0, 0, x * log(x))
  #for each landcover category, create a moving window map and sum
  for (i in 1:length(n)) {
    focal.i <- focal(landcover == i, fw.i)
    D <- D + calc(focal.i, log.i)
    }
  D <- D * -1
  return(D)
}

diversity.100m <- diversity(landcover = nlcd, radius = 100)

# Overall, if we contrast this diversity map to that of land-cover richness , we find 
# that these two metrics across the landscape are weakly correlated (r = 0.24).


#####        paragraph 3.3.3.5      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####


