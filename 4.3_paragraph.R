
#####  R. version: 4.3.2 2023/10/31  #####

#####        paragraph 4.3      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

#####    code tested on _________   #####

# In R, there are a few libraries that can be used for spatial point pattern analysis. 
# Spatial (Venables and Ripley 2002) allows for limited point-pattern analysis, whereas 
# spatstat is a more flexible and comprehensive library. We will focus on the use of spatstat 
# (Baddeley and Turner 2005).

# In spatstat , the basic data types are Point Patterns (ppp), Windows (owin), and Pixel 
# Images (im). A point pattern is a dataset recording the spatial locations of all “events” 
# or “individuals” observed in a certain region. A window is a region in two-dimensional 
# space. It usually represents the boundaries of the study area. A pixel image is an array 
# of values for each grid point in a rectangular grid inside the window. It may contain 
# covariate data (e.g., taken from a raster grid) or it may be the result of calculations 
# (such as kernel density smoothing function).

# As an example of interpreting point data and its spatial patterns, we analyze plant 
# distribution data collected at the Ordway-Swisher Biological Station, a core NEON 
# (National Ecological Observatory Network) site for the southeastern United States 
# (Kampe et al. 2010; Kao et al. 2012). Prickly pear cactus (Opuntia humifusa) is a 
# common plant found in old fields (Fig. 4.4a) and other upland areas that have limited 
# canopy cover. Prickly pear cactus is of considerable interest for three reasons. 
# First, it is a common resource for several insects and vertebrates that use it for 
# foraging and breeding (Sauby et al. 2012; Grunwaldt et al. 2015; Lavelle et al. 2015). 
# Second, some species of Opuntia are grown as agricultural crops in some regions of 
# the world (Lopez 1995; Cruz-Rodriguez et al. 2016). Third, Opuntia has invaded some 
# areas where it is not native, becoming a problematic species that can dominate an 
# area (e.g., some areas of Australia and South Africa) (Freeman 1992; Novoa et al. 2016).

# This system is well-suited for exploring the possibilities of how spatial point pattern 
# analysis can be applied to address applied questions in ecology. We use data of O. humifusa 
# locations in a 50 × 50 m plot (Fig. 4.3b), which were mapped using a high-resolution 
# GPS (~30 cm error) as part of a larger investigation on habiat loss and fragmentation 
# (Fletcher et al. 2018). Each cactus location includes information on the size of cactus, 
# as well as the presence of an insect herbivore, Chelinidea vittiger, that specializes on 
# Opuntia and has been used as a biological control for invasive Opuntia in some regions 
# of the world (DeVol and Goeden 1973). Cactus size can be characterized as a continuous 
# mark, whereas the presence–absence of C . vittiger can be characterized as a binary 
# (or qualitative) mark. Our goal is to interpret whether there is any evidence of spatial 
# aggregation of cactus and the biological control agent, along with identifying the 
# spatial scales of pattern.


#####        paragraph 4.3.3      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

# First, we load the spatstat package and data (cactus.csv), and we create relevant 
# spatstat objects. Because this study area is a square (50 × 50 m), we can provide 
# spatstat with the corners of the study area (cactus_boundary.csv) to delineate the 
# window size (e.g., latitude/longitude or UTMs):

# setting working directory where I stored data from:
# http://​www.​fletcherlab.​com under “Products”
setwd("C:/Users/danil/Documents/UniBo/Spatial Ecology/ebook/Fletcher_Fortin-2018-Supporting_Files/data")   

library(spatstat)
cactus <- read.csv('cactus.csv', header = T)
# boundary <- read.csv("cactus_boundary.csv", header = T) # error message
boundary <- read.csv("cactus_boundaries.csv", header = T) # the name file is "cactus_boundaries"
ppp.window <- owin(xrange = c(boundary$Xmin, boundary$Xmax),
                    yrange = c(boundary$Ymin, boundary$Ymax))
# xrange <- c(boundary$Xmin, boundary$Xmax)
# yrange <- c(boundary$Ymin, boundary$Ymax)
# ppp.window <- owin(xrange, yrange)
ppp.cactus <- ppp(cactus$East, cactus$North, window = ppp.window)

# spatstat can also take polygon files (e.g., shp files) for delineating the study area. 
# For example, we can load an .shp file of a polygon of the plot extent with the 
# rgdal package and create a win object from that in a straightforward manner:

# we need the archived package rgdal. We install it thanks to Rtools43 from
# the archived package list on CRAN:

# install.packages("https://cran.r-project.org/src/contrib/Archive/PACKAGE ROOT AND FILE")
library(rgdal)
# boundary.poly <- readOGR("cactus_boundary.shp")  # we miss the file "cactus_boundary.shp"
# ppp.window.poly <- as.owin(boundary.poly) # missing file from data

# Once the data are in spatstat format, there are several exploratory graphs and summary 
# statistics that spatstat can provide. Below are a few examples (Fig. 4.4c):

plot(ppp.cactus) #graph of point locations
plot(density(ppp.cactus, 1)) #density/intensity plot
summary(ppp.cactus)

# This summary shows that there are 82 points (cactus patches) and provides the observed 
# intensity, λ. The density plot (Fig. 4.4d) can be a helpful visualization of intensity 
# of points across the plot. By plotting the spatial intensity in this way, we can get an 
# idea of whether or not there may be spatial trends in the point occurrences that may 
# violate the assumption of a homogeneous point process.

# We can also make tallies of counts of point locations based on quadrats overlaid on the 
# plot (Fig. 4.4c). To determine whether these quadrat counts conform to CSR (i.e., 
# a homogeneous Poisson process), we can use a simple Chi-square test statistic:

Q <- quadratcount(ppp.cactus, nx = 4, ny = 4)#12.5x12.5m quadrats
plot(ppp.cactus)
plot(Q, add = TRUE)  # in order to work we have to plot first "ppp.cactus"
quadrat.test(ppp.cactus, nx = 4, ny = 4,method = "Chisq")

# This test statistic suggests highly a non-random point pattern at the scale of the 
# quadrat that we defined. Note that this test is more akin to a first-order point pattern 
# analysis because it is based on the dispersion of points among sampling quadrats.


#####        paragraph 4.3.4      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####















