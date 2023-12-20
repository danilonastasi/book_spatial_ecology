
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

# Second-order point pattern analyses can readily be implemented in spatstat . Below 
# illustrates Ripley’s K and the standardized L functions (Fig. 4.5), initially ignoring 
# edge effects (correction="none").

Knone <- Kest(ppp.cactus, correction = "none")
plot(Knone)
Lnone <- Lest(ppp.cactus, correction = "none")
plot(Lnone) #standardized to a 1:1 expectation
plot(Lnone, . - r ~ r) #standardized to a zero expectation

# You will notice that for these functions, two lines are drawn. The “Lpois” line is a 
# dashed line that represents the expected (theoretical) value based on a Poisson process 
# (CSR). The way that spatstat calculates L is to linearize K such that the expected value 
# is r (or the radius). The other solid line represents the estimated L (linearized K), 
# when we ignore edge effects.

# The above analysis ignores the problem of edge effects. spatstat provides a variety of 
# edge corrections. We contrast an isotropic and translate correction for adjusting for 
# boundary effects (Fig. 4.6). The isotropic correction uses a simple weighting scheme 
# for the area sampled near the plot boundary (Ripley 1988), while the translate correction 
# uses a toroidal shift. We adjust for potential boundary effects by typing:

Liso <- Lest(ppp.cactus, correction = "isotropic")
plot(Liso, . - r~r)
Ltrans <- Lest(ppp.cactus, correction = "translate")
plot(Ltrans, . - r ~ r)

# When comparing the L function that ignores boundaries to those above that account for 
# boundaries, notice that patterns change at larger distances—we expect that the L function 
# at larger distances should potentially be more biased than at smaller distances 
# (because larger radii will naturally overlap more with the boundary of the study area). 
# When we ignore edge effects, we are in effect counting fewer points within the radius r 
# near the boundary, so the observed value for L or K should have an artifact of decreasing 
# as r increases.

# The analyses so far are exploratory. While the observed statistics (K, L) appear 
# different than the expectation, it is unclear if these are substantially 
# (or significantly) different. To conduct formal inference regarding if the point pattern 
# follows CSR, we can use Monte Carlo simulations to calculate a confidence envelope under 
# CSR with the envelope function. This function can be applied to several point pattern 
# statistics.

Lcsr <- envelope(ppp.cactus, Lest, nsim = 99, rank = 1, 
                 correction = "translate", global = FALSE)
plot(Lcsr, . - r ~ r, shade=c("hi", "lo"), legend = F)

# In the envelope function, rank specifies the alpha for the simulations. For a rank=1, 
# the max and min are used for envelopes, such that for 99 simulations, α = 0.01 while 
# for 19 simulations, α = 0.05. Also note that we used global = FALSE. This means that 
# these are “pointwise envelopes.” These envelopes work better for L than K because of 
# variance stabilizing properties.

# Plots of pointwise envelopes show the stated upper and lower quantiles of simulated 
# patterns for any distance r (Fig. 4.7). Because such analyses are calculating envelopes 
# for many distances, pointwise envelopes with a specified α should not be used to reject 
# a null model at that level (because of the multiple tests). Consequently, there are 
# alternative global tests that can be used in this way. While global tests are under 
# active development (Baddeley et al. 2014; Wiegand et al. 2016), spatstat does provide 
# one option for a global test (using global = T in the above model). This approach 
# estimates the maximum deviation from the Poisson point process across all r 
# (i.e., D = max|K(r) − Kpois(r)|). This approach is referred to as a simultaneous 
# envelope (or critical band) rather than a pointwise envelope (Fig. 4.7a). If the 
# observed line falls outside the simultaneous envelope at any point on r, we would 
# reject the null hypothesis.

# Now, say we are more interested in estimating the distance at which spatial patterns 
# arise, such that using a “ring” rather than a circle (as in Ripley’s K) is more 
# appropriate. To estimate the pair correlation function, g, most of the arguments are 
# similar to above. The main exception is that instead of calling Lest, we call pcf 
# (pair correlation function; Fig. 4.7b):

Ptrans <- pcf(ppp.cactus, correction = "translate")
plot(Ptrans)
Penv <- envelope(ppp.cactus, pcf, nsim = 99, rank = 1, 
                 correction = "translate", global = FALSE)
plot(Penv, shade = c("hi", "lo"), legend = FALSE)

# The pcf function uses a smoothing kernel such that distance bins are not needed. The 
# default bandwidth coefficient (related to sigma in a Gaussian kernel; see Chap. 2) for 
# the smoothing kernel is set to 0.15 (Stoyan and Stoyan 1994). We can adjust the smoothing 
# on the pair correlation function using the stoyan command in the pcf function. 
# Increasing the value of the bandwidth coefficient (e.g., stoyan = 0.4) results in a less 
# wiggly g function (Fig. 4.7b).

# Finally, we can use similar arguments for the G-function to estimate the probability 
# of finding a nearest neighbor as a function of distance (Fig. 4.7c). spatstat uses a 
# similar approach as above with the Gest function. Note that for Gest, there are subtly 
# different ways to account for edge effects relative to above. Below we use rs, the 
# reduced sample correction. We can check the observed G-function calculated by spatstat 
# to the cumulative distribution function of the empirical data (with the ecdf function):

Gtrans <- Gest(ppp.cactus, correction = "rs")
plot(Gtrans, legend = F)
Genv <- envelope(ppp.cactus, Gest, nsim = 99, rank = 1, 
                 correction = "rs", global = FALSE)
plot(Genv, shade = c("hi", "lo"), legend = FALSE)
#nearest neighbor distance for each point
nn.dist <- nndist(ppp.cactus)
plot(ecdf(nn.dist), add = T)  # in order to run this code we need the plot above in row 212

# Note that the radius considered for the G-function is much smaller than for the 
# L-function or the pair correlation function. This makes sense, because the nearest 
# neighbor distances will emphasize the shortest distances between points.

# Taken together, the analyses using the L, g, and G-functions provide complementary 
# insights regarding the spatial pattern of Opuntia. Using pointwise envelopes, the 
# L function suggests an aggregated pattern occurring at scales of approximately 2–13 m, 
# while the pair correlation function suggests that most of the observed effect in the 
# L function is generated from shorter distances, on the order of 2–6 m. Similarly, 
# the G-function suggests that nearest neighbor distances are random at very small 
# scales (<2 m), while distances are closer than expected at larger scales, consistent 
# with aggregation.


#####        paragraph 4.3.5      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####




