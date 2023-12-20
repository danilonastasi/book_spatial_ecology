
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
# ppp.window <- owin(xrange = c(boundary$Xmin, boundary$Xmax), 
#                    Yrange = c(boundary$Ymin, boundary$Ymax)) # error message
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

# Many of the above univariate analyses can be extended to ask interesting and important 
# questions based on marked point patterns. First, consider the issue of resource use 
# versus availability in the context of marked point patterns (Lancaster and Downes 2004). 
# There are several insect herbivores that use Opuntia cactus. We may be interested in 
# interpreting the spatial dispersion of these herbivores. We want to know the distribution 
# of herbivores, given the underlying distribution of cactus. For this and other 
# complexities, such as interpreting spatial covariance between species, we need to use 
# a marked point pattern analysis.

# In this situation, we will interpret the spatial distribution of an insect herbivore, 
# Chelinidea vittiger, on cactus. C. vittiger is a pest insect that has been used as a 
# biological control for Opuntia cactus in Australia (although with limited effectiveness) 
# (DeVol and Goeden 1973). It specializes on Opuntia, where it feeds and breeds on cactus 
# segments. It is a poor disperser (Fletcher et al. 2011) and tends to show aggregated 
# distribution patterns on cactus (Miller et al. 2013).

# To interpret C. vittiger distribution, we can use a randomization procedure where we 
# relabel marks (shuffle the locations of used versus unused cacti, termed 
# “random labeling”) to interpret the observed pattern of the herbivore with the rlabel 
# function. That is, we are interested in insect dispersion, conditional on cactus 
# distribution. In other circumstances, we might be interested in the joint distribution 
# of two marked processes (e.g., competition between two species). For such situations, 
# we can use the rshift function in a similar way as using rlabel. Rather than shuffling 
# labels, the rshift function performs a toroidal shift of the point pattern of one mark 
# while leaving the other marked point pattern constant.

# First, we need to make a new spatstat object that includes the marks for 
# presence–absence data. In the data provided, there is information from six surveys 
# conducted at each patch regarding the number of C. vittiger detected per patch. 
# Here, we truncate the data to produce a bivariate mark of presence–absence of 
# C. vittiger:

cactus$CheliPA <-as.factor(ifelse(cactus$chelinidea > 0, "presence", "absence"))

# With this new variable, CheliPA, we can create a new spatstat object:

ppp.PA <- ppp(cactus$East, cactus$North, window = ppp.window, marks = cactus$CheliPA)
split(ppp.PA) #summary statistics by mark
plot(split(ppp.PA)) #separate plots

# We first interpret the spatial pattern of C. vittiger distribution, ignoring the 
# underlying distribution of cactus (Fig. 4.8a).

cheli.data <- subset(cactus, chelinidea > 0)
ppp.bug <- ppp(cheli.data$East, cheli.data$North, window = ppp.window)
Lbug <- envelope(ppp.bug, Lest, nsim = 99, rank = 1, i = "presence", global = F)

# Then, we contrast these results to those based on a bivariate K function (or a bivariate 
# pair correlation function) with a random-labeling simulation to interpret the spatial 
# pattern of marks (Fig. 4.8b).

# Lmulti <- envelope(ppp.PA, Lcross, nsim = 99, rank = 1, 
#                   I = "presence", global = FALSE,   # error message because ov "I" variable
#                   simulate = expression(rlabel(ppp.PA))) 

# let's change the code with

Lmulti <- envelope(ppp.PA, Lcross, nsim = 99, rank = 1, 
                   I2 = "presence", global = FALSE,  
                   simulate = expression(rlabel(ppp.PA))) 

# Taken together, if we only did an analysis that ignored the distribution of cactus, 
# we would have very different conclusions regarding the spatial pattern (Fig. 4.8), 
# where we would conclude that C. vittiger has an aggregated distribution. However, 
# this pattern is driven by the pattern of cactus, which constrains the distribution of 
# C. vittiger.

# Finally, we can consider continuous marks through the use of the mark correlation 
# function. For this example, we consider cactus area as a continuous mark. 
# Consequently, we ask whether cactus tends to be aggregated by size or if there is an 
# inhibition process where larger cacti tend to be near smaller ones. This can be done by 
# creating a spatstat object where we use cactus size as the quantitative mark and then 
# use the markcorr function (Fig. 4.9):

ppp.area <- ppp(cactus$East, cactus$North, window = ppp.window, marks = cactus$Area)
mcf.area <- markcorr(ppp.area)
MCFenv <- envelope(ppp.area, markcorr, nsim = 99, correction = "iso", global = FALSE)
plot(MCFenv, shade = c("hi", "lo"), legend = F)

# Note that in this case, if the observed value is above 1, there is evidence for a 
# positive mark correlation, such that large cacti tend to be near other large cacti. 
# If the value is <1, large cacti tend to be near smaller cacti. This analysis suggests 
# that there is no strong spatial pattern of cactus size across the plot (i.e., large 
# cacti are not aggregated).


#####        paragraph 4.3.6      #####
#####  from the book "Spatial Ecology and Conservation Modeling" - Springer(2018)  #####
#####  revisited  #####

# Point process models allow for understanding and accounting for inhomogeneous point 
# processes: when point processes vary by location, such as across environmental gradients. 
# An inhomogeneous point process model is similar to a generalized linear model (GLM) for 
# point data (very similar to a Poisson regression; more on GLMs in Chap. 6), where we are 
# modeling the intensity of points in the study area (Renner et al. 2015).

# We could use a variety of covariates to account for inhomogeneous point processes. 
# We will consider two types of covariates. First, we start with simply accounting for 
# spatial trend based on x–y coordinates (see Ch. 6 for more on spatial trend). We can 
# fit different point process models and inspect the model fit. Second, we will import 
# a raster layer that quantifies herbaceous vegetation height in the plot (see Chap. 5 
# for more on these data and their interpretation). Surrounding vegetation height may be 
# relevant for interpreting Opuntia distribution due to light limitation (Hicks and 
# Mauchamp 2000) or indirect effects from variation in herbivory (Burger and Louda 1994). 
# To fit a point process model, we use the ppm function:

#simple intercept and trend models based on x,y coordinates
pp.int <- ppm(ppp.cactus, ~ 1) #no trend(homogeneous)
pp.xy <- ppm(ppp.cactus, ~ x + y) #linear trend
pp.xy2 <- ppm(ppp.cactus, ~ polynom(x, y, 2)) #quadratic trend

# Adding x-y coordinates in a point process model can sometimes cause difficulty for model 
# convergence, such that it may require rescaling coordinates. In the above models, we 
# manually centered the ppp objects (window and point coordinates by subtracting the mean 
# of the x and y coordinates; code not shown) to insure convergence, but the rescale 
# function in the spatstat package could also be used. To use a raster layer, we must 
# convert the raster (Fig. 4.10a) to a matrix and then an image file that spatstat can 
# interpret. We can then fit the model and contrast models with AIC.

#model based on covariates from a raster layer
library(raster)
# veg.height <- raster('cactus_matrix') # we miss the file "cactus_matrix"

#####  we skip this code because we miss a file "cactus_matrix"   #####

#raster into an image covariate that spatstat can read
# veg.height <- data.frame(rasterToPoints(veg.height))
# veg.height <- veg.height[order(veg.height$x, veg.height$y), ]#sort
# veg.height.mat <- matrix(NA, nrow=length(unique(veg.height$x)), ncol=length(unique(veg.height$y)))
# veg.height.mat[] <- veg.height$Height
# cov.veg <- im(mat = veg.height.mat, Xrange = c(boundary$Xmin, boundary$Xmax), 
#               Yrange = c(boundary$Ymin, boundary$Ymax))
#point process model based on vegetation covariate
# pp.veg <- ppm(ppp.cactus,~ veg, covariates = list(veg=cov.veg))
#model selection with AIC
# data.frame(model = c("int", "xy", "xy^2", "veg"), 
#            AIC = c(AIC(pp.int), AIC(pp.xy), AIC(pp.xy2), AIC(pp.veg)))

##### let's have a look to the code in the zip file with all data from the book ####

AIC(pp.xy)
summary(pp.xy) #Warning: singular matrix; need to re-scale x-y coordinates

#Rescale coordinates via centering (also see rescale function)
centerx <- (boundary$Xmax + boundary$Xmin)/2
centery <- (boundary$Ymax + boundary$Ymin)/2

ppp.windows <- owin(xrange=c(boundary$Xmin - centerx, boundary$Xmax - centerx),
                   yrange=c(boundary$Ymin- centery, boundary$Ymax- centery))
ppp.cactuss <- ppp(cactus$East- centerx, cactus$North- centery, window=ppp.windows)

#re-fit with rescaled coordinates and window
pp.xy <- ppm(ppp.cactuss, ~ x + y)
summary(pp.xy)
AIC(pp.xy)
AIC(pp.xy2)

#no trend(homogeneous point process)
pp.int <- ppm(ppp.cactuss, ~1)
summary(pp.int)
AIC(pp.int)

#point process model with quadratic trend
pp.xy2 <- ppm(ppp.cactuss, ~ polynom(x, y, 2))
summary(pp.xy2)
AIC(pp.xy2)

#point process model based on vegetation covariate
veg.height <- read.csv('cactus_matrix.csv', header=T)

#inspect
head(veg.height)

#make a square matrix for creating im object
veg.height <- veg.height[order(veg.height$x, veg.height$y), ]#sort
veg.height.mat <- matrix(NA, nrow=length(unique(veg.height$x)), ncol=length(unique(veg.height$y)))
veg.height.mat[] <- veg.height$Height

#create im object
cov.veg <- im(mat=veg.height.mat,
              xrange=c(boundary$Xmin - centerx, boundary$Xmax - centerx),
              yrange=c(boundary$Ymin- centery, boundary$Ymax- centery))

#fit ppm model with vegetation
pp.veg <- ppm(ppp.cactuss, ~veg, covariates=list(veg=cov.veg))
AIC(pp.veg)
summary(pp.veg)

#plot relationship
# plot(effectfun(pp.veg, "veg", se.fit=T)) # error message

##### let's comeback to the book without using "cactus_matrix"   #####
#####                                                            #####

veg.height <- read.csv('cactus_matrix.csv', header=T)
# veg.height <- data.frame(rasterToPoints(veg.height))  # it is already a dataframe
veg.height <- veg.height[order(veg.height$x, veg.height$y), ]#sort
veg.height.mat <- matrix(NA, nrow=length(unique(veg.height$x)), ncol=length(unique(veg.height$y)))
veg.height.mat[] <- veg.height$Height
# cov.veg <- im(mat = veg.height.mat, Xrange = c(boundary$Xmin, boundary$Xmax), # error message
#                Yrange = c(boundary$Ymin, boundary$Ymax)) # error message

# let's fix the code:

cov.veg <- im(mat = veg.height.mat, xrange = c(boundary$Xmin, boundary$Xmax),
               yrange = c(boundary$Ymin, boundary$Ymax))

#point process model based on vegetation covariate
pp.veg <- ppm(ppp.cactus,~ veg, covariates = list(veg=cov.veg))
#model selection with AIC
data.frame(model = c("int", "xy", "xy^2", "veg"), 
            AIC = c(AIC(pp.int), AIC(pp.xy), AIC(pp.xy2), AIC(pp.veg)))

# Based on the model selection criteria (AIC), there is some evidence of spatial 
# heterogeneity in the intensity of points, suggesting that this could be considered an 
# inhomogeneous point process. The most supported model is one with a quadratic trend, 
# but including the raster image of vegetation height also had some support. We can plot 
# this estimated process with the predict function:

#plot the point process models
plot(predict(pp.xy2, type = "trend"))
plot(ppp.cactus, add = T)

# We can also adjust for this pattern in the K function. To do so, we first need to make 
# an image object of the predicted point process model that spatstat can use (similar to 
# a raster map of the covariate of interest). We then use the Linhom function to account 
# for this heterogeneity when quantifying spatial dispersion (Fig. 4.10c).

# pp.xy.pred <- predict.ppm(pp.xy2, type = "trend”)  # error message, let's change:
pp.xy.pred <- predict.ppm(pp.xy2, type = "trend")
# Lxycsr <- envelope(ppp.cactus, Linhom, nsim = 99, rank = 1,  
#                          correction = "translate", 
#                          simulate = expression(rpoispp(pp.xy.pred)), global = F) # error message
# plot(Lxycsr, . - r ~ r, shade = c("hi", "lo"), legend = F) # error message


