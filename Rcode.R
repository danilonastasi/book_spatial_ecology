
##### paragraph 2.3.3  #####

library(raster)
set.seed(16)#sets random number seed for repeatability
toy <- raster(ncol=6, nrow=6, xmn=1, xmx=6, ymn=1, ymx=6)
values(toy) <- rpois(ncell(toy), lambda=3)
ncell(toy)
plot(toy)
text(toy, digits=2)

ncell(toy)
toy2 <- toy
values(toy2) <- 1:ncell(toy)
plot(toy2)
text(toy2, digits=2)

toy_mean <- aggregate(toy, fact=2, fun=mean) #mean value
toy_maj <- aggregate(toy, fact=3, fun=modal) #majority rule

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

nlcd_proj <- projection(nlcd)
res(nlcd)

nlcd <- as.factor(nlcd) #convert to factor
levels(nlcd)

sites <- read_sf("reptiledata") # from package sf
# sites <- vect("reptiledata") # from package terra
class(sites)

summary(sites)
