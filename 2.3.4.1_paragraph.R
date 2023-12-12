
##### paragraph 2.3.4.1 #####

setwd("C:/Users/danil/Documents/UniBo/Spatial Ecology/ebook/Fletcher_Fortin-2018-Supporting_Files/data")   
nlcd <- raster("nlcd2011SE")

proj4string(nlcd) 
# or
st_crs(nlcd) # from sf package, there is a difference

nlcd_proj <- projection(nlcd) 
# or
nlcd_proj <- st.crs(nlcd) # from sf package, tehre is a difference

res(nlcd)

nlcd <- as.factor(nlcd) #convert to factor
levels(nlcd)

# library(rgdal) --> not working anymore, let's change function
library(sf)
library(terra)
# sites <- readOGR("reptiledata") we change readOGR function with read_sf function from package sf
sites <- read_sf("reptiledata") # from package sf
# sites <- vect("reptiledata") # from package terra
class(sites)

summary(sites)

# proj4string(sites) <- nlcd_proj #set projection   # it does not work
st_crs(sites) <- nlcd_proj  # from sf package

head(sites, 2)
sites <- subset(sites, management!="Corn")

x.min <- min(sites$coords_x1)-10000
x.max <- max(sites$coords_x1)+10000
y.min <- min(sites$coords_x2)-10000
y.max <- max(sites$coords_x2)+10000
extent.new <- extent(x.min, x.max, y.min, y.max)
nlcd <- crop(nlcd, extent.new)

#create a new forest layer
forest <- nlcd
values(forest) <- 0
forest[nlcd==41 | nlcd==42 | nlcd==43] <- 1 #forest categorie

#reclassification vector
reclass <- c(rep(0,7), rep(1,3), rep(0,6))

#create reclassification matrix
reclass.mat <- cbind(levels(nlcd)[[1]], reclass)
head(reclass.mat, 3)

forest <- reclassify(nlcd, reclass.mat)

#buffer sites
buf1km <- 1000
buf5km <- 5000
#buffer only first site
# buffer.site1.1km <- buffer(sites[1,], width=buf1km) # it does not work, let's change with st_buffer function 
                                                      # and argument 'dist' instead of 'width'
buffer.site1.1km <- st_buffer(sites[1,], dist=buf1km)
# buffer.site1.5km <- buffer(sites[1,], width=buf5km)
buffer.site1.5km <- st_buffer(sites[1,], dist=buf5km)

#zoom into area for viewing
zoom(nlcd,buffer.site1.5km)
plot(buffer.site1.5km, border="red", lwd = 5, add=T)
plot(buffer.site1.1km, border="red", lwd = 3, add=T)
points(sites[1,], pch=19, cex=2, add=T)

buffer.forest1.1km <- crop(forest, buffer.site1.1km)
buffer.forest1.1km <- mask(forest, buffer.site1.1km)

#area of each cell, in ha
grainarea <- res(forest)[[1]]^2/10000
#area of 1km buffer
bufferarea <- (3.14159*buf1km^2)/10000
#calculation of forest cover and % cover
forestcover1km <- cellStats(buffer.forest1.1km, 'sum') * grainarea
percentforest1km <- forestcover1km / bufferarea * 100

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

#create empty vector for storing output first
f1km <- vector(NA, length = nrow(sites))
f2km <- vector(NA, length = nrow(sites))
