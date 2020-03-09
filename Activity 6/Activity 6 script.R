#install packages
install.packages(c("raster", "sp", "rgdal", "rgeos", "plyr"))
#installing plyr as dplyr masks spatial analyses
#load packages in
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this

g1966 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
#OGR data source with driver: ESRI Shapefile
#Source: "Y:\Students\hkropp\a06\GNPglaciers\GNPglaciers_1966.shp", layer: "GNPglaciers_1966"
#with 39 features
#It has 13 fields
#Integer64 fields read as strings:  OBJECTID

g1998 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
#OGR data source with driver: ESRI Shapefile 
#Source: "Y:\Students\hkropp\a06\GNPglaciers\GNPglaciers_1998.shp", layer: "GNPglaciers_1998"
#with 39 features
#It has 13 fields
#Integer64 fields read as strings:  OBJECTID

g2005 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
#OGR data source with driver: ESRI Shapefile 
#Source: "Y:\Students\hkropp\a06\GNPglaciers\GNPglaciers_2005.shp", layer: "GNPglaciers_2005"
#with 39 features
#It has 13 fields
#Integer64 fields read as strings:  OBJECTID

g2015 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_2015.shp")
#OGR data source with driver: ESRI Shapefile 
#Source: "Y:\Students\hkropp\a06\GNPglaciers\GNPglaciers_2015.shp", layer: "GNPglaciers_2015"
#with 39 features
#It has 21 fields
#Integer64 fields read as strings:  OBJECTID Recno

str(g2015)
#data stores all acccompanying info/measurements for each object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

g1966@proj4string
#CRS arguments:
#+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs +ellps=GRS80
#+towgs84=0,0,0

#####Question 1#####
#Look up the projection and datum. What does zone mean? Describe the projected coordinate system
#and the properties that it maintain.s What spatial scale is the projected coordinate system meant for?

#####Continuing On#####

#using spplot to plot vector data
spplot(g1966, "GLACNAME")

#check out gacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#glacier names don't match up, fix them so it's consistent over time

g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#now working with Raster Data

#read in rgb imagery from landsat
redL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_blue.tif")

#check coordinate system
redL@crs

#CRS arguments:
# +proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84
# +towgs84=0,0,0 

#make a brick that stacks all layers
rgbL <-brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#use ext to zoom in
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#####Working with Raster data#####

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list()
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("Y:\\Students\\hkropp\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
  
}

#observing what's in a raster function in 2003
str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs
#CRS arguments:
#+proj=laea +lat_0=48 +lon_0=-113 +x_0=0 +y_0=0 +a=6370997
#+b=6370997 +units=m +no_defs 

#####Question 2#####
#Why do you want to work with glacier data in an equal area projection rather than the UTM projection?


#####Continuing On#####

#plot that NDVI!
plot(NDVIraster[[1]])

#####Question 3#####
#What happesn if you try to plot NDVI with the 1966 glacier polygons?
#Make ap lot of the 2003 data side by side with the 1966 glacier extent.
#Explain why you can't put both data files on the same map.

#Use mfrow to get 2 side by side
par(mfrow = c(1,2))
plot(NDVIraster[[1]], axes = TRUE, xaxis = "i", yaxis = "i")
plot(g1966, axes = TRUE, axis = "i", yaxis = "i")


#look at details of equal area projection
#In UTM projection zone 12, the x axes are wonky
#all ranges on axes are wonky af
#can't put on same plot b/c the difference in projected coordinate systems
#the x and y values are different despite being in same geographic space!

#####Vector Data Analysis#####

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#####Question 4#####
#Make a map with both the maximum NDVI and the glaciers in 2015. Don't show
#the axes labesl with the xa nd y coordinate system values. Make the 2015 glacier polygon
#with no fill color and a black border. What are the patterns in NDVI in the map and around glaciers?

#####Continuing On#####
#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#Joining the data from plyr
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

#making a plot of the area for each glacier using this table
par = mfrow(c(1))
plot(c(1966,1998,2005,2015),
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],
       gAll$a2015m.sq[1]),
     type = "b",
     pch = 19, col = rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim = c(0,2000000),
     ylab = "Area of glacier (meters squared",
     xlab = "Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015),
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}

#####Question 5#####
#Calculate the % change in area between 1966 and 2015. Make a spplot
#of the glaciers in 2015 showing the % change that each glacier has experienced.

#####Continuing On#####
#Making a polygon showing difference in glaciers over time
#gidfference function removes overlapping areas

diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plotting it with NDVI
plot(NDVIraster[[13]], axes = FALSE, box = FALSE)
plot(diffPoly, col = "black", border = NA, add = TRUE)

#####Question 6#####
#Find the glacier with the largest % loss. Make a map that best displays the glacial extent
#for all years for that glacier with the highest % loss. Include background imagery
#You can use the original data for the map to match the imagery. Add a map title
#that includes the % loss and the glacier name. The subset function will be helpful here.

#####Raster Data Analysis: Does more veg. grow with glacial retreat

#extract some NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop though all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference pollygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm = TRUE)
}

#plotting the data
plot(ndviYear, meanDiff, type = "b",
     xlab = "Year",
     ylab = "Average NDVI (unitless)",
     pch = 19)
