#install packages
install.packages(c("raster", "sp", "rgdal", "rgeos", "plyr"))
#installing plyr as dplyr masks spatial analyses
#load packages in
library(sp) #must be loaded before raster
library(raster)
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
#and the properties that it maintains .What spatial scale is the projected coordinate system meant for?

#find more info about projection, etc.
g1966@proj4string

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
#Make a p lot of the 2003 data side by side with the 1966 glacier extent.
#Explain why you can't put both data files on the same map.

#Use mfrow to get 2 side by side
par(mfrow = c(1,2))
#plotting the 2003 data with 1966 extent
plot(NDVIraster[[1]], axes = TRUE, xaxis = "i", yaxis = "i")
plot(g1966, axes = TRUE, axis = "i", yaxis = "i")

#####Continuing on#####

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#####Question 4#####
#Make a map with both the maximum NDVI and the glaciers in 2015. Don't show
#the axes labesl with the x and y coordinate system values. Make the 2015 glacier polygon
#with no fill color and a black border. What are the patterns in NDVI in the map and around glaciers?

#fixing the dataframe
par(mfrow = c(1,1))
plot(NDVIraster[[13]], axes = FALSE) #plotting 2015 NDVI
plot(g2015p, col = "NA", add=TRUE, border="black") #adding the glaciers

#In and around the glaciers there are very low values for NDVI, likely due to the fact that there is no vegetation
#growing on the glaciers

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
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     axes = TRUE,
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   

#####Question 5#####
#Calculate the % change in area between 1966 and 2015. Make a spplot
#of the glaciers in 2015 showing the % change that each glacier has experienced.

#calculating percent change
g2015p$gchange = (((gAll$a2015m.sq - gAll$a1966m.sq)/(gAll$a1966m.sq))*100) #percent change = (final-initial)/(initial)*100
per.change = data.frame(g2015p$GLACNAME, g2015p$gchange) #creating a data frame
colnames(per.change) = c("GLACNAME", "gchange") #naming columns
spplot(g2015p, "gchange", main = "Percent Change in Glacial Area from 1966 to 2015") #creating the plot


#####Continuing On#####
#Making a polygon showing difference in glaciers over time
#gidfference function removes overlapping areas

#diffPoly <- gDifference(g1966p, g2015p)
#plot(diffPoly)

#plotting it with NDVI
#plot(NDVIraster[[13]], axes = FALSE, box = FALSE)
#plot(diffPoly, col = "black", border = NA, add = TRUE)

#####Question 6#####
#Find the glacier with the largest % loss. Make a map that best displays the glacial extent
#for all years for that glacier with the highest % loss. Include background imagery
#You can use the original data for the map to match the imagery. Add a map title
#that includes the % loss and the glacier name. The subset function will be helpful here.

#finding glacier with largest percent loss
largest.loss <- gAll[which(g2015p$gchange == min(g2015p$gchange)),]

#finding the area for the same glacier over each year data collected
largest.loss.1966 = subset(g1966,GLACNAME == largest.loss$GLACNAME)
largest.loss.1998 = subset(g1998,GLACNAME == largest.loss$GLACNAME)
largest.loss.2005 = subset(g2005,GLACNAME == largest.loss$GLACNAME)
largest.loss.2015 = subset(g2015,GLACNAME == largest.loss$GLACNAME)

#plotting the data with extent appropriate zoom
par(mai = c(1,1,1,1))
plotRGB(rgbL, ext=c(271500,276500,5425000,5429000), stretch="lin", main = "Boulder Glacier: -84.72067% Loss Between 1966 and 2015")

#Title: "Boulder Glacier: -84.72067% Loss Between 1966 and 2015"

#plotting each glacial extent
plot(largest.loss.1966, col="yellow", border= NA, add=TRUE) 
plot(largest.loss.1998, col="royalblue3", add=TRUE, border=NA)
plot(largest.loss.2005, col="green", add=TRUE, border=NA)
plot(largest.loss.2015, col="tomato3", add=TRUE, border=NA)

#creating the legend
legend("topright",legend = c("Glacial Area 1966", "Glacial Area 1998",
                           "Glacial Area 2005", "Glacial Area 2015"),
       fill = c("yellow", "royalblue3","green", "tomato3"))

#####Continuing On#####

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

#Designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])) {
    NA}else{
      #fit a regression and extract a slope
      lm(x~timeT)$coefficients[2]
    }
  }

#apply the slope function to the rasters
NDVIfit<-calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes = FALSE)

#####Question 7#####
#What are the patterns in maximum NDVI change across the park?

#####Continuing On#####

#buffer glaciers
glacier500m <-gBuffer(g1966p,#data to buffer
                      byid=TRUE,#keeps original shape id
                      width = 500)#width in coordinate system units

#convert to a raster
buffRaster <-rasterize(glacier500m,#vector to convert to raster
                       NDVIraster[[1]], #raster to match cells and extent
                       field=glacier500m@data$GLACNAME, #field to convert to raster data
                       background = 0) #background value for missing data

#plot it
plot(buffRaster)

#rasterize glaciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

#####Question 8#####
#Describe what glacZones looks like. How did the raster math accomplish removing the glaciers from the zones.
#What are the similarities and differences between this raster operation and gDifference?

#####Continuing On#####

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones, #raster with zones
                    "mean") #function to apply
head(meanChange)  

#####Question 9#####
#Add the mean change in NDVI per year into the 2015 glacier polygons.
#Make a map where the mean change in vegetation is color coded within
#the 2015 glacier polygons. Does there seem to be any patterns.

#removing item 0 (no glacial area)
meanChange2 = data.frame(meanChange[-c(1),])
head(meanChange2)

#Adding meanChange values to the dataframe containing 2015 glacier shapefiles
g2015p$meanChange = meanChange2$mean

#plotting the data
spplot(g2015p, "meanChange", main = "Mean Change in NDVI per Year", border = "NA")

#####Question 10#####
#Do you think vegetation is changing as glaciers recede and why? Are the values of yearly 
#change substantial? Hint: consider the range of NDVI and also consider
#the magnitude of change over a 10 or 15 year period.

#getting quartiles, etc.
summary(meanChange2[,2])
#small values of average change

#looking at change
range(meanChange2$mean)
#small range of values

#####Question 11#####
#What is the average maximum NDVI across all years within Glacier National Park
#(hint make a raster dataset to answer this)? 
#Is there a pattern between glacier size and the average NDVI across all years within 500m? 
#Make a map that shows both the most recent glacier extent color coded with the surrounding maximum NDVI average,
#and use the raster of average maximum NDVI as a basemap.

#creating a dataframe for the means
meanNDVI <- mean(NDVIstack)
#finding the mean cell value
cellStats(meanNDVI, "mean")

#finding means in each zone
zonal.meanNDVI = zonal(meanNDVI, #NDVI function to summarize
      glacZones, #raster with zones
      "mean") #function to apply

#note that areas without glaciers the mean is 0.7510976 before removing

#getting rid of the "0" value for nonglacial areas
zonal.meanNDVI2 = data.frame(zonal.meanNDVI[-c(1),])

#adding to dataframes
gAll$meanNDVI = zonal.meanNDVI2$mean
g2015p$meanNDVI = zonal.meanNDVI2$mean


#visualizing a relationship between size and NDVI
par(mai=c(1,1,1,1))
plot(gAll$a2015m.sq, gAll$meanNDVI, axes = TRUE,
     main = "Relationship between Area of Glaciers in 2015 and change in NDVI",
     xlab = expression(paste("Glacial Area in 2015 mi"^"2")),
     ylab = "Mean Change in NDVI")

#As size increases for the glaciers in 2015, there is less of a mean change in NDVI across all years within 500 m

#creating a map

#creating a color ramp from base r Blue colors -- blue is not in the base NDVI map
#using darker blues for greater mean NDVI max
g2015p@data$NDVIcol <- ifelse(g2015p@data$meanNDVI<0.2,"cadetblue4",
                              ifelse(g2015p@data$meanNDVI<0.4,"deepskyblue",
                                     ifelse(g2015p@data$meanNDVI<0.6, "dodgerblue4",
                                            ifelse(g2015p@data$meanNDVI<0.8, "darkslateblue","darkblue"))))

#Plotting: Mean NDVI of 2015 Glacial Extents in Glacier Natioanl Park over years 1966 - 2015
plot(meanNDVI, axes = FALSE)
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)
legend("bottomleft", cex = 0.5, legend = c("Glacial Extent; NDVI < 0.2",
                             "Glacial Extent; NDVI < 0.4",
                             "Glacial Extent; NDVI < 0.6",
                             "Glacial Extent; NDVI < 0.8",
                             "Glacial Extent; NDVI > 0.8"),
       fill = c("cadetblue4", "deepskyblue", "dodgerblue4", "darkslateblue", "darkblue"))



#####Question 12#####
#Using the Carlson and Anderson paper, what kinds of data would you need to validate
#what is happening with vegetation as glaciers recede? What might be the next steps
#in making a more accurate analysis of vegetation change for research?


#####Question 13#####
#Copy and paste the link to your GitHub code



