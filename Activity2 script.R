#Practicing With Matrixes
heights <- c(30,41,20,22)

#converts to cm
heights_cm <- heights*100
heights_cm

#Fist Data point
heights[1]

#Looking at 2nd and 3rd Data Point
heights[2:3]

#Setting up a matrix with 2 columns to fill by rows
#First argument is the vector of numbers to fill in the matrix
Mat <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow = TRUE)
Mat

#Creating a matrix that fills by columns
Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow= FALSE)
Mat.bycol

#Looking at the data point in row 1, column 2
Mat.bycol[1,2]

#Look at the data points in row 1
Mat.bycol[1,]

#Look at the data points in column 2
Mat.bycol[,2]

#read in weather station file from the data folder
datW <-read.csv("y:\\Students\\hkropp\\a02\\2011124.csv")

#Get more information about the data frame
str(datW)

########QUESTION 1#########
#How many rows and columns are in this dataset?#
#There are 9 columns and 157849 rows#

#specify a colun with a proper date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
str(datW)

#create a date column by reformatting to only include years
#ensure it as as a number
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#######QUESTION 2#########
#Using your textbook or online R resources,
#describe the difference between character, numeric,
#integer, and factor data. Create an example vector
#of each data type with 5 objects in it

#find out all unique site names
levels(datW$NAME)

#Mean Max temperature for aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#Find mean with na.rm function
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm = TRUE)

#Calculate average daily temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#Change automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("Name", "MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

##########QUESTION 3##########
#Using help(hist) and help(paste) look up all of the arguments in my
#hist function above. Describe what each argument is doign in the hist
#function above

#creating the same histogram but with lines for mean and sd
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN ==1], na.rm =TRUE),
       col = "tomato3",
       lwd = 3)

#Add sd line below the mean in same color and thickness
abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE)
       -sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)

#Ad sd line above
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)
       + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#########QUESTION 4##############
#Make histograms for at least three other sites' daily
#average temp (you choose the sites, but label!)
#include SD and Mean
#use different colors for bars, add them all into the same window
#using par(mfrow=c(2,2)) before you run the code that makes all 4 histograms

