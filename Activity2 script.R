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

########QUESTION 1##############################
#How many rows and columns are in this dataset?
#There are 9 columns and 157849 rows

#####CONTINUING ON##############################

#specify a colun with a proper date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
str(datW)

#create a date column by reformatting to only include years
#ensure it as as a number
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#######QUESTION 2##############################
#Using your textbook or online R resources,
#describe the difference between character, numeric,
#integer, and factor data. Create an example vector
#of each data type with 5 objects in it

Character = c("a", "b", "c", "d", "e", "f")
str(Character)

Numeric = c(1,2,3,4,5)
str(Numeric)

Integer = as.integer(Numeric)
str(Integer)

Factor = as.factor(c("First Year", "Sophomore", "Junior", "Senior", "Super Senior"))
str(Factor)

######CONTINUING ON #######

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

##########QUESTION 3###################
#Using help(hist) and help(paste) look up all of the arguments in my
#hist function above. Describe what each argument is doign in the hist
#function above

help(hist)
help(paste)

######## CONTINUING ON ################

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

par(mfrow = c(2,2))

###Site 2###
hist(datW$TAVE[datW$siteN == 2],
    freq=FALSE, 
    main = paste(levels(datW$NAME)[2]),
    xlab = "Average daily temperature (degrees C)", 
    ylab="Relative frequency",
    col="grey60",
    border="white")
#Adding the Mean line
abline(v = mean(datW$TAVE[datW$siteN ==2], na.rm =TRUE),
       col = "tomato3",
       lwd = 3)
#Adding SD Below
abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm = TRUE)
       -sd(datW$TAVE[datW$siteN == 2], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
#Adding SD Above
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE)
       + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

###Site 3###
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="blue50",
     border="white")
#Adding the Mean line
abline(v = mean(datW$TAVE[datW$siteN ==3], na.rm =TRUE),
       col = "tomato3",
       lwd = 3)
#Adding SD Below
abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm = TRUE)
       -sd(datW$TAVE[datW$siteN == 3], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
#Adding SD Above
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE)
       + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

###Site 4###
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="green50",
     border="white")
#Adding the Mean line
abline(v = mean(datW$TAVE[datW$siteN ==4], na.rm =TRUE),
       col = "tomato3",
       lwd = 3)
#Adding SD Below
abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm = TRUE)
       -sd(datW$TAVE[datW$siteN == 4], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
#Adding SD Above
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE)
       + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

###Site 5###
hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="yellow50",
     border="white")
#Adding the Mean line
abline(v = mean(datW$TAVE[datW$siteN ==5], na.rm =TRUE),
       col = "tomato3",
       lwd = 3)
#Adding SD Below
abline(v = mean(datW$TAVE[datW$siteN == 5], na.rm = TRUE)
       -sd(datW$TAVE[datW$siteN == 5], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
#Adding SD Above
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE)
       + sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


########CONTINUING ON###########

#Creating histogram for first site
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq = FALSE,
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average Daily Temperature (Degrees C)",
           ylab = "Relative Frequency",
           col = "grey50",
           border = "white")

##########plotting the normal curve##

#Generating a sequence of numbers to plot the normal curve across the range
#Creates the x coordinates
x.plot <-seq(-10,30, length.out = 100)

#dnorm produces probability density based on the 2 parameters: xbar and sd
y.plot <- dnorm(seq(-10,30, length.out = 100),
                mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
                sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))

#creates a density saled to fit the plot
#puts multiple thins on hte same plot
#Maximum value is the same between the two datasets on the plot
#creates the y coordinates
y.scaled <- (max(h1$density)/max(y.plot)) *y.plot

#points function adds points or lines
points(x.plot,
       y.scaled,
       type = "l",
       col = "royalblue3",
       lwd = 4,
       lty = 2)

######## QUESTION 5 #################
#Refer to the histograms you made in question 4,
#Does daily air temperature look like it is normally
#distributed at all sites?

###### continuing on ################

help(dnorm)
#dnorm used for probability densit
#pnorm returns vector for probability below q
#qnorm returns the value associated wtih a possibility


#probability of all average temperature below 0
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))

#probability of all average temperature below 5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))

#pnorm with 5 HERE gives you all probability from 0-5
(pnorm(5,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))- pnorm(0,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE)))

#pnorm with 20 is probability below 20, subtracting from 1 does area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
          sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))

#qnorm returns the exact probability
#using a data of 0.95 makes you observe probability of an unusually high temp
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))

###### QUESTION 6 ######################
#Assume CC increases mean temp by 4 degrees C in Aberdeen,
#but the standard deviation stays the same as current climate.
#How often do you expect to observe temperatures greater than the
#current threshold for extreme high temperatures?

##### QUESTION 7 ######################
#Make a hist of daily precipitation for Aberdeen. This is an example of a data distribution
#that isn't normal. Look up exponential, beta, and gamma distribution using the internet.
#Look at the range of values these descriptions can describe. Would any of these
#distributions describe the shape of daily precipitation data?

##### QUESTION 8 #####################
#Use the sum function to get precipitation for each year and site in the data
#Choose one site and make a histogram of annual precipitation. 
#Describe the general shape of the data and whether you think it is normally distributed

##### QUESTION 9 ####################
#Get the mean of annual precipitation for all sites. Compare to the mean
#annual temperatures you calculated earlier. In general terms,
#describe how the climate varies between sites.

#### QUESTION 10 ####################
#What's the GitHub link to your script for this activity?