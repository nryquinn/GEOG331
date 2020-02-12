#create a function. The names of the arguments for your function will be in parentheses.
#everything in curly brackets will be rn each time the function is run.

assert <- function(statement, err.message){
  #if evaluates if a statement is true or fale for a single item
  if(statement == FALSE){
    print(err.message)
  }
}

#see how it works, evaluate false statement
assert(1 == 2, "error:unequal values")

#####Question 1######

#Describe how the accuracy of the relative humidity measurements vary with temperature and humidity

#####Question 2######

#Describe the quality assurance protocols in place for the anemometer.
#What type of unreliable measurements do they prevent? What are cases where 
#data would be incorrectly makred as unreliable?

######Continuing on ######
install.packages("lubridate")
library(lubridate)

#reading in the dataset
datW <- read.csv("c:\\Users\\nquinn\\Documents\\GitHub\\GEOG331\\Activity3\\bewkes_weather.csv",
                 na.strings =c("#N/A"), skip=3, header = FALSE)
#preview data
print(datW[1,])

#get sensor info from file
sensorInfo <- read.csv("c:\\Users\\nquinn\\Documents\\GitHub\\GEOG331\\Activity3\\bewkes_weather.csv",
                       na.strings =c("#N/A"),nrows=2)
print(sensorInfo)

#get column names from sensorInfo table and set weather sattion colnames to be the same
colnames(datW) <- colnames(sensorInfo)
print(datW[1,])

#####Question 3#####

#What is the deifference between skip and nrows in these two read.csv commands?
#What does header=FALSE also do?

#####Continuing On######

#convert to standardized date format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz = "America/New_York")

#calculate day of year
datW$doy <- yday(dates)

#calculate hour in the dat
datW$hour <-hour(dates) + (minute(dates)/60)

#calculate decimal date of year
datW$DD <- datW$doy + (datW$hour/24)

#quick preview of new date calculations
datW[1,]

#see how many values have missing data for each sensor observation
#air temp
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))
