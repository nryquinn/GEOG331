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

#What is the difference between skip and nrows in these two read.csv commands?
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

#make aplot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Soil Moisture (cm3 water per cm3 soil")

#Making a plot for air temperature
plot(datW$DD, datW$air.temperature, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Air Temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularily confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check extreme range and throughout percentiles
quantile(datW$air.tempQ1)
#looking at days with low air temp
datW[datW$air.tempQ1 < 8,]
#looking at days with high air temp
datW[datW$air.tempQ1 > 33,]

##### Question 4#####
#Are the extreme high and low values in this dataset reliably measured by the sensor?
#Explain your answer
quantile(datW$solar.radiation)
quantile(datW$precipitation)
quantile(datW$lightning.acvitivy)
quantile(datW$lightning.distance)
quantile(datW$wind.dir)
quantile(datW$wind.speed.Q1, na.rm = TRUE)
quantile(datW$gust.speed)
quantile(datW$atmospheric.pressure)
quantile(datW$soil.moisture, na.rm = TRUE)
quantile(datW$soil.temp, na.rm = TRUE)


#####Continuing On#####

#plot precipitation and lighting strikes on same plot

#normalize lightning strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & Lightning",
     type = "n")

#plot precipitation points only when there is precip.
#Make the points semi transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col = rgb(95/255, 158/255, 160/255, 0.5), pch = 15)

#plot lightning points only when there is lightning
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col = "tomato3", pch = 19)

#####Question 5#####
#The variable lightscale used to graph lightning values on the same plot as precipitation
#is not in the datW dataframe. Explain why you can still use it to subset values in datW.
#Provide evidence for your answer by creating a test that uses your assert function from part 1

assert()
nrow(datW)
length(ligthscale)
#Creating lightscale as a dataframe to count the number of rows
#Number of rows is the number of observations
assert((nrow(datW)) == (length(lightscale)), "error: unequal values")


#####Continuing On#####

#filter out storms in wind and air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm
#create a new airtemp column
datW$airtempQ2 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0, NA,
                         ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#####Question 6######
#Repeat the code above to remove suspect measurements from wind speed measurements
#Create a test using assert to verify that this filtered the data as expected.
#Describe your outcome. Include a plot with both lines and points of windspeed with new data.
datW$wind.speed.Q1 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0, NA,
                             ifelse(datW$precipitation > 5, NA, datW$wind.speed))
head(datW$wind.speed.Q1)
length(datW$wind.speed.Q1[is.na(datW$wind.speed.Q1)])
length(datW$airtempQ2[is.na(datW$airtempQ2)])
assert(length(datW$wind.speed.Q1[is.na(datW$wind.speed.Q1)]) == length(datW$airtempQ2[is.na(datW$airtempQ2)]) , "error: Unequal Values")

plot(datW$DD, datW$wind.speed.Q1, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Wind Speed (m/s)")

#####Question 7#####
#Check that the soil temperature and moisture measurements are reliable in the days
#leading up to the soil sensor outage. Explain your reasoning and show all code.
#Keep in mind that precipitation and air temperature can be used to help provide context
par(mfrow=c(1,1))
plot(datW$DD, datW$soil.moisture, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Soil Moisture (mm)", xlim = c(185,195))
points(datW$DD[datW$DD < 192.8750], datW$precipitation[datW$DD < 192.8750], pch = 19, col = "tomato3")


plot(datW$DD, datW$soil.temp, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Soil Temperature (degrees C", xlim = c(185,195), ylim = c(10, 30))
points(datW$DD[datW$DD < 192.8750], datW$airtempQ2[datW$DD < 192.8750], pch = 19, col = "tomato3")

#Note: soil moisture/temp measurements were tampered with in mid-July

#####Question 8#####
#The researchers requested a table with the average air temperature, wind speed, soil moisture,
#and soil temperature for their study period. They would also like the total precipitation.
#Indicate how many observations went into these calculations and the time period of measurement.
#Report your findings with the correct number of decimal places that is within the sensor error


Mean.Air.Temp <- c(mean(datW$airtempQ2,na.rm = TRUE))
Mean.Wind.Speed <- c(mean(datW$wind.speed.Q1, na.rm = TRUE))
Mean.Soil.Moisture <- c(mean(datW$soil.moisture, na.rm = TRUE))
Mean.Soil.Temp <- c(mean(datW$soil.temp, na.rm = TRUE))
Total.Precipitation <- c(sum(datW$precipitation))

#Rounding
#Air Temp has resolution of 0.1 degrees Celsius
Rounded.Mean.Air.Temp <- round(Mean.Air.Temp, 1)
#Wind Speed has resolution of 0.01 m/s
Rounded.Mean.Wind.Speed <- round(Mean.Wind.Speed,2)
#Soil Moisture has resolution of 0.0008 m3/m3
Rounded.Mean.Soil.Moisture <- round(Mean.Soil.Moisture,2)
#Soil Temperature has resolution of 0.1 degree celsius
Rounded.Mean.Soil.Temp <- round(Mean.Soil.Temp, 1)
#Precipitation has resolution of 0.017 mm
Rounded.Total.Precipitation <- round(Total.Precipitation, 3)


Mydata <- data.frame(Rounded.Mean.Air.Temp, Rounded.Mean.Wind.Speed, Rounded.Mean.Soil.Moisture, Rounded.Mean.Soil.Temp, Rounded.Total.Precipitation)
names(Mydata)<- c("Mean Air Temperature (degrees Celsius)", "Mean Wind Speed (m/s)", "Mean Soil Moisture (m3/m3)", "Mean Soil Temperature (degrees Celsius",
                  "Total Precipitation (mm)")

#finding lengths
length.airtemp = c(length(datW$air.temperature) - length(datW$airtempQ2[is.na(datW$airtempQ2)]))
length.windspeed = c(length(datW$wind.speed) - length(datW$wind.speed.Q1[is.na(datW$wind.speed.Q1)]))
length.soilmoisture = c(length(datW$soil.moisture))
length.soiltemp = c(length(datW$soil.temp))
length.precip = c(length(datW$precipitation))

LengthData <- data.frame(length.airtemp, length.windspeed, length.soilmoisture, length.soiltemp, length.precip)
names(LengthData) <- c("Mean Air Temperature (degrees Celsius)", "Mean Wind Speed (m/s)", "Mean Soil Moisture (m3/m3)", "Mean Soil Temperature (degrees Celsius",
                       "Total Precipitation (mm)")


#Time Period
TimePeriod <- data.frame("6/12/18 - 7/26/18", "6/12/18 - 7/26/18", "6/12/18 - 7/11/18", "6/12/18 - 7/11/18", "6/12/18 - 7/26/18")
names(TimePeriod) <- c("Mean Air Temperature (degrees Celsius)", "Mean Wind Speed (m/s)", "Mean Soil Moisture (m3/m3)", "Mean Soil Temperature (degrees Celsius",
                       "Total Precipitation (mm)")

#Binding Together
FinalData <- rbind(Mydata, LengthData, TimePeriod)
rownames(FinalData) <- c("Data Requested", "Number of Observations", "Time Period")

#####Question 9######
#Make four plots of soil moisture, air temperature, soil temperature, and precipitation throughout
#all observations in the study period. Use the same x axis range for each plot. In a few sentences, 
#briefly describe trends in the data.

par(mfrow = c(4,1))
plot(datW$DD, datW$airtempQ2, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Air Temperature (degrees C)")
plot(datW$DD, datW$soil.temp, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Soil Temperature (degrees C)")
plot(datW$DD, datW$precipitation, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Precipitation (mm)")
plot(datW$DD, datW$soil.moisture, pch = 19, type = "b", xlab = "Day of Year",
     ylab = "Soil Moisture (mm)")

#####Question 10#####
#Copy the URL of your Rscript in your GitHub Repository