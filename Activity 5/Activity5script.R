#####Question 1 ######
#Given the description and location of the stream, what factors would you expect
#to influence streamflow? Would there be times taht you might expect higher flow
#than would be typical for a given amount of rain in the drainage basin?

#####Continuing On#####

#load lubridate
library(lubridate)

#reading in the data
datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)

#reading in precipitation data
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")
head(datP)

#Only use most reliable measurements
datD <-datH[datH$discharge.flag == "A", ]

###define time for streamflow###
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <-year(datesD)
#define time
timesD <- hm(datD$time)

###define time for precipitation ###
dateP <-ymd_hm(datP$DATE)
#get doy
datP$doy <-yday(dateP)
#get year
datP$year <-year(dateP)

###get decimal formats
#convert time from a string to a more usable format w/ decimal hour
datD$hour <- hour(timesD) + (minute(timesD)/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <-ifelse(leap_year(datD$year), datD$year + (datD$decDay/366),
                      datD$year  + (datD$decDay/365))
#calculate times for dat                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))

#####Question 2######
#Explain how decimal year is calculated and how leap year is accounted for.
#What do the results of the leap_year function look like?

#####Continuing on######

#plot discharge
plot(datD$decYear, datD$discharge, type = "l", xlab = "Year",
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#####Question 3#####
#How many observations are in the stream flow and precipitation data?
#What is the frequency of the observations for each data type?

#####Question 4#####
#Look up the documentation on the expression function and explain what
#expression(paste()) in the plot argument did. Are there any issues with 
#this default plot formatting and labels? How does resizing the plot affect these issues?

######Continuing On#####

#basic formatting
aveF <- aggregate(datD$discharge, by = list(datD$doy), FUN = "mean")
colnames(aveF) <- c("doy", "dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN = "sd")
colnames(sdF) <- c("doy", "dailySD")

#bigger margins
par(mai = c(1,1,1,1))
#make plot
plot(aveF$doy, aveF$dailyAve,
     type = "l",
     xlab = "Year",
     ylab = expression(paste("Discharge ft "^"3 "," sec"^"-1")))

#Adding Polygons
#bigger margins
par(mai = c(1,1,1,1))
#plot it
plot(aveF$doy, aveF$dailyAve,
     type = "l",
     xlab = "Year",
     ylab = expression(paste("Discharge ft"^"3 "," sec"^"-1")),
     lwd =2,
     ylim = c(0,90),
     xaxs = "i", yaxs = "i") #remove gaps from axes
#show sd around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col = rgb(0.392,0.584,0.929,.2),#color that is semi transparent
        border=NA#noborder
        )

#using the axis function and las arguments
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360,by=40)) #tick labels
axis(2, seq(0,80,by=20),
     seq(0,80,by=20),
     las=2)#show ticks at 90 degree angle

#adding a legend
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean", "1 standard deviation"), #legend items
       lwd =c(2,NA), #lines
       fill = c(NA,rgb(0.392,0.584,0.929,0.2)),#fill boxes
       border=NA, #no border for both fill boxes (don't need a vector here since both are same)
       bty = "n")#no legend border

#making it line up
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean", "1 standard deviation"), #legend items
       lwd = c(2,NA), #lines
       col = c("black", rgb(0.392,0.584,0.929,0.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

######Question 5#####
#Add a line that shows observations for 2017 onto this graph of the average.
#You may have toa djust the axes limits. Change the x axis label so that they show
#each month instead of doy. Make the 2017 line a different color than current colors in your plot.

#####Question 6#####
#Describe the trends in streamflow in 2017 and the mean/sd. After looking at
#this plot more closely , why do you think median and quartiles might better 
#represent typical conditions for streamflow discharge compared to the mean and sd?

#####Question 7#####
#create a dataframe that indicates what days have a full 24 hours of precipitation
#measurements. Make a plot of all discharge measurements and symbolize the days
#that have all precipitation measurements available. Be sure to include all labels.

#####Continuing On#####

#subset discharge and precipitation within range of interest
hydroD <-datD[datD$doy >= 248 & datD$doy <250 & datD$year == 2011,]
hydroP <-datP[datP$doy >= 248 & datP$doy <250 & datP$year == 2011,]
min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl


#plot it
par(mai=c(1,1,1,1))
plot(hydroD$decDay,
     hydroD$discharge,
     type="l",
     ylim = c(yl,yh),
     lwd=2,
     xlab="Day of Year",
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#####Question 8#####
#Choose another day to make a second hydrograph during the winter.
#Explain how you chose a time period. How do the two hydrographs
#compare? Are there any limitatios in interpreting the hydrograph
#given we only have hourly precipitiation? Why do you think spikes in
#streamflow exist without rain?

######Continuing On######
library(ggplot2)
#specify year as a factor
datD$yearPlot <-as.factor(datD$year)
#make a boxplot
ggplot(data = datD, aes(yearPlot,discharge)) +
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) +
  geom_violin()

#####Question 9#####
#Make a violin plot by season for 2016 and 2017 separately.
#Be sure the plots are aesthetically pleaisng and properly labelled.
#Describe differences in streamflow discharge between seasons and years.

#####Question 10#####
#Copy and paste the GitHub URL for your Rscript here