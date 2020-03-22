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
#observe data
head(datH)


#reading in precipitation data
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")

#observe data
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
datD$decYear <-ifelse(leap_year(datD$year), datD$year + ((datD$doy-1)/366),
                      datD$year  + ((datD$doy-1)/365))
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
leap_year(datP$year)

#####Continuing on######

#plot discharge
plot(datD$decYear, datD$discharge, type = "l", xlab = "Year",
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#####Question 3#####
#How many observations are in the stream flow and precipitation data?
#What is the frequency of the observations for each data type?

#using length to determine the amount of obs.
length(datP)
length(datD)
length(datH)

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
     xaxis="i", yaxis ="i",#remove gaps from axes
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
#You may have to adjust the axes limits. Change the x axis label so that they show
#each month instead of doy. Make the 2017 line a different color than current colors in your plot.

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,100),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

datD2017 <- (datD[datD$year == 2017,])
DailyDischarge2017 <- aggregate(datD2017$discharge, by = list(datD2017$doy), FUN = "mean")

#creating tick marks

Months = c(0,"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec") #Creating label for ticks
lines(DailyDischarge2017, col = "tomato3", lwd = 1.5)
axis(1, seq(0,360, by=30), #tick intervals
     lab=Months) #tick labels
axis(2, seq(0,100, by=20),
     seq(0,100, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean", "1 standard deviation", "2017"), #legend items
       lwd = c(2,NA), #lines
       col = c("black", rgb(0.392,0.584,0.929,0.2), "tomato3"),#colors
       pch=c(NA,15),#symbols
       bty="n") #no legend border

#####Question 6#####
#Describe the trends in streamflow in 2017 and the mean/sd. After looking at
#this plot more closely , why do you think median and quartiles might better 
#represent typical conditions for streamflow discharge compared to the mean and sd?

#streamflow in 2017 appears to follow the trend of the mean; however, it does appear to have larger extremes such as in between months 2 and 3, as well as
#in the spikes between months 3/4, month 6, and month 10. In most places it falls within the SD, at least on the lower end, with some outliers.

#####Question 7#####
#create a dataframe that indicates what days have a full 24 hours of precipitation
#measurements. Make a plot of all discharge measurements and symbolize the days
#that have all precipitation measurements available. Be sure to include all labels.

#aggregating to only show desired variables
Agg_Precip<-aggregate(datP$hour, by=list(datP$doy,datP$year), length)
colnames(Agg_Precip) <- c("doy","year","HourCount")
#aggregating to hours which equal 24
Full24<-Agg_Precip[which(Agg_Precip$HourCount == 24),]


par(mai=c(1,1,1,1))
#plotting discharge
plot(datD$decYear,datD$discharge,     
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#Creating decimal year for Full24 so as to add to plot
Full24$decYear <- ifelse(leap_year(Full24$year),Full24$year + ((Full24$doy-1)/366),
                          Full24$year + ((Full24$doy-1)/365))

#visualizing days with 24 hours of measurements
points(Full24$decYear, y=rep(0,length(Full24$decYear)), pch=19, col="tomato3")

legend("topleft", c("discharge","24 hours of precip data"), #adding legend items
       lwd=c(2,NA),#lines
       col=c("black","tomato3"), #signifying colors
       pch=c(NA,19),
       bty="n")


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
#compare? Are there any limitations in interpreting the hydrograph
#given we only have hourly precipitiation? Why do you think spikes in
#streamflow exist without rain?

hydroD2 <-datD[datD$doy >= 32 & datD$doy <34 & datD$year == 2008,]
hydroP2 <-datP[datP$doy >= 32 & datP$doy <34 & datP$year == 2008,]
min(hydroD2$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydroD2$discharge))-1
#celing rounds up to the integer
yh2 <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <- ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydroP2$HPCP) + yl2


#plot it
par(mai=c(1,1,1,1))
plot(hydroD2$decDay,
     hydroD2$discharge,
     type="l",
     ylim = c(yl2,yh2),
     lwd=2,
     xlab="Day of Year",
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation
for(i in 1:nrow(hydroP2)){
  polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
            hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
          c(yl,hydroP2$pscale[i],hydroP2$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


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
help("geom_violin")

#load package
library(ggplot2)

#doy of seasons in 2016
#First day of winter = 12/21, doy = 355
#first day of spring = 3/20, doy = 80
#First day of summer = 6/20, doy = 172
#first day of fall = 9/22, doy = 266

#creating dataframe for 2016
datD2016 = datD[datD$year == 2016,]
#adding in column for seasons
datD2016$season <- ifelse(datD2016$doy < 80, "winter",
                          ifelse(datD2016$doy < 172, "spring",
                                 ifelse(datD2016$doy < 266, "fall",
                                        ifelse(datD2016$doy < 355, "summer", "winter"))))
#making seasons a factor
datD2016$season_plot <- as.factor(datD2016$season)

#plotting it
ggplot(data= datD2016, aes(season_plot,discharge, color = season_plot)) +
  geom_violin() + 
  labs(title = "Discharge by Season in 2016",x = "Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



  
#Repeat for 2017
#doy of seasons in 2017
#First day of winter = 12/21, doy = 354
#first day of spring = 3/20, doy = 79
#First day of summer = 6/21, doy = 172
#first day of fall = 9/22, doy = 265

#creating dataframe for 2016
datD2017_2 = datD[datD$year == 2017,]
#adding in column for seasons
datD2017_2$season <- ifelse(datD2017_2$doy < 79, "winter",
                          ifelse(datD2017_2$doy < 172, "spring",
                                 ifelse(datD2017_2$doy < 265, "fall",
                                        ifelse(datD2017_2$doy < 354, "summer", "winter"))))
#making seasons a factor
datD2017_2$season_plot <- as.factor(datD2017_2$season)

#plotting it
ggplot(data= datD2017_2, aes(season_plot,discharge, color = season_plot)) +
  geom_violin() + 
  labs(title = "Discharge by Season in 2017", x = "Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#####Question 10#####
#Copy and paste the GitHub URL for your Rscript here
