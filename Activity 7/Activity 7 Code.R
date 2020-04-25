#Installing new packages
install.packages(c("mapview","caret","randomForest","nnet"))

#loading packages required
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(mapview)
library(caret)
library(randomForest)
library(nnet)

#set up directory for oneida data folder
dirR <- "/Users/Nick 1/Documents/GEOG331/Activity 7/oneida"

#read in Sentinel data

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB5 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B05_20m.tif"))
rdatB6 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B06_20m.tif"))
rdatB7 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B07_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))
rdatB11 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B11_20m.tif"))
rdatB12 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B12_20m.tif"))
clouds <- raster(paste0(dirR,"/sentinel/MSK_CLDPRB_20m.tif"))

#read in validation data
#here verbose=FALSE hiddes
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
built <- readOGR(paste0(dirR,"/Oneida/built.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

#stack red green blue
rgbS <- stack(rdatB4,rdatB3,rdatB2)
#stack all raster data
allbands <- stack(rdatB2,rdatB3,rdatB4,rdatB5,rdatB6,rdatB7,rdatB8,rdatB11,rdatB12,clouds)

#view raster, maximum digigtal is around 20000 so set scale to that
plotRGB(rgbS, scale=20000)

#view raster, maximum digigtal is around 20000 so set scale to that
plotRGB(rgbS, scale=20000, stretch="lin")

#use mapview package
#view rgb and set up a contrast stretch, exclude clouds with high values
viewRGB(rgbS,r=1,g=2,b=3,maxpixels =  2297430, #view all pixels don' lower resolution
        quantiles = c(0.00,0.995), #quantiles for stretch. Cuts off high reflectance from clouds
        homebutton=FALSE,
        viewer.suppress=FALSE)#view in Rstudio 

#use mapview package
#view rgb and set up a contrast stretch, exclude clouds with high values
#and view all landcover types
viewRGB(rgbS,r=1,g=2,b=3,maxpixels =  2297430,quantiles = c(0.00,0.995), homebutton=FALSE,
        viewer.suppress=FALSE)+mapview(algae, color="grey25",col.regions="palegreen")+mapview(agri, color="grey25",col.regions="violet")+mapview(built, color="grey25",col.regions="darkgoldenrod3")+mapview(forest, color="grey25",col.regions="tan4")+mapview(water, color="grey25",col.regions="royalblue")+mapview(wetlands, color="grey25",col.regions="orangered2")

#plotting cloud mask
plot(allbands[[10]])

#set clouds to NA
allbandsCloud <- list()
for(i in 1:9){
  allbandsCloud[[i]] <- setValues(allbands[[i]],
                                  ifelse(getValues(allbands[[10]])>60,NA,getValues(allbands[[i]])))
  
}
allbandsCloudf <- stack(allbandsCloud[[1]],allbandsCloud[[2]],allbandsCloud[[3]],
                        allbandsCloud[[4]],allbandsCloud[[5]],allbandsCloud[[6]],
                        allbandsCloud[[7]],allbandsCloud[[8]],allbandsCloud[[9]])

#view all layers
plot(allbandsCloudf)

#plotting false color image
plotRGB(allbandsCloudf,r=4, g=3, b=2,  
        scale=10000, 
        stretch="lin", 
        margins=TRUE,
        colNA="grey50")

#####Question 1#####
#Describe the difference in reflected/emitted light across the land surface.
#What are some major differences you notice in the different land classes?

#####Classifying LandCover: Validation and Training Data#####

#set seed so samples always the same
set.seed(12153)

#randomly select 

algSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
algData <- rep("train",120)
#randomly replace half of the data to be validating data
algData[algSamp] <- "valid"

waterSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
waterData <- rep("train",120)
#randomly replace half of the data to be validating data
waterData[waterSamp] <- "valid"

agriSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
agriData <- rep("train",120)
#randomly replace half of the data to be validating data
agriData[agriSamp] <- "valid"

builtSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
builtData <- rep("train",120)
#randomly replace half of the data to be validating data
builtData[builtSamp] <- "valid"

forestSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
forestData <- rep("train",120)
#randomly replace half of the data to be validating data
forestData[forestSamp] <- "valid"

wetlandsSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
wetlandsData <- rep("train",120)
#randomly replace half of the data to be validating data
wetlandsData[wetlandsSamp] <- "valid"

#create id table that gives each landcover an ID
landclass <- data.frame(landcID= seq(1,6),
                        landcover = c("algal bloom", "open water","agriculture","built","forest","wetlands"))

#set up table with coordinates and data type (validate or train) for each point
landExtract <-  data.frame(landcID = rep(seq(1,6),each=120),
                           sampleType=c(algData,waterData,agriData,builtData,forestData, wetlandsData),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],built@coords[,1],forest@coords[,1],wetlands@coords[,1] ),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],built@coords[,2],forest@coords[,2],wetlands@coords[,2] ))

#extract raster data at each point
#using point coordinates
rasterEx <- data.frame(extract(allbandsCloudf,landExtract[,3:4]))
#give names of bands
colnames(rasterEx) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")

#combine point information with raster information
dataAll <- cbind(landExtract,rasterEx)
#preview
head(dataAll)

#Separating training and validation data
trainD <- dataAll[dataAll$sampleType == "train",]
validD <- dataAll[dataAll$sampleType == "valid",]

#####RandomForest#####
#Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:sqrt(9)) # number of variables available for splitting at each tree node

#getting rid of the clouds from landcID points
trainD <- na.omit(trainD)

# Train the random forest model to the Sentinel-2 data
#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainD[,c(5:13)], #digital number data
                         y = as.factor(trainD$landcID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model

# Change name in raster stack to match training data
names(allbandsCloudf) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")
# Apply the random forest model to the Sentinel-2 data
rf_prediction <- raster::predict(allbandsCloudf, model=rf_model)
#view predictions
plot(rf_prediction)

#landcover class names
landclass

#set up categorical colors to improve map
landclass$cols <-c("#a6d854","#8da0cb","#66c2a5",
                   "#fc8d62","#ffffb3","#ffd92f")
#make plot and hide legend
plot(rf_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols ,bty="n")

#get validation data from raster by extracting 
#cell values at the cell coordinates
rf_Eval <- extract(rf_prediction, validD[,3:4])

#make the confusion matrix
rf_errorM <- confusionMatrix(as.factor(rf_Eval),as.factor(validD$landcID))
#add landcover names
colnames(rf_errorM$table) <- landclass$landcover
rownames(rf_errorM$table) <- landclass$landcover
#view the matrix
rf_errorM$table

#look at the overall accuracy
rf_errorM$overall

#####Question 2#####
#What land classes had the highest rates of misclassification?
#What sort of bias would you introduce if you used these predictions in an analysis


######Neural Network######

#set up grid
nnet.grid <- expand.grid(size = seq(from = 28, to = 32, by = 2), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.1, to = 0.5, by = 0.1)) # regularization parameter to avoid over-fitting 

#training the model
nnet_model <- caret::train(x = trainD[,c(5:13)], y = as.factor(trainD$landcID),
                           method = "nnet", metric="Accuracy", trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)

#view the training summary
nnet_model

# Apply the neural network model to the Sentinel-2 data
nnet_prediction <- raster::predict(allbandsCloudf, model=nnet_model)

#make plot and hide legend
plot(nnet_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE)
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols ,bty="n")

#extract predictions
nn_Eval = extract(nnet_prediction, validD[,3:4])
#confusion matrix
nn_errorM = confusionMatrix(as.factor(nn_Eval),as.factor(validD$landcID))
colnames(nn_errorM$table) <- landclass$landcover
rownames(nn_errorM$table) <- landclass$landcover
nn_errorM$table
nn_errorM$overall

#comparing the prediction models through maps
par(mfrow=c(2,1), mai=c(0,0,0,0))
#random forest
plot(rf_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE)
#legend
legend("bottomright", paste(landclass$landcover),
       fill=landclass$cols ,bty="n",
       cex = 0.8, ncol = 2)
#add title
mtext("Random Forest", side=3,cex=1.5, line=-2.5)

#neural network
plot(nnet_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
#add legend
legend("bottomright", paste(landclass$landcover),
       fill=landclass$cols ,bty="n",
       cex = 0.8, ncol = 2)
#add title
mtext("Neural Network", side=3,cex=1.5, line = -3.25)

######Question 3#####
#Show both maps from your classification outcome in your document along with your confusion matrices.
#Describe the spatial patterns and general location of different land classes.
#Describe the general differences between predictions from each method.

#####Analyzing Predictions of land cover#####

#cell count neural net
freq(nnet_prediction)

#cell count random forest
freq(rf_prediction)

######Question 4#####
#Knowing each cell is 20m x 20m what is difference in area predicted to be algal blooms in each method?
#Which method predicts a higher area of algal blooms?

#Creating unique variables for each confusion matrix
AlgalBloom_RF = freq(rf_prediction)[1,2]
AlgalBloom_NN = freq(nnet_prediction)[1,2]

#Calculating Area, as the cell area is 400 square meters
AlgalBloom_RF_Area = AlgalBloom_RF*400
AlgalBloom_NN_Area = AlgalBloom_NN*400

#Finding the difference in area
AlgalBloom_NN_Area - AlgalBloom_RF_Area


#####Question 5#####
#Create a raster that shows whether neural network and random forest predictions agree or disagree.
#Describe the spatial patterns you observe. Where are these areas prone to disagreement between predictions?

#creating the raster
Pred_Raster = rf_prediction == nnet_prediction
#plotting it
dev.off() # to reset the graphics pars to defaults
plot(Pred_Raster,
     col=c("orangered4", "seagreen1"),
     legend=FALSE,
     axes = FALSE)
#adding legend
legend("bottomleft",c("Predictions Disagree", "Predictions Agree"),
       fill = c("orangered4", "seagreen1"),
       cex = 1)
#adding title
mtext("Alignment of Random Forest",
      side=3,cex=1.5, line=-5)
mtext("And Neural Network",
      side=3,cex=1.5, line=-6.5)
mtext("Predictions",
      side=3,cex=1.5, line=-8)

#####Question 6#####
#What is the producers and users accuracy for algal blooms for both methods?
#What is the producers and users accuracy for agriculture in both methods?

#calculating sums for RF
RF_rowsum = rowSums(rf_errorM$table[1:6,]) #Specific Row sums are denominator for Users Accuracy
RF_colsum = colSums(rf_errorM$table[,1:6]) #Specific Col sums are denominator for Producers Accuracy

#calculating sums for NN
NN_rowsum = rowSums(nn_errorM$table[1:6,]) #Specific Row sums are denominator for Users Accuracy
NN_colsum = colSums(nn_errorM$table[,1:6]) #Specific Col sums are denominator for Producers Accuracy

#RF Error
(rf_errorM$table[1,1]/RF_colsum[1])*100 #Algal Bloom Producers Accuracy
(rf_errorM$table[1,1]/RF_rowsum[1])*100 #Algal Bloom Users Accuracy
(rf_errorM$table[3,3]/RF_colsum[3])*100 #Agriculture Producers Accuracy
(rf_errorM$table[3,3]/RF_rowsum[3])*100 #Agriculture Users Accuracy
           
#NN Error
(nn_errorM$table[1,1]/NN_colsum[1])*100 #Algal Bloom Producers Accuracy
(nn_errorM$table[1,1]/NN_rowsum[1])*100 #Algal Bloom Users Accuracy
(nn_errorM$table[3,3]/NN_colsum[3])*100 #Agriculture Producers Accuracy
(nn_errorM$table[3,3]/NN_rowsum[3])*100 #Agriculture Users Accuracy

#####Question 7#####
#Algal blooms are prone to occur in areas where sunlight, stagnant water, and excess nutrient inputs can promote growth.
#Visually, do you think the landcover around the lake contributes to the algal blooms?
#Explain your answer. How would you answer this question quantitatively using spatial analysis techniques?


#####Question 8#####
#Which prediction method would you use for a more formal quantitative analysis of question 7?
#What bias would you need to consider in the interpretation of your results?


#####Question 9#####
#This type of prediction can only classify algal growth from a satellite measurement.
#What might some of the issues with relying on satellites to observe the presence of algal blooms?
#What data and approaches might you use to predict whether an algal bloom is expected to occur in the future?


#####Question 10#####
#Copy the link to your GitHub code.