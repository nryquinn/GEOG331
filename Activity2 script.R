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
