#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length
versicolor <- iris[iris$Species == "versicolor", ]


x <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
y <- c("Sepal.Width", "Petal.Width", "Petal.Length")

lm.out <- list()

#paste function necessary if the objects in dataframes are factors (r usually assumes they are)
#use double brackets when making "list of lists"
for(i in 1:3){
  lm.out[[i]] <- lm(versicolor[ ,paste(y[i])] ~ versicolor[ ,paste(x[i])])
}

lm.out
str(lm.out)
summary(lm.out)
lm.out[[1]]
lm.out[[2]]

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data fram
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#iris left
#height right
#join height into iris
iris2 <- left_join(iris, height, by = "Species")
iris2$Petal.Width/iris2$Height.cm

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(data = iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data = iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point()+
  theme_classic()


#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size

ggplot(data = iris, aes(Sepal.Length, Sepal.Width, color=Species)) +
  geom_point(size = 4)+
  theme_classic()

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		