
# Packages and script set-up ---- 

rm(list = ls())
library(tidyverse)

# Loading the car dataset ----

data<-mtcars
# Descriptive Analysis ----

## Explore the data structure ----

#' For a first overview over our data, we can use the base functions 
#' (e.g. those which are without any package installations part of R) 
#' names() str(), head(), summary(), table(), quantile(), and View().
names(data)
str(data)
head(data)
summary(data)
table(data$mpg) # you could use any other variable here.
quantile(data$mpg, na.rm = TRUE)
View(data)

## Mean
mean(data$cyl, na.rm=T) 

## Median
median(data$cyl, na.rm=T) 

##Range

range(data$cyl, na.rm=T)

#Table

## Onedimesnional Table: Cylinder
table(data$cyl)

## Multi-dimensional Table: Cylinder and Gear
table(data$cyl,data$gear) #


# Histogram

hist(data$gear, main = paste("Gear Histogram"),xlab="Cylinder in numbers", ylab="Frequence")


# Boxplot 

boxplot(data$mpg, na.rm=T,main=paste("Boxplot mpg"))




