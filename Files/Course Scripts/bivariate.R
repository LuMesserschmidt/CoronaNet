
# Packages and script set-up ---- 

rm(list = ls())
# install.packages("tidyverse")
library(tidyverse)

# Loading the dataset ----

data<- mtcars

#Bivariate statistics

##T-Test

# Conduct an independent sample t-test of the number of Cylinders and the Miles per Gallon (mpg) 
t.test(data$mpg[data$cyl==4],
       data$mpg[data$cyl==8])


# Run a dependent sample t-test on variables
t.test(data$gear, data$carb, paired=TRUE)


# Run the one-way ANOVA model
aov(data$mpg ~ data$cyl, data=data)

# Display full ANOVA results
summary(aov(data$mpg ~ data$cyl, data=data))

# Chi-Squared Test
chisq.test(data$mpg, data$cyl, correct=FALSE)


# Correlation


cor(data$cyl,data$mpg, use="complete.obs")


# Get a subset of variables
correlationsubset <- data[c("mpg","cyl","hp")]
# Get the correlation matrix
cor(correlationsubset, use="complete.obs")

