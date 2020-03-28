# Setup ----
#' Clear the environment and load the plm and the stargazer packages.
# Introduction ----

rm(list=ls())
airquality<-airquality
# Inject outliers into data.
cars1 <- cars[1:30, ]  # original data
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))  # introduce outliers.
cars2 <- rbind(cars1, cars_outliers)  # data with outliers.

# Plot of data with outliers.
par(mfrow=c(1, 2))
plot(cars2$speed, cars2$dist, xlim=c(0, 28), ylim=c(0, 230), main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

# Plot of original data without outliers. Note the change in slope (angle) of best fit line.
plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), main="Outliers removed \n A much better fit!", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)

#Detect Outliers 


par(mfrow=c(1, 1))
outlier_values <- boxplot.stats(airquality$Ozone)  # outlier values.
boxplot(airquality$Ozone, main="Ozone", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Bivariate approach

# For categorical variable
boxplot(Ozone ~ Month, data=airquality, main="Ozone reading across months")  # clear pattern is noticeable.
boxplot(Ozone ~ Day, data=airquality, main="Ozone reading for days of week")  # this may not be significant, as day of week variable is a subset of the month var.


# For continuous variable (convert to categorical if needed.)
boxplot(Ozone ~ Solar.R, data=airquality, main="Boxplot for Solar (continous) vs Ozone")
boxplot(Ozone ~ cut(Solar.R, pretty(airquality$Solar.R)), data=airquality, main="Boxplot for Solar (categorial) vs Ozone", cex.axis=0.5)

#Multivariate Model Approach

##Cooks distance
mod <- lm(Ozone ~ ., data=airquality)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

