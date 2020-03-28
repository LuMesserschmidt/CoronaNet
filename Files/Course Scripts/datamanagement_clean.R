
# Packages and script set-up ---- 

rm(list = ls())
# install.packages("tidyverse")
library(tidyverse)

# Loading the dataset ----

## .csv ----
 
#'Complete the empty spaces! (soep_europ.csv)
data <- read.csv("_______") # Looks weird....

#' To read the data correctly, we have to add the argument "sep" to the function.
data <- _____("data/soep_europ.csv", sep="_____") #

 
#' 2 Addenda: 

#'1. Try out to to read a US style csv with soep_us.csv**

data <-
  
#' 2. An easier way to read European style csv files is to use read_csv2() from the tidyverse. 
#'   (Accordingly, read_csv() is for US style csv files)

data <-



## .dta / .sav / excel ----

data <- haven::read_dta("data/soep.dta")

data <- haven::read_sav("data/soep.sav")

data <- readxl::read_xlsx("data/soep.xlsx")

data <- read_csv2("data/soep_europ.csv")


# Basic Data Management ----

## The pipe %>% ----

mean(c(1:5))
# can be expressed with the pipe as
c(1:5) %>%
  mean()

## Select the relevant variables ----

#' Accordingly, we `select` the relevant ones:
#' hhnr, persnr, gebjahr, sex, tp72 (Work Overtime), tp7001 (Contracted Working Hours), 
#' tp7003 (Hours Per Week Actual), tp7602 (Net Income Last Month)
#' 

data <- data %>% # remember the pipe?
   select(______) # fill in the missing names

 
## Explore the data structure ----

#' For a first overview over our data, we can use the base functions 
#' (e.g. those which are without any package installations part of R) 
#' str(), head(), summary(), table(), quantile(), and View().

str(data)
head(data)
summary(data)
table(data$tp72) # you could use any other variable here.
quantile(data$tp72, na.rm = TRUE)
View(data)

## Basic recoding and mutate() ----

#' The dataset is from 2003. Compute the age accordingly by creating the new variable age
data$age <- 2003 - _____
 
#' There is also a dplyr way to do this: mutate(). 

data <- ______ %>%
   mutate(age = ______)

## Filtering ----

#' Let's say, we only want to keep those observations which are at least 18.
data <- data %>%
   filter(____ >= _____)

## Arrange ----
#' Sometimes, we want to order data. 
#' We do so with arrange(). desc(x) sorts x in descending order. 

data <- _____ %>%
   _____(desc(___))
# 

## All in one ----
#' By the way, we could have done all of this in one batch:

data <- read_csv2("data/soep_europ.csv") # read data again.

data <- data %>%
  select(hhnr, persnr, gebjahr, sex, tp72, tp7001, tp7003 , tp0301, tp0302, tp7602) %>%
  mutate(age = 2003-_____) %>%
  filter(______) %>% # only those who are at least 18
  _____(____(____)) # Arrange the dataframe so that age is displayed in descending order




# Advanced stuff ----

## Recode and mutltiple operations ----

#' The benefit of mutate() is that you can easily apply more than one 
#' operation (on the same variable) at once in a tidy way. For example, let's say we 
#' * want to recode the binary variable tp72 (Work Overtime) into three levels: 
#'   + 10 Yes
#'   + 20 No
#'   + 0 Everything else
#' * Apply those changes to tp72 by creating the new variable over
#' * Do something similar with tp7602 (recreate as netinc, change specific values to NA)
 

data <- data %>%
  mutate(
    over = recode(
      tp72,
      "3" = 0, #actually means Does not apply: Self-Employment
      "-2" = 0, # actually means does not apply
      "-1" = 0, # actually means no answer
      .default = tp72
    ),
    over = over * 10,
    netinc = ____(
      _____,
      "-3" = NA_real_, # remember the difference between integer and double?
      "-2" = NA_real_,
      "-1" = NA_real_,
      .default = _____
    )
  )


 
#' Let's check whether the data manipulation worked 

table(data$over)

table(data$over,
       useNA = "always") # we add the argument useNA in order to also get the number of NA values.


## Factorize ----

#' As we have practiced before, we can factorize variables. 
#' This is also possible within the dplyr world (mutate() again). Let's do that for sex.

_____ <- _____ _____
  ____(sex = factor(
    sex ,
    levels = c(1, 2),
    labels = c("male", "female")
  ))

## Recode II: if_else ----
 

#' * tp7001(Contracted Working Hours): mutate to column "contract", 
#' change all values below 0 (errors) to NA, and divide the variable by 10
#' * Do the same for tp7003: Actual working hours


data <- data %>%
  mutate(
    contract =
      if_else(condition = tp7001 < 0, 
              true = NA_real_, 
              false = tp7001),
    actual =
      ____(__________________________________),
    contract = contract / 10,
    actual = _______________
  )


## Multiple if_else: case_when ---- 

#' Let's say, we want to build a character variable inc.quant which indicates 
#' the quantile() (see above) of netinc. 
 
data <- data %>%
  mutate(
    inc.quant = case_when(
      netinc < quantile(netinc, na.rm = TRUE)[2] ~ "Q1",

      netinc >= quantile(netinc, na.rm = TRUE)[2] &
        netinc < quantile(netinc, na.rm = TRUE)[3] ~ "Q2",

      netinc >= quantile(netinc, na.rm = TRUE)[__] &
        netinc < _______(netinc, na.rm = TRUE)[__] ~ "Q3",

      netinc ______________________________________
    )
  )

## Check data.frame again ----
head(data)
summary(data)

## All in one II ----

#' Again, we could have done all of this in one batch - but it would have been untidy... 
data <- read_csv2("data/soep_europ.csv")

data <- data %>% # remember the pipe?
  
  select(hhnr, persnr, gebjahr, sex, tp72, tp7001, tp7003 , tp0301, tp0302, tp7602)%>%
  
  mutate(age = 2003-gebjahr) %>%
  
  filter(age>=18) %>%
  
  arrange(desc(age))%>%
  mutate(
         over = recode(
           tp72,
           "3" = 0, #actually means Does not apply: Self-Employment
           "-2" = 0, # actually means does not apply
           "-1" = 0, # actually means no answer
           .default = tp72
          ),
         over = over * 10,
         netinc = recode(tp7602,
                         "-3" = NA_real_,
                         "-2" = NA_real_,
                         "-1" = NA_real_
          ),
         
         sex = factor(
           sex ,
           levels = c(1, 2),
           labels = c("male", "female")
         ),    
         
         contract =
           if_else(condition = tp7001 < 0, 
                   true = NA_real_, 
                   false = tp7001),
         actual =
           if_else(condition = tp7003 < 0, 
                   true = NA_real_, 
                   false = tp7003),
         contract = contract / 10,
         actual = actual / 10,

         inc.quant = case_when(
           netinc < quantile(netinc, na.rm = TRUE)[2] ~ "Q1",
           netinc >= quantile(netinc, na.rm = TRUE)[2] &
             netinc < quantile(netinc, na.rm = TRUE)[3] ~ "Q2",
           netinc >= quantile(netinc, na.rm = TRUE)[3] &
             netinc < quantile(netinc, na.rm = TRUE)[4] ~ "Q3",
           netinc >= quantile(netinc, na.rm = TRUE)[4] ~ "Q4"
         )
  )


# (Easy) additional stuff ----

## Summarize ----

data %>%
  summarize(mean = mean(age, na.rm = TRUE),
            sd = sd(age, na.rm = TRUE))

## Grouping ----

#' The same is also possible for grouped structures. 
#' Say, for example, you would want to calculate separate values for different genders: 

data %>%
  group_by(sex) %>%
  summarise(mean = mean(age, na.rm = TRUE),
            sd = sd(age, na.rm = TRUE))


#' A bit more nuanced, let's say you want to create a variable cohort.deviance 
#' which gives you the deviation in netinc from the mean of all people of the same age


data <- data %>%
  _________ %>%
  mutate(cohort.deviance = netinc - ______(_______, na.rm = TRUE))

## Filtering II ----

#' Let's say, you only want to keep those observations which are older than 18 
#' and who are in the top three income quintiles. We store this in data2


data2 <- data %>%
  filter(age > 18,
         inc.quant %in% c("Q2", "Q3", "Q4"),
         sex == "male")

## Merge ----

#' We reread the SOEP data under a different name, merge the relevant 
#' variables from this data.frame with the one we have been working with all the time and 
#' create a new data.frame (data2). 

data.origin <- read_csv2("data/soep_europ.csv")

data2 <- data %>%
  left_join(data.origin[,c("hhnr", "tp0101")], by=c("hhnr"))



data <- data %>%
  left_join(data.origin[,c("hhnr", "tp0101", ____)], by=c("hhnr", ______))

## Spread and Gather ----

#' At the moment, our data is in wide format (e.g. all the variables are in 
#' separate columns). Suppose, we would want to make the data long:


data.long <- data %>%
  gather(key=key, value = value, -c(hhnr, persnr))


#' Can you make data.long wide again?
data.wide <- data.long %>%
   spread(key=____, value=______)


