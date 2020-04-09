#Contributors

rm(list=ls())
library(readr)
library(readxl)
library(tidyverse)
library(data.table)

form<- read_csv("CoronaNet Research Assistant Form  .csv")
form<-form[,c(2,3)]
form<- form %>%
  rename("Name" = 1,
         "Vita" =2)


library(gsheet)
ra_allocation<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1qxkKu7gOdt2I0JjgJmviD6EpKdJoP9gU1p5cjOqgONk/edit?usp=sharing")
ra_allocation<- ra_allocation[,c(2,4,5)]
ra_allocation<- ra_allocation %>%
  rename("Country" = 1,
         "Name" =2,
         "Mail"=3)

afil<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1cJv94NrO9Boahf441LkAqdY-xAZIlMCwnWdsrEQb8Wc/edit?usp=sharing')
afil<-afil[,-c(8,9)]
afil<- afil[,c(1:3)]
afil<- afil %>%
  rename("Name" = 1,
         "Affiliation" =2 ,
         "Mail" = 3)

a<- left_join(ra_allocation,afil,by=c("Mail","Name"))

b<- left_join(a,form,by=c("Name"))

c<-b[!is.na(b$Name),]

 data<-c %>%
  group_by(Name) %>% 
  mutate(country = paste0(Country, collapse = ", ")) 
 
 data <- data[,-1]

 data<- setcolorder(data, c(1,3,5,4))
 qualtrics <- readRDS("~/Documents/github/CoronaNet/data/coranaNetData_clean.rds")
 
 
 qualtrics<- sort(unique(qualtrics$ra_name))
 
 qualtrics
 data<-data[data$Name%in%qualtrics,]
 data<- data[,-5]
 
contribution<-  data[!duplicated(data$Name), ]






saveRDS(contribution,file="~/Documents/github/CoronaNet/data/people/contribution.rds")
write_csv(contribution,"~/Documents/github/CoronaNet/data/people/contribution.csv")

##Hogwarts
house <- read_csv("~/Downloads/CoronaNet House Sign Up 2.csv")

house <- house %>%
  select(3,4)%>%
  rename(Mail=1,
         House = 2)

qualtrics <- readRDS("~/Documents/github/CoronaNet/data/coranaNetData_clean.rds")

qualtrics <- qualtrics %>%
  select(18,12)%>%
  rename (Name = 1,
          Mail = 2) %>%
  distinct()

hogwarts <- left_join(qualtrics,house, by=c("Mail"))

table(hogwarts$House) 

saveRDS(hogwarts,file="~/Documents/github/CoronaNet/data/people/hogwarts.rds")
