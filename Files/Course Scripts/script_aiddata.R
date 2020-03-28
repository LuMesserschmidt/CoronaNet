
# Data Management 1----
rm(list=ls())

library(plyr)
library(tidyverse)
library(magrittr)
library(ggiraph)
library(stargazer) 
library(foreign)
library(ggthemes)
library(readxl)
library(bit64)
library(data.table)
library(plm)

load("data/aiddata.Rdata")

countrynames2<- c("Cabo Verde", "Gambia, The", "Ivory Coast","Angola","Benin","Botswana","Burkina Faso","Burundi","Cameroon","Cape Verde","Central African Republic","Chad",
                  "Comoros","Congo, Rep.","Congo, Dem. Rep.","Cote d'Ivoire", "Cote D'Ivoire","Djibouti","Equatorial Guinea","Eritrea","Ethiopia", "Gabon", "Gambia"
                  ,"Ghana", "Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania","Mauritius","Mozambique","Namibia","Niger",
                  "Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia","South Africa","Swaziland","Tanzania","Togo","Uganda","Zambia","Zimbabwe", "Sudan")
countrynames<- c("Angola","Benin","Botswana","Burkina Faso","Burundi","Cameroon","Cape Verde","Central African Republic","Chad",
                 "Comoros","Congo, Rep.","Congo, Dem. Rep.","Ivory Coast","Cote d'Ivoire", "Cote D'Ivoire","Djibouti","Equatorial Guinea","Eritrea","Ethiopia", "Gabon", "Gambia"
                 ,"Ghana", "Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania","Mauritius","Mozambique","Namibia","Niger",
                 "Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia","South Africa","Swaziland","Tanzania","Togo","Uganda","Zambia","Zimbabwe", "Sudan")
countrynames3<- c("Cabo Verde", "Gambia, The", "Ivory Coast","Angola","Benin","Botswana","Burkina Faso","Burundi","Cameroon","Cape Verde","Central African Republic","Chad",
                  "Comoros","Congo, Rep.","Congo, Dem. Rep.","Cote d'Ivoire", "Cote D'Ivoire","Djibouti","Equatorial Guinea","Eritrea","Ethiopia", "Gabon", "Gambia"
                  ,"Ghana", "Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania","Mauritius","Mozambique","Namibia","Niger",
                  "Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia","South Africa","Swaziland","Tanzania","Togo","Uganda","Zambia","Zimbabwe", "Sudan", "Republic of Congo")


countrycodes<- c("ANG","AGO","BEN","BWA","BFA", "BDI","CMR","CPV","CAF","TCD","COM","COD","COG","CIV",
                 "DJI","GNQ","ERI","ETH","GAB","GMB","GHA","GIN","GNB","KEN","LSO","LBR","MDG",
                 "MWI","MLI","MRT","MUS","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE",
                 "SOM","ZAF","SSD","TZA","TGO","UGA","ZMB","ZWE")


aiddata$country<-aiddata$donor
df_USA <- 
  aiddata %>% 
  filter(donor_iso == "US") %>%
  group_by(recipient_iso,year) %>% 
  summarise(sum = sum(commitment_amount_usd_constant,na.rm = TRUE)) %>% 
  as.data.frame()

  # Democracy data ----
df_Dem1<- read_excel("data/p4v2015.xls")

df_Dem <- data.frame(df_Dem1)
df_Dem$country[df_Dem$country== "Congo Kinshasa"] <- "Congo, Dem. Rep."
df_Dem$country[df_Dem$country== "Congo Brazzaville"] <- "Congo, Rep."
df_Dem$country[df_Dem$country== "Ivory Coast"] <- "Ivory Coast"

df_Dem <- df_Dem[df_Dem$year %in% c(2010, 2011, 2012),]
myvars_Dem <- c("scode", "country", "year", "polity2")
df_Dem <- df_Dem[myvars_Dem]


  # Worldbank Data ----
#Dataset:WB Data 

df_worlddata <- fread("data/worldbank.csv",
                      sep=",",
                      nrows = -1,
                      na.strings = c("NA","N/A",""),
                      stringsAsFactors=FALSE,
                      header=TRUE
)

df_worlddata2<- data.frame(df_worlddata)


df_worlddata2<-df_worlddata2[df_worlddata2$Country.Name %in% countrynames2,]
df_worlddata2$Country.Name[df_worlddata2$Country.Name== "Cabo Verde"] <- "Cape Verde"
df_worlddata2$Country.Name[df_worlddata2$Country.Name== "Gambia, The"] <- "Gambia"
df_worlddata2$Country.Name[df_worlddata2$Country.Name== "Cote d'Ivoire"] <- "Ivory Coast"
names(df_worlddata2)[names(df_worlddata2) == "Country.Name"] <- "country"

library(reshape2)
library(stringr)


#Melting and Merging
melted <- 
  melt(data = df_worlddata2,
       id.vars = c("Series.Name",
                   "Series.Code",
                   "country",
                   "Country.Code"),
       variable.name = "year",value.name = "value")

melted$year <- melted$year %>% str_sub(start = 2,end = 5) %>% as.numeric()

melted<-melted[-2]
wide_again <- melted %>% spread(key = Series.Name, #oder .Code, wie du willst
                                                         value = value)
df_world <- wide_again


merged1 <- left_join(df_world,aiddata,
                     by = c("country","year"))

merged2 <- left_join(merged1,df_Dem,
                     by = c("country","year"))

df_main_raw<- merged2


  # Subset and Select needed variables ----
#Transform variables to needed format

df_main<-df_main_raw

df_main$export_cost <- as.character(df_main$"Cost to export (US$ per container)")%>% as.numeric() #USD per Container
df_main$CPIA <- as.character(df_main$"CPIA property rights and rule-based governance rating (1=low to 6=high)")%>% as.numeric() #1=low 6= high
df_main$export <- as.character(df_main$export)%>% as.numeric() #Constant USD 2010 to 2011
df_main$export <- df_main$export * 1.03
df_main$FDI <- as.character(df_main$`Foreign direct investment, net inflows (% of GDP)`)%>% as.numeric() #% of GDP
df_main$GDPpc<-as.character(df_main$`GDP per capita (constant 2010 US$)`)%>% as.numeric() #as constant of 2010--> 2011
df_main$GDPpc<- df_main$GDPpc*1.03
df_main$loggdppc<- log(df_main$GDPpc)
df_main$Military<- as.character(df_main$`Military expenditure (% of GDP)`)%>% as.numeric() #% of GDP
df_main$ODA_total<- as.character(df_main$`Net official aid received (constant 2015 US$)`)%>% as.numeric() # To 2010_: 0.93
df_main$ODA_total <- df_main$ODA_total * 0.93
df_main$PopTotal<- as.character(df_main$`Population, total`)%>% as.numeric()
df_main$logpop<- log(df_main$PopTotal)
df_main$Rail<- as.character(df_main$`Rail lines (total route-km)`)%>% as.numeric()
df_main$Exporttax<- as.character(df_main$`Taxes on exports (% of tax revenue)`)%>% as.numeric()
df_main$Resourcerent_perc<- as.character(df_main$`Total natural resources rents (% of GDP)`)%>% as.numeric()
df_main$gdp_2010 <- as.character(df_main$ "GDP (constant 2010 US$)")%>% as.numeric()
df_main$Resourcerent_tot <- df_main$Resourcerent_perc * df_main$gdp_2010
df_main$log_Resourcerent <- log(df_main$Resourcerent_tot)

df_main$aid<- df_main$commitment_amount_usd_constant
save(df_main,file="df_aiddata.Rdata")


#pd for main
pd_main<-pdata.frame(df_main,index=c("country","year"))

pd_main$lpolity2<- lag(pd_main$polity2, k=1)



# Data Visualization----
'to be continued'

