# Data Management


rm(list=ls())

## Loading of packages


library(plyr)
library(dplyr)
library(haven)
library(ggplot2)
library(ggiraph)

load("data/immigration.Rdata")

#Description of Datasample

#Age
mean(df_main$age, na.rm=T) #36.33415 years

hist(df_main$age, na.rm=T, breaks=80)#Bessere Grafik

table(df_main$age)

boxplot(df_main$age)
#Sex

summary(df_main$female)
# 0   1 
#225 183 
 183/408

#Edu 

hist(df_main$edu, breaks = 3)

summary(df_main$educ2)

#0    1    2    3 NA's 
#1    7  230  178    9 


# Ost

summary(df_main$ost)
#0    1 NA's 
#352   69    4

69/421 # 16,38 Prozent Ostdeutschland ( so iemlich genau Zensus 2011, egal weil keine Repräsentation): http://www.bpb.de/nachschlagen/zahlen-und-fakten/soziale-situation-in-deutschland/61532/bevoelkerungsentwicklung


#PID

summary(df_main$pid)/425

#         1          2          3          4 
#0.15764706 0.04235294 0.19058824 0.25176471 
#5          6          7          8 
#0.07764706 0.03764706 0.08000000 0.02117647 
#NA's 
#0.24000000 


## Analyse DV´s 

# Acceptance Rate 

df_main$accepted <- df_main$accepted_e

mean(df_main$accepted, na.rm=T)

hist(df_main$accepted, breaks=10)

boxplot(df_main$accepted)

summary(df_main$accepted)


#Entweder in Altersgruppen, oder in einzelne Boxplots

# Altersgruppen: 

plot_data1 <- df_main%>%
  group_by(age)%>%
  summarize(mean = mean(accepted,na.rm = TRUE)) %>% 
  as.data.frame()

ggplot(data=plot_data1, aes(x=age, y=mean))+
  geom_line()

# Man sieht keine patterns des Alters da hinter, nochmal kurz in Gruppen
df_main$agegroup<- df_main$age

df_main$agegroup[df_main$agegroup<0]<- NA
df_main$agegroup[df_main$agegroup>18 & df_main$agegroup<=29]<- 1
df_main$agegroup[df_main$agegroup>29 & df_main$agegroup<=40]<- 2
df_main$agegroup[df_main$agegroup>40 & df_main$agegroup<=50]<- 3
df_main$agegroup[df_main$agegroup>50 & df_main$agegroup<=65]<- 4
df_main$agegroup[df_main$agegroup>65 ]<- 5


plot_data2 <- df_main%>%
  group_by(agegroup)%>%
  summarize(mean = mean(accepted,na.rm = TRUE)) %>% 
  as.data.frame()

a<- ggplot(data=plot_data2, aes(x=agegroup, y=mean))+
  geom_line() + expand_limits(y = c(0,1))

a

mean(df_main$accepted[df_main$agegroup==5 ], na.rm=T)

# Sex

plot_data3 <- df_main%>%
  group_by(female)%>%
  summarize(mean = mean(accepted,na.rm = TRUE)) %>% 
  as.data.frame()

#0         1
#0.6800000 0.6958106

#Education



plot_data4 <- df_main%>%
  group_by(edu)%>%
  summarize(mean = mean(accepted,na.rm = TRUE)) %>% 
  as.data.frame()

c<- ggplot(data=plot_data4, aes(x=edu, y=mean))+
  geom_line() + expand_limits(y = c(0,1))

c
# Ost West 

plot_data5 <- df_main%>%
  group_by(ost)%>%
  summarize(mean = mean(accepted,na.rm = TRUE)) %>% 
  as.data.frame()

plot_data5 <- plot_data5[-c(3), ]

plot(plot_data5)
op <- par(mfrow = c(2, 2))

boxplot(accepted~ female, data= df_main, names= c("Männlich", "Weiblich"), main= "Akzeptanzrate nach Geschlecht") 
boxplot(accepted~ agegroup, data= df_main, names= c("18-29", "30-40", "41-50", "51-65", "65+"), main= "Akzeptanzrate nach Altersgruppe") 
boxplot(accepted~ edu, data= df_main, names= c("Niedrig", "Mittel", "Hoch"), main= "Akzeptanzrate nach Bildungsniveau") 
boxplot(accepted~ ost, data= df_main, names= c("Alte Bundesländer", "Neue Bundesländer"), main= "Akzeptanzrate nach Region") 



###Analyse Ethnocentrism

#Decriptive

op <- par(mfrow = c(1,1))

mean(df_main$ethno, na.rm=T)

hist(df_main$ethno, breaks=1000, main="Häufigkeitsverteilung ethnozentristischer Tendenzen")

boxplot(df_main$ethno)

ggplot(data=df_main, aes(x=age, y=ethno))+
  geom_line()

summary(df_main$ethno)

#Entweder in Altersgruppen, oder in einzelne Boxplots


# Altersgruppen: 

plot_data6 <- df_main%>%
  group_by(age)%>%
  summarize(mean = mean(ethno,na.rm = TRUE)) %>% 
  as.data.frame()

ggplot(data=plot_data6, aes(x=age, y=mean))+
  geom_line()

# Man sieht keine patterns des Alters da hinter, nochmal kurz in Gruppen



plot_data7 <- df_main%>%
  group_by(agegroup)%>%
  summarize(mean = mean(ethno,na.rm = TRUE)) %>% 
  as.data.frame()

d<- ggplot(data=plot_data7, aes(x=agegroup, y=mean))+
  geom_line() + expand_limits(y = c(-1,1))


d
# Sex

plot_data8 <- df_main%>%
  group_by(female)%>%
  summarize(mean = mean(ethno,na.rm = TRUE)) %>% 
  as.data.frame()

#0         1
#0.06918519 0.05573770


#Education

plot_data9 <- df_main%>%
  group_by(edu)%>%
  summarize(mean = mean(ethno,na.rm = TRUE)) %>% 
  as.data.frame()

e<- ggplot(data=plot_data9, aes(x=edu, y=mean))+
  geom_line() + expand_limits(y = c(-1,1))

e
# Ost West 

plot_data10 <- df_main%>%
  group_by(ost)%>%
  summarize(mean = mean(ethno,na.rm = TRUE)) %>% 
  as.data.frame()

plot_data10 <- plot_data10[-c(3), ]

plot(plot_data10)

op <- par(mfrow = c(2, 2))


boxplot(ethno~ female, data= df_main, names= c("Männlich", "Weiblich"), main= "Ethnozentrismus nach Geschlecht") 
boxplot(ethno~ agegroup, data= df_main, names= c("18-29", "30-40", "41-50", "51-65", "65+"), main= "Ethnozentrismus nach Altersgruppe") 
boxplot(ethno~ edu, data= df_main, names= c("Niedrig", "Mittel", "Hoch"), main= "Ethnozentrismus nach Bildungsniveau") 
boxplot(ethno~ ost, data= df_main, names= c("Alte Bundesländer", "Neue Bundesländer"), main= "Ethnozentrismus nach Region") 



#Bivariate

#Teilen des Samples Ethno 

median(df_main$ethno) #Median = 0 

df_main$ethnosample <- df_main$ethno




df_main$ethnosample[df_main$ethno<=0.5]<- 0
df_main$ethnosample[df_main$ethno>0.5]<- 1

df_main$ethnosample<- df_main$ethnosample %>% as.character() %>% as.factor()
table(df_main$ethnosample)


df_subset1<- df_main[df_main$ethnosample==0,]

df_subset2<- df_main[df_main$ethnosample==1,]



cor(df_subset1$ethno, df_subset1$accepted)
cor(df_subset2$ethno, df_subset2$accepted)


a1<- ggplot(data=df_main, aes(x=ethno, y=accepted))+
  geom_point(colour="grey")+
  geom_smooth(position="identity", method=lm, colour="black")+
  ggtitle("Beziehung zwischen Ethnozentrismus und Akzeptanzrate")+
  ylab("Akzeptanzrate")+
  xlab("Ethnozentrismus")+
  theme(legend.position="none")

ggsave(filename="ggplot_Ethno_all.jpeg",
       plot=a1,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)



a2<- ggplot(data=df_main, aes(x=ethno, y=accepted, col=ethnosample))+
  geom_point(colour="grey")+
  geom_smooth(position="identity", method=lm)+
  ggtitle("Beziehung zwischen Ethnozentrismus und Akzeptanzrate", subtitle = "Median Splitted Sample")+
  ylab("Akzeptanzrate")+
  xlab("Ethnozentrismus")+
  theme(legend.position="none")

ggsave(filename="ggplot_Ethno_mediansplit.jpeg",
       plot=a2,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)



#Inference Statistics

#Single Regression 

lm1<- lm(df_main$accepted~ df_main$ethno)
lm2<- lm(accepted~ ethno, data= df_main [df_main$ethnosample== 0,])
lm3<- lm(accepted~ ethno, data= df_main [df_main$ethnosample== 1,])

summary(lm3)

#Multicollinearity? Correlation test (not for control variables & heimatland)

df_main$pid2<- df_main$pid

df_main$pid2 <- df_main$pid2 %>% as.character() %>% as.numeric()



#Ethnocentrism
cor(df_main$ethno, df_main$age, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$religiosity, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$incomepercentil, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$citysize, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$edu, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$terror, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$kons, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$immi_index1, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$immi_index2, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$ethno, df_main$pid2, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )

cor(df_main$ethno, df_main$heimatlandimmi, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )




#Age

cor(df_main$age, df_main$religiosity, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$age, df_main$incomepercentil, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$age, df_main$citysize, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$age, df_main$edu, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$age, df_main$terror, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$age, df_main$pid2, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )

#Religiosity

cor(df_main$religiosity, df_main$incomepercentil, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$religiosity, df_main$citysize, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$religiosity, df_main$edu, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$religiosity, df_main$terror, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$religiosity, df_main$pid2, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )


#Incomepercentil


cor(df_main$incomepercentil, df_main$citysize, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$incomepercentil, df_main$edu, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$incomepercentil, df_main$terror, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$incomepercentil, df_main$pid2, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )


#citysize


cor(df_main$citysize, df_main$edu, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$citysize, df_main$terror, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$citysize, df_main$pid2, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )


#edu


cor(df_main$edu, df_main$terror, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )
cor(df_main$edu, df_main$pid2, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )

#terror

cor(df_main$terror, df_main$pid2, use = "complete.obs",
    method = c("pearson", "kendall", "spearman") )

lm88<- lm(accepted~ ethno, data= df_main)
lm99<- lm(accepted~ ethno , data= df_main [df_main$ethnosample==0,] )
lm100<- lm(accepted~ ethno , data= df_main[df_main$ethnosample==1,] )

1.473-(1.183/2)
#Big Correlation between ethnocentrism & Konservatism & Immigration Index--> Ommit these from regression 

lm4.1<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize+
          terror + edu , data= df_main)

lm5.1<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize + 
          terror + edu , data= df_main [df_main$ethnosample==0,] )

lm6.1<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize + 
          terror + edu , data= df_main[df_main$ethnosample==1,] )

lm4.2<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil +   
          terror + edu , data= df_main)

lm5.2<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil +
          terror + edu , data= df_main [df_main$ethnosample==0,] )

lm6.2<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + 
          terror + edu , data= df_main[df_main$ethnosample==1,] )

#Multiple Regression (with I-Effects)

summary(lm6.3)
lm4.3<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize + heimatlandimmi + heimatlandflucht+
          terror + edu , data= df_main)

lm5.3<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize + heimatlandimmi + heimatlandflucht + 
          terror + edu , data= df_main [df_main$ethnosample==0,] )

lm6.3<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize + heimatlandimmi + heimatlandflucht+
          terror + edu , data= df_main[df_main$ethnosample==1,] )


lm4.4<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize + HeimatlandIrak + HeimatlandPolen + HeimatlandItalien + HeimatlandTürkei + HeimatlandRussland + HeimatlandUSA+
          terror + edu , data= df_main)

lm5.4<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize +  HeimatlandIrak + HeimatlandPolen + HeimatlandItalien + HeimatlandTürkei + HeimatlandRussland + HeimatlandUSA + 
          terror + edu , data= df_main [df_main$ethnosample==0,] )

lm6.4<- lm(accepted~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize +  HeimatlandIrak + HeimatlandPolen + HeimatlandItalien + HeimatlandTürkei + HeimatlandRussland + HeimatlandUSA+
          terror + edu , data= df_main[df_main$ethnosample==1,] )





#Niqab


lm7.1<- lm(acceptedn~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize+  
          terror + edu , data= df_main)

lm8.1<- lm(acceptedn~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize + 
          terror + edu , data= df_main [df_main$ethnosample==0,] )

lm9.1<- lm(acceptedn~ ethno+ age+ female + religiosity + incomepercentil + ost +
           citysize + 
          terror + edu , data= df_main[df_main$ethnosample==1,] )

lm7.2<- lm(acceptedn~ ethno+ age+ female + religiosity + incomepercentil +   
          terror + edu , data= df_main)

lm8.2<- lm(acceptedn~ ethno+ age+ female + religiosity + incomepercentil +
          terror + edu , data= df_main [df_main$ethnosample==0,] )

lm9.2<- lm(acceptedn~ ethno+ age+ female + religiosity + incomepercentil + 
          terror + edu , data= df_main[df_main$ethnosample==1,] )

vif(lm4.1)
vif(lm4.3)
vif(lm5.1)
vif(lm5.3)
vif(lm6.1)
vif(lm6.3)
vif(lm7.1)
vif(lm8.1)
vif(lm9.1)




#Final Regression Table 

stargazer(lm4.1, lm4.3, lm5.3, lm6.3, type="html", 
          order = c(12,1,2,3,4,5,6,7,10, 11, 8,9),
          covariate.labels = c("Konstante", "Ethnozentrismus",
                               "Alter",
                               "Geschlecht",
                               "Religiosität",
                               "Oekonomische Selbsteinschaetzung",
                               "Neue Bundeslaender", 
                               "Stadtgroeße", 
                               "Terror Angst",
                               "Bildung",
                               "Herkunft: Immigration",
                               "Herkunft: Flucht"
                               
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          report = c("vc*s"),
          style = "commadefault", se = NULL ,
          dep.var.caption = "Multiple Regression der Akzeptanzrate",
          dep.var.labels = c("Immigrations-Experiment"), out="econ_reg.html"
)



stargazer(lm4.1, lm4.4, lm5.4, lm6.4, type="html", 
          order = c(16,1,2,3,4,5,6,7,14,15, 8, 9, 10, 11, 12,13),
          covariate.labels = c("Konstante", "Ethnozentrismus",
                               "Alter",
                               "Geschlecht",
                               "Religiosität",
                               "Oekonomische Selbsteinschaetzung",
                               "Neue Bundeslaender", 
                               "Stadtgroeße", 
                               "Terror Angst",
                               "Bildung",
                               "Herkunft: Irak",
                               "Herkunft: Polen",
                               "Herkunft: Italien",
                               "Herkunft: Türkei",
                               "Herkunft: Russland",
                               "Herkunft: USA"
                               
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          report = c("vc*s"),
          style = "commadefault", se = NULL ,
          dep.var.caption = "Multiple Regression der Akzeptanzrate",
          dep.var.labels = c("Immigrations-Experiment"), out="econ_reg2.html"
)


stargazer(lm4.1, lm4.4, lm7.1, type="html", 
          order = c(16,1,2,3,4,5,6,7,14,15, 8, 9, 10, 11, 12,13),
          covariate.labels = c("Konstante", "Ethnozentrismus",
                               "Alter",
                               "Geschlecht",
                               "Religiosität",
                               "Oekonomische Selbsteinschaetzung",
                               "Neue Bundeslaender", 
                               "Stadtgroeße", 
                               "Terror Angst",
                               "Bildung",
                               "Herkunft: Irak",
                               "Herkunft: Polen",
                               "Herkunft: Italien",
                               "Herkunft: Türkei",
                               "Herkunft: Russland",
                               "Herkunft: USA"
                               
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          report = c("vc*s"),
          style = "commadefault", se = NULL ,
          dep.var.caption = "Multiple Regression der Akzeptanzrate",
          dep.var.labels = c("Immigrations-Experiment", "Flucht-Experiment"), out="all_reg2.html"
)

stargazer(lm4.1,lm5.1, lm6.1, lm4.3, lm5.3, lm6.3, lm7.1, lm8.1, lm9.1, type="html", 
          order = c(14,1,2,3,4,5,6,7,10, 11, 8,9,12,13),
          covariate.labels = c("Konstante", "Ethnozentrismus",
                               "Alter",
                               "Geschlecht",
                               "Religiosität",
                               "Oekonomische Selbsteinschaetzung",
                               "Neue Bundeslaender", 
                               "Stadtgroeße", 
                               "Terror Angst",
                               "Bildung",
                               "Herkunft: Immigration",
                               "Herkunft: Flucht",
                             "Interaktion: Ethnozentrismus/ Herkunft Immigration", 
                               "Interaktion: Ethnozentrismus/ Herkunft Flucht"
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          report = c("vc*s"),
          style = "commadefault", se = NULL ,
          dep.var.caption = "Multiple Regression der Akzeptanzrate",
          dep.var.labels = c("Immigrations-Experiment", "Flucht-Experiment"), out="all_reg.html"
)


