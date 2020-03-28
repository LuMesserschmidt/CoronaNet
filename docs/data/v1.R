# Data Management


rm(list=ls())

## Loading of packages


'library(plyr)
library(dplyr)
library(haven)
library(ggplot2)
library(ggiraph)
'
'install.packages("plyr")  
install.packages("dplyr") 
install.packages("ggplot2") 
install.packages("ggiraph")
install.packages("stargazer") 
install.packages("foreign")
install.packages("stringr")
install.packages("ggthemes")
install.packages("car")
install.packages("ggExtra")
install.packages("dynlm")
install.packages("hrbrthemes")
install.packages("calibrate")
install.packages("ggrepel")
install.packages("extrafont")
loadfonts(device="pdf")
install.packages("lubridate")
install.packages("ggalt")
install.packages("reshape2")
install.packages("mvinfluence")
install.packages("plotly")'

library(plyr)  
library(dplyr) 
library(ggplot2) 
library(ggiraph)
library(stargazer) 
library(foreign)
library(stringr)
library(ggthemes)
library(car)
library(ggExtra)
library(dynlm)
library(hrbrthemes)
library(calibrate)
library(ggrepel)
library(extrafont)
loadfonts(device="pdf")
library(lubridate)
library(ggalt)
library(reshape2)
library(mvinfluence)
library(plotly)

### Directories
setwd("/Users/jasminmesserschmidt/Desktop/1_Ethnozentrismus/data") 
#setwd("/Users/jasminmesserschmidt/Downloads")

load("conjoint_count_econ.Rdata")

count_econ<-conc_e
as.data.frame(count_econ)
rm(conc_e)

load("conjoint_count_norms.Rdata")
count_niqab<-conc_n
as.data.frame(count_niqab)
rm(conc_n)

#Isolate Dataset count_niqab

count_niqab <- count_niqab[ -c(3:342) ]
count_niqab$acceptedn <- count_niqab$accepted 

count_niqab <- count_niqab[ -c(2) ]

#Clean Dataset count_econ

count_econ <- count_econ[ -c(5:20,24,29:32,35:42,43,45,63:70,71:84, 91:100) ]
count_econ <- count_econ[ -c(38:40,43,47) ]
count_econ <- count_econ[ -c(43:274) ]

names(count_econ)[names(count_econ) == 'v_382'] <- 'sex'
names(count_econ)[names(count_econ) == 'v_1165'] <- 'age'
names(count_econ)[names(count_econ) == 'v_1905'] <- 'nationalitydeu'
names(count_econ)[names(count_econ) == 'v_322'] <- 'bornself'
names(count_econ)[names(count_econ) == 'v_323'] <- 'borndad'
names(count_econ)[names(count_econ) == 'v_324'] <- 'bornmom'
names(count_econ)[names(count_econ) == 'v_978'] <- 'citysize'
names(count_econ)[names(count_econ) == 'v_968'] <- 'educ'
names(count_econ)[names(count_econ) == 'v_461'] <- 'pid'
names(count_econ)[names(count_econ) == 'v_1113'] <- 'terrorall'
names(count_econ)[names(count_econ) == 'v_1115'] <- 'terrorislam'
names(count_econ)[names(count_econ) == 'v_389'] <- 'immi_policy'
names(count_econ)[names(count_econ) == 'v_731'] <- 'immi1'
names(count_econ)[names(count_econ) == 'v_732'] <- 'immi2'
names(count_econ)[names(count_econ) == 'v_733'] <- 'immi3'
names(count_econ)[names(count_econ) == 'v_873'] <- 'immi4'
names(count_econ)[names(count_econ) == 'v_1207'] <- 'immi5' #Immigration Thermometer Batterie: 1 Angst/keine Flüchtlinge 6 Keine Angst, Pro
names(count_econ)[names(count_econ) == 'v_901'] <- 'immi_emotionpos'
names(count_econ)[names(count_econ) == 'v_1164'] <- 'immi_emotionneg'


#Ethnozentrismus
names(count_econ)[names(count_econ) == 'v_989'] <- 'moral_d'
names(count_econ)[names(count_econ) == 'v_990'] <- 'moral_t'
names(count_econ)[names(count_econ) == 'v_991'] <- 'moral_i'
names(count_econ)[names(count_econ) == 'v_992'] <- 'moral_p'

names(count_econ)[names(count_econ) == 'v_954'] <- 'vertrauen_d'
names(count_econ)[names(count_econ) == 'v_955'] <- 'vertrauen_t'
names(count_econ)[names(count_econ) == 'v_956'] <- 'vertrauen_i'
names(count_econ)[names(count_econ) == 'v_957'] <- 'vertrauen_p'

#Konservativismus Batterie (Aufpassen: Kons 2, Kons 4 sind positiv formuliert )

names(count_econ)[names(count_econ) == 'v_632'] <- 'kons1'
names(count_econ)[names(count_econ) == 'v_339'] <- 'kons2'
names(count_econ)[names(count_econ) == 'v_726'] <- 'kons3'
names(count_econ)[names(count_econ) == 'v_727'] <- 'kons4'
names(count_econ)[names(count_econ) == 'v_1119'] <- 'kons5'
names(count_econ)[names(count_econ) == 'v_1121'] <- 'kons6'



#Soziodemo
names(count_econ)[names(count_econ) == 'v_700'] <- 'educ2'
names(count_econ)[names(count_econ) == 'v_198'] <- 'incomepercentil'
names(count_econ)[names(count_econ) == 'v_705'] <- 'contactref'
names(count_econ)[names(count_econ) == 'v_706'] <- 'bundesland' #hier nach West und Ost Binär
names(count_econ)[names(count_econ) == 'v_921'] <- 'religiosity'


#Media kommt auch raus


#we need to exlude other nationalities (see v_922) (mostly European neighbours, 4 from Turkey--> Internal Validity for ethnocentrism)
#main problem: We need higher n for turkish and other nationalities for our ethnocentrism

#Erwerbstätigkeit (v_1866) exludiert, weil wenig Varianz und hohe Multicollinearität (income, educ etc.)
#Datensatzbeschriebung: Viele in Ausbildung, verzerrung, externe Validität

#pid wird präferiert. Problem mit vielen NA´s,, aber pid aus Theorie eher genommen, als DIstanzmodelle, die aufgrund
#untersch. Einschätzungen der Sympathien zu einzelnen Distanzen sehr subjektiv sind. Viele AFD Wähler z.b. durch Symapthieskalometer identisch
# Problem bleibt dass Partisanship nicht hilft weil stärkenaspekt der präferenz fehlt 
'Terror zusammenlegen!!!: Wichtige Determinanten: Mindestens 1 Mensch getötet wird (v_1115 & v_1113) (islamistisch erst mal egal und auch kein großer unterschied:
> mean(count_econ$v_1113, na.rm=T)
[1] 3.529703
> mean(count_econ$v_872, na.rm=T)
[1] 2.311881
> mean(count_econ$v_1114, na.rm=T)
[1] 2.410569
> mean(count_econ$v_1115, na.rm=T)
[1] 3.544715)'

#Populism raus, weil kollinearität und unnötig genauso wie medien




#Datamanagement & index-building


#1. Terror Threat (0 : 1)
count_econ$terrorall[is.na(count_econ$terrorall)] <- 0
count_econ$terrorislam[is.na(count_econ$terrorislam)] <- 0

count_econ$terror<- count_econ$terrorall + count_econ$terrorislam
count_econ$terror[count_econ$terror==0]<- NA
count_econ$terror[count_econ$terror==1]<- 0
count_econ$terror[count_econ$terror==2]<- 0.33
count_econ$terror[count_econ$terror==3]<- 0.66
count_econ$terror[count_econ$terror==4]<- 1




#2. Immigration Battery (Ignore immiemotion )
#2 Indizes: Policy & Emotion und getrennt
#Policy Issue: 0-1 Skala (0=Einschränken, 1=Erleichtern )

count_econ$immi_policy[count_econ$immi_policy==0]<- NA
count_econ$immi_policy[count_econ$immi_policy==1]<- 1
count_econ$immi_policy[count_econ$immi_policy==2]<- 0.8334
count_econ$immi_policy[count_econ$immi_policy==3]<- 0.6667
count_econ$immi_policy[count_econ$immi_policy==4]<- 0.5
count_econ$immi_policy[count_econ$immi_policy==5]<- 0.3334
count_econ$immi_policy[count_econ$immi_policy==6]<- 0.1667
count_econ$immi_policy[count_econ$immi_policy==7]<- 0




#Emotion (Gefühlsthermometer: Positive Emotionen minus Negative Emotionen= Gefühlsdifferenz, Standardisieren auf -1:1)

count_econ$immi_emotionpos[is.na(count_econ$immi_emotionpos)] <- 0
count_econ$immi_emotionneg[is.na(count_econ$immi_emotionneg)] <- 0
count_econ$immi_emotion= (count_econ$immi_emotionpos - count_econ$immi_emotionneg)/100


#Existenzielle Ängste & Keine Aufnahme vs. Keine Ängste & Willkommenskultur: alle 3 Faktoren


#Bring Immi´s to -1:1
count_econ$immi1[count_econ$immi1==1]<- -1
count_econ$immi1[count_econ$immi1==2]<- -0.66
count_econ$immi1[count_econ$immi1==3]<- -0.33
count_econ$immi1[count_econ$immi1==4]<- 0.33
count_econ$immi1[count_econ$immi1==5]<- 0.66
count_econ$immi1[count_econ$immi1==6]<- 1
count_econ$immi1[count_econ$immi1==0]<- NA

count_econ$immi2[count_econ$immi2==1]<- -1
count_econ$immi2[count_econ$immi2==2]<- -0.66
count_econ$immi2[count_econ$immi2==3]<- -0.33
count_econ$immi2[count_econ$immi2==4]<- 0.33
count_econ$immi2[count_econ$immi2==5]<- 0.66
count_econ$immi2[count_econ$immi2==6]<- 1
count_econ$immi2[count_econ$immi2==0]<- NA

count_econ$immi3[count_econ$immi3==1]<- -1
count_econ$immi3[count_econ$immi3==2]<- -0.66
count_econ$immi3[count_econ$immi3==3]<- -0.33
count_econ$immi3[count_econ$immi3==4]<- 0.33
count_econ$immi3[count_econ$immi3==5]<- 0.66
count_econ$immi3[count_econ$immi3==6]<- 1
count_econ$immi3[count_econ$immi3==0]<- NA

count_econ$immi4[count_econ$immi4==1]<- -1
count_econ$immi4[count_econ$immi4==2]<- -0.66
count_econ$immi4[count_econ$immi4==3]<- -0.33
count_econ$immi4[count_econ$immi4==4]<- 0.33
count_econ$immi4[count_econ$immi4==5]<- 0.66
count_econ$immi4[count_econ$immi4==6]<- 1
count_econ$immi4[count_econ$immi4==0]<- NA

count_econ$immi5[count_econ$immi5==1]<- -1
count_econ$immi5[count_econ$immi5==2]<- -0.66
count_econ$immi5[count_econ$immi5==3]<- -0.33
count_econ$immi5[count_econ$immi5==4]<- 0.33
count_econ$immi5[count_econ$immi5==5]<- 0.66
count_econ$immi5[count_econ$immi5==6]<- 1
count_econ$immi5[count_econ$immi5==0]<- NA

#Set Policy issue scale to -1:1: New var: Immi_policy_all

count_econ$immi_policy_all<- count_econ$immi_policy

count_econ$immi_policy_all[count_econ$immi_policy_all==1]<- 1
count_econ$immi_policy_all[count_econ$immi_policy_all==0.8334]<- 0.66
count_econ$immi_policy_all[count_econ$immi_policy_all==0.6667]<- 0.33
count_econ$immi_policy_all[count_econ$immi_policy_all==0.5]<- 0
count_econ$immi_policy_all[count_econ$immi_policy_all==0.3334]<- -0.33
count_econ$immi_policy_all[count_econ$immi_policy_all==0.1667]<- -0.66
count_econ$immi_policy_all[count_econ$immi_policy_all==0]<- -1

#Combine Them: 7 Variables: Sum / 7 -> -1:1 Scale OR 5 variables: Iteam battery without emotions & policy


count_econ$immi_index1<- (count_econ$immi1 + count_econ$immi2 + count_econ$immi3 + count_econ$immi4 
                         + count_econ$immi5 + count_econ$immi_policy_all + count_econ$immi_emotion) / 7

count_econ$immi_index2<- (count_econ$immi1 + count_econ$immi2 + count_econ$immi3 + count_econ$immi4 + count_econ$immi5) / 5
                          

#Ethnozentrismus Scale: Differenz of In and Outgroup & auf -1 (outgroup preference) und 1 (ingroup superior )bringen
'TO DO!!!( Annahme alle Deutsch, ergo müssen wir nichtdeutsche rausnehmen)'

#Arbeitsmoral

count_econ$moral_d <- count_econ$moral_d/5
count_econ$moral_t <- count_econ$moral_t/5
count_econ$moral_i <- count_econ$moral_i/5
count_econ$moral_p <- count_econ$moral_p/5

#outgroup building

count_econ$outgroup_moral<- (count_econ$moral_t + count_econ$moral_i + count_econ$moral_p)/3

#difference

count_econ$ethno_moral<- count_econ$moral_d - count_econ$outgroup_moral

#Vertrauen

count_econ$vertrauen_d <- count_econ$vertrauen_d/5
count_econ$vertrauen_t <- count_econ$vertrauen_t/5
count_econ$vertrauen_i <- count_econ$vertrauen_i/5
count_econ$vertrauen_p <- count_econ$vertrauen_p/5

#outgroup building

count_econ$outgroup_vertrauen<- (count_econ$vertrauen_t + count_econ$vertrauen_i + count_econ$vertrauen_p)/3

#difference

count_econ$ethno_vertrauen<- count_econ$vertrauen_d - count_econ$outgroup_vertrauen



#Ethnozentrismus Index

count_econ$ethno <- (count_econ$ethno_vertrauen + count_econ$ethno_moral)/2

count_econ$ethno <- (count_econ$ethno +1) /2
#Konservativimus- Maß / fehlende int. Solidarität (-1: no Kons : 1 kons )

count_econ$kons1[count_econ$kons1==0]<- NA
count_econ$kons1[count_econ$kons1==1]<- -1
count_econ$kons1[count_econ$kons1==2]<- -0.5
count_econ$kons1[count_econ$kons1==3]<- 0
count_econ$kons1[count_econ$kons1==4]<- 0.5
count_econ$kons1[count_econ$kons1==5]<- 1

count_econ$kons2[count_econ$kons2==0]<- NA
count_econ$kons2[count_econ$kons2==5]<- -1
count_econ$kons2[count_econ$kons2==4]<- -0.5
count_econ$kons2[count_econ$kons2==3]<- 0
count_econ$kons2[count_econ$kons2==2]<- 0.5
count_econ$kons2[count_econ$kons2==1]<- 1

count_econ$kons3[count_econ$kons3==0]<- NA
count_econ$kons3[count_econ$kons3==1]<- -1
count_econ$kons3[count_econ$kons3==2]<- -0.5
count_econ$kons3[count_econ$kons3==3]<- 0
count_econ$kons3[count_econ$kons3==4]<- 0.5
count_econ$kons3[count_econ$kons3==5]<- 1

count_econ$kons4[count_econ$kons4==0]<- NA
count_econ$kons4[count_econ$kons4==5]<- -1
count_econ$kons4[count_econ$kons4==4]<- -0.5
count_econ$kons4[count_econ$kons4==3]<- 0
count_econ$kons4[count_econ$kons4==2]<- 0.5
count_econ$kons4[count_econ$kons4==1]<- 1

count_econ$kons5[count_econ$kons5==0]<- NA
count_econ$kons5[count_econ$kons5==1]<- -1
count_econ$kons5[count_econ$kons5==2]<- -0.5
count_econ$kons5[count_econ$kons5==3]<- 0
count_econ$kons5[count_econ$kons5==4]<- 0.5
count_econ$kons5[count_econ$kons5==5]<- 1

count_econ$kons6[count_econ$kons6==0]<- NA
count_econ$kons6[count_econ$kons6==1]<- -1
count_econ$kons6[count_econ$kons6==2]<- -0.5
count_econ$kons6[count_econ$kons6==3]<- 0
count_econ$kons6[count_econ$kons6==4]<- 0.5
count_econ$kons6[count_econ$kons6==5]<- 1

count_econ$kons<- (count_econ$kons1 + count_econ$kons2 + count_econ$kons3 + 
  count_econ$kons4 + count_econ$kons5 + count_econ$kons6) /6


#City size: 0-20k 20k-100.000 >100.000

count_econ$citysize[count_econ$citysize==1| count_econ$citysize==2 | count_econ$citysize==3 | count_econ$citysize==4]<- 1
count_econ$citysize[count_econ$citysize==5| count_econ$citysize==6 ]<- 2
count_econ$citysize[count_econ$citysize==7| count_econ$citysize==8 ]<- 3

#PID wertlos, weil wir wenn partisanship bräuchten und aus der Frage keine Stärker der Neigung ableiten können 8wie bei Kam und Kinder in biparteiensystem mit Abstufungen) 

# Stattdessen: Religiösität

#Income Perzentil

count_econ$incomepercentil<- count_econ$incomepercentil / 10


#Bundesland East (1) West (0)

count_econ$bundesland[count_econ$bundesland==1| count_econ$bundesland==2 | count_econ$bundesland==5 | count_econ$bundesland==6 | count_econ$bundesland==7 | count_econ$bundesland==9 | count_econ$bundesland==10 | count_econ$bundesland==11 | count_econ$bundesland==12 | count_econ$bundesland==15]<- 0
count_econ$bundesland[count_econ$bundesland==3| count_econ$bundesland==4 |  count_econ$bundesland==8 | count_econ$bundesland==13 | count_econ$bundesland==14 | count_econ$bundesland==16 ]<- 1

count_econ$ost <- count_econ$bundesland

count_econ$ost<- count_econ$ost %>% as.character() %>% as.factor()



# Gender 

count_econ$sex[count_econ$sex==1]<- 0
count_econ$sex[count_econ$sex==2]<- 1

count_econ$female <- count_econ$sex

count_econ$female<- count_econ$female %>% as.character() %>% as.factor()

#age

count_econ$age <- ((count_econ$age)-2017)*(-1)
count_econ$age[count_econ$age>=90]<- NA

#Education (1:3)

count_econ$educ2[count_econ$educ2==1 | count_econ$educ2==2 | count_econ$educ2==3]<- 1
count_econ$educ2[count_econ$educ2==4 | count_econ$educ2==5 | count_econ$educ2==6]<- 2
count_econ$educ2[count_econ$educ2==7 ]<- 3
count_econ$educ2[count_econ$educ2==8 | count_econ$educ2==0]<- NA

count_econ$edu<- count_econ$educ2

count_econ$educ2<- count_econ$educ2 %>% as.character() %>% as.factor()


#Contact Refugee (0= Nein, 1= Ja)

count_econ$contactref[count_econ$contactref==1]<- 1
count_econ$contactref[count_econ$contactref==2]<- 0

count_econ$contactref<- count_econ$contactref %>% as.character() %>% as.factor()


#PID

count_econ$pid<- count_econ$pid %>% as.character() %>% as.factor()



#Datacleaning: 

#Delete all Ethno NA´s (proxy: all terror = 0)

count_econ<-count_econ[!(count_econ$terror==0),]

count_econ<-count_econ[!is.na(count_econ$ethno),]

#Delete all other nations

count_econ<-count_econ[!(count_econ$nationalitydeu==2),]


count_econ <- count_econ[ -c(4,5, 7:10, 12, 14, 15, 17:24, 25, 26, 27, 29:37, 42) ]


#Merge!


#names(count_econ)[names(count_econ) == 'accepted'] <- 'accepted_e'
#names(count_niqab)[names(count_niqab) == 'accepted'] <- 'accepted_n'

#df_main <- merge(count_econ,count_niqab,by="lfdn")


load("conjoint_econ.Rdata")
econ<-con_e
as.data.frame(econ)
rm(con_e)



mat1 <- model.matrix( ~ choice  +  Geschlecht + Heimatland + 
                        Deutschkenntnisse +
                       lfdn,data = econ) # Hinter der Tilde alle Variablen, die Du auch aggregieren willst + die ID variable. Bei data den jeweiligen Conjointdatensatz wählen.

agg_mat1 <- aggregate(mat1,list(mat1[,"lfdn"]),FUN = mean) # Diesen Datensatz kannst Du über left_join() oder merge() mit den restlichen Befragtenvariablen zusammenspielen.

df_main <- merge(count_econ,agg_mat1,by="lfdn")


#Datamange df_main

df_main$deutschkenntnisse<- 1 - df_main$Deutschkenntnissekeine

df_main$heimatlandimmi <- (df_main$HeimatlandItalien + df_main$HeimatlandPolen + df_main$HeimatlandTürkei)

df_main$heimatlandflucht <- df_main$HeimatlandIrak

#Nationalität als Interaktionsfaktor: 2 Modelle: A mit eEInteilung, b mit einzelnen Interaktions-Nationalitäten 

df_main <- df_main[ -c(27:29, 37:38) ]

df_main <- merge(df_main,count_niqab,by="lfdn")


rm(agg_mat1)
rm(count_econ)
rm(econ)
rm(mat1)
rm(count_niqab)

'sample teilen (median split): ethnozentrismus und nicht ethnozentrismus 


ethnozentrismus und Nationalität als Interaktionseffekt

warum ethnozentrismus effekt auf flucht?

- kulturelle unterschiede zwischen personen 
- hainmüller#: ethnozentrismus einziger i-effekt 2 sample nach ethno teilen 

#Arbeiten nur mit dem Econ Modell 
I-Effekte hinsichtlich Nationalität 
-Sample Splitting (Median modell) von Ethnozentrismus und Vergleiche der Effekte (Einfach mittels binary variable)'



# H1: Sinkender Ethnozentrismus hat einen Positiven Einfluss auf die Acceptance Rate



# Nur in Diskusionsteil H3: Ethnozentrismus besitzt einen stärkeren Effekt bezüglich der Akkzeptanz von geflüchteten Menschen 



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

mean(df_main$accepted, na.rm=T)

hist(df_main$accepted, breaks=10)

boxplot(df_main$accepted)

ggplot(data=df_main, aes(x=age, y=accepted))+
  geom_line()

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








'To DO: 


Fahrplan:

DONE  Deskriptiv: 

      Datasample: 
      Edu, sex, Age, Historgram, pid


DONE  Analyse DV: 

      1)Acceptance Rate

      mean()
      -Mean
      -Soziodemo (Age, Sex, Edu)

2) 

Analyse Primär IV´s:

DONE  Ethnocentrism 
      -Descriptive: Age, East, Sex 

      -Bivariate Analysis to DV´s (Plot)




Inference Statistics 

DONE - Multivariate Regression (DV: acceptance)
 
  

Test Statistics: 

- Difference between niqab & Econ (Coefficient regresso)'

