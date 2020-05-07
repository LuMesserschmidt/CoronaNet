#Contributors

rm(list=ls())
library(readr)
library(readxl)
library(tidyverse)
library(data.table)

form<- read_csv("CoronaNet Research Assistant Form.csv")
form<-form[,c(2,3)]
form1<- form %>%
  rename("Name" = 1,
         "Vita" =2)

form<- read_csv("2.0.csv")
form<-form[,c(2,3)]
form2<- form %>%
  rename("Name" = 1,
         "Vita" =2)
form<-bind_rows(form1,form2)

library(gsheet)
ra_allocation<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1qxkKu7gOdt2I0JjgJmviD6EpKdJoP9gU1p5cjOqgONk/edit?usp=sharing")
ra_allocation<- ra_allocation[,c(2,3,4)]
ra_allocation<- ra_allocation %>%
  rename("Country" = 1,
         "Name" =2,
         "Mail"=3)
leave<- read_excel("leave.xlsx")
leave<- leave[,c(2,4,5)]
leave<- leave %>%
  rename("Country" = 1,
         "Name" =2,
         "Mail"=3)
ra_allocation<- bind_rows(ra_allocation,leave)

afil<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1yqz6q1-iVh6_j_a1ub7_krditog-KdweXPFM6r3Sv1Y/edit?usp=sharing')
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
 
 testing<- read_excel("testing.xlsx")
 data<- bind_rows(data,testing)
 
 qualtrics <- read_csv("~/Documents/github/corona_tscs/data/CoronaNet/coronanet_raw_latest.csv")

 qualtrics<- sort(unique(qualtrics$ra_name))

 qualtrics
 qualtrics<- append(qualtrics,c("Beatrice von Braunschweig",
                                "Lily Zandstra",
                                "Luise Modrakowski",
                                "Ricardo Buitrago",
                                "Sana Moghis",
                                "Elliot Weir",
                                "Kojo Vandyck",
                                "Li Cheng",
                                "Ha-Neul Yu",
                                "Josef Montag",
                                "Alette Mengerink",
                                "Bruno Ciccarini",
                                "Borja Arrue-Astrain",
                                "Eduardo Landaeta",
                                "Fernanda Werneck",
                                "Karina Lisboa Båsund",
                                "Rosana Fayazzadh",
                                "Tasia Wagner",
                                "Victoria Atanasov",
                                "Angeline Kanyangi",
                                "Rahman Demirkol"))

 qualtrics
 data<-data[data$Name%in%qualtrics,]
 data<- data[,-5]
 
contribution<-  data[!duplicated(data$Name), ]

contribution[which(contribution$Name=="Adriana Poppe"),"Affiliation"]  = "University of Cologne"
contribution[which(contribution$Name=="Adriana Poppe"),"Vita"]  = "Master Student of Sociology and Social Research at the Universtiy of Cologne"
contribution[which(contribution$Name=="Andreas Duncan"),"Vita"]  = "Andy is an undergraduate student in Sustainable Regional Management."
contribution[which(contribution$Name=="Cara Kim"),"Vita"]  = "Medical student from Germany"
contribution[which(contribution$Name=="Caress Schenk"),"Vita"]  = "Associate Professor of Political Science"
contribution[which(contribution$Name=="Cheng-Hao SHEN"),"Vita"]  = "A political science student interested in comparative government, British politics, and cross-strait relations from the Republic of China"
contribution[which(contribution$Name=="Fabio Kadner"),"Vita"]  = "I'm currently writing my master thesis in the programme 'Society, Globalization, Development' at the university of Bonn, Germany. My main research topics include migration, religion and international relations."
contribution[which(contribution$Name=="Fadhilah Fitri Primandari"),"Vita"]  = "Final year political science student at Universitas Indonesia, with a concentration in comparative politics. Her views on Indonesian politics have previously appeared on several notable platforms, such as East Asia Forum, New Mandala, and The Diplomat."
contribution[which(contribution$Name=="Imogen Rickert"),"Affiliation"]  = "Policy Advisor in non-profit sector"
contribution[which(contribution$Name=="Imogen Rickert"),"Vita"]  = "Social researcher with M.A. in Sociology from Freie Universität Berlin, B.A. from the University of Sydney and experience in providing policy analysis in the non-profit sector."
contribution[which(contribution$Name=="Ines Böhret"),"Affiliation"]  = "University of Manchester, University of Passau"
contribution[which(contribution$Name=="Ines Böhret"),"Vita"]  = "Ines has a B.A. in International Emergency and Disaster Relief and currently writes her theses for a M.Sc. in Global Health and a M.A. in Caritas Science and Value-based Management."
contribution[which(contribution$Name=="Jasmina Sowa"),"Vita"]  = "I am Psychology student from Germany in the fourth year of my bachelors degree."
contribution[which(contribution$Name=="Konstanze Schönfeld"),"Vita"]  = "Global Studies student at Uni Leipzig / Fudan University, focusing on visa policy; BA in Japanese Studies from Uni Heidelberg"
contribution[which(contribution$Name=="Lena Kolb"),"Vita"]  = "I study in 4th Semester of political science at TUM"
contribution[which(contribution$Name=="Maheen Zahra"),"Affiliation"]  = "Lecturer, Social Policy specialist"
contribution[which(contribution$Name=="Maheen Zahra"),"Vita"]  = "Lecturer at the Department of Development Studies, National University of Science and Technology (NUST), Pakistan"
contribution[which(contribution$Name=="Maximilian Dirks"),"Vita"]  = "I am studying Economic Policy Consulting M.Sc. at the University of Bochum."
contribution[which(contribution$Name=="Miranda Tessore Janowski"),"Vita"]  = "I am a graduate of Politics, Psychology, Law and Economics (PPLE) with a specialisation in International Law from the University of Amsterdam, where I graduated with an Upper 2:1. I currently live in London and will start a Master's in International Peace and Security at King's College London in September 2020."
contribution[which(contribution$Name=="Sana Moghis"),"Vita"]  = "I am a young doctor who has just graduated from Shifa College of Medicine. Passionate about developing a career in Critical Care and exploring methods that revolutionalize modern healthcare."
contribution[which(contribution$Name=="Tanja Matheis"),"Vita"]  = "PhD candidate, Friedrich Ebert Foundation Fellow, writer and consultant with a background in economics, passionate about decent work in supply chains."
contribution[which(contribution$Name=="Veronika Bartáková"),"Vita"]  = "I am a student at the London School of Economics and Political Science, pursuing an MSc in Theory & History of International Relations. I am passionate about research, data, public policy and I am very excited to be a part of this project."
contribution[which(contribution$Name=="Victor Abuor"),"Affiliation"]  = "Kenyatta University"
contribution[which(contribution$Name=="Victor Abuor"),"Vita"]  = "A data-driven young professional passionate in research, data analysis and presentation."
contribution[which(contribution$Name=="Chloë Fraser"),"Affiliation"]  = "Dual BA Sciences Po Paris/University of British Columbia"
contribution[which(contribution$Name=="Chloë Fraser"),"Vita"]  = "Having grown up near Montreal and close to Brussels, I am now completing my second year in a Dual BA in social sciences between Sciences Po and UBC, and with an interest in human rights work and sustainable development."
contribution[which(contribution$Name=="Daniel Boey"),"Affiliation"]  = "Hertie School & Columbia University"
contribution[which(contribution$Name=="Raquel Karl"),"Affiliation"]  = "Zeppelin University"
contribution[which(contribution$Name=="Raquel Karl"),"Vita"]  = "Undergraduate student in Sociology, Politics & Economics."
contribution[which(contribution$Name=="Tasia Wagner"),"Affiliation"]  = " Institute for Islamic Strategic Affairs (IISA),  programme advisor & advisor to Executive Director"
contribution[which(contribution$Name=="Alette Mengerink"),"Vita"]  = "Teacher (German and children’s rights)."
contribution[which(contribution$Name=="Sana Moghis"),"Vita"]  = "I am a young doctor who has just graduated from Shifa College of Medicine. Passionate about developing a career in Critical Care and exploring methods that revolutionalize modern healthcare."
contribution[which(contribution$Name=="Sana Moghis"),"country"]  = "Bangladesh, Nepal, Testing Data"
contribution[which(contribution$Name=="Cheng-Hao SHEN"),"Affiliation"]  = "Sciences Po Paris"
contribution[which(contribution$Name=="Kojo Vandyck"),"Affiliation"]  = "NYU Abu Dhabi"
contribution[which(contribution$Name=="Kojo Vandyck"),"Vita"]  = "A Ghanaian STEM enthusiast keen on battling COVID-19!"
contribution[which(contribution$Name=="Ricardo Buitrago"),"Affiliation"]  = "Universidad de La Salle Colombia"
contribution[which(contribution$Name=="Ricardo Buitrago"),"Vita"]  = "Head of the B.A. in International Business & Relations"
contribution[which(contribution$Name=="Tess de Rooij"),"Affiliation"]  = "University of Amsterdam"
contribution[which(contribution$Name=="Tess de Rooij"),"Vita"]  = "I hold a BSc in Politics, Psychology, Law & Economics (politics major, cum laude) from the University of Amsterdam. I've worked as a guest teacher and campaigner, and I'm currently deciding where to pursue my master's next year - next to assisting in the CoronaNet Research Project!"
contribution[which(contribution$Name=="Samantha Reinard"),"Vita"]  = "Undergraduate student of International Relations and Comparative World Literature, soon to study in Taiwan."
contribution[which(contribution$Name=="Alexander Pachanov"),"Vita"]  = "Master's student at Berlin School of Public Health"

contribution<- contribution[order(contribution$Name),]









saveRDS(contribution,file="~/Documents/github/CoronaNet/data/people/contribution.rds")

write_csv(contribution,"~/Documents/github/CoronaNet/data/people/contribution.csv")

write_csv(contribution,"~/Documents/github/corona_tscs/data/CoronaNet/People/contribution.csv")

##Hogwarts
house <- read_csv("~/Downloads/CoronaNet House Sign Up 3.csv")

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
