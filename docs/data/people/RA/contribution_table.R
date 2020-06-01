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

library(gsheet)
form<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1stwbU5h-gAxkKYcNkMK8kcG3w8wz1VnyBewvKmJl6Q4/edit?usp=sharing")
form$Name<- paste(form$`First Name`,form$`Last Name`, sep = " ")
form<-form[,c(11,5)]
form3<- form %>%
  rename("Name" = 1,
         "Vita" =2)

form<-bind_rows(form1,form2,form3)


ra_allocation<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1qxkKu7gOdt2I0JjgJmviD6EpKdJoP9gU1p5cjOqgONk/edit?usp=sharing")
ra_allocation<- ra_allocation[,c(2,3,4,7,9,10,11,12)]
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

c<-c[!duplicated(c),]
 
data<-c %>%
  group_by(Name) %>% 
  mutate(country = paste0(Country, collapse = ", "))
 
 data <- data[,-1]

 data<- setcolorder(data, c(1,6,8,7,2,3,4,5))
 
 testing<- read_excel("testing.xlsx")
 data<- bind_rows(data,testing)
 
 qualtrics <- read_csv("~/Documents/github/corona_tscs/data/CoronaNet/RA/ra_data_pull_1.csv")
qualtrics<-qualtrics[,c(1,18)]

qualtrics <- qualtrics%>% 
  mutate(date_start=as.Date(StartDate, '%Y-%m-%d'),
         date_end=as.Date(StartDate, '%Y-%m-%d'))%>%
  select(-1,)

qualtrics_date<-qualtrics%>%
  group_by(ra_name) %>%
  # create ranking sequence
  mutate(
    start = min(date_start),
    end = max(date_start)
  )%>%
  select(1,4,5)%>%
  filter(!duplicated(ra_name))%>%
  rename("Name" = 1,
         "Start Date" =2 ,
         "End Date" = 3)


qualtrics<- sort(unique(qualtrics$ra_name))

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
                                "Rahman Demirkol",
                                "Seung-A Paik",
                                "Tara Goodsir"))

 qualtrics
 data<-data[data$Name%in%qualtrics,]
 data<- data[,-5]
 
contribution<-  data[!duplicated(data$Name), ]

contribution[which(contribution$Name=="Adriana Poppe"),"Affiliation"]  = "University of Cologne"
contribution[which(contribution$Name=="Adriana Poppe"),"Vita"]  = "Master Student of Sociology and Social Research at the University of Cologne"
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
contribution[which(contribution$Name=="Ugochukwu Okoye"),"Vita"]  = "An MSc graduate in Africa and International Development from the University of Edinburgh, with research experience in African politics, environmental governance and social anthropology"
contribution[which(contribution$Name=="Janice Klaiber"),"Vita"]  = "Final year US-German double degree undergraduate in International Business / Management"
contribution[which(contribution$Name=="Lea Clara Frömchen-Zwick"),"Vita"]  = "Currently doing an M.A. in Pedagogy."
contribution[which(contribution$Name=="Nicole Oubre"),"Vita"]  = "Master of Public Policy candidate at the Willy Brandt School of Public Policy with a background in criminology."
contribution[which(contribution$Name=="Franziska Nguyen"),"Affiliation"]  = "ESB Business School"
contribution[which(contribution$Name=="Feifei Wang"),"Affiliation"]  = "Eötvös Loránd University"
contribution[which(contribution$Name=="Brian Chesney Quartey"),"Vita"]  = "I am an undergraduate Engineering major at New York University Abu Dhabi, originally from Ghana. "
contribution[which(contribution$Name=="Alette Mengerink"),"Affiliation"]  = "Teacher (German and children's rights) to people with a migration background"
contribution[which(contribution$Name=="Anna Sophia Körner"),"Affiliation"]  = "Sciences Po Paris/FU Berlin"
contribution[which(contribution$Name=="Brahim Ouerghi"),"Vita"]  = "I am a 22 years old student at the technical university of Munich where I study technology and management"
contribution[which(contribution$Name=="Elfriede Derrer-Merk"),"Affiliation"]  = "University of Liverpool"
contribution[which(contribution$Name=="Fabienne Lind"),"Affiliation"]  = "University of Vienna"
contribution[which(contribution$Name=="Frederic Denker"),"Vita"]  = "Undergraduate student interested in innovation and development economics."
contribution[which(contribution$Name=="Luise Modrakowski"),"Vita"]  = "Master student of security risk management at Copenhagen University, originally from Dresden (DE), focusing on risk governance, poltical risk analysis, and sustainability."
contribution[which(contribution$Name=="Nida Hasan"),"Affiliation"]  = "Dual BA Sciences Po Paris/Columbia University"
contribution[which(contribution$Name=="Sana Moghis"),"Vita"]  = "I am a young doctor who has just graduated from Shifa College of Medicine. Passionate about developing a career in Critical Care and exploring methods that revolutionize modern healthcare."
contribution[which(contribution$Name=="Tara Goodsir"),"Affiliation"]  = "University of Amsterdam"
contribution[which(contribution$Name=="Jessica Johansson"),"Affiliation"]  = "University of Hamburg"


contribution = contribution %>% mutate(
  Vita = gsub('righs', 'rights', Vita),
  Vita = gsub('Universtiy|Univesity|Universiity', 'University', Vita),
  Vita = gsub('Scienes', 'Sciences', Vita),
  Vita = gsub('Student an ', 'Student at ', Vita),
  Vita = gsub('Equatorinal', 'Equatorial', Vita),
  Vita = gsub('I am a 22 years old student at the technical university of munich where i study technology and management', 
              'I am a 22 year old student at the Technical University of Munich where I study technology and management', Vita),
  Vita = gsub('(Ph. D)in ', '(Ph.D) in ', Vita),
  Vita = gsub('these exceptional time', 'this exceptional time', Vita),
  Vita = gsub('Palastine', 'Palestine', Vita),
  Vita = gsub('develepment', 'development', Vita),
  Vita = gsub('spanish', 'Spanish', Vita),
  Vita = gsub('poltical', 'political', Vita),
  Vita = gsub('revolutionalize', 'revolutionize', Vita),
  Vita = gsub('Sience', 'Science', Vita))


contribution<- contribution[order(contribution$Name),]

contribution<- left_join(contribution,qualtrics_date,by=c("Name"))

contribution<-contribution %>%
              rename("Cleaning"=7,
                      "Since"=8,
                     "End"=9)
contribution[which(contribution$country=="Testing Data"),"country"]  = "2020-04-01"

certificate<- contribution
certificate$Inactive <- as.character(certificate$Inactive)
certificate$Validating <- as.character(certificate$Validating)
certificate$Cleaning <- as.character(certificate$Cleaning)
certificate$Managing <-as.character(certificate$Regional_Manager)
certificate$Special <-as.character(certificate$Special_Role)


certificate[which(certificate$Validating==1),"Validating"]  = "* Data validation: This role involved re-coding data previously collected by other RAs. The RA reentered the data without seeing prior entries. If the new entries matched the original data, then the data is considered accurate. If not, then the data needs to be rechecked by a third RA for resolution of errors. This task requires high expertise in coding and knwoledge of the codebook"
certificate[which(certificate$`Cleaning`==1),"Cleaning"]  = "* Data cleaning: This role required the RA to use R-Studio in order to clean errors that were detected in the policy entries. The RA then checked the data for errors and fixed it in the Qualtrics survey. This role involved performing repetitive tasks over a long period with high accuracy."

certificate[which(certificate$Validating=="0"),"Validating"]  = ""
certificate[which(certificate$`Cleaning`=="0"),"Cleaning"]  = ""

certificate$Managing[is.na(certificate$Managing)]  = "0"
certificate$Managing[certificate$Managing!="0"]  = "* Regional Manager: This role comes along with the responsibility of leading a group of RAs. It involves the oversight and support of RAs as well as the conduction of regular performance and activity appraisals to guarantee the development of a consistent and up-to-date dataset. The RAs can approach the regional manager for research related questions and organizational issues. The role requires a profound knowledge of the construction of the dataset and strong leadership skills in order to contribute to the development of a certain team spirit among the RAs."
certificate[which(certificate$Managing=="0"),"Managing"]  = ""

table(certificate$Special_Role)
certificate$Special[is.na(certificate$Special)]  = "0"
certificate[which(certificate$Special=="0"),"Special"]  = ""
certificate$Special[certificate$Special=="TUM Course"]  = "* Regional Manager: The RA participated in a course at the Hochschule für Politik at the Technical University of Munich. Taking part in the class ´Analyzing the Coronavirus Pandemic in Real Time: An Introduction to Evidenced-Based Global Public Policy´ allows the students to gain experience in applying quantitative and statistical analyses in order to generate original knowledge that can help us better understand the politics of the pandemic. In this course, the notion of “evidence-based public policy” is introduced and explored on the basis of own work that addresses a variety of questions about the drivers and consequences of the public policies adopted across the globe in response to the pandemic."
certificate$Special[certificate$Special=="TUM Project Study"]  = "* Project Study: The RA was part of the Project Study format. In this, students do receive 18 ECTS for being a full-time member of CoronaNet. During this time, the RA has shown amazing skills in all kinds of work: validation, cleaning, managing of data and leading other RAs"
certificate$Special[certificate$Special=="TUM RA"]  = ""
certificate$Special[certificate$Special=="Project Management"]  = "* Project Manager: The RA was responsible for the selection, training, allocation, and monitoring of RAs."
certificate$Special[certificate$Special=="Reports"]  = "The RA has written a research note on a country or a specific topic of interest. This includes not only self-managed analysis of the data and writing but also the dedication to making this data understandable for policy makers and the public. The article can be found online via www.coronanet-project.org"
certificate$Special[certificate$Special=="Prefect"]  = "* Senior RA Manager: This role comes along with the responsibility of leading a group of RAs. It involves the oversight and support of RAs as well as the conduction of regular performance and activity appraisals to guarantee the development of a consistent and up-to-date dataset. The RAs can approach the managing RA for research related questions and organizational issues. The role requires a profound knowledge of the construction of the dataset and strong leadership skills in order to contribute to the development of a certain team spirit among the RAs."
certificate$Special[certificate$Special=="Data Science Team"]  = "* Data Science Analyst: The work of data scientists at CoronaNet consists of the technical management of the rapidly evolving database and the analysis of the large amount of data the RAs are collecting. The data science team then uses the insights of this analysis to identify opportunities for leveraging the data and driving the optimization and the development of the dataset. Being part of the data science team requires strong problem-solving skills and the ability to work with different data architectures."

names(certificate)

write_csv(certificate,"~/Documents/github/CoronaNet/data/people/RA/certificate.csv")


cont<-contribution$Name
paste(cont, collapse = ", ")

saveRDS(contribution,file="~/Documents/github/CoronaNet/data/people/contribution.rds")

write_csv(contribution,"~/Documents/github/CoronaNet/data/people/contribution.csv")

write_csv(contribution,"~/Documents/github/corona_tscs/data/CoronaNet/People/contribution.csv")

#Country Activity----

qualtrics <- read_csv("~/Documents/github/corona_tscs/data/CoronaNet/RA/ra_data_pull_1.csv")
qualtrics<-qualtrics[,c(1,18,51,160,161)]

qualtrics <- qualtrics%>% 
  mutate(date_start=as.Date(StartDate, '%Y-%m-%d'),
         date_end=as.Date(StartDate, '%Y-%m-%d'))%>%
  select(-1)%>%
  slice(-1:-2)

update_country<-qualtrics%>%
  select(3,5,6)%>%
  group_by(init_country) %>%
  # create ranking sequence
  mutate(
    start = min(date_start),
    end = max(date_start)
  )%>%
  select(-2,-3)%>%
  filter(!duplicated(init_country))

update_ra<-qualtrics%>%
  select(1,5,6)%>%
  group_by(ra_name) %>%
  # create ranking sequence
  mutate(
    start = min(date_start),
    end = max(date_start)
  )%>%
  select(-2,-3)%>%
  filter(!duplicated(ra_name))


ra<-update_ra %>% arrange(end)
ras<-ra[1:151,]
ra<-ras$ra_name
ras<- ra_allocation[ra_allocation$Name %in% ra,]
ras<- ras[ras$Inactive!=1,]
ras<-ras[,3]

qualtrics <- read_csv("~/Documents/github/corona_tscs/data/CoronaNet/RA/ra_data_pull_1.csv")
qualtrics<-qualtrics[,c(1,18,51,160,161)]
country_alloc<-qualtrics%>%
  group_by(init_country) %>%
  summarize(n=n())

##Hogwarts----
house <- read_csv("~/Downloads/CoronaNet House Sign Up 3.csv")

form1 <- house %>%
  select(2,4)%>%
  rename(Name=1,
         House = 2)

form<- read_csv("2.0.csv")
form<-form[,c(2,6)]
form2<- form %>%
  rename(Name = 1,
         House =2)
form<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1stwbU5h-gAxkKYcNkMK8kcG3w8wz1VnyBewvKmJl6Q4/edit?usp=sharing")
form$Name<- paste(form$`First Name`,form$`Last Name`, sep = " ")
form<-form[,c(11,6)]
form3<- form %>%
  rename(Name = 1,
         House =2)

form<-bind_rows(form1,form2,form3)

write_csv(form,"~/Documents/github/CoronaNet/data/people/hogwarts_list.csv")

qualtrics <- readRDS("~/Documents/github/CoronaNet/data/coranaNetData_clean.rds")

qualtrics <- qualtrics %>%
  select(18,12)%>%
  rename (Name = 1,
          Mail = 2) %>%
  distinct()

hogwarts <- left_join(qualtrics,house, by=c("Mail"))

table(hogwarts$House) 
write_csv(hogwarts,"~/Documents/github/CoronaNet/data/people/hogwarts.csv")

saveRDS(hogwarts,file="~/Documents/github/CoronaNet/data/people/hogwarts.rds")
