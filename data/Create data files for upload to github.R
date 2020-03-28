
# SOEP data ----
#Daten als Objekt importieren
econ <- readstata13::read.dta13(file="data/offline/econometrics.dta" ,
                   convert.factors=F,
                   nonint.factors = F)

# Subsample with 5000 Observations
econ <- econ [sample(c(1:nrow(econ)),size = 5000,replace = FALSE),]

# Randomly add NA values
econ2 <- data.frame(lapply(econ, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.75, 0.25), size = length(cc), replace = TRUE) ]))

# Clean Gebjahr (easier for the logic of the Tut) AND keep only distinct hhnr and persnr rows (for merge)
econ2 <- econ2 %>%
  mutate(gebjahr = recode(gebjahr, "-1"=NA_integer_)) %>%
  distinct(hhnr, persnr, .keep_all = TRUE)

write_csv(econ2, "data/soep_us.csv")

write_csv2(econ2, "data/soep_europ.csv")

haven::write_dta(econ2, "data/soep.dta")

haven::write_sav(econ2, "data/soep.sav")

openxlsx::write.xlsx(econ2, "data/soep.xlsx")


# Eurostat Data----

eurost <- read_csv2(file = "data/offline/data_uptodate.csv")

eurost <- eurost %>%
  mutate(emigration_t = emmigration_t) %>%
  filter(!geo_code %in% c("BE1", "BE2", "BE3", "EA19", "EE", "EU15", "EU28", "EA12", "EU27_2019", "DE_TOT", "EEA30", "EEA31", "EEA28",
                          "EA18", "EU",   "EU27", "EA17", "RS", "MK", "TR", "ME", "AZ", "BY", "EA", "EFTA", "FX", "AL", "BA", "XK", 
                          "LI", "US", "AZ", "GE", "UA", "MD", "RU", "AD", "AM", "MC", "SM")) %>%
  select(geo_code, time, unemp_youth_t, gdp_gr, inv_per_empl, population_total_t, unemp_workagepop_t,
         emigration_t, immigration_t)


# channge emmigration to emigration


a <- data.frame(geo_code = c("AT", "BE", "BG", "CH","CY","CZ","DE","DK","EL","ES","FI",
                             "FR","HR","HU","IE","IS","IT","LT","LU","LV","MT","NL",
                             "NO","PL","PT","RO","SE","SI","SK","UK"),
                location = c("Central", "West", "East", "Central", "South", "Central", "Central", "North", "South", "South", "North",
                             "West", "East", "Central", "North", "North", "South", "North", "North", "North", "South", "West",
                             "North", "Central", "South", "East", "North", "Central", "Central", "North"),
                geo_name = c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic", "Germany", "Denmark", "Greece","Spain", "Finland", 
                             "France", "Croatia", "Hungary", "Ireland", "Iceland", "Italy", "Lithuania","Luxembourg", "Latvia", "Malta", "Netherlands",
                             "Norway", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "UK"))



 

eurost <- left_join(x=eurost, y=a, by="geo_code")

write_csv2(eurost, "data/eurostat_data.csv")
