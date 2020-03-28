# Setup the script

library(tidyverse)
library(eurostat)


# Employment rate by educational attainment levels ----

## Countries: **lfsa_ergaed** Employment rates by sex, age and educational attainment level ----
data_a <-
  get_eurostat(id = "lfsa_ergaed", # the relevant id to be downloaded
               time_format = "num") # retrieve time column as.numeric right away (bc we only have yearly data)

data_a <- label_eurostat(data_a,
                           code = c("geo",
                                    "isced11",
                                    "sex"), # we want to keep the "geo" plus relevant identifier items to be able to merge data easily
                           lang = "en") # English is the default, line included for transparency

data_a <- data_a %>%
  filter(age == "From 15 to 64 years",
         # filter for appropriate values --> if more variables (for example sex or age) are needed, make changes here!
         isced11_code %in% c("ED0-2", "ED3_4", "ED5-8")) %>%
  mutate(
    variab = "emp", # Add variable identifier
    isced11_code = str_replace(isced11_code, "-", "_"),
    # R doesn't like '-'(minus) in column names, change that to '_'
    key = paste(variab, isced11_code, sex_code, sep = "_")
  ) %>% # Merge sex and isced to get different data vars
  select(-c(unit, sex, isced11, sex_code, isced11_code, age, geo)) %>% # remove unnecessary rows (which would be in the way of spread)
  spread(key = key, value = values) %>% # spread out the column whose individual levels are needed as columns
  rename_all(tolower) # all column names in lowercase



## Belgium: **lfst_r_lfe2emprtn** Employment rates by sex, age, educational attainment level, citizenship and NUTS 2 regions ----
data_b <-
  get_eurostat(id = "lfst_r_lfe2emprtn", # the relevant id to be downloaded
               time_format = "num") # retrieve time column as.numeric right away (bc we only have yearly data)


data_b <- label_eurostat(
  data_b,
  code = c("geo",
           "isced11",
           "sex"),
  # we want to keep the "geo" and "na_item" column to be able to merge data easily
  lang = "en",
  # English is the default, line included for transparency
  fix_duplicated = TRUE
)

data_b <- data_b %>%
  filter(
    geo_code %in% c("BE1", "BE2", "BE3"),
    citizen == "Total", # geo_code and citizen are the only two differences in Belgium compared to Country level for the filter
    age == "From 15 to 64 years",
    # filter for appropriate values --> if more variables (for example sex or age) are needed, make changes here!
    isced11_code %in% c("ED0-2", "ED3_4", "ED5-8")
  ) %>%
  mutate(
    variab = "emp", # Add variable identifier
    isced11_code = str_replace(isced11_code, "-", "_"),
    # R doesn't like '-'(minus) in column names, change that to '_'
    key = paste(variab, isced11_code, sex_code, sep = "_")
  ) %>% # Merge sex and isced to get different data vars
  select(-c(unit, sex, isced11, sex_code, isced11_code, age, geo, citizen)) %>% # remove unnecessary rows (which would be in the way of spread) + Do not forget also to unfilter citizen (extra variable in the Belgium data)
  spread(key = key, value = values) %>% # spread out the column whose individual levels are needed as columns
  rename_all(tolower) # all column names in lowercase

## Rbind a and b ----
empl_educ <- bind_rows(data_a, data_b)


# A. Employment rate by sex and age ----

## Countries: **lfsa_ergaed** Employment rates by sex, age and educational attainment level ----

data_a <-
  get_eurostat(id = "lfsa_ergaed",
               time_format = "num")

data_a <- label_eurostat(data_a,
                         code = c("geo",
                                  "sex",
                                  "age"))

data_a <- data_a %>%
  filter(age %in% c("From 15 to 64 years", "From 15 to 24 years"),
         isced11 == "All ISCED 2011 levels") %>%
  mutate(
    variab = "emp",
    variab2 = ifelse(age_code == "Y15-64", "workagepop", "youth"),
    key = paste(variab, variab2, sex_code, sep = "_")
  ) %>%
  select(geo_code, key, time, values) %>%
  spread(key = key, value = values) %>%
  rename_all(tolower)

## Belgium: **lfst_r_lfe2emprtn** Employment rates by sex, age, educational attainment level, citizenship and NUTS 2 regions----
data_b <-
  get_eurostat(id = "lfst_r_lfe2emprtn",
               time_format = "num")

data_b <- label_eurostat(data_b,
                         code = c("geo",
                                  "sex",
                                  "age"),
                         fix_duplicated = TRUE) 

data_b <- data_b %>%
  filter(
    geo_code %in% c("BE1", "BE2", "BE3"),
    age %in% c("From 15 to 64 years", "From 15 to 24 years"), # only data on working age population available
    citizen == "Total",
    isced11 == "All ISCED 2011 levels"
  ) %>%
  mutate(
    variab = "emp",
    variab2 = ifelse(age_code == "Y15-64", "workagepop", "youth"),
    key = paste(variab, variab2, sex_code, sep = "_")
  ) %>%
  select(geo_code, key, time, values) %>%
  spread(key = key, value = values) %>%
  rename_all(tolower) %>%
  mutate(emp_youth_f = NA,
         emp_youth_m = NA,
         emp_youth_t = NA)


## Rbind a and b: empl_educ ----
empl_educ <- rbind(data_a, data_b)


# B. Unemployment rates ----

## Unemployment rates by sex, age and citizenship (%) [lfsa_urgan] ----
data_a <-
  get_eurostat(id = "lfsa_urgan",
               time_format = "num")

data_a <- label_eurostat(data_a,
                         code = c("geo",
                                  "sex",
                                  "age"))

data_a <- data_a %>%
  filter(age %in% c("From 15 to 64 years", "From 15 to 24 years"),
         citizen == "Total") %>%
  mutate(
    variab = "unemp",
    variab2 = ifelse(age_code == "Y15-64", "workagepop", "youth"),
    key = paste(variab, variab2, sex_code, sep = "_")
  ) %>%
  select(geo_code, key, time, values) %>%
  spread(key = key, value = values) %>%
  rename_all(tolower)


## Unemployment rates by sex, age, ((, country of birth)) and NUTS 2 regions (%) [lfst_r_lfu3rt AND lfst_r_lfur2gac]----

data_b1 <-
  get_eurostat(id = "lfst_r_lfur2gac",
               time_format = "num")

data_b1 <- label_eurostat(data_b1,
                          code = c("geo",
                                   "sex",
                                   "age"),
                          fix_duplicated = TRUE) 

data_b1 <- data_b1 %>%
  filter(
    geo_code %in% c("BE1", "BE2", "BE3"),
    age %in% c("From 15 to 64 years", "From 15 to 24 years"),
    c_birth == "Total"
  ) %>% # lfst_r_lfur2gac has 15-64
  mutate(
    variab = "unemp",
    variab2 = ifelse(age_code == "Y15-64", "workagepop", "youth"),
    key = paste(variab, variab2, sex_code, sep = "_")
  ) %>%
  select(geo_code, key, time, values) %>%
  spread(key = key, value = values) %>%
  rename_all(tolower)


# lfst_r_lfu3rt

data_b2 <-
  get_eurostat(id = "lfst_r_lfu3rt",
               time_format = "num")

data_b2 <- label_eurostat(data_b2,
                          code = c("geo",
                                   "sex",
                                   "age"),
                          fix_duplicated = TRUE) 

data_b2 <- data_b2 %>%
  filter(
    geo_code %in% c("BE1", "BE2", "BE3"),
    age %in% c("From 15 to 64 years", "From 15 to 24 years")) %>% # lfst_r_lfur2gac has 15-64
  mutate(
    variab = "unemp",
    variab2 = ifelse(age_code == "Y15-64", "workagepop", "youth"),
    key = paste(variab, variab2, sex_code, sep = "_")
  ) %>% 
  select(geo_code, key, time, values) %>% 
  spread(key = key, value = values) %>% 
  rename_all(tolower)

# Merge youth and working age pop
data_b <- merge(data_b1, data_b2, by=c("geo_code", "time"))

## Rbind a and b----
unempl <- rbind(data_a, data_b)

# C. Nominal GDP per capita ----

## Gross domestic product at market prices tec00001----
data_a <-
  get_eurostat(id = "tec00001",
               time_format = "num")

data_a <- label_eurostat(data_a,
                         code = c("geo"))

data_a <- data_a %>%
  filter(unit=="Current prices, euro per capita") %>%
  mutate(gdp_pc = values) %>%
  select(geo_code, time, gdp_pc) 


## Gross domestic product (GDP) at current market prices by NUTS 2 regions [nama_10r_2gdp]----
data_b <-
  get_eurostat(id = "nama_10r_2gdp",
               time_format = "num")

data_b <- label_eurostat(data_b,
                         code = c("geo"), 
                         fix_duplicated = TRUE)

data_b <- data_b %>%
  filter(unit == "Euro per inhabitant",
         geo_code %in% c("BE1", "BE2", "BE3")) %>%
  mutate(gdp_pc = values) %>%
  select(geo_code, time, gdp_pc)

## Rbind a and b----
gdp_pc <- rbind(data_a, data_b)

# D. Investment Rate ----

## Annual enterprise statistics for special aggregates of activities (NACE Rev. 2) [sbs_na_sca_r2] ----
data_a <-
  get_eurostat(id = "sbs_na_sca_r2",
               time_format = "num")

data_a <- label_eurostat(data_a,
                         code = c("geo"),
                         fix_duplicated = TRUE)

data_a <- data_a %>%
  filter(
    nace_r2 ==
      "Total business economy; repair of computers, personal and household goods; except financial and insurance activities",
    indic_sb %in% c(
      "Investment rate (investment/value added at factors cost) - percentage",
      "Investment per person employed - milliers d'euros"
    )
  ) %>%
  mutate(
    key = ifelse(
      indic_sb == "Investment per person employed - milliers d'euros",
      "inv_per_empl",
      "inv_total"
    )
  ) %>%
  select(-c(nace_r2, indic_sb, geo)) %>%
  spread(key = key, value = values) %>%
  select(geo_code, time, inv_per_empl, inv_total) 

## Rbind a and b----
inv <- data_a


# E. Difficult: Employment Growth ----

data_a <-
  get_eurostat(id = "lfsi_emp_a",
               time_format = "num")

data_a <- label_eurostat(data_a,
                         code = c("geo",
                                  "sex",
                                  "age"))

data_a <- data_a %>%
  filter(
    age %in% c("From 15 to 64 years", "From 15 to 24 years"),
    indic_em == "Total employment (resident population concept - LFS)",
    unit == "Thousand persons"
  ) %>%
  mutate(
    values = values * 1000,
    # Working Population values are in 1000
    variab = "workingpop",
    variab2 = ifelse(age_code == "Y15-64", "workagepop", "youth"),
    key = paste(variab, variab2, sex_code, sep = "_")
  ) %>%
  select(geo_code, key, time, values) %>%
  group_by(geo_code, key) %>%
  mutate(lag_value = lag(values),
         pct_change = (values-lag_value)/lag_value) %>%
  select(geo_code, time, key, pct_change) %>%
  spread(key=key, value=pct_change) %>%
  rename_all(tolower)



## Employment by sex, age and NUTS 2 regions (1 000) [lfst_r_lfe2emp]----

data_b <-
  get_eurostat(id = "lfst_r_lfe2emp",
               time_format = "num")

data_b <- label_eurostat(data_b,
                         code = c("geo",
                                  "sex",
                                  "age"),
                         fix_duplicated = TRUE)

data_b <- data_b %>%
  filter(
    geo_code %in% c("BE1", "BE2", "BE3"),
    age %in% c("From 15 to 64 years", "From 15 to 24 years")
  ) %>%
  mutate(
    values = values * 1000,
    # Working Population values are in 1000
    variab = "workingpop",
    variab2 = ifelse(age_code == "Y15-64", "workagepop", "youth"),
    key = paste(variab, variab2, sex_code, sep = "_")
  ) %>%
  select(geo_code, key, time, values) %>%
  group_by(geo_code, key) %>%
  mutate(lag_value = lag(values),
         pct_change = (values-lag_value)/lag_value) %>%
  select(geo_code, time, key, pct_change) %>%
  spread(key=key, value=pct_change) %>%
  rename_all(tolower)

## Rbind a and b----
empl_growth <- rbind(data_a, data_b)

# F. Merge all df together and save as csv ----
df_final <-
  Reduce(
    function(x, y)
      full_join(x, y, by = c("geo_code", "time")),
    list(empl_educ, unempl, gdp_gr, empl_growth)
  )

df_final <- df_final %>%
  group_by(geo_code, time) %>%
  rename_all(tolower)

write_excel_csv2(df_final, "data_uptodate.csv")