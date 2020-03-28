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

## Belgium: **lfst_r_lfe2emprtn** Employment rates by sex, age, educational attainment level, citizenship and NUTS 2 regions ----

## Rbind a and b: empl_educ ----




# B. Unemployment rates ----

## Unemployment rates by sex, age and citizenship (%) [lfsa_urgan] ----

## Unemployment rates by sex, age, ((, country of birth)) and NUTS 2 regions (%) [lfst_r_lfu3rt AND lfst_r_lfur2gac] ----

#' There is only one single data management task for you here: 
#' * Somewhat randomly, Unemployment data for Youth and working age population exist 
#' but in different datasets:
#'   + lfst_r_lfur2gac has 15-64
#*   + lfst_r_lfu3rt has 15-24
#' * Accordingly, we load and manipulate both data seperately and merge them


## Rbind a and b: unempl ----





# C. Nominal GDP per capita ----

## Gross domestic product at market prices tec00001 ----

## Gross domestic product (GDP) at current market prices by NUTS 2 regions [nama_10r_2gdp] ----

## Rbind a and b gdp.pc




# D. Investment Rate ----

## Annual enterprise statistics for special aggregates of activities (NACE Rev. 2) [sbs_na_sca_r2] ----
#' * Only Data for country level
#' * getting data for the variables:
#'   + Investment rate (investment/value added at factors cost) - percentage
#'   + Investment per person employed - milliers deuros

## Only data for country level available

## Rbind a and b: Just save data_a as inv ---- 




# Difficult: Employment Growth  ----

## Employment and activity by sex and age  - annual data  [lfsi_emp_a] ----

## Employment by sex, age and NUTS 2 regions (1 000) [lfst_r_lfe2emp] ----

## Rbind a and b:empl_growth


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