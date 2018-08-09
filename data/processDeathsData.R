# Author: Timothy Dobbins
# Date: 10 July 2018
# Purpose: Import and transform Excel data for deaths, append esimated resident populations and calculate rates and confidence intervals
# Rerun 2018-08-06 to incorporate corrected data from ABS
# Rerun 2018-08-07 to incorporate all intents, underlying for amphetamines
# Rerun 2018-08-09 to incorporate all intents, underlying for cocaine

library(readxl)
library(tidyverse)
library(janitor)
library(summarytools)
library(epitools)


# Import and transform deaths data ----------------------------------------
sheet1 <- read_excel("Data/Transformed/ABS Feed transformed 20180809.xlsx", sheet=1, skip=1) %>% 
  clean_names() %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  fill(drug, intent, nature, cause_of_death, sex, jurisdiction) %>% 
  gather(key=age, value=n, age15_24, age25_34, age35_44, age45_54, age55_64, age65_74, age75_84, age15_54, age15_64, age_all) %>% 
  mutate(age = gsub("_","",age))

sheet2 <- read_excel("Data/Transformed/ABS Feed transformed 20180809.xlsx", sheet=2, skip=1) %>% 
  clean_names() %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  fill(drug, intent, nature, cause_of_death, sex, jurisdiction) %>% 
  gather(key=age, value=n, age15_54, age15_64, age_all) %>% 
  mutate(age = gsub("_","",age))

sheet3 <- read_excel("Data/Transformed/ABS Feed transformed 20180809.xlsx", sheet=3, skip=1) %>% 
  clean_names() %>% 
  filter_all(any_vars(!is.na(.))) %>% # Drop rows with all missing columns
  fill(drug, nature, intent, sex, jurisdiction) %>% 
  gather(key=age, value=n, age15_24, age25_34, age35_44, age45_54, age55_64, age65_74, age75_84, age15_54, age15_64, age_all) %>% 
  mutate(age = gsub("_","",age))

sheet4 <- read_excel("Data/Transformed/ABS Feed transformed 20180809.xlsx", sheet=4, skip=1) %>% 
  clean_names() %>% 
  filter_all(any_vars(!is.na(.))) %>% # Drop rows with all missing columns
  fill(drug, nature, intent, jurisdiction) %>% 
  gather(key=sex_age, value=n, male_age15_64, female_age15_64, all_age15_64, male_age_all, female_age_all, all_age_all) %>% 
  mutate(sex = word(sex_age, 1, sep="\\_"),
         age = paste0(word(sex_age, 2, sep="\\_"),  word(sex_age, 3, sep="\\_"))) %>% 
  select(-sex_age)

sheet5 <- read_excel("Data/Transformed/ABS Feed transformed 20180809.xlsx", sheet=5, skip=1) %>% 
  clean_names() %>% 
  filter_all(any_vars(!is.na(.))) %>% # Drop rows with all missing columns
  fill(drug, intent, nature, sex) %>% 
  gather(key=state_age, value=n, -year, -drug, -intent, -nature, -sex, -jurisdiction) %>% 
  mutate(jurisdiction = word(state_age, 1, sep="\\_"),
         age = paste0(word(state_age, 2, sep="\\_"),  word(state_age, 3, sep="\\_"))) %>% 
  select(-state_age)

sheet6 <- read_excel("Data/Transformed/ABS Feed transformed 20180809.xlsx", sheet=6, skip=1) %>% 
  clean_names() %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  fill(drug, intent) %>% 
  gather(key=age, value=n, age15_24, age25_34, age35_44, age45_54, age55_64, age65_74, age75_84, age15_64, age_all) %>% 
  mutate(age = gsub("_","",age),
         sex="All",
         jurisdiction="AUS",
         nature = "Underlying")

sheet7 <- read_excel("Data/Transformed/ABS Feed transformed 20180809.xlsx", sheet=7, skip=1) %>% 
  clean_names() %>% 
  filter_all(any_vars(!is.na(.))) %>% # Drop rows with all missing columns
  fill(drug, intent) %>% 
  gather(key=sex_age, value=n, male_age15_64, female_age15_64, all_age15_64, male_age_all, female_age_all, all_age_all) %>% 
  mutate(sex = word(sex_age, 1, sep="\\_"),
         age = paste0(word(sex_age, 2, sep="\\_"),  word(sex_age, 3, sep="\\_")),
         jurisdiction = "AUS",
         nature = "Underlying") %>% 
  select(-sex_age)

sheet8 <- read_excel("Data/Transformed/ABS Feed transformed 20180809.xlsx", sheet=8, skip=1) %>% 
  clean_names() %>% 
  filter_all(any_vars(!is.na(.))) %>% # Drop rows with all missing columns
  fill(drug, intent) %>% 
  gather(key=sex_age, value=n, male_age15_64, female_age15_64, all_age15_64, male_age_all, female_age_all, all_age_all) %>% 
  mutate(sex = word(sex_age, 1, sep="\\_"),
         age = paste0(word(sex_age, 2, sep="\\_"),  word(sex_age, 3, sep="\\_")),
         jurisdiction = "AUS",
         nature = "Underlying") %>% 
  select(-sex_age)

# Join all death data frames ----------------------------------------------
deaths_td <- bind_rows(sheet1, sheet2, sheet3, sheet4, sheet5, sheet6, sheet7, sheet8) %>% 
  mutate(jurisdiction = ifelse(jurisdiction %in% c("australia", "Australia"), "AUS", str_to_upper(jurisdiction)),
         sex = case_when( sex %in% c("all", "All", "Persons") ~ "All",
                          sex %in% c("female", "Females") ~ "Female",
                          sex %in% c("male", "Males") ~ "Male"),
         age = str_replace(str_sub(age, 4, -1), "\\d{4}", paste0(str_sub(age, 4, 5), "-", str_sub(age, 6,7))),
         age = ifelse(age=="all", "Allages", age),
         nature = ifelse(nature=="On board", "Any mention", nature),
         intent = case_when (
           intent=="Accidental, Intentional self-poisoning & Undetermined" ~ "All",
           intent=="Accidental, Intentional self-poisoning & Undetermined intent" ~ "All",
           intent=="Intentional self-poisoning" ~ "Intentional",
           intent=="Undetermined intent" ~ "Undetermined",
           TRUE ~ intent
         )) %>%
         rename(age_group=age) %>% 
  select(-cause_of_death)

# Note: n's may differ across tables for small numbers of deaths (e.g. opium) due to confidentialisation
# We will use the first identified count of deaths
deaths_td_distinct <- distinct(deaths_td, year, drug, intent, nature, sex, jurisdiction, age_group, .keep_all=TRUE)

dfSummary(deaths_td_distinct)

# Read in and restructure population data ---------------------------------
pop <- read_csv("Data/Transformed/2018-31-05_ERP_All_1971_2017.csv")

pop10 <- pop %>%
  filter(age >= 15 & age <= 84) %>%
  mutate(age_group = case_when(
    age >= 15 & age < 25 ~ "15-24",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-54",
    age >= 55 & age < 65 ~ "55-64",
    age >= 65 & age < 75 ~ "65-74",
    age >= 75 & age < 85 ~ "75-84"
  )) %>% 
  group_by(year, sex, location, age_group) %>%
  summarise(n = sum(n))

pop1554 <- pop %>%
  filter(age >= 15 & age <= 54) %>%
  group_by(year, sex, location) %>%
  summarise(n = sum(n)) %>%
  mutate(age_group = "15-54")

pop1564 <- pop %>%
  filter(age >= 15 & age <= 64) %>%
  group_by(year, sex, location) %>%
  summarise(n = sum(n)) %>%
  mutate(age_group = "15-64")

popall <- pop %>%
  group_by(year, sex, location) %>%
  summarise(n = sum(n)) %>%
  mutate(age_group = "Allages")

popmerged <- bind_rows(pop10, pop1554, pop1564, popall) %>%
  ungroup() %>% 
  mutate(
    location = str_to_upper(location),
    sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "persons" ~ "All"
    )
  ) %>%
  rename(pop = n, jurisdiction = location)


# Merge deaths and population data ----------------------------------------
dfallpop <- left_join(x = deaths_td_distinct, y = popmerged) %>% 
  mutate(
    rate_ht = n / pop * 100000,
    rate_ht_lcl = pois.exact(n, pop)$lower * 100000,
    rate_ht_ucl = pois.exact(n, pop)$upper * 100000,
    rate_m = rate_ht * 10,
    rate_m_lcl = rate_ht_lcl * 10,
    rate_m_ucl = rate_ht_ucl * 10
  )

dfSummary(dfallpop)
# Export data as csv ------------------------------------------------------
write_csv(dfallpop, "Data/Transformed/Deaths_Pop_CI.csv")
write_csv(dfallpop, "code/ChartAllDeaths/Deaths_Pop_CI.csv")
