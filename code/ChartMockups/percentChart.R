library(readr)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)

df <- read_csv("data/Transformed/Deaths_Pop_CI.csv")

sub <- filter(df, drug %in% c( "Exclusive illicit opioids",
                                "Exclusive pharmaceutical opioids",
                                "Heroin/Opium with pharmaceutical opioids",
                                "Unspecified opioids") &
                intent %in% c("All", "Accidental")) %>% 
  select(year, drug, intent, nature, sex, jurisdiction, age_group, n) %>% 
  group_by(year, intent, nature, sex, jurisdiction, age_group) %>% 
  mutate(alldeaths = sum(n),
         percent = round(n/sum(n)*100, 2))

sub

ggplot(filter(sub, (intent=="All" & sex=="Male" & age_group=="Allages")), aes(x=year, y=percent, fill=drug)) + 
  geom_area() + 
  scale_fill_brewer(palette = "PuBu")
ggplot(filter(sub, (intent=="All" & sex=="Female" & age_group=="Allages")), aes(x=year, y=percent, fill=drug)) + geom_area()
ggplot(filter(sub, (intent=="All" & sex=="All" & age_group=="Allages")), aes(x=year, y=percent, fill=drug)) + geom_area()

ggplot(filter(sub, (intent=="Accidental" & sex=="Male" & age_group=="Allages")), aes(x=year, y=percent, fill=drug)) + geom_area()
ggplot(filter(sub, (intent=="Accidental" & sex=="Female" & age_group=="Allages")), aes(x=year, y=percent, fill=drug)) + geom_area()
g <- ggplot(filter(sub, (intent=="Accidental" & sex=="All" & age_group=="Allages")), aes(x=year, y=percent, fill=drug)) + geom_area()
g
ggplotly(g, group=1)