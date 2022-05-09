download.file("https://raw.githubusercontent.com/KUBDatalab/R-intro/main/data/flightdata.xlsx",
              "data/flightdata.xlsx", mode = "wb")

install.packages("readxl")
library(readxl)
read_excel("data/flightdata.xlsx")
data <- read_excel("data/flightdata.xlsx")
data
View(data)
# install.packages("tidyverse")
library(tidyverse)

data %>% select(dep_delay) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay))
# Hov NA!!
# hjælp! F1
data %>% select(dep_delay) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE))

# Hvordan finder vi ud af hvor mange NA værdier der er ?
data %>% select(dep_delay) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            manglende_værdier = sum(is.na(dep_delay)),
            antal_værdier = n(),
            andel_manglende = manglende_værdier/antal_værdier*100)

data %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            manglende_værdier = sum(is.na(dep_delay)),
            antal_værdier = n(),
            andel_manglende = manglende_værdier/antal_værdier*100)
# vi behøver ikke at selecte! måske først senere.

data %>% 
  summarise(gennemsnit_forsinket_afgang = mean(dep_delay, na.rm =T),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T))

# vi kan prøve at lave dnene, med antal og andel af manglende værider. så 
# har vi noget at pivotere senere. med mindre vi finder bedret ting at pivotere.

