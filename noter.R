#Gøres inden vi overhovedet går igang - fortæller senere hvad den gør
#install.packages("tidyverse")

#download.file("https://raw.githubusercontent.com/KUBDatalab/R-intro/main/data/flightdata.xlsx",
#              "flightdata.xlsx", mode = "wb")

#install.packages("readxl")
library(readxl)

read_excel("flightdata.xlsx")
data <- read_excel("flightdata.xlsx")
data
View(data)
library(tidyverse)

data %>% select(carrier, dep_delay, arr_delay)

data %>% 
  select(carrier, dep_delay, arr_delay) %>% 
  arrange(carrier) 


data %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay))
# Hov NA!!
# hjælp! F1 på mean og ?mean- hvilke argumenter kan den tage
data %>% select(dep_delay) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE))


data %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            manglende_værdier = sum(is.na(dep_delay)),
            antal_værdier = n(),
            andel_manglende = manglende_værdier/antal_værdier*100)
# vi behøver ikke at selecte! måske først senere.

data %>% 
  summarise(gennemsnit_forsinket_afgang = mean(dep_delay, na.rm =T),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T))

#kan summarise over flere kolonner
data %>% 
  summarise(gennemsnit_forsinket_afgang = mean(dep_delay, na.rm =T),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T))

#måske kunne det være se hvordan det variere fra flyselskab til flyselskab
data %>%
  group_by(carrier) %>% 
  summarise(gennemsnit_forsinket_afgang = mean(dep_delay, na.rm =T),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T)) 

#kan de to gennemsnit visualiseres 
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T)) %>% 
  ggplot(mapping = aes(x = gennemsnitlig_forsinket_afgang, 
                       y = gennemsnit_forsinket_ankomst, color = carrier)) +
  geom_point() +
  geom_hline(aes(yintercept = 0))

# Hvordan finder vi ud af hvor mange NA værdier der er ?
data %>% select(dep_delay) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            manglende_værdier = sum(is.na(dep_delay)),
            antal_værdier = n(),
            andel_manglende = manglende_værdier/antal_værdier*100)


#Det kunne være interessant at se hvor mange observationer der er for hver
#carrier, samt andel af NA værdier der er i observationerne
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            manglende_departure = sum(is.na(dep_delay)),
            antal_values = n(),
            andel_departure = manglende_departure/antal_values*100)


#vi kan godt udelade nogle kolonner - kun beholde andel_departure og lave en 
#andel_arrival
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100)


#arrange
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  arrange(proportion_departure, proportion_arrival)

data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  filter(gennemsnitlig_forsinket_afgang > 20)

#Øvelse til dem - Find ud af hvem der har den laveste gennemsnitlige forsinkede
#ankomst - brug plottet til at sjusse sig til værdi - hvem er det?
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  filter(gennemsnit_forsinket_ankomst < -9)

#Man kan også udvælge to carrier hvis der er noget man vil nær studere
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>%
  filter(carrier == 'F9' | carrier == "AS")





#øvelse to
#Finde ud af hvad flyselskaberne egentlig hedder
#vise excel dokumentet - der er flere ark
#ark to airlines
#pt kun ark 1 hvordan får vi fat i ark2?
read_excel("flightdata.xlsx", sheet="airlines")

#F1

airlines <- read_excel("flightdata.xlsx", sheet="airlines")

airlines %>%  filter(carrier == "F9" | carrier == "AS")

#kunne godt tænke os at få det på den tidligere dataframe
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  left_join(airlines, by = c("carrier" = "carrier"))

#carrier skal være lig med name og name stå først
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  left_join(airlines, by = c("carrier" = "carrier")) %>% 
  mutate(carrier = name) %>% 
  select(-name)

#lave plottet fra før, men få name ind i plottet
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  left_join(airlines, by = c("carrier" = "carrier")) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  ggplot(mapping = aes(x = gennemsnitlig_forsinket_afgang, 
                       y = gennemsnit_forsinket_ankomst, color = carrier)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_label(aes(label = carrier))

#fjerne legends
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  left_join(airlines, by = c("carrier" = "carrier")) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  ggplot(mapping = aes(x = gennemsnitlig_forsinket_afgang, 
                       y = gennemsnit_forsinket_ankomst, color = carrier)) +
  geom_point() +
  theme(legend.position = "none") +
  geom_hline(aes(yintercept = 0)) +
  geom_label(aes(label = carrier))

#undgå at de overlagger - via hjælpepakke ggrepel
install.packages("ggrepel")
library(ggrepel)

data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  left_join(airlines, by = c("carrier" = "carrier")) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  ggplot(mapping = aes(x = gennemsnitlig_forsinket_afgang, 
                       y = gennemsnit_forsinket_ankomst, color = carrier)) +
  geom_point() +
  theme(legend.position = "none") +
  geom_hline(aes(yintercept = 0)) +
  geom_label_repel(aes(label = carrier)) 

#prikken ved 'alaska airlines inc' vises ikke
#vi prøver at udvide koordinatsystemet.
data %>% 
  group_by(carrier) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T),
            proportion_departure = sum(is.na(dep_delay))/n()*100,
            proportion_arrival = sum(is.na(arr_delay))/n()*100) %>% 
  left_join(airlines, by = c("carrier" = "carrier")) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  ggplot(mapping = aes(x = gennemsnitlig_forsinket_afgang, 
                       y = gennemsnit_forsinket_ankomst, color = carrier)) +
  geom_point() +
  theme(legend.position = "none") +
  geom_hline(aes(yintercept = 0)) +
  geom_label_repel(aes(label = carrier)) +
  ylim(-15, 25)

#øvelse tre
#boxplot - alder af fly som carrier flyver med
planes <- read_excel("flightdata.xlsx", sheet="planes")


#have alder i data og kombinere på 
data %>%
  select(carrier, tailnum) %>% 
  distinct() %>% 
  left_join(planes) %>% 
  select(carrier,tailnum, year) %>% 
  left_join(airlines) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  mutate(age = 2013 - year)

#kan se at der er NA værdier, men kan vi undersøge for om der er NA værdier
#er interesseret i om der er NA i year
data %>%
  select(carrier, tailnum) %>% 
  distinct() %>% 
  left_join(planes) %>% 
  select(carrier,tailnum, year) %>% 
  left_join(airlines) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  mutate(age = 2013 - year) %>% 
  summary()

#boxplot på age som funtion af carrier - uden NA værdier
data %>%
  select(carrier, tailnum) %>% 
  distinct() %>% 
  left_join(planes) %>%
  filter(!is.na(year))
select(carrier,tailnum, year) %>% 
  left_join(airlines) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  mutate(age = 2013 - year) %>% 
  ggplot(aes(x = carrier, y = age)) +
  geom_boxplot()

#Få den læsbar
data %>%
  select(carrier, tailnum) %>% 
  distinct() %>% 
  left_join(planes) %>%
  filter(!is.na(year))%>% 
  select(carrier,tailnum, year) %>% 
  left_join(airlines) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  mutate(age = 2013 - year) %>% 
  ggplot(aes(x = carrier, y = age)) +
  geom_boxplot()+
  coord_flip()

#styre rækkefølgen efter medianen på kategoriske variable 
#ved at arbejde med faktorer
data %>%
  select(carrier, tailnum) %>% 
  distinct() %>% 
  left_join(planes) %>%
  filter(!is.na(year))%>% 
  select(carrier,tailnum, year) %>% 
  left_join(airlines) %>% 
  mutate(carrier = name) %>% 
  select(-name) %>% 
  mutate(age = 2013 - year) %>% 
  mutate(carrier = fct_reorder(carrier, age))%>%
  ggplot(aes(x = carrier, y = age)) +
  geom_boxplot()+
  coord_flip()

#top 3 nederste i box plot sammenlignet med top 3 øverste i boxplot
#i forhold til forsinkelser






