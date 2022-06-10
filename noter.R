#Gøres inden vi overhovedet går igang - fortæller senere hvad den gør
#install.packages("tidyverse")

#download.file("https://raw.githubusercontent.com/KUBDatalab/R-intro/main/data/flightdata.xlsx",
#              "flightdata.xlsx", mode = "wb")

#install.packages("readxl")
library(readxl)

read_excel("flightdata.xlsx")
data <- read_excel("data/flightdata.xlsx")
library(tidyverse)

data %>% 
  sample_frac(.005) %>%
  ggplot(mapping = aes(x=dep_delay, y=arr_delay)) +
  geom_point()

# vi tænker lige oer sample_frac. Den kan forvirre.
# Men den tid vi bruger på at vente på plottet...

# Og inden vi taler om ggplot - så må vi heller kigge
# på hvad datastrukturen er.
data
View(data)
# Det er vigtigt at vide hvilke data vi har-
# så ved vi nemlig at planlagt ankomst er gemt som
# antal minutter efter midnat.


# Indholdet af hjælpe filen til denne bør vi nok 
# have med på siden.
# flights


# vi har lige set på hele datasættet. Og henvist til
# hjemmesiden, hvor databsekrivelserne er - og 
# nævnt at når man laver sit eget, er det godt at have
# et (maskinlæsbart) dokument med de beskrivelser også.

# STort og forvirrnede. Er der flyselskaber er er 
# bedre til at flyge til tiden= Eller ankomme rettigidt

data %>% select(carrier, dep_delay, arr_delay)

data %>% 
  select(carrier, dep_delay, arr_delay) %>% 
  arrange(carrier) 

data %>% 
  select(carrier, dep_delay, arr_delay) %>% 
  arrange(carrier) %>% 
  view()

data %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay))

# Hov NA!!
# Og vi så faktisk (sandsynligvis) det same under
# plottet
# hjælp! F1 på mean og ?mean- hvilke argumenter kan den tage
data %>% select(dep_delay) %>% 
  summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE))

# denne har for travlt og skal enten helt væk, eller langt senere
# data %>% 
#   summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
#             manglende_værdier = sum(is.na(dep_delay)),
#             antal_værdier = n(),
#             andel_manglende = manglende_værdier/antal_værdier*100)
# vi behøver ikke at selecte! måske først senere.

#kan summarise over flere kolonner
data %>% 
  summarise(gennemsnit_forsinket_afgang = mean(dep_delay, na.rm =T),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T))


#måske kunne det være se hvordan det variere fra flyselskab til flyselskab
data %>%
  group_by(carrier) %>% 
  summarise(gennemsnit_forsinket_afgang = mean(dep_delay, na.rm =T),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T)) 

# Nu er det tid at introducere variable, assignment og "objekter"
# "genbruge" resultatet af ting vi har gjort
forsinkelser <- data %>%
  group_by(carrier) %>% 
  summarise(gennemsnit_forsinket_afgang = mean(dep_delay, na.rm =T),
            gennemsnit_forsinket_ankomst = mean(arr_delay, na.rm=T)) 
forsinkelser

#kan de to gennemsnit visualiseres 
forsinkelser %>% 
  ggplot(mapping = aes(x = gennemsnit_forsinket_afgang, 
                       y = gennemsnit_forsinket_ankomst, color = carrier)) +
      geom_point() +
#  geom_hline(aes(yintercept = 0)) +
  NULL
  
# Hvordan finder vi ud af hvor mange NA værdier der er ?
# data %>% select(dep_delay) %>% 
#   summarise(gennemsnitlig_forsinket_afgang = mean(dep_delay, na.rm = TRUE),
#             manglende_værdier = sum(is.na(dep_delay)),
#             antal_værdier = n(),
#             andel_manglende = manglende_værdier/antal_værdier*100)

# herunder er der fine ting. Men vi skal i retning af kode ----
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

# her begynder der er være ting vi skal have med nu. ------------
forsinkelser %>% 
  filter(gennemsnit_forsinket_afgang > 15)





#Øvelse til dem - Find ud af hvem der har den laveste gennemsnitlige forsinkede
#ankomst - brug plottet til at sjusse sig til værdi - hvem er det?
forsinkelser %>% 
  filter(gennemsnit_forsinket_ankomst < -9)

# default AND, men pædagosik &
forsinkelser %>% 
  filter(gennemsnit_forsinket_afgang >10 & gennemsnit_forsinket_afgang < 15)

#Man kan også udvælge to carrier hvis der er noget man vil nær studere
# or operatoren. forskellige anførselstegn?
forsinkelser %>% 
  filter(carrier == 'F9' | carrier == "AS") 

# bøvlet hvis det var fem carriers vi ville kigge på!
# B6, MQ, AS, F9, OO
# her %in% og vektor!
flyselskaber <- c("B6", "MQ", "AS", "F9", "OO" )

forsinkelser %>% 
  filter(carrier %in% flyselskaber)

# a <- 2.5
# b <- a*2.47
# hvad sker der så når vi ændrer på a?'
# Ændring af variable - og følgevirkninger
# Tilføje til flyselskaber
# Se på et andet interval af forsinkelser ovenfor?
# forsinkelse på afgang = 5 minutter
# forsinkelse af ankomst= forsinkelse på afgang + 5 minutter
# Nu ændrer vi dep_delay - ændres arr_delay?
# Nu skal vi tale lidt om vektorer. Herunder at dataframes består af 
# vektorer. 
# datatyper
# Nok begge!
# class()
# gem data - nok omkring hvor vi gemmer i en variabel!
# Split-apply-combine som begreb i forbindelse med group_by?
# gem data!
# dimensioner af datafrmaes, dim, ncol, nrow, head, tail, names
# summary
# subsetting af vektorer (og dataframes) - overveje om 
# vi skal fortælle at vi gør det gennem dplyr, men at der er andre
# måder. Eller om vi skal vise de andre måder?
#   Vektorer - kun en datatype!
#   numeric, character, (logical)
#   Illustration fra carpentries - 
#     
#     dataframes - en samling af vektorer - der skal være lige lange
#   Det betyder også! at der kun kna være en datatype i en kolonne 
#   i dataframen!

# spørgsmål: skal vi også tale om coercion her?

sum(is.na(data))

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







