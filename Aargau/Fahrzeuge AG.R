library(readr)
library(tidyverse)
Privatverkehr_Fahrzeuge_Fahrzeugbestand_Bestand_nach_Fahrzeugarten <- read_delim("Downloads/Privatverkehr, Fahrzeuge, Fahrzeugbestand, Bestand nach Fahrzeugarten.csv", 
                                                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Privatverkehr_Fahrzeuge_Fahrzeugbestand_Bestand_nach_Fahrzeugarten)

df_all <- Privatverkehr_Fahrzeuge_Fahrzeugbestand_Bestand_nach_Fahrzeugarten

# if ... then make an na
df[df == "..."] <- NA

df<- df_all %>% filter(locationName == "Kanton Aargau")


# sum for every year Personenwagen
PW_AG <- df %>% 
  group_by(year) %>% 
  summarise(sum = sum(PERS_WAGEN))

# sum for every year Nutzfahrzeuge
NUTZFZ_AG <- df %>% 
  group_by(year) %>% 
  summarise(sum = sum(NUTZFZ))

# sum for every year Motorräder

MOTORR_AG <- df %>% 
  group_by(year) %>% 
  summarise(sum = sum(MOTORR))

# sum for every year Anhänger

ANH_AG <- df %>% 
  group_by(year) %>% 
  summarise(sum = sum(as.numeric(ANHAENGER), na.rm = TRUE))

# sum for every year Landwirtschaftliche Fahrzeuge

LANDW_AG <- df %>% 
  group_by(year) %>% 
  summarise(sum = sum(as.numeric(LANDW_MOTORFZ), na.rm = TRUE))

# sum for every year Geselschaftswagen

GESWAG_AG <- df %>% 
  group_by(year) %>% 
  summarise(sum = sum(as.numeric(GSLWAGEN_KLBUSSE), na.rm = TRUE))

# sum for every year motorisierte Zweiräder

MF_AG <- df %>% 
  group_by(year) %>% 
  summarise(sum = sum(as.numeric(MOTORFAHRR), na.rm = TRUE))




