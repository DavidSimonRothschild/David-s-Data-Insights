library(dplyr)
library(sf)
library(readr)

# Geodaten einlesen
gemeinden <- st_read("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg", layer = "TLM_HOHEITSGEBIET")

# Abstimmungsdaten einlesen
data <- read_csv("merged_election_ownership.csv")

# Nur Kanton Zürich
zh_gemeinden <- gemeinden %>%
  filter(kantonsnummer == 1) %>%
  select(BFS_NR = bfs_nummer, NAME = name)

# Mit Abstimmungsdaten verbinden
map_data <- zh_gemeinden %>%
  left_join(data, by = "BFS_NR")

# Fehlende Daten finden
missing_data <- map_data %>%
  st_drop_geometry() %>%
  filter(is.na(Ja_Prozent_Eigenmietwert_EM) | is.na(Eigentumsobjekte_Prozent))

cat("Gemeinden ohne Abstimmungsdaten (graue Flächen):\n")
print(missing_data %>% select(BFS_NR, NAME))

cat("\nAnzahl Gemeinden im Kanton Zürich (Geodaten):", nrow(zh_gemeinden), "\n")
cat("Anzahl Gemeinden mit Abstimmungsdaten:", nrow(data), "\n")
cat("Anzahl Gemeinden ohne Daten:", nrow(missing_data), "\n")
