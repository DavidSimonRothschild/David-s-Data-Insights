library(dplyr)
library(sf)
library(readr)

# Geodaten einlesen
gemeinden <- st_read("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg", layer = "TLM_HOHEITSGEBIET", quiet = TRUE)

# Abstimmungsdaten einlesen
data <- read_csv("merged_election_ownership.csv", show_col_types = FALSE)

# Nur Kanton Zürich
zh_gemeinden <- gemeinden %>%
  filter(kantonsnummer == 1) %>%
  select(BFS_NR = bfs_nummer, NAME = name) %>%
  st_drop_geometry()

# Mit Abstimmungsdaten verbinden
merged <- zh_gemeinden %>%
  left_join(data, by = "BFS_NR")

# Fehlende Daten finden
missing_data <- merged %>%
  filter(is.na(Ja_Prozent_Eigenmietwert_EM) | is.na(Eigentumsobjekte_Prozent)) %>%
  select(BFS_NR, NAME) %>%
  arrange(NAME)

cat("\n=== FEHLENDE GEMEINDEN ===\n")
print(as.data.frame(missing_data))

# Prüfen ob es Seen sind
seen <- missing_data %>%
  filter(grepl("see", NAME, ignore.case = TRUE))

cat("\nDavon Seen:\n")
print(seen)

# Echte Gemeinden ohne Daten
echte_gemeinden <- missing_data %>%
  filter(!grepl("see", NAME, ignore.case = TRUE))

cat("\nEchte Gemeinden ohne Abstimmungsdaten:\n")
print(echte_gemeinden)
