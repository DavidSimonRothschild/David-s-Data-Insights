library(ggplot2)
library(dplyr)
library(sf)
library(readxl)
library(viridis)

# Geodaten für Stadt Zürich einlesen
gemeinden <- st_read("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg", layer = "TLM_HOHEITSGEBIET")

# Stadtkreis-Daten einlesen
data_stdzh <- read_excel("data_stdzh.xlsx")

# Nur Stadt Zürich aus Geodaten filtern
zh_stadt <- gemeinden %>%
  filter(bfs_nummer == 261) %>%
  select(geometry = geom)

# Da wir keine separaten Stadtkreis-Grenzen haben, erstellen wir eine vereinfachte Darstellung
# Wir verwenden die gesamte Stadt und zeigen die Daten als Punkte oder Labels

# Erstelle manuelle Positionen für Stadtkreise (ungefähre Koordinaten)
stadtkreis_coords <- data.frame(
  GEBIET_NAME = c("Zürichrich Kreise 1 und 2", "Zürich, Zürich Kreis 3", 
                  "Zürich, Zürich Kreise 4 und 5", "Zürich, Zürich Kreis 6",
                  "Zürich, Zürich Kreise 7 und 8", "Zürich, Zürich Kreis 9",
                  "Zürich, Zürich Kreis 10", "Zürich, Zürich Kreis 11",
                  "Zürich, Zürich Kreis 12"),
  # Ungefähre LV95 Koordinaten für Stadtkreise
  x = c(2683000, 2681500, 2682000, 2684000, 2686000, 2681000, 2684500, 2685500, 2687000),
  y = c(1248000, 1249000, 1247000, 1249500, 1248500, 1247500, 1246500, 1250000, 1251500),
  # Vereinfachte Namen für die Darstellung
  label = c("1&2", "3", "4&5", "6", "7&8", "9", "10", "11", "12")
)

# Mit Daten verbinden
stadtkreis_data <- stadtkreis_coords %>%
  left_join(data_stdzh, by = "GEBIET_NAME")

# Karte 1: Eigenmietwert-Abstimmung
p1 <- ggplot() +
  # Stadt Zürich Umriss
  geom_sf(data = zh_stadt, fill = "grey95", color = "grey50", size = 1) +
  
  # Stadtkreise als Punkte mit Farbe nach Ja-Anteil
  geom_point(data = stadtkreis_data,
             aes(x = x, y = y, color = Ja_Prozent_Eigenmietwert, size = Eigentumsobjekte_Prozent),
             alpha = 0.8) +
  
  # Labels für Stadtkreise
  geom_text(data = stadtkreis_data,
            aes(x = x, y = y, label = label),
            size = 4, fontface = "bold", color = "black") +
  
  # Farbskala für Ja-Anteil
  scale_color_gradient2(
    name = "Ja-Anteil (%)",
    low = "#d73027",
    mid = "#fee08b",
    high = "#1a9850",
    midpoint = 42,
    limits = c(33, 50),
    breaks = seq(35, 50, by = 5),
    labels = function(x) paste0(x, "%")
  ) +
  
  # Größenskala für Eigentumsquote
  scale_size_continuous(
    name = "Eigentumsquote (%)",
    range = c(8, 15),
    breaks = c(4, 8, 12),
    labels = function(x) paste0(x, "%")
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a"),
    plot.subtitle = element_text(size = 12, color = "#4a4a4a", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "#6a6a6a", hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  labs(
    title = "Stadt Zürich: Eigenmietwert-Abstimmung nach Stadtkreisen",
    subtitle = "Farbe = Ja-Anteil | Punktgröße = Eigentumsquote",
    caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Stadt Zürich\nHinweis: Kreise 1&2, 4&5, 7&8 sind als Wahlkreise zusammengefasst"
  ) +
  
  coord_sf(xlim = c(2680000, 2688000), ylim = c(1246000, 1252000))

# Speichern
ggsave("LinkedIn_StadtZH_Map_Eigenmietwert.png", 
       plot = p1, 
       width = 10, 
       height = 8, 
       dpi = 150,
       bg = "white")

# Alternative: Balkendiagramm-Karte
stadtkreis_data <- stadtkreis_data %>%
  mutate(
    # Kategorien für bessere Visualisierung
    eigentum_kat = cut(Eigentumsobjekte_Prozent,
                       breaks = c(0, 6, 9, 15),
                       labels = c("Niedrig (4-6%)", "Mittel (6-9%)", "Hoch (>9%)"),
                       include.lowest = TRUE)
  )

# Karte 2: Mit Balken für alle drei Abstimmungen
p2 <- ggplot() +
  # Stadt Zürich Umriss
  geom_sf(data = zh_stadt, fill = "grey95", color = "grey50", size = 1) +
  
  # Hintergrund-Kreise für Eigentumsquote
  geom_point(data = stadtkreis_data,
             aes(x = x, y = y, size = Eigentumsobjekte_Prozent),
             color = "grey70", alpha = 0.3) +
  
  # Eigenmietwert
  geom_point(data = stadtkreis_data,
             aes(x = x, y = y - 200, color = "Eigenmietwert"),
             size = stadtkreis_data$Ja_Prozent_Eigenmietwert/5,
             alpha = 0.8) +
  
  # Untermiete
  geom_point(data = stadtkreis_data,
             aes(x = x - 150, y = y + 150, color = "Untermiete"),
             size = stadtkreis_data$Ja_Prozent_Untermiete/5,
             alpha = 0.8) +
  
  # Eigenbedarf
  geom_point(data = stadtkreis_data,
             aes(x = x + 150, y = y + 150, color = "Eigenbedarf"),
             size = stadtkreis_data$Ja_Prozent_Eigenbedarf/5,
             alpha = 0.8) +
  
  # Labels für Stadtkreise
  geom_text(data = stadtkreis_data,
            aes(x = x, y = y, label = label),
            size = 4, fontface = "bold", color = "black") +
  
  # Farben für Abstimmungen
  scale_color_manual(
    name = "Abstimmung",
    values = c("Eigenmietwert" = "#004F90",
               "Untermiete" = "#FF6B35",
               "Eigenbedarf" = "#2E7D32")
  ) +
  
  # Größenskala
  scale_size_continuous(
    name = "Eigentumsquote (%)",
    range = c(8, 15),
    breaks = c(4, 8, 12),
    labels = function(x) paste0(x, "%"),
    guide = "none"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a"),
    plot.subtitle = element_text(size = 12, color = "#4a4a4a", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "#6a6a6a", hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  labs(
    title = "Stadt Zürich: Alle drei Abstimmungen nach Stadtkreisen",
    subtitle = "Punktgröße = Ja-Anteil | Grauer Hintergrund = Eigentumsquote",
    caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Stadt Zürich"
  ) +
  
  coord_sf(xlim = c(2680000, 2688000), ylim = c(1246000, 1252000))

# Speichern
ggsave("LinkedIn_StadtZH_Map_Alle.png", 
       plot = p2, 
       width = 10, 
       height = 8, 
       dpi = 150,
       bg = "white")

# Statistiken ausgeben
cat("\n=== STADT ZÜRICH KARTE: STATISTIKEN ===\n")
cat("Stadtkreis mit höchstem Ja-Anteil Eigenmietwert:", 
    stadtkreis_data$label[which.max(stadtkreis_data$Ja_Prozent_Eigenmietwert)],
    paste0("(", max(stadtkreis_data$Ja_Prozent_Eigenmietwert), "%)"), "\n")
cat("Stadtkreis mit niedrigstem Ja-Anteil Eigenmietwert:", 
    stadtkreis_data$label[which.min(stadtkreis_data$Ja_Prozent_Eigenmietwert)],
    paste0("(", min(stadtkreis_data$Ja_Prozent_Eigenmietwert), "%)"), "\n")
cat("Stadtkreis mit höchster Eigentumsquote:", 
    stadtkreis_data$label[which.max(stadtkreis_data$Eigentumsobjekte_Prozent)],
    paste0("(", max(stadtkreis_data$Eigentumsobjekte_Prozent), "%)"), "\n")

print(p1)
