library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(viridis)

# Geodaten einlesen
gemeinden <- st_read("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg", layer = "TLM_HOHEITSGEBIET")

# Abstimmungsdaten einlesen
data <- read_csv("merged_election_ownership.csv")

# Nur Kanton Zürich filtern
zh_gemeinden <- gemeinden %>%
  filter(kantonsnummer == 1) %>%
  select(BFS_NR = bfs_nummer, NAME = name, geometry = geom)

# Mit Abstimmungsdaten verbinden
map_data <- zh_gemeinden %>%
  left_join(data, by = "BFS_NR") %>%
  mutate(
    # Kategorien für Eigentumsquote
    eigentum_kat = cut(Eigentumsobjekte_Prozent, 
                       breaks = c(0, 30, 50, 100),
                       labels = c("Niedrig (<30%)", "Mittel (30-50%)", "Hoch (>50%)"),
                       include.lowest = TRUE),
    # Kategorien für Ja-Anteil
    ja_kat = cut(Ja_Prozent_Eigenmietwert_EM,
                 breaks = c(0, 60, 70, 100),
                 labels = c("<60%", "60-70%", ">70%"),
                 include.lowest = TRUE)
  )

# Option 1: Bivariate Choropleth Map
# Erstelle 9 Kategorien (3x3 Matrix)
map_data <- map_data %>%
  mutate(
    eigentum_tercile = ntile(Eigentumsobjekte_Prozent, 3),
    ja_tercile = ntile(Ja_Prozent_Eigenmietwert_EM, 3),
    bivariate_class = paste0(eigentum_tercile, "-", ja_tercile)
  )

# Farbpalette für bivariate map (3x3 Matrix)
bivariate_colors <- c(
  "1-1" = "#e8e8e8", "1-2" = "#b5c0da", "1-3" = "#6c83b5",
  "2-1" = "#b8d6be", "2-2" = "#90b2b3", "2-3" = "#567994",
  "3-1" = "#73ae80", "3-2" = "#5a9178", "3-3" = "#2a5a5b"
)

# Bivariate Map erstellen
p_bivariate <- ggplot(map_data) +
  geom_sf(aes(fill = bivariate_class), color = "white", size = 0.1) +
  scale_fill_manual(
    values = bivariate_colors,
    guide = "none"
  ) +
  
  # Labels für wichtige Städte
  geom_sf_text(
    data = map_data %>% filter(GEBIET_NAME %in% c("Zürich", "Winterthur", "Uster", "Dübendorf")),
    aes(label = GEBIET_NAME),
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a"),
    plot.subtitle = element_text(size = 12, color = "#4a4a4a", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "#6a6a6a", hjust = 0),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 80, 20, 20)
  ) +
  
  labs(
    title = "Wohneigentum und Eigenmietwert-Abstimmung im Kanton Zürich",
    subtitle = "Je dunkler, desto höher sowohl Eigentumsquote als auch Ja-Anteil",
    caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Kanton Zürich"
  )

# Legende als separates Objekt erstellen
legend_data <- expand.grid(eigentum = 1:3, ja = 1:3) %>%
  mutate(bivariate_class = paste0(eigentum, "-", ja))

legend_plot <- ggplot(legend_data, aes(x = eigentum, y = ja, fill = bivariate_class)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = bivariate_colors, guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = c("Niedrig", "Mittel", "Hoch"),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:3, labels = c("Niedrig", "Mittel", "Hoch"),
                     expand = c(0, 0)) +
  labs(x = "Eigentumsquote →", y = "Ja-Anteil →") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_fixed()

# Kombiniere Map und Legende
library(patchwork)
p_final <- p_bivariate + 
  inset_element(legend_plot, 
                left = 0.75, bottom = 0.05, 
                right = 0.95, top = 0.25)

ggsave("LinkedIn_Map_Merged_Bivariate.png", 
       plot = p_final, 
       width = 12, 
       height = 8, 
       dpi = 150,
       bg = "white")

# Option 2: Punkte-Overlay für Eigentumsquote
p_overlay <- ggplot(map_data) +
  # Flächen zeigen Ja-Anteil
  geom_sf(aes(fill = Ja_Prozent_Eigenmietwert_EM), color = "white", size = 0.1) +
  scale_fill_gradient2(
    name = "Ja-Anteil (%)",
    low = "#d73027",
    mid = "#fee08b", 
    high = "#1a9850",
    midpoint = 50,
    limits = c(20, 90),
    breaks = seq(30, 80, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  
  # Punkte zeigen Eigentumsquote (Größe = Eigentumsquote)
  geom_sf(
    data = st_centroid(map_data),
    aes(size = Eigentumsobjekte_Prozent),
    color = "black",
    alpha = 0.5,
    show.legend = "point"
  ) +
  scale_size_continuous(
    name = "Eigentumsquote (%)",
    range = c(0.1, 4),
    breaks = c(20, 40, 60),
    labels = function(x) paste0(x, "%")
  ) +
  
  # Labels für wichtige Städte
  geom_sf_text(
    data = map_data %>% filter(GEBIET_NAME %in% c("Zürich", "Winterthur")),
    aes(label = GEBIET_NAME),
    size = 3,
    color = "black",
    fontface = "bold",
    nudge_y = 2000
  ) +
  
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
    title = "Doppelter Effekt: Eigentumsquote und Ja-Anteil zur Eigenmietwert-Abschaffung",
    subtitle = "Farbe = Ja-Anteil | Punktgröße = Eigentumsquote",
    caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Kanton Zürich"
  ) +
  
  guides(
    fill = guide_colorbar(order = 1),
    size = guide_legend(order = 2)
  )

# Warnungen unterdrücken
suppressWarnings({
  ggsave("LinkedIn_Map_Merged_Overlay.png", 
         plot = p_overlay, 
         width = 12, 
         height = 8, 
         dpi = 150,
         bg = "white")
})

# Statistiken ausgeben
cat("\n=== MERGED MAP STATISTIKEN ===\n")
cat("Gemeinden mit hoher Eigentumsquote UND hohem Ja-Anteil (beide >60%):\n")
map_data %>%
  st_drop_geometry() %>%
  filter(Eigentumsobjekte_Prozent > 60 & Ja_Prozent_Eigenmietwert_EM > 70) %>%
  arrange(desc(Ja_Prozent_Eigenmietwert_EM)) %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, Ja_Prozent_Eigenmietwert_EM) %>%
  print()

print(p_final)
