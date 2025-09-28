library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(viridis)
library(ggrepel)

# Geodaten einlesen (SwissBoundaries)
gemeinden <- st_read("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg", layer = "TLM_HOHEITSGEBIET")

# Abstimmungsdaten einlesen
data <- read_csv("merged_election_ownership.csv")

# Nur Kanton Zürich filtern (kantonsnummer == 1)
zh_gemeinden <- gemeinden %>%
  filter(kantonsnummer == 1) %>%
  select(BFS_NR = bfs_nummer, NAME = name, geometry = geom)

# Mit Abstimmungsdaten verbinden und Seen/fehlende Daten ausschließen
map_data <- zh_gemeinden %>%
  left_join(data, by = "BFS_NR") %>%
  filter(!is.na(Ja_Prozent_Eigenmietwert_EM) & !is.na(Eigentumsobjekte_Prozent))

# Funktion für Karten
create_map <- function(data, fill_var, title, subtitle, legend_title, palette = "viridis", breaks = NULL) {
  
  p <- ggplot(data) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "white", size = 0.1) +
    
    # Farbskala
    {if(palette == "diverging") {
      scale_fill_gradient2(
        name = legend_title,
        low = "#d73027",
        mid = "#fee08b", 
        high = "#1a9850",
        midpoint = 50,
        limits = c(20, 90),
        breaks = breaks,
        labels = function(x) paste0(x, "%")
      )
    } else if(palette == "eigentum") {
      scale_fill_gradient(
        name = legend_title,
        low = "#f7f7f7",
        high = "#2b8cbe",
        limits = c(10, 75),
        breaks = breaks,
        labels = function(x) paste0(x, "%")
      )
    } else {
      scale_fill_viridis_c(
        name = legend_title,
        option = palette,
        breaks = breaks,
        labels = function(x) paste0(x, "%")
      )
    }} +
    
    # Nur Bezirksnamen als Labels
    geom_sf_text(
      data = data %>% 
        group_by(Bezirk) %>%
        summarise(geometry = st_union(geometry)) %>%
        st_centroid() %>%
        filter(!is.na(Bezirk)),
      aes(label = gsub("Bezirk ", "", Bezirk)),
      size = 3.5,
      color = "#333333",
      fontface = "italic",
      alpha = 0.7
    ) +
    
    # Theme
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a"),
      plot.subtitle = element_text(size = 12, color = "#4a4a4a", margin = margin(b = 10)),
      plot.caption = element_text(size = 9, color = "#6a6a6a", hjust = 0),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Kanton Zürich"
    )
  
  return(p)
}

# Karte 1: Eigenmietwert Ja-Anteil
map1 <- create_map(
  map_data,
  "Ja_Prozent_Eigenmietwert_EM",
  "Eigenmietwert-Abschaffung: Ja-Anteil nach Gemeinde",
  "Kanton Zürich - Abstimmung vom 28. September 2025",
  "Ja-Anteil",
  palette = "diverging",
  breaks = seq(30, 80, by = 10)
)

ggsave("LinkedIn_Map_Eigenmietwert_JaAnteil.png", 
       plot = map1, 
       width = 12, 
       height = 8, 
       dpi = 150,
       bg = "white")

# Karte 2: Eigentumsquote
map2 <- create_map(
  map_data,
  "Eigentumsobjekte_Prozent",
  "Wohneigentumsquote im Kanton Zürich",
  "Anteil Eigentumsobjekte nach Gemeinde",
  "Eigentumsquote",
  palette = "eigentum",
  breaks = seq(20, 70, by = 10)
)

ggsave("LinkedIn_Map_Eigentumsquote.png", 
       plot = map2, 
       width = 12, 
       height = 8, 
       dpi = 150,
       bg = "white")

# Karte 3: Kombinierte Darstellung (2 Karten nebeneinander)
library(patchwork)

# Kompaktere Version für Side-by-side mit besserer Legende
map1_compact <- map1 + 
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        legend.margin = margin(t = 5, b = 5),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 10),
        plot.margin = margin(10, 10, 5, 10)) +
  labs(title = "Ja-Anteil Eigenmietwert-Abschaffung",
       subtitle = NULL) +
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 0.5))

map2_compact <- map2 + 
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        legend.margin = margin(t = 5, b = 5),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 10),
        plot.margin = margin(10, 10, 5, 10)) +
  labs(title = "Wohneigentumsquote",
       subtitle = NULL) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 0.5))

combined_maps <- map2_compact + map1_compact +
  plot_annotation(
    title = "Wohneigentum prägt das Abstimmungsverhalten",
    subtitle = "Kanton Zürich: Eigentumsquote und Ja-Anteil zur Eigenmietwert-Abschaffung",
    caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Kanton Zürich",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a"),
      plot.subtitle = element_text(size = 12, color = "#4a4a4a"),
      plot.caption = element_text(size = 9, color = "#6a6a6a", hjust = 0)
    )
  )

ggsave("LinkedIn_Map_Kombiniert.png", 
       plot = combined_maps, 
       width = 14, 
       height = 7, 
       dpi = 150,
       bg = "white")

# Statistiken für die Karten
cat("\n=== KARTENSTATISTIKEN ===\n")
cat("Gemeinden mit höchstem Ja-Anteil Eigenmietwert:\n")
map_data %>%
  st_drop_geometry() %>%
  arrange(desc(Ja_Prozent_Eigenmietwert_EM)) %>%
  head(5) %>%
  select(GEBIET_NAME, Ja_Prozent_Eigenmietwert_EM, Eigentumsobjekte_Prozent) %>%
  print()

cat("\nGemeinden mit höchster Eigentumsquote:\n")
map_data %>%
  st_drop_geometry() %>%
  arrange(desc(Eigentumsobjekte_Prozent)) %>%
  head(5) %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, Ja_Prozent_Eigenmietwert_EM) %>%
  print()

print(map1)
