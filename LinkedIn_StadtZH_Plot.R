library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(ggrepel)
library(tidyr)

# Daten einlesen
data <- read_excel("data_stdzh.xlsx")

# Daten vorbereiten - alle Zeilen verwenden
data_clean <- data %>%
  filter(!is.na(Eigentumsobjekte_Prozent))

# Funktion für einzelne Plots
create_city_plot <- function(data, y_var, y_label, title_suffix, color, filename_prefix) {
  
  # Korrelation berechnen
  correlation <- cor(data$Eigentumsobjekte_Prozent, data[[y_var]], use = "complete.obs")
  
  # Lineare Regression
  model <- lm(as.formula(paste(y_var, "~ Eigentumsobjekte_Prozent")), data = data)
  r_squared <- summary(model)$r.squared
  
  # Plot erstellen
  p <- ggplot(data, aes(x = Eigentumsobjekte_Prozent, y = .data[[y_var]])) +
    # Punkte
    geom_point(size = 4, color = color, alpha = 0.8) +
    
    # Regressionslinie ohne Konfidenzintervall
    geom_smooth(method = "lm", se = FALSE, color = color, size = 1.2) +
    
    # 50% Linie wenn relevant
    {if(y_var != "Ja_Prozent_Eigenmietwert") geom_hline(yintercept = 50, linetype = "dashed", color = "#666666", alpha = 0.5)} +
    
    # Labels für alle Stadtkreise
    geom_text_repel(
      aes(label = GEBIET_NAME),
      size = 2.5,
      box.padding = 0.3,
      point.padding = 0.2,
      segment.color = "grey50",
      segment.alpha = 0.5,
      max.overlaps = 20,
      force = 2
    ) +
    
    # Achsenbeschriftungen
    labs(
      title = paste0("Stadt Zürich: Wohneigentum und ", title_suffix),
      subtitle = paste0("Zusammenhang auf Stadtkreis-Ebene (inkl. zusammengefasste Wahlkreise)\n",
                        "Korrelation: r = ", round(correlation, 3), " | R² = ", round(r_squared, 3)),
      x = "Wohneigentumsquote (%)",
      y = y_label,
      caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Stadt Zürich\nHinweis: Kreise 1&2, 4&5, 7&8 sind als Wahlkreise zusammengefasst"
    ) +
    
    # Theme
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a"),
      plot.subtitle = element_text(size = 12, color = "#4a4a4a", margin = margin(b = 10)),
      plot.caption = element_text(size = 9, color = "#6a6a6a", hjust = 0),
      axis.title = element_text(size = 11, face = "bold", color = "#2a2a2a"),
      axis.text = element_text(size = 10, color = "#3a3a3a"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    # Achsen formatieren
    scale_x_continuous(
      breaks = seq(0, 14, by = 2),
      labels = function(x) paste0(x, "%"),
      limits = c(3, 13)
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%")
    )
  
  # Speichern
  ggsave(paste0(filename_prefix, ".png"), 
         plot = p, 
         width = 12, 
         height = 6.75, 
         dpi = 150,
         bg = "white")
  
  # Quadratische Version
  ggsave(paste0(filename_prefix, "_square.png"), 
         plot = p, 
         width = 8, 
         height = 8, 
         dpi = 150,
         bg = "white")
  
  return(p)
}

# Plot 1: Eigenmietwert
p1 <- create_city_plot(
  data_clean,
  "Ja_Prozent_Eigenmietwert",
  "Ja-Anteil Eigenmietwert-Abschaffung (%)",
  "Eigenmietwert-Abschaffung",
  "#004F90",
  "LinkedIn_StadtZH_Eigenmietwert"
)

# Plot 2: Untermiete
p2 <- create_city_plot(
  data_clean,
  "Ja_Prozent_Untermiete",
  "Ja-Anteil Untermiete-Initiative (%)",
  "Untermiete-Initiative",
  "#FF6B35",
  "LinkedIn_StadtZH_Untermiete"
)

# Plot 3: Eigenbedarf
p3 <- create_city_plot(
  data_clean,
  "Ja_Prozent_Eigenbedarf",
  "Ja-Anteil Eigenbedarf-Initiative (%)",
  "Eigenbedarf-Initiative",
  "#2E7D32",
  "LinkedIn_StadtZH_Eigenbedarf"
)

# Kombinierter Plot mit allen drei Abstimmungen
# Daten für kombinierten Plot vorbereiten
data_long <- data_clean %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, 
         Ja_Prozent_Eigenmietwert, Ja_Prozent_Untermiete, Ja_Prozent_Eigenbedarf) %>%
  pivot_longer(
    cols = c(Ja_Prozent_Eigenmietwert, Ja_Prozent_Untermiete, Ja_Prozent_Eigenbedarf),
    names_to = "Abstimmung",
    values_to = "Ja_Prozent"
  ) %>%
  mutate(
    Abstimmung = case_when(
      Abstimmung == "Ja_Prozent_Eigenmietwert" ~ "Eigenmietwert-Abschaffung",
      Abstimmung == "Ja_Prozent_Untermiete" ~ "Untermiete-Initiative",
      Abstimmung == "Ja_Prozent_Eigenbedarf" ~ "Eigenbedarf-Initiative"
    ),
    Abstimmung = factor(Abstimmung, levels = c("Eigenmietwert-Abschaffung", "Untermiete-Initiative", "Eigenbedarf-Initiative"))
  )

# Kombinierter Plot
p_combined <- ggplot(data_long, aes(x = Eigentumsobjekte_Prozent, y = Ja_Prozent, color = Abstimmung)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  
  # 50% Linie
  geom_hline(yintercept = 50, linetype = "dashed", color = "#666666", alpha = 0.5) +
  
  # Facetten für jede Abstimmung
  facet_wrap(~ Abstimmung, ncol = 3) +
  
  # Labels
  labs(
    title = "Stadt Zürich: Wohneigentum prägt das Abstimmungsverhalten",
    subtitle = "Zusammenhang zwischen Eigentumsquote und Ja-Anteil bei drei Mietrechts-Vorlagen",
    x = "Wohneigentumsquote (%)",
    y = "Ja-Anteil (%)",
    caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Stadt Zürich\nHinweis: Kreise 1&2, 4&5, 7&8 sind als Wahlkreise zusammengefasst"
  ) +
  
  # Theme
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a"),
    plot.subtitle = element_text(size = 12, color = "#4a4a4a", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "#6a6a6a", hjust = 0),
    axis.title = element_text(size = 11, face = "bold", color = "#2a2a2a"),
    axis.text = element_text(size = 9, color = "#3a3a3a"),
    legend.position = "none",
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Achsen formatieren
  scale_x_continuous(
    breaks = seq(4, 12, by = 2),
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  
  # Farben
  scale_color_manual(values = c(
    "Eigenmietwert-Abschaffung" = "#004F90",
    "Untermiete-Initiative" = "#FF6B35",
    "Eigenbedarf-Initiative" = "#2E7D32"
  ))

# Speichern
ggsave("LinkedIn_StadtZH_Alle_Abstimmungen.png", 
       plot = p_combined, 
       width = 14, 
       height = 5, 
       dpi = 150,
       bg = "white")

# Statistiken ausgeben
cat("\n=== STADT ZÜRICH: STATISTISCHE ZUSAMMENFASSUNG ===\n\n")

for(abstimmung in c("Ja_Prozent_Eigenmietwert", "Ja_Prozent_Untermiete", "Ja_Prozent_Eigenbedarf")) {
  
  name <- switch(abstimmung,
                 "Ja_Prozent_Eigenmietwert" = "EIGENMIETWERT-ABSCHAFFUNG",
                 "Ja_Prozent_Untermiete" = "UNTERMIETE-INITIATIVE",
                 "Ja_Prozent_Eigenbedarf" = "EIGENBEDARF-INITIATIVE")
  
  cat(name, ":\n")
  
  correlation <- cor(data_clean$Eigentumsobjekte_Prozent, data_clean[[abstimmung]], use = "complete.obs")
  model <- lm(as.formula(paste(abstimmung, "~ Eigentumsobjekte_Prozent")), data = data_clean)
  
  cat("  Korrelation:", round(correlation, 4), "\n")
  cat("  R²:", round(summary(model)$r.squared, 4), "\n")
  cat("  Steigung:", round(coef(model)[2], 4), "\n")
  cat("  p-Wert:", format.pval(summary(model)$coefficients[2,4], digits = 4), "\n\n")
}

# Stadtkreise nach Eigentumsquote sortiert
cat("Stadtkreise sortiert nach Eigentumsquote:\n")
data_clean %>%
  arrange(desc(Eigentumsobjekte_Prozent)) %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, 
         Ja_Prozent_Eigenmietwert, Ja_Prozent_Untermiete, Ja_Prozent_Eigenbedarf) %>%
  print(n = 20)

# Stadtkreise mit höchstem und niedrigstem Ja-Anteil
cat("\nStadtkreise mit höchstem Ja-Anteil Eigenmietwert:\n")
data_clean %>%
  arrange(desc(Ja_Prozent_Eigenmietwert)) %>%
  head(3) %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, Ja_Prozent_Eigenmietwert) %>%
  print()

cat("\nStadtkreise mit niedrigstem Ja-Anteil Eigenmietwert:\n")
data_clean %>%
  arrange(Ja_Prozent_Eigenmietwert) %>%
  head(3) %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, Ja_Prozent_Eigenmietwert) %>%
  print()

print(p1)
