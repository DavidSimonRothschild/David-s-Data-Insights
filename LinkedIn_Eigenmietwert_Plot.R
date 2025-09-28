library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(ggrepel)

# Daten einlesen
data <- read_csv("merged_election_ownership.csv")

# Daten filtern - nur Gemeinden mit vollständigen Daten
data_clean <- data %>%
  filter(!is.na(Ja_Prozent_Eigenmietwert_EM) & !is.na(Eigentumsobjekte_Prozent))

# Korrelation berechnen
correlation <- cor(data_clean$Eigentumsobjekte_Prozent, 
                   data_clean$Ja_Prozent_Eigenmietwert_EM, 
                   use = "complete.obs")

# Lineare Regression für R²
model <- lm(Ja_Prozent_Eigenmietwert_EM ~ Eigentumsobjekte_Prozent, data = data_clean)
r_squared <- summary(model)$r.squared

# Hauptplot erstellen
p <- ggplot(data_clean, aes(x = Eigentumsobjekte_Prozent, y = Ja_Prozent_Eigenmietwert_EM)) +
  # Punkte mit Bezirk-Färbung
  geom_point(aes(color = Bezirk), size = 3, alpha = 0.7) +
  
  # Regressionslinie mit Konfidenzintervall
  geom_smooth(method = "lm", se = TRUE, color = "#004F90", fill = "#004F90", alpha = 0.2) +
  
  # Labels für einige interessante Gemeinden
  geom_text_repel(
    data = data_clean %>% 
      filter(Eigentumsobjekte_Prozent > 70 | 
             Eigentumsobjekte_Prozent < 25 |
             Ja_Prozent_Eigenmietwert_EM > 85 |
             Ja_Prozent_Eigenmietwert_EM < 60),
    aes(label = GEBIET_NAME),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "grey50",
    segment.alpha = 0.5,
    max.overlaps = 15
  ) +
  
  # Achsenbeschriftungen
  labs(
    title = "Eigentum und Abstimmungsverhalten im Kanton Zürich",
    subtitle = paste0("Zusammenhang zwischen Wohneigentumsquote und Ja-Anteil zur Eigenmietwert-Abschaffung\n",
                      "Korrelation: r = ", round(correlation, 3), " | R² = ", round(r_squared, 3)),
    x = "Wohneigentumsquote (%)",
    y = "Ja-Anteil Eigenmietwert-Abschaffung (%)",
    color = "Bezirk",
    caption = "Daten: Abstimmung vom 28.09.2025 | Quelle: Statistisches Amt Kanton Zürich"
  ) +
  
  # Theme anpassen für LinkedIn
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a"),
    plot.subtitle = element_text(size = 12, color = "#4a4a4a", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "#6a6a6a", hjust = 0),
    axis.title = element_text(size = 11, face = "bold", color = "#2a2a2a"),
    axis.text = element_text(size = 10, color = "#3a3a3a"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e0e0e0", size = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Achsen formatieren
  scale_x_continuous(
    breaks = seq(10, 80, by = 10),
    labels = function(x) paste0(x, "%"),
    limits = c(min(data_clean$Eigentumsobjekte_Prozent) - 2, 
               max(data_clean$Eigentumsobjekte_Prozent) + 2)
  ) +
  scale_y_continuous(
    breaks = seq(50, 90, by = 5),
    labels = function(x) paste0(x, "%"),
    limits = c(min(data_clean$Ja_Prozent_Eigenmietwert_EM) - 2, 
               max(data_clean$Ja_Prozent_Eigenmietwert_EM) + 2)
  ) +
  
  # Farbpalette für Bezirke
  scale_color_manual(values = c(
    "#004F90", "#0066CC", "#3399FF", "#66B2FF", 
    "#99CCFF", "#FF6B35", "#FF8C42", "#FFA652",
    "#2E7D32", "#43A047", "#66BB6A", "#81C784"
  ))

# Plot speichern in hoher Qualität für LinkedIn (1200x675 px ist optimal für LinkedIn)
ggsave("LinkedIn_Eigenmietwert_Grafik.png", 
       plot = p, 
       width = 12, 
       height = 6.75, 
       dpi = 150,
       bg = "white")

# Zusätzlich eine quadratische Version für Instagram/andere Plattformen
p_square <- p + 
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(nrow = 2))

ggsave("LinkedIn_Eigenmietwert_Grafik_square.png", 
       plot = p_square, 
       width = 8, 
       height = 8, 
       dpi = 150,
       bg = "white")

# Statistiken ausgeben
cat("\n=== STATISTISCHE ZUSAMMENFASSUNG ===\n")
cat("Anzahl Gemeinden:", nrow(data_clean), "\n")
cat("Korrelation (Pearson):", round(correlation, 4), "\n")
cat("R²:", round(r_squared, 4), "\n")
cat("Steigung:", round(coef(model)[2], 4), "\n")
cat("p-Wert:", format.pval(summary(model)$coefficients[2,4], digits = 4), "\n\n")

# Top 5 Gemeinden mit höchster Eigentumsquote
cat("Top 5 Gemeinden mit höchster Eigentumsquote:\n")
data_clean %>%
  arrange(desc(Eigentumsobjekte_Prozent)) %>%
  head(5) %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, Ja_Prozent_Eigenmietwert_EM) %>%
  print()

cat("\n")

# Top 5 Gemeinden mit niedrigster Eigentumsquote
cat("Top 5 Gemeinden mit niedrigster Eigentumsquote:\n")
data_clean %>%
  arrange(Eigentumsobjekte_Prozent) %>%
  head(5) %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, Ja_Prozent_Eigenmietwert_EM) %>%
  print()

print(p)
