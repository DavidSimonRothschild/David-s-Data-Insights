library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(ggrepel)

# Daten einlesen
data <- read_csv("merged_election_ownership.csv")

# Daten filtern - nur Gemeinden mit vollständigen Daten
data_clean <- data %>%
  filter(!is.na(Ja_Prozent_Untermiete) & !is.na(Eigentumsobjekte_Prozent))

# Korrelation berechnen
correlation <- cor(data_clean$Eigentumsobjekte_Prozent, 
                   data_clean$Ja_Prozent_Untermiete, 
                   use = "complete.obs")

# Lineare Regression für R²
model <- lm(Ja_Prozent_Untermiete ~ Eigentumsobjekte_Prozent, data = data_clean)
r_squared <- summary(model)$r.squared

# Hauptplot erstellen
p <- ggplot(data_clean, aes(x = Eigentumsobjekte_Prozent, y = Ja_Prozent_Untermiete)) +
  # Punkte mit Bezirk-Färbung
  geom_point(aes(color = Bezirk), size = 3, alpha = 0.7) +
  
  # Regressionslinie mit Konfidenzintervall
  geom_smooth(method = "lm", se = TRUE, color = "#FF6B35", fill = "#FF6B35", alpha = 0.2) +
  
  # 50% Linie für Annahme/Ablehnung
  geom_hline(yintercept = 50, linetype = "dashed", color = "#666666", alpha = 0.5) +
  annotate("text", x = 75, y = 51, label = "50% Annahmeschwelle", 
           size = 3, color = "#666666", hjust = 1) +
  
  # Labels für einige interessante Gemeinden
  geom_text_repel(
    data = data_clean %>% 
      filter(Eigentumsobjekte_Prozent > 70 | 
             Eigentumsobjekte_Prozent < 25 |
             Ja_Prozent_Untermiete > 65 |
             Ja_Prozent_Untermiete < 45),
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
    title = "Wohneigentum und Untermiete-Initiative im Kanton Zürich",
    subtitle = paste0("Zusammenhang zwischen Eigentumsquote und Ja-Anteil zur Untermiete-Initiative\n",
                      "Korrelation: r = ", round(correlation, 3), " | R² = ", round(r_squared, 3),
                      " | Angenommen in ", sum(data_clean$Untermiete_Angenommen), " von ", 
                      nrow(data_clean), " Gemeinden"),
    x = "Wohneigentumsquote (%)",
    y = "Ja-Anteil Untermiete-Initiative (%)",
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
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
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
    breaks = seq(35, 70, by = 5),
    labels = function(x) paste0(x, "%"),
    limits = c(35, 70)
  ) +
  
  # Farbpalette für Bezirke
  scale_color_manual(values = c(
    "#004F90", "#0066CC", "#3399FF", "#66B2FF", 
    "#99CCFF", "#FF6B35", "#FF8C42", "#FFA652",
    "#2E7D32", "#43A047", "#66BB6A", "#81C784"
  ))

# Plot speichern in hoher Qualität für LinkedIn
ggsave("LinkedIn_Untermiete_Grafik.png", 
       plot = p, 
       width = 12, 
       height = 6.75, 
       dpi = 150,
       bg = "white")

# Zusätzlich eine quadratische Version
p_square <- p + 
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(nrow = 2))

ggsave("LinkedIn_Untermiete_Grafik_square.png", 
       plot = p_square, 
       width = 8, 
       height = 8, 
       dpi = 150,
       bg = "white")

# Statistiken ausgeben
cat("\n=== UNTERMIETE-INITIATIVE: STATISTISCHE ZUSAMMENFASSUNG ===\n")
cat("Anzahl Gemeinden:", nrow(data_clean), "\n")
cat("Korrelation (Pearson):", round(correlation, 4), "\n")
cat("R²:", round(r_squared, 4), "\n")
cat("Steigung:", round(coef(model)[2], 4), "\n")
cat("p-Wert:", format.pval(summary(model)$coefficients[2,4], digits = 4), "\n")
cat("Gemeinden mit Annahme (>50%):", sum(data_clean$Untermiete_Angenommen), "\n")
cat("Gemeinden mit Ablehnung (<50%):", sum(!data_clean$Untermiete_Angenommen), "\n\n")

# Gemeinden nahe der 50% Schwelle
cat("Gemeinden nahe der 50% Schwelle (49-51%):\n")
data_clean %>%
  filter(Ja_Prozent_Untermiete >= 49 & Ja_Prozent_Untermiete <= 51) %>%
  arrange(Ja_Prozent_Untermiete) %>%
  select(GEBIET_NAME, Eigentumsobjekte_Prozent, Ja_Prozent_Untermiete) %>%
  print()

print(p)
