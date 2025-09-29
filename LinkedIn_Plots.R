#
# =========================
# LinkedIn-optimierte Plots für Abstimmungsanalyse
# =========================

library(tidyverse)
library(ggrepel)
library(readxl)

# Helfer-Funktionen
numify <- function(x) {
  if (is.numeric(x)) return(x)
  x_chr <- as.character(x)
  x_chr <- gsub("%", "", x_chr)
  x_chr <- gsub("\\s+", "", x_chr)
  x_chr <- gsub(",", ".", x_chr, fixed = TRUE)
  suppressWarnings(as.numeric(x_chr))
}

pick_first_col <- function(df, patterns) {
  nms <- names(df)
  for (p in patterns) {
    hit <- nms[grepl(p, nms, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  return(NA_character_)
}

pick_sheet <- function(path) {
  sh <- readxl::excel_sheets(path)
  cand_gem <- sh[grepl("gemeinde", sh, ignore.case = TRUE)]
  if (length(cand_gem) > 0) return(cand_gem[1])
  cand_kant <- sh[grepl("kant", sh, ignore.case = TRUE)]
  if (length(cand_kant) > 0) return(cand_kant[1])
  sh[1]
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# =========================
# Daten einlesen
# =========================
xlsx_path <- "abstimmung_eigentumsanteil_rohdaten_bereinigt.xlsx"
sheet_name <- pick_sheet(xlsx_path)

df <- read_excel(xlsx_path, sheet = sheet_name)

# Spalten automatisch finden
gemeinde_col <- pick_first_col(df, c("gemeinde", "name", "ort", "bezirk", "kanton"))
x_col <- pick_first_col(df, c("^eigentums.*pro", "eigentum.*(anteil|proz|%)", "eigentum"))
y_unter_col <- pick_first_col(df, c("^ja.*untermiet", "untermiet"))
y_eigen_col <- pick_first_col(df, c("^ja.*eigenbed", "eigenbed", "eigenbedarf"))

# Falls keine Name-Spalte gefunden: Index erzeugen
if (is.na(gemeinde_col)) {
  df <- df %>% mutate(Index = row_number())
  gemeinde_col <- "Index"
}

# Numerik sicherstellen
df <- df %>%
  mutate(
    !!x_col := numify(.data[[x_col]]),
    !!y_unter_col := numify(.data[[y_unter_col]]),
    !!y_eigen_col := numify(.data[[y_eigen_col]])
  ) %>%
  filter(!is.na(.data[[x_col]]), !is.na(.data[[y_unter_col]]), !is.na(.data[[y_eigen_col]]))

# =========================
# LinkedIn-Plot Funktion (Vereinfachte, robuste Version)
# =========================
create_linkedin_plot <- function(data, x_var, y_var, name_var, title, y_label, 
                                color_primary = "#0077B5", color_secondary = "#E1F5FE") {
  
  # Einfache lineare Regression für Trendlinie
  model <- lm(reformulate(x_var, y_var), data = data)
  r_squared <- summary(model)$r.squared
  slope <- coef(model)[2]
  
  # Extreme Werte für Labeling identifizieren
  data_with_residuals <- data %>%
    mutate(
      predicted = predict(model),
      residual = .data[[y_var]] - predicted,
      abs_residual = abs(residual)
    )
  
  # Top 8 extreme Werte für Labeling
  extreme_points <- data_with_residuals %>%
    slice_max(abs_residual, n = 8)
  
  # Plot erstellen
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    # Hintergrund
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "grey99", alpha = 0.8) +
    
    # Konfidenzband
    geom_smooth(method = "lm", se = TRUE, 
                color = color_primary, fill = color_secondary,
                alpha = 0.3, linewidth = 2) +
    
    # Datenpunkte mit deutlicher Schattierung nach Quartilen
    geom_point(aes(color = .data[[x_var]], size = .data[[x_var]]), 
               alpha = 0.85, stroke = 1) +
    
    # Zusätzliche Hervorhebung für extreme Punkte
    geom_point(data = extreme_points, 
               color = "#E74C3C", fill = "white", 
               shape = 21, size = 6, stroke = 2, alpha = 1) +
    
    # Labels für extreme Punkte
    geom_text_repel(
      data = extreme_points,
      aes(label = .data[[name_var]]),
      size = 4, 
      color = "#2C3E50",
      fontface = "bold",
      box.padding = 0.8,
      point.padding = 0.5,
      max.overlaps = 12,
      min.segment.length = 0.2,
      force = 5,
      segment.color = "#34495E",
      segment.size = 0.8
    ) +
    
    # Farbskalen mit sehr deutlicher Schattierung
    scale_color_gradient2(low = "#D32F2F", mid = "#FFF59D", high = "#1976D2", 
                         midpoint = median(data[[x_var]], na.rm = TRUE),
                         name = "Eigentumsanteil (%)", guide = "legend") +
    
    # Größenskala für zusätzliche Betonung
    scale_size_continuous(range = c(1.5, 6), guide = "none") +
    
    # Styling für LinkedIn
    labs(
      title = title,
      subtitle = paste0("R² = ", sprintf("%.3f", r_squared), 
                       " • ", nrow(data), " Gemeinden • ",
                       if(slope > 0) "Positive" else "Negative", " Korrelation"),
      x = "Eigentumsanteil (%)",
      y = y_label,
      caption = "📊 Datenquelle: Abstimmungsresultate Kanton Zürich"
    ) +
    
    # Verbessertes Theme
    theme_minimal(base_size = 16) +
    theme(
      # Titel und Untertitel
      plot.title = element_text(
        size = 24, face = "bold", hjust = 0.5,
        color = "#1A1A1A", 
        margin = margin(t = 20, b = 15)
      ),
      plot.subtitle = element_text(
        size = 14, hjust = 0.5,
        color = "#4A4A4A", 
        margin = margin(b = 25)
      ),
      
      # Achsen
      axis.title.x = element_text(
        size = 16, face = "bold", 
        color = "#2C3E50",
        margin = margin(t = 20)
      ),
      axis.title.y = element_text(
        size = 16, face = "bold", 
        color = "#2C3E50",
        margin = margin(r = 20)
      ),
      axis.text = element_text(
        size = 13, color = "#34495E"
      ),
      
      # Grid
      panel.grid.major = element_line(
        color = "#E8E8E8", linewidth = 0.7
      ),
      panel.grid.minor = element_line(
        color = "#F4F4F4", linewidth = 0.4
      ),
      
      # Hintergrund
      plot.background = element_rect(
        fill = "white", color = "#DDDDDD", linewidth = 1
      ),
      panel.background = element_rect(
        fill = "white", color = NA
      ),
      
      # Legende
      legend.position = "bottom",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11),
      legend.key.width = unit(2, "cm"),
      
      # Caption
      plot.caption = element_text(
        size = 12, color = "#7F8C8D", 
        hjust = 1, margin = margin(t = 20)
      ),
      
      # Ränder
      plot.margin = margin(30, 30, 30, 30)
    )
  
  return(p)
}

# =========================
# Plot 1: Untermiet-Initiative
# =========================
plot_untermiete <- create_linkedin_plot(
  data = df,
  x_var = x_col,
  y_var = y_unter_col,
  name_var = gemeinde_col,
  title = "Untermiet-Initiative: Eigentumsanteil vs. Zustimmung",
  y_label = "Ja-Stimmen (%)",
  color_primary = "#1F77B4",
  color_secondary = "#AED6F1"
)

# Speichern als hochauflösendes PNG
ggsave("LinkedIn_Untermiet_Initiative.png", plot_untermiete, 
       width = 12, height = 8, dpi = 300, bg = "white")

print("Plot 1 erstellt: LinkedIn_Untermiet_Initiative.png")
print(plot_untermiete)

# =========================
# Plot 2: Eigenbedarfs-Initiative  
# =========================
plot_eigenbedarf <- create_linkedin_plot(
  data = df,
  x_var = x_col,
  y_var = y_eigen_col,
  name_var = gemeinde_col,
  title = "Eigenbedarfs-Initiative: Eigentumsanteil vs. Zustimmung",
  y_label = "Ja-Stimmen (%)",
  color_primary = "#FF7F0E", 
  color_secondary = "#FFE5CC"
)

# Speichern als hochauflösendes PNG
ggsave("LinkedIn_Eigenbedarfs_Initiative.png", plot_eigenbedarf, 
       width = 12, height = 8, dpi = 300, bg = "white")

print("Plot 2 erstellt: LinkedIn_Eigenbedarfs_Initiative.png")
print(plot_eigenbedarf)

# =========================
# Zusätzliche Infos für LinkedIn Posts
# =========================
cat("\n=== INFORMATIONEN FÜR LINKEDIN POSTS ===\n")
cat("Untermiet-Initiative:\n")
untermiete_model <- lm(reformulate(x_col, y_unter_col), data = df)
cat("- R² =", round(summary(untermiete_model)$r.squared, 3), "\n")
cat("- Korrelation:", if(coef(untermiete_model)[2] > 0) "positiv" else "negativ", "\n")
cat("- Anzahl Gemeinden:", nrow(df), "\n")

cat("\nEigenbedarfs-Initiative:\n")
eigenbedarf_model <- lm(reformulate(x_col, y_eigen_col), data = df)
cat("- R² =", round(summary(eigenbedarf_model)$r.squared, 3), "\n")
cat("- Korrelation:", if(coef(eigenbedarf_model)[2] > 0) "positiv" else "negativ", "\n")
cat("- Anzahl Gemeinden:", nrow(df), "\n")

cat("\n=== PLOTS GESPEICHERT ===\n")
cat("📊 LinkedIn_Untermiet_Initiative.png (12x8 Zoll, 300 DPI)\n")
cat("📊 LinkedIn_Eigenbedarfs_Initiative.png (12x8 Zoll, 300 DPI)\n")
