# =========================
# Pakete
# =========================
library(tidyverse)
library(ggrepel)
library(readxl)
# lme4 wird nicht benötigt, kann aber geladen bleiben

# =========================
# Helfer
# =========================
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

# =========================
# Daten einlesen (robust)
# =========================
xlsx_path <- "abstimmung_eigentumsanteil_rohdaten_bereinigt.xlsx"
sheet_name <- pick_sheet(xlsx_path)
cat("Gewähltes Sheet:", sheet_name, "\n")

df <- read_excel(xlsx_path, sheet = sheet_name)

# Falls es nur numerische Spalten gibt, später Index erzeugen.
# Versuche Spalten automatisch zu finden:
gemeinde_col <- pick_first_col(df, c("gemeinde", "name", "ort", "bezirk", "kanton"))

x_col <- pick_first_col(df, c(
  "^eigentums.*pro", "eigentum.*(anteil|proz|%)", "eigentum"
))
y_unter_col <- pick_first_col(df, c(
  "^ja.*untermiet", "untermiet"
))
y_eigen_col <- pick_first_col(df, c(
  "^ja.*eigenbed", "eigenbed", "eigenbedarf"
))

# Diagnose
cat("Spaltenwahl:\n",
    "  Name/Gemeinde: ", gemeinde_col %||% "—", "\n",
    "  X (Eigentum%): ", x_col %||% "—", "\n",
    "  Y (Untermiete): ", y_unter_col %||% "—", "\n",
    "  Y (Eigenbedarf): ", y_eigen_col %||% "—", "\n", sep = "")

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# Falls keine Name-Spalte gefunden: Index erzeugen
if (is.na(gemeinde_col)) {
  df <- df %>% mutate(Index = row_number())
  gemeinde_col <- "Index"
  cat("Hinweis: Keine Namensspalte gefunden – verwende 'Index' als Label.\n")
}

# Existenz der Zielspalten prüfen
missing_cols <- setdiff(c(x_col, y_unter_col, y_eigen_col), names(df))
if (length(missing_cols) > 0) {
  stop("Fehlende erforderliche Spalten im Datensatz: ", paste(missing_cols, collapse = ", "),
       "\nVerfügbare Spalten sind: ", paste(names(df), collapse = " | "))
}

# Numerik sicherstellen
df <- df %>%
  mutate(
    !!x_col := numify(.data[[x_col]]),
    !!y_unter_col := numify(.data[[y_unter_col]]),
    !!y_eigen_col := numify(.data[[y_eigen_col]])
  )

# =========================
# Kernfunktion: Outlier + Plot
# =========================
plot_with_outliers <- function(data, x, y, name_col,
                               title, subtitle_suffix = NULL,
                               label_top_n = 15, pi_level = 0.95,
                               seed = 42) {
  
  stopifnot(all(c(x, y, name_col) %in% names(data)))
  d <- data %>% filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
  if (nrow(d) < 5) stop("Zu wenige Beobachtungen nach NA-Filter (n < 5).")
  
  m <- lm(reformulate(x, y), data = d)
  n <- nobs(m)
  
  pi_df <- as.data.frame(predict(m, newdata = d, interval = "prediction", level = pi_level))
  
  d <- d %>%
    mutate(
      fit   = pi_df$fit,
      lwr   = pi_df$lwr,
      upr   = pi_df$upr,
      resid = residuals(m),
      rstud = rstudent(m),
      cook  = cooks.distance(m),
      hat   = hatvalues(m)
    ) %>%
    mutate(
      outside_pi = (.data[[y]] < lwr) | (.data[[y]] > upr),
      big_rstud  = abs(rstud) > 2,
      big_cook   = cook > (4 / n),
      is_deviant = outside_pi | big_rstud | big_cook,
      dev_dir = dplyr::case_when(
        is_deviant & resid > 0 ~ "deutlich höher als erwartet",
        is_deviant & resid < 0 ~ "deutlich tiefer als erwartet",
        TRUE ~ "im Erwartungsband"
      )
    )
  
  to_label <- d %>% filter(is_deviant) %>% arrange(desc(abs(rstud))) %>% slice_head(n = label_top_n)
  d_ribbon <- d %>% arrange(.data[[x]])
  
  set.seed(seed)
  
  gg <- ggplot(d, aes(x = .data[[x]], y = .data[[y]], color = dev_dir, shape = dev_dir)) +
    geom_ribbon(
      data = d_ribbon,
      aes(x = .data[[x]], ymin = lwr, ymax = upr),
      inherit.aes = FALSE, alpha = 0.12
    ) +
    geom_line(
      data = d_ribbon,
      aes(x = .data[[x]], y = fit),
      inherit.aes = FALSE, linewidth = 0.9
    ) +
    geom_point(alpha = 0.85, size = 2.8) +
    ggrepel::geom_text_repel(
      data = to_label,
      aes(label = .data[[name_col]], x = .data[[x]], y = .data[[y]]),
      size = 3.4, max.overlaps = 20, box.padding = 0.5,
      point.padding = 0.35, force = 2, min.segment.length = 0
    ) +
    scale_color_manual(
      values = c(
        "deutlich höher als erwartet" = "#D55E00",
        "deutlich tiefer als erwartet" = "#0072B2",
        "im Erwartungsband"          = "#7F8C8D"
      ),
      name = "Abweichung"
    ) +
    scale_shape_manual(
      values = c(
        "deutlich höher als erwartet" = 17,
        "deutlich tiefer als erwartet" = 17,
        "im Erwartungsband"            = 16
      ),
      name = "Abweichung"
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 3)),
      shape = guide_legend(override.aes = list(size = 3))
    ) +
    labs(
      title = title,
      subtitle = paste0(
        "Deutlich abweichende Gemeinden (", sum(d$is_deviant, na.rm = TRUE), ")",
        if (!is.null(subtitle_suffix)) paste0(" — ", subtitle_suffix) else ""
      ),
      x = "Anteil Eigentumsobjekte (%)",
      y = "Ja-Stimmenanteil (%)",
      caption = paste0(
        "R² = ", round(summary(m)$r.squared, 3),
        " | β(Eigentumsanteil) p = ", signif(summary(m)$coefficients[2, 4], 3),
        " | Kriterien: außerhalb ", 100 * pi_level, "%-PI, |r_student|>2, Cook's D>4/n"
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, colour = "grey30"),
      plot.caption = element_text(colour = "grey40"),
      legend.position = "bottom"
    )
  
  list(data = d, model = m, plot = gg)
}

# =========================
# Anwendung: Untermiete
# =========================
res_untermiete <- plot_with_outliers(
  data = df,
  x = x_col,
  y = y_unter_col,
  name_col = gemeinde_col,
  title = "Eigentumsanteil vs. Ja-Anteil (Untermiet-Initiative)",
  subtitle_suffix = "Deutliche Abweichungen hervorgehoben",
  label_top_n = 15,
  seed = 42
)
print(res_untermiete$plot)

out_untermiete <- res_untermiete$data %>%
  filter(is_deviant) %>%
  transmute(
    Gemeinde = .data[[gemeinde_col]],
    !!x_col := .data[[x_col]],
    !!y_unter_col := .data[[y_unter_col]],
    Residuum = resid, r_student = rstud, CookD = cook, Hebel = hat, Abweichung = dev_dir
  ) %>%
  arrange(desc(abs(r_student)))
cat("\nOutlier-Gemeinden (Untermiet-Initiative):\n"); print(out_untermiete, n = 30)

cat("\nDiagnostik (Untermiet-Initiative):\n")
cat("n =", nobs(res_untermiete$model),
    "| Mean(|Residuen|) =", round(mean(abs(res_untermiete$data$resid), na.rm = TRUE), 2),
    "| SD(Residuen) =", round(sd(res_untermiete$data$resid, na.rm = TRUE), 2),
    "| # |r_student|>2 =", sum(abs(res_untermiete$data$rstud) > 2, na.rm = TRUE),
    "| # CookD>4/n =", sum(res_untermiete$data$cook > 4/nobs(res_untermiete$model), na.rm = TRUE), "\n")

# =========================
# Anwendung: Eigenbedarf
# =========================
res_eigenbedarf <- plot_with_outliers(
  data = df,
  x = x_col,
  y = y_eigen_col,
  name_col = gemeinde_col,
  title = "Eigentumsanteil vs. Ja-Anteil (Eigenbedarfs-Initiative)",
  subtitle_suffix = "Deutliche Abweichungen hervorgehoben",
  label_top_n = 15,
  seed = 42
)
print(res_eigenbedarf$plot)

out_eigenbedarf <- res_eigenbedarf$data %>%
  filter(is_deviant) %>%
  transmute(
    Gemeinde = .data[[gemeinde_col]],
    !!x_col := .data[[x_col]],
    !!y_eigen_col := .data[[y_eigen_col]],
    Residuum = resid, r_student = rstud, CookD = cook, Hebel = hat, Abweichung = dev_dir
  ) %>%
  arrange(desc(abs(r_student)))
cat("\nOutlier-Gemeinden (Eigenbedarfs-Initiative):\n"); print(out_eigenbedarf, n = 30)

cat("\nDiagnostik (Eigenbedarfs-Initiative):\n")
cat("n =", nobs(res_eigenbedarf$model),
    "| Mean(|Residuen|) =", round(mean(abs(res_eigenbedarf$data$resid), na.rm = TRUE), 2),
    "| SD(Residuen) =", round(sd(res_eigenbedarf$data$resid, na.rm = TRUE), 2),
    "| # |r_student|>2 =", sum(abs(res_eigenbedarf$data$rstud) > 2, na.rm = TRUE),
    "| # CookD>4/n =", sum(res_eigenbedarf$data$cook > 4/nobs(res_eigenbedarf$model), na.rm = TRUE), "\n")
