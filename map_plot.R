# =========================
# Pakete
# =========================
library(tidyverse)
library(sf)
library(readxl)
library(stringi)

# =========================
# USER: Pfade setzen (ANPASSEN)
# =========================
xlsx_path <- "abstimmung_eigentumsanteil_rohdaten_bereinigt.xlsx"
# Optionaler Hinweis (Datei ODER ZIP ODER Ordner). Kann auch "" bleiben – dann wird gesucht.
sb_hint   <- "swissBOUNDARIES3D_1_5_LV95_LN02.gpkg"

# Stadt Zürich Quartiere (WFS) – kein Download nötig
quartiere_wfs <- paste0(
  "https://www.ogd.stadt-zuerich.ch/wfs/geoportal/Statistische_Quartiere",
  "?service=WFS&version=1.1.0&request=GetFeature",
  "&typename=adm_statistische_quartiere_map&outputFormat=GeoJSON"
)

# =========================
# Helpers
# =========================
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

numify <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub("%", "", x)
  x <- gsub("\\s+", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

normalize_str <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    trimws() %>%
    gsub("[-’'`´]", " ", ., perl = TRUE) %>%
    gsub("\\s+", " ", .)
}

pick_first_col <- function(df, patterns) {
  for (p in patterns) {
    hit <- names(df)[grepl(p, names(df), ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

# --- swissBOUNDARIES3D finden/entpacken (GPKG bevorzugt, Fallback SHP) ---
find_sb3d <- function(hint = NULL) {
  # 1) Direkter Treffer?
  if (!is.null(hint) && nzchar(hint)) {
    if (file.exists(hint)) return(normalizePath(hint, mustWork = TRUE))
    # Falls ZIP übergeben → entpacken nebenan
    if (grepl("\\.zip$", hint, ignore.case = TRUE) && file.exists(hint)) {
      message("Entpacke ZIP: ", hint)
      utils::unzip(hint, exdir = dirname(hint))
    }
  }
  # 2) GPKG direkt im WD/Unterordnern
  gpkg <- list.files(pattern = "swissbound.*2056_5728.*\\.gpkg$", ignore.case = TRUE, recursive = TRUE)
  if (length(gpkg) >= 1) return(normalizePath(gpkg[1], mustWork = TRUE))
  # 3) ZIP → entpacken → GPKG suchen
  zips <- list.files(pattern = "swissbound.*2056_5728.*\\.gpkg\\.zip$", ignore.case = TRUE, recursive = TRUE)
  if (length(zips) >= 1) {
    message("Entpacke ZIP: ", zips[1])
    utils::unzip(zips[1], exdir = dirname(zips[1]))
    gpkg <- list.files(pattern = "swissbound.*2056_5728.*\\.gpkg$", ignore.case = TRUE, recursive = TRUE)
    if (length(gpkg) >= 1) return(normalizePath(gpkg[1], mustWork = TRUE))
  }
  # 4) SHP-Verzeichnis finden (irgendein .shp vorhanden?)
  shp <- list.files(pattern = "swissbound.*2056_5728.*\\.shp$", ignore.case = TRUE, recursive = TRUE)
  if (length(shp) >= 1) return(normalizePath(dirname(shp[1]), mustWork = TRUE))  # Ordner zurückgeben
  stop("Keine swissBOUNDARIES3D-Datei gefunden. Bitte GPKG (oder SHP) in das Arbeitsverzeichnis legen/entpacken.")
}

# --- swissBOUNDARIES3D laden (GPKG oder SHP-Ordner) ---
load_sb3d_any <- function(path_or_dir) {
  find_first <- function(items, patterns) {
    for (pat in patterns) {
      hit <- items[grepl(pat, items, ignore.case = TRUE)]
      if (length(hit) > 0) return(hit[1])
    }
    NA_character_
  }
  if (dir.exists(path_or_dir)) {
    # SHP-Ordner: passende Layer-Dateien finden
    muni_shp <- list.files(path_or_dir, pattern = "(GEMEINDE|MUNICIPAL|COMMUNE|HOHEITSGEBIET).*\\.shp$", ignore.case = TRUE, full.names = TRUE)
    kant_shp <- list.files(path_or_dir, pattern = "(KANTON|CANTON).*\\.shp$",    ignore.case = TRUE, full.names = TRUE)
    if (length(muni_shp) < 1 || length(kant_shp) < 1)
      stop("Im SHP-Ordner wurden Gemeinde/Kanton-SHPs nicht gefunden.")
    list(
      muni = st_read(muni_shp[1], quiet = TRUE),
      kant = st_read(kant_shp[1], quiet = TRUE)
    )
  } else {
    # GPKG-Datei
    if (!file.exists(path_or_dir)) stop("GPKG nicht gefunden: ", path_or_dir)
    lays <- sf::st_layers(path_or_dir)$name
    muni_layer <- find_first(lays, c("GEMEINDE", "COMMUNE", "MUNICIPAL", "HOHEITSGEBIET"))
    kant_layer <- find_first(lays, c("KANTON", "CANTON"))
    if (is.na(muni_layer) || is.na(kant_layer)) {
      stop("Gemeinde-/Kanton-Layer im GPKG nicht gefunden. Layers: ", paste(lays, collapse = ", "))
    }
    list(
      muni = st_read(path_or_dir, layer = muni_layer, quiet = TRUE),
      kant = st_read(path_or_dir, layer = kant_layer, quiet = TRUE)
    )
  }
}

# Modell + Residuen (Ja ~ Eigentum) + Dev-Kategorien
fit_residuals <- function(df, x_col, y_col) {
  d <- df %>% filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]))
  stopifnot(nrow(d) >= 5)
  m <- lm(reformulate(x_col, y_col), data = d)
  n <- nobs(m)
  pi_df <- as.data.frame(predict(m, newdata = d, interval = "prediction", level = 0.95))
  d <- d %>%
    mutate(.fit = pi_df$fit, .lwr = pi_df$lwr, .upr = pi_df$upr,
           .res = resid(m), .rstu = rstudent(m),
           .cook = cooks.distance(m), .hat = hatvalues(m),
           .outside = (.data[[y_col]] < .lwr) | (.data[[y_col]] > .upr),
           .big_r = abs(.rstu) > 2, .big_c = .cook > (4/n),
           .dev = .outside | .big_r | .big_c,
           dev_dir = case_when(
             .dev & .res > 0 ~ "deutlich höher als erwartet",
             .dev & .res < 0 ~ "deutlich tiefer als erwartet",
             TRUE ~ "im Erwartungsband"
           ))
  list(data = d, model = m)
}

# Farbskalen
scale_fill_resid <- function() {
  scale_fill_gradient2(name = "Residuum (pp)",
                       low = "#0072B2", mid = "grey90", high = "#D55E00",
                       midpoint = 0, na.value = "grey85")
}
scale_fill_devcat <- function() {
  scale_fill_manual(values = c(
    "deutlich höher als erwartet" = "#D55E00",
    "deutlich tiefer als erwartet" = "#0072B2",
    "im Erwartungsband"          = "#7F8C8D"
  ), na.value = "grey85", name = "Abweichung")
}

# =========================
# 1) Excel laden & Spalten finden
# =========================
sheets <- excel_sheets(xlsx_path)
sheet_name <- if (any(grepl("gemeinde", sheets, ignore.case = TRUE))) {
  sheets[grepl("gemeinde", sheets, ignore.case = TRUE)][1]
} else if (any(grepl("kant", sheets, ignore.case = TRUE))) {
  sheets[grepl("kant", sheets, ignore.case = TRUE)][1]
} else sheets[1]
cat("Gewähltes Sheet:", sheet_name, "\n")

df <- read_excel(xlsx_path, sheet = sheet_name)

name_col <- pick_first_col(df, c("gemeinde","quartier","kreis","kanton","name","ort","bezirk"))
x_col    <- pick_first_col(df, c("^eigentums.*pro","eigentum.*(anteil|proz|%)","eigentum"))
y_col    <- pick_first_col(df, c("^ja.*untermiet","untermiet"))
if (is.na(name_col)) { df <- df %>% mutate(Index = row_number()); name_col <- "Index" }
stopifnot(!is.na(x_col), !is.na(y_col))

df <- df %>% mutate(
  !!x_col := numify(.data[[x_col]]),
  !!y_col := numify(.data[[y_col]])
)

mod <- fit_residuals(df, x_col, y_col)
df_res <- mod$data
cat("R²:", round(summary(mod$model)$r.squared, 3),
    " | β(Eigentum) p =", signif(summary(mod$model)$coefficients[2,4], 3), "\n")

# =========================
# 2) swissBOUNDARIES3D laden (robust)
# =========================

# 2a) Falls du den genauen Pfad kennst: HIER EINTRAGEN (entweder .gpkg ODER .gpkg.zip ODER SHP-Ordner)
# Beispiele:
# sb_hint <- "/Users/deinname/Downloads/swissboundaries3d_2025-04_2056_5728.gpkg"
# sb_hint <- "C:/Users/deinname/Downloads/swissboundaries3d_2025-04_2056_5728.gpkg"
# sb_hint <- "/Users/deinname/Downloads/swissboundaries3d_2025-04_2056_5728.gpkg.zip"
# sb_hint <- "/Users/deinname/Downloads/swissboundaries3d_2025-04_2056_5728_shp"  # Ordner mit SHP-Dateien
# Wenn leer, versuchen wir Downloads-Ordner & Dateiauswahl.
if (!exists("sb_hint")) sb_hint <- ""

pick_sb_path <- function(hint = "") {
  # 0) Bereits vorhandene Datei/Ordner?
  if (nzchar(hint) && (file.exists(hint) || dir.exists(hint))) return(normalizePath(hint, mustWork = FALSE))
  
  # 1) Häufiger Ort: Downloads
  dl <- path.expand("~/Downloads")
  candidates <- c(
    list.files(dl, pattern = "swissbound.*2056_5728.*\\.gpkg$", ignore.case = TRUE, full.names = TRUE),
    list.files(dl, pattern = "swissbound.*2056_5728.*\\.gpkg\\.zip$", ignore.case = TRUE, full.names = TRUE),
    list.dirs(dl, recursive = FALSE, full.names = TRUE)
  )
  cand_gpkg <- candidates[grepl("\\.gpkg$", candidates, ignore.case = TRUE)]
  if (length(cand_gpkg) >= 1) return(cand_gpkg[1])
  
  cand_zip <- candidates[grepl("\\.gpkg\\.zip$", candidates, ignore.case = TRUE)]
  if (length(cand_zip) >= 1) return(cand_zip[1])
  
  # 2) Letzter Versuch: interaktive Dateiauswahl (nur in RStudio/R GUI)
  if (interactive()) {
    message("Bitte wähle das swissBOUNDARIES3D GeoPackage (.gpkg) ODER das ZIP aus.")
    p <- tryCatch(file.choose(new = FALSE), error = function(e) "")
    if (nzchar(p)) return(p)
  }
  
  stop("Keine swissBOUNDARIES3D-Datei gefunden. Setze 'sb_hint' auf die .gpkg (oder .gpkg.zip) bzw. SHP-Ordner.")
}

sb_path0 <- pick_sb_path(sb_hint)

# Falls ZIP: entpacken und .gpkg verwenden
if (grepl("\\.zip$", sb_path0, ignore.case = TRUE)) {
  message("Entpacke ZIP: ", sb_path0)
  utils::unzip(sb_path0, exdir = dirname(sb_path0))
  # Suche entpackte .gpkg im selben Ordner
  gpkg_after <- list.files(dirname(sb_path0), pattern = "swissbound.*2056_5728.*\\.gpkg$", ignore.case = TRUE, full.names = TRUE)
  if (length(gpkg_after) < 1) stop("ZIP entpackt, aber keine .gpkg gefunden. Prüfe den Inhalt der ZIP.")
  sb_path <- gpkg_after[1]
} else {
  sb_path <- sb_path0
}

cat("Gefunden:", sb_path, "\n")

# Teste Layers (nur für Diagnose)
if (!dir.exists(sb_path)) print(sf::st_layers(sb_path))

sb <- load_sb3d_any(sb_path)

# Gemeinden Kanton Zürich verbinden
muni <- sb$muni
if (!"kantonsnummer" %in% names(muni)) {
  stop("Erwartete Spalte 'kantonsnummer' im Gemeinde-Layer nicht gefunden. Verfügbare Spalten: ", paste(names(muni), collapse = ", "))
}

muni_zh <- muni %>%
  mutate(kantonsnummer = as.numeric(kantonsnummer)) %>%
  filter(kantonsnummer == 1)

if ("BFS_NR" %in% names(df_res) && "bfs_nummer" %in% names(muni_zh)) {
  joined_zh <- muni_zh %>%
    mutate(BFS_NR = as.numeric(bfs_nummer)) %>%
    left_join(df_res, by = "BFS_NR")
} else {
  message("BFS-Abgleich nicht möglich – fallback auf Namensabgleich.")
  joined_zh <- muni_zh %>%
    mutate(.GEO_KEY = normalize_str(name)) %>%
    left_join(
      df_res %>% mutate(.DATA_KEY = normalize_str(.data[[name_col]])),
      by = join_by(.GEO_KEY == .DATA_KEY)
    )
}

miss_zh <- sum(is.na(joined_zh$.res))
if (miss_zh > 0) {
  message("Hinweis (Kanton): ", miss_zh, " Gemeinden ohne Match – prüfe BFS-Nummern bzw. Namen in Excel vs. swissBOUNDARIES.")
  print(
    joined_zh %>%
      filter(is.na(.res)) %>%
      st_drop_geometry() %>%
      select(BFS_NR, name) %>%
      arrange(BFS_NR)
  )
}

joined_zh_matched <- joined_zh %>% filter(!is.na(.res))
if (nrow(joined_zh_matched) == 0) {
  stop("Keine Gemeinde konnte gematcht werden – bitte Schlüssel überprüfen.")
}

# =========================
# 3) Karten (Kanton Zürich – Gemeinden)
# =========================
p_zh_res <- ggplot(joined_zh_matched) +
  geom_sf(aes(fill = .res), color = "white", size = 0.1) +
  scale_fill_resid() +
  labs(title = "Abweichung vom Erwartungswert - Kanton Zürich (Gemeinden)",
       subtitle = "Residuum = tatsächlicher Ja-Anteil - vorhergesagter Ja-Anteil (gegeben Eigentumsanteil)",
       caption = paste0("Modell: Ja ~ Eigentum | R²=", round(summary(mod$model)$r.squared, 3))) +
  theme_minimal()

p_zh_cat <- ggplot(joined_zh_matched) +
  geom_sf(aes(fill = dev_dir), color = "white", size = 0.1) +
  scale_fill_devcat() +
  labs(title = "Deutliche Abweichungen - Kanton Zürich (Gemeinden)",
       subtitle = "Kriterien: ausserhalb 95%-PI, |r_student|>2, Cook's D>4/n") +
  theme_minimal()

print(p_zh_res); print(p_zh_cat)

# =========================
# 4) Stadt Zürich – Quartiere (WFS, optional)
# =========================
quart <- tryCatch(st_read(quartiere_wfs, quiet = TRUE), error = function(e) NULL)
if (!is.null(quart)) {
  q_name_col <- pick_first_col(quart, c("QNAME","QUARTIER","NAME","ANAME","BEZ"))
  joined_city <- quart %>%
    mutate(.GEO_KEY = normalize_str(.data[[q_name_col]])) %>%
    left_join(df_res %>% mutate(.DATA_KEY = normalize_str(.data[[name_col]])),
              by = join_by(.GEO_KEY == .DATA_KEY))
  
  miss_c <- sum(is.na(joined_city$.res))
  if (miss_c > 0) message("Hinweis (Stadt): ", miss_c, " Quartiere ohne Match – prüfe Bezeichner in Excel vs. WFS.")
  
  joined_city_matched <- joined_city %>% filter(!is.na(.res))
  if (nrow(joined_city_matched) == 0) {
    message("Stadt-Karten übersprungen (keine Quartiere gematcht).")
  } else {
    p_city_res <- ggplot(joined_city_matched) +
      geom_sf(aes(fill = .res), color = "white", size = 0.15) +
      scale_fill_resid() +
      labs(title = "Abweichung vom Erwartungswert - Stadt Zürich (Statistische Quartiere)",
           subtitle = "Residuum = tatsächlicher Ja-Anteil - vorhergesagter Ja-Anteil (gegeben Eigentumsanteil)",
           caption = paste0("Modell: Ja ~ Eigentum | R²=", round(summary(mod$model)$r.squared, 3))) +
      theme_minimal()

    p_city_cat <- ggplot(joined_city_matched) +
      geom_sf(aes(fill = dev_dir), color = "white", size = 0.15) +
      scale_fill_devcat() +
      labs(title = "Deutliche Abweichungen - Stadt Zürich (Statistische Quartiere)",
           subtitle = "Kriterien: ausserhalb 95%-PI, |r_student|>2, Cook's D>4/n") +
      theme_minimal()

    print(p_city_res); print(p_city_cat)
  }
} else {
  message("Quartier-WFS nicht erreichbar – Stadt-Karten übersprungen.")
}
