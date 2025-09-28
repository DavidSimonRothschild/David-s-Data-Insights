library(sf)

# Geodaten einlesen
gemeinden <- st_read("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg", layer = "TLM_HOHEITSGEBIET")

# Verfügbare Spalten anzeigen
cat("Verfügbare Spalten:\n")
print(names(gemeinden))

# Erste paar Zeilen anzeigen
cat("\nErste 3 Zeilen (ohne Geometrie):\n")
print(head(st_drop_geometry(gemeinden), 3))

# Nach Kanton Zürich filtern - verschiedene Möglichkeiten testen
cat("\nKanton Zürich Filteroptionen:\n")
if("KANTONSNUM" %in% names(gemeinden)) {
  zh_count <- sum(gemeinden$KANTONSNUM == 1, na.rm = TRUE)
  cat("KANTONSNUM == 1:", zh_count, "Gemeinden\n")
}

if("KANTONSNUMMER" %in% names(gemeinden)) {
  zh_count <- sum(gemeinden$KANTONSNUMMER == 1, na.rm = TRUE)
  cat("KANTONSNUMMER == 1:", zh_count, "Gemeinden\n")
}

if("NAME" %in% names(gemeinden)) {
  zh_gemeinden <- gemeinden[grepl("Zürich|Winterthur|Uster", gemeinden$NAME), ]
  cat("Gefundene Zürich-Gemeinden über NAME:", nrow(zh_gemeinden), "\n")
}
