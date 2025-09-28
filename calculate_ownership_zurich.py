import pandas as pd

# Daten einlesen
df = pd.read_excel('data_stdzh.xlsx')

# Einwohnerzahl Stadt Zürich (Stand 2024)
einwohner_stadt_zh = 443037  # Quelle: Stadt Zürich Statistik

# Durchschnittliche Eigentumsquote berechnen
# Gewichtet nach Einwohnerzahl der Stadtkreise (geschätzt)
# Kreise 7&8 und 11 haben mehr Einwohner, Kreis 12 hat die meisten

# Geschätzte Einwohnerverteilung (basierend auf Stadtstatistik)
einwohner_verteilung = {
    'Zürichrich Kreise 1 und 2': 10000,  # Altstadt, kleine Kreise
    'Zürich, Zürich Kreis 3': 52000,     # Wiedikon
    'Zürich, Zürich Kreise 4 und 5': 40000,  # Aussersihl, Industriequartier
    'Zürich, Zürich Kreis 6': 31000,     # Unterstrass, Oberstrass
    'Zürich, Zürich Kreise 7 und 8': 70000,  # Fluntern, Riesbach, Seefeld
    'Zürich, Zürich Kreis 9': 45000,     # Altstetten, Albisrieden
    'Zürich, Zürich Kreis 10': 37000,    # Höngg, Wipkingen
    'Zürich, Zürich Kreis 11': 55000,    # Affoltern, Oerlikon, Seebach
    'Zürich, Zürich Kreis 12': 103000    # Schwamendingen
}

# Eigentumsquoten aus den Daten
for index, row in df.iterrows():
    gebiet = row['GEBIET_NAME']
    eigentum = row['Eigentumsobjekte_Prozent']
    if gebiet in einwohner_verteilung:
        einwohner = einwohner_verteilung[gebiet]
        eigentümer = einwohner * (eigentum / 100)
        print(f"{gebiet}: {eigentum:.1f}% = ca. {int(eigentümer):,} Personen")

# Gesamtberechnung
total_eigentümer = 0
total_einwohner = sum(einwohner_verteilung.values())

for index, row in df.iterrows():
    gebiet = row['GEBIET_NAME']
    eigentum = row['Eigentumsobjekte_Prozent']
    if gebiet in einwohner_verteilung:
        einwohner = einwohner_verteilung[gebiet]
        eigentümer = einwohner * (eigentum / 100)
        total_eigentümer += eigentümer

gewichtete_quote = (total_eigentümer / total_einwohner) * 100

print(f"\n=== ZUSAMMENFASSUNG STADT ZÜRICH ===")
print(f"Geschätzte Einwohnerzahl: {total_einwohner:,}")
print(f"Geschätzte Anzahl Wohneigentümer: {int(total_eigentümer):,}")
print(f"Gewichtete Eigentumsquote: {gewichtete_quote:.1f}%")

# Mit offizieller Einwohnerzahl
eigentümer_offiziell = einwohner_stadt_zh * (gewichtete_quote / 100)
print(f"\nMit offizieller Einwohnerzahl ({einwohner_stadt_zh:,}):")
print(f"Geschätzte Wohneigentümer: {int(eigentümer_offiziell):,} Personen")

# Haushalte statt Personen (durchschnittlich 1.9 Personen pro Haushalt in Zürich)
personen_pro_haushalt = 1.9
haushalte_eigentum = eigentümer_offiziell / personen_pro_haushalt
print(f"\nBei {personen_pro_haushalt} Personen pro Haushalt:")
print(f"Ca. {int(haushalte_eigentum):,} Haushalte mit Wohneigentum")
