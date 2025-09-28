import pandas as pd

# Excel-Datei einlesen
df = pd.read_excel('data_stdzh.xlsx')

print("=== VOLLSTÄNDIGE DATEN ===")
print(df.to_string())

print("\n=== DATENANALYSE ===")
print(f"Anzahl Zeilen: {len(df)}")
print(f"Anzahl Zeilen mit Durchschnitt='Ja': {len(df[df['Durchschnitt'] == 'Ja'])}")
print(f"Anzahl Zeilen mit Durchschnitt='Nein': {len(df[df['Durchschnitt'] == 'Nein'])}")
print(f"Anzahl Zeilen ohne Durchschnitt-Markierung: {len(df[df['Durchschnitt'].isna()])}")

print("\n=== STADTKREISE (ohne Durchschnitt) ===")
stadtkreise = df[df['Durchschnitt'].isna()]
print(stadtkreise[['GEBIET_NAME', 'Eigentumsobjekte_Prozent', 'Ja_Prozent_Eigenmietwert', 'Ja_Prozent_Untermiete', 'Ja_Prozent_Eigenbedarf']])

# CSV für einfacheres Arbeiten speichern
stadtkreise.to_csv('stadtzh_stadtkreise.csv', index=False)
print("\nDaten als 'stadtzh_stadtkreise.csv' gespeichert")
