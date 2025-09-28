import pandas as pd

# Excel-Datei einlesen
df = pd.read_excel('data_stdzh.xlsx')

# Datenstruktur anzeigen
print("Spalten in der Datei:")
print(df.columns.tolist())
print("\nErste 5 Zeilen:")
print(df.head())
print("\nDatentypen:")
print(df.dtypes)
print("\nAnzahl Zeilen:", len(df))
