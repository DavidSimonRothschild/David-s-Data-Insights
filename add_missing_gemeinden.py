import pandas as pd

# CSV einlesen
df = pd.read_csv('merged_election_ownership.csv')

# Neue Gemeinden hinzufügen
new_rows = [
    {
        'BFS_NR': 231.0,
        'GEBIET_NAME': 'Zell (ZH)',
        'Bezirk': 'Bezirk Winterthur',
        'Eigentumsobjekte_Prozent': 56.8,
        'Ja_Prozent_Untermiete': None,  # Keine Daten vorhanden
        'Ja_Prozent_Eigenbedarf': None,  # Keine Daten vorhanden
        'Ja_Prozent_Eigenmietwert': None,  # Keine Daten vorhanden
        'Ja_Prozent_Eigenmietwert_EM': 71.91,
        'Untermiete_Angenommen': None,
        'Eigenbedarf_Angenommen': None
    },
    {
        'BFS_NR': 298.0,
        'GEBIET_NAME': 'Wiesendangen',
        'Bezirk': 'Bezirk Winterthur',
        'Eigentumsobjekte_Prozent': None,  # Fehlt laut User
        'Ja_Prozent_Untermiete': None,
        'Ja_Prozent_Eigenbedarf': None,
        'Ja_Prozent_Eigenmietwert': None,
        'Ja_Prozent_Eigenmietwert_EM': 74.17,
        'Untermiete_Angenommen': None,
        'Eigenbedarf_Angenommen': None
    },
    {
        'BFS_NR': 294.0,
        'GEBIET_NAME': 'Elgg',
        'Bezirk': 'Bezirk Winterthur',
        'Eigentumsobjekte_Prozent': 40.1,
        'Ja_Prozent_Untermiete': None,
        'Ja_Prozent_Eigenbedarf': None,
        'Ja_Prozent_Eigenmietwert': None,
        'Ja_Prozent_Eigenmietwert_EM': 68.40,
        'Untermiete_Angenommen': None,
        'Eigenbedarf_Angenommen': None
    }
]

# Neue Zeilen hinzufügen
df_new = pd.concat([df, pd.DataFrame(new_rows)], ignore_index=True)

# Nach BFS_NR sortieren
df_new = df_new.sort_values('BFS_NR')

# Speichern
df_new.to_csv('merged_election_ownership.csv', index=False)

print(f"CSV aktualisiert: {len(df)} -> {len(df_new)} Zeilen")
print("\nHinzugefügte Gemeinden:")
for row in new_rows:
    print(f"- {row['GEBIET_NAME']}: {row['Ja_Prozent_Eigenmietwert_EM']}% Ja-Anteil, {row.get('Eigentumsobjekte_Prozent', 'N/A')}% Eigentum")
