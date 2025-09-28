#!/usr/bin/env python3
"""
Merge ZH_EB_UM.xlsx and ZH_EM.xlsx on BFS number
to combine election data with property ownership percentages
"""

import pandas as pd
import numpy as np

# Read the first file (contains property ownership data and voting results)
print("Reading ZH_EB_UM.xlsx...")
df_eb_um = pd.read_excel('ZH_EB_UM.xlsx')
print(f"ZH_EB_UM shape: {df_eb_um.shape}")
print(f"Columns: {df_eb_um.columns.tolist()}\n")

# Read the second file (contains Eigenmietwert voting results)
print("Reading ZH_EM.xlsx...")
df_em = pd.read_excel('ZH_EM.xlsx')

# Remove the first row if it contains NaN values (header row issue)
if df_em.iloc[0].isna().all():
    df_em = df_em.iloc[1:].reset_index(drop=True)

print(f"ZH_EM shape: {df_em.shape}")
print(f"Columns: {df_em.columns.tolist()}\n")

# Convert BFS column to numeric in df_em to match df_eb_um
df_em['BFS'] = pd.to_numeric(df_em['BFS'], errors='coerce')

# Rename columns in df_em for clarity
df_em = df_em.rename(columns={
    'BFS': 'BFS_NR',
    'Ja %': 'Ja_Prozent_Eigenmietwert_EM'
})

# Select relevant columns from df_em
df_em_selected = df_em[['BFS_NR', 'Bezirk', 'Ja_Prozent_Eigenmietwert_EM']]

# Merge the dataframes on BFS_NR
print("Merging dataframes on BFS_NR...")
merged_df = pd.merge(
    df_eb_um,
    df_em_selected,
    on='BFS_NR',
    how='outer',
    indicator=True
)

# Check merge results
print("\nMerge results:")
print(merged_df['_merge'].value_counts())

# Remove the merge indicator column
merged_df = merged_df.drop('_merge', axis=1)

# Sort by BFS_NR
merged_df = merged_df.sort_values('BFS_NR')

# Reorder columns for better readability
column_order = [
    'BFS_NR', 
    'GEBIET_NAME', 
    'Bezirk',
    'Eigentumsobjekte_Prozent',
    'Ja_Prozent_Untermiete', 
    'Ja_Prozent_Eigenbedarf', 
    'Ja_Prozent_Eigenmietwert',
    'Ja_Prozent_Eigenmietwert_EM',
    'Untermiete_Angenommen', 
    'Eigenbedarf_Angenommen'
]

# Only include columns that exist
column_order = [col for col in column_order if col in merged_df.columns]
merged_df = merged_df[column_order]

# Display summary statistics
print("\nMerged dataframe summary:")
print(f"Total rows: {len(merged_df)}")
print(f"Columns: {merged_df.columns.tolist()}")
print("\nFirst 10 rows:")
print(merged_df.head(10))

# Check for any missing BFS numbers
missing_bfs = merged_df[merged_df['BFS_NR'].isna()]
if not missing_bfs.empty:
    print(f"\nWarning: {len(missing_bfs)} rows with missing BFS_NR")

# Save to Excel
output_file = 'merged_election_ownership.xlsx'
print(f"\nSaving merged data to {output_file}...")
merged_df.to_excel(output_file, index=False, engine='openpyxl')

# Also save as CSV for easier viewing
csv_file = 'merged_election_ownership.csv'
merged_df.to_csv(csv_file, index=False)
print(f"Also saved as CSV: {csv_file}")

# Print summary statistics for key columns
print("\nSummary statistics for key columns:")
print("\nEigentumsobjekte_Prozent:")
print(merged_df['Eigentumsobjekte_Prozent'].describe())

print("\nJa_Prozent_Untermiete:")
print(merged_df['Ja_Prozent_Untermiete'].describe())

print("\nJa_Prozent_Eigenbedarf:")
print(merged_df['Ja_Prozent_Eigenbedarf'].describe())

print("\nJa_Prozent_Eigenmietwert (from ZH_EB_UM):")
print(merged_df['Ja_Prozent_Eigenmietwert'].describe())

print("\nJa_Prozent_Eigenmietwert_EM (from ZH_EM):")
print(merged_df['Ja_Prozent_Eigenmietwert_EM'].describe())

print("\nMerge completed successfully!")
