#!/usr/bin/env python3
"""
Filter voting data for all Zürich municipalities and districts and save as Excel file.
"""

import pandas as pd

def filter_zurich_voting():
    """
    Filter the voting CSV file for all Zürich entries (canton, districts, municipalities) 
    and save as Excel.
    """
    
    input_file = '/Users/davidrothschild/Downloads/gpzuerich-2/abstimmung_20250928.csv'
    output_file = '/Users/davidrothschild/Downloads/gpzuerich-2/abstimmung_zuerich_20250928.xlsx'
    
    print("Reading voting data CSV file...")
    
    # Read the CSV file with semicolon separator
    df = pd.read_csv(input_file, sep=';', encoding='utf-8')
    
    print(f"Original data shape: {df.shape}")
    print(f"Columns: {list(df.columns)}")
    
    # Display unique region types
    print(f"Region types: {df['region_type'].unique()}")
    print(f"Canton names: {df['parent_canton_name'].unique()}")
    
    # Filter for all Zürich entries (parent_canton_id=1 or canton=Zürich)
    zurich_df = df[
        (df['parent_canton_id'] == 1) | 
        ((df['region_type'] == 'canton') & (df['region_name'] == 'Zürich'))
    ]
    
    print(f"Filtered data shape (Zürich entries): {zurich_df.shape}")
    
    # Display breakdown by region type
    region_counts = zurich_df['region_type'].value_counts()
    print(f"\nBreakdown by region type:")
    for region_type, count in region_counts.items():
        print(f"  {region_type}: {count} entries")
    
    # Display some example entries
    print(f"\nSample Zürich entries:")
    sample_entries = zurich_df[['region_type', 'region_name', 'yes_votes_count', 'no_votes_count', 'cast_votes_count']].head(10)
    for _, row in sample_entries.iterrows():
        print(f"  {row['region_type']:12} | {row['region_name']:20} | Ja: {row['yes_votes_count']:6} | Nein: {row['no_votes_count']:6} | Total: {row['cast_votes_count']:6}")
    
    # Calculate Yes percentage for analysis
    zurich_df = zurich_df.copy()
    zurich_df['yes_percentage'] = (zurich_df['yes_votes_count'] / zurich_df['cast_votes_count'] * 100).round(2)
    zurich_df['turnout_percentage'] = (zurich_df['cast_votes_count'] / zurich_df['eligible_voters_count'] * 100).round(2)
    
    # Sort by region type and then by region name
    zurich_df = zurich_df.sort_values(['region_type', 'region_name'])
    
    print(f"\nSaving to Excel file: {output_file}")
    zurich_df.to_excel(output_file, index=False, engine='openpyxl')
    
    # Summary statistics
    print(f"\nSummary for Kanton Zürich voting results:")
    print(f"Total entries: {len(zurich_df)}")
    print(f"Total cast votes (all regions): {zurich_df['cast_votes_count'].sum():,}")
    print(f"Average Yes percentage: {zurich_df['yes_percentage'].mean():.1f}%")
    print(f"Average turnout: {zurich_df['turnout_percentage'].mean():.1f}%")
    
    return zurich_df

if __name__ == "__main__":
    filtered_data = filter_zurich_voting()
