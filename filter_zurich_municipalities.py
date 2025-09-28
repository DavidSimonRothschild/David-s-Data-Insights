#!/usr/bin/env python3
"""
Filter CSV data for municipalities in Kanton Zürich and save as Excel file.
"""

import csv
import pandas as pd

def filter_zurich_municipalities():
    """
    Filter the CSV file for Kanton Zürich municipalities and save as Excel.
    BFS numbers for Zürich canton municipalities range from 1 to 296.
    """
    
    input_file = '/Users/davidrothschild/Downloads/gpzuerich-2/data_1476706.csv'
    output_file = '/Users/davidrothschild/Downloads/gpzuerich-2/zurich_municipalities_data.xlsx'
    
    print("Reading CSV file...")
    
    # Read the CSV file with semicolon separator
    df = pd.read_csv(input_file, sep=';', encoding='utf-8')
    
    print(f"Original data shape: {df.shape}")
    print(f"Columns: {list(df.columns)}")
    
    # Display BFS_NR range to understand the data better
    print(f"BFS_NR range: {df['BFS_NR'].min()} - {df['BFS_NR'].max()}")
    
    # Filter for Kanton Zürich municipalities
    # BFS numbers 1-296 are for Kanton Zürich
    zurich_df = df[(df['BFS_NR'] >= 1) & (df['BFS_NR'] <= 296)]
    
    print(f"Filtered data shape (Zürich municipalities): {zurich_df.shape}")
    print(f"Number of unique municipalities: {zurich_df['GEBIET_NAME'].nunique()}")
    
    # Display some sample municipality names
    print(f"\nSample municipalities:")
    sample_municipalities = zurich_df['GEBIET_NAME'].unique()[:10]
    for municipality in sample_municipalities:
        print(f"  - {municipality}")
    
    # Save as Excel file
    print(f"\nSaving to Excel file: {output_file}")
    zurich_df.to_excel(output_file, index=False, engine='openpyxl')
    
    print(f"Successfully saved {len(zurich_df)} records to Excel file.")
    
    return zurich_df

if __name__ == "__main__":
    filtered_data = filter_zurich_municipalities()
