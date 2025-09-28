#!/usr/bin/env python3
"""
Merge voting data with property ownership data based on BFS_NR.
"""

import pandas as pd
import numpy as np

def merge_voting_ownership():
    """
    Merge the Zürich voting data with property ownership data on BFS_NR.
    """
    
    voting_file = '/Users/davidrothschild/Downloads/gpzuerich-2/abstimmung_zuerich_20250928.xlsx'
    ownership_file = '/Users/davidrothschild/Downloads/gpzuerich-2/abstimmung_eigentumsanteil_rohdaten_bereinigt.xlsx'
    output_file = '/Users/davidrothschild/Downloads/gpzuerich-2/merged_voting_ownership.xlsx'
    
    print("Reading voting data...")
    voting_df = pd.read_excel(voting_file, engine='openpyxl')
    print(f"Voting data shape: {voting_df.shape}")
    print(f"Voting columns: {list(voting_df.columns)}")
    
    print("\nReading property ownership data...")
    ownership_df = pd.read_excel(ownership_file, engine='openpyxl')
    print(f"Ownership data shape: {ownership_df.shape}")
    print(f"Ownership columns: {list(ownership_df.columns)}")
    
    # Check if BFS_NR exists in ownership data
    if 'BFS_NR' in ownership_df.columns:
        print(f"\nBFS_NR found in ownership data")
        print(f"Unique BFS_NR in ownership: {ownership_df['BFS_NR'].nunique()}")
    else:
        # Try to find the correct column name
        print("\nBFS_NR not found directly. Checking column names...")
        for col in ownership_df.columns:
            print(f"  - {col}")
            if 'BFS' in col.upper() or 'NR' in col.upper():
                print(f"    -> Potential match: {col}")
    
    # Filter voting data for municipalities only (they have BFS numbers)
    municipalities_df = voting_df[voting_df['region_type'] == 'municipality'].copy()
    print(f"\nFiltered to municipalities only: {municipalities_df.shape}")
    
    # Check for duplicates in voting data
    dup_bfs = municipalities_df[municipalities_df.duplicated(subset=['BFS_NR'], keep=False)]
    if len(dup_bfs) > 0:
        print(f"Found {len(dup_bfs)} duplicate BFS_NR entries in voting data")
        print("Removing duplicates, keeping first occurrence...")
        municipalities_df = municipalities_df.drop_duplicates(subset=['BFS_NR'], keep='first')
        print(f"After removing duplicates: {municipalities_df.shape}")
    
    # Check BFS_NR in voting data
    print(f"Sample BFS_NR from voting data: {municipalities_df['BFS_NR'].head(10).tolist()}")
    
    # Perform the merge
    print("\nPerforming merge on BFS_NR...")
    
    # First, let's check what columns exist in ownership data
    if 'BFS_NR' in ownership_df.columns:
        merge_col = 'BFS_NR'
    elif 'BFS-Nr' in ownership_df.columns:
        merge_col = 'BFS-Nr'
        ownership_df['BFS_NR'] = ownership_df[merge_col]
    elif 'BFS Nr' in ownership_df.columns:
        merge_col = 'BFS Nr'
        ownership_df['BFS_NR'] = ownership_df[merge_col]
    else:
        # Try to find the first column (often BFS_NR)
        first_col = ownership_df.columns[0]
        print(f"Using first column as BFS_NR: {first_col}")
        ownership_df['BFS_NR'] = ownership_df[first_col]
    
    # Convert BFS_NR to numeric in both dataframes
    municipalities_df['BFS_NR'] = pd.to_numeric(municipalities_df['BFS_NR'], errors='coerce')
    ownership_df['BFS_NR'] = pd.to_numeric(ownership_df['BFS_NR'], errors='coerce')
    
    # Check for duplicates in ownership data
    dup_ownership = ownership_df[ownership_df.duplicated(subset=['BFS_NR'], keep=False)]
    if len(dup_ownership) > 0:
        print(f"Found {len(dup_ownership)} duplicate BFS_NR entries in ownership data")
        print("Removing duplicates, keeping first occurrence...")
        ownership_df = ownership_df.drop_duplicates(subset=['BFS_NR'], keep='first')
        print(f"After removing duplicates in ownership: {ownership_df.shape}")
    
    # Perform the merge
    merged_df = pd.merge(
        municipalities_df,
        ownership_df,
        on='BFS_NR',
        how='inner',  # Changed to inner join to only keep matched records
        indicator=True
    )
    
    print(f"\nMerge results:")
    print(f"Total rows after merge: {len(merged_df)}")
    print(f"Merge indicator counts:")
    print(merged_df['_merge'].value_counts())
    
    # Check for successful matches
    both_matched = merged_df[merged_df['_merge'] == 'both']
    print(f"\nSuccessfully matched: {len(both_matched)} municipalities")
    
    # Remove the merge indicator column for the final output
    merged_df = merged_df.drop('_merge', axis=1)
    
    # Sort by BFS_NR
    merged_df = merged_df.sort_values('BFS_NR')
    
    # Save to Excel
    print(f"\nSaving merged data to: {output_file}")
    merged_df.to_excel(output_file, index=False, engine='openpyxl')
    
    # Display some statistics if ownership data has relevant columns
    if 'Eigentumsanteil' in merged_df.columns or 'INDIKATOR_VALUE' in merged_df.columns:
        ownership_col = 'Eigentumsanteil' if 'Eigentumsanteil' in merged_df.columns else 'INDIKATOR_VALUE'
        
        # Calculate correlation between ownership rate and yes percentage
        valid_data = both_matched.dropna(subset=['yes_percentage', ownership_col])
        if len(valid_data) > 0:
            correlation = valid_data['yes_percentage'].corr(valid_data[ownership_col])
            print(f"\nCorrelation between property ownership rate and Yes vote %: {correlation:.3f}")
            
            # Show some examples
            print(f"\nSample merged data:")
            sample_cols = ['region_name', 'yes_percentage', ownership_col, 'cast_votes_count']
            sample_cols = [col for col in sample_cols if col in valid_data.columns]
            print(valid_data[sample_cols].head(10).to_string())
    
    print(f"\nMerge completed successfully!")
    return merged_df

if __name__ == "__main__":
    merged_data = merge_voting_ownership()
