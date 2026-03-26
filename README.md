# OBIS Dataset Candidates

A project to identify GBIF datasets that are marine-related but not yet part of OBIS (Ocean Biodiversity Information System).

## Overview

This project scans GBIF for datasets that should potentially be included in the OBIS network. It starts by analyzing existing OBIS datasets to understand common characteristics and keywords used in marine biodiversity datasets.

## OBIS Network

- **GBIF Network UUID**: `2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6`
- **Network URL**: https://www.gbif.org/network/2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6

## Project Structure

```
obis-dataset-candidates/
├── README.md                      # This file
├── analyze_obis_datasets.R        # Main analysis script
├── obis_datasets.csv              # Output: All OBIS datasets
├── word_frequencies.csv           # Output: Common words in descriptions
└── marine_keywords.csv            # Output: Marine-related keywords
```

## Prerequisites

Install required R packages:

```r
install.packages(c("httr", "jsonlite", "dplyr", "tidytext", "stringr"))
```

## Usage

### Step 1: Analyze OBIS Datasets

Run the main analysis script to fetch and analyze all OBIS datasets:

```r
source("analyze_obis_datasets.R")
results <- main()
```

This will:
1. Fetch all datasets from the OBIS network via GBIF API
2. Extract and analyze commonly used words in titles and descriptions
3. Identify marine-related keywords
4. Generate three CSV files with the results

### Step 2: Identify Candidate Datasets

(Coming soon) Use the identified keywords to search GBIF for datasets that:
- Contain marine-related keywords in their descriptions
- Are NOT currently part of the OBIS network
- Have geographic coverage in marine areas

## Outputs

### obis_datasets.csv
Complete information about all OBIS datasets including:
- Dataset key (UUID)
- Title
- Description
- Publishing organization
- Type
- And more metadata

### word_frequencies.csv
Top 200 most frequently used words across all OBIS dataset descriptions and titles (excluding common stop words).

### marine_keywords.csv
Subset of words that are specifically marine-related with their frequencies.

## Next Steps

1. ✅ Analyze existing OBIS datasets for common keywords
2. 🔲 Search GBIF for datasets containing marine keywords
3. 🔲 Filter out datasets already in OBIS
4. 🔲 Rank candidates by relevance
5. 🔲 Generate report of potential OBIS candidates

## API References

- [GBIF Network API](https://www.gbif.org/developer/registry#networks)
- [GBIF Dataset Search API](https://www.gbif.org/developer/registry#datasets)
- [OBIS Homepage](https://obis.org/)

## Notes

- The script includes rate limiting (0.5s delay between requests) to be respectful of the GBIF API
- All API calls are made to the public GBIF API which doesn't require authentication
- Datasets are fetched in batches of 300 to handle pagination efficiently
