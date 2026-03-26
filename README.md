# OBIS Dataset Candidates

A project to identify GBIF datasets that are marine-related but not yet part of OBIS (Ocean Biodiversity Information System).

## Overview

This project scans GBIF for datasets that should potentially be included in the OBIS network. It uses marine keywords to find candidates, calculates their WORMS (World Register of Marine Species) taxonomic coverage, and filters datasets with ≥70% WORMS coverage.

## Output Files

The filtered candidate datasets are saved to:

📁 **[exports/obis_candidates_filtered.tsv](exports/obis_candidates_filtered.tsv)** - Final list of GBIF datasets recommended for OBIS inclusion

Additional intermediate files:
- [data/gbif_candidates.tsv](data/gbif_candidates.tsv) - All marine keyword matches from GBIF
- [data/percentage_worms.tsv](data/percentage_worms.tsv) - WORMS coverage calculations for each dataset

## Selection Criteria

Datasets are included if they:
- Match marine-related keywords
- Are NOT already in the OBIS network
- Are NOT from PANGAEA publisher (`d5778510-eb28-11da-8629-b8a03c50a862`)
- Have ≥70% of species present in the WORMS taxonomy

## Automated Updates

The workflow runs automatically on the 1st of each month via GitHub Actions, updating the candidate list with newly published datasets.

## OBIS Network

- **GBIF Network UUID**: `2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6`
- **Network URL**: https://www.gbif.org/network/2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6

