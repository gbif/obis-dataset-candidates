#!/bin/bash

# OBIS Dataset Candidates - Complete Workflow
# This script runs all R scripts in the correct order to generate final OBIS candidate datasets

set -e  # Exit on error

echo "======================================================================"
echo "  OBIS Dataset Candidates Workflow"
echo "======================================================================"
echo ""

# Check if Rscript is available
if ! command -v Rscript &> /dev/null; then
    echo "Error: Rscript not found. Please install R."
    exit 1
fi

echo "Step 1/3: Finding GBIF candidate datasets..."
echo "----------------------------------------------------------------------"
cd scripts/workflow
Rscript find_obis_candidates.R
cd ../..
echo ""

echo "Step 2/3: Calculating WORMS coverage for each candidate..."
echo "----------------------------------------------------------------------"
cd scripts/workflow
Rscript calculate_worms_coverage.R
cd ../..
echo ""

echo "Step 3/3: Filtering candidates by WORMS coverage (>=70%)..."
echo "----------------------------------------------------------------------"
cd scripts/workflow
Rscript filter_by_worms_coverage.R
cd ../..
echo ""

echo "======================================================================"
echo "  Workflow Complete!"
echo "======================================================================"
echo ""
echo "Output files generated:"
echo "  - data/gbif_candidates.tsv"
echo "  - data/percentage_worms.tsv"
echo "  - exports/obis_candidates_filtered.tsv"
echo ""
echo "To view summary statistics, run:"
echo "  cd scripts/analysis && Rscript summarize_candidates.R"
echo ""
