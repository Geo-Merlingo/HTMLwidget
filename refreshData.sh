#!/bin/bash

# Set data directory
DATA_DIR="${GITHUB_WORKSPACE}/data"
mkdir -p "$DATA_DIR"

# Download URLs
URL="https://io.trap.nz/geo/trapnz-projects/wfs/fO5fOhkWbtVGWaEUlGttNMoFiZpNk_DDO4mFLayq0xM/275455?service=WFS&version=1.0.0&request=GetFeature&typeName=trapnz-projects:my-projects-trap-records&outputFormat=csv"
URL2="https://catchit.co.nz/apps/RqURK71e/WaitakereNetwork-Trap-Checks.csv"

# Download data
echo "Downloading data..."
wget -O "$DATA_DIR/trapnz_data.csv" "$URL"
wget -O "$DATA_DIR/LastYearCIT.csv" "$URL2"


# Run R script
echo "Running R script..."
Rscript script.R
Rscript totals.R

echo "Done at $(date)"
