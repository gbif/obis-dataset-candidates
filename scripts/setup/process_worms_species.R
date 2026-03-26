library(dplyr)
library(readr)

# Process WORMS matched chunks to extract GBIF usageKeys for accepted species
# Filters: rank=SPECIES, matchType=EXACT, status=ACCEPTED

cat("==========================================================\n")
cat("  Processing WORMS Matched Chunks for Species Keys\n")
cat("==========================================================\n\n")

# Find all matched chunk files
chunk_files <- list.files("../../WORMS/matched_chunks", 
                         pattern = "chunk_.*_matched\\.tsv$", 
                         full.names = TRUE)

cat(sprintf("Found %d chunk files to process\n\n", length(chunk_files)))

# Process all chunks
all_usage_keys <- c()

for (chunk_file in chunk_files) {
  cat(sprintf("Processing %s...\n", basename(chunk_file)))
  
  # Read chunk
  chunk <- read_tsv(chunk_file, show_col_types = FALSE)
  
  # Filter for SPECIES + EXACT + ACCEPTED
  species_keys <- chunk %>%
    filter(rank == "SPECIES",
           matchType == "EXACT",
           status == "ACCEPTED",
           !is.na(usageKey)) %>%
    pull(usageKey)
  
  cat(sprintf("  Found %d species records\n", length(species_keys)))
  
  all_usage_keys <- c(all_usage_keys, species_keys)
}

# Get unique keys
unique_keys <- unique(all_usage_keys)

cat(sprintf("\nTotal species records: %d\n", length(all_usage_keys)))
cat(sprintf("Unique species keys: %d\n", length(unique_keys)))

# Write to output file
output_file <- "../../data/worms-species.tsv"
write.table(data.frame(usageKey = unique_keys), 
            file = output_file,
            sep = "\t",
            quote = FALSE,
            row.names = FALSE)

cat(sprintf("\nSpecies keys saved to: %s\n", output_file))

# Show sample
cat("\nSample of extracted usageKeys:\n")
print(head(unique_keys, 10))
