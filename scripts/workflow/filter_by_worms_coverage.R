# Filter GBIF candidates by WORMS coverage
# Keep only datasets with at least 70% WORMS coverage

library(dplyr)
library(readr)

# Read the percentage WORMS file
percentage_worms <- read_tsv("../../data/percentage_worms.tsv", show_col_types = FALSE)

# Read the GBIF candidates file
gbif_candidates <- read_tsv("../../data/gbif_candidates.tsv", show_col_types = FALSE)

# Join the two datasets and filter by WORMS percentage >= 70%
filtered_candidates <- gbif_candidates %>%
  inner_join(percentage_worms, by = c("key" = "datasetKey")) %>%
  filter(worms_percentage >= 70) %>%
  # Reorder columns to show WORMS stats prominently
  select(key, title = title.x, publishingOrganizationKey, publishingOrganizationTitle, matched_keywords,
         total_species, worms_species, worms_percentage, total_occurrences)

# Display summary
cat("Original GBIF candidates:", nrow(gbif_candidates), "\n")
cat("Candidates with WORMS data:", nrow(percentage_worms), "\n")
cat("Candidates with >= 70% WORMS coverage:", nrow(filtered_candidates), "\n")
cat("\nWORMS coverage distribution of filtered candidates:\n")
print(summary(filtered_candidates$worms_percentage))

# Save the filtered candidates
write_tsv(filtered_candidates, "../../exports/obis_candidates_filtered.tsv")

cat("\nFiltered candidates saved to: ../../exports/obis_candidates_filtered.tsv\n")
