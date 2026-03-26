# Summarize GBIF Candidate Datasets
# Generate summary statistics to identify patterns

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# Read the filtered candidates
filtered_candidates <- read_tsv("gbif_candidates_filtered.tsv", show_col_types = FALSE)

cat("=" %>% rep(70) %>% paste0(collapse = ""), "\n")
cat("GBIF Dataset Candidates Summary Statistics\n")
cat("=" %>% rep(70) %>% paste0(collapse = ""), "\n\n")

# Overall summary
cat("OVERALL SUMMARY\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
cat("Total filtered datasets:", nrow(filtered_candidates), "\n")
cat("Total species across all datasets:", sum(filtered_candidates$total_species), "\n")
cat("Total occurrences across all datasets:", sum(filtered_candidates$total_occurrences), "\n")
cat("Total WORMS species:", sum(filtered_candidates$worms_species), "\n")
cat("\n")

# WORMS coverage statistics
cat("WORMS COVERAGE STATISTICS\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
print(summary(filtered_candidates$worms_percentage))
cat("\n")

# Species count statistics
cat("SPECIES COUNT STATISTICS\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
cat("Species per dataset:\n")
print(summary(filtered_candidates$total_species))
cat("\n")

# Occurrence count statistics
cat("OCCURRENCE COUNT STATISTICS\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
cat("Occurrences per dataset:\n")
print(summary(filtered_candidates$total_occurrences))
cat("\n")

# Top publishers by dataset count
cat("TOP 20 PUBLISHERS BY DATASET COUNT\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
publisher_counts <- filtered_candidates %>%
  group_by(publishingOrganizationTitle) %>%
  summarise(
    dataset_count = n(),
    total_species = sum(total_species),
    total_occurrences = sum(total_occurrences),
    avg_worms_pct = round(mean(worms_percentage), 1)
  ) %>%
  arrange(desc(dataset_count)) %>%
  head(20)

print(publisher_counts, n = 20)
cat("\n")

# Publishers with most species
cat("TOP 20 PUBLISHERS BY TOTAL SPECIES\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
publisher_species <- filtered_candidates %>%
  group_by(publishingOrganizationTitle) %>%
  summarise(
    dataset_count = n(),
    total_species = sum(total_species),
    total_occurrences = sum(total_occurrences),
    avg_worms_pct = round(mean(worms_percentage), 1)
  ) %>%
  arrange(desc(total_species)) %>%
  head(20)

print(publisher_species, n = 20)
cat("\n")

# Publishers with most occurrences
cat("TOP 20 PUBLISHERS BY TOTAL OCCURRENCES\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
publisher_occurrences <- filtered_candidates %>%
  group_by(publishingOrganizationTitle) %>%
  summarise(
    dataset_count = n(),
    total_species = sum(total_species),
    total_occurrences = sum(total_occurrences),
    avg_worms_pct = round(mean(worms_percentage), 1)
  ) %>%
  arrange(desc(total_occurrences)) %>%
  head(20)

print(publisher_occurrences, n = 20)
cat("\n")

# Keyword analysis
cat("KEYWORD FREQUENCY ANALYSIS\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")

# Split keywords and count frequency
keyword_counts <- filtered_candidates %>%
  select(key, matched_keywords) %>%
  separate_rows(matched_keywords, sep = ", ") %>%
  filter(matched_keywords != "") %>%
  group_by(matched_keywords) %>%
  summarise(dataset_count = n()) %>%
  arrange(desc(dataset_count)) %>%
  head(30)

cat("Top 30 matched keywords:\n")
print(keyword_counts, n = 30)
cat("\n")

# Datasets with highest WORMS coverage
cat("TOP 20 DATASETS BY WORMS COVERAGE (with >50 species)\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
top_worms_datasets <- filtered_candidates %>%
  filter(total_species > 50) %>%
  arrange(desc(worms_percentage), desc(total_species)) %>%
  select(title, publishingOrganizationTitle, total_species, worms_percentage, total_occurrences) %>%
  head(20)

print(top_worms_datasets, n = 20)
cat("\n")

# Largest datasets by species count
cat("TOP 20 DATASETS BY SPECIES COUNT\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
largest_datasets <- filtered_candidates %>%
  arrange(desc(total_species)) %>%
  select(title, publishingOrganizationTitle, total_species, worms_percentage, total_occurrences) %>%
  head(20)

print(largest_datasets, n = 20)
cat("\n")

# Largest datasets by occurrence count
cat("TOP 20 DATASETS BY OCCURRENCE COUNT\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
largest_occurrence_datasets <- filtered_candidates %>%
  arrange(desc(total_occurrences)) %>%
  select(title, publishingOrganizationTitle, total_species, total_occurrences, worms_percentage) %>%
  head(20)

print(largest_occurrence_datasets, n = 20)
cat("\n")

# Distribution of WORMS coverage percentages
cat("WORMS COVERAGE DISTRIBUTION\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
coverage_bins <- filtered_candidates %>%
  mutate(coverage_bin = cut(worms_percentage, 
                            breaks = c(0, 70, 80, 90, 95, 100),
                            labels = c("60-70%", "70-80%", "80-90%", "90-95%", "95-100%"),
                            include.lowest = TRUE)) %>%
  group_by(coverage_bin) %>%
  summarise(
    dataset_count = n(),
    total_species = sum(total_species),
    total_occurrences = sum(total_occurrences)
  )

print(coverage_bins)
cat("\n")

# Save summary tables
write_tsv(publisher_counts, "summary_publishers_by_count.tsv")
write_tsv(publisher_species, "summary_publishers_by_species.tsv")
write_tsv(publisher_occurrences, "summary_publishers_by_occurrences.tsv")
write_tsv(keyword_counts, "summary_keywords.tsv")

cat("=" %>% rep(70) %>% paste0(collapse = ""), "\n")
cat("Summary tables saved:\n")
cat("  - summary_publishers_by_count.tsv\n")
cat("  - summary_publishers_by_species.tsv\n")
cat("  - summary_publishers_by_occurrences.tsv\n")
cat("  - summary_keywords.tsv\n")
cat("=" %>% rep(70) %>% paste0(collapse = ""), "\n")
