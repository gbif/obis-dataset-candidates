# Analyze specific publisher
# UAR PatriNat publisher analysis

library(dplyr)
library(readr)

# Read the filtered candidates
filtered_candidates <- read_tsv("../data/gbif_candidates_filtered.tsv", show_col_types = FALSE)

# UAR PatriNat publisher key
publisher_key <- "1928bdf0-f5d2-11dc-8c12-b8a03c50a862"

# Filter for UAR PatriNat datasets
patrinat_datasets <- filtered_candidates %>%
  filter(publishingOrganizationKey == publisher_key)

cat("=" %>% rep(70) %>% paste0(collapse = ""), "\n")
cat("UAR PatriNat Publisher Analysis\n")
cat("=" %>% rep(70) %>% paste0(collapse = ""), "\n\n")

cat("Publisher Key:", publisher_key, "\n")
cat("Publisher Name:", unique(patrinat_datasets$publishingOrganizationTitle)[1], "\n\n")

cat("SUMMARY STATISTICS\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
cat("Total datasets:", nrow(patrinat_datasets), "\n")
cat("Total species:", sum(patrinat_datasets$total_species), "\n")
cat("Total occurrences:", sum(patrinat_datasets$total_occurrences), "\n")
cat("Total WORMS species:", sum(patrinat_datasets$worms_species), "\n\n")

cat("AVERAGE WORMS PERCENTAGE:", round(mean(patrinat_datasets$worms_percentage), 2), "%\n")
cat("MEDIAN WORMS PERCENTAGE:", round(median(patrinat_datasets$worms_percentage), 2), "%\n\n")

cat("WORMS PERCENTAGE DISTRIBUTION\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
print(summary(patrinat_datasets$worms_percentage))
cat("\n")

cat("SPECIES COUNT DISTRIBUTION\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
print(summary(patrinat_datasets$total_species))
cat("\n")

cat("OCCURRENCE COUNT DISTRIBUTION\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
print(summary(patrinat_datasets$total_occurrences))
cat("\n")

# WORMS coverage bins
cat("WORMS COVERAGE BINS\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
coverage_bins <- patrinat_datasets %>%
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

# Top 20 datasets by species count
cat("TOP 20 DATASETS BY SPECIES COUNT\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
top_datasets <- patrinat_datasets %>%
  arrange(desc(total_species)) %>%
  select(title, total_species, worms_percentage, total_occurrences) %>%
  head(20)

print(top_datasets, n = 20)
cat("\n")

# Datasets with lowest WORMS coverage
cat("20 DATASETS WITH LOWEST WORMS COVERAGE\n")
cat("-" %>% rep(70) %>% paste0(collapse = ""), "\n")
lowest_worms <- patrinat_datasets %>%
  arrange(worms_percentage, desc(total_species)) %>%
  select(title, total_species, worms_percentage, total_occurrences) %>%
  head(20)

print(lowest_worms, n = 20)
cat("\n")

cat("=" %>% rep(70) %>% paste0(collapse = ""), "\n")
cat("Analysis complete\n")
cat("=" %>% rep(70) %>% paste0(collapse = ""), "\n")
