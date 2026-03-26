library(rgbif)
library(dplyr)
library(readr)

# Script to calculate WORMS species coverage for GBIF candidate datasets
# For each dataset, fetches all species keys and calculates percentage that are in WORMS

cat("==========================================================\n")
cat("  Calculating WORMS Species Coverage for GBIF Datasets\n")
cat("==========================================================\n\n")

# Load WORMS species keys
cat("Loading WORMS species keys...\n")
worms_species <- read_tsv("../../data/worms-species.tsv", show_col_types = FALSE)
worms_keys <- unique(worms_species$usageKey)
cat(sprintf("Loaded %d unique WORMS species keys\n\n", length(worms_keys)))

# Load candidate datasets
cat("Loading candidate datasets...\n")
candidates <- read_tsv("../../data/gbif_candidates.tsv", show_col_types = FALSE)
cat(sprintf("Loaded %d candidate datasets\n\n", nrow(candidates)))

# Check for existing results file and load already processed datasets
output_file <- "../../data/percentage_worms.tsv"
processed_keys <- character()

if (file.exists(output_file)) {
  cat("Found existing results file, loading already processed datasets...\n")
  existing_results <- read_tsv(output_file, show_col_types = FALSE)
  processed_keys <- existing_results$datasetKey
  cat(sprintf("Already processed: %d datasets\n", length(processed_keys)))
  cat(sprintf("Remaining: %d datasets\n\n", nrow(candidates) - length(processed_keys)))
} else {
  # Create file with header
  cat("Creating new results file...\n")
  header_df <- data.frame(
    datasetKey = character(),
    title = character(),
    total_species = integer(),
    worms_species = integer(),
    worms_percentage = numeric(),
    total_occurrences = integer()
  )
  write_tsv(header_df, output_file)
  cat("Results file created\n\n")
}

# Function to get species keys for a dataset
get_dataset_species <- function(dataset_key) {
  tryCatch({
    result <- occ_count(datasetKey = dataset_key, facet = "speciesKey", facetLimit = 100000)
    
    # occ_count returns a data frame with columns: speciesKey and count
    if (is.null(result) || nrow(result) == 0) {
      return(data.frame(speciesKey = integer(), count = integer()))
    }
    
    return(result)
  }, error = function(e) {
    message(sprintf("  Error fetching species for dataset %s: %s", dataset_key, e$message))
    return(data.frame(speciesKey = integer(), count = integer()))
  })
}

# Process each dataset
cat("Processing datasets...\n")
processed_count <- 0
skipped_count <- 0

for (i in 1:nrow(candidates)) {
  dataset_key <- candidates$key[i]
  dataset_title <- candidates$title[i]
  
  # Skip if already processed
  if (dataset_key %in% processed_keys) {
    skipped_count <- skipped_count + 1
    if (skipped_count %% 100 == 0) {
      cat(sprintf("Skipped %d already processed datasets...\n", skipped_count))
    }
    next
  }
  
  cat(sprintf("[%d/%d] %s\n", i, nrow(candidates), dataset_key))
  
  # Get all species keys for this dataset
  species_data <- get_dataset_species(dataset_key)
  
  if (nrow(species_data) == 0) {
    cat("  No species found\n")
    result_row <- data.frame(
      datasetKey = dataset_key,
      title = dataset_title,
      total_species = 0,
      worms_species = 0,
      worms_percentage = NA_real_,
      total_occurrences = 0
    )
  } else {
    # Count distinct species
    total_species <- nrow(species_data)
    total_occurrences <- sum(species_data$count, na.rm = TRUE)
    
    # Count how many species are in WORMS
    worms_matches <- sum(species_data$speciesKey %in% worms_keys)
    
    # Calculate percentage
    worms_percentage <- (worms_matches / total_species) * 100
    
    cat(sprintf("  Species: %d total, %d in WORMS (%.1f%%)\n", 
                total_species, worms_matches, worms_percentage))
    
    result_row <- data.frame(
      datasetKey = dataset_key,
      title = dataset_title,
      total_species = total_species,
      worms_species = worms_matches,
      worms_percentage = worms_percentage,
      total_occurrences = total_occurrences
    )
  }
  
  # Append result immediately to file
  write_tsv(result_row, output_file, append = TRUE)
  processed_count <- processed_count + 1
  
  # Progress update every 100 datasets
  if (processed_count %% 100 == 0) {
    cat(sprintf("==> Processed %d new datasets (total in file: %d)\n", 
                processed_count, length(processed_keys) + processed_count))
  }
  
  # Add delay to avoid API rate limiting
  Sys.sleep(0.5)
}

cat(sprintf("\n==> Processing complete!\n"))
cat(sprintf("New datasets processed: %d\n", processed_count))
cat(sprintf("Already processed (skipped): %d\n", skipped_count))
cat(sprintf("Results saved to: %s\n", output_file))

cat(sprintf("\n==> Processing complete!\n"))
cat(sprintf("New datasets processed: %d\n", processed_count))
cat(sprintf("Already processed (skipped): %d\n", skipped_count))
cat(sprintf("Results saved to: %s\n", output_file))

# Load final results for summary statistics
cat("\nGenerating summary statistics...\n")
results_df <- read_tsv(output_file, show_col_types = FALSE)

# Summary statistics
cat("\n==========================================================\n")
cat("Summary Statistics:\n")
cat("==========================================================\n")
cat(sprintf("Total datasets in results: %d\n", nrow(results_df)))
cat(sprintf("Datasets with species data: %d\n", sum(results_df$total_species > 0)))
cat(sprintf("Datasets with no species: %d\n", sum(results_df$total_species == 0)))
cat(sprintf("Mean WORMS coverage: %.1f%%\n", mean(results_df$worms_percentage, na.rm = TRUE)))
cat(sprintf("Median WORMS coverage: %.1f%%\n", median(results_df$worms_percentage, na.rm = TRUE)))
cat(sprintf("Datasets with >50%% WORMS coverage: %d\n", 
            sum(results_df$worms_percentage > 50, na.rm = TRUE)))
cat(sprintf("Datasets with 100%% WORMS coverage: %d\n", 
            sum(results_df$worms_percentage == 100, na.rm = TRUE)))

# Show top 10 by WORMS percentage
cat("\nTop 10 datasets by WORMS coverage:\n")
top_10 <- results_df %>%
  filter(total_species > 0) %>%
  arrange(desc(worms_percentage)) %>%
  head(10) %>%
  select(datasetKey, worms_percentage, total_species, worms_species)
print(top_10, n = 10)
