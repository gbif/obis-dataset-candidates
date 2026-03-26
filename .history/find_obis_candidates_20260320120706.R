# Find GBIF Dataset Candidates for OBIS
# This script searches GBIF for marine datasets not currently in OBIS

library(rgbif)
library(dplyr)
library(stringr)
library(rlang)  # For %||% operator

# Load marine keywords from OBIS word frequency analysis
load_marine_keywords <- function(freq_file = "obis_word_frequencies.csv", 
                                  top_n = 100) {
  if (file.exists(freq_file)) {
    word_freq <- read.csv(freq_file)
    
    # Select top N words (file is already filtered to marine terms)
    keywords <- word_freq %>%
      head(top_n) %>%
      pull(word)
    
    cat(sprintf("Loaded %d keywords from %s\n", length(keywords), freq_file))
    cat(sprintf("Frequency range: %d - %d\n", 
                word_freq$n[1], 
                word_freq$n[min(top_n, nrow(word_freq))]))
    
    return(keywords)
  } else {
    warning(sprintf("Word frequency file '%s' not found. Run analyze_obis_datasets.R first.", freq_file))
    # Default marine keywords as fallback
    return(c(
      "marine", "ocean", "sea", "coastal", "reef", "fish",
      "whale", "dolphin", "coral", "plankton", "benthic"
    ))
  }
}

# Fetch existing OBIS dataset keys using network_constituents
load_obis_dataset_keys <- function() {
  obis_network_uuid <- "2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6"
  all_datasets <- list()
  offset <- 0
  limit <- 1000
  
  cat("Fetching OBIS datasets from network...\n")
  repeat {
    cat(sprintf("  offset %d...\n", offset))
    
    batch <- network_constituents(uuid = obis_network_uuid, limit = limit, start = offset)
    
    if (nrow(batch) == 0) break
    
    all_datasets <- append(all_datasets, list(batch))
    
    if (nrow(batch) < limit) break
    
    offset <- offset + limit
    Sys.sleep(0.3)
  }
  
  obis_datasets <- bind_rows(all_datasets)
  cat(sprintf("Fetched %d OBIS datasets\n", nrow(obis_datasets)))
  
  return(obis_datasets$key)
}

# Search GBIF datasets by keyword using rgbif::dataset_export (returns all matches)
search_gbif_datasets <- function(keyword) {
  tryCatch({
    result <- dataset_export(
      query = keyword,
      type = "OCCURRENCE"  # Focus on occurrence datasets
    )
    
    # dataset_export returns a data frame directly
    # Rename datasetKey to key for consistency
    if (!is.null(result) && nrow(result) > 0) {
      if ("datasetKey" %in% names(result)) {
        result <- result %>% rename(key = datasetKey)
      }
    }
    
    return(result)
  }, error = function(e) {
    warning(sprintf("Failed to fetch data for keyword '%s': %s", keyword, e$message))
    return(NULL)
  })
}



# Find candidate datasets
find_candidates <- function() {
  # Load data
  marine_keywords <- load_marine_keywords()
  obis_keys <- load_obis_dataset_keys()
  
  cat("OBIS datasets to exclude:", length(obis_keys), "\n")
  cat("Marine keywords to search:", length(marine_keywords), "\n\n")
  
  all_candidates <- list()
  unique_keys <- character(0)
  
  for (keyword in marine_keywords) {
    cat(sprintf("Searching GBIF for keyword: '%s'...\n", keyword))
    
    # dataset_export returns all matching datasets (no pagination needed)
    result <- search_gbif_datasets(keyword)
    
    if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
      # Filter out datasets already in OBIS
      candidates <- result %>%
        filter(!(key %in% obis_keys)) %>%
        filter(!(key %in% unique_keys))  # Avoid duplicates
      
      if (nrow(candidates) > 0) {
        cat(sprintf("  Found %d new candidates (total for keyword: %d)\n", 
                    nrow(candidates), nrow(result)))
        all_candidates <- append(all_candidates, list(candidates))
        unique_keys <- c(unique_keys, candidates$key)
      } else {
        cat(sprintf("  No new candidates (total found: %d, all already in OBIS or duplicates)\n", 
                    nrow(result)))
      }
    }
    
    Sys.sleep(0.3)  # Be nice to the API
  }
  
  # Combine all results
  if (length(all_candidates) > 0) {
    candidates_df <- bind_rows(all_candidates)
    candidates_df <- candidates_df %>%
      distinct(key, .keep_all = TRUE)  # Remove any remaining duplicates
    
    cat(sprintf("\nTotal unique candidate datasets found: %d\n", nrow(candidates_df)))
    return(candidates_df)
  } else {
    cat("\nNo candidates found.\n")
    return(data.frame())
  }
}

# Track which keywords match each candidate dataset
track_matched_keywords <- function(candidates_df, marine_keywords) {
  cat("Identifying matched keywords for each candidate...\n")
  
  candidates_df <- candidates_df %>%
    rowwise() %>%
    mutate(
      text = tolower(paste(title %||% "", description %||% "")),
      matched_keywords = {
        matches <- sapply(marine_keywords, function(kw) {
          grepl(kw, text, fixed = TRUE)
        })
        paste(marine_keywords[matches], collapse = ", ")
      }
    ) %>%
    ungroup() %>%
    select(key, title, publishingOrganizationTitle, matched_keywords)
  
  return(candidates_df)
}

# Find GBIF Dataset Candidates for OBIS
cat("=" %>% rep(60) %>% paste0(collapse = ""), "\n")
cat("Finding GBIF Dataset Candidates for OBIS\n")
cat("=" %>% rep(60) %>% paste0(collapse = ""), "\n\n")

# Load marine keywords from OBIS word frequency analysis
cat("Loading keywords from OBIS word frequency analysis...\n")
marine_keywords <- load_marine_keywords(
  top_n = 91  # Load all manually filtered marine keywords
)


# Find candidates using dataset_search with pagination
candidates <- find_candidates()

if (nrow(candidates) > 0) {
  # Track which keywords matched each candidate
  cat("\nTracking matched keywords...\n")
  candidates_with_keywords <- track_matched_keywords(candidates, marine_keywords)
  
  # Save results as TSV
  output_file <- "gbif_candidates.tsv"
  write.table(candidates_with_keywords, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
  cat(sprintf("\nCandidate datasets saved to: %s\n", output_file))
  cat(sprintf("Total candidates: %d\n", nrow(candidates_with_keywords)))
  
  # Display sample of candidates
  cat("\nSample of candidate datasets:\n")
  cat("-" %>% rep(60) %>% paste0(collapse = ""), "\n")
  
  sample_candidates <- candidates_with_keywords %>%
    head(10) %>%
    mutate(matched_keywords = substr(matched_keywords, 1, 50))  # Truncate for display
  
  print(sample_candidates)
} else {
  cat("\nNo candidates to analyze.\n")
}
