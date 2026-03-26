# Find GBIF Dataset Candidates for OBIS
# This script searches GBIF for marine datasets not currently in OBIS

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(rgbif)

# Load marine keywords from previous analysis
load_marine_keywords <- function(keyword_file = "marine_keywords.csv") {
  if (file.exists(keyword_file)) {
    keywords <- read.csv(keyword_file)
    return(keywords$word)
  } else {
    # Default marine keywords if file doesn't exist
    return(c(
      "marine", "ocean", "sea", "coastal", "reef", "fish",
      "whale", "dolphin", "coral", "plankton", "benthic"
    ))
  }
}

# Load existing OBIS dataset keys
load_obis_dataset_keys <- function(obis_file = "obis_datasets.csv") {
  if (file.exists(obis_file)) {
    obis_data <- read.csv(obis_file)
    return(obis_data$key)
  } else {
    warning("OBIS datasets file not found. Run analyze_obis_datasets.R first.")
    return(character(0))
  }
}

# Search GBIF datasets by keyword
search_gbif_datasets <- function(keyword, offset = 0, limit = 100) {
  url <- "https://api.gbif.org/v1/dataset/search"
  
  response <- GET(
    url,
    query = list(
      q = keyword,
      offset = offset,
      limit = limit,
      type = "OCCURRENCE"  # Focus on occurrence datasets
    )
  )
  
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(content, flatten = TRUE)
    return(data)
  } else {
    warning(sprintf("Failed to fetch data for keyword '%s': %d", keyword, status_code(response)))
    return(NULL)
  }
}

# Fetch full dataset description using rgbif
fetch_dataset_description <- function(dataset_key) {
  tryCatch({
    dataset <- rgbif::dataset_get(dataset_key)
    return(dataset$data$description)
  }, error = function(e) {
    warning(sprintf("Failed to fetch description for dataset %s: %s", dataset_key, e$message))
    return(NA)
  })
}

# Enrich candidate datasets with full descriptions
enrich_with_descriptions <- function(candidates_df) {
  cat("Fetching full descriptions from GBIF using rgbif...
")
  
  candidates_df <- candidates_df %>%
    rowwise() %>%
    mutate(
      full_description = fetch_dataset_description(key),
      # Use full description if available, otherwise fall back to search result description
      description = ifelse(!is.na(full_description) & full_description != "", 
                          full_description, 
                          description %||% "")
    ) %>%
    ungroup() %>%
    select(-full_description)  # Remove temporary column
  
  cat("Description enrichment complete.\n")
  return(candidates_df)
}

# Filter datasets by geographic and description criteria
is_likely_marine <- function(dataset) {
  # Check if dataset has marine-related keywords in title or description
  text <- tolower(paste(
    dataset$title %||% "",
    dataset$description %||% ""
  ))
  
  marine_terms <- c(
    "marine", "ocean", "sea", "coastal", "reef", "benthic",
    "pelagic", "fish", "whale", "dolphin", "coral", "plankton",
    "seabird", "kelp", "estuary", "intertidal", "subtidal"
  )
  
  has_marine_keyword <- any(str_detect(text, marine_terms))
  
  # Check geographic coverage (if available)
  # Marine areas are typically within specific lat/lon ranges
  # This is a simplified check
  
  return(has_marine_keyword)
}

# Find candidate datasets
find_candidates <- function(top_keywords = 20, max_per_keyword = 100) {
  # Load data
  marine_keywords <- load_marine_keywords()
  obis_keys <- load_obis_dataset_keys()
  
  cat("OBIS datasets to exclude:", length(obis_keys), "\n")
  cat("Marine keywords to search:", length(marine_keywords), "\n\n")
  
  # Use top keywords for searching
  search_keywords <- head(marine_keywords, top_keywords)
  
  all_candidates <- list()
  unique_keys <- character(0)
  
  for (keyword in search_keywords) {
    cat(sprintf("Searching GBIF for keyword: '%s'...\n", keyword))
    
    result <- search_gbif_datasets(keyword, offset = 0, limit = max_per_keyword)
    
    if (!is.null(result) && length(result$results) > 0) {
      # Filter out datasets already in OBIS
      candidates <- result$results %>%
        filter(!(key %in% obis_keys)) %>%
        filter(!(key %in% unique_keys))  # Avoid duplicates
      
      if (nrow(candidates) > 0) {
        cat(sprintf("  Found %d new candidates\n", nrow(candidates)))
        all_candidates <- append(all_candidates, list(candidates))
        unique_keys <- c(unique_keys, candidates$key)
      }
    }
    
    Sys.sleep(0.5)  # Be nice to the API
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

# Score candidates by relevance
score_candidates <- function(candidates_df) {
  marine_keywords <- load_marine_keywords()
  
  candidates_df <- candidates_df %>%
    rowwise() %>%
    mutate(
      text = tolower(paste(title %||% "", description %||% "")),
      marine_keyword_count = sum(str_count(text, marine_keywords)),
      marine_score = marine_keyword_count / (str_length(text) + 1) * 1000
    ) %>%
    ungroup() %>%
    arrange(desc(marine_score))
  
  return(candidates_df)
}

# Main workflow
main <- function() {
  cat("=" %>% rep(60) %>% paste0(collapse = ""), "\n")
  cat("Finding GBIF Dataset Candidates for OBIS\n")
  cat("=" %>% rep(60) %>% paste0(collapse = ""), "\n\n")
  
  # Find candidates
  candidates <- find_candidates(top_keywords = 15, max_per_keyword = 100)
  
  if (nrow(candidates) > 0) {
    # Enrich with full descriptions
    cat("\nEnriching candidates with full descriptions...\n")
    enriched_candidates <- enrich_with_descriptions(candidates)
    
    # Score candidates
    cat("\nScoring candidates by marine relevance...\n")
    scored_candidates <- score_candidates(enriched_candidates)
    
    # Save results
    output_file <- "gbif_candidates.csv"
    write.csv(scored_candidates, output_file, row.names = FALSE)
    cat(sprintf("\nCandidate datasets saved to: %s\n", output_file))
    
    # Display top candidates
    cat("\nTop 20 candidate datasets:\n")
    cat("-" %>% rep(60) %>% paste0(collapse = ""), "\n")
    
    top_candidates <- scored_candidates %>%
      select(title, publishingOrganizationTitle, marine_score, key) %>%
      head(20)
    
    print(top_candidates)
    
    return(scored_candidates)
  } else {
    cat("\nNo candidates to analyze.\n")
    return(data.frame())
  }
}

# Run if executed directly
if (!interactive()) {
  results <- main()
} else {
  cat("Script loaded. Run main() to find candidate datasets.\n")
  cat("Make sure you've run analyze_obis_datasets.R first!\n")
}
