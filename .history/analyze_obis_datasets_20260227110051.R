# OBIS Dataset Word Analysis
# This script analyzes OBIS dataset descriptions and titles to identify
# commonly used words that characterize marine biodiversity datasets

library(rgbif)
library(dplyr)
library(tidytext)
library(stringr)

# Function to fetch all OBIS datasets using rgbif
fetch_all_obis_datasets <- function() {
  # OBIS network UUID on GBIF
  obis_network_uuid <- "2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6"
  
  cat("Fetching OBIS datasets using rgbif...\n")
  
  # Use rgbif's network_constituents function to get all datasets
  # This handles pagination automatically
  all_datasets <- list()
  offset <- 0
  limit <- 1000  # Maximum allowed by GBIF API
  
  repeat {
    cat(sprintf("Fetching datasets at offset %d...\n", offset))
    
    datasets_batch <- network_constituents(
      uuid = obis_network_uuid,
      limit = limit,
      start = offset
    )
    
    if (nrow(datasets_batch) == 0) {
      break
    }
    
    all_datasets <- append(all_datasets, list(datasets_batch))
    
    # If we got fewer results than the limit, we've reached the end
    if (nrow(datasets_batch) < limit) {
      break
    }
    
    offset <- offset + limit
    Sys.sleep(0.3)  # Be nice to the API
  }
  
  # Combine all results
  if (length(all_datasets) > 0) {
    datasets_df <- bind_rows(all_datasets)
    cat(sprintf("Total datasets fetched: %d\n", nrow(datasets_df)))
    return(datasets_df)
  } else {
    stop("No datasets found for OBIS network")
  }
}

# Function to extract and analyze words from descriptions and titles
analyze_dataset_text <- function(datasets_df) {
  # Combine title and description
  text_data <- datasets_df %>%
    select(key, title, description) %>%
    mutate(
      description = ifelse(is.na(description), "", description),
      combined_text = paste(title, description, sep = " ")
    )
  
  # Tokenize the text
  words <- text_data %>%
    unnest_tokens(word, combined_text) %>%
    filter(
      !word %in% stop_words$word,  # Remove common stop words
      str_length(word) > 3,         # Remove very short words
      !str_detect(word, "^\\d+$")   # Remove pure numbers
    )
  
  return(words)
}

# Function to get word frequencies
get_word_frequencies <- function(words_df, top_n = 500) {
  word_freq <- words_df %>%
    count(word, sort = TRUE) %>%
    head(top_n)
  
  return(word_freq)
}
