# OBIS Dataset Word Analysis
# Extract most common words from OBIS dataset descriptions and titles

library(rgbif)
library(dplyr)
library(tidytext)
library(stringr)

# Fetch all OBIS datasets with pagination
obis_network_uuid <- "2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6"
all_datasets <- list()
offset <- 0
limit <- 1000

cat("Fetching OBIS datasets...\n")
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

# Extract words from titles and descriptions
words <- obis_datasets %>%
  select(title, description) %>%
  mutate(
    description = ifelse(is.na(description), "", description),
    combined_text = paste(title, description, sep = " ")
  ) %>%
  unnest_tokens(word, combined_text) %>%
  filter(
    !word %in% stop_words$word,  # Remove common stop words
    str_length(word) > 3,         # Remove very short words
    !str_detect(word, "^\\d+$")   # Remove pure numbers
  )

# Count word frequencies
word_freq <- words %>%
  count(word, sort = TRUE) %>%
  head(500)

# Save to CSV
write.csv(word_freq, "../data/obis_word_frequencies.csv", row.names = FALSE)
cat(sprintf("Saved %d common words to ../data/obis_word_frequencies.csv\n", nrow(word_freq)))
