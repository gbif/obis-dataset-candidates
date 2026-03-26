# WORMS to GBIF Backbone Matching
# Match scientific names from WORMS COLDP data to GBIF backbone taxonomy

library(rgbif)
library(dplyr)
library(readr)

# Configuration
input_file <- "WORMS/NameUsage.tsv"
output_dir <- "WORMS/matched_chunks"
chunk_size <- 10000

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Read the full dataset
cat("Reading WORMS NameUsage file...\n")
worms_data <- read_tsv(input_file, show_col_types = FALSE)
cat(sprintf("Total records: %d\n", nrow(worms_data)))

# Check if col:scientificName column exists
if (!"col:scientificName" %in% colnames(worms_data)) {
  stop("Column 'col:scientificName' not found in NameUsage.tsv")
}

# Calculate number of chunks
total_chunks <- ceiling(nrow(worms_data) / chunk_size)
cat(sprintf("Will process %d chunks of %d rows each\n\n", total_chunks, chunk_size))

# Check for existing processed chunks
existing_chunks <- list.files(output_dir, pattern = "^chunk_\\d+_matched\\.tsv$")
processed_chunk_numbers <- as.integer(sub("chunk_(\\d+)_matched\\.tsv", "\\1", existing_chunks))

cat("Existing processed chunks:", length(processed_chunk_numbers), "\n")
if (length(processed_chunk_numbers) > 0) {
  cat("Chunk numbers:", sort(processed_chunk_numbers), "\n")
}
cat("\n")

# Process each chunk
for (chunk_num in 1:total_chunks) {
  
  # Check if chunk already processed
  if (chunk_num %in% processed_chunk_numbers) {
    cat(sprintf("Chunk %d/%d: Already processed, skipping\n", chunk_num, total_chunks))
    next
  }
  
  cat(sprintf("Processing chunk %d/%d...\n", chunk_num, total_chunks))
  
  # Calculate row indices for this chunk
  start_row <- (chunk_num - 1) * chunk_size + 1
  end_row <- min(chunk_num * chunk_size, nrow(worms_data))
  
  # Extract chunk
  chunk_data <- worms_data[start_row:end_row, ]
  cat(sprintf("  Rows %d to %d (%d names)\n", start_row, end_row, nrow(chunk_data)))
  
  # Extract scientific names
  scientific_names <- chunk_data$`col:scientificName`
  
  # Match to GBIF backbone
  cat("  Matching to GBIF backbone...\n")
  tryCatch({
    matched_data <- name_backbone_checklist(scientific_names)
    
    # Combine original data with GBIF matches
    result <- bind_cols(chunk_data, matched_data)
    
    # Save chunk
    output_file <- file.path(output_dir, sprintf("chunk_%04d_matched.tsv", chunk_num))
    write_tsv(result, output_file)
    
    cat(sprintf("  Saved to: %s\n", output_file))
    cat(sprintf("  Matched: %d/%d names\n", 
                sum(!is.na(matched_data$usageKey)), 
                nrow(matched_data)))
    
  }, error = function(e) {
    cat(sprintf("  ERROR in chunk %d: %s\n", chunk_num, e$message))
    cat("  Skipping to next chunk...\n")
  })
  
  cat("\n")
  
  # Brief pause to be nice to the API
  Sys.sleep(1)
}

cat("=" %>% rep(60) %>% paste0(collapse = ""), "\n")
cat("Processing complete!\n")
cat("Matched chunks saved in:", output_dir, "\n")

# Summary
final_chunks <- list.files(output_dir, pattern = "^chunk_\\d+_matched\\.tsv$")
cat(sprintf("Total chunks processed: %d/%d\n", length(final_chunks), total_chunks))
