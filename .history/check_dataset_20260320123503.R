library(rgbif)
library(dplyr)

obis_network_uuid <- "2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6"
all_datasets <- list()
offset <- 0
limit <- 1000

cat("Fetching OBIS datasets...\n")
repeat {
  batch <- network_constituents(uuid = obis_network_uuid, limit = limit, start = offset)
  if (nrow(batch) == 0) break
  all_datasets <- append(all_datasets, list(batch))
  if (nrow(batch) < limit) break
  offset <- offset + limit
}

obis_datasets <- bind_rows(all_datasets)
cat(sprintf("Total OBIS datasets: %d\n", nrow(obis_datasets)))

# Check the specific dataset
target_key <- "57cbea0b-0bf4-4bc3-b3c7-c25a9caa5ef0"

if (target_key %in% obis_datasets$key) {
  cat("\n*** YES - This dataset IS in OBIS network ***\n\n")
  match <- obis_datasets[obis_datasets$key == target_key,]
  print(match[, c("key", "title")])
} else {
  cat("\n*** NO - This dataset is NOT in OBIS network ***\n")
}

# Also check in the candidates file
cat("\n\nChecking candidates file...\n")
candidates <- read.delim("gbif_candidates.tsv")
if (target_key %in% candidates$key) {
  cat("Dataset IS in candidates file\n")
  match_cand <- candidates[candidates$key == target_key,]
  print(match_cand)
} else {
  cat("Dataset is NOT in candidates file\n")
}
