library(rgbif)
library(dplyr)
library(rlang)

# Get the dataset
result <- dataset_export(query = "57cbea0b-0bf4-4bc3-b3c7-c25a9caa5ef0", type = "OCCURRENCE")
result <- result %>% rename(key = datasetKey)

obis_network_uuid <- "2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6"

cat("Before filtering:\n")
cat("Dataset key:", result$key[1], "\n")
cat("networkKeys:", result$networkKeys[1], "\n")

# Apply the new filtering logic
filtered <- result %>%
  rowwise() %>%
  filter(!grepl(obis_network_uuid, networkKeys %||% "", fixed = TRUE)) %>%
  ungroup()

cat("\nAfter filtering:\n")
cat("Rows remaining:", nrow(filtered), "\n")

if (nrow(filtered) == 0) {
  cat("\n*** SUCCESS: Dataset was correctly filtered out! ***\n")
} else {
  cat("\n*** FAILED: Dataset was NOT filtered out ***\n")
}
