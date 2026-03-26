library(rgbif)
library(dplyr)

# Get the dataset via dataset_export (what the script uses)
result <- dataset_export(query = "57cbea0b-0bf4-4bc3-b3c7-c25a9caa5ef0", type = "OCCURRENCE")

if (!is.null(result) && nrow(result) > 0) {
  cat("Found in dataset_export:\n")
  cat("Key:", result$datasetKey[1], "\n")
  cat("Title:", result$title[1], "\n")
  
  if ("networkKeys" %in% names(result)) {
    cat("Network Keys:", result$networkKeys[1], "\n")
    
    # Check if OBIS network is in the keys
    obis_uuid <- "2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6"
    if (!is.na(result$networkKeys[1])) {
      network_list <- strsplit(as.character(result$networkKeys[1]), ",")[[1]]
      cat("Network list:", paste(network_list, collapse = ", "), "\n")
      
      if (obis_uuid %in% network_list) {
        cat("\n*** YES - OBIS network UUID is in networkKeys ***\n")
      } else {
        cat("\n*** NO - OBIS network UUID is NOT in networkKeys ***\n")
      }
    } else {
      cat("networkKeys is NA\n")
    }
  } else {
    cat("No networkKeys field in dataset_export result\n")
  }
  
  cat("\nAll fields:\n")
  print(names(result))
} else {
  cat("Dataset not found in dataset_export\n")
}
