library(rgbif)

dataset <- dataset_get("57cbea0b-0bf4-4bc3-b3c7-c25a9caa5ef0")

cat("Title:", dataset$title, "\n")
cat("Publishing Org:", dataset$publishingOrganizationTitle, "\n")
cat("Additional Info:", substr(dataset$additionalInfo, 1, 200), "\n\n")

# Check if networkKeys field exists
cat("Has networkKeys field:", "networkKeys" %in% names(dataset), "\n")

# List all field names to find network-related fields
cat("\nAll fields:\n")
print(names(dataset))
