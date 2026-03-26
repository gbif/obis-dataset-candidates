library(rgbif)

dataset <- dataset_get("57cbea0b-0bf4-4bc3-b3c7-c25a9caa5ef0")

cat("Dataset:", dataset$data$title, "\n")
cat("Publishing Org:", dataset$data$publishingOrganizationTitle, "\n")
cat("Network Keys:", paste(dataset$data$networkKeys, collapse=", "), "\n")
cat("\nOBIS Network UUID: 2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6\n")

if ("2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6" %in% dataset$data$networkKeys) {
  cat("\n*** YES - This dataset IS listed as part of OBIS network ***\n")
} else {
  cat("\n*** NO - This dataset is NOT listed as part of OBIS network ***\n")
}
