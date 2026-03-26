library(rgbif)

# Test dataset_export
cat("Testing dataset_export...\n")
res_export <- dataset_export(query='marine', type='OCCURRENCE')
cat("Type:", class(res_export), "\n")
cat("Length:", length(res_export), "\n")

if(is.data.frame(res_export)) {
  cat("Rows:", nrow(res_export), "\n")
  cat("Columns:", paste(colnames(res_export), collapse=", "), "\n")
} else if(is.list(res_export)) {
  cat("Names:", paste(names(res_export), collapse=", "), "\n")
  if(!is.null(res_export$data)) {
    cat("Data rows:", nrow(res_export$data), "\n")
  }
}

cat("\n\nTesting dataset_search for comparison...\n")
res_search <- dataset_search(query='marine', type='OCCURRENCE', limit=100)
cat("Type:", class(res_search), "\n")
if(!is.null(res_search$results)) {
  cat("Results rows:", nrow(res_search$results), "\n")
  cat("Total count:", res_search$meta$count, "\n")
}
