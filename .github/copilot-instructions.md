# Copilot Instructions for OBIS Dataset Candidates

## GBIF API Preferences

When making API calls to GBIF (Global Biodiversity Information Facility), prefer using the `rgbif` R package over direct HTTP requests or other methods.

### Key Functions to Use

- **`network_constituents()`**: Fetch datasets belonging to a network (e.g., OBIS network UUID: `2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6`)
- **`dataset_export()`**: Export all matching datasets for a query (returns complete results without pagination)
- **`dataset_search()`**: Search for datasets with pagination support (use `limit` and `start` parameters)
- **`dataset_get()`**: Retrieve full dataset metadata by UUID/key

### Pagination Pattern

When fetching large result sets from GBIF:

```r
all_results <- list()
offset <- 0
limit <- 100

repeat {
  result <- dataset_search(query = "keyword", limit = limit, start = offset)
  
  if (!is.null(result$data) && nrow(result$data) > 0) {
    all_results <- append(all_results, list(result$data))
    
    if (nrow(result$data) < limit || result$meta$endOfRecords) break
    
    offset <- offset + limit
  } else {
    break
  }
}

combined <- bind_rows(all_results)
```

### Network Constituents Pattern

For fetching all datasets in a network (e.g., OBIS):

```r
all_datasets <- list()
offset <- 0
limit <- 1000

repeat {
  batch <- network_constituents(uuid = network_uuid, limit = limit, start = offset)
  
  if (nrow(batch) == 0) break
  
  all_datasets <- append(all_datasets, list(batch))
  
  if (nrow(batch) < limit) break
  
  offset <- offset + limit
  Sys.sleep(0.3)  # Rate limiting
}

datasets <- bind_rows(all_datasets)
```

### Dataset Export Pattern (All Results)

For fetching all matching datasets without pagination:

```r
result <- dataset_export(
  query = "marine",
  type = "OCCURRENCE"
)

# dataset_export returns a data frame with column 'datasetKey'
# Rename to 'key' for consistency
if (!is.null(result) && nrow(result) > 0) {
  result <- result %>% rename(key = datasetKey)
}
```

## Project Context

This project identifies GBIF datasets that could belong to OBIS but are not currently part of the network. It uses keyword-based searches to find marine-related datasets and filters out existing OBIS constituents.
