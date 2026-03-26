library(jsonlite)
library(dplyr)

# Test fetching OBIS organizations
obis_network_uuid <- "2b7c7b4f-4d4f-40d3-94de-c28b6fa054a6"
all_orgs <- list()
offset <- 0
limit <- 300

cat("Fetching OBIS network organizations...\n")
repeat {
  url <- sprintf("https://api.gbif.org/v1/network/%s/organization?limit=%d&offset=%d",
                 obis_network_uuid, limit, offset)
  
  cat(sprintf("  Fetching from offset %d...\n", offset))
  response <- jsonlite::fromJSON(url)
  
  cat(sprintf("  Got %d results (endOfRecords: %s)\n", length(response$results), response$endOfRecords))
  
  if (length(response$results) == 0) break
  
  all_orgs <- append(all_orgs, list(response$results))
  
  if (response$endOfRecords) break
  
  offset <- offset + limit
  Sys.sleep(0.3)
}

if (length(all_orgs) > 0) {
  orgs_df <- bind_rows(all_orgs)
  cat(sprintf("\nTotal OBIS network organizations: %d\n", nrow(orgs_df)))
  
  # Show some examples
  cat("\nSample organization keys:\n")
  print(head(orgs_df$key, 10))
  
  # Show titles
  cat("\nSample organization titles:\n")
  print(head(orgs_df$title, 10))
} else {
  cat("No organizations found\n")
}
