# Filter marine-related keywords from word frequencies

library(dplyr)
library(stringr)

# Read word frequencies
word_freq <- read.csv("obis_word_frequencies.csv")

# Define marine-related patterns and keywords
marine_related <- c(
  # Marine animals
  "fish", "whale", "dolphin", "seal", "turtle", "shark", "cetacean", "mammal",
  "porpoise", "orca", "salmon", "oyster", "coral", "sponge", "plankton", "zooplankton",
  "phytoplankton", "krill", "shrimp", "crab", "lobster", "jellyfish", "squid", "octopus",
  "seabird", "penguin", "albatross", "tern", "gull", "pelican",
  
  # Marine habitats/environments
  "marine", "ocean", "sea", "coastal", "reef", "benthic", "pelagic", "shelf",
  "estuary", "intertidal", "subtidal", "mangrove", "seagrass", "kelp", "seamount",
  "hydrothermal", "abyssal", "bathyal", "neritic", "offshore", "underwater",
  "seafloor", "seabed", "littoral", "sublittoral",
  
  # Marine locations/water bodies
  "atlantic", "pacific", "arctic", "antarctic", "mediterranean", "caribbean",
  "gulf", "strait", "channel", "bay", "harbor", "harbour", "bering", "chukchi",
  
  # Marine activities/methods
  "fishery", "fisheries", "trawl", "seine", "longline", "gillnet", "fishing",
  "aquaculture", "mariculture", "vessel", "cruise", "transect", "dive", "sonar",
  "acoustic", "telemetry", "tracking", "tagging", "satellite",
  
  # Marine processes/features
  "tide", "current", "upwelling", "salinity", "thermocline", "halocline",
  "migration", "breeding", "spawning", "nesting", "foraging", "feeding",
  
  # Marine biology terms
  "benthos", "nekton", "neuston", "invertebrate", "vertebrate", "fauna", "flora",
  "sessile", "motile", "pelagic", "demersal", "anadromous", "catadromous"
)

# Filter for marine-related words
marine_keywords <- word_freq %>%
  filter(
    # Match any of the marine keywords
    str_detect(word, paste(marine_related, collapse = "|")) |
    # Or words ending in marine-related suffixes
    str_detect(word, "fish$|shark$|whale$") |
    # Species-specific terms
    word %in% c("loggerhead", "hawksbill", "green", "leatherback", 
                "humpback", "sperm", "killer", "bottlenose",
                "grey", "harbor", "spotted", "finless")
  ) %>%
  # Remove if word is too generic despite matching pattern
  filter(!word %in% c("green", "black", "common", "grey"))

# Save filtered keywords
write.csv(marine_keywords, "marine_keywords.csv", row.names = FALSE)
cat(sprintf("Filtered from %d to %d marine-related keywords\n", 
            nrow(word_freq), nrow(marine_keywords)))

# Show top 50
cat("\nTop 50 marine-related keywords:\n")
print(head(marine_keywords, 50))
