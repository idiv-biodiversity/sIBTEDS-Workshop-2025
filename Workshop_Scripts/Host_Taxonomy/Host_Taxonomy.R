# Script to obtain taxonomic information for a list of species using the GBIF API

# Requires the 'rgbif' package to access the GBIF database
# Make sure you have it installed:
# install.packages("rgbif")
# install.packages("tidyverse")
library(rgbif)
library(tidyverse)

# List of species names (modify this vector according to your data)
species_list <- c("Panthera leo", "Canis lupus", "Homo sapiens", "Lynx", "Insecta")

# Empty list to store the taxonomic results
taxonomy_results <- list()

# Definition of the desired taxonomic ranks
taxonomic_ranks <- c("kingdom", "phylum", "class", "order", "family", "genus")

# Loop to obtain the taxonomic information of each species
for (i in 1:length(species_list)) {
  
  # Query to the GBIF API to get the taxonomic information of the current species
  taxonomic_data <- rgbif::name_backbone(species_list[i])
  
  # Check if all desired taxonomic ranks were retrieved
  if (length(intersect(names(taxonomic_data), taxonomic_ranks)) == length(taxonomic_ranks)) {
    
    # If the information is complete, select the required columns
    taxonomic_clean <- taxonomic_data %>%
      select(all_of(taxonomic_ranks))
    
    # Add the original species name for reference
    taxonomic_clean$species_name <- species_list[i]
    
    # Save the result in the list
    taxonomy_results[[i]] <- taxonomic_clean
    
  } else {
    
    # Identify the missing taxonomic ranks
    missing_ranks <- setdiff(taxonomic_ranks, names(taxonomic_data))
    available_ranks <- intersect(taxonomic_ranks, names(taxonomic_data))
    
    # Select the available ranks
    taxonomic_partial <- taxonomic_data %>%
      select(all_of(available_ranks))
    
    # Create empty columns (NA) for the missing ranks
    missing_data <- matrix(NA, nrow = 1, ncol = length(missing_ranks))
    colnames(missing_data) <- missing_ranks
    missing_data_tibble <- as_tibble(missing_data)
    
    # Combine the available and missing ranks (in the correct order)
    complete_taxonomy <- bind_cols(taxonomic_partial, missing_data_tibble)
    complete_taxonomy <- complete_taxonomy[, taxonomic_ranks]
    
    # Add the original species name
    complete_taxonomy$species_name <- species_list[i]
    
    # Save the result in the list
    taxonomy_results[[i]] <- complete_taxonomy
  }
  
  # Print the iteration number to monitor progress
  print(paste("Species", i, "of", length(species_list)))
}

# At the end, 'taxonomy_results' contains the taxonomic information for all species
# You can combine the list into a single data frame if needed:
taxonomy_dataframe <- bind_rows(taxonomy_results)
