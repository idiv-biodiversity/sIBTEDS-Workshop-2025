################################################################################
################################################################################
################################################################################
## MAPS
## sIBTEDS project
## https://rstudio.github.io/leaflet/markers.html 
################################################################################
################################################################################
################################################################################
## Working Directory
getwd()
setwd("C:YOUR/WORKING/DIRECTORY")
################################################################################
################################################################################
################################################################################
## Packages
#install.packages("leaflet")
## to install the development version from Github, run
#install.packages("devtools")
library(devtools)
#devtools::install_github("rstudio/leaflet")
library(leaflet)
library(dplyr)
# Load required libraries
library(ggplot2)
library(sf)
#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages("rnaturalearthdata")
library(rnaturalearthdata)
#install.packages("ggthemes")
library(ggthemes)
################################################################################
################################################################################
################################################################################
## MiCODA V2 PRIMER V4 (COMPLETE)
# Load dataset
MiCoDaV2 <- read.csv("MiCoDaV2_PrimerV4_20250309.csv", sep = ",")

# Subset data
df.20 <- MiCoDaV2[1:41105,]

# Aggregate data to count samples per locality (longitude and latitude)
df.aggregated <- df.20 %>%
  group_by(long, lat, environment_.biome.) %>%
  summarise(sample_count = n(), .groups = "drop")

# Convert to an sf object for spatial plotting
df.sf <- st_as_sf(df.aggregated, coords = c("long", "lat"), crs = 4326)

# Load world map with gray background
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define color mapping with a colorblind-friendly palette
color_map <- c(
  "Water" = "#377eb8",    # DodgerBlue
  "Organism" = "#984ea3", # DarkOrange
  "Other" = "#4daf4a",    # LimeGreen
  "Mineral" = "#ff7f00"   # Crimson
)

# Create the map
ggplot() +
  # Add world map in gray
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  # Add sample points with color based on environment biome
  geom_sf(data = df.sf, aes(size = sample_count, color = environment_.biome.), alpha = 0.7) +
  scale_color_manual(values = color_map) +  # Apply custom colors
  scale_size_continuous(range = c(2, 12)) +  # Adjust point sizes
  theme_minimal() +
  labs(
    title = "MiCoDa Version 2.0",
    subtitle = "Primer V4",
    color = "Environment Biome",
    size = "Sample Count"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )



################################################################################
################################################################################
################################################################################
#  MiCODA V2 PRIMER V4 (FILTERED BY QUALITY CHECK)
MiCoDaV2 <- read.csv("Metadata_MiCoDaV2_PrimerV4_20250310_V2.csv", sep = ",")
names(MiCoDaV2)

# Subset data
df.20 <- MiCoDaV2[1:32983,]

# Aggregate data to count samples per locality (longitude and latitude)
df.aggregated <- df.20 %>%
  group_by(long, lat, environment_.biome.) %>%
  summarise(sample_count = n(), .groups = "drop")

# Convert to an sf object for spatial plotting
df.sf <- st_as_sf(df.aggregated, coords = c("long", "lat"), crs = 4326)

# Load world map with gray background
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define color mapping with a colorblind-friendly palette
color_map <- c(
  "Water" = "#377eb8",    # DodgerBlue
  "Organism" = "#984ea3", # DarkOrange
  "Other" = "#4daf4a",    # LimeGreen
  "Mineral" = "#ff7f00"   # Crimson
)

# Create the map
ggplot() +
  # Add world map in gray
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  # Add sample points with color based on environment biome
  geom_sf(data = df.sf, aes(size = sample_count, color = environment_.biome.), alpha = 0.7) +
  scale_color_manual(values = color_map) +  # Apply custom colors
  scale_size_continuous(range = c(2, 12)) +  # Adjust point sizes
  theme_minimal() +
  labs(
    title = "MiCoDa Version 2.0",
    subtitle = "Primer V4",
    color = "Environment Biome",
    size = "Sample Count"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )


################################################################################
################################################################################
################################################################################
# END :)
