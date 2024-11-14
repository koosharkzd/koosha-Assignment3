# Load all required libraries at the beginning
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("Biostrings")

library(Biostrings)
library(pwalign)
library(stats)
library(tidyverse)
library(viridis)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)

# Assignment 1
# Alignment; Western and Eastern Monarch Butterflies

# DNA Sequences for Western and Eastern Monarchs
seq_west <- DNAString("TATATTTTATTTTTGGAATTTGAGCAGGAATAGTTGGGACATCTTTAAGTCTTTTAATTCGAACAGAATTAGGAACTCCTGGATCTTTAATTGGTGATGATCAAATTTATAATACTATTGTTACAGCTCATGCTTTTATTATAATTTTTTTTATAGTTATACCAATTATAATTGGAGGATTTGGTAATTGATTAGTACCCCTAATATTAGGAGCTCCTGATATAGCTTTCCCCCGAATAAATAATATAAGATTTTGACTTTTACCCCCATCATTAATTTTATTAATTTCAAGAAGAATCGTAGAAAATGGTGCAGGAACAGGATGAACAGTTTACCCCCCACTTTCATCAAATATTGCTCATAGAGGATCTTCTGTAGATCTAGCTATTTTTTCTTTACATTTAGCTGGAATTTCATCTATTTTAGGAGCTATTAATTTTATTACTACAATCTTAAATATACGAATTAATAATATAACATTTGATCAAATACCTTTATTTGTTTGAGCAGTAGGTATTACAGCTCTTCTTTTATTACTTTCTTTACCAGTTTTAGCAGGAGCAATTACTATACTTCTTACTGATCGAAATTTAAATACTTCTTTTTTTGATCCTGCTGGTGGAGGAGACCCTATTTTATATCAACATTTATTT")
seq_east <- DNAString("TACTTTATATTTTATTTTTGGAATTTGAGCAGGAATAGTTGGGACATCTTTAAGTCTTTTAATTCGAACAGAATTAGGAACTCCTGGATCTTTAATTGGTGATGATCAAATTTATAATACTATTGTTACAGCTCATGCTTTTATTATAATTTTTTTTATAGTTATACCAATTATAATTGGAGGATTTGGTAATTGATTAGTACCCCTAATATTAGGAGCTCCTGATATAGCTTTCCCCCGAATAAATAATATAAGATTTTGACTTTTACCCCCATCATTAATTTTATTAATTTCAAGAAGAATCGTAGAAAATGGTGCAGGAACAGGATGAACAGTTTACCCCCCACTTTCATCAAATATTGCTCATAGAGGATCTTCTGTAGATCTAGCTATTTTTTCTTTACATTTAGCTGGAATTTCATCTATTTTAGGAGCTATTAATTTTATTACTACAATCTTAAATATACGAATTAATAATATAACATTTGATCAAATACCTTTATTTGTTTGAGCAGTAGGTATTACAGCTCTTCTTTTATTACTTTCTTTACCAGTTTTAGCAGGAGCAATTACTATACTTCTTACTGATCGAAATTTAAATACTTCTTTTTTTGATCCTGCTGGTGGAGGAGACCCTATTTTATATCAACATTTATTT")

# Global alignment of sequences
global_alignment <- pwalign::pairwiseAlignment(seq_west, seq_east, type = "global")
print(global_alignment)

# Load the monarch butterflies data from the .tsv file
monarch_data <- read.delim(file = "C:/Users/dhruv/OneDrive/Desktop/assignment3.6210/data/Denaus.tsv", header = TRUE, sep = "\t")

# Data Cleaning and Categorization
monarch_data_clean <- monarch_data %>%
  drop_na(lon, lat) %>%  # Removes rows with missing coordinates
  mutate(region = ifelse(lon < -100, "Western", "Eastern"))  # Categorize butterflies by longitude

# Summarize Data by Region
summary_data <- monarch_data_clean %>%
  group_by(region) %>%
  summarise(count = n(), avg_lat = mean(lat), avg_lon = mean(lon))
print(summary_data)

# Improved Statistical Test: T-test to Compare Latitude Distributions Between Regions
# Conducting a t-test to see if there’s a significant difference in latitude between Eastern and Western monarchs
latitude_ttest <- t.test(lat ~ region, data = monarch_data_clean)
print(latitude_ttest)

# Visualization 1: Geographical Distribution of Monarch Butterflies Worldwide
ggplot() +
  geom_sf(data = ne_countries(scale = "medium", returnclass = "sf"), fill = "lightblue") +  # Base map
  geom_sf(data = st_as_sf(monarch_data_clean, coords = c("lon", "lat"), crs = 4326), aes(color = region), size = 2, alpha = 0.6) +
  labs(title = "Geographical Distribution of Monarch Butterflies Worldwide", color = "Region") +
  theme_minimal()

# Visualization 2: Comparison of Eastern and Western Monarch Butterflies
ggplot(monarch_data_clean, aes(x = lon, y = lat, color = region)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~region) +
  labs(
    title = "Comparison of Eastern and Western Monarch Butterflies",
    x = "Longitude",
    y = "Latitude",
    color = "Region"
  ) +
  theme_minimal()

# Visualization 3: Latitude Density for Eastern and Western Monarchs
ggplot(monarch_data_clean, aes(x = lat, fill = region)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Latitude Density of Eastern and Western Monarch Butterflies",
    x = "Latitude",
    y = "Density",
    fill = "Region"
  ) +
  theme_minimal()

# Visualization 4: Habitat Comparison - Park vs. Forest for Western Monarchs
# Filtering for park and forest habitats
filtered_data <- monarch_data_clean %>%
  filter(!is.na(habitat) & habitat %in% c("Park", "Forest"))

# Bar plot comparing Western Monarch Butterflies in parks and forests
ggplot(filtered_data, aes(x = habitat, fill = habitat)) +
  geom_bar() +
  labs(
    title = "Comparison of Western Monarch Butterflies in Parks and Forests",
    x = "Habitat Type",
    y = "Total Butterflies"
  ) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

