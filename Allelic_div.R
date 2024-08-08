# This script calculates allelic diversity (A) from the RangeShiftR output. Gen_analysis scripts need to be run before to generate gen_ind_out files.

library(tidyverse)
# List of file names
file_names <- c("gen_ind_out_s9.RData", "gen_ind_out_s66.RData", "gen_ind_out_s77.RData", "gen_ind_out_s78.RData", "gen_ind_out_s79.RData", "gen_ind_out_s80.RData", "gen_ind_out_s81.RData", "gen_ind_out_s99.RData")

# Define a named vector mapping PatchIDs to their names
patch_names <- c(
  "6" = "Borecka",
  "4" = "Augustowska",
  "15" = "Knyszyn",
  "23" = "Bialowieza",
  "109" = "Bieszczady",
  "22" = "West Poland 1",
  "24" = "West Poland 2",
  "83" = "Rothaar",
  "2" = "Romincka",
  "73" = "Janowskie"
)

# Create an empty list to store Allelic_div data frames
Allelic_div_list <- list()

# Loop through each file
for (file in file_names) {
  # Load the data
  load(paste("data/", file, sep = ""))
  
  # Get the loaded data by its name
  gen_out_data <- get(gsub("\\.RData", "", file))
  
  # Perform data transformations
  gen_out_long <- gen_out_data %>%
    dplyr::select(-Species) %>% # exclude species from table
    pivot_longer(
      cols = starts_with("Chr"),
      names_to = "Allele",
      values_to = "Allele_Value"
    )
  
  summary_gen_out <- gen_out_long %>%
    group_by(Year, Allele_Value, PatchID) %>%
    summarise(Frequency = n())
  
  years_frequencies <- summary_gen_out %>%
    group_by(Year, PatchID) %>%
    summarise(TotalFrequency = sum(Frequency))
  
  summary_gen_out <- summary_gen_out %>%
    left_join(years_frequencies, by = c("Year", "PatchID")) %>%
    mutate(Proportion = Frequency/TotalFrequency) %>%
    dplyr::select(Year, Allele_Value, PatchID, Frequency, Proportion)
  
  Allelic_div <- summary_gen_out %>%
    group_by(Year, PatchID) %>%
    summarise(A = sum(n_distinct(Allele_Value))/58)
  
  # Add a new column with the actual names
  Allelic_div <- Allelic_div %>%
    mutate(Patch_Name = patch_names[as.character(PatchID)])
  
  # Add Allelic_div to the list with the file name as the name
  Allelic_div_list[[file]] <- Allelic_div
}

# Save the list of Allelic_div data frames
save(Allelic_div_list, file = "data/Allelic_div_list.RData")

