## In this script I define a sequence for source/receiving populations for my transloction scenario, so that source/receiving population are alternating
##### Genetically least similar ######
library(dplyr)
library(tidyr)
load("data/s9_gen_sim_df20.Rdata")
load("data/s9_gen_sim_df210.Rdata")
# Step 1: filter out only original populations
gen_sim_y20_op <- gen_sim_df20_s9 %>%
  filter(Population1 %in% c(6, 4, 15, 23, 109, 22, 24, 83, 2, 73) &
           Population2 %in% c(6, 4, 15, 23, 109, 22, 24, 83, 2, 73))

# Step 2: order by ascending value of genetic similarity
gen_sim_y20_op <- gen_sim_y20_op %>%
  arrange(Avg_Gen_sim)
# remove every other (duplicate) row
gen_sim_y20_op <- gen_sim_y20_op[seq(1, nrow(gen_sim_y20_op), by = 2),]
# save
save(gen_sim_y20_op, file = "data/gen_sim_y20_op.RData")

# Step 3: Generate a sequence of translocation pairs alternating between source and recipient populations
# Get unique populations
populations <- sort(unique(c(gen_sim_y20_op$Population1, gen_sim_y20_op$Population2)))

genetic_similarity <- gen_sim_y20_op
# Initialize source population sequence
recipient_source_comb <- list()

# Generate source population sequence for each recipient population
for (pop in populations) {
  # Filter genetic similarity data for the current recipient population as Population1 or Population2
  pop_similarities <- subset(genetic_similarity, Population1 == pop | Population2 == pop)
  
  # Extract unique source populations
  source_populations <- unique(c(pop_similarities$Population1, pop_similarities$Population2))
  
  # Remove the recipient population itself from the source populations
  source_populations <- source_populations[source_populations != pop]
  
  # Ensure that each recipient has exactly 9 source populations
  n_sources <- length(source_populations)
  if (n_sources < 9) {
    missing_sources <- populations[!populations %in% source_populations]
    source_populations <- c(source_populations, missing_sources[1:(9 - n_sources)])
  } else if (n_sources > 9) {
    source_populations <- source_populations[1:9] # Limit to the first 9 source populations
  }
  
  # Store the ordered source populations for the current recipient population
  recipient_source_comb[[as.character(pop)]] <- source_populations
}

# Print the source population sequence
for (pop in populations) {
  cat("Recipient Population:", pop, "\n")
  cat("Source Populations:", paste(recipient_source_comb[[as.character(pop)]], collapse = ","), "\n\n")
}

## Now transform the outcome into two strings: one for recipient and one for source
# Recipient
recipient_sequence <- character()
# Repeat recipient population sequence 10 times each
for (i in 1:9) {
  # Append recipient population sequence
  recipient_sequence <- c(recipient_sequence, populations)
}

# Source population
# Convert the list of 10 lists into a continuous string
source_sequence <- unlist(lapply(seq_along(recipient_source_comb[[1]]), function(i) {
  paste(unlist(lapply(recipient_source_comb, function(x) x[i])), collapse = ",")
}), use.names = FALSE)

source_sequence <- paste(source_sequence, collapse = ",")
length(unlist(strsplit(source_sequence, ",")))
length(unlist(strsplit(recipient_sequence, ",")))
recipient_sequence <- paste(recipient_sequence, collapse = ",")
recipient_sequence
source_sequence

#---------------------------------------------------------------
#### Exclude small populations (Rothaar, Romnicka, Janowski, Augustowska)

# Initialize source population sequence
recipient_source_comb2 <- list()

# Generate source population sequence for each recipient population
for (pop in populations) {
  # Filter genetic similarity data for the current recipient population as Population1 or Population2
  pop_similarities <- subset(genetic_similarity, Population1 == pop | Population2 == pop)
  
  # Extract unique source populations
  source_populations <- unique(c(pop_similarities$Population1, pop_similarities$Population2))
  
  # Remove the populations to be excluded
  source_populations <- source_populations[!source_populations %in% c(4, 83, 2, 73)]
  
  # Remove the recipient population itself from the source populations
  source_populations <- source_populations[source_populations != pop]
  
  # Ensure that each recipient has exactly 9 source populations
  n_sources <- length(source_populations)
  if (n_sources < 5) {
    missing_sources <- populations[!populations %in% source_populations]
    source_populations <- c(source_populations, missing_sources[1:(5 - n_sources)])
  } else if (n_sources > 5) {
    source_populations <- source_populations[1:5] # Limit to the first 9 source populations
  }
  
  # Store the ordered source populations for the current recipient population
  recipient_source_comb2[[as.character(pop)]] <- source_populations
}

# Convert the list of 10 lists into a continuous string
source_sequence2 <- unlist(lapply(seq_along(recipient_source_comb2[[1]]), function(i) {
  paste(unlist(lapply(recipient_source_comb2, function(x) x[i])), collapse = ",")
}), use.names = FALSE)

# Combine into a single string separated by ","
source_sequence2 <- paste(source_sequence2, collapse = ",")


source_sequence2
length(strsplit(source_sequence2, ",")[[1]])
recipient_sequence
length(strsplit(recipient_sequence, ",")[[1]])
