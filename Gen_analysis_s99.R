## This script is for running calculations on Allelic diversity, heterozygosity and genetic similarity on the 
# ecoc9(z) cluster. 
#
##### Allelic diversity + heterozygosity ####
#---------------------------------------------------------------------------------------------#
#### Preparation ####
library(dplyr)
library(tidyr)
library(stringr)
#### Preparation ####
# Define the number of replicates
num_replicates <- 100

## Genetic output
# Initialize an empty list to store the genetic outputs
gen_out <- data.frame(matrix(ncol=120))

# Loop through each replicate
for (replicate in 0:(num_replicates - 1)) { # replicates 0-9
  # Read in genetic output
  gen_out1 <- read.table(paste0("Outputs/Batch99_Sim1_Land1_Rep", replicate, "_Genetics.txt"), header = TRUE)
  # merge
  gen_out <- merge(gen_out, gen_out1, all = T)
}

## Individuals output
ind_out <- data.frame(matrix(ncol=13))
t<-read.table("Outputs/Batch99_Sim1_Land1_Rep0_Inds.txt")
# Loop through each replicate
for (replicate in 0:(num_replicates - 1)) { # replicates 0-9
  # Read in genetic output
  ind_out1 <- read.table(paste0("Outputs/Batch99_Sim1_Land1_Rep", replicate, "_Inds.txt"), header = TRUE)
  # merge
  ind_out <- merge(ind_out, ind_out1, all = T)
}

# I need to exclude individuals with status = 9 (exceeded maximum age), as they are not included in the genetics output. Ind_out and gen_out need to match regarding the individuals
ind_out <- ind_out %>%
  filter(Status >= 0 & Status <= 8)

# Merge genetic + individuals output
# Add the patch ID of each individual in the gen_out to the respective column in new df
gen_ind_out_s99 <- gen_out %>%
  left_join(dplyr::select(ind_out, IndID, Rep, Year, PatchID, Natal_patch), by = c("IndID", "Year", "Rep"))
# save
gen_ind_out_s99[121:240] <- NULL
save(gen_ind_out_s99, file = "data/gen_ind_out_s99.RData")


#---------------------------------------------------------------------------------------#
#### Genetic similarity ####
#### Preparation ####
library(poppr) # includes package "adegenet" needed for creating genind and genpop objects for dist.genpop()
library(purrr)

# Load data
load("data/gen_ind_out_s99.RData")
# subset data and drop all rows with PatchID = 0 (currently not in a population)
filtered_data_s99 <- gen_ind_out_s99[!(gen_ind_out_s99$PatchID==0),]

## Splitting by by Year + replicate number
# Initialize a list to store filtered data for each year and replicate
filtered_data_list <- list()

# Loop through each year and replicate number
for (year in c(20, 210)) {
  for (rep in 0:(num_replicates - 1)) { # replicates 0-9
    # Print the filtering condition
    cat("Filtering Condition - Year:", year, "Replicate:", rep, "\n")
    
    # Filter data for the current year and replicate, excluding NA values
    filtered_data_year_rep <- filtered_data_s99[!is.na(filtered_data_s99$Year) & 
                                                  !is.na(filtered_data_s99$Rep) &
                                                  filtered_data_s99$Year == year & 
                                                  filtered_data_s99$Rep == rep, ]
    
    # Print out the number of rows in the filtered data
    cat("Year:", year, "Replicate:", rep, "Rows:", nrow(filtered_data_year_rep), "\n")
    
    # Check if the filtered data is empty
    if (nrow(filtered_data_year_rep) == 0) {
      print(paste("No data found for Year", year, "Replicate", rep))
    }
    
    # Create a name for the filtered data based on year and replicate
    data_name <- paste0("filtered_data", year, "_", rep)
    
    # Store the filtered data in the list
    filtered_data_list[[data_name]] <- filtered_data_year_rep
  }
}


## Now first create a so called genind object from our filtered data. For this we only want the Allele information from columns 5 & 6

# Initialize lists to store gen_data objects
gen_data_list_20 <- list()
gen_data_list_210 <- list()

# Loop through each year and replicate number for Year 20
for (rep in 0:(num_replicates - 1)) { # replicates 0-99
  # Get filtered data for Year 20 and the current replicate
  filtered_data_rep <- filtered_data_list[[paste0("filtered_data20_", rep)]]
  
  # Identify columns containing allele data
  allele_columns <- 5:120  # Assuming columns 5 to 120 contain allele data
  
  tryCatch({
    # Print dimensions before conversion
    cat("Dimensions before conversion in replicate", rep, ":", dim(filtered_data_rep[, allele_columns]), "\n")
    
    # Convert non-character entries to character
    filtered_data_rep[, allele_columns] <- lapply(filtered_data_rep[, allele_columns], as.character)
    
    # Print dimensions after conversion
    cat("Dimensions after conversion in replicate", rep, ":", dim(filtered_data_rep[, allele_columns]), "\n")
    
    # Create gen_data object for Year 20 and the current replicate
    gen_data <- df2genind(as.data.frame(filtered_data_rep[, c(5:120)]), sep = ":")
    
    # Assign population information to genind object
    pop(gen_data) <- filtered_data_rep$PatchID
    
    # Store gen_data object in the list
    gen_data_list_20[[paste0("gen_data20_", rep)]] <- gen_data
  }, error = function(e) {
    cat("An error occurred in replicate", rep, ": ", conditionMessage(e), "\n")
    # Skip to the next iteration
    next
  })
}

# Loop through each year and replicate number for Year 210
for (rep in 0:(num_replicates - 1)) { # replicates 0-99
  # Get filtered data for Year 20 and the current replicate
  filtered_data_rep <- filtered_data_list[[paste0("filtered_data210_", rep)]]
  
  # Identify columns containing allele data
  allele_columns <- 5:120  # Assuming columns 5 to 120 contain allele data
  
  tryCatch({
    # Print dimensions before conversion
    cat("Dimensions before conversion in replicate", rep, ":", dim(filtered_data_rep[, allele_columns]), "\n")
    
    # Convert non-character entries to character
    filtered_data_rep[, allele_columns] <- lapply(filtered_data_rep[, allele_columns], as.character)
    
    # Print dimensions after conversion
    cat("Dimensions after conversion in replicate", rep, ":", dim(filtered_data_rep[, allele_columns]), "\n")
    
    # Create gen_data object for Year 210 and the current replicate
    gen_data <- df2genind(as.data.frame(filtered_data_rep[, c(5:120)]), sep = ":")
    
    # Assign population information to genind object
    pop(gen_data) <- filtered_data_rep$PatchID
    
    # Store gen_data object in the list
    gen_data_list_210[[paste0("gen_data210_", rep)]] <- gen_data
  }, error = function(e) {
    cat("An error occurred in replicate", rep, ": ", conditionMessage(e), "\n")
    # Skip to the next iteration
    next
  })
}

## Create genpop ibjects
# Initialize lists to store genpop objects for each year
gen_pop_data_list_20 <- list()
gen_pop_data_list_210 <- list()

# Loop through each genind object for Year 20
for (gen_data in gen_data_list_20) {
  # Create genpop object for Year 20
  gen_pop_data <- genind2genpop(gen_data)
  
  # Store genpop object in the list
  gen_pop_data_list_20 <- c(gen_pop_data_list_20, list(gen_pop_data))
}

# Loop through each genind object for Year 210
for (gen_data in gen_data_list_210) {
  # Create genpop object for Year 210
  gen_pop_data <- genind2genpop(gen_data)
  
  # Store genpop object in the list
  gen_pop_data_list_210 <- c(gen_pop_data_list_210, list(gen_pop_data))
}

##### Genetic Similarity ####
# Initialize empty lists to store genetic similarity matrices for Year 20 and Year 210
gensim_list_20 <- list()
gensim_list_210 <- list()

# Loop through each genpop object for Year 20
for (i in seq_along(gen_pop_data_list_20)) {
  gen_pop_data <- gen_pop_data_list_20[[i]]
  
  # Calculate Nei's genetic similarity matrix using matrix operations
  numerator <- gen_pop_data$tab %*% t(gen_pop_data$tab)
  denominator <- outer(sqrt(rowSums(gen_pop_data$tab^2)), sqrt(rowSums(gen_pop_data$tab^2)), "*")
  
  gen_sim_matrix <- numerator / denominator
  
  # Fill only the upper triangular part of the matrix
  #gen_sim_matrix[upper.tri(gen_sim_matrix)] <- NA
  
  # Store the genetic similarity matrix in the list for Year 20
  gensim_list_20[[i]] <- gen_sim_matrix
  
  # Create an object named gen_sim + the last parts of the genpop object name
  assign(paste0("gen_sim", sub("gen_pop_", "", names(gen_pop_data_list_20)[i])), gen_sim_matrix)
}

# Loop through each genpop object for Year 210
for (i in seq_along(gen_pop_data_list_210)) {
  gen_pop_data <- gen_pop_data_list_210[[i]]
  
  # Calculate Nei's genetic similarity matrix using matrix operations
  numerator <- gen_pop_data$tab %*% t(gen_pop_data$tab)
  denominator <- outer(sqrt(rowSums(gen_pop_data$tab^2)), sqrt(rowSums(gen_pop_data$tab^2)), "*")
  
  gen_sim_matrix <- numerator / denominator
  
  # Fill only the upper triangular part of the matrix
  #gen_sim_matrix[upper.tri(gen_sim_matrix)] <- NA
  
  # Store the genetic similarity matrix in the list for Year 210
  gensim_list_210[[i]] <- gen_sim_matrix
  
  # Create an object named gen_sim + the last parts of the genpop object name
  assign(paste0("gen_sim", sub("gen_pop_", "", names(gen_pop_data_list_210)[i])), gen_sim_matrix)
}

### Transform matrices to dfs
## First Year
gen_sim_20_df <- reshape2::melt(gensim_list_20)
colnames(gen_sim_20_df) <- c("Population1", "Population2", "Gen_Similarity", "Replicate")

# Initialize an empty list to store melted data frames
melted_df_list_20 <- list()

# Loop through each matrix in the list
for (i in seq_along(gensim_list_20)) {
  # Extract matrix values
  mat_values <- gensim_list_20[[i]]
  
  # Use melt to convert the matrix to a long-format data frame
  melted_df <- reshape2::melt(mat_values)
  
  # Filter rows where Pop1 is not equal to Pop2 (exclude Similarity between same population)
  melted_df <- melted_df[melted_df$Var1 != melted_df$Var2, ]
  
  # Store the melted data frame in the list
  melted_df_list_20[[i]] <- melted_df
}
# Now I have one large df for all replicates, I want to split them and store the respective dfs (per replicate) in a list for further processing (I am lazy and want to use the same code for aggregation as I have for the distance objects)

for(i in seq_along(melted_df_list_20)) {
  # Access data frame
  melted_df <- melted_df_list_20[[i]]
  # Rename columns
  colnames(melted_df) <- c("Population1", "Population2", "Gen_Similarity")
  
  # Assign
  melted_df_list_20[[i]] <- melted_df
}

# Access the melted data frames using melted_df_list or individual objects
print(melted_df_list_20)

## Last Year
gen_sim_210_df <- reshape2::melt(gensim_list_210)
colnames(gen_sim_210_df) <- c("Population1", "Population2", "Gen_Similarity", "Replicate")

# Initialize an empty list to store melted data frames
melted_df_list_210 <- list()

# Loop through each matrix in the list
for (i in seq_along(gensim_list_210)) {
  # Extract matrix values
  mat_values <- gensim_list_210[[i]]
  
  # Use melt to convert the matrix to a long-format data frame
  melted_df <- reshape2::melt(mat_values)
  
  # Filter rows where Pop1 is not equal to Pop2
  melted_df <- melted_df[melted_df$Var1 != melted_df$Var2, ]
  
  # Store the melted data frame in the list
  melted_df_list_210[[i]] <- melted_df
}
# Now I have one large df for all replicates, I want to split them and store the respective dfs (per replicate) in a list for further processing (I am lazy and want to use the same code for aggregation as I have for the distance objects)

for(i in seq_along(melted_df_list_210)) {
  # Access data frame
  melted_df <- melted_df_list_210[[i]]
  # Rename columns
  colnames(melted_df) <- c("Population1", "Population2", "Gen_Similarity")
  
  # Assign
  melted_df_list_210[[i]] <- melted_df
}

# Access the melted data frames using melted_df_list or individual objects
print(melted_df_list_210)

## Now to aggregation
# First year
# Bind all dfs and calculate average genetic distance
gen_sim_df20_s99 <- melted_df_list_20 %>%
  bind_rows() %>% # Combine to single df
  group_by(Population1, Population2) %>%
  summarize(Avg_Gen_sim = mean(Gen_Similarity))
# save
save(gen_sim_df20_s99, file = "data/s99_gen_sim_df20.Rdata")

# Last year
# Bind all dfs and calculate average genetic distance
gen_sim_df210_s99 <- melted_df_list_210 %>%
  bind_rows() %>% # Combine to single df
  group_by(Population1, Population2) %>%
  summarize(Avg_Gen_sim = mean(Gen_Similarity))
# save
save(gen_sim_df210_s99, file = "data/s99_gen_sim_df210.Rdata")

