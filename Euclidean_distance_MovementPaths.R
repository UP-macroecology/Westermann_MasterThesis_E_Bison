dirpath <- ""
steps0 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep0_MovePaths.txt"), header = T)
steps0$Rep <- 0
steps1 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep1_MovePaths.txt"), header = T)
steps1$Rep <- 1
steps2 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep2_MovePaths.txt"), header = T)
steps2$Rep <- 2
steps3 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep3_MovePaths.txt"), header = T)
steps3$Rep <- 3
steps4 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep4_MovePaths.txt"), header = T)
steps4$Rep <- 4
steps5 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep5_MovePaths.txt"), header = T)
steps5$Rep <- 5
steps6 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep6_MovePaths.txt"), header = T)
steps6$Rep <- 6
steps7 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep7_MovePaths.txt"), header = T)
steps7$Rep <- 7
steps8 <- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep8_MovePaths.txt"), header = T)
steps8$Rep <- 8
steps9<- read.table(paste0(dirpath,"Outputs/Batch9_Sim1_Land1_Rep9_MovePaths.txt"), header = T)
steps9$Rep <- 9

steps <- merge(steps0, steps1, all = T)
steps <- merge(steps, steps2, all = T)
steps <- merge(steps, steps3, all = T)
steps <- merge(steps, steps4, all = T)
steps <- merge(steps, steps5, all = T)
steps <- merge(steps, steps6, all = T)
steps <- merge(steps, steps7, all = T)
steps <- merge(steps, steps8, all = T)
steps <- merge(steps, steps9, all = T)

rm(steps0, steps1, steps2, steps3, steps4, steps5, steps6, steps7, steps8, steps9)
head(steps)

# Load necessary libraries
library(dplyr)

# Function to calculate Euclidean distance between initial and final positions
calculate_distance <- function(data, max_step_females, max_step_males) {
  # Find starting and end positions for each individual
  start_positions <- data %>%
    filter(Status == 0) %>%
    select(IndID, Rep, x_start = x, y_start = y)
  
  end_positions <- data %>%
    group_by(IndID, Rep) %>%
    filter(Step == max(data$Step)) %>%
    ungroup() %>%
    select(IndID, Rep, x_end = x, y_end = y)
  
  # Filter dispersers with maximum steps for males and females
  max_steps_females <- data %>%
    #filter(Status == 6) %>%
    group_by(IndID, Rep) %>%
    filter(Step == max_step_females) %>%
    ungroup() %>%
    left_join(start_positions, by = c("IndID", "Rep")) %>%
    left_join(end_positions, by = c("IndID", "Rep"))
  
  max_steps_males <- data %>%
    #filter(Status == 6) %>%
    group_by(IndID, Rep) %>%
    filter(Step == max_step_males) %>%
    ungroup() %>%
    left_join(start_positions, by = c("IndID", "Rep")) %>%
    left_join(end_positions, by = c("IndID", "Rep"))
  
  # Calculate distances for females and males
  distance_female <- sqrt((max_steps_females$x_start - max_steps_females$x)^2 + 
                            (max_steps_females$y_start - max_steps_females$y)^2)
  
  distance_male <- sqrt((max_steps_males$x_start - max_steps_males$x)^2 + 
                          (max_steps_males$y_start - max_steps_males$y)^2)
  
  return(list(distance_female = distance_female, distance_male = distance_male))
}


# Define maximum steps for females and males from the model
max_step_females <- 36
max_step_males <- 2000

distances <- calculate_distance(steps, max_step_females, max_step_males)


# Output results
print("Euclidean distance for females:")
summary(distances$distance_female)
print("Euclidean distance for males:")
summary(distances$distance_male)

hist(distances$distance_male)
hist(distances$distance_female)
#####################################

# Function to calculate Euclidean distance between Step 1 and Step 2 positions
calculate_distance_step1_step2 <- function(data) {
  # Filter Step 1 and Step 2 positions for each individual
  step1_positions <- data %>%
    filter(Step == 0) %>%
    select(IndID, x_step1 = x, y_step1 = y)
  
  step2_positions <- data %>%
    filter(Step == 1) %>%
    select(IndID, x_step2 = x, y_step2 = y)
  
  # Join Step 1 and Step 2 positions
  positions <- left_join(step1_positions, step2_positions, by = "IndID")
  
  # Calculate distances
  distances <- sqrt((positions$x_step1 - positions$x_step2)^2 + 
                      (positions$y_step1 - positions$y_step2)^2)
  
  return(distances)
}

# Calculate distances between Step 1 and Step 2 positions
distances_step1_step2 <- calculate_distance_step1_step2(steps)

# Output results
print("Summary of Euclidean distances between Step 1 and Step 2 positions:")
print(summary(distances_step1_step2)) # when individuals move diagonally through a cell, they cover a greater distance

########### Movement paths ################
# read output file
#rm(list = ls())
#gc()
steps_mp <- read.table(paste0(dirpath,"Outputs/Batch99_Sim1_Land1_Rep3_MovePaths.txt"), header = T)
head(steps_mp)

# we ask how often a step was logged per individual
hist(c(table(steps_mp$IndID)),xlab='Number of steps',main='Steps per dispersing individual')

#### Females ####
# find all individuals with a 36-step path:
Inds <- names(which(table(steps_mp$IndID) > 35))
steps_36 <- subset(steps_mp, IndID %in% Inds)

# look at first individual with a starting path
(steps_i <- subset(steps_mp, IndID == subset(steps_mp, Step == 0)[1,"IndID"]))
#(steps_i <- subset(steps, IndID == subset(steps_36, Step < 37)[1,"IndID"]) )

# Plot movement paths
# find all dispersing individuals of the years xx - xx of the first map
steps_map1 <- subset(steps_mp, Year > 200 & Year <= 210) # Only a 10 year time span to keep the amount of data as small as possible, can be checked for every time period of the model
Inds <- subset(steps_map1, Status == 6, select = IndID)  # Trajectory contains start of path

Inds_outcome <- sapply(Inds$IndID, function(Ind) {
  steps_i <- subset(steps_map1, IndID == Ind)
  outcome <- steps_i$Status[nrow(steps_i)]
  if (outcome == 6 && any(steps_i$Step == 36 & steps_i$Status == 6)) {
    return("female")
  } else {
    return("male")
  }
})


# Create a data frame
Inds <- data.frame(IndID = Inds, Outcome = Inds_outcome)

# load raster of first map
patchFilename <- "disp_costs.asc"
map1 <- terra::rast(paste0(dirpath,"Inputs/",patchFilename[1]))
# multiply x and y counts by map resolution
steps_map1[,c(4,5)] <- steps_map1[,c(4,5)]*res(map1)

mycol_terrain <- c("#D0C096","#E2E2E2","#027C1E","#97A753","darkred")
# We use ggplot to plot the movement paths
pathplot_1 <- ggplot()+
  #geom_tile(data = as.data.frame(map1, xy = TRUE), aes(x = x, y = y)) + # plotting with basemap doesn't work yet and I haven't figured out how to make it work properly...
  geom_path(data = steps_map1[steps_map1$IndID %in% Inds[Inds$Outcome == "male",1],], aes(x = x, y = y, group = factor(IndID)), col = "mediumorchid", alpha = .5, linewidth = 1) +
  geom_path(data = steps_map1[steps_map1$IndID %in% Inds[Inds$Outcome == "female",1],], aes(x = x, y = y, group = factor(IndID)), col = "red", alpha = .5, linewidth = 1) +
  scale_fill_manual(breaks = 1:5, values = mycol_terrain) +
  guides(fill = "none", color = "none") +
  theme_void()  
#theme(aspect.ratio = 1)
pathplot_1
