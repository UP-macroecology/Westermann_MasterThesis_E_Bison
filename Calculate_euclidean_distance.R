dirpath <- ""
# First load all Output files containing information on dispersal distances/steps
steps0 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep0_MovePaths.txt"), header = T)
steps0$Rep <- 0
steps1 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep1_MovePaths.txt"), header = T)
steps1$Rep <- 1
steps2 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep2_MovePaths.txt"), header = T)
steps2$Rep <- 2
steps3 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep3_MovePaths.txt"), header = T)
steps3$Rep <- 3
steps4 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep4_MovePaths.txt"), header = T)
steps4$Rep <- 4
steps5 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep5_MovePaths.txt"), header = T)
steps5$Rep <- 5
steps6 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep6_MovePaths.txt"), header = T)
steps6$Rep <- 6
steps7 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep7_MovePaths.txt"), header = T)
steps7$Rep <- 7
steps8 <- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep8_MovePaths.txt"), header = T)
steps8$Rep <- 8
steps9<- read.table(paste0(dirpath,"Outputs/Batch5_Sim1_Land1_Rep9_MovePaths.txt"), header = T)
steps9$Rep <- 9

# Merge to one file
steps <- merge(steps0, steps1, all = T)
steps <- merge(steps, steps2, all = T)
steps <- merge(steps, steps3, all = T)
steps <- merge(steps, steps4, all = T)
steps <- merge(steps, steps5, all = T)
steps <- merge(steps, steps6, all = T)
steps <- merge(steps, steps7, all = T)
steps <- merge(steps, steps8, all = T)
steps <- merge(steps, steps9, all = T)

# Remove files not needed anymore
rm(steps0, steps1, steps2, steps3, steps4, steps5, steps6, steps7, steps8, steps9)
head(steps)

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
max_step_males <- 2005

distances <- calculate_distance(steps, max_step_females, max_step_males)


# Output results
print("Euclidean distance for females:")
summary(distances$distance_female)
print("Euclidean distance for males:")
summary(distances$distance_male)

hist(distances$distance_male)
hist(distances$distance_female)