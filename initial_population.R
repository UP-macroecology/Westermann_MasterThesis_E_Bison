##### INITIAL POPULATION
## use numbers from EBPB 2021, and create txt file with 1/2 of these nr (as female-only) per population 
## UPDATE PATCH ID depending on patch file used (varying min patch size, and human pressure classes lead to variation in patch IDs!)
# total population Borecka [EBPB 2022]: 125
# total population Augustowska [EBPB 2022]: 20
# total population Knyszyn [EBPB 2022]: 212
# total population Bialowieza [EBPB 2022]: 779
# total population Bieszczady [EBPB 2022]: 729
# total population Western Poland [EBPB 2022]: 340
# total population Rothaar [EBPB 2022]: 24
# required format: Year Species PatchID Ninds Age Stage
# Define the relative proportion of individuals in each age class
# I have stage-structured model with 4 stages: calves/newborns, juveniles, reproductive adults, senescent adults
# but cannot initialize calves! so specify only 3 stages here

dirpath <- ""
# Define the age ranges for each age class
age_class1_range1 <- c(0:3)
age_class2_range1 <- c(4:20)
age_class3_range1 <- c(21:25)

age_class1_range2 <- c(0:1)
age_class2_range2 <- c(1:3)
age_class3_range2 <- c(4:20)
age_class4_range2 <- c(21:25)

## Draw initial ages from an exponential distribution
# Set parameters
max_age <- 25
lambda <- 0.2  # Adjust the lambda parameter based on the desired distribution shape; lambda=0.2 works well for value range 1-25 

patchID_borecka <- 6
patchID_augustowska <- 4
patchID_knyszyn <- 15
patchID_bialowieza <- 23
patchID_bieszczady <- 109
patchID_west_poland_mainpatch <- 22 ## --> ca 90% (if 2 patches)
patchID_west_poland_patch2 <- 24    ## --> ca 10% (if 2 patches)
patchID_rothaar <- 83
patchID_romincka <- 2
patchID_janowskie <- 73



#### BORECKA ####
# total population Borecka [EBPB 2022]: 127
total_pop_borecka <- 127 
## take 50% of population as I have female-only model (maybe need to change this to 60% or so? Some populations have clear female-skew)
# I have stage-structured model with 4 stages: calves/newborns, juveniles, reproductive adults, senescent adults
# but cannot initialize calves! so specify only 3 stages here
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 0.5 to avoid age 0 and have min age 1)
## round to integer
ages <- round(rexp(round(total_pop_borecka/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages[ages > max_age] <- sample(21:25, size = sum(ages > max_age), replace = TRUE)
## check distribution
table(ages)
hist(ages)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
borecka_pop1 <- data.frame(as.data.frame(table(ages))) %>%
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_borecka)
## 5 stage model
borecka_pop2 <- data.frame(as.data.frame(table(ages))) %>%
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_borecka)

# # # Check the distribution of ages
# # hist(ages, breaks=seq(1, 25, by=4), main="Age Distribution", xlab="Age")
# # Create a data frame
# ages_df <- data.frame(age = age)
# # Calculate the number of individuals for each age
# age_counts <- table(ages_df$age)
# age_counts_df <- data.frame(age = as.numeric(names(age_counts)), count = as.numeric(age_counts))
# ## get percentages in different age/stage classes
# # Calculate percentages for each age class
# percentage_age_class_1 <- sum(age_counts_df$count[age_counts_df$age %in% age_class1_range]) / total_population_size * 100
# percentage_age_class_2 <- sum(age_counts_df$count[age_counts_df$age %in% age_class2_range]) / total_population_size * 100
# percentage_age_class_3 <- sum(age_counts_df$count[age_counts_df$age %in% age_class3_range]) / total_population_size * 100
# # Print the percentages
# cat("Percentage for ages 1-3: ", percentage_age_class_1, "%\n")
# cat("Percentage for ages 4-20: ", percentage_age_class_2, "%\n")
# cat("Percentage for ages 21-25: ", percentage_age_class_3, "%\n")
# hist(ages)


#### AUGUSTOWSKA ####
# total population Augustowska [EBPB 2022]: 20
total_pop_augustowska <- 23
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages <- round(rexp(round(total_pop_augustowska/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages[ages > max_age] <- sample(21:25, size = sum(ages > max_age), replace = TRUE)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
augustowska_pop1 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_augustowska) %>% 
  dplyr::relocate(Year, Species, PatchID)
## 5 stage model
augustowska_pop2 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_augustowska) %>% 
  dplyr::relocate(Year, Species, PatchID)

#### JANOWSKIE ####
# total population janowskie [EBPB 2022]: 9
total_pop_janowskie <- 9
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages <- round(rexp(round(total_pop_janowskie/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages[ages > max_age] <- sample(21:25, size = sum(ages > max_age), replace = TRUE)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
janowskie_pop1 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_janowskie) %>% 
  dplyr::relocate(Year, Species, PatchID)
## 5 stage model
janowskie_pop2 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_janowskie) %>% 
  dplyr::relocate(Year, Species, PatchID)


#### ROMinCKA ####
# total population Augustowska [EBPB 2022]: 20
total_pop_romincka <- 23
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages <- round(rexp(round(total_pop_romincka/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages[ages > max_age] <- sample(21:25, size = sum(ages > max_age), replace = TRUE)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
romincka_pop1 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_romincka) %>% 
  dplyr::relocate(Year, Species, PatchID)
## 5 stage model
romincka_pop2 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_romincka) %>% 
  dplyr::relocate(Year, Species, PatchID)

#### KNYSZYN ####
# total population Knyszyn [EBPB 2022]: 298
total_pop_knyszyn <- 298
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages <- round(rexp(round(total_pop_knyszyn/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages[ages > max_age] <- sample(21:25, size = sum(ages > max_age), replace = TRUE)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
knyszyn_pop1 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_knyszyn) %>% 
  dplyr::relocate(Year, Species, PatchID)
## 5 stage model
knyszyn_pop2 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_knyszyn) %>% 
  dplyr::relocate(Year, Species, PatchID)



#### BIALOWIEZA ####
# total population Bialowieza [EBPB 2022]: 829
total_pop_bialowieza <- 829
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages <- round(rexp(round(total_pop_bialowieza/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages[ages > max_age] <- sample(21:25, size = sum(ages > max_age), replace = TRUE)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
bialowieza_pop1 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_bialowieza) %>% 
  dplyr::relocate(Year, Species, PatchID)
## 5 stage model
bialowieza_pop2 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_bialowieza) %>% 
  dplyr::relocate(Year, Species, PatchID)


#### BIESZCZADY ####
# total population Bieszczady [EBPB 2022]: 750
total_pop_bieszczady <- 750
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages <- round(rexp(round(total_pop_bieszczady/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages[ages > max_age] <- sample(21:25, size = sum(ages > max_age), replace = TRUE)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
bieszczady_pop1 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_bieszczady) %>% 
  dplyr::relocate(Year, Species, PatchID)
## 5 stage model
bieszczady_pop2 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_bieszczady) %>% 
  dplyr::relocate(Year, Species, PatchID)


#### WESTERN POLAND ####
## IN MOST PATCH VERSIONS THERE ARE 2-3 PATCHES OCCUPIED!!
## one main patch (ca. 75% of pop) and 1-2 (or now more!) other patches
# total population Western Poland [EBPB 2022]: 340
total_pop_west_poland <- 340

## For medium patch version with 3 patches WestPL
pop_west_poland_mainpatch <- total_pop_west_poland*0.75
pop_west_poland_patch2 <- total_pop_west_poland*0.15
pop_west_poland_patch3 <- total_pop_west_poland*0.1


## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages_mainpatch <- round(rexp(round(pop_west_poland_mainpatch/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages_mainpatch[ages_mainpatch > max_age] <- sample(21:25, size = sum(ages_mainpatch > max_age), replace = TRUE)
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages_patch2 <- round(rexp(round(pop_west_poland_patch2/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages_patch2[ages_patch2 > max_age] <- sample(21:25, size = sum(ages_patch2 > max_age), replace = TRUE)
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages_patch3 <- round(rexp(round(pop_west_poland_patch3/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages_patch3[ages_patch3 > max_age] <- sample(21:25, size = sum(ages_patch3 > max_age), replace = TRUE)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
west_poland_pop1 <- data.frame(as.data.frame(table(ages_mainpatch))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages_mainpatch) %>%
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_west_poland_mainpatch) %>% 
  dplyr::relocate(Year, Species, PatchID)

if(patchID_west_poland_patch2>0){
  west_poland_pop1 <- rbind(west_poland_pop1,
                           data.frame(as.data.frame(table(ages_patch2))) %>% 
                             dplyr::relocate(Ninds = Freq) %>% 
                             dplyr::rename(Age = ages_patch2) %>%
                             mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                                                   ifelse(Age %in% age_class2_range1, 2, 3))) %>%
                             mutate(Year = 0, Species = 0, PatchID = patchID_west_poland_patch2) %>% 
                             dplyr::relocate(Year, Species, PatchID))
}
## 5 stage model
west_poland_pop2 <- data.frame(as.data.frame(table(ages_mainpatch))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages_mainpatch) %>%
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_west_poland_mainpatch) %>% 
  dplyr::relocate(Year, Species, PatchID)

if(patchID_west_poland_patch2>0){
  west_poland_pop2 <- rbind(west_poland_pop2,
                           data.frame(as.data.frame(table(ages_patch2))) %>% 
                             dplyr::relocate(Ninds = Freq) %>% 
                             dplyr::rename(Age = ages_patch2) %>%
                             mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                                                   ifelse(Age %in% age_class2_range2, 2, 
                                                          ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
                             mutate(Year = 0, Species = 0, PatchID = patchID_west_poland_patch2) %>% 
                             dplyr::relocate(Year, Species, PatchID))
}

#if(patchID_west_poland_patch3>0){
 # west_poland_pop <- rbind(west_poland_pop,
  #                         data.frame(as.data.frame(table(ages_patch3))) %>% 
   #                          dplyr::relocate(Ninds = Freq) %>% 
    #                         dplyr::rename(Age = ages_patch3) %>%
     #                        mutate(Stage = ifelse(Age %in% age_class1_range, 1, 
      #                                             ifelse(Age %in% age_class2_range, 2, 
       #                                                   ifelse(Age %in% age_class3_range, 3, 4)))) %>%
        #                     mutate(Year = 0, Species = 0, PatchID = patchID_west_poland_patch3) %>% 
         #                    dplyr::relocate(Year, Species, PatchID))
#}


#### ROTHAAR ####
# total population Rothaar [EBPB 2022]: 30
total_pop_rothaar <- 30
## set.seed before random sampling
set.seed(1909)
# Generate ages from an exponential distribution (add 1 to avoid age 0 and have min age 1)
## round to integer
ages <- round(rexp(round(total_pop_rothaar/2), rate = lambda) + 0.5)
# Randomly assign ages between 21 and 25 (stage 4) to individuals exceeding the maximum age
ages[ages > max_age] <- sample(21:25, size = sum(ages > max_age), replace = TRUE)
# required format: Year Species PatchID Ninds Age Stage
## 4 stage model
rothaar_pop1 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range1, 1, 
                        ifelse(Age %in% age_class2_range1, 2, 3))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_rothaar) %>% 
  dplyr::relocate(Year, Species, PatchID)
## 5 stage model
rothaar_pop2 <- data.frame(as.data.frame(table(ages))) %>% 
  dplyr::relocate(Ninds = Freq) %>% 
  dplyr::rename(Age = ages) %>% 
  mutate(Stage = ifelse(Age %in% age_class1_range2, 1, 
                        ifelse(Age %in% age_class2_range2, 2, 
                               ifelse(Age %in% age_class3_range2, 3, 4)))) %>%
  mutate(Year = 0, Species = 0, PatchID = patchID_rothaar) %>% 
  dplyr::relocate(Year, Species, PatchID)



initial_pop_df1 <- rbind(borecka_pop1, augustowska_pop1, knyszyn_pop1, bialowieza_pop1, bieszczady_pop1,
                        west_poland_pop1, rothaar_pop1, romincka_pop1, janowskie_pop1)

initial_pop_df2 <- rbind(borecka_pop2, augustowska_pop2, knyszyn_pop2, bialowieza_pop2, bieszczady_pop2,
                         west_poland_pop2, rothaar_pop2, romincka_pop2, janowskie_pop2)

initial_pop_df1 <- initial_pop_df1[,c("Year", "Species", "PatchID", "Ninds", "Age", "Stage")]
initial_pop_df2 <- initial_pop_df2[,c("Year", "Species", "PatchID", "Ninds", "Age", "Stage")]

write.table(initial_pop_df1, 
            file = "Inputs/init_pop1.txt",
            sep = "\t", quote=FALSE,
            row.names = FALSE)

write.table(initial_pop_df2, 
            file = "Inputs/init_pop2.txt",
            sep = "\t", quote=FALSE,
            row.names = FALSE)
