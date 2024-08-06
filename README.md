This repository contains all scripts necessary for my master thesis "Modelling genetic connectivity among European Bison populations in the fragmented Central European landscape". I conducted the study using RangeShiftR.

The main script is "Bison_bonasus_genetic_connectivity.Rmd". In this script I create all the modules needed to build and run RangeShiftR models. 
  - First all necessary modules are built (landscape, demography, genetics, dispersal, management, initialisation, simulation). In the parameter master all models built are saved under a unique batch number. Then models are run.
  - The scripts "Euclidean_distance_MovementPaths.R" and "Translocation_sequence.R" do not need to be run again, as the output was directly transferred to the respective section in the main script. I have mentioned the usage of these scripts in the respective chunks of code in the comments (dispersal and management, respectively).
  - Genetic output from the model output was processed to calculate allelic diversity in the script "Allelic_div_heterozygosity.R". In this script there are also calculations for heterozygosity which I did not use further (heterozygosity can therefore be ignored). The use of this script is again mentioned in the respective chunk in the main script.
  - Genetic output from the models was further processed to calculate measures of genetic connectivity in seperate scripts for each model I ran, respectively --> "Gen_analysis_sXX.R". The use of these script is again mentioned in the respective chunk in the main script.
  - After models are run, the script continues to load in the files generated for allelic diversity to visualise the output and run statistical tests. The same counts for files generated for genetic connectivity.
  - The change in genetic connectivity is further mapped for each model/scenario run.
  - Next, abundance and occupancy probabilities are plotted and compared statistically.
  - For allelic diversity, genetic connectivity, abundance and occupancy probability relative values were calculated to compare across scenarios.
  - The script finishes by producing some descriptive statistics for all important measurements  
  
For the base model, a sensitivity analysis was carried out "Sensitivity_analysis.Rmd". Which parameters were altered and by how much can be found at the top of the script.
