This repository contains all scripts necessary for my master thesis "Modelling genetic connectivity among European Bison populations in the fragmented Central European landscape". I conducted the study using RangeShiftR.

The main script is "Bison_bonasus_genetic_connectivity.Rmd". In this script I create all the modules needed to build and run RangeShiftR models. 
  - The scripts "Euclidean_distance_MovementPaths.R" and "Translocation_sequence.R" do not need to be run again, as the output was directly transferred to the respective section in the main script. I have mentioned the usage of these scripts in the respective chunks of code in the comments.
  - Genetic output from the model output was processed to calculate allelic diversity in the script "Allelic_div_heterozygosity.R". In this script there are also calculations for heterozygosity which I did not use further (heterozygosity can therefore be ignored). 
  - Genetic output from the models was further processed to calculate measures of genetic connectivity in seperate scripts for each model I ran, respectively --> "Gen_analysis_sXX.R"
  - 
  
