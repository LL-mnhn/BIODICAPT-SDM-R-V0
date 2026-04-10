##### Global Parameters #####
# These parameters are 'hidden': they can be modified, but you should not change 
# them without a good reason. 
#
# The parameters grouped in this file are re-used by several scripts, having 
# them all in the same place allows easier access, control and harmonization.


### Paths
RAW_DATA_PATH <- file.path("resources", "raw_data")
PROCESSED_DATA_PATH <- file.path("resources", "preprocessed_data")
FIGURES_PATH <- file.path("outputs", "figures")
SIMULATION_PATH <- file.path("outputs", "simulation")


### Data
OBS_YEAR <- "2018"  # Most recent year for CLC dataset
OBS_COL <- "TM"     # Mean temperatures from the meteo-france dataset (TM = "Temperature moyenne")
RES_KM <- 5         # best compromise I think