##### Global Parameters #####
# These parameters are 'hidden': they can be modified, but you should not change 
# them without a good reason. 
#
# The parameters grouped in this file are re-used by several scripts, having 
# them all in the same place allows easier access, control and harmonization.
library(ggplot2)


### Paths
RAW_DATA_PATH <- file.path("resources", "raw_data")
PROCESSED_DATA_PATH_RASTER <- file.path("resources", "preprocessed_data", "rasters")
PROCESSED_DATA_PATH_CSV <- file.path("resources", "preprocessed_data", "CSVs")
FIGURES_PATH <- file.path("outputs", "figures")
SIMULATION_PATH <- file.path("outputs", "simulation")


### Data
OBS_YEAR <- "2018"  # Most recent year for CLC dataset
OBS_COL <- "TM"     # Mean temperatures from the meteo-france dataset (TM = "Temperature moyenne")
RES_KM <- 5         # best compromise I think


### Styling
PALETTE <- c("#D9054E", "#28A349", "#246CBC", "#5D7B84", "#C2562F", "#FFB703")
SHAPES <- c(21, 22, 24, 23, 25, 8)
SIZES <- c(1.66, 1.85, 1.5, 1.66, 1.5, 1.66)
CUSTOM_SCALES <- list(
    ggplot2::scale_color_manual(values = PALETTE),
    ggplot2::scale_fill_manual(values = PALETTE),
    ggplot2::scale_shape_manual(values = SHAPES),
    ggplot2::scale_size_manual(values = SIZES)
)