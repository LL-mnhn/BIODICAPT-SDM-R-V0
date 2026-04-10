##### Imports #####
library(rnaturalearthdata)
library(rnaturalearth)
library(tidyverse)
library(sf)

source(here::here("R/utils_figures.R"))
source(here::here("resources/config.R")) # Import global parameters


##### Parameters ####
### CAN BE MODIFIED
# these values change the aspect and/or which figures are saved
BORDER_TYPE <- "national"           # "national"/"dpt"
SHOW_EMPTY_FRANCE <- TRUE
SHOW_WEATHER_STATIONS <- TRUE       # Location of meteo-france stations
SHOW_WEATHER_INTERPOLATION <- TRUE  # Temperatures interpolated from meteo-france
SHOW_CHELSA_ST <- TRUE              # Surface temperatures from CHESLA model
SHOW_CORINE_RASTER <- TRUE          # Land Cover

### DO NOT MODIFY
# Figure size
WIDTH <- 1080
HEIGHT <- 1080


##### Main Loop #####
if (sys.nframe() == 0) {
    ### generate empty map of France
    empty_map_of_france <- ggplot_get_france_base_map(BORDER_TYPE)
    
    ### Choose which plots should be shown
    # base background map only
    if (SHOW_EMPTY_FRANCE){
        print(empty_map_of_france)
        save_figure_harmonized(
            empty_map_of_france,
            file.path(FIGURES_PATH, "background_map.pdf")
        )
    }
    
    # map with locations of weather stations
    if (SHOW_WEATHER_STATIONS){
        g1 <- ggplot_CSV_points_scattered_on_france_map(
            base_map = empty_map_of_france,
            csv_path = file.path(PROCESSED_DATA_PATH, paste0("meteo_france_", OBS_YEAR, "_annual_means.csv"))
        )
        
        print(g1)
        save_figure_harmonized(
            g1,
            file.path(FIGURES_PATH, paste0("meteo_stations_", OBS_YEAR, ".pdf"))
        )
    }
    
    # map with meshgrid of temperatures interpolated from weather stations
    if (SHOW_WEATHER_INTERPOLATION){
        g2 <- ggplot_quantitative_raster_on_france_map(
            base_map = empty_map_of_france,
            raster_path = file.path(PROCESSED_DATA_PATH, paste0("meteo_france_", OBS_YEAR, "_res", RES_KM, "_annual_means.tif")),
            colname = "focal_mean"
        )
        
        print(g2)
        save_figure_harmonized(
            g2,
            file.path(FIGURES_PATH, paste0("meteo_france_", OBS_YEAR, "_res", RES_KM, "_avg_temp.pdf"))
        )
    }

    # map with meshgrid of CHELSA modelled surface temperatures
    if (SHOW_CHELSA_ST){
        g3 <- ggplot_quantitative_raster_on_france_map(
            base_map = empty_map_of_france,
            raster_path = file.path(PROCESSED_DATA_PATH, paste0("CHELSA_Celsius_AAT_", OBS_YEAR, "_res", RES_KM, ".tif")),
            colname = "mean"
        )
        
        print(g3)
        save_figure_harmonized(
            g3,
            file.path(FIGURES_PATH, paste0("CHELSA_", OBS_YEAR, "_res", RES_KM, "_avg_temp.pdf"))
        )
    }
    
    # map with CORINE raster
    if (SHOW_CORINE_RASTER){
        corine_raster_path <- file.path(PROCESSED_DATA_PATH, paste0("CLC2018_WGS84_custom_france_res", RES_KM, "_simplified.tif"))

        g4 <- ggplot_categorical_raster_on_france_map(
            base_map = empty_map_of_france,
            raster_path = corine_raster_path,
            layer_name = "LABEL"
        )
        
        print(g4)
        save_figure_harmonized(
            g4,
            file.path(FIGURES_PATH, paste0("CORINE_Land_Cover_2018", "_res", RES_KM, ".pdf")),
            rescale_legend = c(1.5, 1)
        )
    }
}