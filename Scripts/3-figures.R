##### Imports #####
library(colorspace) # used to darken a color list

source(here::here("R/utils_figures.R"))
source(here::here("resources/config.R")) # Import global parameters


##### Parameters ####
### CAN BE MODIFIED
# these values change the aspect and/or which figures are saved
BORDER_TYPE <- "dpt"           # "national"/"dpt"
SHOW_EMPTY_FRANCE <- TRUE
SHOW_WEATHER_STATIONS <- FALSE      # Location of meteo-france stations
SHOW_WEATHER_INTERPOLATION <- FALSE # Temperatures interpolated from meteo-france
SHOW_CHELSA_ST <- TRUE              # Surface temperatures from CHESLA model
SHOW_CORINE_RASTER <- TRUE          # Land Cover

# plots with options
SHOW_RESEARCH_NETWORK <- TRUE
color_column <- "network"
colors <- c(
    "MONTPELLIER" = "#67A300",
    "SCARABEE" = "#00D1D1",
    "DYNAFOR" = "#D13800",
    "VCG" = "#C000E6",
    "ZAAR" = "gray80"
)
colors_dark <- colorspace::darken(colors, amount = 0.85)

### DO NOT MODIFY
set.seed(43) # For reproducible results


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
            csv_path = file.path(PROCESSED_DATA_PATH_CSV, paste0("meteo_france_", OBS_YEAR, "_annual_means.csv"))
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
            raster = file.path(PROCESSED_DATA_PATH_RASTER, paste0("meteo_france_", OBS_YEAR, "_res", RES_KM, "_annual_means.tif")),
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
            raster = file.path(PROCESSED_DATA_PATH_RASTER, paste0("CHELSA_Celsius_AAT_", OBS_YEAR, "_res", RES_KM, ".tif")),
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
        corine_raster_path <- file.path(PROCESSED_DATA_PATH_RASTER, paste0("CLC2018_WGS84_custom_france_res", RES_KM, "_simplified.tif"))
        g4 <- ggplot_categorical_raster_on_france_map(
            base_map = empty_map_of_france,
            raster = corine_raster_path,
            layer_name = "LABEL"
        )
        
        print(g4)
        save_figure_harmonized(
            g4,
            file.path(FIGURES_PATH, paste0("CORINE_Land_Cover_2018", "_res", RES_KM, ".pdf")),
            rescale_legend = c(1.5, 1)
        )
    }
    # map of the research network
    if (SHOW_RESEARCH_NETWORK){
        g5 <- ggplot_CSV_points_scattered_on_france_map(
            base_map = empty_map_of_france,
            csv_path = file.path(PROCESSED_DATA_PATH_CSV, "BIODICAPT_survey_data.csv"),
            category = color_column
        )
        # specify colors
        g5 <- g5 +
            ggplot2::scale_fill_manual(values = colors) +
            ggplot2::scale_color_manual(values = colors_dark)
        print(g5)
        save_figure_harmonized(
            g5,
            file.path(FIGURES_PATH, "BIODICAPT_surveyed_locations.pdf"),
            rescale_legend = c(1.5, 1)
        )
    }
}