##### Imports #####
library(rnaturalearthdata)
library(rnaturalearth)
library(data.table)
library(tidyverse)
library(sf)

source(here::here("utils/utils_data.R"))


##### Paramaters ####
ACTIVATE_FRANCE_DPT = TRUE

SHOW_EMPTY_FRANCE = FALSE
SHOW_METEO_STATIONS = FALSE
SHOW_METEO_INTERPOLATION = TRUE

METEO_DATA_PATH = file.path("resources", "preprocessed_data", "meteo_france_2018_annual_means.csv")

##### Main Loop #####
if (sys.nframe() == 0) {
    # Get natural earth shapefile of europe
    europe_shp = ne_countries(continent = "Europe", scale = "large", returnclass = "sf")
    
    if (ACTIVATE_FRANCE_DPT){
        france_shp = ne_states(country = "France", returnclass = "sf")
    } else {
        france_shp = ne_countries(country = "France", scale = "large", returnclass = "sf")    
    }
    
    # generate empty map of France
    empty_map_of_france = ggplot(europe_shp) +
        geom_sf(fill = "grey80", color = "white") +                     # color of countries
        geom_sf(data = france_shp, fill = "white", color = "black") +   # color of France
        coord_sf(xlim = c(-5, 10), ylim = c(41, 51)) +  # Zoom to metropolitan France
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "lightcyan1", color = NA),
            panel.grid.major = element_line(color = "lightcyan1"),
            panel.grid.minor = element_line(color = "lightcyan1")
            )
    
    if (SHOW_EMPTY_FRANCE){
        # show empty map (with borders)
        empty_map_of_france
    }
    
    if (SHOW_METEO_STATIONS){
        # load preprocessed temperature data
        meteo_df = fread(METEO_DATA_PATH)
        
        # make sure that coordinates are in the right coordinates system
        meteo_df_sf = st_as_sf(meteo_df, coords = c("LON", "LAT"), crs = 4326)
        
        # show plot with station locations
        empty_map_of_france +
            geom_sf(data = meteo_df_sf, color = "darkorange1", size = 1) +   
            coord_sf(xlim = c(-5, 10), ylim = c(41, 51))
    }
    
    if (SHOW_METEO_INTERPOLATION){
        # load preprocessed temperature data
        meteo_df = fread(METEO_DATA_PATH)
        
        # compute meshgrid from data
        interp_meteo_raster = interpolate_meshgrid(meteo_df, val = "TM", res = 10)
        interp_meteo_raster_clipped = clip_raster_from_shapefile(interp_meteo_raster, france_shp)
        
        # convert to dataframe for ggplot2
        interp_meteo_df = as.data.frame(interp_meteo_raster_clipped, xy = TRUE)

        empty_map_of_france +
            geom_raster(data=interp_meteo_df, aes(x = x, y = y, fill = value)) +
            scale_fill_continuous(na.value = "transparent", palette="plasma")
    }
}