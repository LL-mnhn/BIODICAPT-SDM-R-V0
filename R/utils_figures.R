##### Imports #####
library(rnaturalearthdata)
library(rnaturalearth)
library(data.table)
library(tidyverse)
library(cowplot)
library(sf)

source(here::here("R/utils_data.R"))


##### Parameters #####
# empty for now...


##### Functions #####
#' Create a ggplot that shows a basic map of France
#'
#' @param borders_type A string. Direct call to \code{\link{get_france_shapefile}}. Should be \code{"national"} (default) for outside borders or \code{"dpt"} for french 'département' borders.
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{get_france_shapefile}}
#'
#' @export
ggplot_get_france_base_map <- function(borders_type="national"){
    europe_shp = rnaturalearth::ne_countries(continent = "Europe", scale = "large", returnclass = "sf")
    france_shp = get_france_shapefile(borders_type)
    
    base_map <- ggplot2::ggplot(europe_shp) +
        ggplot2::geom_sf(fill = "grey80", color = "white") +                     # color of countries
        ggplot2::geom_sf(data = france_shp, fill = "white", color = "black") +   # color of France
        ggplot2::coord_sf(xlim = c(-5, 10), ylim = c(41, 51)) +  # Zoom to metropolitan France
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "lightcyan1", color = NA),
            panel.grid.major = ggplot2::element_line(color = "lightcyan1"),
            panel.grid.minor = ggplot2::element_line(color = "lightcyan1")
        ) +
        ggplot2::labs(
            x = "longitude",
            y = "latitude"
        )
    
    return(base_map)
}

#' Create a ggplot that shows weather stations locations
#'
#' Creates a ggplot that shows weather stations locations 
#' on a basic map of France
#'
#' @param base_map A base ggplot on which to draw the stations locations (usually, obtained from \code{\link{ggplot_get_france_base_map}})
#' @param csv_path A string that is the path to a CSV with LON and LAT columns (CRS 4326).
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{ggplot_get_france_base_map}}
#'
#' @export
ggplot_points_scattered_on_france_map <- function(base_map, csv_path){
    # import csv file
    df <- data.table::fread(csv_path)
    
    # make sure that coordinates are in the right coordinates system
    data <- sf::st_as_sf(df, coords = c("LON", "LAT"), crs = 4326)
    
    # show plot with station locations
    map_data <- base_map +
        ggplot2::geom_sf(data = data, color = "darkorange1", size = 1) +   
        ggplot2::coord_sf(xlim = c(-5, 10), ylim = c(41, 51))

    return(map_data)
}

#' Create a ggplot that shows a grid of continuous values on the map of France
#'
#' @param base_map A base ggplot on which to draw the grid (usually, obtained from \code{\link{ggplot_get_france_base_map}})
#' @param raster_path A string that is the path to a raster (.tif) file.
#' @param limits Value limits for the color bars. Can either be NULL (auto limits by steps of 5) or a list of 2 values (c(min, max)).
#' @param colname Name of the column for which the values will be plotted.
#' @param unit Name of the unit for colname.
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{ggplot_get_france_base_map}}
#'
#' @export
ggplot_quantitative_raster_on_france_map <- function(
        base_map, 
        raster_path,
        colname,
        unit="°C",
        limits=NULL){
    # convert to dataframe for ggplot2
    raw_raster <- terra::rast(raster_path)
    raw_df <- as.data.frame(raw_raster, xy = TRUE)
    
    if (is.vector(limits) && length(limits) == 2){
        low_limit <- limits[1]
        high_limit <- limits[2]
    } else if (is.null(limits)) {
        # round palette scale to the bottom and top nearest multiple of 5
        low_limit <- floor(min(raw_df[colname], na.rm = TRUE) / 5) * 5
        high_limit <- ceiling(max(raw_df[colname], na.rm = TRUE) / 5) * 5
    } else {
        stop(paste("'limits' is not recognised. Expected vector of length 2 or NULL, got", limits))
    }

    map_temperature_grid <- base_map +
        ggplot2::geom_raster(data = raw_df, aes(x = x, y = y, fill = .data[[colname]])) +
        ggplot2::scale_fill_continuous(
            na.value = "transparent", 
            palette = "plasma",
            limits = c(low=low_limit, high=high_limit)) +
        ggplot2::labs(
            x = "longitude",
            y = "latitude",
            fill = unit
        )
    
    return(map_temperature_grid)
}

#' Create a ggplot that shows a grid of categorical values on the map of France
#'
#' @param base_map A base ggplot on which to draw the grid (usually, obtained from \code{\link{ggplot_get_france_base_map}})
#' @param raster_path A string that is the path to a raster (.tif) file.
#' @param layer_name String that corresponds to the name of the layer to plot.
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{ggplot_get_france_base_map}}
#'
#' @export
ggplot_categorical_raster_on_france_map <- function(
        base_map, 
        raster_path, 
        layer_name) {
    
    # Get raster data
    raster <- terra::rast(raster_path)
        
    # Convert to dataframe for ggplot2
    df <- as.data.frame(raster, xy = TRUE) 
    
    # build colors from color table
    coltab <- terra::coltab(raster)[[1]]
    colors_hex <- rgb(
        coltab$red / 255,
        coltab$green / 255,
        coltab$blue / 255,
        alpha = coltab$alpha / 255
    )
    lvls <- terra::levels(raster)[[1]] 
    
    # Match colors to labels
    label_colors <- setNames(
        colors_hex[match(lvls$value, coltab$value)],
        lvls[[layer_name]]
    )
    
    # make ggplot
    map_category_grid <- base_map +
        ggplot2::geom_raster(data = df, aes(x = x, y = y, fill = .data[[layer_name]])) +
        ggplot2::scale_fill_manual(
            values = label_colors,
            na.value = "transparent"
        ) +
        ggplot2::labs(
            x = "longitude", 
            y = "latitude", 
            fill = "Land Cover")
    
    return(map_category_grid)
}

#' Save a ggplot and its legend separately
#'
#' Create to separate plots from one ggplot entry: one for the plot and one
#' for the legend, then saves them as separate files. If there is no legend,
#' only one file is saved for the figure.
#'
#' @param ggplot_obj A ggplot object to be saved as PDF
#' @param save_path The path were the file should be saved. Must end with ".pdf". The legend will be saved with the same filename, appended with '_legend'.
#' @param rescale_legend A vector of 2 numeric values. Change if the legend is too wide/tall to fit inside the default square. Default is c(1,1) (no rescaling). Increase first value for width, second value for height.
#'
#' @export
save_figure_harmonized <- function(ggplot_obj, save_path, rescale_legend=c(4, 4)){
    # check if string ends with ".pdf"
    if (!endsWith(save_path, ".pdf")){
        stop(paste("Provided save_path must end with '.pdf', got", save_path))
    }
    
    # extract legend from original figure
    legend <- cowplot::get_legend(ggplot_obj)

    # Re-create plot (without legend)
    ggplot_obj_no_legend <- ggplot_obj + ggplot2::theme(legend.position = "none")

    # Save separately
    ggplot2::ggsave(
        filename = save_path,
        plot = ggplot_obj_no_legend,
        dpi = 300,
        width = 4,
        height = 4,
        units = "in")
    
    if (!is.null(legend)){
        ggplot2::ggsave(
            filename = paste0(substr(save_path, 1, nchar(save_path)-4), "_legend.pdf"),
            plot = cowplot::plot_grid(legend),
            dpi = 300,
            width = 4*rescale_legend[1],
            height = 4*rescale_legend[2],
            units = "in")        
    }
}