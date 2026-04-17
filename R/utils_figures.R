##### Imports #####
library(rnaturalearthdata)
library(rnaturalearth)
library(viridisLite)
library(colorspace)
library(data.table)
library(tidyverse)
library(cowplot)
library(sf)

source(here::here("R/utils_data.R"))
source(here::here("resources/config.R")) # Import global parameters


##### Parameters #####
# CAN BE MODIFIED
# Zoom to metropolitan France
MAP_DISPLAY_LIMITS = c(
    xmin = -5,
    xmax = 10,
    ymin = 41,
    ymax = 51
)


##### Functions #####
#' Create a custom ggplot theme that can be applied to any ggplot figure
#'
#' @param figure A ggplot2 figure.
#' @param with_palette A boolean. If TRUE (default), adds custom color, fill, shape and size scales.
#'
#' @return A ggplot figure.
#'
#' @export
my_custom_ggplot_theme <- function(figure, with_palette=TRUE){
    customised_fig <- figure +
        ggplot2::theme_linedraw(
            base_family = "Lexend"
        ) +
        ggplot2::theme(
            # title and subtitle styling
            plot.title.position = "plot",
            plot.title = ggplot2::element_text(
                size = 18,
                face = "bold",
                color = "#000000",  
                margin = margin(b = 10)
            ),
            plot.subtitle = ggplot2::element_text(
                size = 14,
                color = "#777777", 
                margin = ggplot2::margin(b = 10)
            ),
            
            # plot styling
            plot.caption.position = "plot",
            plot.caption = ggplot2::element_text(
                size = 9,
                color = "#999999", 
                margin = ggplot2::margin(t = 15),
                hjust = 0
            ),
            axis.text = ggplot2::element_text(
                size = 11,
                color = "#000000"
            ),
            
            # external grid
            axis.ticks = ggplot2::element_line(
                linetype = "solid",
                linewidth = 0.50,
                color = "#000000"
            ),
            panel.border = ggplot2::element_rect(
                colour = "#000000",
                linewidth = 1,
                fill = NA
            ),
            
            # internal grid
            panel.grid.major = ggplot2::element_line(
                linetype = "solid",
                linewidth = 0.15,
                color = "#999999"
            ),
            panel.grid.minor = ggplot2::element_blank(),
        )
        
        if (with_palette){
            return(customised_fig + CUSTOM_SCALES)
        } else {
            return(customised_fig)
        }
}


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
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "lightcyan1", color = NA),
            panel.grid.major = ggplot2::element_line(color = "lightcyan1"),
            panel.grid.minor = ggplot2::element_line(color = "lightcyan1")
        ) +
        ggplot2::labs(
            x = "longitude",
            y = "latitude"
        ) +
        ggplot2::coord_sf(
            xlim = c(MAP_DISPLAY_LIMITS[1], MAP_DISPLAY_LIMITS[2]), 
            ylim = c(MAP_DISPLAY_LIMITS[3], MAP_DISPLAY_LIMITS[4]))
    
    return(my_custom_ggplot_theme(base_map, with_palette=FALSE))
}

#' Create a ggplot that shows weather stations locations
#'
#' @param base_map A base ggplot on which to draw the stations locations (usually, obtained from \code{\link{ggplot_get_france_base_map}})
#' @param df A data.frame with columns LON and LAT corresponding to coordinates in CRS:4326
#' @param LON A string, the name of a column in df (Longitude coordinate)
#' @param LAT A string, the name of a column in df (Latitude coordinate)
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{ggplot_get_france_base_map}}
#'
#' @export
LEGACY_ggplot_xy_points_scattered_on_france_map <- function(
        base_map, 
        df, 
        LON = "x", 
        LAT = "y"){
    # make sure that coordinates are in the right coordinates system
    data <- sf::st_as_sf(df, coords = c(LON, LAT), crs = 4326)
    
    # shuffle to avoid biased overlaps
    data <- dplyr::slice_sample(data, prop = 1)
    
    # show plot with station locations
    map_data <- base_map +
        ggplot2::geom_sf(
            data = data,
            size = SIZES[1],
            shape = 21,        
            fill = PALETTE[1], 
            color = darken(PALETTE[1], amount = 0.2),    
            stroke = 0.8) +   
        ggplot2::coord_sf(
            xlim = c(MAP_DISPLAY_LIMITS[1], MAP_DISPLAY_LIMITS[2]), 
            ylim = c(MAP_DISPLAY_LIMITS[3], MAP_DISPLAY_LIMITS[4]))
    
    return(my_custom_ggplot_theme(map_data, with_palette = FALSE))
}

#' Create a ggplot that shows a grid of continuous values on the map of France
#'
#' @param base_map A base ggplot on which to draw the grid (usually, obtained from \code{\link{ggplot_get_france_base_map}})
#' @param raster Either a string (path to a raster .tif file) or a SpatRaster object.
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
        raster,
        colname,
        unit="°C",
        limits=NULL){
    if (class(raster)[1] == "character") {
        # convert to dataframe for ggplot2
        raster <- terra::rast(raster)  
    } else if (class(raster)[1] != "SpatRaster") {
        stop(paste("Was expecting a string or SpatRaster object, got", class(raster)))
    }

    raw_df <- as.data.frame(raster, xy = TRUE)
    
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
        ggplot2::geom_raster(
            data = raw_df, 
            aes(x = x, y = y, fill = .data[[colname]])) +
        ggplot2::scale_fill_continuous(
            na.value = "transparent", 
            palette = "turbo",
            limits = c(low=low_limit, high=high_limit)) +
        ggplot2::labs(
            x = "longitude",
            y = "latitude",
            fill = unit
        ) +
        ggplot2::coord_sf(
            xlim = c(MAP_DISPLAY_LIMITS[1], MAP_DISPLAY_LIMITS[2]), 
            ylim = c(MAP_DISPLAY_LIMITS[3], MAP_DISPLAY_LIMITS[4]))
    
    return(my_custom_ggplot_theme(map_temperature_grid, with_palette = FALSE))
}

#' Create a ggplot that shows a grid of categorical values on the map of France
#'
#' @param base_map A base ggplot on which to draw the grid (usually, obtained from \code{\link{ggplot_get_france_base_map}})
#' @param raster Either a string (path to a raster .tif file) or a SpatRaster object.
#' @param layer_name String that corresponds to the name of the layer to plot.
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{ggplot_get_france_base_map}}
#'
#' @export
ggplot_categorical_raster_on_france_map <- function(
        base_map, 
        raster, 
        layer_name) {
    
    if (class(raster)[1] == "character") {
        # get raster data
        raster <- terra::rast(raster)  
    } else if (class(raster)[1] != "SpatRaster") {
        stop(paste("Was expecting a string or SpatRaster object, got", class(raster)))
    }
        
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
            fill = "Land Cover") +
        ggplot2::coord_sf(
            xlim = c(MAP_DISPLAY_LIMITS[1], MAP_DISPLAY_LIMITS[2]), 
            ylim = c(MAP_DISPLAY_LIMITS[3], MAP_DISPLAY_LIMITS[4]))
    
    return(my_custom_ggplot_theme(map_category_grid, with_palette = FALSE))
}


#' Create a ggplot that shows the locations of CSV rows
#'
#' @param base_map A base ggplot on which to draw the stations locations (usually, obtained from \code{\link{ggplot_get_france_base_map}})
#' @param csv_path A string that is the path to a CSV with LON and LAT columns (CRS 4326).
#' @param category A string that is on of the columns in the imported CSV. Allows the user to control color and fill attributes of the points plotted on the ggplot after its generation. (Default is NULL -> use of default colors).
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{ggplot_get_france_base_map}}
#'
#' @export
LEGACY_ggplot_CSV_points_scattered_on_france_map <- function(
        base_map, 
        csv_path,
        category){
    # import csv file
    df <- data.table::fread(csv_path)
    
    # make sure that coordinates are in the right coordinates system
    data <- sf::st_as_sf(df, coords = c("LON", "LAT"), crs = 4326)
    
    # shuffle to avoid biased overlaps
    data <- dplyr::slice_sample(data, prop = 1)
    
    # leave colors options to the user
    map_data <- base_map +
        ggplot2::geom_sf(
            data = data,
            ggplot2::aes(
                fill = .data[[category]], 
                color = .data[[category]],
                size = .data[[category]],
                shape = .data[[category]]),
            stroke = 1) +   
        ggplot2::coord_sf(
            xlim = c(-5, 10), 
            ylim = c(41, 51))
    
    return(my_custom_ggplot_theme(map_data, with_palette = TRUE))
}


#' Create a ggplot that shows scattered locations on a map with their categories
#'
#' @param base_map A base ggplot on which to draw the grid (usually, obtained from \code{\link{ggplot_get_france_base_map}})
#' @param df A data.frame (or a CSV path to fetch) with columns LON and LAT corresponding to coordinates in CRS:4326
#' @param LON A string, the name of a column in df (Longitude coordinate)
#' @param LAT A string, the name of a column in df (Latitude coordinate)
#' @param column A string, name of the column where the observations are stored (values must be binary c(0, 1)).
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{ggplot_get_france_base_map}}
#'
#' @export
ggplot_categorical_df_on_france_map <- function(
        base_map, 
        df, 
        LON = "x",
        LAT = "y",
        column = NULL) {
    
    # import csv file if not already a dataframe
    if (class(df)[1] == "character"){
        df <- data.table::fread(df)
    }
    
    # make sure that coordinates are in the right coordinates system
    data <- sf::st_as_sf(df, coords = c(LON, LAT), crs = 4326)
    
    # shuffle to avoid biased overlaps
    data <- dplyr::slice_sample(data, prop = 1)
    
    if (!is.null(column)) {
        uniques_vals <- unique(data[[column]])
        if (length(uniques_vals) > 6){
            stop(paste(c(
                "This function can handle up to 6 unique values, found",
                length(uniques_vals),
                "in column",
                column,
                ". (The function can be easily modified but didnt need it until now)."
            )))
        } else {
            # make plot
            map_obs <- base_map +
                ggplot2::geom_sf(
                    data = data,
                    stroke = 0.8,
                    aes(
                        color = .data[[column]],
                        fill = .data[[column]],
                        shape = .data[[column]],
                        size = .data[[column]],
                    )
                ) +
                ggplot2::labs(
                    x = "longitude",
                    y = "latitude",
                    color = "Sampling location",
                    fill = "Sampling location",
                    shape = "Sampling location",
                    size = "Sampling location"
                ) +
                ggplot2::coord_sf(
                    xlim = c(MAP_DISPLAY_LIMITS[1], MAP_DISPLAY_LIMITS[2]),
                    ylim = c(MAP_DISPLAY_LIMITS[3], MAP_DISPLAY_LIMITS[4])
                )
        }
        return(my_custom_ggplot_theme(map_obs, with_palette = TRUE))
        
    } else {
        # make plot
        map_obs <- base_map +
            ggplot2::geom_sf(
                data = data,
                size = SIZES[1],
                shape = SHAPES[1],        
                fill = PALETTE[1], 
                color = darken(PALETTE[1], amount = 0.66), 
                stroke = 0.8
            ) +
            ggplot2::labs(
                x = "longitude",
                y = "latitude",
                color = "Sampling location",
                fill = "Sampling location",     
                shape = "Sampling location",
                size = "Sampling location"
            ) +
            ggplot2::coord_sf(
                xlim = c(MAP_DISPLAY_LIMITS[1], MAP_DISPLAY_LIMITS[2]),
                ylim = c(MAP_DISPLAY_LIMITS[3], MAP_DISPLAY_LIMITS[4])
            )
        return(my_custom_ggplot_theme(map_obs, with_palette = FALSE))
    }
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


#' Create a density ggplot of a normal function
#'
#' Does what the title says but also can show vertical lines for custom thresholds.
#' Also, show shaded grey areas for values outside the 95% CI.
#'
#' @param mean_val Mean value of the normal law.
#' @param sd_val Standard deviation of the normal law.
#' @param thresholds A vector of numeric values (optional), will add vertical lines corresponding to those on the plot.
#'
#' @export
normal_density_plot_with_thresholds <- function(
        mean_val = 1, 
        sd_val = 1.5,
        thresholds = NULL){
    
    # limits of the density plot
    boundaries <- c(mean_val - 4*sd_val, mean_val + 4*sd_val)
    
    # Calculate 95% CI bounds (±1.96 * sd)
    ci_lower <- mean_val - 1.96 * sd_val
    ci_upper <- mean_val + 1.96 * sd_val
    
    graph <- ggplot(data.frame(x = boundaries), aes(x)) +
        # Shade left tail (< 2.5%)
        stat_function(
            fun = function(x) ifelse(x < ci_lower, stats::dnorm(x, mean_val, sd_val), NA),
            geom = "area",
            fill = "gray50",
            alpha = 0.5
        ) +
        # Shade right tail (> 97.5%)
        stat_function(
            fun = function(x) ifelse(x > ci_upper, stats::dnorm(x, mean_val, sd_val), NA),
            geom = "area",
            fill = "gray50",
            alpha = 0.5
        ) +
        stat_function(
            fun = stats::dnorm, 
            args = list(mean = mean_val, sd = sd_val),
            aes(color = "Distribution"), 
            linewidth = 1) +
        labs(x = "Value", y = "Density") +
        scale_color_manual(values = c("Distribution" = PALETTE[1])) +
        theme_minimal()
    
    if (!is.null(thresholds)){
        threshold_df <- data.frame(
            value = thresholds,
            label = paste("Value: ", thresholds),
            color = PALETTE[1:length(thresholds)]
        )
        
        graph <- graph +
            geom_vline(
                data = threshold_df,
                aes(xintercept = value, color = label),
                linewidth = 0.66,
                linetype = "dashed"
            ) +
            scale_color_manual(
                values = c("Distribution" = "black", 
                           setNames(threshold_df$color, threshold_df$label))
            ) +
            guides(color = guide_legend(
                override.aes = list(
                    linewidth = c(1, rep(2, length(thresholds))),
                    linetype = c("solid", rep("dashed", length(thresholds)))
                )
            ))
    } else {
        graph <- graph +
            scale_color_manual(values = c("Distribution" = "black"))
    }
    
    return(my_custom_ggplot_theme(graph, with_palette = FALSE))
}


#' Create a grid of density and/or scatter points
#'
#' @param df_points A data.frame with columns x, y and z.
#' @param df_lines A data.frame with columns x, y and z.
#' @param x A string, the name of a column in both df (x-axis).
#' @param y A string, the name of a column in both df (y-axis).
#' @param category A string, the name of a column in both df, that has qualitative values.
#'
#' @export
plot_grid_scattered_densities <- function(
        x, 
        y, 
        category,
        df_points = NULL, 
        df_lines = NULL) {
    # Use whichever df is available to get category info
    ref_df <- if (!is.null(df_lines)) df_lines else df_points
    
    # Dynamic column calculation
    n_cats <- length(unique(ref_df[[category]]))
    n_cols <- ceiling(n_cats / 2)
    
    # Initialise plot
    grid <- ggplot2::ggplot()
    
    # Add line layers if df_lines is provided
    if (!is.null(df_lines)) {
        shadow_df <- data.table::copy(df_lines)
        shadow_df$dummy <- shadow_df[[category]]
        shadow_df[[category]] <- NULL
        
        grid <- grid +
            ggplot2::geom_line(
                data = shadow_df,
                ggplot2::aes(x = .data[[x]], y = .data[[y]], group = dummy),
                color = "gray80",
                linewidth = 0.5
            ) +
            ggplot2::geom_line(
                data = df_lines,
                ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                color = "black",
                linewidth = 1
            )
    }
    
    # Add point layer if df_points is provided
    if (!is.null(df_points)) {
        grid <- grid +
            ggplot2::geom_point(
                data = df_points,
                ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                color = PALETTE[1],
                size = 1
            )
    }
    
    # Add shared formatting
    grid <- grid +
        ggplot2::facet_wrap(stats::as.formula(paste("~", category)), ncol = n_cols) +
        ggplot2::theme_bw() +
        ggplot2::lims(
            x = range(ref_df[[x]]),
            y = c(0, 1)
        )
    
    return(my_custom_ggplot_theme(grid, with_palette = TRUE))
}