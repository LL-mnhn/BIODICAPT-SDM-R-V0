##### Imports #####
library(data.table)
library(tidyverse)
library(tidyterra)
library(jsonlite)
library(readxl)
library(raster)
library(akima)
library(purrr)
library(terra)
library(sf)


##### Parameters #####
# DO NOT MODIFY
# Extent of raster around France with some space around
LON_MIN <- -5
LON_MAX <- 9.55
LAT_MIN <- 41.35
LAT_MAX <- 51.05


##### Functions #####
#' Download a shapefile of France
#'
#' @param borders A string. Should be \code{"national"} (default) for outside borders or \code{"dpt"} for french 'département' borders.
#'
#' @return A shapefile of france.
#'
#' @export
get_france_shapefile <- function(borders = "national"){
    if (borders == "dpt"){
        france_sf <- rnaturalearth::ne_states(
            country = "France", 
            returnclass = "sf")
    } else if (borders == "national") {
        france_sf <- rnaturalearth::ne_countries(
            country = "France", 
            scale = "large", 
            returnclass = "sf")    
    } else {
        stop(paste0(borders, " is not recognised (should be one of 'regional' or 'national')"))
    }
    
    return(france_sf)
}

#' Create a raster to serve as template in other functions
#'
#' The raster created is centered on France It is used by several 
#' other functions to create identical raster grids.
#'
#' @param res_km A numeric (int or float) that represents the average spatial resolution of the grid (each cell will be approx. \code{res}*\code{res} km)
#'
#' @return A raster with no values assigned.
#'
#' @export
get_france_raster_template <- function(res_km){
    
    lat_mean <- (LAT_MIN + LAT_MAX) / 2
    res_lat <- res_km / 111.0
    res_lon <- res_km / (111.0 * cos(lat_mean * pi / 180))
    
    template <- terra::rast(
        extent = terra::ext(LON_MIN, LON_MAX, LAT_MIN, LAT_MAX),
        resolution = c(res_lon, res_lat),
        crs = "EPSG:4326"
    )
    return(template)
}


#' Fetch and filter urls of meteo-france datasets available on data.gouv.fr
#'
#' Gets the urls from the API and filters out "documentation" urls and
#' departement 99 (whose definition is unclear). Only the years 1950-2024 are 
#' kept.
#' 
#' @return A vector of strings, which are links to CSVs of meteo-france 
#' available online.
#'
#' @export
get_meteofr_urls <- function(){
    # Define URL (from API key, see data.gouv.fr for details)
    api_url <- "https://www.data.gouv.fr/api/1/datasets/6569b3d7d193b4daf2b43edc/"
    
    # get json info from API
    json_content <- jsonlite::fromJSON(api_url)
    
    # select urls (discard resource of type "documentation")
    urls <- json_content$resources %>%
        dplyr::filter(type != "documentation") %>%
        dplyr::pull(url)
    
    # select urls ending with "1950-2024.csv.gz", 
    # we get a csv.gz file for each french "département"
    gz_urls <- urls %>%
        dplyr::keep(~ str_ends(., "1950-2024.csv.gz"))
    
    # remove urls for data outside of metropolitan france
    metropolitan_urls <- gz_urls %>%
        dplyr::keep(~ 
                 nchar(
                     strsplit(
                         strsplit(., ".", fixed = TRUE)[[1]][5], 
                         "_", fixed = TRUE)[[1]][3]
                 ) < 3
        )
    
    # remove specific file ("département" 99 does not exist in France...)
    filtered_metropolitan_urls <- metropolitan_urls[
        metropolitan_urls != "https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/MENS/MENSQ_99_previous-1950-2024.csv.gz"
    ]
    
    return(filtered_metropolitan_urls)
}

#' Import and concatenate meteo france dataset to a data.frame
#'
#' Takes a vector of url strings and keeps the data corresponding to the year
#' that is indicated to import the data into one dataframe. 
#'
#' @param url_list A vector of strings containing CSVs urls.
#' @param keep_year A string corresponding to a year in 1950-2024 (fmt: "YYYY").
#' @param keep_columns A vector of strings corresponding to the names of columns that will be kept after loading the CSVs.
#' @param verbose A boolean. If \code{TRUE} activates progressbar and messages.
#'
#' @return A single dataframe containing selected meteo france data.
#'
#' @seealso \code{\link{get_meteofr_urls}}
#'
#' @export
load_url_2_df <- function(
        url_list,
        keep_year,
        keep_columns = c(
            "NUM_POSTE", 
            "NOM_USUEL", 
            "LAT", 
            "LON", 
            "AAAAMM", 
            "TX", 
            "TN", 
            "TM"
        ),
        verbose = TRUE) {
    
    list_dfs <- list() # list initialization
    res_km <- res
    lat_mean <- (lat_min + lat_max) / 2
    if (verbose) {
        pb <- txtProgressBar(
            min = 1,
            max = length(url_list),
            style = 3,
            char = "#"
        )    
        
        cat("Loading dataframes from urls:\n")
    }
    
    # import each csv
    for (i in seq_along(url_list)){
        temp_df <- data.table::fread(
            url_list[[i]],
            select = keep_columns,
            verbose = FALSE,
            showProgress = FALSE) # fast loading of csv.gz
        
        # ensure correct data type and select year
        temp_df$AAAAMM <- as.character(temp_df$AAAAMM) 
        obs_year_temp_df <- temp_df[startsWith(temp_df$AAAAMM, keep_year), ] 
        list_dfs[[i]] <- obs_year_temp_df
        
        if (verbose) {
            setTxtProgressBar(pb, i)
        }
    }
    
    # concatenate dataframe together for result
    return(dplyr::bind_rows(list_dfs))
}

#' Turn monthly data from meteo-france to annual data
#'
#' Groups the dataframe of monthly collected temperature by meteo france into
#' annual data. Can choose between "average" or "median" grouping. 
#' Weather stations with less than 12 data points (i.e. 12 month of collected 
#' data in the year) are discarded.
#'
#' @param df A dataframe (from \code{\link{load_url_2_df}}).
#' @param mode A string for the grouping method. Should be either \code{"average"} (default) or \code{"median"}.
#'
#' @return A single dataframe containing annual data.
#'
#' @seealso \code{\link{load_url_2_df}}
#'
#' @export
select_year_round_obs <- function(df, mode = "average") {
    # Exclusion of stations with less than 12 observations
    obs_counts <- table(df$NUM_POSTE)
    df_ <- df[obs_counts[as.character(df$NUM_POSTE)] == 12, ]
    df_$AAAAMM <- NULL
    
    # Grouping of measured values
    if (mode == "median") {
        df_annual <- stats::aggregate(. ~ NUM_POSTE + NOM_USUEL, data = df_, FUN = median, na.action = na.pass)
    } else if (mode == "average") {
        df_annual <- stats::aggregate(. ~ NUM_POSTE + NOM_USUEL, data = df_, FUN = mean, na.action = na.pass)
    } else {
        stop(paste0(mode, " is not recognised (should be one of 'median' or 'average')"))
    }
    
    return(df_annual)
}

#' Create a raster from scattered data
#'
#' Inspired by meshgrid in Python. Takes a dataframe of geographically scattered
#' points and computes a grid that interpolates values between each point, using
#' linear or spline method.
#'
#' @param df A dataframe with columns c("LON", "LAT", val). 
#' @param val A string, which is the name for the value in \code{df} that will be interpolated.
#' @param res_km A numeric (int or float) that represents the average spatial resolution of the grid (each cell will be approx. \code{res}*\code{res} km)
#' @param linear A boolean, \code{TRUE} (default) for linear interpolation, \code{FALSE} for spline interpolation.
#'
#' @return A raster grid in the WGS84 coordinate system.
#'
#' @seealso \code{\link{load_url_2_df}}, \code{\link{select_year_round_obs}}
#'
#' @export
interpolate_scattered_data <- function(
        df, 
        val = "TM", 
        res_km = 10, 
        linear = TRUE){
    # Extract coordinates and values
    points_lon <- df[["LON"]]
    points_lat <- df[["LAT"]]
    values <- df[[val]]

    # Resolution conversion (reprojection using template)
    template <- get_france_raster_template(res_km = res_km)
    
    # Create grid vectors from the template.
    # This guarantees pixel centers are exactly where terra expects them.
    grid_lon_vec <- raster::xFromCol(template, 1:ncol(template))
    grid_lat_vec <- raster::yFromRow(template, 1:nrow(template))
    
    # Interpolate onto the grid
    interp_result <- akima::interp(
        x = points_lon,
        y = points_lat,
        z = values,
        xo = grid_lon_vec,
        yo = sort(grid_lat_vec),  # akima needs ascending order
        linear = linear,
        extrap = FALSE,
        duplicate = "mean"
    )
    
    # Fill the terra raster directly with the values we got
    final_mat <- t(interp_result$z[, ncol(interp_result$z):1])
    
    # Check dimensions before assigning to avoid the warning
    if(nrow(final_mat) != nrow(template) || ncol(final_mat) != ncol(template)){
        stop(paste("Dimension mismatch! Matrix:", nrow(final_mat), "x", ncol(final_mat), 
                   "| Raster:", nrow(template), "x", ncol(template)))
    }
    
    values(template) <- final_mat
    
    # expand oustide initial blob a values to cover the whole map
    # defaults to only 2, should not be changed without careful hypotheses
    n_expand <- 2  # increase if your NA border is wide
    for (i in seq_len(n_expand)) {
        template <- terra::focal(
            template,
            w = 9,                  # 3x3 neighbourhood
            fun = mean,
            na.policy = "only",     # only fill NA cells
            na.rm = TRUE
        )
    }
    
    # clip to France borders
    mask_sf <- get_france_shapefile()
    interp_clipped <- clip_raster_from_shapefile(template, mask_sf)
    return(interp_clipped)
}

#' A simple function to clip a raster using a shapefile
#'
#' @param raset A raster object.
#' @param shapefile A shapefile object.
#'
#' @return The raster file with NAs outside of the shapefile mask.
#'
#' @export
clip_raster_from_shapefile <- function(raster, shapefile){
    # verify raster and shapefile crs are marching
    if (terra::same.crs(raster, shapefile)){
        # Clip the raster using the polygon
        clipped_raster <- terra::mask(raster, shapefile)
        return(clipped_raster)
    } else {
        stop("raster and shapefile have different CRS.")
    }
}

#' Load weather dataframe and pre-proccess it to a grid
#'
#' Loads dataframe saved as CSV from \code{\link{select_year_round_obs}} outputs
#' and then either returns it as a raster grid covering France.
#'
#' @param data_path A string that is the path to a CSV (saved from \code{\link{select_year_round_obs}} output).
#' @param res_km A numeric (int or float) that represents the average spatial resolution of the grid (each cell will be approx. \code{res}*\code{res} km)
#' @param colname A string corresponding to a columns in the CSV imported. Value that will be interpolated.
#'
#' @return A raster of values covering france
#'
#' @seealso \code{\link{interpolate_meshgrid}}, \code{\link{clip_raster_from_shapefile}}, \code{\link{select_year_round_obs}}
#'
#' @export
scattered_points_2_grid_on_france <- function(
        data_path, 
        res_km = 10, 
        colname = "TM"){
    # load preprocessed temperature data
    df <- data.table::fread(data_path)
    
    # compute meshgrid from data
    interp_raster <- interpolate_scattered_data(df, val=colname, res_km=res_km)
    
    # get mask shapefile mask data outside of France.
    mask_sf <- get_france_shapefile()
    final_raster <- clip_raster_from_shapefile(interp_raster, mask_sf)

    return(final_raster)
}

#' Load weather dataframe and pre-proccess it to a grid
#'
#' Loads dataframe saved as CSV from \code{\link{select_year_round_obs}} outputs
#' and then either returns it as a raster grid covering France.
#'
#' @param data_path A string that is the path to a CSV (saved from \code{\link{select_year_round_obs}} output).
#' @param res_km A numeric (int or float) that represents the average spatial resolution of the grid (each cell will be approx. \code{res}*\code{res} km)
#' @param colname A string corresponding to a columns in the CSV imported. Value that will be interpolated.
#'
#' @return A raster of values covering france
#'
#' @seealso \code{\link{interpolate_meshgrid}}, \code{\link{clip_raster_from_shapefile}}, \code{\link{select_year_round_obs}}
#'
#' @export
import_and_transform_corine_raster <- function(
    corine_tiff_path,
    res_km = 10,
    verbose = TRUE){
    
    # import raw_data
    raster <- terra::rast(corine_tiff_path)
    
    # crop before reprojection so that it goes faster (with small tolerance)
    template <- get_france_raster_template(res_km = res_km)
    
    target_wgs84_extent <- terra::ext(
        terra::ext(template)$xmin - abs(terra::ext(template)$xmin / 10), 
        terra::ext(template)$xmax + abs(terra::ext(template)$xmax / 10), 
        terra::ext(template)$ymin - abs(terra::ext(template)$ymin / 50), 
        terra::ext(template)$ymax + abs(terra::ext(template)$ymax / 50))
    
    if (!verbose) {
        hideProgress()
    } else {
        cat("Projecting extent to new coordinate system...\n")
    }
    target_3035_extent <- terra::project(
        target_wgs84_extent, 
        from = "EPSG:4326", 
        to = "EPSG:3035")
    raster_cropped <- terra::crop(raster, target_3035_extent)
    
    # Resolution conversion 
    if (verbose) {
        cat("Projecting raster to new coordinate system...\n")
    }
    raster_final <- terra::project(raster_cropped, template, method = "near")

    # get mask shapefile and mask data outside of France.
    mask_sf <- get_france_shapefile()
    corine_clipped_data <- clip_raster_from_shapefile(raster_final, mask_sf)
    
    # reset terra verbose before exiting the function
    if (!verbose) {
        showProgress()
    }

    return(corine_clipped_data)
}

#' Group together similar categories of CORINE Land Cover
#'
#' The CORINE Land Cover (CLC) raster has many sub-categories that may or may not be 
#' useful to the user depending on usage. This function enables to group the 
#' subcategories into their respective categories to minimize the number of 
#' different variables. The function replaces layer names and color table.
#' 
#'
#' @param clc_raster The CLC raster to modify.
#' @param group_urban A boolean that controls the grouping (TRUE, default) of urban areas into a single category. 
#' @param group_crops A boolean that controls the grouping (TRUE) of agricultural areas into a single category. Default is FALSE. 
#' @param group_forests A boolean that controls the grouping (TRUE, default) of forests and semi-natural areas into a single category. 
#' @param group_wetlands A boolean that controls the grouping (TRUE, default) of wetlands into a single category. 
#' @param group_water A boolean that controls the grouping (TRUE, default) of water bodies into a single category. 
#'
#' @return The CLC raster with its new categories (layer is labelled: LABEL1).
#'
#' @export
simplify_CLC <- function(
        clc_raster,
        group_urban = TRUE,
        group_crops = FALSE,
        group_forests = TRUE,
        group_wetlands = TRUE,
        group_water = TRUE){
    # get table of categories in raster
    cat_table <- terra::cats(clc_raster)[[1]]
    
    # assign to knew classes based on regex rules (surprisingly easy)
    # -> create an new column (CUSTOM_LABEL) that will hold new category names
    cat_table$CUSTOM_LABEL <- dplyr::case_when( 
        grepl("^1", cat_table$CODE_18) ~ if (group_urban) {"Artificial surfaces"} else {cat_table$LABEL3},
        grepl("^2", cat_table$CODE_18) ~ if (group_crops) {"Agricultural areas"} else {cat_table$LABEL3},
        grepl("^3", cat_table$CODE_18) ~ if (group_forests) {"Forest and semi-natural areas"} else {cat_table$LABEL3},
        grepl("^4", cat_table$CODE_18) ~ if (group_wetlands) {"Wetlands"} else {cat_table$LABEL3},
        grepl("^5", cat_table$CODE_18) ~ if (group_water) {"Water bodies"} else {cat_table$LABEL3},
        TRUE ~ "NODATA"
    )
    
    # Create new ID class
    class_levels <- unique(cat_table$CUSTOM_LABEL)
    cat_table$CUSTOM_CODE <- terra::match(cat_table$CUSTOM_LABEL, class_levels)
    reclass_matrix <- as.matrix(cat_table[, c("Value", "CUSTOM_CODE")]) 
    
    # Assign new category labels to 
    clc_raster_aug <- terra::classify(clc_raster, reclass_matrix)
    
    # Assign new category labels
    base::levels(clc_raster_aug) = data.frame(
        ID = seq_along(class_levels),
        LABEL = class_levels
    ) 
    
    # Re-assign original colors (using levels in a similar way)
    # Compute color for each category
    color_table <- cat_table %>%
        dplyr::group_by(CUSTOM_CODE, CUSTOM_LABEL) %>%
        dplyr::summarise(
            Red = mean(Red,   na.rm = TRUE),
            Green = mean(Green, na.rm = TRUE),
            Blue = mean(Blue,  na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::arrange(CUSTOM_CODE)
    
    # Assign new layer in raster
    color_levels <- data.frame(
        ID = color_table$CUSTOM_CODE, 
        LABEL = color_table$CUSTOM_LABEL
    )
    base::levels(clc_raster_aug) <- color_levels    
    
    # Assign new (default) color table in raster
    coltab <- data.frame(
        value = color_table$CUSTOM_CODE,
        col = rgb(
            color_table$Red, 
            color_table$Green, 
            color_table$Blue,
            maxColorValue = 255)
    )
    
    terra::coltab(clc_raster_aug) <- coltab 
    
    return(clc_raster_aug)
}

#' Group together the selected categories of a raster
#'
#' Reduces the number of categories in a raster by grouping together the 
#' categories specified by the user. Conserves original colors.
#'
#' @param raster A basic SpatRaster object. Must have values & land_cover fields
#' @param cats_2_group A list of categories to group together (must be a sample of `terra::cats(raster)[[1]]$land_cover`)
#' @param new_group_name A string. Self-explanatory.
#' @param new_group_color A string. Self-explanatory. See `help(col2rgb)` for accepted formats.
#' 
#' @return The raster with its new categories.
#'
#' @export
group_CLC_categories <- function(
        raster,
        cats_2_group,
        new_group_name = "unmodelled",
        new_group_color = "#000000"
){
    ### 1. Grouping labels to discard together
    # 1.1. get table of categories in raster
    cat_table <- terra::cats(raster)[[1]]
    
    # 1.2. recode each old label to a new one
    recode_map <- ifelse(
        cat_table$land_cover %in% cats_2_group,
        new_group_name,
        cat_table$land_cover
    )
    cat_table$NEW_LABEL <- recode_map
    
    # 1.3. Assign new values
    new_label_groups <- unique(cat_table$NEW_LABEL)
    new_values <- setNames(seq_along(new_label_groups), new_label_groups)
    cat_table$NEW_VALUE <- new_values[cat_table$NEW_LABEL]
    
    # 1.4. Reclassification of the raster
    rcl_matrix <- as.matrix(cat_table[, c("value", "NEW_VALUE")])
    r_reclass <- terra::classify(raster, rcl_matrix)
    
    # 1.5. Attach the new category labels
    new_cat_table <- data.frame(
        value = unname(new_values),
        land_cover = names(new_values)
    )
    base::levels(r_reclass) <- new_cat_table
    
    # 2. Remake color table (lost when using classify())
    # 2.1. get initial color table
    orig_colors <- terra::coltab(raster)[[1]]  
    
    # 2.2. Categories with unchanged LABEL keep the same colors
    cats_with_same_labels <- cat_table[!cat_table$NEW_LABEL %in% c(new_group_name), ]
    values_of_cats_with_same_labels <- cats_with_same_labels[!duplicated(cats_with_same_labels$NEW_VALUE), c("value", "NEW_VALUE")]
    colors_of_cats_with_same_labels <- terra::merge(values_of_cats_with_same_labels, orig_colors, by = "value")
    colors_of_cats_with_same_labels$value <- colors_of_cats_with_same_labels$NEW_VALUE  # replace old ID with new ID
    
    # remove NEW_VALUE column
    colors_of_cats_with_same_labels$NEW_VALUE <- NULL 
    
    # 2.3. Add a new color row for the new group (here: grey, feel free to change)
    rgb_vals <- col2rgb(new_group_color)
    new_group_row <- data.frame(
        value = unname(new_values[new_group_name]),
        red = as.integer(rgb_vals["red",   1]),
        green = as.integer(rgb_vals["green", 1]),
        blue = as.integer(rgb_vals["blue",  1]),
        alpha = 255L
    )
    
    # 2.4. Combine and sort by value
    new_colors <- rbind(colors_of_cats_with_same_labels, new_group_row)
    new_colors  <- new_colors[order(new_colors$value), ]
    
    terra::coltab(r_reclass) <- new_colors
    
    return(r_reclass)
}


#' Load and format monthly collected rasters to a single annual raster
#'
#' Loads 12 raster (each one corresponding to a different month) from given 
#' paths. Then, crop to France extent and reprojects to a new template grid.
#'
#' @param raster_paths A vector of 12 strings that are paths to raster files.
#' @param res_km A numeric (int or float) that represents the average spatial resolution of the grid (each cell will be approx. \code{res}*\code{res} km)
#' @param fun A function for the aggregation of month data to annual data (usually one of: mean, median, sum)
#' 
#'
#' @return A raster of values covering france
#'
#' @seealso \code{\link{interpolate_meshgrid}}, \code{\link{clip_raster_from_shapefile}}, \code{\link{select_year_round_obs}}
#'
#' @export
monthly_2_yearly_rasters <- function(raster_paths, res_km, fun = mean){
    if (length(raster_paths) != 12){
        stop(paster("List of strings recieved contains", length(raster_paths), "elements, expected 12."))
    }

    raster_list <- c()
    for (n_path in 1:12){
        # import raster
        raster <- terra::rast(raster_paths[n_path])
        
        if (!terra::same.crs(raster, "EPSG:4326")){
            stop(paste("Raster (at ", raster_paths[n_path], ") has crs", terra::crs(raster), "expected 'EPSG:4326'."))
        }
        
        # crop it (reduce size loaded in memory, no need to keep the entire map)
        template <- get_france_raster_template(res_km=res_km)
        target_wgs84_extent <- terra::ext(
            terra::ext(template)$xmin - abs(terra::ext(template)$xmin / 10), 
            terra::ext(template)$xmax + abs(terra::ext(template)$xmax / 10), 
            terra::ext(template)$ymin - abs(terra::ext(template)$ymin / 50), 
            terra::ext(template)$ymax + abs(terra::ext(template)$ymax / 50))
        cropped_raster = terra::crop(raster, target_wgs84_extent)
        
        if (n_path == 1){
            rasters_together <- cropped_raster
        } else {
            raster_list <- c(rasters_together, cropped_raster)
        }
    }
    
    # Compute and return average/median/other on list of cropped raster
    annual_raster <- terra::app(rasters_together, fun = fun)

    # Reproject to template grid
    annual_raster <- resample(annual_raster, template, method = "bilinear")


    # Clip values outside of France
    mask_sf <- get_france_shapefile()
    final_raster <- clip_raster_from_shapefile(annual_raster, mask_sf)

    return(final_raster)
}


#' Load and format BIODICAPT surveys obtained from networks
#'
#' @param xlsx_paths A vector of 12 strings that are paths to raster files.
#'
#' @return A dataframe (a concatenation of all surveys)
#'
#' @export
import_biodicapt_land_surveys <- function(xlsx_paths){
    # Initialize list of dataframes
    list_dfs <- list()
    
    # import and format each one in xlsx_paths
    for (i in 1:length(xlsx_paths)){
        print(xlsx_paths[i])
        list_dfs[[i]] <- readxl::read_xlsx(
            xlsx_paths[i],
            sheet = if (grepl("MONTPELLIER", xlsx_paths[i], fixed = TRUE)) {1} else {2})
        # Note : for MTP, data is in a different sheet
        
        if (grepl("SCARABEE", NAMES_SAMPLES_XLSX[i], fixed = TRUE)){
            # exchange X_L93 with Y_L93 for SCARABEE
            list_dfs[[i]][, c("LON", "LAT")] <- list_dfs[[i]][, c("Y_L93", "X_L93")]
        } else {
            # convert Lambert93 to WGS84 for the others
            temp_df <- sf::st_as_sf(list_dfs[[i]], coords = c("X_L93", "Y_L93"), crs = 2154)
            temp_df <- sf::st_transform(temp_df, crs = 4326)
            list_dfs[[i]]$LON <- sf::st_coordinates(temp_df)[, 1]
            list_dfs[[i]]$LAT  <- sf::st_coordinates(temp_df)[, 2]
        }
        
        # Remove old GPS columns
        list_dfs[[i]]$X_L93 <- NULL
        list_dfs[[i]]$Y_L93 <- NULL
        
        # Ensure formatting
        list_dfs[[i]]$code_local <- as.character(list_dfs[[i]]$code_local)
        if (any(grepl("IFT total", names(list_dfs[[i]]), fixed = TRUE))){
            list_dfs[[i]]$`IFT total` <- as.numeric(list_dfs[[i]]$`IFT total`)
        }
        
        # Add column to keep track of original file
        split_name <- strsplit(xlsx_paths[i], "_", fixed = TRUE)
        location <- split_name[[1]][length(split_name[[1]])]
        location <- substr(location, 1, nchar(location)-5)
        list_dfs[[i]]$network <- rep(location, nrow(list_dfs[[i]]))
        print(names(list_dfs[[i]]))
        
    }
    
    final_df <- dplyr::bind_rows(list_dfs)
    print(names(final_df))
    return(final_df)
}

#' Compute a range and snap limits to closest multiples
#'
#' @param x A numeric vector.
#' @param multiple A numeric. The multiple to which the limits will snap to.
#'
#' @return A vector of limits c(min, max)
#'
#' @export
pretty_range <- function(x, multiple = 5) {
    r <- range(x, na.rm = TRUE)
    c(
        floor(r[1] / multiple) * multiple,   # snap min DOWN to nearest multiple
        ceiling(r[2] / multiple) * multiple  # snap max UP to nearest multiple
    )
}