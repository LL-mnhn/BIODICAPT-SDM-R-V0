##### Imports #####
library(data.table)
library(jsonlite)
library(raster)
library(akima)
library(purrr)
library(dplyr)
library(terra)
library(sf)


##### Functions #####
get_meteo_urls <- function(){
    # Define URL (from API key, see data.gouv.fr for details)
    api_url = "https://www.data.gouv.fr/api/1/datasets/6569b3d7d193b4daf2b43edc/"
    
    # get json info from API
    json_content = jsonlite::fromJSON(api_url)
    
    # select urls (discard resource of type "documentation")
    urls = json_content$resources %>%
        filter(type != "documentation") %>%
        pull(url)
    
    # select urls ending with "1950-2024.csv.gz", 
    # we get a csv.gz file for each french "département"
    gz_urls = urls %>%
        keep(~ str_ends(., "1950-2024.csv.gz"))
    
    # remove urls for data outside of metropolitan france
    metropolitan_urls = gz_urls %>%
        keep(~ 
                 nchar(
                     strsplit(
                         strsplit(., ".", fixed=TRUE)[[1]][5], 
                         "_", fixed=TRUE)[[1]][3]
                 ) < 3
        )
    
    # remove specific file ("département" 99 does not exist in France...)
    filtered_metropolitan_urls = metropolitan_urls[
        metropolitan_urls != "https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/MENS/MENSQ_99_previous-1950-2024.csv.gz"
    ]
    
    return(filtered_metropolitan_urls)
}

load_url_2_df <- function(
        url_list,
        keep_year,
        keep_columns=c("NUM_POSTE", "NOM_USUEL", "LAT", "LON", "AAAAMM", "TX", "TN", "TM"),
        verbose=TRUE) {
    
    list_dfs = list() # list initialization
    
    if (verbose) {
        pb = txtProgressBar(
            min=1,
            max=length(url_list),
            style=3,
            char="#",
        )    
        
        cat("Loading dataframes from urls:\n")
    }
    
    # import each csv
    for (i in seq_along(url_list)){
        temp_df = fread(
            url_list[[i]],
            select=keep_columns,
            verbose=FALSE,
            showProgress=FALSE) # fast loading of csv.gz
        temp_df$AAAAMM = as.character(temp_df$AAAAMM) # ensure correct data type
        obs_year_temp_df = temp_df[startsWith(temp_df$AAAAMM, keep_year), ] # select based on year
        
        list_dfs[[i]] = obs_year_temp_df # append list
        
        if (verbose) {
            setTxtProgressBar(pb, i)
        }
    }
    
    # concatenate dataframe together for result
    return(bind_rows(list_dfs))
}

select_year_round_obs <- function(df, mode = "average") {
    # Exclusion of stations with less than 12 observations
    obs_counts = table(df$NUM_POSTE)
    df_ = df[obs_counts[as.character(df$NUM_POSTE)] == 12, ]
    df_$AAAAMM = NULL
    
    # Grouping of measured values
    if (mode == "median") {
        df_annual = aggregate(. ~ NUM_POSTE + NOM_USUEL, data = df_, FUN = median, na.action = na.pass)
    } else if (mode == "average") {
        df_annual = aggregate(. ~ NUM_POSTE + NOM_USUEL, data = df_, FUN = mean, na.action = na.pass)
    } else {
        stop(paste0(mode, " is not recognised (should be one of 'median' or 'average')"))
    }
    
    return(df_annual)
}

interpolate_meshgrid <- function(df, val = "TM", res = 3, linear = TRUE){
    ## results : lon = interp_meteo_df$x, lat = interp_meteo_df$y
    # Extract coordinates and values
    points_lon = df[["LON"]]
    points_lat = df[["LAT"]]
    values = df[[val]]
    
    lon_min = min(points_lon); lon_max = max(points_lon)
    lat_min = min(points_lat); lat_max = max(points_lat)
    
    # Resolution conversion 
    # A km is the same anywhere on the globe but for longitude, it varies 
    # depending on latitude. So we use an approx based on the mean latitude.
    res_km = res
    lat_mean = (lat_min + lat_max) / 2
    res_lat = res_km / 111.0
    res_lon = res_km / (111.0 * cos(lat_mean * pi / 180))
    
    # Create grid vectors
    grid_lon_vec = seq(lon_min, lon_max + res_lon, by = res_lon)
    grid_lat_vec = seq(lat_min, lat_max + res_lat, by = res_lat)
    
    # Interpolate onto the grid
    interp_result <- akima::interp(
        x = points_lon,
        y = points_lat,
        z = values,
        xo = grid_lon_vec,
        yo = grid_lat_vec,
        linear = linear,   # FALSE => spline interpolation
        extrap = FALSE,
        duplicate="mean"
    )

    # convert to dataframe
    interp_table = expand.grid(
        lon = interp_result$x,
        lat = interp_result$y
    )
    interp_table$value <- as.vector(interp_result$z)
    
    # convert to raster with default projection
    interp_raster = rasterFromXYZ(interp_table)
    crs(interp_raster) <- CRS("+proj=longlat +datum=WGS84")
    
    return(interp_raster)
}

clip_raster_from_shapefile <- function(raster, shapefile){
    # verify raster and shapefile crs are marching
    if (compareCRS(raster, shapefile)){
        # Clip the raster using the polygon
        clipped_raster = mask(raster, shapefile)
        return(clipped_raster)
    } else {
        stop("raster and shapefile have different CRS.")
    }
}