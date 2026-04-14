##### Imports #####
library(data.table)

source(here::here("R/utils_data.R"))
source(here::here("resources/config.R")) # Import global parameters

##### Parameters #####
### CAN BE MODIFIED
VERBOSE <- TRUE
PATH_FOLDER_RAW_XLSX <- file.path(".", "resources", "raw_data", "reseaux")
NAMES_SAMPLES_XLSX <- c(
    "BIODICAPT_parcelles_MONTPELLIER.xlsx",
    "Preselection_Parcelles_BIODICAPT_SCARABEE.xlsx",
    "Preselection_Parcelles_BIODICAPT_DYNAFOR.xlsx",
    "Preselection_Parcelles_BIODICAPT_VCG.xlsx",
    "Preselection_Parcelles_BIODICAPT_ZAAR.xlsx"
)

### DO NOT MODIFY
XLSX_FILEPATHS <- c()
for (i in 1:length(NAMES_SAMPLES_XLSX)){
    XLSX_FILEPATHS[i] <- file.path(PATH_FOLDER_RAW_XLSX, NAMES_SAMPLES_XLSX[i])
}


##### Main Loop #####
if (sys.nframe() == 0) {
    # Yes I come from Python, how could you tell ?
    # Anw, this 'if' means the following instructions will run only when 
    # this script is the top-level call
    
    ##### Import of XLSX surveys on locations sampled in research network #####
    if (VERBOSE) {
        cat("Loading and formatting BIODICAPT surveys...\n")
    }
    xlsx_df <- import_biodicapt_land_surveys(XLSX_FILEPATHS)
    
    # remove unrelevant columns
    xlsx_df[ ,c("code_local", "Contact_Viti", "Referent_Projet", "Lot", "ocsol2026",
                "IFT total", "IFT_data_year", "lineaire(m)_haies_buffer_1km",
                "%surface_haies_buffer_1km", "%surface_ESN", "...11", "...12"
    )] <- list(NULL)
    data.table::fwrite(
        xlsx_df, 
        file.path(PROCESSED_DATA_PATH_CSV, "BIODICAPT_survey_data.csv")
    )
    if (VERBOSE) {
        cat("BIODICAPT survey data is ready.\n\n")
    }
    
    
    ##### Meteo France (weather) dataset #####
    # Import and save raw dataset on first use only
    if (!file.exists(file.path(RAW_DATA_PATH, paste0("meteo_france_", OBS_YEAR, ".csv")))){
        if (VERBOSE) {
            cat("Loading weather data from data.gouv.fr API...\n")
        }
        # dataset is available online through an API
        weather_csv_urls <- get_meteofr_urls()
        full_weather_df <- load_url_2_df(
            url_list = weather_csv_urls,
            keep_year = OBS_YEAR,
            verbose = VERBOSE
        ) 
    
        # Save for future reference
        data.table::fwrite(
            full_meteo_df, 
            file.path(RAW_DATA_PATH, paste0("meteo_france_", OBS_YEAR, ".csv"))
        )
    }
    
    if (VERBOSE) {
        cat("Filtering weather data from Meteo France...\n")
    }
    # Use the saved dataset to make reproducible changes
    full_weather_df <- fread(file.path(RAW_DATA_PATH, paste0("meteo_france_", OBS_YEAR, ".csv")))
    
    # Transform dataset from monthly to yearly data
    obs_year_weather_df <- select_year_round_obs(full_weather_df)
    
    # exclude rows with NAs
    obs_year_clean_weather_df <- obs_year_weather_df %>% 
        filter(!is.na(TX), !is.na(TN), !is.na(TM))
    
    # Save preprocessed dataframe
    data.table::fwrite(
        obs_year_clean_weather_df, 
        file.path(PROCESSED_DATA_PATH_CSV, paste0("meteo_france_", OBS_YEAR, "_annual_means.csv"))
    )
    if (VERBOSE) {
        cat("Meteo France data is ready.\n\n")
    }
    
    
    ### Make grid from Meteo France Dataset ###
    obs_year_clean_weather_raster <- scattered_points_2_grid_on_france(
        file.path(PROCESSED_DATA_PATH_RASTER, paste0("meteo_france_", OBS_YEAR, "_annual_means.csv")), 
        res_km = RES_KM, 
        colname = "TM")
    writeRaster(
        obs_year_clean_weather_raster, 
        file.path(PROCESSED_DATA_PATH_RASTER, paste0("meteo_france_", OBS_YEAR, "_res", RES_KM, "_annual_means.tif")),
        overwrite = TRUE)
        
    
    ### Alternative Weather data: CHELSA ###
    if (VERBOSE) {
        cat("Loading CHELSA data from local folder...\n")
    }
    
    # check if local folder contains the necessary files
    CHELSA_12_paths <- c() # initialisation
    for (month in 1:12){
        month_fmt <- sprintf("%02d", as.integer(month))
        CHELSA_tiff_path <- file.path(RAW_DATA_PATH, "CHELSA-monthly", paste0("CHELSA_tas_", month_fmt, "_", OBS_YEAR, "_V.2.1.tif"))
        if (!file.exists(CHELSA_tiff_path)){
            warning("CHELSA tiff files must be manually downloaded and placed into the resources/raw_data/CHELSA-monthly folder. Download is available at https://www.chelsa-climate.org/datasets/chelsa_monthly.\n")
            stop(paste0("Cannot find file '", CHELSA_tiff_path,"' are you sure that you downloaded it manually before ? If not, follow instructions under this message."))            
        }
        CHELSA_12_paths <- c(CHELSA_12_paths, CHELSA_tiff_path) # store paths for import (see next section)
    }
    
    # if all files are here, then assemble them to get annual data
    if (VERBOSE) {
        cat("Preparing rasters...\n")
    }
    CHELSA_annual_raster <- monthly_2_yearly_rasters(
        CHELSA_12_paths, 
        res_km = RES_KM, 
        fun = mean)
    
    # Convert from kelvin to celsius
    CHELSA_annual_raster <- CHELSA_annual_raster - 273.15
        
    # Save final raster
    writeRaster(
        CHELSA_annual_raster, 
        file.path(PROCESSED_DATA_PATH_RASTER, paste0("CHELSA_Celsius_AAT_", OBS_YEAR, "_res", RES_KM, ".tif")), 
        overwrite = TRUE)
    if (VERBOSE) {
        cat("CHELSA data is ready.\n\n")
    }
    
    
    ##### CORINE Land Cover 2018 Dataset #####
    if (VERBOSE) {
        cat("Loading CORINE Land Cover data from local folder...\n")
    }
    # I'm too lazy to get it from the API sorry, maybe I'll update it later
    corine_tiff_path <- file.path(RAW_DATA_PATH, "u2018_clc2018_v2020_20u1_raster100m", "DATA", "U2018_CLC2018_V2020_20u1.tif")
    if (file.exists(corine_tiff_path)){
        if (VERBOSE) {
            cat("Re-projecting CORINE Land Cover data...\n")
        }
        corine_raster_new_res <- import_and_transform_corine_raster(
            corine_tiff_path,
            res_km = RES_KM
        )
        writeRaster(
            corine_raster_new_res, 
            file.path(PROCESSED_DATA_PATH_RASTER, paste0("CLC2018_WGS84_custom_france_res", RES_KM, ".tif")), 
            overwrite = TRUE)
        
        
        if (VERBOSE) {
            cat("Grouping similar categories together...\n")
        }
        corine_raster_simple <- simplify_CLC(
            clc_raster = corine_raster_new_res,
            group_urban = TRUE,
            group_crops = FALSE,
            group_forests = TRUE,
            group_wetlands = TRUE,
            group_water = TRUE
        )
        writeRaster(
            corine_raster_simple, 
            file.path(PROCESSED_DATA_PATH_RASTER, paste0("CLC2018_WGS84_custom_france_res", RES_KM, "_simplified.tif")), 
            overwrite = TRUE)
        
        
        if (VERBOSE) {
            cat("CORINE data is ready.\n\n")
        }
    } else {
        warning("Download CORINE raster file from copernicus at https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download and place the folder into the resources/raw_data folder.\n")
        warning("You will need an account to download it, and it might take some time, sorry.")
        stop(paste0("Cannot find file '", corine_tiff_path,"' are you sure that you downloaded it manually before ? If not, follow instructions under this message."))
    }
}