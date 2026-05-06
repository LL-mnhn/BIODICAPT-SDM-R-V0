##### Imports #####
library(data.table)

source(here::here("R/utils_data.R"))
source(here::here("resources/config.R")) # Import global parameters

##### Parameters #####
### CAN BE MODIFIED
VERBOSE <- TRUE
PATH_FOLDER_RAW_XLSX <- file.path(".", "resources", "raw_data", "reseaux")
PATH_500ENI_CSV <- file.path(".", "resources", "raw_data", "500ENI", "table_parcelle_NA_excluded.csv")
NAMES_SAMPLES_XLSX <- c(
    "BIODICAPT_parcelles_MONTPELLIER.xlsx",
    "Preselection_Parcelles_BIODICAPT_SCARABEE.xlsx",
    "Preselection_Parcelles_BIODICAPT_DYNAFOR.xlsx",
    "Preselection_Parcelles_BIODICAPT_VCG.xlsx",
    "Preselection_Parcelles_BIODICAPT_ZAAR.xlsx")


### DO NOT MODIFY
set.seed(43)        # for reproducible results

XLSX_FILEPATHS <- c()
for (i in 1:length(NAMES_SAMPLES_XLSX)){
    XLSX_FILEPATHS[i] <- file.path(PATH_FOLDER_RAW_XLSX, NAMES_SAMPLES_XLSX[i])
}


##### Helper/Wrapper Functions #####
check_delete_existing_files <- function() {
    if (file.exists(file.path(PROCESSED_DATA_PATH_CSV, "BIODICAPT_survey_data.csv"))) {
        var <- readline(prompt = "Do you want to overwite ALL existing files\n(preprocessing, simulation, results and figures)?\n[Y/n] ")
        if (var == "Y") {           
            # remove old preprocessed data
            unlink(file.path(PROCESSED_DATA_PATH_RASTER, "*"))
            unlink(file.path(PROCESSED_DATA_PATH_CSV, "*"))

            # remove outputs
            unlink(file.path(FIGURES_PATH, "*"))
            unlink(file.path(RESULTS_PATH, "*"))
            unlink(file.path(SIMULATION_PATH, "*"))
        } else {
            stop("Script cannot proceed, existing files would be overwritten.")
        }
    }
}

process_xlsx_survey_data <- function(verbose = VERBOSE) {
    if (verbose) {
        cat("Loading and formatting BIODICAPT surveys...\n")
    }
    xlsx_df <- import_biodicapt_land_surveys(XLSX_FILEPATHS)
  
    # anonymize data
    xlsx_df <- blur_coordinates(xlsx_df, "LON", "LAT", RES_KM)
  
    # remove irrelevant columns
    xlsx_df[ ,c("code_local", "Contact_Viti", "Referent_Projet", "Lot", "ocsol2026",
                "IFT total", "IFT_data_year", "lineaire(m)_haies_buffer_1km",
                "%surface_haies_buffer_1km", "%surface_ESN", "...11", "...12"
    )] <- list(NULL)
    fwrite(
        xlsx_df, 
        file.path(PROCESSED_DATA_PATH_CSV, "BIODICAPT_survey_data.csv")
    )
    if (verbose) {
        cat("BIODICAPT survey data is ready (sample below).")
        # print(sample_n(xlsx_df, 6))
        print("\n\n")
    }
}

process_500ENI_survey_data <- function(verbose = VERBOSE) {
    if (verbose) {
        cat("Loading and formatting 500 ENI surveys...\n")
    }
    csv_df <- fread(PATH_500ENI_CSV)
   
    # remove irrelevant columns
    csv_df[ , c("id_parcelle", "code_parcelle", "nom_parcelle", "commune",
                 "lieu_dit", "code_postal", "pourcent_pente",
                 "commentaire_parcelle", "derniere_modif_parcelle_par",
                 "derniere_modif_parcelle_le", "derniere_modif_donnees_agro_par",
                 "derniere_modif_donnees_agro_le", "derniere_modif_pratiques_par",
                 "derniere_modif_pratiques_le"
    )] <- list(NULL)
     
    # anonymize data
    csv_df <- blur_coordinates(as.data.frame.matrix(csv_df), "X", "Y", RES_KM)
  
    fwrite(
        csv_df, 
        file.path(PROCESSED_DATA_PATH_CSV, "500ENI_survey_data.csv")
    )
    if (verbose) {
        cat("500ENI survey data is ready (sample below).")
        # print(sample_n(csv_df, 6))
        print("\n\n")
    }
}

LEGACY_import_meteo_france_data <- function(verbose = VERBOSE){
    # Import and save raw dataset on first use only
    if (!file.exists(file.path(RAW_DATA_PATH, paste0("meteo_france_", OBS_YEAR, ".csv")))){
        if (verbose) {
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
        fwrite(
            full_meteo_df,
            file.path(RAW_DATA_PATH, paste0("meteo_france_", OBS_YEAR, ".csv"))
        )
    }

    if (verbose) {
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
    fwrite(
        obs_year_clean_weather_df,
        file.path(PROCESSED_DATA_PATH_CSV, paste0("meteo_france_", OBS_YEAR, "_annual_means.csv"))
    )
    if (verbose) {
        cat("Meteo France data is ready.\n\n")
    }

    # Make grid from Meteo France Dataset
    obs_year_clean_weather_raster <- scattered_points_2_grid_on_france(
        file.path(PROCESSED_DATA_PATH_RASTER, paste0("meteo_france_", OBS_YEAR, "_annual_means.csv")),
        res_km = RES_KM,
        colname = "TM")
    writeRaster(
        obs_year_clean_weather_raster,
        file.path(PROCESSED_DATA_PATH_RASTER, paste0("meteo_france_", OBS_YEAR, "_res", RES_KM, "_annual_means.tif")),
        overwrite = TRUE)
}

process_chelsa_raster <- function(verbose = VERBOSE) {
    if (verbose) {
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
    if (verbose) {
        cat("Preparing rasters...\n")
    }
    
    # default format is per month, I need data per year and to a standard resolution
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
    if (verbose) {
        cat("CHELSA data is ready (plot `CHELSA_annual_raster` for visualisation).\n\n")
    }
    
}

process_corine_raster <- function(verbose = VERBOSE) {
    if (verbose) {
        cat("Loading CORINE Land Cover data from local folder...\n")
    }
    
    # File should be available locally (API download not necessary for 1 file)
    corine_tiff_path <- file.path(RAW_DATA_PATH, "u2018_clc2018_v2020_20u1_raster100m", "DATA", "U2018_CLC2018_V2020_20u1.tif")
    if (file.exists(corine_tiff_path)){
        if (verbose) {
            cat("Re-projecting CORINE Land Cover data...\n")
        }
        
        # change resolution to a standardised one
        corine_raster_new_res <- import_and_transform_corine_raster(
            corine_tiff_path,
            res_km = RES_KM
        )
        writeRaster(
            corine_raster_new_res, 
            file.path(PROCESSED_DATA_PATH_RASTER, paste0("CLC2018_WGS84_custom_france_res", RES_KM, ".tif")), 
            overwrite = TRUE)
        
        
        if (verbose) {
            cat("Grouping similar categories together...\n")
        }
        
        # CORINE subcategories can be grouped into categories
        # We are only interested in agricultural data, the others are reduced
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
        
        
        if (verbose) {
            cat("CORINE data is ready (plot `corine_raster_simple` for visualisation).\n\n")
        }
    } else {
        warning("Download CORINE raster file from copernicus at https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download and place the folder into the resources/raw_data folder.\n")
        warning("You will need an account to download it, and it might take some time, sorry.")
        stop(paste0("Cannot find file '", corine_tiff_path,"' are you sure that you downloaded it manually before ? If not, follow instructions under this message."))
    }
}


##### Main Loop #####
if (sys.nframe() == 0) {
    ##### 0. Clean previous run
    check_delete_existing_files()

    ##### 1. Import XLSX surveys from research network #####
    process_xlsx_survey_data()
    
    ##### 2. Import 
    process_500ENI_survey_data()
    
    ##### 2. Import weather data: CHELSA climate model ###
    process_chelsa_raster()
    
    ##### 3. CORINE Land Cover 2018 Dataset #####
    process_corine_raster()
}