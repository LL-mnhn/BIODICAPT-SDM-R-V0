##### Imports #####
library(data.table)
source(here::here("utils/utils_data.R"))


##### Parameters #####
OBS_YEAR = "2018"

RAW_DATA_PATH = file.path("resources", "raw_data")
PROCESSED_DATA_PATH = file.path("resources", "preprocessed_data")


##### Main Loop #####
if (sys.nframe() == 0) {
    # Yes I come from Python, how could you tell ?
    # Anw, this 'if' means the following instructions will run only when 
    # this script is the top-level call
    
    ##### Meteo France dataset ####
    # Import and save raw dataset on first use only
    if (!file.exists(file.path(RAW_DATA_PATH, paste0("meteo_france_", OBS_YEAR, ".csv")))){
        # dataset is available online through an API
        meteo_csv_urls = get_meteo_urls()
        full_meteo_df = load_url_2_df(
            url_list=meteo_csv_urls,
            keep_year=OBS_YEAR,
            verbose=TRUE
        ) 
    
        # Save for future reference
        fwrite(
            full_meteo_df, 
            file.path(RAW_DATA_PATH, paste0("meteo_france_", OBS_YEAR, ".csv"))
        )
    }
    
    # Use the saved dataset to make reproducible changes
    full_meteo_df = fread(file.path(RAW_DATA_PATH, paste0("meteo_france_", OBS_YEAR, ".csv")))
    
    # Transform dataset from monthly to yearly data
    obs_year_meteo_df = select_year_round_obs(full_meteo_df)
    
    # exclude rows with NAs
    obs_year_clean_meteo_df = obs_year_meteo_df %>% 
        filter(!is.na(TX), !is.na(TN), !is.na(TM))
    
    # Save preprocessed dataframe
    fwrite(
        obs_year_clean_meteo_df, 
        file.path(PROCESSED_DATA_PATH, paste0("meteo_france_", OBS_YEAR, "_annual_means.csv"))
    )
    
    ### Meteo France meshgrid ###
    
    
    
    ##### CORINE Land Cover 2018 Dataset ####
    # I'm too lazy to get it from the API sorry, maybe I'll update it later
    cat("Download CORINE raster file from copernicus at https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download")
    cat("You will need an account for that, anoying I know.")
}