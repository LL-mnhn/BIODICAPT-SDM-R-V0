# WARNING: VERY EXPLORATIVE WORK WITH NO ACTUAL MEANING IN TERMS OF SPECIES
# DISTRIBUTION.
#
# Using Model 1 (Bernoulli 2 variables):
# y_i ~ Bernoulli(θ_i)
# logit(θ_i) = β_j + β_1 * T_i

##### Imports #####
library(tidyverse)
library(terra)
library(tidyterra)
library(viridisLite)

source(here::here("R/utils_data.R"))
source(here::here("R/utils_figures.R"))
source(here::here("resources/config.R")) # Import global parameters


##### Parameters #####
### CAN BE MODIFIED
VERBOSE <- TRUE
SHOW_PLOTS <- TRUE

### DO NOT MODIFY
set.seed(43)     # for reproducible results 
# Simplification of CORINE categories
# Arbitrary choices: BIODICAPT is only interested in agricultural fields
NEW_RASTER_GROUPS = list(
    c("Artificial surfaces", "Forest and semi-natural areas", "Wetlands",
      "Water bodies", "NODATA"),
    c("Non-irrigated arable land", "Permanently irrigated land",
      "Complex cultivation patterns"),
    c("Land principally occupied by agriculture, with significant areas of natural vegetation",
      "Agro-forestry areas", "Rice fields", "Olive groves", "Vineyards",
      "Annual crops associated with permanent crops", "Pastures",
      "Fruit trees and berry plantations")
)
NEW_RASTER_NAMES = c("Others", "Extensive fields", "Agro-natural fields")
NEW_RASTER_COLORS = c("gray90", PALETTE[6], PALETTE[2])
TRANSLATIONS_BIODICAPT <- c( 
    "vigne"          = "Agro-natural fields",
    "grande_culture" = "Extensive fields",
    "verger"         = "Agro-natural fields",
    "prairie"        = "Agro-natural fields"
)
TRANSLATIONS_500ENI <- c( 
    "Vigne"  = "Agro-natural fields",
    "Ble"    = "Extensive fields",
    "Mais"   = "Extensive fields",
    "Salade" = "Extensive fields"
)

# Prior distribution of parameters (weakly informative)
PRIOR_SD <- 10
PRIOR_MEAN <- 0
PRIOR_CROSS <- 9 # where P(presence) = 0.5, 9°C here for our fictive species

# Parameters for simulation (hand picked, kinda ideal)
BETAS <- list(
    AAT = c(temperature = 5),
    CLC = c("Others" = NA, 
            "Extensive fields" = -(5*8.25), 
            "Agro-natural fields" = -(5*10.5))
)


##### Helper/Wrapper Functions #####
load_rasters <- function(verbose = VERBOSE, show_plots = SHOW_PLOTS) {
    if (verbose){
        cat("Loading rasters...")    
    }
    # load pre-processed raster of average annual temperatures (AAT)
    raster_AAT <- terra::rast(file.path(
        PROCESSED_DATA_PATH_RASTER, 
        paste0("CHELSA_Celsius_AAT_", OBS_YEAR, "_res", RES_KM, ".tif")
    ))
    # load pre-processed raster of CORINE Land Cover data
    raster_CLC <- terra::rast(file.path(
        PROCESSED_DATA_PATH_RASTER, 
        paste0("CLC2018_WGS84_custom_france_res", RES_KM, "_simplified.tif"))
    )
    
    # Set proper names and categories
    names(raster_CLC) <- "land_cover"
    rat <- cats(raster_CLC)[[1]]
    names(rat)[2] <- "land_cover" 
    levels(raster_CLC) <- rat
    
    if (show_plots){
        g_AAT <- ggplot_quantitative_raster_on_france_map(
            base_map = ggplot_get_france_base_map(),
            raster = raster_AAT,
            colname = "mean") + 
            ggplot2::labs(title = "Map of annual average temperature (°C)")
        print(g_AAT)
        
        g_CLC <- ggplot_categorical_raster_on_france_map(
            base_map = ggplot_get_france_base_map(),
            raster = raster_CLC,
            layer_name = "land_cover") + 
            ggplot2::labs(title = "CORINE land cover map (2018)")
        print(g_CLC)
    }
    
    return(list(aat = raster_AAT, clc = raster_CLC))
}

simplify_corine_raster <- function(
        raster, 
        verbose = VERBOSE, show_plots = SHOW_PLOTS) {
    if (verbose){
        unique_cats <- unique(raster)[[1]]
        cat(paste0("Initial CLC categories (n=", length(unique_cats), "):\n",
                   paste0("\t- ", unique_cats, collapse=",\n"), ".\n\n")) 
    }
    
    # simplify categories by replacing them one by one in the raster
    raster_simplified <- terra::deepcopy(raster)
    for (i in seq_along(NEW_RASTER_GROUPS)) {
        raster_simplified <- group_CLC_categories(
            raster = raster_simplified,
            cats_2_group = NEW_RASTER_GROUPS[[i]],
            new_group_name = NEW_RASTER_NAMES[i],
            new_group_color = NEW_RASTER_COLORS[i]
        )
    }
    
    if (show_plots){
        g_CLC_simplified <- ggplot_categorical_raster_on_france_map(
            base_map = ggplot_get_france_base_map(),
            raster = raster_simplified,
            layer_name = "land_cover") +
            ggplot2::labs(title = "Simplified CORINE land cover map (2018)")
        print(g_CLC_simplified)
    }
    
    # Save custom raster
    writeRaster(
        raster_simplified, 
        file.path(SIMULATION_PATH, paste0("CLC_custom_groups", OBS_YEAR, "_res", RES_KM, ".tif")), 
        overwrite = TRUE)
    
    return(raster_simplified)
}

load_network_df <- function(
        path, raster_aat, raster_clc, project, show_plots = SHOW_PLOTS) {
    # import sampled locations within the research network
    network_df <- data.table::fread(path)
    if (project == "BIODICAPT") {
        x_lon <- "LON"
        y_lat <- "LAT"
        translations <- TRANSLATIONS_BIODICAPT
        crop_type <- network_df$culture
        color_type <- "network"
    } else if (project == "500ENI") {
        x_lon <- "X"
        y_lat <- "Y"
        translations <- TRANSLATIONS_500ENI
        crop_type <- network_df$plante_reference
        color_type <- NULL
    } else {
        stop(paste0(
            "Was expection project in c('BIODICAPT', '500ENI'), got ", 
            project))
    }
    
    network_df <- sf::st_as_sf(
        network_df, coords = c(x_lon, y_lat), crs = 4326, remove = FALSE) 
    
    # extract information from rasters
    network_df$temperature <- terra::extract(raster_aat, network_df)[, 2]
    network_df$land_cover <- translations[crop_type]   

    
    if (show_plots) {
        g_research_network <- ggplot_categorical_df_on_france_map(
            base_map = ggplot_get_france_base_map(),
            df = network_df,
            column = color_type,
            LON = x_lon,
            LAT = x_lat) +
            ggplot2::labs(title = paste0("Sampling locations for ", project, "'s research network"))
        print(g_research_network)
        
        g_hist_research_network <- ggplot_bars(
            df = network_df,
            x = "temperature",
            category = "land_cover",
            binwidth = 0.1) +
            ggplot2::labs(title = "Distribution of modelled temperatures in the research network.") +
            ggplot2::scale_x_continuous(
                limits = c(
                    min(pretty_range(network_df$temperature, multiple = 2)), 
                    max(pretty_range(network_df$temperature, multiple = 2))), 
                expand = expansion(mult = 0)) 
        print(g_hist_research_network)
        
        
    }
    
    return(network_df)
}

show_credible_range_of_thetas <- function(
        obs_df,       # dataframe with temperatures, land covers and locations
        n_lines = 100, # number of random lines to generate
        show_plots = SHOW_PLOTS) {
    # x-axis limits
    temperature_range <- seq(
        min(pretty_range(obs_df$temperature, multiple = 2)),
        max(pretty_range(obs_df$temperature, multiple = 2)),
        0.25)

    # Turn theta into a distribution
    distributions <- data.frame()
    for (i in 1:n_lines){
        rd_Beta_1 <- rnorm(1, mean = PRIOR_MEAN, sd = PRIOR_SD)
        rd_Beta_j <- rnorm(1, mean = -PRIOR_CROSS*rd_Beta_1, sd = PRIOR_SD)

        temp_df <- data.frame(
            x = temperature_range,
            y = plogis(rd_Beta_j + (rd_Beta_1 * temperature_range)),
            line = i)
        distributions <- rbind(distributions, temp_df)
    }

    # Show results with ggplot2
    if (show_plots){
        g_range_of_thetas <- ggplot(
            distributions,
            ggplot2::aes(
                x = x,
                y = y,
                group = line,
                color = ""
            )) +
            ggplot2::scale_color_manual(values = PALETTE[3], guide = "none") +
            ggplot2::geom_line(alpha = 0.3) +
            ggplot2::labs(
                title = "Probability of presence for random Beta values",
                x = "T (°C)", 
                y = "P(presence)",
                caption = paste0("P(presence) = plogis(Beta_j + Beta_1 * temperature), with Beta ~ Normal(", PRIOR_MEAN, ", sd=", PRIOR_SD, ").")) +
            ggplot2::scale_x_continuous(
                limits = c(min(temperature_range), max(temperature_range)), 
                expand = expansion(mult = 0)) +
            ggplot2::scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0))
        
        print(my_custom_ggplot_theme(g_range_of_thetas, with_palette=FALSE))
    }
    
}

show_temperature_distrib_vs_probability <- function(
        obs_df, 
        show_plots = SHOW_PLOTS) {
    # x-axis limits
    temperature_range <- seq(
        min(pretty_range(obs_df$temperature, multiple = 2)),
        max(pretty_range(obs_df$temperature, multiple = 2)),
        0.25)
    
    # make P(presence) for each CLC category
    distributions <- data.frame()
    for (j in seq_along(BETAS$CLC)) {
        if (is.na(BETAS$CLC[j])) {next}
        temp_df <- data.frame(
            x = temperature_range,
            y = plogis(BETAS$CLC[j] + (BETAS$AAT * temperature_range)),
            line = names(BETAS$CLC[j]))
        distributions <- rbind(distributions, temp_df)
    }
    
    if (show_plots) {
        # Scaling factor for distributions
        max_count <- max(hist(
            obs_df$temperature,
            breaks = seq(min(obs_df$temperature), max(obs_df$temperature) + 0.1, by = 0.1),
            plot = FALSE)$counts)
        
        
        g_hist_VS_P <- ggplot_bars(
            df = obs_df,
            x = "temperature",
            category = "land_cover",
            binwidth = 0.1) +
            ggplot2::labs(title = "Modelled temperatures per land cover VS P(presence).") +
            ggplot2::scale_x_continuous(
                limits = c(
                    min(pretty_range(obs_df$temperature, multiple = 2)), 
                    max(pretty_range(obs_df$temperature, multiple = 2))), 
                expand = expansion(mult = 0)) +
            ggplot2::geom_line(
                data = distributions,
                ggplot2::aes(x = x, y = y * max_count, color = line),
                linewidth = 0.8,
                inherit.aes = FALSE,
                show.legend = "none") +
            ggplot2::scale_y_continuous(
                name = "Occurences of temperatures",
                limits = c(0, max_count),
                sec.axis = sec_axis(
                    transform = ~ . / max_count,   # inverse transform back to [0,1]
                    name = "Simulation of P(presence)",
                    breaks = seq(0, 1, by = 0.2)
                )
            )
        print(g_hist_VS_P)
    }
}

show_probability_map <- function(
        categories_raster, temperature_raster, show_plots = SHOW_PLOTS) {
    # copy map of categories
    raster_betas_clc <- terra::deepcopy(categories_raster)
    
    # convert names to simulation values
    categories_in_raster <- cats(categories_raster)[[1]]
    categories_in_raster$beta_clc <- BETAS$CLC[categories_in_raster$land_cover]
    
    # inject simulation values in raster
    levels(raster_betas_clc) <- categories_in_raster
    raster_betas_clc <- catalyze(raster_betas_clc)$beta_clc
    
    # compute thetas
    raster_probability <- terra::app(
        raster_betas_clc + BETAS$AAT * temperature_raster,
        stats::plogis)

    # Make a beautiful plot
    if (show_plots){
        plot_theoretical_map <- ggplot_quantitative_raster_on_france_map(
            ggplot_get_france_base_map(),
            raster = raster_probability,
            colname= "lyr.1" ,
            unit = "P(presence)",
            limits = c(0,1))
        print(plot_theoretical_map)
    }
    
    # Save simulation raster
    writeRaster(
        raster_probability,
        file.path(SIMULATION_PATH, paste0("Simulated_theta_", OBS_YEAR, "_res", RES_KM, ".tif")),
        overwrite = TRUE)
    
    return(raster_probability)
}

simulate_presence_absence <- function(obs_df, show_plots = SHOW_PLOTS) {
    # Compute theta_i at each observation point
    obs_df$theta <- stats::plogis(
        BETAS$CLC[as.character(obs_df$land_cover)] +
            (BETAS$AAT * obs_df$temperature)
    )

    # Draw Bernoulli observations
    obs_df$presence_absence <- as.factor(rbinom(
        n = nrow(obs_df),
        size = 1,
        prob = obs_df$theta
    ))
    # rename items
    obs_df$observation <- factor(obs_df$presence_absence,
                         levels = c(0, 1),
                         labels = c("Species not observed", "Species observed"))

    # Show simulated observations
    if (show_plots) {
        map_obs <- ggplot_categorical_df_on_france_map(
            ggplot_get_france_base_map(),
            obs_df,
            column="observation"
            )
        print(map_obs)
    }

    return(obs_df)
}


##### Main Loop #####
if (sys.nframe() == 0) {
    ##### 1. Load and prepare data #####
    ### 1.1. Load rasters ###
    rasters <- load_rasters()
    raster_clc_custom <- simplify_corine_raster(rasters$clc)
    writeRaster(
        raster_clc_custom, 
        file.path(SIMULATION_PATH, paste0("CLC2018_WGS84_custom_france_res", RES_KM, "_simplified_custom.tif")), 
        overwrite = TRUE)

    ### 1.2. Load research/500ENI data ###
    BIODICAPT_df <- load_network_df(
        path = file.path(PROCESSED_DATA_PATH_CSV, "BIODICAPT_survey_data.csv"),
        project = "BIODICAPT",
        raster_aat = rasters$aat, 
        raster_clc = raster_clc_custom)
    ENI_df <- load_network_df(
        path = file.path(PROCESSED_DATA_PATH_CSV, "500ENI_survey_data.csv"),
        project = "500ENI",
        raster_aat = rasters$aat, 
        raster_clc = raster_clc_custom)

    
    ##### 2. Simulation #####
    ### 2.0. Visualise prior ranges ###
    show_credible_range_of_thetas(obs_df = BIODICAPT_df)

    ### 2.1. Create probability map from parameters ###
    show_temperature_distrib_vs_probability(
        obs_df = BIODICAPT_df)
    show_probability_map(
        categories_raster = raster_clc_custom,
        temperature_raster = rasters$aat)
    
    ### 2.2. Simulate presence/absence at sampled locations ###
    BIODICAPT_df <- simulate_presence_absence(BIODICAPT_df)
    ENI_df <- simulate_presence_absence(ENI_df)
    
    # 2.3. Save simulation and parameters
    data.table::fwrite(
        BIODICAPT_df,
        file.path(SIMULATION_PATH, paste0("BIODICAPT_simulation_results.csv"))
    )
    data.table::fwrite(
        ENI_df,
        file.path(SIMULATION_PATH, paste0("500ENI_Simulation_results.csv"))
    )
    save(
        BETAS,
        file = file.path(SIMULATION_PATH, "Parameters_for_simulation.RData")
    )
}
