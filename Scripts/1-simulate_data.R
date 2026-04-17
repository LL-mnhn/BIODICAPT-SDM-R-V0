# WARNING: VERY EXPLORATIVE WORK WITH NO ACTUAL MEANING IN TERMS OF SPECIES
# DISTRIBUTION. THE FOLLOWING MODELS ARE USED FOR OED* EXPLORATION.
# *OED: 
#
# Ok, now that its clear: we're gonna use a simple model of presence-absence 
# for an entirely theoretical species. We will refere to it as 
# "model 1 (Bernoulli 2 variables)", see `BIODICAPT_model.md` for details.
#
# Each observation (i) of the species (y) is defined as follow:
# y_i ~ Bernoulli(\theta_i)
# logit(\theta_i) = \Beta_j + Beta_1 . T_i
# with Beta_j, Beta_1 ~ Normal
# 
# Now let's simulate some data!

##### Imports #####
library(tidyverse)
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
set.seed(43)        # for reproducible results

# There are too many categories in CORINE Land Cover, modification hereafter:
new_groups = list(
    c(   # grouping together urban/water/etc. -> not modelled
        "Artificial surfaces",
        "Forest and semi-natural areas",
        "Wetlands",
        "Water bodies",
        "NODATA"
    ),
    c(  # grouping together """tree""" agriculture
        "Fruit trees and berry plantations",
        "Olive groves",
        "Vineyards"
    ),
    c(  # grouping together """extensive""" agriculture
        "Non-irrigated arable land",
        "Permanently irrigated land",
        "Rice fields",
        "Complex cultivation patterns"
    ),
    c(  # grouping together argo-natural agricultural systems
        "Land principally occupied by agriculture, with significant areas of natural vegetation",
        "Agro-forestry areas",
        "Annual crops associated with permanent crops"
    ),
    c(  # Single one for pastures
        "Pastures"
    )
)
new_raster_names = c("Others", "Fruit trees", "Extensive fields", "Agro-natural areas", "Pastures")
new_raster_colors = c("gray90", PALETTE[1], PALETTE[6], PALETTE[3], PALETTE[2])

# Model parameters
# As this script is only made to prepare the code for later explorations, values
# are chosen arbitrarily and not backed up by actual scientific knowledge.
# Beta_1: effect of the temperature. 
Beta_1 <- 1.33
# Beta_j: effect of the land cover. 
Beta_j <- c(
    "Others" = NA,            # urban areas, water bodies, etc. are not modelled
    "Extensive fields" = Beta_1*1,
    "Pastures" = Beta_1*0.2,
    "Fruit trees" = -Beta_1*0.5,
    "Agro-natural areas" = Beta_1*1.5
) 
Beta_j <- Beta_j - (Beta_1*9)   # base intercept (centered around 9°C)


##### Main Loop #####
if (sys.nframe() == 0) {
    ##### 1. Preparing datasets #####
    ### 1.1. Data importation ###
    if (VERBOSE){
        cat("Loading rasters...")    
    }
    raster_AAT <- terra::rast(file.path(
        PROCESSED_DATA_PATH_RASTER, 
        paste0("CHELSA_Celsius_AAT_", OBS_YEAR, "_res", RES_KM, ".tif")
    )) # AAT : average annual temperature
    raster_CLC <- terra::rast(file.path(
        PROCESSED_DATA_PATH_RASTER, 
        paste0("CLC2018_WGS84_custom_france_res", RES_KM, "_simplified.tif"))
    )
    
    # rename CLC layer for readability
    names(raster_CLC) <- "land_cover"
    rat <- cats(raster_CLC)[[1]]
    names(rat)[2] <- "land_cover" 
    levels(raster_CLC) <- rat
    if (SHOW_PLOTS){
        g_AAT <- ggplot_quantitative_raster_on_france_map(
            base_map = ggplot_get_france_base_map(),
            raster = raster_AAT,
            colname = "mean"
        )
        print(g_AAT)

        g_CLC <- ggplot_categorical_raster_on_france_map(
            base_map = ggplot_get_france_base_map(),
            raster = raster_CLC,
            layer_name = "land_cover"
        )
        print(g_CLC)
    }

    ### 1.2. Data importation ###
    # Why: there are way too many categories for me to consider.
    # In BIODICAPT, we are only interest in agricultural land, everything else
    # will be discarded. In addition, we will group some categories together
    # to limit the number of categories and parameters to compute
    # (some of these choices are arbitrary and *I know* highly questionable, but
    # this is a theoretical model, not the reality, gimme a break).
    raster_CLC_crops <- terra::deepcopy(raster_CLC)
    if (VERBOSE){
        cat(paste0(
            "Initial categories in CLC raster (n=",  
            length(unique(raster_CLC_crops)[[1]]), 
            "):\n", 
            paste0(paste0("\t- ",unique(raster_CLC_crops)[[1]]), collapse=",\n"), 
            ".\n\n"
        ))     
    }
    for (i in 1:length(new_groups)){
        raster_CLC_crops <- group_CLC_categories(
            raster = raster_CLC_crops, 
            cats_2_group = new_groups[[i]], 
            new_group_name = new_raster_names[i],
            new_group_color = new_raster_colors[i])    
    }
    if (SHOW_PLOTS){
        g_CLC_crops <- ggplot_categorical_raster_on_france_map(
            base_map = ggplot_get_france_base_map(),
            raster = raster_CLC_crops,
            layer_name = "land_cover"
        )
        print(g_CLC_crops)
    }
    
    # Save custom raster
    writeRaster(
        raster_CLC_crops, 
        file.path(SIMULATION_PATH, paste0("CLC_custom_groups", OBS_YEAR, "_res", RES_KM, ".tif")), 
        overwrite = TRUE)
    
    
    ##### 2. Get BIODICAPT sampling locations data #####
    ### 2.1. Importation of crops in the BIODICAPT research network
    # import known locations of land crops within the research network
    network_df <- data.table::fread(
        file.path(PROCESSED_DATA_PATH_CSV, "BIODICAPT_survey_data.csv")
    )
    network_df <- sf::st_as_sf(network_df, coords = c("LON", "LAT"), crs = 4326)
    
    # extract information from rasters
    extracted_CLC <- terra::extract(raster_CLC_crops, network_df)
    network_df$land_cover <- extracted_CLC[, 2]
    extracted_AAT <- terra::extract(raster_AAT, network_df)
    network_df$temperature <- extracted_AAT[, 2]
    
    
    # Actually, we have much more precise land_cover information : the surveys
    # We only need to translate from french to english
    translations <- c(
        "vigne"          = "Agro-natural areas",
        "grande_culture" = "Extensive fields",
        "verger"         = "Fruit trees",
        "prairie"        = "Pastures"
    )
    network_df$land_cover <- translations[network_df$culture]
    
    
    ##### 3. Generate observations #####
    ### 3.0. Theoretical model () ###
    # Each observation (i) of the species (y) is defined as follow:
    # y_i ~ Bernoulli(\theta_i)
    # logit(\theta_i) = \Beta_j + Beta_1 . T_i
    # with Beta_j, Beta_1 ~ Normal
    #
    # Let's try to better define Beta_j and Beta_1

    ### 3.1 Theoretical distribution ###
    ### 3.1.1. Exploring the credible range of theta_i
    n_lines <- 100 # number of random lines to generate
    temperature_range <- seq(
        min(pretty_range(network_df$temperature, multiple = 2)), 
        max(pretty_range(network_df$temperature, multiple = 2)),
        0.25)
    # temperature_range <- seq(-15, 15, 0.1)
    
    # We assuming Beta_j have similar priors even though they could be different
    Beta_1_mean = 1.5 
    Beta_1_sd = 2.5
    Beta_j_mean = -(Beta_1_mean*9) 
    Beta_j_sd = 2.5
    lines_df <- data.frame()
    for (i in 1:n_lines){
        rd_Beta_1 <- rnorm(1, mean = Beta_1_mean, sd = Beta_1_sd)
        rd_Beta_j <- rnorm(1, mean = Beta_j_mean, sd = Beta_j_sd) # centered around 9°C
        
        temp_df <- data.frame(
            x = temperature_range, 
            y = plogis(rd_Beta_j + (rd_Beta_1 * temperature_range)),
            line = i)
        lines_df <- rbind(lines_df, temp_df)
    }
    
    # Show results with ggplot2
    # For our species, we assume that its presence 
    if (SHOW_PLOTS){
        g_bernoulli_rd_range <- ggplot(
            lines_df, 
            aes(
                x = x, 
                y = y, 
                group = line,
                color = ""
            )) +
            scale_color_manual(values = PALETTE[3], guide = "none") +
            geom_line(alpha = 0.3) +
            labs(x = "T (°C)", y = "P(presence)") +
            scale_x_continuous(limits = c(6, 12), expand = expansion(mult = 0)) +
            scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0))
        print(my_custom_ggplot_theme(g_bernoulli_rd_range, with_palette=FALSE))
    }
    
    
    # Ok so let's have:
    #   Beta_j ~ Normal(0, 5): huge variations possible
    #   and Beta_1 ~ Normal(0.5, 0.33): the probability of presence increases 
    #   with temperature, but with a wide range of possibilities
    #
    # In Bayesian statistics, we would say these are low-information priors
    # They give some kind of information, but are not based on literature.
    # It is the credible range of the data.
    
    # Show chosen parameters VS their prior ranges
    if (SHOW_PLOTS){
        # Beta_1
        plot_Beta_1 <- normal_density_plot_with_thresholds(
            Beta_1_mean, Beta_1_sd, Beta_1)
        print(plot_Beta_1)
        # Beta_j
        plot_Beta_j <- normal_density_plot_with_thresholds(
            Beta_j_mean, Beta_j_sd, Beta_j[!is.na(Beta_j)])
        print(plot_Beta_j)
    }
    
    
    ### 3.1.2. Distribution laws for our parameters
    # Now that we confirmed that our chosen parameters (see headers) are within
    # a credible range for our model, let's visualise what they do
    if (SHOW_PLOTS) {
        # 4 modes for the qualitative variable
        # Show results as 4 graphs
        model_df <- data.frame()
        names_Beta_j <- names(Beta_j[!is.na(Beta_j)])
        
        for (i in 1:length(names_Beta_j)) {
            temp_df <- data.frame(
                temperature = temperature_range,
                land_cover = rep(names_Beta_j[i], length(temperature_range)),
                bernoulli = plogis(Beta_j[names_Beta_j[i]] + Beta_1 * temperature_range)
            )
            model_df <- rbind(model_df, temp_df)
        }
        
        graph_densities <- plot_grid_scattered_densities(
            df_lines = model_df, 
            x="temperature", 
            y="bernoulli", 
            category="land_cover")
        print(graph_densities)
    }
        
    
    ### 3.1.3. Data simulation
    # Create a new raster 
    raster_Beta_j <- terra::deepcopy(raster_CLC_crops)
    temp_data_table <- cats(raster_CLC_crops)[[1]]
    # Inject Beta_j values
    temp_data_table$Beta_j <- Beta_j[temp_data_table$land_cover]
    levels(raster_Beta_j) <- temp_data_table
    raster_Beta_j <- catalyze(raster_Beta_j)$Beta_j
    # Compute theta_i (plogis is the opposite of logit)
    raster_logit_theta <- terra::app(
        raster_Beta_j + Beta_1 * raster_AAT, 
        stats::plogis)
    
    # Make a beautiful plot
    if (SHOW_PLOTS){
        plot_theoretical_map <- ggplot_quantitative_raster_on_france_map(
            ggplot_get_france_base_map(), 
            raster = raster_logit_theta,
            colname= "lyr.1" ,
            unit = "P(presence)",
            limits = c(0,1))
        print(plot_theoretical_map)
    }
    # Save simulation raster
    writeRaster(
        raster_logit_theta, 
        file.path(SIMULATION_PATH, paste0("Simulated_theta_", OBS_YEAR, "_res", RES_KM, ".tif")), 
        overwrite = TRUE)
    

    ##### 3.2. Simulate presence/absence at sampled locations #####
    # Compute theta_i at each observation point
    network_df$theta <- stats::plogis(
        Beta_j[as.character(network_df$land_cover)] +
            (Beta_1 * network_df$temperature)
    )
    
    # Draw Bernoulli observations
    network_df$presence_absence <- as.factor(rbinom(
        n = nrow(network_df),
        size = 1,
        prob = network_df$theta
    ))
    # rename items
    network_df$observation <- factor(network_df$presence_absence, 
                         levels = c(0, 1), 
                         labels = c("Species not observed", "Species observed"))
    
    # Show simulated observations
    if (SHOW_PLOTS) {
        map_obs <- ggplot_categorical_df_on_france_map(
            ggplot_get_france_base_map(), 
            network_df, 
            column="observation"
            )
        print(map_obs)
    }

    # Save simulated observations
    data.table::fwrite(
        network_df, 
        file.path(SIMULATION_PATH, paste0("Simulation_results.csv"))
    )
    save(
        Beta_j, Beta_1, 
        file = file.path(SIMULATION_PATH, "Parameters_for_simulation.RData")
    )
}
