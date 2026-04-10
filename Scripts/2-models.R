##### Imports #####
library(tidyverse)
library(tidyterra)

source(here::here("R/utils_data.R"))
source(here::here("R/utils_figures.R"))
source(here::here("resources/config.R")) # Import global parameters


##### Parameters #####
### CAN BE MODIFIED
VERBOSE <- TRUE
SHOW_PLOTS <- TRUE

# BIODICAPT aims at sampling 20+ farm plots within each research network 
# There are 4 network currently in the project.
# With the unevitable mistake of recording or in communication, let's say we
# have 15 farm plots per network with actual observations.
n_samples <- 4 * 15     


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
    )
)
new_names = c("Others", "Fruit trees", "Extensive fields", "Agro-natural areas")
new_colors = c("grey90", "#E97882", "#E6C65C", "#5B7EC9")

# Model parameters
# As this script is only made to prepare the code for later explorations, values
# are chosen arbitrarily and not backed up by actual scientific knowledge.
# Beta_j: effect of the land cover. 
Beta_j <- c(
    "Others" = NA,            # urban areas, water bodies, etc. are not modelled
    "Extensive fields" = -3,
    "Pastures" = -2,
    "Fruit trees" = -1.5,
    "Agro-natural areas" = 0
)
# Beta_1: effect of the temperature. 
Beta_1 <- 0.36


##### Main Loop #####
if (sys.nframe() == 0) {
    ##### 1. Preparing datasets #####
    ### 1.1. Data importation ###
    if (VERBOSE){
        cat("Loading rasters...")    
    }
    raster_AAT <- terra::rast(file.path(PROCESSED_DATA_PATH, paste0("CHELSA_Celsius_AAT_", OBS_YEAR, "_res", RES_KM, ".tif"))) # AAT : average annual temperature
    raster_CLC <- terra::rast(file.path(PROCESSED_DATA_PATH, paste0("CLC2018_WGS84_custom_france_res", RES_KM, "_simplified.tif")))
    # rename CLC layer for readability
    names(raster_CLC) <- "land_cover"
    rat <- cats(raster_CLC)[[1]]
    names(rat)[2] <- "land_cover" 
    levels(raster_CLC) <- rat
    if (SHOW_PLOTS){
        plot(raster_AAT, col=turbo(100))    
        plot(raster_CLC)
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
            new_group_name = new_names[i],
            new_group_color = new_colors[i])    
    }
    
    # Manual color remapping (for better readability of pastures, value = 5)
    # to know what land_cover corresponds to each value, run $cats(raster_CLC_crops)
    color_table <- coltab(raster_CLC_crops)[[1]]
    new_color <- col2rgb("#6FC05F")
    color_table[color_table$value == 5, c("red", "green", "blue")] <- c(new_color[1], new_color[2], new_color[3])
    coltab(raster_CLC_crops) <- color_table
    if (VERBOSE){
        cat(paste0(
            "New categories in CLC raster (n=",  
            length(unique(raster_CLC_crops)[[1]]), 
            "):\n", 
            paste0(paste0("\t- ",unique(raster_CLC_crops)[[1]]), collapse=",\n"), 
            ".\n\n",
            "For information, repartition of cells in raster:\n"
        ))  
        temp_ <- as.data.frame(raster_CLC_crops) %>% 
            dplyr::count(land_cover, sort = TRUE) %>%
            dplyr::mutate(freq = n / sum(n) * 100)
        print(temp_)
    }
    if (SHOW_PLOTS){
        plot(raster_CLC_crops)    
    }
    # Save custom raster
    writeRaster(
        raster_CLC_crops, 
        file.path(SIMULATION_PATH, paste0("CLC_custom_groups", OBS_YEAR, "_res", RES_KM, ".tif")), 
        overwrite = TRUE)
    
    
    ##### 2. TODO : Get BIODICAPT sampling locations data #####
    ### random for now, we'll use actual locations later.
    # mask cells that should not be picked (NAs and "Others")
    raster_CLC_crops_masked <- terra::deepcopy(raster_CLC_crops)
    raster_CLC_crops_masked[is.na(raster_CLC_crops) | raster_CLC_crops == "Others"] <- NA
    
    # random sampling
    sampled_cells <- terra::spatSample(
        raster_CLC_crops_masked, 
        size = n_samples, 
        method = "random", 
        as.points = TRUE,
        na.rm = TRUE)
    sampled_cells_df <- as.data.frame(sampled_cells, geom = "XY")
    sampled_cells_df <- sf::st_as_sf(
        sampled_cells_df, 
        coords = c("x", "y"), 
        crs = 4326)
    if (SHOW_PLOTS){
        plot_sampled_areas <- ggplot_xy_points_scattered_on_france_map(
            ggplot_get_france_base_map(), 
            sampled_cells_df)
        print(plot_sampled_areas)
    }

    # Extract observation values (temperature) from rasters
    extracted_AAT <- terra::extract(raster_AAT, sampled_cells_df)
    sampled_cells_df$temperature <- extracted_AAT[, 2]
    

    ##### 3. Generate observations #####
    ### 3.0. Theoretical model ###
    # WARNING: VERY EXPLORATIVE WORK WITH NO ACTUAL MEANING IN TERMS OF SPECIES
    # DISTRIBUTION. THE FOLLOWING MODELS ARE USED FOR OED* EXPLORATION.
    # *OED: Optimal Experimental Design
    
    # Ok, now that its clear, we chose a simple model (see `BIODICAPT_model.md`)
    # of presence-absence for a theoretical species. 
    # Each observation (i) of the species (y) is defined as follow:
    # y_i ~ Bernoulli(\theta_i)
    # logit(\theta_i) = \Beta_j + B_1 . T_i
    # with Beta_j, B_1 ~ Normale(0, 1.5)
    save(
        Beta_j, Beta_1, 
        file = file.path(SIMULATION_PATH, "Parameters_for_simulation.RData")
    )

    ### 3.1 Theoretical distribution (theta_i: probability) ###
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
    sampled_cells_df$theta <- stats::plogis(
        Beta_j[as.character(sampled_cells_df$land_cover)] +
            (Beta_1 * sampled_cells_df$temperature)
    )
    
    # Draw Bernoulli observations
    sampled_cells_df$species_was_observed <- as.factor(rbinom(
        n = nrow(sampled_cells_df),
        size = 1,
        prob = sampled_cells_df$theta
    ))
    sampled_cells_df$species_was_observed  # rename items
    
    # Show simulated observations
    if (SHOW_PLOTS) {
        map_obs <- ggplot_categorical_raster_on_france_map(
            ggplot_get_france_base_map(), 
            sampled_cells_df, 
            column="species_was_observed"
            )
        print(map_obs)
    }

    # Save simulated observations
    data.table::fwrite(
        sampled_cells_df, 
        file.path(SIMULATION_PATH, paste0("Observations_", OBS_YEAR, ".csv"))
    )
    
    
    ##### 4. Fit model on observations #####
    ### 4.1 Basic frequentist model
    freq_model <- glm(
        species_was_observed ~ land_cover + temperature,
        data = sampled_cells_df,
        family = binomial(link = "logit")
    )
    
    
    # 
    # if (VERBOSE) {
    #     cat("GLM fitted coefficients vs. true parameters:\n")
    #     coef_table <- as.data.frame(summary(model_fit)$coefficients)
    #     coef_table$true_value <- NA_real_
    # 
    #     # Match land-cover coefficients
    #     for (cat_name in crop_categories) {
    #         row_name <- paste0("land_cover", cat_name)
    #         if (row_name %in% rownames(coef_table)) {
    #             coef_table[row_name, "true_value"] <- Beta_j[cat_name]
    #         }
    #     }
    #     coef_table["temperature", "true_value"] <- B_1
    #     print(round(coef_table, 3))
    #     cat("\n")
    # }
    # 
    # ### 4.2. Predict occupancy probability over the full raster ###
    # # Build a prediction grid from the CLC + AAT rasters
    # pred_df <- as.data.frame(c(raster_CLC_crops, raster_AAT), na.rm = FALSE)
    # colnames(pred_df) <- c("land_cover", "temperature")
    # pred_df$land_cover <- as.character(pred_df$land_cover)
    # 
    # # Only predict on agricultural cells
    # agri_mask <- !is.na(pred_df$land_cover) & pred_df$land_cover != "Others"
    # pred_df$theta_hat <- NA_real_
    # pred_df$theta_hat[agri_mask] <- predict(
    #     model_fit,
    #     newdata = pred_df[agri_mask, ],
    #     type    = "response"
    # )
}
