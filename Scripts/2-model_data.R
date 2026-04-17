# Goal : import simulation data and try to model it using different strategies
# then, try to optimize the collection of data (OED).
#
# As a reminder : in this script, we consider model 1 (Bernoulli 2 variables):
# y_i ~ Bernoulli(\theta_i)
# logit(\theta_i) = \Beta_j + Beta_1 . T_i
# with Beta_j, Beta_1 ~ Normal
##### Imports #####
library(MASS)
library(tidyverse)
library(tidyterra)

source(here::here("resources/config.R")) # Import global parameters
source(here::here("R/utils_figures.R"))


##### Parameters #####
### CAN BE MODIFIED
VERBOSE <- TRUE
SHOW_PLOTS <- TRUE

### DO NOT MODIFY
set.seed(43)        # for reproducible results


##### LOCAL FUNCTION #####
# A function that uses the coefficient fitted under here to draw fitted curves
# It only works with model 1 (Bernoulli 2 variables).
compute_bernoulli_curves <- function(
        param_quali, 
        param_quanti,
        xrange,
        x = "Temperature (°C)",
        group = "land_cover",
        y = "Bernoulli density function") {
    model_df <- data.frame()
    
    for (i in 1:length(param_quali)) {
        temp_df <- data.frame(
            xrange,
            rep(names(param_quali)[i], length(xrange)),
            plogis(param_quali[i] + param_quanti * xrange)
        )
        temp_df <- setNames(temp_df, c(x, group, y))
        model_df <- rbind(model_df, temp_df)
    }
    
    return(model_df)
}



##### Main Loop #####
if (sys.nframe() == 0) {
    ##### 0. Import data #####
    load(file.path(
        SIMULATION_PATH, 
        "Parameters_for_simulation.RData"
    ))
    observations_df <- data.table::fread(file.path(
        SIMULATION_PATH, 
        "Simulation_results.csv"
    ))
    
    ##### 1. Fit models on observations #####
    ### 1.1. Classic: Frequentist approach ###
    ### 1.1.1. Model fit ###
    freq_model <- stats::glm(
        presence_absence ~ land_cover + temperature - 1,
        data = observations_df,
        family = binomial(link = "logit")
    )

    ### 1.1.2. Results ###
    if (VERBOSE) {
        cat("Modelisation using frequentist approach:\n")
        summary(freq_model)
    }
    if (SHOW_PLOTS) {
        # Extract coefficients
        freq_model_Beta_j <- c(
            "Extensive fields" = unname(stats::coef(freq_model)["land_coverExtensive fields"]),
            "Pastures" = unname(stats::coef(freq_model)["land_coverPastures"]),
            "Fruit trees" = unname(stats::coef(freq_model)["land_coverFruit trees"]),
            "Agro-natural areas" = unname(stats::coef(freq_model)["land_coverAgro-natural areas"])
        )
        freq_model_Beta_1 <- coef(freq_model)["temperature"]
        # Modelisation
        freq_model_df <- compute_bernoulli_curves(
            param_quali = freq_model_Beta_j, 
            param_quanti = freq_model_Beta_1,
            xrange = seq(5,12,0.1),
            x = "temperature",
            y = "presence_absence")
        # plot
        grid1 <- plot_grid_scattered_densities(
            df_points = observations_df, 
            df_lines = freq_model_df, 
            x = "temperature", 
            y = "presence_absence", 
            category ="land_cover")
        print(grid1)
    }
    
    ### 1.1.3. Comparison with original values ###
    # Convert to data frames
    df_original_params <- data.frame(
        category = names(c(Beta_j, temperature = Beta_1)), 
        original_value = as.numeric(c(Beta_j, temperature = Beta_1)))
    rownames(df_original_params) <- df_original_params$category
    df_estimated_params <- data.frame(
        category = names(c(freq_model_Beta_j, freq_model_Beta_1)), 
        estimated_value = as.numeric(c(freq_model_Beta_j, freq_model_Beta_1)))
    rownames(df_estimated_params) <- df_estimated_params$category
    # Merge them together
    freq_Beta_df <- merge(
        df_original_params, 
        df_estimated_params, 
        by = "category") 
    # Compute differences
    freq_Beta_df$difference <- freq_Beta_df$original_value - freq_Beta_df$estimated_value
    
    if (VERBOSE) {
        cat("Comparison of simulation values VS estimated values:\n")
        print(freq_Beta_df[c("category", "difference")])
    }
    if (SHOW_PLOTS) {
        # WARNING : 
        # WE ASSUME THAT ALL PARAMETERS FOLLOW A NORMAL DISTRIBUTION
        # These plots will reflect this hypothesis. Here, it is easy to make 
        # this assumption because the simulation uses normal distributions, but
        # IT IS NOT ALWAYS THAT SIMPLE
        
        coef_summary <- summary(freq_model)$coefficients
        rownames(df_original_params) <- df_original_params$category
        bell_range <- seq(-30, 5, length.out = 301)
        distrib_estimate_df <- data.frame()
        estimate_df <- data.frame()
        for (name in rownames(coef_summary)) {
            # Extract results for a specific coefficient
            estimate <- coef_summary[name, "Estimate"]
            se <- coef_summary[name, "Std. Error"]
            
            # Remove "land_cover" at the beginning of the names
            if (grepl("land_cover", name)) {
                clean_name <- substr(name, nchar("land_cover")+1, nchar(name))
            } else {
                clean_name <- name
            }
            
            # Create distribution
            distrib <- dnorm(bell_range, mean = estimate, sd = se)
            
            temp_df <- data.frame(
                parameter_value = bell_range,
                density = distrib,
                parameter = rep(clean_name, length(bell_range))
            )
            distrib_estimate_df <- rbind(distrib_estimate_df, temp_df)
            
            # Add value estimated to other table
            temp_df_bis <- data.frame(
                parameter_value = df_estimated_params[clean_name, "estimated_value"],
                density = dnorm(df_estimated_params[clean_name, "estimated_value"], mean = estimate, sd = se),
                parameter = clean_name
            )
            estimate_df <- rbind(estimate_df, temp_df_bis)
        }
        
        graph_densities <- plot_grid_scattered_densities(
            df_lines = distrib_estimate_df, 
            df_points = estimate_df,
            x="parameter_value", 
            y="density", 
            category="parameter") +
            labs(caption = paste0(
                "Range manually limited to [", min(bell_range), ", ", max(bell_range), "].",
                "Note that the estimated distribution for 'Pasture' is very flat"))
        print(graph_densities)
    }
    
    
    ### 1.2. Slightly less classic: Bayesian approach ###
    # The advantage here (normally) is that plotting priors and posteriors is
    # the standard approach, ther is no need to make custom plots (I hope...)
    
    
    ##### 99. Conclusions #####
    # It does not look like it will be easy to model our data for the moment.
    # I chose the values so that my curves were matching my data range, 
    # it was easier this way, but it will not look, like this in reality.
    
}