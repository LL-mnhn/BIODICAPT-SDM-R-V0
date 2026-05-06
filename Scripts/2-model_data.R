# Model fitting comparison: Frequentist vs Bayesian approaches
# Tests both GLM and NIMBLE implementations on simulated species distribution data
#
# Reminder about Model 1 (Bernoulli 2 variables): 
# y_i ~ Bernoulli(θ_i)
# logit(θ_i) = β_j + β_1 * T_i

##### Imports #####
library(sf)
library(MASS)
library(nimble)
library(MCMCvis)
library(progress)
library(tidyverse)
library(tidyterra)
library(latex2exp)

source(here::here("R/utils_model.R"))
source(here::here("R/utils_figures.R"))
source(here::here("resources/config.R"))
source(here::here("Scripts/1-simulate_data.R"))


##### Parameters #####
### CAN BE MODIFIED
VERBOSE <- TRUE
SHOW_PLOTS <- TRUE
N_BURNIN <- 3e4
N_ITER <- 8e4
N_CHAINS <- 3

PRIOR_AAT_MEAN <- 5
PRIOR_AAT_SD <- 2.5
PRIOR_CLC_MEAN <- -50
PRIOR_CLC_SD <- 2.5

BAYESIAN_SHOWCASE <- FALSE

### DO NOT MODIFY
set.seed(43)        # for reproducible results


##### Helper/Wrapper Functions #####
load_simulation_data <- function() {
    # Load research observations
    obs_df <- data.table::fread(
        file.path(SIMULATION_PATH, "BIODICAPT_simulation_results.csv"))
    obs_df$land_cover_num <- as.numeric(as.factor(obs_df$land_cover))
    # obs_df$temperature_c <- scale(
    #     obs_df$temperature, center = TRUE, scale = FALSE)
    return(obs_df)
}

show_temperature_distrib_vs_probability_bis <- function(
        obs_df, parameters, show_plots = SHOW_PLOTS) {
    # x-axis limits
    temperature_range <- seq(
        min(pretty_range(obs_df$temperature, multiple = 2)),
        max(pretty_range(obs_df$temperature, multiple = 2)),
        0.25)
    
    # make P(presence) for each CLC category
    distributions <- data.frame()
    for (j in seq_along(parameters$CLC)) {
        if (is.na(parameters$CLC[j])) {next}
        temp_df <- data.frame(
            x = temperature_range,
            y = plogis(parameters$CLC[j] + (parameters$AAT * temperature_range)),
            line = names(parameters$CLC[j]))
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
            ggplot2::labs(title = "Modelled P(presence) and collected data.") +
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
        return(g_hist_VS_P)
    }
}

fit_frequentist_model <- function(data, verbose = VERBOSE) {
    if (verbose){
        cat("=== Modelisation using frequentist approach ===")    
    }
    mod <- stats::glm(
        presence_absence ~ land_cover + temperature - 1,
        data = data,
        family = binomial(link = "logit"))
    return(mod)
}

extract_frequentist_coefs <- function(mod) {
    if (!("glm" %in% class(mod))) {
        stop("Given model is not a glm")
    }
    
    coefs <- stats::coef(mod)
    beta_j <- coefs[grepl("^land_cover", names(coefs))]
    names(beta_j) <- sub("land_cover", "", names(beta_j))
    
    list(
        AAT = setNames(coef(mod)["temperature"], "temperature"),
        CLC = beta_j
    )
}

get_report_frequentist <- function(
        mod, target_parameters, obs_df, 
        verbose = VERBOSE, show_plots = SHOW_PLOTS) {
    if (!("glm" %in% class(mod))) {
        stop("Given model is not a glm")
    }
    
    if (verbose) {
        print(summary(mod))
    }
    
    # Extract model coefs
    mod_parameters <- extract_frequentist_coefs(mod)
    freq_comparison_df <- make_comparison_dataframe(
        x_params = target_parameters,
        y_params = mod_parameters
    )

    if (verbose) {
        cat("\n==> Comparison of simulation values (x) VS estimated values (y):\n")
        print(freq_comparison_df)
        
        cat("\n\n==> 95% CI of estimated values VS target values:\n")
        print(suppressMessages(stats::confint(mod, level = 0.95)))
        print(target_parameters)
    }
    
    
    if (show_plots) {
        graph <- show_temperature_distrib_vs_probability_bis(
            obs_df = obs_df,
            parameters = mod_parameters)
        print(graph + ggplot2::labs(subtitle = "Frequentist model"))
    }
}

find_Doptimal_new_obs <- function(
    obs_df, new_obs_df, n_new_obs = 1, method="sequential",
    verbose = VERBOSE, show_plots = SHOW_PLOTS) {
    
    if (verbose) {
        # messages
        cat("\n=== Searching for D-optimal design ===")
        initial_det_FIM <- compute_det_FIM(compute_fim(fit_frequentist_model(obs_df, verbose = FALSE)))       
        cat(sprintf("\n\n[INITIAL MODEL]\t\tdet(FIM)/n_obs = %.4g", initial_det_FIM/nrow(obs_df)))
      
        # Initialise progress bar
        pb <- progress_bar$new(
            format = "Processing [:bar] :percent | ETA: :eta",
            clear = FALSE,
            total = nrow(new_obs_df)
        )
    }

    # Use only variables that are needed (for better performance)
    features <- c("temperature", "land_cover", "presence_absence")

  
    # Easy peasy : we just want to add one row
    if (n_new_obs == 1) {
        # Initialise best values that the loop will hold
        best_crit <- NULL
        best_row <- NULL

        # Re-fit model 
        for (row in 1:nrow(new_obs_df)){
            if (verbose) {
                # advance progress bar
                pb$tick()
            }

            # make new data.table with new observation
            temp_df <- obs_df[, ..features]
            new_row <- new_obs_df[row, ..features]
            temp_df <- rbind(temp_df, new_row)
            
            # fit model on the new data.table
            temp_model <- fit_frequentist_model(temp_df, verbose = FALSE)
            
            # compute det(FIM)
            temp_crit <- compute_det_FIM(compute_fim(temp_model))
            
            # We keep the maximum det(FIM) to reach D-optimality
            if (is.null(best_crit) || (temp_crit > best_crit)) {
                best_crit <- temp_crit
                best_row <- row
            }
        }
    } else if ((n_new_obs > 1) & (n_new_obs%%1==0)) {
        # Now this is the difficult part.
        # Testing all combination can take a VERY long time.
        # Sooooo, we should use other approaches

        if (method == "sequential") {
            # Principle is very simple : find first best row, then the
            # next, then the next etc.
            cat("TODO see comments")
        } else if (method == "Fedorov") {
            cat("TODO Multi-start Fedorov")
        }
    } else {
        stop(paste0("Was expecting n_new_obs to be an integer >= 0, got ", n_new_obs))
    }

    
    # show results
    if (verbose) {
        cat(sprintf("\n[D-OPTIMAL RESULT]\tdet(FIM)/n_obs = %.4g", best_crit/(nrow(obs_df)+1)), "\n\n")
    }
    

    if (show_plots) { # plot 
        temp_df <- new_obs_df[best_row, ]
        g_crit_location <- ggplot_categorical_df_on_france_map(
            base_map = ggplot_get_france_base_map(),
            df = temp_df,
            LON = "X",
            LAT = "Y") +
            ggplot2::labs(title = paste0("New location(s) identified as most informative."))
        print(g_crit_location)
    }

    return(c(best_row))
}


# Just in case, for later use...
create_nimble_model <- function(n_obs, n_covers) {
    nimble::nimbleCode({
        # Priors (weakly informative)
        AAT ~ dnorm(PRIOR_AAT_MEAN, sd = PRIOR_AAT_SD)
        for (j in 1:n_covers) {
            CLC[j] ~ dnorm(PRIOR_CLC_MEAN, sd = PRIOR_CLC_SD)
        }
        
        # Likelihood
        for (i in 1:n_obs) {
            logit(theta[i]) <- CLC[land_cover_num[i]] + AAT * temperature[i]
            presence_absence[i] ~ dbern(theta[i])
        }
    })
}

fit_bayesian_model <- function(
        data, verbose = VERBOSE, show_plots = SHOW_PLOTS) {
    if (verbose) {
        cat("=== Modelisation using Bayesian approach ===\n")    
    }
    
    # Constants for model
    constants <- list(
        n_obs = nrow(data),
        n_covers = n_distinct(data$land_cover_num),
        PRIOR_AAT_MEAN = PRIOR_AAT_MEAN,
        PRIOR_AAT_SD = PRIOR_AAT_SD,
        PRIOR_CLC_MEAN = PRIOR_CLC_MEAN,
        PRIOR_CLC_SD = PRIOR_CLC_SD
    )

    # Model
    default_model <- create_nimble_model(constants$n_obs, constants$n_covers)
    
    gen_chain_initial_values <- function() {
        list(
            AAT = runif(1, PRIOR_AAT_MEAN-(2*PRIOR_AAT_SD), PRIOR_AAT_MEAN+(2*PRIOR_AAT_SD)),
            CLC = runif(constants$n_covers, PRIOR_CLC_MEAN-(2*PRIOR_CLC_SD), PRIOR_CLC_MEAN+(2*PRIOR_CLC_SD))
        )
    }
    
    # Fit model
    mod <- suppressMessages(nimble::nimbleMCMC(
        code = default_model,
        data = data[, c("temperature", "presence_absence", "land_cover_num")],
        constants = constants,
        inits = gen_chain_initial_values,
        monitors = c("AAT", "CLC", "theta"),
        niter = N_ITER,
        nburnin = N_BURNIN,
        nchains = N_CHAINS
    ))
    
    # show trace fits
    if (show_plots) {
        param_to_plot <- c(
            "AAT",
            colnames(mod$chain1)[grepl("^CLC", colnames(mod$chain1))]
            )
        trace_plots <- ggplot_custom_MCMCtrace(
            nimble_model = mod,
            params = param_to_plot
        )
        print(trace_plots)
    }
    
    return(mod)
}

extract_bayesian_coefs <- function(mod, data) {
    if (!("list" %in% class(mod))) {
        stop("Given model is not a list (which is the expected format for NIMBLE output)")
    }
    
    coefs_means <- MCMCvis::MCMCsummary(
        mod,
        params = c("AAT", "CLC")
    )
    
    table_land_cover = unique(data[, c("land_cover_num", "land_cover")])
    
    CLC_parameters <- c()
    for (i in 1:nrow(table_land_cover)) {
        CLC_parameters <- c(
            CLC_parameters,
            setNames(coefs_means[paste0("CLC[", i, "]"), "mean"], 
                     table_land_cover[table_land_cover$land_cover_num == i]$land_cover)
        )
    }
    
    list(
        AAT = setNames(coefs_means["AAT", "mean"], "temperature"),
        CLC = CLC_parameters
    )
}

get_report_bayesian <- function(
        mod, target_parameters, obs_df, 
        verbose = VERBOSE, show_plots = SHOW_PLOTS) {
    if (!("list" %in% class(mod))) {
        stop("Given model is not a list (which is the expected format for NIMBLE output)")
    }
    
    if (verbose) {
        baye_summary <- MCMCvis::MCMCsummary(
            mod, params = c("AAT", "CLC"), round = 2)
        print(baye_summary)
    }
    
    # Extract model coefs
    mod_parameters <- extract_bayesian_coefs(mod, data = obs_df)
    baye_comparison_df <- make_comparison_dataframe(
        x_params = target_parameters,
        y_params = mod_parameters
    )
    
    if (verbose) {
        cat("\n==> Comparison of simulation values (x) VS estimated values (y):\n")
        print(baye_comparison_df)
        
        cat("\n\n==> 95% CI of estimated values VS target values:\n")
        print(baye_summary[c("2.5%","97.5%")])
        print(target_parameters)
        cat("\n")
    }
    
    
    if (show_plots) {
        graph <- show_temperature_distrib_vs_probability_bis(
            obs_df = obs_df,
            parameters = mod_parameters)
        print(graph + ggplot2::labs(subtitle = "Bayesian model"))
    }
}


##### Main Loop #####
if (sys.nframe() == 0) {
    ##### 0. Import data #####
    load(file.path(SIMULATION_PATH, "Parameters_for_simulation.RData"))
    observations_df <- load_simulation_data()
    simulation_graph <- show_temperature_distrib_vs_probability_bis(
        obs_df = observations_df,
        parameters = BETAS)
    print(simulation_graph + ggplot2::labs(subtitle = "Simulation"))
    
    ##### 1. Fit frequentist model #####
    freq_model <- fit_frequentist_model(observations_df)
    get_report_frequentist(
        mod = freq_model, 
        obs_df = observations_df,
        target_parameters = BETAS)
    
    ##### 2. OED #####
    ### 2.1. Initial State ###
    # Compute current D-optimality
    freq_fim <- compute_fim(freq_model)
    freq_D_crit <- compute_det_FIM(freq_fim)
    
    # Compare it with a dummy model
    dummy_model <- fit_frequentist_model(
        observations_df[sample(nrow(observations_df), 10), ], 
        verbose = FALSE)
    dummy_fim <- compute_fim(dummy_model)
    dummy_D_crit <- compute_det_FIM(dummy_fim)
    if (VERBOSE) {
        cat("[RESULTS] det(FIM)/n_obs:") # normalise by number of observation to compare
        cat(sprintf("\n\tWhen using 10 samples: %.4g", dummy_D_crit/nrow(dummy_model$data)))
        cat(sprintf("\n\tWhen using all samples: %.4g", freq_D_crit/nrow(freq_model$data)), "\n\n")
    }

    
    ### 2.2. Finding the a D-Optimal design ###
    # load 500ENI dataset
    ENI_df <- data.table::fread(
        file.path(SIMULATION_PATH, paste0("500ENI_Simulation_results.csv")))
    
    # Find next best location to sample
    ENI_best_rows <- find_Doptimal_new_obs(
        obs_df = observations_df, new_obs_df = ENI_df, 
        n_new_obs = 1)
        
    
    ##### BONUS. Fit Bayesian model #####
    if (BAYESIAN_SHOWCASE) {
        baye_model <- fit_bayesian_model(observations_df)
        get_report_bayesian(
            mod = baye_model, 
            obs_df = observations_df,
            target_parameters = BETAS)
    }
    
}