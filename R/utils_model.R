##### Imports #####
library(tidyverse)
library(cowplot)
library(MCMCvis)


##### Parameters #####
# Nothing to see here...


##### Functions #####
#' Compare two sets of parameter values
#'
#' @param x_params Named vector of true/reference values
#' @param y_params Named vector of estimated values
#'
#' @return Data.frame with comparison metrics
make_comparison_dataframe <- function(x_params, y_params) {
    x = c()
    y = c()
    for (name in names(x_params)) {
        for (subname in names(x_params[[name]])) {
            if (is.na(x_params[[name]][[subname]])) {next}
            x <- c(x, setNames(x_params[[name]][[subname]], paste0(name, "$", subname)))
            y <- c(y, setNames(y_params[[name]][[subname]], paste0(name, "$", subname)))
        }
    }
    
    tibble(
        parameter = names(x),
        true_value = as.numeric(x),
        estimated_value = as.numeric(y),
        difference = true_value - estimated_value,
        percent_diff = (difference / true_value) * 100
    )
}


#' A custom function mimicking MCMCtrace
#'
#' @param nimble_model Self explanatory. Should be an object containing "chains".
#' @param n_burnin A numeric, the number of burn-in iterations for the model.
#' @param params A vector of strings, the names of the parameters in nimble_model to be plotted.
#' @param params_labels A vector of strings, the labels to show for each parameter of the model (optional).
#' @param show_Rhat A boolean. If TRUE (default) computes and displays Rhat at the bottom of the plot. It is a value used for convergence diagnostic (usually, we consider that the chains converged if Rhat < 1.05, a value of Rhat > 1.1 should be concerning).
#' @param show_Neff A boolean. If TRUE (default) computes and displays Neff at the bottom of the plot. It is the "effective sample size", or the number of independent samples (in practice, Neff > 400 is considered acceptable).
#' 
#' @return A list of cowplot objects.
#'
#' @seealso \code{\link{MCMCvis::MCMCtrace}}, \code{\link{cowplot::plotgrid}}
#'
#' @export
ggplot_custom_MCMCtrace <- function(
        nimble_model,
        params = NULL,
        show_Rhat = TRUE,
        show_Neff = TRUE) {
    
    # select columns
    if (is.null(params)) {
        params <- colnames(nimble_model)
    }
    
    if ((show_Rhat) ||(show_Neff)) {
        model_summary <- MCMCvis::MCMCsummary(
            nimble_model, 
            params = params,
            round = 2,
            ISB = FALSE
        )
    }
    
    # add iteration number and chain ID
    chain_list <- list()
    for (i in 1:N_CHAINS) {
        chain_df <- as.data.frame(nimble_model[[paste0("chain", i)]])
        chain_df$Iteration <- 1:nrow(chain_df)
        chain_df$Chains <- paste("chain", i)
        chain_list[[i]] <- chain_df
    }
    # bind chains into a single table
    chains_combined <- dplyr::bind_rows(chain_list)
    chains_combined <- chains_combined[c(params, "Iteration", "Chains")]
    
    list_plots <- list()
    for (i in 1:length(params)) {
        local_param <- params[i]
        local_df <- chains_combined[c(local_param, "Iteration", "Chains")]
        
        trace_plot <- ggplot2::ggplot(
            data = local_df,
            aes(
                x = .data[["Iteration"]], 
                y = .data[[local_param]], 
                color = .data[["Chains"]])) +
            geom_line(size = 0.25, alpha = 0.5)
        trace_plot <- my_custom_ggplot_theme(trace_plot, with_palette = TRUE)
        trace_plot <- trace_plot + 
            theme(legend.position = "none")
        
        if ((show_Rhat) ||(show_Neff)) {
            caption_info <- ""
            
            if (show_Neff) {
                caption_info <- paste0(caption_info, "Neff: ", model_summary[local_param, "n.eff"], ". ")
            }
            if (show_Rhat) {
                caption_info <- paste0(caption_info, "Rhat: ", model_summary[local_param, "Rhat"], ". ")
            }
            trace_plot <- trace_plot + ggplot2::labs(caption = caption_info)
        }
        
        # Create density plot
        density_plot <- ggplot2::ggplot(
            data = local_df,
            aes(
                x = .data[[local_param]], 
                color = .data[["Chains"]])) +
            geom_density(fill = NA, size = 0.5) +
            coord_flip() 
        density_plot <- my_custom_ggplot_theme(density_plot, with_palette = TRUE)
        density_plot <- density_plot + ggplot2::labs(x = NULL, y = "Density")
        
        if ((show_Rhat) ||(show_Neff)) {
            density_plot <- density_plot + ggplot2::labs(caption = "")
        }
        
        
        # Combine plots side by side
        list_plots[[i]] <- cowplot::plot_grid(
            trace_plot, density_plot, 
            ncol = 2, 
            rel_widths = c(2, 1))
    }
    
    return(list_plots)
}


#' Compute the Fisher Information Matrix for a given glm
#'
#' @param mod The model to evaluate
#'
#' @return a matrix: the FIM
compute_fim <- function(mod) {
    if (!("glm" %in% class(mod))) {
        # naive check, better than none
        stop("Model 'mod' is not a glm!")
    }
    # Get the design matrix X
    X <- model.matrix(mod)
    
    # Get fitted probabilities theta_i
    theta <- fitted(mod)
    
    # Build the weight matrix W = diag(theta_i * (1 - theta_i))
    W <- diag(theta * (1 - theta))
    
    # Compute the FIM: X^T W X
    FIM <- t(X) %*% W %*% X
    
    return(FIM)
}


#' Compute a single D-optimality criterion
#'
#' @param FIM A matrix, the Fisher Information Matrix of a model
#'
#' @return a numeric, the D-optimality criterion
compute_det_FIM <- function(FIM) {
    d <- determinant(FIM, logarithm = TRUE)
    if (d$sign <= 0) {
        warning("FIM is singular or negative definite — design may not be identifiable.")
        return(-Inf)
    }
    return(det(FIM))
}