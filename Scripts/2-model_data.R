
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