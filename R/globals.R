# File: R/globals.R
# This file is used to declare global variables that are used in a way
# that R CMD check might flag as "no visible binding for global variable".
# This typically occurs with dplyr when using bare column names or the .data pronoun,
# or when using column names in ggplot2 aesthetics.
# Listing them here tells R CMD check that these variables are intentionally global
# or are known to be defined in the data masking context of dplyr/ggplot2.

utils::globalVariables(c(
  # Standard dplyr/rlang pronouns
  ".data",
  ".", # The dot placeholder, often used in magrittr pipes

  # --- Raw Input Column Names (from read_birdnet_data typical inputs) ---
  # (These are before standardization by read_birdnet_data)
  "Scientific name", # Note the space
  "Common name",     # Note the space
  "Confidence",      # This is also a standardized name
  "Start (s)",       # Note the space
  "End (s)",         # Note the space
  "File",

  # --- Standardized Column Names from read_birdnet_data & used in calculate_pattern_metrics ---
  "Scientific_Name", # Standardized version, with underscore
  # "Confidence" (already listed above)
  "Detection_Date",
  "Detection_Hour",
  "AudioMoth_ID_Numeric",

  # --- Columns created and/or used within calculate_pattern_metrics & its helpers ---
  # General & CI Metrics (from general_ci_metrics summarize block and mutate)
  "Total_Detections",
  "N_Plots_Detected",
  "Median_CI",
  "Mean_CI",
  "Min_CI",
  "Max_CI",
  "Q1_CI",
  "Q3_CI",
  "Pct_Dets_Above_CI_High", # Based on ci_thresholds["high"]
  "Pct_Dets_Above_CI_Medium",# Based on ci_thresholds["medium"]
  "CI_Skewness",
  "CI_IQR", # Calculated from Q1_CI and Q3_CI
  "Plot_Occupancy_Pct",

  # Temporal Metrics (from calculate_temporal_metrics)
  "N_Dets_This_Day", # Intermediate in daily_summary
  "N_Unique_Days_Detected",
  "Mean_Dets_Per_Active_Day",
  "SD_Dets_Per_Active_Day",
  "Max_Dets_Single_Day",
  "Daily_Dets_CV",
  "Pct_Days_Active",
  "N_Dets_This_Hour", # Intermediate in hourly_counts
  "Total_Dets_Species", # Intermediate in hourly_metrics calculation
  "Total_Dets_Species_Hourly", # Added from your calculate_metrics.R code
  "Prop_Dets_This_Hour", # Intermediate
  "Prop_Squared",        # Intermediate for Simpson's
  "Hour_Radians",        # Intermediate for circular stats
  "Cos_Hour",            # Intermediate
  "Sin_Hour",            # Intermediate
  "Mean_Cos",            # Intermediate
  "Mean_Sin",            # Intermediate
  "Sum_Cos_Hour_Weighted", # Added from your calculate_metrics.R code
  "Sum_Sin_Hour_Weighted", # Added from your calculate_metrics.R code
  "Sum_N_Dets_This_Hour",  # Added from your calculate_metrics.R code
  "N_Unique_Hours_Detected",
  "Hourly_Shannon_H",
  "Hourly_Simpson_D",
  "Hourly_Simpson_D_calc", # Added from your calculate_metrics.R code
  "Peak_Hour",
  "Mean_Hour_Circular",
  "Hourly_R_Statistic",
  "Hourly_Max_H",        # Intermediate for Evenness
  "Hourly_Evenness_J",
  "Hourly_Activity_Concentration",

  # Spatial Metrics (from calculate_spatial_metrics)
  "N_Dets_This_Plot", # Intermediate in plot_summary
  "N_Active_Plots_Species", # Intermediate
  "Mean_Dets_Per_Active_Plot",
  "SD_Dets_Per_Active_Plot",
  "Max_Dets_Single_Plot",
  "Variance_Dets",         # Intermediate for Aggregation Index
  "Spatial_Aggregation_Index",
  "Dets_Per_Plot_CV",

  # Anomaly Score Columns (from calculate_anomaly_scores)
  # Intermediate transformed columns:
  "CI_Skewness_Trans",                # Added from your calculate_metrics.R code
  "Spatial_Aggregation_Index_Trans", # Added from your calculate_metrics.R code
  # Intermediate individual anomaly scores:
  "Z_Median_CI",                      # Added from your calculate_metrics.R code
  "Z_CI_IQR",                         # Added from your calculate_metrics.R code
  "Z_CI_Skewness",                    # Added from your calculate_metrics.R code
  "Z_Daily_Dets_CV",                  # Added from your calculate_metrics.R code
  "Z_Hourly_Activity_Concentration",  # Added from your calculate_metrics.R code
  "Z_Hourly_R_Statistic",             # Added from your calculate_metrics.R code
  "Z_Plot_Occupancy_Pct",             # Added from your calculate_metrics.R code
  "Z_Dets_Per_Plot_CV",               # Added from your calculate_metrics.R code
  "Z_Spatial_Aggregation_Index",      # Added from your calculate_metrics.R code
  # Composite anomaly scores:
  "CI_Anomaly_Score",
  "Temporal_Anomaly_Score",
  "Spatial_Anomaly_Score",
  "Overall_Anomaly_Score",

  # tidyselect helper function for dplyr::across
  "where", # Added to address the NOTE

  # --- Columns used in other S3 methods or helper functions (examples) ---
  "Review_Score",

  NULL
))
