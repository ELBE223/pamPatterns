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
  "Prop_Dets_This_Hour", # Intermediate
  "Prop_Squared",        # Intermediate for Simpson's
  "Hour_Radians",        # Intermediate for circular stats
  "Cos_Hour",            # Intermediate
  "Sin_Hour",            # Intermediate
  "Mean_Cos",            # Intermediate
  "Mean_Sin",            # Intermediate
  "N_Unique_Hours_Detected",
  "Hourly_Shannon_H",
  "Hourly_Simpson_D",
  "Peak_Hour",
  "Mean_Hour_Circular",  # Note: your code uses Mean_Hour_Circular, earlier notes had Mean_Hour_Circ
  "Hourly_R_Statistic",  # Note: your code uses Hourly_R_Statistic, earlier notes had R_Statistic_Circ
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
  # Intermediate individual anomaly scores:
  "CI_Median_Anomaly",
  "CI_IQR_Anomaly",
  "CI_Skew_Anomaly",
  "Daily_CV_Anomaly",
  "Hourly_Conc_Anomaly", # Corresponds to Hourly_Activity_Concentration
  "Hourly_R_Anomaly",    # Corresponds to Hourly_R_Statistic
  "Plot_Occ_Anomaly",    # Corresponds to Plot_Occupancy_Pct
  "Spatial_CV_Anomaly",  # Corresponds to Dets_Per_Plot_CV
  "Spatial_Agg_Anomaly", # Corresponds to Spatial_Aggregation_Index
  # Composite anomaly scores (these were in the last R CMD check output):
  "CI_Anomaly_Score",
  "Temporal_Anomaly_Score",
  "Spatial_Anomaly_Score",
  "Overall_Anomaly_Score",

  # --- Columns used in other S3 methods or helper functions (examples) ---
  # (You showed R/pamPatterns-class.R which uses some of these)
  "Review_Score", # Used in print.pam_metrics, summary.pam_metrics
  # Flag columns (common pattern, add specific ones if they appear in NOTES)
  # Example: "Flag_Low_Total_Detections" (if used bare in dplyr/ggplot)
  # Generally, if flag columns are only ever created (LHS of mutate) or checked
  # (e.g., sum(object[[flag_col_name]]) ), they might not need to be here.
  # Add if they appear in "no visible binding" notes.

  # Variables used in ggplot2 calls (if any are bare column names in aes())
  # e.g., from plot_pattern_diagnostics or plot.pam_metrics
  # "Value", "Metric", "Flag", "Is_Outlier", "Text_Label", "Date",
  # "Mean_Confidence", "Value_Scaled", "Anomaly_Label", "Confidence_Bin", "Count", "Density",
  # "Lower_MAD", "Upper_MAD",

  # Variables from other functions if they trigger notes
  # e.g. "AudioMoth_ID", "DateTime_Extracted", "Detection_Time_HMS",
  # "Detection_Start_DateTime_UTC", "Detection_End_DateTime_UTC",
  # "Species_Present", "Duration_s", "Day_Seconds", "Study_Day_Numeric",
  # "Time_of_Day_Segment", "Hour_Factor", "Weekday", "Weekend_Flag", "Year_Day",
  # "N_Detections", "N_Unique_Plots", "N_Unique_Dates", "N_Unique_Species",
  # "Plot_ID", "Cumulative_Species", "Recording_Effort_Days"

  NULL # utils::globalVariables expects a character vector; NULL is ignored but keeps structure.
))
