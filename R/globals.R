# File: R/globals.R
# This file is used to declare global variables that are used in a way
# that R CMD check might flag as "no visible binding for global variable".
# This typically occurs with dplyr when using bare column names or the .data pronoun.
# Listing them here tells R CMD check that these variables are intentionally global
# or are known to be defined in the data masking context of dplyr.

utils::globalVariables(c(
  # For the .data pronoun used in dplyr pipes
  ".data",

  # For raw column names from the input CSV used in read_birdnet_data()
  "Scientific name",
  "Confidence",
  "Start (s)",
  "End (s)",
  "File",
  "Common name",

  # New columns added in enhanced version
  "Detection_Hour",
  "Detection_Date",
  "N_Dets_This_Day",
  "N_Dets_This_Hour",
  "Total_Dets_Species",
  "Prop_Dets_This_Hour",
  "Prop_Squared",
  "Hour_Radians",
  "Cos_Hour",
  "Sin_Hour",
  "Mean_Cos",
  "Mean_Sin",
  "N_Dets_This_Plot",
  "N_Active_Plots_Species",
  "Variance_Dets",

  # Anomaly score columns
  "CI_Median_Anomaly",
  "CI_IQR_Anomaly",
  "CI_Skew_Anomaly",
  "Daily_CV_Anomaly",
  "Hourly_Conc_Anomaly",
  "Hourly_R_Anomaly",
  "Plot_Occ_Anomaly",
  "Spatial_CV_Anomaly",
  "Spatial_Agg_Anomaly",

  # Data quality columns
  "Date_Index",
  "Species_This_Day",
  "Cumulative_Species",
  "Days_Span",
  "n",

  # Visualization columns
  "Size_Category",
  "Has_Flag",
  "Anomaly_Type",
  "Score",
  "Hour_Label",
  "Recorder_Label",

  NULL  # Keep structure clean
))
