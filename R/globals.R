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
  # before they are standardized, especially if used directly in dplyr verbs
  # without .data$ (though the explicit rename in the current read_birdnet_data
  # should handle most of this, this is a safeguard for R CMD check's parsing).
  "Scientific name",
  "Confidence",
  "Start (s)",
  "End (s)",
  "File",
  "Common name",

  # Add any other column names here that appear in "no visible binding" notes
  # and are used as bare names within dplyr verbs in your functions.
  # Most of your other standardized column names (e.g., Scientific_Name, Total_Detections)
  # should be fine if they are on the left side of assignments or already used with .data$.
  # If new notes appear for other columns, add them to this vector.

  NULL # utils::globalVariables expects a character vector, NULL is ignored but keeps structure clean.
))
