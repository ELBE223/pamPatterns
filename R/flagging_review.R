#' Flag Species Based on Pattern Metrics and Calculate Review Score
#'
#' This function takes a data frame of calculated pattern metrics (output from
#' `calculate_pattern_metrics`) and applies a set of user-defined or
#' data-suggested thresholds to flag species that exhibit potentially
#' suspicious patterns. It also calculates a weighted review score to help
#' prioritize species for manual validation.
#'
#' @param metrics_df A `tibble` or `data.frame` with calculated metrics for each
#'   species, typically the output of `calculate_pattern_metrics()`.
#'   Must contain columns like `Median_CI`, `Plot_Occupancy_Pct`,
#'   `Pct_Dets_Above_CI_High`, `Daily_Dets_CV`, `Hourly_Activity_Concentration`,
#'   and `Total_Detections`.
#' @param thresholds A list of named numeric values representing the thresholds
#'   for flagging. Missing thresholds will use defaults. Expected names:
#'   \itemize{
#'     \item `min_median_ci`: Minimum median confidence (e.g., 0.20). Species below are flagged.
#'     \item `max_plot_occupancy`: Maximum plot occupancy percentage (e.g., 0.90). Species above are flagged.
#'     \item `min_pct_high_ci`: Minimum percentage of detections above the 'high' CI threshold
#'           (e.g., 0.05). Species below this (and with enough detections) are flagged.
#'     \item `max_daily_cv`: Maximum coefficient of variation for daily detections (e.g., 2.0).
#'           Species with higher CV (more erratic daily detections) and sufficient active days are flagged.
#'     \item `max_hourly_concentration`: Maximum hourly activity concentration (e.g., 0.8).
#'           Species with higher concentration (activity focused in few hours) and sufficient active hours are flagged.
#'   }
#'   Use `suggest_thresholds()` to get data-driven suggestions.
#' @param weights A named numeric vector specifying the weights for each flag when
#'   calculating the `Review_Score`. Higher weights mean the flag contributes
#'   more to the score. Default weights emphasize low confidence and high occupancy.
#'   Expected names match the flags: `low_ci`, `high_occupancy`, `low_high_ci_pct`,
#'   `erratic_daily`, `concentrated_hourly`.
#'
#' @return A `tibble` identical to `metrics_df` but with added boolean flag
#'   columns (e.g., `Flag_Low_Median_CI`) and a numeric `Review_Score`,
#'   sorted by `Review_Score` (descending) and then `Total_Detections` (descending).
#' @export
#' @examples
#' \dontrun{
#' # Assume 'all_metrics' is the output from calculate_pattern_metrics()
#' # set.seed(123) # For reproducibility if using random example_detections
#' # n_species <- 10
#' # example_metrics <- dplyr::tibble(
#' #   Scientific_Name = paste("Species", LETTERS[1:n_species]),
#' #   Total_Detections = sample(50:1000, n_species, replace = TRUE),
#' #   Median_CI = runif(n_species, 0.1, 0.9),
#' #   Plot_Occupancy_Pct = runif(n_species, 0.05, 0.95),
#' #   Pct_Dets_Above_CI_High = runif(n_species, 0, 0.5),
#' #   Daily_Dets_CV = runif(n_species, 0.5, 3),
#' #   N_Unique_Days_Detected = sample(3:20, n_species, replace = TRUE),
#' #   Hourly_Activity_Concentration = runif(n_species, 0.2, 0.9),
#' #   N_Unique_Hours_Detected = sample(3:15, n_species, replace = TRUE)
#' # )
#' #
#' # # Use default thresholds or get suggestions
#' # suggested_thresh <- suggest_thresholds(example_metrics)
#' #
#' # flagged_data <- flag_species(example_metrics, thresholds = suggested_thresh)
#' # print(head(flagged_data[, c("Scientific_Name", "Review_Score", "Flag_Low_Median_CI")]))
#' #
#' # # Using custom weights
#' # custom_weights <- c(low_ci = 5, high_occupancy = 3, low_high_ci_pct = 1,
#' #                     erratic_daily = 1, concentrated_hourly = 1)
#' # flagged_data_custom_weights <- flag_species(example_metrics, weights = custom_weights)
#' # print(head(flagged_data_custom_weights[, c("Scientific_Name", "Review_Score")]))
#' }
flag_species <- function(metrics_df,
                         thresholds = list(), # Empty list means all defaults will be used
                         weights = NULL) {

  if (!is.data.frame(metrics_df) || nrow(metrics_df) == 0) {
    # warning("Input 'metrics_df' is not a data frame or is empty. Returning it as is.")
    if(is.data.frame(metrics_df) && !"Review_Score" %in% names(metrics_df)) {
      metrics_df$Review_Score <- numeric(0) # Ensure column exists if df is empty
    }
    return(metrics_df)
  }

  # --- Define Default Thresholds and Weights ---
  default_thresholds <- list(
    min_median_ci = 0.20,
    max_plot_occupancy = 0.90,
    min_pct_high_ci = 0.05, # Minimum 5% of detections should be high confidence
    max_daily_cv = 2.0,     # CV > 2 indicates high variability
    max_hourly_concentration = 0.8 # 80% concentration
  )

  # Merge user-provided thresholds with defaults
  # User values override defaults if provided
  final_thresholds <- utils::modifyList(default_thresholds, thresholds)


  default_weights <- c(
    low_ci = 3,             # Low median confidence gets a high weight
    high_occupancy = 5,     # Very widespread species (potential over-splitting or common noise)
    low_high_ci_pct = 2,    # Lacking many high-confidence detections
    erratic_daily = 1,      # Erratic daily patterns
    concentrated_hourly = 1 # Activity highly concentrated in few hours
  )
  # Merge user-provided weights with defaults
  final_weights <- if (is.null(weights)) default_weights else utils::modifyList(default_weights, weights)


  # --- Check for required columns in metrics_df ---
  # Columns needed for flagging logic + Total_Detections for conditional flags
  required_metric_cols_for_flags <- c(
    "Median_CI", "Plot_Occupancy_Pct", "Pct_Dets_Above_CI_High",
    "Daily_Dets_CV", "N_Unique_Days_Detected",
    "Hourly_Activity_Concentration", "N_Unique_Hours_Detected",
    "Total_Detections"
  )
  missing_metric_cols <- setdiff(required_metric_cols_for_flags, names(metrics_df))
  if (length(missing_metric_cols) > 0) {
    stop("Missing required columns in 'metrics_df' for flagging: ",
         paste(missing_metric_cols, collapse = ", "),
         ". These are needed to apply thresholds.")
  }

  # --- Apply Flags ---
  # Ensure numeric types for comparisons to avoid issues
  # This assumes metrics_df columns are already mostly numeric from calculate_pattern_metrics
  # Adding explicit as.numeric for robustness where NAs might cause type issues.
  flagged_df <- metrics_df %>%
    dplyr::mutate(
      Flag_Low_Median_CI = .data$Median_CI < final_thresholds$min_median_ci,
      Flag_High_Plot_Occupancy = .data$Plot_Occupancy_Pct > final_thresholds$max_plot_occupancy,
      Flag_Low_Pct_High_CI = .data$Pct_Dets_Above_CI_High < final_thresholds$min_pct_high_ci,
      # Erratic daily activity: flag if CV is high AND there are enough days of data to make CV meaningful
      Flag_Erratic_Daily_Activity = !is.na(.data$Daily_Dets_CV) & .data$Daily_Dets_CV > final_thresholds$max_daily_cv &
        !is.na(.data$N_Unique_Days_Detected) & .data$N_Unique_Days_Detected >= 5, # e.g., at least 5 active days
      # Concentrated hourly activity: flag if concentration is high AND there are enough hours of data
      Flag_Concentrated_Hourly = !is.na(.data$Hourly_Activity_Concentration) & .data$Hourly_Activity_Concentration > final_thresholds$max_hourly_concentration &
        !is.na(.data$N_Unique_Hours_Detected) & .data$N_Unique_Hours_Detected >= 3 # e.g., at least 3 active hours
    )

  # Handle NAs in flag columns: if the metric was NA, the flag should be FALSE (or NA)
  # The logical comparisons above will result in NA if an operand is NA.
  # For scoring, we want NA flags to contribute 0 to the score, so convert NA flags to FALSE.
  flag_cols <- c("Flag_Low_Median_CI", "Flag_High_Plot_Occupancy", "Flag_Low_Pct_High_CI",
                 "Flag_Erratic_Daily_Activity", "Flag_Concentrated_Hourly")
  flagged_df <- flagged_df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(flag_cols), ~dplyr::coalesce(., FALSE)))


  # --- Calculate Review Score ---
  # Only apply weight if the flag is TRUE
  # Some flags might be more relevant for species with many detections
  # e.g., Low_Median_CI is more concerning if Total_Detections is high.
  # For this version, we apply a simple Total_Detections > X condition for some weights.
  # A more sophisticated approach might scale weights by Total_Detections.
  min_detections_for_concern <- 30 # Arbitrary: flags are more concerning if >30 detections

  flagged_df <- flagged_df %>%
    dplyr::mutate(
      Review_Score =
        (.data$Flag_Low_Median_CI & .data$Total_Detections > min_detections_for_concern) * final_weights["low_ci"] +
        (.data$Flag_High_Plot_Occupancy) * final_weights["high_occupancy"] + # High occupancy is a concern regardless of N
        (.data$Flag_Low_Pct_High_CI & .data$Total_Detections > min_detections_for_concern) * final_weights["low_high_ci_pct"] +
        (.data$Flag_Erratic_Daily_Activity) * final_weights["erratic_daily"] + # Already conditioned on N_Unique_Days
        (.data$Flag_Concentrated_Hourly) * final_weights["concentrated_hourly"] # Already conditioned on N_Unique_Hours
    ) %>%
    # Arrange by Review_Score (desc) then Total_Detections (desc) to prioritize
    dplyr::arrange(dplyr::desc(.data$Review_Score), dplyr::desc(.data$Total_Detections))

  return(flagged_df)
}


#' Suggest Thresholds Based on Data Distribution
#'
#' Analyzes the distribution of calculated metrics across all species and
#' suggests data-driven thresholds for use in `flag_species()`.
#' These suggestions are based on quantiles of the metrics, aiming to identify
#' unusual values (e.g., the lowest 5% for median CI, or highest 5% for occupancy).
#'
#' @param metrics_df A `tibble` or `data.frame` with calculated metrics for each
#'   species, typically the output of `calculate_pattern_metrics()`.
#' @param percentiles A named numeric vector specifying the percentiles (quantiles)
#'   to use for each threshold suggestion. Values should be between 0 and 1.
#'   Default percentiles aim to catch outliers (e.g., 5th percentile for minimums,
#'   95th for maximums). Expected names:
#'   \itemize{
#'     \item `min_median_ci_pctile`: Percentile for suggesting `min_median_ci` (e.g., 0.05).
#'     \item `max_plot_occupancy_pctile`: Percentile for suggesting `max_plot_occupancy` (e.g., 0.95).
#'     \item `min_pct_high_ci_pctile`: Percentile for `min_pct_high_ci` (e.g., 0.05).
#'     \item `max_daily_cv_pctile`: Percentile for `max_daily_cv` (e.g., 0.95).
#'     \item `max_hourly_concentration_pctile`: Percentile for `max_hourly_concentration` (e.g., 0.95).
#'   }
#'
#' @return A list of named numeric values representing the suggested thresholds.
#'   These can be directly passed to the `thresholds` argument of `flag_species()`.
#'   Returns `NA` for a threshold if the corresponding metric column is all `NA`
#'   or if calculation fails.
#' @export
#' @examples
#' \dontrun{
#' # Assume 'all_metrics' is the output from calculate_pattern_metrics()
#' # set.seed(123) # For reproducibility if using random example_detections
#' # n_species <- 20
#' # example_metrics_for_suggest <- dplyr::tibble(
#' #   Scientific_Name = paste("Species", LETTERS[1:n_species]),
#' #   Median_CI = runif(n_species, 0.1, 0.9),
#' #   Plot_Occupancy_Pct = runif(n_species, 0.05, 0.95),
#' #   Pct_Dets_Above_CI_High = runif(n_species, 0, 0.5),
#' #   Daily_Dets_CV = c(runif(n_species-1, 0.5, 3), NA), # Add an NA
#' #   Hourly_Activity_Concentration = runif(n_species, 0.2, 0.9)
#' # )
#' #
#' # suggested_thresholds <- suggest_thresholds(example_metrics_for_suggest)
#' # print(suggested_thresholds)
#' #
#' # # Using custom percentiles
#' # custom_percentiles <- c(min_median_ci_pctile = 0.10, # 10th percentile
#' #                         max_plot_occupancy_pctile = 0.90, # 90th percentile
#' #                         min_pct_high_ci_pctile = 0.10,
#' #                         max_daily_cv_pctile = 0.90,
#' #                         max_hourly_concentration_pctile = 0.90)
#' # suggested_custom <- suggest_thresholds(example_metrics_for_suggest,
#' #                                       percentiles = custom_percentiles)
#' # print(suggested_custom)
#' }
suggest_thresholds <- function(metrics_df,
                               percentiles = NULL) {

  if (!is.data.frame(metrics_df) || nrow(metrics_df) == 0) {
    # warning("Input 'metrics_df' is not a data frame or is empty. Cannot suggest thresholds.")
    return(list(
      min_median_ci = NA_real_, max_plot_occupancy = NA_real_,
      min_pct_high_ci = NA_real_, max_daily_cv = NA_real_,
      max_hourly_concentration = NA_real_
    ))
  }

  default_percentiles <- c(
    min_median_ci_pctile = 0.05,  # Lower 5% for min_median_ci
    max_plot_occupancy_pctile = 0.95, # Upper 5% for max_plot_occupancy
    min_pct_high_ci_pctile = 0.05,    # Lower 5% for min_pct_high_ci
    max_daily_cv_pctile = 0.95,       # Upper 5% for max_daily_cv
    max_hourly_concentration_pctile = 0.95 # Upper 5% for max_hourly_concentration
  )

  final_percentiles <- if (is.null(percentiles)) default_percentiles else utils::modifyList(default_percentiles, percentiles)

  # Helper function to safely calculate quantile
  safe_quantile <- function(metric_column, prob) {
    if (!metric_column %in% names(metrics_df) || all(is.na(metrics_df[[metric_column]]))) {
      return(NA_real_)
    }
    # Remove NAs before calculating quantile; ensure there's non-NA data
    valid_data <- metrics_df[[metric_column]][!is.na(metrics_df[[metric_column]])]
    if (length(valid_data) == 0) {
      return(NA_real_)
    }
    return(stats::quantile(valid_data, probs = prob, na.rm = TRUE, type = 7))
  }

  suggestions <- list(
    min_median_ci = safe_quantile("Median_CI", final_percentiles["min_median_ci_pctile"]),
    max_plot_occupancy = safe_quantile("Plot_Occupancy_Pct", final_percentiles["max_plot_occupancy_pctile"]),
    min_pct_high_ci = safe_quantile("Pct_Dets_Above_CI_High", final_percentiles["min_pct_high_ci_pctile"]),
    max_daily_cv = safe_quantile("Daily_Dets_CV", final_percentiles["max_daily_cv_pctile"]),
    max_hourly_concentration = safe_quantile("Hourly_Activity_Concentration", final_percentiles["max_hourly_concentration_pctile"])
  )

  return(suggestions)
}
