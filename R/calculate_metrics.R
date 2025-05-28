#' Calculate Comprehensive Pattern Metrics for Species Detections
#'
#' This is the core function for calculating a wide range of quantitative
#' pattern indicators for each species from processed detection data.
#' These metrics cover general detection statistics, confidence interval (CI)
#' distributions, temporal patterns (daily and hourly), and spatial patterns
#' (across plots/recorders).
#'
#' @param detection_data A `tibble` or `data.frame` containing processed detection data.
#'   It must include the following columns:
#'   \itemize{
#'     \item `Scientific_Name`: Character, the scientific name of the species.
#'     \item `Confidence`: Numeric, the confidence score of the detection.
#'     \item `Detection_Date`: Date object, the date of the detection.
#'     \item `Detection_Hour`: Integer, the hour of the detection (0-23).
#'     \item `AudioMoth_ID_Numeric`: Integer or Numeric, a unique ID for each
#'           recording plot or device. This is crucial for spatial metrics.
#'   }
#'   Typically, this is the output of `read_birdnet_data()` after adding the
#'   `AudioMoth_ID_Numeric` column.
#' @param ci_thresholds A named numeric vector specifying confidence thresholds.
#'   Default is `c(high = 0.7, medium = 0.5)`. These are used to calculate
#'   percentages of detections above these confidence levels. Names must be "high" and "medium".
#'
#' @return A `tibble` where each row represents a species and columns are the
#'   calculated metrics. Metrics include:
#'   \itemize{
#'     \item General: `Total_Detections`, `N_Plots_Detected`, `Plot_Occupancy_Pct`.
#'     \item CI Metrics: `Median_CI`, `Mean_CI`, `Min_CI`, `Max_CI`, `Q1_CI`, `Q3_CI`,
#'           `CI_IQR`, `Pct_Dets_Above_CI_High`, `Pct_Dets_Above_CI_Medium`.
#'     \item Temporal (Daily): `N_Unique_Days_Detected`, `Mean_Dets_Per_Active_Day`,
#'           `SD_Dets_Per_Active_Day`, `Max_Dets_Single_Day`, `Daily_Dets_CV`
#'           (Coefficient of Variation), `Pct_Days_Active`.
#'     \item Temporal (Hourly): `N_Unique_Hours_Detected`, `Hourly_Evenness_J`
#'           (Shannon's Evenness), `Hourly_Activity_Concentration` (1 - Evenness).
#'     \item Spatial: `Mean_Dets_Per_Active_Plot`, `SD_Dets_Per_Active_Plot`,
#'           `Max_Dets_Single_Plot`, `Dets_Per_Plot_CV`.
#'   }
#'
#' @export
#' @importFrom stats median quantile sd na.omit
#' @examples
#' \dontrun{
#' # Assume 'detections_with_id' is prepared as in read_birdnet_data example
#' # (output of read_birdnet_data + AudioMoth_ID_Numeric column)
#'
#' # Create a more complete dummy detections_with_id for testing
#' set.seed(123)
#' n_dets <- 200
#' example_detections <- dplyr::tibble(
#'   Scientific_Name = sample(c("Sylvia atricapilla", "Erithacus rubecula", "Parus major"),
#'                            n_dets, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
#'   Confidence = runif(n_dets, 0.1, 0.99),
#'   Detection_Date = Sys.Date() - sample(0:10, n_dets, replace = TRUE),
#'   Detection_Hour = sample(0:23, n_dets, replace = TRUE),
#'   AudioMoth_ID_Numeric = sample(1:3, n_dets, replace = TRUE) # 3 mock recorders
#' )
#'
#' metrics <- calculate_pattern_metrics(example_detections)
#' print(metrics)
#'
#' # Using custom CI thresholds
#' metrics_custom_ci <- calculate_pattern_metrics(example_detections,
#'                                               ci_thresholds = c(high = 0.8, medium = 0.4))
#' print(metrics_custom_ci)
#' }
calculate_pattern_metrics <- function(detection_data,
                                      ci_thresholds = c(high = 0.7, medium = 0.5)) {

  # --- Input Validations ---
  if (!is.data.frame(detection_data)) {
    stop("'detection_data' must be a data frame or tibble.")
  }

  # Define expected column names for an empty metrics tibble to ensure consistency
  # This list should match the columns produced by the function.
  expected_metric_cols <- c(
    "Scientific_Name", "Total_Detections", "N_Plots_Detected", "Median_CI", "Mean_CI",
    "Min_CI", "Max_CI", "Q1_CI", "Q3_CI", "CI_IQR", "Pct_Dets_Above_CI_High",
    "Pct_Dets_Above_CI_Medium", "Plot_Occupancy_Pct", "N_Unique_Days_Detected",
    "Mean_Dets_Per_Active_Day", "SD_Dets_Per_Active_Day", "Max_Dets_Single_Day",
    "Daily_Dets_CV", "Pct_Days_Active", "N_Unique_Hours_Detected",
    "Hourly_Evenness_J", "Hourly_Activity_Concentration", "Mean_Dets_Per_Active_Plot",
    "SD_Dets_Per_Active_Plot", "Max_Dets_Single_Plot", "Dets_Per_Plot_CV"
  )

  if (nrow(detection_data) == 0) {
    # warning("Input 'detection_data' is empty. Returning an empty tibble for metrics.")
    return(dplyr::tibble(!!!stats::setNames(lapply(expected_metric_cols, function(x) vector(mode = "logical", length = 0)), expected_metric_cols)))
  }

  required_cols <- c("Scientific_Name", "Confidence", "Detection_Date",
                     "Detection_Hour", "AudioMoth_ID_Numeric")
  missing_cols <- setdiff(required_cols, names(detection_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in 'detection_data': ",
         paste(missing_cols, collapse = ", "),
         ". These are essential for metric calculation.")
  }

  if (!is.numeric(ci_thresholds) || is.null(names(ci_thresholds)) ||
      !all(c("high", "medium") %in% names(ci_thresholds))) {
    stop("'ci_thresholds' must be a named numeric vector with names 'high' and 'medium'.")
  }
  if (any(ci_thresholds < 0 | ci_thresholds > 1)) {
    stop("'ci_thresholds' values must be between 0 and 1.")
  }

  detection_data <- detection_data %>%
    dplyr::mutate(
      Detection_Date = lubridate::as_date(.data$Detection_Date),
      Confidence = as.numeric(.data$Confidence),
      Detection_Hour = as.integer(.data$Detection_Hour),
      AudioMoth_ID_Numeric = as.numeric(.data$AudioMoth_ID_Numeric)
    )

  # --- Get Study-Level Metadata ---
  # Use na.omit for n_distinct to ensure NAs in ID don't count as a distinct plot
  total_plots <- dplyr::n_distinct(stats::na.omit(detection_data$AudioMoth_ID_Numeric))
  if (total_plots == 0 && dplyr::n_distinct(detection_data$AudioMoth_ID_Numeric, na.rm = FALSE) > 0) {
    warning("All 'AudioMoth_ID_Numeric' values are NA. Spatial metrics will be NA or 0.")
  } else if (total_plots == 0) {
    # message("No valid 'AudioMoth_ID_Numeric' found. Spatial metrics will be NA or 0.")
  }


  min_study_date <- min(detection_data$Detection_Date, na.rm = TRUE)
  max_study_date <- max(detection_data$Detection_Date, na.rm = TRUE)

  if (is.infinite(min_study_date) || is.infinite(max_study_date) || is.na(min_study_date) || is.na(max_study_date)) {
    # warning("Could not determine valid study date range from 'Detection_Date'. Pct_Days_Active might be NA.")
    total_study_days <- NA_integer_
  } else {
    total_study_days <- as.integer(max_study_date - min_study_date) + 1
  }

  # --- Calculate General and CI Metrics per Species ---
  general_ci_metrics <- detection_data %>%
    dplyr::group_by(.data$Scientific_Name) %>%
    dplyr::summarise(
      Total_Detections = dplyr::n(),
      N_Plots_Detected = dplyr::n_distinct(.data$AudioMoth_ID_Numeric, na.rm = TRUE),
      Median_CI = stats::median(.data$Confidence, na.rm = TRUE),
      Mean_CI = mean(.data$Confidence, na.rm = TRUE),
      Min_CI = if (all(is.na(.data$Confidence))) NA_real_ else min(.data$Confidence, na.rm = TRUE),
      Max_CI = if (all(is.na(.data$Confidence))) NA_real_ else max(.data$Confidence, na.rm = TRUE),
      Q1_CI = stats::quantile(.data$Confidence, 0.25, na.rm = TRUE, type = 7),
      Q3_CI = stats::quantile(.data$Confidence, 0.75, na.rm = TRUE, type = 7),
      Pct_Dets_Above_CI_High = if (.data$Total_Detections > 0) sum(.data$Confidence >= ci_thresholds["high"], na.rm = TRUE) / .data$Total_Detections else 0,
      Pct_Dets_Above_CI_Medium = if (.data$Total_Detections > 0) sum(.data$Confidence >= ci_thresholds["medium"], na.rm = TRUE) / .data$Total_Detections else 0,
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      CI_IQR = .data$Q3_CI - .data$Q1_CI,
      Plot_Occupancy_Pct = if (total_plots > 0) .data$N_Plots_Detected / total_plots else NA_real_
    )

  temporal_metrics_df <- calculate_temporal_metrics(detection_data, total_study_days)
  spatial_metrics_df <- calculate_spatial_metrics(detection_data)

  all_metrics <- general_ci_metrics %>%
    dplyr::left_join(temporal_metrics_df, by = "Scientific_Name") %>%
    dplyr::left_join(spatial_metrics_df, by = "Scientific_Name")

  # Ensure all expected columns are present, even if some joins failed or parts were empty
  # This is a safeguard for consistent output structure.
  # Create a template with all expected columns and NA of the correct type.
  # Then merge `all_metrics` into this template.
  # This might be overly complex if the internal functions are robust.
  # For now, assume internal functions return all their columns for all species.

  return(all_metrics)
}


#' @keywords internal
calculate_temporal_metrics <- function(detection_data, total_study_days) {
  if (nrow(detection_data) == 0) {
    return(dplyr::tibble(
      Scientific_Name = character(0), N_Unique_Days_Detected = integer(0),
      Mean_Dets_Per_Active_Day = numeric(0), SD_Dets_Per_Active_Day = numeric(0),
      Max_Dets_Single_Day = integer(0), Daily_Dets_CV = numeric(0),
      Pct_Days_Active = numeric(0), N_Unique_Hours_Detected = integer(0),
      Hourly_Evenness_J = numeric(0), Hourly_Activity_Concentration = numeric(0)
    ))
  }

  # --- Daily Patterns ---
  daily_summary <- detection_data %>%
    dplyr::filter(!is.na(.data$Detection_Date)) %>% # Ensure no NA dates interfere
    dplyr::group_by(.data$Scientific_Name, .data$Detection_Date) %>%
    dplyr::summarise(N_Dets_This_Day = dplyr::n(), .groups = 'drop_last')

  if(nrow(daily_summary) == 0) {
    daily_metrics <- detection_data %>% dplyr::distinct(.data$Scientific_Name) %>%
      dplyr::mutate(N_Unique_Days_Detected = 0L, Mean_Dets_Per_Active_Day = NA_real_,
                    SD_Dets_Per_Active_Day = NA_real_, Max_Dets_Single_Day = 0L)
  } else {
    daily_metrics <- daily_summary %>%
      dplyr::summarise(
        N_Unique_Days_Detected = dplyr::n_distinct(.data$Detection_Date, na.rm = TRUE), # Should be redundant due to filter
        Mean_Dets_Per_Active_Day = mean(.data$N_Dets_This_Day, na.rm = TRUE),
        SD_Dets_Per_Active_Day = if(dplyr::n() > 1) stats::sd(.data$N_Dets_This_Day, na.rm = TRUE) else NA_real_, # SD needs >1 obs
        Max_Dets_Single_Day = if (all(is.na(.data$N_Dets_This_Day))) NA_integer_ else max(.data$N_Dets_This_Day, na.rm = TRUE),
        .groups = 'drop'
      )
  }

  daily_metrics <- daily_metrics %>%
    dplyr::mutate(
      Daily_Dets_CV = dplyr::if_else(
        !is.na(.data$Mean_Dets_Per_Active_Day) & .data$Mean_Dets_Per_Active_Day > 0 &
          !is.na(.data$SD_Dets_Per_Active_Day) & .data$N_Unique_Days_Detected > 1, # CV meaningful if mean > 0 and SD calculable
        .data$SD_Dets_Per_Active_Day / .data$Mean_Dets_Per_Active_Day,
        NA_real_
      ),
      Pct_Days_Active = if (!is.na(total_study_days) && total_study_days > 0) {
        .data$N_Unique_Days_Detected / total_study_days
      } else {
        NA_real_
      }
    )

  # --- Hourly Patterns ---
  hourly_counts <- detection_data %>%
    dplyr::filter(!is.na(.data$Detection_Hour)) %>%
    dplyr::group_by(.data$Scientific_Name, .data$Detection_Hour) %>%
    dplyr::summarise(N_Dets_This_Hour = dplyr::n(), .groups = 'drop_last')

  if(nrow(hourly_counts) == 0) {
    hourly_metrics <- detection_data %>% dplyr::distinct(.data$Scientific_Name) %>%
      dplyr::mutate(N_Unique_Hours_Detected = 0L, Hourly_Shannon_H = NA_real_)
  } else {
    hourly_metrics <- hourly_counts %>%
      dplyr::mutate(Total_Dets_Species = sum(.data$N_Dets_This_Hour, na.rm = TRUE)) %>% # Total detections for this species
      dplyr::filter(.data$Total_Dets_Species > 0) %>% # Ensure species has detections
      dplyr::mutate(Prop_Dets_This_Hour = .data$N_Dets_This_Hour / .data$Total_Dets_Species) %>%
      dplyr::summarise(
        N_Unique_Hours_Detected = dplyr::n_distinct(.data$Detection_Hour, na.rm = TRUE),
        Hourly_Shannon_H = -sum(.data$Prop_Dets_This_Hour * log(.data$Prop_Dets_This_Hour), na.rm = TRUE),
        .groups = 'drop'
      )
  }

  # Merge empty species back if they were filtered out by hourly_counts logic
  all_species_names <- dplyr::distinct(detection_data, .data$Scientific_Name)
  hourly_metrics <- dplyr::left_join(all_species_names, hourly_metrics, by = "Scientific_Name") %>%
    # Fill NAs for species with no hourly data if necessary
    dplyr::mutate(
      N_Unique_Hours_Detected = dplyr::coalesce(.data$N_Unique_Hours_Detected, 0L),
      Hourly_Shannon_H = dplyr::coalesce(.data$Hourly_Shannon_H, NA_real_)
    )


  hourly_metrics <- hourly_metrics %>%
    dplyr::mutate(
      Hourly_Max_H = dplyr::if_else(.data$N_Unique_Hours_Detected > 0, log(.data$N_Unique_Hours_Detected), NA_real_),
      Hourly_Evenness_J = dplyr::case_when(
        is.na(.data$Hourly_Shannon_H) | is.na(.data$Hourly_Max_H) ~ NA_real_,
        .data$Hourly_Max_H == 0 & .data$Hourly_Shannon_H == 0 & .data$N_Unique_Hours_Detected == 1 ~ 1.0, # Single hour detection -> perfect evenness by some defs
        .data$Hourly_Max_H == 0 & .data$N_Unique_Hours_Detected > 0 ~ NA_real_, # Should not happen if N_Unique_Hours_Detected > 0 -> log(N) > 0
        .data$Hourly_Max_H > 0 ~ .data$Hourly_Shannon_H / .data$Hourly_Max_H,
        TRUE ~ NA_real_ # Default case
      ),
      Hourly_Activity_Concentration = 1 - .data$Hourly_Evenness_J
    ) %>%
    dplyr::select("Scientific_Name", "N_Unique_Hours_Detected", "Hourly_Evenness_J", "Hourly_Activity_Concentration")


  temporal_all <- dplyr::left_join(daily_metrics, hourly_metrics, by = "Scientific_Name")
  return(temporal_all)
}

#' @keywords internal
calculate_spatial_metrics <- function(detection_data) {
  if (nrow(detection_data) == 0 || !"AudioMoth_ID_Numeric" %in% names(detection_data)) {
    return(dplyr::tibble(
      Scientific_Name = if("Scientific_Name" %in% names(detection_data)) character(0) else dplyr::distinct(detection_data, .data$Scientific_Name)$Scientific_Name,
      Mean_Dets_Per_Active_Plot = numeric(0), SD_Dets_Per_Active_Plot = numeric(0),
      Max_Dets_Single_Plot = integer(0), Dets_Per_Plot_CV = numeric(0)
    ))
  }

  # Detections per plot for each species
  plot_summary <- detection_data %>%
    dplyr::filter(!is.na(.data$AudioMoth_ID_Numeric)) %>% # Ensure no NA IDs interfere
    dplyr::group_by(.data$Scientific_Name, .data$AudioMoth_ID_Numeric) %>%
    dplyr::summarise(N_Dets_This_Plot = dplyr::n(), .groups = 'drop_last') # drop_last for next summarise

  if(nrow(plot_summary) == 0) { # No detections to summarize spatially
    spatial_metrics <- detection_data %>% dplyr::distinct(.data$Scientific_Name) %>%
      dplyr::mutate(Mean_Dets_Per_Active_Plot = NA_real_, SD_Dets_Per_Active_Plot = NA_real_,
                    Max_Dets_Single_Plot = 0L, N_Active_Plots_Species = 0L)
  } else {
    spatial_metrics <- plot_summary %>%
      dplyr::summarise(
        N_Active_Plots_Species = dplyr::n_distinct(.data$AudioMoth_ID_Numeric, na.rm = TRUE), # N_Plots_Detected for THIS species
        Mean_Dets_Per_Active_Plot = mean(.data$N_Dets_This_Plot, na.rm = TRUE),
        SD_Dets_Per_Active_Plot = if(dplyr::n() > 1) stats::sd(.data$N_Dets_This_Plot, na.rm = TRUE) else NA_real_,
        Max_Dets_Single_Plot = if (all(is.na(.data$N_Dets_This_Plot))) NA_integer_ else max(.data$N_Dets_This_Plot, na.rm = TRUE),
        .groups = 'drop'
      )
  }

  spatial_metrics <- spatial_metrics %>%
    dplyr::mutate(
      Dets_Per_Plot_CV = dplyr::if_else(
        !is.na(.data$Mean_Dets_Per_Active_Plot) & .data$Mean_Dets_Per_Active_Plot > 0 &
          !is.na(.data$SD_Dets_Per_Active_Plot) & .data$N_Active_Plots_Species > 1, # CV needs mean > 0 and SD calculable (N_plots > 1)
        .data$SD_Dets_Per_Active_Plot / .data$Mean_Dets_Per_Active_Plot,
        NA_real_
      )
    ) %>%
    dplyr::select(-"N_Active_Plots_Species") # This was helper, N_Plots_Detected is the main one

  return(spatial_metrics)
}
