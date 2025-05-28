#' Calculate Comprehensive Pattern Metrics for Species Detections
#'
#' This is the core function for calculating a wide range of quantitative
#' pattern indicators for each species from processed detection data.
#' These metrics cover general detection statistics, confidence interval (CI)
#' distributions, temporal patterns (daily and hourly), spatial patterns
#' (across plots/recorders), and statistical anomaly scores.
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
#' @param calculate_anomalies Logical. If `TRUE` (default), calculates statistical
#'   anomaly scores for each species based on their metrics.
#'
#' @return A `tibble` where each row represents a species and columns are the
#'   calculated metrics. Metrics include:
#'   \itemize{
#'     \item General: `Total_Detections`, `N_Plots_Detected`, `Plot_Occupancy_Pct`.
#'     \item CI Metrics: `Median_CI`, `Mean_CI`, `Min_CI`, `Max_CI`, `Q1_CI`, `Q3_CI`,
#'           `CI_IQR`, `CI_Skewness`, `Pct_Dets_Above_CI_High`, `Pct_Dets_Above_CI_Medium`.
#'     \item Temporal (Daily): `N_Unique_Days_Detected`, `Mean_Dets_Per_Active_Day`,
#'           `SD_Dets_Per_Active_Day`, `Max_Dets_Single_Day`, `Daily_Dets_CV`,
#'           `Pct_Days_Active`.
#'     \item Temporal (Hourly): `N_Unique_Hours_Detected`, `Hourly_Evenness_J`
#'           (Shannon's Evenness), `Hourly_Simpson_D` (Simpson's Diversity),
#'           `Hourly_Activity_Concentration` (1 - Evenness), `Peak_Hour`,
#'           `Mean_Hour_Circular`, `Hourly_R_Statistic`.
#'     \item Spatial: `Mean_Dets_Per_Active_Plot`, `SD_Dets_Per_Active_Plot`,
#'           `Max_Dets_Single_Plot`, `Dets_Per_Plot_CV`, `Spatial_Aggregation_Index`.
#'     \item Anomaly Scores (if calculate_anomalies = TRUE): `CI_Anomaly_Score`,
#'           `Temporal_Anomaly_Score`, `Spatial_Anomaly_Score`, `Overall_Anomaly_Score`.
#'   }
#'
#' @export
#' @importFrom stats median quantile sd na.omit mad
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
#' # Using custom CI thresholds and skipping anomaly detection
#' metrics_custom <- calculate_pattern_metrics(example_detections,
#'                                            ci_thresholds = c(high = 0.8, medium = 0.4),
#'                                            calculate_anomalies = FALSE)
#' print(metrics_custom)
#' }
calculate_pattern_metrics <- function(detection_data,
                                      ci_thresholds = c(high = 0.7, medium = 0.5),
                                      calculate_anomalies = TRUE) {

  # --- Input Validations ---
  if (!is.data.frame(detection_data)) {
    stop("'detection_data' must be a data frame or tibble.")
  }

  # Define expected column names for an empty metrics tibble to ensure consistency
  expected_metric_cols <- c(
    "Scientific_Name", "Total_Detections", "N_Plots_Detected", "Median_CI", "Mean_CI",
    "Min_CI", "Max_CI", "Q1_CI", "Q3_CI", "CI_IQR", "CI_Skewness", "Pct_Dets_Above_CI_High",
    "Pct_Dets_Above_CI_Medium", "Plot_Occupancy_Pct", "N_Unique_Days_Detected",
    "Mean_Dets_Per_Active_Day", "SD_Dets_Per_Active_Day", "Max_Dets_Single_Day",
    "Daily_Dets_CV", "Pct_Days_Active", "N_Unique_Hours_Detected",
    "Hourly_Evenness_J", "Hourly_Simpson_D", "Hourly_Activity_Concentration",
    "Peak_Hour", "Mean_Hour_Circular", "Hourly_R_Statistic",
    "Mean_Dets_Per_Active_Plot", "SD_Dets_Per_Active_Plot", "Max_Dets_Single_Plot",
    "Dets_Per_Plot_CV", "Spatial_Aggregation_Index"
  )

  if (nrow(detection_data) == 0) {
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
  total_plots <- dplyr::n_distinct(stats::na.omit(detection_data$AudioMoth_ID_Numeric))
  if (total_plots == 0 && dplyr::n_distinct(detection_data$AudioMoth_ID_Numeric, na.rm = FALSE) > 0) {
    warning("All 'AudioMoth_ID_Numeric' values are NA. Spatial metrics will be NA or 0.")
  }

  min_study_date <- min(detection_data$Detection_Date, na.rm = TRUE)
  max_study_date <- max(detection_data$Detection_Date, na.rm = TRUE)

  if (is.infinite(min_study_date) || is.infinite(max_study_date) || is.na(min_study_date) || is.na(max_study_date)) {
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
      CI_Skewness = calculate_skewness(.data$Confidence),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      CI_IQR = .data$Q3_CI - .data$Q1_CI,
      Plot_Occupancy_Pct = if (total_plots > 0) .data$N_Plots_Detected / total_plots else NA_real_
    )

  temporal_metrics_df <- calculate_temporal_metrics(detection_data, total_study_days)
  spatial_metrics_df <- calculate_spatial_metrics(detection_data, total_plots)

  all_metrics <- general_ci_metrics %>%
    dplyr::left_join(temporal_metrics_df, by = "Scientific_Name") %>%
    dplyr::left_join(spatial_metrics_df, by = "Scientific_Name")

  # Calculate anomaly scores if requested
  if (calculate_anomalies && nrow(all_metrics) > 1) {
    anomaly_scores <- calculate_anomaly_scores(all_metrics)
    all_metrics <- all_metrics %>%
      dplyr::left_join(anomaly_scores, by = "Scientific_Name")
  }

  return(all_metrics)
}


#' Calculate skewness of a numeric vector
#' @keywords internal
calculate_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)

  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(NA_real_)

  skew <- sum((x - m)^3) / (n * s^3)
  return(skew)
}


#' @keywords internal
calculate_temporal_metrics <- function(detection_data, total_study_days) {
  if (nrow(detection_data) == 0) {
    return(dplyr::tibble(
      Scientific_Name = character(0), N_Unique_Days_Detected = integer(0),
      Mean_Dets_Per_Active_Day = numeric(0), SD_Dets_Per_Active_Day = numeric(0),
      Max_Dets_Single_Day = integer(0), Daily_Dets_CV = numeric(0),
      Pct_Days_Active = numeric(0), N_Unique_Hours_Detected = integer(0),
      Hourly_Evenness_J = numeric(0), Hourly_Simpson_D = numeric(0),
      Hourly_Activity_Concentration = numeric(0), Peak_Hour = integer(0),
      Mean_Hour_Circular = numeric(0), Hourly_R_Statistic = numeric(0)
    ))
  }

  # --- Daily Patterns ---
  daily_summary <- detection_data %>%
    dplyr::filter(!is.na(.data$Detection_Date)) %>%
    dplyr::group_by(.data$Scientific_Name, .data$Detection_Date) %>%
    dplyr::summarise(N_Dets_This_Day = dplyr::n(), .groups = 'drop_last')

  if(nrow(daily_summary) == 0) {
    daily_metrics <- detection_data %>% dplyr::distinct(.data$Scientific_Name) %>%
      dplyr::mutate(N_Unique_Days_Detected = 0L, Mean_Dets_Per_Active_Day = NA_real_,
                    SD_Dets_Per_Active_Day = NA_real_, Max_Dets_Single_Day = 0L)
  } else {
    daily_metrics <- daily_summary %>%
      dplyr::summarise(
        N_Unique_Days_Detected = dplyr::n_distinct(.data$Detection_Date, na.rm = TRUE),
        Mean_Dets_Per_Active_Day = mean(.data$N_Dets_This_Day, na.rm = TRUE),
        SD_Dets_Per_Active_Day = if(dplyr::n() > 1) stats::sd(.data$N_Dets_This_Day, na.rm = TRUE) else NA_real_,
        Max_Dets_Single_Day = if (all(is.na(.data$N_Dets_This_Day))) NA_integer_ else max(.data$N_Dets_This_Day, na.rm = TRUE),
        .groups = 'drop'
      )
  }

  daily_metrics <- daily_metrics %>%
    dplyr::mutate(
      Daily_Dets_CV = dplyr::if_else(
        !is.na(.data$Mean_Dets_Per_Active_Day) & .data$Mean_Dets_Per_Active_Day > 0 &
          !is.na(.data$SD_Dets_Per_Active_Day) & .data$N_Unique_Days_Detected > 1,
        .data$SD_Dets_Per_Active_Day / .data$Mean_Dets_Per_Active_Day,
        NA_real_
      ),
      Pct_Days_Active = if (!is.na(total_study_days) && total_study_days > 0) {
        .data$N_Unique_Days_Detected / total_study_days
      } else {
        NA_real_
      }
    )

  # --- Hourly Patterns with Enhanced Metrics ---
  hourly_counts <- detection_data %>%
    dplyr::filter(!is.na(.data$Detection_Hour)) %>%
    dplyr::group_by(.data$Scientific_Name, .data$Detection_Hour) %>%
    dplyr::summarise(N_Dets_This_Hour = dplyr::n(), .groups = 'drop_last')

  if(nrow(hourly_counts) == 0) {
    hourly_metrics <- detection_data %>% dplyr::distinct(.data$Scientific_Name) %>%
      dplyr::mutate(N_Unique_Hours_Detected = 0L, Hourly_Shannon_H = NA_real_,
                    Hourly_Simpson_D = NA_real_, Peak_Hour = NA_integer_,
                    Mean_Hour_Circular = NA_real_, Hourly_R_Statistic = NA_real_)
  } else {
    # Calculate diversity indices and circular statistics
    hourly_metrics <- hourly_counts %>%
      dplyr::mutate(Total_Dets_Species = sum(.data$N_Dets_This_Hour, na.rm = TRUE)) %>%
      dplyr::filter(.data$Total_Dets_Species > 0) %>%
      dplyr::mutate(
        Prop_Dets_This_Hour = .data$N_Dets_This_Hour / .data$Total_Dets_Species,
        Prop_Squared = .data$Prop_Dets_This_Hour^2,
        # For circular statistics
        Hour_Radians = .data$Detection_Hour * 2 * pi / 24,
        Cos_Hour = cos(.data$Hour_Radians) * .data$N_Dets_This_Hour,
        Sin_Hour = sin(.data$Hour_Radians) * .data$N_Dets_This_Hour
      ) %>%
      dplyr::summarise(
        N_Unique_Hours_Detected = dplyr::n_distinct(.data$Detection_Hour, na.rm = TRUE),
        Hourly_Shannon_H = -sum(.data$Prop_Dets_This_Hour * log(.data$Prop_Dets_This_Hour), na.rm = TRUE),
        Hourly_Simpson_D = 1 - sum(.data$Prop_Squared, na.rm = TRUE),
        Peak_Hour = .data$Detection_Hour[which.max(.data$N_Dets_This_Hour)],
        # Circular mean hour
        Mean_Cos = sum(.data$Cos_Hour) / sum(.data$N_Dets_This_Hour),
        Mean_Sin = sum(.data$Sin_Hour) / sum(.data$N_Dets_This_Hour),
        Mean_Hour_Circular = (atan2(.data$Mean_Sin, .data$Mean_Cos) * 24 / (2 * pi)) %% 24,
        # Rayleigh R statistic for concentration
        Hourly_R_Statistic = sqrt(.data$Mean_Cos^2 + .data$Mean_Sin^2),
        .groups = 'drop'
      ) %>%
      dplyr::select(-"Mean_Cos", -"Mean_Sin")
  }

  # Merge empty species back if they were filtered out
  all_species_names <- dplyr::distinct(detection_data, .data$Scientific_Name)
  hourly_metrics <- dplyr::left_join(all_species_names, hourly_metrics, by = "Scientific_Name") %>%
    dplyr::mutate(
      N_Unique_Hours_Detected = dplyr::coalesce(.data$N_Unique_Hours_Detected, 0L),
      Hourly_Shannon_H = dplyr::coalesce(.data$Hourly_Shannon_H, NA_real_),
      Hourly_Simpson_D = dplyr::coalesce(.data$Hourly_Simpson_D, NA_real_),
      Peak_Hour = dplyr::coalesce(.data$Peak_Hour, NA_integer_),
      Mean_Hour_Circular = dplyr::coalesce(.data$Mean_Hour_Circular, NA_real_),
      Hourly_R_Statistic = dplyr::coalesce(.data$Hourly_R_Statistic, NA_real_)
    )

  hourly_metrics <- hourly_metrics %>%
    dplyr::mutate(
      Hourly_Max_H = dplyr::if_else(.data$N_Unique_Hours_Detected > 0, log(.data$N_Unique_Hours_Detected), NA_real_),
      Hourly_Evenness_J = dplyr::case_when(
        is.na(.data$Hourly_Shannon_H) | is.na(.data$Hourly_Max_H) ~ NA_real_,
        .data$Hourly_Max_H == 0 & .data$Hourly_Shannon_H == 0 & .data$N_Unique_Hours_Detected == 1 ~ 1.0,
        .data$Hourly_Max_H == 0 & .data$N_Unique_Hours_Detected > 0 ~ NA_real_,
        .data$Hourly_Max_H > 0 ~ .data$Hourly_Shannon_H / .data$Hourly_Max_H,
        TRUE ~ NA_real_
      ),
      Hourly_Activity_Concentration = 1 - .data$Hourly_Evenness_J
    ) %>%
    dplyr::select("Scientific_Name", "N_Unique_Hours_Detected", "Hourly_Evenness_J",
                  "Hourly_Simpson_D", "Hourly_Activity_Concentration", "Peak_Hour",
                  "Mean_Hour_Circular", "Hourly_R_Statistic")

  temporal_all <- dplyr::left_join(daily_metrics, hourly_metrics, by = "Scientific_Name")
  return(temporal_all)
}


#' @keywords internal
calculate_spatial_metrics <- function(detection_data, total_plots) {
  if (nrow(detection_data) == 0 || !"AudioMoth_ID_Numeric" %in% names(detection_data)) {
    return(dplyr::tibble(
      Scientific_Name = if("Scientific_Name" %in% names(detection_data)) character(0) else dplyr::distinct(detection_data, .data$Scientific_Name)$Scientific_Name,
      Mean_Dets_Per_Active_Plot = numeric(0), SD_Dets_Per_Active_Plot = numeric(0),
      Max_Dets_Single_Plot = integer(0), Dets_Per_Plot_CV = numeric(0),
      Spatial_Aggregation_Index = numeric(0)
    ))
  }

  # Detections per plot for each species
  plot_summary <- detection_data %>%
    dplyr::filter(!is.na(.data$AudioMoth_ID_Numeric)) %>%
    dplyr::group_by(.data$Scientific_Name, .data$AudioMoth_ID_Numeric) %>%
    dplyr::summarise(N_Dets_This_Plot = dplyr::n(), .groups = 'drop_last')

  if(nrow(plot_summary) == 0) {
    spatial_metrics <- detection_data %>% dplyr::distinct(.data$Scientific_Name) %>%
      dplyr::mutate(Mean_Dets_Per_Active_Plot = NA_real_, SD_Dets_Per_Active_Plot = NA_real_,
                    Max_Dets_Single_Plot = 0L, N_Active_Plots_Species = 0L,
                    Spatial_Aggregation_Index = NA_real_)
  } else {
    spatial_metrics <- plot_summary %>%
      dplyr::summarise(
        N_Active_Plots_Species = dplyr::n_distinct(.data$AudioMoth_ID_Numeric, na.rm = TRUE),
        Mean_Dets_Per_Active_Plot = mean(.data$N_Dets_This_Plot, na.rm = TRUE),
        SD_Dets_Per_Active_Plot = if(dplyr::n() > 1) stats::sd(.data$N_Dets_This_Plot, na.rm = TRUE) else NA_real_,
        Max_Dets_Single_Plot = if (all(is.na(.data$N_Dets_This_Plot))) NA_integer_ else max(.data$N_Dets_This_Plot, na.rm = TRUE),
        # Calculate spatial aggregation index (variance to mean ratio)
        Variance_Dets = if(dplyr::n() > 1) var(.data$N_Dets_This_Plot, na.rm = TRUE) else NA_real_,
        .groups = 'drop'
      ) %>%
      dplyr::mutate(
        # Aggregation index: >1 indicates clustering, <1 indicates regular distribution
        Spatial_Aggregation_Index = dplyr::if_else(
          !is.na(.data$Mean_Dets_Per_Active_Plot) & .data$Mean_Dets_Per_Active_Plot > 0,
          .data$Variance_Dets / .data$Mean_Dets_Per_Active_Plot,
          NA_real_
        )
      )
  }

  spatial_metrics <- spatial_metrics %>%
    dplyr::mutate(
      Dets_Per_Plot_CV = dplyr::if_else(
        !is.na(.data$Mean_Dets_Per_Active_Plot) & .data$Mean_Dets_Per_Active_Plot > 0 &
          !is.na(.data$SD_Dets_Per_Active_Plot) & .data$N_Active_Plots_Species > 1,
        .data$SD_Dets_Per_Active_Plot / .data$Mean_Dets_Per_Active_Plot,
        NA_real_
      )
    ) %>%
    dplyr::select(-"N_Active_Plots_Species", -"Variance_Dets")

  return(spatial_metrics)
}


#' Calculate statistical anomaly scores for pattern metrics
#' @keywords internal
calculate_anomaly_scores <- function(metrics_df) {
  # Function to calculate robust z-score using MAD
  robust_z_score <- function(x) {
    x_median <- median(x, na.rm = TRUE)
    x_mad <- mad(x, na.rm = TRUE)
    if (x_mad == 0) return(rep(0, length(x)))
    abs(x - x_median) / x_mad
  }

  # Calculate anomaly scores for different metric groups
  anomaly_df <- metrics_df %>%
    dplyr::mutate(
      # CI-based anomaly scores
      CI_Median_Anomaly = robust_z_score(.data$Median_CI),
      CI_IQR_Anomaly = robust_z_score(.data$CI_IQR),
      CI_Skew_Anomaly = robust_z_score(abs(.data$CI_Skewness)),

      # Temporal anomaly scores
      Daily_CV_Anomaly = robust_z_score(.data$Daily_Dets_CV),
      Hourly_Conc_Anomaly = robust_z_score(.data$Hourly_Activity_Concentration),
      Hourly_R_Anomaly = robust_z_score(.data$Hourly_R_Statistic),

      # Spatial anomaly scores
      Plot_Occ_Anomaly = robust_z_score(.data$Plot_Occupancy_Pct),
      Spatial_CV_Anomaly = robust_z_score(.data$Dets_Per_Plot_CV),
      Spatial_Agg_Anomaly = robust_z_score(log1p(.data$Spatial_Aggregation_Index)),

      # Composite scores (mean of component scores, capped at 5)
      CI_Anomaly_Score = pmin(rowMeans(dplyr::across(c(CI_Median_Anomaly, CI_IQR_Anomaly, CI_Skew_Anomaly)), na.rm = TRUE), 5),
      Temporal_Anomaly_Score = pmin(rowMeans(dplyr::across(c(Daily_CV_Anomaly, Hourly_Conc_Anomaly, Hourly_R_Anomaly)), na.rm = TRUE), 5),
      Spatial_Anomaly_Score = pmin(rowMeans(dplyr::across(c(Plot_Occ_Anomaly, Spatial_CV_Anomaly, Spatial_Agg_Anomaly)), na.rm = TRUE), 5),

      # Overall anomaly score
      Overall_Anomaly_Score = rowMeans(dplyr::across(c(CI_Anomaly_Score, Temporal_Anomaly_Score, Spatial_Anomaly_Score)), na.rm = TRUE)
    ) %>%
    dplyr::select(Scientific_Name, CI_Anomaly_Score, Temporal_Anomaly_Score,
                  Spatial_Anomaly_Score, Overall_Anomaly_Score)

  return(anomaly_df)
}
