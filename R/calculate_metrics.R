# File: R/calculate_metrics.R

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
#' @importFrom stats var lm coef median quantile sd na.omit mad
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
  # Add anomaly score columns if they are expected to be calculated by default
  if (calculate_anomalies) {
    expected_metric_cols <- c(expected_metric_cols, "CI_Anomaly_Score",
                              "Temporal_Anomaly_Score", "Spatial_Anomaly_Score",
                              "Overall_Anomaly_Score")
  }


  if (nrow(detection_data) == 0) {
    # Create an empty tibble with all expected columns of correct types
    empty_metrics_list <- lapply(expected_metric_cols, function(col_name) {
      if (grepl("Name", col_name, ignore.case = TRUE)) character(0)
      else if (grepl("Count|Total|Detected|Day|Hour|Plot", col_name, ignore.case = TRUE) && !grepl("Pct|CV|Index|Score|Median|Mean|Min|Max|Q1|Q3|IQR|Skewness|Evenness|Simpson|Concentration|Statistic", col_name, ignore.case = TRUE)) integer(0)
      else numeric(0)
    })
    names(empty_metrics_list) <- expected_metric_cols
    return(dplyr::as_tibble(empty_metrics_list))
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
      Scientific_Name = as.character(.data$Scientific_Name),
      Detection_Date = lubridate::as_date(.data$Detection_Date),
      Confidence = as.numeric(.data$Confidence),
      Detection_Hour = as.integer(.data$Detection_Hour),
      AudioMoth_ID_Numeric = as.numeric(.data$AudioMoth_ID_Numeric)
    )

  total_plots <- dplyr::n_distinct(stats::na.omit(detection_data$AudioMoth_ID_Numeric))
  if (total_plots == 0 && dplyr::n_distinct(detection_data$AudioMoth_ID_Numeric, na.rm = FALSE) > 0 && nrow(detection_data) > 0) {
    warning("All 'AudioMoth_ID_Numeric' values are NA or only one unique non-NA ID. Spatial metrics might be NA or limited.")
  } else if (total_plots == 0 && nrow(detection_data) > 0) {
    warning("No valid 'AudioMoth_ID_Numeric' values found (all NA or zero unique non-NA IDs). Spatial metrics will be NA.")
  }

  min_study_date <- min(detection_data$Detection_Date, na.rm = TRUE)
  max_study_date <- max(detection_data$Detection_Date, na.rm = TRUE)

  if (is.infinite(min_study_date) || is.infinite(max_study_date) || is.na(min_study_date) || is.na(max_study_date)) {
    total_study_days <- NA_integer_
  } else {
    total_study_days <- as.integer(max_study_date - min_study_date) + 1
  }

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

  if (calculate_anomalies) {
    if (nrow(all_metrics) > 1) {
      anomaly_scores <- calculate_anomaly_scores(all_metrics)
      all_metrics <- dplyr::left_join(all_metrics, anomaly_scores, by = "Scientific_Name")
    } else {
      all_metrics <- all_metrics %>%
        dplyr::mutate(
          CI_Anomaly_Score = NA_real_,
          Temporal_Anomaly_Score = NA_real_,
          Spatial_Anomaly_Score = NA_real_,
          Overall_Anomaly_Score = NA_real_
        )
    }
  }

  current_cols <- names(all_metrics)
  missing_expected_metric_cols <- setdiff(expected_metric_cols, current_cols)
  if(length(missing_expected_metric_cols) > 0) {
    for(col_to_add in missing_expected_metric_cols) {
      if (grepl("Name", col_to_add, ignore.case = TRUE)) all_metrics[[col_to_add]] <- NA_character_
      else if (grepl("Count|Total|Detected|Day|Hour|Plot", col_to_add, ignore.case = TRUE) && !grepl("Pct|CV|Index|Score|Median|Mean|Min|Max|Q1|Q3|IQR|Skewness|Evenness|Simpson|Concentration|Statistic", col_to_add, ignore.case = TRUE)) all_metrics[[col_to_add]] <- NA_integer_
      else all_metrics[[col_to_add]] <- NA_real_
    }
  }
  final_cols_order <- intersect(expected_metric_cols, names(all_metrics))
  all_metrics <- dplyr::select(all_metrics, dplyr::all_of(final_cols_order))

  return(all_metrics)
}


#' @keywords internal
calculate_skewness <- function(x) {
  x_clean <- stats::na.omit(x)
  n <- length(x_clean)
  if (n < 3) return(NA_real_)

  m <- mean(x_clean)
  s <- stats::sd(x_clean)
  if (is.na(s) || s == 0) return(NA_real_)

  skew <- sum((x_clean - m)^3) / (n * s^3)
  return(skew)
}

#' @keywords internal
calculate_temporal_metrics <- function(detection_data, total_study_days) {
  empty_temporal_df_structure <- dplyr::tibble(
    Scientific_Name = character(0),
    N_Unique_Days_Detected = integer(0),
    Mean_Dets_Per_Active_Day = numeric(0),
    SD_Dets_Per_Active_Day = numeric(0),
    Max_Dets_Single_Day = integer(0),
    Daily_Dets_CV = numeric(0),
    Pct_Days_Active = numeric(0),
    N_Unique_Hours_Detected = integer(0),
    Hourly_Evenness_J = numeric(0),
    Hourly_Simpson_D = numeric(0),
    Hourly_Activity_Concentration = numeric(0),
    Peak_Hour = integer(0),
    Mean_Hour_Circular = numeric(0),
    Hourly_R_Statistic = numeric(0)
  )

  if (nrow(detection_data) == 0) {
    return(empty_temporal_df_structure)
  }

  all_species_in_data <- dplyr::distinct(detection_data, .data$Scientific_Name)

  daily_summary <- detection_data %>%
    dplyr::filter(!is.na(.data$Detection_Date)) %>%
    dplyr::group_by(.data$Scientific_Name, .data$Detection_Date) %>%
    dplyr::summarise(N_Dets_This_Day = dplyr::n(), .groups = 'drop_last')

  if(nrow(daily_summary) == 0) {
    daily_metrics <- all_species_in_data %>%
      dplyr::mutate(N_Unique_Days_Detected = 0L, Mean_Dets_Per_Active_Day = NA_real_,
                    SD_Dets_Per_Active_Day = NA_real_, Max_Dets_Single_Day = 0L,
                    Daily_Dets_CV = NA_real_, Pct_Days_Active = NA_real_)
  } else {
    daily_metrics <- daily_summary %>%
      dplyr::group_by(.data$Scientific_Name) %>%
      dplyr::summarise(
        N_Unique_Days_Detected = dplyr::n_distinct(.data$Detection_Date, na.rm = TRUE),
        Mean_Dets_Per_Active_Day = mean(.data$N_Dets_This_Day, na.rm = TRUE),
        SD_Dets_Per_Active_Day = if(dplyr::n_distinct(.data$Detection_Date, na.rm=TRUE) > 1) stats::sd(.data$N_Dets_This_Day, na.rm = TRUE) else NA_real_,
        Max_Dets_Single_Day = if (all(is.na(.data$N_Dets_This_Day))) NA_integer_ else max(.data$N_Dets_This_Day, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::mutate( # Corrected: Using dplyr::mutate
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
    daily_metrics <- dplyr::left_join(all_species_in_data, daily_metrics, by = "Scientific_Name") %>%
      dplyr::mutate( # Corrected: Using dplyr::mutate
        N_Unique_Days_Detected = dplyr::coalesce(.data$N_Unique_Days_Detected, 0L),
        Max_Dets_Single_Day = dplyr::coalesce(.data$Max_Dets_Single_Day, 0L)
      )
  }

  hourly_counts <- detection_data %>%
    dplyr::filter(!is.na(.data$Detection_Hour)) %>%
    dplyr::group_by(.data$Scientific_Name, .data$Detection_Hour) %>%
    dplyr::summarise(N_Dets_This_Hour = dplyr::n(), .groups = 'drop_last')

  if(nrow(hourly_counts) == 0) {
    hourly_metrics <- all_species_in_data %>%
      dplyr::mutate(N_Unique_Hours_Detected = 0L, Hourly_Shannon_H = NA_real_,
                    Hourly_Simpson_D = NA_real_, Peak_Hour = NA_integer_,
                    Mean_Hour_Circular = NA_real_, Hourly_R_Statistic = NA_real_,
                    Hourly_Max_H = NA_real_, Hourly_Evenness_J = NA_real_,
                    Hourly_Activity_Concentration = NA_real_)
  } else {
    hourly_metrics <- hourly_counts %>%
      dplyr::group_by(.data$Scientific_Name) %>%
      dplyr::mutate(Total_Dets_Species_Hourly = sum(.data$N_Dets_This_Hour, na.rm = TRUE)) %>% # Corrected: Using dplyr::mutate
      dplyr::filter(.data$Total_Dets_Species_Hourly > 0) %>%
      dplyr::mutate( # Corrected: Using dplyr::mutate
        Prop_Dets_This_Hour = .data$N_Dets_This_Hour / .data$Total_Dets_Species_Hourly,
        Prop_Squared = .data$Prop_Dets_This_Hour^2,
        Hour_Radians = .data$Detection_Hour * 2 * pi / 24,
        Cos_Hour = cos(.data$Hour_Radians) * .data$N_Dets_This_Hour,
        Sin_Hour = sin(.data$Hour_Radians) * .data$N_Dets_This_Hour
      ) %>%
      dplyr::summarise(
        N_Unique_Hours_Detected = dplyr::n_distinct(.data$Detection_Hour, na.rm = TRUE),
        Hourly_Shannon_H = -sum(.data$Prop_Dets_This_Hour * log(.data$Prop_Dets_This_Hour), na.rm = TRUE),
        Hourly_Simpson_D_calc = sum(.data$Prop_Squared, na.rm = TRUE),
        Peak_Hour = .data$Detection_Hour[which.max(.data$N_Dets_This_Hour)][1],
        Sum_Cos_Hour_Weighted = sum(.data$Cos_Hour, na.rm = TRUE),
        Sum_Sin_Hour_Weighted = sum(.data$Sin_Hour, na.rm = TRUE),
        Sum_N_Dets_This_Hour = sum(.data$N_Dets_This_Hour, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::mutate( # Corrected: Using dplyr::mutate
        Hourly_Simpson_D = 1 - .data$Hourly_Simpson_D_calc,
        Mean_Cos = dplyr::if_else(.data$Sum_N_Dets_This_Hour > 0, .data$Sum_Cos_Hour_Weighted / .data$Sum_N_Dets_This_Hour, NA_real_),
        Mean_Sin = dplyr::if_else(.data$Sum_N_Dets_This_Hour > 0, .data$Sum_Sin_Hour_Weighted / .data$Sum_N_Dets_This_Hour, NA_real_),
        Mean_Hour_Circular = dplyr::if_else(!is.na(.data$Mean_Cos) & !is.na(.data$Mean_Sin), (atan2(.data$Mean_Sin, .data$Mean_Cos) * 24 / (2 * pi)) %% 24, NA_real_),
        Hourly_R_Statistic = dplyr::if_else(!is.na(.data$Mean_Cos) & !is.na(.data$Mean_Sin), sqrt(.data$Mean_Cos^2 + .data$Mean_Sin^2), NA_real_)
      ) %>%
      dplyr::mutate( # Corrected: Using dplyr::mutate
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

    hourly_metrics <- dplyr::left_join(all_species_in_data, hourly_metrics, by = "Scientific_Name") %>%
      dplyr::mutate( # Corrected: Using dplyr::mutate
        N_Unique_Hours_Detected = dplyr::coalesce(.data$N_Unique_Hours_Detected, 0L),
        Peak_Hour = dplyr::coalesce(.data$Peak_Hour, NA_integer_)
      )
  }

  temporal_all <- dplyr::left_join(daily_metrics, hourly_metrics, by = "Scientific_Name")
  for(col_name in names(empty_temporal_df_structure)){
    if(!col_name %in% names(temporal_all)){
      temporal_all[[col_name]] <- switch(
        class(empty_temporal_df_structure[[col_name]])[1],
        "integer" = NA_integer_,
        "numeric" = NA_real_,
        "character" = NA_character_,
        NA
      )
    }
  }
  temporal_all <- temporal_all %>% dplyr::select(dplyr::all_of(names(empty_temporal_df_structure)))

  return(temporal_all)
}


#' @keywords internal
calculate_spatial_metrics <- function(detection_data, total_plots) {
  empty_spatial_df_structure <- dplyr::tibble(
    Scientific_Name = character(0),
    Mean_Dets_Per_Active_Plot = numeric(0),
    SD_Dets_Per_Active_Plot = numeric(0),
    Max_Dets_Single_Plot = integer(0),
    Dets_Per_Plot_CV = numeric(0),
    Spatial_Aggregation_Index = numeric(0)
  )

  if (nrow(detection_data) == 0 || !"AudioMoth_ID_Numeric" %in% names(detection_data) || total_plots == 0) {
    if(nrow(detection_data) > 0 && "Scientific_Name" %in% names(detection_data)){
      all_species_in_data <- dplyr::distinct(detection_data, .data$Scientific_Name)
      return(dplyr::left_join(all_species_in_data, empty_spatial_df_structure, by="Scientific_Name") %>%
               dplyr::mutate(Max_Dets_Single_Plot = dplyr::coalesce(.data$Max_Dets_Single_Plot, 0L))) # Corrected: Using dplyr::mutate
    } else {
      return(empty_spatial_df_structure)
    }
  }

  all_species_in_data <- dplyr::distinct(detection_data, .data$Scientific_Name)

  plot_summary <- detection_data %>%
    dplyr::filter(!is.na(.data$AudioMoth_ID_Numeric)) %>%
    dplyr::group_by(.data$Scientific_Name, .data$AudioMoth_ID_Numeric) %>%
    dplyr::summarise(N_Dets_This_Plot = dplyr::n(), .groups = 'drop_last')

  if(nrow(plot_summary) == 0) {
    spatial_metrics <- all_species_in_data %>%
      dplyr::mutate(Mean_Dets_Per_Active_Plot = NA_real_, SD_Dets_Per_Active_Plot = NA_real_, # Corrected: Using dplyr::mutate
                    Max_Dets_Single_Plot = 0L,
                    Spatial_Aggregation_Index = NA_real_, Dets_Per_Plot_CV = NA_real_)
  } else {
    spatial_metrics <- plot_summary %>%
      dplyr::group_by(.data$Scientific_Name) %>%
      dplyr::summarise(
        N_Active_Plots_Species = dplyr::n_distinct(.data$AudioMoth_ID_Numeric, na.rm = TRUE),
        Mean_Dets_Per_Active_Plot = mean(.data$N_Dets_This_Plot, na.rm = TRUE),
        SD_Dets_Per_Active_Plot = if(dplyr::n_distinct(.data$AudioMoth_ID_Numeric, na.rm=TRUE) > 1) stats::sd(.data$N_Dets_This_Plot, na.rm = TRUE) else NA_real_,
        Max_Dets_Single_Plot = if (all(is.na(.data$N_Dets_This_Plot))) NA_integer_ else max(.data$N_Dets_This_Plot, na.rm = TRUE),
        Variance_Dets = if(dplyr::n_distinct(.data$AudioMoth_ID_Numeric, na.rm=TRUE) > 1) stats::var(.data$N_Dets_This_Plot, na.rm = TRUE) else NA_real_,
        .groups = 'drop'
      ) %>%
      dplyr::mutate( # Corrected: Using dplyr::mutate
        Spatial_Aggregation_Index = dplyr::if_else(
          !is.na(.data$Mean_Dets_Per_Active_Plot) & .data$Mean_Dets_Per_Active_Plot > 0 & !is.na(.data$Variance_Dets),
          .data$Variance_Dets / .data$Mean_Dets_Per_Active_Plot,
          NA_real_
        ),
        Dets_Per_Plot_CV = dplyr::if_else(
          !is.na(.data$Mean_Dets_Per_Active_Plot) & .data$Mean_Dets_Per_Active_Plot > 0 &
            !is.na(.data$SD_Dets_Per_Active_Plot) & .data$N_Active_Plots_Species > 1,
          .data$SD_Dets_Per_Active_Plot / .data$Mean_Dets_Per_Active_Plot,
          NA_real_
        )
      ) %>%
      dplyr::select("Scientific_Name", "Mean_Dets_Per_Active_Plot", "SD_Dets_Per_Active_Plot",
                    "Max_Dets_Single_Plot", "Dets_Per_Plot_CV", "Spatial_Aggregation_Index")

    spatial_metrics <- dplyr::left_join(all_species_in_data, spatial_metrics, by = "Scientific_Name") %>%
      dplyr::mutate(Max_Dets_Single_Plot = dplyr::coalesce(.data$Max_Dets_Single_Plot, 0L)) # Corrected: Using dplyr::mutate
  }

  for(col_name in names(empty_spatial_df_structure)){
    if(!col_name %in% names(spatial_metrics)){
      spatial_metrics[[col_name]] <- switch(
        class(empty_spatial_df_structure[[col_name]])[1],
        "integer" = NA_integer_,
        "numeric" = NA_real_,
        "character" = NA_character_,
        NA
      )
    }
  }
  spatial_metrics <- spatial_metrics %>% dplyr::select(dplyr::all_of(names(empty_spatial_df_structure)))

  return(spatial_metrics)
}


#' @keywords internal
calculate_anomaly_scores <- function(metrics_df) {
  empty_anomaly_df_structure <- dplyr::tibble(
    Scientific_Name = character(0),
    CI_Anomaly_Score = numeric(0),
    Temporal_Anomaly_Score = numeric(0),
    Spatial_Anomaly_Score = numeric(0),
    Overall_Anomaly_Score = numeric(0)
  )

  if (!is.data.frame(metrics_df) || nrow(metrics_df) <= 1) {
    if(nrow(metrics_df) > 0 && "Scientific_Name" %in% names(metrics_df)){
      all_species_in_data <- dplyr::distinct(metrics_df, .data$Scientific_Name)
      return(dplyr::left_join(all_species_in_data, empty_anomaly_df_structure, by="Scientific_Name"))
    } else {
      return(empty_anomaly_df_structure)
    }
  }

  robust_z_score <- function(x) {
    if (!is.numeric(x)) return(rep(NA_real_, length(x)))
    x_valid <- x[!is.na(x)]
    if (length(x_valid) < 2) return(rep(NA_real_, length(x)))

    x_median <- stats::median(x_valid, na.rm = TRUE)
    x_mad <- stats::mad(x_valid, constant = 1.4826, na.rm = TRUE)

    z <- rep(NA_real_, length(x))
    idx_not_na <- !is.na(x)

    if (x_mad == 0) {
      z[idx_not_na & x == x_median] <- 0
      z[idx_not_na & x != x_median] <- NA_real_
    } else {
      z[idx_not_na] <- abs(x[idx_not_na] - x_median) / x_mad
    }
    return(z)
  }

  metrics_transformed <- metrics_df %>%
    dplyr::mutate( # Corrected: Using dplyr::mutate
      CI_Skewness_Trans = if ("CI_Skewness" %in% names(.)) abs(.data$CI_Skewness) else NA_real_,
      Spatial_Aggregation_Index_Trans = if ("Spatial_Aggregation_Index" %in% names(.)) log1p(.data$Spatial_Aggregation_Index) else NA_real_
    )

  z_scores_df <- metrics_transformed %>%
    dplyr::mutate( # Corrected: Using dplyr::mutate
      Z_Median_CI = if ("Median_CI" %in% names(.)) robust_z_score(.data$Median_CI) else NA_real_,
      Z_CI_IQR = if ("CI_IQR" %in% names(.)) robust_z_score(.data$CI_IQR) else NA_real_,
      Z_CI_Skewness = if ("CI_Skewness_Trans" %in% names(.)) robust_z_score(.data$CI_Skewness_Trans) else NA_real_,

      Z_Daily_Dets_CV = if ("Daily_Dets_CV" %in% names(.)) robust_z_score(.data$Daily_Dets_CV) else NA_real_,
      Z_Hourly_Activity_Concentration = if ("Hourly_Activity_Concentration" %in% names(.)) robust_z_score(.data$Hourly_Activity_Concentration) else NA_real_,
      Z_Hourly_R_Statistic = if ("Hourly_R_Statistic" %in% names(.)) robust_z_score(.data$Hourly_R_Statistic) else NA_real_,

      Z_Plot_Occupancy_Pct = if ("Plot_Occupancy_Pct" %in% names(.)) robust_z_score(.data$Plot_Occupancy_Pct) else NA_real_,
      Z_Dets_Per_Plot_CV = if ("Dets_Per_Plot_CV" %in% names(.)) robust_z_score(.data$Dets_Per_Plot_CV) else NA_real_,
      Z_Spatial_Aggregation_Index = if ("Spatial_Aggregation_Index_Trans" %in% names(.)) robust_z_score(.data$Spatial_Aggregation_Index_Trans) else NA_real_
    )

  z_cap <- 5

  anomaly_scores_calculated <- z_scores_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate( # Corrected: Using dplyr::mutate
      CI_Anomaly_Score = pmin(mean(c(
        if ("Z_Median_CI" %in% names(.)) .data$Z_Median_CI else NA,
        if ("Z_CI_IQR" %in% names(.)) .data$Z_CI_IQR else NA,
        if ("Z_CI_Skewness" %in% names(.)) .data$Z_CI_Skewness else NA
      ), na.rm = TRUE), z_cap),

      Temporal_Anomaly_Score = pmin(mean(c(
        if ("Z_Daily_Dets_CV" %in% names(.)) .data$Z_Daily_Dets_CV else NA,
        if ("Z_Hourly_Activity_Concentration" %in% names(.)) .data$Z_Hourly_Activity_Concentration else NA,
        if ("Z_Hourly_R_Statistic" %in% names(.)) .data$Z_Hourly_R_Statistic else NA
      ), na.rm = TRUE), z_cap),

      Spatial_Anomaly_Score = pmin(mean(c(
        if ("Z_Plot_Occupancy_Pct" %in% names(.)) .data$Z_Plot_Occupancy_Pct else NA,
        if ("Z_Dets_Per_Plot_CV" %in% names(.)) .data$Z_Dets_Per_Plot_CV else NA,
        if ("Z_Spatial_Aggregation_Index" %in% names(.)) .data$Z_Spatial_Aggregation_Index else NA
      ), na.rm = TRUE), z_cap)
    ) %>%
    dplyr::ungroup()

  anomaly_scores_calculated <- anomaly_scores_calculated %>%
    dplyr::rowwise() %>%
    dplyr::mutate( # Corrected: Using dplyr::mutate
      Overall_Anomaly_Score = mean(c(.data$CI_Anomaly_Score, .data$Temporal_Anomaly_Score, .data$Spatial_Anomaly_Score), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "Scientific_Name",
      "CI_Anomaly_Score",
      "Temporal_Anomaly_Score",
      "Spatial_Anomaly_Score",
      "Overall_Anomaly_Score"
    )
  anomaly_scores_calculated <- anomaly_scores_calculated %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~dplyr::if_else(is.nan(.), NA_real_, .))) # Using dplyr::where

  return(anomaly_scores_calculated)
}
