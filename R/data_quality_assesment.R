#' Calculate Data Quality Indicators for PAM Dataset
#'
#' Assesses the quality and completeness of a passive acoustic monitoring dataset
#' by calculating various coverage, consistency, and reliability metrics.
#'
#' @param detection_data A `tibble` or `data.frame` containing processed detection data,
#'   typically the output of `read_birdnet_data()` with `AudioMoth_ID_Numeric` added.
#' @param expected_n_recorders Integer. The expected number of recording devices/plots
#'   in the study. Used to calculate spatial coverage. If NULL (default), uses the
#'   number of unique recorder IDs found in the data.
#' @param expected_hours_per_day Numeric. The expected number of recording hours per day
#'   (e.g., 24 for continuous recording, 12 for daylight-only). Default is 24.
#' @param min_detections_threshold Integer. Minimum number of detections for a species
#'   to be considered "well-sampled". Default is 30.
#'
#' @return A list with two components:
#'   \itemize{
#'     \item `overall_metrics`: A single-row tibble with dataset-wide quality metrics
#'     \item `species_quality`: A tibble with quality indicators for each species
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'detections_with_id' is your processed detection data
#' quality_report <- calculate_data_quality(detections_with_id,
#'                                          expected_n_recorders = 10,
#'                                          expected_hours_per_day = 24)
#'
#' # View overall quality metrics
#' print(quality_report$overall_metrics)
#'
#' # View species-specific quality
#' print(quality_report$species_quality)
#' }
calculate_data_quality <- function(detection_data,
                                   expected_n_recorders = NULL,
                                   expected_hours_per_day = 24,
                                   min_detections_threshold = 30) {

  # Input validation
  if (!is.data.frame(detection_data)) {
    stop("'detection_data' must be a data frame or tibble.")
  }

  if (nrow(detection_data) == 0) {
    warning("Input data is empty. Returning NA quality metrics.")
    return(list(
      overall_metrics = dplyr::tibble(
        Total_Detections = 0L,
        N_Species = 0L,
        Date_Range_Days = NA_integer_,
        Temporal_Coverage = NA_real_,
        Spatial_Coverage = NA_real_,
        Hourly_Coverage = NA_real_,
        Mean_Daily_Detections = NA_real_,
        Detection_Regularity = NA_real_,
        Species_Accumulation_Rate = NA_real_,
        Data_Consistency_Score = NA_real_
      ),
      species_quality = dplyr::tibble()
    ))
  }

  # Set expected recorders if not provided
  if (is.null(expected_n_recorders)) {
    expected_n_recorders <- dplyr::n_distinct(detection_data$AudioMoth_ID_Numeric, na.rm = TRUE)
    if (expected_n_recorders == 0) expected_n_recorders <- 1
  }

  # Calculate date range
  date_range <- range(detection_data$Detection_Date, na.rm = TRUE)
  total_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days")) + 1

  # Overall metrics
  overall_metrics <- dplyr::tibble(
    Total_Detections = nrow(detection_data),
    N_Species = dplyr::n_distinct(detection_data$Scientific_Name, na.rm = TRUE),
    Date_Range_Days = total_days,

    # Temporal coverage: proportion of days with detections
    Temporal_Coverage = dplyr::n_distinct(detection_data$Detection_Date, na.rm = TRUE) / total_days,

    # Spatial coverage: proportion of recorders with detections
    Spatial_Coverage = dplyr::n_distinct(detection_data$AudioMoth_ID_Numeric, na.rm = TRUE) / expected_n_recorders,

    # Hourly coverage: proportion of possible hours with detections
    Hourly_Coverage = dplyr::n_distinct(detection_data$Detection_Hour, na.rm = TRUE) / expected_hours_per_day,

    # Mean daily detection rate
    Mean_Daily_Detections = nrow(detection_data) / total_days,

    # Detection regularity (using CV of daily counts)
    Detection_Regularity = calculate_detection_regularity(detection_data),

    # Species accumulation rate
    Species_Accumulation_Rate = calculate_species_accumulation_rate(detection_data),

    # Overall consistency score (composite metric)
    Data_Consistency_Score = NA_real_  # Calculated below
  )

  # Calculate consistency score as weighted average of components
  overall_metrics$Data_Consistency_Score <- with(overall_metrics,
                                                 0.3 * Temporal_Coverage +
                                                   0.3 * Spatial_Coverage +
                                                   0.2 * Hourly_Coverage +
                                                   0.2 * (1 - pmin(Detection_Regularity, 1))  # Lower CV is better
  )

  # Species-level quality metrics
  species_quality <- detection_data %>%
    dplyr::group_by(.data$Scientific_Name) %>%
    dplyr::summarise(
      N_Detections = dplyr::n(),
      N_Days_Present = dplyr::n_distinct(.data$Detection_Date, na.rm = TRUE),
      N_Recorders_Present = dplyr::n_distinct(.data$AudioMoth_ID_Numeric, na.rm = TRUE),
      Mean_Confidence = mean(.data$Confidence, na.rm = TRUE),
      Min_Date = min(.data$Detection_Date, na.rm = TRUE),
      Max_Date = max(.data$Detection_Date, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      Days_Span = as.numeric(difftime(.data$Max_Date, .data$Min_Date, units = "days")) + 1,
      Temporal_Consistency = .data$N_Days_Present / .data$Days_Span,
      Spatial_Consistency = .data$N_Recorders_Present / expected_n_recorders,
      Is_Well_Sampled = .data$N_Detections >= min_detections_threshold,

      # Quality score for each species
      Species_Quality_Score = (
        0.3 * pmin(.data$Temporal_Consistency, 1) +
          0.3 * .data$Spatial_Consistency +
          0.2 * .data$Mean_Confidence +
          0.2 * pmin(.data$N_Detections / 100, 1)  # Normalized by 100 detections
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Species_Quality_Score))

  return(list(
    overall_metrics = overall_metrics,
    species_quality = species_quality
  ))
}


#' Calculate detection regularity (CV of daily detection counts)
#' @keywords internal
calculate_detection_regularity <- function(detection_data) {
  daily_counts <- detection_data %>%
    dplyr::count(.data$Detection_Date) %>%
    dplyr::pull(.data$n)

  if (length(daily_counts) < 2) return(NA_real_)

  cv <- sd(daily_counts, na.rm = TRUE) / mean(daily_counts, na.rm = TRUE)
  return(cv)
}


#' Calculate species accumulation rate
#' @keywords internal
calculate_species_accumulation_rate <- function(detection_data) {
  # Order detections by date and calculate cumulative species count
  species_accumulation <- detection_data %>%
    dplyr::arrange(.data$Detection_Date) %>%
    dplyr::mutate(
      Date_Index = as.numeric(.data$Detection_Date - min(.data$Detection_Date, na.rm = TRUE)) + 1
    ) %>%
    dplyr::group_by(.data$Date_Index) %>%
    dplyr::summarise(
      Species_This_Day = dplyr::n_distinct(.data$Scientific_Name),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(.data$Date_Index) %>%
    dplyr::mutate(
      Cumulative_Species = cumsum(!duplicated(cumsum(.data$Species_This_Day)))
    )

  if (nrow(species_accumulation) < 2) return(NA_real_)

  # Calculate rate as slope of log-transformed accumulation curve
  # (assumes power law relationship)
  tryCatch({
    log_days <- log(species_accumulation$Date_Index)
    log_species <- log(species_accumulation$Cumulative_Species)

    # Remove -Inf values from log(0)
    valid_indices <- is.finite(log_days) & is.finite(log_species)
    if (sum(valid_indices) < 2) return(NA_real_)

    lm_fit <- lm(log_species[valid_indices] ~ log_days[valid_indices])
    return(coef(lm_fit)[2])  # Return slope
  }, error = function(e) {
    return(NA_real_)
  })
}


#' Generate Data Quality Report
#'
#' Creates a formatted text report summarizing the data quality assessment results.
#'
#' @param quality_assessment Output from `calculate_data_quality()`
#' @param output_file Optional. Path to save the report as a text file.
#'   If NULL (default), the report is only printed to console.
#'
#' @return Invisibly returns the report as a character vector (one element per line)
#' @export
#' @examples
#' \dontrun{
#' quality <- calculate_data_quality(detections_with_id, expected_n_recorders = 10)
#'
#' # Print to console
#' generate_quality_report(quality)
#'
#' # Save to file
#' generate_quality_report(quality, output_file = "data_quality_report.txt")
#' }
generate_quality_report <- function(quality_assessment, output_file = NULL) {

  if (!is.list(quality_assessment) ||
      !all(c("overall_metrics", "species_quality") %in% names(quality_assessment))) {
    stop("Input must be output from calculate_data_quality()")
  }

  overall <- quality_assessment$overall_metrics
  species <- quality_assessment$species_quality

  # Build report
  report_lines <- c(
    "=====================================",
    "PAM Data Quality Assessment Report",
    "=====================================",
    paste("Generated:", Sys.time()),
    "",
    "OVERALL DATASET METRICS",
    "-----------------------",
    sprintf("Total Detections: %d", overall$Total_Detections),
    sprintf("Number of Species: %d", overall$N_Species),
    sprintf("Date Range: %d days", overall$Date_Range_Days),
    sprintf("Temporal Coverage: %.1f%%", overall$Temporal_Coverage * 100),
    sprintf("Spatial Coverage: %.1f%%", overall$Spatial_Coverage * 100),
    sprintf("Hourly Coverage: %.1f%%", overall$Hourly_Coverage * 100),
    sprintf("Mean Daily Detections: %.1f", overall$Mean_Daily_Detections),
    sprintf("Detection Regularity (CV): %.2f", overall$Detection_Regularity),
    sprintf("Species Accumulation Rate: %.2f", overall$Species_Accumulation_Rate),
    sprintf("Overall Data Consistency Score: %.2f", overall$Data_Consistency_Score),
    "",
    "SPECIES QUALITY SUMMARY",
    "-----------------------",
    sprintf("Well-sampled species (>=%d detections): %d",
            30,  # Using default threshold
            sum(species$Is_Well_Sampled, na.rm = TRUE)),
    sprintf("Poorly-sampled species: %d",
            sum(!species$Is_Well_Sampled, na.rm = TRUE)),
    "",
    "TOP 10 HIGHEST QUALITY SPECIES",
    "-------------------------------"
  )

  # Add top species
  top_species <- utils::head(species, 10)
  for (i in seq_len(nrow(top_species))) {
    report_lines <- c(report_lines,
                      sprintf("%d. %s (Score: %.2f, N=%d, Days=%d, Recorders=%d)",
                              i,
                              top_species$Scientific_Name[i],
                              top_species$Species_Quality_Score[i],
                              top_species$N_Detections[i],
                              top_species$N_Days_Present[i],
                              top_species$N_Recorders_Present[i])
    )
  }

  # Add warnings/recommendations
  report_lines <- c(report_lines,
                    "",
                    "WARNINGS & RECOMMENDATIONS",
                    "--------------------------"
  )

  # Check for issues and add recommendations
  if (overall$Temporal_Coverage < 0.8) {
    report_lines <- c(report_lines,
                      "- Low temporal coverage: Consider checking for missing recording days")
  }

  if (overall$Spatial_Coverage < 0.8) {
    report_lines <- c(report_lines,
                      "- Low spatial coverage: Some recorders may have failed or have no detections")
  }

  if (overall$Detection_Regularity > 2) {
    report_lines <- c(report_lines,
                      "- High variability in daily detections: May indicate inconsistent recording effort")
  }

  if (overall$Species_Accumulation_Rate > 0.5) {
    report_lines <- c(report_lines,
                      "- High species accumulation rate: Consider extending survey duration")
  }

  n_rare_species <- sum(species$N_Detections < 10, na.rm = TRUE)
  if (n_rare_species > 0) {
    report_lines <- c(report_lines,
                      sprintf("- %d species with <10 detections: Review for potential false positives", n_rare_species))
  }

  # Footer
  report_lines <- c(report_lines,
                    "",
                    "=====================================",
                    "End of Report"
  )

  # Print to console
  cat(report_lines, sep = "\n")

  # Save to file if requested
  if (!is.null(output_file)) {
    writeLines(report_lines, output_file)
    message("\nReport saved to: ", output_file)
  }

  invisible(report_lines)
}
