# File: R/pamPatterns-class.R
# S3 class definitions and methods for pamPatterns objects

#' Create a pam_metrics object
#'
#' Converts a data frame of pattern metrics into a pam_metrics object
#' with special print and summary methods.
#'
#' @param x A data frame containing pattern metrics, typically output from
#'   `calculate_pattern_metrics()` or `flag_species()`
#' @param detection_data Optional. The original detection data used to create
#'   the metrics. Stored as an attribute for use in plotting functions.
#'
#' @return A pam_metrics object (data frame with additional class and attributes)
#' @export
#' @examples
#' \dontrun{
#' metrics_df <- calculate_pattern_metrics(detections_with_id)
#' metrics_obj <- as.pam_metrics(metrics_df, detection_data = detections_with_id)
#' print(metrics_obj)  # Uses custom print method
#' summary(metrics_obj)  # Uses custom summary method
#' }
as.pam_metrics <- function(x, detection_data = NULL) {
  if (!is.data.frame(x)) {
    stop("x must be a data frame")
  }

  # Add class
  class(x) <- c("pam_metrics", class(x))

  # Add attributes
  attr(x, "creation_time") <- Sys.time()
  attr(x, "pamPatterns_version") <- as.character(utils::packageVersion("pamPatterns"))

  if (!is.null(detection_data)) {
    attr(x, "detection_data") <- detection_data
  }

  # Add summary statistics as attributes for quick access
  if ("Total_Detections" %in% names(x)) {
    attr(x, "total_detections") <- sum(x$Total_Detections, na.rm = TRUE)
  }
  if ("Scientific_Name" %in% names(x)) {
    attr(x, "n_species") <- dplyr::n_distinct(x$Scientific_Name, na.rm = TRUE)
  }

  return(x)
}

#' Print method for pam_metrics objects
#' @export
#' @param x A pam_metrics object
#' @param n Number of rows to print (default 10)
#' @param ... Additional arguments passed to print
print.pam_metrics <- function(x, n = 10, ...) {
  cat("PAM Pattern Metrics\n")
  cat("===================\n")
  cat("Created:", format(attr(x, "creation_time"), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Species:", attr(x, "n_species"), "\n")
  cat("Total detections:", attr(x, "total_detections"), "\n")

  if ("Review_Score" %in% names(x)) {
    n_flagged <- sum(x$Review_Score > 0, na.rm = TRUE)
    cat("Flagged species:", n_flagged, "\n")
  }

  cat("\n")

  # Print top n rows
  print_data <- utils::head(x, n)
  class(print_data) <- class(print_data)[class(print_data) != "pam_metrics"]
  print(print_data, ...)

  if (nrow(x) > n) {
    cat("... with", nrow(x) - n, "more rows\n")
  }

  invisible(x)
}

#' Summary method for pam_metrics objects
#' @export
#' @param object A pam_metrics object
#' @param ... Additional arguments (unused)
summary.pam_metrics <- function(object, ...) {
  cat("Summary of PAM Pattern Metrics\n")
  cat("==============================\n")

  # Basic info
  cat("\nDataset Information:\n")
  cat("  Total species:", attr(object, "n_species"), "\n")
  cat("  Total detections:", attr(object, "total_detections"), "\n")

  # Key metric summaries
  cat("\nKey Metrics (median values across species):\n")

  summary_metrics <- c("Median_CI", "Plot_Occupancy_Pct", "Pct_Days_Active",
                       "Hourly_Evenness_J", "Daily_Dets_CV")

  for (metric in summary_metrics) {
    if (metric %in% names(object)) {
      med_val <- median(object[[metric]], na.rm = TRUE)
      cat(sprintf("  %s: %.3f\n", metric, med_val))
    }
  }

  # Flagged species info
  if ("Review_Score" %in% names(object)) {
    cat("\nFlagging Summary:\n")
    flag_cols <- grep("^Flag_", names(object), value = TRUE)
    for (flag in flag_cols) {
      n_flagged <- sum(object[[flag]], na.rm = TRUE)
      cat(sprintf("  %s: %d species\n", flag, n_flagged))
    }

    # Top flagged species
    top_flagged <- object %>%
      dplyr::filter(.data$Review_Score > 0) %>%
      dplyr::arrange(dplyr::desc(.data$Review_Score)) %>%
      dplyr::slice_head(n = 5)

    if (nrow(top_flagged) > 0) {
      cat("\nTop 5 species for review:\n")
      for (i in seq_len(nrow(top_flagged))) {
        cat(sprintf("  %d. %s (Score: %.1f, Detections: %d)\n",
                    i,
                    top_flagged$Scientific_Name[i],
                    top_flagged$Review_Score[i],
                    top_flagged$Total_Detections[i]))
      }
    }
  }

  # Anomaly scores if present
  if ("Overall_Anomaly_Score" %in% names(object)) {
    cat("\nAnomaly Detection:\n")
    high_anomaly <- sum(object$Overall_Anomaly_Score > 3, na.rm = TRUE)
    cat(sprintf("  Species with high anomaly scores (>3): %d\n", high_anomaly))
  }

  invisible(object)
}

#' Plot method for pam_metrics objects
#' @export
#' @param x A pam_metrics object
#' @param type Character. Type of plot to create. See \code{\link{plot_pattern_diagnostics}}
#' @param ... Additional arguments passed to plot_pattern_diagnostics
plot.pam_metrics <- function(x, type = "overview", ...) {
  # Extract detection data if stored as attribute
  detection_data <- attr(x, "detection_data")

  # Remove the pam_metrics class temporarily to avoid recursion
  class(x) <- class(x)[class(x) != "pam_metrics"]

  # Call the main plotting function
  plot_pattern_diagnostics(x, detection_data = detection_data,
                           plot_type = type, ...)
}
