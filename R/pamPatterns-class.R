# File: R/pamPatterns-class.R
# S3 class definitions and methods for pamPatterns objects

# Note: The '@importFrom stats var lm coef' line has been moved to
# the package-level documentation file (e.g., R/pamPatterns-package.R)

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
#' # Assuming 'detections_with_id' is your processed detection data
#' metrics_df <- calculate_pattern_metrics(detections_with_id)
#' metrics_obj <- as.pam_metrics(metrics_df, detection_data = detections_with_id)
#' print(metrics_obj)  # Uses custom print method
#' summary(metrics_obj)  # Uses custom summary method
#' plot(metrics_obj, type = "overview") # Uses custom plot method
#' }
as.pam_metrics <- function(x, detection_data = NULL) {
  if (!is.data.frame(x)) {
    stop("Input 'x' must be a data frame.")
  }

  # Add class
  current_class <- class(x)
  if (!"pam_metrics" %in% current_class) {
    class(x) <- c("pam_metrics", current_class)
  }


  # Add attributes
  attr(x, "creation_time") <- Sys.time()
  # Ensure pamPatterns is loaded to get version, or handle if not (e.g. during tests)
  if (requireNamespace("pamPatterns", quietly = TRUE)) {
    attr(x, "pamPatterns_version") <- as.character(utils::packageVersion("pamPatterns"))
  } else {
    attr(x, "pamPatterns_version") <- "unknown (pamPatterns not fully loaded)"
  }


  if (!is.null(detection_data)) {
    if (!is.data.frame(detection_data)) {
      warning("'detection_data' provided is not a data frame and will not be stored as an attribute.")
    } else {
      attr(x, "detection_data") <- detection_data
    }
  }

  # Add summary statistics as attributes for quick access
  if ("Total_Detections" %in% names(x) && sum(!is.na(x$Total_Detections)) > 0) {
    attr(x, "total_detections_in_metrics") <- sum(as.numeric(x$Total_Detections), na.rm = TRUE)
  } else if ("Total_Detections_Species" %in% names(x) && sum(!is.na(x$Total_Detections_Species)) > 0) { # Common alternative name
    attr(x, "total_detections_in_metrics") <- sum(as.numeric(x$Total_Detections_Species), na.rm = TRUE)
  }


  if ("Scientific_Name" %in% names(x)) {
    attr(x, "n_species_in_metrics") <- dplyr::n_distinct(x$Scientific_Name, na.rm = TRUE)
  }

  return(x)
}

#' Print method for pam_metrics objects
#' @param x A pam_metrics object
#' @param n Integer, number of rows to print from the head of the data frame.
#'          Default is 10. Use `NULL` or `Inf` to print all rows.
#' @param ... Additional arguments passed to the underlying print method for data frames.
#' @export
print.pam_metrics <- function(x, n = 10, ...) {
  cat("PAM Pattern Metrics Object\n")
  cat("==========================\n")
  if (!is.null(attr(x, "creation_time"))) {
    cat("  Created:", format(attr(x, "creation_time"), "%Y-%m-%d %H:%M:%S"), "\n")
  }
  if (!is.null(attr(x, "n_species_in_metrics"))) {
    cat("  Species in metrics table:", attr(x, "n_species_in_metrics"), "\n")
  }
  if (!is.null(attr(x, "total_detections_in_metrics"))) {
    cat("  Total detections represented:", format(attr(x, "total_detections_in_metrics"), big.mark = ","), "\n")
  }

  if ("Review_Score" %in% names(x) && sum(!is.na(x$Review_Score)) > 0) {
    n_flagged <- sum(x$Review_Score > 0, na.rm = TRUE)
    cat("  Flagged species (Review_Score > 0):", n_flagged, "\n")
  }
  cat("\n")

  # Prepare data for printing (remove S3 class to use default data.frame print)
  print_data <- x
  class(print_data) <- setdiff(class(print_data), "pam_metrics")

  if (is.null(n) || is.infinite(n) || n >= nrow(print_data)) {
    print(print_data, ...)
  } else {
    print(utils::head(print_data, n), ...)
    if (nrow(x) > n) {
      cat("... (omitted", nrow(x) - n, "more species)\n")
    }
  }
  invisible(x)
}

#' Summary method for pam_metrics objects
#' @param object A pam_metrics object
#' @param top_n_flagged Integer, number of top flagged species to list. Default 5.
#' @param ... Additional arguments (currently unused).
#' @export
summary.pam_metrics <- function(object, top_n_flagged = 5, ...) {
  cat("Summary of PAM Pattern Metrics\n")
  cat("==============================\n")

  # Basic info from attributes
  cat("\nDataset Information (from attributes):\n")
  n_species_attr <- attr(object, "n_species_in_metrics")
  total_dets_attr <- attr(object, "total_detections_in_metrics")

  cat(sprintf("  Number of species in this metrics table: %s\n",
              if (is.null(n_species_attr)) "N/A" else n_species_attr))
  cat(sprintf("  Total detections represented in this table: %s\n",
              if (is.null(total_dets_attr)) "N/A" else format(total_dets_attr, big.mark = ",")))

  # Key metric summaries (calculated from the data frame)
  cat("\nKey Metrics (median values across species):\n")
  key_metrics_to_summarize <- c(
    "Total_Detections", "Median_CI", "Plot_Occupancy_Pct",
    "Pct_Days_Active", "Daily_Dets_CV", "Hourly_Evenness_J",
    "Hourly_Activity_Concentration", "Overall_Anomaly_Score"
  )

  for (metric_col in key_metrics_to_summarize) {
    if (metric_col %in% names(object) && is.numeric(object[[metric_col]])) {
      median_val <- stats::median(object[[metric_col]], na.rm = TRUE)
      cat(sprintf("  Median %s: %.3f\n", gsub("_", " ", metric_col), median_val))
    }
  }

  # Flagging summary
  if ("Review_Score" %in% names(object) && sum(!is.na(object$Review_Score)) > 0 ) {
    cat("\nFlagging Summary:\n")
    flag_cols <- grep("^Flag_", names(object), value = TRUE)
    flag_cols <- flag_cols[sapply(object[flag_cols], is.logical) | sapply(object[flag_cols], is.numeric)] # Only logical/numeric flags

    for (flag in flag_cols) {
      if (sum(!is.na(object[[flag]])) > 0) { # Ensure there are non-NA values
        n_flagged_species <- sum(object[[flag]] == TRUE | object[[flag]] > 0, na.rm = TRUE) # Handle logical or numeric flags
        cat(sprintf("  Species with %s: %d\n", gsub("Flag_", "", flag), n_flagged_species))
      }
    }

    # Top flagged species by Review_Score
    if (top_n_flagged > 0 && sum(object$Review_Score > 0, na.rm = TRUE) > 0) {
      top_species_for_review <- object %>%
        dplyr::filter(.data$Review_Score > 0) %>%
        dplyr::arrange(dplyr::desc(.data$Review_Score), dplyr::desc(.data$Total_Detections)) %>%
        dplyr::slice_head(n = top_n_flagged) %>%
        dplyr::select(dplyr::any_of(c("Scientific_Name", "Common_Name", "Review_Score", "Total_Detections")))

      if (nrow(top_species_for_review) > 0) {
        cat(sprintf("\nTop %d species for review (by Review_Score):\n", nrow(top_species_for_review)))
        for (i in seq_len(nrow(top_species_for_review))) {
          species_id <- if ("Common_Name" %in% names(top_species_for_review) && !is.na(top_species_for_review$Common_Name[i])) {
            paste0(top_species_for_review$Scientific_Name[i], " (", top_species_for_review$Common_Name[i], ")")
          } else {
            top_species_for_review$Scientific_Name[i]
          }
          cat(sprintf("  %d. %s (Score: %.2f, Detections: %s)\n",
                      i,
                      species_id,
                      top_species_for_review$Review_Score[i],
                      format(top_species_for_review$Total_Detections[i], big.mark = ",")))
        }
      }
    }
  }
  invisible(object)
}


#' Plot method for pam_metrics objects
#'
#' This is an S3 method that dispatches to \code{\link{plot_pattern_diagnostics}}.
#' It attempts to retrieve the original detection data if it was stored as an
#' attribute in the `pam_metrics` object.
#'
#' @param x A `pam_metrics` object.
#' @param type Character string or vector specifying the type(s) of plot(s) to generate.
#'             Passed to `plot_type` in \code{\link{plot_pattern_diagnostics}}.
#'             See \code{\link{plot_pattern_diagnostics}} for available types.
#'             Default is "overview".
#' @param species_name Optional character string, the scientific name of a single
#'             species for which to generate detailed plots. If NULL (default),
#'             overview plots are typically generated.
#' @param ... Additional arguments passed to \code{\link{plot_pattern_diagnostics}}.
#'
#' @return A ggplot object or a list of ggplot objects, invisibly.
#' @export
#' @seealso \code{\link{plot_pattern_diagnostics}}
plot.pam_metrics <- function(x, type = "overview", species_name = NULL, ...) {
  # Extract detection data if stored as an attribute
  detection_data_attr <- attr(x, "detection_data")

  # Temporarily remove the pam_metrics class to avoid S3 dispatch recursion
  # if plot_pattern_diagnostics itself tries to call plot() on x.
  metrics_df <- x
  class(metrics_df) <- setdiff(class(metrics_df), "pam_metrics")

  # Call the main plotting function, passing the extracted detection data if available
  # and other arguments.
  if (!is.null(species_name) && !is.null(detection_data_attr)) {
    # If a specific species is requested and detection data is available,
    # plot_pattern_diagnostics might use it.
    plot_output <- plot_pattern_diagnostics(metrics_df = metrics_df,
                                            detection_data = detection_data_attr,
                                            plot_type = type,
                                            species_name = species_name,
                                            ...)
  } else {
    # For overview plots or if detection_data is not available/needed for the type.
    plot_output <- plot_pattern_diagnostics(metrics_df = metrics_df,
                                            plot_type = type,
                                            species_name = species_name,
                                            detection_data = detection_data_attr, # Pass it anyway
                                            ...)
  }
  # The plot_pattern_diagnostics function should handle printing the plot(s).
  # Return the plot object(s) invisibly, consistent with base R plot methods.
  invisible(plot_output)
}
