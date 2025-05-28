#' Print Summary of Pattern Metrics
#'
#' Prints a concise summary of the calculated pattern metrics to the console.
#' This includes total species, total detections, top N species by detections,
#' and median values for key metrics.
#'
#' @param metrics_df A `tibble` or `data.frame` with calculated metrics for each
#'   species, typically the output of `calculate_pattern_metrics()` or
#'   `flag_species()`.
#' @param n Integer, the number of top species (by total detections) to display.
#'   Default is 10.
#'
#' @return Invisible `NULL`. This function is called for its side effect of
#'   printing to the console.
#' @export
#' @examples
#' \dontrun{
#' # Assume 'flagged_species_data' is the output from flag_species()
#' # set.seed(123)
#' # n_species_ex <- 15
#' # example_metrics_print <- dplyr::tibble(
#' #   Scientific_Name = paste("Bird", sample(LETTERS, n_species_ex, replace = TRUE),
#' #                           sample(1:100, n_species_ex, replace = TRUE)),
#' #   Total_Detections = sample(1:2000, n_species_ex, replace = TRUE),
#' #   Median_CI = runif(n_species_ex, 0.1, 0.95),
#' #   Plot_Occupancy_Pct = runif(n_species_ex, 0.01, 1.0),
#' #   Daily_Dets_CV = runif(n_species_ex, 0, 3.5),
#' #   Hourly_Activity_Concentration = runif(n_species_ex, 0, 1.0),
#' #   Review_Score = sample(0:10, n_species_ex, replace = TRUE)
#' # ) %>% dplyr::arrange(dplyr::desc(Review_Score), dplyr::desc(Total_Detections))
#' #
#' # print_metrics_summary(example_metrics_print, n = 5)
#' }
print_metrics_summary <- function(metrics_df, n = 10) {

  if (!is.data.frame(metrics_df)) {
    cat("Input is not a data frame. Cannot print summary.\n")
    return(invisible(NULL))
  }
  if (nrow(metrics_df) == 0) {
    cat("=== pamPatterns Metrics Summary ===\n")
    cat("No species data to summarize (metrics data frame is empty).\n")
    return(invisible(NULL))
  }

  cat("=== pamPatterns Metrics Summary ===\n")
  cat(sprintf("Total unique species: %d\n", dplyr::n_distinct(metrics_df$Scientific_Name, na.rm = TRUE)))

  if ("Total_Detections" %in% names(metrics_df)) {
    cat(sprintf("Total detections across all species: %d\n", sum(metrics_df$Total_Detections, na.rm = TRUE)))
  } else {
    cat("Column 'Total_Detections' not found in metrics_df.\n")
  }
  cat("\n")

  # Display top N species by Total_Detections
  if ("Total_Detections" %in% names(metrics_df) && "Scientific_Name" %in% names(metrics_df)) {
    cat(sprintf("Top %d species by total detections:\n", min(n, nrow(metrics_df))))

    cols_to_show <- c("Scientific_Name", "Total_Detections", "Median_CI", "Plot_Occupancy_Pct")
    if ("Review_Score" %in% names(metrics_df)) { # Include Review_Score if available
      cols_to_show <- c(cols_to_show, "Review_Score")
    }

    # Ensure we only select existing columns to avoid errors
    cols_to_show_existing <- intersect(cols_to_show, names(metrics_df))

    if (length(cols_to_show_existing) > 1) { # Need at least Scientific_Name + one metric
      top_species_df <- metrics_df %>%
        dplyr::arrange(dplyr::desc(.data$Total_Detections)) %>%
        dplyr::slice_head(n = min(n, nrow(metrics_df))) %>%
        dplyr::select(dplyr::all_of(cols_to_show_existing))
      print(as.data.frame(top_species_df)) # as.data.frame for cleaner console printing
    } else {
      cat("Not enough relevant columns to display top species (need Scientific_Name and Total_Detections at least).\n")
    }

  } else {
    cat("Cannot display top species: 'Scientific_Name' or 'Total_Detections' column missing.\n")
  }

  # Summary statistics for key metrics (Median of medians, etc.)
  cat("\n=== Overall Metric Distributions (Medians) ===\n")
  summary_cols <- c(
    "Median_CI", "Plot_Occupancy_Pct", "Daily_Dets_CV",
    "Hourly_Activity_Concentration", "Review_Score" # Add Review_Score here too
  )
  # Filter to only columns that actually exist in metrics_df
  summary_cols_existing <- intersect(summary_cols, names(metrics_df))

  if (length(summary_cols_existing) > 0) {
    summary_stats_list <- lapply(summary_cols_existing, function(col_name) {
      # Calculate median only if there are non-NA values
      valid_data <- metrics_df[[col_name]][!is.na(metrics_df[[col_name]])]
      if (length(valid_data) > 0) {
        stats::median(valid_data, na.rm = TRUE)
      } else {
        NA_real_
      }
    })
    names(summary_stats_list) <- paste0(summary_cols_existing, " (median)")

    # Convert list to a data frame for printing
    summary_df <- as.data.frame(summary_stats_list)
    # Transpose for better readability in console
    print(t(summary_df), quote = FALSE)
  } else {
    cat("No key metric columns found for overall distribution summary.\n")
  }
  cat("--------------------------------------\n")

  invisible(NULL)
}


#' Export Metrics to CSV File
#'
#' Writes the calculated (and optionally flagged) metrics data frame to a CSV file.
#' Can optionally include metadata lines at the beginning of the CSV file,
#' commented out with '#'.
#'
#' @param metrics_df A `tibble` or `data.frame` containing the metrics to export.
#' @param file_path Character string: The desired path for the output CSV file.
#'   The directory will be created if it does not exist.
#' @param include_metadata Logical. If `TRUE` (default), adds metadata lines
#'   (e.g., generation date, package version) as comments at the top of the CSV.
#'
#' @return Invisibly returns the `file_path` of the exported file.
#'   Called for its side effect of writing a file.
#' @export
#' @importFrom utils packageVersion
#' @examples
#' \dontrun{
#' # Assume 'flagged_species_data' is the output from flag_species()
#' # output_file <- "my_pam_patterns_results.csv"
#' # export_metrics(flagged_species_data, output_file)
#' # message("Metrics exported to: ", output_file)
#' #
#' # # Export without metadata
#' # export_metrics(flagged_species_data, "results_no_meta.csv", include_metadata = FALSE)
#' }
export_metrics <- function(metrics_df, file_path, include_metadata = TRUE) {

  if (!is.data.frame(metrics_df)) {
    stop("'metrics_df' must be a data frame or tibble.")
  }
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("'file_path' must be a single character string.")
  }

  # Create directory if it doesn't exist
  output_dir <- dirname(file_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (include_metadata) {
    metadata_lines <- c(
      paste0("# pamPatterns Metrics Export"),
      paste0("# Generated: ", Sys.time()),
      paste0("# Package: pamPatterns, Version: ", tryCatch(as.character(utils::packageVersion("pamPatterns")), error = function(e) "unknown")), # Safer way to get version
      # Conditional metadata for species count
      if ("Scientific_Name" %in% names(metrics_df)) {
        paste0("# Number of unique species: ", dplyr::n_distinct(metrics_df$Scientific_Name, na.rm = TRUE))
      } else {
        paste0("# Number of rows in dataset: ", nrow(metrics_df)) # Fallback if no Scientific_Name
      },
      # Conditional metadata for total detections
      if ("Total_Detections" %in% names(metrics_df)) {
        paste0("# Total detections in dataset: ", sum(metrics_df$Total_Detections, na.rm = TRUE))
      } else {
        NULL # Add nothing if Total_Detections is not present
      },
      "" # Empty line before data
    )
    # Remove any NULL elements that might have been introduced by the conditional logic
    metadata_lines <- metadata_lines[!sapply(metadata_lines, is.null)]
    # Remove empty strings from metadata_lines if any resulted from missing columns
    metadata_lines <- metadata_lines[metadata_lines != "" | sapply(metadata_lines, Negate(is.null))]


    # Write metadata lines first
    tryCatch({
      writeLines(metadata_lines, file_path)
      # Append the data frame below the metadata
      readr::write_csv(metrics_df, file_path, append = TRUE, na = "NA")
    }, error = function(e) {
      stop("Failed to write metrics to CSV with metadata: ", file_path, "\nOriginal error: ", e$message)
    })

  } else {
    # Write data frame directly without metadata
    tryCatch({
      readr::write_csv(metrics_df, file_path, na = "NA")
    }, error = function(e) {
      stop("Failed to write metrics to CSV: ", file_path, "\nOriginal error: ", e$message)
    })
  }

  # message("Metrics successfully exported to: ", file_path) # Optional success message
  invisible(file_path)
}
