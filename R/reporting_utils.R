# File: R/reporting_utils.R

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
#' @importFrom dplyr arrange slice_head select all_of n_distinct desc
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
      valid_data <- metrics_df[[col_name]][!is.na(metrics_df[[col_name]])]
      if (length(valid_data) > 0) {
        stats::median(valid_data, na.rm = TRUE)
      } else {
        NA_real_
      }
    })
    names(summary_stats_list) <- paste0(summary_cols_existing, " (median)")

    summary_df <- as.data.frame(summary_stats_list)
    print(t(summary_df), quote = FALSE)
  } else {
    cat("No key metric columns found for overall distribution summary.\n")
  }
  cat("--------------------------------------\n")

  invisible(NULL)
}


#' Export Metrics to CSV File (Super Manual Control - Take 7)
#'
#' Writes the calculated (and optionally flagged) metrics data frame to a CSV file.
#' Can optionally include metadata lines at the beginning of the CSV file,
#' commented out with '#'.
#'
#' @param metrics_df A `tibble` or `data.frame` containing the metrics to export.
#' @param file_path Character string: The desired path for the output CSV file.
#'   The directory will be created if it does not exist.
#' @param include_metadata Logical. If `TRUE` (default), adds metadata lines.
#'
#' @return Invisibly returns the `file_path` of the exported file.
#' @export
#' @importFrom utils packageVersion
#' @importFrom dplyr n_distinct
#' @importFrom readr write_csv
#'
#' @examples
#' \dontrun{
#' temp_dir <- tempdir()
#' example_metrics_df <- dplyr::tibble(
#'   Scientific_Name = "Sylvia atricapilla",
#'   Total_Detections = 100L,
#'   Median_CI = 0.5,
#'   Plot_Occupancy_Pct = 0.1,
#'   Daily_Dets_CV = 1.2,
#'   Hourly_Activity_Concentration = 0.3,
#'   Notes = "A species, with notes"
#' )
#'
#' # Test with metadata
#' output_file_meta <- file.path(temp_dir, "my_pam_patterns_results_meta_take7.csv")
#' export_metrics(example_metrics_df, output_file_meta, include_metadata = TRUE)
#' print(paste("Metadata file written to:", output_file_meta))
#' print(readLines(output_file_meta, n = 10))
#'
#' # Reloading this file:
#' reloaded_meta_readr <- readr::read_csv(output_file_meta, comment = "#", show_col_types = FALSE)
#' print("Reloaded with readr:")
#' print(head(reloaded_meta_readr))
#' print(dim(reloaded_meta_readr))
#'
#' reloaded_meta_fread <- data.table::fread(output_file_meta) # fread skips comments
#' print("Reloaded with fread:")
#' print(head(reloaded_meta_fread))
#' print(dim(reloaded_meta_fread))
#'
#' # Test without metadata
#' output_file_no_meta <- file.path(temp_dir, "my_pam_patterns_results_no_meta_take7.csv")
#' export_metrics(example_metrics_df, output_file_no_meta, include_metadata = FALSE)
#' print(paste("No-metadata file written to:", output_file_no_meta))
#' print(readLines(output_file_no_meta, n = 10))
#' reloaded_no_meta_readr <- readr::read_csv(output_file_no_meta, show_col_types = FALSE)
#' print(head(reloaded_no_meta_readr))
#'
#' unlink(c(output_file_meta, output_file_no_meta)) # Clean up
#' }
export_metrics <- function(metrics_df, file_path, include_metadata = TRUE) {

  if (!is.data.frame(metrics_df)) {
    stop("'metrics_df' must be a data frame or tibble.")
  }
  # No nrow(metrics_df) == 0 check here, as we handle it by writing header only if empty.
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("'file_path' must be a single character string.")
  }

  output_dir <- dirname(file_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (include_metadata) {
    # Open file connection for writing. This creates/truncates the file.
    file_conn <- file(file_path, "wt")
    on.exit(close(file_conn), add = TRUE) # Ensure connection is closed

    pkg_version <- tryCatch(
      as.character(utils::packageVersion("pamPatterns")), # Ensure your package name here
      error = function(e) "unknown"
    )
    comment_lines <- c(
      paste0("# pamPatterns Metrics Export"),
      paste0("# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      paste0("# Package: pamPatterns, Version: ", pkg_version)
    )
    if (nrow(metrics_df) > 0 && "Scientific_Name" %in% names(metrics_df)) {
      comment_lines <- c(comment_lines, paste0("# Number of unique species: ", dplyr::n_distinct(metrics_df$Scientific_Name, na.rm = TRUE)))
    } else if (nrow(metrics_df) > 0) {
      comment_lines <- c(comment_lines, paste0("# Number of rows in dataset: ", nrow(metrics_df)))
    } else {
      comment_lines <- c(comment_lines, "# Dataset is empty.")
    }
    if (nrow(metrics_df) > 0 && "Total_Detections" %in% names(metrics_df) && is.numeric(metrics_df$Total_Detections)) {
      comment_lines <- c(comment_lines, paste0("# Total detections in dataset: ", sum(metrics_df$Total_Detections, na.rm = TRUE)))
    }

    # Write comment lines to the connection
    for (line in comment_lines) {
      writeLines(line, con = file_conn)
    }

    # Write the actual CSV header row (not commented) to the connection
    # Ensure metrics_df has column names, even if empty
    if (is.null(colnames(metrics_df)) && ncol(metrics_df) > 0) {
      header_row_string <- paste(paste0("V", 1:ncol(metrics_df)), collapse=",")
    } else if (ncol(metrics_df) == 0) {
      header_row_string <- "" # Empty header for empty df with 0 cols
    } else {
      header_row_string <- paste(colnames(metrics_df), collapse = ",")
    }
    writeLines(header_row_string, con = file_conn)

    # Write data rows if any, using readr::write_csv to a temp file for robust formatting
    if (nrow(metrics_df) > 0) {
      temp_data_file <- tempfile(fileext = ".csvdata")
      on.exit(unlink(temp_data_file, force = TRUE), add = TRUE)

      # Write metrics_df to temp file WITH header (to get data rows correctly formatted by readr)
      readr::write_csv(metrics_df, temp_data_file, na = "NA", col_names = TRUE)

      # Read the lines from the temp file, skip its header, and write to our main connection
      data_lines_from_temp <- readLines(temp_data_file)
      if(length(data_lines_from_temp) > 1) { # If there's more than just the header
        for (i in 2:length(data_lines_from_temp)) { # Start from 2 to skip header
          writeLines(data_lines_from_temp[i], con = file_conn)
        }
      }
    }
    # File connection closed by on.exit()
  } else {
    # If no metadata, just write a standard CSV using readr::write_csv
    tryCatch({
      readr::write_csv(metrics_df, file_path, na = "NA", col_names = TRUE)
    }, error = function(e) {
      stop("Failed to write metrics to CSV (no metadata): ", file_path, "\nOriginal error: ", e$message)
    })
  }
  invisible(file_path)
}
