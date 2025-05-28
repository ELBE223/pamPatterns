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
      paste0("# Package: pamPatterns, Version: ", tryCatch(as.character(utils::packageVersion("pamPatterns")), error = function(e) "unknown")),
      # Conditional metadata for species count
      if ("Scientific_Name" %in% names(metrics_df)) {
        paste0("# Number of unique species: ", dplyr::n_distinct(metrics_df$Scientific_Name, na.rm = TRUE))
      } else {
        paste0("# Number of rows in dataset: ", nrow(metrics_df))
      },
      # Conditional metadata for total detections
      if ("Total_Detections" %in% names(metrics_df)) {
        paste0("# Total detections in dataset: ", sum(metrics_df$Total_Detections, na.rm = TRUE))
      } else {
        NULL
      }
    )
    # Remove any NULL elements
    metadata_lines <- metadata_lines[!sapply(metadata_lines, is.null)]

    # Write metadata lines first
    tryCatch({
      writeLines(metadata_lines, file_path)
      # Then append the data frame WITH HEADER
      # Important: col_names = TRUE to include column names!
      readr::write_csv(metrics_df, file_path, append = TRUE, na = "NA", col_names = TRUE)
    }, error = function(e) {
      stop("Failed to write metrics to CSV with metadata: ", file_path, "\nOriginal error: ", e$message)
    })

  } else {
    # Write data frame directly without metadata
    tryCatch({
      readr::write_csv(metrics_df, file_path, na = "NA", col_names = TRUE)
    }, error = function(e) {
      stop("Failed to write metrics to CSV: ", file_path, "\nOriginal error: ", e$message)
    })
  }

  invisible(file_path)
}
