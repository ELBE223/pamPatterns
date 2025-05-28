#' Read and Standardize BirdNET Detection Data
#'
#' This function reads a BirdNET results CSV file (or a similarly structured file
#' from other classifiers if column names are adjusted), standardizes essential
#' column names, extracts date-time information from filenames, and applies
#' initial filters for species and confidence.
#'
#' @param file_path Character string: The full path to the BirdNET CSV file (or compatible format).
#' @param species_list Optional. A character vector of scientific names. If provided,
#'   only detections of these species will be kept. Default is `NULL` (all species).
#' @param min_confidence Numeric. The minimum confidence score for a detection to be
#'   kept. Detections below this threshold are discarded. Default is `0.01`.
#' @param time_range Optional. A vector of two POSIXct objects specifying the
#'   start and end times for filtering detections. Default is `NULL` (no time filtering).
#' @param hour_range Optional. A numeric vector of two integers (0-23) specifying
#'   the hour range for filtering detections. Default is `NULL` (all hours).
#' @param exclude_weekends Logical. If `TRUE`, excludes detections from weekends
#'   (Saturday and Sunday). Default is `FALSE`.
#' @param custom_filter Optional. A function that takes the detection data frame
#'   as input and returns a filtered data frame. Default is `NULL`.
#' @param use_data_table Logical. If `TRUE` and the data.table package is available,
#'   uses data.table::fread for potentially faster reading of large files. Default is `FALSE`.
#'
#' @return A `tibble` (data frame) with processed detection data. Key columns include:
#'   \itemize{
#'     \item `Scientific_Name`: Standardized scientific name.
#'     \item `Confidence`: Classifier confidence score.
#'     \item `Start_s`, `End_s`: Start and end time of detection in seconds within the file.
#'     \item `Original_File_Path`: Original path to the audio file.
#'     \item `Filename_Only`: Extracted filename.
#'     \item `File_Start_DateTime_UTC`: Start time of the audio file, parsed from filename.
#'           Assumes a "YYYYMMDD_HHMMSS" or "YYYY-MM-DD_HH-MM-SS" like pattern in the filename.
#'     \item `Detection_Start_DateTime_UTC`: Absolute start time of the detection in UTC.
#'     \item `Detection_Date`: Date of the detection.
#'     \item `Detection_Hour`: Hour of the detection (0-23).
#'     \item `Detection_Duration`: Duration of the detection in seconds.
#'     \item `Common_Name` (optional): Standardized common name, if present in input.
#'   }
#'   Rows where `File_Start_DateTime_UTC` could not be parsed are removed.
#'
#' @details
#'   \strong{Important for `calculate_pattern_metrics`}: This function does NOT
#'   automatically create the `AudioMoth_ID_Numeric` column. This column,
#'   containing unique numeric identifiers for each recording location/device,
#'   is required by `calculate_pattern_metrics`. You should add it after calling
#'   `read_birdnet_data`, for example, by using `extract_audiomoth_id()`
#'   and `dplyr::mutate()`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a dummy CSV file for example
#' temp_csv_path <- tempfile(fileext = ".csv")
#' mock_df <- data.frame(
#'   `Scientific name` = rep(c("Sylvia atricapilla", "Erithacus rubecula"), each = 2),
#'   `Common name` = rep(c("Blackcap", "Robin"), each = 2),
#'   `Confidence` = c(0.8, 0.9, 0.7, 0.6),
#'   `Start (s)` = c(10.1, 20.2, 5.5, 15.5),
#'   `End (s)` = c(12.1, 22.2, 7.5, 17.5),
#'   `File` = rep(
#'     c("path/to/recorder_01/20230515_100000.WAV",
#'       "path/to/recorder_02/20230515_110000.WAV"),
#'     each = 2
#'    ),
#'   check.names = FALSE
#' )
#' readr::write_csv(mock_df, temp_csv_path)
#'
#' # Basic usage
#' detections_data <- read_birdnet_data(temp_csv_path, min_confidence = 0.7)
#'
#' # With time filtering
#' start_time <- as.POSIXct("2023-05-15 09:00:00", tz = "UTC")
#' end_time <- as.POSIXct("2023-05-15 12:00:00", tz = "UTC")
#' detections_filtered <- read_birdnet_data(temp_csv_path,
#'                                          time_range = c(start_time, end_time))
#'
#' # Filter for dawn and dusk hours only
#' crepuscular_detections <- read_birdnet_data(temp_csv_path,
#'                                              hour_range = c(5, 8))
#'
#' # Custom filter function
#' high_conf_long_dets <- function(df) {
#'   df %>% dplyr::filter(Confidence > 0.8 & Detection_Duration > 2)
#' }
#' detections_custom <- read_birdnet_data(temp_csv_path,
#'                                        custom_filter = high_conf_long_dets)
#'
#' unlink(temp_csv_path) # Clean up
#' }
read_birdnet_data <- function(file_path,
                              species_list = NULL,
                              min_confidence = 0.01,
                              time_range = NULL,
                              hour_range = NULL,
                              exclude_weekends = FALSE,
                              custom_filter = NULL,
                              use_data_table = FALSE) {

  # Validate inputs
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  if (!is.numeric(min_confidence) || min_confidence < 0 || min_confidence > 1) {
    stop("'min_confidence' must be a number between 0 and 1.")
  }
  if (!is.null(species_list) && !is.character(species_list)) {
    stop("'species_list' must be a character vector or NULL.")
  }
  if (!is.null(time_range)) {
    if (length(time_range) != 2) {
      stop("'time_range' must be a vector of two POSIXct objects.")
    }
    if (!all(inherits(time_range, "POSIXct"))) {
      stop("'time_range' elements must be POSIXct objects.")
    }
  }
  if (!is.null(hour_range)) {
    if (length(hour_range) != 2 || !is.numeric(hour_range) ||
        any(hour_range < 0) || any(hour_range > 23)) {
      stop("'hour_range' must be a numeric vector of two integers between 0 and 23.")
    }
  }
  if (!is.logical(exclude_weekends) || length(exclude_weekends) != 1) {
    stop("'exclude_weekends' must be a single logical value.")
  }
  if (!is.null(custom_filter) && !is.function(custom_filter)) {
    stop("'custom_filter' must be a function or NULL.")
  }
  if (!is.logical(use_data_table) || length(use_data_table) != 1) {
    stop("'use_data_table' must be a single logical value.")
  }

  # Read data
  if (use_data_table && requireNamespace("data.table", quietly = TRUE)) {
    raw_data <- tryCatch({
      dt <- data.table::fread(file_path, showProgress = FALSE)
      # Convert to tibble for consistency with rest of package
      dplyr::as_tibble(dt)
    }, error = function(e) {
      # Fall back to readr if data.table fails
      message("data.table::fread failed, falling back to readr::read_csv")
      readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE, guess_max = 10000)
    })
  } else {
    raw_data <- tryCatch({
      readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE, guess_max = 10000)
    }, error = function(e) {
      stop("Failed to read CSV file: ", file_path, "\nOriginal error: ", e$message)
    })
  }

  if (nrow(raw_data) == 0) {
    return(dplyr::tibble())
  }

  # --- Explicit Renaming Block & Initial Mutation for Filename_Only ---
  expected_original_cols_in_csv <- c(
    "Scientific name", "Confidence", "Start (s)", "End (s)", "File"
  )
  current_csv_cols <- names(raw_data)
  missing_expected_cols <- setdiff(expected_original_cols_in_csv, current_csv_cols)

  if (length(missing_expected_cols) > 0) {
    stop("The input CSV file '", file_path, "' is missing the following expected columns: ",
         paste(missing_expected_cols, collapse = ", "), ".\n",
         "Actual columns found: ", paste(current_csv_cols, collapse = ", "))
  }

  standardized_data <- raw_data %>%
    dplyr::rename(
      Scientific_Name = `Scientific name`,
      Confidence = Confidence,
      Start_s = `Start (s)`,
      End_s = `End (s)`,
      Original_File_Path = File
    ) %>%
    dplyr::mutate(Filename_Only = basename(.data$Original_File_Path))

  if ("Common name" %in% current_csv_cols) {
    standardized_data <- standardized_data %>%
      dplyr::rename(Common_Name = `Common name`)
  }

  # Extract metadata from file paths
  processed_data <- standardized_data %>%
    dplyr::mutate(
      DateTimeString_From_Filename = stringr::str_extract(.data$Filename_Only, "\\d{8}[_T\\- ]?\\d{6}"),
      File_Start_DateTime_UTC = lubridate::ymd_hms(.data$DateTimeString_From_Filename, tz = "UTC", quiet = TRUE),
      Start_s_numeric = as.numeric(.data$Start_s),
      End_s_numeric = as.numeric(.data$End_s),
      Detection_Start_DateTime_UTC = .data$File_Start_DateTime_UTC + lubridate::seconds(.data$Start_s_numeric),
      Detection_Date = lubridate::as_date(.data$Detection_Start_DateTime_UTC),
      Detection_Hour = lubridate::hour(.data$Detection_Start_DateTime_UTC),
      Detection_Duration = .data$End_s_numeric - .data$Start_s_numeric
    ) %>%
    dplyr::select(-"Start_s_numeric", -"End_s_numeric")

  original_row_count <- nrow(processed_data)

  # Keep track of rows before filtering by date for better warning message
  rows_before_date_filter <- standardized_data %>%
    dplyr::mutate(
      DateTimeString_temp = stringr::str_extract(.data$Filename_Only, "\\d{8}[_T\\- ]?\\d{6}"),
      File_Start_DateTime_UTC_temp = lubridate::ymd_hms(.data$DateTimeString_temp, tz = "UTC", quiet = TRUE)
    )

  processed_data <- processed_data %>%
    dplyr::filter(!is.na(.data$File_Start_DateTime_UTC))

  if (nrow(processed_data) < original_row_count && original_row_count > 0) {
    dropped_rows <- original_row_count - nrow(processed_data)
    problematic_rows_info <- rows_before_date_filter %>%
      dplyr::filter(is.na(.data$File_Start_DateTime_UTC_temp))

    example_problem_filename <- if(nrow(problematic_rows_info) > 0) {
      utils::head(problematic_rows_info$Filename_Only, 1)
    } else {
      "Could not identify a specific problematic filename."
    }

    warning(dropped_rows, " row(s) were removed due to inability to parse date/time from filenames. ",
            "Ensure filenames contain a 'YYYYMMDD_HHMMSS' (or similar) pattern and are in UTC. ",
            "Example of problematic filename: '", example_problem_filename, "'")
  }

  if (nrow(processed_data) == 0) {
    return(dplyr::tibble())
  }

  # Apply filters
  # Species filter
  if (!is.null(species_list)) {
    processed_data <- processed_data %>%
      dplyr::filter(.data$Scientific_Name %in% species_list)
  }

  # Confidence filter
  processed_data <- processed_data %>%
    dplyr::filter(.data$Confidence >= min_confidence)

  # Time range filter
  if (!is.null(time_range)) {
    processed_data <- processed_data %>%
      dplyr::filter(.data$Detection_Start_DateTime_UTC >= time_range[1],
                    .data$Detection_Start_DateTime_UTC <= time_range[2])
  }

  # Hour range filter
  if (!is.null(hour_range)) {
    # Handle hour ranges that cross midnight
    if (hour_range[1] <= hour_range[2]) {
      processed_data <- processed_data %>%
        dplyr::filter(.data$Detection_Hour >= hour_range[1],
                      .data$Detection_Hour <= hour_range[2])
    } else {
      # Range crosses midnight (e.g., 22 to 2)
      processed_data <- processed_data %>%
        dplyr::filter(.data$Detection_Hour >= hour_range[1] |
                        .data$Detection_Hour <= hour_range[2])
    }
  }

  # Weekend filter
  if (exclude_weekends) {
    processed_data <- processed_data %>%
      dplyr::filter(!lubridate::wday(.data$Detection_Date) %in% c(1, 7))
  }

  # Custom filter
  if (!is.null(custom_filter) && is.function(custom_filter)) {
    processed_data <- custom_filter(processed_data)
  }

  return(processed_data)
}


#' Extract Numeric ID from Path
#'
#' Extracts a numeric ID (e.g., AudioMoth ID, site ID) from a character vector
#' of file or folder paths using a regular expression.
#'
#' @param path_vector Character vector of paths from which to extract IDs.
#' @param id_pattern Regular expression (character string) designed to capture
#'   the numeric ID.
#' @param from_folder_name Logical. If `TRUE`, the `id_pattern` is applied to
#'   `basename(dirname(path_vector))`. If `FALSE` (default), the pattern is
#'   applied directly to `path_vector`.
#'
#' @return An integer vector of extracted numeric IDs. Returns `NA` where the
#'   pattern does not match or does not yield a convertible numeric value.
#' @export
#' @examples
#' paths1 <- c("/data/AudioMoth_01/file.wav", "/data/site_unit_123/rec.wav",
#'             "project/Deployment_XYZ/Recorder_Active_007/sound.wav")
#'
#' # Example 1: Extract digits following "AudioMoth_" or "unit_" directly from path
#' extract_audiomoth_id(paths1, id_pattern = "(?<=AudioMoth_|unit_)\\d+")
#'
#' # Example 2: Extract trailing digits from the parent folder name
#' # e.g., from "/data/AudioMoth_01" or ".../Recorder_Active_007"
#' extract_audiomoth_id(paths1, id_pattern = "\\d+$", from_folder_name = TRUE)
#'
#' # Example that was too long (now broken into multiple lines):
#' # 'from_folder_name = FALSE' here as 'dirname(paths1)' already provides folder paths.
#' # This demonstrates applying a pattern to already isolated folder names.
#' folder_names_only <- dirname(paths1)
#' extract_audiomoth_id(folder_names_only, id_pattern = "\\d+",
#'                      from_folder_name = FALSE)
#'
extract_audiomoth_id <- function(path_vector,
                                 id_pattern = "\\d+$",
                                 from_folder_name = FALSE) {

  if (!is.character(path_vector)) {
    stop("'path_vector' must be a character vector.")
  }
  if (!is.character(id_pattern) || length(id_pattern) != 1) {
    stop("'id_pattern' must be a single character string (regex).")
  }
  if (!is.logical(from_folder_name) || length(from_folder_name) != 1) {
    stop("'from_folder_name' must be a single logical value (TRUE/FALSE).")
  }

  if (from_folder_name) {
    target_strings <- basename(dirname(path_vector))
  } else {
    target_strings <- path_vector
  }

  extracted_ids_char <- stringr::str_extract(target_strings, id_pattern)
  extracted_ids_int <- suppressWarnings(as.integer(extracted_ids_char))

  return(extracted_ids_int)
}
