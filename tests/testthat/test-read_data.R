# File: tests/testthat/test-read_data.R

# --- Tests for extract_audiomoth_id ---
test_that("extract_audiomoth_id correctly extracts IDs from various path structures", {
  paths_to_test <- c(
    "/project/data/AudioMoth_01_SiteA/recording_20230101T100000.WAV",
    "/project/data/Unit_123_LocationB/soundfile.WAV",
    "some/other/path/Recorder_Active_007/clip.WAV",
    "no_id_in_folder_name/file.WAV",
    "file_with_id_AM25.WAV",
    "another_file_RECORDER42_data.WAV"
  )

  expect_equal(
    extract_audiomoth_id(paths_to_test, id_pattern = "(?<=AudioMoth_|Unit_|Recorder_Active_)\\d+"),
    c(1, 123, 7, NA, NA, NA)
  )
  expect_equal(
    extract_audiomoth_id(paths_to_test, id_pattern = "\\d+$", from_folder_name = TRUE),
    c(NA, NA, 7, NA, NA, NA)
  )
  expect_equal(
    extract_audiomoth_id(paths_to_test, id_pattern = "(?<=AM|RECORDER)\\d+", from_folder_name = FALSE),
    c(NA, NA, NA, NA, 25, 42)
  )
  expect_equal(extract_audiomoth_id(character(0)), integer(0))
  expect_true(all(is.na(extract_audiomoth_id(paths_to_test, id_pattern = "this_pattern_will_not_match_anything"))))
  expect_error(extract_audiomoth_id(123), "'path_vector' must be a character vector.")
  expect_error(extract_audiomoth_id(paths_to_test, id_pattern = NULL), "'id_pattern' must be a single character string")
  expect_error(extract_audiomoth_id(paths_to_test, from_folder_name = "maybe"), "'from_folder_name' must be a single logical value")
})

# --- Tests for read_birdnet_data ---
test_that("read_birdnet_data handles CSV reading, filtering, and column standardization", {
  temp_csv_file <- tempfile(fileext = ".csv")
  mock_birdnet_data <- data.frame(
    `Scientific name` = c("Turdus merula", "Erithacus rubecula", "Turdus merula", "Sylvia atricapilla", "Nonexistentus birdus"),
    `Common name` = c("Blackbird", "Robin", "Blackbird", "Blackcap", "Fake Bird"),
    `Confidence` = c(0.92, 0.45, 0.81, 0.77, 0.99),
    `Start (s)` = c(10.5, 20.1, 30.3, 40.0, 50.5),
    `End (s)` = c(12.5, 22.1, 32.3, 42.0, 52.5),
    `File` = c(
      "audio_files/SiteRec01/20230101_100000.WAV",
      "audio_files/SiteRec02/20230101_110000.WAV",
      "audio_files/SiteRec01/bad_date_in_filename.WAV", # This will cause a warning
      "audio_files/SiteRec03/20230102_120000.WAV",
      "audio_files/SiteRec04/20230102_130000.WAV"
    ),
    check.names = FALSE
  )
  readr::write_csv(mock_birdnet_data, temp_csv_file)

  expected_date_warning_regex <- "row\\(s\\) were removed due to inability to parse date/time"

  # Test Case 1: Basic reading and confidence filtering (expects a warning, then check result)
  # First, check for the warning when the function is called
  expect_warning(
    read_birdnet_data(temp_csv_file, min_confidence = 0.75),
    regexp = expected_date_warning_regex
  )
  # Then, call the function again to get its actual result for further checks
  result_conf_filter <- read_birdnet_data(temp_csv_file, min_confidence = 0.75)

  expect_s3_class(result_conf_filter, "tbl_df")
  expect_equal(nrow(result_conf_filter), 3)
  expect_true("Scientific_Name" %in% names(result_conf_filter))
  expect_true("File_Start_DateTime_UTC" %in% names(result_conf_filter))
  if (nrow(result_conf_filter) > 0) {
    expect_false(any(is.na(result_conf_filter$File_Start_DateTime_UTC)))
  }

  # Test Case 2: Species list filtering (expects a warning, then check result)
  expect_warning(
    read_birdnet_data(temp_csv_file,
                      species_list = c("Erithacus rubecula", "Sylvia atricapilla"),
                      min_confidence = 0.1),
    regexp = expected_date_warning_regex
  )
  result_species_filter <- read_birdnet_data(temp_csv_file,
                                             species_list = c("Erithacus rubecula", "Sylvia atricapilla"),
                                             min_confidence = 0.1)
  expect_equal(nrow(result_species_filter), 2)
  if (nrow(result_species_filter) > 0) {
    expect_true(all(result_species_filter$Scientific_Name %in% c("Erithacus rubecula", "Sylvia atricapilla")))
  }

  # Test Case 3: Date parsing failure handling (expects a warning, then check result)
  expect_warning(
    read_birdnet_data(temp_csv_file, min_confidence = 0.1),
    regexp = expected_date_warning_regex
  )
  result_date_parsing <- read_birdnet_data(temp_csv_file, min_confidence = 0.1)
  expect_equal(nrow(result_date_parsing), 4)
  if (nrow(result_date_parsing) > 0) {
    expect_false("bad_date_in_filename.WAV" %in% basename(result_date_parsing$Original_File_Path))
  }

  # Test Case 4: Non-existent file path
  expect_error(read_birdnet_data("this_file_does_not_exist.csv"), regexp = "File not found")

  # Test Case 5: Empty file (header only) - no specific warning expected from this
  temp_empty_file_header_only <- tempfile(fileext = ".csv")
  writeLines("Scientific name,Common name,Confidence,Start (s),End (s),File", temp_empty_file_header_only)
  result_empty_header <- read_birdnet_data(temp_empty_file_header_only)
  expect_s3_class(result_empty_header, "tbl_df")
  expect_equal(nrow(result_empty_header), 0)
  unlink(temp_empty_file_header_only)

  temp_empty_file_0_bytes <- tempfile(fileext = ".csv")
  file.create(temp_empty_file_0_bytes)
  result_empty_0_bytes <- read_birdnet_data(temp_empty_file_0_bytes)
  expect_s3_class(result_empty_0_bytes, "tbl_df")
  expect_equal(nrow(result_empty_0_bytes), 0)
  unlink(temp_empty_file_0_bytes)

  # Test Case 6: CSV with missing essential columns
  temp_missing_col_csv <- tempfile(fileext = ".csv")
  mock_data_missing_col <- mock_birdnet_data[, !names(mock_birdnet_data) %in% "Confidence"]
  readr::write_csv(mock_data_missing_col, temp_missing_col_csv)
  expect_error(read_birdnet_data(temp_missing_col_csv),
               regexp = "missing the following expected columns: Confidence")
  unlink(temp_missing_col_csv)

  unlink(temp_csv_file) # Clean up the main temporary CSV file
})
