#' Create Diagnostic Plots for Pattern Metrics
#'
#' Generates a comprehensive set of diagnostic plots to visualize pattern metrics
#' and identify potential issues with species detections. Can create plots for
#' all species or a specific species.
#'
#' @param metrics_df A `tibble` or `data.frame` with calculated metrics,
#'   typically the output of `calculate_pattern_metrics()` or `flag_species()`.
#' @param species_name Optional. Scientific name of a specific species to plot.
#'   If NULL (default), creates overview plots for all species.
#' @param detection_data Optional. The original detection data used to create
#'   the metrics. Required for some detailed plots (temporal patterns, etc.).
#' @param plot_type Character vector specifying which plots to create. Options:
#'   "overview", "confidence", "temporal", "spatial", "anomaly".
#'   Default is "overview".
#' @param save_plots Logical. If TRUE, saves plots to files instead of displaying.
#'   Default is FALSE.
#' @param output_dir Character. Directory to save plots if save_plots = TRUE.
#'   Default is "pam_diagnostic_plots".
#'
#' @return If save_plots = FALSE, returns a list of ggplot objects.
#'   If save_plots = TRUE, invisibly returns the paths of saved files.
#'
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have metrics_df from calculate_pattern_metrics()
#'
#' # Create overview plots for all species
#' plots <- plot_pattern_diagnostics(metrics_df)
#'
#' # Create all plot types for a specific species
#' plots_species <- plot_pattern_diagnostics(
#'   metrics_df,
#'   species_name = "Erithacus rubecula",
#'   detection_data = detections_with_id,
#'   plot_type = c("confidence", "temporal", "spatial")
#' )
#'
#' # Save plots to files
#' plot_pattern_diagnostics(
#'   metrics_df,
#'   plot_type = "overview",
#'   save_plots = TRUE,
#'   output_dir = "diagnostic_plots"
#' )
#' }
plot_pattern_diagnostics <- function(metrics_df,
                                     species_name = NULL,
                                     detection_data = NULL,
                                     plot_type = "overview",
                                     save_plots = FALSE,
                                     output_dir = "pam_diagnostic_plots") {

  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it.")
  }

  # Additional packages for enhanced plots
  has_scales <- requireNamespace("scales", quietly = TRUE)
  has_viridis <- requireNamespace("viridisLite", quietly = TRUE)

  # Input validation
  if (!is.data.frame(metrics_df)) {
    stop("'metrics_df' must be a data frame or tibble.")
  }

  if (!is.null(species_name) && !species_name %in% metrics_df$Scientific_Name) {
    stop("Species '", species_name, "' not found in metrics_df.")
  }

  # Create output directory if saving
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Initialize plot list
  plot_list <- list()

  # Filter to specific species if requested
  if (!is.null(species_name)) {
    metrics_subset <- metrics_df %>%
      dplyr::filter(.data$Scientific_Name == species_name)

    if (!is.null(detection_data)) {
      detection_subset <- detection_data %>%
        dplyr::filter(.data$Scientific_Name == species_name)
    }
  } else {
    metrics_subset <- metrics_df
    detection_subset <- detection_data
  }

  # Create plots based on type
  if ("overview" %in% plot_type) {
    plot_list$overview <- create_overview_plot(metrics_subset, has_viridis)
  }

  if ("confidence" %in% plot_type) {
    plot_list$confidence <- create_confidence_plot(metrics_subset, has_scales)
  }

  if ("temporal" %in% plot_type && !is.null(detection_subset)) {
    plot_list$temporal <- create_temporal_plot(detection_subset, species_name)
  }

  if ("spatial" %in% plot_type && !is.null(detection_subset)) {
    plot_list$spatial <- create_spatial_plot(detection_subset, metrics_subset, species_name)
  }

  if ("anomaly" %in% plot_type && "Overall_Anomaly_Score" %in% names(metrics_subset)) {
    plot_list$anomaly <- create_anomaly_plot(metrics_subset)
  }

  # Save or return plots
  if (save_plots) {
    saved_files <- character()
    for (plot_name in names(plot_list)) {
      filename <- if (!is.null(species_name)) {
        file.path(output_dir, paste0(gsub(" ", "_", species_name), "_", plot_name, ".png"))
      } else {
        file.path(output_dir, paste0("all_species_", plot_name, ".png"))
      }

      ggplot2::ggsave(filename, plot_list[[plot_name]],
                      width = 10, height = 8, dpi = 300)
      saved_files <- c(saved_files, filename)
    }
    message("Plots saved to: ", output_dir)
    invisible(saved_files)
  } else {
    return(plot_list)
  }
}


#' Create overview scatter plot of key metrics
#' @keywords internal
create_overview_plot <- function(metrics_df, has_viridis = FALSE) {

  # Prepare data - handle potential Review_Score
  plot_data <- metrics_df %>%
    dplyr::mutate(
      Size_Category = cut(.data$Total_Detections,
                          breaks = c(0, 50, 200, 1000, Inf),
                          labels = c("<50", "50-200", "200-1000", ">1000")),
      Has_Flag = if ("Review_Score" %in% names(.)) .data$Review_Score > 0 else FALSE
    )

  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = .data$Median_CI,
                                    y = .data$Plot_Occupancy_Pct)) +
    ggplot2::geom_point(ggplot2::aes(size = .data$Total_Detections,
                                     color = .data$Has_Flag),
                        alpha = 0.7) +
    ggplot2::scale_size_continuous(range = c(2, 10),
                                   breaks = c(50, 200, 1000),
                                   name = "Total\nDetections") +
    ggplot2::scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                                name = "Flagged",
                                labels = c("No", "Yes")) +
    ggplot2::labs(
      title = "Species Detection Patterns Overview",
      subtitle = "Each point represents a species",
      x = "Median Confidence Score",
      y = "Plot Occupancy (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      legend.position = "right"
    )

  # Add species labels for flagged species
  if (sum(plot_data$Has_Flag) > 0 && sum(plot_data$Has_Flag) <= 10) {
    p <- p +
      ggplot2::geom_text(
        data = plot_data %>% dplyr::filter(.data$Has_Flag),
        ggplot2::aes(label = .data$Scientific_Name),
        vjust = -1, size = 3, check_overlap = TRUE
      )
  }

  return(p)
}


#' Create confidence interval distribution plot
#' @keywords internal
create_confidence_plot <- function(metrics_df, has_scales = FALSE) {

  # Select top species by total detections for clarity
  n_species_to_show <- min(20, nrow(metrics_df))

  plot_data <- metrics_df %>%
    dplyr::arrange(dplyr::desc(.data$Total_Detections)) %>%
    dplyr::slice_head(n = n_species_to_show) %>%
    dplyr::mutate(
      Scientific_Name = forcats::fct_reorder(.data$Scientific_Name, .data$Median_CI)
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Scientific_Name)) +
    # Confidence interval range
    ggplot2::geom_segment(ggplot2::aes(xend = .data$Scientific_Name,
                                       y = .data$Q1_CI,
                                       yend = .data$Q3_CI),
                          size = 3, alpha = 0.5, color = "skyblue") +
    # Min-Max range
    ggplot2::geom_segment(ggplot2::aes(xend = .data$Scientific_Name,
                                       y = .data$Min_CI,
                                       yend = .data$Max_CI),
                          size = 0.5, color = "gray50") +
    # Median point
    ggplot2::geom_point(ggplot2::aes(y = .data$Median_CI),
                        size = 3, color = "darkblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Confidence Score Distributions by Species",
      subtitle = paste("Top", n_species_to_show, "species by detection count"),
      x = "",
      y = "Confidence Score",
      caption = "Thick bars: IQR, Thin lines: Min-Max, Points: Median"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 8)
    )

  if (has_scales) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format())
  }

  return(p)
}


#' Create temporal activity pattern plot
#' @keywords internal
create_temporal_plot <- function(detection_data, species_name = NULL) {

  # Aggregate by hour for all species or specific species
  hourly_data <- detection_data %>%
    dplyr::group_by(.data$Detection_Hour) %>%
    dplyr::summarise(
      N_Detections = dplyr::n(),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      Hour_Label = sprintf("%02d:00", .data$Detection_Hour)
    )

  # Ensure all hours are represented
  all_hours <- data.frame(Detection_Hour = 0:23)
  hourly_data <- dplyr::left_join(all_hours, hourly_data, by = "Detection_Hour") %>%
    dplyr::mutate(
      N_Detections = dplyr::coalesce(.data$N_Detections, 0L),
      Hour_Label = sprintf("%02d:00", .data$Detection_Hour)
    )

  title_text <- if (!is.null(species_name)) {
    paste("Hourly Activity Pattern:", species_name)
  } else {
    "Hourly Activity Pattern: All Species Combined"
  }

  p <- ggplot2::ggplot(hourly_data,
                       ggplot2::aes(x = .data$Detection_Hour,
                                    y = .data$N_Detections)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
    ggplot2::geom_smooth(method = "loess", se = TRUE,
                         color = "darkred", size = 1.2) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, by = 3),
      labels = sprintf("%02d:00", seq(0, 23, by = 3))
    ) +
    ggplot2::labs(
      title = title_text,
      x = "Hour of Day",
      y = "Number of Detections"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # Add dawn/dusk shading (approximate)
  p <- p +
    ggplot2::annotate("rect", xmin = 5, xmax = 7, ymin = -Inf, ymax = Inf,
                      alpha = 0.1, fill = "orange") +
    ggplot2::annotate("rect", xmin = 18, xmax = 20, ymin = -Inf, ymax = Inf,
                      alpha = 0.1, fill = "orange") +
    ggplot2::annotate("text", x = 6, y = max(hourly_data$N_Detections) * 0.9,
                      label = "Dawn", color = "orange", alpha = 0.7) +
    ggplot2::annotate("text", x = 19, y = max(hourly_data$N_Detections) * 0.9,
                      label = "Dusk", color = "orange", alpha = 0.7)

  return(p)
}


#' Create spatial distribution plot
#' @keywords internal
create_spatial_plot <- function(detection_data, metrics_df, species_name = NULL) {

  # Aggregate by recorder
  spatial_data <- detection_data %>%
    dplyr::group_by(.data$AudioMoth_ID_Numeric) %>%
    dplyr::summarise(
      N_Detections = dplyr::n(),
      N_Species = dplyr::n_distinct(.data$Scientific_Name),
      Mean_Confidence = mean(.data$Confidence, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::filter(!is.na(.data$AudioMoth_ID_Numeric)) %>%
    dplyr::mutate(
      Recorder_Label = paste("Recorder", .data$AudioMoth_ID_Numeric)
    )

  if (nrow(spatial_data) == 0) {
    # Return empty plot with message
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No spatial data available\n(AudioMoth_ID_Numeric missing)",
                          size = 6) +
        ggplot2::theme_void()
    )
  }

  title_text <- if (!is.null(species_name)) {
    paste("Spatial Distribution:", species_name)
  } else {
    "Spatial Distribution: All Species"
  }

  p <- ggplot2::ggplot(spatial_data,
                       ggplot2::aes(x = forcats::fct_reorder(.data$Recorder_Label,
                                                             .data$N_Detections),
                                    y = .data$N_Detections)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$Mean_Confidence)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title_text,
      x = "Recording Location",
      y = "Number of Detections",
      fill = "Mean\nConfidence"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold")
    )

  # Use viridis color scale if available
  if (requireNamespace("viridisLite", quietly = TRUE)) {
    p <- p + ggplot2::scale_fill_viridis_c()
  } else {
    p <- p + ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue")
  }

  return(p)
}


#' Create anomaly score visualization
#' @keywords internal
create_anomaly_plot <- function(metrics_df) {

  # Check for anomaly columns
  anomaly_cols <- c("CI_Anomaly_Score", "Temporal_Anomaly_Score",
                    "Spatial_Anomaly_Score", "Overall_Anomaly_Score")

  available_cols <- intersect(anomaly_cols, names(metrics_df))

  if (length(available_cols) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No anomaly scores available\n(Run calculate_pattern_metrics with calculate_anomalies = TRUE)",
                          size = 6) +
        ggplot2::theme_void()
    )
  }

  # Select top anomalous species
  plot_data <- metrics_df %>%
    dplyr::filter(.data$Overall_Anomaly_Score > 0) %>%
    dplyr::arrange(dplyr::desc(.data$Overall_Anomaly_Score)) %>%
    dplyr::slice_head(n = 20) %>%
    dplyr::select(Scientific_Name, dplyr::all_of(available_cols)) %>%
    tidyr::pivot_longer(cols = -Scientific_Name,
                        names_to = "Anomaly_Type",
                        values_to = "Score") %>%
    dplyr::mutate(
      Anomaly_Type = gsub("_Anomaly_Score", "", .data$Anomaly_Type),
      Anomaly_Type = factor(.data$Anomaly_Type,
                            levels = c("CI", "Temporal", "Spatial", "Overall"))
    )

  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = .data$Anomaly_Type,
                                    y = forcats::fct_reorder(.data$Scientific_Name,
                                                             .data$Score,
                                                             .fun = max),
                                    fill = .data$Score)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "white",
      mid = "yellow",
      high = "red",
      midpoint = 2.5,
      limits = c(0, 5),
      name = "Anomaly\nScore"
    ) +
    ggplot2::labs(
      title = "Species Anomaly Scores",
      subtitle = "Higher scores indicate more unusual patterns",
      x = "Anomaly Type",
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8)
    )

  return(p)
}
