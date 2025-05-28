getwd()

# Test enhanced reading with filters
test_data <- read_birdnet_data(
  "/Users/lucasbeseler/Desktop/Statistik_Paper_27.05.25/01_Analysis_Outputs/PAM_Pattern_Quantification_20250528/species_pattern_indicators.csv",
  min_confidence = 0.5,
  hour_range = c(5, 20),
  exclude_weekends = TRUE
)

# Test enhanced metrics with anomaly detection
metrics <- calculate_pattern_metrics(test_data, calculate_anomalies = TRUE)

# Test data quality assessment
quality <- calculate_data_quality(test_data)
generate_quality_report(quality)

# Test visualization (if ggplot2 installed)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  plots <- plot_pattern_diagnostics(metrics, plot_type = c("overview", "confidence"))
}

# Test S3 class
metrics_obj <- as.pam_metrics(metrics)
print(metrics_obj)
summary(metrics_obj)
