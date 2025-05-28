# File: R/zzz.R
# Package configuration and setup

.onLoad <- function(libname, pkgname) {
  # Set default options for the package
  op <- options()
  op.pamPatterns <- list(
    pamPatterns.default_ci_thresholds = c(high = 0.7, medium = 0.5),
    pamPatterns.default_min_confidence = 0.01,
    pamPatterns.default_min_detections = 30,
    pamPatterns.n_cores = 1L,
    pamPatterns.verbose = TRUE
  )
  toset <- !(names(op.pamPatterns) %in% names(op))
  if(any(toset)) options(op.pamPatterns[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "pamPatterns ", utils::packageVersion("pamPatterns"), "\n",
    "Pattern Analysis for Passive Acoustic Monitoring Data\n",
    "Use help(package = 'pamPatterns') for documentation."
  )
}
