# =============================================================================
# run_analysis.R - MASTER ENTRY POINT
# =============================================================================
# RTV Isingiro Productive Asset Transfer Pilot - Evaluation Pipeline
# Analyst: Maxwell Joseph Ochora | Portfolio Project | June 2026
#
# USAGE:
#   From RStudio: open RTV_Pilot.Rproj, then source("run_analysis.R")
#   From terminal: Rscript run_analysis.R
#
# To run a single script:
#   source("R/06_models.R")
#   source("R/08_figures.R")
#
# To run tests only:
#   source("tests/test-cleaning.R")
#   source("tests/test-models.R")
#
# PIPELINE FLOW:
#   utils.R + 00_packages.R  (shared across all scripts)
#       |
#   01_import.R        -> data/processed/01_raw_data.rds
#       |
#   02_validation.R    -> data/processed/02_validation_flags.rds
#       |                             + p99_income
#   03_cleaning.R      -> data/processed/03_df_clean.rds
#       |                             + 03_cleaning_log.rds
#       |                             + data/processed/analysis_dataset.csv
#   04_descriptive.R   -> data/processed/04_descriptives.rds
#       |
#   05_balance_checks.R -> data/processed/05_balance.rds
#       |
#   06_models.R        -> data/processed/06_models.rds
#       |                             + 06_bootstrap.rds
#       |                             + 06_threshold.rds
#   07_subgroups.R     -> data/processed/07_df_sub.rds
#       |                             + 07_subgroups.rds
#   08_figures.R       -> output/figures/figure01_fcs_distribution.png
#       |                             + figure02_did_estimates.png
#       |                             + figure03_subgroup_trajectories.png
#   09_tables.R        -> output/tables/table01_sample_summary.csv
#       |                             + table02 through table06
#   10_sensitivity.R   -> output/tables/table07_sensitivity.csv
#       |                             + data/processed/10_sensitivity.rds
#   [tests]            -> console output only
# =============================================================================

# -- Locate project root from this file's path ---------------------------------
.args     <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)

PROJ_ROOT <- if (length(.file_arg) > 0) {
  normalizePath(dirname(sub("^--file=", "", .file_arg[1])))
} else {
  # Interactive / source() - walk up looking for .Rproj
  .dir <- getwd()
  repeat {
    if (length(list.files(.dir, pattern="\\.Rproj$")) > 0) break
    .parent <- dirname(.dir)
    if (.parent == .dir) break
    .dir <- .parent
  }
  .dir
}
PROJ_ROOT <- "C:/Users/USER/Desktop/rtv-pilot-analysis"
setwd(PROJ_ROOT)
cat("Project root:", PROJ_ROOT, "\n\n")
setwd(PROJ_ROOT)
cat("Project root:", PROJ_ROOT, "\n\n")

# -- Load shared foundation ----------------------------------------------------
source(file.path(PROJ_ROOT, "R", "utils.R"))
source(file.path(PROJ_ROOT, "R", "00_packages.R"))

# -- Runner helper -------------------------------------------------------------
.run <- function(script, step_num, label) {
  cat("\n", strrep("=", 55), "\n", sep="")
  cat(sprintf("  STEP %02d - %s\n", step_num, label))
  cat(strrep("=", 55), "\n\n", sep="")
  t0 <- proc.time()["elapsed"]
  tryCatch(
    source(file.path(PROJ_ROOT, "R", script), local=FALSE),
    error = function(e) {
      cat(sprintf("\n!!! STEP %02d FAILED: %s\n", step_num, label))
      cat("Error:", conditionMessage(e), "\n\n")
      stop(sprintf("Pipeline halted at Step %02d.", step_num), call.=FALSE)
    }
  )
  elapsed <- proc.time()["elapsed"] - t0
  cat(sprintf("\n  [DONE] Step %02d completed in %.1fs\n", step_num, elapsed))
  invisible(elapsed)
}

.run_test <- function(test_file, label) {
  cat(sprintf("\n  [TEST] %s\n", label))
  tryCatch(
    source(file.path(PROJ_ROOT, "tests", test_file), local=FALSE),
    error = function(e) {
      cat(sprintf("  [FAIL] %s: %s\n", label, conditionMessage(e)))
    }
  )
}

# =============================================================================
# EXECUTE PIPELINE
# =============================================================================
t_start <- proc.time()["elapsed"]

cat(strrep("#", 55), "\n", sep="")
cat("  RTV ISINGIRO PILOT EVALUATION PIPELINE\n")
cat(strrep("#", 55), "\n\n", sep="")

# Phase 1: Data preparation
.run("01_import.R",         1, "Import Raw Data")
.run("02_validation.R",     2, "Data Quality Validation")
.run("03_cleaning.R",       3, "Data Cleaning + Attrition")
.run("04_descriptive.R",    4, "Descriptive Statistics")
.run("05_balance_checks.R", 5, "Baseline Balance + Parallel Trends")

# Phase 2: Analysis
.run("06_models.R",         6, "DiD Regressions + Bootstrap")
.run("07_subgroups.R",      7, "Subgroup Analysis")

# Phase 3: Outputs
.run("08_figures.R",        8, "Figures")
.run("09_tables.R",         9, "Tables")
.run("10_sensitivity.R",   10, "Sensitivity Analyses")

# Tests
cat("\n", strrep("=", 55), "\n", sep="")
cat("  RUNNING TESTS\n")
cat(strrep("=", 55), "\n", sep="")
.run_test("test-import.R",     "Import checks")
.run_test("test-validation.R", "Validation checks")
.run_test("test-cleaning.R",   "Cleaning checks")
.run_test("test-models.R",     "Model checks")

# =============================================================================
# SUMMARY
# =============================================================================
total <- proc.time()["elapsed"] - t_start

cat("\n", strrep("#", 55), "\n", sep="")
cat(sprintf("  PIPELINE COMPLETE - %.1fs total\n", total))
cat(strrep("#", 55), "\n\n", sep="")

cat("FIGURES  -> output/figures/\n")
list.files(file.path(PROJ_ROOT, "output", "figures")) |>
  (\(f) cat(sprintf("  %s\n", f)))()

cat("\nTABLES   -> output/tables/\n")
list.files(file.path(PROJ_ROOT, "output", "tables")) |>
  (\(f) cat(sprintf("  %s\n", f)))()

cat("\nLOGS     -> output/logs/\n")
list.files(file.path(PROJ_ROOT, "output", "logs")) |>
  (\(f) cat(sprintf("  %s\n", f)))()

cat("\nPROCESSED -> data/processed/\n")
list.files(file.path(PROJ_ROOT, "data", "processed")) |>
  (\(f) cat(sprintf("  %s\n", f)))()
