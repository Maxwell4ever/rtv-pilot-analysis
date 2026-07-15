# =============================================================================
# R/00_packages.R - Package Loading and Environment Check
# =============================================================================
# Run first. Loads all packages used across the pipeline and checks versions.
# If a package is missing, prints an installation instruction and stops.
#
# To restore the full reproducible environment: renv::restore()
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  if (nzchar(sys.frame(1)$ofile %||% "")) sys.frame(1)$ofile else "."
), "utils.R"))

step_banner(0, "Package Loading")

# -- Required packages and minimum versions ------------------------------------
REQUIRED <- list(
  dplyr      = "1.0.0",
  tidyr      = "1.0.0",
  ggplot2    = "3.3.0",
  estimatr   = "0.26.0",
  broom      = "0.7.0",
  scales     = "1.1.0",
  rlang      = "0.4.0"
)

OPTIONAL <- list(
  openxlsx   = "4.2.0",   # for data_dictionary.xlsx
  testthat   = "3.0.0"    # for tests/
)

# -- Load and check ------------------------------------------------------------
pkg_status <- lapply(names(REQUIRED), function(pkg) {
  min_ver <- REQUIRED[[pkg]]
  avail   <- requireNamespace(pkg, quietly = TRUE)

  if (!avail) {
    log_msg(paste0("MISSING package: ", pkg,
                   " - install with: install.packages('", pkg, "')"))
    return(list(pkg = pkg, status = "MISSING", version = NA))
  }

  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  ver <- as.character(packageVersion(pkg))
  ok  <- utils::compareVersion(ver, min_ver) >= 0

  if (!ok) {
    log_msg(sprintf("OUTDATED: %s %s (need >= %s)", pkg, ver, min_ver))
    return(list(pkg = pkg, status = "OUTDATED", version = ver))
  }

  log_msg(sprintf("  OK  %-12s %s", pkg, ver))
  list(pkg = pkg, status = "OK", version = ver)
})

# Optional packages
for (pkg in names(OPTIONAL)) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    log_msg(sprintf("  OK  %-12s %s (optional)",
                    pkg, as.character(packageVersion(pkg))))
  } else {
    log_msg(paste0("  --  ", pkg, " not installed (optional - skipped)"))
  }
}

# -- Stop if any required package is missing or outdated ----------------------
failures <- Filter(function(x) x$status != "OK", pkg_status)
if (length(failures) > 0) {
  stop(
    "Required packages missing or outdated: ",
    paste(sapply(failures, `[[`, "pkg"), collapse = ", "),
    "\nRun renv::restore() to restore the project environment."
  )
}

# -- Analysis-wide constants (available after 00_packages.R is sourced) --------
ANALYSIS <- list(
  seed          = 42L,
  cluster_var   = "village_id",
  se_type       = "CR2",
  bootstrap_B   = 2000L,
  bh_method     = "BH",
  winsor_pct    = 0.99,
  alpha         = 0.05,
  fcs_poor      = 28L,
  fcs_borderline= 42L,
  fcs_max       = 112L,
  hdds_max      = 12L,
  covariates    = c("hh_size", "female_head", "age_head",
                    "land_ha", "educ_head"),
  outcomes      = list(
    fcs   = list(var = "fcs",         label = "Food Consumption Score (FCS)"),
    hdds  = list(var = "hdds",        label = "Dietary Diversity Score (HDDS)"),
    asset = list(var = "asset_index", label = "Asset Index")
  ),
  pilot_period  = "October 2023 to April 2024",
  district      = "Isingiro District, Uganda",
  n_villages    = 20L,
  n_pilot_vill  = 12L,
  n_comp_vill   = 8L
)

set.seed(ANALYSIS$seed)
log_msg(paste0("Seed set: ", ANALYSIS$seed))
log_msg("00_packages.R complete.")
