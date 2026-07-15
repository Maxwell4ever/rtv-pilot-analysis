# =============================================================================
# R/01_import.R - Import Raw Data
# =============================================================================
# Input:   data/raw/rtv_pilot_hh_data.csv
# Output:  data/processed/01_raw_data.rds
#
# Loads the raw CSV, runs structural checks, and saves an unmodified copy
# as an RDS hand-off. No values are changed here.
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("01_import.R")
step_banner(1, "Import Raw Data")

# -- Load ----------------------------------------------------------------------
section("Loading raw CSV")
log_msg(paste("Source:", PATHS$raw))

df_raw <- read.csv(PATHS$raw, stringsAsFactors = FALSE)

# -- Structural checks ---------------------------------------------------------
section("Structural Inspection")

log_msg(sprintf("Rows: %d  |  Cols: %d", nrow(df_raw), ncol(df_raw)))
log_msg(sprintf("Columns: %s", paste(names(df_raw), collapse = ", ")))
log_msg(sprintf("Unique households: %d  (expected 640)",
                length(unique(df_raw$hhid))))
log_msg(sprintf("Unique villages:   %d  (expected 20)",
                length(unique(df_raw$village_id))))
log_msg(sprintf("Rounds present:    %s",
                paste(sort(unique(df_raw$round)), collapse = ", ")))

# Treatment consistency check - must be village-level, not household-level
mixed_treat <- df_raw |>
  dplyr::group_by(village_id) |>
  dplyr::summarise(n_vals = dplyr::n_distinct(treatment),
                   .groups = "drop") |>
  dplyr::filter(n_vals > 1)

if (nrow(mixed_treat) > 0) {
  log_msg(sprintf("WARNING: Treatment varies within %d village(s): %s",
                  nrow(mixed_treat),
                  paste(mixed_treat$village_id, collapse = ", ")))
} else {
  log_msg("Treatment assignment: consistent within all villages. (OK)")
}

# Village-level treatment summary
vill_summary <- df_raw |>
  dplyr::distinct(village_id, treatment) |>
  dplyr::count(treatment) |>
  dplyr::mutate(group = ifelse(treatment == 1, "Pilot", "Comparison"))

log_msg(sprintf("Pilot villages: %d  |  Comparison villages: %d",
                vill_summary$n[vill_summary$treatment == 1],
                vill_summary$n[vill_summary$treatment == 0]))

# Outcome variable range flags (preliminary - full check in 02_validation.R)
log_msg(sprintf("FCS range:    [%.0f, %.0f]  (valid: 0-112)",
                min(df_raw$fcs, na.rm=TRUE), max(df_raw$fcs, na.rm=TRUE)))
log_msg(sprintf("HDDS range:   [%.0f, %.0f]  (valid: 0-12)",
                min(df_raw$hdds, na.rm=TRUE), max(df_raw$hdds, na.rm=TRUE)))
log_msg(sprintf("Income range: [%.1f, %.1f]",
                min(df_raw$monthly_income_usd, na.rm=TRUE),
                max(df_raw$monthly_income_usd, na.rm=TRUE)))

# -- Save hand-off -------------------------------------------------------------
save_rds(df_raw, "raw_data", "raw dataset (unmodified)")

log_msg("01_import.R complete. Run 02_validation.R next.")
