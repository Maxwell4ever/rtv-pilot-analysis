# =============================================================================
# R/09_tables.R - Formatted Analytical Tables
# =============================================================================
# Input:   03_cleaning_log.rds, 05_balance.rds, 06_models.rds,
#          06_bootstrap.rds, 06_threshold.rds, 07_subgroups.rds
# Output:  output/tables/table01_sample_summary.csv
#          output/tables/table02_balance_check.csv
#          output/tables/table03_primary_results.csv
#          output/tables/table04_bootstrap_comparison.csv
#          output/tables/table05_threshold_crossings.csv
#          output/tables/table06_subgroup_results.csv
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("09_tables.R")
step_banner(9, "Tables")

# -- Load all inputs -----------------------------------------------------------
cleaning_log    <- load_rds("cleaning_log")
balance_out     <- load_rds("balance")
models_out      <- load_rds("models")
bootstrap_tbl   <- load_rds("bootstrap")
threshold_tbl   <- load_rds("threshold")
subgroup_tbl    <- load_rds("subgroups")

save_table <- function(df, filename, label) {
  path <- file.path(PATHS$tables, filename)
  write.csv(df, path, row.names = FALSE)
  log_msg(sprintf("TABLE   %-35s -> output/tables/%s", label, filename))
  invisible(path)
}

# =============================================================================
# TABLE 01: Sample Summary
# =============================================================================
subsection("Table 01: Sample and cleaning summary")

t01 <- data.frame(
  Metric = c(
    "Raw dataset rows",
    "After duplicate removal",
    "FCS values excluded (>112 or <0)",
    "HDDS values excluded (>12 or <0)",
    "Analysis dataset rows",
    "Unique households",
    "Pilot households (12 villages)",
    "Comparison households (8 villages)",
    "Income winsorization threshold (USD)",
    "Attrition verdict"
  ),
  Value = c(
    cleaning_log$n_raw,
    cleaning_log$n_post_dedup,
    cleaning_log$n_fcs_flagged,
    cleaning_log$n_hdds_flagged,
    cleaning_log$n_clean,
    cleaning_log$n_hh,
    cleaning_log$n_hh_pilot,
    cleaning_log$n_hh_comp,
    round(cleaning_log$p99_income, 2),
    cleaning_log$att_verdict
  ),
  stringsAsFactors = FALSE
)
print(t01, row.names = FALSE)


# =============================================================================
# TABLE 02: Baseline Balance
# =============================================================================
subsection("Table 02: Baseline covariate balance")

t02 <- balance_out$balance_tbl |>
  dplyr::rename(
    Variable        = variable,
    `Mean (Pilot)`  = mean_pilot,
    `Mean (Comp.)`  = mean_comparison,
    Difference      = difference,
    `p-value`       = p_value,
    `In Model`      = in_model
  )
print(t02, row.names = FALSE)


# =============================================================================
# TABLE 03: Primary DiD Results
# =============================================================================
subsection("Table 03: Primary DiD results")

t03 <- models_out$primary_results |>
  dplyr::rename(
    Outcome         = outcome,
    Estimate        = estimate,
    `Std. Error`    = std.error,
    `p-value`       = p_display,
    `95% CI Lower`  = conf.low,
    `95% CI Upper`  = conf.high,
    Significance    = sig
  )
print(t03, row.names = FALSE)


# =============================================================================
# TABLE 04: Bootstrap Comparison
# =============================================================================
subsection("Table 04: CR2 vs wild bootstrap p-values")

t04 <- bootstrap_tbl |>
  dplyr::rename(
    Outcome           = outcome,
    `DiD Estimate`    = estimate,
    `CR2 p-value`     = cr2_p,
    `Bootstrap p`     = boot_p,
    `Observed t`      = observed_t,
    `Valid Iterations`= n_valid_its
  )
print(t04, row.names = FALSE)


# =============================================================================
# TABLE 05: FCS Threshold Crossings
# =============================================================================
subsection("Table 05: FCS food security category shifts")

t05 <- threshold_tbl |>
  dplyr::rename(
    `Food Security Band` = band,
    `Baseline (%)`       = baseline_pct,
    `Endline (%)`        = endline_pct,
    `Change (pp)`        = change_pp
  )
print(t05, row.names = FALSE)


# =============================================================================
# TABLE 06: Subgroup Results
# =============================================================================
subsection("Table 06: Subgroup DiD results with BH correction")

t06 <- subgroup_tbl |>
  dplyr::rename(
    Subgroup              = outcome,
    `DiD Estimate`        = estimate,
    `Std. Error`          = std.error,
    `p (unadjusted)`      = p_unadj,
    `Sig. (unadjusted)`   = sig_unadj,
    `p (BH-adjusted)`     = p_BH,
    `Sig. (BH-adjusted)`  = sig_BH,
    `95% CI Lower`        = conf.low,
    `95% CI Upper`        = conf.high
  )
print(t06, row.names = FALSE)

master_workbook_data <- list(
  "Sample Summary"   = t01,
  "Baseline Balance" = t02,
  "Primary DiD"      = t03,
  "Bootstrap Check"  = t04,
  "FCS Thresholds"   = t05,
  "Subgroup Results" = t06
)

export_path <- file.path(PATHS$tables, "RTV_Pilot_Master_Tables.xlsx")

openxlsx::write.xlsx(
  x = master_workbook_data,
  file = export_path,
  asTable = TRUE,
  withFilter = TRUE,
  tableStyle = "TableStyleMedium2",
  overwrite = TRUE
)

log_msg("09_tables.R complete. Run 10_sensitivity.R next.")
