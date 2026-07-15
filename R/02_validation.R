# =============================================================================
# R/02_validation.R - Data Quality Diagnostic Checks
# =============================================================================
# Input:   data/processed/01_raw_data.rds
# Output:  data/processed/02_validation_flags.rds
#
# Runs five diagnostic checks. Makes NO changes to data.
# Results are consumed by 03_cleaning.R to apply documented decisions.
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("02_validation.R")
step_banner(2, "Data Quality Validation")

df_raw <- load_rds("raw_data")

# =============================================================================
# CHECK 1: Duplicate hhid-round rows
# =============================================================================
subsection("CHECK 1: Duplicate hhid-round combinations")

dups <- df_raw |>
  dplyr::count(hhid, round) |>
  dplyr::filter(n > 1)

log_msg(sprintf("Duplicated hhid-round pairs: %d", nrow(dups)))
if (nrow(dups) > 0) {
  log_msg(sprintf("Affected hhid-round: %s",
                  paste(paste(dups$hhid, dups$round, sep=":"), collapse=", ")))
}
log_msg("DECISION: distinct(hhid, round, .keep_all=TRUE) - retain first occurrence")

# =============================================================================
# CHECK 2: Impossible FCS values
# =============================================================================
subsection("CHECK 2: FCS values outside 0 to 112")

bad_fcs <- df_raw |>
  dplyr::filter(fcs > ANALYSIS$fcs_max | fcs < 0)

log_msg(sprintf("FCS out of range: %d rows", nrow(bad_fcs)))
log_msg(sprintf("Values: %s", paste(sort(bad_fcs$fcs), collapse=", ")))
log_msg("DECISION: flag fcs_flag=1, exclude from analysis. Document N in memo.")

# =============================================================================
# CHECK 3: Impossible HDDS values
# =============================================================================
subsection("CHECK 3: HDDS values outside 0 to 12")

bad_hdds <- df_raw |>
  dplyr::filter(hdds > ANALYSIS$hdds_max | hdds < 0)

log_msg(sprintf("HDDS out of range: %d rows", nrow(bad_hdds)))
log_msg(sprintf("Values: %s", paste(sort(bad_hdds$hdds), collapse=", ")))
log_msg("DECISION: flag hdds_flag=1, exclude from analysis. Document N in memo.")

# =============================================================================
# CHECK 4: Extreme income outliers
# =============================================================================
subsection("CHECK 4: Income outliers (|z-score| > 3)")

inc   <- df_raw$monthly_income_usd[!is.na(df_raw$monthly_income_usd)]
p99   <- quantile(inc, ANALYSIS$winsor_pct)

outliers <- df_raw |>
  dplyr::filter(!is.na(monthly_income_usd)) |>
  dplyr::mutate(z = (monthly_income_usd - mean(inc)) / sd(inc)) |>
  dplyr::filter(abs(z) > 3) |>
  dplyr::select(hhid, round, monthly_income_usd, z) |>
  dplyr::mutate(z = round(z, 2))

log_msg(sprintf("Income outliers |z|>3: %d rows", nrow(outliers)))
log_msg(sprintf("Winsorization threshold (p99): USD %.2f", p99))
log_msg("DECISION: income_w = pmin(monthly_income_usd, p99). Used as covariate only.")

# =============================================================================
# CHECK 5: Missing values
# =============================================================================
subsection("CHECK 5: Missing values by variable")

miss <- df_raw |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(is.na(.)))) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to  = "variable",
                      values_to = "n_missing") |>
  dplyr::filter(n_missing > 0) |>
  dplyr::mutate(pct = round(n_missing / nrow(df_raw) * 100, 1)) |>
  dplyr::arrange(dplyr::desc(n_missing))

log_msg("Variables with missing data:")
for (i in seq_len(nrow(miss))) {
  log_msg(sprintf("  %-25s  %d rows  (%.1f%%)",
                  miss$variable[i], miss$n_missing[i], miss$pct[i]))
}
log_msg("DECISION: Listwise deletion for rows missing covariates (land_ha, income_w).")

# -- Summary -------------------------------------------------------------------
section("VALIDATION SUMMARY")
log_msg(sprintf("  Duplicate rows:        %d", nrow(dups)))
log_msg(sprintf("  FCS out of range:      %d", nrow(bad_fcs)))
log_msg(sprintf("  HDDS out of range:     %d", nrow(bad_hdds)))
log_msg(sprintf("  Income outliers:       %d", nrow(outliers)))
log_msg(sprintf("  Variables with NAs:    %d", nrow(miss)))
log_msg(sprintf("  p99 income threshold:  USD %.2f", p99))

# -- Save hand-off -------------------------------------------------------------
validation_flags <- list(
  dups      = dups,     bad_fcs   = bad_fcs,
  bad_hdds  = bad_hdds, outliers  = outliers,
  miss      = miss,     p99_income = p99
)

save_rds(validation_flags, "validation", "validation flags")
log_msg("02_validation.R complete. Run 03_cleaning.R next.")
