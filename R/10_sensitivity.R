# =============================================================================
# R/10_sensitivity.R - Sensitivity Analyses
# =============================================================================
# Input:   data/processed/01_raw_data.rds
#          data/processed/03_df_clean.rds
#          data/processed/06_models.rds
# Output:  data/processed/10_sensitivity.rds
#          output/tables/table07_sensitivity.csv
#
# Sensitivity checks:
#   S1. Winsorization threshold - compare p95, p97, p99 (baseline = p99)
#   S2. Model specification - with and without covariates
#   S3. Alternative clustering - at district level vs village level
#   S4. Restricted balanced panel - drop HH missing either round
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("10_sensitivity.R")
step_banner(10, "Sensitivity Analyses")

df_raw   <- load_rds("raw_data")
df_clean <- load_rds("df_clean")
models   <- load_rds("models")

base_est <- models$primary_results$estimate[
  models$primary_results$outcome ==
    ANALYSIS$outcomes$fcs$label]
base_p   <- models$primary_results$p_display[
  models$primary_results$outcome ==
    ANALYSIS$outcomes$fcs$label]

log_msg(sprintf("Baseline FCS DiD estimate: %+.3f  p=%s", base_est, base_p))

sensitivity_rows <- list()

# =============================================================================
# S1: WINSORIZATION THRESHOLD
# =============================================================================
section("S1: Winsorization Threshold Sensitivity (FCS outcome)")

for (pct in c(0.95, 0.97, 0.99)) {
  p_cut  <- quantile(df_raw$monthly_income_usd, pct, na.rm=TRUE)
  df_s1  <- df_raw |>
    dplyr::distinct(hhid, round, .keep_all=TRUE) |>
    dplyr::mutate(
      fcs_flag  = as.integer(fcs  > ANALYSIS$fcs_max  | fcs  < 0),
      hdds_flag = as.integer(hdds > ANALYSIS$hdds_max | hdds < 0),
      income_w  = pmin(monthly_income_usd, p_cut),
      post      = as.integer(round==1),
      treat_x_post = treatment * post
    ) |>
    dplyr::filter(fcs_flag==0, hdds_flag==0)

  m <- run_did("fcs", "FCS", df_s1)
  co <- m$coef
  log_msg(sprintf("  p%-3.0f winsor (USD %.0f)  est=%+.3f  p=%s",
                  pct*100, p_cut, co$estimate, fmt_p(co$p.value)))

  sensitivity_rows[[paste0("S1_p",pct*100)]] <- data.frame(
    check       = paste0("S1: Income winsorized at p", pct*100),
    outcome     = "FCS",
    estimate    = round(co$estimate, 3),
    p_value     = fmt_p(co$p.value),
    note        = sprintf("Threshold USD %.0f", p_cut),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# S2: MODEL SPECIFICATION
# =============================================================================
section("S2: Model Specification (FCS outcome)")

# S2a: No covariates (unconditional DiD)
m_nocov <- estimatr::lm_robust(
  fcs ~ treatment + post + treat_x_post,
  data     = df_clean,
  clusters = df_clean[[ANALYSIS$cluster_var]],
  se_type  = ANALYSIS$se_type
)
co_nocov <- broom::tidy(m_nocov) |> dplyr::filter(term=="treat_x_post")
log_msg(sprintf("  No covariates (unconditional):  est=%+.3f  p=%s",
                co_nocov$estimate, fmt_p(co_nocov$p.value)))

sensitivity_rows[["S2a"]] <- data.frame(
  check="S2a: No covariates (unconditional DiD)", outcome="FCS",
  estimate=round(co_nocov$estimate, 3), p_value=fmt_p(co_nocov$p.value),
  note="All covariates dropped", stringsAsFactors=FALSE
)

# S2b: Add income_w as additional covariate
covs_plus_income <- c(ANALYSIS$covariates, "income_w")
m_plus <- run_did("fcs", "FCS", df_clean, covariates=covs_plus_income)
co_plus <- m_plus$coef
log_msg(sprintf("  + income_w covariate:           est=%+.3f  p=%s",
                co_plus$estimate, fmt_p(co_plus$p.value)))

sensitivity_rows[["S2b"]] <- data.frame(
  check="S2b: Add income_w as covariate", outcome="FCS",
  estimate=round(co_plus$estimate, 3), p_value=fmt_p(co_plus$p.value),
  note="income_w added to baseline spec", stringsAsFactors=FALSE
)

# =============================================================================
# S3: BALANCED PANEL ONLY
# =============================================================================
section("S3: Restricted Balanced Panel (HH observed at both rounds)")

both_rounds <- df_clean |>
  dplyr::group_by(hhid) |>
  dplyr::summarise(n_rounds=dplyr::n_distinct(round), .groups="drop") |>
  dplyr::filter(n_rounds==2) |>
  dplyr::pull(hhid)

df_balanced <- df_clean |> dplyr::filter(hhid %in% both_rounds)
n_dropped   <- length(unique(df_clean$hhid)) - length(unique(df_balanced$hhid))
log_msg(sprintf("  Balanced panel: %d HH  |  dropped: %d HH with incomplete obs",
                length(unique(df_balanced$hhid)), n_dropped))

m_bal  <- run_did("fcs", "FCS", df_balanced)
co_bal <- m_bal$coef
log_msg(sprintf("  Balanced panel DiD:  est=%+.3f  p=%s",
                co_bal$estimate, fmt_p(co_bal$p.value)))

sensitivity_rows[["S3"]] <- data.frame(
  check="S3: Restricted balanced panel", outcome="FCS",
  estimate=round(co_bal$estimate, 3), p_value=fmt_p(co_bal$p.value),
  note=sprintf("N=%d HH (both rounds only)", length(unique(df_balanced$hhid))),
  stringsAsFactors=FALSE
)

# =============================================================================
# SENSITIVITY SUMMARY TABLE
# =============================================================================
section("Sensitivity Summary")

# Add baseline for comparison
baseline_row <- data.frame(
  check    = "BASELINE: Primary specification (p99 winsor, full covariates)",
  outcome  = "FCS",
  estimate = base_est,
  p_value  = base_p,
  note     = "Main result",
  stringsAsFactors = FALSE
)

sensitivity_tbl <- dplyr::bind_rows(
  baseline_row,
  do.call(rbind, sensitivity_rows)
)

for (i in seq_len(nrow(sensitivity_tbl))) {
  r <- sensitivity_tbl[i,]
  log_msg(sprintf("  %-50s  est=%+.3f  p=%s",
                  substr(r$check, 1, 50), as.numeric(r$estimate), r$p_value))
}

# Check: do any sensitivity checks change the significance conclusion?
main_sig <- base_p == "< 0.001" | as.numeric(gsub("< ", "", base_p)) < ANALYSIS$alpha
sens_sig <- sapply(sensitivity_tbl$p_value[-1], function(p) {
  p == "< 0.001" | suppressWarnings(as.numeric(gsub("< ","",p)) < ANALYSIS$alpha)
})

if (all(sens_sig)) {
  log_msg("VERDICT: FCS DiD remains significant across all sensitivity checks.")
} else {
  fail_checks <- sensitivity_tbl$check[-1][!sens_sig]
  log_msg(paste0("WARNING: Significance lost under: ",
                 paste(fail_checks, collapse="; ")))
}

# -- Save ----------------------------------------------------------------------
save_rds(sensitivity_tbl, "sensitivity", "sensitivity analysis results")

path <- file.path(PATHS$tables, "table07_sensitivity.csv")
write.csv(sensitivity_tbl, path, row.names=FALSE)
log_msg("TABLE   table07_sensitivity.csv -> output/tables/")

log_msg("10_sensitivity.R complete. Pipeline finished.")
