# =============================================================================
# R/05_balance_checks.R - Baseline Balance and Parallel Trends Assessment
# =============================================================================
# Input:   data/processed/03_df_clean.rds
# Output:  data/processed/05_balance.rds
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("05_balance_checks.R")
step_banner(5, "Baseline Balance and Parallel Trends")

df_clean <- load_rds("df_clean")
baseline <- df_clean |> dplyr::filter(round == 0)

# =============================================================================
# BALANCE CHECK - t-tests at baseline
# =============================================================================
section("Covariate Balance at Baseline")

bal_vars <- c("fcs", "hdds", "asset_index", "income_w",
              "hh_size", "female_head", "age_head", "land_ha")

balance_rows <- lapply(bal_vars, function(v) {
  p <- baseline[[v]][baseline$treatment == 1]
  c <- baseline[[v]][baseline$treatment == 0]
  tt <- t.test(p, c)
  data.frame(
    variable       = v,
    mean_pilot     = round(mean(p, na.rm=TRUE), 3),
    mean_comparison= round(mean(c, na.rm=TRUE), 3),
    difference     = round(mean(p,na.rm=TRUE) - mean(c,na.rm=TRUE), 3),
    p_value        = fmt_p(tt$p.value),
    in_model       = v %in% ANALYSIS$covariates,
    stringsAsFactors = FALSE
  )
})

balance_tbl <- do.call(rbind, balance_rows)

for (i in seq_len(nrow(balance_tbl))) {
  r <- balance_tbl[i,]
  flag <- if (r$p_value != "ns" && !r$in_model) " *** ADD TO MODEL" else ""
  log_msg(sprintf("  %-14s  pilot=%.3f  comp=%.3f  diff=%+.3f  p=%s%s",
                  r$variable, r$mean_pilot, r$mean_comparison,
                  r$difference, r$p_value, flag))
}

# =============================================================================
# PARALLEL TRENDS - three robustness arguments
# =============================================================================
section("Parallel Trends Assessment")

# Argument 1: Secular trend in comparison group
comp_trend <- df_clean |>
  dplyr::filter(treatment == 0) |>
  dplyr::group_by(round) |>
  dplyr::summarise(mean_fcs = mean(fcs, na.rm=TRUE), .groups="drop")
secular <- diff(comp_trend$mean_fcs)

log_msg(sprintf("Argument 1: Secular FCS trend in comparison = %+.2f pts", secular))
log_msg(if(abs(secular) < 5) "  -> Small secular trend supports parallel trends." else
  "  -> Large secular trend - interpret DiD with caution.")

# Argument 2: Falsification test on household size (time-invariant)
falsi <- run_did("hh_size", "Household Size (falsification)", df_clean,
                 covariates = setdiff(ANALYSIS$covariates, "hh_size"))
f_co  <- falsi$coef
log_msg(sprintf("Argument 2: Falsification DiD (hh_size) = %+.4f  p = %s",
                f_co$estimate, fmt_p(f_co$p.value)))
log_msg(if(f_co$p.value > 0.10) "  -> Null result. Consistent with valid design." else
  "  -> WARNING: Significant result on time-invariant variable.")

# Argument 3: Baseline covariate balance
n_imbal <- sum(
  sapply(bal_vars, function(v) {
    p  <- baseline[[v]][baseline$treatment==1]
    cc <- baseline[[v]][baseline$treatment==0]
    t.test(p,cc)$p.value < 0.10
  })
)
log_msg(sprintf("Argument 3: %d of %d baseline vars show imbalance (p<0.10).",
                n_imbal, length(bal_vars)))
log_msg(if(n_imbal <= 2) "  -> Imbalanced vars already in model covariates." else
  "  -> Review covariate list in ANALYSIS$covariates.")

pt_verdict <- all(abs(secular) < 5, f_co$p.value > 0.10, n_imbal <= 3)
log_msg(sprintf("Overall parallel trends: %s",
                if(pt_verdict) "SUPPORTED" else "UNCERTAIN - review above"))

# -- Save ----------------------------------------------------------------------
balance_out <- list(
  balance_tbl      = balance_tbl,
  secular_trend    = secular,
  falsification    = f_co,
  n_imbalanced     = n_imbal,
  pt_verdict       = pt_verdict
)
save_rds(balance_out, "balance", "balance and parallel trends")
log_msg("05_balance_checks.R complete. Run 06_models.R next.")
