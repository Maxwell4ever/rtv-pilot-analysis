# =============================================================================
# R/06_models.R - Primary DiD Regressions + Wild Bootstrap Validation
# =============================================================================
# Input:   data/processed/03_df_clean.rds
#          data/processed/05_balance.rds
# Output:  data/processed/06_models.rds
#          data/processed/06_bootstrap.rds
#          data/processed/06_threshold.rds
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("06_models.R")
step_banner(6, "Primary DiD Regressions and Bootstrap")

df_clean <- load_rds("df_clean")
balance  <- load_rds("balance")

if (!balance$pt_verdict)
  log_msg("WARNING: Parallel trends not fully supported. Interpret with caution.")

# =============================================================================
# PRIMARY DiD - one model per outcome
# =============================================================================
section("Primary DiD Regressions (CR2 cluster-robust SEs)")
log_msg(paste("Covariates:", paste(ANALYSIS$covariates, collapse=", ")))
log_msg(paste("Cluster:   ", ANALYSIS$cluster_var, "| SE type:", ANALYSIS$se_type))

model_list <- lapply(ANALYSIS$outcomes, function(o) {
  run_did(o$var, o$label, df_clean)
})

primary_results <- dplyr::bind_rows(lapply(model_list, `[[`, "coef")) |>
  dplyr::mutate(
    estimate  = round(estimate,  3),
    std.error = round(std.error, 3),
    conf.low  = round(conf.low,  3),
    conf.high = round(conf.high, 3),
    p_display = fmt_p(p.value),
    sig       = sig_stars(p.value)
  ) |>
  dplyr::select(outcome, estimate, std.error,
                p_display, conf.low, conf.high, sig)

for (i in seq_len(nrow(primary_results))) {
  r <- primary_results[i,]
  log_msg(sprintf("  %-40s  %+.3f (SE=%.3f)  p=%s  95%%CI[%.3f, %.3f]  %s",
                  r$outcome, r$estimate, r$std.error, r$p_display,
                  r$conf.low, r$conf.high, r$sig))
}

# =============================================================================
# WILD CLUSTER BOOTSTRAP (Fix 2 - validates CR2 with 20 clusters)
# =============================================================================
section(sprintf("Wild Cluster Bootstrap (B=%d, seed=%d)",
                ANALYSIS$bootstrap_B, ANALYSIS$seed))

boot_rows <- lapply(names(ANALYSIS$outcomes), function(nm) {
  o    <- ANALYSIS$outcomes[[nm]]
  log_msg(sprintf("  Bootstrapping: %s ...", o$label))
  boot <- wild_bootstrap(o$var, model_list[[nm]], df_clean)
  cr2_p <- model_list[[nm]]$coef$p.value
  log_msg(sprintf("    CR2 p=%s  |  Bootstrap p=%s  |  t=%.3f",
                  fmt_p(cr2_p), boot$boot_p_fmt, boot$observed_t))
  data.frame(
    outcome     = o$label,
    estimate    = round(model_list[[nm]]$coef$estimate, 3),
    cr2_p       = fmt_p(cr2_p),
    boot_p      = boot$boot_p_fmt,
    observed_t  = round(boot$observed_t, 3),
    n_valid_its = boot$n_valid_its,
    stringsAsFactors = FALSE
  )
})
bootstrap_tbl <- do.call(rbind, boot_rows)

# =============================================================================
# FCS THRESHOLD CROSSINGS
# =============================================================================
section("FCS Food Security Category Shifts (pilot households)")

p_bl <- df_clean |> dplyr::filter(treatment==1, round==0)
p_el <- df_clean |> dplyr::filter(treatment==1, round==1)
cats <- function(d) c(
  poor       = mean(d$fcs <  ANALYSIS$fcs_poor,      na.rm=TRUE)*100,
  borderline = mean(d$fcs >= ANALYSIS$fcs_poor &
                     d$fcs <= ANALYSIS$fcs_borderline,na.rm=TRUE)*100,
  acceptable = mean(d$fcs >  ANALYSIS$fcs_borderline, na.rm=TRUE)*100
)
bl_c <- cats(p_bl); el_c <- cats(p_el)

threshold_tbl <- data.frame(
  band         = c(sprintf("Poor       (FCS < %d)",    ANALYSIS$fcs_poor),
                   sprintf("Borderline (FCS %d to %d)",ANALYSIS$fcs_poor,
                           ANALYSIS$fcs_borderline),
                   sprintf("Acceptable (FCS > %d)",    ANALYSIS$fcs_borderline)),
  baseline_pct = round(bl_c, 1),
  endline_pct  = round(el_c, 1),
  change_pp    = round(el_c - bl_c, 1),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(threshold_tbl))) {
  r <- threshold_tbl[i,]
  log_msg(sprintf("  %-40s  BL=%.1f%%  EL=%.1f%%  chg=%+.1f pp",
                  r$band, r$baseline_pct, r$endline_pct, r$change_pp))
}

# -- Save ----------------------------------------------------------------------
models_out <- list(model_list=model_list, primary_results=primary_results)
save_rds(models_out,    "models",    "DiD models and primary results")
save_rds(bootstrap_tbl, "bootstrap", "bootstrap results")
save_rds(threshold_tbl, "threshold", "FCS threshold crossings")
log_msg("06_models.R complete. Run 07_subgroups.R next.")
