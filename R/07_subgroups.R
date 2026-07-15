# =============================================================================
# R/07_subgroups.R - Subgroup Analysis (BH Correction, Fixed Terciles)
# =============================================================================
# Input:   data/processed/03_df_clean.rds
# Output:  data/processed/07_df_sub.rds
#          data/processed/07_subgroups.rds
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("07_subgroups.R")
step_banner(7, "Subgroup Analysis")

df_clean <- load_rds("df_clean")

# =============================================================================
# BUILD df_sub - corrected tercile assignment (Fix 6 from peer review)
# =============================================================================
section("Building Subgroup Dataset - Fixed Tercile Assignment")

log_msg("Fix 6: ntile() computed on UNIQUE baseline observations only.")
log_msg("       Joining tercile flag onto full panel as many-to-one.")

# Step 1: one row per HH at baseline
bl_lookup <- df_clean |>
  dplyr::filter(round == 0) |>
  dplyr::select(hhid, baseline_fcs = fcs) |>
  dplyr::distinct(hhid, .keep_all = TRUE)

# Step 2: tercile cutpoints on UNIQUE baseline distribution
tercile_lookup <- bl_lookup |>
  dplyr::mutate(poorest_hh = as.integer(dplyr::ntile(baseline_fcs, 3) == 1))

cuts <- quantile(bl_lookup$baseline_fcs, c(0, 1/3, 2/3, 1), na.rm=TRUE)
log_msg(sprintf("Tercile cutpoints (N=%d unique baseline HH):", nrow(bl_lookup)))
log_msg(sprintf("  Bottom third: FCS <= %.2f", cuts[2]))
log_msg(sprintf("  Middle third: %.2f < FCS <= %.2f", cuts[2], cuts[3]))
log_msg(sprintf("  Upper third:  FCS > %.2f", cuts[3]))

# Step 3: join baseline_fcs and poorest_hh onto full panel (many-to-one)
df_sub <- df_clean |>
  dplyr::left_join(bl_lookup,
                   by = "hhid", relationship = "many-to-one") |>
  dplyr::filter(!is.na(baseline_fcs)) |>
  dplyr::left_join(tercile_lookup |> dplyr::select(hhid, poorest_hh),
                   by = "hhid", relationship = "many-to-one")

# Verification
mismatch <- df_sub |>
  dplyr::filter(round == 0) |>
  dplyr::summarise(n = sum(abs(fcs - baseline_fcs) > 0.001)) |>
  dplyr::pull(n)

log_msg(sprintf("Verification: baseline_fcs mismatches = %d  (expected 0)", mismatch))
log_msg(sprintf("Households in df_sub: %d  | dropped: %d",
                length(unique(df_sub$hhid)),
                length(unique(df_clean$hhid)) - length(unique(df_sub$hhid))))

# =============================================================================
# SUBGROUP DiD MODELS (separate models - avoids three-way interaction issues)
# =============================================================================
section("Subgroup DiD Regressions (FCS outcome)")

subgroup_configs <- list(
  list(label="Female-headed households",               expr="female_head==1"),
  list(label="Male-headed households",                 expr="female_head==0"),
  list(label="Poorest tercile (lowest baseline FCS)",  expr="poorest_hh==1"),
  list(label="Upper two terciles",                     expr="poorest_hh==0")
)

sub_rows <- lapply(subgroup_configs, function(cfg) {
  sub_data <- df_sub |> dplyr::filter(!!rlang::parse_expr(cfg$expr))
  n_hh <- length(unique(sub_data$hhid))
  n_cl <- length(unique(sub_data[[ANALYSIS$cluster_var]]))
  log_msg(sprintf("  Running: %-40s  (N=%d HH, %d clusters)",
                  cfg$label, n_hh, n_cl))
  run_did("fcs", cfg$label, sub_data)$coef
})

sub_coefs <- dplyr::bind_rows(sub_rows)

# =============================================================================
# BENJAMINI-HOCHBERG CORRECTION (Fix 3 from peer review)
# =============================================================================
section("Benjamini-Hochberg Multiple Comparisons Correction")

subgroup_tbl <- sub_coefs |>
  dplyr::mutate(
    estimate      = round(estimate,  2),
    std.error     = round(std.error, 2),
    conf.low      = round(conf.low,  2),
    conf.high     = round(conf.high, 2),
    p_unadj       = fmt_p(p.value),
    p_BH          = fmt_p(p.adjust(p.value, method = ANALYSIS$bh_method)),
    sig_unadj     = sig_stars(p.value),
    sig_BH        = sig_stars(p.adjust(p.value, method = ANALYSIS$bh_method))
  ) |>
  dplyr::select(outcome, estimate, std.error,
                p_unadj, sig_unadj, p_BH, sig_BH, conf.low, conf.high)

sig_b  <- sum(sub_coefs$p.value < ANALYSIS$alpha)
sig_bh <- sum(p.adjust(sub_coefs$p.value,
                        method=ANALYSIS$bh_method) < ANALYSIS$alpha)
log_msg(sprintf("Significant before BH: %d of %d", sig_b,  nrow(sub_coefs)))
log_msg(sprintf("Significant after  BH: %d of %d", sig_bh, nrow(sub_coefs)))

for (i in seq_len(nrow(subgroup_tbl))) {
  r <- subgroup_tbl[i,]
  log_msg(sprintf("  %-42s  %+.2f  p(unadj)=%s %s  p(BH)=%s %s",
                  r$outcome, r$estimate,
                  r$p_unadj, r$sig_unadj, r$p_BH, r$sig_BH))
}

# -- Save ----------------------------------------------------------------------
save_rds(df_sub,       "df_sub",    "df_sub (subgroup panel)")
save_rds(subgroup_tbl, "subgroups", "subgroup results (BH-corrected)")
log_msg("07_subgroups.R complete. Run 08_figures.R next.")
