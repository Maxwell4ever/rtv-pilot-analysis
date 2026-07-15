# =============================================================================
# R/04_descriptive.R - Descriptive Statistics
# =============================================================================
# Input:   data/processed/03_df_clean.rds
# Output:  data/processed/04_descriptives.rds
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("04_descriptive.R")
step_banner(4, "Descriptive Statistics")

df_clean <- load_rds("df_clean")
baseline <- df_clean |> dplyr::filter(round == 0)
endline  <- df_clean |> dplyr::filter(round == 1)

# -- Outcome means by group and round ------------------------------------------
section("Means by Treatment Group and Round")

means_tbl <- df_clean |>
  dplyr::group_by(treatment, round) |>
  dplyr::summarise(
    n        = dplyr::n(),
    mean_fcs = round(mean(fcs,         na.rm = TRUE), 2),
    sd_fcs   = round(sd(fcs,           na.rm = TRUE), 2),
    mean_hdds= round(mean(hdds,        na.rm = TRUE), 2),
    mean_asset=round(mean(asset_index, na.rm = TRUE), 3),
    .groups  = "drop"
  ) |>
  dplyr::mutate(
    group  = ifelse(treatment == 1, "Pilot", "Comparison"),
    period = ifelse(round == 0, "Baseline", "Endline")
  )

for (i in seq_len(nrow(means_tbl))) {
  r <- means_tbl[i, ]
  log_msg(sprintf("  %-12s %-9s  n=%-4d  FCS=%.2f (SD %.2f)  HDDS=%.2f  Asset=%.3f",
                  r$group, r$period, r$n, r$mean_fcs, r$sd_fcs,
                  r$mean_hdds, r$mean_asset))
}

# -- Naive DiD (pre-regression quick check) ------------------------------------
section("Naive DiD (unadjusted means, for orientation only)")

naive <- function(var) {
  g <- function(t, r) mean(df_clean[[var]][df_clean$treatment==t &
                                             df_clean$round==r], na.rm=TRUE)
  pilot_diff <- g(1,1) - g(1,0)
  comp_diff  <- g(0,1) - g(0,0)
  list(pilot=pilot_diff, comp=comp_diff, did=pilot_diff-comp_diff)
}

for (o in ANALYSIS$outcomes) {
  n <- naive(o$var)
  log_msg(sprintf("  %-40s  pilot trend=%+.2f  comp trend=%+.2f  naive DiD=%+.2f",
                  o$label, n$pilot, n$comp, n$did))
}

# -- FCS category distribution --------------------------------------------------
section("FCS Food Security Category Distribution")

fcs_cats <- function(d, label) {
  data.frame(
    group      = label,
    poor       = round(mean(d$fcs < ANALYSIS$fcs_poor, na.rm=TRUE)*100, 1),
    borderline = round(mean(d$fcs >= ANALYSIS$fcs_poor &
                              d$fcs <= ANALYSIS$fcs_borderline, na.rm=TRUE)*100, 1),
    acceptable = round(mean(d$fcs > ANALYSIS$fcs_borderline, na.rm=TRUE)*100, 1)
  )
}

cat_tbl <- rbind(
  fcs_cats(baseline[baseline$treatment==1,], "Pilot Baseline"),
  fcs_cats(endline [endline$treatment==1, ], "Pilot Endline"),
  fcs_cats(baseline[baseline$treatment==0,], "Comparison Baseline"),
  fcs_cats(endline [endline$treatment==0, ], "Comparison Endline")
)

for (i in seq_len(nrow(cat_tbl))) {
  r <- cat_tbl[i,]
  log_msg(sprintf("  %-22s  Poor=%.1f%%  Borderline=%.1f%%  Acceptable=%.1f%%",
                  r$group, r$poor, r$borderline, r$acceptable))
}

# -- Save ----------------------------------------------------------------------
descriptives <- list(means_tbl=means_tbl, cat_tbl=cat_tbl)
save_rds(descriptives, "descriptives", "descriptive statistics")
log_msg("04_descriptive.R complete. Run 05_balance_checks.R next.")
