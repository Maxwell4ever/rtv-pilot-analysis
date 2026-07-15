# =============================================================================
# R/03_cleaning.R - Apply Cleaning Decisions + Attrition Analysis
# =============================================================================
# Input:   data/processed/01_raw_data.rds
#          data/processed/02_validation_flags.rds
# Output:  data/processed/03_df_clean.rds
#          data/processed/03_cleaning_log.rds
#          data/processed/analysis_dataset.csv    (human-readable copy)
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("03_cleaning.R")
step_banner(3, "Data Cleaning")

df_raw <- load_rds("raw_data")
flags  <- load_rds("validation")
p99    <- flags$p99_income

# =============================================================================
# APPLY FIVE CLEANING DECISIONS
# =============================================================================
section("Applying Cleaning Decisions")

# D1: Remove duplicates
log_msg(sprintf("D1: Removing %d duplicate hhid-round rows", nrow(flags$dups)))
df <- df_raw |> dplyr::distinct(hhid, round, .keep_all = TRUE)
log_msg(sprintf("    Rows after deduplication: %d", nrow(df)))

# D2 & D3: Flag impossible values (do not drop yet - preserve count for memo)
log_msg(sprintf("D2: Flagging %d rows FCS > 112 or < 0", nrow(flags$bad_fcs)))
log_msg(sprintf("D3: Flagging %d rows HDDS > 12 or < 0", nrow(flags$bad_hdds)))
df <- df |>
  dplyr::mutate(
    fcs_flag  = as.integer(fcs  > ANALYSIS$fcs_max  | fcs  < 0),
    hdds_flag = as.integer(hdds > ANALYSIS$hdds_max | hdds < 0)
  )

# D4: Winsorize income
log_msg(sprintf("D4: Winsorizing income at USD %.2f (p99)", p99))
df <- df |> dplyr::mutate(income_w = pmin(monthly_income_usd, p99))

# D5: DiD structural variables
log_msg("D5: Creating post and treat_x_post interaction terms")
df <- df |>
  dplyr::mutate(
    post         = as.integer(round == 1),
    treat_x_post = treatment * post
  )

# Analysis-ready dataset
df_clean <- df |> dplyr::filter(fcs_flag == 0, hdds_flag == 0)

# =============================================================================
# ATTRITION ANALYSIS (Fix 4 from peer review)
# =============================================================================
section("Attrition Analysis")

bl_ids  <- df_clean$hhid[df_clean$round == 0]
el_ids  <- df_clean$hhid[df_clean$round == 1]
att_ids <- setdiff(el_ids, bl_ids)

df_att  <- df_clean |>
  dplyr::filter(round == 1) |>
  dplyr::mutate(attrited = as.integer(hhid %in% att_ids))

att_grp <- df_att |>
  dplyr::group_by(treatment) |>
  dplyr::summarise(
    n_total      = dplyr::n(),
    n_attrited   = sum(attrited),
    pct_attrited = round(mean(attrited) * 100, 1),
    .groups      = "drop"
  ) |>
  dplyr::mutate(group = ifelse(treatment == 1, "Pilot", "Comparison"))

for (i in seq_len(nrow(att_grp))) {
  log_msg(sprintf("  %-12s  %d HH  |  %d attrited  (%.1f%%)",
                  att_grp$group[i], att_grp$n_total[i],
                  att_grp$n_attrited[i], att_grp$pct_attrited[i]))
}

# Chi-squared test
mat <- matrix(
  c(att_grp$n_attrited[att_grp$treatment==1],
    att_grp$n_total[att_grp$treatment==1] -
      att_grp$n_attrited[att_grp$treatment==1],
    att_grp$n_attrited[att_grp$treatment==0],
    att_grp$n_total[att_grp$treatment==0] -
      att_grp$n_attrited[att_grp$treatment==0]),
  nrow = 2,
  dimnames = list(c("Attrited","Retained"), c("Pilot","Comparison"))
)
chi   <- stats::chisq.test(mat)
att_v <- ifelse(chi$p.value > 0.10, "BALANCED", "UNBALANCED - investigate")

log_msg(sprintf("Chi-sq test: chi2=%.3f  df=%d  p=%s  VERDICT: %s",
                chi$statistic, chi$parameter, fmt_p(chi$p.value), att_v))

# =============================================================================
# CLEANING LOG
# =============================================================================
cleaning_log <- list(
  n_raw          = nrow(df_raw),
  n_post_dedup   = nrow(df),
  n_fcs_flagged  = sum(df$fcs_flag),
  n_hdds_flagged = sum(df$hdds_flag),
  n_clean        = nrow(df_clean),
  n_hh           = length(unique(df_clean$hhid)),
  n_hh_pilot     = length(unique(df_clean$hhid[df_clean$treatment == 1])),
  n_hh_comp      = length(unique(df_clean$hhid[df_clean$treatment == 0])),
  p99_income     = p99,
  att_grp        = att_grp,
  att_chi_p      = chi$p.value,
  att_verdict    = att_v
)

section("Cleaning Log")
log_msg(sprintf("  Raw rows             : %d", cleaning_log$n_raw))
log_msg(sprintf("  Post-deduplication   : %d", cleaning_log$n_post_dedup))
log_msg(sprintf("  FCS flags excluded   : %d", cleaning_log$n_fcs_flagged))
log_msg(sprintf("  HDDS flags excluded  : %d", cleaning_log$n_hdds_flagged))
log_msg(sprintf("  Analysis rows        : %d", cleaning_log$n_clean))
log_msg(sprintf("  Unique HH            : %d", cleaning_log$n_hh))
log_msg(sprintf("  Pilot HH             : %d", cleaning_log$n_hh_pilot))
log_msg(sprintf("  Comparison HH        : %d", cleaning_log$n_hh_comp))
log_msg(sprintf("  Income winsor at USD : %.2f", cleaning_log$p99_income))
log_msg(sprintf("  Attrition verdict    : %s", cleaning_log$att_verdict))

# -- Save hand-offs ------------------------------------------------------------
save_rds(df_clean,    "df_clean",    "df_clean (analysis dataset)")
save_rds(cleaning_log,"cleaning_log","cleaning_log")

# Human-readable CSV in data/processed/ for inspection
csv_path <- data_path("processed", "analysis_dataset.csv")
write.csv(df_clean, csv_path, row.names = FALSE)
log_msg(sprintf("CSV     analysis_dataset.csv  -> data/processed/"))

log_msg("03_cleaning.R complete. Run 04_descriptive.R next.")
