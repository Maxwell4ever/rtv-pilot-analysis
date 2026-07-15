# =============================================================================
# tests/test-models.R
# =============================================================================
if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")),
  "..", "R", "utils.R"))
if (!exists("ANALYSIS")) source(file.path(PROJ_ROOT, "R", "00_packages.R"))

cat("\n--- Running tests/test-models.R ---\n")

stopifnot("06_models.rds must exist"   = file.exists(PROC$models))
stopifnot("06_bootstrap.rds must exist"= file.exists(PROC$bootstrap))

models_out  <- readRDS(PROC$models)
boot_tbl    <- readRDS(PROC$bootstrap)
primary     <- models_out$primary_results

# Three outcomes present
stopifnot("primary_results must have 3 rows" = nrow(primary) == 3)

# FCS estimate must be positive and in plausible range
fcs_est <- primary$estimate[grepl("FCS", primary$outcome)]
stopifnot("FCS DiD estimate must be positive"     = fcs_est > 0)
stopifnot("FCS DiD estimate must be plausible (< 30)" = fcs_est < 30)

# HDDS estimate must be positive
hdds_est <- primary$estimate[grepl("HDDS", primary$outcome)]
stopifnot("HDDS DiD estimate must be positive" = hdds_est > 0)

# FCS and HDDS must be significant (p < 0.05)
fcs_p  <- primary$p_display[grepl("FCS",  primary$outcome)]
hdds_p <- primary$p_display[grepl("HDDS", primary$outcome)]
stopifnot("FCS p-value must be < 0.001"  = fcs_p  == "< 0.001")
stopifnot("HDDS p-value must be < 0.001" = hdds_p == "< 0.001")

# Bootstrap table must have 3 rows
stopifnot("bootstrap_tbl must have 3 rows" = nrow(boot_tbl) == 3)

# Bootstrap iterations must all exceed 1900 (few failures expected)
stopifnot("All bootstrap runs must have > 1900 valid iterations" =
            all(boot_tbl$n_valid_its > 1900))

# CIs must exclude zero for FCS and HDDS
fcs_row  <- primary[grepl("FCS",  primary$outcome), ]
hdds_row <- primary[grepl("HDDS", primary$outcome), ]
stopifnot("FCS 95% CI must exclude zero" =
            fcs_row$conf.low > 0 & fcs_row$conf.high > 0)
stopifnot("HDDS 95% CI must exclude zero" =
            hdds_row$conf.low > 0 & hdds_row$conf.high > 0)

# Subgroup results
stopifnot("07_subgroups.rds must exist" = file.exists(PROC$subgroups))
sg <- readRDS(PROC$subgroups)
stopifnot("Subgroup table must have 4 rows" = nrow(sg) == 4)

# Female-headed estimate must exceed male-headed
fem_est  <- sg$estimate[grepl("Female", sg$outcome)]
male_est <- sg$estimate[grepl("Male",   sg$outcome)]
stopifnot("Female-headed DiD must exceed male-headed DiD" =
            fem_est > male_est)

# All subgroup BH-adjusted p-values must be significant
bh_ps <- as.numeric(gsub("< ", "", sg$p_BH))
stopifnot("All subgroup findings must survive BH correction" =
            all(bh_ps < 0.05 | sg$p_BH == "< 0.001"))

cat("All test-models tests PASSED.\n")
