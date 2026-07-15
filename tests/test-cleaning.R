# =============================================================================
# tests/test-cleaning.R
# =============================================================================
if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")),
  "..", "R", "utils.R"))
if (!exists("ANALYSIS")) source(file.path(PROJ_ROOT, "R", "00_packages.R"))

cat("\n--- Running tests/test-cleaning.R ---\n")

stopifnot("03_df_clean.rds must exist" = file.exists(PROC$df_clean))
df <- readRDS(PROC$df_clean)
cl <- readRDS(PROC$cleaning_log)

# No duplicates in analysis dataset
dups <- df |>
  dplyr::count(hhid, round) |>
  dplyr::filter(n > 1)
stopifnot("Analysis dataset must have no duplicate hhid-round rows" =
            nrow(dups) == 0)

# No impossible FCS values
stopifnot("No FCS values > 112 in df_clean" = all(df$fcs <= 112, na.rm=TRUE))
stopifnot("No FCS values < 0 in df_clean"   = all(df$fcs >= 0,   na.rm=TRUE))

# No impossible HDDS values
stopifnot("No HDDS values > 12 in df_clean" = all(df$hdds <= 12, na.rm=TRUE))
stopifnot("No HDDS values < 0 in df_clean"  = all(df$hdds >= 0,  na.rm=TRUE))

# income_w must not exceed p99
stopifnot("income_w must not exceed p99 threshold" =
            all(df$income_w <= cl$p99_income + 0.01, na.rm=TRUE))

# DiD variables created
stopifnot("post column must exist"         = "post"         %in% names(df))
stopifnot("treat_x_post column must exist" = "treat_x_post" %in% names(df))
stopifnot("post = round for all rows"      =
            all(df$post == df$round))
stopifnot("treat_x_post = treatment * post" =
            all(df$treat_x_post == df$treatment * df$post))

# Sample size bounds
stopifnot("Must have between 1260 and 1280 rows" =
            nrow(df) >= 1260 & nrow(df) <= 1280)
stopifnot("Must have between 630 and 640 unique HH" =
            cl$n_hh >= 630 & cl$n_hh <= 640)

# Attrition must be balanced
stopifnot("Attrition verdict must be BALANCED" =
            cl$att_verdict == "BALANCED")

cat("All test-cleaning tests PASSED.\n")
