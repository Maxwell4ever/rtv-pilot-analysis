# =============================================================================
# tests/test-validation.R
# =============================================================================
if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")),
  "..", "R", "utils.R"))
if (!exists("ANALYSIS")) source(file.path(PROJ_ROOT, "R", "00_packages.R"))

cat("\n--- Running tests/test-validation.R ---\n")

stopifnot("02_validation_flags.rds must exist" =
            file.exists(PROC$validation))
flags <- readRDS(PROC$validation)

# Must detect expected number of issues
stopifnot("Must detect 2 duplicate rows"      = nrow(flags$dups)     == 2)
stopifnot("Must detect 6 bad FCS rows"        = nrow(flags$bad_fcs)  == 6)
stopifnot("Must detect 4 bad HDDS rows"       = nrow(flags$bad_hdds) == 4)
stopifnot("Must detect 3 income outliers"     = nrow(flags$outliers) == 3)
stopifnot("p99 income must be positive"       = flags$p99_income > 0)
stopifnot("p99 income must be below 1000"     = flags$p99_income < 1000)

# FCS flags must all be > 112
stopifnot("All flagged FCS values > 112" =
            all(flags$bad_fcs$fcs > 112))

# HDDS flags must all be > 12
stopifnot("All flagged HDDS values > 12" =
            all(flags$bad_hdds$hdds > 12))

cat("All test-validation tests PASSED.\n")
