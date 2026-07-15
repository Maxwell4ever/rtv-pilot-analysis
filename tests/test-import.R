# =============================================================================
# tests/test-import.R - Unit Tests for 01_import.R
# =============================================================================
# Run: source("tests/test-import.R")
# Checks that the raw data file loads correctly and has expected structure.
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")),
  "..", "R", "utils.R"))
if (!exists("ANALYSIS")) source(file.path(PROJ_ROOT, "R", "00_packages.R"))

cat("\n--- Running tests/test-import.R ---\n")

# Load raw data
raw_path <- PATHS$raw
stopifnot("Raw data file must exist" = file.exists(raw_path))
df <- read.csv(raw_path, stringsAsFactors = FALSE)

# Structure
stopifnot("Must have 13 columns"         = ncol(df) == 13)
stopifnot("Must have 1282 rows"          = nrow(df) == 1282)
stopifnot("hhid column must exist"       = "hhid"       %in% names(df))
stopifnot("village_id column must exist" = "village_id" %in% names(df))
stopifnot("treatment column must exist"  = "treatment"  %in% names(df))
stopifnot("round column must exist"      = "round"      %in% names(df))
stopifnot("fcs column must exist"        = "fcs"        %in% names(df))

# Village counts
n_pilot <- length(unique(df$village_id[df$treatment == 1]))
n_comp  <- length(unique(df$village_id[df$treatment == 0]))
stopifnot("Must have 12 pilot villages"      = n_pilot == 12)
stopifnot("Must have 8 comparison villages"  = n_comp  == 8)

# Household counts
stopifnot("Must have 640 unique households" =
            length(unique(df$hhid)) == 640)

# Rounds
stopifnot("Only rounds 0 and 1 permitted" =
            all(df$round %in% c(0L, 1L)))

# Treatment is binary
stopifnot("Treatment must be 0 or 1" =
            all(df$treatment %in% c(0L, 1L)))

cat("All test-import tests PASSED.\n")
