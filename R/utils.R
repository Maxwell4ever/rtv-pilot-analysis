# =============================================================================
# R/utils.R - Shared Utility Functions
# =============================================================================
# Sourced automatically by run_analysis.R before any numbered script.
# Also sourced directly when running individual scripts in isolation.
#
# Contents:
#   1. Project root detection (finds .Rproj file - works like {here})
#   2. Path helpers (proj_path, data_path, output_path)
#   3. Hand-off helpers (save_rds, load_rds with error messages)
#   4. Logging (log_msg writes to output/logs/ with timestamp)
#   5. Analysis functions (run_did, wild_bootstrap)
#   6. Display helpers (fmt_p, sig_stars)
#   7. Console banners (section, subsection, step_banner)
#   8. ggplot theme (theme_rtv, save_figure)
# =============================================================================

# -- 1. PROJECT ROOT DETECTION -------------------------------------------------
# Finds the nearest parent directory containing an .Rproj file.
# Works when called via Rscript, source(), or interactively.
.find_proj_root <- function() {
  # Check commandArgs for --file= (Rscript path)
  args      <- commandArgs(trailingOnly = FALSE)
  file_flag <- grep("^--file=", args, value = TRUE)

  start_dir <- if (length(file_flag) > 0) {
    normalizePath(dirname(sub("^--file=", "", file_flag[1])))
  } else {
    # Walk sys.frames for ofile (source() path)
    ofile <- NULL
    for (i in seq_len(sys.nframe())) {
      f <- sys.frame(i)$ofile
      if (!is.null(f) && nzchar(f)) { ofile <- f; break }
    }
    if (!is.null(ofile)) normalizePath(dirname(ofile)) else getwd()
  }

  # Walk up directory tree looking for .Rproj file
  dir <- start_dir
  for (i in seq_len(10)) {
    if (length(list.files(dir, pattern = "\\.Rproj$")) > 0) return(dir)
    parent <- dirname(dir)
    if (parent == dir) break  # reached filesystem root
    dir <- parent
  }
  # Fallback: use current working directory
  getwd()
}

# Set once - available to all scripts as PROJ_ROOT
if (!exists("PROJ_ROOT")) {
  PROJ_ROOT <- .find_proj_root()
}

# -- 2. PATH HELPERS -----------------------------------------------------------
proj_path   <- function(...) file.path(PROJ_ROOT, ...)
data_path   <- function(...) file.path(PROJ_ROOT, "data", ...)
output_path <- function(...) file.path(PROJ_ROOT, "output", ...)

# Canonical paths used across all scripts
PATHS <- list(
  raw       = data_path("raw",   "rtv_pilot_hh_data.csv"),
  processed = data_path("processed"),
  metadata  = data_path("metadata"),
  figures   = output_path("figures"),
  tables    = output_path("tables"),
  logs      = output_path("logs"),
  report    = output_path("report")
)

# Processed file registry - every hand-off in one place
PROC <- list(
  raw_data        = data_path("processed", "01_raw_data.rds"),
  validation      = data_path("processed", "02_validation_flags.rds"),
  df_clean        = data_path("processed", "03_df_clean.rds"),
  cleaning_log    = data_path("processed", "03_cleaning_log.rds"),
  descriptives    = data_path("processed", "04_descriptives.rds"),
  balance         = data_path("processed", "05_balance.rds"),
  models          = data_path("processed", "06_models.rds"),
  bootstrap       = data_path("processed", "06_bootstrap.rds"),
  threshold       = data_path("processed", "06_threshold.rds"),
  df_sub          = data_path("processed", "07_df_sub.rds"),
  subgroups       = data_path("processed", "07_subgroups.rds"),
  sensitivity     = data_path("processed", "10_sensitivity.rds")
)

# -- 3. HAND-OFF HELPERS -------------------------------------------------------
save_rds <- function(obj, key, label = key) {
  path <- PROC[[key]]
  if (is.null(path)) stop("Unknown PROC key: ", key)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(obj, path)
  log_msg(sprintf("SAVED   %-30s -> data/processed/%s",
                  label, basename(path)))
  invisible(path)
}

load_rds <- function(key, label = key) {
  path <- PROC[[key]]
  if (is.null(path)) stop("Unknown PROC key: ", key)
  if (!file.exists(path)) {
    stop(sprintf(
      "\n[MISSING INPUT] %s\n  Expected: %s\n  Run the script that produces this file first.\n",
      label, path
    ))
  }
  obj <- readRDS(path)
  log_msg(sprintf("LOADED  %-30s <- data/processed/%s",
                  label, basename(path)))
  invisible(obj)
}

# -- 4. LOGGING ----------------------------------------------------------------
# Writes timestamped messages to both console and output/logs/run_YYYYMMDD.log
.log_file <- NULL

init_log <- function(script_name) {
  dir.create(PATHS$logs, showWarnings = FALSE, recursive = TRUE)
  log_name  <- paste0("run_", format(Sys.Date(), "%Y%m%d"), ".log")
  .log_file <<- file.path(PATHS$logs, log_name)
  log_msg(paste0(strrep("=", 60)))
  log_msg(paste0("SCRIPT: ", script_name))
  log_msg(paste0("TIME:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  log_msg(paste0(strrep("=", 60)))
}

log_msg <- function(msg) {
  ts  <- format(Sys.time(), "%H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  message(line)  # to console
  if (!is.null(.log_file)) {
    cat(line, "\n", file = .log_file, append = TRUE)
  }
}

# -- 5. ANALYSIS FUNCTIONS -----------------------------------------------------

# Run a single DiD regression with CR2 cluster-robust standard errors
# Returns list: $model (lm_robust), $coef (tidy treat_x_post row)
run_did <- function(outcome_var, label, data,
                    covariates = ANALYSIS$covariates,
                    cluster_var = ANALYSIS$cluster_var,
                    se_type     = ANALYSIS$se_type) {

  fml <- as.formula(paste(
    outcome_var,
    "~ treatment + post + treat_x_post +",
    paste(covariates, collapse = " + ")
  ))

  mod <- estimatr::lm_robust(fml,
                              data     = data,
                              clusters = data[[cluster_var]],
                              se_type  = se_type)

  co <- broom::tidy(mod) |>
    dplyr::filter(term == "treat_x_post") |>
    dplyr::mutate(outcome = label)

  invisible(list(model = mod, coef = co))
}

# Rademacher wild cluster bootstrap for one DiD model
# Returns list: observed_t, boot_p (numeric), boot_p_fmt (string)
wild_bootstrap <- function(outcome_var, model_result, data,
                           B           = ANALYSIS$bootstrap_B,
                           cluster_var = ANALYSIS$cluster_var,
                           covariates  = ANALYSIS$covariates) {

  fml_r <- as.formula(paste(
    outcome_var, "~ treatment + post +",
    paste(covariates, collapse = " + ")
  ))
  restricted  <- lm(fml_r, data = data)
  residuals_r <- residuals(restricted)

  observed_t  <- coef(model_result$model)["treat_x_post"] /
    sqrt(vcov(model_result$model)["treat_x_post", "treat_x_post"])

  villages  <- sort(unique(data[[cluster_var]]))
  boot_t    <- numeric(B)

  for (b in seq_len(B)) {
    wts           <- stats::setNames(
      sample(c(-1L, 1L), length(villages), replace = TRUE),
      as.character(villages)
    )
    boot_y        <- fitted(restricted) +
      residuals_r * wts[as.character(data[[cluster_var]])]
    data_b              <- data
    data_b[[outcome_var]] <- boot_y

    fml_full <- as.formula(paste(
      outcome_var, "~ treatment + post + treat_x_post +",
      paste(covariates, collapse = " + ")
    ))
    bmod <- tryCatch(
      estimatr::lm_robust(fml_full,
                          data     = data_b,
                          clusters = data_b[[cluster_var]],
                          se_type  = ANALYSIS$se_type),
      error = function(e) NULL
    )
    if (!is.null(bmod)) {
      bc        <- broom::tidy(bmod) |> dplyr::filter(term == "treat_x_post")
      boot_t[b] <- bc$estimate / bc$std.error
    } else {
      boot_t[b] <- NA_real_
    }
  }

  boot_clean <- boot_t[!is.na(boot_t)]
  boot_p     <- mean(abs(boot_clean) >= abs(observed_t))

  list(observed_t  = round(observed_t, 4),
       boot_p      = boot_p,
       boot_p_fmt  = fmt_p(boot_p),
       n_valid_its = length(boot_clean))
}

# -- 6. DISPLAY HELPERS --------------------------------------------------------
fmt_p <- function(p) {
  ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
}

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.01 ~ "** (p<0.01)",
    p < 0.05 ~ "*  (p<0.05)",
    p < 0.10 ~ "^  (p<0.10)",
    TRUE     ~ "ns"
  )
}

# -- 7. CONSOLE BANNERS --------------------------------------------------------
section    <- function(t) { log_msg(paste0("\n", strrep("=",55), "\n", t,
                                            "\n", strrep("=",55))) }
subsection <- function(t) { log_msg(paste0(strrep("-",45), "\n", t,
                                            "\n", strrep("-",45))) }
step_banner <- function(n, t) {
  log_msg(paste0("\n", strrep("#",55)))
  log_msg(sprintf("  STEP %02d - %s", n, toupper(t)))
  log_msg(strrep("#",55))
}

# -- 8. GGPLOT THEME AND FIGURE SAVER -----------------------------------------
COLOURS <- list(
  navy  = "#1F3864", teal  = "#007B7B",
  lteal = "#A8C8E0", mteal = "#4BA3A3",
  grey  = "#666666", text  = "#2D2D2D"
)

theme_rtv <- function(base_size = 11) {
  ggplot2::theme_minimal(base_family = "sans", base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(colour = COLOURS$navy,
                                               face   = "bold",
                                               size   = base_size + 1),
      plot.subtitle    = ggplot2::element_text(colour = COLOURS$grey,
                                               size   = base_size - 2),
      plot.caption     = ggplot2::element_text(colour = COLOURS$grey,
                                               size   = base_size - 3,
                                               hjust  = 0, face = "italic"),
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(size = base_size - 2),
      panel.grid.minor = ggplot2::element_blank()
    )
}

save_figure <- function(plot_obj, filename,
                        width = 8.5, height = 5.5, dpi = 150) {
  path <- file.path(PATHS$figures, filename)
  ggplot2::ggsave(path, plot_obj, width = width,
                  height = height, dpi = dpi, bg = "white")
  log_msg(sprintf("FIGURE  %-30s -> output/figures/%s", filename, filename))
  invisible(path)
}

log_msg("[utils.R] Loaded.")
