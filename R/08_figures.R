# =============================================================================
# R/08_figures.R - Three Publication-Ready Exhibits
# =============================================================================
# Input:   03_df_clean.rds, 06_models.rds, 07_df_sub.rds
# Output:  output/figures/figure01_fcs_distribution.png
#          output/figures/figure02_did_estimates.png
#          output/figures/figure03_subgroup_trajectories.png
# =============================================================================

if (!exists("PROJ_ROOT")) source(file.path(dirname(
  tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) ".")), "utils.R"))
if (!exists("ANALYSIS"))  source(file.path(PROJ_ROOT, "R", "00_packages.R"))

init_log("08_figures.R")
step_banner(8, "Figures")

df_clean <- load_rds("df_clean")
models   <- load_rds("models")
df_sub   <- load_rds("df_sub")

primary_results <- models$primary_results

# -- Figure 1: FCS Distribution ------------------------------------------------
subsection("Figure 1: FCS distribution shift")

fig1 <- df_clean |>
  dplyr::mutate(
    group = dplyr::case_when(
      treatment==1 & round==0 ~ "Pilot \u2014 Baseline",
      treatment==1 & round==1 ~ "Pilot \u2014 Endline",
      treatment==0 & round==0 ~ "Comparison \u2014 Baseline",
      treatment==0 & round==1 ~ "Comparison \u2014 Endline"
    ),
    group = factor(group, levels = c(
      "Pilot \u2014 Baseline",    "Pilot \u2014 Endline",
      "Comparison \u2014 Baseline","Comparison \u2014 Endline"))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=fcs, fill=group)) +
  ggplot2::geom_density(alpha=0.42, colour="white", linewidth=0.3) +
  ggplot2::geom_vline(xintercept=ANALYSIS$fcs_poor,
                      linetype="dashed", colour="#CC0000", linewidth=0.6) +
  ggplot2::geom_vline(xintercept=ANALYSIS$fcs_borderline,
                      linetype="dashed", colour="#FF8800", linewidth=0.6) +
  ggplot2::annotate("text", x=ANALYSIS$fcs_poor-2, y=0.033,
                    label="Poor\n(<28)", size=3.2, colour="#CC0000", hjust=1) +
  ggplot2::annotate("text", x=ANALYSIS$fcs_borderline-2, y=0.033,
                    label="Borderline\n(28-42)", size=3.2, colour="#FF8800", hjust=1) +
  ggplot2::scale_fill_manual(values=c(COLOURS$navy, COLOURS$teal,
                                       COLOURS$lteal, COLOURS$mteal)) +
  ggplot2::labs(
    title    = "Food Consumption Score Distribution: Baseline vs. Endline",
    subtitle = paste0("Pilot and comparison households \u2014 ",
                      ANALYSIS$district, " | ", ANALYSIS$pilot_period),
    caption  = paste0("N = ", nrow(df_clean), " analysis observations. ",
                      "Dashed lines = WFP food security thresholds."),
    x="Food Consumption Score", y="Density", fill=NULL
  ) +
  theme_rtv()

save_figure(fig1, "figure01_fcs_distribution.png", width=8.5, height=5.5)

# -- Figure 2: DiD Estimates ---------------------------------------------------
subsection("Figure 2: DiD estimates with confidence intervals")

fig2_data <- primary_results |>
  dplyr::mutate(
    label = c("Food Consumption Score\n(FCS, 0\u2013112 pts)",
              "Dietary Diversity Score\n(HDDS, 0\u201312 groups)",
              "Asset Index\n(Composite, 0\u20131 scale)"),
    pt_label = sprintf("%+.3f  p %s", estimate,
                       ifelse(p_display=="< 0.001", "< 0.001",
                              paste0("= ", p_display)))
  )

fig2 <- fig2_data |>
  ggplot2::ggplot(ggplot2::aes(x=estimate, y=reorder(label, estimate))) +
  ggplot2::geom_vline(xintercept=0, linetype="dashed",
                      colour="grey60", linewidth=0.7) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin=conf.low, xmax=conf.high),
    orientation="y", width=0.22,
    colour=COLOURS$teal, linewidth=1.1
  ) +
  ggplot2::geom_point(colour=COLOURS$navy, size=4.5) +
  ggplot2::geom_text(ggplot2::aes(label=pt_label),
                     vjust=-0.9, size=3.1, colour=COLOURS$navy) +
  ggplot2::labs(
    title    = "Pilot Effect Estimates \u2014 Difference-in-Differences",
    subtitle = paste0("CR2 cluster-robust 95% CIs | Village-level clustering | ",
                      "N=", nrow(df_clean), " HH-rounds"),
    caption  = paste0("Controls: ", paste(ANALYSIS$covariates, collapse=", "),
                      ". Validated by Rademacher wild bootstrap (B=",
                      ANALYSIS$bootstrap_B, ")."),
    x="Estimated treatment effect (DiD on treat_x_post)", y=NULL
  ) +
  theme_rtv() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

save_figure(fig2, "figure02_did_estimates.png", width=8.5, height=4.5)

# -- Figure 3: Subgroup Trajectories ------------------------------------------
subsection("Figure 3: FCS trajectories by household type")

traj <- df_clean |>
  dplyr::mutate(
    hh_type = ifelse(female_head==1, "Female-headed", "Male-headed"),
    grp     = paste0(hh_type, " | ",
                     ifelse(treatment==1, "Pilot", "Comparison")),
    period  = factor(ifelse(round==0,"Baseline","Endline"),
                     levels=c("Baseline","Endline"))
  ) |>
  dplyr::group_by(grp, hh_type, treatment, period) |>
  dplyr::summarise(mean_fcs=mean(fcs, na.rm=TRUE), .groups="drop")

fig3 <- traj |>
  ggplot2::ggplot(ggplot2::aes(x=period, y=mean_fcs, colour=grp,
                                group=grp, linetype=factor(treatment))) +
  ggplot2::geom_line(linewidth=1.15) +
  ggplot2::geom_point(size=3.5) +
  ggplot2::geom_text(ggplot2::aes(label=sprintf("%.1f",mean_fcs)),
                     vjust=-1.1, size=3.2, fontface="bold") +
  ggplot2::scale_colour_manual(values=c(COLOURS$navy, COLOURS$teal,
                                         COLOURS$lteal, COLOURS$mteal)) +
  ggplot2::scale_linetype_manual(values=c("1"="solid","0"="dashed"),
                                  guide="none") +
  ggplot2::scale_y_continuous(limits=c(28,58), breaks=seq(28,58,5)) +
  ggplot2::labs(
    title    = "FCS Trajectories by Household Type and Treatment Status",
    subtitle = "Solid = Pilot villages  |  Dashed = Comparison villages",
    caption  = paste0("Source: RTV Isingiro pilot household survey, 2023-2024. ",
                      "Mean FCS by household type."),
    x=NULL, y="Mean Food Consumption Score", colour=NULL
  ) +
  theme_rtv()

save_figure(fig3, "figure03_subgroup_trajectories.png", width=8.5, height=5.5)

log_msg("08_figures.R complete. Run 09_tables.R next.")
