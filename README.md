# RTV Isingiro Productive Asset Transfer Pilot — Evaluation Analysis

**Analyst:** Maxwell Joseph Ochora  
**Organisation:** Raising The Village — VENN Department, Pilots and Innovation Unit  
**Pilot period:** October 2023 to April 2024  
**Geography:** Isingiro District, Uganda (20 villages: 12 pilot, 8 comparison)  
**Date:** June 2026

---

## Study Overview

This repository contains the full reproducible analysis pipeline for the
evaluation of RTV's productive asset transfer pilot in Isingiro District.
The pilot tested a three-component intervention — dairy goat transfer,
structured savings group (VSLA) membership, and four nutrition training
sessions — across 640 households.

The primary evaluation questions are:

1. Did the pilot improve food security outcomes (FCS, HDDS, asset index)?
2. Which households benefited most?
3. Which components should be scaled, and which require further evidence?

---

## Method

**Design:** Difference-in-Differences (DiD) with household-level panel data.  
**Estimator:** OLS with CR2 cluster-robust standard errors (village level).  
**Validation:** Rademacher wild cluster bootstrap (B = 2,000).  
**Multiple comparisons:** Benjamini-Hochberg FDR correction on subgroup tests.  
**Parallel trends:** Supported by baseline balance, small secular trend (+1.3 pts),
and null falsification test on household size.

---

## Repository Structure

```
rtv-pilot-analysis/
│
├── README.md               This file
├── .gitignore              Git ignore (processed data, outputs not tracked)
├── RTV_Pilot.Rproj         RStudio project file
├── renv.lock               Package lockfile (reproducible environment)
├── run_analysis.R          Master entry point — run this to execute everything
│
├── data/
│   ├── raw/
│   │   └── rtv_pilot_hh_data.csv       Raw survey data (never modified)
│   ├── processed/                       Intermediate .rds files (gitignored)
│   └── metadata/
│       └── data_dictionary.xlsx         Variable definitions and coding
│
├── R/                      Analysis scripts (run in numbered order)
│   ├── utils.R             Shared functions (loaded first by every script)
│   ├── 00_packages.R       Package loading and environment check
│   ├── 01_import.R         Load raw data, structural inspection
│   ├── 02_validation.R     Five data quality diagnostic checks
│   ├── 03_cleaning.R       Cleaning decisions + attrition analysis
│   ├── 04_descriptive.R    Descriptive statistics and sample summary
│   ├── 05_balance_checks.R Baseline balance + parallel trends assessment
│   ├── 06_models.R         Primary DiD regressions + wild bootstrap
│   ├── 07_subgroups.R      Subgroup DiD + BH multiple comparisons correction
│   ├── 08_figures.R        Three publication-ready exhibits (PNG)
│   ├── 09_tables.R         Six analytical tables (CSV)
│   └── 10_sensitivity.R    Sensitivity analyses (winsorization, spec checks)
│
├── output/
│   ├── figures/            Exhibit 1, 2, 3 (PNG, 150 dpi)
│   ├── tables/             Six CSV tables
│   ├── logs/               Timestamped run logs
│   └── report/             Report brief (.docx)
│
└── tests/
    ├── test-import.R       Tests for 01_import.R
    ├── test-cleaning.R     Tests for 03_cleaning.R
    ├── test-models.R       Tests for 06_models.R
    └── test-validation.R   Tests for 02_validation.R
```

---

## Quick Start

### Run the full pipeline

```r
# From RStudio: open RTV_Pilot.Rproj, then:
source("run_analysis.R")

# From terminal:
Rscript run_analysis.R
```

### Run a single script

Each script is self-contained. It reads its required inputs from
`data/processed/` and writes its outputs back there or to `output/`.

```r
source("R/06_models.R")      # re-run DiD analysis only
source("R/08_figures.R")     # regenerate figures only
```

### Run tests

```r
source("tests/test-cleaning.R")
source("tests/test-models.R")
```

---

## Key Results

| Outcome | DiD Estimate | p-value | Decision |
|---|---|---|---|
| Food Consumption Score | +8.5 pts | < 0.001 | Scale NOW |
| Dietary Diversity Score | +1.2 groups | < 0.001 | Scale NOW |
| Asset Index | +0.055 | 0.003 | Hold — 12-month endline |

Female-headed households gained +10.9 pts FCS versus +7.0 pts for
male-headed households. All subgroup findings survive BH correction.

---
### Key Findings

Here are the primary results from the RTV pilot analysis:

![Food Consuption Score distribution: Baseline VS Endline](output/figures/figure01_fcs_distribution.png)
![Pilot Effect Estimates...Difference in Disfference](output/figures/figure02_did_estimates.png)
---
## Reproducibility

This project uses `renv` for package management.

```r
# Restore the exact package environment used for this analysis
renv::restore()
```

Package versions are locked in `renv.lock`. See that file for the
complete dependency manifest.

---

## Data

Raw data (`data/raw/rtv_pilot_hh_data.csv`) is the household survey
dataset and should never be modified. All cleaning is performed
programmatically in `R/03_cleaning.R` with every decision documented.

Variable definitions are in `data/metadata/data_dictionary.xlsx`.

---

## Contact

Maxwell Joseph Ochora | Data Analyst, Pilots and Innovation  
VENN Department | Raising The Village  
Analysis code and questions: raise an issue in this repository.
