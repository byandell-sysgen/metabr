# metabr: Metabolite Data Processing and QC Automation

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

**metabr** is an R package for metabolite data ingestion, automated quality control (QC) batch correction, normalization, and curation discrepancy analysis. Developed for mass spectrometry metabolomics data (El-MAVEN / LC-MS) in systems genetics studies (Kibbey Lab / Attie Lab), `metabr` automates plate-by-plate QC pool batch correction and prepares standardized datasets for downstream analysis.

---

## Features

- **Data Ingestion**: Flexible import of raw LC-MS peak-area spreadsheets (CSV and multi-sheet Excel) for both targeted and untargeted assays (`read_raw_metab()`, `read_batch_metab()`, `read_metab_minute()`).
- **Automated QC Batch Correction**: Automated plate-level correction factor calculation based on pooled QC samples (`qc_steps()`, `calc_cf()`, `correct_raw_cf()`).
- **Missingness Handling**: Automatic substitution with plate-wide average correction factors when metabolites are missing from specific plate QC runs (`replace_missing_ave_cf()`).
- **Untargeted Peak Support**: Seamlessly accommodates multiple chromatographic peaks per compound differentiated by retention time (`medRt`).
- **Curation Diagnostics**: S3 class and `ggplot2` diagnostics to evaluate and visualize concordance between automated corrections and manual hand curation (`hand_auto()`, `autoplot()`).
- **Standardized Export**: Reshaping and export of long-format data into wide CSV matrices ready for downstream pipelines (`write_metab()`).

---

## Installation

You can install the development version of `metabr` from GitHub:

```r
# install.packages("remotes")
remotes::install_github("byandell/metabr")
```

---

## Quick Start Workflow

### Step 0: Data Setup

Before running batch correction, ensure raw data and/or reference files are located in the expected dated directory under `data/"QC Batch Correction"` (documented in [`datasets.Rmd`](vignettes/devel_guide/datasets.Rmd)):

```r
# Timestamped data directories by study batch:
# - "20231127": Founder Plasma (Raw CSVs and Hand-calculated Excel)
# - "20251112": DO Livers (Raw CSVs)
# - "20261020": DO Plasma (Files for Batch Correction)
batch_date <- "20261020" # Default batch
dirpath <- file.path("data", "QC Batch Correction", batch_date)
```

### Option 1: Automated QC Pipeline via `QC_Automation.Rmd`

The package includes an end-to-end parameterized R Markdown workflow in [`QC_Automation.Rmd`](QC_Automation.Rmd) that automatically resolves paths for the selected batch date, runs QC correction across modes (`HC_Neg` and `RP_Pos`) and assay types (targeted and untargeted), and exports standardized CSV matrices:

```r
library(metabr)

# 1. Render report for default batch (20261020):
rmarkdown::render("QC_Automation.Rmd")

# 2. Or render for an earlier cohort / batch:
rmarkdown::render("QC_Automation.Rmd", params = list(batch_date = "20251112"))
rmarkdown::render("QC_Automation.Rmd", params = list(batch_date = "20231127"))

# Or open and step through interactively:
# file.edit("QC_Automation.Rmd")
```

Cohort-specific historical notebooks are also available:
- [`QC_Automation_2023.Rmd`](QC_Automation_2023.Rmd): Founder Plasma study (benchmarked against manual hand curation via `hand_auto()`).
- [`QC_Automation_2025.Rmd`](QC_Automation_2025.Rmd): Diversity Outbred (DO) Liver study.

The core pipeline routine in [`QC_Automation.Rmd`](QC_Automation.Rmd) calls `qc_steps()` across each mode:

```r
# Targeted data
rescale_HCneg <- qc_steps(
  dirpath = raw_dir,
  filename = files$hc_neg_tar,
  exclude_first = FALSE
)

rescale_RPpos <- qc_steps(
  dirpath = raw_dir,
  filename = files$rp_pos_tar,
  exclude_first = TRUE # Exclude 1st QC run on plate if historically unstable
)

# Untargeted data
rescale_HCnegU <- qc_steps(
  dirpath = raw_dir,
  filename = files$hc_neg_untar,
  exclude_first = FALSE
)

rescale_RPposU <- qc_steps(
  dirpath = raw_dir,
  filename = files$rp_pos_untar,
  exclude_first = TRUE
)

# Export batch-corrected datasets to wide CSV matrices
write_metab(rescale_HCneg, file.path(out_dir, "HC_Neg_Targeted.csv"))
write_metab(rescale_HCnegU, file.path(out_dir, "HC_Neg_Untargeted.csv"), target = FALSE)
write_metab(rescale_RPpos, file.path(out_dir, "RP_Pos_Targeted.csv"))
write_metab(rescale_RPposU, file.path(out_dir, "RP_Pos_Untargeted.csv"), target = FALSE)
```

### Option 2: Step-by-Step QC Engine

Alternatively, execute the individual stages of the correction pipeline directly in R:

```r
# 1. Read raw metabolite peak-area data
raw_metab <- read_raw_metab(dirpath, "Founder_Plasma_Raw.csv")

# 2. Compute plate-level and global QC correction factors (CF)
qc_factors <- calc_cf(raw_metab)

# 3. Impute missing plate CFs with average plate CF
qc_factors <- replace_missing_ave_cf(qc_factors)

# 4. Rescale sample peak areas using correction factors
rescaled_metab <- correct_raw_cf(raw_metab, qc_factors)
```

### 3. Comparing Manual vs. Automated Curation

Assess concordance between manual calculations and automated QC routines:

```r
# Compute curation discrepancies
curation_check <- hand_auto(hand_curated_df, automated_df)

# Plot diagnostic comparison
library(ggplot2)
autoplot(curation_check)
```

### 4. Exporting Processed Data

Export processed data to standardized wide CSV files:

```r
write_metab(rescaled_metab, out_dir = "data/processed", filename = "metabolites_corrected.csv")
```

---

## Core QC Methodology

For compound $c$ measured on plate $p$ within batch $b$:

1. **Plate QC Mean**: $\bar{y}_{b, p, c, \text{QC}} = \frac{1}{N_{\text{QC}}} \sum_{i \in \text{QC}_{b,p}} y_{i, c}$
2. **Global QC Mean**: $\bar{y}_{c, \text{QC}} = \frac{1}{N_{\text{total QC}}} \sum_{\text{all QC}} y_{i, c}$
3. **Correction Factor**: $\text{CF}_{b, p, c} = \frac{\bar{y}_{b, p, c, \text{QC}}}{\bar{y}_{c, \text{QC}}}$
4. **Corrected Intensity**: $y_{\text{corrected}, i} = \frac{y_{\text{raw}, i}}{\text{CF}_{b, p, c}}$

---

## Pipeline Context

`metabr` prepares clean, batch-corrected metabolite data for the broader systems genetics and multi-omics toolchain:

- [foundr](https://github.com/byandell/foundr): Genetic architecture, mediation analysis, and orthogonal variance partitioning in multiparent populations.
- [foundrShiny](https://github.com/byandell/foundrShiny): Interactive Shiny application for exploring multi-omics founder data.
- [foundrHarmony](https://github.com/byandell/foundrHarmony): Multi-platform data harmonization.

---

## Documentation & Developer Guide

- See [DEVELOPER.md](DEVELOPER.md) for detailed package architecture, data models, and developer environment setup.
- See [`QC_Automation.Rmd`](QC_Automation.Rmd) and [`QC_steps.Rmd`](QC_steps.Rmd) for detailed analysis walkthroughs across targeted and untargeted modes.
- See [`vignettes/devel_guide/`](vignettes/devel_guide/) for online developer documentation covering architecture, QC formulas, schemas, deployment, and dataset directory organization ([`datasets.Rmd`](vignettes/devel_guide/datasets.Rmd)).
