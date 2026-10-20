# Package Refactoring & Data Reorganization

## Package Refactoring & Data Reorganization

This document records the architectural refactoring, data
reorganization, and automation enhancements introduced in **metabr**
(v0.2.4).

------------------------------------------------------------------------

### 1. Background & Motivation

As metabolomics datasets in the Kibbey Lab / Attie Lab systems genetics
studies expanded across study cohorts (Founder plasma, Diversity Outbred
liver, and Diversity Outbred plasma), `metabr` needed to transition from
project-specific, one-off analysis scripts to a standardized,
parameterized data processing pipeline.

Key goals of the refactoring:

1.  **Standardize Data Layout**: Replace ad-hoc root folders with a
    unified, dated directory hierarchy under
    `data/"QC Batch Correction"`.
2.  **Unify QC Automation**: Transition from separate annual scripts
    (`QC_Automation_2023.Rmd`, `QC_Automation_2025.Rmd`) to a single
    parameterized pipeline (`QC_Automation.Rmd`).
3.  **Harmonize Function APIs**: Ensure consistency in argument naming
    (e.g. `exclude_first` vs. legacy flags) and handle missing plate CF
    imputations.
4.  **Establish Developer Infrastructure**: Implement standard agentic
    instructions
    ([`AGENTS.md`](https://byandell-sysgen.github.io/metabr/AGENTS.md)),
    a living developer guide
    ([`DEVELOPER.md`](https://byandell-sysgen.github.io/metabr/DEVELOPER.md)),
    and comprehensive `pkgdown` vignettes.

------------------------------------------------------------------------

### 2. Chronology of Data Reorganization

All study data transfers are organized by timestamped directories under
`data/"QC Batch Correction"`, documented in
[`datasets.Rmd`](https://byandell-sysgen.github.io/metabr/articles/devel_guide/datasets.md).

``` mermaid
flowchart TD
    subgraph dataRoot ["data/'QC Batch Correction'/"]
        batch2023["20231127/ (Founder Plasma)"]
        batch2025["20251112/ (DO Livers)"]
        batch2026["20261020/ (DO Plasma)"]
    end

    subgraph contents2023 ["20231127 Contents"]
        raw2023["#Founder_Plasma_Raw_Data/ (CSVs)"]
        hand2023["#Founder_Plasma_Batch_Corrected_Data/ (XLSX)"]
        auto2023["Automated/ (Exported CSVs)"]
    end

    subgraph contents2025 ["20251112 Contents"]
        raw2025["2025_Nov_Raw/ (Raw CSVs)"]
        auto2025["2025_Nov_Automated/ (Exported CSVs)"]
    end

    subgraph contents2026 ["20261020 Contents"]
        raw2026["DO_Plasma_tar_untar_metab_For_Batch_Correction/"]
        auto2026["Automated/ (Exported CSVs)"]
    end

    batch2023 --> contents2023
    batch2025 --> contents2025
    batch2026 --> contents2026

    classDef batch fill:#1f77b4,stroke:#333,stroke-width:2px,color:#fff
    classDef sub fill:#ff7f0e,stroke:#333,stroke-width:2px,color:#fff
    class batch2023,batch2025,batch2026 batch
    class raw2023,hand2023,auto2023,raw2025,auto2025,raw2026,auto2026 sub
```

#### Data Cohort Summaries

| Batch Date | Cohort | Tissue | Data Available | Naming Conventions & Notes |
|----|----|----|----|----|
| **`20231127`** | Founder mice | Plasma | `Raw` + `Corrected` (Hand) + `Automated` | Initial benchmark study. Used leading `#` folder prefixes (`#Founder_Plasma_Raw_Data/`, `#Founder_Plasma_Batch_Corrected_Data/`). Evaluated automated pipeline vs. manual human curation using [`hand_auto()`](https://byandell-sysgen.github.io/metabr/reference/hand_auto.md). |
| **`20251112`** | Diversity Outbred | Liver | `Raw` only + `Automated` | Relies entirely on automated pipeline (`2025_Nov_Automated/`). Introduces `(in)` input indicators and `min1000` minimum peak area threshold flags. |
| **`20261020`** | Diversity Outbred | Plasma | `Corrected` (For Batch Correction) | Standardized files prepared for batch correction: `DO_Plasma_{Mode}_{Type}_For_Batch_Correction.csv`. |

------------------------------------------------------------------------

### 3. QC Pipeline Architecture & Unification

#### Historical Scripts vs. Parameterized Pipeline

Prior to refactoring, each new data transfer used a dedicated script
with hardcoded paths. The new architecture provides both historical
traceability and a single parameterized entry point:

1.  **[`QC_Automation.Rmd`](https://byandell-sysgen.github.io/metabr/QC_Automation.Rmd)
    (Unified Pipeline)**:
    - Configured with `params$batch_date` (defaults to `"20261020"`).
    - Dynamically resolves directory paths, filename conventions, and
      assay types.
    - Automatically passes `exclude_first = TRUE` for `RP_Pos` mode to
      drop initial column conditioning outliers.
    - Dynamically checks for reference hand curation and runs
      [`hand_auto()`](https://byandell-sysgen.github.io/metabr/reference/hand_auto.md)
      diagnostics only when available.
    - Implements non-zero file size guards to handle empty or
      placeholder files gracefully without throwing execution errors.
2.  **Historical Reference Scripts**:
    - [`QC_Automation_2023.Rmd`](https://byandell-sysgen.github.io/metabr/QC_Automation_2023.Rmd):
      Preserved analysis trace for Founder Plasma benchmarking.
    - [`QC_Automation_2025.Rmd`](https://byandell-sysgen.github.io/metabr/QC_Automation_2025.Rmd):
      Preserved analysis trace for DO Liver automation.

#### How to Run the Parameterized Pipeline

``` r

library(metabr)

# 1. Run for default batch (20261020)
rmarkdown::render("QC_Automation.Rmd")

# 2. Or re-run any historical batch
rmarkdown::render("QC_Automation.Rmd", params = list(batch_date = "20251112"))
rmarkdown::render("QC_Automation.Rmd", params = list(batch_date = "20231127"))
```

------------------------------------------------------------------------

### 4. Documentation & Infrastructure Overhaul

The documentation infrastructure was restructured for maintainability
and automated publishing:

- **[`AGENTS.md`](https://byandell-sysgen.github.io/metabr/AGENTS.md)**:
  Standardized system instructions following the Pure R Package / Shiny
  App CRAFT archetype, including explicit rules governing version
  control and testing.
- **[`DEVELOPER.md`](https://byandell-sysgen.github.io/metabr/DEVELOPER.md)**:
  Root architectural developer guide with Mermaid flowcharts, function
  tables, QC formulas, and workflow notebook descriptions.
- **[`README.md`](https://byandell-sysgen.github.io/metabr/README.md)**:
  Updated with Step 0 data setup, Option 1 parameterized pipeline
  execution, and Option 2 step-by-step QC functions.
- **`vignettes/devel_guide/`**:
  - [`index.Rmd`](https://byandell-sysgen.github.io/metabr/articles/devel_guide/index.md):
    Ecosystem overview and local development quick start.
  - [`qc_pipeline.Rmd`](https://byandell-sysgen.github.io/metabr/articles/devel_guide/qc_pipeline.md):
    Mathematical correction formulation, `medRt` untargeted handling,
    and
    [`hand_auto()`](https://byandell-sysgen.github.io/metabr/reference/hand_auto.md)
    diagnostics.
  - [`data_ingestion.Rmd`](https://byandell-sysgen.github.io/metabr/articles/devel_guide/data_ingestion.md):
    Schema definitions, input loaders, and
    [`write_metab()`](https://byandell-sysgen.github.io/metabr/reference/write_metab.md)
    export rules.
  - [`deployment.Rmd`](https://byandell-sysgen.github.io/metabr/articles/devel_guide/deployment.md):
    CI/CD architecture and GitHub Pages publishing workflow (moved and
    converted from `inst/doc/devel_guide.md`).
  - [`refactor.Rmd`](https://byandell-sysgen.github.io/metabr/articles/devel_guide/refactor.md):
    This refactoring and reorganization guide.
- **[`_pkgdown.yml`](https://byandell-sysgen.github.io/metabr/_pkgdown.yml)**:
  Configured with Bootstrap 5, dynamic Mermaid JS v10 header rendering,
  and custom Developer Guide dropdown menu.

------------------------------------------------------------------------

### 5. Recipe for Adding New Data Batches

When receiving a new metabolomics data drop from the mass spectrometry
core:

1.  **Create Timestamped Directory**: Create a new dated folder:
    `data/"QC Batch Correction"/<YYYYMMDD>/`.
2.  **Deposit Raw Data**: Place raw El-MAVEN CSV exports into an input
    folder (e.g., `Raw/` or `<Cohort>_Raw/`).
3.  **Document in `datasets.Rmd`**: Add an entry to
    [`datasets.Rmd`](https://byandell-sysgen.github.io/metabr/articles/devel_guide/datasets.md)
    detailing the cohort, tissue, mode files, and OneDrive source link.
4.  **Update `QC_Automation.Rmd` Dispatcher**: Add the date mapping in
    `configure_paths` chunk of `QC_Automation.Rmd` matching the new
    filenames.
5.  **Execute and Verify**: Run
    `rmarkdown::render("QC_Automation.Rmd", params = list(batch_date = "<YYYYMMDD>"))`.
6.  **Inspect Exported Matrices**: Verify generated CSV files in
    `data/"QC Batch Correction"/<YYYYMMDD>/Automated/`.
