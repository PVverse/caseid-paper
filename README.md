# caseid-paper — Case Identification Framework for Pharmacovigilance

Code and de-identified data for:

> Fusaroli M, Felix J, Sartori D, Giunchi V, Härmark L, Scholl J, van Hunsel F, Norén GN, Ellenius J. **Towards a Framework for Case Identification in Pharmacovigilance: Not All Reports are Created Equal.** *Drug Safety* (in press).

## Overview

Retrieval of adverse event reports based on coded drug–event co-occurrence yields candidate reports, not validated cases. This repository contains the annotation study that developed and tested a structured framework for distinguishing retrieved reports from clinically meaningful case series.

Two case studies are included:

- **Case Study 1 — Impulsivity**: dopamine agonists (DAA), third-generation antipsychotics (TGA), methylphenidate, SSRIs, and negative controls. Used to iteratively develop the framework under information-rich conditions.
- **Case Study 2 — Suicidality**: reports retrieved via the narrow SMQ *Suicide and self-injury*. Used to evaluate the framework under routine-review conditions with variable data availability.

## Repository structure

```
caseid-paper/
├── analysis/
│   ├── Analyses.qmd    # main analysis document (renders to HTML)
│   └── Analyses.R      # plain-script mirror of the same analysis
├── R/
│   └── functions.R     # reusable utilities (agreement metrics, data loaders, plots)
├── data/
│   ├── impulsivity_SM.csv   # de-identified impulsivity annotations
│   └── suicidality_SM.csv   # de-identified suicidality annotations
├── DESCRIPTION         # package metadata (for eventual R package conversion)
├── CITATION.cff
├── LICENSE
└── README.md
```

## Reproducing the analysis

### Requirements

- R ≥ 4.3
- [Quarto](https://quarto.org) ≥ 1.4

Install R package dependencies:

```r
install.packages(c(
  "data.table", "dplyr", "ggplot2", "ggpattern", "ggnewscale",
  "here", "irr", "janitor", "patchwork", "plotly", "purrr",
  "readxl", "scales", "stringr", "tidyr", "utf8", "writexl",
  "ComplexUpset"
))
```

For reproducible package versions, use `renv`:

```r
install.packages("renv")
renv::restore()
```

### Render the analysis

```bash
quarto render analysis/Analyses.qmd
```

This produces `analysis/Analyses.html` — a self-contained interactive document covering Sections 4–6 (all publicly reproducible sections).

Sections 2–3 (inter-rater agreement and annotation characterisation) require the original annotation Excel files and are rendered with `eval: false`; they serve as documentation of the non-public analysis steps.

### Optional: ATC classification

Sections 6.2–6.3 (drug heatmap ordering by ATC class and the ATC bar chart) require an external `ATC_DiAna.csv` file from [DiAna](https://github.com/fusaroli/DiAna). Set `ATC_PATH` in `analysis/Analyses.qmd` to your local path; all other sections run without it.

## Data availability

The de-identified annotations in `data/` are released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). The original annotation files (which contain report-level information subject to VigiBase Data Access Conditions) are not distributed. Access to the original data is available from Uppsala Monitoring Centre, subject to Data Access Conditions.

## Citation

If you use this code or data, please cite the paper above. You can also cite the repository directly using the metadata in `CITATION.cff` (GitHub surfaces this via the "Cite this repository" button).

## License

Code: [MIT](LICENSE)  
Data (`data/`): [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
