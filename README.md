# Discrepancy Term Analysis for Diagnostic Accuracy Parameters

## Overview

Early economic evaluations of biomarker tests often involve substantial uncertainty due to limited evidence, indirect outcome measures, and rapidly evolving diagnostic and treatment landscapes. **Discrepancy analysis** is a method for quantifying indirectness and bias in model inputs.

This repository contains R code adjusting diagnostic test sensitivity and specificity for potential systematic bias relative to literature reference values. The method incorporates **discrepancy terms** to quantify structural uncertainty when diagnostic accuracy parameters are derived from surrogate outcomes.


## Repository Structure
```
discrepancy-term-analysis/
├── main_analysis.R              # Main script to execute the analysis
├── discrepancy_functions.R      # Core functions for discrepancy analysis
├── summary_functions.R          # Aid for creating tables and graphs
└── README.md
```

## Requirements

Install the required R packages:

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "purrr"))
```

## Usage

Run the main script:

```r
source("run_discrepancy_analysis.R")
```


## References

1. Fernández Coves A, et al. Prioritising research for biomarker diagnostics with multiple positions in the care pathway: a value of information study using discrepancy terms. [Pending publication]
2. Strong, M., J.E. Oakley, and J. Chilcott, Managing structural uncertainty in health economic decision models: a discrepancy approach. Journal of the Royal Statistical Society: Series C (Applied Statistics), 2012. 61(1): p. 25-45.
3. Valsecchi, M.E., D. Silbermins, N. de Rosa, S.L. Wong, and G.H. Lyman, Lymphatic mapping and sentinel lymph node biopsy in patients with melanoma: a meta-analysis. J Clin Oncol, 2011. 29(11): p. 1479-87.

## Contact

For questions or issues, please open an issue on GitHub or contact:

Andrea Fernández Coves (email: andrea.fernandez.coves@gmail.com)

ORCID: 0009-0000-7698-3849 
