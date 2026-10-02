# litirt

**IRT-Based Proficiency Level Classification for Literacy and Large-Scale Assessments**

## Overview

`litirt` implements Item Response Theory (IRT)-based proficiency level classification for large-scale literacy and educational assessments. Given IRT theta estimates and user-defined cut scores, `litirt` classifies respondents into proficiency levels, computes classification accuracy and standard errors, estimates misclassification probabilities, and produces visualizations.

The package supports any number of proficiency levels and any IRT model supported by `mirt`. Built-in datasets simulate PISA reading literacy and Indonesian AKM literacy assessments.

## Installation

```r
# Install from GitHub
remotes::install_github("rmeasure/litirt")
```

## Quick Start

```r
library(litirt)

# Step 1: Load built-in data (PISA reading literacy)
data(pisa_data)
items <- pisa_data[, 1:20]

# Step 2: Estimate IRT parameters and theta scores
est <- litirt_estimate(items, model = "2PL")
print(est)

# Step 3: Classify into proficiency levels (PISA-style)
bench <- litirt_benchmark(est,
  cuts   = c(-1.5, -0.5, 0.5, 1.5),
  labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))
print(bench)

# Step 4: Compute optimal cut scores
cuts <- litirt_cutoff(est, n_levels = 5, method = "equal")
print(cuts)

# Step 5: Estimate misclassification probabilities
mis <- litirt_misclass(bench)
print(mis)

# Step 6: Visualize
litirt_plot(bench, type = "distribution")
litirt_plot(bench, type = "barplot")
litirt_plot(mis,   type = "misclass")
```

## AKM Example

```r
# AKM Indonesia (4 levels)
data(akm_data)
items_akm <- akm_data[, 1:15]
est_akm   <- litirt_estimate(items_akm, model = "graded")
bench_akm <- litirt_benchmark(est_akm,
  cuts   = c(-1.0, 0.0, 1.0),
  labels = c("Perlu Intervensi Khusus", "Dasar", "Cakap", "Mahir"))
print(bench_akm)
litirt_plot(bench_akm, type = "distribution")
```

## Functions

| Function | Description |
|---|---|
| `litirt_estimate()` | Fit IRT model and estimate theta scores |
| `litirt_benchmark()` | Classify respondents into proficiency levels |
| `litirt_cutoff()` | Determine optimal cut scores |
| `litirt_misclass()` | Compute misclassification probabilities |
| `litirt_plot()` | Visualize classification results |

## Built-in Datasets

| Dataset | Description |
|---|---|
| `pisa_data` | 500 persons, 20 dichotomous items, PISA reading literacy |
| `akm_data` | 400 persons, 15 polytomous items (0-3), AKM Indonesia |

## References

- OECD. (2019). *PISA 2018 Technical Report*. OECD Publishing.
- Kemdikbudristek. (2021). *Panduan Teknis AKM*. Pusat Asesmen dan Pembelajaran.
- Chalmers, R. P. (2012). mirt. *Journal of Statistical Software, 48*(6), 1-29.
- Livingston, S. A., & Lewis, C. (1995). *Journal of Educational Measurement, 32*(2), 179-197.
