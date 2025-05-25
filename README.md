<img src="man/figures/mvn_sticker.png" align="right" width="95"/>


# MVN: An R Package for Assessing Multivariate Normality

[![CRAN Status](https://www.r-pkg.org/badges/version/MVN?color=red)](https://CRAN.R-project.org/package=MVN)
[![CRAN Downloads (last day)](https://cranlogs.r-pkg.org/badges/last-day/MVN?color=yellowgreen)](https://cranlogs.r-pkg.org/)
[![CRAN Downloads (last month)](https://cranlogs.r-pkg.org/badges/MVN?color=yellow)](https://cranlogs.r-pkg.org/)
[![CRAN Downloads (total)](https://cranlogs.r-pkg.org/badges/grand-total/MVN)](https://cranlogs.r-pkg.org/)

---

## Overview

The MVN R package provides a comprehensive framework for assessing multivariate normality, a key assumption in many parametric multivariate statistical methods such as:

- MANOVA
- Linear Discriminant Analysis
- Principal Component Analysis
- Canonical Correlation Analysis

Multivariate normality assessment is critical, and no single method is universally optimal. MVN combines several numerical and graphical tools to offer reliable evaluations across diverse scenarios.

---

## Features

- **Multivariate Normality Tests:**
  - Mardia's Test
  - Henze-Zirkler’s Test
  - Royston’s Test
  - Doornik-Hansen's Test
  - Energy Test

- **Graphical Diagnostics:**
  - Chi-square Q-Q Plots
  - 3D Perspective Plots
  - Contour Plots

- **Multivariate Outlier Detection:**
  - Robust Mahalanobis distance-based methods

- **Univariate Normality Checks:**
  - Multiple tests and visualizations for marginal distributions

- **Data Transformation Options:**
  - Logarithmic
  - Square Root
  - Square
  - Box-Cox (with lambda selection)

- **Descriptive Statistics Output**

- **Subset (grouped) analysis support**

---

## Installation

To install the latest version from **CRAN**:

```r
install.packages("MVN")
```

To install the development version from GitHub:

```r
devtools::install_github("selcukorkmaz/MVN")
```

## Usage Example

```r
library(MVN)

# Run MVN tests and diagnostics on iris data
result <- mvn(
  data = iris[1:50, 1:3],
  mvn_test = "hz"
)

# View results
summary(result)
```

## Web Application

A web-based graphical user interface is available here:
http://biosoft.erciyes.edu.tr/app/MVN


## Citation

Korkmaz S, Goksuluk D, Zararsiz G. MVN: An R Package for Assessing Multivariate Normality. The R Journal. 2014; 6(2):151-162.
https://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf