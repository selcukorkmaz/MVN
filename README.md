<img src="man/figures/mvn_sticker.png" align="right" width="95"/>


# MVN: An R Package for Assessing Multivariate Normality

[![CRAN Status](https://www.r-pkg.org/badges/version/MVN?color=red)](https://CRAN.R-project.org/package=MVN)
[![CRAN Downloads (last day)](https://cranlogs.r-pkg.org/badges/last-day/MVN?color=yellowgreen)](https://cranlogs.r-pkg.org/)
[![CRAN Downloads (last month)](https://cranlogs.r-pkg.org/badges/MVN?color=yellow)](https://cranlogs.r-pkg.org/)
[![CRAN Downloads (total)](https://cranlogs.r-pkg.org/badges/grand-total/MVN)](https://cranlogs.r-pkg.org/)

---

## Overview

MVN is an R package that provides a comprehensive and user-friendly framework for assessing multivariate normality—a key assumption in many multivariate statistical methods such as:

- MANOVA
- Linear Discriminant Analysis
- Principal Component Analysis
- Canonical Correlation Analysis

Multivariate normality is often overlooked or improperly tested. The MVN package addresses this by integrating robust numerical tests, graphical diagnostics, and transformation tools, offering clear insights into the distributional characteristics of your multivariate data.

---

## Features

- **Multivariate Normality Tests:**
  - Mardia's Test
  - Henze-Zirkler’s Test
  - Henze-Wagner’s Test
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

- **Transformations & Imputation**
  - Log, square root, and square transformations
  - Optimal Box–Cox and Yeo–Johnson power transformations
  - Missing data handling via mean, median, or MICE imputation

- **Bootstrap Support**
  -  Optional bootstrap p-values for Mardia, Henze–Zirkler, and Royston tests for improved small-sample inference

- **Descriptive Statistics and Group-Wise Analysis**
  -  Grouped summaries using the subset argument
  -  Integration with tidy data pipelines
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

## Basic Usage

```r
library(MVN)

# Run MVN tests and diagnostics on iris data
result <- mvn(
  data = iris[1:50, 1:3],
  mvn_test = "hz"
  )

# View results
summary(result, "mvn")
```

For grouped analysis:

```r
mvn(data = iris, subset = "Species", mvn_test = "hz")
```


## Shiny Web App

Explore MVN’s features via a user-friendly web interface:
http://biosoft.erciyes.edu.tr/app/MVN

To launch the Shiny app locally from the MVN package, run:

```r
MVN::run_mvn_app()
```


## Documentation and Tutorial

Full documentation and an interactive tutorial site are available at:
https://selcukorkmaz.github.io/mvn-tutorial/


## Citation

Please cite MVN in your publications using:

Korkmaz S, Goksuluk D, Zararsiz G. MVN: An R Package for Assessing Multivariate Normality. The R Journal. 2014; 6(2):151-162.
https://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf


## License

MVN is released under the MIT license.