# MVN: An R Package for Assessing Multivariate Normality
[![Build Status](https://travis-ci.org/selcukorkmaz/MVN.svg?branch=master)](https://travis-ci.org/selcukorkmaz/MVN) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/MVN?color=red)](https://CRAN.R-project.org/package=MVN) [![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/last-day/MVN?color=yellowgreen)](https://github.com/r-hub/cranlogs.app) [![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/MVN?color=yellow)](https://github.com/r-hub/cranlogs.app) [![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/MVN)](https://github.com/r-hub/cranlogs.app) 


Assessing the assumption of multivariate normality is required by many parametric multivariate statistical methods, such as MANOVA, linear discriminant analysis, principal component analysis, canonical correlation, etc. It is important to assess multivariate normality in order to proceed with such statistical methods. There are many analytical methods proposed for checking multivariate normality. However, deciding which method to use is a challenging process, since each method may give different results under certain conditions. Hence, we may say that there is no best method, which is valid under any condition, for normality checking. In addition to numerical results, it is very useful to use graphical methods to decide on multivariate normality. Combining the numerical results from several methods with graphical approaches can be useful and provide more reliable decisions. 

Here, we present an R package, MVN, to assess multivariate normality. It contains the five most widely used multivariate normality tests, including Mardia’s, Henze-Zirkler’s, Royston’s, Doornik-Hansen's and Energy, and graphical approaches, including chi-square Q-Q, perspective and contour plots. It also includes two multivariate outlier detection methods, which are based on robust Mahalanobis distances. This package also offers functions to check the univariate normality of marginal distributions through both tests and plots. Furthermore, it calculates descriptive statistics and has options to apply data transformation, including logarithmic, square and square root. We also provide a user-friendly web application of the package.

MVN main paper: http://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf

MVN R package: http://cran.r-project.org/web/packages/MVN/index.html

MVN web-tool: http://www.biosoft.hacettepe.edu.tr/MVN/

Installation
------------
To install from CRAN:

    install.packages("MVN")
    

To install from github: 
    
    devtools::install_github('selcukorkmaz/MVN')

