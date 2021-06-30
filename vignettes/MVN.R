## ----style, echo = FALSE, results = 'asis'------------------------------------
  BiocStyle::markdown()

## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4, tibble.print_max = 4)

## ----MVN, message = FALSE-----------------------------------------------------
# load MVN package
library(MVN)

## ----iris, message = FALSE----------------------------------------------------
# load Iris data
data(iris)

## ----setosa, message = FALSE--------------------------------------------------
# setosa subset of the Iris data
setosa <- iris[1:50, 1:4]

## ----mvn, message = FALSE, eval=FALSE-----------------------------------------
#  mvn(data, subset = NULL, mvnTest = c("mardia", "hz", "royston", "dh",
#    "energy"), covariance = TRUE, tol = 1e-25, alpha = 0.5,
#    scale = FALSE, desc = TRUE, transform = "none", R = 1000,
#    univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"),
#    univariatePlot = "none", multivariatePlot = "none",
#    multivariateOutlierMethod = "none", bc = FALSE, bcType = "rounded",
#    showOutliers = FALSE, showNewData = FALSE)

## ----mardia, message = FALSE--------------------------------------------------
result <- mvn(data = setosa, mvnTest = "mardia")
result$multivariateNormality

## ----hz, message = FALSE------------------------------------------------------
result <- mvn(data = setosa, mvnTest = "hz")
result$multivariateNormality

## ----royston, message = FALSE-------------------------------------------------
result <- mvn(data = setosa, mvnTest = "royston")
result$multivariateNormality

## ----dh, message = FALSE------------------------------------------------------
result <- mvn(data = setosa, mvnTest = "dh")
result$multivariateNormality

## ----energy, message = FALSE--------------------------------------------------
result <- mvn(data = setosa, mvnTest = "energy")
result$multivariateNormality

## ----qq, echo=FALSE, message=FALSE, fig.width=5, fig.height=5, fig.cap = "Chi-Square Q-Q plot for setosa data set."----
par(mar=c(4.2,4.1,3,0.2))
result <- mvn(data = setosa, mvnTest = "royston", multivariatePlot = "qq")

## ----qqUniPlot, echo=FALSE, message=FALSE, fig.width=5, fig.height=5, fig.cap = "Univariate qq-plots for setosa."----
# create univariate Q-Q plots
result <- mvn(data = setosa, mvnTest = "royston", univariatePlot = "qqplot")


## ----histogram, echo=FALSE, message=FALSE, fig.width=5, fig.height=5, fig.cap = "Univariate histograms for setosa."----
# create univariate histograms
result <- mvn(data = setosa, mvnTest = "royston", univariatePlot = "histogram")

## ----univariateNormality, message = FALSE-------------------------------------
result <- mvn(data = setosa, mvnTest = "royston",  
              univariateTest = "SW", desc = TRUE)
result$univariateNormality
result$Descriptives

## ----include=FALSE------------------------------------------------------------
mard <- mvn(data = setosa[,-4], mvnTest = "mardia")
hz <- mvn(data = setosa[,-4], mvnTest = "hz")
roys <- mvn(data = setosa[,-4], mvnTest = "royston")
dh <- mvn(data = setosa[,-4], mvnTest = "dh")
en <- mvn(data = setosa[,-4], mvnTest = "energy")

mardiaSkewStat = formatC(as.numeric(levels(mard$multivariateNormality$Statistic))[1],digits=3, format="f")
mardiaSkewPval = formatC(as.numeric(levels(mard$multivariateNormality$`p value`))[1],digits=3, format="f")
mardiaKurtStat = formatC(as.numeric(levels(mard$multivariateNormality$Statistic))[2],digits=3, format="f")
mardiaKurtPval = formatC(as.numeric(levels(mard$multivariateNormality$`p value`))[2],digits=3, format="f")
hzStat = formatC(hz$multivariateNormality$HZ,digits=3, format="f")
hzPval = formatC(hz$multivariateNormality$`p value`, digits=3, format="f")
roysStat = formatC(roys$multivariateNormality$H,digits=3, format="f")
roysPval = formatC(roys$multivariateNormality$`p value`, digits=3, format="f")
dhStat = formatC(dh$multivariateNormality$E,digits=3, format="f")
dhPval = formatC(dh$multivariateNormality$`p value`, digits=3, format="f")
enStat = formatC(en$multivariateNormality$Statistic,digits=3, format="f")
enPval = formatC(en$multivariateNormality$`p value`, digits=3, format="f")

## ----setosa2,  message=FALSE--------------------------------------------------
setosa2 <- iris[1:50, 1:2] 

## ----perspective,  message=FALSE, fig.width=5, fig.height=5, fig.cap = "Perspective plot for bivariate setosa2 data set."----
# perspective plot
result <- mvn(setosa2, mvnTest = "hz", multivariatePlot = "persp") 

## ----contour,  message=FALSE, fig.width=5, fig.height=5, fig.cap = "Contour plot for bivariate setosa2 data set."----
# contour plot
result <- mvn(setosa2, mvnTest = "hz", multivariatePlot = "contour")

## ----include=FALSE------------------------------------------------------------
setosa2 <- iris[1:50, 1:2]
mard <- mvn(data = setosa2, mvnTest = "mardia")
hz <- mvn(data = setosa2, mvnTest = "hz")
roys <- mvn(data = setosa2, mvnTest = "royston")
dh <- mvn(data = setosa2, mvnTest = "dh")
en <- mvn(data = setosa2, mvnTest = "energy")

mardiaSkewStat = formatC(as.numeric(levels(mard$multivariateNormality$Statistic))[1],digits=3, format="f")
mardiaSkewPval = formatC(as.numeric(levels(mard$multivariateNormality$`p value`))[1],digits=3, format="f")
mardiaKurtStat = formatC(as.numeric(levels(mard$multivariateNormality$Statistic))[2],digits=3, format="f")
mardiaKurtPval = formatC(as.numeric(levels(mard$multivariateNormality$`p value`))[2],digits=3, format="f")
hzStat = formatC(hz$multivariateNormality$HZ,digits=3, format="f")
hzPval = formatC(hz$multivariateNormality$`p value`, digits=3, format="f")
roysStat = formatC(roys$multivariateNormality$H,digits=3, format="f")
roysPval = formatC(roys$multivariateNormality$`p value`, digits=3, format="f")
dhStat = formatC(dh$multivariateNormality$E,digits=3, format="f")
dhPval = formatC(dh$multivariateNormality$`p value`, digits=3, format="f")
enStat = formatC(en$multivariateNormality$Statistic,digits=3, format="f")
enPval = formatC(en$multivariateNormality$`p value`, digits=3, format="f")

## ----virginica----------------------------------------------------------------
virginica <- iris[101:150, 1:3]

## ----quan,  message=FALSE, fig.cap = "Multivariate outlier detection based on Mahalanobis distance"----
result <- mvn(data = virginica, mvnTest = "hz", 
              multivariateOutlierMethod = "quan")

## ----adj,  message=FALSE, fig.cap = "Multivariate outlier detection based on adjusted-Mahalanobis distance"----
result <- mvn(data = virginica, mvnTest = "hz",
              multivariateOutlierMethod = "adj")

## ----irisData-----------------------------------------------------------------
head(iris)

## ----subset-------------------------------------------------------------------
result <- mvn(data = iris, subset = "Species", mvnTest = "hz")
result$multivariateNormality

