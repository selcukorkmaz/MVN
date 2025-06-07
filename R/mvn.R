utils::globalVariables(c(
  "Statistic", "p.value", "Group", "Test", "Variable", "n", "Mean",
  "Std.Dev", "Median", "Min", "Max", "25th", "75th", "Skew", "Kurtosis",
  "Observation", "Mahalanobis.Distance"
))
#' Comprehensive Multivariate Normality and Diagnostic Function
#'
#' Conduct multivariate normality tests, outlier detection, univariate normality tests,
#' descriptive statistics, and Box-Cox or Yeo-Johnson transformation in one wrapper.
#'
#' @param data A numeric matrix or data frame where each row represents an observation and each column represents a variable. All variables should be numeric; non-numeric columns will be ignored or cause an error depending on implementation.
#' @param subset Optional character string indicating the name of a grouping variable within the data. When provided, analyses will be performed separately for each level of the grouping variable. This is useful for comparing multivariate normality or outlier structure across groups.
#' @param mvn_test A character string specifying which multivariate normality test to use. Supported options include "mardia" (Mardia's test), "hz" (Henze-Zirkler's test), "hw" (Henze-Wagner's test), "royston" (Royston's test), "doornik_hansen" (Doornik-Hansen test), and "energy" (Energy-based test). The default is "hz", which provides good power for detecting departures from multivariate normality.
#' @param use_population A logical value indicating whether to use the population version of the covariance matrix estimator. If TRUE, scales the covariance matrix by (n - 1)/n to estimate the population covariance. If FALSE, the sample covariance matrix is used instead. The default is TRUE.
#' @param tol A small numeric value used as the tolerance parameter for matrix inversion via solve(). This is important when working with nearly singular covariance matrices. The default value is 1e-25, which ensures numerical stability during matrix computations.
#' @param alpha A numeric value specifying the significance level used for defining outliers when the multivariate outlier detection method is set to "adj" (adjusted robust weights). This threshold controls the false positive rate for identifying multivariate outliers. The default is 0.05.
#' @param scale A logical value. If TRUE, the input data will be standardized (zero mean and unit variance) before analysis. This is typically recommended when variables are on different scales. Default is FALSE.
#' @param descriptives A logical value indicating whether to compute descriptive statistics (mean, standard deviation, skewness, and kurtosis) for each variable before conducting multivariate normality or outlier analyses. Default is TRUE.
#' @param transform A character string specifying a marginal transformation to apply to each variable before analysis. Options are "none" (no transformation), "log" (natural logarithm), "sqrt" (square root), and "square" (square of the values). The default is "none".
#' @param impute A character string specifying method for handling missing data. One of \code{"none"}, \code{"mean"}, \code{"median"}, or \code{"mice"}. Default: \code{"none"}.
#' @param power_family A character string specifying the type of power transformation family to apply before analysis. Options include "none" (no transformation), "bcPower" (Box-Cox transformation for positive data), "bcnPower" (Box-Cox transformation that allows for negatives), and "yjPower" (Yeo-Johnson transformation for real-valued data). Default is "none".
#' @param power_transform_type A character string indicating whether to use the "optimal" or "rounded" lambda value for the selected power transformation. "optimal" uses the estimated value with maximum likelihood, while "rounded" uses the closest integer value for interpretability. Default is "optimal".
#' @param bootstrap Logical; if \code{TRUE}, p-values for Mardia, Henze-Zirkler
#'   and Royston tests are obtained via bootstrap resampling. Default is
#'   \code{FALSE}.
#' @param B Integer; number of bootstrap replicates used when
#'   \code{bootstrap = TRUE} or \code{mvn_test = "energy"}. Default is \code{1000}.
#' @param cores Integer; number of cores to use for bootstrap computation.
#'   Default is 1.
#' @param univariate_test A character string indicating which univariate normality test to apply to individual variables when such summaries are requested. Options include "SW" (Shapiro-Wilk), "CVM" (Cramér–von Mises), "Lillie" (Lilliefors/Kolmogorov-Smirnov), "SF" (Shapiro–Francia), and "AD" (Anderson–Darling). Default is "AD".
#' @param multivariate_outlier_method A character string that specifies the method used for detecting multivariate outliers. Options are "none" (no outlier detection), "quan" (robust Mahalanobis distance based on quantile cutoff), and "adj" (adjusted robust weights with a significance threshold). Default is "none".
#' @param show_new_data A logical value. If TRUE, the cleaned data with identified outliers removed will be included in the output. This is useful for downstream analysis after excluding extreme observations. Default is FALSE.
#' @param tidy A logical value. If TRUE, the output will be returned as a tidy data frame, making it easier to use with packages from the tidyverse. A "Group" column will be included when subset analysis is performed. Default is TRUE.

#' @return A named list containing:
#' \describe{
#'   \item{multivariate_normality}{A data frame of the selected multivariate normality (MVN) test results.}
#'   \item{univariate_normality}{A data frame of univariate normality test results.}
#'
#'   \item{descriptives}{(Optional) A data frame of descriptive statistics if \code{descriptives = TRUE}.}
#'   \item{multivariate_outliers}{(Optional) A data frame of flagged multivariate outliers if \code{multivariate_outlier_method != "none"}.}
#'   \item{new_data}{(Optional) Original data with multivariate outliers removed if \code{show_new_data = TRUE}.}
#'   \item{powerTransformLambda}{(Optional) Estimated power transform lambda values if \code{power_family = "bcPower"}.}
#'   \item{data}{The processed data matrix used in the analysis (transformed and/or cleaned).}
#'   \item{subset}{(Optional) The grouping variable used for subset analysis, if applicable.}
#' }
#' @details
#' If \code{mvn_test = "mardia"}, it calculates the Mardia's multivariate skewness and kurtosis coefficients as well as their corresponding statistical significance.
#' It can also calculate corrected version of skewness coefficient for small sample size (n< 20).
#' For multivariate normality, both p-values of skewness and kurtosis statistics should be greater than 0.05.
#' If sample size less than 20 then p.value.small should be used as significance value of skewness instead of p.value.skew.
#' If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.
#'
#' If \code{mvn_test = "hz"}, it calculates the Henze-Zirkler's multivariate normality test. The Henze-Zirkler test is based on a non-negative functional distance that measures the distance between two distribution functions. If the data is multivariate normal, the test statistic HZ is approximately lognormally distributed. It proceeds to calculate the mean, variance and smoothness parameter. Then, mean and variance are lognormalized and the p-value is estimated.
#'
#' If \code{mvn_test = "hw"}, it calculates the Henze-Wagner's multivariate normality test. The Henze-Wagner test is based on a class of weighted L2-statistics that quantify the deviation of the empirical characteristic function from that of the multivariate normal distribution. It uses a weight function involving a smoothness parameter to control the influence of differences in the tails. The test statistic is computed and its null distribution is approximated to obtain the p-value.
#'
#' If \code{mvn_test = "royston"}, it calculates the Royston's multivariate normality test. A function to generate the Shapiro-Wilk's W statistic needed to feed the Royston's H test for multivariate normality However, if kurtosis of the data greater than 3 then Shapiro-Francia test is used for leptokurtic samples else Shapiro-Wilk test is used for platykurtic samples.
#'
#' If \code{mvn_test = "doornik_hansen"}, it calculates the Doornik-Hansen's multivariate normality test. The code is adapted from asbio package (Aho, 2017).
#'
#' If \code{mvn_test = "energy"}, it calculates the Energy multivariate normality test. The code is adapted from energy package (Rizzo and Szekely, 2017).
#'
#' @author Selcuk Korkmaz, \email{selcukorkmaz@gmail.com}
#'
#' @references
#' Korkmaz S, Goksuluk D, Zararsiz G. MVN: An R Package for Assessing Multivariate Normality. The R Journal. 2014 6(2):151-162. URL \url{https://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf}
#'
#' Mardia, K. V. (1970), Measures of multivariate skewness and kurtosis with applications. Biometrika, 57(3):519-530.
#'
#' Mardia, K. V. (1974), Applications of some measures of multivariate skewness and kurtosis for testing normality and robustness studies. Sankhy A, 36:115-128.
#'
#' Henze, N. and Zirkler, B. (1990), A Class of Invariant Consistent Tests for Multivariate Normality. Commun. Statist.-Theor. Meth., 19(10): 35953618.
#'
#' Henze, N. and Wagner, Th. (1997), A New Approach to the BHEP tests for multivariate normality. Journal of Multivariate Analysis, 62:1-23.
#'
#' Royston, J.P. (1982). An Extension of Shapiro and Wilks W Test for Normality to Large Samples. Applied Statistics, 31(2):115124.
#'
#' Royston, J.P. (1983). Some Techniques for Assessing Multivariate Normality Based on the Shapiro-Wilk W. Applied Statistics, 32(2).
#'
#' Royston, J.P. (1992). Approximating the Shapiro-Wilk W-Test for non-normality. Statistics and Computing, 2:117-119.121133.
#'
#' Royston, J.P. (1995). Remark AS R94: A remark on Algorithm AS 181: The W test for normality. Applied Statistics, 44:547-551.
#'
#' Shapiro, S. and Wilk, M. (1965). An analysis of variance test for normality. Biometrika, 52:591611.
#'
#' Doornik, J.A. and Hansen, H. (2008). An Omnibus test for univariate and multivariate normality. Oxford Bulletin of Economics and Statistics 70, 927-939.
#'
#' G. J. Szekely and M. L. Rizzo (2013). Energy statistics: A class of statistics based on distances, Journal of Statistical Planning and Inference, http://dx.doi.org/10.1016/j.jspi.2013.03.018
#'
#' M. L. Rizzo and G. J. Szekely (2016). Energy Distance, WIRES Computational Statistics, Wiley, Volume 8 Issue 1, 27-38. Available online Dec., 2015, http://dx.doi.org/10.1002/wics.1375.
#'
#' G. J. Szekely and M. L. Rizzo (2017). The Energy of Data. The Annual Review of Statistics and Its Application 4:447-79. 10.1146/annurev-statistics-060116-054026
#'
#' @examples
#' result = mvn(data = iris[-4], subset = "Species", mvn_test = "hz",
#'              univariate_test = "AD", 
#'              multivariate_outlier_method = "adj",
#'              show_new_data = TRUE)
#'
#' ### Multivariate Normality Result
#' summary(result, select = "mvn")
#'
#' ### Univariate Normality Result
#' summary(result, select = "univariate")
#'
#' ### Descriptives
#' summary(result, select = "descriptives")
#'
#' ### Multivariate Outliers
#' summary(result, select = "outliers")
#'
#' ### New data without multivariate outliers
#' summary(result, select = "new_data")
#'
#'
#' @importFrom energy mvnorm.etest mvnorm.e
#' @importFrom boot boot
#' @importFrom moments kurtosis skewness
#' @importFrom methods new
#' @importFrom nortest sf.test cvm.test lillie.test ad.test
#' @importFrom MASS kde2d cov.mcd
#' @importFrom car powerTransform
#' @importFrom graphics contour persp abline boxplot curve hist legend par plot text
#' @importFrom stats rnorm var median cor cov dnorm pchisq plnorm pnorm qchisq qnorm qqline qqnorm quantile sd shapiro.test complete.cases mahalanobis
#' @importFrom purrr imap_dfr
#' @importFrom dplyr mutate select %>% bind_rows
#' @importFrom stringr str_remove
#' @importFrom tibble rownames_to_column
#' @export
mvn <- function(data,
                subset = NULL,
                mvn_test = "hz",
                use_population = TRUE,
                tol = 1e-25,
                alpha = 0.05,
                scale = FALSE,
                descriptives = TRUE,
                transform = "none",
                impute = "none",
                bootstrap = FALSE,
                B = 1000,
                cores = 1,
                univariate_test = "AD",
                multivariate_outlier_method = "none",
                power_family = "none",
                power_transform_type = "optimal",
                show_new_data = FALSE,
                tidy = TRUE) {
  
  colnms = colnames(data)
  
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input data must be a data frame or matrix.")
  }
  
  
  if (ncol(data) < 2) {
    stop("Multivariate normality requires at least two variables.")
  }
  
  if (!is.null(subset) && !(subset %in% colnames(data))) {
    stop("The 'subset' variable does not exist in the data.")
  }
  
  p_tmp <- if (!is.null(subset)) ncol(data) - 1 else ncol(data)
  n_tmp <- nrow(data)
  if (p_tmp > n_tmp && mvn_test != "hw") {
    warning("Number of variables exceeds sample size; consider mvn_test = 'hw'.")
  }
  
  power_family <- match.arg(power_family, c("none", "bcPower", "bcnPower", "bcnPowerInverse", "yjPower", "basicPower"))
  power_transform_type <- match.arg(power_transform_type, c("optimal", "rounded"))
  impute <- match.arg(impute, c("none", "mean", "median", "mice"))
  

  if (power_family != "none" && transform != "none") {
    stop("Use either `transform` or `power_family`, not both.")
  }
  
  if (!mvn_test %in% c("mardia", "hz", "hw", "royston", "doornik_hansen", "energy")) {
    stop("Invalid mvn_test. Choose from: 'mardia', 'hz', 'hw', 'royston', 'doornik_hansen', 'energy'.")
  }
  
  if (!univariate_test %in% c("SW", "CVM", "Lillie", "SF", "AD")) {
    stop("Invalid univariate_test. Choose from: 'SW', 'CVM', 'Lillie', 'SF', 'AD'.")
  }
  
  if (power_family == "bcPower" && any(data[sapply(data, is.numeric)] <= 0)) {
    stop("Box-Cox transformation requires strictly positive data.")
  }
  
  
  if (anyNA(data)) {
    if (impute == "none") {
      removed_rows <- sum(!complete.cases(data))
      warning(sprintf("Missing values detected in %d rows. These rows will be removed.", removed_rows))
      data <- data[complete.cases(data), ]
    } else {
      message(sprintf("Missing values detected. Applying '%s' imputation method.", impute))
      data <- impute_missing(data, method = impute)
    }
  }
  
  if (is.null(subset)) {
    if (power_family != "none") {
      result = power_transform(data, family = power_family, type = power_transform_type)
      data = result$data
      powerTransformLambda = result$lambda

    } 
    
    if (transform == "log") {
      data = apply(data, 2, log)
      
    }
    
    if (transform == "sqrt") {
      data = apply(data, 2, sqrt)
      
    }
    
    if (transform == "square") {
      data = apply(data, 2, function(x) {
        return(x^2)
        
      })
      
    }
    
    
    if (!(dim(data)[2] < 2 || is.null(dim(data)))) {
      if (mvn_test == "mardia") {
        mvnResult = mardia(
          data,
          use_population = use_population,
          tol = tol,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
        
      }
      
      if (mvn_test == "hz") {
        mvnResult = hz(
          data,
          use_population = use_population,
          tol = tol,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
        
      }
      
      if (mvn_test == "hw") {
        mvnResult = hw(
          data,
          use_population = use_population,
          tol = tol,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
        
      }
      
      if (mvn_test == "royston") {
        mvnResult = royston(
          data,
          tol = tol,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
        
      }
      
      
      if (mvn_test == "doornik_hansen") {
        mvnResult = doornik_hansen(data,
                                   bootstrap = bootstrap,
                                   B = B,
                                   cores = cores)
        
      }
      
      if (mvn_test == "energy") {
        mvnResult = energy(data, B = B)
        
      }
      
    } else{
      mvnResult = "No MVN result. Number of variables is less than 2!"
    }
    
    if (univariate_test == "SW") {
      uniResult = test_univariate_normality(data, test = "SW")
      
    }
    
    if (univariate_test == "CVM") {
      uniResult = test_univariate_normality(data, test = "CVM")
      
    }
    
    if (univariate_test == "Lillie") {
      uniResult = test_univariate_normality(data, test = "Lillie")
      
    }
    
    if (univariate_test == "SF") {
      uniResult = test_univariate_normality(data, test = "SF")
      
    }
    
    if (univariate_test == "AD") {
      uniResult = test_univariate_normality(data, test = "AD")
      
    }
    
    if (descriptives) {
      descs = descriptives(data)
      descs
      
    } else{
      descs = NULL
    }
    
    if (multivariate_outlier_method != "none") {

      mvOutlierRes = mv_outlier(
        data,
        qqplot = FALSE,
        alpha = alpha,
        method = multivariate_outlier_method,
      )
      mvOutliers = mvOutlierRes$outlier[mvOutlierRes$outlier$Outlier == "TRUE", ]
      newData = mvOutlierRes$newData

    } else{
      mvOutliers = NULL
      newData = NULL

    }

    
  } else{
    if (power_family != "none") {
      sData = split(data[, !(colnames(data) %in% subset)], data[, subset])
      comp <- lapply(sData, complete.cases)
      clean_data <- Map(function(d, c) {
        d[c, ]
      }, sData, comp)
      sData <- lapply(sData, function(x)
        x[complete.cases(x), ])

      result = lapply(sData, power_transform, family = power_family, type = power_transform_type)
      dataList = list()
      powerTransformList = list()

      for (i in 1:length(result)) {
        dataList[[i]] = result[[i]][[1]]
        powerTransformList[[i]] = result[[i]][[2]]
      }

      data = cbind.data.frame(do.call(rbind.data.frame, dataList), data[subset][complete.cases(data), ])

      colnames(data) = colnms

      powerTransformLambda = powerTransformList
      names(powerTransformLambda) = names(sData)

    } 
    
    if (transform == "log") {
      tData = apply(data[!(colnames(data) %in% subset)], 2, log)
      
      data = cbind.data.frame(tData, data[, subset])
      
      colnames(data)[dim(data)[2]] = subset
      
    }
    
    if (transform == "sqrt") {
      tData = apply(data[!(colnames(data) %in% subset)], 2, sqrt)
      
      data = cbind.data.frame(tData, data[, subset])
      
      colnames(data)[dim(data)[2]] = subset
    }
    
    if (transform == "square") {
      tData = apply(data[!(colnames(data) %in% subset)], 2, function(x) {
        return(x^2)
        
      })
      
      data = cbind.data.frame(tData, data[, subset])
      
      colnames(data)[dim(data)[2]] = subset
    }
    
    splitData = split(data[, !(colnames(data) %in% subset)], data[subset])
    
    name = names(splitData)
    
    if (!(is.null(lapply(splitData, dim)[[1]]))) {
      if (mvn_test == "mardia") {
        mvnResult = lapply(
          splitData,
          mardia,
          use_population = use_population,
          tol = tol,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
        
      }
      
      if (mvn_test == "hz") {
        mvnResult = lapply(
          splitData,
          hz,
          use_population = use_population,
          tol = tol,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
      }
      
      
      if (mvn_test == "hw") {
        mvnResult = lapply(
          splitData,
          hw,
          use_population = use_population,
          tol = tol,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
      }
      
      if (mvn_test == "royston") {
        mvnResult = lapply(
          splitData,
          royston,
          tol = tol,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
        
      }
      
      if (mvn_test == "doornik_hansen") {
        mvnResult = lapply(
          splitData,
          doornik_hansen,
          bootstrap = bootstrap,
          B = B,
          cores = cores
        )
        
      }
      
      if (mvn_test == "energy") {
        mvnResult = lapply(splitData, energy, B = B)
        
      }
      
    } else{
      mvnResult = "No MVN result. Number of variables is less than 2 "
    }
    
    if (univariate_test == "SW") {
      uniResult = lapply(splitData, test_univariate_normality, test = "SW")
      
    }
    
    if (univariate_test == "CVM") {
      uniResult = lapply(splitData, test_univariate_normality, test = "CVM")
      
    }
    
    if (univariate_test == "Lillie") {
      uniResult = lapply(splitData, test_univariate_normality, test = "Lillie")
      
    }
    
    if (univariate_test == "SF") {
      uniResult = lapply(splitData, test_univariate_normality, test = "SF")
      
    }
    
    if (univariate_test == "AD") {
      uniResult = lapply(splitData, test_univariate_normality, test = "AD")
      
    }
    
    if (descriptives) {
      descs = lapply(splitData, descriptives)
      
    } else{
      descs = NULL
    }
    
    if (multivariate_outlier_method != "none") {
      mvOutliers = list()
      newData = list()
      
      for (i in 1:length(name)) {
        
        subsetData = splitData[[i]][complete.cases(splitData[[i]]), ]
        mvOutlierRes = mv_outlier(
          subsetData,
          qqplot = FALSE,
          alpha = alpha,
          method = multivariate_outlier_method,
        )
        mvOutliers[[i]] = mvOutlierRes$outlier[mvOutlierRes$outlier$Outlier == "TRUE", ]
        newData[[i]] = mvOutlierRes$newData[order(as.numeric(rownames(mvOutlierRes$newData))), ]
      }
      
      
      names(mvOutliers) = name
      names(newData) = name

    } else{
      mvOutliers = NULL
      newData = NULL
    }
    
    
  }
  
  if (tidy) {
    if (!is.null(subset)) {
      mvnResult <- mvnResult %>%
        purrr::imap_dfr(
          ~ .x %>%
            dplyr::mutate(Group = .y) %>%
            dplyr::select(Group, Test, Statistic, p.value)
        ) %>%
        mutate(
          Statistic = round(Statistic, 3),
          p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)),
          MVN = ifelse(p.value > 0.05, "\u2713 Normal", "\u2717 Not normal")
        )
      
      
      uniResult <- uniResult %>%
        imap_dfr( ~ .x %>%
                    mutate(Group = .y), .id = NULL) %>%
        select(Group, Test, Variable, Statistic, p.value) %>%
        mutate(
          Statistic = round(Statistic, 3),
          p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)),
          Normality = ifelse(p.value > 0.05, "\u2713 Normal", "\u2717 Not normal")
          
        )
      
      if (descriptives) {
        descs <- descs %>%
          imap_dfr( ~ .x %>%
                      mutate(Group = .y), .id = NULL) %>%
          select(Group,
                 n,
                 Mean,
                 Std.Dev,
                 Median,
                 Min,
                 Max,
                 `25th`,
                 `75th`,
                 Skew,
                 Kurtosis) %>%
          mutate(
            Mean = round(Mean, 3),
            Std.Dev = round(Std.Dev, 3),
            Median = round(Median, 3),
            Min = round(Min, 3),
            Max = round(Max, 3),
            `25th` = round(`25th`, 3),
            `75th` = round(`75th`, 3),
            Skew = round(Skew, 3),
            Kurtosis = round(Kurtosis, 3)
            
          )  %>%
          rownames_to_column(var = "Variable") %>%
          mutate(Variable = str_remove(Variable, "\\.\\.\\.[0-9]+$")) %>%
          select(
            Group,
            Variable,
            n,
            Mean,
            Std.Dev,
            Median,
            Min,
            Max,
            `25th`,
            `75th`,
            Skew,
            Kurtosis
          )
        
      }
      
      if (multivariate_outlier_method != "none") {
        mvOutliers <- mvOutliers %>%
          imap_dfr( ~ .x %>%
                      mutate(Group = .y), .id = NULL) %>%
          select(Group, Observation, Mahalanobis.Distance)
      }
      
      
      newData <- do.call(rbind, lapply(names(newData), function(group) {
        df <- newData[[group]]
        df[[subset]] <- group
        df
      }))
      
      
    } else{
      mvnResult <- mvnResult %>%
        mutate(
          Statistic = round(Statistic, 3),
          p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)),
          MVN = ifelse(p.value > 0.05, "\u2713 Normal", "\u2717 Not normal")
          
        )
      
      uniResult <- uniResult %>%
        mutate(
          Statistic = round(Statistic, 3),
          p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)),
          Normality = ifelse(p.value > 0.05, "\u2713 Normal", "\u2717 Not normal")
          
        )
      
      if (descriptives) {
        descs <- descs %>%
          mutate(
            Mean = round(Mean, 3),
            Std.Dev = round(Std.Dev, 3),
            Median = round(Median, 3),
            Min = round(Min, 3),
            Max = round(Max, 3),
            `25th` = round(`25th`, 3),
            `75th` = round(`75th`, 3),
            Skew = round(Skew, 3),
            Kurtosis = round(Kurtosis, 3)
            
          ) %>%
          rownames_to_column(var = "Variable") %>%
          select(Variable,
                 n,
                 Mean,
                 Std.Dev,
                 Median,
                 Min,
                 Max,
                 `25th`,
                 `75th`,
                 Skew,
                 Kurtosis)
        
      }
      
      if (multivariate_outlier_method != "none") {
        mvOutliers <- mvOutliers %>%
          select(Observation, Mahalanobis.Distance)
        
      }
      
    }
    
    }
  
  
  result = list(multivariate_normality = mvnResult,
                univariate_normality = uniResult)
  
  if (descriptives) {
    result = c(result, list(descriptives = descs))
    
  }
  
  if (multivariate_outlier_method != "none") {
    result = c(result, list(multivariate_outliers = mvOutliers))
    
  }
  
  
  if (show_new_data) {
    result = c(result, list(new_data = newData))
    
  }
  
  if (power_family != "none") {
    result = c(result, list(power_transform_lambda = powerTransformLambda))

  } 
  
  result = c(result, list(data = data, subset = subset))
  
  result = c(result, list(outlierMethod = multivariate_outlier_method))
  

  class(result) <- "mvn"
  
  return(result)
  
}
