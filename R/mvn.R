#' Comprehensive Multivariate Normality and Diagnostic Function
#'
#' Conduct multivariate normality tests, outlier detection, univariate normality tests,
#' descriptive statistics, and Box-Cox transformation in one wrapper.
#'
#' @param data A numeric matrix or data frame (rows = observations, columns = variables).
#' @param subset Optional character; name of a grouping variable in \code{data} for subset analyses.
#' @param mvn_test Character; one of \code{"mardia"}, \code{"hz"}, \code{"royston"}, 
#'   \code{"doornik_hansen"}, or \code{"energy"}. Default: \code{"hz"}.
#' @param use_population Logical; if \code{TRUE}, uses the population covariance estimator \eqn{\frac{n-1}{n} \times \Sigma}; otherwise uses the sample covariance. Default is \code{TRUE}.
#' @param tol Numeric; tolerance for matrix inversion via \code{solve()}. Default: \code{1e-25}.
#' @param alpha Numeric; significance level for ARW outlier cutoff when 
#'   \code{multivariate_outlier_method = "adj"}. Default: \code{0.05}.
#' @param scale Logical; if \code{TRUE}, standardizes the data before analysis. Default: \code{FALSE}.
#' @param descriptives Logical; if \code{TRUE}, compute descriptive statistics. Default: \code{TRUE}.
#' @param transform Character; one of \code{"none"}, \code{"log"}, \code{"sqrt"}, \code{"square"}.
#'   Applies marginal transformations before analysis. Default: \code{"none"}.
#' @param box_cox_transform Logical; if \code{TRUE}, applies Box-Cox transformation to all variables. Default: \code{FALSE}.
#' @param box_cox_transform_type Character; either \code{"optimal"} or \code{"rounded"} lambda for Box-Cox. Default: \code{"optimal"}.
#' @param R Integer; number of bootstrap replicates for \code{"energy"} test. Default: \code{1000}.
#' @param univariate_test Character; one of \code{"SW"}, \code{"CVM"}, \code{"Lillie"}, 
#'   \code{"SF"}, \code{"AD"}. Default: \code{"AD"}.
#' @param multivariate_outlier_method Character; \code{"none"}, \code{"quan"}, or \code{"adj"}.
#'   Default: \code{"none"}.
#' @param show_outliers Logical; if \code{TRUE}, include outlier table in output. Default: \code{FALSE}.
#' @param show_new_data Logical; if \code{TRUE}, include cleaned data (non-outliers). Default: \code{FALSE}.
#' @param tidy Logical; if \code{TRUE}, returns the results as a tidy data frame with an added \code{Group} column. Default is \code{TRUE}.

#' @return A named list containing:
#'   - multivariate_normality: Data frame of the selected multivariate normality (MVN) test results.
#'   - univariate_normality: Data frame of univariate normality test results.
#' Additional elements (if requested and available):
#'   - descriptives: Data frame of descriptive statistics (if descriptives = TRUE).
#'   - multivariate_outliers: Data frame of flagged multivariate outliers (if show_outliers = TRUE).
#'   - new_data: Original data with multivariate outliers removed (if show_new_data = TRUE).
#'   - box_cox_lambda: Estimated Box-Cox lambda values (if box_cox_transform = TRUE).
#'   - data: Processed data matrix used in the analysis (transformed and/or cleaned).
#'   - subset: The grouping variable used for subset analysis, if applicable.
#'
#' @details
#' If \code{mvn_test = "mardia"}, it calculates the Mardia's multivariate skewness and kurtosis coefficients as well as their corresponding statistical significance.
#' It can also calculate corrected version of skewness coefficient for small sample size (n< 20).
#' For multivariate normality, both p-values of skewness and kurtosis statistics should be greater than 0.05.
#' If sample size less than 20 then p.value.small should be used as significance value of skewness instead of p.value.skew.
#' If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.
#'
#' If \code{mvn_test = "hz"}, it calculates the Henze-Zirkler's multivariate normality test. The Henze-Zirkler test is based on a non-negative functional distance that measures the distance between two distribution functions. If the data is multivariate normal, the test statistic HZ is approximately lognormally distributed. It proceeds to calculate the mean, variance and smoothness parameter. Then, mean and variance are lognormalized and the p-value is estimated.
#' If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.
#'
#' If \code{mvn_test = "royston"}, it calculates the Royston's multivariate normality test. A function to generate the Shapiro-Wilk's W statistic needed to feed the Royston's H test for multivariate normality However, if kurtosis of the data greater than 3 then Shapiro-Francia test is used for leptokurtic samples else Shapiro-Wilk test is used for platykurtic samples.
#' If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed. Do not apply Royston's test, if dataset includes more than 5000 cases or less than 3 cases, since it depends on Shapiro-Wilk's test.
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
#' Mardia, K. V. (1970), Measures of multivariate skewnees and kurtosis with applications. Biometrika, 57(3):519-530.
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
#'              show_outliers = TRUE, show_new_data = TRUE)
#'
#' ### Multivariate Normality Result
#' result$multivariate_normality
#'
#' ### Univariate Normality Result
#' result$univariate_normality
#'
#' ### Descriptives
#' result$descriptives
#'
#' ### Multivariate Outliers
#' result$multivariate_outliers
#'
#' ### New data without multivariate outliers
#' result$new_data
#'
#' # Note that this function also creates univariate histograms,
#' # multivariate Q-Q plots for multivariate normality assessment
#' # and multivariate outlier detection.
#'
#' @importFrom energy mvnorm.etest
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
                R = 1000,
                univariate_test = "AD",
                multivariate_outlier_method = "none",
                box_cox_transform = FALSE,
                box_cox_transform_type = "optimal",
                show_outliers = FALSE,
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
  
  
  if (box_cox_transform && transform != "none") {
    stop("Use either `transform` or `box_cox_transform`, not both.")
  }
  
  if (!mvn_test %in% c("mardia", "hz", "royston", "doornik_hansen", "energy")) {
    stop("Invalid mvn_test. Choose from: 'mardia', 'hz', 'royston', 'doornik_hansen', 'energy'.")
  }
  
  if (!univariate_test %in% c("SW", "CVM", "Lillie", "SF", "AD")) {
    stop("Invalid univariate_test. Choose from: 'SW', 'CVM', 'Lillie', 'SF', 'AD'.")
  }
  
  if (box_cox_transform && any(data <= 0)) {
    stop("Box-Cox transformation requires strictly positive data.")
  }
  
  
  if (anyNA(data)) {
    warning("Missing values detected. Rows with missing data will be removed.")
    data <- data[complete.cases(data), ]
  }
  
  
  if (is.null(subset)) {
    if (box_cox_transform) {
      result = box_cox_transform(data, type = box_cox_transform_type)
      data = result$data
      BoxCoxPower = result$lambda
      
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
        mvnResult = mardia(data, use_population = use_population, tol = tol)
        
      }
      
      if (mvn_test == "hz") {
        mvnResult = hz(data, use_population = use_population, tol = tol)
        
      }
      
      if (mvn_test == "royston") {
        mvnResult = royston(data, tol = tol)
        
      }
      
      
      if (mvn_test == "doornik_hansen") {
        mvnResult = doornik_hansen(data)
        
      }
      
      if (mvn_test == "energy") {
        mvnResult = energy(data, R = R)
        
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
    if (box_cox_transform) {
      sData = split(data[, !(colnames(data) %in% subset)], data[, subset])
      comp <- lapply(sData, complete.cases)
      clean_data <- Map(function(d, c) {
        d[c, ]
      }, sData, comp)
      sData <- lapply(sData, function(x)
        x[complete.cases(x), ])
      
      result = lapply(sData, box_cox_transform, type = box_cox_transform_type)
      dataList = list()
      bcList = list()
      
      for (i in 1:length(result)) {
        dataList[[i]] = result[[i]][[1]]
        bcList[[i]] = result[[i]][[2]]
      }
      
      data = cbind.data.frame(do.call(rbind.data.frame, dataList), data[subset][complete.cases(data), ])
      
      colnames(data) = colnms
      
      
      BoxCoxPower = bcList
      names(BoxCoxPower) = names(sData)
      
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
        mvnResult = lapply(splitData,
                           mardia,
                           use_population = use_population,
                           tol = tol)
        
      }
      
      if (mvn_test == "hz") {
        mvnResult = lapply(splitData,
                           hz,
                           use_population = use_population,
                           tol = tol)
        
      }
      
      if (mvn_test == "royston") {
        mvnResult = lapply(splitData, royston, tol = tol)
        
      }
      
      if (mvn_test == "doornik_hansen") {
        mvnResult = lapply(splitData, doornik_hansen)
        
      }
      
      if (mvn_test == "energy") {
        mvnResult = lapply(splitData, energy, R = R)
        
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
      
      if (show_outliers) {
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
      
      if (show_outliers) {
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
  
  if (show_outliers) {
    result = c(result, list(multivariate_outliers = mvOutliers))
    
  }
  
  
  if (show_new_data) {
    result = c(result, list(new_data = newData))
    
  }
  
  if (box_cox_transform) {
    result = c(result, list(box_cox_lambda = BoxCoxPower))
    
  }
  
  result = c(result, list(data = data, subset = subset))

  class(result) <- "mvn"
  
  return(result)
  
}
