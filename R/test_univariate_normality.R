#' Univariate Normality Tests
#'
#' Performs one of several common univariate normality tests on each numeric variable
#' in a vector, matrix, or data frame.
#'
#' @param data A numeric vector, matrix, or data frame with observations in rows and variables in columns.
#' Non-numeric columns are dropped with a warning. Each column is tested individually.
#' @param test A character string specifying the normality test to use.
#' Choices are: \code{"SW"} (Shapiro–Wilk), \code{"SF"} (Shapiro–Francia),
#' \code{"AD"} (Anderson–Darling), \code{"CVM"} (Cramér–von Mises), and
#' \code{"Lillie"} (Lilliefors test). Default is the first match from this list.
#'
#' @return A data frame with one row per variable and the following columns:
#' \code{Test}, the name of the test used;
#' \code{Variable}, the name of the tested variable;
#' \code{Statistic}, the test statistic;
#' and \code{p.value}, the associated p-value.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' test_univariate_normality(iris[, 1:4], test = "AD")
#' }
#'
#' @importFrom stats shapiro.test complete.cases
#' @importFrom nortest sf.test ad.test cvm.test lillie.test
#' @export
test_univariate_normality <- function(data, test = c("SW", "CVM", "Lillie", "SF", "AD")) {
    test <- match.arg(test)
    
    # Select appropriate test function and label
    switch(test,
           SW = {fun <- stats::shapiro.test;      TestName <- "Shapiro-Wilk"},
           SF = {fun <- nortest::sf.test;         TestName <- "Shapiro-Francia"},
           AD = {fun <- nortest::ad.test;         TestName <- "Anderson-Darling"},
           CVM= {fun <- nortest::cvm.test;        TestName <- "Cramer-von Mises"},
           Lillie = {fun <- nortest::lillie.test; TestName <- "Lilliefors (KS)"}
    )
    
    # Prepare data frame
    if (is.vector(data)) {
      df <- data.frame(.var = data)
      names(df) <- deparse(substitute(data))
    } else if (is.matrix(data) || is.data.frame(data)) {
      df <- as.data.frame(data)
    } else {
      stop("Input must be a numeric vector, matrix, or data frame.")
    }
    
    # Drop non-numeric columns
    nums <- vapply(df, is.numeric, logical(1))
    if (!all(nums)) {
      warning("Dropping non-numeric columns: ", paste(names(df)[!nums], collapse=", "))
      df <- df[, nums, drop = FALSE]
    }
    if (ncol(df) < 1) stop("No numeric variables to test.")
    
    # Remove missing values per variable
    vars <- names(df)
    results <- lapply(vars, function(var) {
      vec <- df[[var]]
      vec <- vec[stats::complete.cases(vec)]
      if (length(vec) < 3) stop("Too few observations for normality test (n < 3) for variable ", var)
      res <- fun(vec)
      stat <- as.numeric(res$statistic)
      pval <- as.numeric(res$p.value)
      # Format p-value
      list(Statistic = stat, p.value = pval)
    })
    
    # Assemble into data frame
    out <- data.frame(
      Test      = TestName,
      Variable  = vars,
      Statistic = sapply(results, `[[`, "Statistic"),
      p.value   = sapply(results, `[[`, "p.value"),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    return(out)
  }
  