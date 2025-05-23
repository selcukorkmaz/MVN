result = mvn(data = iris[1:3], subset = NULL, mvn_test = "hz",
             univariate_test = "AD", univariate_plot = "histogram",
             multivariate_plot = "qq", multivariate_outlier_method = "adj",
             show_outliers = TRUE, show_new_data = TRUE, alpha = 0.05)


x = result

result = mvn(data = iris[1:3], subset = NULL, mvn_test = "hz",
             univariate_test = "AD", univariate_plot = "histogram",
             multivariate_plot = "qq", multivariate_outlier_method = "adj",
             show_outliers = TRUE, show_new_data = TRUE, alpha = 0.05)


result$multivariate_outliers


print.mvn(result)



library(tibble)
library(stringr)

descs   %>%
  rownames_to_column(var = "Variable")%>%
  mutate(
    Variable = str_remove(Variable, "\\.\\.\\.[0-9]+$")
  ) %>%
  select(Group, Variable, n, Mean, Std.Dev, Median, Min, Max, `25th`, `75th`, Skew, Kurtosis)


result = mvn(data = iris[1:3], subset = NULL, mvn_test = "hz",
             univariate_test = "AD", univariate_plot = "histogram",
             multivariate_plot = "qq", multivariate_outlier_method = "adj",
             show_outliers = TRUE, show_new_data = TRUE, alpha = 0.05)


result$descriptives





result = mvn(data = iris[-4], subset = "Species", mvn_test = "hz",
             univariate_test = "AD", univariate_plot = "histogram",
             multivariate_plot = "qq", multivariate_outlier_method = "adj",
             show_outliers = TRUE, show_new_data = TRUE,  box_cox_transform = T, box_cox_transform_type = "optimal")




result$multivariate_normality
result$univariate_normality
result$descriptives
result$multivariate_outliers
result$new_data





data,
subset = NULL,
mvn_test = "hz",
covariance = TRUE,
tol = 1e-8,
alpha = 0.05,
desc = TRUE,
transform = c("none", "log", "sqrt", "square"),
bc = FALSE,
bc_type = c("optimal", "rounded"),
R = 1000,
univariate_test = "AD",
univariate_plot = "none",
multivariate_plot = "none",
multivariate_outlier_method = "none",
show_outliers = FALSE,
show_new_data = FALSE

#### Multivariate Normality Result
result$multivariateNormality
result$Descriptives
result$univariateNormality




data = iris[-4]
subset = "Species"
mvn_test = "hz"
covariance = TRUE
tol = 1e-25
alpha = 0.5
scale = FALSE
descriptives = TRUE
transform = "none"
R = 1000
univariate_test = "AD"
univariate_plot = "none"
multivariate_plot = "none"
multivariate_outlier_method = "adj"
box_cox_transform = FALSE
box_cox_transform_type = "optimal"
show_outliers = FALSE
show_new_data = FALSE

