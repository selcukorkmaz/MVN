mardia <- function(data, cov = TRUE, tol = 1e-25){

  dataframe=as.data.frame(data)
  dname <- deparse(substitute(data))
  data <- data[complete.cases(data),]
  data <- as.matrix(data)
  n <- nrow(data)
  p <- ncol(data)
  data.org <- data

  data <- scale(data, scale = FALSE)

  if (cov) {
    S <- ((n - 1)/n) * cov(data)
  }else {
    S <- cov(data)
  }
  D <- data %*% solve(S, tol = tol) %*% t(data)
  g1p <- sum(D^3)/n^2
  g2p <- sum(diag((D^2)))/n
  df <- p * (p + 1) * (p + 2)/6
  k = ((p + 1)*(n + 1)*(n + 3))/(n*((n + 1)*(p + 1) - 6))

  if(n < 20){

    skew <- n * k * g1p/6
    p.skew <- pchisq(skew, df, lower.tail = FALSE)



  }else{

    skew <-  n * g1p/6
    p.skew <-  pchisq(skew, df, lower.tail = FALSE)
  }

  kurt <- (g2p - p * (p + 2)) * sqrt(n/(8 * p * (p + 2)))
  p.kurt <-  2 * (1 - pnorm(abs(kurt)))

  skewMVN = ifelse(p.skew > 0.05, "YES", "NO")
  kurtoMVN = ifelse(p.kurt > 0.05, "YES", "NO")

  MVN = ifelse(p.kurt > 0.05 && p.skew > 0.05, "YES", "NO")

    result <- cbind.data.frame(test = "Mardia", g1p = g1p, chi.skew = skew, p.value.skew = p.skew, skewnewss = skewMVN,
                g2p = g2p, z.kurtosis = kurt, p.value.kurt = p.kurt, kurtosis = kurtoMVN, MVN = MVN)

    resultSkewness = cbind.data.frame(Test = "Mardia Skewness", Statistic = as.factor(skew), "p value" = as.factor(p.skew), Result = skewMVN)
    resultKurtosis = cbind.data.frame(Test = "Mardia Kurtosis", Statistic = as.factor(kurt), "p value" = as.factor(p.kurt), Result = kurtoMVN)
    MVNresult = cbind.data.frame(Test = "MVN", Statistic = NA,"p value" = NA, Result = MVN)

    result = rbind.data.frame(resultSkewness, resultKurtosis, MVNresult)
}

hz <- function(data, cov = TRUE, tol = 1e-25){

  dataframe=as.data.frame(data)
  dname <- deparse(substitute(data))
  data <- data[complete.cases(data),]
  data <- as.matrix(data)
  n <- dim(data)[1]
  p <- dim(data)[2]
  data.org <- data

  if (cov){
    S <- ((n-1)/n)*cov(data)
  }
  else    {
    S <- cov(data)
  }

  dif <- scale(data, scale = FALSE)

  Dj <- diag(dif%*%solve(S, tol = tol)%*%t(dif))  #squared-Mahalanobis' distances

  Y <- data%*%solve(S, tol = tol)%*%t(data)


  Djk <- - 2*t(Y) + matrix(diag(t(Y)))%*%matrix(c(rep(1,n)),1,n) + matrix(c(rep(1,n)),n,1)%*%diag(t(Y))

  b <- 1/(sqrt(2))*((2*p + 1)/4)^(1/(p + 4))*(n^(1/(p + 4))) #smoothing
  {                                                                 #parameter
    if (qr(S)$rank == p){
      HZ = n * (1/(n^2) * sum(sum(exp( - (b^2)/2 * Djk))) - 2 *
                  ((1 + (b^2))^( - p/2)) * (1/n) * (sum(exp( - ((b^2)/(2 *
                                                                         (1 + (b^2)))) * Dj))) + ((1 + (2 * (b^2)))^( - p/2)))
    }
    else {
      HZ = n*4
    }

  }
  wb <- (1 + b^2)*(1 + 3*b^2)

  a <- 1 + 2*b^2

  mu <- 1 - a^(- p/2)*(1 + p*b^2/a + (p*(p + 2)*(b^4))/(2*a^2)) #HZ mean

  si2 <- 2*(1 + 4*b^2)^(- p/2) + 2*a^( - p)*(1 + (2*p*b^4)/a^2 + (3*p*
                                                                    (p + 2)*b^8)/(4*a^4)) - 4*wb^( - p/2)*(1 + (3*p*b^4)/(2*wb) + (p*
                                                                                                                                     (p + 2)*b^8)/(2*wb^2)) #HZ variance

  pmu <- log(sqrt(mu^4/(si2 + mu^2))) #lognormal HZ mean
  psi <- sqrt(log((si2 + mu^2)/mu^2)) #lognormal HZ variance

  pValue <- 1 - plnorm(HZ,pmu,psi) #P-value associated to the HZ statistic

  MVN = ifelse(pValue > 0.05, "YES", "NO")

  result <- cbind.data.frame(Test = "Henze-Zirkler", HZ = HZ, "p value" = pValue, MVN = MVN)

  result
}

royston <- function (data, tol = 1e-25) {
  if (dim(data)[2] < 2 || is.null(dim(data))) {
    stop("number of variables must be equal or greater than 2")
  }
  if (!is.data.frame(data) && !is.matrix(data))
    stop("Input must be one of classes \"data frame\" or \"matrix\"")
  dataframe = as.data.frame(data)
  dname <- deparse(substitute(data))
  data <- data[complete.cases(data), ]
  data <- as.matrix(data)
  p <- dim(data)[2]
  n <- dim(data)[1]
  z <- matrix(nrow <- p, ncol = 1)
  z <- as.data.frame(z)
  w <- matrix(nrow <- p, ncol = 1)
  w <- as.data.frame(w)
  data.org <- data
  if (n <= 3) {
    stop("n must be greater than 3")
  }
  else if (n >= 4 || n <= 11) {
    x <- n
    g <- -2.273 + 0.459 * x
    m <- 0.544 - 0.39978 * x + 0.025054 * x^2 - 0.0006714 *
      x^3
    s <- exp(1.3822 - 0.77857 * x + 0.062767 * x^2 - 0.0020322 *
               x^3)
    for (i in 1:p) {
      a2 <- data[, i]
      {
        if (kurtosis(a2) > 3) {
          w <- sf.test(a2)$statistic
        }
        else {
          w <- shapiro.test(a2)$statistic
        }
      }
      z[i, 1] <- (-log(g - (log(1 - w))) - m)/s
    }
  }
  if (n > 2000) {
    stop("n must be less than 2000")
  }
  else if (n >= 12 || n <= 2000) {
    x <- log(n)
    g <- 0
    m <- -1.5861 - 0.31082 * x - 0.083751 * x^2 + 0.0038915 *
      x^3
    s <- exp(-0.4803 - 0.082676 * x + 0.0030302 * x^2)
    for (i in 1:p) {
      a2 <- data[, i]
      {
        if (kurtosis(a2) > 3) {
          w <- sf.test(a2)$statistic
        }
        else {
          w <- shapiro.test(a2)$statistic
        }
      }
      z[i, 1] <- ((log(1 - w)) + g - m)/s
    }
  }
  else {
    stop("n is not in the proper range")
  }
  u <- 0.715
  v <- 0.21364 + 0.015124 * (log(n))^2 - 0.0018034 * (log(n))^3
  l <- 5
  C <- cor(data)
  NC <- (C^l) * (1 - (u * (1 - C)^u)/v)
  T <- sum(sum(NC)) - p
  mC <- T/(p^2 - p)
  edf <- p/(1 + (p - 1) * mC)
  Res <- matrix(nrow = p, ncol = 1)
  Res <- as.data.frame(Res)
  for (i in 1:p) {
    Res <- (qnorm((pnorm(-z[, ]))/2))^2
  }
  data <- scale(data, scale = FALSE)
  Sa <- cov(data)
  D <- data %*% solve(Sa, tol = tol) %*% t(data)

  RH <- (edf * (sum(Res)))/p
  pValue <- pchisq(RH, edf, lower.tail = FALSE)

  MVN = ifelse(pValue > 0.05, "YES", "NO")

  result <- cbind.data.frame(Test = "Royston", H = RH, "p value" = pValue, MVN = MVN)

  result

}

dh <- function (data){
  data=as.data.frame(data)
  dname <- deparse(substitute(data))
  data <- data[complete.cases(data),]
  n <- nrow(data)
  p <- ncol(data)
  sigma <- var(data)
  sd <- diag(sigma)
  V <- diag(sd^(-0.5))
  C <- V %*% sigma %*% V
  L <- diag((eigen(C)$values)^-0.5)
  H <- eigen(C)$vectors
  yi <- H %*% L %*% t(H) %*% V %*% t(data - sapply(data, mean))
  B1 <- apply(yi, 1, function(x) {
    skewness(x)
  })
  B2 <- apply(yi, 1, function(x) {
    kurtosis(x)
  })
  del <- (n - 3) * (n + 1) * (n^2 + (15 * n) - 4)
  a <- ((n - 2) * (n + 5) * (n + 7) * (n^2 + (27 * n) - 70))/(6 * del)
  c <- ((n - 7) * (n + 5) * (n + 7) * (n^2 + (2 * n) - 5))/(6 * del)
  k <- ((n + 5) * (n + 7) * (n^3 + 37 * n^2 + (11 * n) - 313))/(12 * del)
  alpha <- a + B1^2 * c
  chi <- (B2 - 1 - B1^2) * 2 * k
  Z2 <- (((chi/(2 * alpha))^(1/3)) - 1 + (1/(9 * alpha))) * ((9 * alpha)^(0.5))
  del <- (n - 3) * (n + 1) * (n^2 + (15 * n) - 4)
  a <- ((n - 2) * (n + 5) * (n + 7) * (n^2 + (27 * n) - 70))/(6 * del)
  c <- ((n - 7) * (n + 5) * (n + 7) * (n^2 + (2 * n) - 5))/(6 * del)
  k <- ((n + 5) * (n + 7) * (n^3 + 37 * n^2 + (11 * n) - 313))/(12 * del)
  alpha <- a + B1^2 * c
  chi <- (B2 - 1 - B1^2) * 2 * k
  Z2 <- (((chi/(2 * alpha))^(1/3)) - 1 + (1/(9 * alpha))) * ((9 * alpha)^(0.5))
  beta <- (3 * (n^2 + (27 * n) - 70) * (n + 1) * (n + 3))/((n - 2) * (n + 5) * (n + 7) * (n + 9))
  w2 <- -1 + ((2 * (beta - 1))^0.5)
  del <- 1/((log(sqrt(w2)))^0.5)
  y <- B1 * ((((w2 - 1)/2) * (((n + 1) * (n + 3))/(6 * (n - 2))))^0.5)
  Z1 <- del * (log(y + (y^2 + 1)^0.5))
  E <- t(Z1) %*% Z1 + t(Z2) %*% Z2
  pvalue =  pchisq(E, 2 * p, lower.tail = FALSE)
  res = ifelse(pvalue > 0.05, "YES", "NO")

  result <- cbind.data.frame(test = "Doornik-Hansen", TS = E, df = 2 * p, pval = pvalue, res = res)

  names(result) <- c("Test", "E", "df", "p value", "MVN")

  return(result)

}

energy <- function (data, R = 1000){

  data=as.data.frame(data)
  dname <- deparse(substitute(data))
  data <- data[complete.cases(data),]

    n <- nrow(data)
    d <- ncol(data)
    bootobj <- boot(data, statistic = mvnorm.e, R = R,
                          sim = "parametric", ran.gen = function(x, y) {
                            return(matrix(rnorm(n * d), nrow = n, ncol = d))
                          })

  if (R > 0)
    pvalue <- 1 - mean(bootobj$t < bootobj$t0)
  else pvalue <- NA
  # names(bootobj$t0) <- "E-statistic"
  # e <- list(statistic = bootobj$t0, p.value = p, method = "Energy test of multivariate normality: estimated parameters",
  #           data.name = paste("x, sample size ", n, ", dimension ",
  #                             d, ", replicates ", R, sep = ""))

  statistic = bootobj$t0

  res = ifelse(pvalue > 0.05, "YES", "NO")

  result = cbind.data.frame(test = "E-statistic", stat = statistic, pVal = pvalue, result = res)
  names(result) = c("Test", "Statistic", "p value", "MVN")

  return(result)

}

descriptives <- function(data){

  if (is.data.frame(data) || is.matrix(data)){
      varnames = colnames(data)
      dataFull = data[complete.cases(data),]
      n = apply(dataFull, 2, length)
      meanRes = apply(dataFull, 2, mean, na.rm = TRUE)
      sdRes = apply(dataFull, 2, sd, na.rm = TRUE)
      medianRes = apply(dataFull, 2, median, na.rm = TRUE)
      minRes = apply(dataFull, 2, min, na.rm = TRUE)
      maxRes = apply(dataFull, 2, max, na.rm = TRUE)
      q1Res = apply(dataFull, 2, quantile, na.rm = TRUE)[2,]
      q3Res = apply(dataFull, 2, quantile, na.rm = TRUE)[4,]
      skewRes = apply(dataFull, 2, psych::skew, type=3)
      kurtosisRes = apply(dataFull, 2, psych::kurtosi, type=3)

    }else{

      varnames = "variable"
      n = length(data)
      meanRes = mean(data, na.rm = TRUE)
      sdRes = sd(data, na.rm = TRUE)
      medianRes = median(data, na.rm = TRUE)
      minRes = min(data, na.rm = TRUE)
      maxRes = max(data, na.rm = TRUE)
      q1Res = quantile(data, na.rm = TRUE)[2]
      q3Res = quantile(data, na.rm = TRUE)[4]
      skewRes = psych::skew(data, type=3)
      kurtosisRes = psych::kurtosi(data, type=3)


  }


  descriptives = cbind.data.frame(n, as.numeric(meanRes), as.numeric(sdRes), as.numeric(medianRes), as.numeric(minRes),
                                  as.numeric(maxRes), as.numeric(q1Res), as.numeric(q3Res), as.numeric(skewRes),
                                  as.numeric(kurtosisRes))

  colnames(descriptives) = c("n", "Mean", "Std.Dev", "Median", "Min", "Max", "25th", "75th", "Skew", "Kurtosis")
  rownames(descriptives) = varnames

  descriptives

}

uniNorm <- function(data, type = c("SW", "CVM", "Lillie", "SF", "AD")){
  if (!is.data.frame(data) && !is.matrix(data) && !is.numeric(data)) stop(warning('Input must be one of classes \"vector\", \"data frame\" or \"matrix\"'))
  type = match.arg(type)

  data = data[complete.cases(data),]

  if (type == "AD") TestName = "Anderson-Darling"
  if (type == "CVM") TestName = "Cramer-von Mises"
  if (type == "Lillie") TestName = "Lilliefors (Kolmogorov-Smirnov)"
  if (type == "SW") TestName = "Shapiro-Wilk"
  if (type == "SF") TestName = "Shapiro-Francia"

  if (is.data.frame(data) || is.matrix(data)){
    varNames = colnames(data)
    dims = dim(data)

    if (is.matrix(data)){
      data = data.frame(data)
    }

    if (dims[2] >= 1){
      if (nrow(data) < 2) stop(warning("Too few number of observations (n < 2)."))
      if (is.null(varNames)) varNames = paste("Column",1:ncol(data),sep="")

      res = data.frame(matrix(NA, nrow = ncol(data), ncol=5))
      colnames(res) = c("test", "variable", "statistic", "p.value", "normality")
      res[,"test"] = TestName
      res[,"variable"] = varNames

      if (type == "AD") res2 = apply(data, 2, nortest::ad.test)
      if (type == "CVM") res2 = apply(data, 2, nortest::cvm.test)
      if (type == "Lillie") res2 = apply(data, 2, nortest::lillie.test)
      if (type == "SW") res2 = apply(data, 2, shapiro.test)
      if (type == "SF") res2 = apply(data, 2, nortest::sf.test)

      res[, 3:4] = round(plyr::ldply(res2, .fun = function(x)cbind(x$stat, x$p.value))[,-1],4)

      res$normality = ifelse(res[,4] > 0.05, "YES", "NO")

      res[,4] = apply(res[4],1, function(x){
        if(x == 0){x = "<0.001"}else{x = x}
      }
      )

      name.width <- max(sapply(names(res), nchar))
      res = format(res, width = name.width, justify = "centre")
    }

    names(res) = c("Test", "Variable", "Statistic", "p value", "Normality")

    result = res
  }

  if (!is.matrix(data) && !is.data.frame(data) && (is.null(nrow(data)) || is.null(ncol(data)))){
    if (type == "AD") res = nortest::ad.test(data)
    if (type == "CVM") res = nortest::cvm.test(data)
    if (type == "Lillie") res = nortest::lillie.test(data)
    if (type == "SW") res = shapiro.test(data)
    if (type == "SF") res = nortest::sf.test(data)

    Variable = "variable"
    Statistic = res$statistic
    pValue = res$p.value
    Normality = ifelse(res$p.value > 0.05, "YES", "NO")

    result = cbind.data.frame(Test = TestName, Variable = Variable, Statistic = Statistic, pvalue = pValue, Normality = Normality)

    names(result) = c("Test", "Variable", "Statistic", "p value", "Normality")
  }

  result

}


mvnPlot <- function (data,  type = c("persp", "contour"), default = TRUE,
          plotCtrl = c(perspControl(), contourControl()), ...){
  data = data[complete.cases(data),]
  type <- match.arg(type)
  # if (!(class(object)[1] %in% c("mardia", "hz", "royston")))
  #   stop("Object must be in one of the following classes: \"mardia\", \"hz\", \"royston\" ")
  p <- ncol(data)
  if (p != 2)
    stop("Plots are available for bivariate normal distributions. Number of variables exceed 2.")
  dataframe <- data
  data.kde <- kde2d(dataframe[, 1], dataframe[, 2], n = 100)
  if (type == "persp") {
    if (default) {
      persp(data.kde, theta = 1, phi = 30, border = NA,
            shade = 0.5, box = T, xlab = colnames(dataframe)[1],
            ylab = colnames(dataframe)[2], zlab = "Density")
    }
    else {
      persp(data.kde, theta = plotCtrl$theta, phi = plotCtrl$phi,
            r = plotCtrl$r, d = plotCtrl$d, scale = plotCtrl$scale,
            expand = plotCtrl$expand, col = plotCtrl$col,
            border = plotCtrl$border, ltheta = plotCtrl$ltheta,
            lphi = plotCtrl$lphi, shade = plotCtrl$shade,
            box = plotCtrl$box, axes = plotCtrl$axes, nticks = plotCtrl$nticks,
            ticktype = plotCtrl$ticktype, xlab = plotCtrl$xlab,
            ylab = plotCtrl$ylab, zlab = plotCtrl$zlab, main = plotCtrl$main)
    }
  }
  if (type == "contour") {
    if (default) {
      contour(data.kde, nlevels = 20, xlab = colnames(dataframe)[1],
              ylab = colnames(dataframe)[2])
    }
    else {
      contour(data.kde, nlevels = plotCtrl$nlevels, xlab = plotCtrl$xlab,
              ylab = plotCtrl$ylab, labcex = plotCtrl$labcex,
              drawlabels = plotCtrl$drawlabels, method = plotCtrl$method,
              axes = plotCtrl$axes, frame.plot = plotCtrl$frame.plot,
              col = plotCtrl$col, lty = plotCtrl$lty, lwd = plotCtrl$lwd)
    }
  }
}

perspControl <- function (theta = 1, phi = 30, r = sqrt(3), d = 1, scale = TRUE,
          expand = 1, col = "white", border = NULL, ltheta = -135,
          lphi = 0, shade = 0.5, box = TRUE, axes = TRUE, nticks = 5,
          ticktype = "simple", xlab = NULL, ylab = NULL, zlab = NULL,
          main = NULL)
{
  list(theta = theta, phi = phi, r = r, d = d, scale = scale,
       expand = expand, col = col, border = border, ltheta = ltheta,
       lphi = lphi, shade = shade, box = box, axes = axes, nticks = nticks,
       ticktype = ticktype, xlab = xlab, ylab = ylab, zlab = zlab,
       main = main)
}


contourControl <- function (nlevels = 20, labels = NULL, xlab = NULL, ylab = NULL,
          labcex = 0.6, drawlabels = TRUE, method = c("simple", "edge",
                                                      "flattest"), axes = TRUE, frame.plot = TRUE, col = par("fg"),
          lty = par("lty"), lwd = par("lwd"))
{
  list(nlevels = nlevels, labels = labels, xlab = xlab, ylab = ylab,
       labcex = labcex, drawlabels = drawlabels, method = method,
       axes = axes, frame.plot = frame.plot, col = col, lty = lty,
       lwd = lwd)
}


mvOutlier <- function (data, qqplot = TRUE, alpha = 0.5, tol = 1e-25, method = c("quan",
                                                                    "adj"), label = TRUE, position = NULL, offset = 0.5, main)
{
  if (!is.data.frame(data) && !is.matrix(data))
    stop("Input must be one of classes \"data frame\" or \"matrix\"")
  if (dim(data)[2] < 2 || is.null(dim(data))) {
    stop("number of variables must be equal or greater than 2")
  }
  data = data[complete.cases(data),]
  dataframe = as.data.frame(data)
  dname <- deparse(substitute(data))
  method <- match.arg(method)
  n <- dim(data)[1]
  p <- dim(data)[2]
  covr <- covMcd(data, alpha = alpha)
  mah <- mahalanobis(data, center = covr$center, cov = covr$cov,
                     tol = tol)
  d <- mah
  sortMah <- data.frame(sort(mah, decreasing = TRUE))
  out <- cbind(rownames(sortMah), round(sortMah, 3), NA)
  colnames(out) <- c("Observation", "Mahalanobis Distance",
                     "Outlier")
  if (method == "adj") {
    crt <- arw(x = data, m0 = covr$center, c0 = covr$cov,
               alpha = 0.025)$cn
    for (i in 1:n) {
      {
        if (sortMah[i, ] > crt) {
          out[i, 3] <- "TRUE"
        }
        else {
          out[i, 3] <- "FALSE"
        }
      }
    }
    if (qqplot) {
      d <- mah
      r <- rank(d)
      chi2q <- qchisq((r - 0.5)/n, p)
      colors = NULL
      for (i in 1:n) {
        if (d[i] > crt)
          colors[i] = "red"
        else colors[i] = "black"
      }
      plot(d, chi2q, pch = 16, main = main,
           xlab = "Robust Squared Mahalanobis Distance",
           ylab = "Chi-Square Quantile", col = colors)
      abline(v = crt, lwd = 2, col = "blue")
      tbl = table(out[, 3])
      legend("topleft", legend = c(paste("Outliers (n=",
                                         if (is.na(tbl[2])) 0 else tbl[2], ")", sep = ""),
                                   paste("Non-outliers (n=", if (is.na(tbl[1])) 0 else tbl[1],
                                         ")", sep = "")), col = c("red", "black"), pch = 16,  bty = "n")
      if (label && is.element("TRUE", out[, 3])) {
        labelOutlier <- rownames(out)[out[, 3] == TRUE]
        xCoord <- out[out[, 3] == TRUE, 2]
        yCoord <- sort(chi2q, decreasing = T)[1:length(xCoord)]
        text(xCoord, yCoord, labelOutlier, pos = position,
             offset = offset)
      }
      if (max(d) >= crt) {
        text(crt - 0.2, 2, paste("Quantile: ", round(crt,
                                                     3)), srt = 90, pos = 3, col = "blue")
      }
    }
    newData <- out[out$Outlier %in% "FALSE", ]
    ind <- sort(row.names(newData))
    newData <- data[ind, ]
    result <- list(out, newData)
    names(result) <- c("outlier", "newData")
  }
  if (method == "quan") {
    chiSq <- qchisq(0.975, p)
    for (i in 1:n) {
      {
        if (sortMah[i, ] > chiSq) {
          out[i, 3] <- "TRUE"
        }
        else {
          out[i, 3] <- "FALSE"
        }
      }
    }
    if (qqplot) {
      d <- mah
      r <- rank(d)
      chi2q <- qchisq((r - 0.5)/n, p)
      colors = NULL
      for (i in 1:n) {
        if (d[i] > chiSq)
          colors[i] = "red"
        else colors[i] = "black"
      }
      plot(d, chi2q, pch = 16, col = colors, main = main,
           xlab = "Robust Squared Mahalanobis Distance",
           ylab = "Chi-Square Quantile")
      abline(v = chiSq, lwd = 2, col = "red")
      tbl = table(out[, 3])
      legend("topleft", legend = c(paste("Outliers (n=",
                                         if (is.na(tbl[2])) 0 else tbl[2], ")", sep = ""),
                                   paste("Non-outliers (n=", if (is.na(tbl[1])) 0 else tbl[1],
                                         ")", sep = "")), col = c("red", "black"), pch = 16, bty = "n")
      if (label && is.element("TRUE", out[, 3])) {
        labelOutlier <- rownames(out)[out[, 3] == TRUE]
        xCoord <- out[out[, 3] == TRUE, 2]
        yCoord <- sort(chi2q, decreasing = T)[1:length(xCoord)]
        text(xCoord, yCoord, labelOutlier, pos = position,
             offset = offset)
      }
      if (max(d) >= chiSq) {
        text(chiSq - 0.2, 2, paste("Quantile: ", round(chiSq,
                                                       3)), srt = 90, pos = 3, col = "red")
      }
    }
    newData <- out[out$Outlier %in% "FALSE", ]
    ind <- sort(row.names(newData))
    newData <- data[ind, ]
    result <- list(out, newData)
    names(result) <- c("outlier", "newData")
  }
  return(result)
}

uniPlot <- function (data, type = c("qqplot", "histogram", "box", "scatter"),
          mfrow = NULL, ...)
{
  if (!is.data.frame(data) && !is.matrix(data) && !is.numeric(data))
    stop(warning("Input must be one of classes \"vector\", \"data frame\" or \"matrix\""))
  # type = match.arg(type)
  if (is.data.frame(data) || is.matrix(data)) {
    data = data[complete.cases(data),]
    data = as.data.frame(data)
    if (nrow(data) < 2)
      stop(warning("Too few number of observations (n < 2)."))
    if (is.null(colnames(data)))
      varNames = paste("Column", 1:ncol(data), sep = "")
    if (!is.null(colnames(data)))
      varNames = colnames(data)
    if (is.null(mfrow)) {
      nCol = ceiling(sqrt(ncol(data)))
      nRow = ceiling(ncol(data)/nCol)
    }
    if (type != "box") {
      if (is.null(mfrow))
        par(mfrow = c(nRow, nCol))
      else par(mfrow = mfrow)
    }
    if (type == "histogram") {
      for (i in 1:ncol(data)) {
        hist(data[, i], xlab = varNames[i], freq = FALSE,
             main = "", ...)
        x <- NULL
        rm(x)
        curve(dnorm(x, mean = mean(data[, i]), sd = sd(data[,
                                                            i])), col = "red", add = TRUE)
      }
    }
    if (type == "qqplot") {
      for (i in 1:ncol(data)) {
        qqnorm(data[, i], main = paste("Normal Q-Q Plot (",
                                       varNames[i], ")", sep = ""), ...)
        qqline(data[, i])
      }
    }
    if (type == "scatter") {
      if (nrow(data) == 1 || ncol(data) == 1)
        stop(warning("Not available for univariate input."))
      plot(data, ...)
    }
    if (type == "box") {
      warning("Box-Plots are based on standardized values (centered and scaled).")
      boxplot(scale(data), names = varNames, ...)
    }
  }
  if (is.null(ncol(data)) || is.null(nrow(data))) {
    par(mfrow = c(1, 1))
    data = as.numeric(data)
    if (type == "histogram") {
      hist(data, freq = FALSE, main = "", ...)
      curve(dnorm(x, mean = mean(data), sd = sd(data)),
            col = "red", add = TRUE)
    }
    if (type == "qqplot") {
      qqnorm(data, ...)
      qqline(data)
    }
    if (type == "box") {
      boxplot(data, ...)
    }
    if (type == "scatter") {
      stop(warning("Not available for univariate input."))
    }
  }
}

BoxCox <- function(data, type = c("optimal", "rounded")){

  data = data[complete.cases(data),]
  powerTransformation = summary(powerTransform(data))$result

  if(type == "optimal"){

    lambda = powerTransformation[,1]

  }

  if(type == "rounded"){

    lambda = powerTransformation[,2]

  }

  for(i in 1:length(lambda)){

    if(lambda[[i]] == 0){

      data[i] = log(data[i])

    }else{

      data[i] = data[i]^lambda[[i]]

    }
  }

  result = list(data, lambda)

  return(result)
}



#' Multivariate Normality Tests
#'
#' Performs multivariate normality tests, including Marida, Royston, Henze-Zirkler, Dornik-Haansen, E-Statistics, and graphical approaches and implements multivariate outlier detection and univariate normality of marginal distributions through plots and tests, and performs multivariate Box-Cox transformation.
#'
#' @param data a numeric matrix or data frame
#' @param subset define a variable name if subset analysis is required
#' @param mvnTest select one of the MVN tests. Type \code{"mardia"} for Mardia's test, \code{"hz"} for Henze-Zirkler's test, \code{"royston"} for Royston's test, \code{"dh"} for Doornik-Hansen's test and \code{energy} for E-statistic. Default is Henze-Zirkler's test \code{"hz"}. See details for further information.
#' @param covariance this option works for \code{"mardia"} and \code{"royston"}. If \code{TRUE} covariance matrix is normalized by \code{n}, if \code{FALSE} it is normalized by \code{n-1}
#' @param tol a numeric tolerance value which isused for inversion of the covariance matrix (\code{default = 1e-25}
#' @param alpha a numeric parameter controlling the size of the subsets over which the determinant is minimized. Allowed values for the alpha are between 0.5 and 1 and the default is 0.5.
#' @param scale if \code{TRUE} scales the colums of data
#' @param desc a logical argument. If \code{TRUE} calculates descriptive statistics
#' @param transform select a transformation method to transform univariate marginal via logarithm (\code{"log"}), square root (\code{"sqrt"}) and square (\code{"square"}).
#' @param R number of bootstrap replicates for Energy test, default is 1000.
#' @param univariateTest select one of the univariate normality tests, Shapiro-Wilk (\code{"SW"}), Cramer-von Mises (\code{"CVM"}), Lilliefors (\code{"Lillie"}), Shapiro-Francia (\code{"SF"}), Anderson-Darling (\code{"AD"}). Default is Anderson-Darling (\code{"AD"}). Do not apply Shapiro-Wilk's test, if dataset includes more than 5000 cases or less than 3 cases.
#' @param univariatePlot select one of the univariate normality plots, Q-Q plot (\code{"qq"}), histogram (\code{"histogram"}), box plot (\code{"box"}), scatter (\code{"scatter"})
#' @param multivariatePlot \code{"qq"} for chi-square Q-Q plot, \code{"persp"} for perspective plot, \code{"contour"} for contour plot
#' @param multivariateOutlierMethod select multivariate outlier detection method, \code{"quan"} quantile method based on Mahalanobis distance and \code{"adj"} adjusted quantile method based on Mahalanobis distance
#' @param bc if \code{TRUE} it applies Box-Cox power transformation
#' @param bcType select \code{"optimal"} or \code{"rounded"} type of Box-Cox power transformation, only applicable if \code{bc = TRUE}, default is \code{"rounded"}
#' @param showOutliers if \code{TRUE} prints multivariate outliers
#' @param showNewData if \code{TRUE} prints new data without outliers
#'
#' @return \code{multivariateNormality} corresponding multivariate normality test statistics and p-value
#' @return \code{univariateNormality} corresponding univariate normality test statistics and p-value
#' @return \code{Descriptives} Descriptive statistics
#' @return \code{multivariateOutliers} multivariate outliers
#' @return \code{newData} new data without multivariate outliers
#' @return multivariate normality plots, Q-Q, perspective or contour
#' @return chi-square Q-Q plot for multivariate outliers
#' @return univariate normality plots, Q-Q plot, histogram, box plot, scatter
#'
#'@details
#'If \code{mvnTest = "mardia"}, it calculate the Mardia's multivariate skewness and kurtosis coefficients as well as their corresponding statistical significance.
#'It can also calculate corrected version of skewness coefficient for small sample size (n< 20).
#'For multivariate normality, both p-values of skewness and kurtosis statistics should be greater than 0.05.
#'If sample size less than 20 then p.value.small should be used as significance value of skewness instead of p.value.skew.
#'If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.
#'
#'If \code{mvnTest = "hz"}, it calculate the Henze-Zirkler's multivariate normality test. The Henze-Zirkler test is based on a non-negative functional distance that measures the distance between two distribution functions. If the data is multivariate normal, the test statistic HZ is approximately lognormally distributed. It proceeds to calculate the mean, variance and smoothness parameter. Then, mean and variance are lognormalized and the p-value is estimated.
#'If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.
#'
#'If \code{mvnTest = "royston"}, it calculate the Royston's multivariate normality test. A function to generate the Shapiro-Wilk's W statistic needed to feed the Royston's H test for multivariate normality However, if kurtosis of the data greater than 3 then Shapiro-Francia test is used for leptokurtic samples else Shapiro-Wilk test is used for platykurtic samples.
#'If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed. Do not apply Royston's test, if dataset includes more than 5000 cases or less than 3 cases, since it depends on Shapiro-Wilk's test.
#'
#'If \code{mvnTest = "dh"}, it calculate the Doornik-Hansen's multivariate normality test. The code is adapted from asbio package (Aho, 2017).
#'
#'#'If \code{mvnTest = "energy"}, it calculate the Doornik-Hansen's multivariate normality test. The code is adapted from energy package (Rizzo and Szekely, 2017)i
#'
#' @author Selcuk Korkmaz, \email{selcukorkmaz@gmail.com}
#'
#' @references
#'
#'Korkmaz S, Goksuluk D, Zararsiz G. MVN: An R Package for Assessing Multivariate Normality. The R Journal. 2014 6(2):151-162. URL \url{https://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf}
#'
#'Mardia, K. V. (1970), Measures of multivariate skewnees and kurtosis with applications. Biometrika, 57(3):519-530.
#'
#'Mardia, K. V. (1974), Applications of some measures of multivariate skewness and kurtosis for testing normality and robustness studies. Sankhy A, 36:115-128.
#'
#'Henze, N. and Zirkler, B. (1990), A Class of Invariant Consistent Tests for Multivariate Normality. Commun. Statist.-Theor. Meth., 19(10): 35953618.
#'
#'Henze, N. and Wagner, Th. (1997), A New Approach to the BHEP tests for multivariate normality. Journal of Multivariate Analysis, 62:1-23.
#'
#'Royston, J.P. (1982). An Extension of Shapiro and Wilks W Test for Normality to Large Samples. Applied Statistics, 31(2):115124.
#'
#'Royston, J.P. (1983). Some Techniques for Assessing Multivariate Normality Based on the Shapiro-Wilk W. Applied Statistics, 32(2).
#'
#'Royston, J.P. (1992). Approximating the Shapiro-Wilk W-Test for non-normality. Statistics and Computing, 2:117-119.121133.
#'
#'Royston, J.P. (1995). Remark AS R94: A remark on Algorithm AS 181: The W test for normality. Applied Statistics, 44:547-551.
#'
#'Shapiro, S. and Wilk, M. (1965). An analysis of variance test for normality. Biometrika, 52:591611.
#'
#'Doornik, J.A. and Hansen, H. (2008). An Omnibus test for univariate and multivariate normality. Oxford Bulletin of Economics and Statistics 70, 927-939.
#'
#'G. J. Szekely and M. L. Rizzo (2013). Energy statistics: A class of statistics based on distances, Journal of Statistical Planning and Inference, http://dx.doi.org/10.1016/j.jspi.2013.03.018
#'
#'M. L. Rizzo and G. J. Szekely (2016). Energy Distance, WIRES Computational Statistics, Wiley, Volume 8 Issue 1, 27-38. Available online Dec., 2015, http://dx.doi.org/10.1002/wics.1375.
#'
#'G. J. Szekely and M. L. Rizzo (2017). The Energy of Data. The Annual Review of Statistics and Its Application 4:447-79. 10.1146/annurev-statistics-060116-054026
#'
#' @examples
#' result = mvn(data = iris[-4], subset = "Species", mvnTest = "hz",
#'              univariateTest = "AD", univariatePlot = "histogram",
#'              multivariatePlot = "qq", multivariateOutlierMethod = "adj",
#'              showOutliers = TRUE, showNewData = TRUE)
#'
#' #### Multivariate Normality Result
#' result$multivariateNormality
#'
#' ### Univariate Normality Result
#' result$univariateNormality
#'
#' ### Descriptives
#' result$Descriptives
#'
#' ### Multivariate Outliers
#' result$multivariateOutliers
#'
#' ### New data without multivariate outliers
#' result$newData
#'
#' # Note that this function also creates univariate histograms,
#' # multivariate Q-Q plots for multivariate normality assessment
#' # and multivariate outlier detection.
#'
#' @export
#' @import magrittr
#' @import kableExtra
#' @importFrom energy mvnorm.e
#' @importFrom boot boot
#' @importFrom moments kurtosis skewness
#' @importFrom methods new
#' @importFrom nortest sf.test cvm.test lillie.test ad.test
#' @importFrom robustbase covMcd
#' @importFrom MASS kde2d
#' @importFrom mvoutlier arw
#' @importFrom psych describe
#' @importFrom car powerTransform
#' @importFrom graphics contour persp abline boxplot curve hist legend par plot text
#' @importFrom stats rnorm var median cor cov dnorm pchisq plnorm pnorm qchisq qnorm qqline qqnorm quantile sd shapiro.test complete.cases mahalanobis
#'

mvn <- function(data, subset = NULL, mvnTest = c("mardia", "hz", "royston", "dh", "energy"), covariance = TRUE, tol = 1e-25, alpha = 0.5, scale = FALSE, desc = TRUE, transform = "none", R = 1000,
                univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"), univariatePlot = "none",  multivariatePlot = "none", multivariateOutlierMethod = "none",
                bc = FALSE, bcType = "rounded", showOutliers = FALSE, showNewData = FALSE){

  mvnTest <- match.arg(mvnTest)
  univariateTest <- match.arg(univariateTest)

  colnms = colnames(data)

  if(bc && transform != "none"){

    stop("Please select transform = 'none' if you apply Box-Cox transformation or select bc = FALSE and apply one of the transformation method directly, as log, sqrt and square.")
  }

  if(is.null(subset)){

      if(bc){

      result = BoxCox(data, type = bcType)
      data = result[[1]]
      BoxCoxPower = result[[2]]

    }

      if(transform == "log"){

      data = apply(data,2,log)

    }

      if(transform == "sqrt"){

      data = apply(data,2,sqrt)

    }

      if(transform == "square"){

        data = apply(data,2,function(x){

        return(x^2)

      })

    }


    if (!(dim(data)[2] < 2 || is.null(dim(data)))){

        if(mvnTest == "mardia"){

          mvnResult = mardia(data, cov = covariance, tol = tol)

        }

        if(mvnTest == "hz"){

          mvnResult = hz(data, cov = covariance, tol = tol)

        }

        if(mvnTest == "royston"){

          mvnResult = royston(data, tol = tol)

        }


       if(mvnTest == "dh"){

         mvnResult = dh(data)

        }

      if(mvnTest == "energy"){

        mvnResult = energy(data, R = R)

        }

      if (multivariatePlot == "qq") {

        n <- dim(data)[1]
        p <- dim(data)[2]


        if (covariance) {
          S <- ((n - 1)/n) * cov(data)
        }
        else {
          S <- cov(data)
        }

        dif <- scale(data, scale = FALSE)

        d <- diag(dif %*% solve(S, tol = tol) %*% t(dif))

        r <- rank(d)

        chi2q <- qchisq((r - 0.5)/n, p)
        plot(d, chi2q, pch = 19, main = "Chi-Square Q-Q Plot",
             xlab = "Squared Mahalanobis Distance", ylab = "Chi-Square Quantile")
        abline(0, 1, lwd = 2, col = "black")
      }

      if (multivariatePlot == "persp") {

        mvnPlot(data, type = "persp", default = TRUE,
                plotCtrl = c(perspControl(), contourControl()))

      }

      if (multivariatePlot == "contour") {

        mvnPlot(data, type = "contour", default = TRUE,
                plotCtrl = c(perspControl(), contourControl()))

      }


      }else{mvnResult = "No MVN result. Number of variables is less than 2!"}

        if(univariateTest == "SW"){

          uniResult = uniNorm(data, type = "SW")

        }

        if(univariateTest == "CVM"){

          uniResult = uniNorm(data, type = "CVM")

        }

        if(univariateTest == "Lillie"){

         uniResult = uniNorm(data, type = "Lillie")

       }

        if(univariateTest == "SF"){

          uniResult = uniNorm(data, type = "SF")

       }

        if(univariateTest == "AD"){

        uniResult = uniNorm(data, type = "AD")

      }

        if(desc){

           descs = descriptives(data)
           descs

        }else{descs = NULL}

    if(multivariateOutlierMethod != "none"){

    if(multivariateOutlierMethod == "quan"){

      main = "Chi-Square Q-Q Plot"

    }else{

      main = "Adjusted Chi-Square Q-Q Plot"

    }

       mvOutlierRes = mvOutlier(data, qqplot = TRUE, alpha = alpha, tol = tol, method = multivariateOutlierMethod, label = TRUE, position = NULL, offset = 0.5, main = main)
       mvOutliers = mvOutlierRes$outlier[mvOutlierRes$outlier$Outlier == "TRUE",]
       newData = mvOutlierRes$newData

    }

    if(univariatePlot != "none"){

      uniPlot(data, type = univariatePlot)

    }

    }else{


      if(bc){

        sData = split(data[,!(colnames(data) %in% subset)], data[,subset])
        comp <- lapply(sData, complete.cases)
        clean_data <- Map(function(d,c) {d[c,]}, sData, comp)
        sData <- lapply(sData, function(x) x[complete.cases(x),])

        result = lapply(sData, BoxCox, type = bcType)
        dataList = list()
        bcList = list()

        for(i in 1:length(result)){

          dataList[[i]] = result[[i]][[1]]
          bcList[[i]] = result[[i]][[2]]
        }

        data = cbind.data.frame(do.call(rbind.data.frame, dataList), data[subset][complete.cases(data),])

        colnames(data) = colnms


        BoxCoxPower = bcList
        names(BoxCoxPower) = names(sData)

      }

      if(transform == "log"){

        tData = apply(data[!(colnames(data) %in% subset)],2,log)

        data = cbind.data.frame(tData, data[,subset])

        colnames(data)[dim(data)[2]] = subset

      }

      if(transform == "sqrt"){

        tData = apply(data[!(colnames(data) %in% subset)],2,sqrt)

        data = cbind.data.frame(tData, data[,subset])

        colnames(data)[dim(data)[2]] = subset
      }

      if(transform == "square"){

        tData = apply(data[!(colnames(data) %in% subset)],2,function(x){

          return(x^2)

        })

        data = cbind.data.frame(tData, data[,subset])

        colnames(data)[dim(data)[2]] = subset
      }

      splitData = split(data[,!(colnames(data) %in% subset)], data[subset])

      name = names(splitData)

      if (!(is.null(lapply(splitData, dim)[[1]]))){

        if(mvnTest == "mardia"){

          mvnResult = lapply(splitData, mardia, cov = covariance, tol = tol)

        }

        if(mvnTest == "hz"){

          mvnResult = lapply(splitData, hz, cov = covariance, tol = tol)

        }

        if(mvnTest == "royston"){

          mvnResult = lapply(splitData, royston, tol = tol)

        }

        if(mvnTest == "dh"){

          mvnResult = lapply(splitData, dh)

        }

        if(mvnTest == "energy"){

          mvnResult = lapply(splitData, energy, R = R)

        }

      }else{mvnResult = "No MVN result. Number of variables is less than 2 "}

      if(univariateTest == "SW"){

        uniResult = lapply(splitData, uniNorm, type = "SW")

      }

      if(univariateTest == "CVM"){

        uniResult = lapply(splitData, uniNorm, type = "CVM")

      }

      if(univariateTest == "Lillie"){

        uniResult = lapply(splitData, uniNorm, type = "Lillie")

      }

      if(univariateTest == "SF"){

        uniResult = lapply(splitData, uniNorm, type = "SF")

      }

      if(univariateTest == "AD"){

        uniResult = lapply(splitData, uniNorm, type = "AD")

      }

      if(desc){

        descs = lapply(splitData, descriptives)

      }else{descs = NULL}


      if (multivariatePlot == "qq") {

        for(i in 1:length(name)){

          subsetData = splitData[[i]][complete.cases(splitData[[i]]),]


          n <- dim(subsetData)[1]
          p <- dim(subsetData)[2]


          if (covariance) {
            S <- ((n - 1)/n) * cov(subsetData)
          }else {
            S <- cov(subsetData)
          }

          dif <- scale(subsetData, scale = FALSE)

          d <- diag(dif %*% solve(S, tol = tol) %*% t(dif))

          r <- rank(d)

          chi2q <- qchisq((r - 0.5)/n, p)
          plot(d, chi2q, pch = 19, main = paste0("Chi-Square Q-Q Plot for ", name[i]),
               xlab = "Squared Mahalanobis Distance", ylab = "Chi-Square Quantile")
          abline(0, 1, lwd = 2, col = "black")

        }
      }

      if (multivariatePlot == "persp") {

        for(i in 1:length(name)){

        subsetData = splitData[[i]][complete.cases(splitData[[i]]),]

        mvnPlot(subsetData, type = "persp", default = TRUE,
                plotCtrl = c(perspControl(), contourControl()), main = paste0("Perspective Plot for ", name[i]))
        }
      }

      if (multivariatePlot == "contour") {

        for(i in 1:length(name)){

        subsetData = splitData[[i]][complete.cases(splitData[[i]]),]

        mvnPlot(subsetData, type = "contour", default = TRUE,
                plotCtrl = c(perspControl(), contourControl()), main = paste0("Contour Plot for ", name[i]))
        }
      }


      if(multivariateOutlierMethod != "none"){

        mvOutliers = list()
        newData = list()

        if(multivariateOutlierMethod == "quan"){

          main = paste0("Chi-Square Q-Q Plot for ", name[[i]])

        }else{

          main = paste0("Adjusted Chi-Square Q-Q Plot for ", name[[i]])

        }


        for(i in 1:length(name)){
          subsetData = splitData[[i]][complete.cases(splitData[[i]]),]
          mvOutlierRes = mvOutlier(subsetData, qqplot = TRUE, alpha = 0.5, tol = 1e-25, method = multivariateOutlierMethod, label = TRUE, position = NULL, offset = 0.5, main = main)
          mvOutliers[[i]] = mvOutlierRes$outlier[mvOutlierRes$outlier$Outlier == "TRUE",]
          newData[[i]] = mvOutlierRes$newData
        }


        names(mvOutliers) = name
        names(newData) = name
      }


      if(univariatePlot != "none"){

        lapply(splitData, uniPlot, type = univariatePlot)

      }


    }

  result = list(multivariateNormality = mvnResult, univariateNormality = uniResult)

    if(!is.null(descs)){

      result = c(result, list(Descriptives = descs))

    }

   if (showOutliers){

      result = c(result, list(multivariateOutliers = mvOutliers))

    }

   if (showNewData){

    result = c(result, list(newData = newData))

   }

  if (bc){

    result = c(result, list(BoxCoxPowerTransformation = BoxCoxPower))

  }

  return(result)

}



mvn.lm <- function(object, test = c("SW", "CVM", "Lillie", "SF", "AD"), plot = c("qq","histogram","box","scatter"),  desc = TRUE){

  test<- match.arg(test)
  plot<- match.arg(plot)

  data = object$residuals

  if(test == "SW"){

    uniResult = uniNorm(data, type = "SW")

  }

  if(test == "CVM"){

    uniResult = uniNorm(data, type = "CVM")

  }

  if(test == "Lillie"){

    uniResult = uniNorm(data, type = "Lillie")

  }

  if(test == "SF"){

    uniResult = uniNorm(data, type = "SF")

  }

  if(test == "AD"){

    uniResult = uniNorm(data, type = "AD")

  }

  if(desc){

    descs = descriptives(data)
    descs

  }else{descs = NULL}

  if(!is.null(descs)){

    result = list(normalityResult = uniResult, descriptives = descs)

  }else{

    result = list(normalityResult = uniResult)


  }


  result

}

