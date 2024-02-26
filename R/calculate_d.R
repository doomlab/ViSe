#' \eqn{d_s} for Between Subjects with Pooled SD Denominator
#'
#' This function displays d for two between subjects groups
#' and gives the central and non-central confidence interval
#' using the pooled standard deviation as the denominator.
#'
#' To calculate \eqn{d_s}, mean two is subtracted from mean one and divided
#' by the pooled standard deviation.
#' \deqn{d_s = \frac{M_1 - M_2}{S_{pooled}}}
#'
#' You should provide one combination of the following:
#'
#' 1: m1 through n2
#'
#' 2: t, n1, n2
#'
#' 3: model, n1, n2
#'
#' 4: df, "x_col", "y_col"
#'
#' 5: x_col, y_col as numeric vectors
#'
#' 6: d, n1, n2
#'
#' You must provide alpha and lower to ensure the right confidence
#' interval is provided for you.
#'
#' @param m1 mean group one
#' @param m2 mean group two
#' @param sd1 standard deviation group one
#' @param sd2 standard deviation group two
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @param t optional, calculate d from independent t, you must
#' include n1 and n2 for degrees of freedom
#' @param model optional, calculate d from t.test for independent t,
#' you must still include n1 and n2
#' @param df optional dataframe that includes the x_col and y_col
#' @param x_col name of the column that contains the factor levels OR
#' a numeric vector of group 1 scores
#' @param y_col name of the column that contains the dependent score OR
#' a numeric vector of group 2 scores
#' @param d a previously calculated d value from a study
#' @param lower Use this to indicate if you want the lower or upper bound
#' of d for one sided confidence intervals. If d is positive, you generally
#' want \code{lower = TRUE}, while negative d values should enter
#' \code{lower = FALSE} for the upper bound that is closer to zero.
#'
#' @return Provides the effect size (Cohen's *d*) with associated
#' central and non-central confidence intervals,
#' the *t*-statistic, the confidence intervals associated with the means of
#' each group, as well as the standard deviations and standard errors
#' of the means for each group. The one-tailed confidence interval
#' is also included for sensitivity analyses.
#'
#' \item{d}{effect size}
#' \item{dlow}{noncentral lower level confidence interval of d value}
#' \item{dhigh}{noncentral upper level confidence interval of d value}
#' \item{dlow_central}{central lower level confidence interval of d value}
#' \item{dhigh_central}{central upper level confidence interval of d value}
#' \item{done_low}{noncentral lower bound of one tailed confidence interval}
#' \item{done_low_central}{central lower bound of
#' one tailed confidence interval}
#' \item{M1}{mean of group one}
#' \item{sd1}{standard deviation of group one mean}
#' \item{se1}{standard error of group one mean}
#' \item{M1low}{lower level confidence interval of group one mean}
#' \item{M1high}{upper level confidence interval of group one mean}
#' \item{M2}{mean of group two}
#' \item{sd2}{standard deviation of group two mean}
#' \item{se2}{standard error of group two mean}
#' \item{M2low}{lower level confidence interval of group two mean}
#' \item{M2high}{upper level confidence interval of group two mean}
#' \item{spooled}{pooled standard deviation}
#' \item{sepooled}{pooled standard error}
#' \item{n1}{sample size of group one}
#' \item{n2}{sample size of group two}
#' \item{df}{degrees of freedom (n1 - 1 + n2 - 1)}
#' \item{t}{t-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, independent t, between-subjects, pooled
#' standard deviation, pooled sd
#'
#' @examples
#' calculate_d(m1 = 14.37, # neglect mean
#'    sd1 = 10.716, # neglect sd
#'    n1 = 71, # neglect n
#'    m2 = 10.69, # none mean
#'    sd2 = 8.219, # none sd
#'    n2 = 3653, # none n
#'    a = .05, # alpha/confidence interval
#'    lower = TRUE) # lower or upper bound
#'
#' @import stats methods
#' @export

calculate_d <- function (m1 = NULL, m2 = NULL,
                         sd1 = NULL, sd2 = NULL,
                         n1 = NULL, n2 = NULL, t = NULL,
                         model = NULL, df = NULL,
                         x_col = NULL, y_col = NULL,
                         d = NULL,
                         a = .05, lower = TRUE) {

  if (a < 0 || a > 1) { stop("Alpha should be between 0 and 1.") }

  # if they include df, calculate model ----
  if (!is.null(df)){
    # deal with is.null column names
    if (is.null(x_col)){stop("Be sure to include the x column of the data.")}
    if (is.null(y_col)){stop("Be sure to include the y column of the data.")}

    # calculate model
    df <- as.data.frame(na.omit(df[ , c(x_col, y_col)]))
    if(length(table(df[ , x_col])) != 2){stop("Please have only two factor levels.")}
    groups <- unique(df[ , x_col])
    x <- df[ df[ , x_col] == groups[1] , y_col]
    y <- df[ df[ , x_col] == groups[2] , y_col]
    model_calc <- t.test(x, y, var.equal = TRUE)
    t <- unname(model_calc$statistic)
    d <- 2 * t/sqrt(model_calc$parameter)
    p <- model_calc$p.value

    # calculate means and confidence intervals
    m1 <- mean(x)
    m2 <- mean(y)
    sd1 <- sd(x)
    sd2 <- sd(y)
    n1 <- length(x)
    n2 <- length(y)
    se1 <- sd1 / sqrt(n1)
    se2 <- sd1 / sqrt(n2)
    spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
    sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
    M1low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
    M1high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
    M2low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
    M2high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
    # model will then go to other calculations

    # if they include x_col and y_col but not df ----
  } else if (!is.null(x_col) & !is.null(y_col) & is.null(df)) {

    model_calc <- t.test(x_col, y_col, var.equal = TRUE)
    t <- model_calc$statistic
    d <- 2 * t/sqrt(model_calc$parameter)
    p <- model_calc$p.value

    # calculate means and confidence intervals
    m1 <- mean(x_col)
    m2 <- mean(y_col)
    sd1 <- sd(x_col)
    sd2 <- sd(y_col)
    n1 <- length(x_col)
    n2 <- length(y_col)
    se1 <- sd1 / sqrt(n1)
    se2 <- sd1 / sqrt(n2)
    spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
    sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
    M1low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
    M1high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
    M2low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
    M2high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
    # model will then go to other calculations

    # if they include t only ----
    } else if (!is.null(t)){

      # deal with is.null values
      if (is.null(n1)){stop("Be sure to include the sample size n1 for the first group.")}
      if (is.null(n2)){stop("Be sure to include the sample size n2 for the second group.")}

      # calculate from t value here
      d <- 2 * t/sqrt(n1 + n2 - 2)
      p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2

      m1 <- m2 <- M1low <- M1high <- M2low <- M2high <-
        sd1 <- sd2 <- se1 <- se2 <- sepooled <- spooled <- NULL

      # if they include the t-test model ----
    } else if (!is.null(model)){

      # deal with is.null values
      if (is.null(n1)){stop("Be sure to include the sample size n1 for the first group.")}
      if (is.null(n2)){stop("Be sure to include the sample size n2 for the second group.")}

      # calculate from t-test model
      if(is(model, "htest")){
        t <- model$statistic
        d <- 2 * t/sqrt(model$parameter)
        p <- model$p.value

        m1 <- m2 <- M1low <- M1high <- M2low <- M2high <-
          sd1 <- sd2 <- se1 <- se2 <- sepooled <- spooled <- NULL

      } else {
        stop("Only t-tests are calculated at the moment.")
      }

      # if they include d values ----
    } else if (!is.null(d)) {

      # deal with is.null values
      if (is.null(n1)){stop("Be sure to include the sample size n1 for the first group.")}
      if (is.null(n2)){stop("Be sure to include the sample size n2 for the second group.")}

      df <- (n1 - 1 + n2 - 1)
      M1 <- sd1 <- se1 <- M1low <- M1high <- M2 <- sd2 <-
        se2 <- M2low <- M2high <- spooled <- sepooled <-
        NULL
      t <- (d/2)*sqrt(n1 + n2 - 2)
      p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2

      # if they don't have any of the above ----
    } else {
      # deal with is.null values
      if (is.null(m1)){stop("Be sure to include m1 for the first mean.")}
      if (is.null(m2)){stop("Be sure to include m2 for the second mean.")}
      if (is.null(sd1)){stop("Be sure to include sd1 for the first mean.")}
      if (is.null(sd2)){stop("Be sure to include sd2 for the second mean.")}
      if (is.null(n1)){stop("Be sure to include the sample size n1 for the first group.")}
      if (is.null(n2)){stop("Be sure to include the sample size n2 for the second group.")}

      # calculate d
      spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
      d <- (m1 - m2) / spooled

      # calculate t
      se1 <- sd1 / sqrt(n1)
      se2 <- sd2 / sqrt(n2)
      sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
      t <- (m1 - m2) / sepooled

      # calculate means and confidence intervals
      M1low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
      M1high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
      M2low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
      M2high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
      p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2

    }

  # calculate noncentral ci
  ncpboth <- noncentral_t(t, (n1 - 1 + n2 - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  dhigh <- ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2)))

  # calculate central ci
  se <- sqrt(((n1 + n2)/(n1 * n2)) + ((d^2)/(2 * (n1 + n2))))
  central_t <- qt(a/2, (n1 - 1 + n2 - 1), lower.tail = FALSE)
  dlow_central <- d - central_t*se
  dhigh_central <- d + central_t*se

  # calculate lower bound only
  if (lower == TRUE) {
    ncpboth <- noncentral_t(t, (n1 - 1 + n2 - 1), alpha.lower = a, alpha.upper = a, sup.int.warns = TRUE)
    central_t <- qt(a, (n1 - 1 + n2 - 1), lower.tail = FALSE)
    done_low <- ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2)))
    done_low_central <- d - central_t*se
  } else {
    ncpboth <- noncentral_t(t, (n1 - 1 + n2 - 1), alpha.lower = a, alpha.upper = a, sup.int.warns = TRUE)
    central_t <- qt(a, (n1 - 1 + n2 - 1), lower.tail = FALSE)
    done_low <- ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2)))
    done_low_central <- d + central_t*se
  }

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output = list("d" = d, #d stats
              "dlow" = dlow,
              "dhigh" = dhigh,
              "dlow_central" = dlow_central,
              "dhigh_central" = dhigh_central,
              "done_low" = done_low,
              "done_low_central" = done_low_central,
              "M1" = m1, #group 1 stats
              "sd1" = sd1,
              "se1" = se1,
              "M1low" = M1low,
              "M1high" = M1high,
              "M2" = m2, #group 2 stats
              "sd2" = sd2,
              "se2" = se2,
              "M2low" = M2low,
              "M2high" = M2high,
              "spooled" = spooled,
              "sepooled" = sepooled,
              "n1" = n1, #sample stats
              "n2" = n2,
              "df" = (n1 - 1 + n2 - 1),
              "t" = t, #sig stats,
              "p" = p,
              "estimate" = paste("$d_s$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                 apa(dlow,2,T), ", ", apa(dhigh,2,T), "]", sep = ""),
              "statistic" = paste("$t$(", (n1 - 1 + n2 - 1), ") = ", apa(t,2,T), ", $p$ ",
                                  reportp, sep = "")
  )

  return(output)

}

#' @rdname calculate_d
#' @export
