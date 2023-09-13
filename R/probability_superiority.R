#' Probability of Superiority Calculation
#'
#' This function calculates the probability of superiority from
#' independent samples Cohen's d calculation.
#'
#' You should provide one combination of the following:
#'
#' 1: d
#'
#' 2: m1 through n2
#'
#' 3: t, n1, n2
#'
#' 4: model, n1, n2
#'
#' 5: df, "x_col", "y_col"
#'
#' 6: x_col, y_col as numeric vectors
#'
#' @param d the effect size
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
#'
#' @return The probability of superiority.
#'
#' @keywords effect size, probability of superiority
#'
#' @import stats
#' @export
#' @examples
#'
#'
probability_superiority <- function(d = NULL, m1 = NULL, m2 = NULL,
                                    sd1 = NULL, sd2 = NULL,
                                    n1 = NULL, n2 = NULL, a = .05,
                                    t = NULL,
                                    model = NULL, df = NULL,
                                    x_col = NULL, y_col = NULL){

  if(!is.null(d)){
    d <- d

    # if they include df, calculate model
  } else if(!is.null(df)){
    # deal with missing column names
    if (missing(x_col)){stop("Be sure to include the x column of the data.")}
    if (missing(y_col)){stop("Be sure to include the y column of the data.")}
    d <- calculate_d(df = df, x_col = x_col, y_col = y_col)$d

    # just x and y columns
  } else if(!is.null(x_col) & !is.null(y_col)){
    d <- calculate_d(x_col = x_col, y_col = y_col)
  } else if (!is.null(t)){

    # deal with missing values
    if (missing(n1)){stop("Be sure to include the sample size n1 for the first group.")}
    if (missing(n2)){stop("Be sure to include the sample size n2 for the second group.")}
    d <- calculate_d(t = t, n1 = n1, n2 = n2)$d

    # or just the model
  } else if (!is.null(model)){

    # deal with missing values
    if (missing(n1)){stop("Be sure to include the sample size n1 for the first group.")}
    if (missing(n2)){stop("Be sure to include the sample size n2 for the second group.")}
    d <- calculate_d(model = model)$d

    # or if they want ot give all numbers
  } else {

    # deal with missing values
    if (missing(m1)){stop("Be sure to include m1 for the first mean.")}
    if (missing(m2)){stop("Be sure to include m2 for the second mean.")}
    if (missing(sd1)){stop("Be sure to include sd1 for the first mean.")}
    if (missing(sd2)){stop("Be sure to include sd2 for the second mean.")}
    if (missing(n1)){stop("Be sure to include the sample size n1 for the first group.")}
    if (missing(n2)){stop("Be sure to include the sample size n2 for the second group.")}

    d <- calculate_d(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2,
                     n1 = n1, n2 = n2)$d
  }

  return(stats::pnorm(d / sqrt(2)))
}

#' @rdname p_superiority
#' @export



