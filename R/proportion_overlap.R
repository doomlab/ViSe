#' Proportion Overlap Calculations for Cohen's d
#'
#' This function calculates the proportion overlap from two independent
#' group d effect size calculations. Cohen's u1, u2, u3 and proportion
#' overlap are provided.
#'
#' @param model a saved independent t-test model
#' @param df optional dataframe that includes the x_col and y_col
#' @param x_col name of the column that contains the factor levels OR
#' a numeric vector of group 1 scores
#' @param y_col name of the column that contains the dependent score OR
#' a numeric vector of group 2 scores
#' @param d previously calculated d value
#' @return A list of the following:
#'
#' \item{u1}{Proportion of non-overlap across both distributions}
#' \item{u2}{Proportion that one group is more than the same
#' proportion in the other group}
#' \item{u3}{Proportion of one group that is smaller than the
#' median of the other group}
#' \item{p_o}{Proportional overlap of distributions}
#'
#' @keywords effect size, proportion overlap, u1, u2, u3
#' @import stats methods
#'
#' @export
#' @examples
#'
proportion_overlap <- function(model = NULL, x_col = NULL,
                            y_col = NULL, df = NULL, d = NULL){

  # models
  if(!is.null(model)){

    # t.test
    if(is(model, "htest")){
      d_use <- (2 * model$statistic)/sqrt(model$parameter)
    } else {
        stop("Only t-test models are allowed at the moment.")
    }

    # based on dataframes
  } else if(!is.null(df) & !is.null(x_col) & !is.null(y_col)){
    d_use <- calculate_d(df = df, x_col = x_col, y_col = y_col)$d

    # based on x column and y column
  } else if(!is.null(x_col) & !is.null(y_col)){

    # calculate based on calculate_d
    d_use <- calculate_d(x_col = x_col, y_col = y_col)$d

    # calculate just on d
  } else if(!is.null(d)){
    d_use <- d
  } else {
    stop("You must include x and y, OR the df with x and y columns, OR the model, OR d.")
  }

  u2 <- stats::pnorm(abs(d_use) / 2)
  u1 <- (2 * u2 - 1) / u2
  u3 <- stats::pnorm(d_use)
  p_o <- 2 * stats::pnorm(-abs(d_use) / 2)

  return(list("u1" = u1,
              "u2" = u2,
              "u3" = u3,
              "p_o" = p_o))

}

#' @rdname proportion_overlap
#' @export
