#' Calculate the noncentral t confidence interval for d-values
#' Taken from MBESS by Ken Kelley
#' Exported here to avoid importing a zillion package dependencies
#'
#' @param ncp the noncentrality parameter (e.g., observed *t*-value)
#' of interest.
#' @param df the degrees of freedom.
#' @param conf.level the level of confidence for a symmetric confidence
#' interval.
#' @param alpha.lower the proportion of values beyond the lower limit of
#' the confidence interval (cannot be used with \code{conf.level}).
#' @param alpha.upper the proportion of values beyond the upper limit of
#' the confidence interval (cannot be used with \code{conf.level}).
#' @param t.value alias for \code{ncp}
#' @param tol is the tolerance of the iterative method for determining
#' the critical values.
#' @param sup.int.warns Suppress internal warnings (from internal functions):
#' \code{TRUE} or \code{FALSE}
#' @param \dots allows one to potentially include parameter
#' values for inner functions
#' @return Returns the non-central t values for use in calculating
#' the non-central d confidence interval
#' @noRd

noncentral_t <- function(ncp, df, conf.level = .95, alpha.lower = NULL,
                            alpha.upper = NULL, t.value, tol = 1e-9,
                            sup.int.warns = TRUE, ...)
{
  # Note this function is new in version 4, replacing what was used in prior versions.
  # Internal functions for the noncentral t distribution; two appraoches.
  ###########

  if(missing(ncp))
  {
    if(missing(t.value)) stop("You need to specify either 'ncp' or
                              its alias, 't.value,' you have not
                              specified either")
    ncp <- t.value
  }

  # General stop checks.
  if(df <= 0) stop("The degrees of freedom must be some positive
                   value.", call.=FALSE)

  if(abs(ncp) > 37.62) warning("The observed noncentrality parameter
                               of the noncentral t-distribution has
                               exceeded 37.62 in magnitude (R's limitation
                               for accurate probabilities from the noncentral
                               t-distribution) in the function's iterative
                               search for the appropriate value(s). The
                               results may be fine, but they might be
                               inaccurate; use caution.")

  if(sup.int.warns==TRUE) Orig.warn <- options()$warn

  if(!is.null(conf.level) & is.null(alpha.lower) & !is.null(alpha.upper)) stop("You must choose either to use \'conf.level\' or define the \'lower.alpha\' and \'upper.alpha\' values; here, \'upper.alpha\' is specified but \'lower.alpha\' is not", call.=FALSE)
  if(!is.null(conf.level) & !is.null(alpha.lower) & is.null(alpha.upper)) stop("You must choose either to use \'conf.level\' or define the \'lower.alpha\' and \'upper.alpha\' values; here, \'lower.alpha\' is specified but \'upper.alpha\' is not", call.=FALSE)

  if(!is.null(conf.level) & is.null(alpha.lower) & is.null(alpha.upper))
  {
    alpha.lower <- (1-conf.level)/2
    alpha.upper <- (1-conf.level)/2
  }


  .conf.limits.nct.M1 <- function(ncp, df, conf.level=NULL, alpha.lower, alpha.upper, tol=1e-9, sup.int.warns=TRUE, ...)
  {

    if(sup.int.warns==TRUE) Orig.warn <- options()$warn

    min.ncp=min(-150, -5*ncp)
    max.ncp=max(150, 5*ncp)

    # Internal function for upper limit.
    # Note the upper tail is used here, as we seek to find the NCP that has, in its upper tail (alpha.lower, for the lower limit), the specified value of the observed t/ncp.
    ###########################
    .ci.nct.lower <- function(val.of.interest, ...)
    {
      (qt(p=alpha.lower, df=df, ncp=val.of.interest, lower.tail = FALSE, log.p = FALSE) - ncp)^2
    }
    ###########################

    # Internal function for lower limit.
    # Note the lower tail is used here, as we seek to find the NCP that has, in its lower tail (alpha.upper, for the upper limit), the specified value of the observed t/ncp.
    ###########################
    .ci.nct.upper <- function(val.of.interest, ...)
    {
      (qt(p=alpha.upper, df=df, ncp=val.of.interest, lower.tail = TRUE, log.p = FALSE) - ncp)^2
    }

    if(alpha.lower!=0)
    {
      if(sup.int.warns==TRUE) Low.Lim <- suppressWarnings(optimize(f=.ci.nct.lower, interval=c(min.ncp, max.ncp), alpha.lower=alpha.lower, df=df, ncp=ncp, maximize=FALSE, tol=tol))
      if(sup.int.warns==FALSE) Low.Lim <- optimize(f=.ci.nct.lower, interval=c(min.ncp, max.ncp), alpha.lower=alpha.lower, df=df, ncp=ncp, maximize=FALSE, tol=tol)
    }

    if(alpha.upper!=0)
    {
      if(sup.int.warns==TRUE) Up.Lim <- suppressWarnings(optimize(f=.ci.nct.upper, interval=c(min.ncp, max.ncp), alpha.upper=alpha.upper, df=df, ncp=ncp, maximize=FALSE, tol=tol))
      if(sup.int.warns==FALSE) Up.Lim <- optimize(f=.ci.nct.upper, interval=c(min.ncp, max.ncp), alpha.upper=alpha.upper, df=df, ncp=ncp, maximize=FALSE, tol=tol)
    }

    if(alpha.lower==0) Result <- list(Lower.Limit=-Inf, Prob.Less.Lower=0, Upper.Limit=Up.Lim$minimum, Prob.Greater.Upper=pt(q=ncp, ncp=Up.Lim$minimum, df=df))
    if(alpha.upper==0) Result <- list(Lower.Limit=Low.Lim$minimum, Prob.Less.Lower=pt(q=ncp, ncp=Low.Lim$minimum, df=df, lower.tail=FALSE), Upper.Limit=Inf, Prob.Greater.Upper=0)
    if(alpha.lower!=0 & alpha.upper!=0) Result <- list(Lower.Limit=Low.Lim$minimum, Prob.Less.Lower=pt(q=ncp, ncp=Low.Lim$minimum, df=df, lower.tail=FALSE), Upper.Limit=Up.Lim$minimum, Prob.Greater.Upper=pt(q=ncp, ncp=Up.Lim$minimum, df=df))

    if(sup.int.warns==TRUE) options(warn=Orig.warn)

    return(Result)
  }
  ################################################
  .conf.limits.nct.M2 <- function(ncp, df, conf.level=NULL, alpha.lower, alpha.upper, tol=1e-9, sup.int.warns=TRUE, ...)
  {

    # Internal function for upper limit.
    ###########################
    .ci.nct.lower <- function(val.of.interest, ...)
    {
      (qt(p=alpha.lower, df=df, ncp=val.of.interest, lower.tail = FALSE, log.p = FALSE) - ncp)^2
    }
    ###########################

    # Internal function for lower limit.
    ###########################
    .ci.nct.upper <- function(val.of.interest, ...)
    {
      (qt(p=alpha.upper, df=df, ncp=val.of.interest, lower.tail = TRUE, log.p = FALSE) - ncp)^2
    }

    if(sup.int.warns==TRUE)
    {
      Low.Lim <- suppressWarnings(nlm(f=.ci.nct.lower, p=ncp, ...))
      Up.Lim <- suppressWarnings(nlm(f=.ci.nct.upper, p=ncp, ...))
    }

    if(sup.int.warns==FALSE)
    {
      Low.Lim <- nlm(f=.ci.nct.lower, p=ncp, ...)
      Up.Lim <- nlm(f=.ci.nct.upper, p=ncp, ...)
    }

    if(alpha.lower==0) Result <- list(Lower.Limit=-Inf, Prob.Less.Lower=0, Upper.Limit=Up.Lim$estimate, Prob.Greater.Upper=pt(q=ncp, ncp=Up.Lim$estimate, df=df))
    if(alpha.upper==0) Result <- list(Lower.Limit=Low.Lim$estimate, Prob.Less.Lower=pt(q=ncp, ncp=Low.Lim$estimate, df=df, lower.tail=FALSE), Upper.Limit=Inf, Prob.Greater.Upper=0)
    if(alpha.lower!=0 & alpha.upper!=0) Result <- list(Lower.Limit=Low.Lim$estimate, Prob.Less.Lower=pt(q=ncp, ncp=Low.Lim$estimate, df=df, lower.tail=FALSE), Upper.Limit=Up.Lim$estimate, Prob.Greater.Upper=pt(q=ncp, ncp=Up.Lim$estimate, df=df))

    return(Result)
  }

  # Now, use the each of the two methods.
  Res.M1 <- Res.M2 <- NULL
  try(Res.M1 <- .conf.limits.nct.M1(ncp=ncp, df=df, conf.level=NULL, alpha.lower=alpha.lower, alpha.upper=alpha.upper, tol=tol, sup.int.warns=sup.int.warns), silent=TRUE)
  if(length(Res.M1)!=4) Res.M1 <- NULL

  try(Res.M2 <- .conf.limits.nct.M2(ncp=ncp, df=df, conf.level=NULL, alpha.lower=alpha.lower, alpha.upper=alpha.upper, tol=tol, sup.int.warns=sup.int.warns), silent=TRUE)
  if(length(Res.M2)!=4) Res.M2 <- NULL

  # Now, set-up the test to find the best method.
  Low.M1 <- Res.M1$Lower.Limit
  Prob.Low.M1 <- Res.M1$Prob.Less.Lower
  Upper.M1 <- Res.M1$Upper.Limit
  Prob.Upper.M1 <- Res.M1$Prob.Greater.Upper

  Low.M2 <- Res.M2$Lower.Limit
  Prob.Low.M2 <- Res.M2$Prob.Less.Lower
  Upper.M2 <- Res.M2$Upper.Limit
  Prob.Upper.M2 <- Res.M2$Prob.Greater.Upper

  # Choose the best interval limits:
  ##Here low
  Min.for.Best.Low <- min((c(Prob.Low.M1, Prob.Low.M2)-alpha.lower)^2)

  if(!is.null(Res.M1)){if(Min.for.Best.Low==(Prob.Low.M1-alpha.lower)^2) Best.Low <- 1}
  if(!is.null(Res.M2)){if(Min.for.Best.Low==(Prob.Low.M2-alpha.lower)^2) Best.Low <- 2}

  ##Here high
  Min.for.Best.Up <- min((c(Prob.Upper.M1, Prob.Upper.M2)-alpha.upper)^2)

  if(!is.null(Res.M1)){if(Min.for.Best.Up==(Prob.Upper.M1-alpha.upper)^2) Best.Up <- 1}
  if(!is.null(Res.M2)){if(Min.for.Best.Up==(Prob.Upper.M2-alpha.upper)^2) Best.Up <- 2}
  #####################################

  if(is.null(Res.M1)) {Low.M1 <- NA; Prob.Low.M1 <- NA; Upper.M1 <- NA; Prob.Upper.M1 <- NA}
  if(is.null(Res.M2)) {Low.M2 <- NA; Prob.Low.M2 <- NA; Upper.M2 <- NA; Prob.Upper.M2 <- NA}

  Result <- list(Lower.Limit=c(Low.M1, Low.M2)[Best.Low], Prob.Less.Lower=c(Prob.Low.M1, Prob.Low.M2)[Best.Low], Upper.Limit=c(Upper.M1, Upper.M2)[Best.Up], Prob.Greater.Upper=c(Prob.Upper.M1, Prob.Upper.M2)[Best.Up])

  return(Result)
}
