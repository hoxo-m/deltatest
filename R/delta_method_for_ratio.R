#' The Delta Method for Ratio
#'
#' @description
#' Applies the Delta method to the ratio of two random variables,
#' \eqn{f(X,Y)=X/Y}, to estimate the expected value, variance, standard error,
#' and confidence interval.
#'
#' @references
#' - id:sz_dr (2018). Calculating the mean and variance of the ratio of random
#'   variables using the Delta method \[in Japanese\]. *If you are human, think
#'   more now.* \url{https://www.szdrblog.info/entry/2018/11/18/154952}
#'
#' @import R6
#'
#' @export
DeltaMethodForRatio <- R6::R6Class(
  "DeltaMethodForRatio",

  private = list(
    size = NULL,
    x = NULL,
    n = NULL,
    expected_value = NULL,
    variance = NULL,
    squared_standard_error = NULL,
    standard_error = NULL
  ),

  public = list(
    #' @description
    #' Initialize a new DeltaMethodForRatio object.
    #'
    #' @param numerator,denominator numeric vectors sampled from the
    #'   distributions of the random variables in the numerator and denominator
    #'   of the ratio.
    #' @param bias_correction logical value indicating whether correction to the
    #'   mean of the metric is performed using the second-order term of the
    #'   Taylor expansion. The default is `FALSE`.
    initialize = function(numerator, denominator, bias_correction = FALSE) {
      size1 = length(numerator)
      size2 = length(denominator)
      stopifnot(size1 == size2)
      private$size <- size1

      private$x <- sum(numerator)
      private$n <- sum(denominator)

      mean1 <- mean(numerator)
      mean2 <- mean(denominator)
      var1 <- var(numerator)
      var2 <- var(denominator)
      cov <- cov(numerator, denominator)

      private$expected_value <- self$compute_expected_value(
        mean1, mean2, var2, cov, bias_correction)
      private$variance <- self$compute_variance(
        mean1, mean2, var1, var2, cov)
      private$squared_standard_error <- private$variance / private$size
      private$standard_error <- sqrt(private$squared_standard_error)
    },

    #' @description
    #' Get the expected value.
    #' @return numeric estimate of the expected value of the ratio.
    get_expected_value = function() {
      private$expected_value
    },

    #' @description
    #' Get the variance.
    #' @return numeric estimate of the variance of the ratio.
    get_variance = function() {
      private$variance
    },

    #' @description
    #' Get the squared standard error.
    #' @return numeric estimate of the squared standard error of the ratio.
    get_squared_standard_error = function() {
      private$squared_standard_error
    },

    #' @description
    #' Get the standard error.
    #' @return numeric estimate of the standard error of the ratio.
    get_standard_error = function() {
      private$standard_error
    },

    #' @description
    #' Get the confidence interval.
    #'
    #' @param alternative character string specifying the alternative
    #'   hypothesis, must be one of `"two.sided"` (default), `"greater"`, or
    #'   `"less"`. You can specify just the initial letter.
    #' @param conf_level numeric value specifying the confidence level of the
    #'   interval. The default is 0.95.
    #'
    #' @return numeric estimates of the lower and upper bounds of the confidence
    #'   interval of the ratio.
    get_confidence_interval = function(alternative = c("two.sided", "less", "greater"),
                                       conf_level = 0.95) {
      alternative <- match.arg(alternative)

      expected_value <- self$get_expected_value()
      standard_error <- self$get_standard_error()
      self$compute_confidence_interval(
        expected_value, standard_error, alternative, conf_level)
    },

    #' @description
    #' Get statistical information.
    #'
    #' @param alternative character string specifying the alternative
    #'   hypothesis, must be one of `"two.sided"` (default), `"greater"`, or
    #'   `"less"`. You can specify just the initial letter.
    #' @param conf_level numeric value specifying the confidence level of the
    #'   interval. The default is 0.95.
    #'
    #' @return numeric estimates include the expected value, variance, standard
    #'   error, and confidence interval.
    get_info = function(alternative = c("two.sided", "less", "greater"),
                        conf_level = 0.95) {
      alternative <- match.arg(alternative)

      size <- private$size
      x <- private$x
      n <- private$n
      mean <- self$get_expected_value()
      var <- self$get_variance()
      se <- self$get_standard_error()
      ci <- self$get_confidence_interval(alternative, conf_level)
      lower <- ci[1]
      upper <- ci[2]
      data.frame(size, x, n, mean, var, se, lower, upper)
    },

    # class methods -----------------------------------------------------------

    #' @description
    #' Class method to compute the expected value of the ratio using the Delta
    #' method.
    #'
    #' @param mean1 numeric value of the mean numerator of the ratio.
    #' @param mean2 numeric value of the mean denominator of the ratio.
    #' @param var2 numeric value of the variance of the denominator of the ratio.
    #' @param cov numeric value of the covariance between the numerator and
    #'   denominator of the ratio. The default is 0.
    #' @param bias_correction logical value indicating whether correction to the
    #'   mean of the metric is performed using the second-order term of the
    #'   Taylor expansion. The default is `FALSE`.
    #'
    #' @return numeric estimate of the expected value of the ratio.
    compute_expected_value = function(mean1, mean2, var2, cov = 0,
                                      bias_correction = FALSE) {
      expected_value <- mean1 / mean2
      if (bias_correction) {
        bias <- var2 * mean1 / (mean2^3) - cov / (mean2^2)
        expected_value <- expected_value + bias
      }
      expected_value
    },

    #' @description
    #' Class method to compute the variance of the ratio using the Delta method.
    #'
    #' @param mean1 numeric value of the mean numerator of the ratio.
    #' @param mean2 numeric value of the mean denominator of the ratio.
    #' @param var1 numeric value of the variance of the numerator of the ratio.
    #' @param var2 numeric value of the variance of the denominator of the ratio.
    #' @param cov numeric value of the covariance between the numerator and
    #'   denominator of the ratio. The default is 0.
    #'
    #' @return numeric estimate of the variance of the ratio
    compute_variance = function(mean1, mean2, var1, var2, cov = 0) {
      var1 / (mean2^2) + var2 * (mean1^2) / (mean2^4) - 2 * cov * mean1 / (mean2^3)
    },

    #' @description
    #' Class method to compute the confidence interval of the ratio using the
    #' Delta method.
    #'
    #' @param mean numeric value of the estimated mean of the ratio.
    #' @param standard_error numeric value of the estimated standard error of
    #'   the mean of the ratio.
    #' @param alternative character string specifying the alternative
    #'   hypothesis, must be one of `"two.sided"` (default), `"greater"`, or
    #'   `"less"`. You can specify just the initial letter.
    #' @param conf_level numeric value specifying the confidence level of the
    #'   interval. The default is 0.95.
    #'
    #' @return numeric estimates of the lower and upper bounds of the confidence
    #'   interval of the ratio.
    #'
    #' @importFrom stats qnorm
    compute_confidence_interval = function(mean, standard_error,
                                           alternative = c("two.sided", "less", "greater"),
                                           conf_level = 0.95) {
      alternative <- match.arg(alternative)

      if (alternative == "two.sided") {
        p <- 1 - (1 - conf_level) / 2
        lower <- mean - qnorm(p) * standard_error
        upper <- mean + qnorm(p) * standard_error
      } else if (alternative == "less") {
        p <- conf_level
        lower <- -Inf
        upper <- mean + qnorm(p) * standard_error
      } else {
        p <- conf_level
        lower <- mean - qnorm(p) * standard_error
        upper <- Inf
      }
      confidence_interval <- c(lower, upper)
      attr(confidence_interval, "conf.level") <- conf_level
      confidence_interval
    }
  )
)
