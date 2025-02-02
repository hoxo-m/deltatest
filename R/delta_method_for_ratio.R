#' @import R6
NULL

#' The Delta Method for Ratio
#'
#' @export
DeltaMethodForRatio <- R6::R6Class(
  "DeltaMethodForRatio",

  private = list(
    x = NULL,
    n = NULL,
    expected_value = NULL,
    variance = NULL,
    squared_standard_error = NULL,
    standard_error = NULL
  ),

  public = list(
    initialize = function(numerator, denominator, bias_correction = TRUE) {
      size1 = length(numerator)
      size2 = length(denominator)
      stopifnot(size1 == size2)
      size <- size1

      private$x <- sum(numerator)
      private$n <- sum(denominator)

      mean1 <- mean(numerator)
      mean2 <- mean(denominator)
      var1 <- var(numerator)
      var2 <- var(denominator)
      cov <- cov(numerator, denominator)

      private$expected_value <- DeltaMethodForRatio$compute_expected_value(
        mean1, mean2, var2, cov, bias_correction)
      private$variance <- DeltaMethodForRatio$compute_variance(
        mean1, mean2, var1, var2, cov)
      private$squared_standard_error <- private$variance / size
      private$standard_error <- sqrt(private$squared_standard_error)
    },

    get_expected_value = function() {
      private$expected_value
    },

    get_variance = function() {
      private$variance
    },

    get_squared_standard_error = function() {
      private$squared_standard_error
    },

    get_standard_error = function() {
      private$standard_error
    },

    get_confidence_interval = function(alternative = "two.sided", conf_level = 0.95) {
      expected_value <- self$get_expected_value()
      standard_error <- self$get_standard_error()
      DeltaMethodForRatio$compute_confidence_interval(
        expected_value, standard_error, alternative, conf_level)
    },

    get_info = function(alternative = "two.sided", conf_level = 0.95) {
      x <- private$x
      n <- private$n
      mean <- self$get_expected_value()
      var <- self$get_variance()
      se <- self$get_standard_error()
      ci <- self$get_confidence_interval(alternative, conf_level)
      lower <- ci[1]
      upper <- ci[2]
      data.frame(x, n, mean, var, se, lower, upper)
    }
  )
)

DeltaMethodForRatio$compute_expected_value <- function(
    mean1, mean2, var2, cov = 0, bias_correction = TRUE) {

  expected_value <- mean1 / mean2
  if (bias_correction) {
    expected_value <- expected_value - cov / (mean2^2) + var2 * mean1 / (mean2^3)
  }
  expected_value
}

DeltaMethodForRatio$compute_variance <- function(mean1, mean2, var1, var2, cov = 0) {
  var1 / (mean2^2) + var2 * (mean1^2) / (mean2^4) - 2 * cov * mean1 / (mean2^3)
}

#' @importFrom stats qnorm
DeltaMethodForRatio$compute_confidence_interval <-  function(
    estimate, standard_error, alternative, conf_level) {

  if (alternative == "two.sided") {
    p <- 1 - (1 - conf_level) / 2
    lower <- estimate - qnorm(p) * standard_error
    upper <- estimate + qnorm(p) * standard_error
  } else if (alternative == "less") {
    p <- conf_level
    lower <- -Inf
    upper <- estimate + qnorm(p) * standard_error
  } else {
    p <- conf_level
    lower <- estimate - qnorm(p) * standard_error
    upper <- Inf
  }
  confidence_interval <- c(lower, upper)
  attr(confidence_interval, "conf.level") <- conf_level
  confidence_interval
}
