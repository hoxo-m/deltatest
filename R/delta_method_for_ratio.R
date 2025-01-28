#' @import R6
#'
#' @export
DeltaMethodForRatio <- R6::R6Class(
  "DeltaMethodForRatio",
  public = list(
    size = NULL,
    x = NULL,
    n = NULL,
    mean1 = NULL,
    mean2 = NULL,
    var1 = NULL,
    var2 = NULL,
    cov = NULL,

    initialize = function(numerator, denominator) {
      size1 = length(numerator)
      size2 = length(denominator)
      stopifnot(size1 == size2)

      self$size <- size1
      self$x <- sum(numerator)
      self$n <- sum(denominator)
      self$mean1 <- mean(numerator)
      self$mean2 <- mean(denominator)
      self$var1 <- var(numerator)
      self$var2 <- var(denominator)
      self$cov <- cov(numerator, denominator)
    },

    get_expected_value = function(bias_correction = TRUE) {
      DeltaMethodForRatio$compute_expected_value(
        self$mean1, self$mean2, self$var2, self$cov, bias_correction)
    },

    get_variance = function() {
      DeltaMethodForRatio$compute_variance(
        self$mean1, self$mean2, self$var1, self$var2, self$cov)
    },

    get_squared_standard_error = function() {
      self$get_variance() / self$size
    },

    get_info = function(bias_correction = TRUE, alternative = "two.sided",
                        conf_level = 0.95) {
      x <- self$x
      n <- self$n
      mean <- self$get_expected_value(bias_correction)
      var <- self$get_variance()
      se <- sqrt(self$get_squared_standard_error())
      ci <- compute_confidence_interval(mean, se, alternative, conf_level)
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
