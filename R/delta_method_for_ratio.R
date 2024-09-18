#' @import R6
#'
#' @export
DeltaMethodForRatio <- R6::R6Class(
  "DeltaMethodForRatio",
  public = list(
    mean1 = NULL,
    mean2 = NULL,
    var1 = NULL,
    var2 = NULL,
    cov = NULL,

    initialize = function(numerator, denominator) {
      self$mean1 <- mean(numerator)
      self$mean2 <- mean(denominator)
      self$var1 <- var(numerator)
      self$var2 <- var(denominator)
      self$cov <- cov(numerator, denominator)
    },

    get_expected_value = function(order_of_Taylor = c("1", "2")) {
      order_of_Taylor <- as.character(order_of_Taylor)
      order_of_Taylor <- match.arg(order_of_Taylor)
      DeltaMethodForRatio$compute_expected_value(
        self$mean1, self$mean2, self$var2, self$cov, order_of_Taylor)
    },

    get_variance = function() {
      DeltaMethodForRatio$compute_variance(
        self$mean1, self$mean2, self$var1, self$var2, self$cov)
    }
  )
)

DeltaMethodForRatio$compute_expected_value <- function(
    mean1, mean2, var2, cov = 0, order_of_Taylor = c("1", "2")) {
  order_of_Taylor <- as.character(order_of_Taylor)
  order_of_Taylor <- match.arg(order_of_Taylor)

  if (order_of_Taylor == "1") {
    mean1 / mean2
  } else {
    mean1 / mean2 - cov / (mean2^2) + var2 * mean1 / (mean2^3)
  }
}

DeltaMethodForRatio$compute_variance <- function(mean1, mean2, var1, var2, cov = 0) {
  var1 / (mean2^2) + var2 * (mean1^2) / (mean2^4) - 2 * cov * mean1 / (mean2^3)
}
