#' @importFrom broom tidy
#' @method tidy deltatest
#' @export
tidy.deltatest <- function(x, ...) {
  method <- x$method
  x$method <- "Welch Two Sample t-test"
  x$estimate <- x$estimate[1:2]
  tidy <- get("tidy.htest", asNamespace("broom"))
  result <- tidy(x, ...)
  result$method <- method
  result$estimate <- -result$estimate
  result
}
