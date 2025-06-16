#' @importFrom broom tidy
#' @method tidy deltatest
#' @export
tidy.deltatest <- function(x, ...) {
  tibble::tibble(
    estimate = unname(x$estimate[3]),
    est_ctrl = unname(x$estimate[1]),
    est_treat = unname(x$estimate[2]),
    statistic = x$statistic,
    p.value = x$p.value,
    conf.low = x$conf.int[1],
    conf.high = x$conf.int[2],
    method = x$method,
    alternative = x$alternative
  )
}
