#' @importFrom stats rbeta rpois rbinom
#' @importFrom tibble tibble
#'
#' @export
generate_dummy_data <- function(n_user, shape1 = 0.2, shape2 = 5, lambda = 0.5) {
  p <- rbeta(n_user, shape1 = shape1, shape2 = shape2)
  views <- rpois(n_user, lambda = lambda) + 1L
  clicks <- rbinom(n_user, views, p)
  tibble::tibble(click = clicks, view = views)
}
