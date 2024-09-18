#' @importFrom stats rbeta rpois rbinom
#'
#' @export
generate_dummy_data <- function(n_user, shape1 = 0.2, shape2 = 5, lambda = 0.5) {
  p <- rbeta(n_user, shape1 = shape1, shape2 = shape2)
  imp <- rpois(n_user, lambda = lambda) + 1L
  click <- rbinom(n_user, imp, p)
  data.frame(click, imp)
}
