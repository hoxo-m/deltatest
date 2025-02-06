#' Generate Dummy Data
#'
#'
#'
#' @param n_user integer value of the number of users.
#' @param model character string specifying the model that generates the
#'   potential outcomes. It must be one of `"Bernoulli"` (default) or
#'   `"normal"`. You can specify just the initial letter.
#' @param xi numeric value specifying the treatment effect variation (TEV) under
#'   the Bernoulli model, where \eqn{TEV = 2\xi}. This argument is ignored if
#'   the `model` argument is set to `"normal"`.
#' @param sigma numeric value specifying the treatment effect variation (TEV)
#'   under the normal model, where \eqn{TEV = \sigma}. This argument is ignored
#'   if the `model` argument is set to `"Bernoulli"`.
#' @param random_unit character string specifying the randomization unit. It
#'   must be one of `"user"` (default), `"session"`, or `"pageview"`. You can
#'   specify just the initial letter.
#' @param treatment_ratio numeric value specifying the ratio assigned to
#'   treatment. The default value is 0.5.
#'
#' @references
#' - Deng, A., Lu, J., & Litz, J. (2017). Trustworthy Analysis of Online A/B
#'   Tests: Pitfalls, challenges and solutions. *Proceedings of the Tenth ACM
#'   International Conference on Web Search and Data Mining.*
#'
#' @importFrom stats rpois
#'
#' @export
generate_dummy_data <- function(
    n_user, model = c("Bernoulli", "normal"), xi = 0, sigma = 0,
    random_unit = c("user", "session", "pageview"), treatment_ratio = 0.5) {

  # check arguments
  model <- match.arg(model)
  random_unit <- match.arg(random_unit)

  # generate data
  n_session <- rpois(n_user, 3) + 1L
  n_pageview <- n_session |> lapply(function(x) rpois(x, 3) + 1L) |> unlist()

  user_id <- seq_len(n_user) |> rep(n_session) |> rep(n_pageview)
  group <- generate_group(random_unit, n_user, n_session, n_pageview, treatment_ratio)
  metric <- generate_metric(model, n_user, n_session, n_pageview, xi, sigma, group)

  data.frame(user_id, group, metric)
}

#' @importFrom stats rbinom
generate_group <- function(random_unit, n_user, n_session, n_pageview, treatment_ratio) {
  if (random_unit == "user") {
    rbinom(n_user, size = 1L, prob = treatment_ratio) |> rep(n_session) |> rep(n_pageview)
  } else if(random_unit == "session") {
    rbinom(sum(n_session), size = 1L, prob = treatment_ratio) |> rep(n_pageview)
  } else {
    rbinom(sum(n_pageview), size = 1L, prob = treatment_ratio)
  }
}

generate_metric <- function(model, n_user, n_session, n_pageview, xi, sigma, group) {
  if (model == "Bernoulli") {
    generate_metric_Bernoulli(n_user, n_session, n_pageview, xi, group)
  } else {
    generate_metric_normal(n_user, n_session, n_pageview, sigma, group)
  }
}

#' @importFrom stats runif rbinom
generate_metric_Bernoulli <- function(n_user, n_session, n_pageview, xi, group) {
  mu <- runif(n_user, max = 0.5) |> rep(n_session) |> rep(n_pageview)
  tau <- runif(n_user, max = xi) |> rep(n_session) |> rep(n_pageview)
  rbinom(sum(n_pageview), size = 1L, prob = mu + ifelse(group == 1L, tau, 0))
}

#' @importFrom stats rnorm
generate_metric_normal <- function(n_user, n_session, n_pageview, sigma, group) {
  mu <- rnorm(n_user) |> rep(n_session) |> rep(n_pageview)
  tau <- rnorm(n_user, sd = sigma) |> rep(n_session) |> rep(n_pageview)
  rnorm(sum(n_pageview), mean = mu + ifelse(group == 1L, tau, 0))
}
