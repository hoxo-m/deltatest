#' Generate Dummy Data
#'
#' @importFrom stats rpois
#'
#' @export
generate_dummy_data <- function(
    n_user, model = c("Bernoulli", "normal"), sigma = 0, xi = 0,
    random_unit = c("user", "session", "page"), treatment_ratio = 0.5) {

  # check arguments
  model <- match.arg(model)
  random_unit <- match.arg(random_unit)

  n_session <- rpois(n_user, 3) + 1L
  n_pageview <- n_session |> lapply(function(x) rpois(x, 3) + 1L) |> unlist()

  user_id <- seq_len(n_user) |> rep(n_session) |> rep(n_pageview)
  group <- generate_group(random_unit, n_user, n_session, n_pageview, treatment_ratio)
  metric <- generate_metric(model, n_user, n_session, n_pageview, sigma, xi, group)

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

generate_metric <- function(model, n_user, n_session, n_pageview, sigma, xi, group) {
  if (model == "normal") {
    generate_metric_normal(n_user, n_session, n_pageview, sigma, group)
  } else {
    generate_metric_Bernoulli(n_user, n_session, n_pageview, xi, group)
  }
}

#' @importFrom stats rnorm
generate_metric_normal <- function(n_user, n_session, n_pageview, sigma, group) {
  mu <- rnorm(n_user) |> rep(n_session) |> rep(n_pageview)
  tau <- rnorm(n_user, sd = sigma) |> rep(n_session) |> rep(n_pageview)
  rnorm(sum(n_pageview), mean = mu + ifelse(group == 1L, tau, 0))
}

#' @importFrom stats runif rbinom
generate_metric_Bernoulli <- function(n_user, n_session, n_pageview, xi, group) {
  mu <- runif(n_user, max = 0.5) |> rep(n_session) |> rep(n_pageview)
  tau <- runif(n_user, max = xi) |> rep(n_session) |> rep(n_pageview)
  rbinom(sum(n_pageview), size = 1L, prob = mu + ifelse(group == 1L, tau, 0))
}
