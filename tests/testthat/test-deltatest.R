
n_user <- 100

set.seed(314)
data <- generate_dummy_data(n_user)
df <- data |>
  dplyr::group_by(user_id, group) |>
  dplyr::summarise(click = sum(metric), pageview = dplyr::n(), .groups = "drop")

df_posneg <- data |>
  dplyr::group_by(user_id, group) |>
  dplyr::summarise(pos = sum(metric), neg = dplyr::n() - pos, .groups = "drop")

expected_conf.int <- c(-0.12688957, 0.02009432)
attr(expected_conf.int, "conf.level") <- 0.95

expected_result <- structure(list(
  statistic = c(z = -1.4240665), p.value = 0.15442723, conf.int = expected_conf.int,
  estimate = c("mean in control" = 0.23902344, "mean in treatment" = 0.18562581, "difference" = -0.05339762),
  null.value = c("difference in means between control and treatment" = 0),
  stderr = 0.03749658, alternative = "two.sided",
  method = "Two Sample Z-test Using the Delta Method",
  data.name = "click/pageview by group"
), class = c("deltatest", "htest"))

test_that("deltatest works", {
  expected_conf.int <- c(-0.12688957, 0.02009432)
  attr(expected_conf.int, "conf.level") <- 0.95

  act <- deltatest(df, click / pageview ~ group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result, tolerance = 1e-7)
})

test_that("standard formula works", {
  act <- deltatest(df, click / pageview ~ group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result, tolerance = 1e-7)
})

test_that("standard formula variable works", {
  formula <- click / pageview ~ group
  act <- deltatest(df, formula, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result, tolerance = 1e-7)
})

test_that("lambda formula works", {
  act <- deltatest(df, ~ click / pageview, by = group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result, tolerance = 1e-7)
})

test_that("lambda formula variable works", {
  formula <- ~ click / pageview
  act <- deltatest(df, formula, by = group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result, tolerance = 1e-7)
})

test_that("NSE works", {
  act <- deltatest(df, click / pageview, by = group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result, tolerance = 1e-7)
})

test_that("quote works", {
  metric <- quote(click / pageview)
  act <- deltatest(df, metric, by = group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result, tolerance = 1e-7)
})

test_that("formula is missing", {
  expect_error(deltatest(df, by = group),
               "The 'formula' argument is required but missing.")
})

test_that("formula is incorrect; formula", {
  expect_error(deltatest(df, click * pageview ~ group),
               "The 'formula' argument is incorrect.")
})

test_that("formula is incorrect; NSE", {
  # TODO
  expect_error(deltatest(df, click * pageview, by = group), "click")
})

test_that("formula is incorrect; absent cloumn name", {
  expect_error(deltatest(df, x / y ~ group),
               "The 'formula' argument is incorrect. Column 'x', 'y'")
})

test_that("formula contains calculation", {
  expected_result$data.name <- "pos/(pos + neg) by group"
  act <- deltatest(df_posneg, pos / (pos + neg) ~ group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result, tolerance = 1e-7)
})
