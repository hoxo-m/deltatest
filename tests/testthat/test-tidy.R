# set up ------------------------------------------------------------------
n_user <- 100

set.seed(314)
data <- generate_dummy_data(n_user)
df <- data |>
  dplyr::group_by(user_id, group) |>
  dplyr::summarise(click = sum(metric), pageview = dplyr::n(), .groups = "drop")

dt <- deltatest(df, click / pageview, by = group, quiet = TRUE)

expected <- tibble::tibble(
  estimate = -0.046554475,
  mean_ctrl = 0.23496027,
  mean_treat = 0.188405797,
  statistic = c(Z = -1.241565940),
  p.value = 0.214396755,
  conf.low = -0.12004642,
  conf.high = 0.026937469,
  method = "Two Sample Z-test Using the Delta Method",
  alternative = "two.sided"
)

expected_relative_change <- tibble::tibble(
  estimate = 0.801862354,
  mean_ctrl = 0.23496027,
  mean_treat = 0.188405797,
  statistic = c(Z = -1.328086),
  p.value = 0.184149695,
  conf.low = 0.5094546,
  conf.high = 1.0942701,
  method = "Two Sample Z-test Using the Delta Method",
  alternative = "two.sided"
)

# tests -------------------------------------------------------------------
test_that("tidy() works", {
  act <- broom::tidy(dt)

  expect_s3_class(act, "tbl_df")
  expect_equal(act, expected)
})

test_that("glance() returns the same results as tidy()", {
  act <- broom::glance(dt)

  expect_s3_class(act, "tbl_df")
  expect_equal(act, expected)
})

test_that("augment() throws an error", {
  testthat::expect_error(
    broom::augment(dt),
    regexp = "only defined for chi squared hypothesis tests"
  )
})

test_that("tidy() works for relative change", {
  dt <- deltatest(df, click / pageview, by = group, type = "relative_change",
                  quiet = TRUE)
  act <- broom::tidy(dt)

  expect_s3_class(act, "tbl_df")
  expect_equal(act, expected_relative_change)
})
