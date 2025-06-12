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
  estimate1 = 0.23496027,
  estimate2 = 0.188405797,
  statistic = c(Z = -1.241565940),
  p.value = 0.214396755,
  conf.low = -0.12004642,
  conf.high = 0.026937469,
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
