# set up ------------------------------------------------------------------
n_user <- 100

set.seed(314)
data <- generate_dummy_data(n_user)
df <- data |>
  dplyr::group_by(user_id, group) |>
  dplyr::summarise(click = sum(metric), pageview = dplyr::n(), .groups = "drop")

df_posneg <- data |>
  dplyr::group_by(user_id, group) |>
  dplyr::summarise(pos = sum(metric), neg = dplyr::n() - pos, .groups = "drop")

expected_result <- structure(list(
  statistic = c(Z = -1.24156594), p.value = 0.214396755,
  conf.int = structure(c(-0.12004642, 0.02693747), conf.level = 0.95),
  estimate = c("mean in control" = 0.2349602724, "mean in treatment" = 0.1884057971, "difference" = -0.0465544753),
  null.value = c("difference in means between control and treatment" = 0),
  stderr = 0.037496579, alternative = "two.sided",
  method = "Two Sample Z-test Using the Delta Method",
  data.name = "click/pageview by group"
), class = c("deltatest", "htest"))


# formula -----------------------------------------------------------------
test_that("standard formula works", {
  act <- deltatest(df, click / pageview ~ group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("standard formula variable works", {
  formula <- click / pageview ~ group
  act <- deltatest(df, formula, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("lambda formula works", {
  act <- deltatest(df, ~ click / pageview, by = group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("lambda formula variable works", {
  formula <- ~ click / pageview
  act <- deltatest(df, formula, by = group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("NSE works", {
  act <- deltatest(df, click / pageview, by = group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("quote works", {
  metric <- quote(click / pageview)
  act <- deltatest(df, metric, by = group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
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
               "The 'formula' or 'by' argument is incorrect. .+ 'x', 'y'")
})

test_that("formula contains calculation", {
  expected_result$data.name <- "pos/(pos + neg) by group"
  act <- deltatest(df_posneg, pos / (pos + neg) ~ group, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})


# by ----------------------------------------------------------------------
df_bucket <- df |> dplyr::rename(bucket = group)
expected_result_bucket <- expected_result
expected_result_bucket$data.name <- "click/pageview by bucket"

test_that("'by' argument works; NSE", {
  act <- deltatest(df_bucket, click / pageview, by = bucket, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result_bucket)
})

test_that("'by' argument works; character", {
  act <- deltatest(df_bucket, click / pageview, by = "bucket", quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result_bucket)
})

test_that("'by' argument works; variable", {
  group_col <- "bucket"
  act <- deltatest(df_bucket, click / pageview, by = group_col, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result_bucket)
})

test_that("'by' argument works; variable is ignored", {
  bucket <- "x"
  act <- deltatest(df_bucket, click / pageview, by = bucket, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result_bucket)
})

test_that("'by' argument is incorrect", {
  expect_error(deltatest(df_bucket, click / pageview, by = x, quiet = TRUE),
               "The 'formula' or 'by' argument is incorrect. .+ 'x'")
})

test_that("'by' argument is ambiguous", {
  df_bucket$group <- df_bucket$bucket
  bucket <- "group"
  expect_error(deltatest(df_bucket, click / pageview, by = bucket, quiet = TRUE),
               "The 'by' argument is ambiguous.")
})

test_that("'by' argument is ambiguous; special case", {
  bucket <- "bucket"
  act <- deltatest(df_bucket, click / pageview, by = bucket, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result_bucket)
})

test_that("'by' argument is missing", {
  expect_error(deltatest(df_bucket, click / pageview, quiet = TRUE),
               "The 'by' argument is required.")
})


# group_names -------------------------------------------------------------
test_that("'group_names' works", {
  df <- df |> dplyr::mutate(group = dplyr::if_else(group == 0L, "control", "test"))
  act <- deltatest(df, click / pageview ~ group, group_names = c("control", "test"), quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("'group_names' is 'auto'", {
  expect_message({
    deltatest(df, click / pageview ~ group, group_names = "auto", quiet = FALSE)
  }, "control: 0, treatment: 1")
})

test_that("'group_names' is too long", {
  expect_error({
    deltatest(df, click / pageview ~ group, group_names = c("0", "1", "2"))
  }, "The 'group_names' argument must be either 'auto' or a character vector of length 2.")
})

test_that("'group_names' is too short", {
  expect_error({
    deltatest(df, click / pageview ~ group, group_names = c("0"))
  }, "The 'group_names' argument must be either 'auto' or a character vector of length 2.")
})

test_that("'group_names' is incorrect", {
  expect_error({
    deltatest(df, click / pageview ~ group, group_names = c("x", "y"))
  }, "The 'group_names' argument is incorrect.")
})


# type --------------------------------------------------------------------
test_that("'type' works", {
  act <- deltatest(df, click / pageview ~ group, type = "difference", quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("'type' = 'relative_change'", {
  expected_result$statistic <- c(Z = -1.32808601)
  expected_result$p.value <- 0.184149695
  expected_result$conf.int <- structure(c(0.509454597, 1.094270111), conf.level = 0.95)
  expected_result$estimate <- c("mean in control" = 0.234960272, "mean in treatment" = 0.188405797, "relative change" = 0.801862354)
  expected_result$null.value <- c("relative change in means between control and treatment" = 0)
  expected_result$stderr <- 0.149190373

  act <- deltatest(df, click / pageview ~ group, type = "relative_change", quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})


# bias_correction ---------------------------------------------------------
test_that("'bias_correction' = FALSE", {
  act <- deltatest(df, click / pageview ~ group, bias_correction = FALSE, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("'bias_correction' = TRUE", {
  expected_result$statistic <- c(Z = -1.42406648)
  expected_result$p.value <- 0.154427228
  expected_result$conf.int <- structure(c(-0.126889566, 0.020094323), conf.level = 0.95)
  expected_result$estimate <- c("mean in control" = 0.2390234352, "mean in treatment" = 0.1856258139, "difference" = -0.0533976213)

  act <- deltatest(df, click / pageview ~ group, bias_correction = TRUE, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})


# alternative -------------------------------------------------------------
test_that("'alternative' = 'two.sided", {
  act <- deltatest(df, click / pageview ~ group, alternative = "two.sided", quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("'alternative' = 'less", {
  expected_result$statistic <- c(Z = -1.24156594)
  expected_result$p.value <- 0.214396755
  expected_result$conf.int <- structure(c(-Inf, 0.0151219086), conf.level = 0.95)
  expected_result$alternative <- "less"

  act <- deltatest(df, click / pageview ~ group, alternative = "less", quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("'alternative' = 'greater", {
  expected_result$statistic <- c(Z = -1.24156594)
  expected_result$p.value <- 0.214396755
  expected_result$conf.int <- structure(c(-0.108230859, Inf), conf.level = 0.95)
  expected_result$alternative <- "greater"

  act <- deltatest(df, click / pageview ~ group, alternative = "greater", quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})


# conf.level --------------------------------------------------------------
test_that("'conf.level' works", {
  act <- deltatest(df, click / pageview ~ group, conf.level = 0.95, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)

  expected_result$conf.int <- structure(c(-0.1082308593, 0.0151219086), conf.level = 0.9)

  act2 <- deltatest(df, click / pageview ~ group, conf.level = 0.9, quiet = TRUE)
  act2$info <- NULL
  expect_equal(act2, expected_result)

  expect_lt(act$conf.int[1], act2$conf.int[1])
  expect_gt(act$conf.int[2], act2$conf.int[2])
})

test_that("'conf.level' is character", {
  act <- deltatest(df, click / pageview ~ group, conf.level = "0.95", quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("'conf.level' = 0", {
  diff <- unname(expected_result$estimate["difference"])
  expected_result$conf.int <- structure(c(diff, diff), conf.level = 0)

  act <- deltatest(df, click / pageview ~ group, conf.level = 0, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("'conf.level' = 1", {
  expected_result$conf.int <- structure(c(-Inf, Inf), conf.level = 1)

  act <- deltatest(df, click / pageview ~ group, conf.level = 1, quiet = TRUE)
  act$info <- NULL
  expect_equal(act, expected_result)
})

test_that("'conf.level' is invalid", {
  expect_error({
    deltatest(df, click / pageview ~ group, conf.level = -0.1, quiet = TRUE)
  }, "The 'conf.level' argument must be a single number between 0 and 1")

  expect_error({
    deltatest(df, click / pageview ~ group, conf.level = 1.1, quiet = TRUE)
  }, "The 'conf.level' argument must be a single number between 0 and 1")

  expect_error({
    deltatest(df, click / pageview ~ group, conf.level = -Inf, quiet = TRUE)
  }, "The 'conf.level' argument must be a single number between 0 and 1")

  expect_error({
    deltatest(df, click / pageview ~ group, conf.level = Inf, quiet = TRUE)
  }, "The 'conf.level' argument must be a single number between 0 and 1")

  expect_error({
    deltatest(df, click / pageview ~ group, conf.level = NA, quiet = TRUE)
  }, "The 'conf.level' argument must be a single number between 0 and 1")
})


# na.rm -------------------------------------------------------------------
df_na <- df
df_na[1, "click"] <- NA

test_that("'na.rm' works", {
  expected <- deltatest(df_na[-1, ], click / pageview ~ group, na.rm = TRUE, quiet = TRUE)
  act <- deltatest(df_na, click / pageview ~ group, na.rm = TRUE, quiet = TRUE)

  expect_equal(act, expected)
})

test_that("'na.rm' = FALSE with NA", {
  expect_error({
    deltatest(df_na, click / pageview ~ group, na.rm = FALSE, quiet = TRUE)
  }, "NA value is found in the data at row number 1.")
})


# quiet -------------------------------------------------------------------
test_that("'quiet' works", {
  expect_message({
    deltatest(df, click / pageview ~ group, group_names = "auto", quiet = FALSE)
  }, "control: 0, treatment: 1")

  expect_no_message({
    deltatest(df, click / pageview ~ group, group_names = "auto", quiet = TRUE)
  })
})
