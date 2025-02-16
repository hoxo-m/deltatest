library(dplyr)
library(deltatest)

ztest <- function(data) {
  data_c <- data |> filter(group == 0L)
  data_t <- data |> filter(group == 1L)

  mean_c <- mean(data_c$metric)
  mean_t <- mean(data_t$metric)
  diff <- mean_t - mean_c
  se2_c <- var(data_c$metric)
  se2_t <- var(data_t$metric)
  se <- sqrt(se2_c / nrow(data_c) + se2_t / nrow(data_t))
  z <- diff / se
  p <- 2 * pnorm(-abs(z))
  list(p.value = p)
}

n_user <- 2000L

set.seed(314)
p_values <- NULL
for (i in 1:5000) {
  df <- deltatest::generate_dummy_data(n_user)

  result <- ztest(df)

  p_values[i] <- result$p.value
}

saveRDS(p_values, "data-raw/p_values_from_standard_Z_test.rds")

set.seed(314)
p_values <- NULL
for (i in 1:5000) {
  df <- generate_dummy_data(n_user) |>
    group_by(user_id, group) |>
    summarise(click = sum(metric), pageview = n(), .groups = "drop")

  result <- deltatest(df, click / pageview, by = group, quiet = TRUE)

  p_values[i] <- result$p.value
}

saveRDS(p_values, "data-raw/p_values_from_Delta_meethod.rds")
