
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deltatest

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of deltatest from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("hoxo-m/deltatest")
```

## Example

``` r
library(dplyr)
library(deltatest)

n_user <- 2000

set.seed(314)
df <- generate_dummy_data(n_user) |> 
  mutate(group = if_else(group == 0L, "control", "treatment")) |>
  group_by(user_id, group) |> 
  summarise(click = sum(metric), pageview = n(), .groups = "drop")
df
#> # A tibble: 2,000 × 4
#>    user_id group     click pageview
#>      <int> <chr>     <int>    <int>
#>  1       1 treatment     1        6
#>  2       2 treatment     2       11
#>  3       3 control       0       17
#>  4       4 control       4       12
#>  5       5 control       5       10
#>  6       6 control       1       15
#>  7       7 control       2        6
#>  8       8 treatment     2       11
#>  9       9 treatment     2       16
#> 10      10 control       0       17
#> # ℹ 1,990 more rows

deltatest(df, click / pageview, by = group)
#> 
#>  Two Sample Z-test Using the Delta Method
#> 
#> data:  click/pageview by group
#> z = 0.89707, p-value = 0.3697
#> alternative hypothesis: true difference in means between control and treatment is not equal to 0
#> 95 percent confidence interval:
#>  -0.009110998  0.024490289
#> sample estimates:
#>   mean in control mean in treatment        difference 
#>       0.241848567       0.249538212       0.007689645
```

``` r
library(ggplot2)

set.seed(314)

p_values <- NULL
for (i in 1:5000) {
  df <- generate_dummy_data(n_user) |> 
    group_by(group) |> 
    summarise(click = sum(metric), pageview = n(), .groups = "drop")
  
  result <- prop.test(df$click, df$pageview, correct = FALSE)
  
  p_values[i] <- result$p.value
}

df <- data.frame(p_value = p_values) |>
  mutate(range = cut(p_value, breaks = seq(0, 1, by = 0.05))) |>
  group_by(range) |>
  summarise(p = factor(ceiling(max(p_value) * 20) / 20), n = n()) |>
  mutate(prop = n / sum(n))
ggplot(df, aes(p, prop)) +
  geom_col() +
  geom_hline(yintercept = 0.05, color = "red") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.02), minor_breaks = NULL) +
  xlab("p-value") + ylab("proportion")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="600" />

``` r
set.seed(314)

p_values <- NULL
for (i in 1:5000) {
  df <- generate_dummy_data(n_user) |> 
    group_by(user_id, group) |> 
    summarise(click = sum(metric), pageview = n(), .groups = "drop")
  
  result <- deltatest(df, click / pageview, by = group)
  
  p_values[i] <- result$p.value
}

df <- data.frame(p_value = p_values) |>
  mutate(range = cut(p_value, breaks = seq(0, 1, by = 0.05))) |>
  group_by(range) |>
  summarise(p = factor(ceiling(max(p_value) * 20) / 20), n = n()) |>
  mutate(prop = n / sum(n))
ggplot(df, aes(p, prop)) +
  geom_col() +
  geom_hline(yintercept = 0.05, color = "red") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.01), minor_breaks = NULL) +
  xlab("p-value") + ylab("proportion")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="600" />

## References

- Deng, A., Knoblich, U., & Lu, J. (2018). Applying the Delta Method in
  Metric Analytics: A Practical Guide with Novel Ideas. *Proceedings of
  the 24th ACM SIGKDD International Conference on Knowledge Discovery &
  Data Mining.*
- Deng, A., Lu, J., & Litz, J. (2017). Trustworthy Analysis of Online
  A/B Tests: Pitfalls, challenges and solutions. *Proceedings of the
  Tenth ACM International Conference on Web Search and Data Mining.*
