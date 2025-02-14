
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deltatest: Statistical Hypothesis Testing Using the Delta Method for Online A/B Testing

<!-- badges: start -->

[![R-CMD-check](https://github.com/hoxo-m/deltatest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hoxo-m/deltatest/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## 1. Overview

Online A/B testing poses a significant practical challenge: the
randomization unit often differs from the analysis unit. For instance,
while control and treatment groups are typically assigned at the user
level, evaluation metrics are frequently measured at a more detailed
level, such as the click-through rate per page view. In this scenario,
the randomization unit is the user, but the analysis unit is the page
view.

This discrepancy raises concerns when performing statistical hypothesis
tests, as within-user correlation may be present. Specifically, a single
user can generate multiple page views, and some users may inherently
have a higher likelihood of clicking than others, which may potentially
violate the i.i.d. assumption required for hypothesis tests.

To address this issue, Deng et al. (2018) proposes a statistical
hypothesis testing method using the Delta method. This package has been
developed to easily execute that method.

First, we prepare a data frame that includes the number of clicks and
page views aggregated for each user. This data frame also contains a
column indicating whether each user was assigned to the control or
treatment group.

``` r
library(dplyr)

n_user <- 2000

set.seed(314)
data <- deltatest::generate_dummy_data(n_user) |> 
  mutate(group = if_else(group == 0, "control", "treatment")) |>
  group_by(user_id, group) |> 
  summarise(clicks = sum(metric), pageviews = n(), .groups = "drop")
data
#> # A tibble: 2,000 × 4
#>    user_id group     clicks pageviews
#>      <int> <chr>      <int>     <int>
#>  1       1 treatment      1         6
#>  2       2 treatment      2        11
#>  3       3 control        0        17
#>  4       4 control        4        12
#>  5       5 control        5        10
#>  6       6 control        1        15
#>  7       7 control        2         6
#>  8       8 treatment      2        11
#>  9       9 treatment      2        16
#> 10      10 control        0        17
#> # ℹ 1,990 more rows
```

To perform a statistical hypothesis test using the Delta method on this
data, as follows:

``` r
library(deltatest)

deltatest(data, clicks / pageviews, by = group)
#> 
#>  Two Sample Z-test Using the Delta Method
#> 
#> data:  clicks/pageviews by group
#> Z = 0.31437, p-value = 0.7532
#> alternative hypothesis: true difference in means between control and treatment is not equal to 0
#> 95 percent confidence interval:
#>  -0.01410593  0.01949536
#> sample estimates:
#>   mean in control mean in treatment        difference 
#>       0.245959325       0.248654038       0.002694713
```

## 2. Installation

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
#> Z = 0.31437, p-value = 0.7532
#> alternative hypothesis: true difference in means between control and treatment is not equal to 0
#> 95 percent confidence interval:
#>  -0.01410593  0.01949536
#> sample estimates:
#>   mean in control mean in treatment        difference 
#>       0.245959325       0.248654038       0.002694713
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

<img src="man/figures/README-unnamed-chunk-3-1.png" width="600" />

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

<img src="man/figures/README-unnamed-chunk-4-1.png" width="600" />

## Related Work

- [tidydelta: Estimation of Standard Errors using Delta
  Method](https://cran.r-project.org/package=tidydelta)

## References

- Deng, A., Knoblich, U., & Lu, J. (2018). Applying the Delta Method in
  Metric Analytics: A Practical Guide with Novel Ideas. *Proceedings of
  the 24th ACM SIGKDD International Conference on Knowledge Discovery &
  Data Mining.*
  [doi:10.1145/3219819.3219919](https://doi.org/10.1145/3219819.3219919)
- Deng, A., Lu, J., & Litz, J. (2017). Trustworthy Analysis of Online
  A/B Tests: Pitfalls, challenges and solutions. *Proceedings of the
  Tenth ACM International Conference on Web Search and Data Mining.*
  [doi:10.1145/3018661.3018677](https://doi.org/10.1145/3018661.3018677)
- id:sz_dr (2018). Calculating the mean and variance of the ratio of
  random variables using the Delta method \[in Japanese\]. *If you are
  human, think more now.*
  <https://www.szdrblog.info/entry/2018/11/18/154952>
