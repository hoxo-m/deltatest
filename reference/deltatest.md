# Two Sample Z-Test for Ratio Metrics Using the Delta Method

Performs two sample Z-test to compare the ratio metrics between two
groups using the delta method. The Delta method is used to estimate the
variance by accounting for the correlation between the numerator and
denominator of ratio metrics.

## Usage

``` r
deltatest(
  data,
  formula,
  by,
  group_names = "auto",
  type = c("difference", "relative_change"),
  bias_correction = FALSE,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  na.rm = FALSE,
  quiet = FALSE
)
```

## Arguments

- data:

  data.frame containing the numerator and denominator columns of the
  ratio metric, aggregated by randomization unit. It also includes a
  column indicating the assigned group (control or treatment). For
  example, if randomizing by user while the metric is click-through rate
  (CTR) per page-view, the numerator is the number of clicks per user,
  and the denominator is the number of page views per user.

- formula:

  expression representing the ratio metric. It can be written in three
  styles: standard formula `x/y ~ group`, lambda formula `~ x/y`, or NSE
  expression `x/y`.

- by:

  character string or symbol that indicates the group column. If the
  group column is specified in the `formula` argument, it is not
  required.

- group_names:

  character vector of length 2 or `"auto"`. It specifies which of the
  two strings contained in the group column is the control group and
  which is the treatment group. The first string is considered the
  control group, and the second string is considered the treatment
  group. If `"auto"` is specified, it is interpreted as specifying the
  strings in the group column sorted in lexicographical order. The
  default is `"auto"`.

- type:

  character string specifying the test type. If `"difference"`
  (default), the hypothesis test evaluates the difference in means of
  the ratio metric between two groups. If `"relative_change"`, it
  evaluates the relative change \\(\mu_2 - \mu_1) / \mu_1\\ instead. You
  can specify just the initial letter.

- bias_correction:

  logical value indicating whether correction to the mean of the metric
  is performed using the second-order term of the Taylor expansion. The
  default is `FALSE`.

- alternative:

  character string specifying the alternative hypothesis, must be one of
  `"two.sided"` (default), `"greater"`, or `"less"`. You can specify
  just the initial letter.

- conf.level:

  numeric value specifying the confidence level of the interval. The
  default is 0.95.

- na.rm:

  logical value. If `TRUE`, rows containing NA values in the data will
  be excluded from the analysis. The default is `FALSE`.

- quiet:

  logical value indicating whether messages should be displayed during
  the execution of the function. The default is `FALSE`.

## Value

A list with class `"htest"` containing following components:

- statistic:

  the value of the Z-statistic.

- p.value:

  the p-value for the test.

- conf.int:

  a confidence interval for the difference or relative change
  appropriate to the specified alternative hypothesis.

- estimate:

  the estimated means of the two groups, and the difference or relative
  change.

- null.value:

  the hypothesized value of the difference or relative change in means
  under the null hypothesis.

- stderr:

  the standard error of the difference or relative change.

- alternative:

  a character string describing the alternative hypothesis.

- method:

  a character string describing the method used.

- data.name:

  the name of the data.

## References

- Deng, A., Knoblich, U., & Lu, J. (2018). Applying the Delta Method in
  Metric Analytics: A Practical Guide with Novel Ideas. *Proceedings of
  the 24th ACM SIGKDD International Conference on Knowledge Discovery &
  Data Mining.*
  [doi:10.1145/3219819.3219919](https://doi.org/10.1145/3219819.3219919)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(deltatest)

n_user <- 2000

set.seed(314)
df <- deltatest::generate_dummy_data(n_user) |>
  group_by(user_id, group) |>
  summarise(click = sum(metric), pageview = n(), .groups = "drop")

deltatest(df, click / pageview, by = group)
#> control: 0, treatment: 1
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
#> 
```
