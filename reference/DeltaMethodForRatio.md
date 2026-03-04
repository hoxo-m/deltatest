# The Delta Method for Ratio

Applies the Delta method to the ratio of two random variables,
\\f(X,Y)=X/Y\\, to estimate the expected value, variance, standard
error, and confidence interval.

## References

- id:sz_dr (2018). Calculating the mean and variance of the ratio of
  random variables using the Delta method \[in Japanese\]. *If you are
  human, think more now.*
  <https://www.szdrblog.info/entry/2018/11/18/154952>

## Methods

### Public methods

- [`DeltaMethodForRatio$new()`](#method-DeltaMethodForRatio-new)

- [`DeltaMethodForRatio$get_expected_value()`](#method-DeltaMethodForRatio-get_expected_value)

- [`DeltaMethodForRatio$get_variance()`](#method-DeltaMethodForRatio-get_variance)

- [`DeltaMethodForRatio$get_squared_standard_error()`](#method-DeltaMethodForRatio-get_squared_standard_error)

- [`DeltaMethodForRatio$get_standard_error()`](#method-DeltaMethodForRatio-get_standard_error)

- [`DeltaMethodForRatio$get_confidence_interval()`](#method-DeltaMethodForRatio-get_confidence_interval)

- [`DeltaMethodForRatio$get_info()`](#method-DeltaMethodForRatio-get_info)

- [`DeltaMethodForRatio$compute_expected_value()`](#method-DeltaMethodForRatio-compute_expected_value)

- [`DeltaMethodForRatio$compute_variance()`](#method-DeltaMethodForRatio-compute_variance)

- [`DeltaMethodForRatio$compute_confidence_interval()`](#method-DeltaMethodForRatio-compute_confidence_interval)

- [`DeltaMethodForRatio$clone()`](#method-DeltaMethodForRatio-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new DeltaMethodForRatio object.

#### Usage

    DeltaMethodForRatio$new(numerator, denominator, bias_correction = FALSE)

#### Arguments

- `numerator, denominator`:

  numeric vectors sampled from the distributions of the random variables
  in the numerator and denominator of the ratio.

- `bias_correction`:

  logical value indicating whether correction to the mean of the metric
  is performed using the second-order term of the Taylor expansion. The
  default is `FALSE`.

------------------------------------------------------------------------

### Method `get_expected_value()`

Get the expected value.

#### Usage

    DeltaMethodForRatio$get_expected_value()

#### Returns

numeric estimate of the expected value of the ratio.

------------------------------------------------------------------------

### Method `get_variance()`

Get the variance.

#### Usage

    DeltaMethodForRatio$get_variance()

#### Returns

numeric estimate of the variance of the ratio.

------------------------------------------------------------------------

### Method `get_squared_standard_error()`

Get the squared standard error.

#### Usage

    DeltaMethodForRatio$get_squared_standard_error()

#### Returns

numeric estimate of the squared standard error of the ratio.

------------------------------------------------------------------------

### Method `get_standard_error()`

Get the standard error.

#### Usage

    DeltaMethodForRatio$get_standard_error()

#### Returns

numeric estimate of the standard error of the ratio.

------------------------------------------------------------------------

### Method `get_confidence_interval()`

Get the confidence interval.

#### Usage

    DeltaMethodForRatio$get_confidence_interval(
      alternative = c("two.sided", "less", "greater"),
      conf_level = 0.95
    )

#### Arguments

- `alternative`:

  character string specifying the alternative hypothesis, must be one of
  `"two.sided"` (default), `"greater"`, or `"less"`. You can specify
  just the initial letter.

- `conf_level`:

  numeric value specifying the confidence level of the interval. The
  default is 0.95.

#### Returns

numeric estimates of the lower and upper bounds of the confidence
interval of the ratio.

------------------------------------------------------------------------

### Method `get_info()`

Get statistical information.

#### Usage

    DeltaMethodForRatio$get_info(
      alternative = c("two.sided", "less", "greater"),
      conf_level = 0.95
    )

#### Arguments

- `alternative`:

  character string specifying the alternative hypothesis, must be one of
  `"two.sided"` (default), `"greater"`, or `"less"`. You can specify
  just the initial letter.

- `conf_level`:

  numeric value specifying the confidence level of the interval. The
  default is 0.95.

#### Returns

numeric estimates include the expected value, variance, standard error,
and confidence interval.

------------------------------------------------------------------------

### Method `compute_expected_value()`

Class method to compute the expected value of the ratio using the Delta
method.

#### Usage

    DeltaMethodForRatio$compute_expected_value(
      mean1,
      mean2,
      var2,
      cov = 0,
      bias_correction = FALSE
    )

#### Arguments

- `mean1`:

  numeric value of the mean numerator of the ratio.

- `mean2`:

  numeric value of the mean denominator of the ratio.

- `var2`:

  numeric value of the variance of the denominator of the ratio.

- `cov`:

  numeric value of the covariance between the numerator and denominator
  of the ratio. The default is 0.

- `bias_correction`:

  logical value indicating whether correction to the mean of the metric
  is performed using the second-order term of the Taylor expansion. The
  default is `FALSE`.

#### Returns

numeric estimate of the expected value of the ratio.

------------------------------------------------------------------------

### Method `compute_variance()`

Class method to compute the variance of the ratio using the Delta
method.

#### Usage

    DeltaMethodForRatio$compute_variance(mean1, mean2, var1, var2, cov = 0)

#### Arguments

- `mean1`:

  numeric value of the mean numerator of the ratio.

- `mean2`:

  numeric value of the mean denominator of the ratio.

- `var1`:

  numeric value of the variance of the numerator of the ratio.

- `var2`:

  numeric value of the variance of the denominator of the ratio.

- `cov`:

  numeric value of the covariance between the numerator and denominator
  of the ratio. The default is 0.

#### Returns

numeric estimate of the variance of the ratio

------------------------------------------------------------------------

### Method `compute_confidence_interval()`

Class method to compute the confidence interval of the ratio using the
Delta method.

#### Usage

    DeltaMethodForRatio$compute_confidence_interval(
      mean,
      standard_error,
      alternative = c("two.sided", "less", "greater"),
      conf_level = 0.95
    )

#### Arguments

- `mean`:

  numeric value of the estimated mean of the ratio.

- `standard_error`:

  numeric value of the estimated standard error of the mean of the
  ratio.

- `alternative`:

  character string specifying the alternative hypothesis, must be one of
  `"two.sided"` (default), `"greater"`, or `"less"`. You can specify
  just the initial letter.

- `conf_level`:

  numeric value specifying the confidence level of the interval. The
  default is 0.95.

#### Returns

numeric estimates of the lower and upper bounds of the confidence
interval of the ratio.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DeltaMethodForRatio$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
