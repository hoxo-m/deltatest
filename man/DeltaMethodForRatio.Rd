% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delta_method_for_ratio.R
\name{DeltaMethodForRatio}
\alias{DeltaMethodForRatio}
\title{The Delta Method for Ratio}
\description{
Applies the Delta method to the ratio of two random variables,
\eqn{f(X,Y)=X/Y}, to estimate the expected value, variance, standard error,
and confidence interval.
}
\references{
\itemize{
\item id:sz_dr (2018). Calculating the mean and variance of the ratio of random
variables using the Delta method [in Japanese]. \emph{If you are human, think
more now.} \url{https://www.szdrblog.info/entry/2018/11/18/154952}
}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DeltaMethodForRatio-new}{\code{DeltaMethodForRatio$new()}}
\item \href{#method-DeltaMethodForRatio-get_expected_value}{\code{DeltaMethodForRatio$get_expected_value()}}
\item \href{#method-DeltaMethodForRatio-get_variance}{\code{DeltaMethodForRatio$get_variance()}}
\item \href{#method-DeltaMethodForRatio-get_squared_standard_error}{\code{DeltaMethodForRatio$get_squared_standard_error()}}
\item \href{#method-DeltaMethodForRatio-get_standard_error}{\code{DeltaMethodForRatio$get_standard_error()}}
\item \href{#method-DeltaMethodForRatio-get_confidence_interval}{\code{DeltaMethodForRatio$get_confidence_interval()}}
\item \href{#method-DeltaMethodForRatio-get_info}{\code{DeltaMethodForRatio$get_info()}}
\item \href{#method-DeltaMethodForRatio-compute_expected_value}{\code{DeltaMethodForRatio$compute_expected_value()}}
\item \href{#method-DeltaMethodForRatio-compute_variance}{\code{DeltaMethodForRatio$compute_variance()}}
\item \href{#method-DeltaMethodForRatio-compute_confidence_interval}{\code{DeltaMethodForRatio$compute_confidence_interval()}}
\item \href{#method-DeltaMethodForRatio-clone}{\code{DeltaMethodForRatio$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-new"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new DeltaMethodForRatio object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$new(numerator, denominator, bias_correction = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{numerator, denominator}}{numeric vectors sampled from the
distributions of the random variables in the numerator and denominator
of the ratio.}

\item{\code{bias_correction}}{logical value indicating whether correction to the
mean of the metric is performed using the second-order term of the
Taylor expansion. The default is \code{FALSE}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-get_expected_value"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-get_expected_value}{}}}
\subsection{Method \code{get_expected_value()}}{
Get the expected value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$get_expected_value()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
numeric estimate of the expected value of the ratio.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-get_variance"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-get_variance}{}}}
\subsection{Method \code{get_variance()}}{
Get the variance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$get_variance()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
numeric estimate of the variance of the ratio.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-get_squared_standard_error"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-get_squared_standard_error}{}}}
\subsection{Method \code{get_squared_standard_error()}}{
Get the squared standard error.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$get_squared_standard_error()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
numeric estimate of the squared standard error of the ratio.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-get_standard_error"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-get_standard_error}{}}}
\subsection{Method \code{get_standard_error()}}{
Get the standard error.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$get_standard_error()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
numeric estimate of the standard error of the ratio.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-get_confidence_interval"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-get_confidence_interval}{}}}
\subsection{Method \code{get_confidence_interval()}}{
Get the confidence interval.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$get_confidence_interval(
  alternative = c("two.sided", "less", "greater"),
  conf_level = 0.95
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{alternative}}{character string specifying the alternative
hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"}, or
\code{"less"}. You can specify just the initial letter.}

\item{\code{conf_level}}{numeric value specifying the confidence level of the
interval. The default is 0.95.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
numeric estimates of the lower and upper bounds of the confidence
interval of the ratio.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-get_info"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-get_info}{}}}
\subsection{Method \code{get_info()}}{
Get statistical information.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$get_info(
  alternative = c("two.sided", "less", "greater"),
  conf_level = 0.95
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{alternative}}{character string specifying the alternative
hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"}, or
\code{"less"}. You can specify just the initial letter.}

\item{\code{conf_level}}{numeric value specifying the confidence level of the
interval. The default is 0.95.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
numeric estimates include the expected value, variance, standard
error, and confidence interval.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-compute_expected_value"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-compute_expected_value}{}}}
\subsection{Method \code{compute_expected_value()}}{
Class method to compute the expected value of the ratio using the Delta
method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$compute_expected_value(
  mean1,
  mean2,
  var2,
  cov = 0,
  bias_correction = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mean1}}{numeric value of the mean numerator of the ratio.}

\item{\code{mean2}}{numeric value of the mean denominator of the ratio.}

\item{\code{var2}}{numeric value of the variance of the denominator of the ratio.}

\item{\code{cov}}{numeric value of the covariance between the numerator and
denominator of the ratio. The default is 0.}

\item{\code{bias_correction}}{logical value indicating whether correction to the
mean of the metric is performed using the second-order term of the
Taylor expansion. The default is \code{FALSE}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
numeric estimate of the expected value of the ratio.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-compute_variance"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-compute_variance}{}}}
\subsection{Method \code{compute_variance()}}{
Class method to compute the variance of the ratio using the Delta method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$compute_variance(mean1, mean2, var1, var2, cov = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mean1}}{numeric value of the mean numerator of the ratio.}

\item{\code{mean2}}{numeric value of the mean denominator of the ratio.}

\item{\code{var1}}{numeric value of the variance of the numerator of the ratio.}

\item{\code{var2}}{numeric value of the variance of the denominator of the ratio.}

\item{\code{cov}}{numeric value of the covariance between the numerator and
denominator of the ratio. The default is 0.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
numeric estimate of the variance of the ratio
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-compute_confidence_interval"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-compute_confidence_interval}{}}}
\subsection{Method \code{compute_confidence_interval()}}{
Class method to compute the confidence interval of the ratio using the
Delta method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$compute_confidence_interval(
  mean,
  standard_error,
  alternative = c("two.sided", "less", "greater"),
  conf_level = 0.95
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mean}}{numeric value of the estimated mean of the ratio.}

\item{\code{standard_error}}{numeric value of the estimated standard error of
the mean of the ratio.}

\item{\code{alternative}}{character string specifying the alternative
hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"}, or
\code{"less"}. You can specify just the initial letter.}

\item{\code{conf_level}}{numeric value specifying the confidence level of the
interval. The default is 0.95.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
numeric estimates of the lower and upper bounds of the confidence
interval of the ratio.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeltaMethodForRatio-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DeltaMethodForRatio-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeltaMethodForRatio$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
