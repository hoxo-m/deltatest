\name{DeltaMethodForRatioClassMethod}
\alias{DeltaMethodForRatio$compute_expected_value}
\alias{DeltaMethodForRatio$compute_variance}
\alias{DeltaMethodForRatio$compute_confidence_interval}
\title{
Class Methods of DeltaMethodForRatio
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

}
\usage{
DeltaMethodForRatio$compute_expected_value(mean1, mean2, var2, cov = 0, bias_correction = TRUE)
DeltaMethodForRatio$compute_variance(mean1, mean2, var1, var2, cov = 0)
DeltaMethodForRatio$compute_confidence_interval(estimate, standard_error, alternative, conf_level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mean1,mean2}{}
  \item{var1,var2}{}
  \item{cov}{}
  \item{bias_correction}{logical value indicating whether correction to the mean
        of the metric is performed using the second-order term of the Taylor
        expansion. The default is `TRUE`.}
  \item{estimate}{}
  \item{standard_error}{}
  \item{alternative}{character string specifying the alternative hypothesis,
        must be one of `"two.sided"` (default), `"greater"`, or `"less"`. You
        can specify just the initial letter.}
  \item{conf_level}{numeric value specifying the confidence level of the
        interval. The default is 0.95.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{DeltaMethodForRatio$compute_expected_value}{numeric estimate of the
        expected value of the ratio}
  \item{DeltaMethodForRatio$compute_variance}{numeric estimate of the variance
        of the ratio}
  \item{DeltaMethodForRatio$compute_confidence_interval}{numeric estimates of
        the lower and upper bounds of the confidence interval of the ratio.}
}
\references{
  \itemize{
    \item id:sz_dr (2018). Calculating the mean and variance of the ratio of
      random variables using the Delta method [in Japanese]. \emph{If you are
      human, think more now.}
      \url{https://www.szdrblog.info/entry/2018/11/18/154952}
  }
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{DeltaMethodForRatio}}
}
\examples{
library(deltatest)
library(dplyr)

n_user <- 100

set.seed(314)
df <- generate_dummy_data(n_user) |>
  filter(group == 0) |>
  group_by(user_id, group) |>
  summarise(click = sum(metric), pageview = n(), .groups = "drop")

mean1 <- mean(df$click)
mean2 <- mean(df$pageview)
var1 <- var(df$click)
var2 <- var(df$pageview)
cov <- cov(df$click, df$pageview)

mean <- DeltaMethodForRatio$compute_expected_value(mean1, mean2, var2, cov)
var <- DeltaMethodForRatio$compute_variance(mean1, mean2, var1, var2, cov)
se <- sqrt(var / nrow(df))
DeltaMethodForRatio$compute_confidence_interval(mean, se)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
