#' Two Sample Z-Test for Ratio Metrics Using the Delta Method
#'
#' Performs two sample Z-test to compare the ratio metrics between two groups
#' using the delta method. The Delta method is used to estimate the variance by
#' accounting for the correlation between the numerator and denominator of ratio
#' metrics.
#'
#' @param data data.frame containing the numerator and denominator columns of
#'   the ratio metric, aggregated by randomization unit. It also includes a
#'   column indicating the assigned group (control or treatment). For example,
#'   if randomizing by user while the metric is click-through rate (CTR) per
#'   page-view, the numerator is the number of clicks per user, and the
#'   denominator is the number of page views per user.
#' @param formula expression representing the ratio metric. It can be written in
#'   three styles: standard formula `x/y ~ group`, lambda formula `~ x/y`, or
#'   NSE expression `x/y`.
#' @param by character string or symbol that indicates the group column. If the
#'   group column is specified in the `formula` argument, it is not required.
#' @param group_names character vector of length 2 or `"auto"`. It specifies
#'   which of the two strings contained in the group column is the control group
#'   and which is the treatment group. The first string is considered the control
#'   group, and the second string is considered the treatment group. If `"auto"`
#'   is specified, it is interpreted as specifying the strings in the group
#'   column sorted in lexicographical order. The default is `"auto"`.
#' @param type character string specifying the test type. If `"difference"`
#'   (default), the hypothesis test evaluates the difference in means of the
#'   ratio metric between two groups. If `"relative_change"`, it evaluates the
#'   relative change \eqn{(\mu_2 - \mu_1) / \mu_1} instead. You can specify just
#'   the initial letter.
#' @param bias_correction logical value indicating whether correction to the
#'   mean of the metric is performed using the second-order term of the Taylor
#'   expansion. The default is `FALSE`.
#' @param alternative character string specifying the alternative hypothesis,
#'   must be one of `"two.sided"` (default), `"greater"`, or `"less"`. You can
#'   specify just the initial letter.
#' @param conf.level numeric value specifying the confidence level of the
#'   interval. The default is 0.95.
#' @param na.rm logical value. If `TRUE`, rows containing NA values in the data
#'   will be excluded from the analysis. The default is `FALSE`.
#' @param quiet logical value indicating whether messages should be displayed
#'   during the execution of the function. The default is `FALSE`.
#'
#' @return A list with class `"htest"` containing following components:
#'   \item{statistic}{the value of the Z-statistic.}
#'   \item{p.value}{the p-value for the test.}
#'   \item{conf.int}{a confidence interval for the difference or relative change
#'                   appropriate to the specified alternative hypothesis.}
#'   \item{estimate}{the estimated means of the two groups, and the difference
#'                   or relative change.}
#'   \item{null.value}{the hypothesized value of the difference or relative
#'                     change in means under the null hypothesis.}
#'   \item{stderr}{the standard error of the difference or relative change.}
#'   \item{alternative}{a character string describing the alternative hypothesis.}
#'   \item{method}{a character string describing the method used.}
#'   \item{data.name}{the name of the data.}
#'
#' @examples
#' library(dplyr)
#' library(deltatest)
#'
#' n_user <- 2000
#'
#' set.seed(314)
#' df <- deltatest::generate_dummy_data(n_user) |>
#'   group_by(user_id, group) |>
#'   summarise(click = sum(metric), pageview = n(), .groups = "drop")
#'
#' deltatest(df, click / pageview, by = group)
#'
#' @references
#' - Deng, A., Knoblich, U., & Lu, J. (2018). Applying the Delta
#'   Method in Metric Analytics: A Practical Guide with Novel Ideas.
#'   *Proceedings of the 24th ACM SIGKDD International Conference on Knowledge
#'   Discovery & Data Mining.* \doi{10.1145/3219819.3219919}
#'
#' @importFrom glue glue
#' @importFrom stats complete.cases
#'
#' @export
deltatest <- function(data, formula, by, group_names = "auto",
                      type = c("difference", "relative_change"),
                      bias_correction = FALSE,
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, na.rm = FALSE, quiet = FALSE) {
  # check arguments ---------------------------------------------------------
  data <- as.data.frame(data)
  if (missing(formula)) {
    stop("The 'formula' argument is required but missing. Use the format 'numerator/denominator ~ group', e.g., click/pageview ~ group.")
  }
  group_names <- as.character(group_names)
  if (length(group_names) > 2L || (length(group_names) == 1L && group_names != "auto")) {
    stop("The 'group_names' argument must be either 'auto' or a character vector of length 2.")
  }
  type <- match.arg(type)
  bias_correction <- as.logical(bias_correction)
  alternative <- match.arg(alternative)
  if (length(conf.level) >= 2 || is.na(conf.level) || conf.level < 0 || conf.level > 1) {
    stop("The 'conf.level' argument must be a single number between 0 and 1")
  } else {
    conf.level <- as.double(conf.level)
  }
  na.rm <- as.logical(na.rm)

  # NSE (non-standard evaluation): formula = y / x
  metric_call <- rlang::enexpr(formula)
  if (!missing(by)) {
    group_col_call <- rlang::ensym(by)
  }

  by_argument_required <- TRUE
  if (!rlang::is_call(metric_call, name = "/", n = 2L)) {
    # standard evaluation
    if (rlang::is_formula(formula, lhs = TRUE)) {
      # standard formula: y / x ~ group
      metric_call <- rlang::call_args(formula)[[1L]]
      group_col_call <- rlang::call_args(formula)[[2L]]
      by_argument_required  <- FALSE
    } else if (rlang::is_formula(formula, lhs = FALSE)) {
      # lambda formula: ~ y / x
      formula_quosure <- rlang::as_quosure(formula)
      metric_call <- rlang::quo_get_expr(formula_quosure)
    } else if (rlang::is_call(formula, name = "/", n = 2L)) {
      # formula = quote(y / x)
      metric_call <- formula
    }
  }

  if (missing(by) && by_argument_required) {
    stop("The 'by' argument is required.")
  }

  group_col <- rlang::as_string(group_col_call)

  if (by_argument_required) {
    # check 'by'
    if (group_col %in% ls(parent.frame())) {
      group_col_in_parent <- eval(group_col_call, parent.frame())
      if (group_col_in_parent %in% ls(data) && group_col %in% ls(data)) {
        if (group_col_in_parent != group_col) {
          stop(glue("The 'by' argument is ambiguous. '{group_col}' could either be a column in 'data' or a variable. If it is a variable, it specifies the '{group_col_in_parent}' column."))
        }
      } else if (!group_col %in% ls(data)) {
        group_col <- group_col_in_parent
      }
    }
  }

  # check 'formula'
  if (!rlang::is_call(metric_call, name = "/", n = 2L)) {
    stop("The 'formula' argument is incorrect. Use the format 'numerator/denominator ~ group', e.g., click/pageview ~ group.")
  }

  literals <- c(extract_all_literals(metric_call), group_col)
  absent_literals <- literals[!literals %in% ls(data)]
  if (length(absent_literals) > 0L) {
    absent_literals <- paste0("'", absent_literals, "'", collapse = ", ")
    stop(glue("The 'formula' or 'by' argument is incorrect. The data does not contain a column named {absent_literals}."))
  }

  # check 'group_names'
  if (length(group_names) == 2L && !all(group_names %in% unique(data[[group_col]]))) {
    stop(glue("The 'group_names' argument is incorrect. The '{group_col}' column in the data does not contain the elements '{group_names[1]}' or '{group_names[2]}'."))
  }

  # format 'data' -----------------------------------------------------------
  data_name <- paste(rlang::as_label(metric_call), "by", group_col)
  numer_call <- rlang::call_args(metric_call)[[1L]]
  denom_call <- rlang::call_args(metric_call)[[2L]]

  if (!rlang::is_symbol(numer_call)) {
    data$numer <- eval(numer_call, data)
    numer_call <- quote(numer)
  }
  if (!rlang::is_symbol(denom_call)) {
    data$denom <- eval(denom_call, data)
    denom_call <- quote(denom)
  }

  numer_col <- rlang::as_string(numer_call)
  denom_col <- rlang::as_string(denom_call)

  data <- data[c(numer_col, denom_col, group_col)]
  comlete_cases <- complete.cases(data)
  if (na.rm) {
    data <- data[comlete_cases, ]
  } else if (!all(comlete_cases)) {
    na_row_number <- which(!comlete_cases)[1]
    stop(glue("NA value is found in the data at row number {na_row_number}. By setting the 'na.rm' argument to 'TRUE', you can remove it from the data and proceed with execution."))
  }

  # execute Z-test using the Delta method -----------------------------------
  data_split <- split_control_treatment(data, group_col, group_names, quiet)
  group_names <- names(data_split)
  data_c <- data_split[[1L]]
  data_t <- data_split[[2L]]
  result <- deltatest_impl(data_c[[numer_col]], data_c[[denom_col]],
                           data_t[[numer_col]], data_t[[denom_col]],
                           type = type, bias_correction = bias_correction,
                           alternative = alternative, conf.level = conf.level)
  result$data.name <- data_name
  result$info <- cbind(group = group_names, result$info)
  names(result$info)[1] <- group_col
  result
}

extract_all_literals <- function(call) {
  if (rlang::is_symbol(call)) {
    as.character(call)
  } else if (rlang::is_call(call)) {
    unique(unlist(lapply(rlang::call_args(call), extract_all_literals)))
  } else {
    NULL
  }
}

#' @importFrom glue glue
split_control_treatment <- function(df, group_col, group_names, quiet) {
  df_split <- split(df, df[[group_col]])
  if (length(group_names) == 1L && group_names == "auto") {
    group_names <- sort(names(df_split))
    if (!quiet) {
      message(glue("control: {group_names[1]}, treatment: {group_names[2]}"))
    }
  }
  df_split[group_names]
}

#' @importFrom stats pnorm
deltatest_impl <- function(numer_c, denom_c, numer_t, denom_t,
                           type, bias_correction, alternative, conf.level) {
  # check arguments
  stopifnot(length(numer_c) == length(denom_c))
  stopifnot(length(numer_t) == length(denom_t))

  delta_method_c <- DeltaMethodForRatio$new(numer_c, denom_c, bias_correction)
  delta_method_t <- DeltaMethodForRatio$new(numer_t, denom_t, bias_correction)

  mean_c <- delta_method_c$get_expected_value()
  mean_t <- delta_method_t$get_expected_value()

  squared_SE_c <- delta_method_c$get_squared_standard_error()
  squared_SE_t <- delta_method_t$get_squared_standard_error()

  delta_method <- DeltaMethodForRatio$public_methods
  if (type == "difference") {
    squared_SE_of_diff <- squared_SE_c + squared_SE_t
    standard_error <- sqrt(squared_SE_of_diff)

    diff <- mean_t - mean_c

    z_score <- c("Z" = diff / standard_error)

    confidence_interval <- delta_method$compute_confidence_interval(
      diff, standard_error, alternative, conf.level)

    estimate <- c("mean in control" = mean_c, "mean in treatment" = mean_t,
                  "difference" = diff)
    null_value <- c("difference in means between control and treatment" = 0)
  } else {  # relative change
    squared_SE_of_relative_change <- delta_method$compute_variance(
      mean_t, mean_c, squared_SE_t, squared_SE_c)
    standard_error <- sqrt(squared_SE_of_relative_change)

    relative_change <- delta_method$compute_expected_value(
      mean_t, mean_c, squared_SE_c, cov = 0, bias_correction = bias_correction)

    z_score <- c("Z" = (relative_change - 1) / standard_error)

    confidence_interval <- delta_method$compute_confidence_interval(
      relative_change, standard_error, alternative, conf.level)

    estimate <- c("mean in control" = mean_c, "mean in treatment" = mean_t,
                  "relative change" = relative_change)
    null_value <- c("relative change in means between control and treatment" = 0)
  }

  p_value <- unname(2 * pnorm(-abs(z_score)))

  info <- rbind(
    delta_method_c$get_info(alternative = alternative, conf_level = conf.level),
    delta_method_t$get_info(alternative = alternative, conf_level = conf.level)
  )

  result <- list(statistic = z_score, p.value = p_value, conf.int = confidence_interval,
                 estimate = estimate, null.value = null_value,
                 stderr = standard_error, alternative = alternative,
                 method = "Two Sample Z-test Using the Delta Method",
                 info = info)
  class(result) <- c("deltatest", "htest")
  result
}
