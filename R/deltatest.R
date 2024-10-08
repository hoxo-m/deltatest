#' @importFrom stats pnorm qnorm
#'
#' @export
deltatest <- function(numer_c, denom_c, numer_t, denom_t,
                      bucket_names = c("control", "treatment"),
                      type = c("difference", "relative_change"),
                      order_of_Taylor = c("1", "2")) {
  # check arguments
  stopifnot(length(numer_c) == length(denom_c))
  stopifnot(length(numer_t) == length(denom_t))
  type <- match.arg(type)
  order_of_Taylor <- as.character(order_of_Taylor)
  order_of_Taylor <- match.arg(order_of_Taylor)

  n_c <- length(numer_c)
  n_t <- length(numer_t)

  delta_method_c <- DeltaMethodForRatio$new(numer_c, denom_c)
  delta_method_t <- DeltaMethodForRatio$new(numer_t, denom_t)

  mean_c <- delta_method_c$get_expected_value(order_of_Taylor = order_of_Taylor)
  mean_t <- delta_method_t$get_expected_value(order_of_Taylor = order_of_Taylor)

  var_c <- delta_method_c$get_variance()
  var_t <- delta_method_t$get_variance()

  var_of_diff <- var_c / n_c + var_t / n_t
  standard_error <- sqrt(var_of_diff)

  diff <- mean_t - mean_c

  if (type == "difference") {
    z_score <- c("z" = diff / standard_error)

    lower <- diff - qnorm(0.975) * standard_error
    upper <- diff + qnorm(0.975) * standard_error

    estimate <- c("mean in control" = mean_c, "mean in treatment" = mean_t,
                  "difference" = diff)
    null_value <- c("difference in means between control and treatment" = 0)
  } else {  # relative change
    var <- DeltaMethodForRatio$compute_variance(diff, mean_c, var_of_diff, var_c / n_c)
    standard_error <- sqrt(var)

    relative_change <- DeltaMethodForRatio$compute_expected_value(
      diff, mean_c, var_c, cov = 0, order_of_Taylor = order_of_Taylor)
    z_score <- c("z" = relative_change / standard_error)

    lower <- relative_change - qnorm(0.975) * standard_error
    upper <- relative_change + qnorm(0.975) * standard_error

    estimate <- c("mean in control" = mean_c, "mean in treatment" = mean_t,
                  "relative change" = relative_change)
    null_value <- c("relative change in means between control and treatment" = 0)
  }

  p_value <- unname(2 * pnorm(-abs(z_score)))
  conf_int <- c(lower, upper)
  attr(conf_int, "conf.level") <- 0.95
  data_name <- paste0(
    "(", deparse1(substitute(numer_c)), ", ", deparse1(substitute(denom_c)),
    ") and (",
    deparse1(substitute(numer_t)), ", ", deparse1(substitute(denom_t)), ")")

  se <- sqrt(c(var_c / n_c, var_t / n_t))
  df <- tibble(bucket = bucket_names,
               x = c(sum(numer_c), sum(numer_t)), n = c(n_c, n_t),
               mean = c(mean_c, mean_t), lower = mean - se, upper = mean + se)

  result <- list(statistic = z_score, p.value = p_value, conf.int = conf_int,
                 estimate = estimate, null.value = null_value,
                 stderr = standard_error, alternative = "two.sided",
                 method = "Two Sample z-test with Delta Method",
                 data.name = data_name, df = df)
  class(result) <- "htest"
  result
}



#' @importFrom rlang as_label as_string enquo ensym
#'
#' @export
deltatest_df <- function(df, numer_col, denom_col, bucket_col = "bucket",
                         bucket_names = "auto",
                         type = c("difference", "relative_change"),
                         order_of_Taylor = c("1", "2")) {
  data_name <- rlang::enquo(df)|> rlang::as_label()
  numer_col <- rlang::ensym(numer_col) |> rlang::as_string()
  denom_col <- rlang::ensym(denom_col) |> rlang::as_string()
  bucket_col <- rlang::ensym(bucket_col) |> rlang::as_string()
  df_split <- split_control_treatment(df, bucket_col)
  df_c <- df_split[[1]]
  df_t <- df_split[[2]]
  result <- deltatest(df_c[[numer_col]], df_c[[denom_col]],
                      df_t[[numer_col]], df_t[[denom_col]],
                      bucket_names = names(df_split),
                      type = type, order_of_Taylor = order_of_Taylor)
  result$data.name <- data_name
  result
}

#' @importFrom glue glue
split_control_treatment <- function(df, bucket_col) {
  df_split <- split(df, df[[bucket_col]])
  names <- sort(names(df_split))
  message(glue("control: {names[1]}, treatment: {names[2]}"))
  df_split[names]
}
