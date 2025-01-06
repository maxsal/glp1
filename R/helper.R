#' Check argument types, length, or dimension
#'
#' Copied from Merck/simtrial R package
#' Accessed source code from https://rdrr.io/cran/simtrial/src/R/check_args.R on December 18, 2024.
#'
#' @param arg An argument to be checked.
#' @param type A character vector of candidate argument type.
#' @param length A numeric value of argument length or `NULL`.
#' @param dim A numeric vector of argument dimension or `NULL`.
#'
#' @details If `type`, `length` or `dim` is `NULL`, the corresponding check will not be executed.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Check if arg is NULL.
#'    \item Extract the type, length and dim information from arg.
#'    \item Compare with target values and report error message if it does not match.
#'  }
#'  }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return Check failure detailed error message.
#'
#' @keywords internal
#'
#' @export
#' @examples
#' \dontrun{
#' tbl <- as.data.frame(matrix(1:9, nrow = 3))
#' check_args(arg = tbl, type = c("data.frame"))
#'
#' vec <- c("a", "b", "c")
#' check_args(arg = vec, type = c("character"), length = 3)
#' }
check_args <- function(arg, type, length = NULL, dim = NULL) {
  if (is.null(arg)) {
    return(NULL)
  }

  arg <- as.vector(arg)

  check <- list()
  message <- list()

  if (!is.null(type)) {
    check[["type"]] <- any(class(arg) %in% type) & (!is.null(class(arg)))
    message[["type"]] <- paste("The argument type did not match:", paste(type, collapse = "/"))
  }

  if (!is.null(length)) {
    check[["length"]] <- all(length(arg) == length) & (!is.null(length(arg)))
    message[["length"]] <- paste("The argument length is not", length)
  }

  if (!is.null(dim)) {
    check[["dim"]] <- all(dim(arg) == dim) & (!is.null(dim(arg)))
    message[["dim"]] <- paste("The argument dimension is not", paste(dim, collapse = ","))
  }

  check <- unlist(check)
  message <- unlist(message)

  if (!all(unlist(check))) {
    stop(paste(message[!check], collapse = "\n"))
  } else {
    return(NULL)
  }
}

#' Calculate RMST for a single cut-off time point
#'
#' Copied from Merck/simtrial R package
#' Accessed source code from https://rdrr.io/github/Merck/simtrial/src/R/rmst.R on December 12, 2024.
#'
#' @param time_var A numeric vector of follow up time.
#' @param event_var A numeric or integer vector of the status indicator;
#'   0=alive 1=event.
#' @param tau A value of pre-defined cut-off time point.
#' @param group_label A character of customized treatment group name.
#' @param alpha A numeric value of the significant level for RMST
#'   confidence interval. Default is 0.05.
#'
#' @return
#' A data frame of
#' - Cutoff time: same as `tau`;
#' - Group label: same as `group_label`;
#' - Estimated RMST;
#' - Variance, standard error, and CIs of the estimated RMST;
#' - Number of events.
#'
#' @importFrom survival survfit Surv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(ex1_delayed_effect, "simtrial")
#' data_single_arm <- ex1_delayed_effect[ex1_delayed_effect$trt == 1, ]
#' rmst_single_arm(
#'   time_var = data_single_arm$month,
#'   event_var = data_single_arm$evntd,
#'   tau = 10,
#'   group_label = "Treatment 1",
#'   alpha = 0.05
#' )
#' }
rmst_single_arm <- function(
    time_var,
    event_var,
    tau,
    group_label = "Single Group",
    alpha = 0.05) {
  # Input type check
  glp1::check_args(time_var, type = c("integer", "numeric"))
  glp1::check_args(event_var, type = c("integer", "numeric"))
  glp1::check_args(tau, type = c("integer", "numeric"), length = 1)
  glp1::check_args(group_label, type = c("character", "factor"), length = 1)
  glp1::check_args(alpha, type = c("integer", "numeric"))

  # Input value check
  stopifnot(time_var >= 0)
  stopifnot(event_var %in% c(0, 1))
  stopifnot(tau >= 0)
  stopifnot(0 <= alpha & alpha <= 1)

  # Fit a single Kaplan-Meier curve
  fit <- survival::survfit(survival::Surv(time_var, event_var) ~ 1)

  # Extract survival probability, number of event, number at risk,
  # and number of censored along with observed time from the fitted model
  # as a new data frame.
  df <- data.frame(
    time = fit$time,
    n_risk = fit$n.risk,
    n_event = fit$n.event,
    n_censor = fit$n.censor,
    surv = fit$surv,
    stringsAsFactors = FALSE
  )

  # Filter df by survival time less or equal to the pre-specified cut-off time point tau
  df_fit1 <- df[df$time <= tau, ]

  # Add initial value of (time, survival) = (0,1) for calculating time difference
  df_fit2 <- rbind(df_fit1, c(0, NA, NA, NA, 1))

  # Add cut-off time if no records observed on the pre-specified time point
  if (max(df_fit1$time) != tau) df_fit2 <- rbind(df_fit2, c(tau, NA, NA, NA, NA))

  # Sort the data frame by time
  df_fit2 <- df_fit2[order(df_fit2$time), ]
  n_event <- df_fit2$n_event
  n_risk <- df_fit2$n_risk

  # Calculate the time difference and set the last value as NA
  time_diff <- c(diff((sort(df_fit2$time))), NA)

  # Calculate the area under the curve per time interval
  area <- time_diff * df_fit2$surv

  # Calculate the inverse cumulated area under the curve per observed time point A_i
  big_a <- rev(c(0, cumsum(rev(area)[-1])))

  # Calculation of dev refers to di / Yi * (Yi - di)
  dev <- (n_event / (n_risk * (n_risk - n_event))) * (big_a^2)

  # Based on the calculation, create a data frame with below items:
  # cutoff_time is the input of pre-defined cut-off time point
  cutoff_time <- tau
  # group is the input group name
  group <- group_label
  # rmst is the estimated RMST
  rmst <- sum(area, na.rm = TRUE)
  # std is the standard error of the estimated RMST
  variance <- sum(dev, na.rm = TRUE) * sum(n_event, na.rm = TRUE) / (sum(n_event, na.rm = TRUE) - 1)
  std <- sqrt(variance)
  # lcl and ucl are lower/upper control limit of CIs for RMST
  lcl <- rmst - stats::qnorm(1 - alpha / 2) * std
  ucl <- rmst + stats::qnorm(1 - alpha / 2) * std
  event <- sum(n_event, na.rm = TRUE)

  ans <- data.frame(
    cutoff_time, group, rmst, variance, std, lcl, ucl, event,
    stringsAsFactors = FALSE
  )

  return(ans)
}

#' Generate restricted mean survival time plot
#' @param x dataset with treatment and event indicators and a time-to-event variable
#' @param trt_var character name of the treatment indicator variable
#' @param tte_var character name of the time-to-event variable
#' @param event_var character name of the event indicator variable
#' @param ref reference group of the treatment indicator variable
#' @param .tau time point at which to assess restricted mean survival time
#' @return tibble reporting coefficient, se, HR (95% CI), and p-value
#' @import ggplot2
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' # example forthcoming
#' rmst_plot(data)
#' }

rmst_plot <- function(x, trt_var = "glp1", tte_var = "time", event_var = "outcome", ref = "0", .tau = 365*5) {
  unexposed <- glp1::rmst_single_arm(
    time_var = x |>
      (\(y) y[y[[trt_var]] == 0, ])() |>
      (\(y) y[[tte_var]])(),
    event_var = x |>
      (\(y) y[y[[trt_var]] == 0, ])() |>
      (\(y) y[[event_var]])(),
    tau = .tau
  )
  exposed <- glp1::rmst_single_arm(
    time_var = x |>
      (\(y) y[y[[trt_var]] == 1, ])() |>
      (\(y) y[[tte_var]])(),
    event_var = x |>
      (\(y) y[y[[trt_var]] == 1, ])() |>
      (\(y) y[[event_var]])(),
    tau = .tau
  )
  data.table::rbindlist(list(
    data.table::as.data.table(unexposed)[, .(rmst, lo = lcl, hi = ucl, event, exposure = "Unexposed")],
    data.table::as.data.table(exposed)[, .(rmst, lo = lcl, hi = ucl, event, exposure = "Exposed")]
  )) |>
    ggplot2::ggplot(ggplot2::aes(x = exposure, y = rmst)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lo, ymax = hi)) +
    ggplot2::labs(
      x     = "Exposure status",
      y     = "Restricted mean survival time",
      title = "RMST by exposure status"
    ) +
    ggplot2::ylim(0, NA) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    )
}

#' Generate plot of difference in restricted mean survival time between groups across timepoints
#' @param matched_data dataset with treatment and event indicators and a time-to-event variable
#' @param trt_var character name of the treatment indicator variable
#' @param time_var character name of the time-to-event variable, assumes days
#' @param outcome_var character name of the outcome (event) indicator variable
#' @param .taus vector of time points at which to assess restricted mean survival time, assumes years
#' @return tibble reporting coefficient, se, HR (95% CI), and p-value
#' @import ggplot2
#' @import data.table
#' @importFrom survRM2 rmst2
#' @export
#' @examples
#' \dontrun{
#' # example forthcoming
#' rmst_diff_plot(data)
#' }
rmst_diff_plot <- function(
    matched_data,
    time_var    = "time",
    outcome_var = "outcome",
    trt_var     = "glp1",
    .taus        = 1:5
) {

  tmp <- data.table::rbindlist(
    lapply(
      .taus,
      function(i) {
        res <- survRM2::rmst2(
          matched_data[[time_var]],
          matched_data[[outcome_var]],
          matched_data[[trt_var]],
          tau = i * 365
        )$unadjusted.result |>
          data.table::as.data.table()

        res[res[[1]] == "RMST (arm=1)-(arm=0)", ][
          , time := i
        ][]
      }
    )
  )

  return(
    list(
      data = tmp,
      plot = tmp |>
        ggplot2::ggplot(ggplot2::aes(x = time, y = `Est.`)) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_line() +
        ggplot2::geom_pointrange(aes(ymin = `lower .95`, ymax = `upper .95`)) +
        ggplot2::scale_x_continuous(breaks = 1:10) +
        ggplot2::labs(
          x = "Tau (years)",
          y = "RMST difference, days (1 - 0)") +
        ggplot2::theme_minimal()
    )
  )

}
####
