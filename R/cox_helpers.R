# functions --------------------------------------------------------------------
#' Clean output from coxphf object
#' @param x A `coxphf` object
#' @return tibble reporting coefficient, se, HR (95% CI), and p-value
#' @importFrom purrr reduce
#' @importFrom tibble enframe
#' @importFrom dplyr rename
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#' coxphf_obj <- coxphf::coxphf(...)
#' clean_coxphf(coxphf_obj)
#' }
clean_coxphf <- function(x) {
  purrr::reduce(list(
    tibble::enframe(x$coefficients) |> dplyr::rename(est = value),
    tibble::enframe(x$ci.lower) |> dplyr::rename(or_lo = value),
    tibble::enframe(x$ci.upper) |> dplyr::rename(or_hi = value),
    tibble::enframe(sqrt(diag(x$var))) |> dplyr::rename(se = value),
    tibble::enframe(x$prob) |> dplyr::rename(p = value)
  ), dplyr::full_join, by = "name") |>
    dplyr::mutate(
      or_est = exp(est),
      model_func = "coxphf"
    ) |>
    dplyr::rename(term = name)
}

#' Clean output from coxph object
#' @param x A `coxph` object
#' @return tibble reporting coefficient, se, HR (95% CI), and p-value
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#' coxph_obj <- survival::coxph(...)
#' clean_coxph(coxph_obj)
#' }
clean_coxph <- function(x) {
  base::summary(x)$coefficients |>
    tibble::as_tibble(rownames = "name") |>
    dplyr::select(name, est = coef, or = `exp(coef)`, se = `se(coef)`, p = `Pr(>|z|)`) |>
    dplyr::mutate(or_lo = base::exp(est - stats::qnorm(0.975) * se), or_hi = base::exp(est + stats::qnorm(0.975) * se)) |>
    dplyr::rename(term = name) |>
    dplyr::mutate(model_func = "coxph")
}

#' Clean output from coxph or coxphf object
#' @param x A `coxph` or `coxphf` object
#' @return tibble reporting coefficient, se, HR (95% CI), and p-value
#' @importFrom gpl1 clean_coxphf
#' @importFrom glp1 clean_coxph
#' @export
#' @examples
#' \dontrun{
#' coxph_obj <- survival::coxph(...)
#' coxphf_obj <- coxphf::coxphf(...)
#' clean_coxph_output(coxph_obj)
#' clean_coxph_output(coxphf_obj)
#' }
clean_coxph_output <- function(x) {
  if ("coxphf" %in% base::class(x)) {
    return(clean_coxphf(x))
  } else if ("coxph" %in% base::class(x)) {
    return(clean_coxph(x))
  } else {
    stop("Invalid object class")
  }
}

#' Generate a Surv formula
#' @param time_var  Character name of time variable
#' @param status_var Character name of status variable
#' @param x Character name for exposure of interest variable
#' @param covs  Character vector of covariate variables
#' @param as_formula  Logical; whether to return formula as character (FALSE) or formula (TRUE)
#' @return a character or formula object representing a Surv formula
#' @importFrom stats  as.formula
#' @export
#' @examples
#' \dontrun{
#' time_var <- "time"
#' status_var <- "outcome"
#' exposure <- "glp1"
#' covariates <- c("age", "race")
#' make_cox_formula(
#'   time_var   = time_var,
#'   status_var = status_var,
#'   x          = exposure,
#'   covs       = covariates
#' )
#' }
make_cox_formula <- function(time_var, status_var, x, covs = NULL, as_formula = FALSE) {
  base::paste0(
    "Surv(time = ", time_var, ", event = ", status_var, ") ~ ",
      base::paste0(c(x, covs), collapse = " + ")) |>
    (\(z) if (as_formula) stats::as.formula(z) else z)()
}

#' Helper function to extract variables from models or matching objects
#' @param patterns  Character vector of patterns to explore and return
#' @param character_vector Character vector to check for presence of patterns
#' @return A character vector of patterns present in `character_vector`
#' @export
#' @examples
#' \dontrun{
#' patterns <- c("age", "sex", "race")
#' char_vec <- c("age", "sex", "raceWhite", "raceBlack")
#' matching_patterns(patterns, char_vec)
#' }
matching_patterns <- function(patterns, character_vector) {
  patterns[base::sapply(patterns, \(pattern) base::any(grepl(pattern, character_vector)))]
}

#' Quickly run adjusted and unadjusted matched and unmatched Cox proportional hazards models
#' @param data  data.frame
#' @param time Character string corresponding to time variable
#' @param outcome Character string corresponding to outcome (status) variable
#' @param exposure Character string corresponding to exposure (treatment) variable
#' @param covariates Character vector corresponding to covariates; also serves as predictors in matching model
#' @param firth Logical; indicates whether to fit coxph (FALSE) or Firth bias-corrected coxph (`coxphf::coxphf`; TRUE)
#' @param caliper Caliper width for matching
#' @param match_std_mean_diff_threshold Standardized mean difference threshold; if std_mean_diff is greater than threshold, variable is included as a covariate in disease model in matched cohort
#' @return A list object containing unadjusted unmatched Cox PH model (`unadjusted_unmatched`), unadjusted matched Cox PH model (unadjusted_matched), matching object (`match_obj`), adjusted matched Cox PH model (`adjusted_matched`), and a tibble summarizing coefficients from the models (`coef_sum`)
#' @importFrom survival coxph
#' @importFrom coxphf coxphf
#' @importFrom MatchIt matchit
#' @importFrom MatchIt match.data
#' @importFrom dplyr across
#' @importFrom tidyselect where
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom janitor clean_names
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom glp1 matching_patterns
#' @importFrom glp1 make_cox_formula
#' @importFrom dplyr  bind_rows
#' @importFrom glp1 clean_coxph_output
#' @export
#' @examples
#' \dontrun{
#' patterns <- c("age", "sex", "race")
#' char_vec <- c("age", "sex", "raceWhite", "raceBlack")
#' matching_patterns(patterns, char_vec)
#' }
extract_cox <- function(
    data,
    time,
    outcome,
    exposure,
    covariates = NULL,
    firth      = FALSE,
    caliper    = 0.1,
    match_std_mean_diff_threshold = 0.1
) {
  if (firth) {
    cox_engine <- coxphf::coxphf
  } else {
    cox_engine <- survival::coxph
  }

  # matching
  match_formula <- as.formula(paste0(exposure, " ~ ", paste0(covariates, collapse = " + ")))

  match_obj <- MatchIt::matchit(
    formula  = match_formula,
    data     = data,
    method   = "nearest",
    distance = "glm",
    calpier  = caliper
  )
  matched <- MatchIt::match.data(match_obj) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor), droplevels))

  adj_vars <- summary(match_obj)$sum.matched |>
    tibble::as_tibble(rownames = "term") |>
    janitor::clean_names() |>
    dplyr::filter(abs(std_mean_diff) > match_std_mean_diff_threshold) |>
    dplyr::pull(term) |>
    glp1::matching_patterns(patterns = c(exposure, covariates), character_vector = _)
  not_adj_vars <- covariates[!covariates %in% adj_vars]

  # cox model
  f_un  <- glp1::make_cox_formula(time_var = time, status_var = outcome, x = exposure, covs = NULL, as_formula = TRUE)
  f_adj <- glp1::make_cox_formula(time_var = time, status_var = outcome, x = exposure, covs = adj_vars, as_formula = TRUE)

  unadj_unmatched_cox <- cox_engine(
    formula = f_un,
    data    = data
  )

  unadj_matched_cox <- cox_engine(
    formula = f_un,
    data    = matched
  )

  adj_cox <- cox_engine(
    formula = f_adj,
    data = matched
  )

  coef_sum <- dplyr::bind_rows(
    glp1::clean_coxph_output(unadj_unmatched_cox) |>
      dplyr::mutate(
        model                   = "Unmatched",
        adjusted                = "No",
        adjust_vars             = "None",
        not_adjust_vars         = NA_character_,
        match_vars              = "None",
        std_mean_diff_threshold = NA_real_
      ),
    glp1::clean_coxph_output(unadj_matched_cox) |>
      dplyr::mutate(
        model                   = "Matched",
        adjusted                = "No",
        adjust_vars             = "None",
        not_adjust_vars         = NA_character_,
        match_vars              = paste0(covariates, collapse = ", "),
        std_mean_diff_threshold = NA_real_
      ),
    glp1::clean_coxph_output(adj_cox) |>
      dplyr::mutate(
        model                   = "Matched",
        adjusted                = "Yes",
        adjust_vars             = paste0(adj_vars, collapse = ", "),
        not_adjust_vars         = paste0(not_adj_vars, collapse = ", "),
        match_vars              = paste0(covariates, collapse = ", "),
        std_mean_diff_threshold = match_std_mean_diff_threshold
      )
  )

  list(
    unadjusted_unmatched = unadj_unmatched_cox,
    unadjusted_matched   = unadj_matched_cox,
    match_obj            = match_obj,
    adjusted_matched     = adj_cox,
    coef_sum             = coef_sum
  )

}
