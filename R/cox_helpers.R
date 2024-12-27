# functions --------------------------------------------------------------------
#' Clean output from coxphf object
#' @param x A `coxphf` object
#' @return data.table reporting coefficient, se, HR (95% CI), and p-value
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' coxphf_obj <- coxphf::coxphf(...)
#' clean_coxphf(coxphf_obj)
#' }
clean_coxphf <- function(x) {
  # Create a single data.table with all required components
  tmp <- data.table::data.table(
    term    = names(x$coefficients),
    est     = x$coefficients,
    or_lo   = x$ci.lower,
    or_hi   = x$ci.upper,
    se      = sqrt(diag(x$var)),
    p       = x$prob
  )

  # Add derived columns
  tmp[, `:=`(
    or_est     = exp(est),
    model_func = "coxphf"
  )]

  return(tmp[])
}

#' Clean output from coxph object
#' @param x A `coxph` object
#' @return data.table reporting coefficient, se, HR (95% CI), and p-value
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' coxph_obj <- survival::coxph(...)
#' clean_coxph(coxph_obj)
#' }
clean_coxph <- function(x) {
  tmp <- data.table::as.data.table(base::summary(x)$coefficients, keep.rownames = "term")
  data.table::setnames(
    tmp,
    old = c("coef", "exp(coef)", "se(coef)", "Pr(>|z|)"),
    new = c("est", "or", "se", "p")
  )
  tmp[, `:=`(
    or_lo = exp(est - stats::qnorm(0.975) * se),
    or_hi = exp(est + stats::qnorm(0.975) * se),
    model_func = "coxph"
  )]
  return(tmp)
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
    return(glp1::clean_coxphf(x))
  } else if ("coxph" %in% base::class(x)) {
    return(glp1::clean_coxph(x))
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
#' @import data.table
#' @importFrom survival coxph
#' @importFrom coxphf coxphf
#' @importFrom MatchIt matchit
#' @importFrom MatchIt match.data
#' @importFrom janitor make_clean_names
#' @importFrom glp1 matching_patterns
#' @importFrom glp1 make_cox_formula
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

  # Matching
  match_formula <- as.formula(paste0(exposure, " ~ ", paste0(covariates, collapse = " + ")))
  match_obj <- MatchIt::matchit(
    formula  = match_formula,
    data     = data,
    method   = "nearest",
    distance = "glm",
    caliper  = caliper
  )

  matched <- data.table::as.data.table(MatchIt::match.data(match_obj))
  matched[, (names(matched)) := lapply(.SD, function(x) if (is.factor(x)) droplevels(x) else x)]

  adj_vars <- data.table::as.data.table(summary(match_obj)$sum.matched, keep.rownames = "term")
  data.table::setnames(adj_vars, old = names(adj_vars), new = janitor::make_clean_names(adj_vars))
  adj_vars <- adj_vars[abs(std_mean_diff) > match_std_mean_diff_threshold, term] |>
    glp1::matching_patterns(patterns = c(exposure, covariates), character_vector = _)

  not_adj_vars <- setdiff(covariates, adj_vars)

  # Cox Model Formulas
  f_un <- glp1::make_cox_formula(
    time_var  = time,
    status_var = outcome,
    x          = exposure,
    covs       = NULL,
    as_formula = TRUE
  )

  f_adj <- glp1::make_cox_formula(
    time_var  = time,
    status_var = outcome,
    x          = exposure,
    covs       = adj_vars,
    as_formula = TRUE
  )

  # Cox Models
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
    data    = matched
  )

  # Coefficient Summary
  coef_sum <- data.table::rbindlist(
    list(
      cbind(
        glp1::clean_coxph_output(unadj_unmatched_cox),
        data.table::data.table(
          model                   = "Unmatched",
          adjusted                = "No",
          adjust_vars             = "None",
          not_adjust_vars         = NA_character_,
          match_vars              = "None",
          std_mean_diff_threshold = NA_real_
        )
      ),
      cbind(
        glp1::clean_coxph_output(unadj_matched_cox),
        data.table::data.table(
          model                   = "Matched",
          adjusted                = "No",
          adjust_vars             = "None",
          not_adjust_vars         = NA_character_,
          match_vars              = paste0(covariates, collapse = ", "),
          std_mean_diff_threshold = NA_real_
        )
      ),
      cbind(
        glp1::clean_coxph_output(adj_cox),
        data.table::data.table(
          model                   = "Matched",
          adjusted                = "Yes",
          adjust_vars             = paste0(adj_vars, collapse = ", "),
          not_adjust_vars         = paste0(not_adj_vars, collapse = ", "),
          match_vars              = paste0(covariates, collapse = ", "),
          std_mean_diff_threshold = match_std_mean_diff_threshold
        )
      )
    )
  )

  # Return Results
  return(list(
    unadjusted_unmatched = unadj_unmatched_cox,
    unadjusted_matched   = unadj_matched_cox,
    match_obj            = match_obj,
    adjusted_matched     = adj_cox,
    coef_sum             = coef_sum
  ))
}
