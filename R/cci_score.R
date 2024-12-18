#' Calculate Quan-weighted CCI score for mixed ICD9/ICD10 codes
#' @param icd_data  A data.frame with id, code, and vocabulary_id variables
#' @return data.frame with id, CCI constiuent phenotype indicators, and CCI score
#' @importFrom dplyr filter
#' @importFrom comorbidity comorbidity
#' @importFrom dplyr full_join
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#' cci_score(icd_data)
#' }
cci_score <- function(icd_data) {
  # subset
  icd10_data <- icd_data |>
    dplyr::filter(tolower(vocabulary_id) %in% c("icd10", "icd10cm"))
  icd9_data  <- icd_data |>
    dplyr::filter(tolower(vocabulary_id) %in% c("icd9", "icd9cm"))

  # get comorbidities
  results_icd10 <- comorbidity::comorbidity(icd10_data, id = "id", code = "code",
                                            map = "charlson_icd10_quan", assign0 = TRUE)
  results_icd9 <- comorbidity::comorbidity(icd9_data, id = "id", code = "code",
                                           map = "charlson_icd9_quan", assign0 = TRUE)

  # deduplicate
  comorbid_names              <- names(results_icd10)[names(results_icd10) != "id"]
  names(results_icd10)        <- c("id", paste0(comorbid_names, "10"))
  names(results_icd9)         <- c("id", paste0(comorbid_names, "9"))
  res_merge                   <- dplyr::full_join(results_icd10, results_icd9, by = "id")
  res_merge[is.na(res_merge)] <- 0
  for (cn in comorbid_names) {
    res_merge[[cn]] <- as.numeric((res_merge[[paste0(cn, "10")]] + res_merge[[paste0(cn, "9")]]) > 0)
  }

  res_merge <- res_merge |>
    dplyr::select(tidyselect::all_of(c("id", comorbid_names)))
  class(res_merge) <- c("comorbidity", "data.frame")
  attr(res_merge, "map") <- "charlson_icd10_quan"

  cci_score <- comorbidity::score(res_merge, weights = "quan", assign0 = TRUE)

  res_merge |>
    dplyr::mutate(
      cci_score = cci_score
    )

}
