#' Calculate Quan-weighted CCI score for mixed ICD9/ICD10 codes
#' @param icd_data  A data.frame with id, code, and vocabulary_id variables
#' @return data.frame with id, CCI constiuent phenotype indicators, and CCI score
#' @importFrom comorbidity comorbidity
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' cci_score(icd_data)
#' }
cci_score <- function(icd_data) {
  # check
  if (!data.table::is.data.table(icd_data)) {
    message("coercing icd_data to data.table object")
    icd_data <- data.table::as.data.table(icd_data)
  }

  # subset
  icd10_data <- icd_data[tolower(icd_data[["vocabulary_id"]]) %chin% c("icd10", "icd10cm"), ]
  icd9_data  <- icd_data[tolower(icd_data[["vocabulary_id"]]) %chin% c("icd9", "icd9cm"), ]

  # get comorbidities
  results_icd10 <- comorbidity::comorbidity(icd10_data, id = "id", code = "code",
                                            map = "charlson_icd10_quan", assign0 = TRUE) |>
    data.table::as.data.table()
  results_icd9 <- comorbidity::comorbidity(icd9_data, id = "id", code = "code",
                                           map = "charlson_icd9_quan", assign0 = TRUE) |>
    data.table::as.data.table()

  # deduplicate
  comorbid_names              <- names(results_icd10)[names(results_icd10) != "id"]
  names(results_icd10)        <- c("id", paste0(comorbid_names, "10"))
  names(results_icd9)         <- c("id", paste0(comorbid_names, "9"))

  res_merge <- data.table::merge.data.table(results_icd10, results_icd9, by = "id", all = TRUE)
  res_merge[is.na(res_merge)] <- 0
  for (cn in comorbid_names) {
    res_merge[[cn]] <- as.numeric((res_merge[[paste0(cn, "10")]] + res_merge[[paste0(cn, "9")]]) > 0)
  }

  select_vars <- c("id", comorbid_names)
  res_merge <- res_merge[, select_vars, with = FALSE]
  class(res_merge) <- c("comorbidity", "data.frame")
  attr(res_merge, "map") <- "charlson_icd10_quan"

  cci_score <- comorbidity::score(res_merge, weights = "quan", assign0 = TRUE)

  res_merge$cci_score <- cci_score

  data.table::as.data.table(res_merge)
}
