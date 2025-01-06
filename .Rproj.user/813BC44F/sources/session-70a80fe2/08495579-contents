#' ICD9CM code description
#'
#' A table listing ICD-9-CM diagnosis and procedure codes and their corresponding descriptions.
#' Last manually curated on August 18, 2023.
#'
#' @format ## `icd9cm`
#' A data frame with 17,564 rows and 4 columns:
#' \describe{
#'   \item{code}{ICD-9-CM diagnosis code}
#'   \item{description}{Description of code}
#'   \item{vocabulary_id}{Incidates that the code is from the ICD-9-CM vocabulary}
#'   ...
#' }
#' @source ICD-9-CM from Athena ODHSI downloaded from https://athena.ohdsi.org/ on August 9, 2023.
"icd9cm"

#' ICD10 code description
#'
#' A table listing ICD-10 codes and their corresponding descriptions.
#' Last manually curated on August 18, 2023.
#'
#' @format ## `icd10`
#' A data frame with 16,168 rows and 3 columns:
#' \describe{
#'   \item{code}{ICD-10code}
#'   \item{description}{Description of code}
#'   \item{vocabulary_id}{Incidates that the code is from the ICD-10 vocabulary}
#'   ...
#' }
#' @source ICD-10 from Athena ODHSI downloaded from https://athena.ohdsi.org/ on August 9, 2023.
"icd10"

#' ICD10CM code description
#'
#' A table listing ICD-10-CM codes and their corresponding descriptions.
#' Last manually curated on August 18, 2023.
#'
#' @format ## `icd10cm`
#' A data frame with 98,583 rows and 3 columns:
#' \describe{
#'   \item{code}{ICD-10-CM code}
#'   \item{description}{Description of code}
#'   \item{vocabulary_id}{Incidates that the code is from the ICD-10-CM vocabulary}
#'   ...
#' }
#' @source ICD-10-CM from Athena ODHSI downloaded from https://athena.ohdsi.org/ on August 9, 2023.
"icd10cm"

#' PheWAS catalog information
#'
#' A table listing PheWAS X Codes (or phecodes) and their corresponding
#' description, category, and other information.
#' Last manually curated on June 1, 2023.
#'
#' @format ## `pheinfox`
#' A data frame with 3,612 rows and 8 columns:
#' \describe{
#'   \item{phecode}{PheWAS Code (aka phecode)}
#'   \item{description}{Phecode description}
#'   \item{group}{Phecode disease category or group}
#'   \item{groupnum}{A numeric value corresponding to `phecode` `group`}
#'   \item{order}{A variable ordering by `groupnum` and then numeric `phecode` value, useful for plotting}
#'   \item{color}{Phecode group color inspired by Okabe-Ito colorblind-friendly palette}
#'   \item{color_original}{Original `phecode` `group` color scheme}
#'   \item{sex}{Defines `male` or `female` specific phecodes, otherwise `both`}
#'   ...
#' }
#' @source Curated version of files available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 6 September 2023
"pheinfox"

#' PhenomeX label information
#'
#' An unedited table of PheWAS X Codes (or phecodes) and their corresponding
#' description, category, and other information.
#' Downloaded from github.com/PheWAS/phecodex/ on 2023-09-21.
#'
#' @format ## `phecodex_labels`
#' A data frame with 3,612 rows and 6 columns:
#' \describe{
#'   \item{phenotype}{The phecode label (two letters, “_”, and numeric phecode)}
#'   \item{description}{A descriptive label for phecode}
#'   \item{icd10_only}{A Boolean value: 1 if the phecode is defined only by ICD-10 codes; 0 if the phecode is defined by both ICD-9 and -10 codes}
#'   \item{groupnum}{A numeric value corresponding to the phecode category}
#'   \item{group}{A string indicating the phecode category}
#'   \item{color}{A string value indicating the color to use in plots for each group}
#'   ...
#' }
#' @source Unedited, raw version of phecodeX_R_labels.csv available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 2023-09-21.
"phecodex_labels"

#' PhenomeX phenome structure
#'
#' An unedited table of PheWAS X Codes (or phecodes) and their "roll up" structure.
#' Downloaded from github.com/PheWAS/phecodex/ on 2023-09-21.
#'
#' @format ## `phecodex_rollup`
#' A data frame with 7,956 rows and 2 columns:
#' \describe{
#'   \item{code}{Primary phecode label}
#'   \item{phecode_unrolled}{A phecode that is implied by the primary phecode label}
#'   ...
#' }
#' @source Unedited, raw version of phecodeX_R_rollup_map.csv available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 2023-09-21.
"phecodex_rollup"

#' PhenomeX ICD10 (WHO) mapping
#'
#' An unedited table of PheWAS X Codes (or phecodes) and their ICD10 mappings.
#' Downloaded from https://phewascatalog.org/phecode_x on 2023-09-21.
#'
#' @format ## `phecodex_icd_map`
#' A data frame with 11,403 rows and 7 columns:
#' \describe{
#'   \item{icd}{The code included in the phecode grouping (current supported code types are ICD-10)}
#'   \item{vocabulary_id}{A string indicating the code type (ICD10)}
#'   \item{ICD_string}{A descriptive label for ICD code}
#'   \item{phecode}{The phecode label}
#'   \item{phecode_string}{A descriptive label for phecode}
#'   \item{category_num}{A numeric value corresponding to the phecode category}
#'   \item{category}{A string indicating the phecode category}
#'   ...
#' }
#' @source Unedited, raw version of phecodeX_ICD_WHO_map_flat.csv available via the PheWAS Catalog (https://phewascatalog.org/phecode_x). 2023-09-21.
"phecodex_icd_map"

#' PhenomeX ICD9/10CM mapping
#'
#' An unedited table of PheWAS X Codes (or phecodes) and their ICD9/10CM mappings.
#' Downloaded from github.com/PheWAS/phecodex/ on 2023-09-21.
#'
#' @format ## `phecodex_icdcm_map`
#' A data frame with 79,583 rows and 3 columns:
#' \describe{
#'   \item{code}{The code included in the phecode grouping (current supported code types are ICD-9-CM and ICD-10-CM)}
#'   \item{vocabulary_id}{A string indicating the code type (ICD9CM or ICD10CM)}
#'   \item{phecode}{The phecode label}
#'   ...
#' }
#' @source Unedited, raw version of phecodeX_R_map.csv available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 2023-09-21.
"phecodex_icdcm_map"

#' PhenomeX phecode-sex mappings
#'
#' An unedited table of PheWAS X Codes (or phecodes) and whether they are sex-specific.
#' Downloaded from github.com/PheWAS/phecodex/ on 2023-09-21.
#'
#' @format ## `phecodex_sex`
#' A data frame with 3,612 rows and 3 columns:
#' \describe{
#'   \item{phecode}{The phecode label}
#'   \item{male_only}{A true/false indicator of whether the specific code is used more than 90\% of the time with EHR-reported male sex}
#'   \item{female_only}{A true/false indicator of whether the specific code is used more than 90\% of the time with EHR-reported female sex}
#'  ...
#' }
#' @source Unedited, raw version of phecodeX_R_sex.csv available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 2023-09-21.
"phecodex_sex"
