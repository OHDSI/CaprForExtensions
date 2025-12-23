#' CaprForExtensions: Waveform Extension Table Support for Capr
#'
#' @description
#' CaprForExtensions extends the Capr package to support cohort definitions that
#' reference OMOP CDM Waveform Extension Tables using a placeholder substitution
#' approach.
#'
#' @details
#' This package provides:
#' \itemize{
#'   \item Custom domain registration for waveform and other extension tables
#'   \item Placeholder substitution: extension queries converted to observation queries
#'   \item Post-processing: CirceR SQL modified to use extension tables
#'   \item All CirceR features work automatically (temporal logic, inclusion rules, attrition)
#'   \item ExtensionQuery class for explicit extension table queries
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{registerCustomDomain}}: Register extension table as custom domain
#'   \item \code{\link{listCustomDomains}}: List registered custom domains
#'   \item \code{\link{extensionQuery}}: Create explicit extension table query
#'   \item \code{\link{compileExtendedCohort}}: Compile to JSON with placeholder conversion
#'   \item \code{\link{buildExtendedCohortQuery}}: Generate SQL with placeholder substitution
#'   \item \code{\link{buildExtensionOptions}}: Create CirceR options for SQL generation
#' }
#'
#' @section Extension Table Requirements:
#' Extension tables must include:
#' \itemize{
#'   \item Person linkage field (e.g., person_id)
#'   \item Date/datetime field for temporal criteria
#'   \item Optional: visit_occurrence_id for visit-level joins
#'   \item Optional: concept_id field for standardized coding
#' }
#'
#' @docType package
#' @name CaprForExtensions
#' @import methods
#' @importFrom Capr compile cohort entry
#' @importFrom CirceR buildCohortQuery createGenerateOptions
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom glue glue
#' @importFrom dplyr %>%
#' @importFrom checkmate assertClass assertCharacter assertList
NULL

# Package environment for storing custom domain registry
.pkgenv <- new.env(parent = emptyenv())
.pkgenv$customDomains <- list()

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Initialize custom domain registry
  .pkgenv$customDomains <- list()
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "CaprForExtensions v", utils::packageVersion("CaprForExtensions"), "\n",
    "Extension Table support using placeholder substitution"
  )
}
