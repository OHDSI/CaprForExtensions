#' Build Extended Cohort Query
#'
#' @description
#' Generate SQL for cohort definition that includes both CDM and extension table logic.
#' Uses observation query placeholders that CirceR processes normally, then substitutes
#' extension table logic in post-processing.
#'
#' @param extended_cohort_json Character. JSON from compileExtendedCohort().
#' @param options List. CirceR generate options (created by CirceR::createGenerateOptions()).
#'
#' @return Character string containing SQL
#'
#' @examples
#' \dontrun{
#' library(CirceR)
#'
#' # Compile cohort
#' json_str <- compileExtendedCohort(my_cohort)
#'
#' # Create options
#' options <- createGenerateOptions(
#'   cohortId = 1,
#'   generateStats = TRUE
#' )
#'
#' # Generate SQL
#' sql <- buildExtendedCohortQuery(json_str, options)
#' cat(sql)
#' }
#'
#' @export
buildExtendedCohortQuery <- function(
  extended_cohort_json,
  options
) {
  # Check if extension metadata present
  ext_metadata <- extractExtensionMetadata(extended_cohort_json)

  if (is.null(ext_metadata) || is.null(ext_metadata$extensionQueries) ||
      length(ext_metadata$extensionQueries) == 0) {
    # No extensions - use standard CirceR
    message("No extension queries found - using standard CirceR SQL generation")
    return(CirceR::buildCohortQuery(extended_cohort_json, options))
  }

  message("Building extended cohort query with ", length(ext_metadata$extensionQueries),
          " extension criteria...")

  # Enrich extension metadata with the codeset_id directly from the JSON's ConceptSets.
  # This is the canonical mapping because CirceR uses ConceptSets[i]$id as its codeset_id.
  # Without this, post-processing must scan the SQL for the placeholder concept ID, which
  # fails after an Atlas round-trip where real concept IDs replace the placeholders.
  parsed_json <- jsonlite::fromJSON(extended_cohort_json, simplifyVector = FALSE)
  ext_metadata$extensionQueries <- lapply(ext_metadata$extensionQueries, function(q) {
    expected_name <- paste0("Extension:", q$name)
    for (cs in parsed_json$ConceptSets) {
      if (!is.null(cs$name) && cs$name == expected_name) {
        q$codeset_id <- cs$id   # inject the confirmed codeset_id
        break
      }
    }
    q
  })

  # Generate SQL using standard CirceR (treats placeholders as observation queries)
  message("  Generating base SQL via CirceR...")
  circe_sql <- CirceR::buildCohortQuery(extended_cohort_json, options)

  # Post-process to substitute extension tables
  message("  Post-processing: substituting extension table logic...")
  final_sql <- postProcessExtensionSql(
    circe_sql,
    ext_metadata$extensionQueries,
    schema = options$cdmSchema %||% "@cdm_database_schema"
  )

  # Validate post-processing
  is_valid <- validatePostProcessedSql(final_sql)

  if (!is_valid) {
    warning("Post-processing validation failed. SQL may be incomplete.")
  } else {
    message("  Validation: SUCCESS - all placeholders substituted")
  }

  message("Extended cohort query generated successfully")

  return(final_sql)
}

#' Build Extension Options
#'
#' @description
#' Helper to create CirceR options with additional extension-specific settings.
#'
#' @param cohort_id Integer. Cohort definition ID.
#' @param cdm_schema Character. CDM schema name (default: "@cdm_database_schema").
#' @param target_schema Character. Target schema for results (default: "@target_database_schema").
#' @param target_table Character. Target cohort table (default: "@target_cohort_table").
#' @param vocabulary_schema Character. Vocabulary schema (default: same as cdm_schema).
#' @param generate_stats Logical. Generate inclusion rule statistics (default: TRUE).
#' @param ... Additional arguments passed to CirceR::createGenerateOptions().
#'
#' @return Generate options list
#'
#' @export
buildExtensionOptions <- function(
  cohort_id = 1,
  cdm_schema = "@cdm_database_schema",
  target_schema = "@target_database_schema",
  target_table = "@target_cohort_table",
  vocabulary_schema = NULL,
  generate_stats = TRUE,
  ...
) {
  if (is.null(vocabulary_schema)) {
    vocabulary_schema <- cdm_schema
  }

  CirceR::createGenerateOptions(
    cohortIdFieldName = "cohort_definition_id",
    cohortId = cohort_id,
    cdmSchema = cdm_schema,
    targetTable = target_table,
    resultSchema = target_schema,
    vocabularySchema = vocabulary_schema,
    generateStats = generate_stats,
    ...
  )
}

# Helper function for %||% operator
`%||%` <- function(a, b) {
  if (is.null(a) || (is.character(a) && length(a) == 0)) b else a
}
