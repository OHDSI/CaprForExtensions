#' Render and Translate Extension Cohort SQL
#'
#' @description
#' Helper function to render template parameters and translate SQL to target dialect.
#' This is only needed when executing SQL directly outside of Strategus/CohortGenerator.
#'
#' @param sql Character. Templated SQL from buildExtendedCohortQuery()
#' @param cdm_database_schema Character. CDM schema name (e.g., "omopwave")
#' @param work_database_schema Character. Work schema for temp tables (e.g., "omopwave")
#' @param target_database_schema Character. Schema for cohort table (e.g., "omopwave")
#' @param target_cohort_table Character. Cohort table name (default: "cohort")
#' @param vocabulary_database_schema Character. Vocabulary schema (default: same as cdm_database_schema)
#' @param target_dialect Character. Target SQL dialect (default: "postgresql")
#'
#' @return Character. Rendered and translated SQL ready for execution
#'
#' @examples
#' \dontrun{
#' # Generate templated SQL
#' sql <- buildExtendedCohortQuery(json, options)
#'
#' # Render and translate for PostgreSQL
#' executable_sql <- renderExtensionSql(
#'   sql,
#'   cdm_database_schema = "omopwave",
#'   work_database_schema = "omopwave",
#'   target_database_schema = "omopwave",
#'   target_dialect = "postgresql"
#' )
#'
#' # Execute
#' DatabaseConnector::executeSql(connection, executable_sql)
#' }
#'
#' @export
renderExtensionSql <- function(
  sql,
  cdm_database_schema,
  work_database_schema = cdm_database_schema,
  target_database_schema = work_database_schema,
  target_cohort_table = "cohort",
  vocabulary_database_schema = cdm_database_schema,
  target_dialect = "postgresql"
) {
  if (!requireNamespace("SqlRender", quietly = TRUE)) {
    stop("SqlRender package is required. Install with: install.packages('SqlRender')")
  }

  # Step 1: Render template parameters
  # CirceR uses these parameter names in the generated SQL:
  rendered_sql <- SqlRender::render(
    sql,
    cdm_database_schema = cdm_database_schema,
    work_database_schema = work_database_schema,
    target_database_schema = target_database_schema,
    target_cohort_table = target_cohort_table,
    vocabulary_database_schema = vocabulary_database_schema,
    # Also support alternative naming patterns
    cdmDatabaseSchema = cdm_database_schema,
    workDatabaseSchema = work_database_schema,
    targetDatabaseSchema = target_database_schema,
    targetCohortTable = target_cohort_table,
    vocabularyDatabaseSchema = vocabulary_database_schema
  )

  # Step 2: Translate to target dialect
  translated_sql <- SqlRender::translate(
    rendered_sql,
    targetDialect = target_dialect
  )

  return(translated_sql)
}

#' Check for Unrendered Parameters in SQL
#'
#' @description
#' Utility function to check if SQL still has template parameters that need rendering.
#'
#' @param sql Character. SQL to check
#' @return Character vector. List of unrendered parameters (empty if none found)
#'
#' @examples
#' \dontrun{
#' params <- checkUnrenderedParameters(sql)
#' if (length(params) > 0) {
#'   message("Found unrendered parameters: ", paste(params, collapse = ", "))
#' }
#' }
#'
#' @export
checkUnrenderedParameters <- function(sql) {
  # Find all @parameter patterns
  matches <- regmatches(sql, gregexpr("@[a-zA-Z_][a-zA-Z0-9_]*", sql))
  unique(unlist(matches))
}
