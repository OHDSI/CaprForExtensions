#' ExtensionQuery Class
#'
#' @description
#' S4 class representing a query against a database table outside the OMOP CDM.
#' ExtensionQuery objects can be used alongside standard Capr Query objects in
#' cohort definitions.
#'
#' @slot name Character. Name/identifier for this extension query.
#' @slot tableName Character. Name of the extension table.
#' @slot tableSchema Character. Database schema containing the table (default: "@cdm_database_schema").
#' @slot personIdField Character. Name of person ID field for joining (default: "person_id").
#' @slot visitIdField Character. Name of visit_occurrence_id field for visit-level joining (optional).
#' @slot dateField Character. Name of date/datetime field for temporal criteria.
#' @slot selectFields Character vector. Additional fields to select (optional).
#' @slot filters List. Filter conditions as character strings.
#' @slot attributes List. Additional attributes/modifiers (for future extensibility).
#' @slot temporalConstraint Character. Temporal constraint relative to cohort dates (DEPRECATED - use temporalRelationship).
#' @slot temporalRelationship Character. Temporal relationship: "DURING", "BEFORE", "AFTER", "WITHIN_DAYS" (optional).
#'
#' @examples
#' \dontrun{
#' # Create extension query for patient surveys
#' survey_ext <- extensionQuery(
#'   name = "high_qol_surveys",
#'   table = "patient_surveys",
#'   person_id_field = "person_id",
#'   date_field = "survey_date",
#'   filters = list(
#'     "survey_type = 'DIABETES_QOL'",
#'     "total_score >= 70"
#'   )
#' )
#' }
#'
#' @export
setClass("ExtensionQuery",
         slots = c(
           name = "character",
           tableName = "character",
           tableSchema = "character",
           personIdField = "character",
           visitIdField = "character",
           dateField = "character",
           selectFields = "character",
           filters = "list",
           attributes = "list",
           temporalConstraint = "character",
           temporalRelationship = "character"
         ),
         prototype = list(
           name = character(0),
           tableName = character(0),
           tableSchema = "@cdm_database_schema",
           personIdField = "person_id",
           visitIdField = character(0),
           dateField = character(0),
           selectFields = character(0),
           filters = list(),
           attributes = list(),
           temporalConstraint = character(0),
           temporalRelationship = "DURING"
         ))

#' Validate ExtensionQuery
#' @keywords internal
setValidity("ExtensionQuery", function(object) {
  errors <- character()

  # Check required fields
  if (length(object@name) == 0 || nchar(object@name) == 0) {
    errors <- c(errors, "name must be provided")
  }

  if (length(object@tableName) == 0 || nchar(object@tableName) == 0) {
    errors <- c(errors, "tableName must be provided")
  }

  if (length(object@dateField) == 0 || nchar(object@dateField) == 0) {
    errors <- c(errors, "dateField must be provided")
  }

  # Check field names are valid SQL identifiers
  if (length(object@personIdField) > 0 && !grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", object@personIdField)) {
    errors <- c(errors, "personIdField must be a valid SQL identifier")
  }

  if (length(errors) == 0) TRUE else errors
})

#' Create Extension Query
#'
#' @description
#' Constructor function for ExtensionQuery objects. Defines a query against
#' a database table outside the OMOP CDM.
#'
#' @param name Character. Name/identifier for this extension query.
#' @param table Character. Name of the extension table.
#' @param schema Character. Database schema (default: "@cdm_database_schema").
#' @param person_id_field Character. Person ID field name (default: "person_id").
#' @param date_field Character. Date field name for temporal criteria.
#' @param select_fields Character vector. Additional fields to select (optional).
#' @param filters List of character. SQL filter conditions (optional).
#' @param temporal_constraint Character. Temporal constraint SQL (optional).
#' @param ... Additional attributes (for future extensibility).
#'
#' @return An ExtensionQuery object
#'
#' @examples
#' \dontrun{
#' # Patient survey data
#' survey_query <- extensionQuery(
#'   name = "diabetes_qol",
#'   table = "patient_surveys",
#'   date_field = "survey_date",
#'   filters = list(
#'     "survey_type = 'DIABETES_QOL'",
#'     "total_score >= 70"
#'   ),
#'   temporal_constraint = "survey_date BETWEEN cohort_start_date AND cohort_end_date"
#' )
#'
#' # Custom lab results
#' lab_query <- extensionQuery(
#'   name = "custom_hba1c",
#'   table = "institution_lab_results",
#'   date_field = "lab_date",
#'   select_fields = c("lab_value", "lab_unit"),
#'   filters = list(
#'     "lab_test_code = 'HBA1C_CUSTOM'",
#'     "lab_value < 7.0"
#'   )
#' )
#' }
#'
#' @export
extensionQuery <- function(
  name,
  table,
  schema = "@cdm_database_schema",
  person_id_field = "person_id",
  visit_id_field = character(0),
  date_field,
  select_fields = character(0),
  filters = list(),
  temporal_constraint = character(0),
  temporal_relationship = "DURING",
  ...
) {
  # Validate inputs
  checkmate::assertCharacter(name, len = 1, min.chars = 1)
  checkmate::assertCharacter(table, len = 1, min.chars = 1)
  checkmate::assertCharacter(schema, len = 1, min.chars = 1)
  checkmate::assertCharacter(person_id_field, len = 1, min.chars = 1)
  checkmate::assertCharacter(visit_id_field, null.ok = TRUE)
  checkmate::assertCharacter(date_field, len = 1, min.chars = 1)
  checkmate::assertCharacter(select_fields, null.ok = TRUE)
  checkmate::assertList(filters)
  checkmate::assertChoice(temporal_relationship,
                          choices = c("DURING", "BEFORE", "AFTER", "WITHIN_DAYS", "ANY"),
                          null.ok = TRUE)

  # Create object
  new("ExtensionQuery",
      name = name,
      tableName = table,
      tableSchema = schema,
      personIdField = person_id_field,
      visitIdField = if(length(visit_id_field) > 0) visit_id_field else character(0),
      dateField = date_field,
      selectFields = select_fields,
      filters = filters,
      attributes = list(...),
      temporalConstraint = temporal_constraint,
      temporalRelationship = temporal_relationship)
}

#' Convert ExtensionQuery to List
#'
#' @description
#' Convert ExtensionQuery to list representation for JSON serialization.
#'
#' @param x An ExtensionQuery object
#' @return A list
#' @keywords internal
setMethod("as.list", "ExtensionQuery", function(x) {
  result <- list(
    Name = x@name,
    TableName = x@tableName,
    TableSchema = x@tableSchema,
    PersonIdField = x@personIdField,
    DateField = x@dateField
  )

  if (length(x@selectFields) > 0) {
    result$SelectFields <- as.list(x@selectFields)
  }

  if (length(x@filters) > 0) {
    result$Filters <- x@filters
  }

  if (length(x@temporalConstraint) > 0 && nchar(x@temporalConstraint) > 0) {
    result$TemporalConstraint <- x@temporalConstraint
  }

  if (length(x@attributes) > 0) {
    result$Attributes <- x@attributes
  }

  return(result)
})

#' Show Method for ExtensionQuery
#'
#' @param object An ExtensionQuery object
#' @keywords internal
setMethod("show", "ExtensionQuery", function(object) {
  cat("ExtensionQuery:", object@name, "\n")
  cat("  Table:", object@tableSchema, ".", object@tableName, "\n", sep = "")
  cat("  Person ID Field:", object@personIdField, "\n")
  cat("  Date Field:", object@dateField, "\n")

  if (length(object@selectFields) > 0) {
    cat("  Select Fields:", paste(object@selectFields, collapse = ", "), "\n")
  }

  if (length(object@filters) > 0) {
    cat("  Filters:\n")
    for (filter in object@filters) {
      cat("    -", filter, "\n")
    }
  }

  if (length(object@temporalConstraint) > 0 && nchar(object@temporalConstraint) > 0) {
    cat("  Temporal Constraint:", object@temporalConstraint, "\n")
  }

  if (length(object@temporalRelationship) > 0 && nchar(object@temporalRelationship) > 0) {
    cat("  Temporal Relationship:", object@temporalRelationship, "\n")
  }
})
