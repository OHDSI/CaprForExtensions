#' Custom Domain Query Functions
#'
#' @description
#' Functions for building queries using custom domains. Custom domains can be
#' queried similar to standard OMOP domains in Capr.

#' Build Custom Domain Query
#'
#' @description
#' Create a query against a custom domain. This function is used internally
#' by dynamically created domain-specific functions.
#'
#' @param domain_id Character. Custom domain ID.
#' @param concept_set ConceptSet object (optional). If provided, will filter by concept.
#' @param ... Additional query attributes (filters, temporal constraints, etc.).
#'
#' @return CustomDomainQuery object
#'
#' @examples
#' \dontrun{
#' # After registering a custom domain
#' registerCustomDomain(
#'   domain_id = "patientSurvey",
#'   table = "surveys",
#'   start_date_field = "survey_date"
#' )
#'
#' # Use the auto-created function
#' survey_concepts <- cs(123, 456, name = "QOL Surveys")
#' my_query <- patientSurvey(survey_concepts)
#'
#' # Or with filters
#' my_query <- patientSurvey(
#'   survey_concepts,
#'   valueAsNumber = list(field = "total_score", operator = ">=", value = 70)
#' )
#' }
#'
#' @export
customDomainQuery <- function(domain_id, concept_set = NULL, ...) {
  # Get domain definition
  if (!domain_id %in% names(.pkgenv$.customDomainRegistry)) {
    stop("Custom domain '", domain_id, "' not registered. Use registerCustomDomain() first.")
  }

  domain <- getCustomDomain(domain_id)

  # Build query object
  query_obj <- list(
    type = "CustomDomainQuery",
    domainId = domain@domainId,
    domainName = domain@domainName,
    customDomain = domain,
    conceptSet = concept_set,
    attributes = list(...)
  )

  class(query_obj) <- c("CustomDomainQuery", "list")
  return(query_obj)
}

#' Print Method for CustomDomainQuery
#'
#' @param x A CustomDomainQuery object
#' @param ... Additional arguments (ignored)
#' @export
print.CustomDomainQuery <- function(x, ...) {
  cat("Custom Domain Query:", x$domainName, "\n")
  cat("  Domain ID:", x$domainId, "\n")
  cat("  Table:", x$customDomain@tableSchema, ".", x$customDomain@tableName, "\n", sep = "")

  if (!is.null(x$conceptSet)) {
    cat("  Concept Set:", x$conceptSet@Name, "\n")
  }

  if (length(x$attributes) > 0) {
    cat("  Attributes:\n")
    for (name in names(x$attributes)) {
      cat("    ", name, ": ", paste(x$attributes[[name]], collapse = ", "), "\n", sep = "")
    }
  }

  invisible(x)
}

#' Convert Custom Domain Query to Extension Query
#'
#' @description
#' Convert a CustomDomainQuery to an ExtensionQuery for compilation.
#' This is used internally during cohort compilation.
#'
#' @param custom_query A CustomDomainQuery object.
#' @param query_name Character. Name for the extension query (optional).
#'
#' @return ExtensionQuery object
#'
#' @keywords internal
customDomainQueryToExtensionQuery <- function(custom_query, query_name = NULL) {
  domain <- custom_query$customDomain

  # Generate query name
  if (is.null(query_name)) {
    query_name <- paste0(domain@domainId, "_query")
  }

  # Build filters from domain defaults and attributes
  filters <- domain@defaultFilters

  # Add concept filter if concept set provided
  if (!is.null(custom_query$conceptSet)) {
    if (nchar(domain@conceptIdField) > 0) {
      # Get concept IDs from concept set
      concept_ids <- custom_query$conceptSet@Expression
      if (length(concept_ids) > 0) {
        # Extract concept IDs
        ids <- sapply(concept_ids, function(item) {
          item@Concept@concept_id
        })

        concept_filter <- paste0(
          domain@conceptIdField,
          " IN (",
          paste(ids, collapse = ", "),
          ")"
        )
        filters <- c(filters, list(concept_filter))
      }
    } else {
      warning("Custom domain '", domain@domainId, "' has no concept_id_field, ignoring concept set")
    }
  }

  # Process attributes to filters
  if (length(custom_query$attributes) > 0) {
    for (attr_name in names(custom_query$attributes)) {
      attr_value <- custom_query$attributes[[attr_name]]

      # Handle different attribute types
      if (attr_name == "valueAsNumber" && is.list(attr_value)) {
        # Numeric value filter
        field <- attr_value$field
        operator <- attr_value$operator %||% "="
        value <- attr_value$value

        if (!is.null(field) && !is.null(value)) {
          filter_str <- paste(field, operator, value)
          filters <- c(filters, list(filter_str))
        }
      } else if (attr_name == "valueAsString" && is.list(attr_value)) {
        # String value filter
        field <- attr_value$field
        operator <- attr_value$operator %||% "="
        value <- attr_value$value

        if (!is.null(field) && !is.null(value)) {
          if (operator == "IN") {
            filter_str <- paste0(field, " IN ('", paste(value, collapse = "', '"), "')")
          } else {
            filter_str <- paste0(field, " ", operator, " '", value, "'")
          }
          filters <- c(filters, list(filter_str))
        }
      } else if (is.character(attr_value)) {
        # Direct filter string
        filters <- c(filters, list(attr_value))
      }
    }
  }

  # Determine select fields
  select_fields <- character(0)
  if (length(domain@valueFields) > 0) {
    select_fields <- unlist(domain@valueFields, use.names = FALSE)
  }

  # Create ExtensionQuery
  extensionQuery(
    name = query_name,
    table = domain@tableName,
    schema = domain@tableSchema,
    person_id_field = domain@personIdField,
    date_field = domain@startDateField,
    select_fields = select_fields,
    filters = filters,
    temporal_constraint = character(0)  # Can be added later
  )
}

#' Detect Custom Domain Queries in Cohort
#'
#' @description
#' Recursively search for CustomDomainQuery objects in a cohort definition.
#'
#' @param x Object to search (cohort, entry, criteria, etc.).
#'
#' @return List of CustomDomainQuery objects found
#'
#' @keywords internal
findCustomDomainQueries <- function(x) {
  queries <- list()

  if (inherits(x, "CustomDomainQuery")) {
    return(list(x))
  }

  if (is.list(x)) {
    for (item in x) {
      queries <- c(queries, findCustomDomainQueries(item))
    }
  }

  # Handle S4 objects
  if (isS4(x)) {
    slot_names <- slotNames(x)
    for (slot_name in slot_names) {
      slot_value <- slot(x, slot_name)
      queries <- c(queries, findCustomDomainQueries(slot_value))
    }
  }

  return(queries)
}

#' Custom Domain Query Attribute Helpers
#'
#' @description
#' Helper functions for adding attributes to custom domain queries.
#'
#' @name custom-domain-attributes
NULL

#' Add Numeric Value Filter
#'
#' @description
#' Add a numeric value filter to a custom domain query.
#'
#' @param field Character. Field name.
#' @param operator Character. Comparison operator ("=", ">", "<", ">=", "<=", "BETWEEN").
#' @param value Numeric. Value or vector of values (for BETWEEN).
#'
#' @return List with filter specification
#'
#' @examples
#' \dontrun{
#' # Greater than or equal to 70
#' my_query <- patientSurvey(
#'   concepts,
#'   valueAsNumber = numericValue("total_score", ">=", 70)
#' )
#'
#' # Between 60 and 80
#' my_query <- patientSurvey(
#'   concepts,
#'   valueAsNumber = numericValue("total_score", "BETWEEN", c(60, 80))
#' )
#' }
#'
#' @export
numericValue <- function(field, operator = "=", value) {
  checkmate::assertCharacter(field, len = 1)
  checkmate::assertCharacter(operator, len = 1)
  checkmate::assertNumeric(value)

  list(
    field = field,
    operator = operator,
    value = value
  )
}

#' Add String Value Filter
#'
#' @description
#' Add a string value filter to a custom domain query.
#'
#' @param field Character. Field name.
#' @param operator Character. Comparison operator ("=", "IN", "LIKE").
#' @param value Character. Value or vector of values.
#'
#' @return List with filter specification
#'
#' @examples
#' \dontrun{
#' # Exact match
#' my_query <- patientSurvey(
#'   concepts,
#'   valueAsString = stringValue("survey_type", "=", "QOL")
#' )
#'
#' # IN clause
#' my_query <- patientSurvey(
#'   concepts,
#'   valueAsString = stringValue("survey_type", "IN", c("QOL", "SATISFACTION"))
#' )
#'
#' # LIKE pattern
#' my_query <- patientSurvey(
#'   concepts,
#'   valueAsString = stringValue("survey_type", "LIKE", "DIABETES%")
#' )
#' }
#'
#' @export
stringValue <- function(field, operator = "=", value) {
  checkmate::assertCharacter(field, len = 1)
  checkmate::assertCharacter(operator, len = 1)
  checkmate::assertCharacter(value)

  list(
    field = field,
    operator = operator,
    value = value
  )
}

#' Add Date Range Filter
#'
#' @description
#' Add a date range filter to a custom domain query.
#'
#' @param field Character. Date field name.
#' @param start_date Date or character. Start date.
#' @param end_date Date or character. End date.
#'
#' @return List with filter specification
#'
#' @examples
#' \dontrun{
#' my_query <- patientSurvey(
#'   concepts,
#'   dateRange = dateRange("survey_date", "2020-01-01", "2023-12-31")
#' )
#' }
#'
#' @export
dateRange <- function(field, start_date, end_date) {
  checkmate::assertCharacter(field, len = 1)

  list(
    field = field,
    start_date = as.character(start_date),
    end_date = as.character(end_date)
  )
}

#' Create Custom Query with Filters
#'
#' @description
#' Helper to create custom domain query with multiple filters.
#'
#' @param domain_id Character. Domain ID.
#' @param concept_set ConceptSet object (optional).
#' @param filters List. Named list of filters.
#'
#' @return CustomDomainQuery object
#'
#' @examples
#' \dontrun{
#' my_query <- customQuery(
#'   "patientSurvey",
#'   survey_concepts,
#'   filters = list(
#'     total_score = numericValue("total_score", ">=", 70),
#'     survey_type = stringValue("survey_type", "=", "QOL")
#'   )
#' )
#' }
#'
#' @export
customQuery <- function(domain_id, concept_set = NULL, filters = list()) {
  query <- customDomainQuery(domain_id, concept_set)

  # Add filters as attributes
  if (length(filters) > 0) {
    query$attributes <- filters
  }

  return(query)
}
