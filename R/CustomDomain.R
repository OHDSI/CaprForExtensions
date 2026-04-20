#' Custom Domain Support
#'
#' @description
#' Define and register custom domains that can be used like standard OMOP CDM
#' domains in Capr queries. Custom domains map extension tables to domain-like
#' structures that integrate seamlessly with Capr's query system.

# Package environment for custom domain registry
if (!exists(".customDomainRegistry", envir = .pkgenv)) {
  .pkgenv$.customDomainRegistry <- list()
}

#' CustomDomain Class
#'
#' @description
#' S4 class representing a custom domain (non-CDM table treated as a domain).
#' Custom domains can be used in Capr-style queries.
#'
#' @slot domainId Character. Unique identifier for this domain.
#' @slot domainName Character. Human-readable domain name.
#' @slot tableName Character. Database table name.
#' @slot tableSchema Character. Database schema.
#' @slot personIdField Character. Person ID field name.
#' @slot visitIdField Character. Visit occurrence ID field (optional, for visit-level joins).
#' @slot startDateField Character. Start date field name.
#' @slot endDateField Character. End date field (optional).
#' @slot conceptIdField Character. Concept ID field (optional, for vocabulary integration).
#' @slot valueFields List. Value fields (numeric, string, datetime).
#' @slot requiredFields Character vector. Fields that must be non-null.
#' @slot defaultFilters List. Default filters applied to all queries.
#' @slot metadata List. Additional metadata.
#'
#' @export
setClass("CustomDomain",
         slots = c(
           domainId = "character",
           domainName = "character",
           tableName = "character",
           tableSchema = "character",
           personIdField = "character",
           visitIdField = "character",
           startDateField = "character",
           endDateField = "character",
           conceptIdField = "character",
           valueFields = "list",
           requiredFields = "character",
           defaultFilters = "list",
           metadata = "list"
         ),
         prototype = list(
           domainId = character(0),
           domainName = character(0),
           tableName = character(0),
           tableSchema = "@cdm_database_schema",
           personIdField = "person_id",
           visitIdField = character(0),
           startDateField = character(0),
           endDateField = character(0),
           conceptIdField = character(0),
           valueFields = list(),
           requiredFields = character(0),
           defaultFilters = list(),
           metadata = list()
         ))

#' Validate CustomDomain
#' @keywords internal
setValidity("CustomDomain", function(object) {
  errors <- character()

  # Required fields
  if (length(object@domainId) == 0 || nchar(object@domainId) == 0) {
    errors <- c(errors, "domainId must be provided")
  }

  if (length(object@tableName) == 0 || nchar(object@tableName) == 0) {
    errors <- c(errors, "tableName must be provided")
  }

  if (length(object@startDateField) == 0 || nchar(object@startDateField) == 0) {
    errors <- c(errors, "startDateField must be provided")
  }

  # Domain ID should be valid identifier
  if (length(object@domainId) > 0 && !grepl("^[a-zA-Z][a-zA-Z0-9_]*$", object@domainId)) {
    errors <- c(errors, "domainId must be a valid identifier (letters, numbers, underscore)")
  }

  if (length(errors) == 0) TRUE else errors
})

#' Register Custom Domain
#'
#' @description
#' Register a custom domain that can be used like standard OMOP domains in Capr queries.
#' Once registered, you can create queries using the custom domain ID.
#'
#' @param domain_id Character. Unique identifier (used in R code, e.g., "waveformOccurrence").
#' @param domain_name Character. Human-readable name (e.g., "Waveform Occurrence").
#' @param table Character. Database table name.
#' @param schema Character. Database schema (default: "@cdm_database_schema").
#' @param person_id_field Character. Person ID field (default: "person_id").
#' @param visit_id_field Character. Visit occurrence ID field for visit-level joins (optional).
#' @param start_date_field Character. Start/event date field.
#' @param end_date_field Character. End date field (optional).
#' @param concept_id_field Character. Concept ID field for vocabulary (optional).
#' @param value_fields List. Named list of value fields with types:
#'   \code{list(numeric = c("field1"), string = c("field2"), date = c("field3"))}.
#' @param required_fields Character vector. Fields that must be non-null.
#' @param default_filters List. Default filters applied to all queries.
#' @param metadata List. Additional metadata.
#' @param overwrite Logical. Overwrite if already registered (default: FALSE).
#'
#' @return Invisible NULL (called for side effect)
#'
#' @examples
#' \dontrun{
#' # Register patient survey as custom domain
#' registerCustomDomain(
#'   domain_id = "patientSurvey",
#'   domain_name = "Patient Survey",
#'   table = "patient_survey_results",
#'   start_date_field = "survey_date",
#'   concept_id_field = "survey_type_concept_id",
#'   value_fields = list(
#'     numeric = c("total_score", "domain_score"),
#'     string = c("survey_type", "survey_version")
#'   ),
#'   required_fields = c("survey_type", "total_score")
#' )
#'
#' # Register custom lab domain
#' registerCustomDomain(
#'   domain_id = "customLab",
#'   domain_name = "Custom Lab Results",
#'   table = "institution_lab_results",
#'   start_date_field = "lab_date",
#'   concept_id_field = "lab_test_concept_id",
#'   value_fields = list(
#'     numeric = c("lab_value"),
#'     string = c("lab_unit", "reference_range")
#'   )
#' )
#' }
#'
#' @export
registerCustomDomain <- function(
  domain_id,
  domain_name = domain_id,
  table,
  schema = "@cdm_database_schema",
  person_id_field = "person_id",
  visit_id_field = character(0),
  start_date_field,
  end_date_field = character(0),
  concept_id_field = character(0),
  value_fields = list(),
  required_fields = character(0),
  default_filters = list(),
  metadata = list(),
  overwrite = FALSE
) {
  # Validate inputs
  checkmate::assertCharacter(domain_id, len = 1, min.chars = 1)
  checkmate::assertCharacter(table, len = 1, min.chars = 1)
  checkmate::assertCharacter(start_date_field, len = 1, min.chars = 1)
  checkmate::assertCharacter(visit_id_field, null.ok = TRUE)

  # Check if already registered
  if (domain_id %in% names(.pkgenv$.customDomainRegistry) && !overwrite) {
    stop("Custom domain '", domain_id, "' already registered. Use overwrite=TRUE to replace.")
  }

  # Create CustomDomain object
  custom_domain <- new("CustomDomain",
                       domainId = domain_id,
                       domainName = domain_name,
                       tableName = table,
                       tableSchema = schema,
                       personIdField = person_id_field,
                       visitIdField = if(length(visit_id_field) > 0) visit_id_field else character(0),
                       startDateField = start_date_field,
                       endDateField = end_date_field,
                       conceptIdField = concept_id_field,
                       valueFields = value_fields,
                       requiredFields = required_fields,
                       defaultFilters = default_filters,
                       metadata = metadata)

  # Register
  .pkgenv$.customDomainRegistry[[domain_id]] <- custom_domain

  message("Registered custom domain: ", domain_id, " (", domain_name, ")")

  # Create query constructor function
  createCustomQueryFunction(domain_id)

  invisible(NULL)
}

#' List Custom Domains
#'
#' @description
#' List all registered custom domains.
#'
#' @param pattern Character. Optional regex pattern to filter (default: NULL).
#' @param verbose Logical. Show detailed information (default: FALSE).
#'
#' @return Character vector of domain IDs, or data frame if verbose=TRUE
#'
#' @export
listCustomDomains <- function(pattern = NULL, verbose = FALSE) {
  if (length(.pkgenv$.customDomainRegistry) == 0) {
    message("No custom domains registered")
    return(character(0))
  }

  domain_ids <- names(.pkgenv$.customDomainRegistry)

  # Filter by pattern
  if (!is.null(pattern)) {
    domain_ids <- grep(pattern, domain_ids, value = TRUE)
  }

  if (!verbose) {
    return(domain_ids)
  }

  # Build detailed data frame
  details <- lapply(domain_ids, function(id) {
    domain <- .pkgenv$.customDomainRegistry[[id]]
    data.frame(
      domain_id = domain@domainId,
      domain_name = domain@domainName,
      table = domain@tableName,
      schema = domain@tableSchema,
      start_date_field = domain@startDateField,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, details)
}

#' Get Custom Domain Definition
#'
#' @description
#' Retrieve a registered custom domain definition.
#'
#' @param domain_id Character. Domain ID.
#'
#' @return CustomDomain object
#'
#' @export
getCustomDomain <- function(domain_id) {
  checkmate::assertCharacter(domain_id, len = 1)

  if (!domain_id %in% names(.pkgenv$.customDomainRegistry)) {
    stop("Custom domain '", domain_id, "' not found. Use listCustomDomains() to see registered domains.")
  }

  return(.pkgenv$.customDomainRegistry[[domain_id]])
}

#' Unregister Custom Domain
#'
#' @description
#' Remove a custom domain from the registry.
#'
#' @param domain_id Character. Domain ID to remove.
#'
#' @return Invisible NULL
#'
#' @export
unregisterCustomDomain <- function(domain_id) {
  checkmate::assertCharacter(domain_id, len = 1)

  if (!domain_id %in% names(.pkgenv$.customDomainRegistry)) {
    warning("Custom domain '", domain_id, "' not found")
    return(invisible(NULL))
  }

  .pkgenv$.customDomainRegistry[[domain_id]] <- NULL
  message("Unregistered custom domain: ", domain_id)
  invisible(NULL)
}

#' Clear Custom Domain Registry
#'
#' @description
#' Remove all custom domains from the registry.
#'
#' @return Invisible NULL
#'
#' @export
clearCustomDomainRegistry <- function() {
  n_domains <- length(.pkgenv$.customDomainRegistry)
  .pkgenv$.customDomainRegistry <- list()
  message("Cleared ", n_domains, " custom domain(s)")
  invisible(NULL)
}

#' Show Method for CustomDomain
#'
#' @param object A CustomDomain object
#' @keywords internal
setMethod("show", "CustomDomain", function(object) {
  cat("Custom Domain:", object@domainName, "(", object@domainId, ")\n", sep = "")
  cat("  Table:", object@tableSchema, ".", object@tableName, "\n", sep = "")
  cat("  Person ID:", object@personIdField, "\n")
  cat("  Start Date:", object@startDateField, "\n")

  if (length(object@endDateField) > 0 && nchar(object@endDateField) > 0) {
    cat("  End Date:", object@endDateField, "\n")
  }

  if (length(object@conceptIdField) > 0 && nchar(object@conceptIdField) > 0) {
    cat("  Concept ID:", object@conceptIdField, "\n")
  }

  if (length(object@valueFields) > 0) {
    cat("  Value Fields:\n")
    for (type in names(object@valueFields)) {
      fields <- object@valueFields[[type]]
      if (length(fields) > 0) {
        cat("    ", type, ": ", paste(fields, collapse = ", "), "\n", sep = "")
      }
    }
  }

  if (length(object@requiredFields) > 0) {
    cat("  Required Fields:", paste(object@requiredFields, collapse = ", "), "\n")
  }

  if (length(object@defaultFilters) > 0) {
    cat("  Default Filters:\n")
    for (filter in object@defaultFilters) {
      cat("    -", filter, "\n")
    }
  }
})

#' Create Custom Query Function
#'
#' @description
#' Dynamically create a query constructor function for a custom domain.
#' This allows the domain to be used like standard Capr domains.
#'
#' @param domain_id Character. Domain ID.
#'
#' @return Invisible NULL (creates function in package namespace)
#'
#' @keywords internal
createCustomQueryFunction <- function(domain_id) {
  domain <- getCustomDomain(domain_id)

  # Create function that mimics Capr query constructors
  func <- function(conceptSet = NULL, ...) {
    # Get the custom domain (will be available when function is called)
    domain_obj <- CaprForExtensions::getCustomDomain(domain_id)

    # Create a special query object that includes custom domain info
    query_obj <- list(
      type = "CustomDomainQuery",
      domainId = domain_obj@domainId,
      domainName = domain_obj@domainName,
      customDomain = domain_obj,
      conceptSet = conceptSet,
      attributes = list(...)
    )

    class(query_obj) <- c("CustomDomainQuery", "list")
    return(query_obj)
  }

  # Set attributes for documentation
  attr(func, "domain_id") <- domain_id
  attr(func, "domain_name") <- domain@domainName

  # Assign to global environment so the function is available everywhere,
  # regardless of how deep registerCustomDomain() was called.
  func_name <- domain_id
  assign(func_name, func, envir = globalenv())

  message("Created query function: ", func_name, "()")

  invisible(NULL)
}

#' Check if Query is Custom Domain Query
#'
#' @param query A query object
#' @return Logical
#' @keywords internal
isCustomDomainQuery <- function(query) {
  inherits(query, "CustomDomainQuery")
}

#' Export Custom Domain Registry
#'
#' @description
#' Export custom domain definitions to JSON file.
#'
#' @param path Character. File path.
#' @param pretty Logical. Pretty-print JSON (default: TRUE).
#'
#' @return Invisible NULL
#'
#' @export
exportCustomDomainRegistry <- function(path, pretty = TRUE) {
  if (length(.pkgenv$.customDomainRegistry) == 0) {
    warning("Custom domain registry is empty")
    return(invisible(NULL))
  }

  # Convert CustomDomain objects to lists
  registry_list <- lapply(.pkgenv$.customDomainRegistry, function(domain) {
    list(
      domainId = domain@domainId,
      domainName = domain@domainName,
      tableName = domain@tableName,
      tableSchema = domain@tableSchema,
      personIdField = domain@personIdField,
      startDateField = domain@startDateField,
      endDateField = domain@endDateField,
      conceptIdField = domain@conceptIdField,
      valueFields = domain@valueFields,
      requiredFields = domain@requiredFields,
      defaultFilters = domain@defaultFilters,
      metadata = domain@metadata
    )
  })

  json_str <- jsonlite::toJSON(registry_list, auto_unbox = TRUE, pretty = pretty)
  writeLines(json_str, path)
  message("Custom domain registry exported to: ", path)
  invisible(NULL)
}

#' Import Custom Domain Registry
#'
#' @description
#' Import custom domain definitions from JSON file.
#'
#' @param path Character. File path.
#' @param overwrite Logical. Overwrite existing registrations (default: FALSE).
#'
#' @return Invisible NULL
#'
#' @export
importCustomDomainRegistry <- function(path, overwrite = FALSE) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  json_str <- paste(readLines(path), collapse = "\n")
  imported <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  n_imported <- 0
  n_skipped <- 0

  for (domain_id in names(imported)) {
    domain_def <- imported[[domain_id]]

    # Check if already exists
    if (domain_id %in% names(.pkgenv$.customDomainRegistry) && !overwrite) {
      n_skipped <- n_skipped + 1
      next
    }

    # Register domain
    tryCatch({
      registerCustomDomain(
        domain_id = domain_def$domainId,
        domain_name = domain_def$domainName,
        table = domain_def$tableName,
        schema = domain_def$tableSchema %||% "@cdm_database_schema",
        person_id_field = domain_def$personIdField %||% "person_id",
        start_date_field = domain_def$startDateField,
        end_date_field = domain_def$endDateField %||% character(0),
        concept_id_field = domain_def$conceptIdField %||% character(0),
        value_fields = domain_def$valueFields %||% list(),
        required_fields = domain_def$requiredFields %||% character(0),
        default_filters = domain_def$defaultFilters %||% list(),
        metadata = domain_def$metadata %||% list(),
        overwrite = overwrite
      )
      n_imported <- n_imported + 1
    }, error = function(e) {
      warning("Failed to import domain '", domain_id, "': ", e$message)
    })
  }

  message("Imported ", n_imported, " custom domain(s)")
  if (n_skipped > 0) {
    message("Skipped ", n_skipped, " existing domain(s) (use overwrite=TRUE)")
  }

  invisible(NULL)
}

#' Null Coalesce Operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
