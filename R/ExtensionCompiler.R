#' Compile Extended Cohort to JSON
#'
#' @description
#' Compiles an extended cohort definition to JSON, preserving both CDM and
#' extension metadata. The resulting JSON includes standard CIRCE-BE format
#' for CDM components plus additional ExtensionMetadata section. Also detects
#' and converts custom domain queries to extension queries.
#'
#' @param extended_cohort Either an ExtendedCohort object or a regular Capr cohort
#'   object containing custom domain queries.
#' @param pretty Logical. Pretty-print JSON (default: FALSE).
#'
#' @return Character string containing JSON
#'
#' @examples
#' \dontrun{
#' library(Capr)
#'
#' # Option 1: Regular Capr cohort with custom domain queries (v2.0 approach)
#' my_cohort <- cohort(
#'   entry = entry(
#'     conditionOccurrence(cs(descendants(201254))),
#'     waveformFeature(qtc_concepts, valueAsNumber = numericValue("value", ">=", 450))
#'   )
#' )
#' json_str <- compileExtendedCohort(my_cohort, pretty = TRUE)
#'
#' # Option 2: ExtendedCohort object (legacy approach)
#' my_cohort <- extendedCohort(
#'   entry = extendedEntry(
#'     conditionOccurrence(cs(descendants(201254))),
#'     extension_queries = list(survey_query)
#'   )
#' )
#' json_str <- compileExtendedCohort(my_cohort, pretty = TRUE)
#' }
#'
#' @export
compileExtendedCohort <- function(extended_cohort, pretty = FALSE) {
  # Accept both ExtendedCohort objects and regular Capr cohorts
  # If it's a regular cohort, wrap it for processing
  if (inherits(extended_cohort, "ExtendedCohort")) {
    cohort_obj <- extended_cohort$cohort
    existing_metadata <- extended_cohort$extensionMetadata
  } else {
    # Assume it's a regular Capr cohort
    cohort_obj <- extended_cohort
    existing_metadata <- NULL
  }

  # Convert custom domain queries to observation queries with placeholders
  # Detect custom domain queries in the cohort
  custom_queries <- findCustomDomainQueries(cohort_obj)

  extension_metadata_list <- list()

  if (length(custom_queries) > 0) {
    message("Found ", length(custom_queries), " custom domain quer",
            ifelse(length(custom_queries) == 1, "y", "ies"),
            " - converting to observation placeholders...")

    # Convert each custom query to observation query with placeholder
    for (i in seq_along(custom_queries)) {
      obs_query <- convertCustomQueryToObservation(custom_queries[[i]])

      # Collect extension metadata for post-processing
      ext_meta <- attr(obs_query, "extension_metadata", exact = TRUE)
      if (!is.null(ext_meta)) {
        extension_metadata_list <- c(extension_metadata_list, list(ext_meta))
      }

      # Replace custom query with observation query in cohort definition
      # (This requires modifying the cohort structure in place)
      # For now, store both versions
    }
  }

  # Also convert any ExtensionQuery objects to observation queries
  # (This handles explicit extensionQuery() calls)
  if (!is.null(existing_metadata$extensionQueries)) {
    message("Converting ", length(existing_metadata$extensionQueries),
            " extension quer",
            ifelse(length(existing_metadata$extensionQueries) == 1, "y", "ies"),
            " to observation placeholders...")

    for (i in seq_along(existing_metadata$extensionQueries)) {
      ext_q <- existing_metadata$extensionQueries[[i]]

      # Convert list to ExtensionQuery if needed
      if (!inherits(ext_q, "ExtensionQuery")) {
        ext_q <- do.call(extensionQuery, ext_q)
      }

      obs_query <- convertExtensionQueryToObservation(ext_q)

      # Collect metadata
      ext_meta <- attr(obs_query, "extension_metadata", exact = TRUE)
      if (!is.null(ext_meta)) {
        extension_metadata_list <- c(extension_metadata_list, list(ext_meta))
      }
    }
  }

  # Convert cohort definition to use observation queries
  modified_cohort <- convertAllCustomQueries(cohort_obj)

  # Compile using standard Capr (now with observation placeholders instead of custom queries)
  base_json <- Capr::compile(modified_cohort)

  # Parse base JSON
  base_parsed <- jsonlite::fromJSON(base_json, simplifyVector = FALSE)

  # Add extension metadata for post-processing
  if (length(extension_metadata_list) > 0) {
    # Ensure each metadata item is properly structured as a list
    # Use I() to protect from auto_unbox
    properly_structured_metadata <- lapply(extension_metadata_list, function(item) {
      # Make sure all values are properly structured
      as.list(item)
    })

    base_parsed$ExtensionMetadata <- list(
      extensionQueries = I(properly_structured_metadata),  # Protect from auto-unboxing
      extensionLogic = existing_metadata$extensionLogic %||% "ALL",
      version = "2.0"  # Mark as v2.0 architecture
    )
  } else if (!is.null(existing_metadata)) {
    # Preserve any existing extension metadata
    base_parsed$ExtensionMetadata <- existing_metadata
  }

  # Convert back to JSON
  # Note: auto_unbox = TRUE is needed for Circe JSON compatibility
  # but we need to ensure our extension metadata is properly structured
  extended_json <- jsonlite::toJSON(
    base_parsed,
    auto_unbox = TRUE,
    null = "null",
    pretty = pretty
  )

  return(as.character(extended_json))
}

#' Write Extended Cohort to File
#'
#' @description
#' Write extended cohort definition to JSON file.
#'
#' @param extended_cohort An ExtendedCohort object.
#' @param path Character. File path to write JSON.
#' @param pretty Logical. Pretty-print JSON (default: TRUE).
#'
#' @return Invisible NULL (called for side effect)
#'
#' @examples
#' \dontrun{
#' writeExtendedCohort(my_cohort, "cohort_definition.json")
#' }
#'
#' @export
writeExtendedCohort <- function(extended_cohort, path, pretty = TRUE) {
  json_str <- compileExtendedCohort(extended_cohort, pretty = pretty)
  writeLines(json_str, path)
  message("Extended cohort written to: ", path)
  invisible(NULL)
}

#' Read Extended Cohort from File
#'
#' @description
#' Read extended cohort definition from JSON file.
#' Note: This returns parsed JSON, not a full ExtendedCohort object.
#'
#' @param path Character. File path to JSON file.
#'
#' @return List with cohort definition and extension metadata
#'
#' @examples
#' \dontrun{
#' cohort_def <- readExtendedCohort("cohort_definition.json")
#' }
#'
#' @export
readExtendedCohort <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  json_str <- paste(readLines(path), collapse = "\n")
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  return(parsed)
}

#' Extract Extension Metadata from JSON
#'
#' @description
#' Extract extension metadata from compiled JSON string. Useful for examining
#' the extension queries that will be substituted during SQL post-processing.
#'
#' @param json_str Character. JSON string from compileExtendedCohort().
#'
#' @return List with extension metadata, or NULL if none present
#'
#' @export
extractExtensionMetadata <- function(json_str) {
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  if (!is.null(parsed$ExtensionMetadata)) {
    return(parsed$ExtensionMetadata)
  }

  return(NULL)
}

#' Remove Extension Metadata from JSON
#'
#' @description
#' Remove extension metadata from JSON, returning only CDM components.
#' Used to generate standard Capr JSON for CirceR.
#'
#' @param json_str Character. JSON string with extension metadata.
#'
#' @return Character string with standard Capr JSON (no extension metadata)
#'
#' @keywords internal
removeExtensionMetadata <- function(json_str) {
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  # Remove extension metadata
  parsed$ExtensionMetadata <- NULL

  # Convert back to JSON
  clean_json <- jsonlite::toJSON(
    parsed,
    auto_unbox = TRUE,
    null = "null",
    pretty = FALSE
  )

  return(as.character(clean_json))
}

#' Validate Extended Cohort JSON
#'
#' @description
#' Validate that extended cohort JSON has required structure.
#'
#' @param json_str Character. JSON string to validate.
#'
#' @return Logical. TRUE if valid, FALSE otherwise (with warnings).
#'
#' @export
validateExtendedCohortJson <- function(json_str) {
  errors <- character()

  tryCatch({
    parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

    # Check for required CDM components
    required_fields <- c("ConceptSets", "PrimaryCriteria")
    missing_fields <- setdiff(required_fields, names(parsed))
    if (length(missing_fields) > 0) {
      errors <- c(errors, paste("Missing required CDM fields:", paste(missing_fields, collapse = ", ")))
    }

    # If extension metadata present, validate structure
    if (!is.null(parsed$ExtensionMetadata)) {
      ext_meta <- parsed$ExtensionMetadata

      if (!is.null(ext_meta$extensionQueries)) {
        if (!is.list(ext_meta$extensionQueries)) {
          errors <- c(errors, "ExtensionMetadata$extensionQueries must be a list")
        }

        # Validate each extension query
        for (i in seq_along(ext_meta$extensionQueries)) {
          ext_q <- ext_meta$extensionQueries[[i]]
          required_ext_fields <- c("name", "tableName", "dateField")
          missing_ext_fields <- setdiff(required_ext_fields, names(ext_q))
          if (length(missing_ext_fields) > 0) {
            errors <- c(errors, paste0(
              "Extension query ", i, " missing required fields: ",
              paste(missing_ext_fields, collapse = ", ")
            ))
          }
        }
      }
    }

  }, error = function(e) {
    errors <<- c(errors, paste("JSON parsing error:", e$message))
  })

  if (length(errors) > 0) {
    warning("JSON validation errors:\n", paste(errors, collapse = "\n"))
    return(FALSE)
  }

  return(TRUE)
}

#' Export Extended Cohort to Atlas JSON Format
#'
#' @description
#' Converts a CaprForExtensions JSON (with ExtensionMetadata section) to an
#' Atlas-compatible JSON format where extension metadata is embedded in the
#' value_as_string parameter of existing Observation queries. This allows the cohort
#' to be loaded into Atlas where users can modify standard OMOP elements.
#'
#' @param json_str Character. CaprForExtensions JSON string (with ExtensionMetadata).
#' @param concept_name_pattern Character. Pattern to identify extension concept sets
#'   (default: "^Extension:"). Uses regex matching.
#' @param pretty Logical. Pretty-print JSON (default: TRUE).
#'
#' @return Character string containing Atlas-compatible JSON with embedded extension metadata
#'
#' @examples
#' \dontrun{
#' # Compile extended cohort
#' json_str <- compileExtendedCohort(my_cohort)
#'
#' # Convert to Atlas format for editing in Atlas
#' atlas_json <- exportToAtlasJson(json_str)
#' writeLines(atlas_json, "cohort_for_atlas.json")
#' }
#'
#' @export
exportToAtlasJson <- function(json_str,
                               concept_name_pattern = "^Extension:",
                               pretty = TRUE) {
  # Parse input JSON
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  # Check if ExtensionMetadata exists
  if (is.null(parsed$ExtensionMetadata)) {
    warning("No ExtensionMetadata found in JSON. Returning original JSON.")
    return(json_str)
  }

  # Extract extension metadata
  ext_metadata <- parsed$ExtensionMetadata

  # Check if extensionQueries exist
  if (is.null(ext_metadata$extensionQueries) || length(ext_metadata$extensionQueries) == 0) {
    warning("No extensionQueries found in ExtensionMetadata. Returning original JSON.")
    return(json_str)
  }

  # Find all concept sets that match the extension pattern
  ext_concept_sets <- list()
  if (!is.null(parsed$ConceptSets)) {
    for (i in seq_along(parsed$ConceptSets)) {
      cs <- parsed$ConceptSets[[i]]
      if (!is.null(cs$name) && grepl(concept_name_pattern, cs$name)) {
        ext_concept_sets[[as.character(cs$id)]] <- list(
          index = i,
          id = cs$id,
          name = cs$name
        )
      }
    }
  }

  if (length(ext_concept_sets) == 0) {
    warning("No extension concept sets found matching pattern: ", concept_name_pattern)
    warning("Returning original JSON.")
    return(json_str)
  }

  # For each extension query in metadata, find matching observation and embed metadata
  for (i in seq_along(ext_metadata$extensionQueries)) {
    ext_query <- ext_metadata$extensionQueries[[i]]

    # Find concept set matching this extension query name
    # Look for concept set with name like "Extension:{queryName}"
    query_name <- ext_query$name

    # Validate query name
    if (is.null(query_name) || length(query_name) == 0 || nchar(query_name) == 0) {
      warning("Extension query ", i, " has no name field. Skipping.")
      next
    }

    matching_cs_id <- NULL

    for (cs_id in names(ext_concept_sets)) {
      cs_info <- ext_concept_sets[[cs_id]]
      # Match if concept set name contains the query name
      if (!is.null(cs_info$name) && grepl(query_name, cs_info$name, fixed = TRUE)) {
        matching_cs_id <- cs_info$id
        break
      }
    }

    if (is.null(matching_cs_id)) {
      warning("Could not find concept set for extension query: ", query_name)
      next
    }

    # Find observation with this CodesetId and embed metadata
    if (!is.null(parsed$PrimaryCriteria) &&
        !is.null(parsed$PrimaryCriteria$CriteriaList)) {

      for (j in seq_along(parsed$PrimaryCriteria$CriteriaList)) {
        criterion <- parsed$PrimaryCriteria$CriteriaList[[j]]

        if (!is.null(criterion$Observation) &&
            !is.null(criterion$Observation$CodesetId) &&
            criterion$Observation$CodesetId == matching_cs_id) {

          # Serialize this specific extension query to JSON
          query_json <- jsonlite::toJSON(
            ext_query,
            auto_unbox = TRUE,
            null = "null",
            pretty = FALSE
          )

          # Embed in ValueAsString
          parsed$PrimaryCriteria$CriteriaList[[j]]$Observation$ValueAsString <- list(
            Text = as.character(query_json),
            Op = "contains"
          )

          message("Embedded extension metadata for query '", query_name,
                  "' in observation with CodesetId ", matching_cs_id)
          break
        }
      }
    }
  }

  # Remove ExtensionMetadata section (now embedded in ValueAsString fields)
  parsed$ExtensionMetadata <- NULL

  # Convert back to JSON
  atlas_json <- jsonlite::toJSON(
    parsed,
    auto_unbox = TRUE,
    null = "null",
    pretty = pretty
  )

  return(as.character(atlas_json))
}

#' Import Extended Cohort from Atlas JSON Format
#'
#' @description
#' Converts an Atlas JSON (with extension metadata embedded in ValueAsString)
#' back to CaprForExtensions JSON format with ExtensionMetadata section.
#' This function extracts embedded extension metadata from the value_as_string
#' parameters of observation queries and restores them to the ExtensionMetadata section.
#'
#' @param json_str Character. Atlas JSON string with embedded extension metadata.
#' @param concept_name_pattern Character. Pattern to identify extension concept sets
#'   (default: "^Extension:"). Uses regex matching.
#' @param clear_value_as_string Logical. Remove the ValueAsString fields after
#'   extracting metadata (default: FALSE, keeps them for Atlas compatibility).
#' @param pretty Logical. Pretty-print JSON (default: TRUE).
#'
#' @return Character string containing CaprForExtensions JSON with ExtensionMetadata section
#'
#' @examples
#' \dontrun{
#' # Read Atlas JSON (modified in Atlas)
#' atlas_json <- paste(readLines("cohort_from_atlas.json"), collapse = "\n")
#'
#' # Convert back to CaprForExtensions format
#' extension_json <- importFromAtlasJson(atlas_json)
#'
#' # Validate and save
#' validateExtendedCohortJson(extension_json)
#' writeLines(extension_json, "cohort_definition.json")
#' }
#'
#' @export
importFromAtlasJson <- function(json_str,
                                 concept_name_pattern = "^Extension:",
                                 clear_value_as_string = FALSE,
                                 pretty = TRUE) {
  # Parse input JSON
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  # Find all extension concept sets
  ext_concept_sets <- list()
  if (!is.null(parsed$ConceptSets)) {
    for (i in seq_along(parsed$ConceptSets)) {
      cs <- parsed$ConceptSets[[i]]
      if (!is.null(cs$name) && grepl(concept_name_pattern, cs$name)) {
        ext_concept_sets[[as.character(cs$id)]] <- list(
          index = i,
          id = cs$id,
          name = cs$name
        )
      }
    }
  }

  if (length(ext_concept_sets) == 0) {
    warning("No extension concept sets found matching pattern: ", concept_name_pattern)
    warning("Returning original JSON.")
    return(json_str)
  }

  # Collect extension queries from observations
  extension_queries <- list()

  if (!is.null(parsed$PrimaryCriteria) &&
      !is.null(parsed$PrimaryCriteria$CriteriaList)) {

    for (i in seq_along(parsed$PrimaryCriteria$CriteriaList)) {
      criterion <- parsed$PrimaryCriteria$CriteriaList[[i]]

      if (!is.null(criterion$Observation)) {
        obs <- criterion$Observation
        cs_id <- obs$CodesetId

        # Check if this observation uses an extension concept set
        if (!is.null(cs_id) && !is.null(ext_concept_sets[[as.character(cs_id)]])) {

          # Extract metadata from ValueAsString if present
          if (!is.null(obs$ValueAsString) && !is.null(obs$ValueAsString$Text)) {
            ext_metadata_json <- obs$ValueAsString$Text

            # Parse extension query metadata
            ext_query <- tryCatch({
              jsonlite::fromJSON(ext_metadata_json, simplifyVector = FALSE)
            }, error = function(e) {
              warning("Failed to parse extension metadata from observation with CodesetId ",
                      cs_id, ": ", e$message)
              return(NULL)
            })

            if (!is.null(ext_query)) {
              extension_queries <- c(extension_queries, list(ext_query))

              cs_info <- ext_concept_sets[[as.character(cs_id)]]
              message("Extracted extension metadata for query '", ext_query$name,
                      "' from observation with CodesetId ", cs_id)

              # Optionally clear ValueAsString after extraction
              if (clear_value_as_string) {
                parsed$PrimaryCriteria$CriteriaList[[i]]$Observation$ValueAsString <- NULL
              }
            }
          }
        }
      }
    }
  }

  if (length(extension_queries) == 0) {
    warning("No extension metadata found in ValueAsString fields of observations")
    warning("Returning original JSON.")
    return(json_str)
  }

  # Create ExtensionMetadata section
  # Try to preserve existing metadata structure if present
  existing_metadata <- parsed$ExtensionMetadata

  parsed$ExtensionMetadata <- list(
    extensionQueries = extension_queries,
    extensionLogic = existing_metadata$extensionLogic %||% "ALL",
    version = existing_metadata$version %||% "2.0"
  )

  message("Created ExtensionMetadata section with ", length(extension_queries), " extension quer",
          ifelse(length(extension_queries) == 1, "y", "ies"))

  # Convert back to JSON
  extension_json <- jsonlite::toJSON(
    parsed,
    auto_unbox = TRUE,
    null = "null",
    pretty = pretty
  )

  return(as.character(extension_json))
}

# Helper for %||% operator
`%||%` <- function(a, b) {
  if (is.null(a) || (is.character(a) && length(a) == 0)) b else a
}
