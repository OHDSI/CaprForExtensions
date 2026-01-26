#' Convert Extension Queries to Observation Queries with Placeholders
#'
#' @description
#' Convert CustomDomainQuery or ExtensionQuery objects to standard Capr
#' observation queries with encoded placeholders. CirceR processes these
#' as normal observation queries, then we substitute the extension table
#' logic in post-processing.
#'
#' @name ObservationConverter
NULL

# Custom S4 Attribute Class for Placeholder Strings -------------------------

#' String Attribute Class
#'
#' @description
#' S4 class for a string attribute. This is used to encode placeholder
#' strings in observation queries that will be processed by CirceR.
#'
#' @slot name Character. Attribute name (e.g., "ValueAsString")
#' @slot value Character. Attribute value (the placeholder string)
setClass("stringAttribute",
         slots = c(name = "character", value = "character"),
         prototype = list(name = NA_character_, value = NA_character_))

#' Convert stringAttribute to List
#'
#' @description
#' Converts a stringAttribute to a list for JSON serialization.
#' This method is required for Capr's compilation process.
#'
#' @param x A stringAttribute object
#' @return Named list with attribute name and value
setMethod("as.list", "stringAttribute", function(x) {
  # Return a simple named list: list(ValueAsString = "placeholder_value")
  stats::setNames(list(x@value), x@name)
})

#' Convert Custom Domain Query to Observation Query
#'
#' @description
#' Main conversion function. Takes a CustomDomainQuery and returns a Capr
#' observation query with placeholders embedded.
#'
#' @param custom_query CustomDomainQuery object
#' @return Capr Query object (observation)
#'
#' @examples
#' \dontrun{
#' # After registering waveformFeature domain:
#' query <- waveformFeature(
#'   qtc_concepts,
#'   valueAsNumber = numericValue("value_as_number", ">=", 450)
#' )
#'
#' # Convert to observation:
#' obs_query <- convertCustomQueryToObservation(query)
#' }
#'
#' @export
convertCustomQueryToObservation <- function(custom_query) {
  if (!inherits(custom_query, "list") || custom_query$type != "CustomDomainQuery") {
    stop("Input must be a CustomDomainQuery object")
  }

  domain <- custom_query$customDomain

  # 1. Extract concept IDs if provided
  concept_ids <- NULL
  if (!is.null(custom_query$conceptSet)) {
    # Extract concept IDs from ConceptSet
    concept_ids <- extractConceptIds(custom_query$conceptSet)
  }

  # 2. Generate UNIQUE placeholder concept ID based on table, concepts, and filters
  # This ensures each query gets a different placeholder ID
  placeholder_id <- generatePlaceholderConceptId(
    domain@tableName,
    concept_ids = concept_ids,
    filters = custom_query$attributes
  )

  # 3. Determine if we need a join to parent table
  # (e.g., waveform_feature needs to join to waveform_occurrence)
  join_info <- determineJoinRequirements(domain)

  # 4. Encode all extension query information as placeholder string
  placeholder_string <- encodeExtensionQuery(
    table = domain@tableName,
    concept_field = domain@conceptIdField,
    concept_ids = concept_ids,
    filters = custom_query$attributes,
    join_info = join_info,
    date_field = domain@startDateField
  )

  # 5. Create placeholder concept set
  placeholder_concept <- Capr::cs(
    placeholder_id,
    name = paste0("Extension:", domain@domainName)
  )

  # 6. Build observation query with placeholder concept
  # Note: We don't add ValueAsString here because CirceR expects a TextFilter object
  # Instead, we rely on the placeholder concept ID to identify queries during post-processing
  obs_query <- Capr::observation(placeholder_concept)

  # 7. Store extension metadata in query attributes for later retrieval
  metadata <- list(
    original_type = "CustomDomainQuery",
    domain_id = domain@domainId,
    table_name = domain@tableName,
    placeholder_id = placeholder_id,
    placeholder_string = placeholder_string,
    concept_ids = concept_ids,
    join_info = join_info,
    date_field = domain@startDateField
  )

  # 8. Add custom concept sets if we have real concept IDs
  # These need to be inserted into #Codesets so CirceR can reference them
  # NOTE: We store the placeholder_id so we can find which codeset_id CirceR assigned to it
  if (!is.null(concept_ids) && length(concept_ids) > 0) {
    metadata$custom_concept_sets <- list(
      list(
        placeholder_id = placeholder_id,  # Track placeholder to find CirceR's codeset_id
        concept_ids = concept_ids,
        name = paste0("Extension:", domain@domainName)
      )
    )
  }

  attr(obs_query, "extension_metadata") <- metadata

  return(obs_query)
}

#' Convert Extension Query to Observation Query
#'
#' @description
#' Convert an ExtensionQuery object to observation query with placeholders.
#'
#' @param ext_query ExtensionQuery object
#' @return Capr Query object (observation)
#'
#' @keywords internal
convertExtensionQueryToObservation <- function(ext_query) {
  if (!inherits(ext_query, "ExtensionQuery")) {
    stop("Input must be an ExtensionQuery object")
  }

  # 1. Build filters list from ExtensionQuery
  filters <- list()
  for (filter_str in ext_query@filters) {
    # Parse filter string to extract field, operator, value
    # For now, store as-is
    filters[[paste0("custom_", length(filters) + 1)]] <- list(
      raw = filter_str
    )
  }

  # 2. Generate UNIQUE placeholder concept ID based on table and filters
  # ExtensionQuery doesn't have concept_ids, but filters make it unique
  placeholder_id <- generatePlaceholderConceptId(
    ext_query@tableName,
    concept_ids = NULL,
    filters = filters
  )

  # 3. Determine join requirements
  join_info <- NULL
  if (length(ext_query@visitIdField) > 0 && nchar(ext_query@visitIdField) > 0) {
    # Need to join through parent table for visit
    join_info <- list(
      field = ext_query@visitIdField,
      type = "visit"
    )
  }

  # 4. Encode placeholder string
  placeholder_string <- encodeExtensionQueryFromObject(ext_query, filters, join_info)

  # 5. Create placeholder concept
  placeholder_concept <- Capr::cs(
    placeholder_id,
    name = paste0("Extension:", ext_query@name)
  )

  # 6. Build observation query
  # Note: We don't add ValueAsString here because CirceR expects a TextFilter object
  # Instead, we rely on the placeholder concept ID to identify queries during post-processing
  obs_query <- Capr::observation(placeholder_concept)

  # 7. Store metadata
  attr(obs_query, "extension_metadata") <- list(
    original_type = "ExtensionQuery",
    name = ext_query@name,
    table_name = ext_query@tableName,
    placeholder_id = placeholder_id,
    placeholder_string = placeholder_string,
    filters = ext_query@filters,
    join_info = join_info,
    date_field = ext_query@dateField
  )

  return(obs_query)
}

#' Extract Concept IDs from ConceptSet
#'
#' @keywords internal
extractConceptIds <- function(concept_set) {
  if (is.null(concept_set)) return(NULL)

  # ConceptSet structure varies, handle different formats
  if (inherits(concept_set, "ConceptSet")) {
    # Capr ConceptSet object
    if (length(concept_set@Expression) > 0) {
      concept_ids <- sapply(concept_set@Expression, function(item) {
        item@Concept@concept_id
      })
      return(as.integer(concept_ids))
    }
  }

  return(NULL)
}

#' Determine Join Requirements
#'
#' @description
#' Determine if extension table needs joins to parent tables.
#' For example, waveform_feature doesn't have person_id directly,
#' needs to join through waveform_occurrence.
#'
#' @param domain CustomDomain object
#' @return List with join information, or NULL
#'
#' @keywords internal
determineJoinRequirements <- function(domain) {
  table <- domain@tableName

  # Define known table hierarchies
  # waveform_feature → waveform_occurrence (for person_id, visit_occurrence_id)
  if (table == "waveform_feature") {
    return(list(
      table = "waveform_occurrence",
      field = "waveform_occurrence_id",
      parent = "waveform_feature",
      needed_for = c("person_id", "visit_occurrence_id")
    ))
  }

  # waveform_channel_metadata → waveform_registry → waveform_occurrence
  if (table == "waveform_channel_metadata") {
    return(list(
      table = "waveform_registry",
      field = "waveform_registry_id",
      parent = "waveform_channel_metadata",
      cascade = list(
        table = "waveform_occurrence",
        field = "waveform_occurrence_id"
      )
    ))
  }

  # waveform_registry → waveform_occurrence
  if (table == "waveform_registry") {
    return(list(
      table = "waveform_occurrence",
      field = "waveform_occurrence_id",
      parent = "waveform_registry",
      needed_for = c("person_id", "visit_occurrence_id")
    ))
  }

  # No join needed (table has person_id directly)
  return(NULL)
}

#' Encode Extension Query from ExtensionQuery Object
#'
#' @keywords internal
encodeExtensionQueryFromObject <- function(ext_query, filters, join_info) {
  parts <- c()

  # Table
  parts <- c(parts, encodeTablePlaceholder(ext_query@tableName))

  # Raw filters from ExtensionQuery
  for (filter_str in ext_query@filters) {
    # Store filters as-is, will be used directly in SQL substitution
    parts <- c(parts, paste0(PLACEHOLDER_PREFIX, PLACEHOLDER_FILTER, ":",
                             filter_str, PLACEHOLDER_SUFFIX))
  }

  # Join
  if (!is.null(join_info)) {
    parts <- c(parts, encodeJoinPlaceholder(
      join_info$table %||% "unknown",
      join_info$field %||% "unknown"
    ))
  }

  # Date field
  if (length(ext_query@dateField) > 0) {
    parts <- c(parts, encodeDatePlaceholder(ext_query@dateField))
  }

  paste(parts, collapse = PLACEHOLDER_SEPARATOR)
}

#' Find and Convert All Custom Queries in Cohort Definition
#'
#' @description
#' Recursively search cohort definition for CustomDomainQuery objects
#' and convert them to observation queries with placeholders.
#'
#' @param cohort_def Cohort definition object
#' @return Modified cohort definition with queries converted
#'
#' @keywords internal
convertAllCustomQueries <- function(cohort_def) {
  # Recursively search for CustomDomainQuery objects
  convertNode <- function(node) {
    # Handle CustomDomainQuery objects
    if (inherits(node, "CustomDomainQuery")) {
      # Convert to observation query
      return(convertCustomQueryToObservation(node))
    }

    # Handle list objects
    if (is.list(node)) {
      # Check if this is a CustomDomainQuery list
      if (!is.null(node$type) && node$type == "CustomDomainQuery") {
        # Convert to observation query
        return(convertCustomQueryToObservation(node))
      }

      # Recursively process list elements
      return(lapply(node, convertNode))
    }

    # Handle S4 objects (Entry, Count, Group, etc.)
    if (isS4(node)) {
      # Get all slot names
      slot_names <- slotNames(node)

      # Process each slot
      for (slot_name in slot_names) {
        slot_value <- slot(node, slot_name)

        # Convert the slot value
        converted_value <- convertNode(slot_value)

        # Update the slot if it changed
        if (!identical(slot_value, converted_value)) {
          slot(node, slot_name) <- converted_value
        }
      }

      return(node)
    }

    # Return unchanged for other types
    return(node)
  }

  convertNode(cohort_def)
}

#' Collect Extension Metadata from Converted Cohort
#'
#' @description
#' After conversion, collect all extension metadata from the
#' converted observation queries for later use in post-processing.
#'
#' @param cohort_def Cohort definition (after conversion)
#' @return List of extension metadata
#'
#' @keywords internal
collectExtensionMetadata <- function(cohort_def) {
  metadata_list <- list()

  # Recursively search for extension metadata attributes
  collectFromNode <- function(node) {
    if (is.list(node)) {
      # Check for extension_metadata attribute
      ext_meta <- attr(node, "extension_metadata", exact = TRUE)
      if (!is.null(ext_meta)) {
        metadata_list <<- c(metadata_list, list(ext_meta))
      }

      # Recursively process
      lapply(node, collectFromNode)
    }
  }

  collectFromNode(cohort_def)

  return(metadata_list)
}

# Helper for %||% operator
`%||%` <- function(a, b) {
  if (is.null(a) || (is.character(a) && length(a) == 0)) b else a
}
