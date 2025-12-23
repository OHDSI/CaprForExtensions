#' Placeholder Encoding for Extension Tables
#'
#' @description
#' System for encoding extension table queries as OMOP Observation queries with
#' special placeholder markers. CirceR processes these as standard queries, then
#' CaprForExtensions post-processes the SQL to substitute extension table logic.
#'
#' @name PlaceholderEncoding
NULL

# Placeholder format constants
PLACEHOLDER_PREFIX <- "@EXT_"
PLACEHOLDER_SUFFIX <- "@"
PLACEHOLDER_SEPARATOR <- "|||"

# Placeholder types
PLACEHOLDER_TABLE <- "TABLE"
PLACEHOLDER_CONCEPT <- "CONCEPT"
PLACEHOLDER_FILTER <- "FILTER"
PLACEHOLDER_JOIN <- "JOIN"
PLACEHOLDER_DATE <- "DATE"

#' Generate Unique Placeholder Concept ID
#'
#' @description
#' Generate a unique concept ID for a placeholder based on table name,
#' concept IDs, and filters. Uses high numbers (999900000+) unlikely
#' to conflict with real concepts.
#'
#' @param table_name Character. Extension table name
#' @param concept_ids Integer vector. Concept IDs used in the query (optional)
#' @param filters List. Filter attributes (optional)
#' @return Integer. Unique placeholder concept ID
#'
#' @keywords internal
generatePlaceholderConceptId <- function(table_name, concept_ids = NULL, filters = NULL) {
  # Build unique string from all components
  unique_string <- table_name

  # Add concept IDs to make each query unique
  if (!is.null(concept_ids) && length(concept_ids) > 0) {
    unique_string <- paste0(unique_string, "_", paste(sort(concept_ids), collapse = "_"))
  }

  # Add filter info to make queries with same concepts but different filters unique
  if (!is.null(filters) && length(filters) > 0) {
    filter_strings <- character()
    for (fname in names(filters)) {
      fval <- filters[[fname]]
      if (is.list(fval)) {
        # Extract key components from filter
        filter_str <- paste0(fname, "_",
                           fval$field %||% "", "_",
                           fval$operator %||% "", "_",
                           fval$value %||% "")
        filter_strings <- c(filter_strings, filter_str)
      }
    }
    if (length(filter_strings) > 0) {
      unique_string <- paste0(unique_string, "_", paste(sort(filter_strings), collapse = "_"))
    }
  }

  # Generate hash from unique string
  hash_val <- sum(utf8ToInt(unique_string)) %% 100000
  999900000 + hash_val
}

# Helper for %||% operator
`%||%` <- function(a, b) {
  if (is.null(a) || (is.character(a) && length(a) == 0)) b else a
}

#' Encode Table Placeholder
#'
#' @description
#' Create placeholder marker for table substitution.
#'
#' @param table_name Character. Extension table name
#' @param alias Character. Table alias (optional)
#' @return Character. Encoded placeholder
#'
#' @examples
#' \dontrun{
#' encodeTablePlaceholder("waveform_feature", "wf")
#' # Returns: "@EXT_TABLE:waveform_feature:wf@"
#' }
#'
#' @keywords internal
encodeTablePlaceholder <- function(table_name, alias = NULL) {
  parts <- c(PLACEHOLDER_PREFIX, PLACEHOLDER_TABLE, ":", table_name)
  if (!is.null(alias) && nchar(alias) > 0) {
    parts <- c(parts, ":", alias)
  }
  parts <- c(parts, PLACEHOLDER_SUFFIX)
  paste0(parts, collapse = "")
}

#' Encode Concept Filter Placeholder
#'
#' @description
#' Create placeholder for concept ID filter substitution.
#'
#' @param concept_field Character. Concept ID field name in extension table
#' @param concept_ids Integer vector. Concept IDs to filter
#' @return Character. Encoded placeholder
#'
#' @examples
#' \dontrun{
#' encodeConceptPlaceholder("algorithm_concept_id", c(2000000051, 2000000052))
#' # Returns: "@EXT_CONCEPT:algorithm_concept_id IN (2000000051,2000000052)@"
#' }
#'
#' @keywords internal
encodeConceptPlaceholder <- function(concept_field, concept_ids) {
  if (length(concept_ids) == 0) {
    return("")
  }

  filter_sql <- if (length(concept_ids) == 1) {
    paste0(concept_field, " = ", concept_ids[1])
  } else {
    paste0(concept_field, " IN (", paste(concept_ids, collapse = ","), ")")
  }

  paste0(PLACEHOLDER_PREFIX, PLACEHOLDER_CONCEPT, ":", filter_sql, PLACEHOLDER_SUFFIX)
}

#' Encode Value Filter Placeholder
#'
#' @description
#' Create placeholder for value filter substitution (numeric, string, etc.).
#'
#' @param field Character. Field name
#' @param operator Character. Comparison operator
#' @param value Value to compare
#' @return Character. Encoded placeholder
#'
#' @examples
#' \dontrun{
#' encodeFilterPlaceholder("value_as_number", ">=", 450)
#' # Returns: "@EXT_FILTER:value_as_number >= 450@"
#' }
#'
#' @keywords internal
encodeFilterPlaceholder <- function(field, operator, value) {
  # Handle different value types
  if (is.character(value)) {
    value_str <- paste0("'", value, "'")
  } else if (is.numeric(value)) {
    value_str <- as.character(value)
  } else {
    value_str <- toString(value)
  }

  filter_sql <- paste(field, operator, value_str)
  paste0(PLACEHOLDER_PREFIX, PLACEHOLDER_FILTER, ":", filter_sql, PLACEHOLDER_SUFFIX)
}

#' Encode Join Placeholder
#'
#' @description
#' Create placeholder for join logic (e.g., to parent waveform_occurrence table).
#'
#' @param join_table Character. Table to join to
#' @param join_field Character. Field to join on
#' @param parent_table Character. Parent table name (optional)
#' @return Character. Encoded placeholder
#'
#' @examples
#' \dontrun{
#' encodeJoinPlaceholder("waveform_occurrence", "waveform_occurrence_id")
#' # Returns: "@EXT_JOIN:waveform_occurrence.waveform_occurrence_id@"
#' }
#'
#' @keywords internal
encodeJoinPlaceholder <- function(join_table, join_field, parent_table = NULL) {
  join_spec <- paste0(join_table, ".", join_field)
  if (!is.null(parent_table)) {
    join_spec <- paste0(parent_table, ".", join_spec)
  }
  paste0(PLACEHOLDER_PREFIX, PLACEHOLDER_JOIN, ":", join_spec, PLACEHOLDER_SUFFIX)
}

#' Encode Date Field Placeholder
#'
#' @description
#' Create placeholder for date field mapping (observation_date → custom date field).
#'
#' @param date_field Character. Date field name in extension table
#' @return Character. Encoded placeholder
#'
#' @examples
#' \dontrun{
#' encodeDatePlaceholder("waveform_occurrence_start_datetime")
#' # Returns: "@EXT_DATE:waveform_occurrence_start_datetime@"
#' }
#'
#' @keywords internal
encodeDatePlaceholder <- function(date_field) {
  paste0(PLACEHOLDER_PREFIX, PLACEHOLDER_DATE, ":", date_field, PLACEHOLDER_SUFFIX)
}

#' Encode Extension Query as Placeholder String
#'
#' @description
#' Combine all placeholders into a single string that will be stored in
#' value_as_string field of the observation query.
#'
#' @param table Character. Extension table name
#' @param concept_field Character. Concept ID field (optional)
#' @param concept_ids Integer vector. Concept IDs (optional)
#' @param filters List. Named list of filters
#' @param join_info List. Join information (optional)
#' @param date_field Character. Date field name
#' @return Character. Complete placeholder string
#'
#' @keywords internal
encodeExtensionQuery <- function(
  table,
  concept_field = NULL,
  concept_ids = NULL,
  filters = list(),
  join_info = NULL,
  date_field = NULL
) {
  parts <- c()

  # 1. Table placeholder (always included)
  parts <- c(parts, encodeTablePlaceholder(table))

  # 2. Concept filter placeholder (if applicable)
  if (!is.null(concept_field) && !is.null(concept_ids) && length(concept_ids) > 0) {
    parts <- c(parts, encodeConceptPlaceholder(concept_field, concept_ids))
  }

  # 3. Value filter placeholders
  for (filter_name in names(filters)) {
    filter_def <- filters[[filter_name]]

    if (filter_name == "valueAsNumber") {
      parts <- c(parts, encodeFilterPlaceholder(
        filter_def$field,
        filter_def$operator,
        filter_def$value
      ))
    } else if (filter_name == "valueAsString") {
      parts <- c(parts, encodeFilterPlaceholder(
        filter_def$field,
        filter_def$operator,
        filter_def$value
      ))
    } else if (filter_name == "dateRange") {
      # Handle date range filters
      parts <- c(parts, encodeFilterPlaceholder(
        filter_def$field,
        "BETWEEN",
        paste0(filter_def$start, " AND ", filter_def$end)
      ))
    }
  }

  # 4. Join placeholder (if needed)
  if (!is.null(join_info)) {
    parts <- c(parts, encodeJoinPlaceholder(
      join_info$table,
      join_info$field,
      join_info$parent
    ))
  }

  # 5. Date field placeholder
  if (!is.null(date_field)) {
    parts <- c(parts, encodeDatePlaceholder(date_field))
  }

  # Combine with separator
  paste(parts, collapse = PLACEHOLDER_SEPARATOR)
}

#' Decode Placeholder String
#'
#' @description
#' Parse a placeholder string back into its components.
#'
#' @param placeholder_string Character. Encoded placeholder string
#' @return List with decoded components
#'
#' @keywords internal
decodePlaceholderString <- function(placeholder_string) {
  # Split by separator
  parts <- strsplit(placeholder_string, PLACEHOLDER_SEPARATOR, fixed = TRUE)[[1]]

  result <- list(
    table = NULL,
    concept_filter = NULL,
    value_filters = list(),
    join = NULL,
    date_field = NULL
  )

  for (part in parts) {
    if (grepl(paste0("^", PLACEHOLDER_PREFIX, PLACEHOLDER_TABLE), part)) {
      # Table placeholder
      result$table <- extractPlaceholderValue(part, PLACEHOLDER_TABLE)

    } else if (grepl(paste0("^", PLACEHOLDER_PREFIX, PLACEHOLDER_CONCEPT), part)) {
      # Concept filter placeholder
      result$concept_filter <- extractPlaceholderValue(part, PLACEHOLDER_CONCEPT)

    } else if (grepl(paste0("^", PLACEHOLDER_PREFIX, PLACEHOLDER_FILTER), part)) {
      # Value filter placeholder
      result$value_filters <- c(
        result$value_filters,
        list(extractPlaceholderValue(part, PLACEHOLDER_FILTER))
      )

    } else if (grepl(paste0("^", PLACEHOLDER_PREFIX, PLACEHOLDER_JOIN), part)) {
      # Join placeholder
      result$join <- extractPlaceholderValue(part, PLACEHOLDER_JOIN)

    } else if (grepl(paste0("^", PLACEHOLDER_PREFIX, PLACEHOLDER_DATE), part)) {
      # Date field placeholder
      result$date_field <- extractPlaceholderValue(part, PLACEHOLDER_DATE)
    }
  }

  return(result)
}

#' Extract Value from Placeholder
#'
#' @keywords internal
extractPlaceholderValue <- function(placeholder, type) {
  # Remove prefix and suffix
  prefix <- paste0(PLACEHOLDER_PREFIX, type, ":")

  value <- gsub(prefix, "", placeholder, fixed = TRUE)
  value <- gsub(PLACEHOLDER_SUFFIX, "", value, fixed = TRUE)

  return(value)
}

#' Check if String Contains Placeholders
#'
#' @param string Character
#' @return Logical
#'
#' @keywords internal
hasPlaceholders <- function(string) {
  grepl(PLACEHOLDER_PREFIX, string, fixed = TRUE)
}

#' Extract All Placeholders from SQL
#'
#' @description
#' Find all placeholder strings in SQL text. Useful for debugging and
#' validating that post-processing completed successfully.
#'
#' @param sql Character. SQL text
#' @return Character vector. All placeholder strings found
#'
#' @export
extractPlaceholdersFromSql <- function(sql) {
  # Find all strings matching placeholder pattern
  pattern <- paste0(PLACEHOLDER_PREFIX, "[A-Z_]+:[^", PLACEHOLDER_SUFFIX, "]+", PLACEHOLDER_SUFFIX)
  matches <- regmatches(sql, gregexpr(pattern, sql))
  unique(unlist(matches))
}
