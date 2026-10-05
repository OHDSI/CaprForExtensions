#' SQL Post-Processor for Extension Table Substitution
#'
#' @description
#' Post-process CirceR-generated SQL to substitute observation query placeholders
#' with actual extension table logic. This is the core of the v2.0 architecture.
#'
#' @name SqlPostProcessor
NULL

#' Post-Process CirceR SQL with Extension Table Substitution
#'
#' @description
#' Main post-processing function. Takes standard CirceR SQL (with observation
#' placeholders) and substitutes extension table logic. Also handles adding
#' custom concept sets to the #Codesets table.
#'
#' @param circe_sql Character. SQL from CirceR::buildCohortQuery()
#' @param extension_metadata List. Extension metadata from converted queries
#' @param schema Character. Schema for extension tables (default: "@cdm_database_schema")
#'
#' @return Character. Post-processed SQL with extension tables
#'
#' @examples
#' \dontrun{
#' # After CirceR generates SQL with placeholders:
#' final_sql <- postProcessExtensionSql(circe_sql, extension_metadata)
#' }
#'
#' @export
postProcessExtensionSql <- function(
  circe_sql,
  extension_metadata,
  schema = "@cdm_database_schema"
) {
  if (length(extension_metadata) == 0) {
    message("No extension metadata found - returning original SQL")
    return(circe_sql)
  }

  message("Post-processing SQL with ", length(extension_metadata), " extension table substitution(s)...")

  sql <- circe_sql

  # IMPORTANT: Extract placeholder_id -> codeset_id mappings FIRST
  # before we replace placeholders in #Codesets
  message("  Step 1: Extracting placeholder-to-codeset mappings...")
  placeholder_to_codeset <- extractPlaceholderCodesetMappings(sql, extension_metadata)

  # Step 2: Add custom concept sets to #Codesets (if any)
  # Collect all custom concept sets from extension metadata
  custom_concept_sets <- list()
  for (ext_meta in extension_metadata) {
    if (!is.null(ext_meta$custom_concept_sets) && length(ext_meta$custom_concept_sets) > 0) {
      custom_concept_sets <- c(custom_concept_sets, ext_meta$custom_concept_sets)
    }
  }

  if (length(custom_concept_sets) > 0) {
    message("  Step 2: Adding custom concept sets to #Codesets...")
    sql <- substituteCustomConceptSets(sql, custom_concept_sets)
  }

  # Step 3: Process each extension query using the extracted mappings
  for (i in seq_along(extension_metadata)) {
    ext_meta <- extension_metadata[[i]]

    message("  Step ", i + 1 + length(custom_concept_sets), ": Substituting extension table: ", ext_meta$table_name)

    # Get codeset_id for this placeholder
    placeholder_id <- ext_meta$placeholder_id
    codeset_id <- placeholder_to_codeset[[as.character(placeholder_id)]]

    if (is.null(codeset_id)) {
      warning("Could not find codeset_id for placeholder ", placeholder_id)
      next
    }

    # Substitute this extension query
    sql <- substituteExtensionQueryWithCodeset(
      sql,
      ext_meta,
      codeset_id,
      schema
    )
  }

  message("Post-processing complete")

  return(sql)
}

#' Extract Placeholder to Codeset ID Mappings
#'
#' @description
#' Extract the mapping of placeholder_id -> codeset_id from the SQL
#' BEFORE we do any substitutions. This is critical because we need to know
#' which codeset_id CirceR assigned to each placeholder, but once we substitute
#' the custom concept sets, the placeholders are gone.
#'
#' @param sql Character. SQL text with #Codesets table
#' @param extension_metadata List. Extension metadata containing placeholder_ids
#' @return Named list. Keys are placeholder_ids (as strings), values are codeset_ids
#'
#' @keywords internal
extractPlaceholderCodesetMappings <- function(sql, extension_metadata) {
  mappings <- list()

  # Get all placeholder IDs from metadata
  placeholder_ids <- sapply(extension_metadata, function(meta) meta$placeholder_id)

  # For each placeholder, find which codeset_id it was assigned.
  # Primary: use the codeset_id already injected by buildExtendedCohortQuery from the JSON
  # (reliable after Atlas round-trips where real concept IDs replaced placeholders).
  # Fallback: scan the SQL for the placeholder concept ID (original fresh-compile path).
  for (placeholder_id in placeholder_ids) {
    # Primary: pre-computed codeset_id injected from the JSON's ConceptSets
    pre_computed <- NULL
    for (meta in extension_metadata) {
      if (!is.null(meta$placeholder_id) && meta$placeholder_id == placeholder_id &&
          !is.null(meta$codeset_id)) {
        pre_computed <- as.integer(meta$codeset_id)
        break
      }
    }

    if (!is.null(pre_computed)) {
      mappings[[as.character(placeholder_id)]] <- pre_computed
      message("    Mapped placeholder ", placeholder_id, " -> codeset_id ", pre_computed,
              " (from JSON ConceptSets)")
      next
    }

    # Fallback: search SQL for concept_id in (placeholder_id) and scan back to codeset_id
    sql_lines <- strsplit(sql, "\n")[[1]]
    concept_line_idx <- grep(sprintf("concept_id\\s+in\\s*\\(\\s*%d\\s*\\)", placeholder_id),
                             sql_lines, perl = TRUE, ignore.case = TRUE)

    if (length(concept_line_idx) == 0) {
      warning("Could not find placeholder_id ", placeholder_id, " in SQL")
      next
    }

    codeset_id <- NULL
    for (i in concept_line_idx[1]:1) {
      if (grepl("SELECT\\s+(\\d+)\\s+as\\s+codeset_id", sql_lines[i], perl = TRUE, ignore.case = TRUE)) {
        codeset_match <- regexpr("SELECT\\s+(\\d+)\\s+as\\s+codeset_id", sql_lines[i], perl = TRUE, ignore.case = TRUE)
        codeset_text  <- regmatches(sql_lines[i], codeset_match)
        codeset_id    <- as.integer(gsub(".*SELECT\\s+(\\d+)\\s+as\\s+codeset_id.*", "\\1",
                                         codeset_text, perl = TRUE))
        break
      }
    }

    if (!is.null(codeset_id)) {
      mappings[[as.character(placeholder_id)]] <- codeset_id
      message("    Mapped placeholder ", placeholder_id, " -> codeset_id ", codeset_id,
              " (from SQL scan)")
    }
  }

  return(mappings)
}

#' Substitute Extension Query with Known Codeset ID
#'
#' @description
#' Substitute extension query using a pre-extracted codeset_id.
#'
#' @param sql Character. SQL text
#' @param ext_meta List. Extension metadata
#' @param codeset_id Integer. The codeset_id assigned by CirceR
#' @param schema Character. Schema name
#'
#' @return Character. Modified SQL
#'
#' @keywords internal
substituteExtensionQueryWithCodeset <- function(sql, ext_meta, codeset_id, schema) {
  table_name <- ext_meta$table_name
  placeholder_string <- ext_meta$placeholder_string

  message("    Codeset ID: ", codeset_id)
  message("    Target table: ", table_name)

  # Decode placeholder string to get components
  decoded <- decodePlaceholderString(placeholder_string)

  # Generate table alias
  table_alias <- makeTableAlias(table_name)

  # Find the observation block that uses this codeset_id
  pattern <- sprintf(
    "(?s)(-- Begin Observation Criteria.*?FROM\\s+%s\\.OBSERVATION\\s+[a-z].*?codeset_id\\s*=\\s*%d.*?-- End Observation Criteria)",
    escapeRegex(schema),
    codeset_id
  )

  if (!grepl(pattern, sql, perl = TRUE, ignore.case = TRUE)) {
    message("    Could not find observation block with codeset_id ", codeset_id)
    return(sql)
  }

  # Build replacement SQL block
  replacement_sql <- buildExtensionQueryBlock(
    table_name = table_name,
    table_alias = table_alias,
    decoded = decoded,
    schema = schema
  )

  # Replace the entire block
  sql <- gsub(pattern, replacement_sql, sql, perl = TRUE, ignore.case = TRUE)

  message("    ✓ Replaced observation criteria block with extension table query")

  return(sql)
}

#' Substitute Single Extension Query
#'
#' @description
#' Substitute one extension query's placeholders in SQL.
#'
#' @param sql Character. SQL text
#' @param ext_meta List. Extension metadata for one query
#' @param schema Character. Schema name
#'
#' @return Character. Modified SQL
#'
#' @keywords internal
substituteExtensionQuery <- function(sql, ext_meta, schema) {
  placeholder_id <- ext_meta$placeholder_id
  table_name <- ext_meta$table_name
  placeholder_string <- ext_meta$placeholder_string

  message("    Placeholder ID (concept set identifier): ", placeholder_id)
  message("    Target table: ", table_name)

  # Decode placeholder string to get components
  decoded <- decodePlaceholderString(placeholder_string)

  # NEW APPROACH: Replace the entire observation query block
  sql <- substituteEntireObservationBlock(
    sql,
    placeholder_id,
    table_name,
    decoded,
    schema
  )

  return(sql)
}

#' Substitute Entire Observation Query Block
#'
#' @description
#' Find and replace the entire observation criteria block that uses our
#' placeholder with properly constructed extension table SQL.
#'
#' @keywords internal
substituteEntireObservationBlock <- function(sql, placeholder_id, table_name, decoded, schema) {
  # Generate table alias
  table_alias <- makeTableAlias(table_name)

  # IMPORTANT: placeholder_id is the identifier for a placeholder concept set (e.g., 999901714)
  # It's used as a temporary concept_id value in the placeholder concept set during compilation.
  # The concept set can contain one or more actual concept IDs from the extension table.
  # We need to find which CODESET_ID CirceR assigned to this placeholder concept set.
  # Look in the #Codesets INSERT to find: SELECT N as codeset_id ... concept_id in (placeholder_id)

  # Strategy: Find all lines that contain our placeholder_id
  # Then work backwards to find the associated codeset_id

  # First, find the line with our placeholder_id used as a concept_id value
  sql_lines <- strsplit(sql, "\n")[[1]]
  concept_line_idx <- grep(sprintf("concept_id\\s+in\\s*\\(\\s*%d\\s*\\)", placeholder_id),
                           sql_lines, perl = TRUE, ignore.case = TRUE)

  if (length(concept_line_idx) == 0) {
    message("    ERROR: Could not find placeholder_id ", placeholder_id, " in SQL")
    return(sql)
  }

  # Now search backwards from this line to find the "SELECT N as codeset_id" line
  codeset_id <- NULL
  for (i in concept_line_idx[1]:1) {
    if (grepl("SELECT\\s+(\\d+)\\s+as\\s+codeset_id", sql_lines[i], perl = TRUE, ignore.case = TRUE)) {
      codeset_match <- regexpr("SELECT\\s+(\\d+)\\s+as\\s+codeset_id", sql_lines[i], perl = TRUE, ignore.case = TRUE)
      codeset_text <- regmatches(sql_lines[i], codeset_match)
      codeset_id <- as.integer(gsub(".*SELECT\\s+(\\d+)\\s+as\\s+codeset_id.*", "\\1", codeset_text, perl = TRUE))
      break
    }
  }

  if (is.null(codeset_id)) {
    message("    ERROR: Could not find codeset_id for placeholder_id ", placeholder_id)
    return(sql)
  }

  message("    Found codeset_id ", codeset_id, " for placeholder_id ", placeholder_id)

  # Now find the observation block that uses this codeset_id
  pattern <- sprintf(
    "(?s)(-- Begin Observation Criteria.*?FROM\\s+%s\\.OBSERVATION\\s+[a-z].*?codeset_id\\s*=\\s*%d.*?-- End Observation Criteria)",
    escapeRegex(schema),
    codeset_id
  )

  if (!grepl(pattern, sql, perl = TRUE, ignore.case = TRUE)) {
    message("    Could not find observation block with codeset_id ", codeset_id)
    return(sql)
  }

  # Build replacement SQL block
  replacement_sql <- buildExtensionQueryBlock(
    table_name = table_name,
    table_alias = table_alias,
    decoded = decoded,
    schema = schema
  )

  # Replace the entire block
  sql <- gsub(pattern, replacement_sql, sql, perl = TRUE, ignore.case = TRUE)

  message("    ✓ Replaced observation criteria block with extension table query")

  return(sql)
}

#' Make Table Alias
#'
#' @description
#' Build a short SQL alias from a table name: the initials of its underscore-separated
#' tokens (e.g. "my_extension_table" -> "met"), or the first three characters for a
#' single-token name. Avoids reserved words, the aliases Circe uses, and any alias in
#' \code{avoid}.
#'
#' @param table_name Character. Table name
#' @param avoid Character vector. Aliases that must not be returned
#' @return Character. Lowercase alias
#'
#' @keywords internal
makeTableAlias <- function(table_name, avoid = character(0)) {
  tokens <- Filter(nzchar, strsplit(tolower(table_name), "_", fixed = TRUE)[[1]])
  alias <- if (length(tokens) > 1) {
    paste(substr(tokens, 1, 1), collapse = "")
  } else {
    substr(table_name, 1, min(3, nchar(table_name)))
  }
  alias <- tolower(alias)

  reserved <- c("c", "cs", "o", "e", "as", "by", "in", "is", "on", "or", "to", "if", "no", "of", "at")
  blocked <- c(reserved, tolower(avoid))
  while (alias %in% blocked) {
    alias <- paste0(alias, "x")
  }
  alias
}

#' Build Extension Table Query Block
#'
#' @description
#' Build the complete SQL block for an extension table query. When the placeholder
#' carries a join (registered \code{parent_table}), person and visit fields are read from
#' the parent table, and the date fields come from the parent when \code{date_source}
#' is "parent".
#'
#' @keywords internal
buildExtensionQueryBlock <- function(table_name, table_alias, decoded, schema) {
  # Resolve the join (single parent level), if any
  parent_table <- NULL
  join_field <- NULL
  parent_alias <- NULL
  if (!is.null(decoded$join)) {
    join_parts <- strsplit(decoded$join, ".", fixed = TRUE)[[1]]
    if (length(join_parts) == 3) {
      parent_table <- join_parts[2]
      join_field <- join_parts[3]
      parent_alias <- makeTableAlias(parent_table, avoid = table_alias)
    }
  }
  has_join <- !is.null(parent_table)

  # Alias of the table holding person/visit fields, and of the table holding the dates
  person_alias <- if (has_join) parent_alias else table_alias
  date_alias <- if (has_join && identical(decoded$date_source, "parent")) parent_alias else table_alias

  # Date references (a qualified field is used as-is)
  qualify <- function(field, alias) {
    if (grepl("\\.", field)) field else paste0(alias, ".", field)
  }
  start_ref <- qualify(decoded$date_field, date_alias)

  # Event end date: the registered end date field when there is one (falling back to start + 1 day
  # when it is NULL, like Circe does for CDM events), otherwise start + 1 day
  if (!is.null(decoded$end_date_field) && nzchar(decoded$end_date_field)) {
    end_ref <- qualify(decoded$end_date_field, date_alias)
    end_sql <- sprintf("COALESCE(%s, DATEADD(day,1,%s))", end_ref, start_ref)
  } else {
    end_sql <- sprintf("DATEADD(day,1,%s)", start_ref)
  }

  # Key fields (defaults follow the OMOP conventions)
  event_id_field <- if (!is.null(decoded$primary_key_field)) decoded$primary_key_field else paste0(table_name, "_id")
  person_field <- if (!is.null(decoded$person_id_field)) decoded$person_id_field else "person_id"
  visit_sql <- if (!is.null(decoded$visit_id_field)) {
    sprintf("%s.%s", person_alias, decoded$visit_id_field)
  } else {
    "CAST(NULL AS INTEGER)"
  }

  sql <- sprintf(
"-- Begin %s Criteria (Extension Table)
select C.person_id, C.event_id, C.start_date, C.END_DATE,
       C.visit_occurrence_id, C.start_date as sort_date
from
(
  select %s.%s as person_id, %s.%s as event_id, %s as visit_occurrence_id,
         %s as start_date, %s as end_date
  FROM %s.%s %s",
    table_name,
    person_alias, person_field, table_alias, event_id_field, visit_sql,
    start_ref, end_sql,
    schema, table_name, table_alias
  )

  if (has_join) {
    sql <- paste0(sql, sprintf(
      "\n  JOIN %s.%s %s ON %s.%s = %s.%s",
      schema, parent_table, parent_alias, table_alias, join_field, parent_alias, join_field
    ))
  }

  # Add WHERE clause with concept filter and value filters
  where_clauses <- c()

  if (!is.null(decoded$concept_filter)) {
    where_clauses <- c(where_clauses, decoded$concept_filter)
  }

  if (length(decoded$value_filters) > 0) {
    where_clauses <- c(where_clauses, decoded$value_filters)
  }

  if (length(where_clauses) > 0) {
    sql <- paste0(sql, "\n  WHERE ", paste(where_clauses, collapse = "\n  AND "))
  }

  # Close the block
  sql <- paste0(sql, "\n) C\n\n\n-- End ", table_name, " Criteria (Extension Table)")

  return(sql)
}

#' Substitute Table References
#'
#' @description
#' Replace observation table with extension table within specific SQL blocks,
#' handling joins if needed. Only substitutes within the relevant Observation
#' Criteria block to avoid affecting legitimate observation queries.
#'
#' Based on Circe SQL pattern:
#' ```sql
#' -- Begin Observation Criteria
#' SELECT O.person_id, O.observation_id as event_id, ...
#' FROM @cdm_database_schema.OBSERVATION O
#' JOIN #Codesets cs on (O.observation_concept_id = cs.concept_id and cs.codeset_id = 999)
#' -- End Observation Criteria
#' ```
#'
#' @keywords internal
substituteTableReferences <- function(sql, placeholder_id, table_name, schema, join_info) {
  # Generate table alias (first 2-3 chars of table name, uppercase to match Circe style)
  table_alias <- toupper(substr(table_name, 1, min(3, nchar(table_name))))

  # Find observation blocks that reference this placeholder_id
  # Look for blocks with: JOIN #Codesets cs on (...cs.codeset_id = placeholder_id)

  # First, check if this observation query uses our placeholder
  # Pattern: JOIN #Codesets cs on (O.observation_concept_id = cs.concept_id and cs.codeset_id = placeholder_id)
  check_pattern <- sprintf(
    "observation_concept_id\\s*=\\s*cs\\.concept_id\\s+and\\s+cs\\.codeset_id\\s*=\\s*%d",
    placeholder_id
  )

  if (!grepl(check_pattern, sql, perl = TRUE, ignore.case = TRUE)) {
    # This placeholder is not in the SQL, skip
    return(sql)
  }

  if (is.null(join_info)) {
    # Simple case: direct table substitution
    # FROM @cdm_database_schema.OBSERVATION O
    # → FROM @cdm_database_schema.my_extension_table met

    # Use case-insensitive matching and preserve schema placeholder format
    # Pattern needs to handle newlines and other whitespace flexibly
    # Simplified: match any schema prefix, not just the specific one
    pattern <- sprintf(
      "(FROM\\s+[^.]+\\.)(OBSERVATION)(\\s+[A-Za-z]+)(.*?cs\\.codeset_id\\s*=\\s*%d)",
      placeholder_id
    )

    replacement <- sprintf(
      "\\1%s %s\\4",
      table_name,
      table_alias
    )

    # Use DOTALL flag ((?s)) to make . match newlines
    sql_modified <- gsub(paste0("(?s)", pattern), replacement, sql, perl = TRUE, ignore.case = TRUE)

    # Debug: Check if substitution happened
    if (identical(sql, sql_modified)) {
      message("    WARNING: Table substitution pattern did not match. Trying alternate pattern...")
      # Try a simpler pattern
      pattern2 <- sprintf(
        "FROM\\s+([^\\s]+)\\.OBSERVATION\\s+([A-Za-z]+)(.*?cs\\.codeset_id\\s*=\\s*%d)",
        placeholder_id
      )
      replacement2 <- sprintf(
        "FROM \\1.%s %s\\3",
        table_name,
        table_alias
      )
      sql_modified <- gsub(paste0("(?s)", pattern2), replacement2, sql, perl = TRUE, ignore.case = TRUE)
    }

    sql <- sql_modified

    # Now replace all column references that use the old observation alias
    # The observation domain typically uses 'o' or 'O' as alias
    # Replace observation-specific column patterns within the query that uses our placeholder
    # Pattern: find the SELECT...FROM block containing our codeset_id

    # Target observation-specific columns that would have the 'o' alias
    obs_columns <- c(
      "person_id", "observation_id", "observation_concept_id",
      "visit_occurrence_id", "value_as_number", "value_as_string",
      "value_as_concept_id", "qualifier_concept_id", "unit_concept_id",
      "provider_id", "observation_source_value", "observation_source_concept_id",
      "unit_source_value", "qualifier_source_value"
    )

    # Replace o.column with wav.column (lowercase alias)
    for (col in obs_columns) {
      sql <- gsub(
        paste0("\\bo\\.", col, "\\b"),
        paste0(tolower(table_alias), ".", col),
        sql,
        perl = TRUE
      )
    }

  } else {
    # Complex case: need to add join to parent table
    # Example: a child table needs to join to its parent table for person_id

    # Parse join info
    join_parts <- strsplit(join_info, ".", fixed = TRUE)[[1]]
    parent_table <- join_parts[1]
    join_field <- join_parts[2]

    parent_alias <- toupper(substr(parent_table, 1, min(3, nchar(parent_table))))

    # Pattern to match FROM observation and the context
    # We'll replace and add the JOIN
    pattern <- sprintf(
      "(FROM\\s+%s\\.)(OBSERVATION)(\\s+)([A-Za-z]+)([\\s\\n]+JOIN\\s+#Codesets\\s+cs\\s+on[^)]+cs\\.codeset_id\\s*=\\s*%d)",
      escapeRegex(schema),
      placeholder_id
    )

    # Capture the old alias to replace it later
    old_alias_match <- regmatches(sql, regexec(pattern, sql, perl = TRUE, ignore.case = TRUE))
    if (length(old_alias_match) > 0 && length(old_alias_match[[1]]) > 4) {
      old_alias <- old_alias_match[[1]][5]  # The captured alias

      replacement <- sprintf(
        "\\1%s\\3%s\n  JOIN %s.%s %s ON %s.%s = %s.%s\\5",
        table_name,
        table_alias,
        schema,
        parent_table,
        parent_alias,
        table_alias,
        join_field,
        parent_alias,
        join_field
      )

      sql <- gsub(pattern, replacement, sql, perl = TRUE, ignore.case = TRUE)

      # Replace old alias references with parent alias for person_id, visit_occurrence_id, dates
      # This ensures these fields come from the parent table (parent)
      # instead of the extension table (child)
      sql <- gsub(
        sprintf("\\b%s\\.(person_id|visit_occurrence_id)", old_alias),
        sprintf("%s.\\1", parent_alias),
        sql,
        perl = TRUE,
        ignore.case = TRUE
      )
    }
  }

  return(sql)
}

#' Substitute Concept Filter
#'
#' @description
#' Replace observation_concept_id filter with actual concept filter.
#'
#' @keywords internal
substituteConceptFilter <- function(sql, placeholder_id, concept_filter) {
  # CirceR generates: JOIN #Codesets cs on (o.observation_concept_id = cs.concept_id and cs.codeset_id = placeholder_id)
  # We need to replace the observation_concept_id with the extension table's concept field
  # And optionally remove the codeset join if we're using direct concept IDs

  # Pattern: o.observation_concept_id = cs.concept_id and cs.codeset_id = placeholder_id
  # Replace observation_concept_id reference with the extension table's concept field
  # This pattern finds the observation_concept_id within the JOIN clause that uses our placeholder

  pattern <- sprintf(
    "([a-z]+\\.)observation_concept_id(\\s*=\\s*cs\\.concept_id\\s+and\\s+cs\\.codeset_id\\s*=\\s*%d)",
    placeholder_id
  )

  # Extract the table alias and concept field name from concept_filter
  # concept_filter is like "wav.algorithm_concept_id IN (2000000051)"
  # We just want to replace "observation_concept_id" with "algorithm_concept_id"

  # Simple approach: just replace the column name
  if (grepl("\\.", concept_filter)) {
    # Extract just the column name from the filter
    concept_field <- sub("^[^.]+\\.", "", sub("\\s+IN.*$", "", concept_filter))

    replacement <- sprintf("\\1%s\\2", concept_field)
    sql <- gsub(pattern, replacement, sql, perl = TRUE)
  }

  return(sql)
}

#' Add Value Filters to WHERE Clause
#'
#' @description
#' Add value filters to the WHERE clause for the extension table query.
#' Since we're not using value_as_string placeholder, we add filters after
#' the concept ID filter.
#'
#' @keywords internal
addValueFiltersToWhere <- function(sql, placeholder_id, value_filters) {
  # Build filter SQL
  filter_sql <- paste(value_filters, collapse = " AND ")

  # Find the observation query that uses our placeholder_id
  # Pattern: WHERE ... cs.codeset_id = placeholder_id
  # We want to add our filters after this

  # Look for the codeset join pattern
  pattern <- sprintf(
    "(cs\\.codeset_id\\s*=\\s*%d)(\\s*\\))",
    placeholder_id
  )

  # Add our filters: cs.codeset_id = 999) AND value_as_number >= 450
  replacement <- sprintf("\\1\\2\nAND %s", filter_sql)

  sql <- gsub(pattern, replacement, sql, perl = TRUE)

  return(sql)
}

#' Substitute Date Field
#'
#' @description
#' Replace observation_date with actual date field from extension table.
#'
#' @keywords internal
substituteDateField <- function(sql, table_name, date_field) {
  # Get table alias from context
  table_alias <- substr(table_name, 1, min(3, nchar(table_name)))

  # Pattern: o.observation_date
  # Replace with: alias.event_start_datetime

  # Handle different table aliases that might reference the date
  # Common patterns in CirceR SQL:
  # - <alias>.observation_date
  # - observation_date (without alias in some contexts)

  patterns <- c(
    # With alias
    "[a-z]+\\.observation_date",
    # Without alias
    "\\bobservation_date\\b"
  )

  replacement <- paste0(table_alias, ".", date_field)

  for (pattern in patterns) {
    sql <- gsub(pattern, replacement, sql, perl = TRUE)
  }

  return(sql)
}

#' Escape Regular Expression Special Characters
#'
#' @keywords internal
escapeRegex <- function(string) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", string)
}

#' Validate Post-Processed SQL
#'
#' @description
#' Check if post-processing was successful by verifying all placeholder strings
#' have been substituted. Note: Observation tables may legitimately coexist with
#' extension tables, so we only check for placeholder strings, not observation
#' table presence.
#'
#' @param sql Character. Post-processed SQL
#' @return Logical. TRUE if validation passes, FALSE otherwise
#'
#' @keywords internal
validatePostProcessedSql <- function(sql) {
  # Check for remaining placeholders (this is the key validation)
  remaining_placeholders <- extractPlaceholdersFromSql(sql)

  if (length(remaining_placeholders) > 0) {
    warning("Post-processing incomplete. Remaining placeholders:\n",
            paste(remaining_placeholders, collapse = "\n"))
    return(FALSE)
  }

  # Note: We no longer use value_as_string placeholders, so we don't check for them
  # The placeholder IDs (used as concept set identifiers) are sufficient for identification

  # NOTE: We do NOT check for observation table presence, as legitimate
  # observation queries may coexist with extension tables in the same cohort

  return(TRUE)
}

#' Substitute Custom Concept Sets in #Codesets Table
#'
#' @description
#' Add custom concept sets for extension tables to Circe's #Codesets temporary table.
#' Circe uses #Codesets to store all concept sets with their codeset_id. Custom
#' concept sets for extension tables need to be added here and referenced by their ID.
#'
#' Based on actual Circe SQL pattern:
#' ```sql
#' INSERT INTO #Codesets (codeset_id, concept_id)
#' SELECT 13 as codeset_id, c.concept_id FROM (...) C
#' UNION ALL
#' SELECT 14 as codeset_id, c.concept_id FROM (...) C
#' UNION ALL
#' SELECT 15 as codeset_id, c.concept_id FROM (...) C;
#' ```
#'
#' @param sql Character. SQL containing #Codesets table creation
#' @param custom_concept_sets List. Custom concept sets with structure:
#'   list(
#'     list(codeset_id = 999, concept_ids = c(2000000051, 2000000052), name = "Custom QTc Algorithms"),
#'     ...
#'   )
#' @return Character. Modified SQL with custom concept sets added to #Codesets
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Custom concept set for extension table algorithm concepts
#' custom_sets <- list(
#'   list(
#'     codeset_id = 999,
#'     concept_ids = c(2000000051, 2000000052, 2000000053),
#'     name = "QTc Calculation Algorithms"
#'   )
#' )
#' sql <- substituteCustomConceptSets(circe_sql, custom_sets)
#' }
substituteCustomConceptSets <- function(sql, custom_concept_sets) {
  if (length(custom_concept_sets) == 0) {
    return(sql)
  }

  # For each custom concept set, find which codeset_id CirceR assigned to its placeholder
  # and replace that codeset's query with the real concepts

  for (i in seq_along(custom_concept_sets)) {
    cset <- custom_concept_sets[[i]]

    placeholder_id <- cset$placeholder_id
    concept_ids <- cset$concept_ids

    if (is.null(placeholder_id) || is.null(concept_ids)) {
      next
    }

    message("    Replacing placeholder ", placeholder_id, " with ", length(concept_ids), " real concept(s)")

    # Find the line that queries for this placeholder in CONCEPT table
    # Pattern: concept_id in (placeholder_id)
    placeholder_pattern <- sprintf("concept_id\\s+in\\s*\\(\\s*%d\\s*\\)", placeholder_id)

    # Build replacement: direct SELECT of real concept IDs (not from CONCEPT table)
    inner_selects <- sapply(concept_ids, function(cid) {
      sprintf("  select %s as concept_id", cid)
    })
    inner_sql <- paste(inner_selects, collapse = "\nUNION\n")

    # The replacement query
    replacement_query <- sprintf("select distinct I.concept_id FROM\n(\n%s\n) I", inner_sql)

    # Replace the CONCEPT table query with our direct SELECT
    # Original: select concept_id from omopwave.CONCEPT where (concept_id in (999905923))
    # Replace with: select distinct I.concept_id FROM\n(\n  select 2082499949 as concept_id\n) I

    # Find and replace the pattern
    sql <- gsub(
      sprintf("select\\s+concept_id\\s+from\\s+\\S+\\.CONCEPT\\s+where\\s+\\(%s\\)", placeholder_pattern),
      replacement_query,
      sql,
      ignore.case = TRUE,
      perl = TRUE
    )
  }

  return(sql)
}

#' Extract Circe SQL Block by Comment Markers
#'
#' @description
#' Circe SQL contains comment blocks like "-- Begin Observation Criteria" and
#' "-- End Observation Criteria". This function extracts content between markers.
#'
#' @param sql Character. Full SQL text
#' @param begin_marker Character. Beginning comment marker (e.g., "-- Begin Observation Criteria")
#' @param end_marker Character. Ending comment marker (e.g., "-- End Observation Criteria")
#' @return Character. Extracted block content (or NULL if not found)
#'
#' @keywords internal
extractCirceBlock <- function(sql, begin_marker, end_marker) {
  # Escape regex special characters in markers
  begin_escaped <- escapeRegex(begin_marker)
  end_escaped <- escapeRegex(end_marker)

  # Pattern to extract content between markers
  pattern <- paste0(begin_escaped, "(.*?)", end_escaped)

  matches <- regmatches(sql, regexpr(pattern, sql, perl = TRUE))

  if (length(matches) == 0) {
    return(NULL)
  }

  return(matches[1])
}

#' Replace Circe SQL Block by Comment Markers
#'
#' @description
#' Replace content between Circe comment markers with new content.
#'
#' @param sql Character. Full SQL text
#' @param begin_marker Character. Beginning comment marker
#' @param end_marker Character. Ending comment marker
#' @param new_content Character. New content to insert
#' @return Character. Modified SQL
#'
#' @keywords internal
replaceCirceBlock <- function(sql, begin_marker, end_marker, new_content) {
  begin_escaped <- escapeRegex(begin_marker)
  end_escaped <- escapeRegex(end_marker)

  # Pattern to match entire block including markers
  pattern <- paste0(
    "(", begin_escaped, ")",
    ".*?",
    "(", end_escaped, ")"
  )

  # Replacement keeps markers but replaces content
  replacement <- paste0(
    "\\1\n",
    new_content,
    "\n\\2"
  )

  gsub(pattern, replacement, sql, perl = TRUE)
}

#' Preview Substitutions Without Applying
#'
#' @description
#' Show what substitutions would be made without actually modifying SQL.
#' Useful for debugging.
#'
#' @param extension_metadata List. Extension metadata
#' @return Character. Summary of planned substitutions
#'
#' @export
previewSubstitutions <- function(extension_metadata) {
  if (length(extension_metadata) == 0) {
    return("No extension queries to substitute")
  }

  output <- c("Planned Substitutions:\n", "====================\n\n")

  for (i in seq_along(extension_metadata)) {
    ext_meta <- extension_metadata[[i]]
    decoded <- decodePlaceholderString(ext_meta$placeholder_string)

    output <- c(output, paste0(i, ". ", ext_meta$table_name, ":\n"))
    output <- c(output, paste0("   Placeholder ID: ", ext_meta$placeholder_id, "\n"))

    if (!is.null(decoded$concept_filter)) {
      output <- c(output, paste0("   Concept Filter: ", decoded$concept_filter, "\n"))
    }

    if (length(decoded$value_filters) > 0) {
      output <- c(output, "   Value Filters:\n")
      for (filter in decoded$value_filters) {
        output <- c(output, paste0("     - ", filter, "\n"))
      }
    }

    if (!is.null(decoded$join)) {
      output <- c(output, paste0("   Join: ", decoded$join, "\n"))
    }

    if (!is.null(decoded$date_field)) {
      output <- c(output, paste0("   Date Field: ", decoded$date_field, "\n"))
    }

    if (!is.null(ext_meta$custom_concept_sets) && length(ext_meta$custom_concept_sets) > 0) {
      output <- c(output, "   Custom Concept Sets:\n")
      for (cset in ext_meta$custom_concept_sets) {
        output <- c(output, paste0(
          "     - Codeset ID ", cset$codeset_id, ": ",
          length(cset$concept_ids), " concepts\n"
        ))
      }
    }

    output <- c(output, "\n")
  }

  paste(output, collapse = "")
}
