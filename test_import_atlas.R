library(CaprForExtensions)
library(Capr)

atlas_path <- "C:/Users/JHoughta/Code/CaprForExtensions/atlas_out.json"
cat("=== Importing", atlas_path, "===\n\n")

atlas_json <- paste(readLines(atlas_path), collapse = "\n")

# ── Import ─────────────────────────────────────────────────────────────────────
restored_json <- importFromAtlasJson(atlas_json, pretty = TRUE)
restored      <- jsonlite::fromJSON(restored_json, simplifyVector = FALSE)

# ── 1. ExtensionMetadata restored ─────────────────────────────────────────────
cat("--- Check 1: ExtensionMetadata restored ---\n")
stopifnot("ExtensionMetadata missing" =
            !is.null(restored$ExtensionMetadata))
stopifnot("Wrong number of extension queries" =
            length(restored$ExtensionMetadata$extensionQueries) == 1L)
cat("PASS\n\n")

# ── 2. Key metadata fields ─────────────────────────────────────────────────────
cat("--- Check 2: Key metadata fields ---\n")
q <- restored$ExtensionMetadata$extensionQueries[[1]]
cat("  name        :", q$name,        "\n")
cat("  table_name  :", q$table_name,  "\n")
cat("  date_field  :", q$date_field,  "\n")
cat("  concept_ids :", paste(q$concept_ids, collapse = ", "), "\n")
stopifnot("name mismatch"       = q$name       == "Waveform Feature")
stopifnot("table_name mismatch" = q$table_name == "waveform_feature")
stopifnot("date_field mismatch" = q$date_field == "waveform_occurrence_start_datetime")
stopifnot("concept_id mismatch" = as.integer(q$concept_ids) == 2082499949L)
cat("PASS\n\n")

# ── 3. Concept set contains real concept ID, not placeholder ──────────────────
cat("--- Check 3: Concept set contains real concept ID ---\n")
cs_items    <- restored$ConceptSets[[1]]$expression$items
concept_ids <- sapply(cs_items, function(item) as.integer(item$concept$CONCEPT_ID))
cat("  Concept IDs in concept set:", paste(concept_ids, collapse = ", "), "\n")
stopifnot("Placeholder ID leaked into concept set" =
            !any(concept_ids >= 999900000L & concept_ids <= 999999999L))
stopifnot("Real concept ID 2082499949 missing from concept set" =
            2082499949L %in% concept_ids)
cat("PASS\n\n")

# ── 4. Atlas edits (InclusionRule + CensoringCriteria) preserved ──────────────
cat("--- Check 4: Atlas edits preserved ---\n")
# Atlas added one InclusionRule (ConditionEra, no name)
stopifnot("InclusionRules missing"          = length(restored$InclusionRules)    == 1L)
incl_criteria <- restored$InclusionRules[[1]]$expression$CriteriaList
stopifnot("InclusionRule ConditionEra missing" =
            !is.null(incl_criteria[[1]]$Criteria$ConditionEra))
cat("  InclusionRules     : 1 (ConditionEra)\n")

# Atlas added one CensoringCriteria (Death)
stopifnot("CensoringCriteria missing"       = length(restored$CensoringCriteria) == 1L)
stopifnot("CensoringCriteria Death missing" =
            !is.null(restored$CensoringCriteria[[1]]$Death))
cat("  CensoringCriteria  : 1 (Death)\n")
cat("PASS\n\n")

# ── 5. Write restored JSON for inspection ─────────────────────────────────────
out_path <- "C:/Users/JHoughta/Code/CaprForExtensions/restored_from_atlas.json"
writeLines(restored_json, out_path)
cat("Restored JSON written to:", out_path, "\n\n")

# ── 6. SQL generation from restored JSON ──────────────────────────────────────
cat("--- Check 5: SQL generation from restored JSON ---\n")
registerCustomDomain(
  domain_id        = "waveformFeature",
  domain_name      = "Waveform Feature",
  table            = "waveform_feature",
  start_date_field = "waveform_occurrence_start_datetime",
  concept_id_field = "algorithm_concept_id",
  overwrite        = TRUE
)

sql_options <- buildExtensionOptions(
  cohort_id     = 1,
  cdm_schema    = "@cdm_database_schema",
  target_schema = "@work_database_schema",
  target_table  = "cohort"
)

sql <- buildExtendedCohortQuery(restored_json, sql_options)

remaining <- extractPlaceholdersFromSql(sql)
if (length(remaining) == 0) {
  cat("  All placeholders substituted.\n")
} else {
  stop("Unsubstituted placeholders remain: ", paste(remaining, collapse = ", "))
}

stopifnot("waveform_feature not found in SQL" =
            grepl("waveform_feature", sql, fixed = TRUE))
cat("  Extension table 'waveform_feature' found in SQL.\n")
cat("PASS\n\n")

cat("=== All checks passed ===\n")
