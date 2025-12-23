library(CaprForExtensions)
library(Capr)

message("=== CaprForExtensions Waveform Example ===\n")

# ----

message("STEP 1: Registering waveform_feature as custom domain...")

registerCustomDomain(
  domain_id = "waveformFeature",
  domain_name = "Waveform Feature",
  table = "waveform_feature",
  schema = "@cdm_database_schema",
  person_id_field = "person_id",  # NOTE: waveform_feature doesn't have person_id directly!
  start_date_field = "waveform_feature_start_timestamp",
  concept_id_field = "algorithm_concept_id"
)

message("Registered: waveformFeature\n")

# ----

message("STEP 2: Defining cohort with multiple waveform feature criteria...")

# Define concepts
# Standard OMOP concept (will use CirceR's codeset)
mi_concepts <- cs(descendants(4329847), name = "Myocardial Infarction")

# Custom extension table concepts (2000000000+ range for custom concepts)
# These will require custom concept set insertion in the SQL
qtc_algorithm <- cs(2000000051, name = "Bazett QTc Formula")
hr_algorithm <- cs(2000000052, name = "Heart Rate Detection Algorithm")
rr_algorithm <- cs(2000000053, name = "RR Interval Algorithm")

# Build cohort: MI patients with multiple abnormal cardiac waveform features
# This demonstrates multiple extension table queries in one cohort
# All 3 waveform criteria must be present (AND logic)
mi_with_waveform_cohort <- cohort(
  entry = entry(
    # Standard CDM criterion
    conditionOccurrence(mi_concepts),

    # First extension criterion - prolonged QTc
    # This will be converted to observation placeholder #1
    waveformFeature(
      qtc_algorithm,
      valueAsNumber = numericValue("value_as_number", ">=", 450)
    ),

    # Second extension criterion - elevated heart rate (tachycardia)
    # This will be converted to observation placeholder #2
    waveformFeature(
      hr_algorithm,
      valueAsNumber = numericValue("value_as_number", ">", 100)
    ),

    # Third extension criterion - shortened RR interval
    # This will be converted to observation placeholder #3
    waveformFeature(
      rr_algorithm,
      valueAsNumber = numericValue("value_as_number", "<", 600)
    )
  ),
  exit = exit(endStrategy = observationExit())
)

message("Cohort defined with multiple criteria:")
message("  1. Condition occurrence (MI) - standard CDM")
message("  2. Waveform feature (QTc >= 450ms) - prolonged QT interval")
message("  3. Waveform feature (HR > 100 bpm) - tachycardia")
message("  4. Waveform feature (RR < 600ms) - shortened RR interval")
message("  Total: 3 extension table queries to substitute\n")

# ----


message("STEP 3: Compiling cohort...")
message("  (This converts waveformFeature queries to observation queries with placeholders)\n")

# This will:
# 1. Detect all waveformFeature custom domain queries
# 2. Convert each to observation query with unique placeholder concept ID
# 3. Encode extension table info in placeholder strings
# 4. Compile using standard Capr
cohort_json <- compileExtendedCohort(mi_with_waveform_cohort, pretty = TRUE)

message("\n--- Compiled JSON (excerpt) ---")
json_parsed <- jsonlite::fromJSON(cohort_json)

if (!is.null(json_parsed$ExtensionMetadata)) {
  message("ExtensionMetadata found:")
  message("  Version: ", json_parsed$ExtensionMetadata$version)
  message("  Extension queries: ", length(json_parsed$ExtensionMetadata$extensionQueries))

  if (length(json_parsed$ExtensionMetadata$extensionQueries) > 0) {
    for (i in seq_along(json_parsed$ExtensionMetadata$extensionQueries[,1])) {
      ext_meta <- json_parsed$ExtensionMetadata$extensionQueries[i,]
      message("  Extension ", i, ":")
      message("    Table: ", ext_meta$table_name)
      message("    Placeholder ID: ", ext_meta$placeholder_id)
      message("    Placeholder string: ", substr(ext_meta$placeholder_string, 1, 60), "...")
    }
  }
}

message("\nCompilation complete\n")

# ----

message("STEP 4: Generating SQL via CirceR...")
message("  (CirceR will treat the placeholder as a standard observation query)\n")

options <- buildExtensionOptions(
  cohort_id = 1,
  cdm_schema = "@cdm_database_schema",
  target_schema = "@results_database_schema",
  target_table = "cohort"
)

# This will:
# 1. Extract extension metadata from JSON
# 2. Call CirceR::buildCohortQuery() (generates SQL with observation placeholders)
# 3. Post-process SQL to substitute extension table logic
# 4. Validate that all placeholders were replaced
final_sql <- buildExtendedCohortQuery(cohort_json, options)

message("\nSQL generated\n")

# ----

message("STEP 5: Examining results...\n")

# Check for remaining placeholders (should be none)
remaining_placeholders <- extractPlaceholdersFromSql(final_sql)

if (length(remaining_placeholders) == 0) {
  message("All placeholders successfully substituted!")
} else {
  message("Warning: ", length(remaining_placeholders), " placeholders remain:")
  for (ph in remaining_placeholders) {
    message("  - ", ph)
  }
}

# ----


message("STEP 6: Saving SQL...\n")

output_file <- "v2_proof_of_concept_cohort.sql"
writeLines(final_sql, output_file)
message("SQL saved to: ", output_file, "\n")

message("--- First 50 lines of generated SQL ---")
sql_lines <- strsplit(final_sql, "\n")[[1]]
cat(paste(head(sql_lines, 50), collapse = "\n"))
message("\n... (", length(sql_lines) - 50, " more lines) ...\n")


# ----

message("STEP 7: Preview what substitutions were made...\n")

ext_metadata <- extractExtensionMetadata(cohort_json)
if (!is.null(ext_metadata$extensionQueries)) {
  preview_text <- previewSubstitutions(ext_metadata$extensionQueries)
  cat(preview_text)
}
