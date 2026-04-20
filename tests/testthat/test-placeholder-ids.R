# Tests for placeholder concept set ID generation and Atlas import/export round-trips
# Focus: issues found in PlaceholderEncoding.R and ExtensionCompiler.R
#
# Issues documented and fixed:
#  1. Hash collision in generatePlaceholderConceptSetId (renamed from generatePlaceholderConceptId;
#     was using sum-based hash, now uses digest::digest)
#  2. CodesetId type coercion in exportToAtlasJson (double vs integer equality)
#  3. Partial-name matching ambiguity in exportToAtlasJson (grepl substring → exact match)
#  4. NULL CodesetId guard in importFromAtlasJson
#  5. Search scope was limited to PrimaryCriteria (now also covers AdditionalCriteria/InclusionRules)

# ---------------------------------------------------------------------------
# Helpers – minimal JSON fixtures that mimic compileExtendedCohort() output
# ---------------------------------------------------------------------------

#' Build a minimal CaprForExtensions JSON string with one extension query.
#'
#' @param query_name Name to embed in the extension metadata.
#' @param cs_id Numeric. The concept set id CirceR would assign (sequential integer).
#' @param placeholder_id Numeric. The placeholder concept id baked into the concept set items.
make_single_extension_json <- function(query_name = "qtc_query",
                                        cs_id = 0,
                                        placeholder_id = 999900042L) {
  jsonlite::toJSON(
    list(
      ConceptSets = list(
        list(
          id   = cs_id,
          name = paste0("Extension:", query_name),
          expression = list(
            items = list(
              list(concept = list(CONCEPT_ID = placeholder_id, CONCEPT_NAME = "Placeholder"))
            )
          )
        )
      ),
      PrimaryCriteria = list(
        CriteriaList = list(
          list(
            Observation = list(
              CodesetId = cs_id
            )
          )
        ),
        ObservationWindow = list(PriorDays = 0, PostDays = 0)
      ),
      ExtensionMetadata = list(
        extensionQueries = list(
          list(
            name        = query_name,
            tableName   = "waveform_feature",
            dateField   = "waveform_feature_start_timestamp",
            personIdField = "person_id"
          )
        ),
        extensionLogic = "ALL",
        version = "2.0"
      )
    ),
    auto_unbox = TRUE, null = "null", pretty = FALSE
  ) |> as.character()
}

#' Build a JSON with TWO extension queries whose names share a common prefix.
#' The concept sets are ordered with the LONGER name first so that grepl(short_name, ...)
#' will match the wrong (longer-named) concept set before the correct one.
make_two_similar_name_json <- function() {
  jsonlite::toJSON(
    list(
      ConceptSets = list(
        # qtc_prolonged listed FIRST so that grepl("qtc", name) hits it before "Extension:qtc"
        list(id = 0L, name = "Extension:qtc_prolonged",
             expression = list(items = list(list(concept = list(CONCEPT_ID = 999900002L))))),
        list(id = 1L, name = "Extension:qtc",
             expression = list(items = list(list(concept = list(CONCEPT_ID = 999900001L)))))
      ),
      PrimaryCriteria = list(
        CriteriaList = list(
          list(Observation = list(CodesetId = 0L)),   # CodesetId 0 = qtc_prolonged
          list(Observation = list(CodesetId = 1L))    # CodesetId 1 = qtc
        ),
        ObservationWindow = list(PriorDays = 0, PostDays = 0)
      ),
      ExtensionMetadata = list(
        extensionQueries = list(
          list(name = "qtc",            tableName = "waveform_feature", dateField = "ts"),
          list(name = "qtc_prolonged",  tableName = "waveform_feature", dateField = "ts")
        ),
        extensionLogic = "ALL",
        version = "2.0"
      )
    ),
    auto_unbox = TRUE, null = "null", pretty = FALSE
  ) |> as.character()
}

#' Build a JSON where the extension observation is in InclusionRules, not PrimaryCriteria.
make_inclusion_rule_json <- function(query_name = "qtc_incl", cs_id = 0L) {
  jsonlite::toJSON(
    list(
      ConceptSets = list(
        list(id = cs_id, name = paste0("Extension:", query_name),
             expression = list(items = list(list(concept = list(CONCEPT_ID = 999900099L)))))
      ),
      PrimaryCriteria = list(
        CriteriaList = list(
          list(
            ConditionOccurrence = list(CodesetId = 99L)
          )
        ),
        ObservationWindow = list(PriorDays = 0, PostDays = 0)
      ),
      InclusionRules = list(
        list(
          name = "has_qtc",
          expression = list(
            Type = "ALL",
            CriteriaList = list(
              list(Criteria = list(
                Observation = list(CodesetId = cs_id)
              ))
            )
          )
        )
      ),
      ExtensionMetadata = list(
        extensionQueries = list(
          list(name = query_name, tableName = "waveform_feature", dateField = "ts")
        ),
        extensionLogic = "ALL",
        version = "2.0"
      )
    ),
    auto_unbox = TRUE, null = "null", pretty = FALSE
  ) |> as.character()
}

#' Build Atlas JSON where an Observation has no CodesetId field at all.
make_no_codeset_id_json <- function() {
  jsonlite::toJSON(
    list(
      ConceptSets = list(
        list(id = 0L, name = "Extension:some_query",
             expression = list(items = list()))
      ),
      PrimaryCriteria = list(
        CriteriaList = list(
          list(Observation = list())   # <-- no CodesetId
        ),
        ObservationWindow = list(PriorDays = 0, PostDays = 0)
      )
      # No ExtensionMetadata – simulates already-exported Atlas JSON
    ),
    auto_unbox = TRUE, null = "null", pretty = FALSE
  ) |> as.character()
}

# ============================================================
# 1. generatePlaceholderConceptSetId – uniqueness & range
# ============================================================
test_that("generatePlaceholderConceptSetId returns value in 999900000+ range", {
  id <- CaprForExtensions:::generatePlaceholderConceptSetId("waveform_feature")
  expect_gte(id, 999900000L)
  expect_lte(id, 999999999L)})

test_that("generatePlaceholderConceptSetId gives different IDs for different tables", {
  id1 <- CaprForExtensions:::generatePlaceholderConceptSetId("waveform_feature")
  id2 <- CaprForExtensions:::generatePlaceholderConceptSetId("waveform_occurrence")
  expect_false(id1 == id2,
    label = "Different table names must produce different placeholder concept set IDs")
})

test_that("generatePlaceholderConceptSetId gives different IDs for same table with different concept sets", {
  id1 <- CaprForExtensions:::generatePlaceholderConceptSetId("waveform_feature",
                                                              concept_ids = c(2000000051L))
  id2 <- CaprForExtensions:::generatePlaceholderConceptSetId("waveform_feature",
                                                              concept_ids = c(2000000052L))
  expect_false(id1 == id2,
    label = "Same table but different concept IDs must produce different placeholder concept set IDs")
})

test_that("generatePlaceholderConceptSetId is deterministic for identical inputs", {
  id1 <- CaprForExtensions:::generatePlaceholderConceptSetId("waveform_feature",
                                                              concept_ids = c(2000000051L))
  id2 <- CaprForExtensions:::generatePlaceholderConceptSetId("waveform_feature",
                                                              concept_ids = c(2000000051L))
  expect_equal(id1, id2)
})

test_that("generatePlaceholderConceptSetId does NOT collide for strings with equal character-code sums", {
  # "ab" and "ba" have the same sum(utf8ToInt()) value (the old broken hash).
  # The new digest::digest-based hash must distinguish them.
  id_ab <- CaprForExtensions:::generatePlaceholderConceptSetId("ab")
  id_ba <- CaprForExtensions:::generatePlaceholderConceptSetId("ba")
  expect_false(id_ab == id_ba,
    label = "Anagram table names must NOT produce the same placeholder concept set ID")
})

# ============================================================
# 2. exportToAtlasJson – basic structure & metadata embedding
# ============================================================
test_that("exportToAtlasJson embeds extension metadata into ValueAsString", {
  json_in <- make_single_extension_json(query_name = "qtc_query", cs_id = 0L)
  atlas_json <- exportToAtlasJson(json_in, pretty = FALSE)
  parsed <- jsonlite::fromJSON(atlas_json, simplifyVector = FALSE)

  obs <- parsed$PrimaryCriteria$CriteriaList[[1]]$Observation
  expect_false(is.null(obs$ValueAsString),
    label = "Extension metadata should be embedded in Observation$ValueAsString")
  expect_true(grepl("qtc_query", obs$ValueAsString$Text),
    label = "The embedded JSON should contain the extension query name")
})

test_that("exportToAtlasJson removes ExtensionMetadata section from output", {
  json_in  <- make_single_extension_json()
  atlas_json <- exportToAtlasJson(json_in, pretty = FALSE)
  parsed   <- jsonlite::fromJSON(atlas_json, simplifyVector = FALSE)
  expect_null(parsed$ExtensionMetadata,
    label = "ExtensionMetadata must be removed after export to Atlas format")
})

test_that("exportToAtlasJson handles CodesetId stored as double (JSON numeric type)", {
  # JSON parsers often return integers as double; equality check must still work.
  json_in <- make_single_extension_json(cs_id = 0)    # 0 is numeric (double in R)
  expect_no_warning(
    atlas_json <- exportToAtlasJson(json_in, pretty = FALSE)
  )
  parsed <- jsonlite::fromJSON(atlas_json, simplifyVector = FALSE)
  obs <- parsed$PrimaryCriteria$CriteriaList[[1]]$Observation
  expect_false(is.null(obs$ValueAsString),
    label = "CodesetId as double should still match concept set id")
})

# ============================================================
# 3. exportToAtlasJson – partial name matching ambiguity (known bug)
# ============================================================
test_that("exportToAtlasJson embeds metadata into BOTH observations when query names share prefix", {
  json_in <- make_two_similar_name_json()
  # With the exact-match fix (cs_info$name == paste0("Extension:", query_name)),
  # "qtc" now only matches "Extension:qtc" and not "Extension:qtc_prolonged".
  atlas_json <- exportToAtlasJson(json_in, pretty = FALSE)
  parsed <- jsonlite::fromJSON(atlas_json, simplifyVector = FALSE)

  obs0 <- parsed$PrimaryCriteria$CriteriaList[[1]]$Observation  # CodesetId 0 = qtc_prolonged
  obs1 <- parsed$PrimaryCriteria$CriteriaList[[2]]$Observation  # CodesetId 1 = qtc

  # Both observations must have metadata
  expect_false(is.null(obs0$ValueAsString),
    label = "obs0 (qtc_prolonged, cs_id=0) should have embedded extension metadata")
  expect_false(is.null(obs1$ValueAsString),
    label = "obs1 (qtc, cs_id=1) should have embedded extension metadata")

  # Each must carry the CORRECT metadata
  expect_true(grepl("qtc_prolonged", obs0$ValueAsString$Text, fixed = TRUE),
    label = "obs0 should embed qtc_prolonged metadata")
  expect_false(grepl('"name":"qtc"', obs0$ValueAsString$Text, fixed = TRUE) &&
                 !grepl("qtc_prolonged", obs0$ValueAsString$Text, fixed = TRUE),
    label = "obs0 must not be wrongly assigned the short 'qtc' metadata")
  expect_true(grepl('"name":"qtc"', obs1$ValueAsString$Text, fixed = TRUE),
    label = "obs1 (cs_id=1, qtc concept set) should embed qtc metadata")
})

# ============================================================
# 4. importFromAtlasJson – basic extraction & round-trip
# ============================================================
test_that("importFromAtlasJson restores ExtensionMetadata from ValueAsString", {
  json_in    <- make_single_extension_json(query_name = "qtc_query", cs_id = 0L)
  atlas_json <- exportToAtlasJson(json_in, pretty = FALSE)
  restored   <- importFromAtlasJson(atlas_json, pretty = FALSE)
  parsed     <- jsonlite::fromJSON(restored, simplifyVector = FALSE)

  expect_false(is.null(parsed$ExtensionMetadata),
    label = "ExtensionMetadata should be restored after importFromAtlasJson")
  expect_equal(length(parsed$ExtensionMetadata$extensionQueries), 1L,
    label = "Exactly one extension query should be restored")
  expect_equal(parsed$ExtensionMetadata$extensionQueries[[1]]$name, "qtc_query",
    label = "Restored query name should match original")
})

test_that("importFromAtlasJson preserves extensionLogic and version fields", {
  json_in    <- make_single_extension_json()
  atlas_json <- exportToAtlasJson(json_in, pretty = FALSE)
  restored   <- importFromAtlasJson(atlas_json, pretty = FALSE)
  parsed     <- jsonlite::fromJSON(restored, simplifyVector = FALSE)

  expect_equal(parsed$ExtensionMetadata$extensionLogic, "ALL")
  expect_equal(parsed$ExtensionMetadata$version, "2.0")
})

test_that("exportToAtlasJson followed by importFromAtlasJson is a lossless round-trip", {
  json_in    <- make_single_extension_json(query_name = "qtc_query", cs_id = 0L)

  original_meta <- jsonlite::fromJSON(json_in,
                                      simplifyVector = FALSE)$ExtensionMetadata$extensionQueries[[1]]

  atlas_json  <- exportToAtlasJson(json_in, pretty = FALSE)
  restored    <- importFromAtlasJson(atlas_json, pretty = FALSE)
  restored_meta <- jsonlite::fromJSON(restored,
                                      simplifyVector = FALSE)$ExtensionMetadata$extensionQueries[[1]]

  expect_equal(restored_meta$name,      original_meta$name)
  expect_equal(restored_meta$tableName, original_meta$tableName)
  expect_equal(restored_meta$dateField, original_meta$dateField)
})

# ============================================================
# 5. importFromAtlasJson – NULL CodesetId guard (known bug)
# ============================================================
test_that("importFromAtlasJson does not error when an Observation has no CodesetId", {
  json_in <- make_no_codeset_id_json()
  # Should warn (no extension metadata found) but NOT throw an error/crash
  expect_no_error(
    suppressWarnings(importFromAtlasJson(json_in, pretty = FALSE))
  )
})

# ============================================================
# 6. Search scope – InclusionRules now searched (fixed)
# ============================================================
test_that("exportToAtlasJson embeds metadata for extension query used in InclusionRules", {
  json_in <- make_inclusion_rule_json(query_name = "qtc_incl", cs_id = 0L)

  # The fix extends the search to InclusionRules, so no warning should be issued
  # and the observation inside the rule must receive the embedded metadata.
  expect_no_warning(
    atlas_json <- exportToAtlasJson(json_in, pretty = FALSE)
  )

  parsed <- jsonlite::fromJSON(atlas_json, simplifyVector = FALSE)
  rule_obs <- parsed$InclusionRules[[1]]$expression$CriteriaList[[1]]$Criteria$Observation

  expect_false(is.null(rule_obs$ValueAsString),
    label = "Observation inside InclusionRules should have embedded extension metadata")
  expect_true(grepl("qtc_incl", rule_obs$ValueAsString$Text, fixed = TRUE),
    label = "Embedded metadata should contain the correct query name")
})

test_that("importFromAtlasJson extracts extension metadata from InclusionRules observations", {
  json_in    <- make_inclusion_rule_json(query_name = "qtc_incl", cs_id = 0L)
  atlas_json <- exportToAtlasJson(json_in, pretty = FALSE)
  restored   <- importFromAtlasJson(atlas_json, pretty = FALSE)
  parsed     <- jsonlite::fromJSON(restored, simplifyVector = FALSE)

  expect_false(is.null(parsed$ExtensionMetadata),
    label = "ExtensionMetadata should be restored from InclusionRules observation")
  expect_equal(parsed$ExtensionMetadata$extensionQueries[[1]]$name, "qtc_incl")
})

# ============================================================
# 7. encodeExtensionQuery / decodePlaceholderString round-trip
# ============================================================
test_that("encodeExtensionQuery + decodePlaceholderString round-trip preserves table", {
  encoded <- CaprForExtensions:::encodeExtensionQuery(
    table        = "waveform_feature",
    concept_field = "algorithm_concept_id",
    concept_ids   = c(2000000051L),
    filters       = list(valueAsNumber = list(field = "value_as_number",
                                               operator = ">=",
                                               value = 450)),
    date_field    = "waveform_feature_start_timestamp"
  )
  decoded <- CaprForExtensions:::decodePlaceholderString(encoded)

  expect_equal(decoded$table, "waveform_feature")
})

test_that("encodeExtensionQuery + decodePlaceholderString round-trip preserves concept filter", {
  encoded <- CaprForExtensions:::encodeExtensionQuery(
    table         = "waveform_feature",
    concept_field  = "algorithm_concept_id",
    concept_ids    = c(2000000051L),
    date_field     = "ts"
  )
  decoded <- CaprForExtensions:::decodePlaceholderString(encoded)

  expect_false(is.null(decoded$concept_filter),
    label = "Concept filter should be present after decode")
  expect_true(grepl("2000000051", decoded$concept_filter),
    label = "Decoded concept filter should contain the original concept ID")
})

test_that("encodeExtensionQuery + decodePlaceholderString round-trip preserves date field", {
  encoded <- CaprForExtensions:::encodeExtensionQuery(
    table      = "waveform_feature",
    date_field = "waveform_feature_start_timestamp"
  )
  decoded <- CaprForExtensions:::decodePlaceholderString(encoded)

  expect_equal(decoded$date_field, "waveform_feature_start_timestamp")
})

test_that("hasPlaceholders correctly identifies placeholder strings", {
  encoded <- CaprForExtensions:::encodeExtensionQuery(table = "waveform_feature")
  expect_true(CaprForExtensions:::hasPlaceholders(encoded))
  expect_false(CaprForExtensions:::hasPlaceholders("SELECT * FROM observation"))
})

