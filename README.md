# CaprForExtensions

> Extension Table Support for Capr Cohort Definitions

CaprForExtensions extends the [Capr](https://github.com/OHDSI/Capr) package to support cohort definitions that reference OMOP CDM Extension Tables. This enables you to incorporate extension table data into OHDSI cohort definitions while leveraging ALL CirceR features automatically.

## Features

- **Placeholder Substitution Architecture**: Simple, robust integration using observation placeholders
- **All CirceR Features Work**: Temporal logic, inclusion rules, attrition automatically supported
- **OMOP Extension Tables**: Support for approved OMOP Extension Tables
- **Custom Domains**: Register extension tables as Capr-style domains
- **Temporal Logic**: Rich temporal relationships (DURING, BEFORE, AFTER, WITHIN_DAYS)
- **Simple Implementation**: String substitution replaces complex SQL parsing
- **Easy to Debug**: Placeholders are visible in intermediate SQL

## How It Works

CaprForExtensions uses a placeholder substitution strategy:

1. **Register extension tables** as custom domains
2. **Define cohorts** using Capr-style syntax with extension criteria
3. **Conversion**: Extension queries converted to observation queries with placeholders
4. **CirceR processing**: Standard CirceR generates SQL (treats placeholders as observation queries)
5. **Post-processing**: Placeholders substituted with actual extension table logic

**NOTE** By disguising extension tables as observation queries during compilation, we get ALL CirceR features (temporal logic, inclusion rules, attrition, etc.).
Prior to extension query post processing, we can also load the json object into Atlas to review in more detail.

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("OHDSI/CaprForExtensions")

# Load package
library(CaprForExtensions)
```

### Prerequisites

Required packages:
- Capr (>= 2.0.0)
- CirceR (>= 1.3.0)
- jsonlite, glue, dplyr, checkmate

## Quick Start

### Example: MI with Prolonged QTc

Find patients with myocardial infarction who have a prolonged QTc (>= 450ms):

```r
library(CaprForExtensions)
library(Capr)

# 1. Register waveform_feature table as custom domain
registerCustomDomain(
  domain_id = "waveformFeature",
  domain_name = "Waveform Feature",
  table = "waveform_feature",
  person_id_field = "person_id",
  start_date_field = "waveform_feature_start_timestamp",
  concept_id_field = "algorithm_concept_id"
)

# 2. Define concepts
mi_concepts <- cs(descendants(4329847), name = "Myocardial Infarction")
qtc_algorithm <- cs(2000000051, name = "Bazett QTc Formula")

# 3. Build cohort - extension criteria at same level as CDM!
cohort_def <- cohort(
  entry = entry(
    conditionOccurrence(mi_concepts),
    waveformFeature(
      qtc_algorithm,
      valueAsNumber = numericValue("value_as_number", ">=", 450)
    )
  ),
  exit = exit(endStrategy = observationExit())
)

# 4. Compile and generate SQL
cohort_json <- compileExtendedCohort(cohort_def)
options <- buildExtensionOptions(cohort_id = 1, cdm_schema = "cdm")
sql <- buildExtendedCohortQuery(cohort_json, options)

# SQL now includes waveform_feature table with proper joins!
```

## Architecture

### Placeholder Substitution Approach

```
User Capr Definition
    ↓
ExtensionCompiler detects custom domain queries
    ↓
Convert to observation queries with placeholders
    (e.g., @EXT_TABLE:waveform_feature@)
    ↓
Standard Capr compilation
    ↓
CirceR generates SQL (treats placeholders as observations)
    ↓
Post-processor substitutes placeholders with extension table logic
    ↓
Final SQL with extension tables
```

## OMOP Waveform Extension Tables

CaprForExtensions has been tested on the [OMOP CDM Waveform Extension](https://ohdsi.github.io/WaveformWG/):

### Table Hierarchy

```
waveform_occurrence (clinical context)
  ↓
waveform_registry (files)
  ↓
waveform_channel_metadata (channels)
  ↓
waveform_feature (derived measurements)
```

### Tested Use Cases

1. **Waveform Occurrence**: ECG, telemetry, monitoring sessions
2. **Waveform Features**: Heart rate, QTc, HRV, derived measurements
3. **Multi-table queries**: Join through waveform hierarchy

## Key Functions

### Custom Domains

| Function | Purpose |
|----------|---------|
| `registerCustomDomain()` | Register extension table as custom domain |
| `listCustomDomains()` | List registered domains |
| `waveformOccurrence()` | Auto-created query function (example) |
| `customDomainQuery()` | Create custom domain query manually |

### Extension Queries

| Function | Purpose |
|----------|---------|
| `extensionQuery()` | Create extension table query |
| `extendedEntry()` | Create cohort entry with extensions |
| `extendedCohort()` | Build complete extended cohort |
| `compileExtendedCohort()` | Compile to JSON with extension metadata |
| `buildExtendedCohortQuery()` | Generate SQL with placeholder substitution |

### Helper Functions

| Function | Purpose |
|----------|---------|
| `numericValue()` | Helper for numeric filters |
| `stringValue()` | Helper for string filters |
| `dateRange()` | Helper for date range filters |
| `buildExtensionOptions()` | Create CirceR options |

## Examples

### Example 1: Drug Exposure with QTc Monitoring

Patients starting azithromycin with QTc measurements:

```r
azithromycin <- cs(descendants(1734104), name = "Azithromycin")
qtc_algorithm <- cs(2000000051, name = "Bazett QTc")

azithro_qtc <- cohort(
  entry = entry(
    drugExposure(azithromycin),
    waveformFeature(
      qtc_algorithm,
      # Temporal constraints handled by CirceR!
      startDate = dateAdjustment(index = "startDate", offset = 7)
    )
  ),
  exit = exit(endStrategy = fixedExit("startDate", 90))
)
```

### Example 2: ICU with Continuous Telemetry

ICU patients with continuous monitoring:

```r
registerCustomDomain(
  domain_id = "waveformOccurrence",
  table = "waveform_occurrence",
  person_id_field = "person_id",
  visit_id_field = "visit_occurrence_id",
  start_date_field = "waveform_occurrence_start_datetime",
  concept_id_field = "waveform_occurrence_concept_id"
)

icu_concepts <- cs(9203, name = "ICU Visit")
telemetry_concept <- cs(2000000002, name = "ICU Continuous Monitoring")

cohort(
  entry = entry(
    visit(icu_concepts),
    waveformOccurrence(
      telemetry_concept,
      valueAsNumber = numericValue("num_of_files", ">=", 12)
    )
  ),
  exit = exit(endStrategy = fixedExit("startDate", 30))
)
```

### Example 3: Inclusion Rules with Extensions

MI patients requiring at least one QTc measurement:

```r
mi_cohort <- cohort(
  entry = entry(
    conditionOccurrence(mi_concepts)
  ),
  attrition = attrition(
    "has_qtc_measurement" = withAll(
      atLeast(1, waveformFeature(qtc_algorithm))
    )
  ),
  exit = exit(endStrategy = observationExit())
)
```

## CirceR Features (All Work Automatically!)

Because extension criteria are converted to observation queries before CirceR processes them, ALL CirceR features work automatically:

### 1. Temporal Logic 

```r
waveformFeature(
  qtc_algorithm,
  startDate = dateAdjustment(index = "startDate", offset = 7)
)
```

### 2. Inclusion Rules 

```r
attrition(
  "has_qtc" = withAll(
    atLeast(1, waveformFeature(qtc_algorithm, valueAsNumber >= 450))
  )
)
```

### 3. Continuous Observation 

```r
entry(
  conditionOccurrence(mi_concepts),
  observationWindow = continuousObservation(365, 0),
  waveformFeature(...)
)
```

### 4. Exit Strategies 

All exit strategies work naturally because CirceR generates the cohort period logic.

## Temporal Relationships

Control when extension events occur relative to cohort dates:

| Relationship | Description | Example |
|--------------|-------------|---------|
| `DURING` | Extension event during cohort period | Waveform during hospitalization |
| `BEFORE` | Extension event before cohort start | Baseline EKG before treatment |
| `AFTER` | Extension event after cohort start | Follow-up monitoring |
| `WITHIN_DAYS` | Extension within N days | QTc within 7 days of drug |
| `ANY` | No temporal constraint | Any waveform for patient |

```r
# Example: Baseline EKG (before cohort entry)
baseline_ekg <- extensionQuery(
  name = "baseline_ekg",
  table = "waveform_occurrence",
  person_id_field = "person_id",
  date_field = "waveform_occurrence_start_datetime",
  filters = list("waveform_occurrence_concept_id = 2000000001"),
  temporal_relationship = "BEFORE"
)
```

## Extension Table Requirements

Extension tables must follow OHDSI criteria and should include:

1. **Person linkage field** (e.g., `person_id`)
2. **Date/datetime field** for temporal criteria
3. **Visit linkage field** (optional but recommended: `visit_occurrence_id`)

## Support

- **Issues**: [GitHub Issues](https://github.com/OHDSI/CaprForExtensions/issues)
- **OHDSI Forums**: https://forums.ohdsi.org/
- **Waveform WG**: https://github.com/OHDSI/WaveformWG


## License

Apache License 2.0


## FAQ

**Q: Why use CaprForExtensions instead of custom SQL?**
A: CaprForExtensions integrates extension tables with Capr's programmatic approach, enabling reusable, testable, version-controlled cohort definitions.

**Q: Can I use extension tables other than waveforms?**
A: Of course, waveforms were simply the test case for development. Any extension meeting OHDSI approval criteria could work.

**Q: Does CirceR need to be modified?**
A: No, CaprForExtensions works with standard CirceR by using placeholder substitution.

**Q: Which databases are supported?**
A: Any database supported by CirceR/SqlRender (PostgreSQL, SQL Server, BigQuery, Redshift, etc.)

**Q: Can I mix custom domains and extension queries?**
A: Yes! Custom domains are converted to extension queries during compilation.


## Contact

- **Maintainer**: Jared Houghtaling <houghtaling@ohdsi.org>
- **Repository**: https://github.com/OHDSI/CaprForExtensions
- **OHDSI**: https://www.ohdsi.org/

