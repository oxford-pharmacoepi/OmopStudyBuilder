# Generate Study Analysis Code from a Statistical Analysis Plan (SAP)
# Internals are ported from shinySAP, keeping its snake_case function names

#' Generate study analysis code from a statistical analysis plan
#'
#' Renders the analysis files of a `studyCode/` folder from a statistical
#' analysis plan (SAP), such as one exported by the shinySAP application. The
#' files written are \code{codelist/codelistCreation.R},
#' \code{cohorts/instantiateCohorts.R} and
#' \code{analyses/incidencePrevalence.R} -- the files that
#' \code{\link{initStudy}} leaves empty. \code{codeToRun.R} and
#' \code{runStudy.R} are never touched: they carry site-specific connection
#' details.
#'
#' The SAP names its codelists but does not carry their codes, so the generated
#' \code{codelistCreation.R} reads each codelist from a CSV
#' (\code{concept_id}, \code{concept_name}) that the study team places under
#' \code{codelist/<Category>/<name>.csv}; the folders are created and each
#' expected path is stated in the script.
#'
#' @param sap Path to a SAP file (as exported by shinySAP), or the
#'   already-parsed SAP as a list. The file may be \code{.json} (one SAP
#'   object) or \code{.jsonl}/\code{.ndjson} (one JSON object per line, where
#'   the last record is used).
#' @param directory Path to an existing \code{studyCode/} directory, for
#'   example the one created by \code{\link{initStudy}}.
#'
#' @returns Invisibly, the paths of the files written.
#'
#' @noRd
#'
#' @examples
#' library(OmopStudyBuilder)
#'
#' # A minimal SAP: one codelist, one outcome cohort, one denominator,
#' # and one period prevalence analysis
#' sap <- list(
#'   study = list(title = "Asthma prevalence", version = "1.0"),
#'   codelists = list(list(name = "cs_asthma", category = "Index event")),
#'   cohorts = list(
#'     list(
#'       name = "Asthma", kind = "outcome",
#'       operations = list(
#'         list(op = "concept_cohort", codelist = "cs_asthma"),
#'         list(op = "require_first_entry")
#'       )
#'     ),
#'     list(
#'       name = "General population", kind = "denominator",
#'       ageGroup = list(c(0, 150)), sex = "Both", daysPriorObservation = 365
#'     )
#'   ),
#'   proposed_analyses = list(
#'     list(
#'       name = "Asthma period prevalence",
#'       analysis_type = "estimatePeriodPrevalence",
#'       parameters = list(
#'         denominatorTable = "General population",
#'         outcomeTable = "Asthma",
#'         interval = "years"
#'       )
#'     )
#'   )
#' )
#'
#' study_root <- file.path(tempdir(), "SapStudy")
#' initStudy(study_root, diagnostics = FALSE)
#' generateStudyCode(sap, file.path(study_root, "studyCode"))
generateStudyCode <- function(sap, directory) {
  # Validate inputs
  sap <- read_sap_plan(sap)
  omopgenerics::assertCharacter(directory, length = 1)
  if (!dir.exists(directory)) {
    cli::cli_abort("Provided directory {.pkg {directory}} does not exist.")
  }

  paths <- write_study_files(sap, directory)
  cli::cli_alert_success(
    "Study code generated from the SAP in {.strong {directory}}."
  )
  for (note in study_export_notes(sap)) {
    cli::cli_alert_info(note)
  }
  invisible(paths)
}

# In a .jsonl/.ndjson file the last record is taken as the most recent SAP
read_sap_plan <- function(sap) {
  if (is.character(sap)) {
    omopgenerics::assertCharacter(sap, length = 1)
    if (!file.exists(sap)) {
      cli::cli_abort("SAP file {.file {sap}} does not exist.")
    }
    if (grepl("\\.(jsonl|ndjson)$", sap, ignore.case = TRUE)) {
      lines <- trimws(readLines(sap, warn = FALSE))
      lines <- lines[nzchar(lines)]
      if (!length(lines)) {
        cli::cli_abort("SAP file {.file {sap}} contains no records.")
      }
      if (length(lines) > 1) {
        cli::cli_alert_info(
          "{.file {basename(sap)}} holds {length(lines)} records; using the last one."
        )
      }
      sap <- jsonlite::fromJSON(lines[[length(lines)]], simplifyVector = FALSE)
    } else {
      sap <- jsonlite::fromJSON(sap, simplifyVector = FALSE)
    }
  }
  if (!is.list(sap)) {
    cli::cli_abort(
      "`sap` must be a path to a SAP .json or .jsonl file, or the SAP as a list."
    )
  }
  sap
}

# =============================================================================
# SAP field helpers
# =============================================================================

# Unlike rlang's `%||%`, NULL, length 0 and a single NA all fall through
`%|||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

objective_id <- function(o) {
  v <- if (is.list(o)) as.character(o$id %|||% "")[1] else NA_character_
  if (is.na(v) || !nzchar(v)) NA_character_ else v
}

SAP_COHORT_KINDS <- c(
  "denominator", "target_denominator", "target", "outcome", "censor", "other"
)

canonical_cohort_kind <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return("")
  if (x %in% SAP_COHORT_KINDS) return(x)
  "other"
}

is_denominator_kind <- function(kind) {
  canonical_cohort_kind(kind) %in% c("denominator", "target_denominator")
}

# Bound pairs are indexed with [[ ]] and never unlist()ed: a JSON null upper
# bound (meaning Inf) would collapse and the pair would become length 1
bound_upper <- function(pair) {
  hi <- if (length(pair) >= 2) pair[[2]] else NULL
  if (is.null(hi)) Inf else as.numeric(hi)
}

date_bound <- function(range, i) {
  if (is.null(range) || length(range) < i) return(NULL)
  v <- range[[i]]
  if (is.null(v) || (length(v) == 1 && is.na(v))) return(NULL)
  as.character(v)
}

# outcomeWashout travels as a one-element array: [365] is 365 days, [null] is
# Inf (unbounded), null is "the author never said"
washout_days <- function(w) {
  if (is.null(w) || !length(w)) return(NULL)
  v <- w[[1]]
  if (is.null(v)) return(Inf)
  n <- suppressWarnings(as.numeric(v))
  if (is.na(n)) NULL else n
}

canonical_analysis_role <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return("")
  as.character(x)
}

# =============================================================================
# R literal renderers
# =============================================================================
# All renderers return NULL for an empty or undecided value, and r_call()
# drops NULL arguments so the package defaults stay in force

r_string <- function(x) sprintf('"%s"', gsub('"', '\\\\"', as.character(x)))

r_number <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || is.na(x)) return("NA")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

r_logical <- function(x) if (isTRUE(x)) "TRUE" else "FALSE"

r_chr_vec <- function(x) {
  v <- as.character(unlist(x %|||% character(0)))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(NULL)
  if (length(v) == 1) return(r_string(v))
  sprintf("c(%s)", paste(vapply(v, r_string, character(1)), collapse = ", "))
}

r_num_vec <- function(x) {
  v <- suppressWarnings(as.numeric(unlist(x %|||% numeric(0))))
  v <- v[!is.na(v)]
  if (!length(v)) return(NULL)
  if (length(v) == 1) return(r_number(v))
  sprintf("c(%s)", paste(vapply(v, r_number, character(1)), collapse = ", "))
}

r_bounds <- function(pair) {
  if (!length(pair)) return(NULL)
  lo <- suppressWarnings(as.numeric(pair[[1]]))
  hi <- bound_upper(pair)
  sprintf("c(%s, %s)", r_number(lo), r_number(hi))
}

r_bound_list <- function(pairs, always_list = FALSE) {
  pairs <- pairs %|||% list()
  rendered <- Filter(Negate(is.null), lapply(pairs, r_bounds))
  if (!length(rendered)) return(NULL)
  if (length(rendered) == 1 && !always_list) return(rendered[[1]])
  sprintf("list(%s)", paste(unlist(rendered), collapse = ", "))
}

r_date_range <- function(dr) {
  if (!length(dr)) return(NULL)
  bound <- function(i) {
    v <- date_bound(dr, i)
    if (is.null(v)) "NA" else r_string(v)
  }
  lo <- bound(1)
  hi <- bound(2)
  if (lo == "NA" && hi == "NA") return(NULL)
  sprintf("as.Date(c(%s, %s))", lo, hi)
}

r_strata <- function(groups) {
  groups <- groups %|||% list()
  rendered <- Filter(Negate(is.null), lapply(groups, r_chr_vec))
  if (!length(rendered)) return(NULL)
  sprintf("list(%s)", paste(unlist(rendered), collapse = ", "))
}

split_top_level <- function(s) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- 0
  out <- character(0)
  cur <- character(0)
  for (ch in chars) {
    if (ch == "(") depth <- depth + 1
    if (ch == ")") depth <- depth - 1
    if (ch == "," && depth == 0) {
      out <- c(out, paste(cur, collapse = ""))
      cur <- character(0)
    } else {
      cur <- c(cur, ch)
    }
  }
  trimws(c(out, paste(cur, collapse = "")))
}

r_wrap_value <- function(value, prefix, width = 92) {
  head <- if (grepl("^list\\(", value)) "list(" else if (grepl("^c\\(", value)) "c(" else NULL
  if (is.null(head) || nchar(prefix) + nchar(value) <= width) return(value)
  inner <- substr(value, nchar(head) + 1L, nchar(value) - 1L)
  parts <- split_top_level(inner)
  if (length(parts) < 2) return(value)
  pad <- strrep(" ", nchar(prefix) + nchar(head))
  wrapped <- character(0)
  line <- head
  # Only the first line sits after the "  argName = " prefix
  first <- TRUE
  for (i in seq_along(parts)) {
    piece <- paste0(parts[[i]], if (i < length(parts)) "," else "")
    open <- grepl("\\($", line)
    used <- nchar(line) + if (first) nchar(prefix) else 0L
    sep <- if (open) "" else " "
    if (!open && used + nchar(sep) + nchar(piece) > width) {
      wrapped <- c(wrapped, line)
      line <- paste0(pad, piece)
      first <- FALSE
    } else {
      line <- paste0(line, sep, piece)
    }
  }
  paste0(paste(c(wrapped, line), collapse = "\n"), ")")
}

r_call <- function(fn, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  if (!length(args)) return(paste0(fn, "()"))
  pad <- max(nchar(names(args)))
  lines <- vapply(seq_along(args), function(i) {
    prefix <- sprintf("  %-*s = ", pad, names(args)[[i]])
    paste0(prefix, r_wrap_value(args[[i]], prefix))
  }, character(1))
  paste0(fn, "(\n", paste(lines, collapse = ",\n"), "\n)")
}

r_call_inline <- function(fn, args, width = 76) {
  args <- args[!vapply(args, is.null, logical(1))]
  if (!length(args)) return(paste0(fn, "()"))
  one <- sprintf("%s(%s)", fn, paste(sprintf("%s = %s", names(args), unlist(args)),
                                     collapse = ", "))
  if (nchar(one) <= width) one else r_call(fn, args)
}

cohort_table_name <- function(x) {
  s <- tolower(trimws(as.character(x %|||% "")[1]))
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) return(NA_character_)
  if (grepl("^[0-9]", s)) s <- paste0("c_", s)
  # 63 is Postgres's identifier limit, the tightest of the CDM databases
  substr(s, 1, 63)
}

# =============================================================================
# Data source guards
# =============================================================================
# A network study ships one script to every partner, so a SAP restricting an
# item to some databases becomes a run-time test on omopgenerics::cdmName(cdm)

sap_source_keys <- function(sap) {
  vapply(sap$cdm_sources %|||% list(), function(s) {
    key <- trimws(as.character(s$source_key %|||% "")[1])
    if (!is.na(key) && nzchar(key)) key else trimws(as.character(s$name %|||% "")[1])
  }, character(1))
}

item_sources <- function(x) {
  v <- trimws(as.character(unlist(x$data_sources %|||% character(0))))
  unique(v[!is.na(v) & nzchar(v)])
}

is_source_restricted <- function(item, all_sources) {
  keys <- item_sources(item)
  if (!length(keys) || !length(all_sources)) return(FALSE)
  !setequal(keys, all_sources)
}

indent_block <- function(code) paste0("  ", gsub("\n", "\n  ", code))

# Always c(), even for one source, so the membership test stays diff-stable
r_chr_c <- function(x) sprintf("c(%s)", paste(vapply(
  as.character(unlist(x)), r_string, character(1)), collapse = ", "))

source_guard <- function(code, item, all_sources) {
  if (is.null(code) || !is_source_restricted(item, all_sources)) return(code)
  sprintf("if (omopgenerics::cdmName(cdm) %%in%% %s) {\n%s\n}",
          r_chr_c(item_sources(item)), indent_block(code))
}

# The excluded branch must still define the variable (as an empty summarised
# result) or binding the results at the end would fail with "object not found"
# at the excluded partners
source_guard_estimate <- function(var, code, item, all_sources) {
  if (!is_source_restricted(item, all_sources)) {
    return(sprintf("%s <- %s", var, code))
  }
  sprintf(paste0("%s <- if (omopgenerics::cdmName(cdm) %%in%% %s) {\n%s\n} else {\n",
                 "  # Not run here: this SAP restricts the analysis to the databases above.\n",
                 "  omopgenerics::emptySummarisedResult()\n}"),
          var, r_chr_c(item_sources(item)), indent_block(code))
}

# =============================================================================
# SAP labels carried into the results
# =============================================================================
# The analysis name, role and objectives are not estimator arguments, so they
# are carried as extra omopgenerics settings columns, which survive bind(),
# suppress() and the export/import round trip

SAP_TAG_HELPER <- paste(
  "# The SAP fields that are not estimator arguments, carried into the result so",
  "# the exported settings say which analysis produced it and which one the",
  "# study concludes from. An empty result is left alone: it has no settings rows",
  "# to label, and omopgenerics drops all-NA settings columns anyway.",
  "sapTag <- function(result, analysis, role = NA_character_,",
  "                   objectives = NA_character_) {",
  "  if (nrow(result) == 0) return(result)",
  "  s <- omopgenerics::settings(result)",
  "  s$sap_analysis   <- analysis",
  "  s$sap_role       <- role",
  "  s$sap_objectives <- objectives",
  "  omopgenerics::newSummarisedResult(result, settings = s)",
  "}",
  sep = "\n")

objective_indices <- function(sap, a) {
  objs <- sap$study$objectives %|||% list()
  ids <- vapply(objs, function(o) objective_id(o) %|||% "", character(1))
  named <- as.character(unlist(a$objectives %|||% list()))
  which(ids %in% named)
}

sap_tag_call <- function(var, a, sap) {
  nm <- trimws(as.character(a$name %|||% "")[1])
  if (is.na(nm) || !nzchar(nm)) nm <- "(unnamed analysis)"
  role <- canonical_analysis_role(a$role)
  objs <- objective_indices(sap, a)
  args <- c(var,
            sprintf("analysis = %s", r_string(nm)),
            if (nzchar(role)) sprintf("role = %s", r_string(role)),
            if (length(objs)) sprintf("objectives = %s",
                                      r_string(paste(objs, collapse = ", "))))
  one <- sprintf("%s <- sapTag(%s)", var, paste(args, collapse = ", "))
  if (nchar(one) <= 92) return(one)
  sprintf("%s <- sapTag(\n%s\n)", var,
          paste(sprintf("  %s", args), collapse = ",\n"))
}

# =============================================================================
# Denominator cohorts and analyses
# =============================================================================

cohort_r_code <- function(co) {
  kind <- canonical_cohort_kind(co$kind)
  if (!is_denominator_kind(kind)) return(NULL)
  target <- identical(kind, "target_denominator")

  args <- list(cdm = "cdm", name = r_string(cohort_table_name(co$name) %|||% "denominator"))
  if (target) {
    tgt <- cohort_table_name(co$targetCohortTable)
    args$targetCohortTable <- if (is.na(tgt)) NULL else r_string(tgt)
  }
  args$cohortDateRange <- r_date_range(co[["cohortDateRange"]])
  if (target) args$timeAtRisk <- r_bound_list(co$timeAtRisk)
  # ageGroup is always a list, even at length one: its default is list(c(0, 150))
  args$ageGroup <- r_bound_list(co$ageGroup, always_list = TRUE)
  args$sex <- r_chr_vec(co$sex)
  args$daysPriorObservation <- r_num_vec(co$daysPriorObservation)
  # Always stated: requirementInteractions changes how many cohorts the call
  # generates, so it must not be left implicit
  if (target) args$requirementsAtEntry <- r_logical(co$requirementsAtEntry)
  args$requirementInteractions <- r_logical(co$requirementInteractions)

  r_call(
    if (target) "generateTargetDenominatorCohortSet" else "generateDenominatorCohortSet",
    args
  )
}

analysis_estimator <- function(type) {
  type <- as.character(type %|||% "")[1]
  if (is.na(type) || !nzchar(type)) return(NA_character_)
  estimators <- c("estimatePointPrevalence", "estimatePeriodPrevalence", "estimateIncidence")
  if (type %in% estimators) return(type)
  # A saved Incidence analysis names the registry key; Prevalence always
  # serialises an estimator, so the bare key would not say point or period
  if (identical(type, "Incidence")) return("estimateIncidence")
  NA_character_
}

analysis_r_code <- function(a, cohorts = list()) {
  fn <- analysis_estimator(a$analysis_type)
  if (is.na(fn)) return(NULL)
  p <- a$parameters %|||% list()

  tbl <- function(nm) {
    t <- cohort_table_name(nm)
    if (is.na(t)) NULL else r_string(t)
  }
  ids <- function(x) r_num_vec(x)

  args <- list(cdm = "cdm",
               denominatorTable = tbl(p$denominatorTable),
               outcomeTable = tbl(p$outcomeTable))

  if (identical(fn, "estimateIncidence")) {
    args$censorTable <- tbl(p$censorTable)
    args$denominatorCohortId <- ids(p$denominatorCohortId)
    args$outcomeCohortId <- ids(p$outcomeCohortId)
    args$censorCohortId <- ids(p$censorCohortId)
    args$interval <- r_chr_vec(p$interval)
    args$completeDatabaseIntervals <- r_logical(p$completeDatabaseIntervals)
    # washout_days(), not r_num_vec(): an unbounded washout serialises as
    # [null], which would read as empty and silently drop the argument
    washout <- washout_days(p$outcomeWashout)
    args$outcomeWashout <- if (is.null(washout)) NULL else r_number(washout)
    args$repeatedEvents <- r_logical(p$repeatedEvents)
  } else {
    args$denominatorCohortId <- ids(p$denominatorCohortId)
    args$outcomeCohortId <- ids(p$outcomeCohortId)
    args$interval <- r_chr_vec(p[["interval"]])
    if (identical(fn, "estimatePointPrevalence")) {
      args$timePoint <- r_chr_vec(p$timePoint)
    } else {
      args$completeDatabaseIntervals <- r_logical(p$completeDatabaseIntervals)
      args$fullContribution <- r_logical(p$fullContribution)
      args$level <- r_chr_vec(p$level)
    }
  }

  args$strata <- r_strata(p$strata)
  # Inert without strata, so it is stated only where it does something
  if (!is.null(args$strata)) args$includeOverallStrata <- r_logical(p$includeOverallStrata)

  r_call(fn, args)
}

estimate_var_names <- function(analyses) {
  vapply(seq_along(analyses), function(i) {
    slug <- cohort_table_name(analyses[[i]]$name)
    if (is.na(slug)) return(sprintf("estimate_%d", i))
    slug <- sub("_+$", "", substr(slug, 1, 32))
    if (!nzchar(slug)) sprintf("estimate_%d", i) else sprintf("%s_%d", slug, i)
  }, character(1))
}

# =============================================================================
# Cohort operations
# =============================================================================
# A cohort may carry `operations`: an ordered list of typed steps, each one
# CohortConstructor verb, e.g.
#   {"op": "concept_cohort", "codelist": "cs_asthma"}
#   {"op": "require_first_entry"}
#   {"op": "pad_cohort_end", "days": 1825}

# Field readers return NULL for absent, so an omitted field leaves the
# package's own default in force
op_num <- function(o, key) {
  v <- suppressWarnings(as.numeric(o[[key]] %|||% NA))
  if (length(v) != 1 || is.na(v)) NULL else v
}

op_chr <- function(o, key) {
  v <- as.character(o[[key]] %|||% NA)[1]
  if (is.na(v) || !nzchar(trimws(v))) NULL else trimws(v)
}

op_chr_vec <- function(o, key) {
  v <- trimws(as.character(unlist(o[[key]] %|||% character(0))))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) NULL else v
}

op_pairs <- function(o, key) {
  v <- o[[key]]
  if (!length(v)) return(NULL)
  # Accept a bare pair ([0, 17]) as well as a list of pairs
  if (!is.list(v[[1]]) && length(v) == 2) v <- list(v)
  v
}

concept_set_var <- function(name) {
  v <- cohort_table_name(name)
  if (is.na(v)) "concept_set" else v
}

# Several codelists become one c(), and conceptCohort() then creates one
# cohort per codelist in a single table
concept_set_arg <- function(names) {
  vars <- vapply(names, concept_set_var, character(1), USE.NAMES = FALSE)
  if (length(vars) == 1) vars else sprintf("c(%s)", paste(vars, collapse = ", "))
}

# The op registry, keyed by op:
#   code     function(o, ctx) -> the R call, or NULL when the op emits none
#   entry    TRUE for an op that creates the cohort rather than transforming one
#   assigns  "table" for a call returning a cohort table (cdm$<name> <- ...) or
#            "cdm" for one returning a cdm reference with the table attached
COHORT_OP_TEMPLATES <- list(
  concept_cohort = list(
    entry = TRUE, assigns = "table",
    code = function(o, ctx) {
      cl <- op_chr_vec(o, "codelist")
      r_call("conceptCohort", list(
        cdm = "cdm",
        conceptSet = if (is.null(cl)) NULL else concept_set_arg(cl),
        name = r_string(ctx$table),
        exit = if (identical(op_chr(o, "exit"), "event_start_date")) {
          r_string("event_start_date")
        } else NULL,
        subsetCohort = {
          sub <- op_chr(o, "subset_cohort")
          if (is.null(sub)) NULL else {
            t <- cohort_table_name(sub)
            if (is.na(t)) NULL else r_string(t)
          }
        }
      ))
    }
  ),
  require_first_entry = list(
    entry = FALSE, assigns = "table",
    code = function(o, ctx) "requireIsFirstEntry()"
  ),
  require_demographics = list(
    entry = FALSE, assigns = "table",
    code = function(o, ctx) {
      ages <- op_pairs(o, "age_range")
      r_call_inline("requireDemographics", list(
        ageRange = if (is.null(ages)) NULL else r_bound_list(ages, always_list = TRUE),
        sex = r_chr_vec(op_chr(o, "sex")),
        minPriorObservation = r_num_vec(op_num(o, "min_prior_observation"))
      ))
    }
  ),
  require_in_date_range = list(
    entry = FALSE, assigns = "table",
    code = function(o, ctx) {
      r_call_inline("requireInDateRange", list(
        dateRange = r_date_range(list(op_chr(o, "start"), op_chr(o, "end")))))
    }
  ),
  exit_at_observation_end = list(
    entry = FALSE, assigns = "table",
    code = function(o, ctx) "exitAtObservationEnd()"
  ),
  exit_at_death = list(
    entry = FALSE, assigns = "table",
    code = function(o, ctx) "exitAtDeath()"
  ),
  pad_cohort_end = list(
    entry = FALSE, assigns = "table",
    code = function(o, ctx) r_call_inline("padCohortEnd", list(days = r_num_vec(op_num(o, "days"))))
  ),
  # omopgenerics::bind() returns a cdm reference with the combined table
  # attached, hence assigns = "cdm"
  bind_cohorts = list(
    entry = TRUE, assigns = "cdm",
    code = function(o, ctx) {
      nms <- op_chr_vec(o, "cohorts") %|||% character(0)
      tbls <- vapply(nms, function(n) {
        t <- cohort_table_name(n)
        if (is.na(t)) "cohort" else t
      }, character(1), USE.NAMES = FALSE)
      args <- c(sprintf("cdm$%s", tbls), sprintf("name = %s", r_string(ctx$table)))
      one <- sprintf("omopgenerics::bind(%s)", paste(args, collapse = ", "))
      if (nchar(one) <= 88) return(one)
      sprintf("omopgenerics::bind(\n%s\n)", paste(sprintf("  %s", args), collapse = ",\n"))
    }
  ),
  custom = list(
    entry = FALSE, assigns = "table",
    code = function(o, ctx) NULL
  )
)

cohort_op_type <- function(o) {
  v <- as.character(o$op %|||% "")[1]
  if (is.na(v)) "" else trimws(v)
}

cohort_op_template <- function(op) COHORT_OP_TEMPLATES[[op]]

cohort_operations_code <- function(ch, codelists = list(), cohorts = character(0)) {
  ops <- ch$operations %|||% list()
  if (!length(ops)) return(NULL)
  tbl <- cohort_table_name(ch$name)
  ctx <- list(table = if (is.na(tbl)) "cohort" else tbl,
              codelists = codelists, cohorts = cohorts)

  rendered <- lapply(ops, function(o) {
    tmpl <- cohort_op_template(cohort_op_type(o))
    if (is.null(tmpl)) {
      return(list(code = NULL, note = sprintf(
        "No cohort operation is registered as '%s'.", cohort_op_type(o))))
    }
    note <- if (identical(cohort_op_type(o), "custom")) op_chr(o, "text") else NULL
    list(code = tmpl$code(o, ctx), note = note, entry = isTRUE(tmpl$entry),
         assigns = tmpl$assigns %|||% "table")
  })

  # An operation with no call becomes a comment in place, so a step the author
  # asked for is never silently missing from the script
  as_line <- function(r) {
    if (!is.null(r$code)) return(r$code)
    sprintf("# TODO: %s", r$note %|||% "this step has no generated call.")
  }

  first <- rendered[[1]]
  head_line <- as_line(first)
  # bind_cohorts returns a cdm reference, so it cannot head a
  # `cdm$x <- ... |> ...` pipeline; it becomes its own statement and later
  # steps pipe from the attached table
  if (identical(first$assigns, "cdm")) {
    out <- sprintf("cdm <- %s", head_line)
    if (length(rendered) > 1) {
      steps <- vapply(rendered[-1], function(r) {
        sprintf("  %s", gsub("\n", "\n  ", as_line(r), fixed = TRUE))
      }, character(1))
      out <- paste0(out, sprintf("\ncdm$%s <- cdm$%s |>\n%s",
                                 ctx$table, ctx$table, paste(steps, collapse = " |>\n")))
    }
    return(out)
  }
  if (!isTRUE(first$entry)) {
    head_line <- paste0(
      "# TODO: this cohort has no entry operation, so nothing creates it.\n",
      head_line)
  }
  rest <- vapply(rendered[-1], function(r) {
    sprintf("  %s", gsub("\n", "\n  ", as_line(r), fixed = TRUE))
  }, character(1))

  body <- if (length(rest)) {
    paste0(head_line, " |>\n", paste(rest, collapse = " |>\n"))
  } else {
    head_line
  }
  sprintf("cdm$%s <- %s", ctx$table, body)
}

# Only codelists cited by a typed entry operation count; one cited in free
# text alone is documentation, not an input
cited_codelist_names <- function(cohorts) {
  refs <- unlist(lapply(cohorts %|||% list(), function(co) {
    lapply(Filter(function(o) identical(cohort_op_type(o), "concept_cohort"),
                  co$operations %|||% list()),
           function(o) op_chr_vec(o, "codelist") %|||% character(0))
  }))
  refs <- as.character(refs %|||% character(0))
  unique(refs[nzchar(refs)])
}

# =============================================================================
# Study file rendering
# =============================================================================

STUDY_PATHS <- list(
  codelists = "codelist/codelistCreation.R",
  cohorts = "cohorts/instantiateCohorts.R",
  analyses = "analyses/incidencePrevalence.R",
  codes_dir = "codelist"
)

# The SAP names a codelist but does not carry its codes: the study team places
# the CSV at this path and codelistCreation.R reads it from there
codelist_csv_path <- function(cl) {
  cat_dir <- as.character(cl$category %|||% "")[1]
  if (is.na(cat_dir) || !nzchar(trimws(cat_dir))) cat_dir <- "Other"
  # A category is free text and becomes a directory name, so strip anything a
  # file system would object to
  cat_dir <- gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", trimws(cat_dir)))
  cat_dir <- gsub("^_|_$", "", cat_dir)
  file.path(STUDY_PATHS$codes_dir, cat_dir,
            sprintf("%s.csv", cohort_table_name(cl$name) %|||% "codelist"))
}

study_codelists_r <- function(sap) {
  cited <- cited_codelist_names(sap$cohorts)
  cls <- Filter(function(cl) as.character(cl$name %|||% "")[1] %in% cited,
                sap$codelists %|||% list())
  if (!length(cls)) {
    return(paste("# No concept sets: no cohort in this SAP enters on a codelist yet.\n"))
  }
  blocks <- vapply(cls, function(cl) {
    var <- concept_set_var(cl$name)
    sprintf(paste0(
      "# %s\n",
      "# TODO: place this codelist's CSV (concept_id, concept_name) at the path below.\n",
      '%s <- importCodelist(path = here("%s"), type = "csv")'),
      as.character(cl$name %|||% ""), var, codelist_csv_path(cl))
  }, character(1))
  paste0(paste(blocks, collapse = "\n\n"), "\n")
}

study_cohorts_r <- function(sap) {
  cls <- vapply(sap$codelists %|||% list(),
                function(cl) as.character(cl$name %|||% "")[1], character(1))
  cohort_names <- vapply(sap$cohorts %|||% list(),
                         function(co) as.character(co$name %|||% "")[1], character(1))
  blocks <- Filter(Negate(is.null), lapply(sap$cohorts %|||% list(), function(co) {
    code <- cohort_operations_code(co, cls, cohort_names)
    if (is.null(code)) return(NULL)
    tbl <- cohort_table_name(co$name)
    idx <- if (is.na(tbl)) "" else sprintf(
      "\ncdm$%s <- cdm$%s |> addCohortTableIndex()", tbl, tbl)
    # The index goes inside the guard: at an excluded partner the table is
    # never created and indexing it would fail
    sprintf("# %s\n%s", as.character(co$name %|||% ""),
            source_guard(paste0(code, idx), co, sap_source_keys(sap)))
  }))
  if (!length(blocks)) {
    return(paste0(
      "# No cohorts are generated yet: none of this SAP's cohorts carry typed\n",
      "# operations, so their logic is still prose.\n"))
  }
  paste0(paste(unlist(blocks), collapse = "\n\n"), "\n")
}

# Estimates accumulate into `results` for runStudy.R to bind and export
study_analyses_r <- function(sap) {
  cohorts <- sap$cohorts %|||% list()
  analyses <- sap$proposed_analyses %|||% list()
  out <- character(0)

  all_sources <- sap_source_keys(sap)

  dens <- Filter(Negate(is.null), lapply(cohorts, function(co) {
    code <- cohort_r_code(co)
    if (is.null(code)) return(NULL)
    sprintf("# %s\n%s", as.character(co$name %|||% ""),
            source_guard(sprintf("cdm <- %s", code), co, all_sources))
  }))
  if (length(dens)) {
    out <- c(out, paste0("# Denominator cohort sets ----\n\n",
                         paste(unlist(dens), collapse = "\n\n")))
  }

  vars <- estimate_var_names(analyses)
  est <- Filter(Negate(is.null), lapply(seq_along(analyses), function(i) {
    a <- analyses[[i]]
    nm <- as.character(a$name %|||% "")
    code <- analysis_r_code(a, cohorts)
    if (is.null(code)) {
      return(sprintf("# %s\n#   No IncidencePrevalence function maps onto analysis type '%s'.",
                     nm, as.character(a$analysis_type %|||% "")))
    }
    obj <- objective_labels(sap, a)
    slot <- sprintf('results[["%s"]]', vars[[i]])
    sprintf("# %s%s%s\n%s\n%s", nm, role_label(a), obj,
            source_guard_estimate(slot, code, a, all_sources),
            sap_tag_call(slot, a, sap))
  }))
  if (length(est)) {
    out <- c(out, paste0("# Estimates ----\n\n", SAP_TAG_HELPER, "\n\n",
                         paste(unlist(est), collapse = "\n\n")))
  }

  if (!length(out)) {
    return("# No denominator cohort sets or incidence / prevalence estimates yet.\n")
  }
  paste0(paste(out, collapse = "\n\n"), "\n")
}

role_label <- function(a) {
  role <- canonical_analysis_role(a$role)
  if (!nzchar(role)) "" else sprintf("  (%s)", role)
}

objective_labels <- function(sap, a) {
  hits <- objective_indices(sap, a)
  if (!length(hits)) return("")
  sprintf("  [objective%s %s]", if (length(hits) == 1) "" else "s",
          paste(hits, collapse = ", "))
}

study_file_header <- function(sap, what) {
  study <- sap$study %|||% list()
  version <- as.character(study$version %|||% "unversioned")
  schema <- as.character(sap$sap_schema_version %|||% NA)
  sprintf(paste0(
    "# %s\n",
    "# Generated by OmopStudyBuilder from the statistical analysis plan.\n",
    "#   Study:   %s\n",
    "#   Version: %s%s\n",
    "# Edit the SAP and regenerate rather than editing this file: the plan a\n",
    "# reviewer signed and the code that runs are meant to be the same object.\n"),
    what,
    as.character(study$study_code %|||% study$title %|||% "unnamed"),
    version,
    if (is.na(schema)) "" else sprintf(" (SAP schema %s)", schema))
}

study_files <- function(sap) {
  files <- list()
  files[[STUDY_PATHS$codelists]] <- paste0(
    study_file_header(sap, "Concept sets"), "\n", study_codelists_r(sap))
  files[[STUDY_PATHS$cohorts]] <- paste0(
    study_file_header(sap, "Study cohorts"), "\n", study_cohorts_r(sap))
  files[[STUDY_PATHS$analyses]] <- paste0(
    study_file_header(sap, "Incidence and prevalence"), "\n", study_analyses_r(sap))
  files
}

# What the SAP decides that lives in files this must not write (codeToRun.R
# holds credentials), reported to the user instead
study_export_notes <- function(sap) {
  n <- sap$study$min_cell_count %|||% NULL
  notes <- if (is.null(n)) {
    paste("This SAP states no minimum cell count. Confirm the min_cell_count in",
          "codeToRun.R is the threshold the data partners require.")
  } else {
    sprintf(paste("Set min_cell_count <- %s in codeToRun.R to match this SAP.",
                  "runStudy.R applies it once, on export, so no suppression is",
                  "generated here."), format(n))
  }

  # The guards compare the SAP's source keys against whatever cdmName the
  # partner's codeToRun.R used -- a mismatch cannot be detected here, only said
  all_sources <- sap_source_keys(sap)
  restricted <- Filter(function(x) is_source_restricted(x, all_sources),
                       c(sap$cohorts %|||% list(), sap$proposed_analyses %|||% list()))
  if (length(restricted)) {
    notes <- c(notes, sprintf(paste(
      "%d cohorts or analyses are restricted to named databases, so the script",
      "guards them with omopgenerics::cdmName(cdm). Check that codeToRun.R",
      "creates the cdm reference with the same source keys this SAP uses (%s),",
      "or the guard will never be true and those steps will silently not run."),
      length(restricted), paste(all_sources, collapse = ", ")))
  }
  notes
}

# Creates missing directories but never removes anything
write_study_files <- function(sap, dir) {
  files <- study_files(sap)
  paths <- character(0)
  for (rel in names(files)) {
    path <- file.path(dir, rel)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    # writeLines() adds a final newline, so strip trailing ones from the
    # content or every generated file ends on a blank line
    writeLines(sub("\n+$", "", files[[rel]]), path)
    paths <- c(paths, path)
  }
  # The by-category folders codelistCreation.R expects the CSVs in
  cited <- cited_codelist_names(sap$cohorts)
  for (cl in sap$codelists %|||% list()) {
    if (!as.character(cl$name %|||% "")[1] %in% cited) next
    dir.create(file.path(dir, dirname(codelist_csv_path(cl))),
               recursive = TRUE, showWarnings = FALSE)
  }
  invisible(paths)
}
