#' GFB3 Curation Log Template
#'
#' Returns a curation log template string for documenting GFB3 data processing
#' decisions. Fill in the bracketed placeholders before passing to
#' \code{report_gfb3()} via the \code{curation_log} argument.
#'
#' @return A character string with section headers and placeholder text.
#' @export
curation_log_template <- function() {
  "
DATASET: [dataset name or file name]
COUNTRY: [country]
SITE: [site name]
PI: [PI name]
CURATOR: [your name]
DATE RECEIVED: [YYYY-MM-DD]
DATE PROCESSED: [YYYY-MM-DD]

--- SOURCE FORMAT ---
[Describe the original format: wide/long, number of census layers, column naming
conventions, units, any non-standard structure the contributor used.]

--- PIVOT / RESTRUCTURING ---
[Describe how the data was reshaped into GFB3 paired-census format, including
which columns mapped to DBH/YR/PrevDBH/PrevYR/Status and any assumptions made
(e.g. inferred status for anchor census layer).]

--- DUPLICATE RESOLUTION ---
[Number of duplicate TreeIDs found. For each group: how many were identical vs
conflicting, what criterion was used to resolve them, and which trees were
excluded pending contributor clarification.]

--- MISSING / INTERPOLATED DATA ---
[Any trees with interpolated or imputed measurements, missed census visits, or
structural missingness (e.g. no PrevDBH for anchor layer). Note whether these
were flagged in the source or identified during curation.]

--- SPECIES ISSUES ---
[Unidentified trees (count and how recoded), malformed names, known synonyms
corrected, or species excluded.]

--- EXCLUSIONS ---
[Any trees, plots, or records removed and the reason: unresolvable duplicates,
implausible DBH, out-of-range coordinates, etc.]

--- NOTES ---
[Anything else worth flagging for downstream users: known protocol differences,
plot size caveats, DBH threshold, contributor-specific conventions, pending
follow-up items.]
"
}
