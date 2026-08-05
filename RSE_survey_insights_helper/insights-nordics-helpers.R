# Entry point for Nordic insights helpers.
# Sources themed modules in dependency order.

insights_source_modules <- function() {
  root <- if (file.exists("insights-nordics-theme.R")) {
    "."
  } else if (dir.exists("RSE_survey_insights_helper")) {
    "RSE_survey_insights_helper"
  } else if (file.exists("../RSE_survey_insights_helper/insights-nordics-theme.R")) {
    "../RSE_survey_insights_helper"
  } else if (file.exists("../insights-nordics-theme.R")) {
    ".."
  } else {
    stop("Could not locate insights-nordics module files.")
  }
  source(file.path(root, "insights-nordics-theme.R"), local = FALSE)
  source(file.path(root, "insights-nordics-captions.R"), local = FALSE)
  source(file.path(root, "insights-nordics-data.R"), local = FALSE)
  source(file.path(root, "insights-nordics-plots.R"), local = FALSE)
}

insights_source_modules()

#' Initialise data, metadata, and recode maps for the Nordic insights document
#'
#' @return List with the book path, country filter, respondent data frame,
#'   excluded partial-response count, and survey metadata (`meta`).
#' @export
insights_init <- function() {
  book_root <- if (dir.exists("rse-book/R")) {
    "rse-book"
  } else if (dir.exists("../rse-book/R")) {
    "../rse-book"
  } else if (dir.exists("R")) {
    "."
  } else {
    stop("Could not locate rse-book R sources.")
  }

  Sys.setenv(QUARTO_PROJECT_DIR = normalizePath(book_root, mustWork = TRUE))
  source(file.path(book_root, "R/_common.R"), local = parent.frame())
  source(
    file.path(book_root, "R/recodes/currentEmp5_0_recode.R"),
    local = parent.frame()
  )
  source(
    file.path(book_root, "R/recodes/skill2_recode.R"),
    local = parent.frame()
  )

  # Always Nordic: independent of chapter FILTER in R/.config.
  nordic_countries <- if (exists("NORDIC_COUNTRIES", inherits = TRUE)) {
    NORDIC_COUNTRIES
  } else {
    c("Finland", "Norway", "Sweden", "Denmark", "Iceland", "Estonia")
  }
  df_nordics <- load_filtered_tf(nordic_countries)
  n_excluded <- survey_unsubmitted_n(nordic_countries)

  list(
    book_root = book_root,
    nordic_countries = nordic_countries,
    meta = cols,
    df_nordics = df_nordics,
    n_excluded = n_excluded
  )
}

get_current_emp5_coded <- function(filter) {
  process_current_emp5_with_allocation(
    filter = filter,
    recode_map = currentEmp5_recode_map,
    header = "Official job title (Nordics)",
    other = FALSE,
    cache_id = "currentEmp5_0"
  )
}

get_skill2_coded <- function(filter) {
  process_text_codes_with_allocation(
    filter = filter,
    question_code = "skill2",
    recode_map = skill2_recode_map,
    header = "Skills to acquire (Nordics)",
    other = FALSE,
    cache_id = "skill2"
  )
}
