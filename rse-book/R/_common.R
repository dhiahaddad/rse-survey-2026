book_root <- Sys.getenv("QUARTO_PROJECT_DIR", unset = NA_character_)
if (is.na(book_root) || !nzchar(book_root)) {
  book_root <- if (file.exists("_quarto.yml")) {
    "."
  } else if (file.exists("../_quarto.yml")) {
    ".."
  } else {
    "rse-book/"
  }
}

source(file.path(book_root, "R/postprocessing.R"))
source(file.path(book_root, "R/.config"))

if (!exists("NORDIC_COUNTRIES", inherits = FALSE)) {
  NORDIC_COUNTRIES <- c(
    "Finland", "Norway", "Sweden", "Denmark", "Iceland", "Estonia"
  )
}
if (!exists("FILTER_COMPARE", inherits = FALSE)) {
  FILTER_COMPARE <- list(
    Germany = "Germany",
    Netherlands = "Netherlands"
  )
}

# Chapter-wide filter context derived from R/.config
nordic_countries <- NORDIC_COUNTRIES
filter_countries <- normalize_country_filter(FILTER)
filter_label <- filter_scope_label(FILTER)
show_within <- length(filter_countries) > 1L
country_groups <- build_between_country_groups(FILTER, FILTER_COMPARE)
plot_countries <- countries_from_groups(country_groups)

survey_data_dir <- if (file.exists(file.path(book_root, 
paste0(DATA_DIR, "/2026_all_cols.csv")))) {
  file.path(book_root, DATA_DIR)
} else {
  file.path(book_root, paste0("../", DATA_DIR))
}
cols <- read.csv(file.path(survey_data_dir, "2026_all_cols.csv"))
