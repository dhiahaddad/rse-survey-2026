book_root <- Sys.getenv("QUARTO_PROJECT_DIR", unset = NA_character_)
if (is.na(book_root) || !nzchar(book_root)) {
  book_root <- if (file.exists("_quarto.yml")) {
    "."
  } else if (file.exists("../_quarto.yml")) {
    ".."
  } else {
    "."
  }
}

source(file.path(book_root, "R/postprocessing.R"))
df <- read.csv(file.path(book_root, "data/df_all.csv"))
df2 <- read.csv(file.path(book_root, "data/df_institute.csv"))
df3 <- read.csv(file.path(book_root, "data/df_proj.csv"))

survey_data_dir <- if (file.exists(file.path(book_root, 
"RSE_survey_2026_data/2026_all_cols.csv"))) {
  file.path(book_root, "RSE_survey_2026_data")
} else {
  file.path(book_root, "../RSE_survey_2026_data")
}
cols <- read.csv(file.path(survey_data_dir, "2026_all_cols.csv"))
