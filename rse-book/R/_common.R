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

survey_data_dir <- if (file.exists(file.path(book_root, 
paste0(DATA_DIR, "/2026_all_cols.csv")))) {
  file.path(book_root, DATA_DIR)
} else {
  file.path(book_root, paste0("../", DATA_DIR))
}
cols <- read.csv(file.path(survey_data_dir, "2026_all_cols.csv"))
