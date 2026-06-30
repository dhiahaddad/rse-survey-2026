book_root <- Sys.getenv("QUARTO_PROJECT_DIR", unset = NA_character_)
if (is.na(book_root) || !nzchar(book_root)) {
  book_root <- if (file.exists("_quarto.yml")) {
    "."
  } else if (file.exists("../_quarto.yml")) {
    ".."
  } else {
    "rse-book"
  }
}
source(file.path(book_root, "R/_common.R"))
