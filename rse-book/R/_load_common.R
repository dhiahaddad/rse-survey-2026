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
common_path <- file.path(book_root, "R/_common.R")
if (!file.exists(common_path)) {
  candidates <- unique(c(
    file.path(book_root, ".."),
    if (file.exists("_quarto.yml")) "." else NULL,
    if (file.exists("../_quarto.yml")) ".." else NULL,
    "rse-book"
  ))
  candidate_paths <- file.path(candidates, "R/_common.R")
  hit <- candidates[file.exists(candidate_paths)]
  if (length(hit) > 0L) {
    book_root <- hit[[1]]
    common_path <- file.path(book_root, "R/_common.R")
  }
}
source(common_path)
