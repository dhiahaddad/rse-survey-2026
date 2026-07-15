library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(forcats)
library(gt)
library(stringr)
library(purrr)
library(scales)

#' Return a gt table for empty question results
#'
#' @param header Table title.
#' @param message User-facing empty-state message.
#' @return A gt table object.
#' @keywords internal
empty_results_gt <- function(
    header,
    message = "No responses available for the requested filter."
) {
  tibble::tibble(
    category = message,
    count = NA_integer_,
    pct = ""
  ) |>
    gt::gt() |>
    gt::tab_header(title = header, subtitle = message) |>
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = "category")
    )
}

#' Return an empty ggplot for unavailable question results
#'
#' @param message User-facing empty-state message.
#' @return A ggplot object.
#' @keywords internal
empty_results_plot <- function(
    message = "No responses available for the requested filter."
) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = message,
      size = 4
    ) +
    ggplot2::theme_void()
}

#' Test whether each string matches any regex pattern
#'
#' @param x Character vector to test.
#' @param patterns Character vector of regex patterns.
#' @return Logical vector, one value per element of `x`.
#' @keywords internal
matches_any_pattern <- function(x, patterns) {
  if (length(patterns) == 0L) {
    return(rep(FALSE, length(x)))
  }
  stringr::str_detect(x, stringr::str_c(patterns, collapse = "|"))
}

#' Apply sequential regex-based recoding (first match wins)
#'
#' Each row in `map_df` is applied in order. Values are replaced only when
#' they still match the original input and the row's pattern matches.
#'
#' @param x Character vector of raw responses.
#' @param map_df Tibble with columns `raw` (regex pattern) and `clean` 
#' (replacement).
#' @return Character vector with recoded values.
#' @keywords internal
recode_with_regex <- function(x, map_df) {
  if (nrow(map_df) == 0L) {
    return(x)
  }
  result <- x
  for (i in seq_len(nrow(map_df))) {
    matched <- stringr::str_detect(x, map_df$raw[i])
    result[matched] <- map_df$clean[i]
  }
  result
}

#' Split a recode map into exclusion patterns and active recoding rules
#'
#' @param recode_map Tibble with columns `raw` and `clean`. Rows with `NA` in
#'   `clean` denote responses to exclude from analysis.
#' @return A list with `to_remove` (lowercased exclusion patterns) and
#'   `active_map` (rows with non-NA `clean`, patterns lowercased).
#' @keywords internal
prepare_recode_map <- function(recode_map) {
  list(
    to_remove = recode_map |>
      dplyr::filter(is.na(.data$clean)) |>
      dplyr::pull(.data$raw) |>
      stringr::str_to_lower(),
    active_map = recode_map |>
      dplyr::filter(!is.na(.data$clean)) |>
      dplyr::mutate(raw = stringr::str_to_lower(.data$raw))
  )
}

#' Normalize and tokenize free-text responses for coding
#'
#' Filters to main or "other" responses, splits comma-separated values,
#' lowercases, trims whitespace, and drops empty tokens.
#'
#' @param df Survey data frame containing `is_other` and `col_name`.
#' @param col_name Column with free-text responses (may be comma-separated).
#' @param other Logical; `TRUE` for "other/specify" rows, `FALSE` for main rows.
#' @return Tibble with one row per normalized token in `col_name`.
#' @keywords internal
prepare_text_tokens <- function(df, col_name, other = TRUE) {
  as_tibble(df) |>
    dplyr::filter(is_other == other) |>
    dplyr::select(dplyr::all_of(col_name)) |>
    tidyr::drop_na(dplyr::all_of(col_name)) |>
    dplyr::distinct() |>
    tidyr::separate_longer_delim(
      dplyr::all_of(col_name),
      delim = stringr::regex(",\\s*")
    ) |>
    dplyr::mutate(
      !!rlang::sym(col_name) := stringr::str_squish(
        stringr::str_to_lower(.data[[col_name]])
      )
    ) |>
    dplyr::filter(.data[[col_name]] != "")
}

#' Locate the raw survey data directory across known render contexts
#'
#' @return Character path to the directory holding `2026_*.csv` raw files.
#' @keywords internal
survey_raw_data_dir <- function() {
  existing <- tryCatch(
    get("survey_data_dir", envir = globalenv()),
    error = function(e) NULL
  )
  if (!is.null(existing) && file.exists(file.path(existing, "2026_tf.csv"))) {
    return(existing)
  }
  data_dir_name <- tryCatch(
    get("DATA_DIR", envir = globalenv()),
    error = function(e) "RSE_survey_2026_data"
  )
  book_root <- Sys.getenv("QUARTO_PROJECT_DIR", unset = "")
  candidates <- c(
    file.path(book_root, data_dir_name),
    file.path(book_root, "..", data_dir_name),
    data_dir_name,
    file.path("..", data_dir_name),
    file.path("rse-book", "..", data_dir_name)
  )
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(file.path(candidates, "2026_tf.csv"))]
  if (length(hit) > 0L) {
    return(hit[[1]])
  }
  data_dir_name
}

#' Identify the `2026_tf.csv` columns that belong to one question code
#'
#' Matches the single column named exactly `question_code` (single-response
#' questions, e.g. `"conf2can_0"`) plus every multi-response sub-column of the
#' form `question_code[...]_0` (e.g. `"org2can[SQ001]_0"`). The trailing `[`
#' anchor prevents collisions between related codes (e.g. `currentEmp1` vs
#' `currentEmp13`, or `tool5` vs `tool5can`).
#'
#' @param question_code Survey question code (the column-name stem used in
#'   `2026_tf.csv`).
#' @param all_names Character vector of column names from `2026_tf.csv`.
#' @return Character vector of matching column names.
#' @keywords internal
question_columns <- function(question_code, all_names) {
  all_names[
    all_names == question_code |
      startsWith(all_names, paste0(question_code, "["))
  ]
}

#' Build the column-code -> option-label lookup from `2026_all_cols.csv`
#'
#' Used to decode multi-response check-box columns (which store `"True"` /
#' `"False"` in `2026_tf.csv`) into their human-readable option labels.
#'
#' @param data_dir Directory holding `2026_all_cols.csv`.
#' @return Named character vector mapping `New_name` (column code) to `Option`.
#' @keywords internal
question_label_lookup <- function(data_dir) {
  meta <- read.csv(
    file.path(data_dir, "2026_all_cols.csv"),
    check.names = FALSE
  )
  stats::setNames(as.character(meta$Option), as.character(meta$New_name))
}

#' Reshape a question's `2026_tf.csv` columns into long response rows
#'
#' Pivots the selected question columns to long form, decodes multi-response
#' check-box columns (`"True"` -> option label, `"False"` -> dropped), keeps
#' free-text columns verbatim, flags "other/specify" responses, and drops empty
#' values.
#'
#' @param df Respondent rows from `2026_tf.csv` (already country-filtered).
#' @param col_name Output column name for the response values.
#' @param question_cols Columns belonging to the question (see
#'   [question_columns()]).
#' @param label_lookup Named vector mapping column codes to option labels (see
#'   [question_label_lookup()]).
#' @return Tibble with `row_id`, `is_other`, and `question_code` columns.
#' @keywords internal
clean_cols <- function(df, question_code, question_cols, label_lookup) {
  df |>
    dplyr::select(row_id, dplyr::all_of(question_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(question_cols), as.character)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(question_cols),
      names_to = "question",
      values_to = "value"
    ) |>
    dplyr::mutate(
      label = unname(label_lookup[.data$question]),
      !!rlang::sym(question_code) := dplyr::case_when(
        .data$value == "True" ~ .data$label,
        .data$value == "False" ~ NA_character_,
        TRUE ~ .data$value
      ),
      is_other = stringr::str_detect(.data$question, "other")
    ) |>
    dplyr::select(row_id, is_other, dplyr::all_of(question_code)) |>
    tidyr::drop_na(dplyr::all_of(question_code)) |>
    dplyr::filter(.data[[question_code]] != "")
}

#' Read the full respondent table from `2026_tf.csv`
#'
#' @param data_dir Directory holding `2026_tf.csv`.
#' @return Respondent data frame with unnamed columns fixed.
#' @keywords internal
read_tf <- function(data_dir = survey_raw_data_dir()) {
  tf <- read.csv(
    file.path(data_dir, "2026_tf.csv"),
    check.names = FALSE
  )
  empty_names <- !nzchar(names(tf)) | is.na(names(tf))
  names(tf)[empty_names] <- paste0(".unnamed", seq_len(sum(empty_names)))
  tf
}

#' Test whether a survey row has a non-empty submission timestamp
#'
#' @param submitdate Character or POSIX submission timestamp from `submitdate_0`.
#' @return Logical scalar or vector.
#' @keywords internal
is_submitted_response <- function(submitdate) {
  !is.na(submitdate) & nzchar(trimws(as.character(submitdate)))
}

#' Keep respondents in scope with a recorded submission date
#'
#' @param tf Respondent data frame from [read_tf()].
#' @param filter Country filter passed to `socio1_0`.
#' @return Filtered data frame of submitted respondents.
#' @keywords internal
filter_survey_respondents <- function(tf, filter) {
  if (!"submitdate_0" %in% names(tf)) {
    stop("Expected column submitdate_0 in 2026_tf.csv.")
  }
  dplyr::filter(
    tf,
    .data$socio1_0 %in% filter,
    is_submitted_response(.data$submitdate_0)
  )
}

#' Count partial responses excluded by the submission-date filter
#'
#' @param filter Country filter passed to `socio1_0`.
#' @param data_dir Directory holding `2026_tf.csv`.
#' @return Integer number of respondents in scope without `submitdate_0`.
#' @export
survey_unsubmitted_n <- function(filter, data_dir = survey_raw_data_dir()) {
  tf <- read_tf(data_dir)
  sum(
    tf$socio1_0 %in% filter &
      !is_submitted_response(tf$submitdate_0)
  )
}

#' Build the dataset for one question and country filter on the fly
#'
#' Reads the full respondent table `2026_tf.csv`, keeps submitted rows whose
#' `socio1_0` country is in `filter`, selects the columns matching the
#' question's code, and reshapes them into the `row_id` / `is_other` / value
#' tibble used downstream. No per-question CSV files are read.
#'
#' @param question_code The question code, i.e. the column-name stem in
#'   `2026_tf.csv` (e.g. `"conf2can_0"` for a single-response question or
#'   `"org2can"` for the `org2can[...]_0` multi-response sub-columns). Also used
#'   as the name of the returned value column.
#' @param filter A single country name or a vector of country names, matched
#'   against the `socio1_0` column of `2026_tf.csv`.
#' @param data_dir Directory holding `2026_tf.csv` and `2026_all_cols.csv`.
#' @return Tibble with `row_id`, `is_other`, and `question_code` columns.
#' @keywords internal
load_question_data <- function(
    question_code,
    filter,
    data_dir = survey_raw_data_dir()
) {
  tf <- filter_survey_respondents(read_tf(data_dir), filter)
  question_cols <- question_columns(question_code, names(tf))
  if (length(question_cols) == 0L) {
    stop(sprintf(
      "No columns in 2026_tf.csv match question code '%s'.",
      question_code
    ))
  }
  clean_cols(tf, question_code, question_cols, question_label_lookup(data_dir))
}

#' Resolve the allocation-cache directory for the current Quarto render context
#'
#' @return Character path to `_allocation_cache`.
#' @keywords internal
allocation_cache_dir <- function() {
  book_root <- Sys.getenv("QUARTO_PROJECT_DIR", unset = NA_character_)
  if (!is.na(book_root)) {
    return(file.path(book_root, "_allocation_cache"))
  }
  if (file.exists("_quarto.yml")) {
    return("_allocation_cache")
  }
  if (file.exists("../_quarto.yml")) {
    return("../_allocation_cache")
  }
  if (dir.exists("rse-book")) {
    return("rse-book/_allocation_cache")
  }
  "_allocation_cache"
}

#' List cached allocation `.rds` files across known book locations
#'
#' @return Character vector of file paths, sorted.
#' @keywords internal
allocation_cache_files <- function() {
  dirs <- unique(c(
    allocation_cache_dir(),
    "_allocation_cache",
    "../_allocation_cache",
    "rse-book/_allocation_cache",
    "chapters/_allocation_cache"
  ))
  dirs <- dirs[dir.exists(dirs)]
  sort(unlist(lapply(dirs, function(d) {
    list.files(d, pattern = "\\.rds$", full.names = TRUE)
  })))
}

#' Persist an allocation table for the recoding appendix
#'
#' @param allocation_tbl Output of [allocate_text_codes_from_tokens()].
#' @param cache_id Unique identifier (typically the question id).
#' @param header Display title for the allocation table.
#' @param other Logical; whether this table covers "other" free-text responses.
#' @param show_excluded Whether excluded rows should appear in the appendix 
#' table.
#' @keywords internal
save_allocation_cache <- function(
    allocation_tbl,
    cache_id,
    header,
    other = TRUE,
    show_excluded = TRUE
) {
  cache_dir <- allocation_cache_dir()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(
      allocation = allocation_tbl,
      header = header,
      other = other,
      show_excluded = show_excluded,
      cache_id = cache_id
    ),
    file.path(cache_dir, paste0(cache_id, ".rds"))
  )
  invisible(NULL)
}

#' Build raw-to-category allocation audit table from tokenized responses
#'
#' @param tokens Output of [prepare_text_tokens()].
#' @param col_name Column name containing normalized tokens.
#' @param to_remove Lowercased regex patterns for excluded responses.
#' @param active_map Tibble of active recoding rules.
#' @return Tibble with columns `raw`, `category`, `allocation`, and `n`.
#' @keywords internal
allocate_text_codes_from_tokens <- function(
  tokens, col_name, to_remove, active_map
  ) {
  tokens |>
    dplyr::mutate(
      raw = .data[[col_name]],
      excluded = matches_any_pattern(.data$raw, to_remove),
      category = dplyr::if_else(
        .data$excluded,
        NA_character_,
        recode_with_regex(.data$raw, active_map)
      ),
      allocation = dplyr::case_when(
        .data$excluded ~ "Excluded",
        .data$raw != .data$category ~ "Recoded",
        TRUE ~ "Unchanged"
      )
    ) |>
    dplyr::count(.data$raw, .data$category, .data$allocation, name = "n") |>
    dplyr::group_by(.data$category) |>
    dplyr::mutate(cat_n = sum(.data$n)) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      is.na(.data$category),
      dplyr::desc(.data$cat_n),
      .data$category,
      dplyr::desc(.data$n),
      .data$raw
    ) |>
    dplyr::select(-"cat_n")
}

#' Summarize recoded free-text tokens with counts and percentages
#'
#' @param tokens Output of [prepare_text_tokens()].
#' @param col_name Column name containing normalized tokens.
#' @param to_remove Lowercased regex patterns for excluded responses.
#' @param active_map Tibble of active recoding rules.
#' @return Tibble with `col_name`, `n`, and `pct` columns.
#' @keywords internal
summarize_text_codes_from_tokens <- function(
  tokens, col_name, to_remove, active_map
  ) {
  tokens |>
    dplyr::filter(!matches_any_pattern(.data[[col_name]], to_remove)) |>
    dplyr::mutate(
      !!rlang::sym(col_name) := recode_with_regex(.data[[col_name]], active_map)
    ) |>
    dplyr::count(.data[[col_name]], name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::mutate(pct = scales::percent(.data$n / sum(.data$n), accuracy = 0.1))
}

#' Resolve official job titles, falling back to alternate titles when needed
#'
#' Uses `currentEmp5_0` when present. When that field is empty and the
#' respondent indicated they are known by a different title
#' (`currentEmp6_0 == "True"`), the value from `currentEmp60_0` is used
#' instead.
#'
#' @param tf Respondent data frame from [read_tf()].
#' @return Character vector of resolved job titles (may contain `NA`).
#' @keywords internal
resolve_current_emp5_titles <- function(tf) {
  official <- trimws(as.character(tf$currentEmp5_0))
  alternate <- trimws(as.character(tf$currentEmp60_0))
  uses_alt <- as.character(tf$currentEmp6_0) == "True"

  dplyr::case_when(
    !is.na(official) & nzchar(official) ~ official,
    uses_alt & !is.na(alternate) & nzchar(alternate) ~ alternate,
    TRUE ~ NA_character_
  )
}

#' Load resolved official job titles for one country filter
#'
#' @param filter Country filter passed to [filter_survey_respondents()].
#' @param data_dir Directory holding `2026_tf.csv`.
#' @return Tibble with `row_id`, `is_other`, and `currentEmp5_0` columns.
#' @export
load_current_emp5_data <- function(
    filter,
    data_dir = survey_raw_data_dir()
) {
  tf <- filter_survey_respondents(read_tf(data_dir), filter)
  tibble::tibble(
    row_id = tf$row_id,
    is_other = FALSE,
    currentEmp5_0 = resolve_current_emp5_titles(tf)
  ) |>
    dplyr::filter(
      !is.na(.data$currentEmp5_0),
      .data$currentEmp5_0 != ""
    )
}

#' Count respondents with a resolved official job title
#'
#' Includes alternate titles from `currentEmp60_0` when
#' `currentEmp6_0 == "True"` and the official title is missing.
#'
#' @param filter Country filter passed to [load_current_emp5_data()].
#' @param data_dir Directory holding `2026_tf.csv`.
#' @return Integer number of respondents with a resolved title.
#' @export
current_emp5_n_respondents <- function(
    filter,
    data_dir = survey_raw_data_dir()
) {
  nrow(load_current_emp5_data(filter, data_dir))
}

#' Recode free-text responses from a pre-built question tibble
#'
#' @param df Tibble with `row_id`, `is_other`, and `question_code` columns.
#' @param question_code Column name containing free-text responses.
#' @param recode_map Tibble with `raw` (regex) and `clean` (category) columns.
#' @param header Chapter display title used in table headers.
#' @param other Logical; `TRUE` for "other/specify" rows (default), `FALSE` for
#'  main.
#' @param show_excluded Include excluded rows in the allocation appendix table.
#' @param cache_id If set, save allocation data under this id for the recoding
#'  appendix.
#' @return A list with `coded`, `allocation`, `summary_table`, and
#'   `allocation_table`.
#' @keywords internal
process_text_codes_from_df <- function(
    df,
    question_code,
    recode_map,
    header,
    other = TRUE,
    show_excluded = TRUE,
    cache_id = NULL
) {
  map <- prepare_recode_map(recode_map)
  tokens <- prepare_text_tokens(df, question_code, other = other)

  allocation_tbl <- allocate_text_codes_from_tokens(
    tokens,
    question_code,
    map$to_remove,
    map$active_map
  )
  coded_tbl <- summarize_text_codes_from_tokens(
    tokens,
    question_code,
    map$to_remove,
    map$active_map
  )

  if (!is.null(cache_id)) {
    save_allocation_cache(
      allocation_tbl,
      cache_id = cache_id,
      header = header,
      other = other,
      show_excluded = show_excluded
    )
  }

  list(
    coded = coded_tbl,
    allocation = allocation_tbl,
    summary_table = render_code_table(
      coded_tbl, question_code, header, other = other
    ),
    allocation_table = render_allocation_table(
      allocation_tbl,
      header = header,
      other = other,
      show_excluded = show_excluded
    )
  )
}

#' Recode official job titles, including alternate titles when provided
#'
#' @inheritParams process_text_codes_with_allocation
#' @return A list with `coded` (summary tibble), `allocation` (audit tibble),
#'   `summary_table` (gt), and `allocation_table` (gt).
#' @export
process_current_emp5_with_allocation <- function(
    filter,
    recode_map,
    header,
    other = FALSE,
    show_excluded = TRUE,
    cache_id = "currentEmp5_0",
    data_dir = survey_raw_data_dir()
) {
  df <- load_current_emp5_data(filter, data_dir)
  process_text_codes_from_df(
    df,
    question_code = "currentEmp5_0",
    recode_map = recode_map,
    header = header,
    other = other,
    show_excluded = show_excluded,
    cache_id = cache_id
  )
}

#' Recode free-text responses and render summary plus allocation tables
#'
#' Main entry point for question chapters. Builds the dataset for the requested
#' country `filter` on the fly, tokenizes responses once, applies the recode
#' map, optionally caches the allocation audit trail, and returns both gt
#' tables.
#'
#' @param filter Country filter for the respondents to include: a single
#'   country name or a vector of country names. The dataset is computed
#'   accordingly via [load_question_data()].
#' @param question_code The question code to recode, i.e. the column-name stem
#'   in `2026_tf.csv` (e.g. `"conf2can_0"`). Passed to [load_question_data()].
#' @param recode_map Tibble with `raw` (regex) and `clean` (category) columns.
#'   Rows with `NA` in `clean` exclude matching responses.
#' @param header Chapter display title used in table headers.
#' @param other Logical; `TRUE` for "other/specify" rows (default), `FALSE` for
#'  main.
#' @param show_excluded Include excluded rows in the allocation appendix table.
#' @param cache_id If set, save allocation data under this id for the recoding
#'  appendix.
#' @return A list with `coded` (summary tibble), `allocation` (audit tibble),
#'   `summary_table` (gt), and `allocation_table` (gt).
#' @export
process_text_codes_with_allocation <- function(
    filter,
    question_code,
    recode_map,
    header,
    other = TRUE,
    show_excluded = TRUE,
    cache_id = NULL
) {
  df <- load_question_data(question_code, filter)
  process_text_codes_from_df(
    df,
    question_code = question_code,
    recode_map = recode_map,
    header = header,
    other = other,
    show_excluded = show_excluded,
    cache_id = cache_id
  )
}

#' Render a gt summary table from recoded response counts
#'
#' @param coded_tbl Tibble from [summarize_text_codes_from_tokens()].
#' @param col_name Column containing category labels.
#' @param header Display title for the table.
#' @param other Logical; adds "[Others]" or "[Main]" suffix to the title.
#' @return A gt table object.
#' @keywords internal
render_code_table <- function(coded_tbl, col_name, header, other = TRUE) {
  suffix <- if (other) "[Others]" else "[Main]"

  coded_tbl |>
    gt::gt() |>
    gt::tab_header(title = paste(header, suffix)) |>
    gt::cols_label(
      !!rlang::sym(col_name) := "Response Category",
      n = "Frequency (n)",
      pct = "Percentage (%)"
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_options(table.font.size = gt::px(14))
}

#' Render a gt allocation audit table
#'
#' @param allocation_tbl Tibble from [allocate_text_codes_from_tokens()].
#' @param header Display title for the table.
#' @param other Logical; adds "[Others]" or "[Main]" suffix to the title.
#' @param show_excluded If `FALSE`, rows with allocation type "Excluded" are
#'  dropped.
#' @return A gt table object.
#' @export
render_allocation_table <- function(
    allocation_tbl,
    header,
    other = TRUE,
    show_excluded = TRUE
) {
  suffix <- if (other) "[Others]" else "[Main]"
  tbl <- allocation_tbl
  if (!show_excluded) {
    tbl <- tbl |> dplyr::filter(.data$allocation != "Excluded")
  }

  tbl |>
    gt::gt() |>
    gt::tab_header(
      title = paste(header, "raw-to-category allocation", suffix),
      subtitle = "Each row links one normalized raw response to its category in the summary table"
    ) |>
    gt::cols_label(
      raw = "Raw response",
      category = "Assigned category",
      allocation = "Allocation type",
      n = "Frequency (n)"
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_options(table.font.size = gt::px(14))
}

#' Render all cached allocation tables for the recoding appendix
#'
#' Reads `.rds` files from known cache locations and prints gt tables via
#' `results: asis` chunks. Skips duplicate `cache_id` values.
#'
#' @return Invisibly returns `NULL`.
#' @export
render_allocation_appendix <- function() {
  files <- allocation_cache_files()
  if (length(files) == 0) {
    cat("*No allocation tables have been generated yet. Render the question chapters first.*\n")
    return(invisible(NULL))
  }

  seen_ids <- character()
  for (f in files) {
    obj <- readRDS(f)
    if (obj$cache_id %in% seen_ids) {
      next
    }
    seen_ids <- c(seen_ids, obj$cache_id)
    cat("\n\n### ", obj$header, "\n\n", sep = "")
    print(render_allocation_table(
      obj$allocation,
      header = obj$header,
      other = obj$other,
      show_excluded = obj$show_excluded
    ))
  }
  invisible(NULL)
}

#' Plot a respondent-by-category selection heatmap with marginal counts
#'
#' Visualizes multi-select (non-other) responses: each row is a respondent,
#' each column is a category, and a filled tile indicates selection. Builds the
#' dataset for the requested country `filter` on the fly.
#'
#' @param filter Country filter for the respondents to include: a single
#'   country name or a vector of country names. The dataset is computed
#'   accordingly via [load_question_data()].
#' @param question_code The question code to plot, i.e. the column-name stem in
#'   `2026_tf.csv` (e.g. `"currentEmp13"`). Passed to [load_question_data()].
#' @param title Optional overall plot title spanning both panels. `NULL`
#'   (default) draws no title.
#' @param accent_color Fill color for selected cells and marginal bars.
#' @param panel_theme Optional `theme()` applied to both panels. The marginal
#'   bar chart always hides y-axis labels so they are not duplicated from the
#'   heatmap.
#' @param y_labels Label formatter for the shared y-axis categories. Defaults to
#'   [scales::label_wrap()] at 40 characters.
#' @param wrap_y_in_data If `TRUE`, apply `y_labels` to factor levels in the
#'   plot data so multi-line labels render reliably (including in patchwork).
#' @return A patchwork object combining the heatmap and marginal bar chart.
#' @export
plot_heatmap <- function(
    filter,
    question_code,
    title = NULL,
    accent_color = "darkblue",
    panel_theme = NULL,
    y_labels = scales::label_wrap(40),
    wrap_y_in_data = FALSE
) {
  plot_data <- load_question_data(question_code, filter) |>
    as_tibble() |>
    dplyr::select(row_id, is_other, dplyr::all_of(question_code)) |>
    dplyr::filter(is_other == FALSE) |>
    dplyr::distinct(row_id, !!rlang::sym(question_code)) |>
    tidyr::drop_na() |>
    dplyr::mutate(value = 1L) |>
    tidyr::complete(
      row_id,
      !!rlang::sym(question_code),
      fill = list(value = 0L)
    ) |>
    dplyr::group_by(row_id) |>
    dplyr::mutate(row_sum = sum(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data[[question_code]]) |>
    dplyr::mutate(cat_sum = sum(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      row_id = forcats::fct_reorder(row_id, row_sum),
      !!rlang::sym(question_code) := forcats::fct_reorder(
        .data[[question_code]],
        .data$cat_sum
      )
    )

  if (isTRUE(wrap_y_in_data) && is.function(y_labels)) {
    mapping <- stats::setNames(
      y_labels(levels(plot_data[[question_code]])),
      levels(plot_data[[question_code]])
    )
    col_vals <- as.character(plot_data[[question_code]])
    plot_data[[question_code]] <- factor(
      unname(mapping[col_vals]),
      levels = unname(mapping[levels(plot_data[[question_code]])])
    )
    y_scale <- ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.14))
  } else {
    y_scale <- ggplot2::scale_y_discrete(labels = y_labels)
  }

  p_heat <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = row_id,
      y = .data[[question_code]],
      fill = factor(value)
    )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.01) +
    ggplot2::scale_fill_manual(
      values = c("0" = "white", "1" = accent_color),
      labels = c("0" = "No", "1" = "Yes"),
      name = ""
    ) +
    y_scale +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Respondent (ordered by #)", y = NULL)

  if (!is.null(title)) {
    p_heat <- p_heat +
      ggplot2::labs(title = stringr::str_wrap(title, width = 70)) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      )
  }

  p_margin <- plot_data |>
    dplyr::distinct(!!rlang::sym(question_code), cat_sum) |>
    ggplot2::ggplot(ggplot2::aes(x = cat_sum, y = .data[[question_code]])) +
    ggplot2::geom_col(fill = accent_color) +
    ggplot2::geom_text(
      ggplot2::aes(label = cat_sum),
      hjust = -0.2,
      size = 3
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "n", y = NULL)

  if (!is.null(panel_theme)) {
    p_heat <- p_heat +
      panel_theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
    p_margin <- p_margin +
      panel_theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  }

  combined <- p_heat + p_margin +
    patchwork::plot_layout(widths = c(5, 2)) &
    ggplot2::theme(legend.position = "none")

  combined
}

#' Plot respondent-level heatmaps faceted by country
#'
#' Uses the same encoding as [plot_heatmap()] (one column per respondent, one row
#' per category), but splits the view into one panel per country so selection
#' patterns can be compared across countries. Categories share a common y-axis
#' order (by overall count); respondents are ordered within each country by the
#' number of selections.
#'
#' @param filter Country filter passed to [load_question_data()].
#' @param question_code Question code (e.g. `"currentEmp13"`).
#' @param countries Optional character vector giving panel order. Defaults to
#'   `filter`.
#' @param title Optional plot title.
#' @param ncol Number of facet columns.
#' @return A ggplot object.
#' @export
plot_heatmap_by_country <- function(
    filter,
    question_code,
    countries = NULL,
    title = NULL,
    ncol = 2L
) {
  if (is.null(countries)) {
    countries <- filter
  }

  raw <- load_question_selections(question_code, filter) |>
    dplyr::select(
      "row_id",
      "socio1_0",
      "is_other",
      dplyr::all_of(question_code)
    ) |>
    dplyr::filter(
      .data$is_other == FALSE,
      .data$socio1_0 %in% countries
    ) |>
    dplyr::distinct(
      .data$row_id,
      .data$socio1_0,
      !!rlang::sym(question_code)
    ) |>
    tidyr::drop_na() |>
    dplyr::mutate(value = 1L)

  if (nrow(raw) == 0L) {
    return(empty_results_gt(header = if (is.null(title)) question_code else title))
  }

  all_categories <- raw |>
    dplyr::distinct(!!rlang::sym(question_code)) |>
    dplyr::pull(!!rlang::sym(question_code))
  category_order <- raw |>
    dplyr::count(!!rlang::sym(question_code), name = "cat_sum", sort = TRUE) |>
    dplyr::pull(!!rlang::sym(question_code))
  category_order <- c(category_order, setdiff(all_categories, category_order))

  n_by_country <- raw |>
    dplyr::distinct(.data$row_id, .data$socio1_0) |>
    dplyr::count(.data$socio1_0, name = "n")
  country_order <- countries[countries %in% n_by_country$socio1_0]
  extra_countries <- setdiff(n_by_country$socio1_0, country_order)
  country_order <- c(country_order, extra_countries)
  country_labels <- n_by_country |>
    dplyr::mutate(
      socio1_0 = factor(.data$socio1_0, levels = country_order),
      label = paste0(.data$socio1_0, " (n=", .data$n, ")")
    ) |>
    dplyr::arrange(.data$socio1_0) |>
    dplyr::pull(.data$label)

  plot_data <- raw |>
    tidyr::complete(
      .data$row_id,
      .data$socio1_0,
      !!rlang::sym(question_code) := category_order,
      fill = list(value = 0L)
    ) |>
    dplyr::group_by(.data$socio1_0, .data$row_id) |>
    dplyr::mutate(row_sum = sum(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$socio1_0) |>
    dplyr::mutate(
      row_id = forcats::fct_reorder(
        as.character(.data$row_id),
        .data$row_sum,
        .fun = max,
        .desc = FALSE
      ),
      !!rlang::sym(question_code) := forcats::fct_relevel(
        .data[[question_code]],
        category_order
      ),
      socio1_0 = factor(.data$socio1_0, levels = country_order)
    ) |>
    dplyr::ungroup()

  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$row_id,
      y = .data[[question_code]],
      fill = factor(.data$value)
    )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.01) +
    ggplot2::scale_fill_manual(
      values = c("0" = "white", "1" = "darkblue"),
      labels = c("0" = "No", "1" = "Yes"),
      name = ""
    ) +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(35)) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$socio1_0),
      ncol = ncol,
      scales = "free_x",
      labeller = ggplot2::labeller(socio1_0 = country_labels)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.spacing = grid::unit(0.8, "lines")
    ) +
    ggplot2::labs(x = "Respondent (ordered by # selections)", y = NULL)

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = stringr::str_wrap(title, width = 90))
  }
  p
}

#' Plot discipline-selection pie charts faceted by country
#'
#' For multi-select questions, each pie shows the share of all discipline
#' selections within a country (slice sizes sum to 100% per country). Raw
#' selection counts are used; because respondents may select multiple
#' disciplines, this differs from the percentage of respondents per category.
#'
#' @param filter Country filter passed to [load_question_selections()].
#' @param question_code Question code (e.g. `"currentEmp13"`).
#' @param countries Optional character vector giving panel order. Defaults to
#'   `filter`. Ignored when `country_groups` is set.
#' @param country_groups Optional named list mapping group labels to one or more
#'   countries. Creates a `country_group` column and overrides `countries`.
#' @param title Optional plot title.
#' @param ncol Number of columns in the pie grid.
#' @param label_min_pct Minimum slice percentage for in-chart labels.
#' @return A patchwork object.
#' @export
plot_heatmap_category_piecharts <- function(
    filter,
    question_code,
    countries = NULL,
    country_groups = NULL,
    title = NULL,
    ncol = 3L,
    label_min_pct = 5
) {
  if (is.null(countries) && is.null(country_groups)) {
    countries <- filter
  }

  df <- load_question_selections(question_code, filter)
  grouped <- heatmap_category_grouped_df(
    df,
    question_code,
    group_by = countries,
    country_groups = country_groups
  )
  plot_df <- grouped$data
  group_col <- grouped$group_col

  if (is.null(group_col)) {
    stop("Country or country-group grouping is required for pie charts.")
  }
  if (nrow(plot_df) == 0L) {
    return(empty_results_plot())
  }

  n_by_group <- {
    respondents <- df |>
      dplyr::distinct(.data$row_id, .data$socio1_0)
    if (!is.null(country_groups)) {
      respondents <- assign_country_groups(respondents, country_groups)
    } else if (
      is.character(countries) &&
        length(countries) > 0L &&
        !any(countries %in% names(df))
    ) {
      respondents <- dplyr::filter(respondents, .data$socio1_0 %in% countries)
    }
    respondents |>
      dplyr::count(.data[[group_col]], name = "n")
  }
  group_order <- if (!is.null(country_groups)) {
    names(country_groups)
  } else if (
    is.character(countries) &&
      length(countries) > 0L &&
      !any(countries %in% names(df))
  ) {
    countries
  } else {
    n_by_group[[group_col]]
  }
  group_order <- group_order[group_order %in% n_by_group[[group_col]]]
  extra_groups <- setdiff(n_by_group[[group_col]], group_order)
  group_order <- c(group_order, extra_groups)
  group_labels <- stats::setNames(
    paste0(n_by_group[[group_col]], " (n=", n_by_group$n, ")"),
    n_by_group[[group_col]]
  )[group_order]

  plot_df <- plot_df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::mutate(
      slice_pct = 100 * .data$count / sum(.data$count),
      label = ifelse(
        .data$slice_pct >= label_min_pct,
        sprintf("%.0f%%", .data$slice_pct),
        NA_character_
      )
    ) |>
    dplyr::ungroup()
  plot_df[[group_col]] <- forcats::fct_relevel(plot_df[[group_col]], group_order)
  category_levels <- levels(plot_df$category)
  fill_values <- heatmap_category_palette(category_levels)

  pies <- lapply(group_order, function(group) {
    group_df <- plot_df |>
      dplyr::filter(.data[[group_col]] == group) |>
      dplyr::arrange(dplyr::desc(.data$category)) |>
      dplyr::mutate(
        ymax = cumsum(.data$count),
        ymin = dplyr::lag(.data$ymax, default = 0),
        ymid = (.data$ymin + .data$ymax) / 2
      )
    label_df <- group_df |>
      dplyr::filter(!is.na(.data$label))
    pie_plot <- ggplot2::ggplot(
      group_df,
      ggplot2::aes(
        x = "",
        y = .data$count,
        fill = .data$category
      )
    ) +
      ggplot2::geom_col(
        width = 1,
        color = "white",
        linewidth = 0.35
      ) +
      ggplot2::scale_fill_manual(
        values = fill_values,
        limits = category_levels,
        drop = FALSE,
        name = NULL
      ) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::labs(title = group_labels[[group]]) +
      ggplot2::theme_void() +
      ggplot2::theme(
        aspect.ratio = 1,
        legend.position = "none",
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          face = "bold",
          size = 11
        )
      )
    if (nrow(label_df) > 0L) {
      pie_plot <- pie_plot +
        ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(
            x = "",
            y = .data$ymid,
            label = .data$label
          ),
          inherit.aes = FALSE,
          size = 2.8,
          color = "grey10",
          fontface = "bold"
        )
    }
    pie_plot
  })

  legend_plot <- tibble::tibble(
    category = factor(category_levels, levels = category_levels)
  ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$category,
      y = 1,
      fill = .data$category
    )) +
    ggplot2::geom_col(width = 0.8, color = NA) +
    ggplot2::scale_fill_manual(
      values = fill_values,
      limits = category_levels,
      drop = FALSE,
      name = NULL
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 4)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = grid::unit(0.55, "cm"),
      legend.key = ggplot2::element_rect(
        fill = NA,
        colour = "grey35",
        linewidth = 0.25
      )
    )
  legend_grob <- {
    gtable <- ggplot2::ggplotGrob(legend_plot)
    idx <- which(vapply(gtable$grobs, function(x) x$name, character(1)) == "guide-box")
    if (length(idx) == 0L) {
      stop("Could not extract legend from category pie chart.")
    }
    gtable$grobs[[idx[[1L]]]]
  }

  combined <- patchwork::wrap_plots(
    patchwork::wrap_plots(pies, ncol = ncol),
    patchwork::wrap_elements(legend_grob),
    ncol = 1,
    heights = c(1, 0.15)
  )

  if (!is.null(title)) {
    combined <- combined +
      patchwork::plot_annotation(
        title = stringr::str_wrap(title, width = 90)
      )
  }
  combined
}

#' Plot category-selection stacked bar charts by country or country group
#'
#' For multi-select questions, each bar shows the share of all selections
#' within a country or group (segments sum to 100% per bar). Raw selection
#' counts are used; because respondents may select multiple categories, this
#' differs from the percentage of respondents per category.
#'
#' @inheritParams plot_heatmap_category_piecharts
#' @param bar_width Width of each stacked bar (0--1).
#' @return A ggplot object.
#' @export
plot_heatmap_category_stacked_bars <- function(
    filter,
    question_code,
    countries = NULL,
    country_groups = NULL,
    title = NULL,
    label_min_pct = 5,
    bar_width = 0.7
) {
  if (is.null(countries) && is.null(country_groups)) {
    countries <- filter
  }

  df <- load_question_selections(question_code, filter)
  grouped <- heatmap_category_grouped_df(
    df,
    question_code,
    group_by = countries,
    country_groups = country_groups
  )
  plot_df <- grouped$data
  group_col <- grouped$group_col

  if (is.null(group_col)) {
    stop("Country or country-group grouping is required for stacked bar charts.")
  }
  if (nrow(plot_df) == 0L) {
    return(empty_results_plot())
  }

  n_by_group <- {
    respondents <- df |>
      dplyr::distinct(.data$row_id, .data$socio1_0)
    if (!is.null(country_groups)) {
      respondents <- assign_country_groups(respondents, country_groups)
    } else if (
      is.character(countries) &&
        length(countries) > 0L &&
        !any(countries %in% names(df))
    ) {
      respondents <- dplyr::filter(respondents, .data$socio1_0 %in% countries)
    }
    respondents |>
      dplyr::count(.data[[group_col]], name = "n")
  }
  group_order <- if (!is.null(country_groups)) {
    names(country_groups)
  } else if (
    is.character(countries) &&
      length(countries) > 0L &&
      !any(countries %in% names(df))
  ) {
    countries
  } else {
    n_by_group[[group_col]]
  }
  group_order <- group_order[group_order %in% n_by_group[[group_col]]]
  extra_groups <- setdiff(n_by_group[[group_col]], group_order)
  group_order <- c(group_order, extra_groups)
  group_labels <- stats::setNames(
    paste0(n_by_group[[group_col]], " (n=", n_by_group$n, ")"),
    n_by_group[[group_col]]
  )[group_order]

  plot_df <- plot_df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::mutate(
      slice_pct = 100 * .data$count / sum(.data$count),
      label = ifelse(
        .data$slice_pct >= label_min_pct,
        sprintf("%.0f%%", .data$slice_pct),
        NA_character_
      )
    ) |>
    dplyr::ungroup()
  plot_df[[group_col]] <- forcats::fct_relevel(plot_df[[group_col]], group_order)
  category_levels <- levels(plot_df$category)
  fill_values <- heatmap_category_palette(category_levels)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data[[group_col]],
      y = .data$count,
      fill = .data$category
    )
  ) +
    ggplot2::geom_col(
      position = "fill",
      width = bar_width,
      color = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label),
      position = ggplot2::position_fill(vjust = 0.5),
      size = 3,
      color = "grey10",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = fill_values,
      limits = category_levels,
      drop = FALSE,
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_x_discrete(labels = group_labels) +
    ggplot2::labs(x = NULL, y = "Share of selections") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 4)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = grid::unit(0.55, "cm"),
      axis.text.x = ggplot2::element_text(face = "bold")
    )

  if (!is.null(title)) {
    p <- p + ggplot2::labs(
      title = stringr::str_wrap(title, width = 90)
    )
  }
  p
}

#' Load multi-select question responses with respondent country
#'
#' @param question_code Question code (column-name stem in `2026_tf.csv`).
#' @param filter Country filter passed to `socio1_0`.
#' @param data_dir Directory holding `2026_tf.csv`.
#' @return Tibble with `row_id`, `is_other`, `question_code`, and `socio1_0`.
#' @keywords internal
load_question_selections <- function(
    question_code,
    filter,
    data_dir = survey_raw_data_dir()
) {
  tf <- load_filtered_tf(filter, data_dir)
  question_cols <- question_columns(question_code, names(tf))
  if (length(question_cols) == 0L) {
    stop(sprintf(
      "No columns in 2026_tf.csv match question code '%s'.",
      question_code
    ))
  }
  clean_cols(tf, question_code, question_cols, question_label_lookup(data_dir)) |>
    dplyr::inner_join(
      tf |> dplyr::select("row_id", "socio1_0", "socio3_0"),
      by = "row_id"
    )
}

#' Count respondents who answered a survey question
#'
#' For single-select columns, counts non-missing values. For multi-select
#' questions, counts respondents with at least one selected option or non-empty
#' other text.
#'
#' @param filter Country filter passed to [load_filtered_tf()].
#' @param question_code Question code (column-name stem in `2026_tf.csv`).
#' @param data_dir Directory holding `2026_tf.csv`.
#' @return Integer number of respondents with an answer.
#' @export
question_n_respondents <- function(
    filter,
    question_code,
    data_dir = survey_raw_data_dir()
) {
  tf <- load_filtered_tf(filter, data_dir)
  question_cols <- question_columns(question_code, names(tf))
  if (length(question_cols) == 0L) {
    stop(sprintf(
      "No columns in 2026_tf.csv match question code '%s'.",
      question_code
    ))
  }
  if (length(question_cols) == 1L && identical(question_cols[[1L]], question_code)) {
    col <- question_cols[[1L]]
    return(sum(!is.na(tf[[col]]) & nzchar(as.character(tf[[col]]))))
  }
  answered <- apply(tf[, question_cols, drop = FALSE], 1, function(row) {
    any(
      row == "True" |
        (!is.na(row) & row != "False" & nzchar(as.character(row)))
    )
  })
  sum(answered)
}

#' Distinct fill palette for multi-select category plots
#'
#' Uses Okabe--Ito for the first eight categories and the HCL "Dark 3"
#' qualitative palette for additional levels (see R Journal guidance on
#' `palette.colors()` and `hcl.colors()`).
#'
#' @param categories Character vector of category labels.
#' @return Named character vector of hex colors.
#' @keywords internal
heatmap_category_palette <- function(categories) {
  n <- length(categories)
  okabe <- grDevices::palette.colors(8L, palette = "Okabe-Ito")
  okabe <- okabe[okabe != "#000000"]
  if (n <= length(okabe)) {
    colors <- okabe[seq_len(n)]
  } else {
    colors <- c(
      okabe,
      grDevices::hcl.colors(n - length(okabe), palette = "Dark 3")
    )
  }
  stats::setNames(colors, categories)
}

#' Prepare grouped category-selection counts for heatmap bar plots
#'
#' Counts distinct respondents per category (multi-select: one row per
#' respondent-category pair). Supports the same grouping options as
#' [plot_likert_barplot()].
#'
#' @param df Output of [load_question_selections()].
#' @param question_code Column containing category labels.
#' @param group_by Optional grouping column or country vector.
#' @param country_groups Optional named country groups.
#' @return List with `data` tibble (`category`, `count`, `percent`, optional
#'   group column) and `group_col`.
#' @keywords internal
heatmap_category_grouped_df <- function(
    df,
    question_code,
    group_by = NULL,
    country_groups = NULL
) {
  group_col <- NULL
  plot_df <- df |>
    dplyr::filter(.data$is_other == FALSE)

  if (!is.null(country_groups)) {
    plot_df <- assign_country_groups(plot_df, country_groups)
    group_col <- "country_group"
  } else if (
    is.character(group_by) &&
      length(group_by) == 1L &&
      group_by %in% names(df)
  ) {
    group_col <- group_by
    plot_df <- dplyr::filter(plot_df, !is.na(.data[[group_col]]))
  } else if (
    is.character(group_by) &&
      length(group_by) > 0L &&
      !any(group_by %in% names(df))
  ) {
    plot_df <- dplyr::filter(plot_df, .data$socio1_0 %in% group_by)
    plot_df$socio1_0 <- forcats::fct_relevel(plot_df$socio1_0, group_by)
    group_col <- "socio1_0"
  }

  distinct_cols <- c("row_id", "socio1_0", question_code)
  if (!is.null(group_col) && group_col %in% names(plot_df)) {
    distinct_cols <- c(distinct_cols, group_col)
  }
  plot_df <- plot_df |>
    dplyr::distinct(dplyr::across(dplyr::all_of(distinct_cols)))

  count_df <- if (is.null(group_col)) {
    plot_df |>
      dplyr::count(!!rlang::sym(question_code), name = "count") |>
      dplyr::rename(category = !!rlang::sym(question_code))
  } else {
    plot_df |>
      dplyr::count(
        .data[[group_col]],
        !!rlang::sym(question_code),
        name = "count"
      ) |>
      dplyr::rename(category = !!rlang::sym(question_code))
  }

  if (is.null(group_col)) {
    n_respondents <- dplyr::n_distinct(df$row_id)
    count_df <- count_df |>
      dplyr::mutate(
        percent = 100 * .data$count / n_respondents
      )
  } else {
    group_levels <- if (inherits(plot_df[[group_col]], "factor")) {
      levels(plot_df[[group_col]])
    } else {
      unique(as.character(plot_df[[group_col]]))
    }
    group_sizes_source <- if (!is.null(country_groups)) {
      assign_country_groups(df, country_groups)
    } else {
      plot_df
    }
    group_sizes <- group_sizes_source |>
      dplyr::distinct(.data$row_id, .data[[group_col]]) |>
      dplyr::filter(!is.na(.data[[group_col]])) |>
      dplyr::count(.data[[group_col]], name = "n_respondents")
    count_df <- count_df |>
      dplyr::left_join(group_sizes, by = group_col) |>
      dplyr::mutate(
        percent = 100 * .data$count / .data$n_respondents
      ) |>
      dplyr::select(-"n_respondents")
    count_df[[group_col]] <- factor(count_df[[group_col]], levels = group_levels)
  }

  count_df <- dplyr::filter(count_df, .data$count > 0L)

  category_order <- count_df |>
    dplyr::group_by(.data$category) |>
    dplyr::summarise(total = sum(.data$count), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total)) |>
    dplyr::pull(.data$category)

  count_df <- count_df |>
    dplyr::mutate(
      category = forcats::fct_relevel(.data$category, category_order)
    )

  list(data = count_df, group_col = group_col)
}

#' Count distinct respondents for multi-select heatmap tables
#'
#' @param df Output of [load_question_selections()].
#' @param group_col Optional grouping column (`socio1_0` or `country_group`).
#' @param group_by Optional country vector passed to [heatmap_category_grouped_df()].
#' @param country_groups Optional named country groups.
#' @return Tibble with `n_respondents` and, when grouped, the grouping column.
#' @keywords internal
heatmap_category_respondent_counts <- function(
    df,
    group_col = NULL,
    group_by = NULL,
    country_groups = NULL
) {
  respondents <- df

  if (!is.null(country_groups)) {
    respondents <- assign_country_groups(respondents, country_groups)
    group_col <- "country_group"
  } else if (
    is.character(group_by) &&
      length(group_by) == 1L &&
      group_by %in% names(df)
  ) {
    group_col <- group_by
    respondents <- dplyr::filter(respondents, !is.na(.data[[group_col]]))
  } else if (
    is.character(group_by) &&
      length(group_by) > 0L &&
      !any(group_by %in% names(df))
  ) {
    respondents <- dplyr::filter(respondents, .data$socio1_0 %in% group_by)
    group_col <- "socio1_0"
  }

  if (is.null(group_col)) {
    return(tibble::tibble(
      n_respondents = dplyr::n_distinct(respondents$row_id)
    ))
  }

  respondents |>
    dplyr::distinct(.data$row_id, .data[[group_col]]) |>
    dplyr::count(.data[[group_col]], name = "n_respondents")
}

#' Render a descriptive statistics table for a multi-select question
#'
#' Shows how many respondents selected each predefined category and the
#' corresponding respondent-based percentage. Percentages are computed within
#' each group when `group_by` or `country_groups` is set.
#'
#' @param filter Country filter passed to [load_question_selections()].
#' @param question_code Question code (e.g. `"currentEmp13"`).
#' @param header Display title for the table.
#' @param group_by Optional grouping column or country vector (see
#'   [heatmap_category_grouped_df()]).
#' @param country_groups Optional named country groups.
#' @param group_label Column label for the grouping variable when grouped.
#'   Defaults to `"Country"` for `socio1_0` and `"Country group"` for
#'   `country_group`.
#' @return A gt table object.
#' @export
render_heatmap_category_table <- function(
    filter,
    question_code,
    header,
    group_by = NULL,
    country_groups = NULL,
    group_label = NULL
) {
  df <- load_question_selections(question_code, filter)
  if (!is.null(group_by) && identical(group_by, "age_group")) {
    df <- assign_age_groups_two(df)
  }
  grouped <- heatmap_category_grouped_df(
    df,
    question_code,
    group_by = group_by,
    country_groups = country_groups
  )
  plot_df <- grouped$data
  group_col <- grouped$group_col

  if (nrow(plot_df) == 0L) {
    return(empty_results_gt(header = header))
  }

  respondent_counts <- heatmap_category_respondent_counts(
    df,
    group_col = group_col,
    group_by = group_by,
    country_groups = country_groups
  )
  n_total <- if (is.null(group_col)) {
    respondent_counts$n_respondents[[1L]]
  } else {
    sum(respondent_counts$n_respondents)
  }

  table_df <- plot_df |>
    dplyr::mutate(
      pct = sprintf("%.1f%%", .data$percent)
    )

  if (is.null(group_col)) {
    table_df <- table_df |>
      dplyr::arrange(dplyr::desc(.data$count)) |>
      dplyr::select(category, count, pct)

    return(
      table_df |>
        gt::gt() |>
        gt::tab_header(
          title = header,
          subtitle = paste0("Total respondents: ", n_total)
        ) |>
        gt::cols_label(
          category = "Category",
          count = "Respondents (n)",
          pct = "Percentage (%)"
        ) |>
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_options(table.font.size = gt::px(14))
    )
  }

  if (is.null(group_label)) {
    group_label <- if (identical(group_col, "country_group")) {
      "Country group"
    } else if (identical(group_col, "age_group")) {
      "Age group"
    } else {
      "Country"
    }
  }

  group_levels <- if (inherits(plot_df[[group_col]], "factor")) {
    levels(plot_df[[group_col]])
  } else {
    unique(as.character(plot_df[[group_col]]))
  }
  group_levels <- group_levels[group_levels %in% plot_df[[group_col]]]

  table_df <- table_df |>
    dplyr::mutate(
      !!rlang::sym(group_col) := factor(
        .data[[group_col]],
        levels = group_levels
      )
    ) |>
    dplyr::arrange(.data[[group_col]], dplyr::desc(.data$count))

  group_vec <- table_df[[group_col]]
  display_df <- table_df |>
    dplyr::select(category, count, pct)

  gt_tbl <- display_df |>
    gt::gt() |>
    gt::tab_header(
      title = header,
      subtitle = paste0(
        "Total respondents: ",
        n_total,
        ". Percentages are within each ",
        tolower(group_label),
        "."
      )
    ) |>
    gt::cols_label(
      category = "Category",
      count = "Respondents (n)",
      pct = "Percentage (%)"
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_options(table.font.size = gt::px(14))

  row_groups <- split(seq_len(nrow(display_df)), group_vec)
  group_order <- group_levels
  for (group_name in group_order) {
    if (!group_name %in% names(row_groups)) {
      next
    }
    n_group <- respondent_counts$n_respondents[
      match(group_name, respondent_counts[[group_col]])
    ]
    gt_tbl <- gt_tbl |>
      gt::tab_row_group(
        label = paste0(group_name, " (n=", n_group, ")"),
        rows = row_groups[[group_name]]
      )
  }

  gt_tbl
}

#' Render a multi-select descriptive statistics table for Quarto output
#'
#' Returns a gt table object for reliable Quarto/knitr rendering when used as
#' the final expression in a code chunk.
#'
#' @inheritParams render_heatmap_category_table
#' @return A gt table object.
#' @export
print_heatmap_category_table <- function(
    filter,
    question_code,
    header,
    group_by = NULL,
    country_groups = NULL,
    group_label = NULL
) {
  render_heatmap_category_table(
    filter = filter,
    question_code = question_code,
    header = header,
    group_by = group_by,
    country_groups = country_groups,
    group_label = group_label
  )
}

#' Render a wide multi-select table with age groups in columns
#'
#' Shows discipline (or other category) selections with three age groups
#' (`Below 35`, `35-45`, and `45+`) as column groups instead of stacked row
#' groups.
#'
#' @param filter Country filter passed to [load_question_selections()].
#' @param question_code Question code (e.g. `"currentEmp13"`).
#' @param header Display title for the table.
#' @return A gt table object.
#' @export
render_heatmap_category_age_wide_table <- function(
    filter,
    question_code,
    header
) {
  df <- load_question_selections(question_code, filter)
  df <- assign_age_groups_two(df)
  grouped <- heatmap_category_grouped_df(
    df,
    question_code,
    group_by = "age_group"
  )
  plot_df <- grouped$data

  if (nrow(plot_df) == 0L) {
    return(empty_results_gt(header = header))
  }

  respondent_counts <- heatmap_category_respondent_counts(
    df,
    group_by = "age_group"
  )
  n_total <- sum(respondent_counts$n_respondents)
  age_levels <- c("Below 35", "35-45", "45+")
  respondent_counts <- respondent_counts |>
    dplyr::mutate(age_group = as.character(.data$age_group))
  n_by_age <- purrr::set_names(
    purrr::map_int(age_levels, function(level) {
      idx <- match(level, respondent_counts$age_group)
      if (is.na(idx)) 0L else respondent_counts$n_respondents[[idx]]
    }),
    age_levels
  )

  sort_age_group <- function(group_name) {
    plot_df |>
      dplyr::filter(.data$age_group == group_name) |>
      dplyr::arrange(dplyr::desc(.data$count)) |>
      dplyr::mutate(
        pct = sprintf("%.1f%%", .data$percent),
        category = as.character(.data$category)
      ) |>
      dplyr::select(category, count, pct)
  }

  pad_age_group <- function(tbl, n_rows) {
    if (nrow(tbl) >= n_rows) {
      return(tbl)
    }
    dplyr::bind_rows(
      tbl,
      tibble::tibble(
        category = rep("", n_rows - nrow(tbl)),
        count = rep(NA_integer_, n_rows - nrow(tbl)),
        pct = rep("", n_rows - nrow(tbl))
      )
    )
  }

  age_tables <- purrr::set_names(
    purrr::map(age_levels, sort_age_group),
    age_levels
  )
  n_rows <- max(purrr::map_int(age_tables, nrow))
  age_tables <- purrr::map(age_tables, pad_age_group, n_rows = n_rows)

  wide_df <- tibble::tibble(
    category_below_35 = age_tables[["Below 35"]]$category,
    count_below_35 = age_tables[["Below 35"]]$count,
    pct_below_35 = age_tables[["Below 35"]]$pct,
    category_35_45 = age_tables[["35-45"]]$category,
    count_35_45 = age_tables[["35-45"]]$count,
    pct_35_45 = age_tables[["35-45"]]$pct,
    category_45_plus = age_tables[["45+"]]$category,
    count_45_plus = age_tables[["45+"]]$count,
    pct_45_plus = age_tables[["45+"]]$pct
  )

  wide_df |>
    gt::gt() |>
    gt::tab_header(
      title = header,
      subtitle = paste0(
        "Total respondents with age reported: ",
        n_total,
        ". Rows are ranked separately within each age group by respondent count."
      )
    ) |>
    gt::tab_spanner(
      label = paste0("Below 35 (n=", n_by_age[["Below 35"]], ")"),
      columns = c("category_below_35", "count_below_35", "pct_below_35")
    ) |>
    gt::tab_spanner(
      label = paste0("35-45 (n=", n_by_age[["35-45"]], ")"),
      columns = c("category_35_45", "count_35_45", "pct_35_45")
    ) |>
    gt::tab_spanner(
      label = paste0("45+ (n=", n_by_age[["45+"]], ")"),
      columns = c("category_45_plus", "count_45_plus", "pct_45_plus")
    ) |>
    gt::cols_label(
      category_below_35 = "Category",
      count_below_35 = "Respondents (n)",
      pct_below_35 = "Percentage (%)",
      category_35_45 = "Category",
      count_35_45 = "Respondents (n)",
      pct_35_45 = "Percentage (%)",
      category_45_plus = "Category",
      count_45_plus = "Respondents (n)",
      pct_45_plus = "Percentage (%)"
    ) |>
    gt::cols_width(
      category_below_35 ~ gt::px(170),
      category_35_45 ~ gt::px(240),
      category_45_plus ~ gt::px(170)
    ) |>
    gt::sub_missing(
      columns = "count_below_35",
      missing_text = ""
    ) |>
    gt::sub_missing(
      columns = "count_35_45",
      missing_text = ""
    ) |>
    gt::sub_missing(
      columns = "count_45_plus",
      missing_text = ""
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_options(table.font.size = gt::px(14))
}

#' Return a wide age-group multi-select table for Quarto output
#'
#' @inheritParams render_heatmap_category_age_wide_table
#' @return A gt table object.
#' @export
print_heatmap_category_age_wide_table <- function(
    filter,
    question_code,
    header
) {
  render_heatmap_category_age_wide_table(
    filter = filter,
    question_code = question_code,
    header = header
  )
}

#' Plot category selection percentages for a multi-select heatmap question
#'
#' Shows the percentage of respondents in each group who selected each predefined
#' category, with raw counts annotated as `(n=...)` to the right of each bar.
#' Optional grouping compares percentages between individual countries or named
#' country groups (e.g. Nordics vs Germany vs Netherlands).
#'
#' @param filter Country filter for respondents to include, passed to
#'   [load_question_selections()].
#' @param question_code Question code (e.g. `"currentEmp13"`).
#' @param title Optional plot title; `NULL` draws no title.
#' @param group_by Optional column name to group bars (e.g. `"socio1_0"`), or a
#'   character vector of country names where each country forms its own group.
#' @param country_groups Optional named list mapping group labels to one or more
#'   countries. Creates a `country_group` column and overrides `group_by`.
#' @return A ggplot object.
#' @export
plot_heatmap_category_barplot <- function(
    filter,
    question_code,
    title = NULL,
    group_by = NULL,
    country_groups = NULL,
    bar_color = "darkblue",
    group_colors = NULL,
    category_colors = NULL,
    show_n = TRUE,
    y_labels = scales::label_wrap(40),
    wrap_y_in_data = FALSE
) {
  df <- load_question_selections(question_code, filter)
  if (!is.null(group_by) && identical(group_by, "age_group")) {
    df <- assign_age_groups_two(df)
  }
  grouped <- heatmap_category_grouped_df(
    df,
    question_code,
    group_by = group_by,
    country_groups = country_groups
  )
  plot_df <- grouped$data
  group_col <- grouped$group_col

  if (nrow(plot_df) == 0L) {
    return(empty_results_plot())
  }

  if (isTRUE(wrap_y_in_data) && is.function(y_labels)) {
    raw_levels <- if (is.factor(plot_df$category)) {
      levels(plot_df$category)
    } else {
      unique(as.character(plot_df$category))
    }
    mapping <- stats::setNames(y_labels(raw_levels), raw_levels)
    if (!is.null(category_colors)) {
      category_colors <- stats::setNames(
        unname(category_colors[names(mapping)]),
        unname(mapping[names(category_colors)])
      )
    }
    plot_df <- plot_df |>
      dplyr::mutate(
        category = factor(
          unname(mapping[as.character(.data$category)]),
          levels = unname(mapping[raw_levels])
        )
      )
  }

  x_expand <- function(mult) {
    c(0, max(plot_df$percent, na.rm = TRUE) * mult)
  }
  x_mult <- if (is.null(group_col)) 1.2 else 1.35
  bar_width <- if (is.null(group_col)) 0.9 else 0.9
  dodge_width <- 0.9

  if (is.null(group_col)) {
    n_total <- dplyr::n_distinct(df$row_id)
    use_category_fill <- !is.null(category_colors)
    p <- if (use_category_fill) {
      category_fill <- category_colors[as.character(plot_df$category)]
      category_fill <- category_fill[!is.na(category_fill)]
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data$percent, y = .data$category, fill = .data$category)
      ) +
        ggplot2::geom_col(width = bar_width) +
        ggplot2::scale_fill_manual(values = category_fill, guide = "none")
    } else {
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data$percent, y = .data$category)
      ) +
        ggplot2::geom_col(fill = bar_color, width = bar_width)
    }
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          label = ifelse(
            .data$count > 0L,
            sprintf("(n=%d)", .data$count),
            NA_character_
          )
        ),
        hjust = -0.1,
        size = 3.5
      ) +
      ggplot2::scale_x_continuous(
        limits = x_expand(x_mult),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::labs(x = "Percentage", y = NULL) +
      ggplot2::theme_minimal()

    if (!is.null(title)) {
      plot_title <- if (isTRUE(show_n)) {
        stringr::str_wrap(paste0(title, " (n=", n_total, ")"), width = 70)
      } else {
        stringr::str_wrap(title, width = 70)
      }
      p <- p + ggplot2::labs(title = plot_title)
    }
    return(p)
  }

  n_by_group <- {
    respondents <- df
    if (!is.null(group_col) && group_col %in% names(respondents)) {
      respondents <- respondents |>
        dplyr::distinct(.data$row_id, .data[[group_col]])
    } else {
      respondents <- respondents |>
        dplyr::distinct(.data$row_id, .data$socio1_0)
    }
    if (!is.null(country_groups)) {
      respondents <- assign_country_groups(respondents, country_groups)
    } else if (
      is.character(group_by) &&
        length(group_by) > 0L &&
        !any(group_by %in% names(df))
    ) {
      respondents <- dplyr::filter(respondents, .data$socio1_0 %in% group_by)
    }
    respondents |>
      dplyr::count(.data[[group_col]], name = "n")
  }
  group_order <- if (!is.null(country_groups)) {
    names(country_groups)
  } else if (identical(group_by, "age_group")) {
    c("Below 35", "35-45", "45+")
  } else if (
    is.character(group_by) &&
      length(group_by) > 0L &&
      !any(group_by %in% names(df))
  ) {
    group_by
  } else {
    as.character(n_by_group[[group_col]])
  }
  group_order <- group_order[group_order %in% as.character(n_by_group[[group_col]])]
  extra_groups <- setdiff(as.character(n_by_group[[group_col]]), group_order)
  group_order <- c(group_order, extra_groups)
  n_by_group <- n_by_group |>
    dplyr::mutate(
      !!rlang::sym(group_col) := factor(
        as.character(.data[[group_col]]),
        levels = group_order
      )
    ) |>
    dplyr::arrange(.data[[group_col]])
  n_label <- n_by_group |>
    dplyr::mutate(
      label = paste0(.data[[group_col]], " (n=", .data$n, ")")
    ) |>
    dplyr::pull(.data$label)
  plot_df[[group_col]] <- forcats::fct_relevel(
    as.character(plot_df[[group_col]]),
    group_order
  )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$percent,
      y = .data$category,
      fill = .data[[group_col]]
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge2(width = dodge_width, preserve = "single"),
      width = bar_width
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("(n=%d)", .data$count)),
      position = ggplot2::position_dodge2(width = dodge_width, preserve = "single"),
      hjust = -0.1,
      size = 3
    ) +
    ggplot2::scale_x_continuous(
      limits = x_expand(x_mult),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_fill_discrete(name = NULL, labels = n_label) +
    ggplot2::labs(x = "Percentage", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(title)) {
    p <- p + ggplot2::labs(
      title = stringr::str_wrap(title, width = 70)
    )
  }
  p
}

#' Plot category selection percentages as a Cleveland dot plot
#'
#' Same data and grouping options as [plot_heatmap_category_barplot()], but
#' encodes percentages as points on a common horizontal scale for easier
#' comparison across groups.
#'
#' @inheritParams plot_heatmap_category_barplot
#' @return A ggplot object.
#' @export
plot_heatmap_category_dotplot <- function(
    filter,
    question_code,
    title = NULL,
    group_by = NULL,
    country_groups = NULL
) {
  df <- load_question_selections(question_code, filter)
  grouped <- heatmap_category_grouped_df(
    df,
    question_code,
    group_by = group_by,
    country_groups = country_groups
  )
  plot_df <- grouped$data
  group_col <- grouped$group_col

  if (nrow(plot_df) == 0L) {
    return(empty_results_plot())
  }

  x_expand <- function(mult) {
    c(0, max(plot_df$percent, na.rm = TRUE) * mult)
  }
  x_mult <- if (is.null(group_col)) 1.2 else 1.35

  if (is.null(group_col)) {
    n_total <- dplyr::n_distinct(df$row_id)
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data$percent, y = .data$category)
    ) +
      ggplot2::geom_point(color = "darkblue", size = 3) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("(n=%d)", .data$count)),
        hjust = -0.2,
        size = 3.5
      ) +
      ggplot2::scale_x_continuous(
        limits = x_expand(x_mult),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::scale_y_discrete(labels = scales::label_wrap(40)) +
      ggplot2::labs(x = "Percentage", y = NULL) +
      ggplot2::theme_minimal()

    if (!is.null(title)) {
      p <- p + ggplot2::labs(
        title = stringr::str_wrap(paste0(title, " (n=", n_total, ")"), width = 70)
      )
    }
    return(p)
  }

  n_by_group <- {
    respondents <- df |>
      dplyr::distinct(.data$row_id, .data$socio1_0)
    if (!is.null(country_groups)) {
      respondents <- assign_country_groups(respondents, country_groups)
    } else if (
      is.character(group_by) &&
        length(group_by) > 0L &&
        !any(group_by %in% names(df))
    ) {
      respondents <- dplyr::filter(respondents, .data$socio1_0 %in% group_by)
    }
    respondents |>
      dplyr::count(.data[[group_col]], name = "n")
  }
  group_order <- if (!is.null(country_groups)) {
    names(country_groups)
  } else if (
    is.character(group_by) &&
      length(group_by) > 0L &&
      !any(group_by %in% names(df))
  ) {
    group_by
  } else {
    n_by_group[[group_col]]
  }
  group_order <- group_order[group_order %in% n_by_group[[group_col]]]
  extra_groups <- setdiff(n_by_group[[group_col]], group_order)
  group_order <- c(group_order, extra_groups)
  n_by_group <- n_by_group |>
    dplyr::mutate(
      !!rlang::sym(group_col) := factor(
        .data[[group_col]],
        levels = group_order
      )
    ) |>
    dplyr::arrange(.data[[group_col]])
  n_label <- n_by_group |>
    dplyr::mutate(
      label = paste0(.data[[group_col]], " (n=", .data$n, ")")
    ) |>
    dplyr::pull(.data$label)
  plot_df[[group_col]] <- forcats::fct_relevel(
    plot_df[[group_col]],
    group_order
  )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$percent,
      y = .data$category,
      color = .data[[group_col]]
    )
  ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("(n=%d)", .data$count)),
      hjust = -0.15,
      size = 2.8,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = x_expand(x_mult),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_color_discrete(name = NULL, labels = n_label) +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(40)) +
    ggplot2::labs(x = "Percentage", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(title)) {
    p <- p + ggplot2::labs(
      title = stringr::str_wrap(title, width = 70)
    )
  }
  p
}

#' Default ordering for five-point agreement Likert responses
#'
#' @return Character vector of response labels from negative to positive.
#' @keywords internal
likert5_levels <- function() {
  c(
    "Strongly disagree",
    "Disagree",
    "Neither agree or disagree",
    "Agree",
    "Strongly Agree"
  )
}

#' Default ordering for time-allocation percentage responses
#'
#' @return Character vector from no time to all time.
#' @export
likert_time_levels <- function() {
  c(
    "0% (None at all)",
    "20%",
    "40%",
    "60%",
    "80%",
    "100% (All my time)"
  )
}

#' Default ordering for Never / Always frequency responses
#'
#' @return Character vector from least to most frequent.
#' @export
likert_frequency_levels <- function() {
  c("Never", "Always")
}

#' Color palette for time-allocation activity stacked bars
#'
#' Uses an Okabe--Ito-inspired, colorblind-friendly set with strong contrast
#' between the five standard activity categories.
#'
#' @param activities Character vector of activity labels to color.
#' @return Named character vector of hex colors.
#' @export
likert_time_activity_palette <- function(activities) {
  base <- c(
    "developing software" = "#0072B2",
    "research" = "#009E73",
    "management" = "#D55E00",
    "teaching" = "#CC79A7",
    "other activities" = "#666666"
  )
  missing <- setdiff(activities, names(base))
  if (length(missing) > 0L) {
    extras <- c("#56B4E9", "#E69F00", "#F0E442", "#882255", "#999999")
    base <- c(base, stats::setNames(extras[seq_along(missing)], missing))
  }
  base[activities]
}

#' Suggested figure height for respondent allocation plots
#'
#' @param n_resp Number of respondents (one horizontal bar each).
#' @param inches_per_bar Vertical space per bar in inches.
#' @param min_height Minimum figure height.
#' @param max_height Maximum figure height.
#' @return Numeric figure height in inches.
#' @export
likert_allocation_fig_height <- function(
    n_resp,
    inches_per_bar = 0.18,
    min_height = 7,
    max_height = 50
) {
  as.numeric(max(min_height, min(max_height, n_resp * inches_per_bar)))
}

#' Count respondents with allocation data for one country group
#'
#' @param df Respondent data frame.
#' @param countries Country or countries to include.
#' @param question_code Question code (default `"likert0"`).
#' @param meta Question metadata tibble.
#' @return Integer number of respondents.
#' @export
likert_allocation_n_respondents <- function(
    df,
    countries,
    question_code = "likert0",
    meta = NULL
) {
  plot_df <- prepare_likert0_allocation(df, question_code, countries, meta)
  length(levels(plot_df$respondent))
}

#' Look up the display label for one survey column from question metadata
#'
#' Uses the `Option` field when present (typical for Likert sub-items); otherwise
#' strips bracketed sub-question text from `Question`.
#'
#' @param column_name Column code as in `2026_tf.csv` (e.g. `"likert5b[1]_0"`).
#' @param meta Question metadata tibble, typically `cols` from `_common.R`.
#' @return Character label suitable for plot titles.
#' @export
question_label <- function(column_name, meta = NULL) {
  if (is.null(meta)) {
    meta <- get("cols", envir = globalenv())
  }
  row <- meta[meta$New_name == column_name, , drop = FALSE]
  if (nrow(row) == 0L) {
    return(column_name)
  }
  option <- trimws(as.character(row$Option[[1]]))
  if (nzchar(option)) {
    return(option)
  }
  question <- trimws(as.character(row$Question[[1]]))
  stringr::str_remove_all(question, "\\[.*?\\]") |> trimws()
}

#' Extract all country names referenced in a `country_groups` list
#'
#' @param groups Named list mapping group labels to country name(s).
#' @return Character vector of unique country names.
#' @export
countries_from_groups <- function(groups) {
  unique(unlist(groups, use.names = FALSE))
}

#' Map respondents to named country groups
#'
#' @param df Respondent data frame containing `country_col`.
#' @param groups Named list mapping group labels to country name(s), e.g.
#'   `list(Nordics = c("Finland", "Norway"), Germany = "Germany")`.
#' @param country_col Column holding country names (default `socio1_0`).
#' @param group_col Name of the output grouping column.
#' @return `df` with `group_col` added; rows outside `groups` are dropped.
#' @keywords internal
assign_country_groups <- function(
    df,
    groups,
    country_col = "socio1_0",
    group_col = "country_group"
) {
  group_map <- purrr::imap_dfr(groups, function(countries, label) {
    tibble::tibble(
      !!country_col := countries,
      !!group_col := label
    )
  })
  df |>
    dplyr::inner_join(group_map, by = country_col)
}

#' Collapse survey age bands into three age groups
#'
#' Maps the `socio3_0` age-band responses to `Below 35`, `35-45`, and `45+`.
#' Rows without a usable age response are dropped.
#'
#' @param df Data frame containing an age-band column.
#' @param age_col Column with survey age bands (default `socio3_0`).
#' @param group_col Name of the output grouping column.
#' @return Data frame with `group_col` added.
#' @export
assign_age_groups_two <- function(
    df,
    age_col = "socio3_0",
    group_col = "age_group"
) {
  age_map <- c(
    "18 to 24 years" = "Below 35",
    "25 to 34 years" = "Below 35",
    "35 to 44 years" = "35-45",
    "45 to 54 years" = "45+",
    "55 to 64 years" = "45+",
    "Age 65 or older" = "45+"
  )
  group_levels <- c("Below 35", "35-45", "45+")

  df |>
    dplyr::mutate(
      !!rlang::sym(group_col) := unname(age_map[.data[[age_col]]])
    ) |>
    dplyr::filter(!is.na(.data[[group_col]])) |>
    dplyr::mutate(
      !!rlang::sym(group_col) := factor(
        .data[[group_col]],
        levels = group_levels
      )
    )
}

#' Build count and percent summaries for one Likert column
#'
#' @param df Respondent data frame.
#' @param column_name Likert column in `df`.
#' @param group_col Optional grouping column; `NULL` aggregates all rows.
#' @param levels Response level order.
#' @return Tibble with `response`, `count`, `percent`, and optionally `group_col`.
#' @keywords internal
likert_count_data <- function(
    df,
    column_name,
    group_col = NULL,
    levels = likert5_levels()
) {
  if (is.null(group_col)) {
    responses <- df[[column_name]]
    responses <- responses[!is.na(responses) & nzchar(as.character(responses))]
    plot_df <- tibble::tibble(response = responses) |>
      dplyr::count(.data$response, name = "count") |>
      dplyr::mutate(percent = 100 * .data$count / sum(.data$count))
  } else {
    plot_df <- df |>
      dplyr::filter(
        !is.na(.data[[column_name]]),
        nzchar(as.character(.data[[column_name]]))
      ) |>
      dplyr::rename(response = dplyr::all_of(column_name)) |>
      dplyr::count(.data[[group_col]], .data$response, name = "count") |>
      dplyr::group_by(.data[[group_col]]) |>
      dplyr::mutate(percent = 100 * .data$count / sum(.data$count)) |>
      dplyr::ungroup()
  }

  present_levels <- intersect(levels, plot_df$response)
  extra_levels <- setdiff(plot_df$response, levels)
  all_levels <- c(present_levels, extra_levels)

  plot_df |>
    dplyr::mutate(
      response = forcats::fct_relevel(.data$response, all_levels)
    )
}

#' Load the filtered respondent table from `2026_tf.csv`
#'
#' Keeps respondents in `filter` whose `submitdate_0` is non-empty. Partial
#' responses without a submission timestamp are excluded.
#'
#' @param filter Country filter passed to `socio1_0`.
#' @param data_dir Directory holding `2026_tf.csv`.
#' @return Filtered data frame of submitted respondents.
#' @keywords internal
load_filtered_tf <- function(filter, data_dir = survey_raw_data_dir()) {
  filter_survey_respondents(read_tf(data_dir), filter)
}

#' Prepare grouped count data for one Likert column
#'
#' @param df Respondent data frame.
#' @param column_name Likert column name.
#' @param group_by Optional grouping column or country vector.
#' @param country_groups Optional named country groups.
#' @param levels Response level order.
#' @return List with `data` tibble and `group_col`.
#' @keywords internal
likert_grouped_df <- function(
    df,
    column_name,
    group_by = NULL,
    country_groups = NULL,
    levels = likert5_levels()
) {
  group_col <- group_by
  plot_df <- df

  if (!is.null(country_groups)) {
    plot_df <- assign_country_groups(plot_df, country_groups)
    group_col <- "country_group"
  } else if (!is.null(group_by) && identical(group_by, "age_group")) {
    plot_df <- assign_age_groups_two(plot_df)
    group_col <- "age_group"
  } else if (
    is.character(group_by) &&
      length(group_by) > 0L &&
      !any(group_by %in% names(df))
  ) {
    plot_df <- dplyr::filter(plot_df, socio1_0 %in% group_by)
    plot_df$socio1_0 <- forcats::fct_relevel(plot_df$socio1_0, group_by)
    group_col <- "socio1_0"
  }

  list(
    data = likert_count_data(
      plot_df,
      column_name,
      group_col = group_col,
      levels = levels
    ),
    group_col = group_col
  )
}

#' Count distinct respondents with a valid Likert response
#'
#' @param df Respondent data frame.
#' @param column_name Likert column name.
#' @param group_col Optional grouping column (`socio1_0` or `country_group`).
#' @param group_by Optional country vector passed to [likert_grouped_df()].
#' @param country_groups Optional named country groups.
#' @return Tibble with `n_respondents` and, when grouped, the grouping column.
#' @keywords internal
likert_respondent_counts <- function(
    df,
    column_name,
    group_col = NULL,
    group_by = NULL,
    country_groups = NULL
) {
  plot_df <- df

  if (!is.null(country_groups)) {
    plot_df <- assign_country_groups(plot_df, country_groups)
    group_col <- "country_group"
  } else if (!is.null(group_by) && identical(group_by, "age_group")) {
    plot_df <- assign_age_groups_two(plot_df)
    group_col <- "age_group"
  } else if (
    is.character(group_by) &&
      length(group_by) > 0L &&
      !any(group_by %in% names(df))
  ) {
    plot_df <- dplyr::filter(plot_df, .data$socio1_0 %in% group_by)
    group_col <- "socio1_0"
  }

  respondents <- plot_df |>
    dplyr::filter(
      !is.na(.data[[column_name]]),
      nzchar(as.character(.data[[column_name]]))
    )

  if (is.null(group_col)) {
    return(tibble::tibble(n_respondents = nrow(respondents)))
  }

  respondents |>
    dplyr::count(.data[[group_col]], name = "n_respondents")
}

#' Render a descriptive statistics table for one Likert column
#'
#' Shows response counts and percentages for a single Likert item. Percentages
#' are computed within each group when `group_by` or `country_groups` is set.
#'
#' @param df Respondent data frame.
#' @param column_name Likert column name.
#' @param meta Question metadata tibble for [question_label()].
#' @param header Optional table title; defaults to the question label.
#' @param group_by Optional grouping column or country vector (see
#'   [likert_grouped_df()]).
#' @param country_groups Optional named country groups.
#' @param group_label Column label for the grouping variable when grouped.
#' @param levels Response level order passed to [likert_grouped_df()].
#' @return A gt table object.
#' @export
render_likert_table <- function(
    df,
    column_name,
    meta = NULL,
    header = NULL,
    group_by = NULL,
    country_groups = NULL,
    group_label = NULL,
    levels = likert5_levels()
) {
  if (is.null(header)) {
    header <- question_label(column_name, meta)
  }

  grouped <- likert_grouped_df(
    df,
    column_name,
    group_by = group_by,
    country_groups = country_groups,
    levels = levels
  )
  plot_df <- grouped$data
  group_col <- grouped$group_col

  if (nrow(plot_df) == 0L) {
    stop("No Likert responses available for the requested selection.")
  }

  respondent_counts <- likert_respondent_counts(
    df,
    column_name,
    group_col = group_col,
    group_by = group_by,
    country_groups = country_groups
  )
  n_total <- if (is.null(group_col)) {
    respondent_counts$n_respondents[[1L]]
  } else {
    sum(respondent_counts$n_respondents)
  }

  table_df <- plot_df |>
    dplyr::mutate(
      pct = sprintf("%.1f%%", .data$percent)
    )

  if (is.null(group_col)) {
    table_df <- table_df |>
      dplyr::arrange(.data$response) |>
      dplyr::select(response, count, pct)

    return(
      table_df |>
        gt::gt() |>
        gt::tab_header(
          title = header,
          subtitle = paste0("Total respondents: ", n_total)
        ) |>
        gt::cols_label(
          response = "Response",
          count = "Respondents (n)",
          pct = "Percentage (%)"
        ) |>
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_options(table.font.size = gt::px(14))
    )
  }

  if (is.null(group_label)) {
    group_label <- if (identical(group_col, "country_group")) {
      "Country group"
    } else if (identical(group_col, "age_group")) {
      "Age group"
    } else {
      "Country"
    }
  }

  group_levels <- if (inherits(plot_df[[group_col]], "factor")) {
    levels(plot_df[[group_col]])
  } else {
    unique(as.character(plot_df[[group_col]]))
  }
  group_levels <- group_levels[group_levels %in% plot_df[[group_col]]]

  table_df <- table_df |>
    dplyr::mutate(
      !!rlang::sym(group_col) := factor(
        .data[[group_col]],
        levels = group_levels
      )
    ) |>
    dplyr::arrange(.data[[group_col]], .data$response)

  group_vec <- table_df[[group_col]]
  display_df <- table_df |>
    dplyr::select(response, count, pct)

  gt_tbl <- display_df |>
    gt::gt() |>
    gt::tab_header(
      title = header,
      subtitle = paste0(
        "Total respondents: ",
        n_total,
        ". Percentages are within each ",
        tolower(group_label),
        "."
      )
    ) |>
    gt::cols_label(
      response = "Response",
      count = "Respondents (n)",
      pct = "Percentage (%)"
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_options(table.font.size = gt::px(14))

  row_groups <- split(seq_len(nrow(display_df)), group_vec)
  group_order <- group_levels
  for (group_name in group_order) {
    if (!group_name %in% names(row_groups)) {
      next
    }
    n_group <- respondent_counts$n_respondents[
      match(group_name, respondent_counts[[group_col]])
    ]
    gt_tbl <- gt_tbl |>
      gt::tab_row_group(
        label = paste0(group_name, " (n=", n_group, ")"),
        rows = row_groups[[group_name]]
      )
  }

  gt_tbl
}

#' Plot a horizontal bar chart of Likert response percentages for one column
#'
#' @param df Respondent data frame (already country-filtered).
#' @param column_name Column to plot (e.g. `"likert5b[1]_0"`).
#' @param meta Question metadata tibble for [question_label()]; `NULL` uses
#'   the global `cols` object from `_common.R`.
#' @param title Optional plot title; `NULL` (default) uses [question_label()].
#' @param levels Optional factor levels for the response axis; defaults to
#'   [likert5_levels()].
#' @param group_by Optional column name to group bars (e.g. `"socio1_0"` for
#'   individual countries), or a character vector of country names where each
#'   country forms its own group (e.g. `c("Germany", "Netherlands", "Sweden")`).
#' @param country_groups Optional named list mapping group labels to one or more
#'   countries, e.g. `list(Nordics = c("Finland", "Norway"), Germany = "Germany",
#'   Netherlands = "Netherlands")`. Creates a `country_group` column and
#'   overrides `group_by`.
#' @param xlim_max Optional upper limit for the percent axis; shared when plotting
#'   pairs side by side.
#' @return A ggplot object.
#' @export
plot_likert_barplot <- function(
    df,
    column_name,
    meta = NULL,
    title = NULL,
    levels = likert5_levels(),
    group_by = NULL,
    country_groups = NULL,
    xlim_max = NULL,
    response_colors = NULL,
    group_colors = NULL,
    show_n = TRUE
) {
  grouped <- likert_grouped_df(
    df,
    column_name,
    group_by = group_by,
    country_groups = country_groups,
    levels = levels
  )
  plot_df <- grouped$data
  group_col <- grouped$group_col

  if (is.null(title)) {
    title <- question_label(column_name, meta)
  }

  x_expand <- function(plot_df, mult) {
    upper <- if (is.null(xlim_max)) {
      max(plot_df$percent, na.rm = TRUE) * mult
    } else {
      xlim_max
    }
    c(0, upper)
  }

  if (is.null(group_col)) {
    n_total <- sum(plot_df$count)
    plot_df <- plot_df |>
      dplyr::mutate(
        response = factor(.data$response, levels = levels)
      )

    p <- if (is.null(response_colors)) {
      ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$percent, y = .data$response)) +
        ggplot2::geom_col(fill = "steelblue", width = 0.7)
    } else {
      response_fill <- response_colors[levels]
      response_fill <- response_fill[!is.na(response_fill)]
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data$percent, y = .data$response, fill = .data$response)
      ) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::scale_fill_manual(
          values = response_fill,
          drop = FALSE,
          guide = "none"
        )
    }

    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("(n=%d)", .data$count)),
        hjust = -0.1,
        size = 3.5
      ) +
      ggplot2::scale_x_continuous(
        limits = x_expand(plot_df, 1.2),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::labs(
        title = if (isTRUE(show_n)) {
          stringr::str_wrap(paste0(title, " (n=", n_total, ")"), width = 70)
        } else {
          stringr::str_wrap(title, width = 70)
        },
        x = "Percentage",
        y = NULL
      ) +
      ggplot2::theme_minimal()
    return(p)
  } else {
    n_by_group <- plot_df |>
      dplyr::group_by(.data[[group_col]]) |>
      dplyr::summarise(n = sum(.data$count), .groups = "drop")
    n_label <- n_by_group |>
      dplyr::mutate(label = paste0(.data[[group_col]], " (n=", .data$n, ")")) |>
      dplyr::pull(.data$label)
    plot_df[[group_col]] <- forcats::fct_relevel(
      plot_df[[group_col]],
      levels(n_by_group[[group_col]])
    )

    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(
        x = .data$percent,
        y = .data$response,
        fill = .data[[group_col]]
      )
    ) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("(n=%d)", .data$count)),
        position = ggplot2::position_dodge(width = 0.8),
        hjust = -0.1,
        size = 3
      ) +
      ggplot2::scale_x_continuous(
        limits = x_expand(plot_df, 1.25),
        expand = ggplot2::expansion(mult = c(0, 0))
      )

    if (!is.null(group_colors)) {
      p <- p + ggplot2::scale_fill_manual(
        values = group_colors,
        labels = n_label,
        name = NULL
      )
    } else {
      p <- p + ggplot2::scale_fill_discrete(
        name = NULL,
        labels = n_label
      )
    }

    p <- p +
      ggplot2::labs(
        title = stringr::str_wrap(title, width = 70),
        x = "Percentage",
        y = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    return(p)
  }
}

#' Check whether a data frame has any non-empty Likert responses
#'
#' @param df Respondent data frame.
#' @param column_names Likert column names to inspect.
#' @return Logical scalar.
#' @export
has_likert_observations <- function(df, column_names) {
  if (nrow(df) == 0L || length(column_names) == 0L) {
    return(FALSE)
  }
  any(purrr::map_lgl(column_names, function(col) {
    if (!col %in% names(df)) {
      return(FALSE)
    }
    responses <- df[[col]]
    any(!is.na(responses) & nzchar(as.character(responses)))
  }))
}

#' Print Likert question headings, tables, and plots in sequence
#'
#' For each Likert column, prints a markdown heading with [question_label()],
#' then the summary table and bar plot for that item.
#'
#' @param df Respondent data frame.
#' @param column_names Likert columns to summarise.
#' @param meta Question metadata tibble for [question_label()].
#' @param empty_message Message shown when [has_likert_observations()] is `FALSE`.
#' @param heading_level Markdown heading level for each question (default `3`).
#' @param include_plot Whether to print a bar plot after each table (default
#'   `TRUE`).
#' @param ... Additional arguments passed to [render_likert_table()] and
#'   [plot_likert_barplot()].
#' @return Invisibly `NULL`.
#' @export
print_likert_items <- function(
    df,
    column_names,
    meta = NULL,
    empty_message = "No responses available for this selection.",
    heading_level = 3L,
    include_plot = TRUE,
    ...
) {
  if (!has_likert_observations(df, column_names)) {
    cat(empty_message, "\n")
    return(invisible(NULL))
  }
  heading_prefix <- paste(rep("#", heading_level), collapse = "")
  for (col in column_names) {
    label <- question_label(col, meta)
    cat("\n\n", heading_prefix, " ", label, "\n\n", sep = "")
    print(render_likert_table(df, col, meta = meta, ...))
    if (isTRUE(include_plot)) {
      print(plot_likert_barplot(df, col, meta = meta, ...))
    }
  }
  invisible(NULL)
}

#' Parse a likert time-allocation response to a numeric percent
#'
#' @param x Character vector of responses (e.g. `"20%"`, `"0% (None at all)"`).
#' @return Numeric vector of percentages.
#' @export
parse_likert_time_percent <- function(x) {
  out <- suppressWarnings(as.numeric(stringr::str_extract(x, "^[0-9]+")))
  dplyr::if_else(is.na(out), 0L, out)
}

#' Reshape likert0/1 time-allocation columns to respondent-level shares
#'
#' @param df Respondent data frame.
#' @param question_code Question code (e.g. `"likert0"`).
#' @param countries One or more country names (`socio1_0` values) to include.
#' @param meta Question metadata tibble.
#' @param respondent_levels Optional character vector of `row_id` values defining
#'   y-axis order (e.g. from actual-time sorting for a paired desired-time plot).
#' @return Tibble sorted for stacked respondent bars.
#' @keywords internal
prepare_likert0_allocation <- function(
    df,
    question_code,
    countries,
    meta = NULL,
    respondent_levels = NULL
) {
  if (is.null(meta)) {
    meta <- get("cols", envir = globalenv())
  }
  question_cols <- question_columns(question_code, names(df))
  activity_levels <- purrr::map_chr(question_cols, function(col) {
    stringr::str_remove(question_label(col, meta), "\\?$")
  })

  plot_df <- df |>
    dplyr::filter(.data$socio1_0 %in% countries) |>
    dplyr::select("row_id", dplyr::all_of(question_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(question_cols),
      names_to = "column",
      values_to = "raw"
    ) |>
    dplyr::filter(!is.na(.data$raw), nzchar(as.character(.data$raw))) |>
    dplyr::mutate(
      activity = activity_levels[match(.data$column, question_cols)],
      share = parse_likert_time_percent(.data$raw)
    ) |>
    dplyr::group_by(.data$row_id) |>
    dplyr::mutate(
      total_share = sum(.data$share),
      rescaled = dplyr::first(.data$total_share) != 100L &&
        dplyr::first(.data$total_share) > 0L,
      share_plot = dplyr::if_else(
        .data$rescaled,
        .data$share / .data$total_share * 100,
        .data$share
      ),
      max_share = max(.data$share_plot)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      activity = factor(.data$activity, levels = activity_levels)
    )

  if (is.null(respondent_levels)) {
    respondent_levels <- plot_df |>
      dplyr::distinct(.data$row_id, .data$max_share) |>
      dplyr::arrange(dplyr::desc(.data$max_share), .data$row_id) |>
      dplyr::pull(.data$row_id)
  }

  dplyr::mutate(
    plot_df,
    respondent = factor(.data$row_id, levels = respondent_levels)
  )
}

#' Build a stacked respondent allocation ggplot
#'
#' @param plot_df Output of [prepare_likert0_allocation()].
#' @param title Plot title.
#' @param subtitle Optional subtitle; defaults to respondent count and sort note.
#' @param show_legend Whether to show the activity legend.
#' @return A ggplot object.
#' @keywords internal
.plot_likert_allocation_stacked <- function(
    plot_df,
    title,
    subtitle = NULL,
    show_legend = TRUE,
    activity_colors = NULL,
    respondent_levels = NULL
) {
  if (is.null(subtitle)) {
    n_resp <- dplyr::n_distinct(plot_df$row_id)
    subtitle <- paste0(
      "Sorted by largest single-task share (n=", n_resp,
      "); top = specialists, bottom = balanced"
    )
  }
  if (is.null(respondent_levels)) {
    respondent_levels <- levels(plot_df$respondent)
  }
  if (is.null(activity_colors)) {
    activity_colors <- likert_time_activity_palette(levels(plot_df$activity))
  }
  plot_df <- dplyr::mutate(
    plot_df,
    respondent = factor(.data$row_id, levels = respondent_levels)
  )
  asterisk_df <- plot_df |>
    dplyr::distinct(.data$row_id, .data$respondent, .data$rescaled) |>
    dplyr::filter(.data$rescaled) |>
    dplyr::mutate(label = "(*)", x = 100)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$share_plot, y = .data$respondent, fill = .data$activity)
  ) +
    ggplot2::geom_col(width = 1) +
    ggplot2::geom_text(
      data = asterisk_df,
      ggplot2::aes(x = .data$x, y = .data$respondent, label = .data$label),
      inherit.aes = FALSE,
      hjust = -0.15,
      size = 4
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 106),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_discrete(limits = respondent_levels, drop = FALSE) +
    ggplot2::scale_fill_manual(
      values = activity_colors,
      limits = names(activity_colors),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Percentage",
      y = NULL,
      fill = "Activity"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = if (show_legend) "bottom" else "none",
      legend.justification = "left",
      legend.box.just = "left"
    )
  p
}

#' Plot actual and desired respondent-level time allocation side by side
#'
#' Respondents are sorted by their largest single-task share in the actual-time
#' responses; the same row order is used for desired time.
#'
#' @param df Respondent data frame.
#' @param countries One or more country names (`socio1_0` values) to include.
#' @param country_label Plot title label.
#' @param meta Question metadata tibble.
#' @param actual_code Question code for actual time (default `"likert0"`).
#' @param desired_code Question code for desired time (default `"likert1"`).
#' @return A patchwork object.
#' @export
plot_likert_allocation_paired <- function(
    df,
    countries,
    country_label = NULL,
    meta = NULL,
    actual_code = "likert0",
    desired_code = "likert1"
) {
  if (is.null(country_label)) {
    country_label <- if (length(countries) == 1L) countries else paste(countries, collapse = ", ")
  }
  actual_df <- prepare_likert0_allocation(df, actual_code, countries, meta)
  respondent_levels <- levels(actual_df$respondent)
  n_resp <- length(respondent_levels)
  desired_df <- prepare_likert0_allocation(
    df,
    desired_code,
    countries,
    meta,
    respondent_levels = respondent_levels
  )

  activity_colors <- likert_time_activity_palette(levels(actual_df$activity))
  subtitle <- paste0(
    "Sorted by largest single-task share, actual time (n=", n_resp,
    "); top = specialists, bottom = balanced"
  )
  p_actual <- .plot_likert_allocation_stacked(
    actual_df,
    title = "Actual time",
    subtitle = NULL,
    show_legend = TRUE,
    activity_colors = activity_colors,
    respondent_levels = respondent_levels
  )
  p_desired <- .plot_likert_allocation_stacked(
    desired_df,
    title = "Desired time",
    subtitle = NULL,
    show_legend = FALSE,
    activity_colors = activity_colors,
    respondent_levels = respondent_levels
  )

  combined <- patchwork::wrap_plots(p_actual, p_desired, ncol = 2)
  combined <- combined & ggplot2::theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.box.just = "left",
    legend.box = "horizontal"
  )
  combined + patchwork::plot_annotation(
    title = paste0("Time allocation by respondent — ", country_label),
    subtitle = subtitle
  )
}

#' Summarise actual vs desired time allocation by activity
#'
#' @param df Respondent data frame.
#' @param countries One or more country names (`socio1_0` values) to include.
#' @param meta Question metadata tibble.
#' @param actual_code Question code for actual time (default `"likert0"`).
#' @param desired_code Question code for desired time (default `"likert1"`).
#' @param stat Summary statistic: `"median"` or `"mean"`.
#' @param by_country If `TRUE`, summarise separately for each `socio1_0` value.
#' @return Tibble with `activity`, `actual` (baseline), `change`, `desired`
#'   (`actual + change`), and `direction`. Change is always `desired - actual`
#'   per respondent after rescaling; line endpoints use actual as baseline.
#'   Includes `country` when `by_country = TRUE`.
#' @keywords internal
prepare_likert_allocation_summary <- function(
    df,
    countries,
    meta = NULL,
    actual_code = "likert0",
    desired_code = "likert1",
    stat = c("median", "mean"),
    by_country = FALSE
) {
  stat <- match.arg(stat)
  stat_fn <- if (stat == "median") stats::median else mean

  country_lookup <- df |>
    dplyr::select("row_id", country = "socio1_0") |>
    dplyr::distinct()
  question_cols <- question_columns(actual_code, names(df))
  activity_levels <- purrr::map_chr(question_cols, function(col) {
    stringr::str_remove(question_label(col, meta), "\\?$")
  })

  actual_df <- prepare_likert0_allocation(df, actual_code, countries, meta) |>
    dplyr::left_join(country_lookup, by = "row_id") |>
    dplyr::select("row_id", "country", "activity", actual = "share_plot")
  desired_df <- prepare_likert0_allocation(df, desired_code, countries, meta) |>
    dplyr::left_join(country_lookup, by = "row_id") |>
    dplyr::select("row_id", "country", "activity", desired = "share_plot")

  paired_df <- dplyr::inner_join(
    actual_df,
    desired_df,
    by = c("row_id", "country", "activity")
  ) |>
    dplyr::mutate(change = .data$desired - .data$actual)

  group_cols <- if (by_country) {
    c("country", "activity")
  } else {
    "activity"
  }

  summary_df <- paired_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      actual = stat_fn(.data$actual, na.rm = TRUE),
      change = stat_fn(.data$change, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      activity = factor(.data$activity, levels = activity_levels),
      desired = .data$actual + .data$change,
      direction = dplyr::case_when(
        .data$change > 0 ~ "Increase",
        .data$change < 0 ~ "Decrease",
        TRUE ~ "No change"
      ),
      direction = factor(
        .data$direction,
        levels = c("Increase", "Decrease", "No change")
      )
    )

  if (by_country) {
    summary_df <- summary_df |>
      dplyr::mutate(
        country = factor(.data$country, levels = countries)
      ) |>
      dplyr::arrange(.data$country, .data$activity)
  } else {
    summary_df <- dplyr::arrange(summary_df, .data$activity)
  }

  summary_df
}

#' Line types for allocation summary change direction
#' @keywords internal
likert_allocation_direction_linetypes <- function(dense_decrease = FALSE) {
  c(
    "Increase" = "solid",
    "Decrease" = if (dense_decrease) "1111" else "dotted",
    "No change" = "longdash"
  )
}

#' Point shapes for actual vs desired allocation markers
#' @keywords internal
likert_allocation_point_shapes <- function() {
  c(
    "Actual time" = 15,
    "Desired time" = 17
  )
}

#' Build dumbbell layers for an allocation summary tibble
#' @param y_col Column name used for the y-axis (one row per dumbbell).
#' @keywords internal
.plot_likert_allocation_summary_from_df <- function(
    summary_df,
    title,
    subtitle,
    y_col = "activity",
    direction_colors = NULL,
    point_colors = NULL,
    facet_col = NULL,
    facet_ncol = 3L,
    facet_labels = NULL
) {
  points_df <- summary_df |>
    tidyr::pivot_longer(
      cols = c("actual", "desired"),
      names_to = "type",
      values_to = "pct"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(.data$type, actual = "Actual time", desired = "Desired time"),
      type = factor(.data$type, levels = c("Actual time", "Desired time")),
      y_row = .data[[y_col]]
    )
  direction_linetypes <- likert_allocation_direction_linetypes()
  type_shapes <- likert_allocation_point_shapes()
  y_sym <- rlang::sym(y_col)
  use_direction_color <- !is.null(direction_colors)
  use_point_color <- !is.null(point_colors)

  p <- ggplot2::ggplot(summary_df, ggplot2::aes(y = !!y_sym)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$actual,
        xend = .data$desired,
        y = !!y_sym,
        yend = !!y_sym,
        linetype = .data$direction,
        color = if (use_direction_color) .data$direction else NULL
      ),
      color = if (use_direction_color) NULL else "gray45",
      linewidth = 1.1,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = points_df,
      ggplot2::aes(
        x = .data$pct,
        y = .data$y_row,
        shape = .data$type,
        fill = if (use_point_color) .data$type else NULL
      ),
      color = if (use_point_color) "white" else "black",
      fill = if (use_point_color) NULL else "black",
      linewidth = if (use_point_color) 0.25 else NULL,
      size = 3.5
    )

  if (use_point_color) {
    p <- p +
      ggplot2::scale_fill_manual(
        values = point_colors[levels(points_df$type)],
        name = NULL
      ) +
      ggplot2::scale_shape_manual(
        values = type_shapes,
        name = NULL,
        guide = ggplot2::guide_legend(
          order = 1,
          override.aes = list(color = "white", linewidth = 0.25)
        )
      )
  } else {
    p <- p +
      ggplot2::scale_shape_manual(
        values = type_shapes,
        name = NULL,
        guide = ggplot2::guide_legend(
          order = 1,
          override.aes = list(color = "black", fill = "black")
        )
      )
  }

  if (use_direction_color) {
    p <- p +
      ggplot2::scale_color_manual(
        values = direction_colors,
        name = "Change from actual"
      ) +
      ggplot2::scale_linetype_manual(values = direction_linetypes, guide = "none")
  } else {
    p <- p +
      ggplot2::scale_linetype_manual(
        values = direction_linetypes,
        name = "Change from actual"
      )
  }

  p <- p +
    ggplot2::scale_x_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Percentage",
      y = NULL
    ) +
    ggplot2::guides(
      fill = if (use_point_color) {
        ggplot2::guide_legend(order = 1)
      } else {
        "none"
      },
      shape = if (use_point_color) {
        "none"
      } else {
        ggplot2::guide_legend(
          order = 1,
          override.aes = list(color = "black", fill = "black")
        )
      },
      color = if (use_direction_color) {
        ggplot2::guide_legend(order = 2)
      } else {
        "none"
      },
      linetype = if (use_direction_color) {
        "none"
      } else {
        ggplot2::guide_legend(order = 2)
      }
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.justification = "left",
      legend.box.just = "left",
      axis.text.y = ggplot2::element_text(size = 9),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (!is.null(facet_col)) {
    facet_sym <- rlang::sym(facet_col)
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(!!facet_sym),
        ncol = facet_ncol,
        labeller = if (is.null(facet_labels)) {
          "label_value"
        } else {
          ggplot2::as_labeller(facet_labels)
        }
      )
  }

  p
}

#' Compute y positions for country lines within activity bands
#' @keywords internal
.prepare_likert_allocation_summary_country_positions <- function(
    summary_df,
    countries
) {
  activity_levels <- levels(summary_df$activity)
  activity_idx <- stats::setNames(
    rev(seq_along(activity_levels)),
    activity_levels
  )
  line_spacing <- 0.11
  country_n <- length(countries)

  summary_df |>
    dplyr::mutate(
      country_idx = match(.data$country, countries),
      y_pos = activity_idx[as.character(.data$activity)] +
        (.data$country_idx - (country_n + 1) / 2) * line_spacing,
      country = factor(.data$country, levels = countries)
    )
}

#' Country palette for grouped allocation summary lines
#' @keywords internal
likert_country_palette <- function(countries) {
  palette <- c(
    "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
    "#666666"
  )
  stats::setNames(palette[seq_along(countries)], countries)
}

#' Build allocation summary plot with countries stacked within activity bands
#' @keywords internal
.plot_likert_allocation_summary_grouped_countries <- function(
    summary_df,
    title,
    subtitle,
    countries,
    group_colors = NULL,
    group_labels = NULL,
    group_name = "Country"
) {
  activity_levels <- levels(summary_df$activity)
  activity_breaks <- stats::setNames(
    rev(seq_along(activity_levels)),
    activity_levels
  )
  points_df <- summary_df |>
    tidyr::pivot_longer(
      cols = c("actual", "desired"),
      names_to = "type",
      values_to = "pct"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(.data$type, actual = "Actual time", desired = "Desired time"),
      type = factor(.data$type, levels = c("Actual time", "Desired time"))
    )
  direction_linetypes <- likert_allocation_direction_linetypes(dense_decrease = TRUE)
  type_shapes <- likert_allocation_point_shapes()
  country_colors <- if (is.null(group_colors)) {
    likert_country_palette(countries)
  } else {
    group_colors[countries]
  }
  legend_countries <- countries[countries %in% as.character(summary_df$country)]
  legend_labels <- if (is.null(group_labels)) {
    legend_countries
  } else {
    unname(group_labels[legend_countries])
  }

  ggplot2::ggplot(summary_df, ggplot2::aes(x = .data$actual, y = .data$y_pos)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = .data$desired,
        yend = .data$y_pos,
        linetype = .data$direction,
        color = .data$country
      ),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = points_df,
      ggplot2::aes(x = .data$pct, y = .data$y_pos, shape = .data$type),
      color = "black",
      fill = "black",
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = country_colors,
      breaks = legend_countries,
      labels = legend_labels,
      name = group_name
    ) +
    ggplot2::scale_shape_manual(values = type_shapes, name = NULL) +
    ggplot2::scale_linetype_manual(
      values = direction_linetypes,
      name = "Change from actual"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        order = 1,
        override.aes = list(linetype = "solid", linewidth = 1)
      ),
      shape = ggplot2::guide_legend(
        order = 2,
        override.aes = list(color = "black", fill = "black")
      ),
      linetype = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::scale_y_continuous(
      breaks = unname(activity_breaks),
      labels = names(activity_breaks),
      limits = c(0.5, length(activity_breaks) + 0.5)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Percentage",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.justification = "left",
      legend.box.just = "left",
      legend.box = "horizontal"
    )
}

#' Plot median/mean actual vs desired time by activity (dumbbell chart)
#'
#' For each activity, shows two points (actual and desired central tendency)
#' connected by a line to highlight gaps at a glance.
#'
#' @param df Respondent data frame.
#' @param countries One or more country names (`socio1_0` values) to include.
#' @param country_label Plot title label.
#' @param meta Question metadata tibble.
#' @param actual_code Question code for actual time (default `"likert0"`).
#' @param desired_code Question code for desired time (default `"likert1"`).
#' @param stat Summary statistic: `"median"` or `"mean"`.
#' @return A ggplot object.
#' @export
plot_likert_allocation_summary <- function(
    df,
    countries,
    country_label = NULL,
    meta = NULL,
    actual_code = "likert0",
    desired_code = "likert1",
    stat = c("median", "mean"),
    direction_colors = NULL,
    point_colors = NULL
) {
  stat <- match.arg(stat)
  if (is.null(country_label)) {
    country_label <- if (length(countries) == 1L) countries else paste(countries, collapse = ", ")
  }
  summary_df <- prepare_likert_allocation_summary(
    df,
    countries,
    meta = meta,
    actual_code = actual_code,
    desired_code = desired_code,
    stat = stat
  )
  n_resp <- likert_allocation_n_respondents(df, countries, actual_code, meta)
  stat_label <- stringr::str_to_title(stat)

  .plot_likert_allocation_summary_from_df(
    summary_df,
    title = paste0("Actual vs desired time by activity — ", country_label),
    subtitle = paste0(
      stat_label,
      " change from actual baseline per activity (n=", n_resp,
      "); lines run from actual to actual + change"
    ),
    direction_colors = direction_colors,
    point_colors = point_colors
  )
}

#' Plot actual vs desired allocation summary faceted by country
#'
#' @param df Respondent data frame.
#' @param countries Character vector of country names to facet.
#' @param group_label Plot title label for the combined group.
#' @param meta Question metadata tibble.
#' @param actual_code Question code for actual time (default `"likert0"`).
#' @param desired_code Question code for desired time (default `"likert1"`).
#' @param stat Summary statistic: `"median"` or `"mean"`.
#' @return A ggplot object.
#' @export
plot_likert_allocation_summary_by_country <- function(
    df,
    countries,
    group_label = NULL,
    meta = NULL,
    actual_code = "likert0",
    desired_code = "likert1",
    stat = c("median", "mean")
) {
  stat <- match.arg(stat)
  if (is.null(group_label)) {
    group_label <- paste(countries, collapse = ", ")
  }
  summary_df <- prepare_likert_allocation_summary(
    df,
    countries,
    meta = meta,
    actual_code = actual_code,
    desired_code = desired_code,
    stat = stat,
    by_country = TRUE
  )
  summary_df <- .prepare_likert_allocation_summary_country_positions(
    summary_df,
    countries
  )
  stat_label <- stringr::str_to_title(stat)

  .plot_likert_allocation_summary_grouped_countries(
    summary_df,
    title = paste0("Actual vs desired time by activity — ", group_label, " by country"),
    subtitle = paste0(
      stat_label,
      " change from actual baseline; one line per country within each task (see legend)"
    ),
    countries = countries
  )
}

#' Print actual vs desired allocation summary chart or a fallback message
#'
#' @param df Respondent data frame.
#' @param countries One or more country names (`socio1_0` values) to include.
#' @param meta Question metadata tibble.
#' @param actual_code Question code for actual time (default `"likert0"`).
#' @param desired_code Question code for desired time (default `"likert1"`).
#' @param country_label Plot title label.
#' @param stat Summary statistic: `"median"` or `"mean"`.
#' @param empty_message Message when no responses are available.
#' @return Invisibly `NULL`.
#' @export
print_likert_allocation_summary <- function(
    df,
    countries,
    meta = NULL,
    actual_code = "likert0",
    desired_code = "likert1",
    country_label = NULL,
    stat = c("median", "mean"),
    empty_message = "No responses available for this country."
) {
  actual_cols <- question_columns(actual_code, names(df))
  desired_cols <- question_columns(desired_code, names(df))
  if (
    !has_likert_observations(df, actual_cols) ||
      !has_likert_observations(df, desired_cols)
  ) {
    label <- if (is.null(country_label)) {
      paste(countries, collapse = ", ")
    } else {
      country_label
    }
    cat(label, ":", empty_message, "\n\n")
    return(invisible(NULL))
  }
  print(plot_likert_allocation_summary(
    df,
    countries,
    country_label = country_label,
    meta = meta,
    actual_code = actual_code,
    desired_code = desired_code,
    stat = stat
  ))
  invisible(NULL)
}

#' Print faceted actual vs desired allocation summary by country
#'
#' @param df Respondent data frame.
#' @param countries Character vector of country names to facet.
#' @param group_label Plot title label for the combined group.
#' @param meta Question metadata tibble.
#' @param actual_code Question code for actual time (default `"likert0"`).
#' @param desired_code Question code for desired time (default `"likert1"`).
#' @param stat Summary statistic: `"median"` or `"mean"`.
#' @param empty_message Message when no responses are available.
#' @return Invisibly `NULL`.
#' @export
print_likert_allocation_summary_by_country <- function(
    df,
    countries,
    group_label = NULL,
    meta = NULL,
    actual_code = "likert0",
    desired_code = "likert1",
    stat = c("median", "mean"),
    empty_message = "No responses available for this selection."
) {
  actual_cols <- question_columns(actual_code, names(df))
  desired_cols <- question_columns(desired_code, names(df))
  if (
    !has_likert_observations(df, actual_cols) ||
      !has_likert_observations(df, desired_cols)
  ) {
    label <- if (is.null(group_label)) {
      paste(countries, collapse = ", ")
    } else {
      group_label
    }
    cat(label, ":", empty_message, "\n\n")
    return(invisible(NULL))
  }
  print(plot_likert_allocation_summary_by_country(
    df,
    countries,
    group_label = group_label,
    meta = meta,
    actual_code = actual_code,
    desired_code = desired_code,
    stat = stat
  ))
  invisible(NULL)
}

#' Print paired actual/desired respondent-level allocation plots
#'
#' @param df Respondent data frame.
#' @param countries Character vector of country names.
#' @param meta Question metadata tibble.
#' @param actual_code Question code for actual time (default `"likert0"`).
#' @param desired_code Question code for desired time (default `"likert1"`).
#' @param country_label Plot title label when combining countries.
#' @param empty_message Message when no responses are available.
#' @return Invisibly `NULL`.
#' @export
print_likert_allocation_paired <- function(
    df,
    countries,
    meta = NULL,
    actual_code = "likert0",
    desired_code = "likert1",
    country_label = NULL,
    empty_message = "No responses available for this country."
) {
  actual_cols <- question_columns(actual_code, names(df))
  desired_cols <- question_columns(desired_code, names(df))
  if (
    !has_likert_observations(df, actual_cols) ||
      !has_likert_observations(df, desired_cols)
  ) {
    label <- if (is.null(country_label)) {
      paste(countries, collapse = ", ")
    } else {
      country_label
    }
    cat(label, ":", empty_message, "\n\n")
    return(invisible(NULL))
  }
  print(plot_likert_allocation_paired(
    df,
    countries,
    country_label = country_label,
    meta = meta,
    actual_code = actual_code,
    desired_code = desired_code
  ))
  invisible(NULL)
}
