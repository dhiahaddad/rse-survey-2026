library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(forcats)
library(gt)
library(stringr)
library(purrr)
library(scales)

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
#' @param allocation_tbl Output of [allocate_text_codes()].
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

#' Recode free-text responses and render summary plus allocation tables
#'
#' Main entry point for question chapters. Tokenizes responses once, applies
#' the recode map, optionally caches the allocation audit trail, and returns
#' both gt tables.
#'
#' @param df Survey data frame with `is_other` and the target column.
#' @param col_name Free-text column to recode.
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
    df,
    col_name,
    recode_map,
    header,
    other = TRUE,
    show_excluded = TRUE,
    cache_id = NULL
) {
  map <- prepare_recode_map(recode_map)
  tokens <- prepare_text_tokens(df, col_name, other = other)

  allocation_tbl <- allocate_text_codes_from_tokens(
    tokens,
    col_name,
    map$to_remove,
    map$active_map
  )
  coded_tbl <- summarize_text_codes_from_tokens(
    tokens,
    col_name,
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
      coded_tbl, col_name, header, other = other
      ),
    allocation_table = render_allocation_table(
      allocation_tbl,
      header = header,
      other = other,
      show_excluded = show_excluded
    )
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
#' each column is a category, and a filled tile indicates selection.
#'
#' @param df Survey data frame with `row_id`, `is_other`, and `col_name`.
#' @param col_name Column containing category labels (one per row per respondent).
#' @return A patchwork object combining the heatmap and marginal bar chart.
#' @export
plot_heatmap <- function(df, col_name) {
  plot_data <- as_tibble(df) |>
    dplyr::select(row_id, is_other, dplyr::all_of(col_name)) |>
    dplyr::filter(is_other == FALSE) |>
    dplyr::distinct(row_id, !!rlang::sym(col_name)) |>
    tidyr::drop_na() |>
    dplyr::mutate(value = 1L) |>
    tidyr::complete(
      row_id,
      !!rlang::sym(col_name),
      fill = list(value = 0L)
    ) |>
    dplyr::group_by(row_id) |>
    dplyr::mutate(row_sum = sum(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data[[col_name]]) |>
    dplyr::mutate(cat_sum = sum(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      row_id = forcats::fct_reorder(row_id, row_sum),
      !!rlang::sym(col_name) := forcats::fct_reorder(.data[[col_name]], 
      .data$cat_sum)
    )

  p_heat <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = row_id,
      y = .data[[col_name]],
      fill = factor(value)
    )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.01) +
    ggplot2::scale_fill_manual(
      values = c("0" = "white", "1" = "darkblue"),
      labels = c("0" = "No", "1" = "Yes"),
      name = ""
    ) +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(40)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Respondent (ordered by #)", y = NULL)

  p_margin <- plot_data |>
    dplyr::distinct(!!rlang::sym(col_name), cat_sum) |>
    ggplot2::ggplot(ggplot2::aes(x = cat_sum, y = .data[[col_name]])) +
    ggplot2::geom_col(fill = "darkblue") +
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

  p_heat + p_margin +
    patchwork::plot_layout(widths = c(5, 2)) &
    ggplot2::theme(legend.position = "none")
}
