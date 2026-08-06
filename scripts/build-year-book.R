#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
interactive_charts <- any(args == "--interactive")
if (sum(args == "--interactive") > 1L) {
  stop("Specify --interactive only once.", call. = FALSE)
}
args <- args[args != "--interactive"]
include_free_text <- any(args == "--include-free-text")
if (sum(args == "--include-free-text") > 1L) {
  stop("Specify --include-free-text only once.", call. = FALSE)
}
args <- args[args != "--include-free-text"]
country_filter <- "Germany"
country_equals <- grep("^--country=", args)
country_flag <- which(args == "--country")
if (length(country_equals) + length(country_flag) > 1L) {
  stop("Specify --country only once.", call. = FALSE)
}
if (length(country_equals) == 1L) {
  country_filter <- sub("^--country=", "", args[[country_equals]])
  args <- args[-country_equals]
} else if (length(country_flag) == 1L) {
  if (country_flag == length(args)) {
    stop("--country requires a country name or all.", call. = FALSE)
  }
  country_filter <- args[[country_flag + 1L]]
  args <- args[-c(country_flag, country_flag + 1L)]
}
country_filter <- trimws(country_filter)
if (!nzchar(country_filter)) {
  stop("--country cannot be empty.", call. = FALSE)
}
country_filters <- trimws(strsplit(country_filter, ",", fixed = TRUE)[[1L]])
if (any(!nzchar(country_filters))) {
  stop("--country contains an empty country name.", call. = FALSE)
}
all_countries <- length(country_filters) == 1L &&
  tolower(country_filters[[1L]]) == "all"
if (!all_countries && any(tolower(country_filters) == "all")) {
  stop("Use --country all by itself.", call. = FALSE)
}
inclusion_mode <- "all"
inclusion_equals <- grep("^--inclusion=", args)
inclusion_flag <- which(args == "--inclusion")
if (length(inclusion_equals) + length(inclusion_flag) > 1L) {
  stop("Specify --inclusion only once.", call. = FALSE)
}
if (length(inclusion_equals) == 1L) {
  inclusion_mode <- sub("^--inclusion=", "", args[[inclusion_equals]])
  args <- args[-inclusion_equals]
} else if (length(inclusion_flag) == 1L) {
  if (inclusion_flag == length(args)) {
    stop("--inclusion requires either all or submitted.", call. = FALSE)
  }
  inclusion_mode <- args[[inclusion_flag + 1L]]
  args <- args[-c(inclusion_flag, inclusion_flag + 1L)]
}
if (!inclusion_mode %in% c("all", "submitted")) {
  stop("--inclusion must be either all or submitted.", call. = FALSE)
}

if (length(args) < 2L) {
  stop(
    paste(
      "Usage: Rscript scripts/build-year-book.R YEAR DATA_ROOT [OUTPUT_ROOT]",
      "[--country COUNTRY[,COUNTRY...]|all] [--inclusion all|submitted]",
      "[--include-free-text] [--interactive]"
    ),
    call. = FALSE
  )
}

survey_year <- args[[1L]]
if (!grepl("^[0-9]{4}$", survey_year)) {
  stop("YEAR must contain four digits.", call. = FALSE)
}

data_root <- normalizePath(args[[2L]], mustWork = TRUE)
output_root <- if (length(args) >= 3L) args[[3L]] else ".generated/year-books"
project_dir <- file.path(output_root, survey_year)
data_dir <- file.path(data_root, survey_year)
tf_path <- file.path(data_dir, paste0(survey_year, "_tf.csv"))
cols_path <- file.path(data_dir, paste0(survey_year, "_cols.csv"))

if (!file.exists(tf_path) || !file.exists(cols_path)) {
  stop("Missing survey files for ", survey_year, " in ", data_dir, call. = FALSE)
}

if (interactive_charts) {
  missing_packages <- c("plotly", "htmlwidgets")[!vapply(
    c("plotly", "htmlwidgets"),
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )]
  if (length(missing_packages)) {
    stop(
      "Interactive charts require the R package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Install them before using --interactive.",
      call. = FALSE
    )
  }
}

if (dir.exists(project_dir)) {
  unlink(project_dir, recursive = TRUE)
}
dir.create(file.path(project_dir, "questions"), recursive = TRUE)
dir.create(file.path(project_dir, "figures"), recursive = TRUE)

read_survey_csv <- function(path) {
  x <- read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("NA")
  )
  blank <- is.na(names(x)) | !nzchar(names(x))
  names(x)[blank] <- paste0(".index", seq_len(sum(blank)))
  x
}

question_stem <- function(x) {
  x <- sub("_0$", "", x)
  x <- sub("[-_][0-9]+$", "", x)
  sub("\\[.*$", "", x)
}

nonempty <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

yaml_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

markdown_text <- function(x) {
  x <- gsub("[\r\n]+", " ", as.character(x))
  x <- gsub("|", "\\|", x, fixed = TRUE)
  trimws(x)
}

write_markdown_table <- function(df) {
  if (!nrow(df)) {
    return("_No responses._")
  }
  values <- lapply(df, function(x) markdown_text(ifelse(is.na(x), "", x)))
  df <- as.data.frame(values, stringsAsFactors = FALSE)
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- apply(df, 1L, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, divider, rows), collapse = "\n")
}

row_has_response <- function(df) {
  if (!ncol(df)) return(rep(FALSE, nrow(df)))
  apply(df, 1L, function(row) any(nonempty(row)))
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  gsub("[\r\n]+", "<br>", x)
}

interactive_chart_html <- function(
  plot,
  widget_path,
  png_rel,
  stem,
  height,
  chart_summary = NULL
) {
  tooltip <- if (!is.null(plot$mapping$text)) "text" else c("x", "y")
  widget <- plotly::ggplotly(plot, tooltip = tooltip)
  widget <- plotly::config(
    widget,
    responsive = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d")
  )
  if (!is.null(chart_summary)) {
    widget <- plotly::layout(
      widget,
      title = list(
        text = chart_summary,
        x = 0,
        xanchor = "left",
        font = list(size = 12)
      )
    )
  }
  widget$width <- "100%"
  widget$height <- height
  htmlwidgets::saveWidget(
    widget,
    widget_path,
    selfcontained = FALSE,
    libdir = "widget-lib",
    background = "transparent"
  )
  c(
    paste0(
      "<iframe class=\"survey-interactive-chart\" src=\"../figures/",
      html_escape(basename(widget_path)),
      "\" title=\"Interactive response distribution for ",
      html_escape(stem),
      "\" loading=\"lazy\" style=\"width:100%;height:",
      height,
      "px;border:0;\"></iframe>"
    ),
    "<noscript>",
    paste0(
      "<img src=\"../", html_escape(png_rel),
      "\" alt=\"Response distribution for ", html_escape(stem), ".\">"
    ),
    "</noscript>"
  )
}

free_text_details <- function(values, heading = "Recorded answers") {
  values <- trimws(as.character(values))
  values <- values[nonempty(values)]
  if (!length(values)) return(character())
  counts <- sort(table(values), decreasing = TRUE)
  labels <- html_escape(names(counts))
  count_labels <- ifelse(
    as.integer(counts) > 1L,
    paste0(" <strong>× ", as.integer(counts), "</strong>"),
    ""
  )
  c(
    paste0("## ", heading),
    "",
    "<details>",
    paste0(
      "<summary>Show ", length(values), " recorded answers (",
      length(counts), " distinct)</summary>"
    ),
    "<ul>",
    paste0("<li>", labels, count_labels, "</li>"),
    "</ul>",
    "</details>",
    ""
  )
}

flatten_responses <- function(values) {
  flat <- trimws(unlist(values, use.names = FALSE))
  flat[nonempty(flat)]
}

selection_true_values <- c("true", "1", "yes", "y", "selected", "checked")
selection_false_values <- c("false", "0", "no", "n", "unselected", "unchecked")

is_boolean_selection <- function(values) {
  flat <- tolower(flatten_responses(values))
  length(flat) > 0L && all(flat %in% c(selection_true_values, selection_false_values))
}

parse_numeric_responses <- function(values) {
  cleaned <- trimws(as.character(values))
  cleaned <- sub("%$", "", cleaned)
  decimal_comma <- grepl("^[+-]?[0-9]+,[0-9]+$", cleaned)
  cleaned[decimal_comma] <- sub(",", ".", cleaned[decimal_comma], fixed = TRUE)
  valid <- grepl("^[+-]?([0-9]+([.][0-9]+)?|[.][0-9]+)$", cleaned)
  result <- rep(NA_real_, length(cleaned))
  result[valid] <- as.numeric(cleaned[valid])
  result
}

looks_like_date_values <- function(values) {
  values <- trimws(as.character(values))
  values <- values[nonempty(values)]
  if (!length(values)) return(FALSE)
  patterns <- c(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}",
    "^[0-9]{2}[./-][0-9]{2}[./-][0-9]{4}",
    "^[0-9]{4}$"
  )
  mean(vapply(values, function(value) {
    any(vapply(patterns, grepl, logical(1), x = value))
  }, logical(1))) >= 0.8
}

detect_question_type <- function(stem, columns, values, meta, prompt) {
  flat <- flatten_responses(values)
  unique_values <- unique(flat)
  lower_values <- tolower(unique_values)
  prompt_lower <- tolower(prompt)
  family <- question_family(stem)
  n_columns <- length(columns)
  n_values <- length(flat)
  n_distinct <- length(unique_values)
  distinct_ratio <- if (n_values) n_distinct / n_values else 0
  mean_length <- if (n_values) mean(nchar(flat)) else 0
  upper_length <- if (n_values) unname(quantile(nchar(flat), 0.75)) else 0

  metadata_rows <- meta[question_stem(meta$New_name) == stem, , drop = FALSE]
  metadata_options <- if ("Option" %in% names(metadata_rows)) {
    trimws(as.character(metadata_rows$Option))
  } else {
    character()
  }
  metadata_options <- metadata_options[nonempty(metadata_options)]
  has_structured_metadata <- n_columns > 1L &&
    length(metadata_options) >= max(1L, n_columns - 1L)

  if (n_columns > 1L && is_boolean_selection(values)) {
    return(list(kind = "multiple_select", label = "Multiple select"))
  }
  explicit_boolean <- length(lower_values) > 0L && all(
    lower_values %in% c("true", "false", "yes", "no", "y", "n")
  )
  if (n_columns == 1L && explicit_boolean) {
    return(list(kind = "single_select", label = "Single select"))
  }

  scale_value_pattern <- paste(
    c(
      "strongly agree", "strongly disagree", "neither.*agree", "satisfied",
      "dissatisfied", "not at all", "completely", "never", "always",
      "all the time", "all my time", "mostly me", "mostly other", "rarely",
      "daily", "weekly", "monthly", "very likely", "very unlikely"
    ),
    collapse = "|"
  )
  scale_prompt <- grepl(
    paste(
      c(
        "how often", "how frequently", "how satisfied", "to what extent",
        "how much .*time", "on average, how much", "agree or disagree",
        "who uses .*code"
      ),
      collapse = "|"
    ),
    prompt_lower
  )
  scale_values <- any(grepl(scale_value_pattern, lower_values))
  compact_numeric_scale <- FALSE
  if (n_columns > 1L && n_distinct >= 3L && n_distinct <= 11L) {
    scale_numbers <- parse_numeric_responses(unique_values)
    compact_numeric_scale <- all(!is.na(scale_numbers)) &&
      min(scale_numbers) >= 0 && max(scale_numbers) <= 10
  }
  is_rating_scale <- family == "likert" || compact_numeric_scale ||
    (n_distinct >= 1L && n_distinct <= 11L && (scale_prompt || scale_values))
  if (is_rating_scale) {
    return(list(kind = "likert", label = "Likert / rating scale"))
  }

  long_text_prompt <- grepl(
    paste(
      c(
        "explain", "describe", "further suggestion", "further comment",
        "additional comment", "in your own words", "how did you learn",
        "what .* do you think", "what would .* need", "what specific activities",
        "suggestions or wishes"
      ),
      collapse = "|"
    ),
    prompt_lower
  )
  short_text_prompt <- grepl(
    paste(
      c(
        "please enter", "please specify", "please list", "list any",
        "job title", "which .*tools", "which conference", "at which conference",
        "what training programs", "what three skills", "which online collaboration"
      ),
      collapse = "|"
    ),
    prompt_lower
  )
  high_cardinality_text <- !has_structured_metadata && n_distinct >= 10L &&
    mean_length >= 12 && distinct_ratio >= 0.6
  probable_text <- long_text_prompt || short_text_prompt || high_cardinality_text

  date_prompt <- grepl(
    "(^|[^a-z])(date|when did|what year|which year|in what year)([^a-z]|$)",
    prompt_lower
  )
  if (n_columns == 1L && date_prompt && looks_like_date_values(flat)) {
    return(list(kind = "date", label = "Date / year input"))
  }

  numeric_values <- parse_numeric_responses(flat)
  numeric_ratio <- if (n_values) mean(!is.na(numeric_values)) else 0
  numeric_prompt <- grepl(
    paste(
      c(
        "how many", "what percentage", "percentage of", "duration .*years",
        "duration \\(in years\\)", "number of", "bus factor"
      ),
      collapse = "|"
    ),
    prompt_lower
  )
  is_numeric_input <- n_columns == 1L && !probable_text && (
    numeric_ratio == 1 ||
      (numeric_prompt && numeric_ratio >= 0.8 && n_distinct > 10L)
  )
  if (is_numeric_input) {
    return(list(kind = "numeric", label = "Numeric input"))
  }

  if (probable_text) {
    is_long <- long_text_prompt || mean_length >= 80 || upper_length >= 120 ||
      any(grepl("[\r\n]", flat))
    return(if (is_long) {
      list(kind = "long_text", label = "Long free text")
    } else {
      list(kind = "short_text", label = "Short free text")
    })
  }

  if (n_columns == 1L) {
    return(list(kind = "single_select", label = "Single select"))
  }

  list(kind = "matrix", label = "Matrix / grid")
}

metadata_label <- function(meta, column, fallback) {
  hit <- meta[meta$New_name == column, , drop = FALSE]
  if (nrow(hit) && "Option" %in% names(hit)) {
    option <- as.character(hit$Option[[1L]])
    if (!is.na(option) && nzchar(trimws(option))) {
      return(trimws(option))
    }
  }
  fallback
}

metadata_column_order <- function(meta, columns) {
  positions <- match(columns, as.character(meta$New_name))
  ordering <- order(is.na(positions), positions, seq_along(columns))
  ordered <- columns[ordering]
  source <- if (any(!is.na(positions))) {
    "metadata item order"
  } else {
    "export column order"
  }
  list(columns = ordered, source = source)
}

leading_number <- function(values) {
  cleaned <- trimws(gsub("^﻿", "", as.character(values)))
  valid <- grepl("^[+-]?[0-9]+([.,][0-9]+)?", cleaned)
  extracted <- sub(
    "^([+-]?[0-9]+([.,][0-9]+)?).*$",
    "\\1",
    cleaned
  )
  extracted <- sub(",", ".", extracted, fixed = TRUE)
  result <- rep(NA_real_, length(values))
  result[valid] <- as.numeric(extracted[valid])
  result
}

ordered_response_levels <- function(
  values,
  kind = "single_select",
  metadata_levels = character()
) {
  levels <- unique(trimws(as.character(values)))
  levels <- levels[nonempty(levels)]
  if (!length(levels)) {
    return(list(levels = character(), source = "no recorded answers"))
  }
  lower <- tolower(trimws(gsub("^﻿", "", levels)))

  metadata_levels <- unique(trimws(as.character(metadata_levels)))
  metadata_levels <- metadata_levels[nonempty(metadata_levels)]
  metadata_levels <- metadata_levels[metadata_levels %in% levels]
  if (length(metadata_levels) == length(levels)) {
    return(list(levels = metadata_levels, source = "metadata answer order"))
  }

  numeric_prefix <- leading_number(levels)
  numeric_coverage <- mean(!is.na(numeric_prefix))
  if (numeric_coverage >= 0.6 && sum(!is.na(numeric_prefix)) >= 2L) {
    ordering <- order(
      is.na(numeric_prefix),
      numeric_prefix,
      grepl("prefer not|no answer|not applicable", lower),
      seq_along(levels)
    )
    return(list(levels = levels[ordering], source = "numeric answer order"))
  }

  semantic_rank <- rep(NA_real_, length(levels))
  semantic_rank[grepl("^strongly disagree", lower)] <- 1
  semantic_rank[grepl("^disagree", lower)] <- 2
  semantic_rank[grepl("neither.*agree|neutral", lower)] <- 3
  semantic_rank[grepl("^agree", lower)] <- 4
  semantic_rank[grepl("^strongly agree", lower)] <- 5

  semantic_rank[grepl("^never", lower)] <- 1
  semantic_rank[grepl("^rarely", lower)] <- 2
  semantic_rank[grepl("^monthly", lower)] <- 3
  semantic_rank[grepl("^weekly", lower)] <- 4
  semantic_rank[grepl("^daily", lower)] <- 5

  semantic_rank[grepl("^decrease", lower)] <- 1
  semantic_rank[grepl("^same$|no change", lower)] <- 2
  semantic_rank[grepl("^increase", lower)] <- 3

  semantic_rank[grepl("^true$|^yes$", lower)] <- 1
  semantic_rank[grepl("^false$|^no$", lower)] <- 2
  semantic_rank[grepl("prefer not|no answer|not applicable", lower)] <- Inf

  if (sum(!is.na(semantic_rank)) >= 2L && (
    kind == "likert" || all(!is.na(semantic_rank))
  )) {
    ordering <- order(is.na(semantic_rank), semantic_rank, seq_along(levels))
    return(list(levels = levels[ordering], source = "semantic answer order"))
  }

  counts <- table(factor(values, levels = levels))
  ordering <- order(-as.integer(counts), seq_along(levels))
  list(levels = levels[ordering], source = "frequency fallback")
}

semantic_response_palette <- function(levels, kind) {
  levels <- as.character(levels)
  if (length(levels) < 2L) return(NULL)

  red <- "#C83E4D"
  yellow <- "#F7B552"
  green <- "#2E8B57"
  grey <- "#8A8F98"
  lower <- tolower(trimws(gsub("^﻿", "", levels)))
  non_substantive <- grepl(
    paste(
      c(
        "prefer not", "not applicable", "no answer", "don't know",
        "do not know", "unknown", "cannot say"
      ),
      collapse = "|"
    ),
    lower
  )
  substantive <- !non_substantive
  palette <- rep(NA_character_, length(levels))

  yes <- grepl("^(yes|true)$", lower)
  no <- grepl("^(no|false)$", lower)
  if (all(!substantive | yes | no) && any(yes) && any(no)) {
    palette[yes] <- green
    palette[no] <- red
    palette[non_substantive] <- grey
    names(palette) <- levels
    return(palette)
  }

  if (kind != "likert") return(NULL)

  numeric_values <- leading_number(levels)
  if (
    sum(substantive) >= 2L &&
      all(!substantive | !is.na(numeric_values)) &&
      diff(range(numeric_values[substantive])) > 0
  ) {
    position <- (
      numeric_values - min(numeric_values[substantive])
    ) / diff(range(numeric_values[substantive]))
    ramp <- grDevices::colorRamp(c(red, yellow, green))
    palette[substantive] <- grDevices::rgb(
      ramp(position[substantive]),
      maxColorValue = 255
    )
    palette[non_substantive] <- grey
    names(palette) <- levels
    return(palette)
  }

  score <- rep(NA_real_, length(levels))
  score[grepl("^strongly disagree|^very dissatisfied|^very unlikely", lower)] <- 1
  score[grepl("^disagree|^dissatisfied|^unlikely", lower)] <- 2
  score[grepl("neither|neutral|^sometimes|^occasionally", lower)] <- 3
  score[grepl("^agree|^satisfied|^likely", lower)] <- 4
  score[grepl("^strongly agree|^very satisfied|^very likely", lower)] <- 5
  score[grepl("^never", lower)] <- 1
  score[grepl("^rarely", lower)] <- 2
  score[grepl("^often", lower)] <- 4
  score[grepl("^always", lower)] <- 5

  if (sum(substantive) >= 2L && all(!substantive | !is.na(score))) {
    position <- (score - min(score[substantive])) /
      diff(range(score[substantive]))
    if (all(is.finite(position[substantive]))) {
      ramp <- grDevices::colorRamp(c(red, yellow, green))
      palette[substantive] <- grDevices::rgb(
        ramp(position[substantive]),
        maxColorValue = 255
      )
      palette[non_substantive] <- grey
      names(palette) <- levels
      return(palette)
    }
  }

  NULL
}

chart_height_pixels <- function(kind, row_count, legend_count = 0L) {
  if (kind == "numeric") return(420L)

  row_count <- max(1L, as.integer(row_count))
  base_height <- if (kind == "matrix") 130L else 90L
  legend_height <- if (kind == "matrix" && legend_count > 0L) {
    28L * ceiling(legend_count / 5L)
  } else {
    0L
  }
  max(200L, min(1200L, base_height + 42L * row_count + legend_height))
}

question_text <- function(meta, stem) {
  hit <- meta[question_stem(meta$New_name) == stem, "Question", drop = TRUE]
  hit <- unique(trimws(as.character(hit[nonempty(hit)])))
  if (!length(hit)) return(stem)
  result <- trimws(sub("\\s*\\[[^]]*\\]\\s*$", "", hit[[1L]]))
  if (nonempty(result)) result else stem
}

safe_filename <- function(x) {
  value <- gsub("[^A-Za-z0-9_-]+", "-", x)
  value <- gsub("^-+|-+$", "", value)
  if (nzchar(value)) value else "question"
}

question_family <- function(stem) {
  match <- regexpr("^[A-Za-z]+", stem)
  if (match[[1L]] < 0L) return("")
  regmatches(stem, match)
}

section_families <- list(
  "RSE role" = c("rse"),
  "Education" = c("edu"),
  "Software experience" = c("soft"),
  "Open science" = c("open"),
  "Organisations and community" = c("org", "ukrse", "group"),
  "Employment" = c("currentEmp", "prevEmp"),
  "Work activities" = c("currentWork", "time", "timeLike"),
  "Career, recognition and satisfaction" = c(
    "affRec", "affSat", "percEmp", "perfCheck", "progRSE", "recog", "satisGen"
  ),
  "Attitudes and Likert scales" = c("likert"),
  "Turnover" = c("turnOver"),
  "Job stability" = c("stability"),
  "Publications" = c("paper", "ref"),
  "Conferences" = c("conf"),
  "Project management" = c("proj"),
  "Training and skills" = c("train", "skill", "skillNord"),
  "Funding" = c("fund"),
  "Tooling" = c("tool"),
  "Research infrastructure and data" = c("caf", "canarie", "data", "instit"),
  "Generative AI" = c("genAI"),
  "Demographics" = c("socio")
)
section_order <- c(names(section_families), "Other questions")

question_section <- function(stem) {
  family <- question_family(stem)
  matches <- names(section_families)[vapply(
    section_families,
    function(families) family %in% families,
    logical(1)
  )]
  if (length(matches)) matches[[1L]] else "Other questions"
}

tf_source_all <- read_survey_csv(tf_path)
meta <- read_survey_csv(cols_path)
if (!"Option" %in% names(meta)) {
  meta$Option <- ""
}

country_column <- if ("socio1_0" %in% names(tf_source_all)) "socio1_0" else NULL
if (is.null(country_column) && !all_countries) {
  stop(
    "Cannot filter by country because the export has no socio1_0 column.",
    call. = FALSE
  )
}
if (all_countries) {
  tf_source <- tf_source_all
  country_label <- "All countries"
  country_note <- "All countries in the export are included."
} else {
  countries <- trimws(as.character(tf_source_all[[country_column]]))
  country_rows <- nonempty(countries) &
    tolower(countries) %in% tolower(country_filters)
  available <- sort(unique(countries[nonempty(countries)]))
  missing_countries <- country_filters[
    !tolower(country_filters) %in% tolower(available)
  ]
  if (length(missing_countries)) {
    stop(
      "No records found for: ", paste(missing_countries, collapse = ", "),
      ". Available countries: ",
      paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  country_labels <- vapply(country_filters, function(requested) {
    available[tolower(available) == tolower(requested)][[1L]]
  }, character(1))
  country_label <- paste(country_labels, collapse = ", ")
  tf_source <- tf_source_all[country_rows, , drop = FALSE]
  country_note <- if (length(country_labels) == 1L) {
    paste0(
      "Only respondents whose `socio1_0` value is **",
      country_labels[[1L]], "** are included."
    )
  } else {
    paste0(
      "Only respondents whose `socio1_0` value is one of **",
      paste(country_labels, collapse = "**, **"), "** are included."
    )
  }
}

has_submission_status <- "submitdate_0" %in% names(tf_source)
if (has_submission_status) {
  source_submitted <- nonempty(tf_source$submitdate_0)
  tf_source$.submission_status <- ifelse(source_submitted, "Submitted", "Partial")
  if (inclusion_mode == "submitted") {
    tf <- tf_source[source_submitted, , drop = FALSE]
    submission_note <- "Only records with a non-empty `submitdate_0` are included."
  } else {
    tf <- tf_source
    submission_note <- paste(
      "Submitted and partial records are included. Each question uses only",
      "records with a recorded answer to that question."
    )
  }
} else {
  tf <- tf_source
  tf$.submission_status <- "Recorded"
  submission_note <- paste0(
    "This export has no `submitdate_0` field, so all ", nrow(tf),
    " rows are included."
  )
}

answering_summary <- function(answered_rows) {
  answered_rows[is.na(answered_rows)] <- FALSE
  answered_n <- sum(answered_rows)
  if (!has_submission_status || inclusion_mode == "submitted") {
    return(paste0("**Respondents answering:** ", answered_n))
  }
  statuses <- factor(
    tf$.submission_status[answered_rows],
    levels = c("Submitted", "Partial")
  )
  counts <- table(statuses)
  paste0(
    "**Respondents answering:** ", answered_n,
    " (", unname(counts[["Submitted"]]), " submitted; ",
    unname(counts[["Partial"]]), " partial)"
  )
}

free_text_status_lines <- function(answered_rows) {
  answered_rows[is.na(answered_rows)] <- FALSE
  lines <- c(paste0("Responses recorded: ", sum(answered_rows)))
  if (has_submission_status) {
    lines <- c(
      lines,
      "",
      paste0(
        "Submitted: ",
        sum(answered_rows & tf$.submission_status == "Submitted")
      ),
      "",
      paste0(
        "Partial: ",
        sum(answered_rows & tf$.submission_status == "Partial")
      )
    )
  } else {
    lines <- c(
      lines,
      "",
      "Submitted: not available",
      "",
      "Partial: not available"
    )
  }
  lines
}

system_columns <- c(
  "id", "submitdate_0", "lastpage", "lastpage_0", "startlanguage",
  "startlanguage_0", "seed", "seed_0", "token", "startdate",
  "startdate_0", "datestamp", "ipaddr", "refurl", "email",
  "emailstatus", "sent", "remindersent", "remindercount",
  "lastreminder", "usesleft", "completed", "language", "row_id",
  "Year", "Year_0"
)
response_columns <- setdiff(
  names(tf)[!startsWith(names(tf), ".index")],
  system_columns
)
ordered_stems <- unique(question_stem(response_columns))
metadata_stems <- unique(question_stem(meta$New_name[nonempty(meta$New_name)]))
ordered_stems <- ordered_stems[ordered_stems %in% metadata_stems]
ordered_stems <- ordered_stems[vapply(
  ordered_stems,
  function(stem) {
    columns <- response_columns[question_stem(response_columns) == stem]
    any(row_has_response(tf[, columns, drop = FALSE]))
  },
  logical(1)
)]
question_sections <- vapply(ordered_stems, question_section, character(1))
present_sections <- section_order[section_order %in% question_sections]
section_summary <- data.frame(
  Section = present_sections,
  Questions = vapply(
    present_sections,
    function(section) sum(question_sections == section),
    integer(1)
  )
)
unknown_families <- unique(vapply(
  ordered_stems[question_sections == "Other questions"],
  question_family,
  character(1)
))

country_summary <- if (!is.null(country_column) && all_countries) {
  countries <- trimws(as.character(tf[[country_column]]))
  present <- nonempty(countries)
  counts <- sort(table(countries[present]), decreasing = TRUE)
  result <- data.frame(Country = names(counts), Responses = as.integer(counts))
  if (has_submission_status && inclusion_mode == "all") {
    submitted_counts <- table(countries[present & tf$.submission_status == "Submitted"])
    partial_counts <- table(countries[present & tf$.submission_status == "Partial"])
    result$Submitted <- as.integer(submitted_counts[result$Country])
    result$Partial <- as.integer(partial_counts[result$Country])
    result$Submitted[is.na(result$Submitted)] <- 0L
    result$Partial[is.na(result$Partial)] <- 0L
  }
  result
} else {
  data.frame()
}

book_title <- if (all_countries) {
  paste("International RSE Survey", survey_year)
} else {
  paste("International RSE Survey", survey_year, "—", country_label)
}

index_lines <- c(
  "---",
  paste0("title: ", yaml_string(book_title)),
  "---",
  "",
  paste0("# Survey overview {.unnumbered}"),
  "",
  paste0(
    "This book presents the **", survey_year,
    " survey independently for ", country_label,
    "**, using the questions and answer options stored in that year's export."
  ),
  "",
  paste0("**Country scope:** ", country_label),
  "",
  paste0("**Source records:** ", nrow(tf_source_all)),
  "",
  paste0("**Records matching country scope:** ", nrow(tf_source)),
  "",
  paste0("**Respondents included:** ", nrow(tf)),
  "",
  paste0("**Question groups discovered:** ", length(ordered_stems)),
  "",
  submission_note,
  "",
  country_note,
  "",
  "No cross-year harmonisation is applied.",
  ""
)
if (has_submission_status) {
  index_lines <- c(
    index_lines,
    paste0("**Submitted records:** ", sum(tf_source$.submission_status == "Submitted")),
    "",
    paste0("**Partial records:** ", sum(tf_source$.submission_status == "Partial")),
    ""
  )
}
if (nrow(section_summary)) {
  index_lines <- c(
    index_lines,
    "## Question sections",
    "",
    write_markdown_table(section_summary),
    ""
  )
}
if (nrow(country_summary)) {
  index_lines <- c(
    index_lines,
    if (has_submission_status && inclusion_mode == "submitted") {
      "## Submitted responses by country"
    } else {
      "## Recorded responses by country"
    },
    "",
    write_markdown_table(country_summary),
    ""
  )
}
writeLines(index_lines, file.path(project_dir, "index.qmd"))

chapter_files <- character()
question_types <- character()
question_order_sources <- character()

for (i in seq_along(ordered_stems)) {
  stem <- ordered_stems[[i]]
  columns <- response_columns[question_stem(response_columns) == stem]
  column_order <- metadata_column_order(meta, columns)
  columns <- column_order$columns
  title <- question_text(meta, stem)
  is_other_column <- grepl("[other]", columns, fixed = TRUE)
  other_columns <- columns[is_other_column]
  analysis_columns <- columns[!is_other_column]
  if (!length(analysis_columns)) {
    analysis_columns <- columns
    other_columns <- character()
  }
  other_values <- if (length(other_columns)) {
    trimws(unlist(tf[, other_columns, drop = FALSE], use.names = FALSE))
  } else {
    character()
  }
  other_values <- other_values[nonempty(other_values)]
  other_answered_rows <- row_has_response(tf[, other_columns, drop = FALSE])
  values <- lapply(tf[, analysis_columns, drop = FALSE], as.character)
  all_values <- flatten_responses(values)
  response_metadata_levels <- as.character(
    meta$Option[question_stem(meta$New_name) == stem]
  )
  detected_type <- detect_question_type(
    stem,
    analysis_columns,
    values,
    meta,
    title
  )
  type <- detected_type$label
  question_types <- c(question_types, type)
  is_free_text_group <- detected_type$kind %in% c("short_text", "long_text")
  is_count_only_group <- is_free_text_group || detected_type$kind == "date"
  chart_summary <- NULL
  chart_row_count <- 0L
  chart_legend_count <- 0L
  chart_layout <- "bars"

  if (is_count_only_group) {
    order_source <- "not applicable"
    answered_rows <- row_has_response(tf[, analysis_columns, drop = FALSE])
    display <- data.frame()
    truncated_note <- NULL
    plot <- NULL
  } else if (detected_type$kind == "numeric") {
    chart_layout <- "numeric"
    order_source <- "numeric values"
    raw_all <- trimws(as.character(tf[[analysis_columns[[1L]]]]))
    answered_rows <- nonempty(raw_all)
    raw <- raw_all[nonempty(raw_all)]
    numeric_values <- parse_numeric_responses(raw)
    valid_numeric <- numeric_values[!is.na(numeric_values)]
    display <- data.frame(
      Statistic = c("Numeric responses", "Median", "Mean", "Minimum", "Maximum"),
      Value = c(
        length(valid_numeric),
        format(round(median(valid_numeric), 2), trim = TRUE),
        format(round(mean(valid_numeric), 2), trim = TRUE),
        format(min(valid_numeric), trim = TRUE),
        format(max(valid_numeric), trim = TRUE)
      ),
      stringsAsFactors = FALSE
    )
    chart_summary <- paste0(
      "n = ", length(valid_numeric),
      " · median = ", format(round(median(valid_numeric), 2), trim = TRUE),
      " · mean = ", format(round(mean(valid_numeric), 2), trim = TRUE),
      " · range = ", format(min(valid_numeric), trim = TRUE),
      "–", format(max(valid_numeric), trim = TRUE)
    )
    omitted_numeric <- length(raw) - length(valid_numeric)
    truncated_note <- if (omitted_numeric > 0L) {
      paste0(
        omitted_numeric,
        " recorded value(s) could not be interpreted as numbers and are omitted from the histogram."
      )
    } else {
      NULL
    }
    chart_row_count <- 1L
    plot <- ggplot(data.frame(Value = valid_numeric), aes(x = Value)) +
      geom_histogram(
        bins = min(30L, max(5L, ceiling(sqrt(length(valid_numeric))))),
        fill = "#2C7FB8",
        colour = "white"
      ) +
      labs(
        x = NULL,
        y = "Responses",
        subtitle = chart_summary
      ) +
      theme_minimal(base_size = 11)
  } else if (length(analysis_columns) == 1L) {
    raw_all <- trimws(as.character(tf[[analysis_columns[[1L]]]]))
    answered_rows <- nonempty(raw_all) | other_answered_rows
    raw <- raw_all[nonempty(raw_all)]
    response_order <- ordered_response_levels(
      raw,
      detected_type$kind,
      response_metadata_levels
    )
    order_source <- response_order$source
    counts <- table(factor(raw, levels = response_order$levels))
    answered_n <- sum(answered_rows)
    denominator <- if (answered_n > 0L) answered_n else 1L
    result <- data.frame(
      Response = names(counts),
      Count = as.integer(counts),
      Percent = sprintf("%.1f%%", 100 * as.integer(counts) / denominator)
    )
    display <- if (interactive_charts) result else head(result, 30L)
    chart_row_count <- nrow(display)
    truncated_note <- if (nrow(result) > nrow(display)) {
      paste0("Only the 30 most frequent of ", nrow(result), " distinct responses are shown.")
    } else {
      NULL
    }
    plot_data <- display
    names(plot_data)[1L] <- "label"
    plot_data$label <- factor(
      plot_data$label,
      levels = rev(as.character(plot_data$label))
    )
    plot_data$hover <- paste0(
      as.character(plot_data$label),
      "<br>Count: ", plot_data$Count,
      "<br>Percent: ", plot_data$Percent
    )
    semantic_colors <- semantic_response_palette(
      response_order$levels,
      detected_type$kind
    )
    plot <- ggplot(plot_data, aes(x = label, y = Count, text = hover))
    plot <- if (is.null(semantic_colors)) {
      plot + geom_col(fill = "#9A3270", width = 0.72)
    } else {
      plot +
        geom_col(aes(fill = label), width = 0.72) +
        scale_fill_manual(values = semantic_colors, guide = "none")
    }
    plot <- plot +
      coord_flip() +
      labs(x = NULL, y = "Responses") +
      theme_minimal(base_size = 11)
  } else if (detected_type$kind == "multiple_select") {
    order_source <- column_order$source
    selected <- vapply(values, function(x) {
      sum(tolower(trimws(x)) %in% selection_true_values, na.rm = TRUE)
    }, integer(1))
    answered_rows <- apply(tf[, analysis_columns, drop = FALSE], 1L, function(row) {
      any(
        tolower(trimws(as.character(row))) %in% selection_true_values,
        na.rm = TRUE
      )
    }) | other_answered_rows
    answered_n <- sum(answered_rows)
    labels <- vapply(seq_along(analysis_columns), function(j) {
      metadata_label(meta, analysis_columns[[j]], analysis_columns[[j]])
    }, character(1))
    denominator <- if (answered_n > 0L) answered_n else 1L
    result <- data.frame(
      Option = labels,
      Count = selected,
      Percent = sprintf("%.1f%%", 100 * selected / denominator)
    )
    display <- result
    chart_row_count <- nrow(display)
    truncated_note <- NULL
    plot_data <- result
    names(plot_data)[1L] <- "label"
    plot_data$label <- factor(
      plot_data$label,
      levels = rev(as.character(plot_data$label))
    )
    plot_data$hover <- paste0(
      as.character(plot_data$label),
      "<br>Selections: ", plot_data$Count,
      "<br>Percent of respondents: ", plot_data$Percent
    )
    plot <- ggplot(plot_data, aes(x = label, y = Count, text = hover)) +
      geom_col(fill = "#9A3270", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Selections") +
      theme_minimal(base_size = 11)
  } else {
    chart_layout <- "matrix"
    response_order <- ordered_response_levels(
      all_values,
      detected_type$kind,
      response_metadata_levels
    )
    order_source <- paste(column_order$source, response_order$source, sep = "; ")
    rows <- lapply(seq_along(analysis_columns), function(j) {
      raw <- trimws(as.character(tf[[analysis_columns[[j]]]]))
      raw <- raw[nonempty(raw)]
      if (!length(raw)) return(NULL)
      counts <- table(factor(raw, levels = response_order$levels))
      data.frame(
        Item = metadata_label(
          meta,
          analysis_columns[[j]],
          analysis_columns[[j]]
        ),
        Response = names(counts),
        Count = as.integer(counts),
        stringsAsFactors = FALSE
      )
    })
    result <- do.call(rbind, rows)
    if (is.null(result)) {
      result <- data.frame(Item = character(), Response = character(), Count = integer())
    }
    totals <- ave(result$Count, result$Item, FUN = sum)
    result$Percent <- if (length(totals)) sprintf("%.1f%%", 100 * result$Count / totals) else character()
    answered_rows <- row_has_response(
      tf[, analysis_columns, drop = FALSE]
    ) | other_answered_rows
    answered_n <- sum(answered_rows)
    display <- head(result, 100L)
    truncated_note <- if (nrow(result) > nrow(display)) {
      paste0("Only the first 100 of ", nrow(result), " item-response rows are shown.")
    } else {
      NULL
    }
    item_levels <- unique(as.character(result$Item))
    chart_row_count <- length(item_levels)
    chart_legend_count <- length(response_order$levels)
    result$Item <- factor(result$Item, levels = rev(item_levels))
    result$Response <- factor(result$Response, levels = response_order$levels)
    result$hover <- paste0(
      as.character(result$Item),
      "<br>", as.character(result$Response),
      "<br>Count: ", result$Count,
      "<br>Share: ", result$Percent
    )
    semantic_colors <- semantic_response_palette(
      response_order$levels,
      detected_type$kind
    )
    plot <- ggplot(
      result,
      aes(x = Item, y = Count, fill = Response, text = hover)
    ) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Share", fill = "Response") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom")
    if (!is.null(semantic_colors)) {
      plot <- plot + scale_fill_manual(values = semantic_colors)
    }
  }
  question_order_sources <- c(question_order_sources, order_source)

  file_stem <- sprintf("%03d-%s", i, safe_filename(stem))
  figure_rel <- file.path("figures", paste0(file_stem, ".png"))
  figure_path <- file.path(project_dir, figure_rel)
  chart_height <- chart_height_pixels(
    chart_layout,
    chart_row_count,
    chart_legend_count
  )
  if (!is_count_only_group && nrow(display)) {
    ggsave(
      figure_path,
      plot,
      width = 9,
      height = chart_height / 100,
      dpi = 144
    )
  }

  chart_lines <- if (interactive_charts && !is_count_only_group && nrow(display)) {
    widget_path <- file.path(
      project_dir,
      "figures",
      paste0(file_stem, ".html")
    )
    interactive_chart_html(
      plot,
      widget_path,
      figure_rel,
      stem,
      chart_height,
      chart_summary
    )
  } else {
    paste0(
      "![Response distribution](../", figure_rel,
      "){fig-alt='Response distribution for ", stem, ".'}"
    )
  }

  chapter_rel <- file.path("questions", paste0(file_stem, ".qmd"))
  chapter_lines <- c(
    "---",
    paste0("title: ", yaml_string(stem)),
    "---",
    "",
    paste0("# ", markdown_text(title)),
    "",
    paste0("**Question code:** `", stem, "`"),
    "",
    paste0("**Detected type:** ", type),
    "",
    if (order_source != "not applicable") {
      c(paste0("**Answer order:** ", order_source), "")
    } else {
      character()
    }
  )
  if (is_count_only_group) {
    chapter_lines <- c(
      chapter_lines,
      free_text_status_lines(answered_rows),
      ""
    )
    if (include_free_text && is_free_text_group) {
      chapter_lines <- c(
        chapter_lines,
        free_text_details(all_values)
      )
    }
  } else if (nrow(display)) {
    chapter_lines <- c(
      chapter_lines,
      answering_summary(answered_rows),
      "",
      chart_lines,
      ""
    )
    if (length(other_values)) {
      chapter_lines <- c(
        chapter_lines,
        paste0("Other free-text answers provided: ", length(other_values)),
        ""
      )
      if (include_free_text) {
        chapter_lines <- c(
          chapter_lines,
          free_text_details(other_values, "Other free-text answers")
        )
      }
    }
    if (!interactive_charts) {
      chapter_lines <- c(chapter_lines, write_markdown_table(display), "")
    }
  } else {
    chapter_lines <- c(
      chapter_lines,
      answering_summary(answered_rows),
      ""
    )
    if (length(other_values)) {
      chapter_lines <- c(
        chapter_lines,
        paste0("Other free-text answers provided: ", length(other_values)),
        ""
      )
      if (include_free_text) {
        chapter_lines <- c(
          chapter_lines,
          free_text_details(other_values, "Other free-text answers")
        )
      }
    } else {
      chapter_lines <- c(
        chapter_lines,
        "_No structured responses were recorded for this question._",
        ""
      )
    }
  }
  if (!is.null(truncated_note)) {
    chapter_lines <- c(chapter_lines, paste0("_", truncated_note, "_"), "")
  }
  writeLines(chapter_lines, file.path(project_dir, chapter_rel))
  chapter_files <- c(chapter_files, chapter_rel)
}

quarto_lines <- c(
  "project:",
  "  type: book",
  "  output-dir: _book"
)
if (interactive_charts) {
  quarto_lines <- c(
    quarto_lines,
    "  resources:",
    "    - figures/widget-lib/**"
  )
}
quarto_lines <- c(
  quarto_lines,
  "book:",
  paste0("  title: ", yaml_string(book_title)),
  "  chapters:",
  "    - index.qmd"
)
for (section in present_sections) {
  section_chapters <- chapter_files[question_sections == section]
  quarto_lines <- c(
    quarto_lines,
    paste0("    - part: ", yaml_string(section)),
    "      chapters:",
    paste0("        - ", section_chapters)
  )
}
navigation <- data.frame(
  Section = question_sections,
  SectionOrder = match(question_sections, section_order),
  QuestionOrder = seq_along(chapter_files),
  QuestionCode = ordered_stems,
  Chapter = chapter_files,
  Title = vapply(ordered_stems, function(stem) question_text(meta, stem), character(1)),
  DetectedType = question_types,
  AnswerOrder = question_order_sources,
  stringsAsFactors = FALSE
)
write.csv(
  navigation,
  file.path(project_dir, "navigation.csv"),
  row.names = FALSE,
  na = ""
)
quarto_lines <- c(
  quarto_lines,
  "format:",
  "  html:",
  "    theme: cosmo",
  "    toc: false"
)
writeLines(quarto_lines, file.path(project_dir, "_quarto.yml"))

message(
  "Generated ", survey_year, " book for ", country_label, " with ",
  length(chapter_files),
  " question chapters in ", length(present_sections), " thematic sections at ",
  project_dir
)
if (length(unknown_families)) {
  warning(
    "Unrecognised question families placed in Other questions: ",
    paste(unknown_families, collapse = ", "),
    call. = FALSE
  )
}
