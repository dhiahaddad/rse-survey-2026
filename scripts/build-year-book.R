#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop(
    "Usage: Rscript scripts/build-year-book.R YEAR DATA_ROOT [OUTPUT_ROOT]",
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

question_text <- function(meta, stem) {
  hit <- meta[question_stem(meta$New_name) == stem, "Question", drop = TRUE]
  hit <- unique(trimws(as.character(hit[nonempty(hit)])))
  if (!length(hit)) return(stem)
  trimws(sub("\\s*\\[[^]]+\\]\\s*$", "", hit[[1L]]))
}

safe_filename <- function(x) {
  value <- gsub("[^A-Za-z0-9_-]+", "-", x)
  value <- gsub("^-+|-+$", "", value)
  if (nzchar(value)) value else "question"
}

tf <- read_survey_csv(tf_path)
meta <- read_survey_csv(cols_path)
if (!"Option" %in% names(meta)) {
  meta$Option <- ""
}

if ("submitdate_0" %in% names(tf)) {
  submitted <- nonempty(tf$submitdate_0)
  tf <- tf[submitted, , drop = FALSE]
  submission_note <- "Rows with a non-empty `submitdate_0` are included."
} else {
  submission_note <- paste0(
    "This export has no `submitdate_0` field, so all ", nrow(tf),
    " rows are included."
  )
}

system_columns <- c(
  "id", "submitdate_0", "lastpage", "lastpage_0", "startlanguage",
  "startlanguage_0", "seed", "seed_0", "token", "startdate",
  "startdate_0", "datestamp", "ipaddr", "refurl", "email",
  "emailstatus", "sent", "remindersent", "remindercount",
  "lastreminder", "usesleft", "completed", "language", "row_id",
  "Year_0"
)
response_columns <- setdiff(
  names(tf)[!startsWith(names(tf), ".index")],
  system_columns
)
ordered_stems <- unique(question_stem(response_columns))
metadata_stems <- unique(question_stem(meta$New_name[nonempty(meta$New_name)]))
ordered_stems <- ordered_stems[ordered_stems %in% metadata_stems]

country_column <- if ("socio1_0" %in% names(tf)) "socio1_0" else NULL
country_summary <- if (!is.null(country_column)) {
  countries <- trimws(as.character(tf[[country_column]]))
  countries <- countries[nonempty(countries)]
  counts <- sort(table(countries), decreasing = TRUE)
  data.frame(Country = names(counts), Responses = as.integer(counts))
} else {
  data.frame()
}

index_lines <- c(
  "---",
  paste0("title: ", yaml_string(paste("International RSE Survey", survey_year))),
  "---",
  "",
  paste0("# Survey overview {.unnumbered}"),
  "",
  paste0(
    "This book presents the **", survey_year,
    " survey independently**, using the questions and answer options stored in that year's export."
  ),
  "",
  paste0("**Respondents included:** ", nrow(tf)),
  "",
  paste0("**Question groups discovered:** ", length(ordered_stems)),
  "",
  submission_note,
  "",
  "No cross-year harmonisation is applied.",
  ""
)
if (nrow(country_summary)) {
  index_lines <- c(
    index_lines,
    "## Responses by country",
    "",
    write_markdown_table(country_summary),
    ""
  )
}
writeLines(index_lines, file.path(project_dir, "index.qmd"))

chapter_files <- character()

for (i in seq_along(ordered_stems)) {
  stem <- ordered_stems[[i]]
  columns <- response_columns[question_stem(response_columns) == stem]
  title <- question_text(meta, stem)
  values <- lapply(tf[, columns, drop = FALSE], as.character)
  all_values <- trimws(unlist(values, use.names = FALSE))
  all_values <- all_values[nonempty(all_values)]
  boolean_values <- tolower(unique(all_values))
  is_boolean_grid <- length(columns) > 1L &&
    length(boolean_values) > 0L &&
    all(boolean_values %in% c("true", "false", "0", "1"))

  if (length(columns) == 1L) {
    type <- "Single response or free text"
    raw <- trimws(as.character(tf[[columns[[1L]]]]))
    raw <- raw[nonempty(raw)]
    counts <- sort(table(raw), decreasing = TRUE)
    result <- data.frame(
      Response = names(counts),
      Count = as.integer(counts),
      Percent = sprintf("%.1f%%", 100 * as.integer(counts) / sum(counts))
    )
    answered_n <- length(raw)
    display <- head(result, 30L)
    truncated_note <- if (nrow(result) > nrow(display)) {
      paste0("Only the 30 most frequent of ", nrow(result), " distinct responses are shown.")
    } else {
      NULL
    }
    plot_data <- display
    names(plot_data)[1L] <- "label"
    plot <- ggplot(plot_data, aes(x = reorder(label, Count), y = Count)) +
      geom_col(fill = "#2C7FB8") +
      coord_flip() +
      labs(x = NULL, y = "Responses") +
      theme_minimal(base_size = 11)
  } else if (is_boolean_grid) {
    type <- "Multiple response"
    selected <- vapply(values, function(x) {
      sum(tolower(trimws(x)) %in% c("true", "1"), na.rm = TRUE)
    }, integer(1))
    answered_rows <- apply(tf[, columns, drop = FALSE], 1L, function(row) {
      any(tolower(trimws(as.character(row))) %in% c("true", "1"), na.rm = TRUE)
    })
    answered_n <- sum(answered_rows)
    labels <- vapply(seq_along(columns), function(j) {
      metadata_label(meta, columns[[j]], columns[[j]])
    }, character(1))
    denominator <- if (answered_n > 0L) answered_n else 1L
    result <- data.frame(
      Option = labels,
      Count = selected,
      Percent = sprintf("%.1f%%", 100 * selected / denominator)
    )
    result <- result[order(result$Count, decreasing = TRUE), , drop = FALSE]
    display <- result
    truncated_note <- NULL
    plot_data <- result
    names(plot_data)[1L] <- "label"
    plot <- ggplot(plot_data, aes(x = reorder(label, Count), y = Count)) +
      geom_col(fill = "#2C7FB8") +
      coord_flip() +
      labs(x = NULL, y = "Selections") +
      theme_minimal(base_size = 11)
  } else if (length(unique(all_values)) > 30L) {
    type <- "Repeated free text"
    counts <- sort(table(all_values), decreasing = TRUE)
    result <- data.frame(
      Response = names(counts),
      Count = as.integer(counts),
      Percent = sprintf("%.1f%%", 100 * as.integer(counts) / sum(counts))
    )
    answered_rows <- apply(tf[, columns, drop = FALSE], 1L, function(row) {
      any(nonempty(row))
    })
    answered_n <- sum(answered_rows)
    display <- head(result, 30L)
    truncated_note <- if (nrow(result) > nrow(display)) {
      paste0("Only the 30 most frequent of ", nrow(result), " distinct responses are shown.")
    } else {
      NULL
    }
    plot_data <- display
    names(plot_data)[1L] <- "label"
    plot <- ggplot(plot_data, aes(x = reorder(label, Count), y = Count)) +
      geom_col(fill = "#2C7FB8") +
      coord_flip() +
      labs(x = NULL, y = "Responses") +
      theme_minimal(base_size = 11)
  } else {
    type <- "Question grid"
    rows <- lapply(seq_along(columns), function(j) {
      raw <- trimws(as.character(tf[[columns[[j]]]]))
      raw <- raw[nonempty(raw)]
      if (!length(raw)) return(NULL)
      counts <- sort(table(raw), decreasing = TRUE)
      data.frame(
        Item = metadata_label(meta, columns[[j]], columns[[j]]),
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
    answered_rows <- apply(tf[, columns, drop = FALSE], 1L, function(row) {
      any(nonempty(row))
    })
    answered_n <- sum(answered_rows)
    display <- head(result[order(result$Item, -result$Count), , drop = FALSE], 100L)
    truncated_note <- if (nrow(result) > nrow(display)) {
      paste0("Only the first 100 of ", nrow(result), " item-response rows are shown.")
    } else {
      NULL
    }
    plot <- ggplot(result, aes(x = Item, y = Count, fill = Response)) +
      geom_col(position = "fill") +
      coord_flip() +
      labs(x = NULL, y = "Share", fill = "Response") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom")
  }

  file_stem <- sprintf("%03d-%s", i, safe_filename(stem))
  figure_rel <- file.path("figures", paste0(file_stem, ".png"))
  figure_path <- file.path(project_dir, figure_rel)
  if (nrow(display)) {
    ggsave(figure_path, plot, width = 9, height = max(4.5, min(12, 2.5 + 0.22 * nrow(display))), dpi = 144)
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
    paste0("**Respondents answering:** ", answered_n),
    ""
  )
  if (nrow(display)) {
    chapter_lines <- c(
      chapter_lines,
      paste0("![Response distribution](../", figure_rel, "){fig-alt='Response distribution for ", stem, ".'}"),
      "",
      write_markdown_table(display),
      ""
    )
  } else {
    chapter_lines <- c(chapter_lines, "_No responses were recorded for this question._", "")
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
  "  output-dir: _book",
  "book:",
  paste0("  title: ", yaml_string(paste("International RSE Survey", survey_year))),
  "  chapters:",
  "    - index.qmd",
  "    - part: Questions",
  "      chapters:",
  paste0("        - ", chapter_files),
  "format:",
  "  html:",
  "    theme: cosmo",
  "    toc: true"
)
writeLines(quarto_lines, file.path(project_dir, "_quarto.yml"))

message(
  "Generated ", survey_year, " book with ", length(chapter_files),
  " question chapters at ", project_dir
)
