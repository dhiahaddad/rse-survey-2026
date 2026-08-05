#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
include_free_text <- any(args == "--include-free-text")
if (sum(args == "--include-free-text") > 1L) {
  stop("Specify --include-free-text only once.", call. = FALSE)
}
args <- args[args != "--include-free-text"]
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
      "[--inclusion all|submitted] [--include-free-text]"
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

probable_free_text <- function(values) {
  flat <- trimws(unlist(values, use.names = FALSE))
  flat <- flat[nonempty(flat)]
  if (length(flat) < 10L) return(FALSE)
  distinct <- length(unique(flat))
  distinct >= 10L &&
    distinct / length(flat) >= 0.25 &&
    mean(nchar(flat)) >= 12
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

tf_source <- read_survey_csv(tf_path)
meta <- read_survey_csv(cols_path)
if (!"Option" %in% names(meta)) {
  meta$Option <- ""
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
  "Year_0"
)
response_columns <- setdiff(
  names(tf)[!startsWith(names(tf), ".index")],
  system_columns
)
ordered_stems <- unique(question_stem(response_columns))
metadata_stems <- unique(question_stem(meta$New_name[nonempty(meta$New_name)]))
ordered_stems <- ordered_stems[ordered_stems %in% metadata_stems]
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

country_column <- if ("socio1_0" %in% names(tf)) "socio1_0" else NULL
country_summary <- if (!is.null(country_column)) {
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
  paste0("**Source records:** ", nrow(tf_source)),
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

for (i in seq_along(ordered_stems)) {
  stem <- ordered_stems[[i]]
  columns <- response_columns[question_stem(response_columns) == stem]
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
  all_values <- trimws(unlist(values, use.names = FALSE))
  all_values <- all_values[nonempty(all_values)]
  boolean_values <- tolower(unique(all_values))
  is_boolean_grid <- length(analysis_columns) > 1L &&
    length(boolean_values) > 0L &&
    all(boolean_values %in% c("true", "false", "0", "1"))
  is_free_text_group <- !length(other_columns) && (
    probable_free_text(values) ||
      (length(analysis_columns) > 1L && length(unique(all_values)) > 30L)
  )

  if (is_free_text_group) {
    type <- "Free text"
    answered_rows <- row_has_response(tf[, analysis_columns, drop = FALSE])
    display <- data.frame()
    truncated_note <- NULL
    plot <- NULL
  } else if (length(analysis_columns) == 1L) {
    type <- "Single response"
    raw_all <- trimws(as.character(tf[[analysis_columns[[1L]]]]))
    answered_rows <- nonempty(raw_all) | other_answered_rows
    raw <- raw_all[nonempty(raw_all)]
    counts <- sort(table(raw), decreasing = TRUE)
    answered_n <- sum(answered_rows)
    denominator <- if (answered_n > 0L) answered_n else 1L
    result <- data.frame(
      Response = names(counts),
      Count = as.integer(counts),
      Percent = sprintf("%.1f%%", 100 * as.integer(counts) / denominator)
    )
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
    answered_rows <- apply(tf[, analysis_columns, drop = FALSE], 1L, function(row) {
      any(tolower(trimws(as.character(row))) %in% c("true", "1"), na.rm = TRUE)
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
  } else {
    type <- "Question grid"
    rows <- lapply(seq_along(analysis_columns), function(j) {
      raw <- trimws(as.character(tf[[analysis_columns[[j]]]]))
      raw <- raw[nonempty(raw)]
      if (!length(raw)) return(NULL)
      counts <- sort(table(raw), decreasing = TRUE)
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
  if (!is_free_text_group && nrow(display)) {
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
    ""
  )
  if (is_free_text_group) {
    chapter_lines <- c(
      chapter_lines,
      free_text_status_lines(answered_rows),
      ""
    )
    if (include_free_text) {
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
      paste0("![Response distribution](../", figure_rel, "){fig-alt='Response distribution for ", stem, ".'}"),
      ""
    )
    if (length(other_values)) {
      chapter_lines <- c(
        chapter_lines,
        paste0("Other responses recorded: ", length(other_values)),
        ""
      )
      if (include_free_text) {
        chapter_lines <- c(
          chapter_lines,
          free_text_details(other_values, "Other responses")
        )
      }
    }
    chapter_lines <- c(chapter_lines, write_markdown_table(display), "")
  } else {
    chapter_lines <- c(
      chapter_lines,
      answering_summary(answered_rows),
      ""
    )
    if (length(other_values)) {
      chapter_lines <- c(
        chapter_lines,
        paste0("Other responses recorded: ", length(other_values)),
        ""
      )
      if (include_free_text) {
        chapter_lines <- c(
          chapter_lines,
          free_text_details(other_values, "Other responses")
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
  "  output-dir: _book",
  "book:",
  paste0("  title: ", yaml_string(paste("International RSE Survey", survey_year))),
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
quarto_lines <- c(
  quarto_lines,
  "format:",
  "  html:",
  "    theme: cosmo",
  "    toc: true"
)
writeLines(quarto_lines, file.path(project_dir, "_quarto.yml"))

message(
  "Generated ", survey_year, " book with ", length(chapter_files),
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
