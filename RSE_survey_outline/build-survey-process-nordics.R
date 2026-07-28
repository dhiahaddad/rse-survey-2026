#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  source("rse-book/R/_load_common.R")
  library(dplyr)
  library(stringr)
})

nordic <- FILTER
tf <- load_filtered_tf(nordic)
tf_all <- read.csv(file.path(survey_data_dir, "2026_tf.csv"), check.names = FALSE)
empty <- !nzchar(names(tf_all)) | is.na(names(tf_all))
names(tf_all)[empty] <- paste0(".blank", seq_len(sum(empty)))
all_names <- names(tf_all)

question_stem <- function(x) {
  x <- sub("_0$", "", x)
  x <- sub("\\[.*\\]$", "", x)
  x
}

sys <- c(
  "id", "submitdate_0", "lastpage", "startlanguage", "seed", "token",
  "startdate", "datestamp", "ipaddr", "refurl", "email", "emailstatus",
  "sent", "remindersent", "remindercount", "lastreminder", "usesleft",
  "completed", "language", "row_id", "Year_0"
)
all_cols <- setdiff(all_names[!grepl("^\\.blank", all_names)], sys)
ordered_stems <- unique(question_stem(all_cols))

meta <- read.csv(
  file.path(survey_data_dir, "2026_all_cols.csv"),
  check.names = FALSE
)
meta2 <- meta |>
  mutate(
    stem = question_stem(New_name),
    q_text = clean_question_text(Question)
  )
text_map <- meta2 |>
  group_by(stem) |>
  summarise(text = first(na.omit(q_text[q_text != ""])), .groups = "drop")
type_map <- meta2 |>
  group_by(stem) |>
  summarise(
    type = if (any(grepl("^likert", stem, ignore.case = TRUE))) {
      "Likert"
    } else if (any(nzchar(Option))) {
      "Multi-select"
    } else {
      "Single / free-text"
    },
    .groups = "drop"
  )

shown_to_nordics <- function(s) {
  !grepl(
    paste0(
      "(^currentEmp1qde$|^currentEmp.*deqde$|^prevEmp1qde$|",
      "^org[0-9]*de$|^turnOver.*zaf$|^ref1uk$|^proj6uk$|^fund1uk$|",
      "^train4qzaf$|^fund1can$|^tool5can$|^org1cz$|^org1swiss$|",
      "^org[345]us$|^ukrse12sa$|^currentEmp1qzaf$|^currentEmp11qzaf$|",
      "^currentEmp1qswiss$|^prevEmp1qswiss$|^fund3qnl$|^currentEmp1nl$|",
      "^prevEmp1nl$|^currentEmp11qcl$)"
    ),
    s
  )
}

row_answered <- function(row) {
  vals <- as.character(row)
  any(
    vals == "True" |
      (!is.na(vals) & vals != "False" & nzchar(trimws(vals))),
    na.rm = TRUE
  )
}

stem_n <- function(st) {
  cols <- all_cols[question_stem(all_cols) == st]
  if (!length(cols)) {
    return(NA_integer_)
  }
  if (length(cols) == 1L && identical(cols[[1L]], paste0(st, "_0"))) {
    col <- cols[[1L]]
    return(sum(!is.na(tf[[col]]) & nzchar(as.character(tf[[col]]))))
  }
  sum(apply(tf[, cols, drop = FALSE], 1, row_answered))
}

section_of <- function(s) {
  if (s %in% c("startlanguage", "startdate")) return("Survey metadata")
  if (s == "socio1") return("Screening")
  if (grepl("^rse", s)) return("RSE role")
  if (grepl("^socio", s)) return("Demographics")
  if (grepl("^edu", s)) return("Education")
  if (grepl("^currentEmp|^prevEmp", s)) return("Employment")
  if (grepl("^currentWork", s)) return("Work activities")
  if (grepl("^likert", s)) return("Attitudes (Likert)")
  if (grepl("^conf", s)) return("Conferences")
  if (grepl("^org", s)) return("RSE organisation")
  if (grepl("^proj", s)) return("Project management")
  if (grepl("^tool", s)) return("Tooling")
  if (grepl("^train|^skill", s)) return("Training & skills")
  if (grepl("^fund", s)) return("Funding")
  if (grepl("^genAI", s)) return("Generative AI")
  if (grepl("^turnOver", s)) return("Turnover")
  if (grepl("^ukrse", s)) return("UK RSE network")
  if (grepl("^soft", s)) return("Software experience")
  if (grepl("^paper", s)) return("Publications")
  if (grepl("^open", s)) return("Open science")
  if (grepl("^ref", s)) return("Referencing")
  if (grepl("^stability", s)) return("Job stability")
  "Other"
}

filter_note <- function(st, n_val) {
  notes <- list(
    socio1 = "Routes respondents to Nordic-specific blocks (`org3nord`, `org4nord`, `currentWork2qcl`, `currentWork3nord`, `fund1nord`, `skillNord`) and away from other regional variants.",
    currentEmp1 = "Nordics see `currentEmp1` (not `currentEmp1qde`, `currentEmp1nl`, etc.).",
    prevEmp1 = "Nordics see `prevEmp1` (not `prevEmp1qde`, `prevEmp1nl`, etc.).",
    org3nord = "Shown when `socio1_0` is a Nordic country.",
    org4nord = "Shown when `socio1_0` is a Nordic country.",
    currentWork2qcl = "Shown to Nordics (and Chile via QCL path). Replaces generic audience question for this group.",
    currentWork3nord = "Shown when `socio1_0` is a Nordic country.",
    fund1nord = "Shown when `socio1_0` is a Nordic country.",
    skillNord = "Shown when `socio1_0` is a Nordic country.",
    org1can = "Global question; `org2can` follows if respondent is interested or an association member.",
    org2can = "Follow-up when `org1can = Yes` or respondent is an RSE association member (`ukrse1`).",
    conf1can = "Global question; `conf2can` follows when answer is Yes.",
    conf2can = "Follow-up when `conf1can = Yes`.",
    currentEmp6 = "If Yes, respondent is asked `currentEmp60` for alternate job title.",
    currentEmp60 = "Follow-up when `currentEmp6 = Yes`.",
    genAI1 = "If answer is not Never, `genAI2` and `genAI3` are shown.",
    genAI2 = "Follow-up when `genAI1 ≠ Never`.",
    genAI3 = "Follow-up when `genAI1 ≠ Never`.",
    train4 = "If Yes, respondent is asked `train5` about teaching contribution.",
    train5 = "Follow-up when `train4 = Yes`."
  )
  if (!is.null(notes[[st]])) {
    return(notes[[st]])
  }
  if (grepl("nord$|skillNord|fund1nord|currentWork3nord", st)) {
    return("Nordic-only question.")
  }
  if (grepl("qcl$", st)) {
    return("Shown to Nordics and Chile (QCL path).")
  }
  "—"
}

qdf <- tibble(order = seq_along(ordered_stems), stem = ordered_stems) |>
  filter(stem != "") |>
  left_join(text_map, by = "stem") |>
  left_join(type_map, by = "stem") |>
  mutate(
    section = vapply(stem, section_of, character(1)),
    shown = vapply(stem, shown_to_nordics, logical(1)),
    n = vapply(stem, stem_n, integer(1)),
    filtering = vapply(seq_len(n()), function(i) filter_note(stem[i], n[i]), character(1))
  ) |>
  filter(shown, n > 0) |>
  arrange(order)

by_country <- sort(table(tf$socio1_0), decreasing = TRUE)
n_total <- nrow(tf)
n_partial <- survey_unsubmitted_n(nordic)

n_conf1_yes <- sum(tf$conf1can_0 %in% c("True", TRUE), na.rm = TRUE)
n_org1_yes <- sum(tf$org1can_0 %in% c("True", TRUE), na.rm = TRUE)
n_emp6_yes <- sum(tf$currentEmp6_0 %in% c("True", TRUE), na.rm = TRUE)
n_train4_yes <- sum(tf$train4_0 %in% c("True", TRUE), na.rm = TRUE)
n_genai_not_never <- sum(tf$genAI1_0 != "Never" & !is.na(tf$genAI1_0) & nzchar(tf$genAI1_0), na.rm = TRUE)

md <- c(
  "# International RSE Survey 2026 — Nordic Survey Process",
  "",
  "Questions **shown to Nordic respondents** with at least one answer (**N > 0**). Includes routing and filtering rules where they apply.",
  "",
  "Countries in scope match `FILTER` in `rse-book/R/.config`:",
  "",
  "```r",
  paste0("FILTER = c(", paste0("\"", nordic, "\"", collapse = ", "), ")"),
  "```",
  "",
  "Derived from `RSE_survey_2026_data/2026_tf.csv` and `2026_all_cols.csv`.",
  "",
  "---",
  "",
  "## Analysis filtering (all Nordic questions)",
  "",
  "Before question-level **N** is computed, every Nordic analysis applies:",
  "",
  "| Step | Field | Rule | Result |",
  "|------|-------|------|--------|",
  sprintf("| 1. Submission | `submitdate_0` | Non-empty | **%d** submitted |", n_total),
  sprintf("| | | Partial sessions excluded | **%d** excluded |", n_partial),
  sprintf("| 2. Country | `socio1_0` | In `FILTER` | **%d** Nordic respondents |", n_total),
  "| 3. Question | per question | At least one non-empty answer | **N** in tables below |",
  "",
  "### Submitted Nordic sample by country",
  "",
  "| Country | n |",
  "|---------|---|",
  vapply(
    names(by_country),
    function(nm) sprintf("| %s | %d |", nm, by_country[[nm]]),
    character(1)
  ),
  sprintf("| **Total** | **%d** |", n_total),
  "",
  "**N** counts submitted Nordic respondents who answered that question. Multi-select: any option selected; Likert: any sub-item answered; single Yes/No: includes False.",
  "",
  "---",
  "",
  "## Survey routing for Nordic respondents",
  "",
  "At **`socio1`** (country), Nordic respondents enter the Nordic path and **do not see** Germany/UK/US/Netherlands/Switzerland/South Africa-specific variants (e.g. `currentEmp1qde`, `org3de`, `ref1uk`, `turnOver3zaf`).",
  "",
  "Conditional follow-ups observed in Nordic data:",
  "",
  "| Trigger | Follow-up | Eligible (N) | Answered (N) |",
  "|---------|-----------|--------------|--------------|",
  sprintf(
    "| `socio1_0` ∈ Nordics | `org3nord`, `org4nord`, `currentWork3nord`, `fund1nord`, `skillNord` | %d | 58–60 |",
    n_total
  ),
  sprintf(
    "| `conf1can = Yes` | `conf2can` | %d | %d |",
    n_conf1_yes,
    qdf$n[qdf$stem == "conf2can"]
  ),
  sprintf(
    "| `org1can = Yes` or RSE association member | `org2can` | — | %d |",
    qdf$n[qdf$stem == "org2can"]
  ),
  sprintf(
    "| `currentEmp6 = Yes` | `currentEmp60` | %d | %d |",
    n_emp6_yes,
    qdf$n[qdf$stem == "currentEmp60"]
  ),
  sprintf(
    "| `genAI1 ≠ Never` | `genAI2`, `genAI3` | %d | %d / %d |",
    n_genai_not_never,
    qdf$n[qdf$stem == "genAI2"],
    qdf$n[qdf$stem == "genAI3"]
  ),
  sprintf(
    "| `train4 = Yes` | `train5` | %d | %d |",
    n_train4_yes,
    qdf$n[qdf$stem == "train5"]
  ),
  "",
  sprintf(
    "This document lists **%d** question groups shown to Nordics with N > 0 (of 110 total in the survey).",
    nrow(qdf)
  ),
  "",
  "---",
  "",
  "## Questions shown to Nordics (N > 0)",
  ""
)

sections <- unique(qdf$section)
for (sec in sections) {
  sub <- qdf |> filter(section == sec) |> arrange(order)
  md <- c(
    md,
    sprintf("### %s", sec),
    "",
    "| # | Code | Type | N | Filtering / routing | Question |",
    "|---|------|------|---|---------------------|----------|"
  )
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    md <- c(
      md,
      sprintf(
        "| %d | `%s` | %s | %d | %s | %s |",
        r$order,
        r$stem,
        r$type,
        r$n,
        r$filtering,
        r$text
      )
    )
  }
  md <- c(md, "")
}

md <- c(
  md,
  "---",
  "",
  "*See also [`survey-process.md`](survey-process.md) for the full global survey flow.*",
  ""
)

out_path <- file.path("RSE_survey_outline", "survey-process-nordics.md")
writeLines(md, out_path)
message("Wrote ", out_path, " (", nrow(qdf), " questions, ", length(md), " lines)")
