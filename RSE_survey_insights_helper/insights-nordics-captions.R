# Sample sizes and figure captions for Nordic insights.

insights_with_n <- function(text, n) {
  if (is.null(text) || !nzchar(text)) {
    return(sprintf("N = %d", n))
  }
  if (grepl("\\([nN]\\s*=", text)) {
    return(text)
  }
  paste0(text, " (N = ", n, ")")
}

insights_likert_n <- function(df, column_name) {
  sum(!is.na(df[[column_name]]) & nzchar(as.character(df[[column_name]])))
}

insights_fig_cap_multi <- function(text, labels) {
  if (length(labels) == 0L) {
    return(text)
  }
  paste0(text, " (", paste(labels, collapse = "; "), ")")
}

insights_panel_title <- function(title, n) {
  sprintf("%s (N = %d)", title, n)
}

#' Figure caption for the Generative AI panel with sample-size allocation note
#'
#' @param filter Country filter vector.
#' @param ns Named sample sizes from [insights_question_ns()].
#' @return Character figure caption.
#' @export
insights_genai_fig_cap <- function(filter, ns) {
  tf <- load_filtered_tf(filter)
  cols_genai2 <- question_columns("genAI2", names(tf))

  has_usage <- !is.na(tf$genAI1_0) & nzchar(as.character(tf$genAI1_0))
  never <- has_usage & tf$genAI1_0 == "Never"

  genai2 <- as.matrix(tf[, cols_genai2, drop = FALSE])
  answered_genai2 <- rowSums(genai2 == "True", na.rm = TRUE) > 0L |
    rowSums(
      !is.na(genai2) & genai2 != "False" & nzchar(genai2),
      na.rm = TRUE
    ) > 0L
  non_never_no_purpose <- has_usage & !never & !answered_genai2

  allocation_note <- sprintf(
    paste0(
      "Purposes and tools N count only respondents with at least one selection. ",
      "Among %d usage-frequency responses, %d answered Never (no purposes/tools recorded)%s."
    ),
    sum(has_usage),
    sum(never),
    if (sum(non_never_no_purpose) > 0L) {
      sprintf(
        " and %d selected no purpose despite non-never usage",
        sum(non_never_no_purpose)
      )
    } else {
      ""
    }
  )

  insights_fig_cap_multi(
    paste0(
      "Generative AI usage frequency (single-select), use purposes and tools ",
      "selected at work (multi-select). ",
      allocation_note
    ),
    c(
      sprintf("AI usage frequency: N = %d", ns$genAI1_0),
      sprintf("AI purposes: N = %d", ns$genAI2),
      sprintf("AI tools: N = %d", ns$genAI3)
    )
  )
}

#' Figure caption for the software quality and citation panel
#'
#' @param ns Named sample sizes from [insights_question_ns()].
#' @return Character figure caption.
#' @export
insights_citation_fig_cap <- function(ns) {
  insights_fig_cap_multi(
    paste0(
      "Software testing (multi-select) and software citation practices ",
      "(frequency Likert). Citation questions were not answered by all ",
      "respondents, so N is lower than for software testing."
    ),
    c(
      sprintf("reference directly: N = %d", ns$likert2a_ref_direct),
      sprintf("reference via paper: N = %d", ns$likert2a_ref_paper),
      sprintf("generating DOIs: N = %d", ns$likert2b_doi),
      sprintf("software testing: N = %d", ns$proj4can)
    )
  )
}

#' Sample sizes for every survey question used in the Nordic insights document
#'
#' @param filter Country filter vector.
#' @param meta Survey column metadata from [read.csv()] on `2026_cols.csv`.
#' @param df Respondent data frame (defaults to [load_filtered_tf()]).
#' @return Named list of integer sample sizes.
#' @export
insights_question_ns <- function(
    filter,
    meta,
    df = NULL
) {
  if (is.null(df)) {
    df <- load_filtered_tf(filter)
  }

  multi <- c(
    "currentEmp13", "edu2_0", "currentEmp10_0", "currentEmp5_0",
    "tool4can", "proj6zaf", "tool5", "genAI1_0", "genAI2", "genAI3", "proj4can",
    "ukrse3", "train5", "currentWork3nord_0", "fund3", "org3nord", "org4nord",
    "skill2"
  )
  multi_standard <- setdiff(multi, "currentEmp5_0")
  ns <- as.list(stats::setNames(
    c(
      vapply(multi_standard, question_n_respondents, integer(1), filter = filter),
      current_emp5_n_respondents(filter)
    ),
    c(multi_standard, "currentEmp5_0")
  ))

  likert_cols <- c(
    likert2a_ref_direct = "likert2a[1]_0",
    likert2a_ref_paper = "likert2a[2]_0",
    likert2b_doi = "likert2b[SQ001]_0",
    likert3a_job = "likert3a[SQ001]_0",
    likert3b_career = "likert3b[SQ001]_0",
    likert5b_demand = "likert5b[1]_0",
    likert5a_promotion = "likert5a[2]_0",
    likert5b_process = "likert5b[2]_0",
    likert5a_opportunities = "likert5a[3]_0",
    currentWork2_group = "currentWork2_0"
  )
  ns[names(likert_cols)] <- vapply(
    likert_cols,
    insights_likert_n,
    integer(1),
    df = df
  )

  ns$total <- dplyr::n_distinct(df$row_id)
  ns$allocation <- likert_allocation_n_respondents(
    df,
    filter,
    "likert0",
    meta
  )
  ns
}
