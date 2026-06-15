library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

DATA_DIR <- if (file.exists("RSE_survey_2026_data")) {
  "RSE_survey_2026_data"
} else {
  "../RSE_survey_2026_data"
}
OUT_DIR <- "rse-book/data"

clean_cols <- function(df, col_name) {
  df |>
    tidyr::pivot_longer(
      cols = setdiff(names(df), c("X", "Year_0", "row_id")),
      names_to = "question",
      values_to = col_name
    ) |>
    dplyr::mutate(is_other = stringr::str_detect(question, "other")) |>
    dplyr::select(row_id, is_other, dplyr::all_of(col_name)) |>
    dplyr::filter(.data[[col_name]] != "")
}

# Each group is a named character vector: file_stem -> output column name
groups <- list(
  df_institute = c(
    org2can  = "org_benefit",
    org3nord = "nordic_rse_community",
    org4de   = "national_tasks",
    org4nord = "nordic_rse_tasks",
    org6de   = "national_community"
  ),
  df_proj = c(
    proj4can = "testing_how",
    proj5zaf = "version_control",
    proj6zaf = "collaboration_tool"
  ),
  df_all = c(
    conf2can_0      = "conferences",
    currentEmp13    = "discipline",
    currentWork2qcl = "user_group",
    fund3           = "funding",
    fund3qnl_0      = "grant",
    genAI2          = "ai_use",
    genAI3          = "ai_tool",
    skill2          = "skills",
    tool4can        = "software_language",
    tool5           = "platform",
    train3_0        = "training_programs",
    train5          = "training_role",
    turnOver3zaf    = "challenges",
    turnOver4zaf    = "software_type",
    ukrse3          = "skill_development"
  )
)

# if data directory does not exist yet, create it
if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR)
}

# Read, clean, join, and write each group in one pass
iwalk(groups, \(mapping, out_name) {
  imap(mapping, \(col_name, stem) {
    file.path(DATA_DIR, paste0("2026_", stem, ".csv")) |>
      read.csv() |>
      clean_cols(col_name)
  }) |>
    purrr::reduce(dplyr::full_join, by = c("row_id", "is_other")) |>
    write.csv(file.path(OUT_DIR, paste0(out_name, ".csv")))
})