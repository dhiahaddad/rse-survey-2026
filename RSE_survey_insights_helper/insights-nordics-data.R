# Shared data preparation for Nordic insights plots.

insights_age_category_data <- function(filter, question_code) {
  df <- load_question_selections(question_code, filter) |>
    assign_age_groups_two()
  grouped <- heatmap_category_grouped_df(
    df,
    question_code,
    group_by = "age_group"
  )

  list(
    df = df,
    plot_df = grouped$data |>
      dplyr::mutate(
        age_group = factor(
          as.character(.data$age_group),
          levels = INSIGHTS_AGE_GROUPS
        ),
        category = as.character(.data$category)
      )
  )
}

insights_category_order <- function(plot_df, other_last = TRUE) {
  order <- plot_df |>
    dplyr::group_by(.data$category) |>
    dplyr::summarise(total = sum(.data$count), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total)) |>
    dplyr::pull(.data$category)
  if (other_last && "Other" %in% order) {
    c(setdiff(order, "Other"), "Other")
  } else {
    order
  }
}
