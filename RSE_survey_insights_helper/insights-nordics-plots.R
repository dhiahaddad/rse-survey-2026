# Plot builders and section-level composites for Nordic insights.

insights_heatmap <- function(
    filter,
    question_code,
    title = NULL,
    title_size = NULL
) {
  pal <- insights_palette()
  plot_heatmap(
    filter,
    question_code,
    title = title,
    accent_color = pal$primary,
    panel_theme = insights_theme(title_size = title_size),
    y_labels = insights_y_labels(),
    wrap_y_in_data = TRUE
  )
}

insights_category_barplot <- function(
    filter,
    question_code,
    title = NULL
) {
  pal <- insights_palette()
  df <- load_question_selections(question_code, filter)
  categories <- unique(as.character(df[[question_code]][df$is_other == FALSE]))
  category_colors <- insights_category_response_colors(categories)
  plot_heatmap_category_barplot(
    filter,
    question_code,
    title = title,
    bar_color = pal$primary,
    category_colors = category_colors,
    wrap_y_in_data = TRUE,
    y_labels = insights_y_labels()
  ) +
    insights_theme() +
    insights_y_expand()
}

insights_likert_barplot <- function(
    df,
    column_name,
    meta,
    title = NULL,
    levels = likert5_levels(),
    xlim_max = NULL,
    title_size = NULL
) {
  level_map <- stats::setNames(insights_y_labels()(levels), levels)
  wrapped_levels <- unname(level_map[levels])
  plot_df <- df
  plot_df[[column_name]] <- factor(
    unname(level_map[as.character(plot_df[[column_name]])]),
    levels = wrapped_levels
  )
  plot_likert_barplot(
    plot_df,
    column_name,
    meta = meta,
    title = title,
    levels = wrapped_levels,
    xlim_max = xlim_max,
    response_colors = stats::setNames(
      unname(insights_likert_colors(levels)[levels]),
      wrapped_levels
    )
  ) +
    insights_theme(title_size = title_size) +
    insights_y_expand(wrapped_levels)
}

#' Place Likert bar plots in one row with y-axis labels on the first panel only
#'
#' @param plots A list of ggplot objects with identical response levels.
#' @param col_spacing Horizontal spacing between panels in centimetres. Set to
#'   `NULL` to retain each plot's existing margins.
#' @return A patchwork object.
#' @export
insights_likert_row <- function(plots, col_spacing = 1.5) {
  if (length(plots) == 0L) {
    stop("At least one plot is required.")
  }
  if (length(plots) == 1L) {
    return(plots[[1]])
  }

  styled <- c(
    list(plots[[1]]),
    lapply(
      plots[-1],
      function(p) {
        p +
          ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 0, "pt")
          )
      }
    )
  )

  if (!is.null(col_spacing) && length(styled) > 1L) {
    gap <- grid::convertUnit(grid::unit(col_spacing / 2, "cm"), "pt", valueOnly = TRUE)
    n <- length(styled)
    styled[[1]] <- styled[[1]] +
      ggplot2::theme(
        plot.margin = ggplot2::margin(5.5, gap, 5.5, 5.5, "pt")
      )
    if (n > 2L) {
      for (i in 2:(n - 1L)) {
        styled[[i]] <- styled[[i]] +
          ggplot2::theme(
            plot.margin = ggplot2::margin(5.5, gap, 5.5, gap, "pt")
          )
      }
    }
    styled[[n]] <- styled[[n]] +
      ggplot2::theme(
        plot.margin = ggplot2::margin(5.5, 5.5, 5.5, gap, "pt")
      )
  }

  patchwork::wrap_plots(styled, ncol = length(styled))
}

insights_allocation_summary <- function(
    df,
    countries,
    meta,
    country_label = NULL,
    stat = c("median", "mean")
) {
  plot_likert_allocation_summary(
    df,
    countries,
    meta = meta,
    country_label = country_label,
    stat = stat,
    direction_colors = insights_palette()$allocation_change
  ) +
    insights_theme() +
    insights_y_expand()
}

insights_allocation_age_ns <- function(
    df,
    countries,
    meta,
    question_code = "likert0"
) {
  n_allocation <- likert_allocation_n_respondents(
    df,
    countries,
    question_code,
    meta
  )
  n_with_age <- df |>
    dplyr::filter(.data$socio1_0 %in% countries) |>
    dplyr::inner_join(
      prepare_likert0_allocation(df, question_code, countries, meta) |>
        dplyr::distinct(.data$row_id),
      by = "row_id"
    ) |>
    assign_age_groups_two() |>
    dplyr::distinct(.data$row_id) |>
    nrow()

  list(
    allocation = n_allocation,
    with_age = n_with_age,
    excluded = n_allocation - n_with_age
  )
}

insights_allocation_summary_by_age <- function(
    df,
    countries,
    meta,
    country_label = "Nordics",
    stat = c("median", "mean")
) {
  stat <- match.arg(stat)

  df_age <- df |>
    dplyr::filter(.data$socio1_0 %in% countries) |>
    assign_age_groups_two() |>
    dplyr::mutate(socio1_0 = as.character(.data$age_group))

  actual_cols <- question_columns("likert0", names(df_age))
  desired_cols <- question_columns("likert1", names(df_age))
  if (
    !has_likert_observations(df_age, actual_cols) ||
      !has_likert_observations(df_age, desired_cols)
  ) {
    stop("No allocation responses available for the requested age groups.")
  }

  summary_df <- prepare_likert_allocation_summary(
    df_age,
    INSIGHTS_AGE_GROUPS,
    meta = meta,
    stat = stat,
    by_country = TRUE
  ) |>
    dplyr::rename(age_group = "country") |>
    dplyr::mutate(
      age_group = factor(.data$age_group, levels = INSIGHTS_AGE_GROUPS)
    )

  age_ns <- insights_allocation_age_ns(df, countries, meta)
  n_by_age <- df |>
    dplyr::filter(.data$socio1_0 %in% countries) |>
    dplyr::inner_join(
      prepare_likert0_allocation(df, "likert0", countries, meta) |>
        dplyr::distinct(.data$row_id),
      by = "row_id"
    ) |>
    assign_age_groups_two() |>
    dplyr::distinct(.data$row_id, .data$age_group) |>
    dplyr::count(.data$age_group, name = "n") |>
    dplyr::mutate(age_group = as.character(.data$age_group))
  facet_labels <- stats::setNames(
    vapply(
      INSIGHTS_AGE_GROUPS,
      function(group) {
        n <- n_by_age$n[match(group, n_by_age$age_group)]
        sprintf(
          "%s (N = %d)",
          group,
          if (is.na(n)) 0L else n
        )
      },
      character(1)
    ),
    INSIGHTS_AGE_GROUPS
  )

  stat_label <- stringr::str_to_title(stat)
  subtitle <- paste0(
    stat_label,
    " change from actual baseline per activity (N = ",
    age_ns$with_age,
    ")"
  )
  if (age_ns$excluded > 0L) {
    subtitle <- paste0(
      subtitle,
      "; ",
      age_ns$excluded,
      " of ",
      age_ns$allocation,
      " allocation respondents excluded (age not reported)"
    )
  }

  summary_df <- summary_df |>
    dplyr::mutate(activity = insights_wrap_factor(.data$activity))
  wrapped_activities <- levels(summary_df$activity)
  .plot_likert_allocation_summary_from_df(
    summary_df,
    title = paste0(
      "Actual vs desired time by activity — ",
      country_label,
      " by age group"
    ),
    subtitle = subtitle,
    direction_colors = insights_palette()$allocation_change,
    facet_col = "age_group",
    facet_ncol = 3L,
    facet_labels = facet_labels
  ) +
    insights_theme() +
    ggplot2::theme(panel.spacing.x = INSIGHTS_FACET_SPACING) +
    insights_y_expand(wrapped_activities)
}

plot_coded_category_bar <- function(
    coded_tbl,
    category_col,
    title = NULL,
    top_n = 10L
) {
  plot_df <- coded_tbl |>
    dplyr::mutate(
      pct_num = as.numeric(sub("%$", "", .data$pct))
    ) |>
    dplyr::arrange(dplyr::desc(.data$pct_num)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(
      category = forcats::fct_reorder(
        as.character(.data[[category_col]]),
        .data$pct_num
      ),
      category = insights_wrap_factor(.data$category, levels = levels(.data$category))
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$pct_num, y = .data$category)
  ) +
    ggplot2::geom_col(fill = insights_palette()$primary, width = INSIGHTS_BAR_WIDTH) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%s (N = %d)", .data$pct, .data$n)),
      hjust = -0.05,
      size = INSIGHTS_GEOM_TEXT_SIZE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, max(plot_df$pct_num, na.rm = TRUE) * 1.25),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = if (is.null(title)) NULL else stringr::str_wrap(title, width = 70),
      x = "Percentage",
      y = NULL
    ) +
    insights_y_expand(levels(plot_df$category)) +
    insights_theme()
}

plot_category_stacked_by_age <- function(
    filter,
    question_code,
    title = NULL,
    top_n = NULL,
    normalize = FALSE
) {
  data <- insights_age_category_data(filter, question_code)
  plot_df <- data$plot_df

  if (nrow(plot_df) == 0L) {
    stop("No category selections available for the requested filter.")
  }

  if (!is.null(top_n)) {
    top_cats <- plot_df |>
      dplyr::group_by(.data$category) |>
      dplyr::summarise(total = sum(.data$count), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$total)) |>
      dplyr::slice_head(n = top_n) |>
      dplyr::pull(.data$category)

    plot_df <- plot_df |>
      dplyr::mutate(
        category = ifelse(
          .data$category %in% top_cats,
          .data$category,
          "Other"
        )
      ) |>
      dplyr::group_by(.data$age_group, .data$category) |>
      dplyr::summarise(count = sum(.data$count), .groups = "drop")
  }

  if (isTRUE(normalize)) {
    plot_df <- plot_df |>
      dplyr::group_by(.data$age_group) |>
      dplyr::mutate(percent = 100 * .data$count / sum(.data$count)) |>
      dplyr::ungroup()
  } else {
    n_by_age <- data$df |>
      dplyr::distinct(.data$row_id, .data$age_group) |>
      dplyr::filter(!is.na(.data$age_group)) |>
      dplyr::count(.data$age_group, name = "n_respondents")

    plot_df <- plot_df |>
      dplyr::left_join(n_by_age, by = "age_group") |>
      dplyr::mutate(percent = 100 * .data$count / .data$n_respondents)
  }

  category_order <- insights_category_order(plot_df)
  plot_df <- plot_df |>
    dplyr::mutate(category = factor(.data$category, levels = category_order))

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$age_group, y = .data$percent, fill = .data$category)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = insights_category_colors(levels(plot_df$category)),
      drop = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(.data$percent >= 8, sprintf("%.0f%%", .data$percent), "")
      ),
      position = ggplot2::position_stack(vjust = 0.5),
      size = INSIGHTS_GEOM_TEXT_SIZE
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = if (isTRUE(normalize)) {
        "Share of selections (%)"
      } else {
        "Percentage of respondents"
      },
      fill = NULL
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE, title = NULL)
    ) +
    insights_theme()
}

plot_career_progression_by_age <- function(df) {
  statements <- tibble::tribble(
    ~column, ~statement,
    "likert5a[2]_0", "Likely promotion in current group",
    "likert5b[2]_0", "Clear promotion process",
    "likert5a[3]_0", "Opportunities in career plan"
  )
  statement_labels <- purrr::pmap_chr(statements, function(column, statement) {
    insights_panel_title(statement, insights_likert_n(df, column))
  })

  df_age <- assign_age_groups_two(df)

  plot_df <- purrr::pmap_dfr(statements, function(column, statement) {
    likert_count_data(df_age, column, group_col = "age_group") |>
      dplyr::mutate(
        sentiment = dplyr::case_when(
          .data$response %in% c("Agree", "Strongly Agree") ~ "Agree",
          .data$response %in% c("Disagree", "Strongly disagree") ~ "Disagree",
          TRUE ~ NA_character_
        ),
        statement = statement
      ) |>
      dplyr::group_by(.data$age_group, .data$statement) |>
      dplyr::mutate(total_responses = sum(.data$count)) |>
      dplyr::filter(!is.na(.data$sentiment)) |>
      dplyr::group_by(.data$age_group, .data$statement, .data$sentiment) |>
      dplyr::summarise(
        count = sum(.data$count),
        total_responses = dplyr::first(.data$total_responses),
        .groups = "drop"
      ) |>
      dplyr::mutate(percent = 100 * .data$count / .data$total_responses) |>
      dplyr::ungroup()
  }) |>
    dplyr::mutate(
      age_group = factor(
        as.character(.data$age_group),
        levels = INSIGHTS_AGE_GROUPS
      ),
      statement = factor(
        .data$statement,
        levels = statements$statement,
        labels = statement_labels
      ),
      sentiment = factor(.data$sentiment, levels = c("Agree", "Disagree"))
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$age_group,
      y = .data$percent,
      fill = .data$sentiment
    )
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f%%", .data$percent)),
      position = ggplot2::position_dodge(width = 0.8),
      vjust = -0.4,
      size = INSIGHTS_GEOM_TEXT_SIZE
    ) +
    ggplot2::facet_wrap(~ statement, ncol = 1, scales = "free_x") +
    ggplot2::scale_fill_manual(
      values = insights_palette()$sentiment,
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(
      title = "Career progression and promotion uncertainty",
      subtitle = "Agree vs disagree by age group (neutral responses not shown)",
      x = NULL,
      y = "Percentage within age group"
    ) +
    insights_theme()
}

plot_satisfaction_triple <- function(df, meta) {
  items <- tibble::tribble(
    ~column, ~title,
    "likert3a[SQ001]_0", "Job satisfaction",
    "likert3b[SQ001]_0", "Career satisfaction",
    "likert5b[1]_0", "Labour market demand"
  )

  plots <- purrr::pmap(items, function(column, title) {
    insights_likert_barplot(
      df,
      column,
      meta = meta,
      title = insights_panel_title(title, insights_likert_n(df, column)),
      xlim_max = 75,
      title_size = INSIGHTS_SUBPANEL_TITLE_SIZE
    )
  })

  insights_likert_row(plots) +
    insights_panel_annotation("Job satisfaction and labour market demand")
}

plot_toolbox_grid <- function(filter) {
  n_tool4can <- question_n_respondents(filter, "tool4can")
  n_proj6zaf <- question_n_respondents(filter, "proj6zaf")
  n_tool5 <- question_n_respondents(filter, "tool5")
  panels <- list(
    insights_heatmap(
      filter,
      "tool4can",
      title = insights_panel_title("Programming languages", n_tool4can),
      title_size = INSIGHTS_SUBPANEL_TITLE_SIZE
    ),
    plot_category_stacked_by_age(
      filter,
      "tool4can",
      title = insights_panel_title("Programming languages by age group", n_tool4can),
      top_n = 8L
    ),
    insights_heatmap(
      filter,
      "proj6zaf",
      title = insights_panel_title("Collaboration tools", n_proj6zaf),
      title_size = INSIGHTS_SUBPANEL_TITLE_SIZE
    ),
    insights_heatmap(
      filter,
      "tool5",
      title = insights_panel_title("Deployment platforms", n_tool5),
      title_size = INSIGHTS_SUBPANEL_TITLE_SIZE
    )
  )

  patchwork::wrap_plots(panels, ncol = 2) +
    insights_panel_annotation("RSE technical toolbox")
}

plot_network_community_row <- function(filter, df_nordics, meta) {
  p1 <- insights_category_barplot(
    filter,
    "currentWork3nord_0",
    title = insights_panel_title(
      "Peer network beyond close colleagues",
      question_n_respondents(filter, "currentWork3nord_0")
    )
  )
  p2 <- insights_likert_barplot(
    df_nordics,
    "currentWork2_0",
    meta = meta,
    levels = c("False", "True"),
    title = insights_panel_title(
      "Part of a dedicated research software group",
      insights_likert_n(df_nordics, "currentWork2_0")
    )
  )
  p3 <- insights_category_barplot(
    filter,
    "fund3",
    title = insights_panel_title(
      "Funding sources",
      question_n_respondents(filter, "fund3")
    )
  )

  patchwork::wrap_plots(
    p1,
    p2,
    p3,
    design = "AB\nCC",
    heights = c(1, 1)
  ) +
    insights_panel_annotation("Networks, community and local support structures")
}

plot_category_barplot_facet_age <- function(
    filter,
    question_code,
    title = NULL
) {
  data <- insights_age_category_data(filter, question_code)
  plot_df <- data$plot_df

  if (nrow(plot_df) == 0L) {
    stop("No category selections available for the requested filter.")
  }

  category_order <- insights_category_order(plot_df, other_last = FALSE)
  x_upper <- min(100, max(plot_df$percent, na.rm = TRUE) * 1.25)
  category_colors <- insights_category_response_colors(category_order)
  use_category_fill <- !is.null(category_colors)

  plot_df <- plot_df |>
    dplyr::mutate(
      category = insights_wrap_factor(
        factor(.data$category, levels = rev(category_order)),
        levels = rev(category_order)
      )
    )
  wrapped_order <- levels(plot_df$category)
  if (use_category_fill) {
    category_colors <- stats::setNames(
      unname(category_colors[category_order]),
      wrapped_order
    )
  }

  p <- if (use_category_fill) {
    ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data$percent, y = .data$category, fill = .data$category)
    ) +
      ggplot2::geom_col(width = INSIGHTS_BAR_WIDTH) +
      ggplot2::scale_fill_manual(values = category_colors, guide = "none")
  } else {
    ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data$percent, y = .data$category)
    ) +
      ggplot2::geom_col(fill = insights_palette()$primary, width = INSIGHTS_BAR_WIDTH)
  }

  n_by_age <- data$df |>
    dplyr::distinct(.data$row_id, .data$age_group) |>
    dplyr::filter(!is.na(.data$age_group)) |>
    dplyr::count(.data$age_group, name = "n") |>
    dplyr::mutate(age_group = as.character(.data$age_group))
  facet_labels <- stats::setNames(
    sprintf("%s (N = %d)", n_by_age$age_group, n_by_age$n),
    n_by_age$age_group
  )

  p +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("(N = %d)", .data$count)),
      hjust = -0.1,
      size = INSIGHTS_GEOM_TEXT_SIZE
    ) +
    ggplot2::facet_wrap(
      ~ age_group,
      ncol = 3,
      labeller = ggplot2::as_labeller(facet_labels)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, x_upper),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    insights_y_expand(wrapped_order) +
    ggplot2::labs(
      title = if (is.null(title)) NULL else stringr::str_wrap(title, width = 70),
      x = "Percentage",
      y = NULL
    ) +
    insights_theme() +
    ggplot2::theme(panel.spacing.x = INSIGHTS_FACET_SPACING)
}

plot_nordic_institute_by_age <- function(filter) {
  n_org3 <- question_n_respondents(filter, "org3nord")
  n_org4 <- question_n_respondents(filter, "org4nord")

  p1 <- plot_category_barplot_facet_age(
    filter,
    "org3nord",
    title = insights_panel_title("Community engagement and knowledge transfer", n_org3)
  )

  p2 <- plot_category_barplot_facet_age(
    filter,
    "org4nord",
    title = insights_panel_title("Tasks for Nordic-RSE or a future institute", n_org4)
  )

  patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(1, 1.2)) +
    insights_panel_annotation(
      title = "Priorities for a Nordic-RSE institute by age group",
      subtitle = "Multi-select responses; percentage of respondents in each age group"
    )
}
