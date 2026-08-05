# Shared constants and styling for Nordic insights figures.

INSIGHTS_AGE_GROUPS <- c("Below 35", "35-45", "45+")
INSIGHTS_THEME_SIZE <- 13L
INSIGHTS_SUBPANEL_TITLE_SIZE <- 11L
INSIGHTS_GEOM_TEXT_SIZE <- 3.2
INSIGHTS_BAR_WIDTH <- 0.8
INSIGHTS_FACET_SPACING <- grid::unit(1.5, "cm")
INSIGHTS_TWO_ROW_FIG_HEIGHT <- 10

# Okabe-Ito based palette (colorblind-safe qualitative; diverging for Likert).
# See ggplot2 book ch. 11 and Zeileis et al. (2023) R Journal on palette.colors().
.insights_pal <- local({
  response_negative <- "#D55E00"
  response_neutral <- "#999999"
  response_positive <- "#009E73"

  list(
    primary = "#0072B2",
    likert5 = c(
      "Strongly disagree" = response_negative,
      "Disagree" = response_negative,
      "Neither agree or disagree" = response_neutral,
      "Agree" = response_positive,
      "Strongly Agree" = response_positive
    ),
    sentiment = c(
      "Agree" = response_positive,
      "Neutral" = response_neutral,
      "Disagree" = response_negative
    ),
    binary = c(
      "False" = response_negative,
      "True" = response_positive
    ),
    frequency = c(
      "Never" = response_negative,
      "Sometimes" = response_neutral,
      "Always" = response_positive
    ),
    allocation_change = c(
      "Increase" = "#009E73",
      "Decrease" = "#D55E00",
      "No change" = "#999999"
    ),
    other = response_neutral
  )
})

insights_palette <- function() {
  .insights_pal
}

insights_is_other_category <- function(x) {
  identical(tolower(trimws(as.character(x))), "other")
}

insights_likert_colors <- function(levels) {
  pal <- insights_palette()
  if (identical(levels, likert5_levels())) {
    return(pal$likert5[levels])
  }
  if (identical(levels, likert_frequency_levels())) {
    return(pal$frequency[levels])
  }
  if (all(levels %in% names(pal$binary))) {
    return(pal$binary[levels])
  }
  if (any(grepl("^(Yes|No),", levels))) {
    colors <- insights_category_response_colors(levels)
    if (!is.null(colors)) {
      return(colors[levels])
    }
  }
  stats::setNames(
    insights_qualitative(length(levels)),
    levels
  )
}

insights_qualitative <- function(n) {
  cols <- grDevices::palette.colors(max(as.integer(n), 3L), palette = "Okabe-Ito")
  cols[cols == "#000000"] <- "#666666"
  cols[seq_len(n)]
}

insights_category_colors <- function(categories) {
  categories <- as.character(categories)
  pal <- insights_palette()
  is_other <- vapply(categories, insights_is_other_category, logical(1))
  cols <- rep(NA_character_, length(categories))
  names(cols) <- categories

  if (any(!is_other)) {
    cols[!is_other] <- insights_qualitative(sum(!is_other))
  }
  if (any(is_other)) {
    cols[is_other] <- pal$other
  }

  cols
}

#' Map yes/no multi-select categories to insight colors
#'
#' @param categories Character vector of category labels.
#' @return A named color vector, or `NULL` when no labels start with `"Yes,"`
#'   or `"No,"`.
insights_category_response_colors <- function(categories) {
  categories <- as.character(categories)
  if (!any(grepl("^(Yes|No),", categories))) {
    return(NULL)
  }

  pal <- insights_palette()
  cols <- rep(pal$primary, length(categories))
  names(cols) <- categories
  cols[grepl("^Yes,", categories)] <- unname(pal$binary["True"])
  cols[grepl("^No,", categories)] <- unname(pal$binary["False"])
  cols[vapply(categories, insights_is_other_category, logical(1))] <- pal$other
  cols
}

insights_y_labels <- function(threshold = 20L, wrap_width = 22L) {
  function(x) {
    labels <- as.character(x)
    vapply(
      labels,
      function(label) {
        if (is.na(label) || nchar(label) <= threshold) {
          label
        } else {
          stringr::str_wrap(label, width = wrap_width)
        }
      },
      character(1),
      USE.NAMES = FALSE
    )
  }
}

insights_wrap_factor <- function(x, levels = NULL) {
  raw <- as.character(x)
  raw_levels <- if (is.null(levels)) {
    if (is.factor(x)) levels(x) else unique(raw[!is.na(raw)])
  } else {
    as.character(levels)
  }
  if (length(raw_levels) == 0L) {
    return(factor(raw, levels = raw_levels))
  }
  wrapped_levels <- insights_y_labels()(raw_levels)
  mapping <- stats::setNames(wrapped_levels, raw_levels)
  factor(
    unname(mapping[raw]),
    levels = unname(mapping[raw_levels])
  )
}

insights_y_expand <- function(limits = NULL) {
  if (is.null(limits)) {
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.14))
  } else {
    ggplot2::scale_y_discrete(
      limits = limits,
      expand = ggplot2::expansion(add = 0.14)
    )
  }
}

insights_theme <- function(
    base_size = INSIGHTS_THEME_SIZE,
    title_size = NULL
) {
  title_size <- title_size %||% base_size
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = title_size),
      strip.text = ggplot2::element_text(face = "bold", size = base_size),
      axis.text.x = ggplot2::element_text(size = base_size),
      axis.text.y = ggplot2::element_text(size = base_size, lineheight = 0.75),
      axis.title = ggplot2::element_text(size = base_size),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = max(11L, base_size - 2L))
    )
}

insights_panel_annotation <- function(title, subtitle = NULL) {
  patchwork::plot_annotation(
    title = title,
    subtitle = subtitle,
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  )
}
