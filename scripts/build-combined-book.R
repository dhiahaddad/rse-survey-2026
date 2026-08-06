#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    paste(
      "Usage: Rscript scripts/build-combined-book.R",
      "GENERATED_YEAR_ROOT OUTPUT_DIR YEAR [YEAR ...]"
    ),
    call. = FALSE
  )
}


year_root <- normalizePath(args[[1L]], mustWork = TRUE)
output_dir <- args[[2L]]
survey_years <- args[-c(1L, 2L)]
if (any(!grepl("^[0-9]{4}$", survey_years))) {
  stop("Every YEAR must contain four digits.", call. = FALSE)
}

yaml_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

copy_tree <- function(source, destination) {
  files <- list.files(source, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  files <- files[basename(files) != ".DS_Store"]
  relative <- substring(files, nchar(source) + 2L)
  is_directory <- file.info(files)$isdir
  directories <- unique(c(
    destination,
    file.path(destination, relative[is_directory]),
    dirname(file.path(destination, relative[!is_directory]))
  ))
  invisible(lapply(directories, dir.create, recursive = TRUE, showWarnings = FALSE))
  if (any(!is_directory)) {
    copied <- file.copy(
      files[!is_directory],
      file.path(destination, relative[!is_directory]),
      overwrite = TRUE
    )
    if (!all(copied)) stop("Failed to copy generated year files.", call. = FALSE)
  }
}

if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
dir.create(output_dir, recursive = TRUE)

navigation_by_year <- list()
records_by_year <- integer()
scope_by_year <- character()
for (survey_year in survey_years) {
  source_dir <- file.path(year_root, survey_year)
  manifest_path <- file.path(source_dir, "navigation.csv")
  if (!file.exists(manifest_path)) {
    stop("Missing generated navigation manifest: ", manifest_path, call. = FALSE)
  }

  destination <- file.path(output_dir, survey_year)
  copy_tree(file.path(source_dir, "questions"), file.path(destination, "questions"))
  copy_tree(file.path(source_dir, "figures"), file.path(destination, "figures"))
  file.copy(
    file.path(source_dir, "index.qmd"),
    file.path(destination, "index.qmd"),
    overwrite = TRUE
  )
  year_index <- readLines(file.path(source_dir, "index.qmd"), warn = FALSE)
  record_line <- grep(
    "^\\*\\*Records matching country scope:\\*\\* ",
    year_index,
    value = TRUE
  )
  records_by_year[[survey_year]] <- if (length(record_line)) {
    as.integer(sub("^.*\\*\\* ", "", record_line[[1L]]))
  } else {
    NA_integer_
  }
  scope_line <- grep(
    "^\\*\\*Country scope:\\*\\* ",
    year_index,
    value = TRUE
  )
  if (!length(scope_line)) {
    stop("Missing country scope in ", source_dir, "/index.qmd", call. = FALSE)
  }
  scope_by_year[[survey_year]] <- sub("^.*\\*\\* ", "", scope_line[[1L]])
  navigation_by_year[[survey_year]] <- read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  navigation_by_year[[survey_year]] <- navigation_by_year[[survey_year]][
    order(
      navigation_by_year[[survey_year]]$SectionOrder,
      navigation_by_year[[survey_year]]$QuestionOrder
    ),
    ,
    drop = FALSE
  ]
}

interactive_years <- survey_years[dir.exists(
  file.path(output_dir, survey_years, "figures", "widget-lib")
)]

publication_scopes <- unique(unname(scope_by_year))
if (length(publication_scopes) != 1L) {
  stop(
    "Generated years do not use the same country scope: ",
    paste(publication_scopes, collapse = "; "),
    call. = FALSE
  )
}
publication_scope <- publication_scopes[[1L]]
scope_countries <- trimws(strsplit(publication_scope, ",", fixed = TRUE)[[1L]])
publication_scope_text <- if (length(scope_countries) == 1L) {
  scope_countries[[1L]]
} else if (length(scope_countries) == 2L) {
  paste(scope_countries, collapse = " and ")
} else {
  paste0(
    paste(scope_countries[-length(scope_countries)], collapse = ", "),
    ", and ", scope_countries[[length(scope_countries)]]
  )
}
publication_title <- paste("Research Software Engineers in", publication_scope_text)
website_title <- paste("RSE Survey", publication_scope_text, "| FutuRSI")
survey_year_label <- if (length(survey_years) == 1L) {
  survey_years[[1L]]
} else {
  paste0(survey_years[[1L]], "–", survey_years[[length(survey_years)]])
}

year_card <- function(survey_year) {
  navigation <- navigation_by_year[[survey_year]]
  response_label <- if (is.na(records_by_year[[survey_year]])) {
    "Germany responses"
  } else {
    paste(format(records_by_year[[survey_year]], big.mark = ","), "recorded responses")
  }
  c(
    paste0('<a class="year-card" href="', survey_year, '/index.qmd">'),
    paste0('<span class="year-card__year">', survey_year, '</span>'),
    paste0('<span class="year-card__label">', publication_scope_text, ' survey</span>'),
    paste0(
      '<span class="year-card__meta">', response_label, ' · ',
      nrow(navigation), ' question groups</span>'
    ),
    '<span class="year-card__action">Explore this year <span aria-hidden="true">→</span></span>',
    '</a>'
  )
}

index_lines <- c(
  "---",
  paste0("title: ", yaml_string(publication_title)),
  paste0(
    "subtitle: ",
    yaml_string(paste("International RSE Survey results,", survey_year_label))
  ),
  "title-block-banner: true",
  "page-layout: full",
  "---",
  "",
  "::: {.home-maintainer}",
  paste0(
    '<a href="https://www.futursi.de/" aria-label="FutuRSI website">',
    '<span>Developed and maintained by</span>',
    '<img src="futursi-logo.png" alt="FutuRSI — Next-Level RSE in Germany">',
    '</a>'
  ),
  ":::",
  "",
  "::: {.home-intro}",
  paste("## Explore the RSE community in", publication_scope_text, "over time"),
  "",
  paste(
    paste("Discover how people who develop software for research in", publication_scope_text, "work,"),
    "learn, collaborate, and build their careers. Choose a survey year to begin."
  ),
  ":::",
  "",
  "::: {.year-grid}",
  unlist(lapply(survey_years, year_card)),
  ":::",
  "",
  "::::: {.home-method}",
  "## How to read this publication",
  "",
  ":::: {.home-method__grid}",
  "::: {.home-method__item}",
  "### Selected country scope",
  paste0(
    "Every result is filtered to the selected country-of-work scope: **",
    publication_scope_text, "**."
  ),
  ":::",
  "::: {.home-method__item}",
  "### Each year stands alone",
  "Questions and answer options can change between surveys, so results are not automatically harmonised across years.",
  ":::",
  "::: {.home-method__item}",
  "### Clear response counts",
  "Submitted and partial responses are included, with the relevant counts reported for each question.",
  ":::",
  "::: {.home-method__item}",
  "### Reusable by design",
  "The same generators can focus an equivalent publication on another country or a selected group of countries.",
  ":::",
  "::::",
  ":::::",
  "",
  "::: {.home-footnote}",
  if (identical(publication_scope, "Germany")) {
    "The 2016 export contains only United Kingdom respondents and is therefore not included."
  } else {
    "Only survey years containing records for the selected country scope are included."
  },
  ":::",
  ""
)
writeLines(index_lines, file.path(output_dir, "index.qmd"))

home_css <- file.path("year-books", "home.css")
brand_logo <- file.path("year-books", "futursi-logo.png")
if (!file.exists(home_css) || !file.exists(brand_logo)) {
  stop("Missing home-page stylesheet or FutuRSI logo.", call. = FALSE)
}
file.copy(home_css, file.path(output_dir, "home.css"), overwrite = TRUE)
file.copy(brand_logo, file.path(output_dir, "futursi-logo.png"), overwrite = TRUE)

quarto_lines <- c(
  "project:",
  "  type: website",
  "  output-dir: _site",
  "  render:",
  "    - index.qmd",
  paste0("    - ", survey_years, "/**/*.qmd")
)
if (length(interactive_years)) {
  quarto_lines <- c(
    quarto_lines,
    "  resources:",
    paste0("    - ", interactive_years, "/figures/widget-lib/**")
  )
}
quarto_lines <- c(
  quarto_lines,
  "website:",
  paste0("  title: ", yaml_string(website_title)),
  "  site-url: https://dhiahaddad.github.io/rse-survey-2026/",
  "  navbar:",
  "    logo: futursi-logo.png",
  "    logo-alt: 'FutuRSI — Next-Level RSE in Germany'",
  "    search: true",
  "    left:",
  "      - text: 'Home'",
  "        href: index.qmd",
  unlist(lapply(survey_years, function(survey_year) c(
    paste0("      - text: ", yaml_string(survey_year)),
    paste0("        href: ", survey_year, "/index.qmd")
  ))),
  "  sidebar:"
)

for (survey_year in survey_years) {
  navigation <- navigation_by_year[[survey_year]]
  quarto_lines <- c(
    quarto_lines,
    paste0("    - id: year-", survey_year),
    paste0("      title: ", yaml_string(paste(survey_year, "survey"))),
    "      style: docked",
    "      collapse-level: 1",
    "      contents:",
    "        - text: 'Survey overview'",
    paste0("          href: ", survey_year, "/index.qmd")
  )
  for (section in unique(navigation$Section)) {
    section_rows <- navigation[navigation$Section == section, , drop = FALSE]
    quarto_lines <- c(
      quarto_lines,
      paste0("        - section: ", yaml_string(section)),
      "          contents:"
    )
    for (row in seq_len(nrow(section_rows))) {
      quarto_lines <- c(
        quarto_lines,
        paste0("            - text: ", yaml_string(section_rows$Title[[row]])),
        paste0(
          "              href: ", survey_year, "/",
          section_rows$Chapter[[row]]
        )
      )
    }
  }
}

quarto_lines <- c(
  quarto_lines,
  "format:",
  "  html:",
  "    theme: cosmo",
  "    css: home.css",
  "    toc: false"
)
writeLines(quarto_lines, file.path(output_dir, "_quarto.yml"))

message(
  "Generated combined book for ", paste(survey_years, collapse = ", "),
  " at ", output_dir
)
