# RSE Survey Germany

<a href="https://www.futursi.de/"><img src="year-books/futursi-logo.png" alt="FutuRSI — Next-Level RSE in Germany" width="260"></a>

An independent, dynamically generated analysis of the International RSE
Surveys, developed and maintained by [FutuRSI](https://www.futursi.de/). The
published book focuses on Germany for 2017, 2018, 2022, and 2026.

The project is Germany-focused by default, while its generators are designed
to make an equivalent publication for another country—or a selected group of
countries—with minimal configuration changes. The 2016 export contains only UK
respondents. The existing curated Nordic analysis remains in the repository as
an unpublished reference implementation.

## Data

The publishing workflow reads each year's data directly from
[`softwaresaved/RSE_survey_longitudinal`](https://github.com/softwaresaved/RSE_survey_longitudinal).
Every year uses its own `<year>_tf.csv` response table and `<year>_cols.csv`
question metadata without cross-year harmonisation.

For local work on the curated 2026 book, place the survey exports in
`RSE_survey_2026_data/` at the repository root. The analysis uses:

- `2026_tf.csv`: one row per respondent, including question responses and
  country (`socio1_0`);
- `2026_cols.csv`: question text and option metadata.

The data are not committed to this repository.
They are used under CC BY 4.0; see [NOTICE.md](NOTICE.md) for the required
attribution, provenance, and dataset citation.

## Repository structure

| Path | Purpose |
|------|---------|
| [`rse-book/`](rse-book/) | Unpublished curated Nordic analysis retained as a reference for future country insights |
| [`rse-book/index.qmd`](rse-book/index.qmd) | Existing Nordic insights landing page, not included in deployment |
| [`rse-book/about/user-guide.qmd`](rse-book/about/user-guide.qmd) | User guide for working with the book |
| [`rse-book/chapters/`](rse-book/chapters/) | One Quarto chapter per survey question or question group |
| [`rse-book/R/`](rse-book/R/) | Book configuration, data preparation, analysis, and plotting code |
| [`rse-book/R/recodes/`](rse-book/R/recodes/) | Reusable free-text recoding maps |
| [`scripts/build-year-book.R`](scripts/build-year-book.R) | Generates a standalone generic book from one year's response and metadata files |
| [`scripts/build-combined-book.R`](scripts/build-combined-book.R) | Assembles generated years into one publication with nested left-sidebar navigation |
| [`year-books/`](year-books/) | Multi-year landing page assembled with the rendered year books during deployment |
| [`year-books/futursi-logo.png`](year-books/futursi-logo.png) | Official FutuRSI logo used in the publication header and home page |
| [`NOTICE.md`](NOTICE.md) | Software and survey-data attribution and provenance |
| [`RSE_survey_insights_helper/`](RSE_survey_insights_helper/) | Data, plotting, caption, theme, and setup modules for the Nordic insights page |
| [`RSE_survey_outline/survey-process.md`](RSE_survey_outline/survey-process.md) | Global survey flow, routing, and analysis filtering |
| [`RSE_survey_outline/survey-process-nordics.md`](RSE_survey_outline/survey-process-nordics.md) | Generated Nordic question inventory with per-question **N** and routing notes |
| [`RSE_survey_outline/build-survey-process-nordics.R`](RSE_survey_outline/build-survey-process-nordics.R) | Generator for the Nordic survey-process document |

## Country configuration

The published workflow uses `BOOK_COUNTRY: Germany`. The generic generator's
`--country` setting accepts one country, a comma-separated group of countries,
or `all`. Country names are matched case-insensitively against `socio1_0`.

The files under `rse-book/` are the unpublished curated Nordic reference. They
have their own older configuration in `rse-book/R/.config`:

Book and report code share `rse-book/R/.config`:

- `DATA_DIR` gives the data directory name;
- `NORDIC_COUNTRIES` is the Nordic set (landing page always uses this);
- `FILTER` is the primary country scope for chapters;
- `FILTER_COMPARE` is a named list of extra groups for Between Countries.

The curated 2026 analyses retain only submitted responses (rows with a
non-empty `submitdate_0`). Generic books use the question-level inclusion rules
described below.

## Build the outputs

Run these commands from the repository root.

Regenerate the Nordic survey-process document:

```bash
Rscript RSE_survey_outline/build-survey-process-nordics.R
```

Render the complete analysis book (Nordic insights landing page plus chapters):

```bash
cd rse-book
quarto render
```

To render one book chapter while developing:

```bash
cd rse-book
quarto render chapters/conf2can_0.qmd
```

The book is written to `rse-book/_book/`. Open `_book/index.html` for the
Nordic insights landing page.

Generate and render any survey year using the adjacent longitudinal-data
repository:

```bash
Rscript scripts/build-year-book.R 2022 ../RSE_survey_longitudinal .generated/year-books
quarto render .generated/year-books/2022
```

The generated book is written to
`.generated/year-books/2022/_book/index.html`. Germany is the default country.
Substitute `2016`, `2017`, `2018`, or `2026` to build another year where that
country is represented.

To generate a book for another country, pass its `socio1_0` label. Separate
multiple countries with commas. Use `all` only when an explicitly all-country
book is wanted:

```bash
Rscript scripts/build-year-book.R 2022 ../RSE_survey_longitudinal .generated/year-books --country Netherlands
Rscript scripts/build-year-book.R 2022 ../RSE_survey_longitudinal .generated/year-books --country "Germany,Netherlands"
Rscript scripts/build-year-book.R 2022 ../RSE_survey_longitudinal .generated/year-books --country all
```

Only question groups with at least one response in the selected country are
included. The overview reports both the complete source-row count and the number
of records matching the country scope.

Generic books include both submitted and partial records by default. Each
question uses only records containing an answer to that question and reports
the submitted/partial breakdown. Chapters are grouped into thematic book parts
from their question-code families; unrecognised families are retained under
`Other questions`. To reproduce a submitted-only book instead:

```bash
Rscript scripts/build-year-book.R 2026 ../RSE_survey_longitudinal .generated/year-books --inclusion submitted
```

The unpublished curated Nordic analysis remains submitted-only.

To assemble several generated years into the same publication:

```bash
for year in 2017 2018 2022 2026; do
  Rscript scripts/build-year-book.R "$year" ../RSE_survey_longitudinal .generated/year-books
done
Rscript scripts/build-combined-book.R .generated/year-books .generated/combined-book 2017 2018 2022 2026
quarto render .generated/combined-book
```

The combined output is `.generated/combined-book/_site/index.html`. Survey
years are selected in the page header. Each year has its own left sidebar,
organised as thematic question section → question, so other years never appear
in the current sidebar. The redundant right-hand per-page table of contents is
disabled.

Question types are inferred from question codes and wording, metadata labels,
column structure, and recorded values. The generator distinguishes single and
multiple select, Likert/rating scales, matrix/grid questions, numeric inputs,
date/year inputs, and short and long free text. Numeric inputs use summary
statistics and histograms rather than categorical frequency charts.

Metadata row order is preserved for multi-select options and matrix items.
Numeric ranges and recognised Likert, agreement, and frequency scales use their
natural order. Because the yearly column metadata generally does not enumerate
the choices of single-select questions, unordered nominal answers use a clearly
reported frequency fallback. Each generated navigation manifest records the
detected type and ordering source.

Free-text question groups are reported with response-status counts and without
charts. Structured questions exclude `[other]` text from their charts. To
include raw free-text answers as collapsible text lists—including “Other”
answers beneath the relevant structured chart—add:

```bash
Rscript scripts/build-year-book.R 2026 ../RSE_survey_longitudinal .generated/year-books --include-free-text
```

## Publish the book

The multi-year portal is published to GitHub Pages at
<https://dhiahaddad.github.io/rse-survey-2026/>.

GitHub Actions generates Germany-focused results for 2017, 2018, 2022, and 2026
from each year's own metadata, then assembles them into one publication. The
publishing country is set by `BOOK_COUNTRY` in the workflow, and the generator
accepts `--country` for local builds. The Nordic analysis is not rendered,
linked, or deployed. The German publication is deployed to the `gh-pages`
branch.

Push the changes. On every branch, the `Build and publish survey books`
workflow downloads the current data and validates the combined German book.
Successful builds from `main` are deployed to GitHub Pages; feature-branch
builds do not publish. You can also run the workflow manually from the Actions
tab.

## License, attribution, and citation

The software remains under the [MIT License](LICENSE). The original Nordic RSE
copyright notice is preserved, and FutuRSI project contributors are identified
for the fork's subsequent work.

The survey data are separately licensed under CC BY 4.0. This publication is
an independent transformation of those data and is not presented as an official
publication or endorsement by the dataset creators. See [NOTICE.md](NOTICE.md)
for full attribution and [`CITATION.cff`](CITATION.cff) for citation metadata.
