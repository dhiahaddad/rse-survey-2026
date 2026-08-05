# rse-survey-2026

Analysis and reporting code for the International RSE Surveys. The repository
contains dynamically generated country-focused books, plus a curated 2026
Quarto book with Nordic insights. The published generic books focus on Germany
for 2017, 2018, 2022, and 2026; the 2016 export contains only UK respondents.

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

## Repository structure

| Path | Purpose |
|------|---------|
| [`rse-book/`](rse-book/) | Main Quarto book; About part (insights + user guide) plus question-by-question chapters |
| [`rse-book/index.qmd`](rse-book/index.qmd) | Nordic insights landing page |
| [`rse-book/about/user-guide.qmd`](rse-book/about/user-guide.qmd) | User guide for working with the book |
| [`rse-book/chapters/`](rse-book/chapters/) | One Quarto chapter per survey question or question group |
| [`rse-book/R/`](rse-book/R/) | Book configuration, data preparation, analysis, and plotting code |
| [`rse-book/R/recodes/`](rse-book/R/recodes/) | Reusable free-text recoding maps |
| [`scripts/build-year-book.R`](scripts/build-year-book.R) | Generates a standalone generic book from one year's response and metadata files |
| [`year-books/`](year-books/) | Multi-year landing page assembled with the rendered year books during deployment |
| [`RSE_survey_insights_helper/`](RSE_survey_insights_helper/) | Data, plotting, caption, theme, and setup modules for the Nordic insights page |
| [`RSE_survey_outline/survey-process.md`](RSE_survey_outline/survey-process.md) | Global survey flow, routing, and analysis filtering |
| [`RSE_survey_outline/survey-process-nordics.md`](RSE_survey_outline/survey-process-nordics.md) | Generated Nordic question inventory with per-question **N** and routing notes |
| [`RSE_survey_outline/build-survey-process-nordics.R`](RSE_survey_outline/build-survey-process-nordics.R) | Generator for the Nordic survey-process document |

## Configuration

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

To generate a book for another country, pass its exact `socio1_0` label. Use
`all` only when an explicitly all-country book is wanted:

```bash
Rscript scripts/build-year-book.R 2022 ../RSE_survey_longitudinal .generated/year-books --country Netherlands
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

The curated Nordic analysis remains submitted-only.

Probable free-text question groups are reported with response-status counts and
without charts. Structured questions exclude `[other]` text from their charts.
To include raw free-text answers as collapsible text lists—including “Other”
answers beneath the relevant structured chart—add:

```bash
Rscript scripts/build-year-book.R 2026 ../RSE_survey_longitudinal .generated/year-books --include-free-text
```

## Publish the book

The multi-year portal is published to GitHub Pages at
<https://dhiahaddad.github.io/rse-survey-2026/>.

GitHub Actions generates Germany-focused books for 2017, 2018, 2022, and 2026
independently from each year's own metadata. The publishing country is set by
`BOOK_COUNTRY` in the workflow, and the generator accepts `--country` for local
builds. The curated Nordic analysis is rendered separately from its committed
frozen results and added at `/2026/insights/`. Everything is deployed together
to the `gh-pages` branch.

1. With the survey data available, render the book locally to check analysis
   changes:

   ```bash
   cd rse-book
   quarto render
   ```

2. Commit updated files under `rse-book/_freeze/` whenever the curated analysis
   computations change.

3. Push the changes. On every branch, the `Build and publish survey books`
   workflow downloads the current data and validates all four German books,
   then validates the frozen Nordic analysis without downloading its raw data.
   Successful builds from `main` are assembled and deployed to GitHub Pages;
   feature-branch builds do not publish. You can also run the workflow manually
   from the Actions tab.

The publishing workflow deliberately uses `rse-book/_freeze/` for the curated
analysis. This keeps that snapshot reproducible without putting its full R
dependency installation or live-data execution on the critical publishing
path.

## How to cite
Bockting, F. & Wittke, S. (2026). Analysis Book for the International RSE Survey 2026 (Nordic Focus) (Version 0.1.0). Zenodo. https://doi.org/10.5281/zenodo.21716004
