# rse-survey-2026

Analysis and reporting code for the International RSE Survey 2026. The
repository contains a question-by-question Quarto book (with Nordic insights as
the landing page) and generated descriptions of the survey flow.

## Data

Place the survey exports in `RSE_survey_2026_data/` at the repository root. The
analysis uses:

- `2026_tf.csv`: one row per respondent, including question responses and
  country (`socio1_0`);
- `2026_all_cols.csv`: question text and option metadata.

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

Analyses retain only submitted responses (rows with a non-empty
`submitdate_0`).

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

## Publish the book

The book is published to GitHub Pages at
<https://nordic-rse.github.io/rse-survey-2026/>.

Survey microdata are not available in CI, so computed results are frozen
locally and committed under `rse-book/_freeze/`. GitHub Actions then renders
HTML from those freezes and deploys to the `gh-pages` branch.

1. With the survey data available, render the book so `_freeze/` stays in sync
   with chapter sources and R code:

   ```bash
   cd rse-book
   quarto render
   ```

2. Commit any updated files under `rse-book/_freeze/` together with analysis
   changes.

3. Push to `main`. The `Publish Quarto book` workflow deploys automatically
   when `rse-book/` changes. You can also run it manually from the Actions tab.

## How to cite
Bockting, F. & Wittke, S. (2026). Analysis Book for the International RSE Survey 2026 (Nordic Focus) (Version 0.1.0). Zenodo. https://doi.org/10.5281/zenodo.21716004