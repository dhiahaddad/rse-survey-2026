# rse-survey-2026

Analysis and reporting code for the International RSE Survey 2026. The
repository contains a question-by-question Quarto book, a Nordic insights
report, and generated descriptions of the survey flow.

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
| [`rse-book/`](rse-book/) | Main Quarto book containing the question-by-question analysis |
| [`rse-book/chapters/`](rse-book/chapters/) | One Quarto chapter per survey question or question group |
| [`rse-book/R/`](rse-book/R/) | Book configuration, data preparation, analysis, and plotting code |
| [`rse-book/R/recodes/`](rse-book/R/recodes/) | Reusable free-text recoding maps |
| [`insights.qmd`](insights.qmd) | Nordic insights report |
| [`RSE_survey_insights_helper/`](RSE_survey_insights_helper/) | Refactored data, plotting, caption, theme, and setup modules used by `insights.qmd` |
| [`RSE_survey_outline/survey-process.md`](RSE_survey_outline/survey-process.md) | Global survey flow, routing, and analysis filtering |
| [`RSE_survey_outline/survey-process-nordics.md`](RSE_survey_outline/survey-process-nordics.md) | Generated Nordic question inventory with per-question **N** and routing notes |
| [`RSE_survey_outline/build-survey-process-nordics.R`](RSE_survey_outline/build-survey-process-nordics.R) | Generator for the Nordic survey-process document |

## Configuration

Book and report code share `rse-book/R/.config`:

- `DATA_DIR` gives the data directory name;
- `FILTER` gives the country or countries included by default.

The current filter contains Finland, Norway, Sweden, Denmark, Iceland, and
Estonia. Analyses retain only submitted responses (rows with a non-empty
`submitdate_0`).

## Build the outputs

Run these commands from the repository root.

Render the Nordic insights report:

```bash
quarto render insights.qmd
```

Regenerate the Nordic survey-process document:

```bash
Rscript RSE_survey_outline/build-survey-process-nordics.R
```

Render the complete analysis book:

```bash
cd rse-book
quarto render
```

To render one book chapter while developing:

```bash
cd rse-book
quarto render chapters/conf2can_0.qmd
```

The book is written to `rse-book/_book/`; the standalone Nordic report is
written to `insights.html`, with supporting assets in `insights_files/`.

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

One-time repository setup (requires admin access): **Settings → Pages → Build
and deployment**, set Source to **Deploy from a branch**, Branch `gh-pages` /
`/ (root)`.

Fallback from a machine with data and Quarto authenticated to GitHub:

```bash
cd rse-book
quarto publish gh-pages
```
