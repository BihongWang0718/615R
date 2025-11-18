# British Literary Prizes Shiny App

Interactive exploration of diversity and equity in major British literary prizes (1991–2022), with a reproducible R/Quarto analysis workflow.

## Research questions

1. What is the overall diversity landscape of British literary prizes?
2. Is there a “leaky pipeline” where women are more likely to be shortlisted but less likely to win? *(ORIGINAL)*
3. How has ethnic diversity evolved over time, and are improvements statistically significant?
4. Does “Oxbridge prestige” provide a winning advantage? *(ORIGINAL)*
5. Do certain genres have structural diversity barriers? *(ORIGINAL)*
6. How do prizes compare in diversity performance, and can we identify clusters of progressive vs traditional awards? *(ORIGINAL)*
7. What patterns emerge through interactive, user‑driven exploration?

## Components

- Shiny dashboard (`app.R`) with seven tabs (overview, gender, ethnicity, education, genre, prize comparison, data explorer).
- Data quality report (`data_quality.Rmd` → `data_quality.html`).

## Run the app

Install dependencies:

```r
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "ggplot2",
  "plotly", "DT", "scales"
))
```

Then in RStudio open `app.R` and click “Run App”.

## Data

- 952 rows, 23 variables, years 1991–2022.
- 681 authors, 861 books, 15 prizes.
- Very high completeness (~100%), no duplicate rows.

## Files in this folder

- `app.R` – Shiny dashboard.
- `prizes.csv` – analysis dataset.
- `data_quality.Rmd` – data quality report source.
- `README.md` – project overview.

Data from TidyTuesday (open data), used for teaching and research.
