# Data Collection Documentation

## Project Title
**Aging Society: Demographic Transition and Future Challenges in Iceland**

---

## Data Files

| File | Source | Content | Time Range |
|------|--------|---------|------------|
| `population_by_age_sex.csv/json` | Statistics Iceland | Population by age and sex | 1841-2025 |
| `dependency_ratio.csv/json` | Statistics Iceland | Dependency ratios | 1951-2025 |
| `population_projection.csv/json` | Statistics Iceland | Population projections | 2026-2074 |
| `population_by_municipality.csv/json` | Statistics Iceland | Population by municipality (for map) | 2025 |
| `worldbank_aging.csv` | World Bank | Population 65+ percentage | 1960-2024 |

---

## Data Collection Method

### 1. Statistics Iceland Data

Data retrieved via Statistics Iceland PX-Web API:

```r
library(httr)
library(jsonlite)

BASE_URL <- "https://px.hagstofa.is/pxen/api/v1/en"

# Population by age and sex (MAN00101)
url <- paste0(BASE_URL, "/Ibuar/mannfjoldi/1_yfirlit/yfirlit_mannfjolda/MAN00101.px")
meta <- fromJSON(content(GET(url), "text", encoding = "UTF-8"))

# Build query
query_list <- lapply(meta$variables$code, function(code) {
  list(code = code, selection = list(filter = "all", values = list("*")))
})
query <- list(query = query_list, response = list(format = "json"))

# Get data
data <- fromJSON(content(POST(url, body = toJSON(query, auto_unbox = TRUE),
                               content_type_json()), "text", encoding = "UTF-8"))
```

**API Documentation**: https://px.hagstofa.is/pxen/

### 2. World Bank Data

Population 65+ percentage from World Bank API:

```r
wb_url <- "https://api.worldbank.org/v2/country/ISL/indicator/SP.POP.65UP.TO.ZS?format=json&date=1960:2024&per_page=100"
wb_data <- fromJSON(content(GET(wb_url), "text", encoding = "UTF-8"))

wb_df <- wb_data[[2]] %>%
  select(Year = date, Percent_65_Plus = value) %>%
  filter(!is.na(Percent_65_Plus)) %>%
  arrange(Year)
```

**Indicator Code**: SP.POP.65UP.TO.ZS

---

## Data Structure

### population_by_age_sex (82,140 rows)
| Field | Description |
|-------|-------------|
| Sex | Gender (Total/Males/Females/Non-binary) |
| Age | Age (0, 1, 2, ..., 100+, Total) |
| Year | Year (1841-2025) |
| Population 1841-2025 | Population count (".." = missing) |

### dependency_ratio (609 rows)
| Field | Description |
|-------|-------------|
| Year | Time period (1951-1955, ..., 2025) |
| Division | Category (Total, 0-19 years, 20-64 years, 65+, Dependency ratio) |
| Population by age... | Population count or ratio |

**Definitions**:
- Dependency ratio = (0-19 + 65+) / (20-64) × 100
- Old age ratio = (65+) / (20-64) × 100

### population_projection (82,320 rows)
| Field | Description |
|-------|-------------|
| Sex | Gender |
| Age | Age |
| Year | Year (2026-2074) |
| Variant | Projection variant (L2/L1/Median/H1/H2) |

### population_by_municipality (63 rows)
| Field | Description |
|-------|-------------|
| Municipality | Municipality name |
| Population_2025 | Population in 2025 |

### worldbank_aging (65 rows)
| Field | Description |
|-------|-------------|
| Year | Year (1960-2024) |
| Percent_65_Plus | Population 65+ percentage |

---

## Analysis Uses

| Analysis | Data |
|----------|------|
| Aging trends | population_by_age_sex |
| Population pyramid | population_by_age_sex |
| Dependency ratio analysis | dependency_ratio |
| Future projections | population_projection |
| Municipality map | population_by_municipality |
| International comparison | worldbank_aging |

---

## Data Sources

| Source | URL | License |
|--------|-----|---------|
| Statistics Iceland | https://www.statice.is/ | CC BY 4.0 |
| World Bank | https://data.worldbank.org/ | CC BY 4.0 |

---

## How to Run

```r
# Install dependencies
install.packages(c("httr", "jsonlite", "tidyverse"))

# Run script
source("download_iceland_data.R")
```

---

## Data Collection Date
December 9, 2025
