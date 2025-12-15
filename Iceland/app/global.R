# Global Data Loading for Iceland Aging Society Shiny App
# MA615 Final Project - Fall 2025

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(scales)

source("../R/data_cleaning.R")
source("../R/visualization.R")

# Data Paths
DATA_PATH <- "../ICELAND-DATA-R/"
GEOJSON_PATH <- "../ICELAND-DATA-R/iceland_regions.geojson"

# Load All Data
message("Loading Iceland population data...")
pop_data <- clean_population_data(paste0(DATA_PATH, "population_by_age_sex.csv"))
dep_data <- clean_dependency_data(paste0(DATA_PATH, "dependency_ratio.csv"))
proj_data <- clean_projection_data(paste0(DATA_PATH, "population_projection.csv"))
muni_data <- clean_municipality_data(paste0(DATA_PATH, "population_by_municipality.csv"))
wb_data <- load_worldbank_data(paste0(DATA_PATH, "worldbank_aging.csv"))
comparison_data <- load_worldbank_comparison(paste0(DATA_PATH, "worldbank_comparison.csv"))

# Derived Datasets
message("Preparing derived datasets...")
total_pop <- get_total_population(pop_data)
age_proportions <- get_age_proportions(pop_data)
proj_totals <- get_projected_totals(proj_data)
key_stats <- get_key_stats(pop_data, dep_data, wb_data)
na_analysis <- analyze_na_values(pop_data)

# Statistical Models
message("Fitting statistical models...")
exp_growth_model <- fit_exponential_growth_model(total_pop)
aging_model <- fit_aging_trend_model(wb_data)
comparison_summary <- calculate_aging_comparison(comparison_data)

# Available Years for UI
available_years <- pop_data %>% filter(Sex == "Total", Age == "Total", !is.na(Population)) %>%
  pull(Year) %>% unique() %>% sort()
projection_years <- proj_data %>% pull(Year) %>% unique() %>% sort()

# Sensitivity Analysis Parameters
sensitivity_params <- list(
  fertility_rate = 1.7, mortality_improvement = 0.5, net_migration = 3000,
  base_population = key_stats$population, base_year = key_stats$year
)

project_population_sensitivity <- function(base_pop, years, fertility_mult = 1, mortality_mult = 1, migration_mult = 1) {
  n_years <- length(years)
  population <- numeric(n_years)
  population[1] <- base_pop
  adjusted_rate <- 0.01 + (fertility_mult - 1) * 0.005 + (1 - mortality_mult) * 0.002 + (migration_mult - 1) * 0.003
  for (i in 2:n_years) population[i] <- population[i-1] * (1 + adjusted_rate)
  data.frame(Year = years, Population = round(population))
}

# App Info
APP_TITLE <- "Iceland Aging Society Analysis"
ABOUT_TEXT <- "
This application explores demographic changes in Iceland, focusing on population aging trends.

**Data Sources:**
- Statistics Iceland (px.hagstofa.is)
- World Bank Open Data

**Features:**
- Historical population trends (1841-2025)
- Population pyramids by year
- Age structure analysis
- Geographic distribution
- Future projections (2026-2074)
- Statistical Models (exponential growth, aging trend)
- Sensitivity Analysis (interactive parameter adjustment)
- International Comparison (Nordic countries + Japan)

**Created for:** MA615 Final Project, Fall 2025
"

message("Data loading complete!")
message(paste("Validation tests:", ifelse(run_validation_tests()$all_passed, "All passed", "Some failed")))
