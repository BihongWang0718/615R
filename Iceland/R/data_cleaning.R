# Data Cleaning Functions for Iceland Aging Society Analysis
# MA615 Final Project - Fall 2025

library(tidyverse)

# --- Validation Helpers ---
validate_file_path <- function(file_path) {
  if (!file.exists(file_path)) stop(paste("File not found:", file_path))
  TRUE
}

validate_dataframe <- function(df, required_cols = NULL, min_rows = 1) {
  if (!is.data.frame(df)) stop("Input must be a dataframe")
  if (nrow(df) < min_rows) stop(paste("Need at least", min_rows, "rows"))
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(df))
    if (length(missing) > 0) stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  }
  TRUE
}

validate_numeric <- function(x, allow_na = TRUE, min_val = NULL, max_val = NULL) {
  if (!is.numeric(x)) return(FALSE)
  non_na <- x[!is.na(x)]
  if (!allow_na && any(is.na(x))) return(FALSE)
  if (!is.null(min_val) && any(non_na < min_val)) return(FALSE)
  if (!is.null(max_val) && any(non_na > max_val)) return(FALSE)
  TRUE
}

# --- Population Data ---
clean_population_data <- function(file_path) {
  validate_file_path(file_path)
  df <- read_csv(file_path, show_col_types = FALSE)
  colnames(df) <- c("Sex", "Age", "Year", "Population")
  df <- df %>%
    mutate(Population = as.numeric(na_if(Population, "..")), Year = as.integer(Year))

  attr(df, "na_summary") <- list(
    total_records = nrow(df), na_count = sum(is.na(df$Population)),
    na_percent = round(100 * sum(is.na(df$Population)) / nrow(df), 2)
  )
  df
}

get_total_population <- function(pop_data) {
  pop_data %>%
    filter(Sex == "Total", Age == "Total", !is.na(Population)) %>%
    select(Year, Population) %>% arrange(Year)
}

get_population_by_age <- function(pop_data, selected_year, exclude_total = TRUE) {
  df <- pop_data %>% filter(Year == selected_year, Sex != "Total")
  if (exclude_total) df <- df %>% filter(Age != "Total")
  df %>%
    mutate(Age_Numeric = ifelse(Age == "100 years and over", 100,
                                 as.numeric(gsub(" years?", "", Age)))) %>%
    arrange(Age_Numeric)
}

categorize_age_groups <- function(pop_data) {
  pop_data %>%
    filter(Age != "Total", Sex == "Total") %>%
    mutate(
      Age_Numeric = ifelse(Age == "100 years and over", 100, as.numeric(gsub(" years?", "", Age))),
      Age_Group = case_when(Age_Numeric < 20 ~ "0-19 years", Age_Numeric < 65 ~ "20-64 years", TRUE ~ "65+ years")
    ) %>%
    group_by(Year, Age_Group) %>%
    summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")
}

get_age_proportions <- function(pop_data) {
  categorize_age_groups(pop_data) %>%
    group_by(Year) %>%
    mutate(Total = sum(Population), Proportion = Population / Total * 100) %>%
    ungroup()
}

# --- Dependency Ratio ---
clean_dependency_data <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  colnames(df) <- c("Year", "Division", "Value")
  df <- df %>% mutate(Value = as.numeric(Value))
  list(long = df, wide = df %>% pivot_wider(names_from = Division, values_from = Value))
}

# --- Population Projection ---
clean_projection_data <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  colnames(df) <- c("Variant", "Age", "Sex", "Year", "Population")
  df %>% mutate(
    Variant = case_when(
      grepl("L2", Variant) ~ "Low (L2)", grepl("L1", Variant) ~ "Low-Medium (L1)",
      grepl("Median", Variant) ~ "Median", grepl("H1", Variant) ~ "High-Medium (H1)",
      grepl("H2", Variant) ~ "High (H2)", TRUE ~ Variant
    ),
    Year = as.integer(Year), Population = as.numeric(Population)
  )
}

get_projected_totals <- function(proj_data) {
  proj_data %>%
    filter(Sex == "Total", Age == "Total") %>%
    select(Variant, Year, Population) %>% arrange(Variant, Year)
}

# --- Municipality ---
clean_municipality_data <- function(file_path) {
  read_csv(file_path, show_col_types = FALSE) %>%
    select(Municipality = 1, Population = 5) %>%
    filter(Municipality != "Total") %>%
    mutate(Population = as.numeric(Population))
}

# --- World Bank Data ---
load_worldbank_data <- function(file_path) {
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(Year = as.integer(Year), Percent_65_Plus = as.numeric(Percent_65_Plus))
}

load_worldbank_comparison <- function(file_path) {
  validate_file_path(file_path)
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(Year = as.integer(Year), Percent_65_Plus = as.numeric(Percent_65_Plus)) %>%
    arrange(Country, Year)
}

# --- Key Statistics ---
get_key_stats <- function(pop_data, dep_data, wb_data) {
  latest <- pop_data %>% filter(Sex == "Total", Age == "Total", !is.na(Population))
  list(
    population = latest %>% filter(Year == max(Year)) %>% pull(Population),
    year = max(latest$Year),
    percent_65plus = round(wb_data %>% filter(Year == max(Year)) %>% pull(Percent_65_Plus), 1),
    dependency_ratio = dep_data$long %>% filter(Division == "Dependency ratio", Year == max(Year)) %>% pull(Value)
  )
}

# --- Population Pyramid ---
prepare_pyramid_data <- function(pop_data, selected_year) {
  pop_data %>%
    filter(Year == selected_year, Sex %in% c("Males", "Females"), Age != "Total") %>%
    mutate(
      Age_Numeric = ifelse(Age == "100 years and over", 100, as.numeric(gsub(" years?", "", Age))),
      Population = ifelse(Sex == "Males", -Population, Population)
    ) %>%
    filter(!is.na(Age_Numeric)) %>% arrange(Age_Numeric)
}

# --- Statistical Models ---
fit_exponential_growth_model <- function(pop_data, base_year = NULL) {
  validate_dataframe(pop_data, required_cols = c("Year", "Population"))
  df <- pop_data %>% filter(!is.na(Population)) %>% arrange(Year)
  if (is.null(base_year)) base_year <- min(df$Year)

  df <- df %>% mutate(Year_Centered = Year - base_year, Log_Population = log(Population))
  lm_model <- lm(Log_Population ~ Year_Centered, data = df)

  a <- exp(coef(lm_model)[1]); r <- coef(lm_model)[2]
  df <- df %>% mutate(Fitted = a * exp(r * Year_Centered), Residual = Population - Fitted)

  list(
    model = lm_model, data = df,
    parameters = list(a = a, r = r, base_year = base_year),
    statistics = list(
      r_squared = summary(lm_model)$r.squared, rmse = sqrt(mean(df$Residual^2)),
      doubling_time_years = log(2) / r, annual_growth_rate_percent = (exp(r) - 1) * 100
    ),
    formula = paste0("Population = ", round(a, 0), " × exp(", round(r, 5), " × (Year - ", base_year, "))")
  )
}

fit_aging_trend_model <- function(aging_data) {
  validate_dataframe(aging_data, required_cols = c("Year", "Percent_65_Plus"))
  df <- aging_data %>% filter(!is.na(Percent_65_Plus)) %>% arrange(Year)

  lm_linear <- lm(Percent_65_Plus ~ Year, data = df)
  lm_quadratic <- lm(Percent_65_Plus ~ Year + I(Year^2), data = df)

  best_model <- if (AIC(lm_quadratic) < AIC(lm_linear) - 2) lm_quadratic else lm_linear
  model_type <- if (AIC(lm_quadratic) < AIC(lm_linear) - 2) "quadratic" else "linear"

  df <- df %>% mutate(Fitted = predict(best_model), Residual = Percent_65_Plus - Fitted)
  future_years <- data.frame(Year = 2025:2050)
  future_years$Predicted <- predict(best_model, newdata = future_years)

  # Calculate 21% threshold year
  threshold_21_year <- NA
  if (model_type == "linear") {
    threshold_21_year <- (21 - coef(best_model)[1]) / coef(best_model)[2]
  } else {
    a <- coef(best_model)[3]; b <- coef(best_model)[2]; c <- coef(best_model)[1] - 21
    disc <- b^2 - 4*a*c
    if (disc >= 0) threshold_21_year <- ((-b + sqrt(disc)) / (2 * a))
  }

  list(
    model = best_model, model_type = model_type, data = df, projections = future_years,
    statistics = list(r_squared = summary(best_model)$r.squared, rmse = sqrt(mean(df$Residual^2)),
                      aic_linear = AIC(lm_linear), aic_quadratic = AIC(lm_quadratic)),
    threshold_year_21_percent = round(threshold_21_year)
  )
}

predict_population <- function(model, years) {
  if (inherits(model, "pop_growth_model")) {
    pred <- model$parameters$a * exp(model$parameters$r * (years - model$parameters$base_year))
  } else {
    p <- model$parameters
    pred <- p$K / (1 + exp(-p$r * (years - p$year_mid)))
  }
  data.frame(Year = years, Predicted_Population = pred)
}

# --- International Comparison ---
calculate_aging_comparison <- function(comparison_data) {
  comparison_data %>%
    group_by(Country) %>%
    summarise(
      Start_Year = min(Year), End_Year = max(Year),
      Start_Percent = Percent_65_Plus[Year == min(Year)],
      End_Percent = Percent_65_Plus[Year == max(Year)],
      Change = End_Percent - Start_Percent,
      Annual_Change = Change / (End_Year - Start_Year), .groups = "drop"
    ) %>% arrange(desc(End_Percent))
}

# --- NA Analysis ---
analyze_na_values <- function(pop_data) {
  validate_dataframe(pop_data, required_cols = c("Sex", "Age", "Year", "Population"))
  total <- nrow(pop_data); na_count <- sum(is.na(pop_data$Population))

  na_by_year <- pop_data %>%
    group_by(Year) %>%
    summarise(Total = n(), NA_Count = sum(is.na(Population)),
              NA_Percent = round(100 * NA_Count / Total, 1), .groups = "drop") %>%
    filter(NA_Count > 0)

  complete_years <- pop_data %>%
    group_by(Year) %>%
    summarise(Has_NA = any(is.na(Population)), .groups = "drop") %>%
    filter(!Has_NA) %>% pull(Year)

  list(
    summary = list(total_records = total, na_count = na_count,
                   na_percent = round(100 * na_count / total, 2),
                   complete_years_count = length(complete_years),
                   incomplete_years_count = nrow(na_by_year)),
    na_by_year = na_by_year, complete_years = complete_years
  )
}

# --- Validation Tests ---
run_validation_tests <- function() {
  results <- list()
  results$test_validate_numeric <- tryCatch({
    all(validate_numeric(c(1, 2, 3), allow_na = FALSE),
        validate_numeric(c(1, NA, 3), allow_na = TRUE),
        !validate_numeric(c(1, NA, 3), allow_na = FALSE))
  }, error = function(e) FALSE)

  results$test_validate_dataframe <- tryCatch({
    validate_dataframe(data.frame(a = 1:3, b = 4:6), required_cols = c("a", "b"))
    TRUE
  }, error = function(e) FALSE)

  results$test_exponential_model <- tryCatch({
    test_data <- data.frame(Year = 1900:1950, Population = 100000 * exp(0.01 * (0:50)))
    model <- fit_exponential_growth_model(test_data)
    abs(model$parameters$r - 0.01) < 0.001 && model$statistics$r_squared > 0.99
  }, error = function(e) FALSE)

  results$test_na_analysis <- tryCatch({
    test_data <- data.frame(Sex = rep("Total", 10), Age = rep("Total", 10),
                            Year = 2010:2019, Population = c(100, NA, 102, 103, NA, 105, 106, 107, 108, 109))
    analysis <- analyze_na_values(test_data)
    analysis$summary$na_count == 2
  }, error = function(e) FALSE)

  results$all_passed <- all(unlist(results))
  results$passed_count <- sum(unlist(results[1:4]))
  results$total_tests <- 4
  results
}
