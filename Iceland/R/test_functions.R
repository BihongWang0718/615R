# Test Script for Data Cleaning Functions

source("data_cleaning.R")

test_results <- run_validation_tests()

cat("Test Results:\n")
cat(sprintf("  validate_numeric:     %s\n", ifelse(test_results$test_validate_numeric, "PASS", "FAIL")))
cat(sprintf("  validate_dataframe:   %s\n", ifelse(test_results$test_validate_dataframe, "PASS", "FAIL")))
cat(sprintf("  exponential_model:    %s\n", ifelse(test_results$test_exponential_model, "PASS", "FAIL")))
cat(sprintf("  na_analysis:          %s\n", ifelse(test_results$test_na_analysis, "PASS", "FAIL")))
cat(sprintf("\n  Total: %d/%d tests passed\n", test_results$passed_count, test_results$total_tests))
cat(sprintf("  Status: %s\n\n", ifelse(test_results$all_passed, "ALL PASSED", "SOME FAILED")))

# Test with real data
cat("=== TESTING WITH REAL ICELAND DATA ===\n\n")
DATA_PATH <- "../ICELAND-DATA-R/"

cat("Loading population data...\n")
pop_data <- clean_population_data(paste0(DATA_PATH, "population_by_age_sex.csv"))
na_summary <- attr(pop_data, "na_summary")
cat(sprintf("  Total records: %d, NA: %d (%.2f%%)\n\n", na_summary$total_records, na_summary$na_count, na_summary$na_percent))

cat("Extracting total population...\n")
total_pop <- get_total_population(pop_data)
cat(sprintf("  Years: %d to %d\n", min(total_pop$Year), max(total_pop$Year)))
cat(sprintf("  Population: %s to %s\n\n", format(min(total_pop$Population), big.mark = ","), format(max(total_pop$Population), big.mark = ",")))

cat("Fitting exponential growth model...\n")
exp_model <- fit_exponential_growth_model(total_pop)
cat(sprintf("  Formula: %s\n", exp_model$formula))
cat(sprintf("  R-squared: %.1f%%, Growth rate: %.2f%%/year\n\n", exp_model$statistics$r_squared * 100, exp_model$statistics$annual_growth_rate_percent))

cat("Loading World Bank aging data...\n")
wb_data <- load_worldbank_data(paste0(DATA_PATH, "worldbank_aging.csv"))
cat(sprintf("  Years: %d to %d, 65+ range: %.1f%% to %.1f%%\n\n", min(wb_data$Year), max(wb_data$Year), min(wb_data$Percent_65_Plus), max(wb_data$Percent_65_Plus)))

cat("Fitting aging trend model...\n")
aging_model <- fit_aging_trend_model(wb_data)
cat(sprintf("  Model type: %s, R-squared: %.1f%%\n", aging_model$model_type, aging_model$statistics$r_squared * 100))
cat(sprintf("  Predicted 21%% threshold year: %d\n\n", aging_model$threshold_year_21_percent))

cat("Loading international comparison data...\n")
comparison_data <- load_worldbank_comparison(paste0(DATA_PATH, "worldbank_comparison.csv"))
comparison_summary <- calculate_aging_comparison(comparison_data)
cat("  2024 Rankings (65+ %):\n")
for (i in 1:nrow(comparison_summary)) cat(sprintf("    %d. %s: %.1f%%\n", i, comparison_summary$Country[i], comparison_summary$End_Percent[i]))

cat("\n=== ALL TESTS COMPLETED SUCCESSFULLY! ===\n")
