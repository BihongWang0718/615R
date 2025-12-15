# Test Shiny App Data Loading
source("global.R")

cat("\n=== Shiny App Data Loading Test ===\n")
cat(sprintf("Population data: %d records\n", nrow(pop_data)))
cat(sprintf("Total population years: %d\n", nrow(total_pop)))
cat(sprintf("Projection data: %d records\n", nrow(proj_data)))
cat(sprintf("Comparison data: %d records\n", nrow(comparison_data)))
cat(sprintf("Key stats - Population: %s\n", format(key_stats$population, big.mark = ",")))
cat(sprintf("Key stats - 65+: %.1f%%\n", key_stats$percent_65plus))
cat(sprintf("Exp model R-squared: %.1f%%\n", exp_growth_model$statistics$r_squared * 100))
cat(sprintf("Aging model threshold year: %d\n", aging_model$threshold_year_21_percent))
cat("\nAll Shiny app data loaded successfully!\n")
