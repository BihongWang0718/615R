# Iceland Population Aging - Data Download Script

library(httr)
library(jsonlite)
library(tidyverse)


BASE_URL <- "https://px.hagstofa.is/pxen/api/v1/en"
TABLES <- list(
  population_by_age_sex = "Ibuar/mannfjoldi/1_yfirlit/yfirlit_mannfjolda/MAN00101.px",
  dependency_ratio = "Ibuar/mannfjoldi/1_yfirlit/yfirlit_mannfjolda/MAN00110.px",
  population_projection = "Ibuar/mannfjoldaspa/MAN09010.px",
  population_by_municipality = "Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px"
)

download_table <- function(name, path, custom_query = NULL) {
  url <- paste0(BASE_URL, "/", path)
  meta <- fromJSON(content(GET(url), "text", encoding = "UTF-8"))

  if (is.null(custom_query)) {
    query_list <- lapply(meta$variables$code, function(code)
      list(code = code, selection = list(filter = "all", values = list("*"))))
    query <- list(query = query_list, response = list(format = "json"))
  } else {
    query <- custom_query
  }

  data <- fromJSON(content(POST(url, body = toJSON(query, auto_unbox = TRUE),
                                 content_type_json(), timeout(120)), "text", encoding = "UTF-8"))
  write_json(data, paste0(name, ".json"), pretty = TRUE, auto_unbox = TRUE)

  value_labels <- setNames(
    lapply(seq_len(nrow(meta$variables)), function(i)
      setNames(meta$variables$valueTexts[[i]], meta$variables$values[[i]])),
    meta$variables$code)

  col_names <- data$columns$text
  df <- map_dfr(seq_len(nrow(data$data)), function(i) {
    keys <- data$data$key[[i]]
    row <- setNames(lapply(seq_along(keys), function(j) {
      code <- data$columns$code[j]
      value_labels[[code]][keys[j]] %||% keys[j]
    }), col_names[-length(col_names)])
    row[[col_names[length(col_names)]]] <- data$data$values[[i]][1]
    row
  })
  df
}

# Download each table
for (name in names(TABLES)) {
  cat("\nDownloading:", name, "\n")
  if (name == "population_by_municipality") {
    query <- list(query = list(
      list(code = "Municipality", selection = list(filter = "all", values = list("*"))),
      list(code = "Age", selection = list(filter = "item", values = list("-1"))),
      list(code = "Year", selection = list(filter = "item", values = list("2025"))),
      list(code = "Sex", selection = list(filter = "item", values = list("0")))
    ), response = list(format = "json"))
    df <- download_table(name, TABLES[[name]], query)
  } else {
    df <- download_table(name, TABLES[[name]])
  }
  write_csv(df, paste0(name, ".csv"))
  cat("  ->", paste0(name, ".csv"), "(", nrow(df), "rows)\n")
}

# World Bank data
wb_url <- "https://api.worldbank.org/v2/country/ISL/indicator/SP.POP.65UP.TO.ZS?format=json&date=1960:2024&per_page=100"
wb_data <- fromJSON(content(GET(wb_url), "text", encoding = "UTF-8"))
wb_df <- wb_data[[2]] %>% select(Year = date, Percent_65_Plus = value) %>%
  filter(!is.na(Percent_65_Plus)) %>% arrange(Year)
write_csv(wb_df, "worldbank_aging.csv")
cat("  -> worldbank_aging.csv (", nrow(wb_df), "rows)\n")

