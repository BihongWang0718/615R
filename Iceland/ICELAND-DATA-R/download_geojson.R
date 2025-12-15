# Download Iceland Regions GeoJSON

library(httr)
library(jsonlite)

download_iceland_geojson <- function(output_path = "iceland_regions.geojson") {
  url <- "https://raw.githubusercontent.com/baldurh/iceland-geodata/master/regions/100/iceland_regions.geojson"

  response <- GET(url)
  if (status_code(response) == 200) {
    geojson_content <- content(response, as = "text", encoding = "UTF-8")
    writeLines(geojson_content, output_path, useBytes = TRUE)
    geojson_data <- fromJSON(geojson_content)

    cat("Regions:", length(geojson_data$features), "\n")
    for (i in seq_along(geojson_data$features)) {
      cat(sprintf("  %d. %s\n", i, geojson_data$features[[i]]$properties$Name))
    }
    invisible(geojson_data)
  } else {
    stop(paste("Download failed, HTTP status:", status_code(response)))
  }
}

download_iceland_geojson("iceland_regions.geojson")
