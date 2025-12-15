# Visualization Functions for Iceland Aging Society Analysis
# MA615 Final Project - Fall 2025

library(tidyverse)
library(plotly)
library(ggplot2)
library(scales)
library(leaflet)
library(sf)

# Color Palette
ICELAND_COLORS <- list(
  primary = "#003897", secondary = "#DC1E35", male = "#3498db", female = "#e74c3c",
  age_young = "#2ecc71", age_working = "#3498db", age_elderly = "#e74c3c"
)

# Shared municipality coordinates
MUNI_COORDS <- tribble(
  ~Municipality, ~lat, ~lon,
  "Reykjavikurborg", 64.1466, -21.9426, "Kopavogsbaer", 64.1053, -21.9098,
  "Hafnarfjardarkaupstadur", 64.0671, -21.9377, "Akureyrarbaer", 65.6885, -18.1262,
  "Reykjanesbaer", 63.9972, -22.5621, "Gardabaer", 64.0894, -21.9227,
  "Mosfellsbaer", 64.1667, -21.6972, "Sveitarfelagid Arborg", 63.8667, -20.8167,
  "Seltjarnarnesbaer", 64.1548, -22.0041, "Akraneskaupstadur", 64.3222, -22.0778,
  "Vestmannaeyjabaer", 63.4383, -20.2689, "Fjardabyggd", 65.0167, -14.0167,
  "Mulathing", 65.2667, -14.3833, "Isafjardarbaer", 66.0739, -23.1353,
  "Skagafjordur", 65.75, -19.4167, "Nordurthing", 66.15, -17.3333,
  "Fjallabyggd", 66.15, -18.9167, "Grindavikurbaer", 63.8422, -22.4339,
  "Sveitarfelagid Vogar", 63.9833, -22.3833, "Sudurnesjabaer", 63.9500, -22.6000,
  "Borgarbyggd", 64.5378, -21.9206, "Grundarfjardarbaer", 64.9167, -23.2500,
  "Snaefellsbaer", 64.7500, -23.9000, "Sveitarfelagid Stykkisholmur", 65.0750, -22.7306,
  "Dalabyggd", 65.2167, -21.8833, "Bolungarvikurkaupstadur", 66.1500, -23.2667,
  "Vesturbyggd", 65.7500, -23.8000, "Strandabyggd", 65.7333, -21.6833,
  "Hunabyggd", 65.4500, -20.3833, "Blonduosbaer", 65.6603, -20.2917,
  "Sveitarfelagid Skagastrond", 65.8250, -20.3000, "Akrahreppur", 65.5167, -18.7833,
  "Eyjafjardarsveit", 65.6500, -18.0500, "Hofshreppur", 65.5833, -17.5167,
  "Dalvikurbyggd", 65.9722, -18.5292, "Grytubakkahreppur", 65.9333, -18.2667,
  "Svalbardsstrond", 65.7500, -18.1333, "Thingeyjarsveit", 65.8833, -17.4167,
  "Skuthustadahreppur", 65.6000, -17.0000, "Tjorneshreppur", 66.2000, -17.2500,
  "Langanesbyggd", 66.3833, -15.2500, "Vopnafjardarahreppur", 65.7583, -14.8500,
  "Fljotsdalshreppur", 65.2667, -14.6833, "Sveitarfelagid Hornafjordur", 64.2500, -15.2000,
  "Skaftarhreppur", 63.8333, -18.3833, "Myrdalshreppur", 63.4500, -19.1333,
  "Rangarthing eystra", 63.7833, -20.2333, "Rangarthing ytra", 63.8167, -20.5167,
  "Floahreppur", 64.0167, -20.3667, "Hrunamannahreppur", 64.1333, -20.5167,
  "Blaaskogabyggd", 64.2333, -20.8333, "Grimsnes- og Grafningshreppur", 64.1000, -20.8500,
  "Hveragerdi", 64.0000, -21.1833, "Sveitarfelagid Olfus", 63.9333, -21.1833,
  "Kjosarhreppur", 64.3000, -21.6000, "Hvalfjardarsveit", 64.3833, -21.5167
)

# --- Interactive Charts (Plotly) ---
create_population_trend <- function(total_pop_data) {
  plot_ly(total_pop_data, x = ~Year, y = ~Population, type = 'scatter', mode = 'lines+markers',
          line = list(color = ICELAND_COLORS$primary, width = 2),
          marker = list(color = ICELAND_COLORS$primary, size = 4)) %>%
    layout(title = list(text = "Iceland Population Growth", x = 0.5),
           xaxis = list(title = "Year"), yaxis = list(title = "Population", tickformat = ","),
           hovermode = "x unified")
}

create_population_pyramid <- function(pyramid_data, selected_year) {
  males <- pyramid_data %>% filter(Sex == "Males")
  females <- pyramid_data %>% filter(Sex == "Females")
  plot_ly() %>%
    add_bars(data = males, y = ~Age_Numeric, x = ~Population, orientation = 'h', name = 'Males',
             marker = list(color = ICELAND_COLORS$male)) %>%
    add_bars(data = females, y = ~Age_Numeric, x = ~Population, orientation = 'h', name = 'Females',
             marker = list(color = ICELAND_COLORS$female)) %>%
    layout(title = list(text = paste("Population Pyramid -", selected_year), x = 0.5),
           barmode = 'overlay', bargap = 0.1,
           xaxis = list(title = "Population", tickformat = ",",
                        range = list(-max(abs(pyramid_data$Population)) * 1.1, max(abs(pyramid_data$Population)) * 1.1)),
           yaxis = list(title = "Age"), legend = list(orientation = "h", y = -0.1))
}

create_age_group_area <- function(age_prop_data) {
  age_prop_data <- age_prop_data %>%
    mutate(Age_Group = factor(Age_Group, levels = c("65+ years", "20-64 years", "0-19 years")))
  colors <- c("0-19 years" = ICELAND_COLORS$age_young, "20-64 years" = ICELAND_COLORS$age_working,
              "65+ years" = ICELAND_COLORS$age_elderly)
  plot_ly(age_prop_data, x = ~Year, y = ~Proportion, color = ~Age_Group, colors = colors,
          type = 'scatter', mode = 'none', fill = 'tonexty', stackgroup = 'one') %>%
    layout(title = list(text = "Age Structure Evolution", x = 0.5),
           xaxis = list(title = "Year"), yaxis = list(title = "Percentage of Population", ticksuffix = "%"),
           legend = list(orientation = "h", y = -0.1))
}

create_aging_trend <- function(wb_data) {
  plot_ly(wb_data, x = ~Year, y = ~Percent_65_Plus, type = 'scatter', mode = 'lines+markers',
          line = list(color = ICELAND_COLORS$age_elderly, width = 2),
          marker = list(color = ICELAND_COLORS$age_elderly, size = 6), name = "65+ %") %>%
    add_trace(x = c(min(wb_data$Year), max(wb_data$Year)), y = c(14, 14), type = 'scatter', mode = 'lines',
              line = list(color = 'gray', width = 1, dash = 'dash'), name = "14% Threshold") %>%
    layout(title = list(text = "Population Aged 65+ in Iceland", x = 0.5),
           xaxis = list(title = "Year"), yaxis = list(title = "Percentage of Total Population", ticksuffix = "%"),
           showlegend = TRUE, legend = list(orientation = "h", y = -0.1))
}

create_dependency_chart <- function(dep_data_long) {
  ratios <- dep_data_long %>% filter(Division %in% c("Dependency ratio", "Young age ratio", "Old age ratio"))
  colors <- c("Dependency ratio" = ICELAND_COLORS$primary, "Young age ratio" = ICELAND_COLORS$age_young,
              "Old age ratio" = ICELAND_COLORS$age_elderly)
  plot_ly(ratios, x = ~Year, y = ~Value, color = ~Division, colors = colors,
          type = 'scatter', mode = 'lines+markers', marker = list(size = 8)) %>%
    layout(title = list(text = "Dependency Ratios Over Time", x = 0.5),
           xaxis = list(title = "Year"), yaxis = list(title = "Ratio"),
           legend = list(orientation = "h", y = -0.15))
}

create_projection_chart <- function(proj_totals) {
  variant_colors <- c("Low (L2)" = "#e74c3c", "Low-Medium (L1)" = "#f39c12", "Median" = "#2ecc71",
                      "High-Medium (H1)" = "#3498db", "High (H2)" = "#9b59b6")
  plot_ly(proj_totals, x = ~Year, y = ~Population, color = ~Variant, colors = variant_colors,
          type = 'scatter', mode = 'lines') %>%
    layout(title = list(text = "Population Projections (2026-2074)", x = 0.5),
           xaxis = list(title = "Year"), yaxis = list(title = "Projected Population", tickformat = ","),
           legend = list(orientation = "v", x = 1.02))
}

create_top_municipalities_chart <- function(muni_data) {
  top10 <- muni_data %>% arrange(desc(Population)) %>% head(10) %>%
    mutate(Municipality = reorder(Municipality, Population))
  plot_ly(top10, x = ~Population, y = ~Municipality, type = 'bar', orientation = 'h',
          marker = list(color = ~Population, colorscale = 'Blues')) %>%
    layout(title = list(text = "Top 10 Municipalities", x = 0.5),
           xaxis = list(title = "Population", tickformat = ","), yaxis = list(title = ""))
}

# --- Interactive Leaflet Map ---
create_municipality_map <- function(muni_data, geojson_path = NULL) {
  map_data <- muni_data %>% left_join(MUNI_COORDS, by = "Municipality") %>% filter(!is.na(lat))
  map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -19.0, lat = 65.0, zoom = 6)

  if (!is.null(geojson_path) && file.exists(geojson_path)) {
    regions <- sf::st_read(geojson_path, quiet = TRUE)
    region_pal <- colorFactor(palette = "Set3", domain = regions$Name)
    map <- map %>%
      addPolygons(data = regions, fillColor = ~region_pal(Name), fillOpacity = 0.3,
                  color = "#003897", weight = 2, label = ~Name,
                  popup = ~paste0("<strong>Region: </strong>", Name), group = "Regions")
  }

  if (nrow(map_data) == 0) return(map %>% addMarkers(lng = -21.9426, lat = 64.1466, popup = "Reykjavik"))

  pal <- colorNumeric(palette = "YlOrRd", domain = map_data$Population)
  map %>%
    addCircleMarkers(data = map_data, lng = ~lon, lat = ~lat, radius = ~pmax(sqrt(Population) / 20, 4),
                     color = "white", weight = 2, fillColor = ~pal(Population), fillOpacity = 0.85,
                     popup = ~paste0("<strong>", Municipality, "</strong><br>Population: ", format(Population, big.mark = ",")),
                     label = ~Municipality, group = "Municipalities") %>%
    addLegend(position = "bottomright", pal = pal, values = map_data$Population, title = "Population",
              labFormat = labelFormat(big.mark = ",")) %>%
    addLayersControl(overlayGroups = c("Regions", "Municipalities"), options = layersControlOptions(collapsed = FALSE))
}

# --- Static ggplot2 Charts (for Report) ---
create_static_population_trend <- function(total_pop_data) {
  ggplot(total_pop_data, aes(x = Year, y = Population)) +
    geom_line(color = ICELAND_COLORS$primary, linewidth = 1) +
    geom_point(color = ICELAND_COLORS$primary, size = 0.5, alpha = 0.5) +
    scale_y_continuous(labels = comma_format()) +
    labs(title = "Iceland Population Growth (1841-2025)", x = "Year", y = "Population") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

create_static_pyramid <- function(pyramid_data, selected_year) {
  ggplot(pyramid_data, aes(x = Age_Numeric, y = Population, fill = Sex)) +
    geom_bar(stat = "identity", width = 0.8) + coord_flip() +
    scale_y_continuous(labels = function(x) comma(abs(x)), limits = function(x) c(-max(abs(x)), max(abs(x)))) +
    scale_fill_manual(values = c("Males" = ICELAND_COLORS$male, "Females" = ICELAND_COLORS$female)) +
    labs(title = paste("Population Pyramid -", selected_year), x = "Age", y = "Population", fill = "Sex") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), legend.position = "bottom")
}

create_static_map <- function(muni_data) {
  map_data <- muni_data %>% left_join(MUNI_COORDS, by = "Municipality") %>% filter(!is.na(lat))
  ggplot(map_data, aes(x = lon, y = lat)) +
    geom_point(aes(size = Population, color = Population), alpha = 0.7) +
    scale_size_continuous(range = c(2, 15), labels = comma_format()) +
    scale_color_gradient(low = "#a6bddb", high = "#023858", labels = comma_format()) +
    labs(title = "Population Distribution in Iceland (2025)", x = "Longitude", y = "Latitude") +
    coord_fixed(ratio = 2) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}
