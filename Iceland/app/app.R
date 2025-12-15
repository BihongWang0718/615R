# Iceland Aging Society - Shiny Application
# MA615 Final Project - Fall 2025

source("global.R")

# === UI ===
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = APP_TITLE),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Population Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Aging Analysis", tabName = "aging", icon = icon("user-clock")),
      menuItem("Geographic Distribution", tabName = "geography", icon = icon("map")),
      menuItem("Future Projections", tabName = "projections", icon = icon("chart-area")),
      menuItem("Sensitivity Analysis", tabName = "sensitivity", icon = icon("sliders-h")),
      menuItem("International Comparison", tabName = "comparison", icon = icon("globe")),
      menuItem("Statistical Models", tabName = "models", icon = icon("calculator")),
      hr(),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .info-box { min-height: 90px; }
      .small-box h3 { font-size: 28px; }
    "))),
    tabItems(
      # Tab 1: Overview
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("pop_box", width = 3), valueBoxOutput("age65_box", width = 3),
          valueBoxOutput("dep_box", width = 3), valueBoxOutput("year_box", width = 3)
        ),
        fluidRow(
          box(title = "Welcome to Iceland Aging Society Analysis", status = "primary", solidHeader = TRUE, width = 8,
              h4("Project Overview"),
              p("This interactive dashboard explores demographic changes in Iceland, with a focus on population aging trends."),
              h4("Main Results"),
              tags$ul(
                tags$li("Population grew from approximately 57,000 (1841) to nearly 390,000 (2025)"),
                tags$li("Elderly population (65+) increased from about 8% (1960) to over 15% (2024)"),
                tags$li("The country crossed the 'aged society' threshold (14%) in 2017"),
                tags$li("Projections suggest population between 450,000 and 700,000 by 2074")
              )),
          box(title = "Iceland at a Glance", status = "info", solidHeader = TRUE, width = 4,
              tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Flag_of_Iceland.svg/320px-Flag_of_Iceland.svg.png",
                       width = "100%", style = "margin-bottom: 15px;"),
              tags$ul(
                tags$li(tags$strong("Capital:"), " Reykjavik"), tags$li(tags$strong("Area:"), " 103,000 km²"),
                tags$li(tags$strong("Language:"), " Icelandic"), tags$li(tags$strong("Life Expectancy:"), " ~83 years")
              ))
        ),
        fluidRow(box(title = "Population Timeline", status = "primary", solidHeader = TRUE, width = 12,
                     plotlyOutput("overview_trend", height = "300px")))
      ),

      # Tab 2: Population Trends
      tabItem(tabName = "trends",
        fluidRow(
          box(title = "Controls", status = "warning", solidHeader = TRUE, width = 3,
              sliderInput("year_range", "Year Range:", min = min(available_years), max = max(available_years),
                          value = c(1900, max(available_years)), step = 1, sep = ""), hr(),
              sliderInput("pyramid_year", "Population Pyramid Year:", min = min(available_years), max = max(available_years),
                          value = 2020, step = 1, sep = "", animate = animationOptions(interval = 500))),
          box(title = "Total Population Over Time", status = "primary", solidHeader = TRUE, width = 9,
              plotlyOutput("pop_trend", height = "350px"))
        ),
        fluidRow(box(title = "Population Pyramid", status = "primary", solidHeader = TRUE, width = 12,
                     plotlyOutput("pop_pyramid", height = "500px")))
      ),

      # Tab 3: Aging Analysis
      tabItem(tabName = "aging",
        fluidRow(
          box(title = "Population Aged 65 and Over (%)", status = "danger", solidHeader = TRUE, width = 6,
              plotlyOutput("aging_trend", height = "350px")),
          box(title = "Dependency Ratios", status = "danger", solidHeader = TRUE, width = 6,
              plotlyOutput("dependency_chart", height = "350px"))
        ),
        fluidRow(box(title = "Age Structure Evolution", status = "primary", solidHeader = TRUE, width = 12,
                     plotlyOutput("age_structure", height = "400px"))),
        fluidRow(box(title = "Age Group Data Table", status = "info", solidHeader = TRUE, width = 12, DTOutput("age_table")))
      ),

      # Tab 4: Geographic Distribution
      tabItem(tabName = "geography",
        fluidRow(box(title = "Population by Municipality (2025)", status = "success", solidHeader = TRUE, width = 12,
                     leafletOutput("muni_map", height = "500px"))),
        fluidRow(
          box(title = "Top 10 Municipalities by Population", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("top_municipalities", height = "350px")),
          box(title = "Municipality Data", status = "info", solidHeader = TRUE, width = 6, DTOutput("muni_table"))
        )
      ),

      # Tab 5: Future Projections
      tabItem(tabName = "projections",
        fluidRow(
          box(title = "Projection Settings", status = "warning", solidHeader = TRUE, width = 3,
              checkboxGroupInput("variants", "Select Projection Variants:",
                                 choices = c("Low (L2)", "Low-Medium (L1)", "Median", "High-Medium (H1)", "High (H2)"),
                                 selected = c("Low (L2)", "Median", "High (H2)")), hr(),
              sliderInput("proj_year_range", "Year Range:", min = min(projection_years), max = max(projection_years),
                          value = c(min(projection_years), max(projection_years)), step = 1, sep = "")),
          box(title = "Population Projections (2026-2074)", status = "primary", solidHeader = TRUE, width = 9,
              plotlyOutput("projection_chart", height = "400px"))
        ),
        fluidRow(box(title = "Projection Comparison", status = "info", solidHeader = TRUE, width = 12,
                     p("Projected population for selected years under different scenarios:"), DTOutput("projection_table"))),
        fluidRow(box(title = "About the Projections", status = "info", solidHeader = TRUE, width = 12,
                     p("Statistics Iceland provides five projection variants based on different assumptions:"),
                     tags$ul(
                       tags$li(tags$strong("Low (L2):"), " Conservative estimate"),
                       tags$li(tags$strong("Median:"), " Most likely scenario"),
                       tags$li(tags$strong("High (H2):"), " Optimistic estimate")
                     )))
      ),

      # Tab 6: Sensitivity Analysis
      tabItem(tabName = "sensitivity",
        fluidRow(
          box(title = "Adjust Parameters", status = "warning", solidHeader = TRUE, width = 4,
              h4("Population Growth Factors"),
              p("Adjust the sliders to see how changes affect future population."), hr(),
              sliderInput("fertility_mult", "Fertility Rate Multiplier:", min = 0.5, max = 1.5, value = 1, step = 0.1, post = "x"),
              sliderInput("mortality_mult", "Life Expectancy Improvement:", min = 0.5, max = 1.5, value = 1, step = 0.1, post = "x"),
              sliderInput("migration_mult", "Net Migration Multiplier:", min = 0, max = 2, value = 1, step = 0.1, post = "x"), hr(),
              sliderInput("proj_horizon", "Projection Horizon:", min = 2030, max = 2074, value = 2050, step = 5), hr(),
              actionButton("reset_params", "Reset to Defaults", icon = icon("undo"))),
          box(title = "Sensitivity Analysis Results", status = "primary", solidHeader = TRUE, width = 8,
              plotlyOutput("sensitivity_chart", height = "400px"), hr(),
              h4("Projected Population Summary"), tableOutput("sensitivity_table"))
        ),
        fluidRow(box(title = "Parameter Explanation", status = "info", solidHeader = TRUE, width = 12,
                     p("Adjust the sliders above to explore different demographic scenarios."),
                     tags$ul(
                       tags$li("Fertility: affects birth rate"),
                       tags$li("Life Expectancy: affects mortality"),
                       tags$li("Migration: affects net international migration")
                     )))
      ),

      # Tab 7: International Comparison
      tabItem(tabName = "comparison",
        fluidRow(box(title = "Aging Trends: Iceland vs Other Countries", status = "primary", solidHeader = TRUE, width = 12,
                     plotlyOutput("comparison_chart", height = "450px"))),
        fluidRow(
          box(title = "Country Selection", status = "warning", solidHeader = TRUE, width = 4,
              checkboxGroupInput("selected_countries", "Select Countries to Compare:",
                                 choices = unique(comparison_data$Country), selected = c("Iceland", "Norway", "Japan"))),
          box(title = "Comparison Summary (2024)", status = "info", solidHeader = TRUE, width = 8, DTOutput("comparison_table"))
        ),
        fluidRow(box(title = "Observations", status = "success", solidHeader = TRUE, width = 12,
                     h4("Iceland in Context"),
                     tags$ul(
                       tags$li("Among Nordic countries, Iceland has the lowest percentage of population aged 65+ (15.6%)"),
                       tags$li("Japan serves as a reference point with nearly 30% of population over 65"),
                       tags$li("Studying countries further along in demographic transition can inform policy development")
                     )))
      ),

      # Tab 8: Statistical Models
      tabItem(tabName = "models",
        fluidRow(
          box(title = "Population Growth Model", status = "primary", solidHeader = TRUE, width = 6,
              h4("Exponential Growth Model"), p("Fitted to historical population data (1841-2025):"),
              verbatimTextOutput("exp_model_formula"), hr(), h5("Model Statistics:"),
              tableOutput("exp_model_stats"), hr(), plotlyOutput("exp_model_chart", height = "300px")),
          box(title = "Aging Trend Model", status = "danger", solidHeader = TRUE, width = 6,
              h4("Population 65+ Trend Model"), p("Fitted to World Bank aging data (1960-2024):"),
              verbatimTextOutput("aging_model_formula"), hr(), h5("Model Statistics:"),
              tableOutput("aging_model_stats"), hr(), plotlyOutput("aging_model_chart", height = "300px"))
        ),
        fluidRow(box(title = "Model Notes", status = "info", solidHeader = TRUE, width = 12,
                     h4("About These Models"),
                     tags$ul(
                       tags$li("The exponential growth model assumes a constant percentage growth rate over time."),
                       tags$li("The aging trend model captures acceleration in population aging patterns."),
                       tags$li("Both are descriptive models fitted to historical data and have inherent limitations.")
                     ), hr(), h4("Threshold Projection"), textOutput("threshold_prediction")))
      ),

      # Tab 9: About
      tabItem(tabName = "about",
        fluidRow(
          box(title = "About This Application", status = "primary", solidHeader = TRUE, width = 8,
              HTML(markdown::markdownToHTML(text = ABOUT_TEXT, fragment.only = TRUE))),
          box(title = "Data Sources", status = "info", solidHeader = TRUE, width = 4,
              tags$ul(
                tags$li(tags$a(href = "https://px.hagstofa.is/pxen/", "Statistics Iceland", target = "_blank")),
                tags$li(tags$a(href = "https://data.worldbank.org/", "World Bank Open Data", target = "_blank"))
              ), hr(), p(tags$strong("Created:"), " December 2025"), p(tags$strong("Course:"), " MA615 - Fall 2025"))
        )
      )
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  # Overview Tab
  output$pop_box <- renderValueBox(valueBox(format(key_stats$population, big.mark = ","), "Total Population", icon = icon("users"), color = "blue"))
  output$age65_box <- renderValueBox(valueBox(paste0(key_stats$percent_65plus, "%"), "Population 65+", icon = icon("user-clock"), color = "red"))
  output$dep_box <- renderValueBox(valueBox(key_stats$dependency_ratio, "Dependency Ratio", icon = icon("balance-scale"), color = "yellow"))
  output$year_box <- renderValueBox(valueBox(key_stats$year, "Latest Data Year", icon = icon("calendar"), color = "green"))
  output$overview_trend <- renderPlotly(create_population_trend(total_pop))

  # Population Trends Tab
  filtered_pop <- reactive(total_pop %>% filter(Year >= input$year_range[1], Year <= input$year_range[2]))
  output$pop_trend <- renderPlotly(create_population_trend(filtered_pop()))
  output$pop_pyramid <- renderPlotly({
    pyramid_data <- prepare_pyramid_data(pop_data, input$pyramid_year)
    if (nrow(pyramid_data) > 0) create_population_pyramid(pyramid_data, input$pyramid_year)
  })

  # Aging Analysis Tab
  output$aging_trend <- renderPlotly(create_aging_trend(wb_data))
  output$dependency_chart <- renderPlotly(create_dependency_chart(dep_data$long))
  output$age_structure <- renderPlotly(create_age_group_area(age_proportions))
  output$age_table <- renderDT({
    age_proportions %>% filter(Year %% 10 == 0 | Year == max(Year)) %>%
      mutate(Proportion = round(Proportion, 1)) %>%
      pivot_wider(id_cols = Year, names_from = Age_Group, values_from = c(Population, Proportion)) %>%
      arrange(desc(Year)) %>% datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(columns = c(2:4), currency = "", digits = 0)
  })

  # Geography Tab
  output$muni_map <- renderLeaflet(create_municipality_map(muni_data, geojson_path = GEOJSON_PATH))
  output$top_municipalities <- renderPlotly(create_top_municipalities_chart(muni_data))
  output$muni_table <- renderDT(muni_data %>% arrange(desc(Population)) %>%
                                  datatable(options = list(pageLength = 10), rownames = FALSE) %>%
                                  formatCurrency(columns = "Population", currency = "", digits = 0))

  # Projections Tab
  filtered_projections <- reactive(proj_totals %>% filter(Variant %in% input$variants,
                                                           Year >= input$proj_year_range[1], Year <= input$proj_year_range[2]))
  output$projection_chart <- renderPlotly(create_projection_chart(filtered_projections()))
  output$projection_table <- renderDT({
    proj_totals %>% filter(Year %in% c(2030, 2040, 2050, 2060, 2074)) %>%
      pivot_wider(names_from = Year, values_from = Population) %>%
      datatable(options = list(pageLength = 5, dom = 't'), rownames = FALSE) %>%
      formatCurrency(columns = 2:6, currency = "", digits = 0)
  })

  # Sensitivity Analysis Tab
  observeEvent(input$reset_params, {
    updateSliderInput(session, "fertility_mult", value = 1)
    updateSliderInput(session, "mortality_mult", value = 1)
    updateSliderInput(session, "migration_mult", value = 1)
    updateSliderInput(session, "proj_horizon", value = 2050)
  })

  custom_projection <- reactive({
    years <- seq(sensitivity_params$base_year, input$proj_horizon, by = 1)
    project_population_sensitivity(sensitivity_params$base_population, years,
                                    fertility_mult = input$fertility_mult,
                                    mortality_mult = input$mortality_mult,
                                    migration_mult = input$migration_mult)
  })

  output$sensitivity_chart <- renderPlotly({
    custom_proj <- custom_projection()
    official_median <- proj_totals %>% filter(Variant == "Median", Year <= input$proj_horizon)
    official_low <- proj_totals %>% filter(Variant == "Low (L2)", Year <= input$proj_horizon)
    official_high <- proj_totals %>% filter(Variant == "High (H2)", Year <= input$proj_horizon)

    plot_ly() %>%
      add_trace(data = official_low, x = ~Year, y = ~Population, type = 'scatter', mode = 'lines',
                line = list(color = 'gray', dash = 'dot'), name = 'Official Low') %>%
      add_trace(data = official_median, x = ~Year, y = ~Population, type = 'scatter', mode = 'lines',
                line = list(color = 'gray', dash = 'dash'), name = 'Official Median') %>%
      add_trace(data = official_high, x = ~Year, y = ~Population, type = 'scatter', mode = 'lines',
                line = list(color = 'gray', dash = 'dot'), name = 'Official High') %>%
      add_trace(data = custom_proj, x = ~Year, y = ~Population, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#e74c3c', width = 3), marker = list(size = 6), name = 'Your Scenario') %>%
      layout(title = list(text = "Custom Population Projection vs Official Scenarios", x = 0.5),
             xaxis = list(title = "Year"), yaxis = list(title = "Population", tickformat = ","),
             legend = list(orientation = "h", y = -0.15), hovermode = "x unified")
  })

  output$sensitivity_table <- renderTable({
    custom_proj <- custom_projection()
    official_median <- proj_totals %>% filter(Variant == "Median")
    target_years <- c(2030, 2040, 2050)
    target_years <- target_years[target_years <= input$proj_horizon]
    if (length(target_years) == 0) return(data.frame(Message = "Select projection horizon >= 2030"))

    custom_vals <- custom_proj %>% filter(Year %in% target_years) %>% select(Year, Custom = Population)
    official_vals <- official_median %>% filter(Year %in% target_years) %>% select(Year, Official_Median = Population)

    result <- custom_vals %>% left_join(official_vals, by = "Year") %>%
      mutate(Difference = Custom - Official_Median, Diff_Percent = paste0(round(100 * Difference / Official_Median, 1), "%"))
    result$Custom <- format(result$Custom, big.mark = ",")
    result$Official_Median <- format(result$Official_Median, big.mark = ",")
    result$Difference <- format(result$Difference, big.mark = ",")
    result
  }, striped = TRUE, hover = TRUE)

  # International Comparison Tab
  filtered_comparison <- reactive(comparison_data %>% filter(Country %in% input$selected_countries))

  output$comparison_chart <- renderPlotly({
    df <- filtered_comparison()
    if (nrow(df) == 0) return(plot_ly() %>% layout(title = "Select at least one country"))

    country_colors <- c("Iceland" = "#003897", "Norway" = "#BA0C2F", "Sweden" = "#006AA7",
                        "Denmark" = "#C8102E", "Finland" = "#003580", "Japan" = "#BC002D")
    p <- plot_ly() %>%
      add_trace(x = c(1960, 2024), y = c(7, 7), type = 'scatter', mode = 'lines',
                line = list(color = 'lightgray', dash = 'dot', width = 1), name = 'Aging (7%)') %>%
      add_trace(x = c(1960, 2024), y = c(14, 14), type = 'scatter', mode = 'lines',
                line = list(color = 'gray', dash = 'dash', width = 1), name = 'Aged (14%)') %>%
      add_trace(x = c(1960, 2024), y = c(21, 21), type = 'scatter', mode = 'lines',
                line = list(color = 'darkgray', dash = 'solid', width = 1), name = 'Super-Aged (21%)')

    for (country in unique(df$Country)) {
      country_data <- df %>% filter(Country == country)
      p <- p %>% add_trace(data = country_data, x = ~Year, y = ~Percent_65_Plus, type = 'scatter', mode = 'lines+markers',
                           line = list(color = country_colors[country], width = 2),
                           marker = list(color = country_colors[country], size = 8), name = country)
    }
    p %>% layout(title = list(text = "Population Aged 65+ (%): International Comparison", x = 0.5),
                 xaxis = list(title = "Year"), yaxis = list(title = "Percentage 65+", ticksuffix = "%", range = c(0, 35)),
                 legend = list(orientation = "v", x = 1.02), hovermode = "closest")
  })

  output$comparison_table <- renderDT({
    comparison_summary %>%
      mutate(Start_Percent = round(Start_Percent, 1), End_Percent = round(End_Percent, 1),
             Change = round(Change, 1), Annual_Change = round(Annual_Change, 3)) %>%
      select(Country, `1960 (%)` = Start_Percent, `2024 (%)` = End_Percent,
             `Change (pp)` = Change, `Annual Change` = Annual_Change) %>%
      datatable(options = list(pageLength = 6, dom = 't'), rownames = FALSE)
  })

  # Statistical Models Tab
  output$exp_model_formula <- renderText(exp_growth_model$formula)
  output$exp_model_stats <- renderTable(data.frame(
    Metric = c("R-squared", "RMSE", "Annual Growth Rate", "Doubling Time"),
    Value = c(paste0(round(exp_growth_model$statistics$r_squared * 100, 1), "%"),
              format(round(exp_growth_model$statistics$rmse), big.mark = ","),
              paste0(round(exp_growth_model$statistics$annual_growth_rate_percent, 2), "%"),
              paste0(round(exp_growth_model$statistics$doubling_time_years, 1), " years"))
  ), striped = TRUE)

  output$exp_model_chart <- renderPlotly({
    df <- exp_growth_model$data
    plot_ly() %>%
      add_trace(data = df, x = ~Year, y = ~Population, type = 'scatter', mode = 'markers',
                marker = list(color = '#3498db', size = 4, opacity = 0.5), name = 'Observed') %>%
      add_trace(data = df, x = ~Year, y = ~Fitted, type = 'scatter', mode = 'lines',
                line = list(color = '#e74c3c', width = 2), name = 'Fitted') %>%
      layout(title = list(text = "Exponential Growth Model Fit", x = 0.5),
             xaxis = list(title = "Year"), yaxis = list(title = "Population", tickformat = ","),
             showlegend = TRUE, legend = list(orientation = "h", y = -0.2))
  })

  output$aging_model_formula <- renderText({
    coefs <- coef(aging_model$model)
    if (aging_model$model_type == "linear") paste0("Percent_65+ = ", round(coefs[1], 2), " + ", round(coefs[2], 4), " × Year")
    else paste0("Percent_65+ = ", round(coefs[1], 2), " + ", round(coefs[2], 4), " × Year + ", round(coefs[3], 6), " × Year²")
  })

  output$aging_model_stats <- renderTable(data.frame(
    Metric = c("R-squared", "RMSE", "Model Type", "AIC (linear)", "AIC (quadratic)"),
    Value = c(paste0(round(aging_model$statistics$r_squared * 100, 1), "%"), round(aging_model$statistics$rmse, 3),
              aging_model$model_type, round(aging_model$statistics$aic_linear, 1), round(aging_model$statistics$aic_quadratic, 1))
  ), striped = TRUE)

  output$aging_model_chart <- renderPlotly({
    df <- aging_model$data
    proj <- aging_model$projections %>% filter(Year <= 2040)
    plot_ly() %>%
      add_trace(data = df, x = ~Year, y = ~Percent_65_Plus, type = 'scatter', mode = 'markers',
                marker = list(color = '#e74c3c', size = 6), name = 'Observed') %>%
      add_trace(data = df, x = ~Year, y = ~Fitted, type = 'scatter', mode = 'lines',
                line = list(color = '#3498db', width = 2), name = 'Fitted') %>%
      add_trace(data = proj, x = ~Year, y = ~Predicted, type = 'scatter', mode = 'lines',
                line = list(color = '#2ecc71', width = 2, dash = 'dash'), name = 'Projected') %>%
      add_trace(x = c(1960, 2040), y = c(21, 21), type = 'scatter', mode = 'lines',
                line = list(color = 'gray', dash = 'dot'), name = '21% Threshold') %>%
      layout(title = list(text = "Aging Trend Model", x = 0.5),
             xaxis = list(title = "Year"), yaxis = list(title = "Population 65+ (%)", ticksuffix = "%"),
             showlegend = TRUE, legend = list(orientation = "h", y = -0.2))
  })

  output$threshold_prediction <- renderText({
    year_21 <- aging_model$threshold_year_21_percent
    if (!is.na(year_21)) paste0("Iceland is projected to reach 'super-aged society' status (21% aged 65+) around ", year_21, ".")
    else "Unable to calculate threshold year."
  })
}

shinyApp(ui = ui, server = server)
