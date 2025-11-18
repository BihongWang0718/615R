# British Literary Prizes Shiny App
# TidyTuesday 2025-10-28: Exploring diversity patterns in British literary prizes (1991-2022)
#
# RESEARCH QUESTIONS:
# 1. What is the overall diversity landscape of British literary prizes?
# 2. Is there a "leaky pipeline" where women are shortlisted more but win less? (ORIGINAL)
# 3. How has ethnic diversity evolved, and are improvements statistically significant?
# 4. Does "Oxbridge prestige" provide a winning advantage? (ORIGINAL)
# 5. Do certain genres have structural diversity barriers? (ORIGINAL)
# 6. How do prizes compare in diversity performance? (ORIGINAL)
# 7. What patterns emerge through interactive exploration?

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

prizes <- read.csv("prizes.csv", stringsAsFactors = FALSE, na.strings = c("", "NA", "n/a"))
prizes$prize_year <- as.integer(prizes$prize_year)

# UI DEFINITION

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "British Literary Prizes (1990-2022)",
    titleWidth = 350
  ),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Gender Analysis", tabName = "gender", icon = icon("venus-mars")),
      menuItem("Ethnicity Analysis", tabName = "ethnicity", icon = icon("globe")),
      menuItem("Education", tabName = "education", icon = icon("graduation-cap")),
      menuItem("Genre Patterns", tabName = "genre", icon = icon("book")),
      menuItem("Prize Comparison", tabName = "compare", icon = icon("balance-scale")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    ),

    hr(),
    h4("Filters", style = "text-align: center; font-weight: bold;"),

    sliderInput("year_range", "Year Range:",
                min = min(prizes$prize_year), max = max(prizes$prize_year),
                value = c(min(prizes$prize_year), max(prizes$prize_year)),
                step = 1, sep = ""),

    selectInput("prize_select", "Prize:",
                choices = c("All", sort(unique(prizes$prize_alias))),
                selected = "All", multiple = TRUE),

    selectInput("gender_select", "Gender:",
                choices = c("All", sort(unique(na.omit(prizes$gender)))),
                selected = "All", multiple = TRUE),

    radioButtons("role_select", "Person Role:",
                 choices = c("All" = "all", "Winner" = "winner", "Shortlisted" = "shortlisted"),
                 selected = "all"),

    checkboxInput("uk_only", "UK Residents Only", value = FALSE),

    br(),
    actionButton("reset_filters", "Reset All Filters",
                 icon = icon("undo"), class = "btn-warning btn-block")
  ),

  dashboardBody(
    tabItems(

      # TAB 1: DASHBOARD
      tabItem(tabName = "dashboard",
              h2("Overview & Summary Statistics"),
              h4("RQ: What is the overall landscape of British literary prizes in terms of diversity?"),
              hr(),

              fluidRow(
                valueBoxOutput("total_records", width = 3),
                valueBoxOutput("total_authors", width = 3),
                valueBoxOutput("total_prizes", width = 3),
                valueBoxOutput("year_span", width = 3)
              ),

              fluidRow(
                box(title = "Gender Distribution (Overall)", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("overview_gender_pie")),
                box(title = "Person Role Distribution", status = "info",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("overview_role_bar"))
              ),

              fluidRow(
                box(title = "Prizes Over Time", status = "success",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("overview_timeline", height = 300))
              ),

              fluidRow(
                box(title = "Key Findings", status = "warning",
                    solidHeader = TRUE, width = 12,
                    h4("Overall Landscape:"),
                    p("Nearly equal gender representation overall (49.4% women, 49.9% men), but significant disparities exist in outcomes and across genres."),
                    h4("Pipeline Problem (RQ2):"),
                    p("Women: 54.1% shortlisted → 43.3% winners (63% conversion). Men: 95% conversion. Clear evidence of 'leaky pipeline'."),
                    h4("Genre Barriers (RQ5):"),
                    p("Children's literature: 71% women. SFF: 21% women. 'Diversity ceiling' exists in certain genres."),
                    h4("Oxbridge Effect (RQ4):"),
                    p("Oxford & Cambridge dominate (88 winners), with slightly higher conversion rate (46.6% vs 43.3% non-Oxbridge)."),
                    h4("Ethnic Diversity (RQ3):"),
                    p("Limited representation: 52.4% White British, 20.9% Non-UK White. Booker Prize shows highest ethnic diversity (42.9%)."),
                    h4("Data Note:"),
                    p(em("'Unique People' counted by name (n=681) rather than person_id (n=682) due to duplicate entry for Michael Symmons Roberts with same VIAF.")))
              )
      ),

      # TAB 2: GENDER ANALYSIS
      tabItem(tabName = "gender",
              h2("Gender Diversity Analysis"),
              h4(style = "color: #3498db;",
                 "RQ: Is there a 'leaky pipeline' where women are more likely to be shortlisted but less likely to win? (ORIGINAL)"),
              hr(),

              fluidRow(
                valueBoxOutput("women_pct", width = 3),
                valueBoxOutput("men_pct", width = 3),
                valueBoxOutput("women_winners_pct", width = 3),
                valueBoxOutput("women_shortlisted_pct", width = 3)
              ),

              fluidRow(
                box(title = "Gender Distribution by Prize", status = "primary",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("gender_by_prize", height = 500))
              ),

              fluidRow(
                box(title = "Gender Trends Over Time", status = "info",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("gender_trends", height = 400))
              ),

              fluidRow(
                box(title = "Pipeline Problem Analysis", status = "warning",
                    solidHeader = TRUE, width = 12,
                    p("Conversion rate from shortlist to winner by gender:"),
                    plotlyOutput("pipeline_analysis", height = 400),
                    p(em("Note: Only prizes with shortlisted candidates are shown. Analysis limited to men and women due to small sample sizes in other categories.")),
                    hr(),
                    h4("Key Findings:"),
                    tags$ul(
                      tags$li(strong("Leaky Pipeline Confirmed:"), " Men convert at 95.5% (232/243) while women at 62.6% (181/289)."),
                      tags$li(strong("Prize-Specific Patterns:"), " Gold Dagger shows most extreme gap. Booker Prize: men 24.5%, women 17.5%.")
                    ))
              )
      ),

      # TAB 3: ETHNICITY ANALYSIS
      tabItem(tabName = "ethnicity",
              h2("Ethnicity & Diversity Analysis"),
              h4(style = "color: #e74c3c;",
                 "RQ: How has ethnic diversity evolved over the past 30 years, and are the improvements statistically significant?"),
              hr(),

              fluidRow(
                box(title = "Ethnicity Distribution", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("ethnicity_bar")),
                box(title = "Ethnicity by Person Role", status = "info",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("ethnicity_by_role"))
              ),

              fluidRow(
                box(title = "Ethnicity Trends Over Time", status = "success",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("ethnicity_trends", height = 400))
              ),

              fluidRow(
                box(title = "Key Findings", status = "warning",
                    solidHeader = TRUE, width = 12,
                    tags$ul(
                      tags$li(strong("Limited Diversity:"), " 73.3% White (52.4% White British + 20.9% Non-UK White). Ethnic minorities represent <30%."),
                      tags$li(strong("Major Groups:"), " Asian 7.0%, Irish 5.6%, Jewish 4.5%, Black British 3.0%."),
                      tags$li(strong("Prize Variation:"), " Booker Prize leads with 42.9% ethnic minorities. Gold Dagger lowest at 12.1%."),
                      tags$li(strong("Temporal Trends:"), " Time series shows gradual improvement but remains predominantly White across all years.")
                    ))
              )
      ),

      # TAB 4: EDUCATION
      tabItem(tabName = "education",
              h2("Educational Background Insights"),
              h4(style = "color: #27ae60;",
                 "RQ: Does the 'Oxbridge prestige' really provide a winning advantage, or is it a myth? (ORIGINAL)"),
              hr(),

              fluidRow(
                box(title = "Top Institutions - Winners", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("education_winners")),
                box(title = "Top Institutions - Shortlisted", status = "info",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("education_shortlisted"))
              ),

              fluidRow(
                box(title = "Highest Degree Distribution", status = "success",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("degree_distribution"))
              ),

              fluidRow(
                box(title = "Key Findings", status = "warning",
                    solidHeader = TRUE, width = 12,
                    tags$ul(
                      tags$li(strong("Oxbridge Dominance:"), " Oxford (55 winners) and Cambridge (33 winners) lead all institutions. Combined 88 winners."),
                      tags$li(strong("Slight Advantage Confirmed:"), " Oxbridge conversion rate 46.6% vs Non-Oxbridge 43.3% - modest but measurable."),
                      tags$li(strong("Alternative Paths:"), " UEA (13 winners), Trinity College Dublin (12), and US universities (Harvard, Columbia) show strong representation."),
                      tags$li(strong("Degree Levels:"), " Most winners hold Bachelor's degrees, though significant numbers have Master's and Doctorates.")
                    ))
              )
      ),

      # TAB 5: GENRE PATTERNS
      tabItem(tabName = "genre",
              h2("Genre-Specific Patterns"),
              h4(style = "color: #f39c12;",
                 "RQ: Do certain literary genres have structural barriers to diversity ('diversity ceiling')? (ORIGINAL)"),
              hr(),

              fluidRow(
                box(title = "Gender Distribution by Genre", status = "primary",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("gender_by_genre", height = 500))
              ),

              fluidRow(
                box(title = "Genre Statistics", status = "info",
                    solidHeader = TRUE, width = 12,
                    DTOutput("genre_stats_table"))
              ),

              fluidRow(
                box(title = "Key Findings", status = "warning",
                    solidHeader = TRUE, width = 12,
                    tags$ul(
                      tags$li(strong("Diversity Ceiling Confirmed:"), " SFF (21.2% women) and Non-fiction (31.9% women) show structural barriers."),
                      tags$li(strong("High Diversity Genres:"), " Children's literature (71% women), Fiction (61.6% women) achieve better balance."),
                      tags$li(strong("Genre Hierarchy:"), " Clear ranking from children's → fiction → poetry → biography → crime → non-fiction → SFF."),
                      tags$li(strong("Implication:"), " Genre choice significantly impacts diversity outcomes - certain genres have 'glass ceilings' for women writers.")
                    ))
              )
      ),

      # TAB 6: PRIZE COMPARISON
      tabItem(tabName = "compare",
              h2("Prize Comparison Tool"),
              h4(style = "color: #9b59b6;",
                 "RQ: How do different prizes compare in their diversity performance, and can we identify distinct clusters? (ORIGINAL)"),
              hr(),

              fluidRow(
                box(title = "Select Prizes to Compare", status = "primary",
                    solidHeader = TRUE, width = 12,
                    column(6, selectInput("prize1", "Prize 1:",
                                          choices = sort(unique(prizes$prize_alias)),
                                          selected = "Booker Prize")),
                    column(6, selectInput("prize2", "Prize 2:",
                                          choices = sort(unique(prizes$prize_alias)),
                                          selected = "Women's Prize for Fiction")))
              ),

              fluidRow(
                box(title = "Comparison Metrics", status = "info",
                    solidHeader = TRUE, width = 12,
                    DTOutput("comparison_table"))
              )
      ),

      # TAB 7: DATA EXPLORER
      tabItem(tabName = "explorer",
              h2("Interactive Data Explorer"),
              h4(style = "color: #16a085;",
                 "RQ: What patterns can users discover through interactive exploration?"),
              hr(),

              fluidRow(
                box(title = "Filtered Dataset", status = "primary",
                    solidHeader = TRUE, width = 12,
                    p(textOutput("filtered_count")),
                    downloadButton("download_data", "Download Filtered Data"),
                    br(), br(),
                    DTOutput("data_table"))
              )
      )
    )
  )
)

# SERVER LOGIC

server <- function(input, output, session) {

  # Reactive Data Filtering

  filtered_data <- reactive({
    data <- prizes %>%
      filter(prize_year >= input$year_range[1],
             prize_year <= input$year_range[2])

    if (!"All" %in% input$prize_select && length(input$prize_select) > 0) {
      data <- data %>% filter(prize_alias %in% input$prize_select)
    }

    if (!"All" %in% input$gender_select && length(input$gender_select) > 0) {
      data <- data %>% filter(gender %in% input$gender_select)
    }

    if (input$role_select != "all") {
      data <- data %>% filter(person_role == input$role_select)
    }

    if (input$uk_only) {
      data <- data %>% filter(uk_residence == TRUE)
    }

    data
  })

  observeEvent(input$reset_filters, {
    updateSliderInput(session, "year_range",
                      value = c(min(prizes$prize_year), max(prizes$prize_year)))
    updateSelectInput(session, "prize_select", selected = "All")
    updateSelectInput(session, "gender_select", selected = "All")
    updateRadioButtons(session, "role_select", selected = "all")
    updateCheckboxInput(session, "uk_only", value = FALSE)
  })

  # TAB 1: DASHBOARD

  output$total_records <- renderValueBox({
    valueBox(nrow(filtered_data()), "Total Records",
             icon = icon("database"), color = "blue")
  })

  output$total_authors <- renderValueBox({
    valueBox(n_distinct(filtered_data()$name), "Unique People",
             icon = icon("user"), color = "green")
  })

  output$total_prizes <- renderValueBox({
    valueBox(n_distinct(filtered_data()$prize_alias), "Prizes",
             icon = icon("trophy"), color = "yellow")
  })

  output$year_span <- renderValueBox({
    data <- filtered_data()
    span <- paste(min(data$prize_year), "-", max(data$prize_year))
    valueBox(span, "Year Range", icon = icon("calendar"), color = "red")
  })

  output$overview_gender_pie <- renderPlotly({
    data <- filtered_data() %>%
      count(gender) %>%
      mutate(percentage = n / sum(n) * 100)

    plot_ly(data, labels = ~gender, values = ~n, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent',
            marker = list(colors = c('#3498db', '#f39c12', '#e74c3c', '#9b59b6'))) %>%
      layout(showlegend = TRUE)
  })

  output$overview_role_bar <- renderPlotly({
    data <- filtered_data() %>% count(person_role)

    plot_ly(data, x = ~person_role, y = ~n, type = 'bar',
            marker = list(color = c('#3498db', '#e74c3c'))) %>%
      layout(xaxis = list(title = "Role"), yaxis = list(title = "Count"))
  })

  output$overview_timeline <- renderPlotly({
    data <- filtered_data() %>% count(prize_year)

    plot_ly(data, x = ~prize_year, y = ~n, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#3498db', width = 3), marker = list(size = 8)) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Awards/Nominations"))
  })

  # TAB 2: GENDER ANALYSIS

  output$women_pct <- renderValueBox({
    pct <- filtered_data() %>%
      summarise(pct = sum(gender == "woman") / n() * 100) %>%
      pull(pct) %>% round(1)
    valueBox(paste0(pct, "%"), "Women (Overall)",
             icon = icon("venus"), color = "red")
  })

  output$men_pct <- renderValueBox({
    pct <- filtered_data() %>%
      summarise(pct = sum(gender == "man") / n() * 100) %>%
      pull(pct) %>% round(1)
    valueBox(paste0(pct, "%"), "Men (Overall)",
             icon = icon("mars"), color = "blue")
  })

  output$women_winners_pct <- renderValueBox({
    pct <- filtered_data() %>%
      filter(person_role == "winner") %>%
      summarise(pct = sum(gender == "woman") / n() * 100) %>%
      pull(pct) %>% round(1)
    valueBox(paste0(pct, "%"), "Women Winners",
             icon = icon("trophy"), color = "orange")
  })

  output$women_shortlisted_pct <- renderValueBox({
    pct <- filtered_data() %>%
      filter(person_role == "shortlisted") %>%
      summarise(pct = sum(gender == "woman") / n() * 100) %>%
      pull(pct) %>% round(1)
    valueBox(paste0(pct, "%"), "Women Shortlisted",
             icon = icon("list"), color = "green")
  })

  output$gender_by_prize <- renderPlotly({
    data <- filtered_data() %>%
      count(prize_alias, gender) %>%
      group_by(prize_alias) %>%
      mutate(percentage = n / sum(n) * 100)

    plot_ly(data, x = ~percentage, y = ~reorder(prize_alias, percentage),
            color = ~gender, type = 'bar', orientation = 'h',
            colors = c('#3498db', '#f39c12', '#e74c3c', '#9b59b6')) %>%
      layout(barmode = 'stack', xaxis = list(title = "Percentage (%)"),
             yaxis = list(title = ""), legend = list(title = list(text = "Gender")))
  })

  output$gender_trends <- renderPlotly({
    data <- filtered_data() %>%
      count(prize_year, gender) %>%
      group_by(prize_year) %>%
      mutate(percentage = n / sum(n) * 100)

    plot_ly(data) %>%
      add_trace(x = ~prize_year, y = ~percentage, color = ~gender,
                type = 'scatter', mode = 'lines+markers',
                colors = c('#e74c3c', '#3498db', '#2ecc71', '#f39c12'),
                line = list(width = 3), marker = list(size = 8)) %>%
      layout(xaxis = list(title = "Year"), yaxis = list(title = "Percentage (%)"),
             hovermode = "x unified")
  })

  output$pipeline_analysis <- renderPlotly({
    data <- filtered_data() %>%
      filter(gender %in% c("man", "woman")) %>%
      group_by(prize_alias, gender) %>%
      summarise(
        shortlisted = sum(person_role == "shortlisted"),
        winners = sum(person_role == "winner"),
        conversion_rate = winners / shortlisted * 100,
        .groups = "drop"
      ) %>%
      filter(shortlisted > 0) %>%
      mutate(gender = factor(gender, levels = c("man", "woman")))

    plot_ly(data, x = ~prize_alias, y = ~conversion_rate, color = ~gender,
            type = 'bar', colors = c('#3498db', '#e74c3c')) %>%
      layout(xaxis = list(title = "Prize"), yaxis = list(title = "Conversion Rate (%)"),
             barmode = 'group')
  })

  # TAB 3: ETHNICITY ANALYSIS

  output$ethnicity_bar <- renderPlotly({
    data <- filtered_data() %>%
      count(ethnicity_macro) %>%
      arrange(desc(n))

    plot_ly(data, x = ~reorder(ethnicity_macro, n), y = ~n,
            type = 'bar', marker = list(color = '#3498db')) %>%
      layout(xaxis = list(title = "Ethnicity"), yaxis = list(title = "Count"))
  })

  output$ethnicity_by_role <- renderPlotly({
    data <- filtered_data() %>%
      count(person_role, ethnicity_macro) %>%
      group_by(person_role) %>%
      mutate(percentage = n / sum(n) * 100)

    plot_ly(data, x = ~ethnicity_macro, y = ~percentage, color = ~person_role,
            type = 'bar', colors = c('#3498db', '#e74c3c')) %>%
      layout(barmode = 'group', xaxis = list(title = "Ethnicity"),
             yaxis = list(title = "Percentage (%)"))
  })

  output$ethnicity_trends <- renderPlotly({
    data <- filtered_data() %>%
      count(prize_year, ethnicity_macro) %>%
      group_by(prize_year) %>%
      mutate(percentage = n / sum(n) * 100)

    plot_ly(data, x = ~prize_year, y = ~percentage, color = ~ethnicity_macro,
            type = 'scatter', mode = 'lines', stackgroup = 'one', fillcolor = 'tonexty') %>%
      layout(xaxis = list(title = "Year"), yaxis = list(title = "Percentage (%)"),
             hovermode = "x unified")
  })

  # TAB 4: EDUCATION

  output$education_winners <- renderPlotly({
    data <- filtered_data() %>%
      filter(person_role == "winner") %>%
      count(degree_institution) %>%
      arrange(desc(n)) %>%
      head(15)

    plot_ly(data, x = ~n, y = ~reorder(degree_institution, n),
            type = 'bar', orientation = 'h', marker = list(color = '#3498db')) %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = "Institution"))
  })

  output$education_shortlisted <- renderPlotly({
    data <- filtered_data() %>%
      filter(person_role == "shortlisted") %>%
      count(degree_institution) %>%
      arrange(desc(n)) %>%
      head(15)

    plot_ly(data, x = ~n, y = ~reorder(degree_institution, n),
            type = 'bar', orientation = 'h', marker = list(color = '#e74c3c')) %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = "Institution"))
  })

  output$degree_distribution <- renderPlotly({
    data <- filtered_data() %>%
      count(highest_degree) %>%
      arrange(desc(n))

    plot_ly(data, x = ~reorder(highest_degree, n), y = ~n,
            type = 'bar', marker = list(color = '#2ecc71')) %>%
      layout(xaxis = list(title = "Degree Level"), yaxis = list(title = "Count"))
  })

  # TAB 5: GENRE PATTERNS

  output$gender_by_genre <- renderPlotly({
    data <- filtered_data() %>%
      count(prize_genre, gender) %>%
      group_by(prize_genre) %>%
      mutate(percentage = n / sum(n) * 100)

    plot_ly(data, x = ~percentage, y = ~reorder(prize_genre, percentage),
            color = ~gender, type = 'bar', orientation = 'h',
            colors = c('#3498db', '#f39c12', '#e74c3c', '#9b59b6')) %>%
      layout(barmode = 'stack', xaxis = list(title = "Percentage (%)"),
             yaxis = list(title = "Genre"))
  })

  output$genre_stats_table <- renderDT({
    filtered_data() %>%
      group_by(prize_genre) %>%
      summarise(Total = n(),
                Women_Pct = round(sum(gender == "woman") / n() * 100, 1),
                Men_Pct = round(sum(gender == "man") / n() * 100, 1),
                UK_Residents_Pct = round(sum(uk_residence) / n() * 100, 1),
                .groups = "drop") %>%
      datatable(options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })

  # TAB 6: PRIZE COMPARISON

  output$comparison_table <- renderDT({
    p1 <- prizes %>% filter(prize_alias == input$prize1)
    p2 <- prizes %>% filter(prize_alias == input$prize2)

    comparison <- data.frame(
      Metric = c("Total Records", "Women %", "Men %", "UK Residents %",
                 "Winners", "Shortlisted", "Ethnic Minorities %"),
      Prize1 = c(nrow(p1),
                 round(sum(p1$gender == "woman") / nrow(p1) * 100, 1),
                 round(sum(p1$gender == "man") / nrow(p1) * 100, 1),
                 round(sum(p1$uk_residence) / nrow(p1) * 100, 1),
                 sum(p1$person_role == "winner"),
                 sum(p1$person_role == "shortlisted"),
                 round(sum(!p1$ethnicity_macro %in% c("White British", "Non-UK White")) / nrow(p1) * 100, 1)),
      Prize2 = c(nrow(p2),
                 round(sum(p2$gender == "woman") / nrow(p2) * 100, 1),
                 round(sum(p2$gender == "man") / nrow(p2) * 100, 1),
                 round(sum(p2$uk_residence) / nrow(p2) * 100, 1),
                 sum(p2$person_role == "winner"),
                 sum(p2$person_role == "shortlisted"),
                 round(sum(!p2$ethnicity_macro %in% c("White British", "Non-UK White")) / nrow(p2) * 100, 1))
    )

    colnames(comparison) <- c("Metric", input$prize1, input$prize2)
    datatable(comparison, options = list(dom = 't'), rownames = FALSE)
  })

  # TAB 7: DATA EXPLORER

  output$filtered_count <- renderText({
    paste("Showing", nrow(filtered_data()), "records based on current filters")
  })

  output$data_table <- renderDT({
    filtered_data() %>%
      select(prize_year, prize_alias, name, gender, ethnicity_macro,
             person_role, uk_residence, highest_degree, book_title) %>%
      datatable(options = list(pageLength = 25, searching = TRUE,
                               ordering = TRUE, scrollX = TRUE),
                rownames = FALSE, filter = "top")
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste("british_literary_prizes_filtered_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
