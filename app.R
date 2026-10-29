# ============================================================
# KENYA MAIZE PRODUCTION INTELLIGENCE DASHBOARD
# ============================================================

{
  library(shiny)
  library(bslib)
  library(bsicons)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(scales)
  library(ggplot2)
  library(ggrepel)
  library(sf)
  library(purrr)
  library(fitdistrplus)
  library(ggridges)
  library(viridis)
  library(leaflet)
  library(gganimate)
}


# ============================================================
# 1. GLOBAL SETTINGS
# ============================================================

options(
  scipen = 999,
  shiny.maxRequestSize = 50 * 1024^2
)

# ============================================================
# 2. DATA PREPARATION
# ============================================================

prepare_production_data <- function(path) {

  production <- read_csv(
    path,
    show_col_types = FALSE
  ) |>
    janitor::clean_names()

  # ----------------------------------------------------------
  # Standardise county names
  # ----------------------------------------------------------

  production <- production |>
    mutate(
      county = case_when(
        county %in% c(
          "Elgeyo/Marakwet",
          "Elgeyo Marakwet"
        ) ~ "Elgeyo-Marakwet",

        county == "Homabay" ~ "Homa Bay",

        county == "Taita/Taveta" ~ "Taita Taveta",

        county %in% c(
          "Tharaka-Nthi",
          "Tharaka Nithi"
        ) ~ "Tharaka-Nithi",

        TRUE ~ county
      )
    )

  # ----------------------------------------------------------
  # Standardise types
  # ----------------------------------------------------------

  production <- production |>
    mutate(
      year = as.integer(year),
      area_ha = as.numeric(area_ha),
      production_mt = as.numeric(production_mt),
      yield_mt_ha = as.numeric(yield_mt_ha)
    )

  # ----------------------------------------------------------
  # Treat zero yield as missing
  # ----------------------------------------------------------

  production <- production |>
    mutate(
      yield_mt_ha = if_else(
        yield_mt_ha <= 0,
        NA_real_,
        yield_mt_ha
      )
    )

  production
}


# ============================================================
# 3. NORMALISATION
# ============================================================

calculate_normalisation <- function(production) {

  annual_production <- production |>
    group_by(year) |>
    summarise(
      area = sum(area_ha, na.rm = TRUE),
      production = sum(production_mt, na.rm = TRUE),
      yield = production / area,
      .groups = "drop"
    )

  # ----------------------------------------------------------
  # Smooth national yield over time
  # ----------------------------------------------------------

  loess_model <- loess(
    yield ~ year,
    data = annual_production,
    span = 0.75
  )

  annual_production <- annual_production |>
    mutate(
      predicted_yield = predict(
        loess_model,
        newdata = data.frame(year = year)
      )
    )

  # ----------------------------------------------------------
  # Normalisation factor
  #
  # Latest observed yield becomes the reference level.
  # ----------------------------------------------------------

  reference_yield <- tail(
    annual_production$predicted_yield,
    1
  )

  annual_production <- annual_production |>
    mutate(
      norm_factor =
        reference_yield / predicted_yield
    )

  # ----------------------------------------------------------
  # Apply normalisation to county yields
  # ----------------------------------------------------------

  production_normalised <- production |>
    left_join(
      annual_production |>
        dplyr::select(year, norm_factor),
      by = "year"
    ) |>
    mutate(
      norm_yield =
        yield_mt_ha * norm_factor
    )

  list(
    production = production_normalised,
    annual = annual_production
  )
}


# ============================================================
# 4. COUNTY PRODUCTION LANDSCAPE
# ============================================================

calculate_landscape <- function(production) {

  county_summary <- production |>
    filter(
      !is.na(area_ha),
      area_ha > 0
    ) |>
    group_by(county) |>
    summarise(

      # Geometric mean of cultivated area
      land_scale = exp(
        mean(
          log(area_ha),
          na.rm = TRUE
        )
      ),

      # Mean normalised yield
      tech_efficiency = mean(
        norm_yield,
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  # ----------------------------------------------------------
  # Benchmarks
  # ----------------------------------------------------------

  median_land_scale <- median(
    county_summary$land_scale,
    na.rm = TRUE
  )

  homa_bay_eff <- county_summary |>
    filter(county == "Homa Bay") |>
    pull(tech_efficiency)

  # Fallback if Homa Bay is unavailable
  if (length(homa_bay_eff) == 0 ||
      is.na(homa_bay_eff)) {

    homa_bay_eff <- median(
      county_summary$tech_efficiency,
      na.rm = TRUE
    )
  }

  # ----------------------------------------------------------
  # Strategic classification
  # ----------------------------------------------------------

  anchor <- county_summary |>
    mutate(

      quadrant = case_when(

        land_scale >= median_land_scale &
          tech_efficiency >= homa_bay_eff ~
          "Structural Anchors",

        land_scale < median_land_scale &
          tech_efficiency >= homa_bay_eff ~
          "Hidden Gems",

        land_scale < median_land_scale &
          tech_efficiency < homa_bay_eff ~
          "Emerging Opportunities",

        land_scale >= median_land_scale &
          tech_efficiency < homa_bay_eff ~
          "Structural Deficit",

        TRUE ~ NA_character_
      )
    )

  list(
    data = anchor,
    median_land_scale = median_land_scale,
    homa_bay_eff = homa_bay_eff
  )
}


# ============================================================
# 5. GLOBAL DATA
# ============================================================

production_raw <- prepare_production_data(
  "data/maize-production-2012-2024-Combined.csv"
)

normalised <- calculate_normalisation(
  production_raw
)

landscape <- calculate_landscape(
  normalised$production
)

production <- normalised$production

anchor <- landscape$data

median_land_scale <- landscape$median_land_scale

homa_bay_eff <- landscape$homa_bay_eff


# ============================================================
# 6. UI
# ============================================================

# Reusable plot frame with a bottom-right expand/fullscreen control.
plot_frame <- function(output_ui, label = "Expand plot") {

  div(
    class = "plot-frame",

    output_ui,

    tags$button(
      type = "button",
      class = "plot-expand-btn",
      title = label,
      `aria-label` = label,
      "⛶"
    )
  )
}

ui <- page_fillable(

  theme = bs_theme(
    bootswatch = "minty",
    primary = "#449d95",
    secondary = "#ffbaba",
    info = "#bfddff",
    warning = "#feffba",
    light = "#c4ffb2",
    base_font = font_google("Roboto")
  ),

  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        overflow: visible;
      }

      .bslib-page-fill {
        height: 100vh !important;
        min-height: 100vh !important;
        overflow: visible !important;
      }

      .dashboard-title {
        margin-bottom: 0.35rem;
      }

      /* ------------------------------------------------------
         Fixed dashboard shell with ONE controlled scroll area
         inside the active top-level navigation panel.
         ------------------------------------------------------ */
      .dashboard-nav {
        height: calc(100vh - 105px);
        min-height: auto;
        overflow: visible;
      }

      .dashboard-nav .tab-content {
        min-height: 0;
      }

      .dashboard-nav > .navset-tab-content,
      .dashboard-nav .navset-tab-content,
      .dashboard-nav .tab-content {
        min-height: 0;
      }

      .dashboard-nav > div > .tab-content,
      .dashboard-nav > .tab-content {
        height: calc(100% - 48px);
        overflow: visible;
      }

      .dashboard-nav .tab-pane.active,
      .dashboard-nav .tab-pane.show.active {
        height: 100%;
        min-height: 0;
        overflow-y: auto;
        overflow-x: visible;
        padding: 0.15rem 0.45rem 1rem 0.15rem;
        scrollbar-width: thin;
        scrollbar-color: rgba(68, 157, 149, 0.55) transparent;
      }

      .dashboard-nav .tab-pane::-webkit-scrollbar {
        width: 8px;
      }

      .dashboard-nav .tab-pane::-webkit-scrollbar-thumb {
        background: rgba(68, 157, 149, 0.45);
        border-radius: 10px;
      }

      .dashboard-nav .tab-pane::-webkit-scrollbar-track {
        background: transparent;
      }

      /* ------------------------------------------------------
         Column-based analytical cards
         ------------------------------------------------------ */
      .analytics-grid {
        align-items: stretch;
        margin-bottom: 0.75rem;
      }

      .analytics-grid > .card,
      .analytics-grid > div > .card {
        height: 100%;
        margin-bottom: 0;
      }

      .plot-card {
        min-height: 0;
      }

      .plot-card .card-body {
        min-height: 0;
      }

      .plot-card .card-header {
        font-weight: 600;
      }

      .control-grid {
        margin-bottom: 0.65rem;
      }

      .metric-grid {
        margin-bottom: 0.75rem;
      }

      .compact-value-box {
        min-height: 88px !important;
      }

      .compact-value-box .value-box-title {
        font-size: 0.82rem;
      }

      .compact-value-box .value-box-value {
        font-size: 1.35rem;
      }

      /* ------------------------------------------------------
         Nested nav panels: they do not create another viewport;
         their active content expands naturally inside the outer
         scroll area, preventing bottom clipping.
         ------------------------------------------------------ */
      .dashboard-nav .bslib-navset-card-tab {
        min-height: 0;
        height: auto !important;
      }

      .dashboard-nav .bslib-navset-card-tab > .tab-content {
        min-height: 0;
        height: auto !important;
        overflow: visible !important;
      }

      .dashboard-nav .bslib-navset-card-tab .tab-pane {
        min-height: 0;
        height: auto !important;
        overflow: visible !important;
      }

      /* ------------------------------------------------------
         Responsive fallback: on narrower screens allow normal
         page scrolling rather than squeezing plots too tightly.
         ------------------------------------------------------ */
      @media (max-width: 900px) {
        html, body {
          overflow: auto;
        }

        .bslib-page-fill {
          overflow: auto !important;
        }

        .dashboard-nav {
          height: auto;
          min-height: 0;
          overflow: visible;
        }

        .dashboard-nav > div > .tab-content,
        .dashboard-nav > .tab-content {
          height: auto;
          overflow: visible;
        }

        .dashboard-nav .tab-pane.active,
        .dashboard-nav .tab-pane.show.active {
          height: auto;
          overflow: visible;
        }
      }

      /* ------------------------------------------------------
         Plot expansion control
         ------------------------------------------------------ */
      .plot-frame {
        position: relative;
        width: 100%;
        min-height: 0;
      }

      .plot-expand-btn {
        position: absolute;
        right: 12px;
        bottom: 12px;
        z-index: 1005;
        width: 34px;
        height: 34px;
        padding: 0;
        display: flex;
        align-items: center;
        justify-content: center;
        border: 1px solid rgba(0,0,0,.14);
        border-radius: 8px;
        background: rgba(255,255,255,.92);
        color: #495057;
        box-shadow: 0 2px 8px rgba(0,0,0,.14);
        cursor: pointer;
        font-size: 1.05rem;
        line-height: 1;
      }

      .plot-expand-btn:hover {
        background: #ffffff;
        transform: translateY(-1px);
      }

      .plot-frame:fullscreen {
        width: 100vw !important;
        height: 100vh !important;
        padding: 1rem;
        box-sizing: border-box;
        background: #ffffff;
        overflow: hidden;
      }

      .plot-frame:fullscreen .html-widget,
      .plot-frame:fullscreen .js-plotly-plot,
      .plot-frame:fullscreen .leaflet-container {
        width: 100% !important;
        height: calc(100vh - 2rem) !important;
      }

      .plot-frame:fullscreen .plot-expand-btn {
        right: 22px;
        bottom: 22px;
      }

      .segment-definition {
        margin-top: 0.2rem;
        font-size: 0.72rem;
        line-height: 1.18;
        font-weight: 400;
        opacity: 0.86;
      }
    ")),

    tags$script(HTML("
      document.addEventListener('click', function(event) {
        const button = event.target.closest('.plot-expand-btn');
        if (!button) return;

        const frame = button.closest('.plot-frame');
        if (!frame) return;

        if (!document.fullscreenElement) {
          if (frame.requestFullscreen) frame.requestFullscreen();
        } else if (document.exitFullscreen) {
          document.exitFullscreen();
        }
      });

      document.addEventListener('fullscreenchange', function() {
        window.dispatchEvent(new Event('resize'));

        setTimeout(function() {
          document.querySelectorAll('.plot-frame .js-plotly-plot').forEach(function(el) {
            if (window.Plotly && Plotly.Plots && Plotly.Plots.resize) {
              Plotly.Plots.resize(el);
            }
          });
        }, 150);
      });
    "))
  ),

  div(
    class = "dashboard-title",
    titlePanel("Kenya Maize Production Intelligence")
  ),

  div(
    class = "dashboard-nav",

    navset_tab(
      id = "main_dashboard_nav",

      # ========================================================
      # NATIONAL PRODUCTION INTELLIGENCE
      # ========================================================

      nav_panel(
        "National Production",

        layout_columns(
          col_widths = c(3, 3, 3, 3),

          value_box(
            "Top Producing County",
            textOutput("topCounty"),
            showcase = bs_icon("award"),
            theme_color = "light"
          ),

          value_box(
            "Lowest Producing County",
            textOutput("lowCounty"),
            showcase = bs_icon("arrow-down"),
            theme_color = "secondary"
          ),

          value_box(
            "National Production",
            textOutput("totalProd"),
            showcase = bs_icon("database"),
            theme_color = "info"
          ),

          value_box(
            "National Yield",
            textOutput("avgYield"),
            showcase = bs_icon("bar-chart"),
            theme_color = "warning"
          )
        ),


        layout_columns(
          col_widths = c(4, 4, 4),

          selectInput(
            "varInput",
            "National Indicator",
            choices = c(
              "Production" = "production_mt",
              "Yield" = "yield_mt_ha",
              "Cultivated Area" = "area_ha"
            ),
            selected = "production_mt"
          ),

          selectInput(
            "yearAvg",
            "Analysis Year",
            choices = sort(unique(production$year)),
            selected = max(production$year)
          ),

          selectInput(
            "producerView",
            "Producer Ranking",
            choices = c(
              "Top 5 Producers" = "top",
              "Bottom 5 Producers" = "bottom"
            ),
            selected = "top"
          )
        ),

        navset_card_tab(
          id = "national_analysis_tabs",

          nav_panel(
            "National Performance",
            layout_columns(
              col_widths = c(6, 6),

              card(
                card_header("National Performance Trend"),
                plot_frame(
              plotlyOutput(
                  "trendPlot",
                  height = "480px",
                  width = "100%"
                              )
            )
              ),

              card(
                card_header("Top and Bottom County Producers"),
                plot_frame(
              plotlyOutput(
                  "producerRankingPlot",
                  height = "480px",
                  width = "100%"
                              )
            )
              )
            )
          ),

          nav_panel(
            "Production Structure",
            layout_columns(
              col_widths = c(6, 6),

              card(
                card_header("Production vs Cultivated Area"),
                plot_frame(
              plotlyOutput(
                  "scatterProdArea",
                  height = "500px",
                  width = "100%"
                              )
            )
              ),

              card(
                card_header("Year-on-Year Change in Selected Metric"),
                plot_frame(
              plotlyOutput(
                  "pctChange",
                  height = "500px",
                  width = "100%"
                              )
            )
              )
            )
          )
        )
      ),

      # ========================================================
      # COUNTY PERFORMANCE ANALYTICS
      # ========================================================

      nav_panel(
        "County Performance",

        layout_columns(
          col_widths = c(4, 4, 4),

          selectInput(
            "varInputCounty",
            "County Metric",
            choices = c(
              "Production" = "production_mt",
              "Yield" = "yield_mt_ha",
              "Cultivated Area" = "area_ha"
            ),
            selected = "production_mt"
          ),

          selectInput(
            "countyYear",
            "County Ranking Year",
            choices = sort(unique(production$year)),
            selected = max(production$year)
          ),

          selectInput(
            "countySelect",
            "Counties for Trend Comparison",
            choices = sort(unique(production$county)),
            selected = head(sort(unique(production$county)), 4),
            multiple = TRUE
          )
        ),

        navset_card_tab(
          id = "county_analysis_tabs",

          nav_panel(
            "County Performance",
            layout_columns(
              col_widths = c(6, 6),

              card(
                card_header("County Performance Ranking"),
                plot_frame(
              plotlyOutput(
                  "avgCountyBar",
                  height = "500px",
                  width = "100%"
                              )
            )
              ),

              card(
                card_header("Multi-County Trend Comparison"),
                plot_frame(
              plotlyOutput(
                  "multiCountyCompare",
                  height = "500px",
                  width = "100%"
                              )
            )
              )
            )
          ),

          nav_panel(
            "Yield Landscape",
            layout_columns(
              col_widths = c(12),

              card(
                card_header("County Yield Heatmap"),
                plot_frame(
              plotlyOutput(
                  "yieldHeatmap",
                  height = "650px",
                  width = "100%"
                              )
            )
              )
            )
          )
        )
      ),

      # ========================================================
      # STRATEGIC LANDSCAPE
      # ========================================================

      nav_panel(
        "Strategic Landscape",

        layout_columns(
          col_widths = c(3, 3, 3, 3),

          value_box(
            "Structural Anchors",
            div(
              textOutput("nAnchors"),
              div(
                class = "segment-definition",
                "Large cultivated land scale + high technical efficiency"
              )
            ),
            theme_color = "primary"
          ),

          value_box(
            "Hidden Gems",
            div(
              textOutput("nGems"),
              div(
                class = "segment-definition",
                "Smaller cultivated land scale + high technical efficiency"
              )
            ),
            theme_color = "success"
          ),

          value_box(
            "Emerging Opportunities",
            div(
              textOutput("nOpportunities"),
              div(
                class = "segment-definition",
                "Smaller cultivated land scale + lower technical efficiency"
              )
            ),
            theme_color = "warning"
          ),

          value_box(
            "Structural Deficit",
            div(
              textOutput("nDeficit"),
              div(
                class = "segment-definition",
                "Large cultivated land scale + lower technical efficiency"
              )
            ),
            theme_color = "danger"
          )
        ),

        layout_columns(
          col_widths = c(12),

          card(
            card_header("Kenya's Maize Production Landscape"),
            plot_frame(
              plotlyOutput(
              "strategicLandscape",
              height = "650px",
              width = "100%"
                          )
            )
          )
        )
      ),

      # ========================================================
      # NORMALISED YIELD
      # ========================================================

      nav_panel(
        "Normalised Yield",

        navset_card_tab(
          id = "normalisation_tabs",

          nav_panel(
            "Yield Benchmark",
            layout_columns(
              col_widths = c(12),

              card(
                card_header("Observed vs Smoothed National Yield"),
                plot_frame(
              plotlyOutput(
                  "normalisationPlot",
                  height = "620px",
                  width = "100%"
                              )
            )
              )
            )
          ),

          nav_panel(
            "Adjustment Factors",
            layout_columns(
              col_widths = c(12),

              card(
                card_header("Annual Yield Normalisation Factor"),
                plot_frame(
              plotlyOutput(
                  "normalisationFactorPlot",
                  height = "620px",
                  width = "100%"
                              )
            )
              )
            )
          )
        )
      ),

      # ========================================================
      # STOCHASTIC YIELD
      # ========================================================

      nav_panel(
        "Stochastic Yield",

        layout_columns(
          col_widths = c(4, 4, 4),

          selectInput(
            "betaCounty",
            "Select County",
            choices = sort(unique(production$county))
          ),

          numericInput(
            "betaCeiling",
            "Biological Ceiling Multiplier",
            value = 1.15,
            min = 1,
            max = 3,
            step = 0.05
          ),

          sliderInput(
            "probYield",
            "Yield Threshold",
            min = 0,
            max = 5,
            value = 2,
            step = 0.1
          )
        ),

        layout_columns(
          col_widths = c(8, 4),
          class = "analytics-grid",

          card(
            class = "plot-card",
            card_header("Probabilistic Yield Distribution"),
            plot_frame(
              plotlyOutput(
                "betaDensity",
                height = "620px",
                width = "100%"
              )
            )
          ),

          card(
            class = "plot-card",
            card_header("Probability of Exceeding Yield Threshold"),
            plot_frame(
              plotlyOutput(
                "yieldProbability",
                height = "620px",
                width = "100%"
              )
            )
          )
        )
      ),

      # ========================================================
      # SPATIAL ANALYSIS
      # ========================================================

      nav_panel(
        "Spatial Analysis",

        layout_columns(
          col_widths = c(12),

          card(
            card_header("Spatial Distribution of Maize Production Segments"),
            plot_frame(
              leafletOutput(
              "maizeMap",
              height = "calc(100vh - 190px)",
              width = "100%"
                          )
            )
          )
        )
      ),

      # ========================================================
      # DYNAMIC TRENDS
      # ========================================================

      nav_panel(
        "Dynamic Trends",

        layout_columns(
          col_widths = c(4, 4, 4),

          selectInput(
            "trendQuadrant",
            "Strategic Segment",
            choices = c(
              "Structural Anchors",
              "Hidden Gems",
              "Emerging Opportunities",
              "Structural Deficit"
            ),
            selected = "Structural Anchors"
          ),

          selectInput(
            "trendVariable",
            "Variable",
            choices = c(
              "Production" = "production_mt",
              "Yield" = "norm_yield",
              "Area" = "area_ha"
            ),
            selected = "production_mt"
          ),

          checkboxInput(
            "showTrendLabels",
            "Show County Labels",
            TRUE
          )
        ),

        layout_columns(
          col_widths = c(12),

          card(
            card_header("Dynamic County Trends"),
            plot_frame(
              plotlyOutput(
              "dynamicTrend",
              height = "620px",
              width = "100%"
                          )
            )
          )
        )
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # ==========================================================
  # EXISTING DASHBOARD FUNCTIONALITY
  # ==========================================================

  output$topCounty <- renderText({

    req(input$yearAvg)

    production |>
      filter(year == input$yearAvg) |>
      group_by(county) |>
      summarise(
        production = sum(
          production_mt,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      slice_max(
        production,
        n = 1,
        with_ties = FALSE
      ) |>
      pull(county)
  })


  output$lowCounty <- renderText({

    req(input$yearAvg)

    production |>
      filter(year == input$yearAvg) |>
      group_by(county) |>
      summarise(
        production = sum(
          production_mt,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      slice_min(
        production,
        n = 1,
        with_ties = FALSE
      ) |>
      pull(county)
  })


  output$totalProd <- renderText({

    production |>
      filter(year == input$yearAvg) |>
      summarise(
        total = sum(
          production_mt,
          na.rm = TRUE
        )
      ) |>
      pull(total) |>
      comma() |>
      paste0(" Mt")
  })


  output$avgYield <- renderText({

    production |>
      filter(year == input$yearAvg) |>
      summarise(

        national_yield =
          sum(
            production_mt,
            na.rm = TRUE
          ) /
          sum(
            area_ha,
            na.rm = TRUE
          )

      ) |>
      pull(national_yield) |>
      round(2) |>
      paste0(" Mt/Ha")
  })


  # ==========================================================
  # NATIONAL TREND
  # ==========================================================

  output$trendPlot <- renderPlotly({

    req(input$varInput)

    trend <- production |>
      group_by(year) |>
      summarise(

        value = case_when(

          input$varInput == "yield_mt_ha" ~

            sum(
              production_mt,
              na.rm = TRUE
            ) /
            sum(
              area_ha,
              na.rm = TRUE
            ),

          TRUE ~

            sum(
              .data[[input$varInput]],
              na.rm = TRUE
            )
        ),

        .groups = "drop"
      )

    plot_ly(
      trend,
      x = ~year,
      y = ~value,
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 3),
      marker = list(size = 7)
    ) |>

      layout(
        title = paste(
          "National",
          tools::toTitleCase(
            gsub(
              "_",
              " ",
              input$varInput
            )
          ),
          "Trend"
        ),

        xaxis = list(
          title = "Year"
        ),

        yaxis = list(
          title = gsub(
            "_",
            " ",
            input$varInput
          )
        )
      )
  })


  # ==========================================================
  # YEARLY % CHANGE
  # ==========================================================

  output$pctChange <- renderPlotly({

    req(input$varInput)

    metric_label <- switch(
      input$varInput,
      "production_mt" = "Production",
      "yield_mt_ha" = "Yield",
      "area_ha" = "Cultivated Area"
    )

    trend <- production |>
      group_by(year) |>
      summarise(

        value = case_when(

          input$varInput == "yield_mt_ha" ~
            sum(production_mt, na.rm = TRUE) /
            sum(area_ha, na.rm = TRUE),

          TRUE ~
            sum(.data[[input$varInput]], na.rm = TRUE)
        ),

        .groups = "drop"
      ) |>
      mutate(
        pct_change = (value / lag(value) - 1) * 100
      )

    plot_ly(
      trend,
      x = ~year,
      y = ~pct_change,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        metric_label, ": %{y:.2f}%<extra></extra>"
      )
    ) |>

      layout(
        title = paste("Year-on-Year", metric_label, "Change"),

        xaxis = list(
          title = "Year"
        ),

        yaxis = list(
          title = "Percentage Change (%)"
        )
      )
  })


  # ==========================================================
  # COUNTY COMPARISON
  # ==========================================================

  output$multiCountyCompare <- renderPlotly({

    req(input$countySelect)

    dat <- production |>
      filter(
        county %in% input$countySelect
      ) |>
      group_by(
        county,
        year
      ) |>
      summarise(

        value =
          if (
            input$varInputCounty ==
            "yield_mt_ha"
          ) {

            sum(
              production_mt,
              na.rm = TRUE
            ) /
            sum(
              area_ha,
              na.rm = TRUE
            )

          } else {

            sum(
              .data[[input$varInputCounty]],
              na.rm = TRUE
            )
          },

        .groups = "drop"
      )

    metric_label <- switch(
      input$varInputCounty,
      "production_mt" = "Production",
      "yield_mt_ha" = "Yield",
      "area_ha" = "Cultivated Area"
    )

    y_label <- switch(
      input$varInputCounty,
      "production_mt" = "Production (Metric Tonnes)",
      "yield_mt_ha" = "Yield (Mt/Ha)",
      "area_ha" = "Cultivated Area (Ha)"
    )

    plot_ly(
      dat,
      x = ~year,
      y = ~value,
      color = ~county,
      type = "scatter",
      mode = "lines+markers"
    ) |>

      layout(
        title = paste("Multi-County", metric_label, "Trend Comparison"),
        xaxis = list(title = "Year"),
        yaxis = list(title = y_label)
      )
  })


  # ==========================================================
  # PRODUCTION VS AREA
  # ==========================================================

  output$scatterProdArea <- renderPlotly({

    req(input$yearAvg)

    dat <- production |>
      filter(year == input$yearAvg) |>
      group_by(county) |>
      summarise(
        area_ha = sum(area_ha, na.rm = TRUE),
        production_mt = sum(production_mt, na.rm = TRUE),
        yield_mt_ha = production_mt / area_ha,
        .groups = "drop"
      ) |>
      filter(area_ha > 0, production_mt >= 0, is.finite(yield_mt_ha))

    plot_ly(
      dat,
      x = ~area_ha,
      y = ~production_mt,
      text = ~county,
      type = "scatter",
      mode = "markers",
      size = ~yield_mt_ha,
      hovertemplate =
        paste(
          "<b>%{text}</b><br>",
          "Area: %{x:,.0f} ha<br>",
          "Production: %{y:,.0f} Mt<br>",
          "<extra></extra>"
        )
    ) |>

      layout(
        title =
          paste(
            "Production vs Cultivated Area —",
            input$yearAvg
          ),

        xaxis = list(
          title = "Cultivated Area (Ha)"
        ),

        yaxis = list(
          title = "Production (Metric Tonnes)"
        )
      )
  })


  # ==========================================================
  # YIELD HEATMAP
  # ==========================================================

  output$yieldHeatmap <- renderPlotly({

    dat <- production |>
      group_by(
        county,
        year
      ) |>
      summarise(

        yield =
          sum(
            production_mt,
            na.rm = TRUE
          ) /
          sum(
            area_ha,
            na.rm = TRUE
          ),

        .groups = "drop"
      )

    plot_ly(
      dat,
      x = ~year,
      y = ~county,
      z = ~yield,
      type = "heatmap",
      colorscale = "YlGn",
      colorbar = list(
        title = "Mt/Ha"
      )
    ) |>

      layout(
        title = "County Yield Heatmap",

        xaxis = list(
          title = "Year"
        ),

        yaxis = list(
          title = "County"
        )
      )
  })


  # ==========================================================
  # COUNTY AVERAGES
  # ==========================================================

  output$avgCountyBar <- renderPlotly({

    req(input$countyYear, input$varInputCounty)

    dat <- production |>
      filter(year == input$countyYear) |>
      group_by(county) |>
      summarise(
        value = if (input$varInputCounty == "yield_mt_ha") {
          sum(production_mt, na.rm = TRUE) /
            sum(area_ha, na.rm = TRUE)
        } else {
          sum(.data[[input$varInputCounty]], na.rm = TRUE)
        },
        .groups = "drop"
      ) |>
      filter(is.finite(value)) |>
      arrange(value)

    metric_label <- switch(
      input$varInputCounty,
      "production_mt" = "Production (Metric Tonnes)",
      "yield_mt_ha" = "Yield (Mt/Ha)",
      "area_ha" = "Cultivated Area (Ha)"
    )

    plot_ly(
      dat,
      x = ~value,
      y = ~reorder(county, value),
      type = "bar",
      orientation = "h",
      marker = list(
        color = "#4F772D",
        line = list(color = "#31572C", width = 0.5)
      ),
      text = ~paste0(
        "<b>", county, "</b><br>",
        metric_label, ": ", scales::comma(round(value, 2))
      ),
      textposition = "none",
      hovertemplate = "%{text}<extra></extra>"
    ) |>
      layout(
        title = paste("County Ranking —", input$countyYear),
        xaxis = list(title = metric_label),
        yaxis = list(title = "")
      )
  })


  # ==========================================================
  # PRODUCER RANKING
  # ==========================================================

  output$producerRankingPlot <- renderPlotly({

    req(input$yearAvg, input$producerView)

    dat <- production |>
      filter(year == input$yearAvg) |>
      group_by(county) |>
      summarise(
        production = sum(production_mt, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(is.finite(production))

    if (input$producerView == "top") {
      dat <- dat |>
        slice_max(production, n = 5, with_ties = FALSE) |>
        arrange(production)
      plot_title <- paste("Top 5 Maize Producers —", input$yearAvg)
    } else {
      dat <- dat |>
        slice_min(production, n = 5, with_ties = FALSE) |>
        arrange(desc(production))
      plot_title <- paste("Bottom 5 Maize Producers —", input$yearAvg)
    }

    plot_ly(
      dat,
      x = ~production,
      y = ~reorder(county, production),
      type = "bar",
      orientation = "h",
      marker = list(
        color = "#4F772D",
        line = list(color = "#31572C", width = 0.5)
      ),
      text = ~paste0(
        "<b>", county, "</b><br>",
        "Production: ", scales::comma(round(production)), " metric tonnes"
      ),
      textposition = "none",
      hovertemplate = "%{text}<extra></extra>"
    ) |>
      layout(
        title = plot_title,
        xaxis = list(title = "Production (Metric Tonnes)"),
        yaxis = list(title = "")
      )
  })


  # ==========================================================
  # STRATEGIC LANDSCAPE
  # ==========================================================

  output$nAnchors <- renderText({
    sum(anchor$quadrant == "Structural Anchors")
  })

  output$nGems <- renderText({
    sum(anchor$quadrant == "Hidden Gems")
  })

  output$nOpportunities <- renderText({
    sum(anchor$quadrant == "Emerging Opportunities")
  })

  output$nDeficit <- renderText({
    sum(anchor$quadrant == "Structural Deficit")
  })


  output$strategicLandscape <- renderPlotly({

    p <- ggplot(
      anchor,
      aes(
        x = land_scale,
        y = tech_efficiency,
        text = paste0(
          "<b>", county, "</b><br>",
          "Strategic Segment: ", quadrant, "<br>",
          "Cultivated Land: ",
          comma(round(land_scale)), " ha<br>",
          "Technical Efficiency: ",
          round(tech_efficiency, 2),
          " Mt/ha"
        )
      )
    ) +

      geom_vline(
        xintercept = median_land_scale,
        linetype = "dotted",
        colour = "grey50"
      ) +

      geom_hline(
        yintercept = homa_bay_eff,
        linetype = "dotted",
        colour = "grey50"
      ) +

      annotate(
        "text",
        x = median_land_scale,
        y = max(anchor$tech_efficiency, na.rm = TRUE) * 0.96,
        label = "Median land scale",
        angle = 90,
        vjust = -0.5,
        hjust = 1,
        size = 3.8,
        colour = "grey35",
        fontface = "bold"
      ) +

      annotate(
        "text",
        x = max(anchor$land_scale, na.rm = TRUE) * 0.96,
        y = homa_bay_eff,
        label = "Technical efficiency benchmark",
        hjust = 1,
        vjust = -0.6,
        size = 3.5,
        colour = "grey35",
        fontface = "bold"
      ) +

      geom_point(
        aes(
          size = land_scale,
          fill = quadrant
        ),
        shape = 21,
        colour = "white",
        stroke = 0.8
      ) +

      geom_label_repel(
        aes(label = county),
        size = 3.5,
        show.legend = FALSE
      ) +

      scale_fill_manual(
        name = "Strategic Segment",
        values = c(
          "Structural Anchors" =
            "#1B9E77",

          "Hidden Gems" =
            "#F4A261",

          "Emerging Opportunities" =
            "#2D7FF9",

          "Structural Deficit" =
            "#D62828"
        )
      ) +

      scale_size_continuous(
        name = "Cultivated Land Scale (ha)",
        labels = scales::comma
      ) +

      scale_x_continuous(
        labels = comma
      ) +

      scale_y_continuous(
        labels = number_format(
          accuracy = 0.1
        )
      ) +

      labs(
        title =
          "Kenya's Maize Production Landscape",

        subtitle =
          "Counties classified according to cultivated land scale and technical efficiency",

        x = "Cultivated Land (Ha)",

        y = "Technical Efficiency (Mt/Ha)",

        fill = "Strategic Segment"
      ) +

      theme_minimal(
        base_size = 15
      )

    ggplotly(
      p,
      tooltip = "text"
    ) |>

      layout(
        legend = list(
          orientation = "v",
          x = 0.99,
          xanchor = "right",
          y = 0.99,
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.84)",
          bordercolor = "rgba(0,0,0,0.12)",
          borderwidth = 1
        ),
        margin = list(r = 25, t = 75, b = 55, l = 70)
      )
  })


  # ==========================================================
  # NORMALISATION PLOT
  # ==========================================================

  output$normalisationPlot <- renderPlotly({

    dat <- normalised$annual |>
      dplyr::select(
        year,
        yield,
        predicted_yield
      ) |>
      pivot_longer(
        cols = c(
          yield,
          predicted_yield
        ),
        names_to = "series",
        values_to = "value"
      ) |>
      mutate(
        series = recode(
          series,
          yield = "Observed Yield",
          predicted_yield = "Predicted Yield"
        )
      )

    plot_ly(
      dat,
      x = ~year,
      y = ~value,
      color = ~series,
      type = "scatter",
      mode = "lines+markers"
    ) |>

      layout(
        title = "Observed vs Smoothed National Yield",
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "Yield (Mt/Ha)"
        )
      )
  })


  output$normalisationFactorPlot <- renderPlotly({

    dat <- normalised$annual

    plot_ly(
      dat,
      x = ~year,
      y = ~norm_factor,
      type = "scatter",
      mode = "lines+markers"
    ) |>

      layout(
        title = "Annual Yield Normalisation Factor",
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "Normalisation Factor"
        )
      )
  })


  # ==========================================================
  # STAGE 3 — BETA DISTRIBUTION
  # ==========================================================

  beta_model <- reactive({

    req(input$betaCounty)

    dat <- production |>
      filter(
        county == input$betaCounty,
        !is.na(norm_yield)
      )

    req(nrow(dat) >= 3)

    pmin <- 0

    pmax <-
      max(
        dat$norm_yield,
        na.rm = TRUE
      ) *
      input$betaCeiling

    scaled <- (
      dat$norm_yield - pmin
    ) / (
      pmax - pmin
    )

    scaled <- pmin(
      pmax(
        scaled,
        0.001
      ),
      0.999
    )

    fit <- fitdist(
      scaled,
      "beta",
      method = "mle"
    )

    list(
      fit = fit,
      pmin = pmin,
      pmax = pmax
    )
  })


  output$betaDensity <- renderPlotly({

    model <- beta_model()

    grid <- seq(
      0.001,
      0.999,
      length.out = 300
    )

    density <- dbeta(
      grid,
      shape1 = model$fit$estimate[["shape1"]],
      shape2 = model$fit$estimate[["shape2"]]
    )

    actual_yield <-
      model$pmin +
      grid *
      (
        model$pmax -
          model$pmin
      )

    dat <- data.frame(
      yield = actual_yield,
      density = density
    )

    plot_ly(
      dat,
      x = ~yield,
      y = ~density,
      type = "scatter",
      mode = "lines",
      fill = "tozeroy"
    ) |>

      layout(
        title =
          paste(
            "Probabilistic Yield Distribution —",
            input$betaCounty
          ),

        xaxis = list(
          title = "Normalised Yield (Mt/Ha)"
        ),

        yaxis = list(
          title = "Probability Density"
        )
      )
  })


  output$yieldProbability <- renderPlotly({

    model <- beta_model()

    threshold_scaled <- (
      input$probYield -
        model$pmin
    ) / (
      model$pmax -
        model$pmin
    )

    threshold_scaled <- max(
      0,
      min(
        1,
        threshold_scaled
      )
    )

    probability <- 1 -
      pbeta(
        threshold_scaled,
        shape1 =
          model$fit$estimate[["shape1"]],
        shape2 =
          model$fit$estimate[["shape2"]]
      )

    dat <- data.frame(
      probability = probability
    )

    plot_ly(
      dat,
      x = ~probability,
      y = "Probability",
      type = "bar",
      orientation = "h",
      marker = list(
        color = "#4F772D",
        line = list(color = "#31572C", width = 0.5)
      ),
      hovertemplate = "Probability of exceeding threshold: %{x:.1%}<extra></extra>"
    ) |>

      layout(
        title =
          paste0(
            "P(Yield ≥ ",
            input$probYield,
            ") = ",
            round(
              probability * 100,
              1
            ),
            "%"
          ),

        xaxis = list(
          title = "Probability",
          range = c(0, 1)
        ),

        yaxis = list(
          title = ""
        )
      )
  })


  # ==========================================================
  # SPATIAL ANALYSIS
  # ==========================================================

  kenya_shp <- reactive({

    st_read(
      "data/gadm41_KEN_shp/gadm41_KEN_1.shp",
      quiet = TRUE
    )
  })


  output$maizeMap <- renderLeaflet({

    shp <- kenya_shp() |>
      st_transform(4326)

    map_data <- shp |>
      left_join(
        anchor,
        by = c(
          "NAME_1" = "county"
        )
      )

    bbox <- st_bbox(shp)

    pal <- colorFactor(
      palette = c(
        "#1B9E77",
        "#F4A261",
        "#2D7FF9",
        "#D62828"
      ),
      domain = map_data$quadrant
    )

    leaflet(
      map_data,
      options = leafletOptions(
        minZoom = 5,
        maxZoom = 10,
        worldCopyJump = FALSE,
        maxBoundsViscosity = 1
      )
    ) |>

      fitBounds(
        lng1 = bbox[["xmin"]],
        lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]],
        lat2 = bbox[["ymax"]]
      ) |>

      setMaxBounds(
        lng1 = bbox[["xmin"]],
        lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]],
        lat2 = bbox[["ymax"]]
      ) |>

      addProviderTiles(
        providers$OpenStreetMap.Mapnik
      ) |>

      addPolygons(
        fillColor =
          ~pal(quadrant),

        weight = 1,

        color = "white",

        fillOpacity = 0.8,

        popup = ~paste0(
          "<strong>",
          NAME_1,
          "</strong><br>",
          "Segment: ",
          quadrant,
          "<br>",
          "Land scale: ",
          comma(
            round(
              land_scale
            )
          ),
          " ha<br>",
          "Technical efficiency: ",
          round(
            tech_efficiency,
            2
          ),
          " Mt/ha"
        )
      ) |>

      addLegend(
        position = "topright",
        pal = pal,
        values = ~quadrant,
        title = "Quadrant"
      )
  })


  # ==========================================================
  # DYNAMIC TREND ANALYSIS
  # ==========================================================

  trend_data <- reactive({

    production |>

      left_join(
        anchor |>
          dplyr::select(
            county,
            quadrant
          ),
        by = "county"
      ) |>

      filter(
        quadrant ==
          input$trendQuadrant
      ) |>

      group_by(
        county,
        year
      ) |>

      summarise(

        value = case_when(

          input$trendVariable ==
            "yield_mt_ha" ~

            sum(
              production_mt,
              na.rm = TRUE
            ) /
            sum(
              area_ha,
              na.rm = TRUE
            ),

          TRUE ~

            sum(
              .data[[input$trendVariable]],
              na.rm = TRUE
            )
        ),

        .groups = "drop"
      )
  })


  output$dynamicTrend <- renderPlotly({

    dat <- trend_data()

    p <- ggplot(
      dat,
      aes(
        x = year,
        y = value,
        group = county,
        colour = county
      )
    ) +

      geom_line(
        linewidth = 1.2
      ) +

      geom_point(
        size = 2
      ) +
      
      scale_x_continuous(breaks = sort(unique(dat$year))) +

      labs(
        title =
          paste(
            input$trendQuadrant,
            "— Dynamic Trend"
          ),

        x = "Year",

        y =
          tools::toTitleCase(
            gsub(
              "_",
              " ",
              input$trendVariable
            )
          ),

        colour = "County"
      ) +

      theme_minimal(
        base_size = 14
      )

    ggplotly(
      p,
      tooltip = c(
        "county",
        "year",
        "value"
      )
    )
  })
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
