# =============================================================================
# Module: Golf This Week
# 
# Weekly analysis and projections comparison for golf tournaments:
# - Compare projection distributions across all available weeks
# - Identify high/low scoring weeks for transfer timing
# - Value scatter plots with player headshots
# - Strip plot showing top N player averages by tournament
# =============================================================================

# =============================================================================
# CONSTANTS
# =============================================================================

if (!exists("GOLF_DEFAULT_HEADSHOT")) {
  GOLF_DEFAULT_HEADSHOT <- "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=200&h=146"
}

if (!exists("GOLF_CARD_COLOR")) {
  GOLF_CARD_COLOR <- "gold"
}

# Google Sheet IDs (same as season management)
GOLF_THIS_WEEK_PROJECTIONS_SHEET_ID <- "1yJJAOv5hzNZagYUG7FLpNmRIRC76L0fJNGPbzK61lbw"

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Get available tournaments from projections sheet
get_this_week_tournaments <- function() {
  tryCatch({
    googlesheets4::gs4_deauth()
    sheets <- googlesheets4::sheet_names(GOLF_THIS_WEEK_PROJECTIONS_SHEET_ID)
    # Filter to tournament sheets (exclude any config/metadata sheets)
    tournaments <- sheets[!grepl("^_|config|template", sheets, ignore.case = TRUE)]
    return(tournaments)
  }, error = function(e) {
    log_debug("Error getting tournaments:", e$message, level = "ERROR")
    return(character(0))
  })
}

#' Load projections for a single tournament
load_this_week_projections <- function(tournament_name) {
  tryCatch({
    googlesheets4::gs4_deauth()
    data <- googlesheets4::read_sheet(
      GOLF_THIS_WEEK_PROJECTIONS_SHEET_ID,
      sheet = tournament_name
    )
    
    if (nrow(data) == 0) return(NULL)
    
    # Standardize column names
    names(data) <- tolower(names(data))
    
    # Find projection and salary columns
    proj_col <- names(data)[grepl("proj|points|pts", names(data), ignore.case = TRUE)][1]
    salary_col <- names(data)[grepl("salary|price|cost", names(data), ignore.case = TRUE)][1]
    name_col <- names(data)[grepl("player|name|golfer", names(data), ignore.case = TRUE)][1]
    
    if (is.na(proj_col) || is.na(name_col)) {
      log_debug("Missing required columns in", tournament_name, level = "WARN")
      return(NULL)
    }
    
    result <- data.frame(
      player_name = as.character(data[[name_col]]),
      projection = as.numeric(data[[proj_col]]),
      salary = if (!is.na(salary_col)) as.numeric(data[[salary_col]]) else NA_real_,
      tournament = tournament_name,
      stringsAsFactors = FALSE
    )
    
    # Remove rows with missing projections
    result <- result[!is.na(result$projection), ]
    
    return(result)
  }, error = function(e) {
    log_debug("Error loading projections for", tournament_name, ":", e$message, level = "ERROR")
    return(NULL)
  })
}

# =============================================================================
# UI
# =============================================================================

golf_this_week_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # =========================================================================
    # TOURNAMENT SELECTION
    # =========================================================================
    ui_card(
      title = "Tournament Selection",
      color = GOLF_CARD_COLOR,
      
      fluidRow(
        column(4,
               selectInput(ns("current_tournament"), "Current Tournament",
                           choices = c("Loading..." = ""),
                           selected = "")
        ),
        column(4,
               actionButton(ns("load_all_weeks_btn"), "Load All Weeks", 
                            class = "btn btn-primary", icon = icon("download"),
                            style = "margin-top: 25px;")
        ),
        column(4,
               uiOutput(ns("load_status"))
        )
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # WEEKLY PROJECTIONS COMPARISON
    # =========================================================================
    ui_card(
      title = "Weekly Projections Comparison",
      color = GOLF_CARD_COLOR,
      
      # Controls row
      fluidRow(
        column(2,
               selectInput(ns("viz_type"), "View Type",
                           choices = c("Histogram" = "histogram", 
                                       "Value Scatter" = "scatter",
                                       "Top N Strip" = "strip"),
                           selected = "histogram")
        ),
        column(2,
               numericInput(ns("min_projection"), "Min Projection",
                            value = 30, min = 0, max = 70, step = 5)
        ),
        column(8,
               # Custom legend - built as HTML above charts
               div(
                 style = "display: flex; align-items: center; justify-content: flex-end; gap: 1rem; padding-top: 25px;",
                 span(style = "font-size: 0.7rem; font-weight: 600; color: var(--text-muted); text-transform: uppercase;", "Projection Tier:"),
                 div(style = "display: flex; align-items: center; gap: 0.25rem;",
                     span(style = "width: 14px; height: 14px; background: #E5E9F0; border: 1px solid #ccc; border-radius: 2px; display: inline-block;"),
                     span(style = "font-size: 0.7rem; color: var(--text-secondary);", "<40")
                 ),
                 div(style = "display: flex; align-items: center; gap: 0.25rem;",
                     span(style = "width: 14px; height: 14px; background: #A8C5D4; border: 1px solid #8ab; border-radius: 2px; display: inline-block;"),
                     span(style = "font-size: 0.7rem; color: var(--text-secondary);", "40-50")
                 ),
                 div(style = "display: flex; align-items: center; gap: 0.25rem;",
                     span(style = "width: 14px; height: 14px; background: #A3BE8C; border: 1px solid #8a9; border-radius: 2px; display: inline-block;"),
                     span(style = "font-size: 0.7rem; color: var(--text-secondary);", "50-60")
                 ),
                 div(style = "display: flex; align-items: center; gap: 0.25rem;",
                     span(style = "width: 14px; height: 14px; background: #EBCB8B; border: 1px solid #ca9; border-radius: 2px; display: inline-block;"),
                     span(style = "font-size: 0.7rem; color: var(--text-secondary);", "60-70")
                 ),
                 div(style = "display: flex; align-items: center; gap: 0.25rem;",
                     span(style = "width: 14px; height: 14px; background: #D08770; border: 1px solid #b76; border-radius: 2px; display: inline-block;"),
                     span(style = "font-size: 0.7rem; color: var(--text-secondary);", "70+")
                 )
               )
        )
      ),
      
      # Visualization
      uiOutput(ns("chart_wrapper")),
      
      # Summary assessment card below charts
      div(
        style = "margin-top: 1rem; padding-top: 1rem; border-top: 3px solid var(--text-primary);",
        uiOutput(ns("assessment_card"))
      )
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

golf_this_week_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("golf_this_week_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    rv <- reactiveValues(
      all_projections = NULL,
      current_tournament = NULL,
      tournaments_loaded = FALSE
    )
    
    # =========================================================================
    # POPULATE TOURNAMENT DROPDOWN
    # =========================================================================
    observe({
      tournaments <- tryCatch(get_this_week_tournaments(), error = function(e) character(0))
      
      if (length(tournaments) > 0) {
        updateSelectInput(session, "current_tournament",
                          choices = tournaments,
                          selected = tournaments[length(tournaments)])  # Default to last (most recent)
      }
    })
    
    # =========================================================================
    # LOAD ALL WEEKS
    # =========================================================================
    observeEvent(input$load_all_weeks_btn, {
      log_debug(">>> Loading all weeks projections", level = "INFO")
      
      showNotification("Loading all tournament projections...", type = "message", duration = 3)
      
      tournaments <- tryCatch(get_this_week_tournaments(), error = function(e) character(0))
      
      if (length(tournaments) == 0) {
        showNotification("No tournaments found", type = "error")
        return()
      }
      
      log_debug(sprintf("Found %d tournaments", length(tournaments)), level = "INFO")
      
      # Load projections for each tournament
      all_data <- list()
      
      withProgress(message = "Loading tournaments", value = 0, {
        for (i in seq_along(tournaments)) {
          tourn <- tournaments[i]
          incProgress(1/length(tournaments), detail = tourn)
          
          proj <- tryCatch({
            load_this_week_projections(tourn)
          }, error = function(e) {
            log_debug(sprintf("Failed to load %s: %s", tourn, e$message), level = "WARN")
            NULL
          })
          
          if (!is.null(proj) && nrow(proj) > 0) {
            all_data[[tourn]] <- proj
            log_debug(sprintf("Loaded %d players for %s", nrow(proj), tourn), level = "INFO")
          }
        }
      })
      
      if (length(all_data) > 0) {
        combined <- bind_rows(all_data)
        rv$all_projections <- combined
        rv$current_tournament <- input$current_tournament
        rv$tournaments_loaded <- TRUE
        
        showNotification(
          sprintf("Loaded %d players across %d tournaments", nrow(combined), length(all_data)),
          type = "message"
        )
      } else {
        showNotification("No projection data found", type = "error")
      }
    })
    
    # Update current tournament when dropdown changes
    observeEvent(input$current_tournament, {
      rv$current_tournament <- input$current_tournament
    })
    
    # =========================================================================
    # LOAD STATUS
    # =========================================================================
    output$load_status <- renderUI({
      if (!rv$tournaments_loaded) {
        div(
          style = "padding: 0.5rem; margin-top: 25px; background: var(--bg-secondary); border-radius: 6px; text-align: center;",
          span(style = "color: var(--text-muted); font-size: 0.85rem;", "Click 'Load All Weeks' to begin")
        )
      } else {
        n_tournaments <- length(unique(rv$all_projections$tournament))
        n_players <- nrow(rv$all_projections)
        div(
          style = "padding: 0.5rem; margin-top: 25px; background: #E8F5E9; border-radius: 6px; text-align: center;",
          span(style = "color: #2E7D32; font-size: 0.85rem; font-weight: 600;", 
               sprintf("✓ %d tournaments, %d total players", n_tournaments, n_players))
        )
      }
    })
    
    # =========================================================================
    # CHART WRAPPER (conditional output based on viz type)
    # =========================================================================
    output$chart_wrapper <- renderUI({
      viz_type <- input$viz_type %||% "histogram"
      
      if (viz_type == "histogram") {
        plotOutput(ns("histogram_plot"), height = "400px")
      } else if (viz_type == "scatter") {
        # Scatter with HTML axis labels
        div(
          style = "width: 100%; box-sizing: border-box;",
          div(
            style = "display: flex; align-items: center; width: 100%;",
            div(
              style = "flex-shrink: 0; width: 40px; display: flex; align-items: center; justify-content: center;",
              div(
                style = "transform: rotate(-90deg); transform-origin: center; white-space: nowrap;",
                span(
                  style = "font-family: 'Fjalla One', sans-serif; font-size: 14px; color: #7A7A7A; font-weight: 400;",
                  "PROJECTION (PTS)",
                  span(style = "font-size: 16px; margin-left: 4px; vertical-align: middle;", HTML("&#8593;"))
                )
              )
            ),
            div(
              style = "flex: 1; min-width: 0;",
              ggiraph::girafeOutput(ns("scatter_plot"), height = "400px", width = "100%")
            )
          ),
          div(
            style = "display: flex; justify-content: center; padding: 8px 0 0 40px;",
            span(
              style = "font-family: 'Fjalla One', sans-serif; font-size: 14px; color: #7A7A7A; font-weight: 400;",
              "SALARY ($000s)",
              span(style = "font-size: 16px; margin-left: 4px; vertical-align: middle;", HTML("&#8594;"))
            )
          )
        )
      } else {
        # Strip plot with HTML axis labels
        div(
          style = "width: 100%; box-sizing: border-box;",
          div(
            style = "display: flex; align-items: center; width: 100%;",
            div(
              style = "flex-shrink: 0; width: 40px; display: flex; align-items: center; justify-content: center;",
              div(
                style = "transform: rotate(-90deg); transform-origin: center; white-space: nowrap;",
                span(
                  style = "font-family: 'Fjalla One', sans-serif; font-size: 14px; color: #7A7A7A; font-weight: 400;",
                  "AVG PROJECTION (PTS)",
                  span(style = "font-size: 16px; margin-left: 4px; vertical-align: middle;", HTML("&#8593;"))
                )
              )
            ),
            div(
              style = "flex: 1; min-width: 0;",
              ggiraph::girafeOutput(ns("strip_plot"), height = "400px", width = "100%")
            )
          ),
          div(
            style = "display: flex; justify-content: center; padding: 8px 0 0 40px;",
            span(
              style = "font-family: 'Fjalla One', sans-serif; font-size: 14px; color: #7A7A7A; font-weight: 400;",
              "TOURNAMENT",
              span(style = "font-size: 16px; margin-left: 4px; vertical-align: middle;", HTML("&#8594;"))
            )
          )
        )
      }
    })
    
    # =========================================================================
    # HISTOGRAM PLOT
    # =========================================================================
    output$histogram_plot <- renderPlot({
      if (is.null(rv$all_projections)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "Click 'Load All Weeks' to compare projection distributions",
                          size = 5, color = "gray50") +
                 theme_void() +
                 theme(plot.margin = margin(20, 20, 20, 20)))
      }
      
      data <- rv$all_projections
      min_proj <- input$min_projection %||% 30
      current_tourn <- rv$current_tournament
      
      data_filtered <- data %>% filter(projection >= min_proj)
      
      if (nrow(data_filtered) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No players above minimum projection threshold",
                          size = 5, color = "gray50") +
                 theme_void())
      }
      
      # Global axis range
      x_min <- floor(min(data_filtered$projection, na.rm = TRUE) / 10) * 10
      x_max <- ceiling(max(data_filtered$projection, na.rm = TRUE) / 10) * 10
      hist_breaks <- seq(x_min, x_max + 10, by = 10)
      
      # Define tiers
      data_filtered <- data_filtered %>%
        mutate(
          proj_tier = case_when(
            projection >= 70 ~ "Elite (70+)",
            projection >= 60 ~ "Top Tier (60-70)",
            projection >= 50 ~ "Good (50-60)",
            projection >= 40 ~ "Playable (40-50)",
            TRUE ~ "Fringe (<40)"
          ),
          proj_tier = factor(proj_tier, levels = c("Fringe (<40)", "Playable (40-50)", "Good (50-60)", "Top Tier (60-70)", "Elite (70+)"))
        )
      
      tier_colors <- c(
        "Fringe (<40)" = "#E5E9F0",
        "Playable (40-50)" = "#A8C5D4",
        "Good (50-60)" = "#A3BE8C",
        "Top Tier (60-70)" = "#EBCB8B",
        "Elite (70+)" = "#D08770"
      )
      
      # Order: current week LAST, others ascending by mean
      tourn_order <- data_filtered %>%
        group_by(tournament) %>%
        summarise(mean_proj = mean(projection, na.rm = TRUE), .groups = "drop") %>%
        mutate(is_current = tournament == current_tourn) %>%
        arrange(is_current, mean_proj) %>%
        pull(tournament)
      
      data_filtered <- data_filtered %>%
        mutate(tournament = factor(tournament, levels = tourn_order))
      
      ggplot(data_filtered, aes(x = projection, fill = proj_tier)) +
        geom_hline(yintercept = 0, color = "#3B3226", linewidth = 0.8) +
        geom_histogram(breaks = hist_breaks, color = "white", linewidth = 0.3) +
        facet_wrap(~ tournament, scales = "free_y", ncol = min(length(unique(data_filtered$tournament)), 4)) +
        scale_fill_manual(values = tier_colors, guide = "none") +
        scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 10)) +
        labs(x = NULL, y = NULL) +
        theme_app(base_size = 11) +
        theme(
          legend.position = "none",
          panel.spacing = unit(1, "lines"),
          strip.background = element_rect(fill = "#F5F0EB", color = NA),
          axis.text = element_text(color = "#3B3226", size = 9),
          panel.grid.major.x = element_blank()
        )
    }, res = 96)
    
    # =========================================================================
    # SCATTER PLOT (ggiraph)
    # =========================================================================
    output$scatter_plot <- ggiraph::renderGirafe({
      if (is.null(rv$all_projections)) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Click 'Load All Weeks' to compare",
                   size = 5, color = "gray50") +
          theme_void()
        return(ggiraph::girafe(ggobj = p, width_svg = 10, height_svg = 5))
      }
      
      data <- rv$all_projections
      min_proj <- input$min_projection %||% 30
      current_tourn <- rv$current_tournament
      
      data_filtered <- data %>%
        filter(projection >= min_proj, !is.na(salary), salary > 0)
      
      if (nrow(data_filtered) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No data available",
                   size = 5, color = "gray50") +
          theme_void()
        return(ggiraph::girafe(ggobj = p, width_svg = 10, height_svg = 5))
      }
      
      # Try to join headshots
      headshots <- tryCatch(load_golf_headshots(), error = function(e) NULL)
      if (!is.null(headshots) && nrow(headshots) > 0) {
        data_filtered <- data_filtered %>%
          mutate(
            match_key = tolower(gsub("[^a-z0-9 ]", " ", player_name)),
            match_key = gsub("\\s+", " ", trimws(match_key))
          ) %>%
          left_join(headshots %>% select(match_key, headshot_url), by = "match_key") %>%
          select(-match_key)
      }
      
      if (!"headshot_url" %in% names(data_filtered)) {
        data_filtered$headshot_url <- GOLF_DEFAULT_HEADSHOT
      }
      data_filtered <- data_filtered %>%
        mutate(headshot_url = ifelse(is.na(headshot_url) | headshot_url == "", 
                                     GOLF_DEFAULT_HEADSHOT, headshot_url))
      
      # Global axis ranges
      x_min <- floor(min(data_filtered$salary, na.rm = TRUE) / 1000)
      x_max <- ceiling(max(data_filtered$salary, na.rm = TRUE) / 1000)
      y_min <- floor(min(data_filtered$projection, na.rm = TRUE) / 10) * 10
      y_max <- ceiling(max(data_filtered$projection, na.rm = TRUE) / 10) * 10
      
      data_filtered <- data_filtered %>%
        mutate(
          proj_tier = case_when(
            projection >= 70 ~ "Elite (70+)",
            projection >= 60 ~ "Top Tier (60-70)",
            projection >= 50 ~ "Good (50-60)",
            projection >= 40 ~ "Playable (40-50)",
            TRUE ~ "Fringe (<40)"
          ),
          proj_tier = factor(proj_tier, levels = c("Fringe (<40)", "Playable (40-50)", "Good (50-60)", "Top Tier (60-70)", "Elite (70+)")),
          value = projection / (salary / 1000),
          tooltip_html = paste0(
            "<div style='display:flex;align-items:center;gap:10px;padding:10px;min-width:180px;font-family:sans-serif;'>",
            "<img src='", headshot_url, "' style='width:48px;height:48px;border-radius:50%;object-fit:cover;background:#e8e8e8;flex-shrink:0;' onerror=\"this.src='", GOLF_DEFAULT_HEADSHOT, "'\">",
            "<div>",
            "<strong style='font-size:13px;display:block;'>", player_name, "</strong>",
            "<span style='color:#7A7A7A;font-size:10px;'>", tournament, "</span><br>",
            "<span style='color:#3B3226;font-size:11px;'>Proj: <strong>", sprintf("%.1f", projection), "</strong> pts</span><br>",
            "<span style='color:#3B3226;font-size:11px;'>Salary: <strong>$", sprintf("%.1fk", salary), "</strong></span><br>",
            "<span style='color:#3B3226;font-size:11px;'>Value: <strong>", sprintf("%.2f", value), "</strong> pts/$k</span>",
            "</div>",
            "</div>"
          )
        )
      
      tier_colors <- c(
        "Fringe (<40)" = "#E5E9F0",
        "Playable (40-50)" = "#A8C5D4",
        "Good (50-60)" = "#A3BE8C",
        "Top Tier (60-70)" = "#EBCB8B",
        "Elite (70+)" = "#D08770"
      )
      
      # Order tournaments
      tourn_order <- data_filtered %>%
        group_by(tournament) %>%
        summarise(mean_proj = mean(projection, na.rm = TRUE), .groups = "drop") %>%
        mutate(is_current = tournament == current_tourn) %>%
        arrange(is_current, mean_proj) %>%
        pull(tournament)
      
      data_filtered <- data_filtered %>%
        mutate(tournament = factor(tournament, levels = tourn_order))
      
      p <- ggplot(data_filtered, aes(x = salary / 1000, y = projection, color = proj_tier)) +
        ggiraph::geom_point_interactive(
          aes(tooltip = tooltip_html, data_id = paste0(player_name, "_", tournament)),
          alpha = 0.7, size = 2.5
        ) +
        facet_wrap(~ tournament, ncol = min(length(unique(data_filtered$tournament)), 4)) +
        scale_color_manual(values = tier_colors, guide = "none") +
        scale_x_continuous(limits = c(x_min, x_max)) +
        scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = 10)) +
        labs(x = NULL, y = NULL) +
        theme_app(base_size = 11) +
        theme(
          legend.position = "none",
          panel.spacing = unit(1, "lines"),
          strip.background = element_rect(fill = "#F5F0EB", color = NA),
          axis.text = element_text(color = "#3B3226", size = 9)
        )
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 12,
        height_svg = 5,
        options = list(
          ggiraph::opts_tooltip(
            css = "background-color:#FFFFFF;border:2px solid #3B3226;border-radius:8px;box-shadow:3px 3px 0 #3B3226;padding:0;font-family:sans-serif;",
            opacity = 1,
            use_fill = FALSE
          ),
          ggiraph::opts_hover(
            css = "fill:#EBCB8B;fill-opacity:1;stroke:#3B3226;stroke-width:2;cursor:pointer;"
          ),
          ggiraph::opts_selection(type = "none")
        )
      )
    })
    
    # =========================================================================
    # STRIP PLOT (Top N averages by tournament)
    # =========================================================================
    output$strip_plot <- ggiraph::renderGirafe({
      if (is.null(rv$all_projections)) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Click 'Load All Weeks' to compare",
                   size = 5, color = "gray50") +
          theme_void()
        return(ggiraph::girafe(ggobj = p, width_svg = 12, height_svg = 5))
      }
      
      data <- rv$all_projections
      min_proj <- input$min_projection %||% 30
      current_tourn <- rv$current_tournament
      
      data_filtered <- data %>% filter(projection >= min_proj)
      
      if (nrow(data_filtered) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No data available",
                   size = 5, color = "gray50") +
          theme_void()
        return(ggiraph::girafe(ggobj = p, width_svg = 12, height_svg = 5))
      }
      
      # Calculate top N averages for each tournament
      strip_data <- data_filtered %>%
        group_by(tournament) %>%
        arrange(desc(projection)) %>%
        summarise(
          top5_avg = mean(head(projection, 5), na.rm = TRUE),
          top10_avg = mean(head(projection, 10), na.rm = TRUE),
          top20_avg = mean(head(projection, 20), na.rm = TRUE),
          player_count = n(),
          .groups = "drop"
        ) %>%
        pivot_longer(
          cols = c(top5_avg, top10_avg, top20_avg),
          names_to = "metric",
          values_to = "avg_projection"
        ) %>%
        mutate(
          metric = factor(metric, 
                          levels = c("top5_avg", "top10_avg", "top20_avg"),
                          labels = c("Top 5", "Top 10", "Top 20")),
          is_current = tournament == current_tourn,
          tooltip_html = paste0(
            "<div style='padding:10px;font-family:sans-serif;min-width:120px;'>",
            "<strong style='font-size:13px;'>", tournament, "</strong><br>",
            "<span style='color:#7A7A7A;font-size:11px;'>", metric, " Average</span><br>",
            "<span style='font-size:16px;font-weight:700;color:#3B3226;'>", sprintf("%.1f", avg_projection), " pts</span>",
            "</div>"
          )
        )
      
      # Order tournaments: current last, others by top10 avg ascending
      tourn_order <- strip_data %>%
        filter(metric == "Top 10") %>%
        mutate(is_current = tournament == current_tourn) %>%
        arrange(is_current, avg_projection) %>%
        pull(tournament)
      
      strip_data <- strip_data %>%
        mutate(tournament = factor(tournament, levels = tourn_order))
      
      # Global y-axis range
      y_min <- floor(min(strip_data$avg_projection, na.rm = TRUE) / 5) * 5
      y_max <- ceiling(max(strip_data$avg_projection, na.rm = TRUE) / 5) * 5
      
      # Colors for the three metrics
      metric_colors <- c(
        "Top 5" = "#D08770",   # coral (elite)
        "Top 10" = "#EBCB8B",  # gold
        "Top 20" = "#A3BE8C"   # sage
      )
      
      # Build strip plot
      p <- ggplot(strip_data, aes(x = tournament, y = avg_projection, color = metric)) +
        # Connect points with lines
        geom_line(aes(group = metric), linewidth = 1, alpha = 0.4) +
        # Interactive points
        ggiraph::geom_point_interactive(
          aes(tooltip = tooltip_html, data_id = paste0(tournament, "_", metric),
              size = ifelse(is_current, 5, 3.5)),
          alpha = 0.9
        ) +
        # Highlight current tournament with vertical line
        geom_vline(
          xintercept = which(levels(strip_data$tournament) == current_tourn),
          linetype = "dashed", color = "#3B3226", alpha = 0.5
        ) +
        scale_color_manual(values = metric_colors, name = NULL) +
        scale_size_identity() +
        scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = 5)) +
        labs(x = NULL, y = NULL) +
        theme_app(base_size = 11) +
        theme(
          legend.position = "top",
          legend.justification = "left",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
          axis.text.y = element_text(color = "#3B3226", size = 9),
          panel.grid.major.x = element_line(color = "#E5E9F0", linewidth = 0.3)
        )
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 14,
        height_svg = 5,
        options = list(
          ggiraph::opts_tooltip(
            css = "background-color:#FFFFFF;border:2px solid #3B3226;border-radius:8px;box-shadow:3px 3px 0 #3B3226;padding:0;font-family:sans-serif;",
            opacity = 1,
            use_fill = FALSE
          ),
          ggiraph::opts_hover(
            css = "fill-opacity:1;stroke:#3B3226;stroke-width:2;cursor:pointer;"
          ),
          ggiraph::opts_selection(type = "none")
        )
      )
    })
    
    # =========================================================================
    # ASSESSMENT CARD
    # =========================================================================
    output$assessment_card <- renderUI({
      if (is.null(rv$all_projections) || is.null(rv$current_tournament)) {
        return(div(
          style = "padding: 1rem; background: var(--bg-secondary); border-radius: 6px; text-align: center; color: var(--text-muted);",
          "Load all weeks to see this week's assessment"
        ))
      }
      
      data <- rv$all_projections
      current_tourn <- rv$current_tournament
      min_proj <- input$min_projection %||% 30
      
      # Calculate stats for each tournament
      tourn_stats <- data %>%
        filter(projection >= min_proj) %>%
        group_by(tournament) %>%
        summarise(
          mean_proj = mean(projection, na.rm = TRUE),
          top10_mean = mean(sort(projection, decreasing = TRUE)[1:min(10, n())], na.rm = TRUE),
          top20_mean = mean(sort(projection, decreasing = TRUE)[1:min(20, n())], na.rm = TRUE),
          elite_count = sum(projection >= 70),
          top_tier_count = sum(projection >= 60 & projection < 70),
          .groups = "drop"
        ) %>%
        arrange(desc(mean_proj)) %>%
        mutate(rank = row_number())
      
      current_stats <- tourn_stats %>% filter(tournament == current_tourn)
      
      if (nrow(current_stats) == 0) {
        return(div(
          style = "padding: 1rem; background: var(--bg-secondary); border-radius: 6px; text-align: center;",
          "Current tournament not in comparison data"
        ))
      }
      
      rank <- current_stats$rank
      total <- nrow(tourn_stats)
      top10_mean <- round(current_stats$top10_mean, 1)
      top20_mean <- round(current_stats$top20_mean, 1)
      elite_count <- current_stats$elite_count
      top_tier_count <- current_stats$top_tier_count
      
      if (rank <= ceiling(total * 0.25)) {
        rank_color <- "#A3BE8C"
        assessment <- "HIGH SCORING WEEK"
        recommendation <- "Good time to use transfers - larger pool of elite options means bigger potential gains"
        icon_html <- "&#9650;"
      } else if (rank <= ceiling(total * 0.5)) {
        rank_color <- "#EBCB8B"
        assessment <- "AVERAGE SCORING WEEK"
        recommendation <- "Transfers optional - consider banking if upgrade options are marginal"
        icon_html <- "&#9644;"
      } else {
        rank_color <- "#D08770"
        assessment <- "LOW SCORING WEEK"
        recommendation <- "Consider banking transfers - differentiation value may outweigh raw point gains"
        icon_html <- "&#9660;"
      }
      
      div(
        style = sprintf("display: flex; align-items: center; gap: 2rem; padding: 1rem 1.5rem; background: white; border: 2px solid %s; border-radius: 8px;", rank_color),
        
        div(
          style = "text-align: center; flex-shrink: 0;",
          div(style = sprintf("font-size: 1.5rem; font-weight: 700; color: %s;", rank_color), HTML(icon_html)),
          div(style = sprintf("font-size: 1.25rem; font-weight: 700; color: %s;", rank_color), sprintf("#%d", rank)),
          div(style = "font-size: 0.7rem; color: var(--text-muted);", sprintf("of %d weeks", total))
        ),
        
        div(
          style = "flex: 1;",
          div(style = sprintf("font-size: 0.9rem; font-weight: 700; color: %s; margin-bottom: 0.25rem;", rank_color), assessment),
          div(style = "font-size: 0.8rem; color: var(--text-secondary); line-height: 1.4;", recommendation)
        ),
        
        div(
          style = "display: flex; gap: 1.5rem; flex-shrink: 0;",
          div(style = "text-align: center;",
              div(style = "font-size: 1.1rem; font-weight: 700; color: var(--text-primary);", sprintf("%.1f", top10_mean)),
              div(style = "font-size: 0.65rem; color: var(--text-muted); text-transform: uppercase;", "Top 10 Avg")),
          div(style = "text-align: center;",
              div(style = "font-size: 1.1rem; font-weight: 700; color: var(--text-primary);", sprintf("%.1f", top20_mean)),
              div(style = "font-size: 0.65rem; color: var(--text-muted); text-transform: uppercase;", "Top 20 Avg")),
          div(style = "text-align: center;",
              div(style = sprintf("font-size: 1.1rem; font-weight: 700; color: %s;", "#D08770"), elite_count),
              div(style = "font-size: 0.65rem; color: var(--text-muted); text-transform: uppercase;", "Elite (70+)")),
          div(style = "text-align: center;",
              div(style = sprintf("font-size: 1.1rem; font-weight: 700; color: %s;", "#EBCB8B"), top_tier_count),
              div(style = "font-size: 0.65rem; color: var(--text-muted); text-transform: uppercase;", "Top Tier (60-70)"))
        )
      )
    })
    
  })
}

cat("Golf This Week module loaded: golf_this_week_ui(), golf_this_week_server()\n")