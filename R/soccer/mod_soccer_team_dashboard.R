# =============================================================================
# Module: Soccer Team Dashboard
# 
# Team performance analytics - game-by-game analysis, league comparison charts
# Dependencies: app_themes.R, soccer_config.R, soccer_cache.R, soccer_data_loader.R, 
#              soccer_transforms.R
# =============================================================================

#' Soccer Team Dashboard UI
#' @param id Module namespace ID
soccer_team_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_team_dashboard_ui() called with id:", id, level = "INFO")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Team Dashboard"),
      tags$p(class = "text-muted", "Team performance and league comparison analytics")
    ),
    
    # Filters card
    ui_card(
      title = "Filters",
      color = "sage",
      
      fluidRow(
        column(3,
               shinyWidgets::pickerInput(ns("league"), "League",
                                         choices = c("Loading..." = ""),
                                         selected = NULL,
                                         options = shinyWidgets::pickerOptions(
                                           liveSearch = FALSE,
                                           size = 10
                                         )
               )
        ),
        column(3,
               shinyWidgets::pickerInput(ns("team"), "Team(s)",
                                         choices = c("Select league first"),
                                         selected = NULL,
                                         multiple = TRUE,
                                         options = shinyWidgets::pickerOptions(
                                           actionsBox = FALSE,
                                           liveSearch = FALSE,
                                           maxOptions = 2,
                                           noneSelectedText = "Select team(s)",
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0} teams"
                                         )
               )
        ),
        column(6,
               div(style = "padding-top: 25px; display: flex; flex-direction: column; align-items: flex-end; gap: 0.25rem;",
                   tags$button(
                     id = ns("refresh_data"),
                     class = "btn btn-refresh-subtle",
                     type = "button",
                     "Refresh"
                   ),
                   uiOutput(ns("cache_status"))
               )
        )
      )
    ),
    
    tags$br(),
    
    # Team Performance Section - Game by Game Analysis
    ui_card(
      title = "Team Performance",
      subtitle = "Game-by-game analysis with league average comparison",
      color = "sage",
      
      # Team header (logo + name) at the top
      uiOutput(ns("team_performance_header")),
      
      tags$hr(style = "margin: 0.75rem 0; border-color: var(--bg-secondary);"),
      
      # Controls row
      fluidRow(
        column(3,
               selectInput(ns("performance_metric"), "Metric",
                           choices = c(
                             "Expected Goals (xG)" = "xg",
                             "Shots" = "shots",
                             "Goals" = "goals"
                           ),
                           selected = "xg"
               )
        ),
        column(3,
               selectInput(ns("performance_view"), "View",
                           choices = c(
                             "Actual" = "actual",
                             "6 Game Rolling Avg" = "rolling"
                           ),
                           selected = "rolling"
               )
        ),
        column(3,
               # Only show when 2 teams selected
               conditionalPanel(
                 condition = sprintf("$('#%s').val() && $('#%s').val().length == 2", ns("team"), ns("team")),
                 selectInput(ns("facet_layout"), "Layout",
                             choices = c(
                               "Side by Side" = "side",
                               "Stacked" = "stacked"
                             ),
                             selected = "side"
                 )
               )
        ),
        column(3)
      ),
      
      # Dynamic height - taller for stacked layout
      uiOutput(ns("team_performance_chart_container"))
    ),
    
    tags$br(),
    
    # League Comparison Chart Section
    ui_card(
      title = "League Comparison",
      color = "sage",
      
      fluidRow(
        column(3,
               selectInput(ns("comparison_timeframe"), "Timeframe",
                           choices = c(
                             "Whole Season" = "season",
                             "Last 6 Games" = "last6"
                           ),
                           selected = "season"
               )
        ),
        column(4,
               selectizeInput(ns("chart_metric"), "Metric",
                              choices = list(
                                "Raw Metrics" = c(
                                  "Goals For (per game)" = "goals_for_pg",
                                  "Goals Against (per game)" = "goals_against_pg",
                                  "Goal Difference (per game)" = "goal_diff_pg",
                                  "Shots For (per game)" = "shots_for_pg",
                                  "Shots Against (per game)" = "shots_against_pg",
                                  "xG For (per game)" = "xg_for_pg",
                                  "xG Against (per game)" = "xg_against_pg",
                                  "xG Difference (per game)" = "xg_diff_pg"
                                ),
                                "Opponent-Adjusted (Offense)" = c(
                                  "Goals vs Opponent Avg" = "goals_vs_opp_avg",
                                  "Shots vs Opponent Avg" = "shots_vs_opp_avg",
                                  "xG vs Opponent Avg" = "xg_vs_opp_avg"
                                ),
                                "Opponent-Adjusted (Defense)" = c(
                                  "Goals Against vs Opp Avg" = "goals_against_vs_opp_avg",
                                  "Shots Against vs Opp Avg" = "shots_against_vs_opp_avg",
                                  "xG Against vs Opp Avg" = "xg_against_vs_opp_avg"
                                ),
                                "Advanced" = c(
                                  "xG Analysis" = "xg_scatter",
                                  "Shot Quality" = "shot_quality_scatter",
                                  "Pythagorean Analysis" = "pythag_scatter"
                                )
                              ),
                              selected = "xg_vs_opp_avg"
               )
        ),
        column(2,
               selectInput(ns("comparison_view"), "View",
                           choices = c(
                             "Chart" = "chart",
                             "Table" = "table",
                             "Ranking" = "ranking"
                           ),
                           selected = "chart"
               )
        ),
        column(3)
      ),
      
      tags$br(),
      
      # Conditional output - chart, table, or ranking
      conditionalPanel(
        condition = sprintf("input['%s'] == 'chart'", ns("comparison_view")),
        uiOutput(ns("league_chart_container"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'table'", ns("comparison_view")),
        uiOutput(ns("league_table"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ranking'", ns("comparison_view")),
        uiOutput(ns("league_ranking"))
      )
    )
  )
}

#' Soccer Team Dashboard Server
#' @param id Module namespace ID
soccer_team_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_team_dashboard_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    rv <- reactiveValues(
      shooting_data = NULL,
      possession_data = NULL,
      shot_data = NULL,
      team_goals_data = NULL,  # Actual match scores (includes own goals)
      loading = FALSE,
      initialized = FALSE,
      last_refresh = NULL
    )
    
    # =========================================================================
    # DATA LOADING (with caching)
    # =========================================================================
    
    observe({
      log_debug(">>> Initial data load observer triggered", level = "DEBUG")
      
      if (rv$initialized) return()
      
      rv$loading <- TRUE
      
      tryCatch({
        log_debug("Initializing Google Sheets...", level = "INFO")
        init_google_sheets()
        
        log_debug("Loading shooting summary data (checking cache)...", level = "INFO")
        rv$shooting_data <- load_shooting_summary(force_refresh = FALSE)
        
        log_debug("Loading possession data (checking cache)...", level = "INFO")
        rv$possession_data <- load_possession_data(force_refresh = FALSE)
        
        log_debug("Loading shot data (checking cache)...", level = "INFO")
        rv$shot_data <- load_shot_data(force_refresh = FALSE)
        
        log_debug("Loading team goals data (checking cache)...", level = "INFO")
        rv$team_goals_data <- load_team_goals(force_refresh = FALSE)
        
        rv$last_refresh <- Sys.time()
        rv$initialized <- TRUE
        
        log_debug("All soccer data loaded successfully", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error loading soccer data:", e$message, level = "ERROR")
      })
      
      rv$loading <- FALSE
    })
    
    # Manual refresh
    observeEvent(input$refresh_data, {
      log_debug(">>> Manual refresh triggered", level = "INFO")
      
      rv$loading <- TRUE
      
      tryCatch({
        init_google_sheets()
        rv$shooting_data <- load_shooting_summary(force_refresh = TRUE)
        rv$possession_data <- load_possession_data(force_refresh = TRUE)
        rv$shot_data <- load_shot_data(force_refresh = TRUE)
        rv$team_goals_data <- load_team_goals(force_refresh = TRUE)
        rv$last_refresh <- Sys.time()
        log_debug("Manual refresh completed", level = "INFO")
      }, error = function(e) {
        log_debug("Error during manual refresh:", e$message, level = "ERROR")
      })
      
      rv$loading <- FALSE
    })
    
    # =========================================================================
    # CALCULATE ALL TEAM STATS (for league comparison)
    # =========================================================================
    
    all_team_stats <- reactive({
      req(input$league, input$league != "")
      req(rv$shot_data)
      
      timeframe <- input$comparison_timeframe
      if (is.null(timeframe)) timeframe <- "season"
      
      log_debug(">>> Calculating all team stats, timeframe:", timeframe, level = "INFO")
      
      calculate_all_team_stats(
        rv$shot_data, 
        input$league, 
        timeframe, 
        team_goals_data = rv$team_goals_data  # Pass actual goal data
      )
    })
    
    # =========================================================================
    # CACHE STATUS DISPLAY
    # =========================================================================
    
    output$cache_status <- renderUI({
      input$refresh_data
      
      # Check when data was last loaded in this session
      if (!is.null(rv$last_refresh)) {
        time_ago <- difftime(Sys.time(), rv$last_refresh, units = "hours")
        if (time_ago < 1) {
          time_str <- sprintf("%.0f minutes ago", as.numeric(time_ago) * 60)
        } else {
          time_str <- sprintf("%.1f hours ago", as.numeric(time_ago))
        }
        return(div(
          style = "font-size: 0.7rem; color: var(--text-muted); font-style: italic;",
          sprintf("Data loaded %s", time_str)
        ))
      }
      
      # Fall back to cache status
      status <- tryCatch(
        get_cache_status(),
        error = function(e) NULL
      )
      
      if (is.null(status) || !is.list(status)) {
        return(div(
          style = "font-size: 0.7rem; color: var(--text-muted); font-style: italic;",
          "Data loaded from cache"
        ))
      }
      
      all_cached <- tryCatch(
        all(sapply(status, function(s) isTRUE(s$exists))),
        error = function(e) FALSE
      )
      
      if (all_cached) {
        cache_age <- tryCatch(status$shots$age, error = function(e) "recently")
        div(
          style = "font-size: 0.7rem; color: var(--text-muted); font-style: italic;",
          sprintf("Data cached %s", cache_age)
        )
      } else {
        div(
          style = "font-size: 0.7rem; color: var(--text-muted); font-style: italic;",
          "Data loaded"
        )
      }
    })
    
    # =========================================================================
    # UPDATE LEAGUE CHOICES
    # =========================================================================
    
    observe({
      log_debug(">>> Updating league choices", level = "DEBUG")
      
      req(rv$shot_data)
      
      leagues <- get_available_leagues(rv$shot_data)
      log_debug("Available leagues:", paste(leagues, collapse = ", "), level = "INFO")
      
      if (length(leagues) > 0) {
        default_league <- if ("Premier League" %in% leagues) "Premier League" else leagues[1]
        
        # Build HTML content for each league (logo + name)
        league_content <- sapply(leagues, function(lg) {
          logo_path <- get_league_logo(lg)
          if (!is.null(logo_path)) {
            sprintf('<img src="%s" style="width:20px; height:20px; margin-right:8px; vertical-align:middle; object-fit:contain;"> %s', 
                    logo_path, lg)
          } else {
            lg
          }
        }, USE.NAMES = FALSE)
        
        shinyWidgets::updatePickerInput(session, "league",
                                        choices = leagues,
                                        selected = default_league,
                                        choicesOpt = list(content = league_content)
        )
      }
    })
    
    # =========================================================================
    # UPDATE TEAM CHOICES
    # =========================================================================
    
    observeEvent(input$league, {
      log_debug(">>> League changed to:", input$league, level = "DEBUG")
      
      req(input$league, input$league != "")
      req(rv$shot_data)
      
      teams <- get_league_teams(rv$shot_data, input$league)
      log_debug("Teams available:", length(teams), level = "INFO")
      
      if (length(teams) > 0) {
        # Build HTML content for each team (logo + name)
        team_content <- sapply(teams, function(team) {
          logo_path <- get_soccer_team_logo(team)
          if (!is.null(logo_path)) {
            sprintf('<img src="%s" style="width:20px; height:20px; margin-right:8px; vertical-align:middle; object-fit:contain;"> %s', 
                    logo_path, team)
          } else {
            team
          }
        }, USE.NAMES = FALSE)
        
        # Default to Manchester United for Premier League, else first team
        default_team <- if (input$league == "Premier League" && "Manchester United" %in% teams) {
          "Manchester United"
        } else {
          teams[1]
        }
        
        shinyWidgets::updatePickerInput(session, "team",
                                        choices = teams,
                                        selected = default_team,
                                        choicesOpt = list(content = team_content)
        )
      }
    })
    
    # =========================================================================
    # TEAM PERFORMANCE HEADER
    # =========================================================================
    
    output$team_performance_header <- renderUI({
      req(input$team)
      req(length(input$team) > 0)
      
      # Build header for each selected team
      headers <- lapply(input$team, function(team_name) {
        logo <- get_soccer_team_logo(team_name)
        
        div(
          style = "display: inline-flex; align-items: center; gap: 0.5rem; margin-right: 1.5rem;",
          if (!is.null(logo)) {
            tags$img(src = logo, alt = team_name, 
                     style = "width: 32px; height: 32px; object-fit: contain;")
          },
          tags$span(style = "font-weight: 700; font-size: 1.1rem;", team_name)
        )
      })
      
      div(style = "display: flex; flex-wrap: wrap; align-items: center;", headers)
    })
    
    # =========================================================================
    # TEAM PERFORMANCE CHART
    # =========================================================================
    
    # Dynamic chart container with height based on layout
    output$team_performance_chart_container <- renderUI({
      n_teams <- length(input$team)
      layout <- input$facet_layout
      if (is.null(layout)) layout <- "side"
      
      # Taller for stacked layout with 2 teams
      height <- if (n_teams == 2 && layout == "stacked") "700px" else "400px"
      
      plotOutput(ns("team_performance_chart"), height = height)
    })
    
    output$team_performance_chart <- renderPlot({
      log_debug(">>> Rendering team performance chart", level = "DEBUG")
      
      req(input$league, input$team, input$performance_metric, input$performance_view)
      req(length(input$team) > 0, input$league != "")
      req(rv$shot_data)
      
      metric <- input$performance_metric
      view_type <- input$performance_view
      selected_teams <- input$team
      
      # Get game-by-game data for each selected team
      all_game_data <- lapply(selected_teams, function(t) {
        game_data <- calculate_game_by_game_stats(
          rv$shot_data, 
          input$league, 
          t, 
          team_goals_data = rv$team_goals_data  # Pass actual goal data
        )
        if (nrow(game_data) > 0) {
          game_data$team_name <- t
        }
        game_data
      })
      
      combined_data <- bind_rows(all_game_data)
      req(nrow(combined_data) > 0)
      
      # Metric configuration
      metric_config <- list(
        xg = list(for_col = "xg_for", against_col = "xg_against", 
                  label = "xG", format = "%.2f"),
        shots = list(for_col = "shots_for", against_col = "shots_against", 
                     label = "Shots", format = "%.0f"),
        goals = list(for_col = "goals_for", against_col = "goals_against", 
                     label = "Goals", format = "%.0f")
      )
      
      config <- metric_config[[metric]]
      
      # Calculate league averages
      league_data_name <- LEAGUE_DATA_NAMES[input$league]
      if (is.na(league_data_name)) league_data_name <- input$league
      
      league_shots <- rv$shot_data %>% filter(league == league_data_name)
      league_avgs <- calculate_league_averages(league_shots)
      
      league_avg_for <- switch(metric,
                               xg = league_avgs$xg_pg,
                               shots = league_avgs$shots_pg,
                               goals = league_avgs$goals_pg
      )
      league_avg_against <- league_avg_for
      
      # Create x-axis labels: opponent abbreviation in CAPS with (H) or (A)
      game_data <- combined_data %>%
        mutate(
          opp_abbrev = toupper(sapply(opponent, get_team_abbreviation)),
          venue = ifelse(is_home, "(H)", "(A)"),
          x_label = paste0(opp_abbrev, "\n", venue)
        )
      
      # Apply rolling average if selected (per team) - BEFORE pivoting
      if (view_type == "rolling") {
        game_data <- game_data %>%
          group_by(team_name) %>%
          arrange(match_num) %>%
          mutate(
            !!config$for_col := zoo::rollapplyr(
              .data[[config$for_col]], 
              width = 6, 
              FUN = mean, 
              partial = TRUE,
              na.rm = TRUE
            ),
            !!config$against_col := zoo::rollapplyr(
              .data[[config$against_col]], 
              width = 6, 
              FUN = mean, 
              partial = TRUE,
              na.rm = TRUE
            )
          ) %>%
          ungroup()
      }
      
      # For single team: get unique breaks/labels for x-axis
      if (length(selected_teams) == 1) {
        axis_data <- game_data %>%
          distinct(match_num, x_label) %>%
          arrange(match_num)
        x_breaks <- axis_data$match_num
        x_labels_vec <- axis_data$x_label
      }
      
      # Prepare plot data - pivot to long format
      plot_data <- game_data %>%
        select(match_num, team_name, opponent, x_label,
               for_val = !!sym(config$for_col),
               against_val = !!sym(config$against_col)) %>%
        pivot_longer(cols = c(for_val, against_val),
                     names_to = "direction",
                     values_to = "value") %>%
        mutate(
          direction = factor(
            ifelse(direction == "for_val", "For", "Against"),
            levels = c("For", "Against")
          )
        )
      
      req(nrow(plot_data) > 0)
      
      # Build plot
      p <- ggplot(plot_data, aes(x = match_num, y = value, color = direction, group = direction))
      
      p <- p +
        geom_hline(yintercept = league_avg_for, linetype = "dashed", 
                   color = APP_COLORS$sage, linewidth = 0.7, alpha = 0.8) +
        geom_hline(yintercept = league_avg_against, linetype = "dashed", 
                   color = APP_COLORS$coral, linewidth = 0.7, alpha = 0.8)
      
      if (view_type == "actual") {
        # Overlapping bars - use position="identity" to prevent stacking
        p <- p + 
          geom_col(data = filter(plot_data, direction == "For"),
                   aes(fill = direction), width = 0.55, color = NA, alpha = 0.95,
                   position = "identity") +
          geom_col(data = filter(plot_data, direction == "Against"),
                   aes(fill = direction), width = 0.55, color = NA, alpha = 0.95,
                   position = position_nudge(x = 0.15))
      } else {
        p <- p + 
          geom_line(linewidth = 1.8) +
          geom_point(size = 3.5)
      }
      
      p <- p + scale_color_for_against()
      
      # Only add fill scale when using bars (actual view)
      if (view_type == "actual") {
        p <- p + scale_fill_for_against()
      }
      
      # X-axis configuration
      if (length(selected_teams) == 1) {
        # Single team: show opponent abbreviations + venue
        p <- p + scale_x_continuous(
          breaks = x_breaks,
          labels = x_labels_vec,
          position = "top",
          expand = expansion(mult = c(0.02, 0.02))
        )
      } else {
        # Multiple teams: use facets with simple match numbers (no opponent labels)
        layout <- input$facet_layout
        if (is.null(layout)) layout <- "side"
        
        facet_ncol <- if (layout == "stacked") 1 else 2
        
        p <- p + 
          facet_wrap(~ team_name, ncol = facet_ncol, scales = "free_x", 
                     strip.position = "top") +
          scale_x_continuous(
            breaks = function(limits) {
              seq(floor(min(limits)), ceiling(max(limits)), by = 1)
            },
            position = "top",
            expand = expansion(mult = c(0.02, 0.02))
          )
      }
      
      # Y-axis limits
      max_value <- max(plot_data$value, na.rm = TRUE)
      if (metric == "xg") {
        y_upper <- ceiling(max_value * 2) / 2 + 0.5
      } else {
        y_upper <- ceiling(max_value) + 1
      }
      if ((y_upper - max_value) > (y_upper * 0.3)) {
        y_upper <- y_upper - (if (metric == "xg") 0.5 else 1)
      }
      
      p <- p +
        scale_y_continuous(limits = c(0, y_upper), expand = expansion(mult = c(0, 0))) +
        geom_hline(yintercept = 0, color = APP_COLORS$primary, linewidth = 0.8)
      
      caption_text <- sprintf("Dashed lines show league average: For = %.2f, Against = %.2f", 
                              league_avg_for, league_avg_against)
      if (metric == "goals") {
        caption_text <- paste0(caption_text, "\nNote: Goals from shots only (excludes own goals)")
      }
      
      # Get layout for theme (already defined earlier for facets)
      chart_layout <- if (length(selected_teams) == 2) {
        layout_pref <- input$facet_layout
        if (is.null(layout_pref)) "side" else layout_pref
      } else {
        "side"
      }
      
      # Apply team performance theme (controls fonts, sizes, spacing)
      p <- p +
        labs(x = NULL, y = NULL, caption = caption_text) +
        theme_team_performance(layout = chart_layout)
      
      p
    }, bg = "transparent")
    
    
    # =========================================================================
    # LEAGUE COMPARISON CHART
    # =========================================================================
    
    # Dynamic container that switches between chart types with appropriate axis labels
    output$league_chart_container <- renderUI({
      metric <- input$chart_metric
      
      # Check chart type
      is_scatter <- metric %in% c("xg_scatter", "shot_quality_scatter", "pythag_scatter")
      is_diverging <- grepl("diff|vs_opp", metric)
      
      if (is_scatter) {
        # Get chart title/subtitle and axis labels based on metric
        chart_info <- switch(
          metric,
          "xg_scatter" = list(
            title = "xG For vs Against", 
            subtitle = "Expected goals created and conceded per game",
            y_label = "BETTER OFFENCE",
            x_label = "BETTER DEFENCE"
          ),
          "shot_quality_scatter" = list(
            title = "Shot Volume vs Quality", 
            subtitle = "Shots taken per game vs expected goals generated",
            y_label = "HIGHER QUALITY",
            x_label = "MORE SHOTS"
          ),
          "pythag_scatter" = list(
            title = "Expected vs Actual Performance", 
            subtitle = "Pythagorean win % based on goals vs xG",
            y_label = "BETTER RESULTS",
            x_label = "HIGHER EXPECTED"
          ),
          list(title = "", subtitle = "", y_label = "", x_label = "")
        )
        
        # Scatter chart with external axis labels
        div(
          style = "width: 100%; box-sizing: border-box;",
          
          # Chart title and subtitle - aligned with inputs above
          div(
            style = "padding: 0 0 16px 0;",
            div(
              style = "font-family: var(--font-display, 'Fjalla One'), sans-serif; font-size: 20px; color: var(--text-primary, #3B3226); font-weight: 400; margin-bottom: 4px;",
              chart_info$title
            ),
            div(
              style = "font-family: var(--font-main, 'Source Sans Pro'), sans-serif; font-size: 13px; color: var(--text-muted, #7A7A7A);",
              chart_info$subtitle
            )
          ),
          
          # Main content area with Y-axis label and plot
          div(
            style = "display: flex; align-items: center; width: 100%;",
            
            # Y-axis label (left side, rotated)
            div(
              style = "flex-shrink: 0; width: 50px; display: flex; align-items: center; justify-content: center;",
              div(
                style = "transform: rotate(-90deg); transform-origin: center; white-space: nowrap;",
                span(
                  style = "font-family: 'Fjalla One', sans-serif; font-size: 16px; color: #666; font-weight: 400;",
                  chart_info$y_label,
                  span(style = "font-size: 20px; margin-left: 6px; vertical-align: middle;", HTML("&#8594;"))
                )
              )
            ),
            
            # Plot area (constrained)
            div(
              style = "flex: 1; min-width: 0; padding-right: 50px;",
              plotOutput(ns("league_chart"), height = "500px", width = "100%")
            )
          ),
          
          # X-axis label below
          div(
            style = "display: flex; justify-content: center; padding: 8px 50px 0 50px;",
            span(
              style = "font-family: 'Fjalla One', sans-serif; font-size: 16px; color: #666; font-weight: 400;",
              chart_info$x_label,
              span(style = "font-size: 20px; margin-left: 6px; vertical-align: middle;", HTML("&#8594;"))
            )
          )
        )
      } else if (is_diverging) {
        # Diverging bar chart with labels above showing positive/negative meaning
        div(
          # Labels above the chart - positioned around center
          div(
            style = "display: flex; justify-content: center; align-items: center; padding: 0 0 12px 0;",
            # Left label (negative direction) - right-aligned to be near center
            div(
              style = "flex: 1; text-align: right; padding-right: 20px;",
              htmlOutput(ns("diverging_left_label"), inline = TRUE)
            ),
            # Right label (positive direction) - left-aligned to be near center
            div(
              style = "flex: 1; text-align: left; padding-left: 20px;",
              htmlOutput(ns("diverging_right_label"), inline = TRUE)
            )
          ),
          # Plot
          plotOutput(ns("league_chart"), height = "600px")
        )
      } else {
        # Standard bar chart - with axis label above
        # Get label based on metric
        axis_label <- switch(
          metric,
          "goals_for_pg" = "MORE GOALS",
          "goals_against_pg" = "MORE CONCEDED",
          "shots_for_pg" = "MORE SHOTS",
          "shots_against_pg" = "MORE FACED",
          "xg_for_pg" = "HIGHER XG",
          "xg_against_pg" = "HIGHER XG AGAINST",
          "HIGHER"
        )
        
        div(
          # Axis label above the chart - right aligned
          div(
            style = "display: flex; justify-content: flex-end; padding: 0 0 12px 0;",
            span(
              style = "font-family: 'Fjalla One', sans-serif; font-size: 16px; color: #666; font-weight: 400;",
              axis_label,
              span(style = "font-size: 20px; margin-left: 6px; vertical-align: middle;", HTML("&#8594;"))
            )
          ),
          # Plot
          plotOutput(ns("league_chart"), height = "600px")
        )
      }
    })
    
    # Diverging chart left label (negative/worse direction)
    output$diverging_left_label <- renderUI({
      metric <- input$chart_metric
      
      # Determine meaningful context based on metric type
      label_text <- switch(
        metric,
        # Opponent-adjusted offense
        "goals_vs_opp_avg" = "FEWER GOALS",
        "shots_vs_opp_avg" = "FEWER SHOTS",
        "xg_vs_opp_avg" = "LOWER XG",
        # Opponent-adjusted defense
        "goals_against_vs_opp_avg" = "FEWER CONCEDED",
        "shots_against_vs_opp_avg" = "FEWER FACED",
        "xg_against_vs_opp_avg" = "LOWER XG AGAINST",
        # Difference metrics
        "goal_diff_pg" = "NEGATIVE GD",
        "xg_diff_pg" = "NEGATIVE XGD",
        # Default
        "LOWER"
      )
      
      HTML(paste0(
        '<span style="font-family: var(--font-display, \'Fjalla One\'), sans-serif; font-size: 16px; color: var(--text-secondary, #5C4E3D); font-weight: 400; display: inline-flex; align-items: center;">',
        '<span style="font-size: 22px; line-height: 1; margin-right: 8px;">&larr;</span>',
        '<span style="line-height: 1;">', label_text, '</span>',
        '</span>'
      ))
    })
    
    # Diverging chart right label (positive/better direction)
    output$diverging_right_label <- renderUI({
      metric <- input$chart_metric
      
      # Determine meaningful context based on metric type
      label_text <- switch(
        metric,
        # Opponent-adjusted offense
        "goals_vs_opp_avg" = "MORE GOALS",
        "shots_vs_opp_avg" = "MORE SHOTS",
        "xg_vs_opp_avg" = "HIGHER XG",
        # Opponent-adjusted defense
        "goals_against_vs_opp_avg" = "MORE CONCEDED",
        "shots_against_vs_opp_avg" = "MORE FACED",
        "xg_against_vs_opp_avg" = "HIGHER XG AGAINST",
        # Difference metrics
        "goal_diff_pg" = "POSITIVE GD",
        "xg_diff_pg" = "POSITIVE XGD",
        # Default
        "HIGHER"
      )
      
      HTML(paste0(
        '<span style="font-family: var(--font-display, \'Fjalla One\'), sans-serif; font-size: 16px; color: var(--text-secondary, #5C4E3D); font-weight: 400; display: inline-flex; align-items: center;">',
        '<span style="line-height: 1;">', label_text, '</span>',
        '<span style="font-size: 22px; line-height: 1; margin-left: 8px;">&rarr;</span>',
        '</span>'
      ))
    })
    
    output$league_chart <- renderPlot({
      log_debug(">>> Rendering league comparison chart", level = "DEBUG")
      
      req(input$league, input$team, input$chart_metric)
      req(length(input$team) > 0, input$league != "")
      
      stats_df <- all_team_stats()
      req(stats_df, nrow(stats_df) > 0)
      
      metric <- input$chart_metric
      selected_teams <- input$team
      
      # Handle scatter plots
      if (metric %in% c("xg_scatter", "shot_quality_scatter", "pythag_scatter")) {
        render_scatter_chart(stats_df, metric, selected_teams)
      } else {
        render_bar_chart(stats_df, metric, selected_teams)
      }
    }, bg = "transparent")
    
    # =========================================================================
    # LEAGUE TABLE
    # =========================================================================
    
    output$league_table <- renderUI({
      log_debug(">>> Rendering league table", level = "DEBUG")
      
      req(input$league)
      
      stats_df <- all_team_stats()
      req(stats_df, nrow(stats_df) > 0)
      
      table_data <- stats_df %>%
        arrange(desc(xg_diff_pg)) %>%
        select(
          logo_path, team, matches,
          goals_for_pg, goals_against_pg, goal_diff_pg,
          xg_for_pg, xg_against_pg, xg_diff_pg,
          pythag_pct, xpythag_pct, luck_factor
        )
      
      reactable::reactable(
        table_data,
        sortable = TRUE,
        defaultPageSize = 20,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        theme = app_reactable_theme(),
        columns = list(
          logo_path = reactable::colDef(
            name = "",
            maxWidth = 40,
            sortable = FALSE,
            cell = function(value) {
              if (!is.null(value) && !is.na(value) && value != "") {
                htmltools::tags$img(
                  src = value, 
                  style = "width:20px; height:20px; object-fit:contain;",
                  onerror = "this.style.display='none';"
                )
              } else {
                ""
              }
            }
          ),
          team = reactable::colDef(
            name = "Team",
            minWidth = 120,
            sticky = "left"
          ),
          matches = reactable::colDef(name = "MP", maxWidth = 50),
          goals_for_pg = reactable::colDef(name = "GF/G", format = reactable::colFormat(digits = 2)),
          goals_against_pg = reactable::colDef(name = "GA/G", format = reactable::colFormat(digits = 2)),
          goal_diff_pg = reactable::colDef(name = "GD/G", format = reactable::colFormat(digits = 2)),
          xg_for_pg = reactable::colDef(name = "xGF/G", format = reactable::colFormat(digits = 2)),
          xg_against_pg = reactable::colDef(name = "xGA/G", format = reactable::colFormat(digits = 2)),
          xg_diff_pg = reactable::colDef(name = "xGD/G", format = reactable::colFormat(digits = 2)),
          pythag_pct = reactable::colDef(name = "Pyth%", format = reactable::colFormat(digits = 1)),
          xpythag_pct = reactable::colDef(name = "xPyth%", format = reactable::colFormat(digits = 1)),
          luck_factor = reactable::colDef(name = "Luck", format = reactable::colFormat(digits = 1))
        )
      )
    })
    
    # =========================================================================
    # LEAGUE RANKING TABLE
    # =========================================================================
    
    output$league_ranking <- renderUI({
      log_debug(">>> Rendering league ranking table", level = "DEBUG")
      
      req(input$league)
      
      stats_df <- all_team_stats()
      req(stats_df, nrow(stats_df) > 0)
      
      n_teams <- nrow(stats_df)
      
      # Calculate ranks for each metric (1 = best)
      ranking_data <- stats_df %>%
        mutate(
          # Higher is better - rank descending
          goals_for_rank = rank(-goals_for_pg, ties.method = "min"),
          xg_for_rank = rank(-xg_for_pg, ties.method = "min"),
          xg_vs_opp_rank = rank(-xg_vs_opp_avg, ties.method = "min"),
          pythag_rank = rank(-pythag_pct, ties.method = "min"),
          xpythag_rank = rank(-xpythag_pct, ties.method = "min"),
          
          # Lower is better - rank ascending
          goals_against_rank = rank(goals_against_pg, ties.method = "min"),
          xg_against_rank = rank(xg_against_pg, ties.method = "min"),
          xg_against_vs_opp_rank = rank(xg_against_vs_opp_avg, ties.method = "min")
        ) %>%
        arrange(xpythag_rank) %>%
        select(
          logo_path, team,
          goals_for_rank, goals_against_rank,
          xg_for_rank, xg_against_rank,
          xg_vs_opp_rank, xg_against_vs_opp_rank,
          pythag_rank, xpythag_rank
        )
      
      # Helper function for rank cell styling
      rank_cell <- function(value, n_teams) {
        if (is.na(value)) return("")
        # Color gradient: green (1st) to red (last)
        pct <- (value - 1) / (n_teams - 1)
        if (value <= 3) {
          bg_color <- "rgba(163, 190, 140, 0.3)"  # sage/green for top 3
        } else if (value >= n_teams - 2) {
          bg_color <- "rgba(191, 97, 106, 0.3)"   # coral/red for bottom 3
        } else {
          bg_color <- "transparent"
        }
        htmltools::div(
          style = sprintf("background: %s; padding: 4px 8px; border-radius: 4px; text-align: center; font-weight: 500;", bg_color),
          value
        )
      }
      
      reactable::reactable(
        ranking_data,
        sortable = TRUE,
        defaultPageSize = 20,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        theme = app_reactable_theme(),
        columns = list(
          logo_path = reactable::colDef(
            name = "",
            maxWidth = 40,
            sortable = FALSE,
            cell = function(value) {
              if (!is.null(value) && !is.na(value) && value != "") {
                htmltools::tags$img(
                  src = value, 
                  style = "width:20px; height:20px; object-fit:contain;",
                  onerror = "this.style.display='none';"
                )
              } else {
                ""
              }
            }
          ),
          team = reactable::colDef(name = "Team", minWidth = 120, sticky = "left"),
          goals_for_rank = reactable::colDef(
            name = "GF", 
            maxWidth = 60,
            cell = function(value) rank_cell(value, n_teams)
          ),
          goals_against_rank = reactable::colDef(
            name = "GA",
            maxWidth = 60,
            cell = function(value) rank_cell(value, n_teams)
          ),
          xg_for_rank = reactable::colDef(
            name = "xGF",
            maxWidth = 60,
            cell = function(value) rank_cell(value, n_teams)
          ),
          xg_against_rank = reactable::colDef(
            name = "xGA",
            maxWidth = 60,
            cell = function(value) rank_cell(value, n_teams)
          ),
          xg_vs_opp_rank = reactable::colDef(
            name = "xG+/-",
            maxWidth = 70,
            cell = function(value) rank_cell(value, n_teams)
          ),
          xg_against_vs_opp_rank = reactable::colDef(
            name = "xGA+/-",
            maxWidth = 70,
            cell = function(value) rank_cell(value, n_teams)
          ),
          pythag_rank = reactable::colDef(
            name = "Pyth%",
            maxWidth = 70,
            cell = function(value) rank_cell(value, n_teams)
          ),
          xpythag_rank = reactable::colDef(
            name = "xPyth%",
            maxWidth = 70,
            cell = function(value) rank_cell(value, n_teams)
          )
        )
      )
    })
    
  })
}

# =============================================================================
# CHART HELPER FUNCTIONS
# =============================================================================

#' Render bar chart for league comparison
render_bar_chart <- function(stats_df, metric, selected_teams) {
  # Determine if higher is better (for color coding)
  higher_is_better <- !grepl("_against_", metric)
  
  # Check if values can be negative (for axis handling)
  can_be_negative <- grepl("diff|vs_opp", metric)
  
  # Prepare data for plotting - always sort high to low (descending)
  plot_data <- stats_df %>%
    select(team, value = !!sym(metric)) %>%
    filter(!is.na(value)) %>%
    arrange(desc(value)) %>%
    mutate(
      rank = row_number(),
      is_selected = team %in% selected_teams,
      team = factor(team, levels = rev(team))
    )
  
  req(nrow(plot_data) > 0)
  
  # Colors matching the app theme
  bar_color <- if (higher_is_better) APP_COLORS$sage else APP_COLORS$coral
  highlight_color <- APP_COLORS$primary
  
  # Format labels based on metric type
  if (grepl("xg|xG", metric, ignore.case = TRUE)) {
    label_format <- function(x) sprintf("%.2f", x)
  } else if (grepl("shots", metric)) {
    label_format <- function(x) sprintf("%.1f", x)
  } else {
    label_format <- function(x) sprintf("%.2f", x)
  }
  
  # Create the bar chart
  if (can_be_negative) {
    # DIVERGING bar chart - team names inside, aligned based on value sign
    p <- ggplot(plot_data, aes(x = team, y = value, fill = is_selected)) +
      geom_col(width = 0.7, color = APP_COLORS$primary, linewidth = 0.5) +
      # Value labels (outside the bar) - same size as team names
      geom_text(
        aes(label = label_format(value),
            hjust = ifelse(value >= 0, -0.2, 1.2)),
        size = 5.5,
        fontface = ifelse(plot_data$is_selected, "bold", "plain"),
        color = APP_COLORS$primary
      ) +
      # Team name labels - positioned on opposite side of bar from value (+10% from previous)
      geom_text(
        aes(label = team,
            y = ifelse(value >= 0, -0.02, 0.02),
            hjust = ifelse(value >= 0, 1, 0)),
        size = 5.5,
        fontface = ifelse(plot_data$is_selected, "bold", "plain"),
        color = APP_COLORS$primary
      ) +
      scale_fill_manual(
        values = c("FALSE" = bar_color, "TRUE" = highlight_color),
        guide = "none"
      ) +
      coord_flip() +
      labs(x = NULL, y = NULL) +
      theme_app_bar() +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, margin = margin(b = 8)),
        axis.text.x.top = element_text(size = 16, margin = margin(b = 8))
      )
    
    # Calculate symmetric axis limits (same range on both sides of 0)
    max_abs_value <- max(abs(plot_data$value), na.rm = TRUE)
    # Add padding for labels (20% on each side)
    axis_limit <- max_abs_value * 1.2
    
    # Add baseline and symmetric limits, x-axis at top
    p <- p + 
      geom_hline(yintercept = 0, color = APP_COLORS$primary, linewidth = 0.8) +
      scale_y_continuous(
        limits = c(-axis_limit, axis_limit),
        expand = c(0, 0),
        position = "right"
      )
    
  } else {
    # STANDARD bar chart (all positive or all negative)
    # Create fontface vector for labels
    team_fontfaces <- ifelse(plot_data$is_selected, "bold", "plain")
    
    p <- ggplot(plot_data, aes(x = team, y = value, fill = is_selected)) +
      # Add baseline at 0 (same style as diverging)
      geom_hline(yintercept = 0, color = APP_COLORS$primary, linewidth = 0.8) +
      geom_col(width = 0.7, color = APP_COLORS$primary, linewidth = 0.5) +
      # Value labels (outside the bar)
      geom_text(
        aes(label = label_format(value)),
        hjust = ifelse(plot_data$value >= 0, -0.2, 1.2),
        size = 5.5,
        fontface = team_fontfaces,
        color = APP_COLORS$primary
      ) +
      # Team name labels (at y=0, right-aligned)
      geom_text(
        aes(label = team, y = 0),
        hjust = 1,
        nudge_y = -0.02 * max(abs(plot_data$value), na.rm = TRUE),
        size = 5.5,
        fontface = team_fontfaces,
        color = APP_COLORS$primary
      ) +
      scale_fill_manual(
        values = c("FALSE" = bar_color, "TRUE" = highlight_color),
        guide = "none"
      ) +
      coord_flip() +
      labs(x = NULL, y = NULL) +
      theme_app_bar() +
      theme(
        axis.text.y = element_blank(),  # Hide default axis labels
        axis.text.x = element_text(size = 16, margin = margin(b = 8)),
        axis.text.x.top = element_text(size = 16, margin = margin(b = 8))
      )
    
    # Expand y axis to fit labels, position at top (right after coord_flip)
    all_positive <- all(plot_data$value >= 0, na.rm = TRUE)
    all_negative <- all(plot_data$value <= 0, na.rm = TRUE)
    
    if (all_positive) {
      p <- p + scale_y_continuous(expand = expansion(mult = c(0.25, 0.15)), position = "right")
    } else if (all_negative) {
      p <- p + scale_y_continuous(expand = expansion(mult = c(0.15, 0.25)), position = "right")
    } else {
      p <- p + scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)), position = "right")
    }
  }
  
  p
}

#' Render scatter chart for advanced analysis
render_scatter_chart <- function(stats_df, metric, selected_teams) {
  plot_data <- stats_df %>%
    mutate(is_selected = team %in% selected_teams)
  
  if (metric == "xg_scatter") {
    # xG For vs xG Against scatterplot
    req(nrow(plot_data) > 0)
    
    # Calculate league averages for reference lines
    avg_xg_for <- mean(plot_data$xg_for_pg, na.rm = TRUE)
    avg_xg_against <- mean(plot_data$xg_against_pg, na.rm = TRUE)
    
    # Axis ranges - 10% padding
    x_range <- max(plot_data$xg_against_pg, na.rm = TRUE) - min(plot_data$xg_against_pg, na.rm = TRUE)
    y_range <- max(plot_data$xg_for_pg, na.rm = TRUE) - min(plot_data$xg_for_pg, na.rm = TRUE)
    
    x_min <- min(plot_data$xg_against_pg, na.rm = TRUE) - (x_range * 0.1)
    x_max <- max(plot_data$xg_against_pg, na.rm = TRUE) + (x_range * 0.1)
    y_min <- min(plot_data$xg_for_pg, na.rm = TRUE) - (y_range * 0.1)
    y_max <- max(plot_data$xg_for_pg, na.rm = TRUE) + (y_range * 0.1)
    
    p <- ggplot(plot_data, aes(x = xg_against_pg, y = xg_for_pg)) +
      # League average reference lines
      geom_hline(yintercept = avg_xg_for, linetype = "dashed", 
                 color = APP_COLORS$muted, linewidth = 0.6) +
      geom_vline(xintercept = avg_xg_against, linetype = "dashed", 
                 color = APP_COLORS$muted, linewidth = 0.6) +
      # Non-selected teams (size increased ~75%, slight transparency)
      geom_point(data = filter(plot_data, !is_selected),
                 size = 10.5, shape = 19, color = APP_COLORS$primary, alpha = 0.35) +
      # Selected teams (same size, full opacity)
      geom_point(data = filter(plot_data, is_selected),
                 size = 10.5, shape = 19, color = APP_COLORS$primary) +
      # Labels for selected teams only - no connector lines
      ggrepel::geom_text_repel(
        data = filter(plot_data, is_selected),
        aes(label = toupper(team)),
        size = 7,
        fontface = "bold",
        color = APP_COLORS$primary,
        box.padding = 1.0,
        point.padding = 1.0,
        segment.color = NA,
        min.segment.length = Inf
      ) +
      # Quadrant labels
      annotate("text", x = x_min, y = y_max, 
               label = "ELITE", fontface = "bold", size = 3, 
               color = APP_COLORS$sage, hjust = 0, vjust = 1) +
      annotate("text", x = x_max, y = y_max, 
               label = "OPEN", fontface = "bold", size = 3, 
               color = APP_COLORS$gold, hjust = 1, vjust = 1) +
      annotate("text", x = x_min, y = y_min, 
               label = "DEFENSIVE", fontface = "bold", size = 3, 
               color = APP_COLORS$frost, hjust = 0, vjust = 0) +
      annotate("text", x = x_max, y = y_min, 
               label = "STRUGGLING", fontface = "bold", size = 3, 
               color = APP_COLORS$coral, hjust = 1, vjust = 0) +
      # Axis setup - reversed x, no expansion
      scale_x_reverse(limits = c(x_max, x_min), expand = c(0, 0)) +
      scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0)) +
      labs(x = NULL, y = NULL)
    
  } else if (metric == "shot_quality_scatter") {
    # Shot Quality: Volume (shots per game) vs Total xG - for team's own shots only
    avg_shots <- mean(plot_data$shots_for_pg, na.rm = TRUE)
    avg_xg <- mean(plot_data$xg_for_pg, na.rm = TRUE)
    
    x_range <- max(plot_data$shots_for_pg, na.rm = TRUE) - min(plot_data$shots_for_pg, na.rm = TRUE)
    y_range <- max(plot_data$xg_for_pg, na.rm = TRUE) - min(plot_data$xg_for_pg, na.rm = TRUE)
    
    x_min <- min(plot_data$shots_for_pg, na.rm = TRUE) - (x_range * 0.1)
    x_max <- max(plot_data$shots_for_pg, na.rm = TRUE) + (x_range * 0.1)
    y_min <- min(plot_data$xg_for_pg, na.rm = TRUE) - (y_range * 0.1)
    y_max <- max(plot_data$xg_for_pg, na.rm = TRUE) + (y_range * 0.1)
    
    p <- ggplot(plot_data, aes(x = shots_for_pg, y = xg_for_pg)) +
      geom_hline(yintercept = avg_xg, linetype = "dashed", 
                 color = APP_COLORS$muted, linewidth = 0.6) +
      geom_vline(xintercept = avg_shots, linetype = "dashed", 
                 color = APP_COLORS$muted, linewidth = 0.6) +
      # Non-selected teams (size increased ~75%, slight transparency)
      geom_point(data = filter(plot_data, !is_selected),
                 size = 10.5, shape = 19, color = APP_COLORS$primary, alpha = 0.35) +
      # Selected teams (same size, full opacity)
      geom_point(data = filter(plot_data, is_selected),
                 size = 10.5, shape = 19, color = APP_COLORS$primary) +
      ggrepel::geom_text_repel(
        data = filter(plot_data, is_selected),
        aes(label = toupper(team)),
        size = 7,
        fontface = "bold",
        color = APP_COLORS$primary,
        box.padding = 1.0,
        point.padding = 1.0,
        segment.color = NA,
        min.segment.length = Inf
      ) +
      # Zone labels - high volume + high xG = elite attack
      annotate("text", x = x_max, y = y_max, 
               label = "ELITE ATTACK", fontface = "bold", size = 3, 
               color = APP_COLORS$sage, hjust = 1, vjust = 1) +
      annotate("text", x = x_min, y = y_max, 
               label = "CLINICAL", fontface = "bold", size = 3, 
               color = APP_COLORS$gold, hjust = 0, vjust = 1) +
      annotate("text", x = x_max, y = y_min, 
               label = "WASTEFUL", fontface = "bold", size = 3, 
               color = APP_COLORS$coral, hjust = 1, vjust = 0) +
      annotate("text", x = x_min, y = y_min, 
               label = "LOW THREAT", fontface = "bold", size = 3, 
               color = APP_COLORS$frost, hjust = 0, vjust = 0) +
      scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0)) +
      scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0)) +
      labs(x = NULL, y = NULL)
    
  } else if (metric == "pythag_scatter") {
    # Pythagorean vs xPythagorean
    min_val <- min(c(plot_data$pythag_pct, plot_data$xpythag_pct), na.rm = TRUE)
    max_val <- max(c(plot_data$pythag_pct, plot_data$xpythag_pct), na.rm = TRUE)
    val_range <- max_val - min_val
    axis_min <- min_val - (val_range * 0.1)
    axis_max <- max_val + (val_range * 0.1)
    
    p <- ggplot(plot_data, aes(x = xpythag_pct, y = pythag_pct)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
                  color = APP_COLORS$muted, linewidth = 0.6) +
      # Non-selected teams (size increased ~75%, slight transparency)
      geom_point(data = filter(plot_data, !is_selected),
                 size = 10.5, shape = 19, color = APP_COLORS$primary, alpha = 0.35) +
      # Selected teams (same size, full opacity)
      geom_point(data = filter(plot_data, is_selected),
                 size = 10.5, shape = 19, color = APP_COLORS$primary) +
      ggrepel::geom_text_repel(
        data = filter(plot_data, is_selected),
        aes(label = toupper(team)),
        size = 7,
        fontface = "bold",
        color = APP_COLORS$primary,
        box.padding = 1.0,
        point.padding = 1.0,
        segment.color = NA,
        min.segment.length = Inf
      ) +
      # Zone labels
      annotate("text", x = axis_min, y = axis_max, 
               label = "OVERPERFORMING", fontface = "bold", size = 3, 
               color = APP_COLORS$sage, hjust = 0, vjust = 1) +
      annotate("text", x = axis_max, y = axis_min, 
               label = "UNDERPERFORMING", fontface = "bold", size = 3, 
               color = APP_COLORS$coral, hjust = 1, vjust = 0) +
      scale_x_continuous(
        limits = c(axis_min, axis_max),
        labels = function(x) paste0(x, "%"),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        limits = c(axis_min, axis_max),
        labels = function(x) paste0(x, "%"),
        expand = c(0, 0)
      ) +
      labs(x = NULL, y = NULL)
  }
  
  p + theme_app_scatter()
}