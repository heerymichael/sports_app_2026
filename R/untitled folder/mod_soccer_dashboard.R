# =============================================================================
# Module: Soccer Dashboard
# 
# Team and player analytics dashboard for Soccer
# Shows team performance relative to league, player stats, and set piece analysis
# =============================================================================

#' Soccer Dashboard UI
#' @param id Module namespace ID
soccer_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_dashboard_ui() called with id:", id, level = "INFO")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Soccer Dashboard"),
      tags$p(class = "text-muted", "Team and player analytics across European leagues")
    ),
    
    # Filters card
    ui_card(
      title = "Filters",
      color = "sage",
      
      fluidRow(
        column(3,
               selectInput(ns("league"), "League",
                           choices = c("Loading..." = ""),
                           selected = NULL
               )
        ),
        column(3,
               shinyWidgets::pickerInput(ns("team"), "Team(s)",
                                         choices = c("Select league first"),
                                         selected = NULL,
                                         multiple = TRUE,
                                         options = shinyWidgets::pickerOptions(
                                           actionsBox = FALSE,
                                           liveSearch = TRUE,
                                           maxOptions = 2,
                                           noneSelectedText = "Select team(s)",
                                           selectedTextFormat = "count > 1",
                                           countSelectedText = "{0} teams",
                                           style = "btn-default"
                                         )
               )
        ),
        column(3,
               div(style = "padding-top: 25px;",
                   actionButton(ns("refresh_data"), "Refresh Data", 
                                class = "btn-action",
                                style = "width: 100%;"
                   )
               )
        ),
        column(3,
               # Cache status indicator
               div(style = "padding-top: 25px;",
                   uiOutput(ns("cache_status"))
               )
        )
      )
    ),
    
    tags$br(),
    
    # Team Performance Section
    uiOutput(ns("team_performance")),
    
    tags$br(),
    
    # League Comparison Chart Section
    ui_card(
      title = "League Comparison",
      color = "sage",
      
      fluidRow(
        column(4,
               selectInput(ns("chart_metric"), "Metric",
                           choices = c(
                             "Goals For (per game)" = "goals_for_pg",
                             "Goals Against (per game)" = "goals_against_pg",
                             "Goal Difference" = "goal_diff",
                             "Shots For (per game)" = "shots_for_pg",
                             "Shots Against (per game)" = "shots_against_pg",
                             "xG For (per game)" = "xg_for_pg",
                             "xG Against (per game)" = "xg_against_pg",
                             "xG Difference" = "xg_diff"
                           ),
                           selected = "goals_for_pg"
               )
        ),
        column(8,
               div(style = "font-size: 0.85rem; color: var(--text-muted); padding-top: 30px;",
                   "Teams ranked by selected metric. Your selected team is highlighted."
               )
        )
      ),
      
      tags$br(),
      
      plotOutput(ns("league_chart"), height = "500px")
    ),
    
    tags$br(),
    
    # Player Stats Section
    uiOutput(ns("player_stats")),
    
    tags$br(),
    
    # Set Piece Analysis Section
    uiOutput(ns("set_piece_analysis"))
  )
}

#' Soccer Dashboard Server
#' @param id Module namespace ID
soccer_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_dashboard_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    rv <- reactiveValues(
      shooting_data = NULL,
      possession_data = NULL,
      shot_data = NULL,
      all_team_stats = NULL,  # Cache for league comparison
      loading = FALSE,
      initialized = FALSE,
      last_refresh = NULL
    )
    
    # =========================================================================
    # DATA LOADING (with caching)
    # =========================================================================
    
    # Initial data load (uses cache if available)
    observe({
      log_debug(">>> Initial data load observer triggered", level = "DEBUG")
      
      # Only run once on init
      if (rv$initialized) return()
      
      rv$loading <- TRUE
      
      tryCatch({
        log_debug("Initializing Google Sheets...", level = "INFO")
        init_google_sheets()
        
        # These will use cache if available and valid
        log_debug("Loading shooting summary data (checking cache)...", level = "INFO")
        rv$shooting_data <- load_shooting_summary(force_refresh = FALSE)
        
        log_debug("Loading possession data (checking cache)...", level = "INFO")
        rv$possession_data <- load_possession_data(force_refresh = FALSE)
        
        log_debug("Loading shot data (checking cache)...", level = "INFO")
        rv$shot_data <- load_shot_data(force_refresh = FALSE)
        
        rv$last_refresh <- Sys.time()
        rv$initialized <- TRUE
        
        log_debug("All soccer data loaded successfully", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error loading soccer data:", e$message, level = "ERROR")
      })
      
      rv$loading <- FALSE
    })
    
    # Manual refresh (force refresh from Google Sheets)
    observeEvent(input$refresh_data, {
      log_debug(">>> Manual refresh triggered", level = "INFO")
      
      rv$loading <- TRUE
      rv$all_team_stats <- NULL  # Clear cached team stats
      
      tryCatch({
        init_google_sheets()
        
        # Force refresh bypasses cache
        rv$shooting_data <- load_shooting_summary(force_refresh = TRUE)
        rv$possession_data <- load_possession_data(force_refresh = TRUE)
        rv$shot_data <- load_shot_data(force_refresh = TRUE)
        
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
      
      log_debug(">>> Calculating all team stats for league comparison", level = "INFO")
      
      calculate_all_team_stats(rv$shot_data, input$league)
    })
    
    # =========================================================================
    # CACHE STATUS DISPLAY
    # =========================================================================
    
    output$cache_status <- renderUI({
      # Re-render when refresh happens
      input$refresh_data
      
      status <- get_cache_status()
      
      # Check if all caches exist
      all_cached <- all(sapply(status, function(s) s$exists))
      
      if (all_cached) {
        div(
          style = "font-size: 0.8rem; color: var(--text-muted);",
          sprintf("Data cached %s", status$shots$age)
        )
      } else {
        div(
          style = "font-size: 0.8rem; color: var(--accent-coral);",
          "Loading fresh data..."
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
        updateSelectInput(session, "league",
                          choices = setNames(leagues, leagues),
                          selected = leagues[1]
        )
      }
    })
    
    # =========================================================================
    # UPDATE TEAM CHOICES
    # =========================================================================
    
    observe({
      log_debug(">>> Updating team choices for league:", input$league, level = "DEBUG")
      
      req(input$league, input$league != "")
      req(rv$shot_data)
      
      teams <- get_league_teams(rv$shot_data, input$league)
      log_debug("Teams found:", length(teams), level = "INFO")
      
      if (length(teams) > 0) {
        shinyWidgets::updatePickerInput(session, "team",
                                        choices = teams,
                                        selected = teams[1]  # Select first team by default
        )
      } else {
        shinyWidgets::updatePickerInput(session, "team",
                                        choices = character(0),
                                        selected = NULL
        )
      }
    })
    
    # =========================================================================
    # TEAM PERFORMANCE OUTPUT
    # =========================================================================
    
    output$team_performance <- renderUI({
      log_debug(">>> Rendering team performance", level = "DEBUG")
      
      req(input$league, input$team)
      req(length(input$team) > 0, input$league != "")
      req(rv$shot_data)
      
      # Get stats from the all_team_stats reactive (more efficient)
      stats_df <- all_team_stats()
      
      req(stats_df)
      
      selected_teams <- input$team
      
      # Filter to selected teams
      team_rows <- stats_df %>% filter(team %in% selected_teams)
      
      if (nrow(team_rows) == 0) {
        return(
          ui_card(
            title = "Team Performance",
            color = "sage",
            div(class = "text-muted", "No match data available for selected team(s)")
          )
        )
      }
      
      # Calculate league averages
      league_avg <- list(
        goals_pg = mean(stats_df$goals_for_pg, na.rm = TRUE),
        shots_pg = mean(stats_df$shots_for_pg, na.rm = TRUE),
        xg_pg = mean(stats_df$xg_for_pg, na.rm = TRUE)
      )
      
      # Helper to create comparison indicator
      comparison_indicator <- function(value, avg, higher_is_better = TRUE) {
        if (is.na(avg) || avg == 0) return("")
        
        diff_pct <- ((value - avg) / avg) * 100
        
        if (abs(diff_pct) < 5) {
          return(span(style = "color: var(--text-muted); font-size: 0.75rem;", "≈ avg"))
        }
        
        if ((diff_pct > 0 && higher_is_better) || (diff_pct < 0 && !higher_is_better)) {
          color <- "var(--accent-sage)"
          arrow <- "▲"
        } else {
          color <- "var(--accent-coral)"
          arrow <- "▼"
        }
        
        span(
          style = sprintf("color: %s; font-size: 0.75rem; font-weight: 600;", color),
          sprintf("%s %.0f%%", arrow, abs(diff_pct))
        )
      }
      
      # Build UI for each team
      team_cards <- lapply(seq_len(nrow(team_rows)), function(i) {
        team_stats <- as.list(team_rows[i, ])
        
        div(
          style = if (length(selected_teams) > 1) "flex: 1; min-width: 300px;" else "width: 100%;",
          
          # Team header
          div(
            style = "margin-bottom: 1rem; padding-bottom: 0.5rem; border-bottom: 2px solid var(--accent-sage);",
            tags$h4(style = "margin: 0; font-size: 1.1rem;", team_stats$team),
            div(style = "font-size: 0.8rem; color: var(--text-muted);",
                sprintf("%d matches | GD: %+d | xGD: %+.1f", 
                        team_stats$matches, team_stats$goal_diff, team_stats$xg_diff)
            )
          ),
          
          # Stats grid for this team
          div(
            style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
            
            # Goals
            div(
              style = "text-align: center;",
              div(style = "font-size: 0.7rem; color: var(--text-muted); text-transform: uppercase; margin-bottom: 0.25rem;", "Goals/Game"),
              div(
                style = "display: flex; justify-content: center; gap: 1rem;",
                div(
                  div(style = "font-size: 1.4rem; font-weight: 700; color: var(--accent-sage);", team_stats$goals_for_pg),
                  div(style = "font-size: 0.65rem; color: var(--text-muted);", "FOR")
                ),
                div(
                  div(style = "font-size: 1.4rem; font-weight: 700; color: var(--accent-coral);", team_stats$goals_against_pg),
                  div(style = "font-size: 0.65rem; color: var(--text-muted);", "AGT")
                )
              )
            ),
            
            # Shots
            div(
              style = "text-align: center;",
              div(style = "font-size: 0.7rem; color: var(--text-muted); text-transform: uppercase; margin-bottom: 0.25rem;", "Shots/Game"),
              div(
                style = "display: flex; justify-content: center; gap: 1rem;",
                div(
                  div(style = "font-size: 1.4rem; font-weight: 700; color: var(--accent-sage);", team_stats$shots_for_pg),
                  div(style = "font-size: 0.65rem; color: var(--text-muted);", "FOR")
                ),
                div(
                  div(style = "font-size: 1.4rem; font-weight: 700; color: var(--accent-coral);", team_stats$shots_against_pg),
                  div(style = "font-size: 0.65rem; color: var(--text-muted);", "AGT")
                )
              )
            ),
            
            # xG
            div(
              style = "text-align: center;",
              div(style = "font-size: 0.7rem; color: var(--text-muted); text-transform: uppercase; margin-bottom: 0.25rem;", "xG/Game"),
              div(
                style = "display: flex; justify-content: center; gap: 1rem;",
                div(
                  div(style = "font-size: 1.4rem; font-weight: 700; color: var(--accent-sage);", team_stats$xg_for_pg),
                  div(style = "font-size: 0.65rem; color: var(--text-muted);", "FOR")
                ),
                div(
                  div(style = "font-size: 1.4rem; font-weight: 700; color: var(--accent-coral);", team_stats$xg_against_pg),
                  div(style = "font-size: 0.65rem; color: var(--text-muted);", "AGT")
                )
              )
            )
          )
        )
      })
      
      ui_card(
        title = if (length(selected_teams) == 1) {
          paste(selected_teams[1], "- Team Performance")
        } else {
          "Team Comparison"
        },
        color = "sage",
        
        # Container for team cards (side by side if multiple)
        div(
          style = if (length(selected_teams) > 1) {
            "display: flex; gap: 2rem; flex-wrap: wrap;"
          } else {
            ""
          },
          team_cards
        ),
        
        # League average context
        tags$hr(style = "margin: 1.5rem 0; border-color: var(--bg-secondary);"),
        div(
          style = "text-align: center; font-size: 0.8rem; color: var(--text-muted);",
          sprintf("League average per team: %.2f goals, %.1f shots, %.2f xG per game",
                  league_avg$goals_pg, league_avg$shots_pg, league_avg$xg_pg)
        )
      )
    })
    
    # =========================================================================
    # LEAGUE COMPARISON CHART
    # =========================================================================
    
    output$league_chart <- renderPlot({
      log_debug(">>> Rendering league comparison chart", level = "DEBUG")
      
      req(input$league, input$team, input$chart_metric)
      req(length(input$team) > 0, input$league != "")
      
      stats_df <- all_team_stats()
      req(stats_df, nrow(stats_df) > 0)
      
      metric <- input$chart_metric
      selected_teams <- input$team  # Now can be multiple
      
      # Metric labels for display
      metric_labels <- c(
        "goals_for_pg" = "Goals For (per game)",
        "goals_against_pg" = "Goals Against (per game)",
        "goal_diff" = "Goal Difference",
        "shots_for_pg" = "Shots For (per game)",
        "shots_against_pg" = "Shots Against (per game)",
        "xg_for_pg" = "xG For (per game)",
        "xg_against_pg" = "xG Against (per game)",
        "xg_diff" = "xG Difference"
      )
      
      # Determine if higher is better (for sorting direction)
      higher_is_better <- !grepl("against", metric)
      
      # Prepare data for plotting
      plot_data <- stats_df %>%
        select(team, value = !!sym(metric)) %>%
        filter(!is.na(value)) %>%  # Remove NA values
        arrange(if (higher_is_better) desc(value) else value) %>%
        mutate(
          rank = row_number(),
          is_selected = team %in% selected_teams,
          team = factor(team, levels = rev(team))  # Reverse for horizontal bar chart
        )
      
      req(nrow(plot_data) > 0)
      
      # Colors matching the app theme
      bar_color <- if (higher_is_better) "#A3BE8C" else "#D08770"  # sage or coral
      highlight_color <- "#3B3226"  # dark brown for selected teams
      
      # Create the plot
      p <- ggplot(plot_data, aes(x = team, y = value, fill = is_selected)) +
        geom_col(width = 0.7, color = "#3B3226", linewidth = 0.5) +
        geom_text(
          aes(label = sprintf("%.2f", value)),
          hjust = ifelse(plot_data$value >= 0, -0.2, 1.2),
          size = 3,
          fontface = ifelse(plot_data$is_selected, "bold", "plain"),
          color = "#3B3226"
        ) +
        scale_fill_manual(
          values = c("FALSE" = bar_color, "TRUE" = highlight_color),
          guide = "none"
        ) +
        coord_flip() +
        labs(
          title = metric_labels[metric],
          subtitle = sprintf("All %s teams ranked | %s highlighted", 
                             input$league, 
                             paste(selected_teams, collapse = " & ")),
          x = NULL,
          y = NULL
        ) +
        theme_minimal(base_family = "sans") +
        theme(
          plot.title = element_text(size = 14, face = "bold", color = "#3B3226"),
          plot.subtitle = element_text(size = 10, color = "#7A7A7A"),
          axis.text.y = element_text(size = 9, color = "#5C4E3D"),
          axis.text.x = element_text(size = 9, color = "#5C4E3D"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "#E5E9F0", linewidth = 0.5),
          plot.background = element_rect(fill = "#FFFFFF", color = NA),
          panel.background = element_rect(fill = "#FFFFFF", color = NA)
        )
      
      # Expand y axis to fit labels
      all_positive <- all(plot_data$value >= 0, na.rm = TRUE)
      if (all_positive) {
        p <- p + scale_y_continuous(expand = expansion(mult = c(0.02, 0.15)))
      } else {
        p <- p + scale_y_continuous(expand = expansion(mult = c(0.15, 0.15)))
      }
      
      print(p)
    }, res = 96)
    
    # =========================================================================
    # PLAYER STATS OUTPUT
    # =========================================================================
    
    output$player_stats <- renderUI({
      log_debug(">>> Rendering player stats", level = "DEBUG")
      
      req(input$league, input$team)
      req(length(input$team) > 0, input$league != "")
      req(rv$shot_data, rv$possession_data)
      
      selected_teams <- input$team
      
      # Build player stats tables for each selected team
      team_tables <- lapply(selected_teams, function(current_team) {
        # Get player stats for this team
        player_data <- get_player_stats(
          rv$shooting_data, rv$shot_data, rv$possession_data,
          input$league, current_team
        )
        
        if (is.null(player_data) || nrow(player_data) == 0) {
          return(
            div(
              style = if (length(selected_teams) > 1) "flex: 1; min-width: 400px;" else "width: 100%;",
              tags$h4(style = "margin-bottom: 0.5rem;", current_team),
              div(class = "text-muted", "No player data available")
            )
          )
        }
        
        # Sort by minutes
        player_data <- player_data %>%
          arrange(desc(minutes))
        
        # Determine which columns we have
        has_sca <- "sca" %in% names(player_data) && !all(is.na(player_data$sca))
        has_assists <- "assists" %in% names(player_data) && !all(is.na(player_data$assists))
        has_touches_zones <- "touches_att_3rd" %in% names(player_data)
        
        div(
          style = if (length(selected_teams) > 1) "flex: 1; min-width: 400px;" else "width: 100%;",
          
          if (length(selected_teams) > 1) {
            tags$h4(style = "margin-bottom: 0.5rem; padding-bottom: 0.25rem; border-bottom: 2px solid var(--accent-sage);", 
                    current_team)
          },
          
          div(
            style = "overflow-x: auto;",
            tags$table(
              class = "projections-table",
              style = "width: 100%; border-collapse: collapse; font-size: 0.85rem;",
              
              # Header
              tags$thead(
                tags$tr(
                  tags$th(style = "text-align: left; padding: 0.5rem;", "Player"),
                  tags$th(style = "text-align: center; padding: 0.5rem; width: 50px;", "Pos"),
                  tags$th(style = "text-align: center; padding: 0.5rem; width: 50px;", "GP"),
                  tags$th(style = "text-align: center; padding: 0.5rem; width: 50px;", "M/G"),
                  tags$th(style = "text-align: center; padding: 0.5rem; width: 40px;", "G"),
                  tags$th(style = "text-align: center; padding: 0.5rem; width: 50px;", "xG"),
                  tags$th(style = "text-align: center; padding: 0.5rem; width: 40px;", "Sh"),
                  if (has_assists) tags$th(style = "text-align: center; padding: 0.5rem; width: 40px;", "A"),
                  if (has_sca) tags$th(style = "text-align: center; padding: 0.5rem; width: 50px;", "SCA"),
                  if (has_touches_zones) tags$th(style = "text-align: center; padding: 0.5rem; width: 50px;", "T.A3"),
                  if (has_touches_zones) tags$th(style = "text-align: center; padding: 0.5rem; width: 50px;", "T.Box"),
                  tags$th(style = "text-align: center; padding: 0.5rem; width: 50px;", "Prog")
                )
              ),
              
              # Body
              tags$tbody(
                lapply(1:min(nrow(player_data), 25), function(i) {  # Limit to 25 rows
                  row <- player_data[i, ]
                  
                  tags$tr(
                    # Player name
                    tags$td(
                      style = "padding: 0.5rem; font-weight: 600;",
                      row$player
                    ),
                    
                    # Position
                    tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      span(class = "position-badge", 
                           style = "font-size: 0.65rem;",
                           if (!is.na(row$position)) substr(row$position, 1, 4) else "-"
                      )
                    ),
                    
                    # Matches
                    tags$td(style = "text-align: center; padding: 0.5rem;", row$matches),
                    
                    # Minutes per game
                    tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      if (!is.na(row$mins_per_match)) row$mins_per_match else "-"
                    ),
                    
                    # Goals
                    tags$td(
                      style = "text-align: center; padding: 0.5rem; font-weight: 600;",
                      if (!is.na(row$goals) && row$goals > 0) row$goals else "-"
                    ),
                    
                    # xG
                    tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      if (!is.na(row$xg) && row$xg > 0) sprintf("%.1f", row$xg) else "-"
                    ),
                    
                    # Shots
                    tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      if (!is.na(row$shots) && row$shots > 0) row$shots else "-"
                    ),
                    
                    # Assists (if available)
                    if (has_assists) tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      if (!is.na(row$assists) && row$assists > 0) row$assists else "-"
                    ),
                    
                    # SCA (if available)
                    if (has_sca) tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      if (!is.na(row$sca) && row$sca > 0) row$sca else "-"
                    ),
                    
                    # Touches in attacking third (if available)
                    if (has_touches_zones) tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      if ("touches_att_3rd" %in% names(row) && !is.na(row$touches_att_3rd)) row$touches_att_3rd else "-"
                    ),
                    
                    # Touches in box (if available)
                    if (has_touches_zones) tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      if ("touches_att_pen_area" %in% names(row) && !is.na(row$touches_att_pen_area)) row$touches_att_pen_area else "-"
                    ),
                    
                    # Progressive actions
                    tags$td(
                      style = "text-align: center; padding: 0.5rem;",
                      if (!is.na(row$progressive_actions) && row$progressive_actions > 0) row$progressive_actions else "-"
                    )
                  )
                })
              )
            )
          )
        )
      })
      
      ui_card(
        title = sprintf("Player Statistics%s", 
                        if (length(selected_teams) == 1) paste0(" - ", selected_teams[1]) else ""),
        color = "sage",
        
        div(
          style = if (length(selected_teams) > 1) "display: flex; gap: 2rem; flex-wrap: wrap;" else "",
          team_tables
        )
      )
    })
    
    # =========================================================================
    # SET PIECE ANALYSIS OUTPUT
    # =========================================================================
    
    output$set_piece_analysis <- renderUI({
      log_debug(">>> Rendering set piece analysis", level = "DEBUG")
      
      req(input$league, input$team)
      req(length(input$team) > 0, input$league != "")
      req(rv$shot_data, rv$possession_data)
      
      selected_teams <- input$team
      
      # Build set piece analysis for each team
      team_analyses <- lapply(selected_teams, function(current_team) {
        # Get set piece specialists for the team
        specialists <- get_set_piece_specialists(
          rv$shot_data, rv$possession_data,
          input$league, current_team
        )
        
        # Get all set piece shots for the team
        sp_shots <- get_set_piece_shots(rv$shot_data, input$league, current_team)
        
        sp_headers <- sp_shots %>% filter(body_part == "Head")
        
        sp_goals <- sum(sp_shots$outcome == "Goal", na.rm = TRUE)
        sp_xg <- sum(sp_shots$xg_shot, na.rm = TRUE)
        
        div(
          style = if (length(selected_teams) > 1) "flex: 1; min-width: 350px;" else "width: 100%;",
          
          if (length(selected_teams) > 1) {
            tags$h4(style = "margin-bottom: 0.5rem; padding-bottom: 0.25rem; border-bottom: 2px solid var(--accent-sage);", 
                    current_team)
          },
          
          # Summary stats
          div(
            style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 0.75rem; margin-bottom: 1rem;",
            
            div(
              class = "stat-mini",
              div(class = "stat-mini__label", "SP Shots"),
              div(class = "stat-mini__value", nrow(sp_shots))
            ),
            
            div(
              class = "stat-mini",
              div(class = "stat-mini__label", "Headers"),
              div(class = "stat-mini__value stat-mini__value--info", nrow(sp_headers))
            ),
            
            div(
              class = "stat-mini",
              div(class = "stat-mini__label", "Goals"),
              div(class = "stat-mini__value stat-mini__value--success", sp_goals)
            ),
            
            div(
              class = "stat-mini",
              div(class = "stat-mini__label", "xG"),
              div(class = "stat-mini__value stat-mini__value--primary", sprintf("%.1f", sp_xg))
            )
          ),
          
          # Header specialists table
          if (nrow(specialists) > 0) {
            tagList(
              tags$h5(style = "margin-bottom: 0.5rem; font-size: 0.85rem; color: var(--text-muted);", 
                      "Header Threats (Def/Mid)"
              ),
              
              div(
                style = "overflow-x: auto;",
                tags$table(
                  class = "projections-table",
                  style = "width: 100%; border-collapse: collapse; font-size: 0.8rem;",
                  
                  tags$thead(
                    tags$tr(
                      tags$th(style = "text-align: left; padding: 0.4rem;", "Player"),
                      tags$th(style = "text-align: center; padding: 0.4rem;", "Pos"),
                      tags$th(style = "text-align: center; padding: 0.4rem;", "Hdr"),
                      tags$th(style = "text-align: center; padding: 0.4rem;", "G"),
                      tags$th(style = "text-align: center; padding: 0.4rem;", "xG")
                    )
                  ),
                  
                  tags$tbody(
                    lapply(1:min(nrow(specialists), 6), function(i) {
                      row <- specialists[i, ]
                      
                      tags$tr(
                        tags$td(style = "padding: 0.4rem; font-weight: 600;", row$player),
                        tags$td(style = "text-align: center; padding: 0.4rem;", 
                                span(class = "position-badge", style = "font-size: 0.6rem;",
                                     if (!is.na(row$position)) substr(row$position, 1, 4) else "-"
                                )
                        ),
                        tags$td(style = "text-align: center; padding: 0.4rem; font-weight: 700;", row$set_piece_headers),
                        tags$td(style = "text-align: center; padding: 0.4rem; color: var(--accent-sage);", row$sp_header_goals),
                        tags$td(style = "text-align: center; padding: 0.4rem;", sprintf("%.1f", row$sp_header_xg))
                      )
                    })
                  )
                )
              )
            )
          } else {
            div(class = "text-muted", style = "font-size: 0.8rem;",
                "No SP header data"
            )
          }
        )
      })
      
      ui_card(
        title = "Set Piece Analysis",
        subtitle = "Shots from dead ball situations (corners, free kicks)",
        color = "sage",
        
        div(
          style = if (length(selected_teams) > 1) "display: flex; gap: 2rem; flex-wrap: wrap;" else "",
          team_analyses
        )
      )
    })
    
  })
}