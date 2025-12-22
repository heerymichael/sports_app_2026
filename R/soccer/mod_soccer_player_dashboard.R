# =============================================================================
# Module: Soccer Player Dashboard - UPDATED
# 
# Player statistics and set piece analysis
# UPDATED: Uses combined player_data (Summary + Possession) with Parquet caching
# Dependencies: app_themes.R, soccer_config.R, soccer_cache.R, soccer_data_loader.R, 
#              soccer_transforms.R
# =============================================================================

#' Soccer Player Dashboard UI
#' @param id Module namespace ID
soccer_player_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_player_dashboard_ui() called with id:", id, level = "INFO")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Player Dashboard"),
      tags$p(class = "text-muted", "Player statistics and set piece analysis")
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
                     "↻ Refresh"
                   ),
                   uiOutput(ns("cache_status"))
               )
        )
      )
    ),
    
    tags$br(),
    
    # Player Stats Section
    ui_card(
      title = "Player Statistics",
      color = "sage",
      
      # Controls row
      fluidRow(
        column(2,
               selectInput(ns("player_view"), "View",
                           choices = c("Per Game" = "per_game", "By Gameweek" = "by_gameweek"),
                           selected = "per_game"
               )
        ),
        column(2,
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'by_gameweek'", ns("player_view")),
                 selectInput(ns("gameweek_metric"), "Metric",
                             choices = c(
                               "Goals" = "goals",
                               "xG" = "xg",
                               "Shots" = "shots",
                               "Minutes" = "minutes"
                             ),
                             selected = "xg"
                 )
               )
        ),
        column(3,
               shinyWidgets::pickerInput(ns("player_team_filter"), "Team Filter",
                                         choices = c("All Teams"),
                                         selected = NULL,
                                         multiple = TRUE,
                                         options = shinyWidgets::pickerOptions(
                                           actionsBox = TRUE,
                                           liveSearch = TRUE,
                                           noneSelectedText = "All Teams",
                                           selectedTextFormat = "count > 2",
                                           countSelectedText = "{0} teams"
                                         )
               )
        ),
        column(3,
               shinyWidgets::pickerInput(ns("player_position_filter"), "Position Filter",
                                         choices = c("FW", "MF", "DF", "GK"),
                                         selected = c("FW", "MF", "DF"),
                                         multiple = TRUE,
                                         options = shinyWidgets::pickerOptions(
                                           actionsBox = TRUE,
                                           noneSelectedText = "All Positions"
                                         )
               )
        ),
        column(2,
               div(style = "padding-top: 25px;",
                   checkboxInput(ns("show_all_teams"), "Show all league teams", value = FALSE)
               )
        )
      ),
      
      tags$hr(style = "margin: 0.5rem 0 1rem 0; border-color: var(--bg-secondary);"),
      
      # Table output
      uiOutput(ns("player_stats_table"))
    ),
    
    tags$br(),
    
    # Set Piece Analysis Section
    uiOutput(ns("set_piece_analysis"))
  )
}

#' Soccer Player Dashboard Server
#' @param id Module namespace ID
soccer_player_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_player_dashboard_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES - UPDATED: Combined player_data
    # =========================================================================
    
    rv <- reactiveValues(
      player_data = NULL,    # Combined Summary + Possession data
      shot_data = NULL,      # Individual shots (separate grain)
      loading = FALSE,
      initialized = FALSE,
      last_refresh = NULL
    )
    
    # =========================================================================
    # DATA LOADING (with Parquet caching)
    # =========================================================================
    
    observe({
      log_debug(">>> Initial data load observer triggered", level = "DEBUG")
      
      if (rv$initialized) return()
      
      rv$loading <- TRUE
      
      tryCatch({
        log_debug("Initializing Google Sheets...", level = "INFO")
        init_google_sheets()
        
        # UPDATED: Single call for combined player data
        log_debug("Loading player match stats (checking cache)...", level = "INFO")
        rv$player_data <- load_player_match_stats(force_refresh = FALSE)
        
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
    
    # Manual refresh
    observeEvent(input$refresh_data, {
      log_debug(">>> Manual refresh triggered", level = "INFO")
      
      rv$loading <- TRUE
      
      tryCatch({
        init_google_sheets()
        rv$player_data <- load_player_match_stats(force_refresh = TRUE)
        rv$shot_data <- load_shot_data(force_refresh = TRUE)
        rv$last_refresh <- Sys.time()
        log_debug("Manual refresh completed", level = "INFO")
      }, error = function(e) {
        log_debug("Error during manual refresh:", e$message, level = "ERROR")
      })
      
      rv$loading <- FALSE
    })
    
    # =========================================================================
    # CACHE STATUS DISPLAY
    # =========================================================================
    
    output$cache_status <- renderUI({
      input$refresh_data
      
      if (!is.null(rv$last_refresh)) {
        age <- difftime(Sys.time(), rv$last_refresh, units = "mins")
        age_text <- if (age < 1) {
          "just now"
        } else if (age < 60) {
          sprintf("%.0f min ago", age)
        } else {
          sprintf("%.1f hrs ago", age / 60)
        }
        
        div(
          style = "font-size: 0.7rem; color: var(--text-muted); font-style: italic;",
          sprintf("Data loaded %s", age_text)
        )
      } else {
        div(
          style = "font-size: 0.7rem; color: var(--accent-coral); font-style: italic;",
          "Loading data..."
        )
      }
    })
    
    # =========================================================================
    # UPDATE LEAGUE CHOICES
    # =========================================================================
    
    observe({
      log_debug(">>> Updating league choices", level = "DEBUG")
      
      req(rv$player_data)
      
      leagues <- get_available_leagues(rv$player_data)
      log_debug("Available leagues:", paste(leagues, collapse = ", "), level = "INFO")
      
      if (length(leagues) > 0) {
        default_league <- if ("Premier League" %in% leagues) "Premier League" else leagues[1]
        
        updateSelectInput(session, "league",
                          choices = setNames(leagues, leagues),
                          selected = default_league
        )
      }
    })
    
    # =========================================================================
    # UPDATE TEAM CHOICES
    # =========================================================================
    
    observeEvent(input$league, {
      log_debug(">>> League changed to:", input$league, level = "DEBUG")
      
      req(input$league, input$league != "")
      req(rv$player_data)
      
      teams <- get_league_teams(rv$player_data, input$league)
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
        
        # Also update player team filter with logos
        shinyWidgets::updatePickerInput(session, "player_team_filter",
                                        choices = teams,
                                        selected = NULL,
                                        choicesOpt = list(content = team_content)
        )
      }
    })
    
    # =========================================================================
    # PLAYER STATS TABLE
    # =========================================================================
    
    output$player_stats_table <- renderUI({
      log_debug(">>> Rendering player stats table", level = "DEBUG")
      
      req(input$league)
      req(rv$player_data, rv$shot_data)
      
      view_type <- input$player_view
      
      # Determine which teams to show
      if (input$show_all_teams) {
        teams_to_show <- get_league_teams(rv$player_data, input$league)
      } else if (!is.null(input$player_team_filter) && length(input$player_team_filter) > 0) {
        teams_to_show <- input$player_team_filter
      } else if (!is.null(input$team) && length(input$team) > 0) {
        teams_to_show <- input$team
      } else {
        return(div(class = "text-muted", "Select a team to view player statistics"))
      }
      
      # Position filter
      position_filter <- input$player_position_filter
      if (is.null(position_filter) || length(position_filter) == 0) {
        position_filter <- c("FW", "MF", "DF", "GK")
      }
      
      if (view_type == "per_game") {
        render_player_stats_per_game(
          rv$player_data, rv$shot_data,
          input$league, teams_to_show, position_filter
        )
      } else {
        render_player_stats_by_gameweek(
          rv$player_data, rv$shot_data,
          input$league, teams_to_show, position_filter,
          input$gameweek_metric
        )
      }
    })
    
    # =========================================================================
    # SET PIECE ANALYSIS OUTPUT
    # =========================================================================
    
    output$set_piece_analysis <- renderUI({
      log_debug(">>> Rendering set piece analysis", level = "DEBUG")
      
      req(input$league, input$team)
      req(length(input$team) > 0, input$league != "")
      req(rv$shot_data, rv$player_data)
      
      selected_teams <- input$team
      
      # Build set piece analysis for each team
      team_analyses <- lapply(selected_teams, function(current_team) {
        specialists <- get_set_piece_specialists(
          rv$shot_data, rv$player_data,
          input$league, current_team
        )
        
        sp_shots <- get_set_piece_shots(rv$shot_data, input$league, current_team)
        sp_headers <- sp_shots %>% filter(body_part == "Head")
        sp_goals <- sum(sp_shots$outcome == "Goal", na.rm = TRUE)
        sp_xg <- sum(sp_shots$xg_shot, na.rm = TRUE)
        
        team_logo <- get_soccer_team_logo(current_team)
        
        div(
          style = if (length(selected_teams) > 1) "flex: 1; min-width: 350px;" else "width: 100%;",
          
          if (length(selected_teams) > 1) {
            div(
              style = "display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.5rem; padding-bottom: 0.25rem; border-bottom: 2px solid var(--accent-sage);",
              if (!is.null(team_logo)) {
                tags$img(src = team_logo, alt = current_team, style = "width: 28px; height: 28px; object-fit: contain;")
              },
              tags$h4(style = "margin: 0;", current_team)
            )
          },
          
          # Summary stats
          div(
            style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 0.75rem; margin-bottom: 1rem;",
            
            div(class = "stat-mini",
                div(class = "stat-mini__label", "SP Shots"),
                div(class = "stat-mini__value", nrow(sp_shots))
            ),
            
            div(class = "stat-mini",
                div(class = "stat-mini__label", "Headers"),
                div(class = "stat-mini__value stat-mini__value--info", nrow(sp_headers))
            ),
            
            div(class = "stat-mini",
                div(class = "stat-mini__label", "Goals"),
                div(class = "stat-mini__value stat-mini__value--success", sp_goals)
            ),
            
            div(class = "stat-mini",
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

# =============================================================================
# PLAYER STATS HELPER FUNCTIONS - UPDATED
# =============================================================================

#' Render per-game player stats table
#' @param player_data Combined player match stats
#' @param shot_data Individual shot data
#' @param league League name
#' @param teams Vector of team names
#' @param position_filter Vector of positions to include
render_player_stats_per_game <- function(player_data, shot_data,
                                         league, teams, position_filter) {
  log_debug(">>> render_player_stats_per_game for", length(teams), "teams", level = "DEBUG")
  
  # Gather stats for all requested teams
  all_players <- lapply(teams, function(t) {
    stats <- get_player_stats(player_data, shot_data, league, t)
    if (nrow(stats) > 0) {
      stats$team <- t
      stats$logo_path <- get_soccer_team_logo(t)
    }
    stats
  })
  
  combined <- bind_rows(all_players)
  
  if (nrow(combined) == 0) {
    return(div(class = "text-muted", "No player data available"))
  }
  
  # Filter by position
  combined <- combined %>%
    filter(grepl(paste(position_filter, collapse = "|"), position, ignore.case = TRUE) | 
             is.na(position))
  
  # Calculate per-90 stats
  combined <- combined %>%
    mutate(
      goals_p90 = round(goals / pmax(minutes, 1) * 90, 2),
      xg_p90 = round(xg / pmax(minutes, 1) * 90, 2),
      shots_p90 = round(shots / pmax(minutes, 1) * 90, 2)
    ) %>%
    arrange(desc(xg)) %>%
    head(100)
  
  # Build reactable
  reactable::reactable(
    combined %>% select(player, team, logo_path, position, matches, minutes, 
                        goals, xg, shots, goals_p90, xg_p90, shots_p90),
    searchable = TRUE,
    sortable = TRUE,
    defaultPageSize = 25,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(25, 50, 100),
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    theme = app_reactable_theme(),
    columns = list(
      player = reactable::colDef(name = "Player", minWidth = 140, sticky = "left"),
      team = reactable::colDef(
        name = "Team",
        minWidth = 50,
        cell = function(value, index) {
          logo <- combined$logo_path[index]
          if (!is.null(logo) && !is.na(logo) && logo != "") {
            htmltools::tags$img(src = logo, style = "width:20px; height:20px; vertical-align:middle;")
          } else {
            ""
          }
        }
      ),
      logo_path = reactable::colDef(show = FALSE),
      position = reactable::colDef(name = "Pos", maxWidth = 60),
      matches = reactable::colDef(name = "MP", maxWidth = 50),
      minutes = reactable::colDef(name = "Mins", maxWidth = 60),
      goals = reactable::colDef(name = "G", maxWidth = 50),
      xg = reactable::colDef(name = "xG", format = reactable::colFormat(digits = 2)),
      shots = reactable::colDef(name = "Sh", maxWidth = 50),
      goals_p90 = reactable::colDef(name = "G/90", format = reactable::colFormat(digits = 2)),
      xg_p90 = reactable::colDef(name = "xG/90", format = reactable::colFormat(digits = 2)),
      shots_p90 = reactable::colDef(name = "Sh/90", format = reactable::colFormat(digits = 2))
    )
  )
}

#' Render player stats by gameweek
#' @param player_data Combined player match stats
#' @param shot_data Individual shot data
#' @param league League name
#' @param teams Vector of team names
#' @param position_filter Vector of positions to include
#' @param metric Metric to display
render_player_stats_by_gameweek <- function(player_data, shot_data,
                                            league, teams, position_filter, metric) {
  log_debug(">>> render_player_stats_by_gameweek, metric:", metric, level = "DEBUG")
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Get player-gameweek data from combined player_data
  if (!is.null(player_data) && nrow(player_data) > 0) {
    player_gw <- player_data %>%
      filter(league == league_data_name) %>%
      filter(team %in% teams | length(teams) == 0)
  } else {
    # Fallback: Aggregate from shot data
    player_gw <- shot_data %>%
      filter(league == league_data_name) %>%
      filter(team %in% teams | length(teams) == 0) %>%
      group_by(player, team, gameweek) %>%
      summarise(
        goals = sum(outcome == "Goal", na.rm = TRUE),
        shots = n(),
        xg = sum(xg_shot, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  if (nrow(player_gw) == 0) {
    return(div(class = "text-muted", "No gameweek data available"))
  }
  
  # Get position info (already in combined data)
  if (!"position" %in% names(player_gw)) {
    player_positions <- player_data %>%
      filter(league == league_data_name) %>%
      distinct(player, position) %>%
      group_by(player) %>%
      slice(1) %>%
      ungroup()
    
    player_gw <- player_gw %>%
      left_join(player_positions, by = "player")
  }
  
  player_gw <- player_gw %>%
    filter(grepl(paste(position_filter, collapse = "|"), position, ignore.case = TRUE) | 
             is.na(position))
  
  # Get logo paths
  team_logos <- setNames(
    sapply(unique(player_gw$team), get_soccer_team_logo),
    unique(player_gw$team)
  )
  player_gw$logo_path <- team_logos[player_gw$team]
  
  # Pivot to wide format
  metric_col <- switch(metric,
                       goals = "goals",
                       xg = "xg",
                       shots = "shots",
                       minutes = "minutes"
  )
  
  # Make sure metric column exists
  if (!metric_col %in% names(player_gw)) {
    return(div(class = "text-muted", sprintf("Metric '%s' not available", metric)))
  }
  
  wide_data <- player_gw %>%
    filter(!is.na(gameweek)) %>%
    mutate(gameweek = paste0("GW", gameweek)) %>%
    select(player, team, logo_path, gameweek, value = !!sym(metric_col)) %>%
    group_by(player, team, logo_path) %>%
    mutate(total = sum(as.numeric(value), na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = gameweek, values_from = value, values_fill = 0) %>%
    arrange(desc(total)) %>%
    head(50)
  
  # Get gameweek columns in order
  gw_cols <- names(wide_data)[grepl("^GW", names(wide_data))]
  gw_nums <- as.numeric(gsub("GW", "", gw_cols))
  gw_cols <- gw_cols[order(gw_nums)]
  
  metric_labels <- c(goals = "Goals", xg = "xG", shots = "Shots", minutes = "Minutes")
  
  # Build column definitions
  col_defs <- list(
    player = reactable::colDef(name = "Player", minWidth = 140, sticky = "left"),
    total = reactable::colDef(name = "Total", sticky = "left",
                              cell = if (metric == "xg") {
                                function(value) sprintf("%.1f", value)
                              } else {
                                function(value) as.character(as.integer(value))
                              }),
    team = reactable::colDef(
      name = "Team",
      minWidth = 50,
      sticky = "left",
      cell = function(value, index) {
        logo <- wide_data$logo_path[index]
        if (!is.null(logo) && !is.na(logo) && logo != "") {
          htmltools::tags$img(src = logo, style = "width:20px; height:20px; vertical-align:middle;")
        } else {
          ""
        }
      }
    ),
    logo_path = reactable::colDef(show = FALSE)
  )
  
  # Add gameweek column definitions
  for (gw in gw_cols) {
    col_defs[[gw]] <- reactable::colDef(
      name = gsub("GW", "", gw),
      align = "center",
      maxWidth = 55,
      cell = if (metric == "xg") {
        function(value) if (is.na(value) || value == 0) "" else sprintf("%.2f", value)
      } else {
        function(value) if (is.na(value) || value == 0) "" else as.character(as.integer(value))
      }
    )
  }
  
  tagList(
    tags$p(
      style = "font-size: 0.85rem; color: var(--text-muted); margin-bottom: 0.5rem;",
      sprintf("Showing %s by gameweek (top 50 players by total)", metric_labels[metric])
    ),
    reactable::reactable(
      wide_data,
      searchable = TRUE,
      sortable = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      theme = app_reactable_theme(),
      columns = col_defs
    )
  )
}