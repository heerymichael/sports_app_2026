################################################################################
# MODULE: Soccer Showdown
#
# Single-match lineup builder for FanTeam Soccer Showdown contests.
#
# Architecture:
#   1. Load match data (salaries + lineup) from Google Sheets
#   2. User adjusts match odds -> team-level stat predictions update
#   3. Player projections generated from team stats + individual data
#   4. Handbuild or optimize 5-player lineups (1 CPT + 4 FLEX)
#
# Dependencies:
#   - soccer_showdown_config.R (constants, data loaders, projection engine)
#   - app_themes.R (ui_card, APP_COLORS, app_reactable_theme)
#   - lpSolve (optimization)
################################################################################

# =============================================================================
# UI
# =============================================================================

soccer_showdown_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Soccer Showdown"),
      tags$p(class = "text-muted",
             "Single-match lineup builder | 5 players, 1 CPT (1.5x) | Budget: 53M")
    ),
    
    # =========================================================================
    # SETTINGS CARD
    # =========================================================================
    ui_card(
      title = "Match Selection",
      color = APP_COLORS$sage,
      
      fluidRow(
        column(4,
               selectInput(ns("match_sheet"), "Select Match",
                           choices = c("Loading..." = ""),
                           selected = NULL
               )
        ),
        column(2,
               numericInput(ns("salary_cap"), "Salary Cap",
                            value = SHOWDOWN_RULES$salary_cap,
                            min = 30, max = 100, step = 0.5)
        ),
        column(2,
               actionButton(ns("load_data"), "Load Data",
                            class = "btn-primary",
                            style = "margin-top: 25px; width: 100%;")
        ),
        column(4,
               div(style = "margin-top: 25px; padding: 0.5rem; background: var(--bg-secondary); border-radius: 6px; font-size: 0.82rem;",
                   tags$strong("Format: "),
                   "5 players | 1 CPT (1.5x pts) | No position limits | CS stack penalty"
               )
        )
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # YOUR VIEW - ODDS ADJUSTMENT
    # =========================================================================
    ui_card(
      title = "Your View",
      color = APP_COLORS$sage,
      
      div(
        style = "font-size: 0.85rem; color: var(--text-muted); margin-bottom: 1rem;",
        "Adjust the odds below to express your view on this match. ",
        "If your view differs from the market, see how it changes the stats that feed into fantasy scoring."
      ),
      
      fluidRow(
        # Left: Result probabilities
        column(6,
               tags$h4("Result Probability",
                       style = "margin-bottom: 0.75rem; font-weight: 600; text-align: center;"),
               fluidRow(
                 column(4,
                        numericInput(ns("home_win_pct"), "Home Win %",
                                     value = 50, min = 0, max = 100, step = 1)
                 ),
                 column(4,
                        numericInput(ns("draw_pct"), "Draw %",
                                     value = 25, min = 0, max = 100, step = 1)
                 ),
                 column(4,
                        numericInput(ns("away_win_pct"), "Away Win %",
                                     value = 25, min = 0, max = 100, step = 1)
                 )
               ),
               
               # Probability bar visualization
               uiOutput(ns("prob_bar")),
               
               tags$hr(style = "margin: 1rem 0;"),
               
               tags$h4("Total Match Goals",
                       style = "margin-bottom: 0.5rem; font-weight: 600; text-align: center;"),
               div(
                 style = "max-width: 160px; margin: 0 auto;",
                 numericInput(ns("total_goals"), NULL,
                              value = 2.5, min = 0.5, max = 7.0, step = 0.1)
               ),
               
               div(
                 style = "text-align: center; margin-top: 1rem;",
                 actionButton(ns("update_view"), "Update View",
                              icon = icon("refresh"),
                              class = "btn-primary",
                              style = paste0(
                                "background: ", APP_COLORS$sage, "; ",
                                "color: white; font-weight: 700; font-size: 0.95rem; ",
                                "border: 3px solid var(--outline); border-radius: 10px; ",
                                "padding: 0.6rem 2rem; ",
                                "box-shadow: 3px 3px 0 var(--shadow);"
                              ))
               )
        ),
        
        # Right: Implied stats output
        column(6,
               tags$h4("Implied Stats",
                       style = "margin-bottom: 0.75rem; font-weight: 600; text-align: center;"),
               uiOutput(ns("implied_stats_display"))
        )
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # MATCH SIMULATION - Score & Shot Matrices
    # =========================================================================
    ui_card(
      title = "Match Simulation",
      color = APP_COLORS$sage,
      
      div(
        style = "font-size: 0.85rem; color: var(--text-muted); margin-bottom: 1rem;",
        "Bivariate Poisson simulation calibrated to your view. ",
        "The correlation parameter (\u03C1) is fitted to match your draw probability."
      ),
      
      fluidRow(
        column(3,
               selectInput(ns("sim_matrix_type"), "Matrix",
                           choices = c("Score" = "score",
                                       "Shots" = "shots",
                                       "Shots on Target" = "sot"),
                           selected = "score")
        ),
        column(9,
               uiOutput(ns("sim_calibration_info"))
        )
      ),
      
      # Simulated result probabilities bar (score matrix only)
      uiOutput(ns("sim_result_bar")),
      
      tags$br(),
      
      # Single matrix display (centred)
      div(
        style = "display: flex; justify-content: center;",
        uiOutput(ns("sim_matrix_html"))
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # LINEUP CONFIRMATION
    # =========================================================================
    ui_card(
      title = "Confirmed Lineups",
      color = APP_COLORS$sage,
      
      div(
        style = "font-size: 0.85rem; color: var(--text-muted); margin-bottom: 0.75rem;",
        "Starting lineups from the data sheet. Override status by clicking the lineup badge. ",
        "Only 'expected' and 'possible' players receive projections."
      ),
      
      fluidRow(
        column(6, uiOutput(ns("home_lineup_display"))),
        column(6, uiOutput(ns("away_lineup_display")))
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # PLAYER PROJECTIONS TABLE
    # =========================================================================
    ui_card(
      title = "Player Projections",
      color = APP_COLORS$sage,
      
      fluidRow(
        column(3,
               selectInput(ns("proj_filter_team"), "Filter Team",
                           choices = c("All" = "all"), selected = "all")
        ),
        column(3,
               selectInput(ns("proj_filter_pos"), "Filter Position",
                           choices = c("All" = "all", "GK", "DEF", "MID", "FWD"),
                           selected = "all")
        ),
        column(3,
               selectInput(ns("proj_filter_lineup"), "Filter Lineup Status",
                           choices = c("Starters" = "starters", "All" = "all"),
                           selected = "starters")
        ),
        column(3,
               div(style = "margin-top: 25px;",
                   actionButton(ns("recalc_projections"), "Recalculate",
                                class = "btn-primary", style = "width: 100%;")
               )
        )
      ),
      
      reactableOutput(ns("projections_table"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # LINEUP BUILDER
    # =========================================================================
    ui_card(
      title = "Lineup Builder",
      color = APP_COLORS$sage,
      
      fluidRow(
        column(4,
               tags$h4("Build Settings", style = "font-weight: 600;"),
               selectInput(ns("cpt_lock"), "Lock Captain",
                           choices = c("Auto-optimize" = ""), selected = ""),
               checkboxGroupInput(ns("excluded_players"), "Exclude Players",
                                  choices = c(), selected = NULL),
               tags$hr(),
               numericInput(ns("n_lineups"), "Number of Lineups",
                            value = 5, min = 1, max = 20, step = 1),
               sliderInput(ns("variance_pct"), "Variance %",
                           min = 0, max = 50, value = 15, step = 5),
               actionButton(ns("generate_lineups"), "Generate Lineups",
                            class = "btn-primary", style = "width: 100%; margin-top: 0.5rem;")
        ),
        column(8,
               uiOutput(ns("lineups_display"))
        )
      )
    ),
    
    # =========================================================================
    # HANDBUILD SECTION
    # =========================================================================
    tags$br(),
    
    ui_card(
      title = "Handbuild",
      color = APP_COLORS$sage,
      
      div(
        style = "font-size: 0.85rem; color: var(--text-muted); margin-bottom: 0.75rem;",
        "Click players in the projections table to add them to your lineup. ",
        "Click the CPT badge to toggle captain."
      ),
      
      uiOutput(ns("handbuild_lineup")),
      
      fluidRow(
        column(6,
               actionButton(ns("clear_handbuild"), "Clear Lineup",
                            class = "btn-outline-secondary", style = "margin-top: 0.5rem;")
        ),
        column(6,
               actionButton(ns("add_to_handbuild"), "Add Selected Player",
                            class = "btn-primary", style = "margin-top: 0.5rem; width: 100%;")
        )
      )
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

soccer_showdown_server <- function(id, soccer_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_showdown_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    rv <- reactiveValues(
      players         = NULL,    # Raw player data from Google Sheet
      notonlyfpl      = NULL,    # NotOnlyFPL summary stats
      merged_players  = NULL,    # Players merged with stats + projections
      match_info      = NULL,    # Parsed match details (home/away teams)
      home_stats      = NULL,    # Predicted team stats for home
      away_stats      = NULL,    # Predicted team stats for away
      lineup_overrides = list(), # Manual lineup status overrides
      handbuild_slots = list(),  # Current handbuild lineup
      handbuild_cpt   = NULL,    # Player ID of captain in handbuild
      generated_lineups = NULL,  # Generated lineup results
      confirmed_view  = NULL,    # Confirmed odds snapshot (updated by button)
      initialized     = FALSE
    )
    
    # =========================================================================
    # INITIALIZATION: Load available match sheets
    # =========================================================================
    observe({
      if (rv$initialized) return()
      
      tryCatch({
        sheets <- get_showdown_sheets()
        
        if (length(sheets) > 0) {
          choices <- setNames(sheets, sapply(sheets, function(s) {
            info <- parse_sheet_name(s)
            if (!is.null(info)) info$display_label else s
          }))
          
          updateSelectInput(session, "match_sheet",
                            choices = choices,
                            selected = choices[1])
        } else {
          updateSelectInput(session, "match_sheet",
                            choices = c("No matches found" = ""),
                            selected = "")
        }
      }, error = function(e) {
        log_debug(paste("Error loading sheets:", e$message), level = "ERROR")
      })
      
      rv$initialized <- TRUE
    })
    
    # =========================================================================
    # LOAD DATA
    # =========================================================================
    observeEvent(input$load_data, {
      req(input$match_sheet, input$match_sheet != "")
      
      log_debug(sprintf("Loading data for: %s", input$match_sheet), level = "INFO")
      
      # Parse match info
      rv$match_info <- parse_sheet_name(input$match_sheet)
      
      # Load player salary data
      withProgress(message = "Loading player data...", value = 0.3, {
        rv$players <- load_showdown_players(input$match_sheet)
        
        setProgress(0.6, message = "Loading player stats...")
        rv$notonlyfpl <- load_notonlyfpl_summary()
        
        setProgress(0.8, message = "Matching players...")
        if (!is.null(rv$players)) {
          # Merge with NotOnlyFPL data
          rv$merged_players <- merge_player_data(rv$players, rv$notonlyfpl)
          
          # Update team filter
          teams <- unique(rv$players$club_abbrev)
          team_choices <- c("All" = "all", setNames(teams, sapply(teams, function(t) {
            ifelse(!is.na(TEAM_DISPLAY_NAMES[t]), TEAM_DISPLAY_NAMES[t], t)
          })))
          updateSelectInput(session, "proj_filter_team", choices = team_choices)
          
          # Update captain lock choices
          starter_players <- rv$merged_players[rv$merged_players$lineup %in% c("expected", "possible"), ]
          cpt_choices <- c("Auto-optimize" = "")
          if (nrow(starter_players) > 0) {
            cpt_choices <- c(cpt_choices, setNames(
              starter_players$player_id,
              paste(starter_players$display_name, "-", starter_players$position,
                    sprintf("(%.1f)", starter_players$price))
            ))
          }
          updateSelectInput(session, "cpt_lock", choices = cpt_choices)
          
          # Update exclude choices
          if (nrow(starter_players) > 0) {
            excl_choices <- setNames(
              starter_players$player_id,
              paste(starter_players$display_name, starter_players$position)
            )
            updateCheckboxGroupInput(session, "excluded_players",
                                     choices = excl_choices, selected = NULL)
          }
          
          # Reset overrides
          rv$lineup_overrides <- list()
          rv$handbuild_slots <- list()
          rv$handbuild_cpt <- NULL
          rv$generated_lineups <- NULL
        }
        
        # -----------------------------------------------------------------
        # Load market odds for this match and populate inputs
        # -----------------------------------------------------------------
        setProgress(0.9, message = "Loading market odds...")
        tryCatch({
          odds <- load_fanteam_odds(rv$match_info$gameweek)
          if (!is.null(odds) && nrow(odds) > 0 && "home_away" %in% names(odds)) {
            home_canonical <- names(TEAM_ABBREVIATIONS)[
              match(rv$match_info$home_abbrev, TEAM_ABBREVIATIONS)
            ]
            
            if (!is.na(home_canonical) && "odds_team_normalized" %in% names(odds)) {
              home_row <- odds[
                tolower(trimws(as.character(odds$home_away))) %in% c("home", "h") &
                  odds$odds_team_normalized == home_canonical, 
              ]
              
              if (nrow(home_row) > 0) {
                hw <- round(as.numeric(home_row$win_pct[1]))
                dr <- round(as.numeric(home_row$draw_pct[1]))
                aw <- 100 - hw - dr
                tg <- if ("implied_total" %in% names(home_row)) {
                  round(as.numeric(home_row$implied_total[1]), 1)
                } else if ("implied_team_goals" %in% names(home_row) && 
                           "implied_opp_goals" %in% names(home_row)) {
                  round(as.numeric(home_row$implied_team_goals[1]) + 
                          as.numeric(home_row$implied_opp_goals[1]), 1)
                } else 2.5
                
                updateNumericInput(session, "home_win_pct", value = hw)
                updateNumericInput(session, "draw_pct", value = dr)
                updateNumericInput(session, "away_win_pct", value = aw)
                updateNumericInput(session, "total_goals", value = tg)
                
                # Auto-confirm with market odds
                rv$confirmed_view <- list(
                  home_win = hw, draw = dr, away_win = aw, total_goals = tg
                )
                
                log_debug(sprintf("Odds loaded: H%d%% D%d%% A%d%% Total:%.1f", hw, dr, aw, tg), level = "INFO")
              } else {
                log_debug("No matching home row in odds for:", home_canonical, level = "WARN")
                # Confirm defaults so simulation still works
                rv$confirmed_view <- list(home_win = 50, draw = 25, away_win = 25, total_goals = 2.5)
              }
            } else {
              rv$confirmed_view <- list(home_win = 50, draw = 25, away_win = 25, total_goals = 2.5)
            }
          } else {
            log_debug("No odds data available for GW", rv$match_info$gameweek, level = "WARN")
            rv$confirmed_view <- list(home_win = 50, draw = 25, away_win = 25, total_goals = 2.5)
          }
        }, error = function(e) {
          log_debug("Odds load failed:", e$message, level = "WARN")
          rv$confirmed_view <- list(home_win = 50, draw = 25, away_win = 25, total_goals = 2.5)
        })
        
        setProgress(1.0, message = "Done!")
      })
    })
    
    # =========================================================================
    # HELPER: Merge player data with NotOnlyFPL stats
    # =========================================================================
    merge_player_data <- function(players, notonlyfpl) {
      if (is.null(notonlyfpl) || nrow(notonlyfpl) == 0) {
        # No stats available, add empty columns
        players$total_pts    <- NA_real_
        players$avg_pts      <- NA_real_
        players$matches_played <- NA_real_
        players$mins_per_game <- NA_real_
        players$goals_per_game <- NA_real_
        players$assists_per_game <- NA_real_
        players$shots_per_game <- NA_real_
        players$sot_per_game <- NA_real_
        players$saves_per_game <- NA_real_
        players$yellows_per_game <- NA_real_
        players$cs_total     <- NA_real_
        players$matched      <- FALSE
        return(players)
      }
      
      # Match by normalized name
      players$matched <- FALSE
      stats_cols <- c("total_pts", "avg_pts", "matches", "mins_per_game",
                      "goals_per_game", "assists_per_game", "shots_per_game",
                      "sot_per_game", "saves_per_game", "yellows_per_game",
                      "clean_sheets")
      
      for (col in stats_cols) {
        players[[col]] <- NA_real_
      }
      
      for (i in seq_len(nrow(players))) {
        player_name <- players$display_name[i]
        player_norm <- players$name_normalized[i]
        player_club <- players$club_abbrev[i]
        
        # Try exact normalized match
        match_idx <- which(notonlyfpl$name_normalized == player_norm)
        
        # If no match, try last name only
        if (length(match_idx) == 0) {
          last_name <- normalize_name(players$name[i])
          match_idx <- which(grepl(last_name, notonlyfpl$name_normalized, fixed = TRUE))
        }
        
        # If multiple matches, try to disambiguate by club
        if (length(match_idx) > 1 && !is.na(player_club)) {
          club_match <- match_idx[sapply(match_idx, function(j) {
            nfpl_team <- notonlyfpl$team[j]
            if (is.na(nfpl_team)) return(FALSE)
            nfpl_abbrev <- TEAM_ABBREV_MAP[nfpl_team]
            if (!is.na(nfpl_abbrev)) return(unname(nfpl_abbrev) == player_club)
            return(grepl(player_club, nfpl_team, ignore.case = TRUE))
          })]
          if (length(club_match) > 0) match_idx <- club_match[1]
          else match_idx <- match_idx[1]
        }
        
        if (length(match_idx) == 1) {
          for (col in stats_cols) {
            players[[col]][i] <- notonlyfpl[[col]][match_idx]
          }
          players$matched[i] <- TRUE
        }
      }
      
      # Rename for clarity
      names(players)[names(players) == "matches"] <- "matches_played"
      names(players)[names(players) == "clean_sheets"] <- "cs_total"
      
      log_debug(sprintf("Matched %d / %d players with NotOnlyFPL data",
                        sum(players$matched), nrow(players)), level = "INFO")
      
      return(players)
    }
    
    # =========================================================================
    # REACTIVE: Get effective lineup status (with overrides)
    # =========================================================================
    effective_lineup <- reactive({
      req(rv$merged_players)
      df <- rv$merged_players
      
      # Apply overrides
      for (pid in names(rv$lineup_overrides)) {
        idx <- which(df$player_id == as.integer(pid))
        if (length(idx) > 0) {
          df$lineup[idx] <- rv$lineup_overrides[[pid]]
        }
      }
      
      df
    })
    
    # =========================================================================
    # UPDATE VIEW: Snapshot inputs on button press
    # =========================================================================
    observeEvent(input$update_view, {
      rv$confirmed_view <- list(
        home_win    = input$home_win_pct %||% 50,
        draw        = input$draw_pct %||% 25,
        away_win    = input$away_win_pct %||% 25,
        total_goals = input$total_goals %||% 2.5
      )
    })
    
    # =========================================================================
    # REACTIVE: Calculate team stats from confirmed view
    # =========================================================================
    team_predictions <- reactive({
      req(rv$confirmed_view)
      req(rv$match_info)
      
      view <- rv$confirmed_view
      
      # Calculate implied goals per team
      impl <- calculate_implied_goals(
        view$home_win, view$draw, view$away_win, view$total_goals
      )
      
      # Predict team-level stats
      home_stats <- predict_team_stats(impl$home_goals, impl$away_goals, is_away = FALSE)
      away_stats <- predict_team_stats(impl$away_goals, impl$home_goals, is_away = TRUE)
      
      list(
        home_goals = impl$home_goals,
        away_goals = impl$away_goals,
        home_stats = home_stats,
        away_stats = away_stats
      )
    })
    
    # =========================================================================
    # REACTIVE: Player projections
    # =========================================================================
    player_projections <- reactive({
      req(rv$merged_players)
      preds <- team_predictions()
      req(preds)
      
      df <- effective_lineup()
      info <- rv$match_info
      
      # Count starters per position per team
      starters <- df[df$lineup %in% c("expected", "possible"), ]
      
      home_starters <- starters[starters$club_abbrev == info$home_abbrev, ]
      away_starters <- starters[starters$club_abbrev == info$away_abbrev, ]
      
      home_pos_counts <- table(factor(home_starters$position, levels = c("GK", "DEF", "MID", "FWD")))
      away_pos_counts <- table(factor(away_starters$position, levels = c("GK", "DEF", "MID", "FWD")))
      
      # Calculate position average PPG for adjustment
      pos_avgs <- tapply(starters$avg_pts, starters$position, mean, na.rm = TRUE)
      
      # Generate projections for each player
      df$projected_pts <- NA_real_
      df$proj_breakdown <- vector("list", nrow(df))
      
      for (i in seq_len(nrow(df))) {
        if (!df$lineup[i] %in% c("expected", "possible")) next
        
        is_home <- df$club_abbrev[i] == info$home_abbrev
        team_stats <- if (is_home) preds$home_stats else preds$away_stats
        opp_stats  <- if (is_home) preds$away_stats else preds$home_stats
        
        pos_counts <- if (is_home) home_pos_counts else away_pos_counts
        pos <- df$position[i]
        n_at_pos <- as.integer(pos_counts[pos])
        if (is.na(n_at_pos) || n_at_pos == 0) n_at_pos <- 1
        
        # Build player summary for individual adjustment
        player_summary <- if (df$matched[i]) {
          list(
            avg_pts = df$avg_pts[i],
            mins_per_game = df$mins_per_game[i]
          )
        } else NULL
        
        pos_avg <- if (!is.na(pos_avgs[pos])) pos_avgs[pos] else NULL
        
        result <- project_player_points(
          player       = df[i, ],
          team_stats   = team_stats,
          opp_stats    = opp_stats,
          starters_at_pos = n_at_pos,
          player_summary = player_summary,
          pos_avg_pts  = pos_avg
        )
        
        df$projected_pts[i] <- result$projected_pts
        df$proj_breakdown[[i]] <- result$breakdown
      }
      
      # Calculate value
      df$value <- ifelse(!is.na(df$projected_pts) & df$price > 0,
                         round(df$projected_pts / df$price, 2), NA)
      
      # CPT effective points
      df$cpt_pts <- ifelse(!is.na(df$projected_pts),
                           round(df$projected_pts * SHOWDOWN_RULES$cpt_multiplier, 1), NA)
      
      df
    })
    
    # =========================================================================
    # RECALCULATE BUTTON
    # =========================================================================
    observeEvent(input$recalc_projections, {
      # Force recalculation by invalidating (the reactive chain handles it)
      rv$merged_players <- rv$merged_players
    })
    
    # =========================================================================
    # RENDER: Probability bar
    # =========================================================================
    output$prob_bar <- renderUI({
      hw <- input$home_win_pct %||% 50
      dr <- input$draw_pct %||% 25
      aw <- input$away_win_pct %||% 25
      total <- hw + dr + aw
      if (total == 0) total <- 100
      
      hw_pct <- round(hw / total * 100)
      dr_pct <- round(dr / total * 100)
      aw_pct <- 100 - hw_pct - dr_pct
      
      div(
        style = "display: flex; height: 52px; border-radius: 10px; overflow: hidden; margin-top: 0.75rem; border: 3px solid var(--outline);",
        div(style = sprintf("width: %d%%; background: %s; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.9rem; font-weight: 700;",
                            hw_pct, APP_COLORS$sage),
            sprintf("H %d%%", hw_pct)),
        div(style = sprintf("width: %d%%; background: #78909C; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.9rem; font-weight: 700;",
                            dr_pct),
            sprintf("D %d%%", dr_pct)),
        div(style = sprintf("width: %d%%; background: %s; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.9rem; font-weight: 700;",
                            aw_pct, APP_COLORS$coral),
            sprintf("A %d%%", aw_pct))
      )
    })
    
    # =========================================================================
    # RENDER: Implied stats display
    # =========================================================================
    output$implied_stats_display <- renderUI({
      preds <- tryCatch(team_predictions(), error = function(e) NULL)
      if (is.null(preds)) return(div("Adjust odds and load data to see predictions"))
      
      info <- rv$match_info
      if (is.null(info)) return(div("Load a match first"))
      
      home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
                          TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
      away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
                          TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
      
      # Get team logos
      home_logo <- get_logo_from_abbrev(info$home_abbrev)
      away_logo <- get_logo_from_abbrev(info$away_abbrev)
      
      hs <- preds$home_stats
      as <- preds$away_stats
      
      # Logo element helper
      logo_el <- function(logo_path, label) {
        if (!is.null(logo_path)) {
          tags$img(src = logo_path,
                   style = "width: 36px; height: 36px; object-fit: contain;",
                   title = label, alt = label)
        } else {
          span(style = "font-weight: 700; font-size: 1rem;", label)
        }
      }
      
      stat_row <- function(label, home_val, away_val, fmt = "%.1f") {
        div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 0.3rem 0; border-bottom: 1px solid #F0F0F0;",
          span(style = "width: 25%; text-align: right; font-weight: 500;", sprintf(fmt, home_val)),
          span(style = "width: 50%; text-align: center; color: var(--text-muted); font-size: 0.82rem;", label),
          span(style = "width: 25%; text-align: left; font-weight: 500;", sprintf(fmt, away_val))
        )
      }
      
      div(
        # Team headers with logos
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.75rem; padding: 0.5rem 0; border-bottom: 2px solid var(--outline);",
          div(style = "display: flex; align-items: center; gap: 0.5rem;",
              logo_el(home_logo, home_name),
              span(style = "font-weight: 600; font-size: 0.8rem; color: var(--text-muted);", "(H)")
          ),
          div(style = "display: flex; align-items: center; gap: 0.5rem;",
              span(style = "font-weight: 600; font-size: 0.8rem; color: var(--text-muted);", "(A)"),
              logo_el(away_logo, away_name)
          )
        ),
        
        stat_row("Implied Goals", preds$home_goals, preds$away_goals, "%.2f"),
        stat_row("Predicted Shots", hs$shots, as$shots),
        stat_row("Predicted SoT", hs$sot, as$sot),
        stat_row("Predicted Yellows", hs$yellow_cards, as$yellow_cards),
        stat_row("Clean Sheet %", hs$cs_pct, as$cs_pct, "%.0f%%"),
        
        # Summary insight
        div(
          style = "margin-top: 1rem; padding: 0.75rem; background: var(--bg-secondary); border-radius: 6px; font-size: 0.82rem;",
          tags$strong("Summary: "),
          sprintf(
            "Your view implies %s scores %.2f goals (CS %.0f%%) and %s scores %.2f goals (CS %.0f%%). Total: %.1f goals.",
            home_name, preds$home_goals, hs$cs_pct,
            away_name, preds$away_goals, as$cs_pct,
            preds$home_goals + preds$away_goals
          )
        )
      )
    })
    
    # =========================================================================
    # REACTIVE: Simulation matrices (bivariate Poisson)
    # =========================================================================
    simulation_results <- reactive({
      preds <- tryCatch(team_predictions(), error = function(e) NULL)
      if (is.null(preds)) return(NULL)
      
      view <- rv$confirmed_view
      if (is.null(view)) return(NULL)
      
      # Use confirmed draw probability (normalized)
      total_pct <- view$home_win + view$draw + view$away_win
      if (total_pct == 0) total_pct <- 100
      draw_pct_normalized <- (view$draw / total_pct) * 100
      
      # Score matrix (goals)
      score <- generate_score_matrix(
        mu_home   = preds$home_goals,
        mu_away   = preds$away_goals,
        draw_pct  = draw_pct_normalized,
        max_goals = 4
      )
      
      # Shots matrix (binned ranges)
      shots <- generate_shots_matrix(
        shots_home = preds$home_stats$shots,
        shots_away = preds$away_stats$shots,
        goal_rho   = score$rho
      )
      
      # Shots on target matrix
      sot <- generate_sot_matrix(
        sot_home = preds$home_stats$sot,
        sot_away = preds$away_stats$sot,
        goal_rho = score$rho,
        max_sot  = 4
      )
      
      list(score = score, shots = shots, sot = sot)
    })
    
    # =========================================================================
    # RENDER: Calibration info bar
    # =========================================================================
    output$sim_calibration_info <- renderUI({
      sim <- tryCatch(simulation_results(), error = function(e) NULL)
      if (is.null(sim)) return(NULL)
      
      div(
        style = "margin-top: 25px; padding: 0.5rem 0.75rem; background: var(--bg-secondary); border-radius: 6px; font-size: 0.82rem;",
        sprintf(
          "\u03BB_home = %.2f  |  \u03BB_away = %.2f  |  \u03C1 = %.4f  |  Model: Bivariate Poisson (Holgate 1964)",
          sim$score$mu_home, sim$score$mu_away, sim$score$rho
        )
      )
    })
    
    # =========================================================================
    # RENDER: Simulated result probabilities bar (score matrix only)
    # =========================================================================
    output$sim_result_bar <- renderUI({
      sim <- tryCatch(simulation_results(), error = function(e) NULL)
      if (is.null(sim)) return(NULL)
      
      # Only show result bar when score matrix is selected
      selected_type <- input$sim_matrix_type %||% "score"
      if (selected_type != "score") return(NULL)
      
      info <- rv$match_info
      if (is.null(info)) return(NULL)
      
      home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
                          TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
      away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
                          TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
      
      rp <- sim$score$result_probs
      
      div(
        style = "margin-bottom: 0.5rem;",
        div(
          style = "display: flex; height: 36px; border-radius: 10px; overflow: hidden; border: 3px solid var(--outline);",
          div(style = sprintf(
            "width: %.1f%%; background: %s; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.82rem; font-weight: 700;",
            rp$home_win, APP_COLORS$sage),
            sprintf("%s %.1f%%", home_name, rp$home_win)),
          div(style = sprintf(
            "width: %.1f%%; background: #78909C; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.82rem; font-weight: 700;",
            rp$draw),
            sprintf("Draw %.1f%%", rp$draw)),
          div(style = sprintf(
            "width: %.1f%%; background: %s; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.82rem; font-weight: 700;",
            rp$away_win, APP_COLORS$coral),
            sprintf("%s %.1f%%", away_name, rp$away_win))
        ),
        div(
          style = "text-align: center; font-size: 0.75rem; color: var(--text-muted); margin-top: 0.25rem;",
          "Simulated result probabilities (from bivariate Poisson)"
        )
      )
    })
    
    # =========================================================================
    # HELPER: Render HTML heatmap grid
    # =========================================================================
    render_html_matrix <- function(sim_data, home_label, away_label,
                                   value_label = "", pct_fmt = "%.1f%%") {
      mat <- sim_data$matrix
      row_labels <- sim_data$row_labels
      col_labels <- sim_data$col_labels
      nr <- length(row_labels)
      nc <- length(col_labels)
      max_prob <- max(mat)
      
      # Color scale: white -> deep red (matching CannonStats style)
      cell_color <- function(prob) {
        if (is.na(prob) || prob <= 0) return("background: #FAFAFA; color: #CCC;")
        intensity <- min(1, prob / max(max_prob, 0.01))
        r <- round(255 - intensity * 75)
        g <- round(255 - intensity * 215)
        b <- round(255 - intensity * 215)
        text_color <- if (intensity > 0.4) "white" else "#333"
        sprintf("background: rgb(%d,%d,%d); color: %s;", r, g, b, text_color)
      }
      
      # Header row (away team)
      header_cells <- paste0(
        '<th style="padding: 0.4rem 0.6rem; text-align: center; font-weight: 700; font-size: 0.78rem; border-bottom: 2px solid var(--outline);">',
        col_labels, '</th>', collapse = "\n"
      )
      header_row <- paste0(
        '<tr><th style="padding: 0.4rem; border-bottom: 2px solid var(--outline); border-right: 2px solid var(--outline);"></th>\n',
        header_cells, '</tr>'
      )
      
      # Data rows (home team)
      data_rows <- sapply(1:nr, function(i) {
        row_cells <- sapply(1:nc, function(j) {
          prob <- mat[i, j]
          style <- cell_color(prob)
          sprintf(
            '<td style="%s padding: 0.5rem 0.4rem; text-align: center; font-size: 0.82rem; font-weight: 500; min-width: 55px; border: 1px solid rgba(0,0,0,0.05);">%s</td>',
            style, sprintf(pct_fmt, prob * 100)
          )
        })
        paste0(
          sprintf('<tr><td style="padding: 0.4rem 0.6rem; text-align: center; font-weight: 700; font-size: 0.78rem; border-right: 2px solid var(--outline);">%s</td>\n', row_labels[i]),
          paste(row_cells, collapse = "\n"),
          '</tr>'
        )
      })
      
      # Away label (top)
      away_label_text <- if (value_label != "") paste(away_label, value_label) else away_label
      home_label_text <- if (value_label != "") paste(home_label, value_label) else home_label
      
      away_header <- sprintf(
        '<div style="text-align: center; font-weight: 700; font-size: 0.85rem; margin-bottom: 0.25rem; color: %s;">%s \u2192</div>',
        APP_COLORS$coral, away_label_text
      )
      
      table_html <- sprintf(
        '%s<div style="display: flex; align-items: flex-start; gap: 0.25rem;"><div style="writing-mode: vertical-rl; transform: rotate(180deg); font-weight: 700; font-size: 0.85rem; text-align: center; color: %s; padding-right: 0.25rem;">\u2190 %s</div><table style="border-collapse: collapse; border: 3px solid var(--outline); border-radius: 10px; overflow: hidden;">%s\n%s</table></div>',
        away_header,
        APP_COLORS$sage, home_label_text,
        header_row, paste(data_rows, collapse = "\n")
      )
      
      HTML(table_html)
    }
    
    # =========================================================================
    # RENDER: Selected matrix (HTML) - driven by dropdown
    # =========================================================================
    output$sim_matrix_html <- renderUI({
      sim <- tryCatch(simulation_results(), error = function(e) NULL)
      if (is.null(sim)) {
        return(div(
          style = "text-align: center; color: var(--text-muted); padding: 2rem;",
          "Load a match and adjust your view to generate simulation"
        ))
      }
      
      info <- rv$match_info
      if (is.null(info)) return(NULL)
      
      home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
                          TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
      away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
                          TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
      
      selected_type <- input$sim_matrix_type %||% "score"
      
      if (selected_type == "score") {
        render_html_matrix(sim$score, home_name, away_name, "Goals")
      } else if (selected_type == "shots") {
        render_html_matrix(sim$shots, home_name, away_name, "Shots")
      } else {
        render_html_matrix(sim$sot, home_name, away_name, "SoT")
      }
    })
    
    # =========================================================================
    # RENDER: Lineup displays (home and away)
    # =========================================================================
    render_lineup_panel <- function(team_abbrev, team_label) {
      df <- effective_lineup()
      if (is.null(df)) return(div("No data loaded"))
      
      team_players <- df[df$club_abbrev == team_abbrev, ]
      team_players <- team_players[order(
        match(team_players$position, c("GK", "DEF", "MID", "FWD")),
        -team_players$price
      ), ]
      
      lineup_badge <- function(status) {
        color <- switch(status,
                        "expected"   = APP_COLORS$sage,
                        "possible"   = "#E8A838",
                        "unexpected" = "#B0BEC5",
                        "injured"    = APP_COLORS$coral,
                        "suspended"  = "#D08770",
                        "#B0BEC5"
        )
        span(
          style = sprintf(
            "display: inline-block; padding: 2px 8px; border-radius: 4px; background: %s; color: white; font-size: 0.72rem; font-weight: 600; cursor: pointer;",
            color
          ),
          toupper(substr(status, 1, 3))
        )
      }
      
      player_rows <- lapply(seq_len(nrow(team_players)), function(i) {
        p <- team_players[i, ]
        div(
          style = "display: flex; align-items: center; gap: 0.5rem; padding: 0.25rem 0; border-bottom: 1px solid #F5F5F5;",
          # Lineup status badge (clickable to override)
          tags$a(
            href = "#", onclick = sprintf(
              "Shiny.setInputValue('%s', {id: %d, time: Date.now()}, {priority: 'event'});return false;",
              ns("toggle_lineup"), p$player_id
            ),
            lineup_badge(p$lineup)
          ),
          # Position
          span(style = "width: 30px; font-size: 0.78rem; color: var(--text-muted); font-weight: 500;",
               p$position),
          # Name
          span(style = "flex: 1; font-weight: 500; font-size: 0.85rem;",
               p$display_name),
          # Price
          span(style = "font-size: 0.82rem; color: var(--text-muted);",
               sprintf("%.1f", p$price))
        )
      })
      
      starters <- sum(team_players$lineup %in% c("expected", "possible"))
      
      div(
        tags$h4(sprintf("%s (%d starters)", team_label, starters),
                style = "font-weight: 600; margin-bottom: 0.5rem;"),
        do.call(tagList, player_rows)
      )
    }
    
    output$home_lineup_display <- renderUI({
      req(rv$match_info, rv$merged_players)
      info <- rv$match_info
      home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
                          TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
      render_lineup_panel(info$home_abbrev, paste(home_name, "(H)"))
    })
    
    output$away_lineup_display <- renderUI({
      req(rv$match_info, rv$merged_players)
      info <- rv$match_info
      away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
                          TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
      render_lineup_panel(info$away_abbrev, paste(away_name, "(A)"))
    })
    
    # =========================================================================
    # LINEUP TOGGLE HANDLER
    # =========================================================================
    observeEvent(input$toggle_lineup, {
      pid <- as.character(input$toggle_lineup$id)
      df <- rv$merged_players
      idx <- which(df$player_id == as.integer(pid))
      if (length(idx) == 0) return()
      
      current <- if (!is.null(rv$lineup_overrides[[pid]])) {
        rv$lineup_overrides[[pid]]
      } else {
        df$lineup[idx]
      }
      
      # Cycle: expected -> possible -> unexpected -> expected
      new_status <- switch(current,
                           "expected"   = "unexpected",
                           "possible"   = "expected",
                           "unexpected" = "expected",
                           "injured"    = "expected",
                           "suspended"  = "expected",
                           "expected"
      )
      
      rv$lineup_overrides[[pid]] <- new_status
      log_debug(sprintf("Toggled %s lineup: %s -> %s",
                        df$display_name[idx], current, new_status), level = "INFO")
    })
    
    # =========================================================================
    # RENDER: Projections table
    # =========================================================================
    output$projections_table <- renderReactable({
      df <- tryCatch(player_projections(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      # Apply filters
      if (!is.null(input$proj_filter_team) && input$proj_filter_team != "all") {
        df <- df[df$club_abbrev == input$proj_filter_team, ]
      }
      if (!is.null(input$proj_filter_pos) && input$proj_filter_pos != "all") {
        df <- df[df$position == input$proj_filter_pos, ]
      }
      if (!is.null(input$proj_filter_lineup) && input$proj_filter_lineup == "starters") {
        df <- df[df$lineup %in% c("expected", "possible"), ]
      }
      
      if (nrow(df) == 0) return(NULL)
      
      # Sort by projected points (descending)
      df <- df[order(-df$projected_pts, na.last = TRUE), ]
      
      # Select display columns
      display_df <- data.frame(
        Player    = df$display_name,
        Club      = df$club_abbrev,
        Pos       = df$position,
        Price     = df$price,
        Status    = df$lineup,
        Proj      = df$projected_pts,
        CPT_Pts   = df$cpt_pts,
        Value     = df$value,
        Avg_Pts   = df$avg_pts,
        Mins_PG   = df$mins_per_game,
        Goals_PG  = df$goals_per_game,
        Assists_PG = df$assists_per_game,
        Shots_PG  = df$shots_per_game,
        SoT_PG    = df$sot_per_game,
        CS        = df$cs_total,
        YC_PG     = df$yellows_per_game,
        player_id = df$player_id,
        stringsAsFactors = FALSE
      )
      
      reactable(
        display_df,
        theme = app_reactable_theme(),
        defaultPageSize = 25,
        compact = TRUE,
        selection = "single",
        onClick = "select",
        columns = list(
          Player = colDef(name = "Player", minWidth = 140,
                          style = list(fontWeight = 500)),
          Club = colDef(name = "Club", width = 55, align = "center"),
          Pos = colDef(name = "Pos", width = 45, align = "center",
                       style = function(value) {
                         bg <- switch(value,
                                      "GK" = "#E8D44D", "DEF" = "#5B9BD5",
                                      "MID" = "#70AD47", "FWD" = "#FF6B6B", "#999")
                         list(color = "white", background = bg, fontWeight = 600,
                              borderRadius = "4px", textAlign = "center")
                       }),
          Price = colDef(name = "Price", width = 55, format = colFormat(digits = 1)),
          Status = colDef(name = "Status", width = 65, align = "center",
                          cell = function(value) {
                            color <- switch(value,
                                            "expected" = APP_COLORS$sage, "possible" = "#E8A838",
                                            "unexpected" = "#B0BEC5", "injured" = APP_COLORS$coral, "#999")
                            span(style = sprintf(
                              "padding: 2px 6px; border-radius: 3px; background: %s; color: white; font-size: 0.72rem; font-weight: 600;",
                              color), toupper(substr(value, 1, 3)))
                          }),
          Proj = colDef(name = "Proj", width = 55, align = "center",
                        style = function(value) {
                          if (is.na(value)) return(list(color = "#CCC"))
                          list(fontWeight = 700, color = if (value >= 5) APP_COLORS$sage else "#3B3226")
                        }),
          CPT_Pts = colDef(name = "CPT", width = 55, align = "center",
                           style = list(fontWeight = 600, color = "#8B6914")),
          Value = colDef(name = "Val", width = 50, align = "center",
                         style = function(value) {
                           if (is.na(value)) return(list(color = "#CCC"))
                           list(fontWeight = 500, color = if (value >= 0.7) APP_COLORS$sage else "#3B3226")
                         }),
          Avg_Pts = colDef(name = "PPG", width = 50, format = colFormat(digits = 1)),
          Mins_PG = colDef(name = "Min/G", width = 50),
          Goals_PG = colDef(name = "G/G", width = 45, format = colFormat(digits = 2)),
          Assists_PG = colDef(name = "A/G", width = 45, format = colFormat(digits = 2)),
          Shots_PG = colDef(name = "Sh/G", width = 50, format = colFormat(digits = 1)),
          SoT_PG = colDef(name = "SoT/G", width = 50, format = colFormat(digits = 1)),
          CS = colDef(name = "CS", width = 40),
          YC_PG = colDef(name = "YC/G", width = 50, format = colFormat(digits = 2)),
          player_id = colDef(show = FALSE)
        )
      )
    })
    
    # =========================================================================
    # LINEUP GENERATION
    # =========================================================================
    observeEvent(input$generate_lineups, {
      df <- tryCatch(player_projections(), error = function(e) NULL)
      req(df)
      
      # Filter to starters with projections
      eligible <- df[df$lineup %in% c("expected", "possible") & !is.na(df$projected_pts), ]
      req(nrow(eligible) >= SHOWDOWN_RULES$roster_size)
      
      # Get excluded players
      excluded <- as.integer(input$excluded_players)
      
      # Get locked captain
      locked_cpt <- if (!is.null(input$cpt_lock) && input$cpt_lock != "") {
        as.integer(input$cpt_lock)
      } else NULL
      
      withProgress(message = "Generating lineups...", value = 0.5, {
        rv$generated_lineups <- generate_showdown_lineups(
          players      = eligible,
          n_lineups    = input$n_lineups %||% 5,
          variance_pct = input$variance_pct %||% 15,
          salary_cap   = input$salary_cap %||% SHOWDOWN_RULES$salary_cap,
          roster_size  = SHOWDOWN_RULES$roster_size,
          cpt_multiplier = SHOWDOWN_RULES$cpt_multiplier,
          locked_cpt   = locked_cpt,
          excluded_ids = excluded
        )
        setProgress(1.0, message = "Done!")
      })
    })
    
    # =========================================================================
    # RENDER: Generated lineups
    # =========================================================================
    output$lineups_display <- renderUI({
      lineups <- rv$generated_lineups
      if (is.null(lineups) || length(lineups) == 0) {
        return(div(
          style = "padding: 2rem; text-align: center; color: var(--text-muted);",
          "Click 'Generate Lineups' to create optimized lineups"
        ))
      }
      
      # Summary stats
      all_pts <- sapply(lineups, function(l) l$total_pts)
      
      summary_div <- div(
        style = "display: flex; gap: 1rem; margin-bottom: 1rem;",
        div(style = "padding: 0.5rem 1rem; background: var(--bg-secondary); border-radius: 6px; text-align: center;",
            div(style = "font-size: 0.75rem; color: var(--text-muted);", "Best"),
            div(style = "font-weight: 700; font-size: 1.1rem;", sprintf("%.1f", max(all_pts)))
        ),
        div(style = "padding: 0.5rem 1rem; background: var(--bg-secondary); border-radius: 6px; text-align: center;",
            div(style = "font-size: 0.75rem; color: var(--text-muted);", "Average"),
            div(style = "font-weight: 700; font-size: 1.1rem;", sprintf("%.1f", mean(all_pts)))
        ),
        div(style = "padding: 0.5rem 1rem; background: var(--bg-secondary); border-radius: 6px; text-align: center;",
            div(style = "font-size: 0.75rem; color: var(--text-muted);", "Lineups"),
            div(style = "font-weight: 700; font-size: 1.1rem;", length(lineups))
        )
      )
      
      # Lineup cards
      lineup_cards <- lapply(seq_along(lineups), function(idx) {
        lu <- lineups[[idx]]
        lineup_df <- lu$lineup
        
        # Check for CS stacking penalty
        cs_warning <- check_cs_stack_warning(lineup_df)
        
        player_rows <- lapply(seq_len(nrow(lineup_df)), function(j) {
          p <- lineup_df[j, ]
          is_cpt <- p$role == "CPT"
          
          div(
            style = sprintf(
              "display: flex; align-items: center; gap: 0.4rem; padding: 0.3rem 0.5rem; %s",
              if (is_cpt) "background: #FFF8E1; border-left: 3px solid #E8A838;" else ""
            ),
            # Role badge
            if (is_cpt) {
              span(style = "padding: 1px 5px; background: #E8A838; color: white; border-radius: 3px; font-size: 0.7rem; font-weight: 700;", "CPT")
            } else {
              span(style = "width: 32px;")
            },
            # Position
            span(style = sprintf("width: 30px; font-size: 0.75rem; color: %s; font-weight: 600;",
                                 switch(p$position, "GK"="#E8D44D","DEF"="#5B9BD5","MID"="#70AD47","FWD"="#FF6B6B","#999")),
                 p$position),
            # Name
            span(style = "flex: 1; font-size: 0.82rem; font-weight: 500;", p$display_name),
            # Club
            span(style = "font-size: 0.75rem; color: var(--text-muted);", p$club_abbrev),
            # Price
            span(style = "width: 35px; text-align: right; font-size: 0.82rem;",
                 sprintf("%.1f", p$price)),
            # Effective pts
            span(style = "width: 40px; text-align: right; font-weight: 700; font-size: 0.85rem;",
                 sprintf("%.1f", p$effective_pts))
          )
        })
        
        div(
          style = "border: 1px solid #E5E9F0; border-radius: 8px; overflow: hidden; margin-bottom: 0.75rem;",
          # Header
          div(
            style = sprintf("padding: 0.5rem 0.75rem; background: %s; color: white; display: flex; justify-content: space-between;",
                            APP_COLORS$sage),
            span(style = "font-weight: 600;", sprintf("Lineup #%d", idx)),
            span(sprintf("%.1f pts | %.1fM (%.1f remaining)",
                         lu$total_pts, lu$total_salary, lu$remaining))
          ),
          # CS warning
          if (!is.null(cs_warning)) {
            div(style = "padding: 0.3rem 0.75rem; background: #FFF3CD; font-size: 0.78rem; color: #856404;",
                tags$i(class = "fas fa-exclamation-triangle"), " ", cs_warning)
          },
          # Players
          div(style = "padding: 0.25rem 0;", do.call(tagList, player_rows))
        )
      })
      
      tagList(summary_div, do.call(tagList, lineup_cards))
    })
    
    # =========================================================================
    # HELPER: Check CS stacking penalty warning
    # =========================================================================
    check_cs_stack_warning <- function(lineup_df) {
      # Check for 2+ GK/DEF from same club
      def_gk <- lineup_df[lineup_df$position %in% c("GK", "DEF"), ]
      if (nrow(def_gk) < 2) return(NULL)
      
      club_counts <- table(def_gk$club_abbrev)
      stacked_clubs <- names(club_counts[club_counts >= 2])
      
      if (length(stacked_clubs) == 0) return(NULL)
      
      warnings <- sapply(stacked_clubs, function(club) {
        n <- as.integer(club_counts[club])
        penalty <- sum(SHOWDOWN_CS_STACK_PENALTY[2:n])
        club_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[club]),
                            TEAM_DISPLAY_NAMES[club], club)
        sprintf("CS Stack: %d GK/DEF from %s (-%dp on clean sheet)", n, club_name, abs(penalty))
      })
      
      paste(warnings, collapse = " | ")
    }
    
    # =========================================================================
    # HANDBUILD FUNCTIONALITY
    # =========================================================================
    
    # Add player from table selection
    observeEvent(input$add_to_handbuild, {
      selected <- getReactableState("projections_table", "selected")
      req(selected)
      
      df <- tryCatch(player_projections(), error = function(e) NULL)
      req(df)
      
      # Get filtered df in same order as displayed
      filtered_df <- df
      if (!is.null(input$proj_filter_team) && input$proj_filter_team != "all") {
        filtered_df <- filtered_df[filtered_df$club_abbrev == input$proj_filter_team, ]
      }
      if (!is.null(input$proj_filter_pos) && input$proj_filter_pos != "all") {
        filtered_df <- filtered_df[filtered_df$position == input$proj_filter_pos, ]
      }
      if (!is.null(input$proj_filter_lineup) && input$proj_filter_lineup == "starters") {
        filtered_df <- filtered_df[filtered_df$lineup %in% c("expected", "possible"), ]
      }
      filtered_df <- filtered_df[order(-filtered_df$projected_pts, na.last = TRUE), ]
      
      if (selected > nrow(filtered_df)) return()
      
      player <- filtered_df[selected, ]
      pid <- as.character(player$player_id)
      
      # Check if already in lineup
      if (pid %in% names(rv$handbuild_slots)) {
        showNotification("Player already in lineup", type = "warning")
        return()
      }
      
      # Check roster size
      if (length(rv$handbuild_slots) >= SHOWDOWN_RULES$roster_size) {
        showNotification("Lineup is full (5 players)", type = "warning")
        return()
      }
      
      # Check salary
      current_salary <- sum(sapply(rv$handbuild_slots, function(s) s$price))
      if (current_salary + player$price > (input$salary_cap %||% SHOWDOWN_RULES$salary_cap)) {
        showNotification("Exceeds salary cap", type = "warning")
        return()
      }
      
      rv$handbuild_slots[[pid]] <- list(
        player_id    = player$player_id,
        display_name = player$display_name,
        position     = player$position,
        club_abbrev  = player$club_abbrev,
        price        = player$price,
        projected_pts = player$projected_pts
      )
      
      # First player added becomes captain by default
      if (is.null(rv$handbuild_cpt)) {
        rv$handbuild_cpt <- pid
      }
    })
    
    # Clear handbuild
    observeEvent(input$clear_handbuild, {
      rv$handbuild_slots <- list()
      rv$handbuild_cpt <- NULL
    })
    
    # Toggle captain
    observeEvent(input$toggle_cpt, {
      pid <- as.character(input$toggle_cpt$id)
      rv$handbuild_cpt <- pid
    })
    
    # Remove from handbuild
    observeEvent(input$remove_handbuild, {
      pid <- as.character(input$remove_handbuild$id)
      rv$handbuild_slots[[pid]] <- NULL
      if (identical(rv$handbuild_cpt, pid)) {
        # Reassign captain to first player
        if (length(rv$handbuild_slots) > 0) {
          rv$handbuild_cpt <- names(rv$handbuild_slots)[1]
        } else {
          rv$handbuild_cpt <- NULL
        }
      }
    })
    
    # Render handbuild lineup
    output$handbuild_lineup <- renderUI({
      slots <- rv$handbuild_slots
      cap <- input$salary_cap %||% SHOWDOWN_RULES$salary_cap
      
      if (length(slots) == 0) {
        return(div(
          style = "padding: 1.5rem; text-align: center; color: var(--text-muted); border: 2px dashed #E5E9F0; border-radius: 8px;",
          "Select a player in the projections table and click 'Add Selected Player'"
        ))
      }
      
      total_salary <- sum(sapply(slots, function(s) s$price))
      total_pts <- 0
      
      player_divs <- lapply(names(slots), function(pid) {
        s <- slots[[pid]]
        is_cpt <- identical(rv$handbuild_cpt, pid)
        eff_pts <- if (is_cpt) s$projected_pts * SHOWDOWN_RULES$cpt_multiplier else s$projected_pts
        total_pts <<- total_pts + eff_pts
        
        div(
          style = sprintf(
            "display: flex; align-items: center; gap: 0.5rem; padding: 0.4rem 0.75rem; border-bottom: 1px solid #F0F0F0; %s",
            if (is_cpt) "background: #FFF8E1;" else ""
          ),
          # CPT toggle
          tags$a(
            href = "#", onclick = sprintf(
              "Shiny.setInputValue('%s', {id: '%s', time: Date.now()}, {priority: 'event'});return false;",
              ns("toggle_cpt"), pid
            ),
            span(
              style = sprintf(
                "padding: 2px 6px; border-radius: 3px; font-size: 0.72rem; font-weight: 700; cursor: pointer; %s",
                if (is_cpt) "background: #E8A838; color: white;" else "background: #E5E9F0; color: #999;"
              ),
              if (is_cpt) "CPT" else "FLEX"
            )
          ),
          # Position
          span(style = "width: 35px; font-size: 0.78rem; font-weight: 600;", s$position),
          # Name
          span(style = "flex: 1; font-weight: 500;", s$display_name),
          # Club
          span(style = "font-size: 0.78rem; color: var(--text-muted);", s$club_abbrev),
          # Price
          span(style = "width: 40px; text-align: right;", sprintf("%.1f", s$price)),
          # Effective points
          span(style = "width: 45px; text-align: right; font-weight: 700;",
               sprintf("%.1f", eff_pts)),
          # Remove button
          tags$a(
            href = "#", onclick = sprintf(
              "Shiny.setInputValue('%s', {id: '%s', time: Date.now()}, {priority: 'event'});return false;",
              ns("remove_handbuild"), pid
            ),
            span(style = "color: #D08770; cursor: pointer; font-size: 0.9rem;", "x")
          )
        )
      })
      
      cs_warning <- NULL
      if (length(slots) > 0) {
        # Build a simple df for CS check
        hb_df <- data.frame(
          position = sapply(slots, function(s) s$position),
          club_abbrev = sapply(slots, function(s) s$club_abbrev),
          stringsAsFactors = FALSE
        )
        cs_warning <- check_cs_stack_warning(hb_df)
      }
      
      tagList(
        # Summary bar
        div(
          style = "display: flex; justify-content: space-between; padding: 0.5rem 0.75rem; background: var(--bg-secondary); border-radius: 6px; margin-bottom: 0.5rem;",
          span(sprintf("Players: %d / %d", length(slots), SHOWDOWN_RULES$roster_size)),
          span(sprintf("Salary: %.1f / %.1f (%.1f remaining)",
                       total_salary, cap, cap - total_salary)),
          span(style = "font-weight: 700;", sprintf("Total: %.1f pts", total_pts))
        ),
        # CS warning
        if (!is.null(cs_warning)) {
          div(style = "padding: 0.4rem 0.75rem; background: #FFF3CD; border-radius: 6px; font-size: 0.78rem; color: #856404; margin-bottom: 0.5rem;",
              cs_warning)
        },
        # Player rows
        do.call(tagList, player_divs),
        # Empty slots
        if (length(slots) < SHOWDOWN_RULES$roster_size) {
          empty_divs <- lapply(1:(SHOWDOWN_RULES$roster_size - length(slots)), function(i) {
            div(
              style = "padding: 0.4rem 0.75rem; border-bottom: 1px dashed #E5E9F0; color: #CCC; font-style: italic; font-size: 0.85rem;",
              "Empty slot"
            )
          })
          do.call(tagList, empty_divs)
        }
      )
    })
    
  })
}