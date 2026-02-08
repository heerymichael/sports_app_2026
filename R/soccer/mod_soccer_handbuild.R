# =============================================================================
# Module: Soccer Handbuild
# 
# Manual lineup building with player ratings and optimization support
# for FanTeam Monster contests.
#
# Features:
#   (a) Player pool combining historical stats + this week's matchup + ratings
#   (b) Cash / GPP / Captain word grades from soccer_ratings.R
#   (c) Ownership estimation (heuristic, calibrated against GW20+ data)
#   (d) Lineup builder with budget tracking
#   (e) LP optimizer (maximize projected points within salary cap + position rules)
#   (f) Rating guide with methodology explanation
#
# Data Pipeline:
#   1. FanTeam salaries   -> soccer_fanteam_loader.R        (col: position)
#   2. Odds report        -> load_fanteam_odds()            (from mod_soccer_matchups.R)
#   3. Player stats       -> load_fanteam_stats_overview()  (col: pos)
#   4. Name matching      -> soccer_fanteam_matching.R
#   5. Column unify       -> pos = coalesce(pos_from_stats, position_from_salary)
#   6. Matchup quality    -> calculate_matchup_quality()    (from soccer_ratings.R)
#   7. Ownership estimate -> estimate_ownership()           (from soccer_ratings.R)
#   8. Player ratings     -> rate_players()                 (from soccer_ratings.R)
#
# Column convention: salary loader uses `position`, stats/ratings use `pos`.
# We unify to `pos` early in the pipeline and keep it throughout.
#
# Depends on: soccer_ratings.R, soccer_fanteam_loader.R, soccer_fanteam_matching.R,
#             mod_soccer_matchups.R (for load_fanteam_odds), mod_soccer_player_stats.R
# =============================================================================

library(lpSolve)

# =============================================================================
# UI FUNCTION
# =============================================================================

#' Soccer Handbuild UI
#' @param id Module namespace ID
soccer_handbuild_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_handbuild_ui() called with id:", id, level = "INFO")
  
  # Get available gameweeks at UI build time
  gameweeks <- tryCatch(get_fanteam_soccer_gameweeks(), error = function(e) c())
  gw_choices <- if (length(gameweeks) > 0) {
    setNames(as.character(gameweeks), paste("GW", gameweeks))
  } else {
    c("No data" = "")
  }
  
  tagList(
    # Enable shinyjs for dynamic UI
    shinyjs::useShinyjs(),
    
    # Page header
    div(
      class = "page-header",
      tags$h2("Soccer Handbuild"),
      tags$p(class = "text-muted", "Build lineups manually with ratings and optimization assistance")
    ),
    
    # ==========================================================================
    # SETTINGS CARD
    # ==========================================================================
    ui_card(
      title = "Settings",
      color = "sage",
      
      fluidRow(
        column(3,
               selectInput(
                 ns("gameweek"), "Gameweek",
                 choices = gw_choices,
                 selected = if (length(gameweeks) > 0) as.character(gameweeks[1]) else NULL
               )
        ),
        column(3,
               selectInput(
                 ns("contest_type"), "Contest Type",
                 choices = c("FanTeam Classic" = "classic", "FanTeam Showdown" = "showdown"),
                 selected = "classic"
               )
        ),
        column(3,
               numericInput(
                 ns("salary_cap"), "Salary Cap (M)",
                 value = 100, min = 50, max = 150, step = 0.5
               )
        ),
        column(3,
               div(
                 style = "padding-top: 25px; display: flex; gap: 0.5rem;",
                 actionButton(
                   ns("load_data"), "Load Data",
                   icon = icon("download"),
                   class = "btn-primary",
                   style = "flex: 1;"
                 ),
                 actionButton(
                   ns("refresh_data"), "Refresh",
                   icon = icon("refresh"),
                   class = "btn-secondary",
                   style = "flex: 1;"
                 )
               )
        )
      ),
      
      # Status / error messages
      uiOutput(ns("load_status"))
    ),
    
    tags$br(),
    
    # ==========================================================================
    # RATING GUIDE (collapsible)
    # ==========================================================================
    ui_card(
      title = "Rating Guide",
      color = "sage",
      collapsed = TRUE,
      uiOutput(ns("rating_guide"))
    ),
    
    tags$br(),
    
    # ==========================================================================
    # PLAYER POOL + LINEUP BUILDER
    # ==========================================================================
    fluidRow(
      # Left: Player Pool (8 cols)
      column(
        8,
        ui_card(
          title = "Player Pool",
          color = "sage",
          
          # Filters row
          fluidRow(
            column(3,
                   div(
                     style = "display: flex; gap: 0.4rem; flex-wrap: wrap; margin-bottom: 0.5rem;",
                     actionButton(ns("filter_all"), "ALL", class = "btn-position-filter active",
                                  style = "font-size: 0.75rem; padding: 0.25rem 0.6rem;"),
                     actionButton(ns("filter_gk"), "GK", class = "btn-position-filter",
                                  style = "font-size: 0.75rem; padding: 0.25rem 0.6rem;"),
                     actionButton(ns("filter_def"), "DEF", class = "btn-position-filter",
                                  style = "font-size: 0.75rem; padding: 0.25rem 0.6rem;"),
                     actionButton(ns("filter_mid"), "MID", class = "btn-position-filter",
                                  style = "font-size: 0.75rem; padding: 0.25rem 0.6rem;"),
                     actionButton(ns("filter_fwd"), "FWD", class = "btn-position-filter",
                                  style = "font-size: 0.75rem; padding: 0.25rem 0.6rem;")
                   )
            ),
            column(3,
                   selectizeInput(
                     ns("filter_team"), label = NULL,
                     choices = c("All Teams" = "all"),
                     selected = "all",
                     options = list(placeholder = "All Teams")
                   )
            ),
            column(3,
                   selectInput(
                     ns("sort_by"), label = NULL,
                     choices = c("Salary" = "salary", "PPG" = "ppg",
                                 "Cash Score" = "cash_score", "GPP Score" = "gpp_score",
                                 "Captain Score" = "captain_score",
                                 "Est Own%" = "est_own_pct", "Value" = "value"),
                     selected = "cash_score"
                   )
            ),
            column(3,
                   div(
                     style = "display: flex; gap: 0.5rem; align-items: center;",
                     tags$span(
                       style = "font-size: 0.8rem; color: var(--text-muted);",
                       uiOutput(ns("pool_count"), inline = TRUE)
                     )
                   )
            )
          ),
          
          # Player table
          div(
            style = "max-height: 600px; overflow-y: auto;",
            reactableOutput(ns("player_pool_table"))
          )
        )
      ),
      
      # Right: Lineup Builder (4 cols)
      column(
        4,
        ui_card(
          title = "Lineup Builder",
          color = "sage",
          
          # Budget summary
          div(
            style = "margin-bottom: 0.75rem; padding: 0.5rem; background: var(--bg-tertiary); border-radius: 6px; border: 2px solid var(--outline);",
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 0.3rem;",
              div(
                style = "text-align: center;",
                div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted); letter-spacing: 0.5px;", "Salary"),
                div(style = "font-weight: 700; font-size: 0.9rem;",
                    textOutput(ns("salary_used"), inline = TRUE))
              ),
              div(
                style = "text-align: center;",
                div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted); letter-spacing: 0.5px;", "Remaining"),
                div(style = "font-weight: 700; font-size: 0.9rem;",
                    uiOutput(ns("salary_remaining"), inline = TRUE))
              ),
              div(
                style = "text-align: center;",
                div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted); letter-spacing: 0.5px;", "Slots"),
                div(style = "font-weight: 700; font-size: 0.9rem;",
                    textOutput(ns("slots_filled"), inline = TRUE))
              )
            )
          ),
          
          # Lineup slots
          uiOutput(ns("lineup_slots")),
          
          # Action buttons
          div(
            style = "display: flex; gap: 0.5rem; margin-top: 0.75rem;",
            actionButton(ns("optimize_lineup"), "Optimize",
                         icon = icon("magic"),
                         class = "btn-primary", style = "flex: 1; font-size: 0.85rem;"),
            actionButton(ns("clear_lineup"), "Clear",
                         icon = icon("trash"),
                         class = "btn-secondary", style = "flex: 1; font-size: 0.85rem;")
          ),
          
          # Projected total
          div(
            style = "margin-top: 0.75rem; padding: 0.5rem; background: var(--accent-sage); color: white; border-radius: 6px; text-align: center;",
            tags$span("Projected PPG", style = "font-size: 0.65rem; opacity: 0.9;"),
            tags$div(
              style = "font-size: 1.4rem; font-weight: 700;",
              textOutput(ns("projected_ppg"), inline = TRUE)
            )
          )
        )
      )
    )
  )
}


# =============================================================================
# SERVER FUNCTION
# =============================================================================

#' Soccer Handbuild Server
#' @param id Module namespace ID
soccer_handbuild_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_handbuild_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # CONSTANTS
    # =========================================================================
    LINEUP_STRUCTURE <- list(
      classic = list(
        slots = c("GK", "DEF", "DEF", "DEF", "DEF", "MID", "MID", "MID", "FWD", "FWD", "FWD"),
        labels = c("GK", "DEF", "DEF", "DEF", "DEF", "MID", "MID", "MID", "FWD", "FWD", "FWD"),
        total = 11
      ),
      showdown = list(
        slots = c("CPT", "FLEX", "FLEX", "FLEX", "FLEX", "FLEX"),
        labels = c("CPT", "FLEX", "FLEX", "FLEX", "FLEX", "FLEX"),
        total = 6
      )
    )
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    rv <- reactiveValues(
      player_data      = NULL,   # Combined, rated player pool (uses `pos` column)
      salaries         = NULL,   # Raw FanTeam salary data
      odds             = NULL,   # Odds report data
      stats_overview   = NULL,   # Historical player stats (overview)
      stats_detail     = NULL,   # Historical per-gameweek detail
      ownership_hist   = NULL,   # Historical ownership for calibration
      matchup_context  = NULL,   # Per-team matchup info
      lineup           = list(), # Named list: slot_index -> player row
      position_filter  = "all",  # Current position filter
      loading          = FALSE,
      load_error       = NULL,
      data_loaded      = FALSE
    )
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    observeEvent(input$load_data, {
      gw <- as.integer(input$gameweek)
      req(gw)
      
      rv$loading <- TRUE
      rv$load_error <- NULL
      rv$data_loaded <- FALSE
      
      log_debug("========================================", level = "INFO")
      log_debug("Loading handbuild data for GW", gw, level = "INFO")
      
      tryCatch({
        # --- 1. Load FanTeam salaries ---
        salaries <- load_fanteam_soccer_with_logos(gw)
        if (is.null(salaries) || nrow(salaries) == 0) {
          rv$load_error <- "No salary data found for this gameweek."
          rv$loading <- FALSE
          return()
        }
        rv$salaries <- salaries
        log_debug("  Salaries:", nrow(salaries), "players", level = "INFO")
        
        # --- 2. Load odds report ---
        odds <- tryCatch(load_fanteam_odds(gw), error = function(e) {
          log_debug("  Odds load error:", e$message, level = "WARN")
          NULL
        })
        rv$odds <- odds
        log_debug("  Odds:", if (!is.null(odds)) nrow(odds) else 0, "rows", level = "INFO")
        
        # --- 3. Load player stats (historical) ---
        stats_ov <- tryCatch(load_fanteam_stats_overview(force_refresh = FALSE), 
                             error = function(e) NULL)
        stats_det <- tryCatch(load_fanteam_stats_detail(force_refresh = FALSE),
                              error = function(e) NULL)
        rv$stats_overview <- stats_ov
        rv$stats_detail <- stats_det
        log_debug("  Stats overview:", if (!is.null(stats_ov)) nrow(stats_ov) else 0, "rows", level = "INFO")
        log_debug("  Stats detail:", if (!is.null(stats_det)) nrow(stats_det) else 0, "rows", level = "INFO")
        
        # --- 4. Load ownership history (for calibration) ---
        own_hist <- tryCatch(load_ownership_history(), error = function(e) {
          log_debug("  Ownership history error:", e$message, level = "WARN")
          NULL
        })
        rv$ownership_hist <- own_hist
        log_debug("  Ownership history:", if (!is.null(own_hist)) nrow(own_hist) else 0, "rows", level = "INFO")
        
        # --- 5. Build matchup context from odds ---
        matchup_ctx <- build_matchup_context(odds, salaries)
        rv$matchup_context <- matchup_ctx
        
        # --- 6. Combine everything into rated player pool ---
        rv$player_data <- build_rated_player_pool(salaries, stats_ov, stats_det,
                                                  odds, matchup_ctx, own_hist)
        
        rv$data_loaded <- TRUE
        
        # Update team filter choices
        if (!is.null(rv$player_data) && nrow(rv$player_data) > 0) {
          teams <- sort(unique(rv$player_data$team_normalized[!is.na(rv$player_data$team_normalized)]))
          updateSelectizeInput(session, "filter_team",
                               choices = c("All Teams" = "all", setNames(teams, teams)),
                               selected = "all")
        }
        
        log_debug("Handbuild data loaded successfully:", nrow(rv$player_data), "rated players", level = "INFO")
        log_debug("========================================", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error loading handbuild data:", e$message, level = "ERROR")
        rv$load_error <- e$message
      })
      
      rv$loading <- FALSE
    })
    
    # Force refresh (clear cache, then reload)
    observeEvent(input$refresh_data, {
      tryCatch({
        cache_files <- list.files("data/cache", pattern = "fanteam_ft_", full.names = TRUE)
        if (length(cache_files) > 0) file.remove(cache_files)
        log_debug("Cleared stats cache files", level = "INFO")
      }, error = function(e) NULL)
      
      shinyjs::click("load_data")
    })
    
    # =========================================================================
    # DATA PIPELINE HELPERS
    # =========================================================================
    
    #' Build per-team matchup context from odds data
    build_matchup_context <- function(odds, salaries) {
      if (is.null(odds) || nrow(odds) == 0) return(NULL)
      
      tryCatch({
        slate_teams <- unique(salaries$team_normalized[!is.na(salaries$team_normalized)])
        
        ctx <- odds %>%
          filter(odds_team_normalized %in% slate_teams) %>%
          transmute(
            team_normalized = odds_team_normalized,
            opponent = odds_opponent_normalized,
            home_away = home_away,
            win_pct = as.numeric(win_pct),
            draw_pct = if ("draw_pct" %in% names(.)) as.numeric(draw_pct) else 30,
            implied_goals = as.numeric(implied_team_goals),
            implied_opp_goals = as.numeric(implied_opp_goals),
            implied_total = implied_goals + implied_opp_goals,
            # Poisson CS approximation: P(0 goals) = exp(-lambda)
            cs_prob = round(exp(-implied_opp_goals) * 100, 1)
          )
        
        log_debug("Built matchup context for", nrow(ctx), "teams", level = "INFO")
        return(ctx)
      }, error = function(e) {
        log_debug("Error building matchup context:", e$message, level = "WARN")
        NULL
      })
    }
    
    #' Combine salary, stats, odds, and ratings into a single rated pool
    #'
    #' COLUMN UNIFICATION:
    #'   Salary loader outputs `position` (GK/DEF/MID/FWD)
    #'   Stats overview outputs `pos` (GK/DEF/MID/FWD)
    #'   We unify to `pos` for all downstream functions (ratings engine uses `pos`)
    build_rated_player_pool <- function(salaries, stats_ov, stats_det,
                                        odds, matchup_ctx, own_hist) {
      log_debug("Building rated player pool...", level = "INFO")
      
      # Start with salaries; unify position column to `pos`
      pool <- salaries %>%
        mutate(pos = position)
      
      # --- Merge historical stats (PPG, floor, ceiling, Sortino) ---
      if (!is.null(stats_ov) && nrow(stats_ov) > 0) {
        stats_cols <- stats_ov %>%
          select(any_of(c("name", "team", "pos", "x1_mp", "total_pts", "avg_pts", "picked_by")))
        
        # Calculate floor, ceiling, Sortino from per-GW detail if available
        if (!is.null(stats_det) && nrow(stats_det) > 0) {
          pts_col <- if ("pts" %in% names(stats_det)) "pts" else 
            if ("total_pts" %in% names(stats_det)) "total_pts" else NULL
          
          if (!is.null(pts_col)) {
            stats_det[[pts_col]] <- as.numeric(stats_det[[pts_col]])
            
            player_perf <- stats_det %>%
              group_by(name) %>%
              summarise(
                n_games = n(),
                ppg_hist = mean(.data[[pts_col]], na.rm = TRUE),
                pts_floor = {
                  pts <- sort(.data[[pts_col]][!is.na(.data[[pts_col]])])
                  if (length(pts) >= 4) pts[2] else if (length(pts) > 0) pts[1] else NA_real_
                },
                pts_ceiling = {
                  pts <- sort(.data[[pts_col]][!is.na(.data[[pts_col]])], decreasing = TRUE)
                  if (length(pts) >= 4) pts[2] else if (length(pts) > 0) pts[1] else NA_real_
                },
                downside_dev = {
                  pts <- .data[[pts_col]][!is.na(.data[[pts_col]])]
                  avg <- mean(pts, na.rm = TRUE)
                  below <- pts[pts < avg]
                  if (length(below) > 1) sqrt(mean((below - avg)^2)) else NA_real_
                },
                .groups = "drop"
              ) %>%
              mutate(
                sortino_raw = if_else(!is.na(ppg_hist) & !is.na(downside_dev) & downside_dev > 0,
                                      ppg_hist / downside_dev, NA_real_)
              )
            
            # Bayesian shrinkage on Sortino (same approach as Player Stats module)
            k_prior <- 10
            pos_avgs <- player_perf %>%
              left_join(stats_cols %>% select(name, pos) %>% distinct(), by = "name") %>%
              filter(n_games >= 6, !is.na(sortino_raw)) %>%
              group_by(pos) %>%
              summarise(pos_avg_sort = mean(sortino_raw, na.rm = TRUE), .groups = "drop")
            
            player_perf <- player_perf %>%
              left_join(stats_cols %>% select(name, pos) %>% distinct(), by = "name") %>%
              left_join(pos_avgs, by = "pos") %>%
              mutate(
                pos_avg_sort = coalesce(pos_avg_sort, mean(sortino_raw, na.rm = TRUE)),
                sortino = if_else(!is.na(sortino_raw) & !is.na(n_games),
                                  (n_games * sortino_raw + k_prior * pos_avg_sort) / (n_games + k_prior),
                                  NA_real_)
              ) %>%
              select(name, n_games, ppg_hist, pts_floor, pts_ceiling, sortino)
            
            stats_cols <- stats_cols %>%
              left_join(player_perf, by = "name")
          }
        }
        
        # Join to pool via case-insensitive name matching
        pool <- pool %>%
          mutate(match_name = tolower(trimws(player))) %>%
          left_join(
            stats_cols %>% mutate(match_name = tolower(trimws(name))),
            by = "match_name",
            suffix = c("", "_stats")
          ) %>%
          select(-match_name) %>%
          mutate(
            # Prefer stats pos if available (more reliable), fall back to salary position
            pos = coalesce(pos_stats, pos),
            ppg = coalesce(as.numeric(ppg_hist), as.numeric(avg_pts), 0),
            pts_floor = coalesce(pts_floor, 0),
            pts_ceiling = coalesce(pts_ceiling, 0),
            sortino = coalesce(sortino, 0),
            n_games = coalesce(n_games, 0L),
            form = ppg  # Use PPG as form proxy
          ) %>%
          select(-any_of(c("pos_stats", "name_stats", "team_stats")))
        
        log_debug("  Merged stats for", sum(pool$n_games > 0), "/", nrow(pool), "players", level = "INFO")
      } else {
        pool <- pool %>%
          mutate(ppg = 0, pts_floor = 0, pts_ceiling = 0, sortino = 0,
                 n_games = 0L, form = 0, picked_by = NA_real_)
      }
      
      # --- Merge opponent info from odds ---
      if (!is.null(matchup_ctx) && nrow(matchup_ctx) > 0) {
        pool <- pool %>%
          left_join(
            matchup_ctx %>% select(team_normalized, opponent, home_away),
            by = "team_normalized"
          )
      } else {
        pool <- pool %>% mutate(opponent = NA_character_, home_away = NA_character_)
      }
      
      # --- Calculate matchup quality (position-aware, uses `pos` column) ---
      pool <- calculate_matchup_quality(pool, matchup_ctx)
      
      # --- Estimate ownership ---
      pool <- estimate_ownership(pool, historical = own_hist)
      
      # --- Rate players (Cash, GPP, Captain) ---
      pool <- rate_players(pool)
      
      # --- Add value column ---
      pool <- pool %>%
        mutate(value = round(ppg / pmax(salary, 3), 2))
      
      log_debug("Rated player pool complete:", nrow(pool), "players", level = "INFO")
      return(pool)
    }
    
    # =========================================================================
    # LOAD STATUS OUTPUT
    # =========================================================================
    
    output$load_status <- renderUI({
      if (rv$loading) {
        return(div(
          style = "margin-top: 0.5rem; padding: 0.5rem; background: #FFF3E0; border-radius: 4px;",
          icon("spinner", class = "fa-spin"),
          tags$span("Loading data...", style = "margin-left: 0.5rem; font-size: 0.85rem;")
        ))
      }
      
      if (!is.null(rv$load_error)) {
        return(div(
          style = "margin-top: 0.5rem; padding: 0.5rem; background: #FFEBEE; border-radius: 4px; color: #C62828;",
          icon("exclamation-triangle"),
          tags$span(rv$load_error, style = "margin-left: 0.5rem; font-size: 0.85rem;")
        ))
      }
      
      if (rv$data_loaded && !is.null(rv$player_data)) {
        n <- nrow(rv$player_data)
        n_rated <- sum(rv$player_data$n_games > 0, na.rm = TRUE)
        n_odds <- sum(!is.na(rv$player_data$opponent))
        n_own <- if (!is.null(rv$ownership_hist)) length(unique(rv$ownership_hist$gameweek)) else 0
        
        return(div(
          style = "margin-top: 0.5rem; padding: 0.5rem; background: #E8F5E9; border-radius: 4px; font-size: 0.8rem; color: #2E7D32;",
          icon("check-circle"),
          sprintf(" %d players loaded. %d with historical stats. %d with odds context. Ownership calibrated on %d gameweeks.",
                  n, n_rated, n_odds, n_own)
        ))
      }
      
      div(
        style = "margin-top: 0.5rem; padding: 0.5rem; background: var(--bg-secondary); border-radius: 4px; font-size: 0.8rem; color: var(--text-muted);",
        icon("info-circle"),
        " Select a gameweek and click 'Load Data' to begin."
      )
    })
    
    # =========================================================================
    # RATING GUIDE
    # =========================================================================
    
    output$rating_guide <- renderUI({
      generate_rating_guide_html()
    })
    
    # =========================================================================
    # POSITION FILTER BUTTONS
    # =========================================================================
    
    lapply(c("all", "gk", "def", "mid", "fwd"), function(pos_id) {
      observeEvent(input[[paste0("filter_", pos_id)]], {
        rv$position_filter <- if (pos_id == "all") "all" else toupper(pos_id)
        
        # Toggle active class via shinyjs
        all_buttons <- paste0("#", ns(paste0("filter_", c("all", "gk", "def", "mid", "fwd"))))
        for (btn in all_buttons) {
          shinyjs::removeClass(selector = btn, class = "active")
        }
        shinyjs::addClass(selector = paste0("#", ns(paste0("filter_", pos_id))), class = "active")
      })
    })
    
    # =========================================================================
    # FILTERED + SORTED POOL
    # =========================================================================
    
    filtered_pool <- reactive({
      req(rv$player_data)
      data <- rv$player_data
      
      # Position filter (uses unified `pos` column)
      if (rv$position_filter != "all") {
        data <- data %>% filter(pos == rv$position_filter)
      }
      
      # Team filter
      team_f <- input$filter_team
      if (!is.null(team_f) && team_f != "all") {
        data <- data %>% filter(team_normalized == team_f)
      }
      
      # Sort
      sort_col <- input$sort_by %||% "cash_score"
      if (sort_col %in% names(data)) {
        data <- data %>% arrange(desc(.data[[sort_col]]))
      }
      
      data
    })
    
    # =========================================================================
    # POOL COUNT
    # =========================================================================
    
    output$pool_count <- renderUI({
      data <- filtered_pool()
      n <- if (!is.null(data)) nrow(data) else 0
      tags$span(paste(n, "players"))
    })
    
    # =========================================================================
    # PLAYER POOL TABLE
    # =========================================================================
    
    output$player_pool_table <- renderReactable({
      data <- filtered_pool()
      
      if (is.null(data) || nrow(data) == 0) {
        return(reactable(
          data.frame(Message = "No players available. Load data first."),
          theme = app_reactable_theme()
        ))
      }
      
      # Prepare display columns
      display <- data %>%
        select(any_of(c(
          "player", "pos", "team_normalized", "salary",
          "ppg", "pts_floor", "pts_ceiling", "sortino",
          "matchup_quality", "est_own_pct",
          "cash_score", "cash_rating", "cash_color", "cash_bg",
          "gpp_score", "gpp_rating", "gpp_color", "gpp_bg",
          "captain_score", "captain_rating", "captain_color", "captain_bg",
          "opponent", "home_away", "value"
        )))
      
      # Rename for display
      if ("team_normalized" %in% names(display)) {
        display <- display %>% rename(team = team_normalized)
      }
      
      reactable(
        display,
        theme = app_reactable_theme(),
        columns = list(
          player = colDef(name = "Player", minWidth = 130, sticky = "left",
                          style = list(fontWeight = 600, fontSize = "0.8rem")),
          pos = colDef(name = "Pos", maxWidth = 50, align = "center",
                       cell = function(value) {
                         div(style = sprintf(
                           "background: %s; color: white; padding: 0.1rem 0.3rem; border-radius: 3px; font-weight: 700; font-size: 0.65rem; text-align: center;",
                           get_position_color(value)),
                           value)
                       }),
          team = colDef(name = "Team", maxWidth = 90, style = list(fontSize = "0.75rem")),
          salary = colDef(name = "Sal", maxWidth = 55, align = "center",
                          cell = function(value) sprintf("%.1f", value),
                          style = list(fontWeight = 600, fontSize = "0.8rem")),
          ppg = colDef(name = "PPG", maxWidth = 50, align = "center",
                       format = colFormat(digits = 1),
                       style = list(fontSize = "0.75rem")),
          pts_floor = colDef(name = "Floor", maxWidth = 50, align = "center",
                             format = colFormat(digits = 1),
                             style = list(fontSize = "0.75rem")),
          pts_ceiling = colDef(name = "Ceil", maxWidth = 50, align = "center",
                               format = colFormat(digits = 1),
                               style = list(fontSize = "0.75rem")),
          sortino = colDef(name = "Sort", maxWidth = 50, align = "center",
                           format = colFormat(digits = 2),
                           style = list(fontSize = "0.75rem")),
          matchup_quality = colDef(name = "MQ", maxWidth = 45, align = "center",
                                   format = colFormat(digits = 0),
                                   style = list(fontSize = "0.75rem")),
          est_own_pct = colDef(name = "Own%", maxWidth = 50, align = "center",
                               cell = function(value) sprintf("%.1f", value),
                               style = list(fontSize = "0.75rem", color = "#6A1B9A")),
          cash_score = colDef(show = FALSE),
          cash_rating = colDef(
            name = "Cash", maxWidth = 90, align = "center",
            cell = function(value, index) {
              color <- display$cash_color[index]
              bg <- display$cash_bg[index]
              div(style = sprintf("background: %s; color: %s; padding: 0.15rem 0.35rem; border-radius: 4px; font-weight: 700; font-size: 0.7rem; text-align: center; white-space: nowrap;", bg, color), value)
            }
          ),
          cash_color = colDef(show = FALSE),
          cash_bg = colDef(show = FALSE),
          gpp_score = colDef(show = FALSE),
          gpp_rating = colDef(
            name = "GPP", maxWidth = 95, align = "center",
            cell = function(value, index) {
              color <- display$gpp_color[index]
              bg <- display$gpp_bg[index]
              div(style = sprintf("background: %s; color: %s; padding: 0.15rem 0.35rem; border-radius: 4px; font-weight: 700; font-size: 0.7rem; text-align: center; white-space: nowrap;", bg, color), value)
            }
          ),
          gpp_color = colDef(show = FALSE),
          gpp_bg = colDef(show = FALSE),
          captain_score = colDef(show = FALSE),
          captain_rating = colDef(
            name = "Cpt", maxWidth = 95, align = "center",
            cell = function(value, index) {
              color <- display$captain_color[index]
              bg <- display$captain_bg[index]
              div(style = sprintf("background: %s; color: %s; padding: 0.15rem 0.35rem; border-radius: 4px; font-weight: 700; font-size: 0.7rem; text-align: center; white-space: nowrap;", bg, color), value)
            }
          ),
          captain_color = colDef(show = FALSE),
          captain_bg = colDef(show = FALSE),
          opponent = colDef(name = "Opp", maxWidth = 70, 
                            style = list(fontSize = "0.75rem")),
          home_away = colDef(name = "H/A", maxWidth = 35, align = "center",
                             style = list(fontSize = "0.75rem")),
          value = colDef(name = "Val", maxWidth = 45, align = "center",
                         format = colFormat(digits = 2),
                         style = list(fontSize = "0.75rem"))
        ),
        searchable = TRUE,
        sortable = TRUE,
        defaultSorted = "cash_score",
        defaultSortOrder = "desc",
        pagination = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        selection = "multiple",
        onClick = "select",
        rowStyle = list(cursor = "pointer")
      )
    })
    
    # =========================================================================
    # ADD PLAYER TO LINEUP (from table selection)
    # =========================================================================
    
    observeEvent(reactable::getReactableState("player_pool_table", "selected"), {
      selected_rows <- reactable::getReactableState("player_pool_table", "selected")
      req(selected_rows)
      
      data <- filtered_pool()
      req(data, nrow(data) > 0)
      
      contest <- input$contest_type %||% "classic"
      structure <- LINEUP_STRUCTURE[[contest]]
      cap <- input$salary_cap %||% 100
      
      for (idx in selected_rows) {
        if (idx > nrow(data)) next
        player_row <- data[idx, ]
        
        # Check player not already in lineup
        already_in <- length(rv$lineup) > 0 && any(vapply(rv$lineup, function(p) {
          if (!is.null(p)) identical(p$player, player_row$player) else FALSE
        }, logical(1)))
        if (already_in) next
        
        # Find first open slot for this position
        added <- FALSE
        for (slot_idx in seq_along(structure$slots)) {
          slot_pos <- structure$slots[slot_idx]
          
          if (is.null(rv$lineup[[as.character(slot_idx)]])) {
            # Position match (FLEX/CPT accept any)
            if (slot_pos %in% c("FLEX", "CPT") || slot_pos == player_row$pos) {
              # Check salary cap
              current_salary <- if (length(rv$lineup) == 0) 0 else {
                sum(vapply(rv$lineup, function(p) {
                  if (!is.null(p)) as.numeric(p$salary %||% 0) else 0
                }, numeric(1)))
              }
              if (current_salary + as.numeric(player_row$salary) <= cap) {
                rv$lineup[[as.character(slot_idx)]] <- as.list(player_row)
                added <- TRUE
                break
              }
            }
          }
        }
        
        if (!added) {
          log_debug("Could not add player:", player_row$player, "(no open slot or over cap)", level = "DEBUG")
        }
      }
      
      # Clear table selection
      reactable::updateReactable("player_pool_table", selected = NA)
    })
    
    # =========================================================================
    # LINEUP DISPLAY
    # =========================================================================
    
    lineup_stats <- reactive({
      lineup <- rv$lineup
      cap <- input$salary_cap %||% 100
      contest <- input$contest_type %||% "classic"
      structure <- LINEUP_STRUCTURE[[contest]]
      
      # Guard: empty lineup returns safe defaults
      if (length(lineup) == 0) {
        return(list(
          total_salary = 0,
          remaining = cap,
          filled = 0,
          total_slots = structure$total,
          total_ppg = 0
        ))
      }
      
      # Use vapply for type-safe iteration (sapply on lists returns list, breaking sum())
      total_sal <- sum(vapply(lineup, function(p) {
        if (!is.null(p)) as.numeric(p$salary %||% 0) else 0
      }, numeric(1)))
      
      filled <- sum(vapply(lineup, function(p) !is.null(p), logical(1)))
      
      total_ppg <- sum(vapply(lineup, function(p) {
        if (!is.null(p)) as.numeric(p$ppg %||% 0) else 0
      }, numeric(1)))
      
      list(
        total_salary = total_sal,
        remaining = cap - total_sal,
        filled = filled,
        total_slots = structure$total,
        total_ppg = total_ppg
      )
    })
    
    output$salary_used <- renderText({
      sprintf("%.1fM", lineup_stats()$total_salary)
    })
    
    output$salary_remaining <- renderUI({
      remaining <- lineup_stats()$remaining
      color <- if (remaining < 0) "#C62828" else "#2E7D32"
      tags$span(style = sprintf("color: %s;", color),
                sprintf("%.1fM", remaining))
    })
    
    output$slots_filled <- renderText({
      stats <- lineup_stats()
      sprintf("%d / %d", stats$filled, stats$total_slots)
    })
    
    output$projected_ppg <- renderText({
      sprintf("%.1f", lineup_stats()$total_ppg)
    })
    
    output$lineup_slots <- renderUI({
      contest <- input$contest_type %||% "classic"
      structure <- LINEUP_STRUCTURE[[contest]]
      lineup <- rv$lineup
      
      slot_uis <- lapply(seq_along(structure$slots), function(i) {
        slot_pos <- structure$labels[i]
        player <- lineup[[as.character(i)]]
        
        if (is.null(player)) {
          # Empty slot
          div(
            style = "display: flex; align-items: center; padding: 0.35rem 0.5rem; margin-bottom: 0.3rem; background: var(--bg-secondary); border: 2px dashed var(--outline); border-radius: 6px;",
            span(
              style = sprintf("background: %s; color: white; padding: 0.15rem 0.35rem; border-radius: 4px; font-weight: 700; font-size: 0.65rem; min-width: 32px; text-align: center;",
                              get_position_color(slot_pos)),
              slot_pos
            ),
            span(style = "flex: 1; padding-left: 0.5rem; color: var(--text-muted); font-style: italic; font-size: 0.8rem;", "Empty")
          )
        } else {
          # Filled slot
          p_pos <- player$pos %||% slot_pos
          p_opp <- if (!is.na(player$opponent %||% NA)) paste("vs", player$opponent) else ""
          
          div(
            style = "display: flex; align-items: center; padding: 0.35rem 0.5rem; margin-bottom: 0.3rem; background: white; border: 2px solid var(--accent-sage); border-radius: 6px;",
            span(
              style = sprintf("background: %s; color: white; padding: 0.15rem 0.35rem; border-radius: 4px; font-weight: 700; font-size: 0.65rem; min-width: 32px; text-align: center;",
                              get_position_color(p_pos)),
              p_pos
            ),
            div(
              style = "flex: 1; padding-left: 0.5rem;",
              div(style = "font-weight: 600; font-size: 0.8rem; line-height: 1.2;", player$player),
              div(
                style = "font-size: 0.65rem; color: var(--text-muted);",
                paste(player$team_normalized %||% "", p_opp)
              )
            ),
            div(
              style = "text-align: right; padding-right: 0.3rem;",
              div(style = "font-weight: 600; font-size: 0.75rem;", sprintf("%.1fM", as.numeric(player$salary))),
              if (!is.null(player$ppg) && as.numeric(player$ppg %||% 0) > 0) {
                div(style = "font-size: 0.65rem; color: #2E7D32; font-weight: 600;",
                    sprintf("%.1f ppg", as.numeric(player$ppg)))
              }
            ),
            actionButton(
              ns(paste0("remove_slot_", i)),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.15rem 0.35rem; min-width: auto; font-size: 0.65rem;",
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("remove_slot"), i)
            )
          )
        }
      })
      
      tagList(slot_uis)
    })
    
    # Remove slot handler
    observeEvent(input$remove_slot, {
      slot_idx <- as.character(input$remove_slot)
      log_debug("Removing player from slot", slot_idx, level = "DEBUG")
      rv$lineup[[slot_idx]] <- NULL
    })
    
    # Clear lineup handler
    observeEvent(input$clear_lineup, {
      log_debug("Clearing lineup", level = "INFO")
      rv$lineup <- list()
    })
    
    # =========================================================================
    # LP OPTIMIZER
    # =========================================================================
    
    observeEvent(input$optimize_lineup, {
      req(rv$player_data)
      
      data <- rv$player_data %>%
        filter(!is.na(ppg), !is.na(salary), !is.na(pos))
      
      cap <- input$salary_cap %||% 100
      contest <- input$contest_type %||% "classic"
      
      log_debug("Running LP optimizer for", contest, "contest.", nrow(data), "eligible players", level = "INFO")
      
      tryCatch({
        if (contest == "classic") {
          n <- nrow(data)
          
          # Objective: maximize PPG
          obj <- data$ppg
          
          # Constraint matrix:
          #   Row 1: GK count = 1
          #   Row 2: DEF count = 4
          #   Row 3: MID count = 3
          #   Row 4: FWD count = 3
          #   Row 5: Total salary <= cap
          #   Row 6: Total players = 11
          pos_matrix <- matrix(0, nrow = 4, ncol = n)
          pos_matrix[1, ] <- as.integer(data$pos == "GK")
          pos_matrix[2, ] <- as.integer(data$pos == "DEF")
          pos_matrix[3, ] <- as.integer(data$pos == "MID")
          pos_matrix[4, ] <- as.integer(data$pos == "FWD")
          
          const_mat <- rbind(
            pos_matrix,
            data$salary,
            rep(1, n)
          )
          const_dir <- c("=", "=", "=", "=", "<=", "=")
          const_rhs <- c(1, 4, 3, 3, cap, 11)
          
          result <- lp("max", obj, const_mat, const_dir, const_rhs, all.bin = TRUE)
          
          if (result$status == 0) {
            selected_idx <- which(result$solution == 1)
            opt_data <- data[selected_idx, ]
            
            # Build lineup in correct slot order
            rv$lineup <- list()
            slot_cursor <- 1
            
            for (target_pos in c("GK", "DEF", "DEF", "DEF", "DEF", "MID", "MID", "MID", "FWD", "FWD", "FWD")) {
              pos_rows <- opt_data %>% filter(pos == target_pos)
              if (nrow(pos_rows) > 0) {
                rv$lineup[[as.character(slot_cursor)]] <- as.list(pos_rows[1, ])
                opt_data <- opt_data %>% filter(player != pos_rows$player[1])
              }
              slot_cursor <- slot_cursor + 1
            }
            
            opt_salary <- sum(data$salary[selected_idx])
            showNotification(
              sprintf("Optimal lineup: %.1f projected PPG, %.1fM salary",
                      result$objval, opt_salary),
              type = "message"
            )
            log_debug(sprintf("Optimizer success: %.1f PPG, %.1fM salary", result$objval, opt_salary), level = "INFO")
          } else {
            showNotification("No feasible lineup found within constraints.", type = "warning")
            log_debug("Optimizer: no feasible solution (status", result$status, ")", level = "WARN")
          }
          
        } else {
          showNotification("Showdown optimization coming soon!", type = "message")
        }
        
      }, error = function(e) {
        log_debug("Optimizer error:", e$message, level = "ERROR")
        showNotification(paste("Optimizer error:", e$message), type = "error")
      })
    })
    
  })
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get position badge color (consistent with app theme)
#' @param pos Position abbreviation (GK/DEF/MID/FWD/CPT/FLEX)
#' @return Hex color string
get_position_color <- function(pos) {
  colors <- c(
    "GK"  = "#5C9A9A",
    "DEF" = "#6B8E6B",
    "MID" = "#8B7355",
    "FWD" = "#BF7460",
    "CPT" = "#EBCB8B",
    "FLEX" = "#7A7A7A"
  )
  if (pos %in% names(colors)) colors[[pos]] else "#7A7A7A"
}