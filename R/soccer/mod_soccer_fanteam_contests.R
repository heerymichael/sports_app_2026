# =============================================================================
# Module: Soccer FanTeam Contests - REWORKED
# 
# Player salary tables for FanTeam Monster contests
# Team summary with FBref xG data and odds integration
# =============================================================================
# Soccer card color
SOCCER_CARD_COLOR <- APP_COLORS$sage
# FanTeam Stats Google Sheet ID
FANTEAM_STATS_SHEET_ID <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"
# Cache for stats overview data
FANTEAM_STATS_CACHE <- new.env()

# =============================================================================
# ODDS DATA LOADING
# =============================================================================

#' Load odds report from CSV file in fanteam_monster_salaries folder
#' @param gameweek Optional gameweek number to match specific file
#' @return Data frame with odds data or NULL if not found
load_fanteam_odds <- function(gameweek = NULL) {
  if (!dir.exists(FANTEAM_SOCCER_DIR)) {
    log_debug("Odds: Directory not found:", FANTEAM_SOCCER_DIR, level = "DEBUG")
    return(NULL)
  }
  
  # Look for exact pattern: week_X_odds_report.csv
  all_files <- list.files(FANTEAM_SOCCER_DIR, pattern = "\\.csv$", full.names = FALSE)
  
  selected_file <- NULL
  if (!is.null(gameweek)) {
    # Exact match: week_19_odds_report.csv
    exact_pattern <- sprintf("week_%d_odds_report.csv", gameweek)
    if (exact_pattern %in% all_files) {
      selected_file <- exact_pattern
    }
  }
  
  # If no gameweek specified or no exact match, find most recent odds_report file
  if (is.null(selected_file)) {
    odds_report_files <- all_files[grepl("^week_\\d+_odds_report\\.csv$", all_files)]
    if (length(odds_report_files) > 0) {
      # Sort by week number descending
      weeks <- as.integer(gsub("^week_(\\d+)_odds_report\\.csv$", "\\1", odds_report_files))
      selected_file <- odds_report_files[order(weeks, decreasing = TRUE)[1]]
    }
  }
  
  if (is.null(selected_file)) {
    log_debug("Odds: No odds_report file found", level = "DEBUG")
    return(NULL)
  }
  
  file_path <- file.path(FANTEAM_SOCCER_DIR, selected_file)
  log_debug("Odds: Loading file:", selected_file, level = "INFO")
  
  odds_data <- tryCatch({
    raw_lines <- readLines(file_path, n = 3, encoding = "UTF-8")
    skip_rows <- 0
    if (length(raw_lines) > 1) {
      first_line <- raw_lines[1]
      if (grepl("Implied Goals", first_line, ignore.case = TRUE) || 
          nchar(gsub(",", "", first_line)) < 10) {
        skip_rows <- 1
        log_debug("Odds: Skipping header row", level = "DEBUG")
      }
    }
    
    read_csv(file_path, skip = skip_rows, show_col_types = FALSE, 
             locale = locale(encoding = "UTF-8")) %>%
      clean_names()
  }, error = function(e) {
    log_debug("Odds: Error reading file:", e$message, level = "WARN")
    return(NULL)
  })
  
  if (is.null(odds_data) || nrow(odds_data) == 0) return(NULL)
  
  log_debug("Odds: Raw columns after clean_names:", paste(names(odds_data), collapse = ", "), level = "DEBUG")
  
  # Standardize column names - handle multiple variants
  col_renames <- c(
    "team" = "odds_team",
    "opponent" = "odds_opponent", 
    "h_a" = "home_away",
    # Win % variants
    "win" = "win_pct",
    "win_percent" = "win_pct",
    # Draw % variants  
    "draw" = "draw_pct",
    "draw_percent" = "draw_pct",
    # Clean Sheet % variants
    "clean_sheet" = "clean_sheet_pct",
    "clean_sheet_percent" = "clean_sheet_pct",
    # Implied goals
    "scored" = "implied_team_goals",
    "allowed" = "implied_opp_goals",
    "total" = "implied_total"
  )
  
  for (old_name in names(col_renames)) {
    new_name <- col_renames[old_name]
    if (old_name %in% names(odds_data) && !new_name %in% names(odds_data)) {
      names(odds_data)[names(odds_data) == old_name] <- new_name
      log_debug(sprintf("Odds: Renamed %s -> %s", old_name, new_name), level = "DEBUG")
    }
  }
  
  log_debug("Odds: Final columns:", paste(names(odds_data), collapse = ", "), level = "DEBUG")
  
  # Normalize team names - trim whitespace first
  if ("odds_team" %in% names(odds_data)) {
    odds_data <- odds_data %>%
      mutate(
        odds_team = trimws(odds_team),
        odds_opponent = trimws(odds_opponent),
        odds_team_normalized = normalize_team_names(odds_team),
        odds_opponent_normalized = normalize_team_names(odds_opponent)
      )
    log_debug("Odds: Teams:", paste(unique(odds_data$odds_team_normalized), collapse = ", "), level = "DEBUG")
  } else {
    log_debug("Odds: WARNING - no odds_team column found!", level = "WARN")
  }
  
  return(odds_data)
}

#' Merge odds data with player salary data
merge_fanteam_with_odds <- function(salary_data, odds_data) {
  if (is.null(salary_data) || nrow(salary_data) == 0) return(salary_data)
  
  # Add empty columns if no odds data
  if (is.null(odds_data) || nrow(odds_data) == 0) {
    log_debug("Merge: No odds data provided", level = "DEBUG")
    salary_data$opponent <- NA_character_
    salary_data$home_away <- NA_character_
    salary_data$implied_team_goals <- NA_real_
    salary_data$implied_opp_goals <- NA_real_
    salary_data$clean_sheet_pct <- NA_real_
    return(salary_data)
  }
  
  log_debug(sprintf("Merge: Odds data has %d rows", nrow(odds_data)), level = "DEBUG")
  log_debug(sprintf("Merge: Odds columns: %s", paste(names(odds_data), collapse = ", ")), level = "DEBUG")
  
  # Select odds columns for join
  odds_cols <- c("odds_team_normalized", "odds_opponent_normalized", "home_away",
                 "implied_team_goals", "implied_opp_goals", "win_pct", "clean_sheet_pct")
  odds_cols <- intersect(odds_cols, names(odds_data))
  
  log_debug(sprintf("Merge: Using columns: %s", paste(odds_cols, collapse = ", ")), level = "DEBUG")
  
  if (!"odds_team_normalized" %in% odds_cols) {
    log_debug("Merge: ERROR - odds_team_normalized not found!", level = "WARN")
    salary_data$opponent <- NA_character_
    salary_data$home_away <- NA_character_
    salary_data$implied_team_goals <- NA_real_
    salary_data$implied_opp_goals <- NA_real_
    salary_data$clean_sheet_pct <- NA_real_
    return(salary_data)
  }
  
  # Check team name overlap - debug output
  odds_teams <- unique(odds_data$odds_team_normalized)
  salary_teams <- unique(salary_data$team_normalized)
  overlap <- intersect(odds_teams, salary_teams)
  log_debug(sprintf("Merge: %d odds teams, %d salary teams, %d overlap", 
                    length(odds_teams), length(salary_teams), length(overlap)), level = "DEBUG")
  
  if (length(overlap) == 0) {
    log_debug("Merge: NO TEAM OVERLAP! Showing team names for debugging:", level = "WARN")
    log_debug(sprintf("  Odds teams: %s", paste(sort(odds_teams), collapse = ", ")), level = "WARN")
    log_debug(sprintf("  Salary teams: %s", paste(sort(salary_teams), collapse = ", ")), level = "WARN")
  }
  
  odds_subset <- odds_data %>% select(all_of(odds_cols))
  
  merged <- salary_data %>%
    left_join(odds_subset, by = c("team_normalized" = "odds_team_normalized"))
  
  if ("odds_opponent_normalized" %in% names(merged)) {
    merged <- merged %>% rename(opponent = odds_opponent_normalized)
  } else {
    merged$opponent <- NA_character_
  }
  
  if (!"home_away" %in% names(merged)) merged$home_away <- NA_character_
  if (!"implied_team_goals" %in% names(merged)) merged$implied_team_goals <- NA_real_
  if (!"implied_opp_goals" %in% names(merged)) merged$implied_opp_goals <- NA_real_
  if (!"clean_sheet_pct" %in% names(merged)) merged$clean_sheet_pct <- NA_real_
  
  # Log merge results
  matched <- sum(!is.na(merged$opponent))
  log_debug(sprintf("Merge: %d/%d players matched (%.1f%%)", 
                    matched, nrow(merged), 100 * matched / nrow(merged)), level = "INFO")
  
  return(merged)
}

#' Load FanTeam stats overview data from Google Sheet
load_fanteam_stats_overview <- function(force_refresh = FALSE) {
  cache_key <- "stats_overview"
  cache_time_key <- "stats_overview_time"
  
  if (!force_refresh && 
      exists(cache_key, envir = FANTEAM_STATS_CACHE) &&
      exists(cache_time_key, envir = FANTEAM_STATS_CACHE)) {
    cache_age <- difftime(Sys.time(), get(cache_time_key, envir = FANTEAM_STATS_CACHE), units = "mins")
    if (cache_age < 10) {
      return(get(cache_key, envir = FANTEAM_STATS_CACHE))
    }
  }
  
  stats <- tryCatch({
    raw_data <- read_sheet(FANTEAM_STATS_SHEET_ID, sheet = "stats_overview") %>%
      clean_names()
    
    col_renames <- c(
      "name" = "player",
      "total_pts" = "total_points",
      "avg_pts" = "average_points",
      "g" = "goals",
      "shots_on_target" = "sot",
      "a" = "assists",
      "x60_mp" = "mins_60",
      "x90_mp" = "mins_90",
      "cs" = "clean_sheets",
      "sv" = "saves",
      "gc" = "goals_conceded",
      "yel" = "yellows",
      "red" = "reds"
    )
    
    for (old_name in names(col_renames)) {
      new_name <- col_renames[old_name]
      if (old_name %in% names(raw_data) && !new_name %in% names(raw_data)) {
        names(raw_data)[names(raw_data) == old_name] <- new_name
      }
    }
    
    numeric_cols <- c("total_points", "average_points", "goals", "sot", "assists",
                      "mins_60", "mins_90", "clean_sheets", "saves", "goals_conceded",
                      "yellows", "reds")
    
    for (col in numeric_cols) {
      if (col %in% names(raw_data)) {
        raw_data[[col]] <- suppressWarnings(as.numeric(raw_data[[col]]))
      }
    }
    
    raw_data
  }, error = function(e) {
    log_debug("Error loading stats_overview:", e$message, level = "WARN")
    return(NULL)
  })
  
  if (!is.null(stats) && nrow(stats) > 0) {
    assign(cache_key, stats, envir = FANTEAM_STATS_CACHE)
    assign(cache_time_key, Sys.time(), envir = FANTEAM_STATS_CACHE)
  }
  
  return(stats)
}

#' Merge FanTeam salary data with stats overview
merge_fanteam_with_stats <- function(salary_data, stats_data) {
  if (is.null(salary_data) || nrow(salary_data) == 0) return(salary_data)
  if (is.null(stats_data) || nrow(stats_data) == 0) return(salary_data)
  
  if (!"player" %in% names(stats_data)) return(salary_data)
  
  cols_to_exclude <- c("salary", "price", "cost", "value", "weekly_salary")
  stats_cols_to_drop <- intersect(cols_to_exclude, names(stats_data))
  if (length(stats_cols_to_drop) > 0) {
    stats_data <- stats_data %>% select(-all_of(stats_cols_to_drop))
  }
  
  local_normalize <- function(x) {
    if (is.null(x) || length(x) == 0) return(x)
    x <- tolower(trimws(as.character(x)))
    if (requireNamespace("stringi", quietly = TRUE)) {
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    }
    gsub("\\s+", " ", x)
  }
  
  salary_data <- salary_data %>% mutate(player_norm = local_normalize(player))
  stats_data <- stats_data %>% mutate(player_norm = local_normalize(player))
  
  stat_cols <- c("total_points", "average_points", "goals", "sot", "assists",
                 "mins_60", "mins_90", "clean_sheets", "saves", "goals_conceded", 
                 "yellows", "reds")
  available_stat_cols <- intersect(stat_cols, names(stats_data))
  
  if (length(available_stat_cols) == 0) {
    return(salary_data %>% select(-player_norm))
  }
  
  merged <- tryCatch({
    salary_data %>%
      left_join(
        stats_data %>% select(player_norm, all_of(available_stat_cols)),
        by = "player_norm"
      ) %>%
      select(-player_norm)
  }, error = function(e) {
    salary_data %>% select(-player_norm)
  })
  
  return(merged)
}

# =============================================================================
# UI
# =============================================================================
soccer_fanteam_contests_ui <- function(id) {
  ns <- NS(id)
  
  gameweeks <- get_fanteam_soccer_gameweeks()
  
  if (length(gameweeks) > 0) {
    gw_choices <- setNames(as.character(gameweeks), paste("Gameweek", gameweeks))
    gw_selected <- as.character(gameweeks[1])
  } else {
    gw_choices <- c("No data found" = "")
    gw_selected <- NULL
  }
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("FanTeam Contests"),
      tags$p(class = "text-muted", "Team analysis and player salaries for FanTeam Monster contests")
    ),
    
    # Top filters: Gameweek and Status only
    ui_card(
      title = "Filters",
      color = SOCCER_CARD_COLOR,
      fluidRow(
        column(4,
               selectInput(ns("gameweek"), "Gameweek",
                           choices = gw_choices,
                           selected = gw_selected)
        ),
        column(4,
               shinyWidgets::pickerInput(ns("status"), "Availability",
                                         choices = c("Expected" = "expected",
                                                     "Possible" = "possible",
                                                     "Unexpected" = "unexpected",
                                                     "Injured" = "injured",
                                                     "Suspended" = "suspended"),
                                         selected = c("expected", "possible", "unexpected"),
                                         multiple = TRUE,
                                         options = shinyWidgets::pickerOptions(
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 2"
                                         ))
        ),
        column(4,
               div(style = "padding-top: 25px; font-size: 0.85rem; color: var(--text-muted);",
                   "11 players: 1 GK, 3+ DEF, 3+ MID, 1+ FWD"))
      )
    ),
    
    tags$br(),
    
    # Team Summary Table - Position filter without ALL option, defaults to FWD
    ui_card(
      title = "Team Summary",
      color = SOCCER_CARD_COLOR,
      fluidRow(
        column(3,
               selectInput(ns("summary_position"), "Position Group",
                           choices = c("Forwards" = "FWD",
                                       "Midfielders" = "MID",
                                       "Defenders" = "DEF",
                                       "Goalkeepers" = "GK"),
                           selected = "FWD")
        ),
        column(2,
               selectInput(ns("summary_view"), "View",
                           choices = c("Table" = "table",
                                       "Plots" = "plots"),
                           selected = "table")
        ),
        column(3,
               # Only show when Plots view is selected
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'plots'", ns("summary_view")),
                 selectInput(ns("summary_plot_type"), "Plot Type",
                             choices = c("Avg Salary vs Fantasy Opportunity" = "salary_vs_fos",
                                         "Avg Salary vs Clean Sheet %" = "salary_vs_cs",
                                         "Avg Salary vs Implied Goals For" = "salary_vs_gf",
                                         "Avg Salary vs Implied Goals Against" = "salary_vs_ga"),
                             selected = "salary_vs_fos")
               )
        ),
        column(3,
               # Only show xG Timeframe when Table view is selected
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'table'", ns("summary_view")),
                 selectInput(ns("xg_timeframe"), "xG Timeframe",
                             choices = c("Whole Season" = "season",
                                         "Last 6 Games" = "last6"),
                             selected = "season")
               )
        )
      ),
      
      # Dynamic content based on view selection
      uiOutput(ns("team_summary_content"))
    ),
    
    tags$br(),
    
    # Position filter for player table
    ui_card(
      title = "Player Salaries",
      color = SOCCER_CARD_COLOR,
      fluidRow(
        column(3,
               selectInput(ns("position"), "Position",
                           choices = c("All Positions" = "all", "GK", "DEF", "MID", "FWD"),
                           selected = "all")
        ),
        column(3,
               shinyWidgets::pickerInput(ns("team"), "Team",
                                         choices = c("All Teams" = "all"),
                                         selected = "all",
                                         options = shinyWidgets::pickerOptions(liveSearch = TRUE, size = 10))
        ),
        column(3,
               selectInput(ns("sort_by"), "Sort By",
                           choices = c("Salary (High to Low)" = "salary_desc",
                                       "Salary (Low to High)" = "salary_asc",
                                       "Total Points" = "points_desc",
                                       "Avg Points" = "avg_desc",
                                       "Player Name" = "player"),
                           selected = "salary_desc")
        ),
        column(3,
               numericInput(ns("salary_cap"), "Salary Cap",
                            value = 118, min = 50, max = 200, step = 1)
        )
      ),
      uiOutput(ns("salary_table"))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================
soccer_fanteam_contests_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      salary_data = NULL,
      team_xg_season = NULL,
      team_xg_last6 = NULL,
      loading = FALSE
    )
    
    # Load data on gameweek change
    observe({
      gameweek <- input$gameweek
      if (is.null(gameweek) || gameweek == "") return()
      
      rv$loading <- TRUE
      
      # Load salary data
      data <- load_fanteam_soccer_with_logos(as.integer(gameweek))
      
      if (!is.null(data) && nrow(data) > 0) {
        # Player matching
        tryCatch({
          shot_data <- load_shot_data()
          player_data <- load_player_match_stats()
          
          if ((!is.null(shot_data) && nrow(shot_data) > 0) || 
              (!is.null(player_data) && nrow(player_data) > 0)) {
            fbref_players <- get_fbref_player_list(shot_data, player_data)
            if (nrow(fbref_players) > 0) {
              data <- match_fanteam_to_fbref(data, fbref_players, 
                                             gameweek = as.integer(gameweek),
                                             write_unmatched = TRUE)
            }
          }
        }, error = function(e) {
          log_debug("Player matching skipped:", e$message, level = "WARN")
        })
        
        # Stats merge
        tryCatch({
          stats_data <- load_fanteam_stats_overview()
          if (!is.null(stats_data) && nrow(stats_data) > 0) {
            data <- merge_fanteam_with_stats(data, stats_data)
          }
        }, error = function(e) {
          log_debug("Stats merge error:", e$message, level = "ERROR")
        })
        
        # Odds merge
        tryCatch({
          log_debug("Loading odds for gameweek:", gameweek, level = "INFO")
          odds_data <- load_fanteam_odds(as.integer(gameweek))
          if (!is.null(odds_data)) {
            log_debug(sprintf("Odds loaded: %d rows", nrow(odds_data)), level = "INFO")
          } else {
            log_debug("Odds: NULL returned from load_fanteam_odds", level = "WARN")
          }
          data <- merge_fanteam_with_odds(data, odds_data)
        }, error = function(e) {
          log_debug("Odds merge error:", e$message, level = "WARN")
          data$opponent <- NA_character_
          data$home_away <- NA_character_
          data$implied_team_goals <- NA_real_
          data$implied_opp_goals <- NA_real_
          data$clean_sheet_pct <- NA_real_
        })
        
        rv$salary_data <- data
        
        # Calculate team xG stats from FBref - BOTH season and last6
        tryCatch({
          shot_data <- load_shot_data()
          team_goals_data <- load_team_goals()
          
          if (!is.null(shot_data) && nrow(shot_data) > 0) {
            # Season stats
            rv$team_xg_season <- calculate_all_team_stats(
              shot_data, 
              "Premier League", 
              timeframe = "season",
              team_goals_data = team_goals_data
            )
            log_debug(sprintf("Loaded season xG stats for %d teams", 
                              if (!is.null(rv$team_xg_season)) nrow(rv$team_xg_season) else 0), level = "INFO")
            
            # Last 6 games stats
            rv$team_xg_last6 <- calculate_all_team_stats(
              shot_data, 
              "Premier League", 
              timeframe = "last6",
              team_goals_data = team_goals_data
            )
            log_debug(sprintf("Loaded last6 xG stats for %d teams", 
                              if (!is.null(rv$team_xg_last6)) nrow(rv$team_xg_last6) else 0), level = "INFO")
          }
        }, error = function(e) {
          log_debug("Team xG calculation error:", e$message, level = "WARN")
        })
        
        # Update team dropdown
        teams <- sort(unique(data$team_normalized[!is.na(data$team_normalized)]))
        if (length(teams) > 0) {
          team_choices <- c("all", teams)
          names(team_choices) <- c("All Teams", teams)
          shinyWidgets::updatePickerInput(session, "team", choices = team_choices, selected = "all")
        }
      }
      
      rv$loading <- FALSE
    })
    
    # =========================================================================
    # TEAM SUMMARY CONTENT (switches between table and plots)
    # =========================================================================
    
    output$team_summary_content <- renderUI({
      req(rv$salary_data)
      
      view_mode <- input$summary_view
      if (is.null(view_mode) || view_mode == "") view_mode <- "table"
      
      if (view_mode == "table") {
        # Table view with legend
        div(style = "overflow-x: auto; margin-top: 1rem;",
            uiOutput(ns("team_summary_table")),
            div(style = "margin-top: 0.75rem; padding: 0.5rem 0.75rem; background: var(--bg-tertiary); border-radius: 6px; font-size: 0.75rem; color: var(--text-muted);",
                tags$span(style = "font-weight: 600; margin-right: 0.5rem;", "Legend:"),
                "Goals For/Ag. = Implied from odds | ",
                "xGF/xGA Adj = Team's xG vs opponent avg (+ is better) | ",
                "Opp xGF/xGA Adj = This week's opponent's adjusted xG"
            )
        )
      } else {
        # Plot view
        plot_type <- input$summary_plot_type
        if (is.null(plot_type)) plot_type <- "salary_vs_cs"
        
        pos_filter <- input$summary_position
        if (is.null(pos_filter) || pos_filter == "") pos_filter <- "FWD"
        pos_label <- switch(pos_filter,
                            "FWD" = "Forward",
                            "MID" = "Midfielder",
                            "DEF" = "Defender",
                            "GK" = "Goalkeeper",
                            "Position")
        
        # Get chart title/subtitle and axis labels based on plot type
        chart_info <- switch(
          plot_type,
          "salary_vs_fos" = list(
            title = paste(pos_label, "Avg Salary vs Fantasy Opportunity"),
            subtitle = paste0("Position-weighted score combining clean sheet odds & implied goals (", pos_label, " weights)"),
            y_label = "HIGHER SALARY",
            x_label = "BETTER OPPORTUNITY"
          ),
          "salary_vs_cs" = list(
            title = paste(pos_label, "Avg Salary vs Clean Sheet Odds"),
            subtitle = "Average salary for position group vs bookmaker clean sheet probability",
            y_label = "HIGHER SALARY",
            x_label = "HIGHER CS ODDS"
          ),
          "salary_vs_gf" = list(
            title = paste(pos_label, "Avg Salary vs Implied Goals For"),
            subtitle = "Average salary for position group vs bookmaker implied team goals",
            y_label = "HIGHER SALARY",
            x_label = "MORE GOALS"
          ),
          "salary_vs_ga" = list(
            title = paste(pos_label, "Avg Salary vs Implied Goals Against"),
            subtitle = "Average salary for position group vs bookmaker implied opponent goals",
            y_label = "HIGHER SALARY",
            x_label = "MORE CONCEDED"
          ),
          list(title = "", subtitle = "", y_label = "", x_label = "")
        )
        
        # Scatter chart with external axis labels (matching Team Dashboard style)
        div(
          style = "width: 100%; box-sizing: border-box; margin-top: 1rem;",
          
          # Chart title and subtitle
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
              plotOutput(ns("team_summary_plot"), height = "450px", width = "100%")
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
      }
    })
    
    # =========================================================================
    # TEAM SUMMARY TABLE
    # =========================================================================
    
    output$team_summary_table <- renderUI({
      req(rv$salary_data)
      data <- rv$salary_data
      
      if (is.null(data) || nrow(data) == 0) {
        return(div(style = "padding: 2rem; text-align: center;", "No data available"))
      }
      
      # Get position filter - no "all" option anymore, default to FWD
      pos_filter <- input$summary_position
      if (is.null(pos_filter) || pos_filter == "") pos_filter <- "FWD"
      
      # Position counts - max players per position to show
      pos_counts <- c("GK" = 2, "DEF" = 5, "MID" = 5, "FWD" = 3)
      
      # Filter to selected position
      filtered_data <- data %>% filter(position == pos_filter)
      max_for_pos <- pos_counts[pos_filter]
      
      # Build team salary pivot with average first
      team_salaries <- filtered_data %>%
        filter(!is.na(team_normalized), !is.na(position), !is.na(salary)) %>%
        group_by(team_normalized) %>%
        arrange(desc(salary)) %>%
        mutate(pos_rank = row_number()) %>%
        filter(pos_rank <= max_for_pos) %>%
        summarise(
          avg_salary = round(mean(salary, na.rm = TRUE), 2),
          salary_list = list(salary[order(-salary)]),
          .groups = "drop"
        )
      
      # Expand salary list to individual columns
      for (i in 1:max_for_pos) {
        col_name <- paste0(pos_filter, i)
        team_salaries[[col_name]] <- sapply(team_salaries$salary_list, function(x) {
          if (length(x) >= i) x[i] else NA_real_
        })
      }
      team_salaries$salary_list <- NULL
      
      # Get odds data per team
      team_odds <- data %>%
        filter(!is.na(team_normalized)) %>%
        group_by(team_normalized) %>%
        summarise(
          opponent = first(na.omit(opponent)),
          home_away = first(na.omit(home_away)),
          implied_team_goals = first(na.omit(implied_team_goals)),
          implied_opp_goals = first(na.omit(implied_opp_goals)),
          clean_sheet_pct = first(na.omit(clean_sheet_pct)),
          .groups = "drop"
        )
      
      log_debug(sprintf("Team Summary: Odds data has %d rows with %d having opponent", 
                        nrow(team_odds), sum(!is.na(team_odds$opponent))), level = "DEBUG")
      
      team_salaries <- team_salaries %>%
        left_join(team_odds, by = "team_normalized")
      
      # Add FBref xG data - SEASON (opponent-adjusted)
      if (!is.null(rv$team_xg_season) && nrow(rv$team_xg_season) > 0) {
        # Team's opponent-adjusted xG
        team_salaries <- team_salaries %>%
          left_join(
            rv$team_xg_season %>% 
              select(team, xgf_vs_opp_strength, xga_vs_opp_strength) %>%
              rename(xgf_adj_season = xgf_vs_opp_strength, xga_adj_season = xga_vs_opp_strength),
            by = c("team_normalized" = "team")
          )
        
        # Opponent's opponent-adjusted xG stats (for this week's matchup)
        team_salaries <- team_salaries %>%
          left_join(
            rv$team_xg_season %>% 
              select(team, xgf_vs_opp_strength, xga_vs_opp_strength) %>%
              rename(opp_xgf_adj_season = xgf_vs_opp_strength, opp_xga_adj_season = xga_vs_opp_strength),
            by = c("opponent" = "team")
          )
      }
      
      # Add FBref xG data - LAST 6 (opponent-adjusted)
      if (!is.null(rv$team_xg_last6) && nrow(rv$team_xg_last6) > 0) {
        # Team's opponent-adjusted xG
        team_salaries <- team_salaries %>%
          left_join(
            rv$team_xg_last6 %>% 
              select(team, xgf_vs_opp_strength, xga_vs_opp_strength) %>%
              rename(xgf_adj_l6 = xgf_vs_opp_strength, xga_adj_l6 = xga_vs_opp_strength),
            by = c("team_normalized" = "team")
          )
        
        # Opponent's opponent-adjusted xG stats (for this week's matchup)
        team_salaries <- team_salaries %>%
          left_join(
            rv$team_xg_last6 %>% 
              select(team, xgf_vs_opp_strength, xga_vs_opp_strength) %>%
              rename(opp_xgf_adj_l6 = xgf_vs_opp_strength, opp_xga_adj_l6 = xga_vs_opp_strength),
            by = c("opponent" = "team")
          )
      }
      
      # Sort by implied team goals
      if ("implied_team_goals" %in% names(team_salaries) && any(!is.na(team_salaries$implied_team_goals))) {
        team_salaries <- team_salaries %>% arrange(desc(implied_team_goals))
      }
      
      if (nrow(team_salaries) == 0) {
        return(div(style = "padding: 2rem; text-align: center;", "No team data"))
      }
      
      # Define salary columns for later
      individual_salary_cols <- paste0(pos_filter, 1:max_for_pos)
      
      # Build column definitions with larger text
      col_defs <- list()
      
      # Team column with oversized logo background - edge to edge
      col_defs$team_normalized <- reactable::colDef(
        name = "Team",
        minWidth = 180,
        style = list(borderRight = "2px solid #374151"),
        cell = function(value, index) {
          logo_path <- get_soccer_team_logo(value)
          opp <- team_salaries$opponent[index]
          ha <- team_salaries$home_away[index]
          
          # Build matchup string with full opponent name
          matchup <- ""
          if (!is.na(opp) && opp != "") {
            if (!is.na(ha) && toupper(ha) == "H") {
              matchup <- paste("vs", opp)
            } else {
              matchup <- paste("@", opp)
            }
          }
          
          # Outer wrapper - positions logo to extend to cell edge
          htmltools::tags$div(
            style = "position: relative; min-height: 56px; overflow: hidden; margin: -8px; padding: 8px;",
            # Oversized logo - positioned to fill to absolute cell edge
            if (!is.null(logo_path) && logo_path != "") {
              htmltools::tags$img(
                src = logo_path,
                style = "position: absolute; right: -8px; top: 50%; transform: translateY(-50%); width: 150px; height: 150px; opacity: 0.12; object-fit: contain; pointer-events: none;"
              )
            },
            # Text content with tighter left padding
            htmltools::tags$div(
              style = "position: relative; z-index: 1; padding: 8px 8px 8px 4px;",
              htmltools::tags$div(style = "font-weight: 700; font-size: 1rem;", value),
              if (matchup != "") {
                htmltools::tags$div(style = "font-size: 0.85rem; color: #6b7280; margin-top: 2px;", matchup)
              } else {
                htmltools::tags$div(style = "font-size: 0.85rem; color: #9ca3af; margin-top: 2px; font-style: italic;", "No fixture data")
              }
            )
          )
        }
      )
      
      # Average salary column - clean styling
      if ("avg_salary" %in% names(team_salaries)) {
        col_defs$avg_salary <- reactable::colDef(
          name = paste0("Avg ", pos_filter, " Sal"), minWidth = 80, align = "center",
          headerStyle = list(fontSize = "0.8rem", fontWeight = 600),
          style = list(fontWeight = 600, fontSize = "0.9rem"),
          cell = function(value) if (is.na(value)) "-" else sprintf("%.1f", value)
        )
      }
      
      # IMPLIED GOALS columns - more descriptive headers
      if ("implied_team_goals" %in% names(team_salaries)) {
        col_defs$implied_team_goals <- reactable::colDef(
          name = "Goals For", minWidth = 70, align = "center",
          headerStyle = list(fontSize = "0.8rem"),
          style = list(fontSize = "0.9rem"),
          cell = function(value) if (is.na(value)) "-" else sprintf("%.2f", value)
        )
      }
      if ("implied_opp_goals" %in% names(team_salaries)) {
        col_defs$implied_opp_goals <- reactable::colDef(
          name = "Goals Ag.", minWidth = 70, align = "center",
          headerStyle = list(fontSize = "0.8rem"),
          style = list(fontSize = "0.9rem"),
          cell = function(value) if (is.na(value)) "-" else sprintf("%.2f", value)
        )
      }
      if ("clean_sheet_pct" %in% names(team_salaries)) {
        col_defs$clean_sheet_pct <- reactable::colDef(
          name = "Clean Sheet", minWidth = 75, align = "center",
          headerStyle = list(fontSize = "0.8rem"),
          style = list(fontSize = "0.9rem"),
          cell = function(value) if (is.na(value)) "-" else sprintf("%.0f%%", value)
        )
      }
      
      # Get timeframe selection
      xg_timeframe <- input$xg_timeframe
      if (is.null(xg_timeframe) || xg_timeframe == "") xg_timeframe <- "season"
      
      # Helper function for +/- formatting
      format_adj <- function(value) {
        if (is.na(value)) return("-")
        if (value > 0) return(sprintf("+%.2f", value))
        sprintf("%.2f", value)
      }
      
      # xG columns - only show selected timeframe (opponent-adjusted)
      if (xg_timeframe == "season") {
        # SEASON xG columns (opponent-adjusted)
        if ("xgf_adj_season" %in% names(team_salaries)) {
          col_defs$xgf_adj_season <- reactable::colDef(
            name = "xGF Adj", minWidth = 70, align = "center",
            headerStyle = list(fontSize = "0.8rem"),
            style = function(value) {
              color <- if (!is.na(value) && value > 0) "#059669" else if (!is.na(value) && value < 0) "#dc2626" else NULL
              list(fontSize = "0.9rem", color = color, fontWeight = if (!is.null(color)) 600 else 400)
            },
            cell = function(value) format_adj(value)
          )
        }
        if ("xga_adj_season" %in% names(team_salaries)) {
          col_defs$xga_adj_season <- reactable::colDef(
            name = "xGA Adj", minWidth = 70, align = "center",
            headerStyle = list(fontSize = "0.8rem"),
            style = function(value) {
              # For xGA, negative is good (concede less than opponents usually create)
              color <- if (!is.na(value) && value < 0) "#059669" else if (!is.na(value) && value > 0) "#dc2626" else NULL
              list(fontSize = "0.9rem", color = color, fontWeight = if (!is.null(color)) 600 else 400)
            },
            cell = function(value) format_adj(value)
          )
        }
        if ("opp_xgf_adj_season" %in% names(team_salaries)) {
          col_defs$opp_xgf_adj_season <- reactable::colDef(
            name = "Opp xGF Adj", minWidth = 80, align = "center",
            headerStyle = list(fontSize = "0.8rem"),
            style = function(value) {
              # Opponent's attacking strength - high is bad for us
              color <- if (!is.na(value) && value > 0) "#dc2626" else if (!is.na(value) && value < 0) "#059669" else NULL
              list(fontSize = "0.9rem", color = color, fontWeight = if (!is.null(color)) 600 else 400)
            },
            cell = function(value) format_adj(value)
          )
        }
        if ("opp_xga_adj_season" %in% names(team_salaries)) {
          col_defs$opp_xga_adj_season <- reactable::colDef(
            name = "Opp xGA Adj", minWidth = 80, align = "center",
            headerStyle = list(fontSize = "0.8rem"),
            style = function(value) {
              # Opponent's defensive weakness - high is good for us (they concede more than expected)
              color <- if (!is.na(value) && value > 0) "#059669" else if (!is.na(value) && value < 0) "#dc2626" else NULL
              list(fontSize = "0.9rem", color = color, fontWeight = if (!is.null(color)) 600 else 400)
            },
            cell = function(value) format_adj(value)
          )
        }
        # Hide Last 6 columns
        col_defs$xgf_adj_l6 <- reactable::colDef(show = FALSE)
        col_defs$xga_adj_l6 <- reactable::colDef(show = FALSE)
        col_defs$opp_xgf_adj_l6 <- reactable::colDef(show = FALSE)
        col_defs$opp_xga_adj_l6 <- reactable::colDef(show = FALSE)
        
        xg_cols <- c("xgf_adj_season", "xga_adj_season", "opp_xgf_adj_season", "opp_xga_adj_season")
        xg_group_name <- "Season xG (Adj)"
      } else {
        # LAST 6 xG columns (opponent-adjusted)
        if ("xgf_adj_l6" %in% names(team_salaries)) {
          col_defs$xgf_adj_l6 <- reactable::colDef(
            name = "xGF Adj", minWidth = 70, align = "center",
            headerStyle = list(fontSize = "0.8rem"),
            style = function(value) {
              color <- if (!is.na(value) && value > 0) "#059669" else if (!is.na(value) && value < 0) "#dc2626" else NULL
              list(fontSize = "0.9rem", color = color, fontWeight = if (!is.null(color)) 600 else 400)
            },
            cell = function(value) format_adj(value)
          )
        }
        if ("xga_adj_l6" %in% names(team_salaries)) {
          col_defs$xga_adj_l6 <- reactable::colDef(
            name = "xGA Adj", minWidth = 70, align = "center",
            headerStyle = list(fontSize = "0.8rem"),
            style = function(value) {
              color <- if (!is.na(value) && value < 0) "#059669" else if (!is.na(value) && value > 0) "#dc2626" else NULL
              list(fontSize = "0.9rem", color = color, fontWeight = if (!is.null(color)) 600 else 400)
            },
            cell = function(value) format_adj(value)
          )
        }
        if ("opp_xgf_adj_l6" %in% names(team_salaries)) {
          col_defs$opp_xgf_adj_l6 <- reactable::colDef(
            name = "Opp xGF Adj", minWidth = 80, align = "center",
            headerStyle = list(fontSize = "0.8rem"),
            style = function(value) {
              color <- if (!is.na(value) && value > 0) "#dc2626" else if (!is.na(value) && value < 0) "#059669" else NULL
              list(fontSize = "0.9rem", color = color, fontWeight = if (!is.null(color)) 600 else 400)
            },
            cell = function(value) format_adj(value)
          )
        }
        if ("opp_xga_adj_l6" %in% names(team_salaries)) {
          col_defs$opp_xga_adj_l6 <- reactable::colDef(
            name = "Opp xGA Adj", minWidth = 80, align = "center",
            headerStyle = list(fontSize = "0.8rem"),
            style = function(value) {
              color <- if (!is.na(value) && value > 0) "#059669" else if (!is.na(value) && value < 0) "#dc2626" else NULL
              list(fontSize = "0.9rem", color = color, fontWeight = if (!is.null(color)) 600 else 400)
            },
            cell = function(value) format_adj(value)
          )
        }
        # Hide Season columns
        col_defs$xgf_adj_season <- reactable::colDef(show = FALSE)
        col_defs$xga_adj_season <- reactable::colDef(show = FALSE)
        col_defs$opp_xgf_adj_season <- reactable::colDef(show = FALSE)
        col_defs$opp_xga_adj_season <- reactable::colDef(show = FALSE)
        
        xg_cols <- c("xgf_adj_l6", "xga_adj_l6", "opp_xgf_adj_l6", "opp_xga_adj_l6")
        xg_group_name <- "Last 6 xG (Adj)"
      }
      
      # Hide helper columns and any individual salary columns
      col_defs$opponent <- reactable::colDef(show = FALSE)
      col_defs$home_away <- reactable::colDef(show = FALSE)
      
      # Hide all individual salary columns (we only show avg)
      for (i in 1:max_for_pos) {
        col_name <- paste0(pos_filter, i)
        if (col_name %in% names(team_salaries)) {
          col_defs[[col_name]] <- reactable::colDef(show = FALSE)
        }
      }
      
      # Custom theme - header border matches team column right border
      custom_theme <- reactable::reactableTheme(
        borderColor = "#e5e7eb",
        headerStyle = list(
          borderBottom = "2px solid #374151",
          fontWeight = 600,
          fontSize = "0.8rem"
        ),
        cellStyle = list(
          borderBottom = "1px solid #e5e7eb",
          fontSize = "0.9rem"
        )
      )
      
      # Build column groups for visual grouping
      col_groups_def <- list(
        list(name = "Implied (Odds)", columns = c("implied_team_goals", "implied_opp_goals", "clean_sheet_pct")),
        list(name = xg_group_name, columns = xg_cols)
      )
      
      # Filter to only include columns that exist, then create colGroup objects
      col_groups <- list()
      for (grp in col_groups_def) {
        existing_cols <- intersect(as.character(grp$columns), names(team_salaries))
        if (length(existing_cols) > 0) {
          col_groups <- c(col_groups, list(reactable::colGroup(name = grp$name, columns = existing_cols)))
        }
      }
      
      # Reorder columns: Team, Avg Salary, Implied, xG
      desired_order <- c(
        "team_normalized",
        "avg_salary",
        "implied_team_goals", "implied_opp_goals", "clean_sheet_pct",
        xg_cols,
        "opponent", "home_away"
      )
      existing_order <- intersect(desired_order, names(team_salaries))
      remaining_cols <- setdiff(names(team_salaries), existing_order)
      team_salaries <- team_salaries[, c(existing_order, remaining_cols)]
      
      reactable::reactable(
        team_salaries,
        sortable = TRUE,
        defaultPageSize = 22,
        striped = TRUE,
        highlight = TRUE,
        compact = FALSE,
        fullWidth = TRUE,
        theme = custom_theme,
        columns = col_defs,
        columnGroups = col_groups,
        defaultColDef = reactable::colDef(
          vAlign = "center",
          headerVAlign = "center",
          minWidth = 55
        )
      )
    })
    
    # =========================================================================
    # TEAM SUMMARY SCATTER PLOT
    # =========================================================================
    
    output$team_summary_plot <- renderPlot({
      log_debug(">>> Rendering team summary scatter plot", level = "DEBUG")
      
      req(rv$salary_data)
      data <- rv$salary_data
      
      if (is.null(data) || nrow(data) == 0) return(NULL)
      
      # Get position filter
      pos_filter <- input$summary_position
      if (is.null(pos_filter) || pos_filter == "") pos_filter <- "FWD"
      
      plot_type <- input$summary_plot_type
      if (is.null(plot_type)) plot_type <- "salary_vs_fos"
      
      # Position counts - max players per position to show
      pos_counts <- c("GK" = 2, "DEF" = 5, "MID" = 5, "FWD" = 3)
      
      # Filter to selected position
      filtered_data <- data %>% filter(position == pos_filter)
      max_for_pos <- pos_counts[pos_filter]
      
      # Build team salary aggregates
      team_salaries <- filtered_data %>%
        filter(!is.na(team_normalized), !is.na(position), !is.na(salary)) %>%
        group_by(team_normalized) %>%
        arrange(desc(salary)) %>%
        mutate(pos_rank = row_number()) %>%
        filter(pos_rank <= max_for_pos) %>%
        summarise(
          avg_salary = round(mean(salary, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Get odds data per team
      team_odds <- data %>%
        filter(!is.na(team_normalized)) %>%
        group_by(team_normalized) %>%
        summarise(
          implied_team_goals = first(na.omit(implied_team_goals)),
          implied_opp_goals = first(na.omit(implied_opp_goals)),
          clean_sheet_pct = first(na.omit(clean_sheet_pct)),
          .groups = "drop"
        )
      
      plot_data <- team_salaries %>%
        left_join(team_odds, by = "team_normalized")
      
      # Position-specific weights for Fantasy Opportunity Score
      # GKs: CS dominant, minimal goal involvement
      # DEFs: CS important, but attacking returns matter more than originally thought
      # MIDs: Goals/assists more valuable - theory was correct
      # FWDs: Almost entirely attacking
      # Updated Jan 2026 based on regression analysis (modest adjustment toward goals)
      fos_weights <- list(
        GK  = c(cs = 0.80, gf = 0.20),
        DEF = c(cs = 0.50, gf = 0.50),
        MID = c(cs = 0.25, gf = 0.75),
        FWD = c(cs = 0.10, gf = 0.90)
      )
      
      # Remove rows with missing values for the selected metric
      if (plot_type == "salary_vs_fos") {
        # Fantasy Opportunity Score - need both CS and implied goals
        plot_data <- plot_data %>% 
          filter(!is.na(avg_salary), !is.na(clean_sheet_pct), !is.na(implied_team_goals))
        
        if (nrow(plot_data) == 0) return(NULL)
        
        # Normalize implied goals to 0-100 scale (min-max within gameweek)
        gf_min <- min(plot_data$implied_team_goals, na.rm = TRUE)
        gf_max <- max(plot_data$implied_team_goals, na.rm = TRUE)
        gf_range <- gf_max - gf_min
        if (gf_range < 0.01) gf_range <- 1  # Avoid division by zero
        
        plot_data <- plot_data %>%
          mutate(
            gf_normalized = ((implied_team_goals - gf_min) / gf_range) * 100,
            # CS% is already 0-100
            fantasy_opp_score = (clean_sheet_pct * fos_weights[[pos_filter]]["cs"]) + 
              (gf_normalized * fos_weights[[pos_filter]]["gf"])
          )
        
        x_var <- "fantasy_opp_score"
        x_label_format <- function(x) sprintf("%.0f", x)
        
      } else if (plot_type == "salary_vs_cs") {
        plot_data <- plot_data %>% filter(!is.na(avg_salary), !is.na(clean_sheet_pct))
        x_var <- "clean_sheet_pct"
        x_label_format <- function(x) paste0(x, "%")
      } else if (plot_type == "salary_vs_gf") {
        plot_data <- plot_data %>% filter(!is.na(avg_salary), !is.na(implied_team_goals))
        x_var <- "implied_team_goals"
        x_label_format <- function(x) sprintf("%.1f", x)
      } else {
        plot_data <- plot_data %>% filter(!is.na(avg_salary), !is.na(implied_opp_goals))
        x_var <- "implied_opp_goals"
        x_label_format <- function(x) sprintf("%.1f", x)
      }
      
      if (nrow(plot_data) == 0) return(NULL)
      
      # Calculate averages for reference lines
      avg_salary <- mean(plot_data$avg_salary, na.rm = TRUE)
      avg_x <- mean(plot_data[[x_var]], na.rm = TRUE)
      
      # Axis ranges - 10% padding
      x_range <- max(plot_data[[x_var]], na.rm = TRUE) - min(plot_data[[x_var]], na.rm = TRUE)
      y_range <- max(plot_data$avg_salary, na.rm = TRUE) - min(plot_data$avg_salary, na.rm = TRUE)
      
      # Ensure minimum range
      if (x_range < 0.1) x_range <- 1
      if (y_range < 0.1) y_range <- 1
      
      x_min <- min(plot_data[[x_var]], na.rm = TRUE) - (x_range * 0.1)
      x_max <- max(plot_data[[x_var]], na.rm = TRUE) + (x_range * 0.1)
      y_min <- min(plot_data$avg_salary, na.rm = TRUE) - (y_range * 0.1)
      y_max <- max(plot_data$avg_salary, na.rm = TRUE) + (y_range * 0.1)
      
      # Build the scatter plot
      p <- ggplot(plot_data, aes(x = .data[[x_var]], y = avg_salary)) +
        # League average reference lines
        geom_hline(yintercept = avg_salary, linetype = "dashed", 
                   color = APP_COLORS$muted, linewidth = 0.6) +
        geom_vline(xintercept = avg_x, linetype = "dashed", 
                   color = APP_COLORS$muted, linewidth = 0.6) +
        # All points with full opacity
        geom_point(size = 10.5, shape = 19, color = APP_COLORS$primary) +
        # Labels for all teams
        ggrepel::geom_text_repel(
          aes(label = toupper(get_team_abbreviation(team_normalized))),
          size = 5,
          fontface = "bold",
          color = APP_COLORS$primary,
          box.padding = 0.8,
          point.padding = 0.8,
          segment.color = NA,
          min.segment.length = Inf,
          max.overlaps = 20
        ) +
        # Axis setup
        scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0), labels = x_label_format) +
        scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0), 
                           labels = function(y) sprintf("%.1fM", y)) +
        labs(x = NULL, y = NULL) +
        theme_app_scatter()
      
      p
    })
    
    # =========================================================================
    # FILTERED PLAYER DATA
    # =========================================================================
    
    filtered_data <- reactive({
      req(rv$salary_data)
      data <- rv$salary_data
      
      # Status filter
      if (!is.null(input$status) && length(input$status) > 0 && "status" %in% names(data)) {
        data <- data %>% filter(is.na(status) | status == "" | tolower(status) %in% input$status)
      }
      
      # Position filter
      if (!is.null(input$position) && input$position != "all") {
        data <- data %>% filter(position == input$position)
      }
      
      # Team filter
      if (!is.null(input$team) && input$team != "all") {
        data <- data %>% filter(team_normalized == input$team)
      }
      
      # Sort
      sort_by <- input$sort_by
      if (!is.null(sort_by)) {
        data <- switch(sort_by,
                       "salary_desc" = data %>% arrange(desc(salary)),
                       "salary_asc" = data %>% arrange(salary),
                       "points_desc" = data %>% arrange(desc(total_points)),
                       "avg_desc" = data %>% arrange(desc(average_points)),
                       "player" = data %>% arrange(player),
                       data)
      }
      
      return(data)
    })
    
    # =========================================================================
    # PLAYER SALARY TABLE
    # =========================================================================
    
    output$salary_table <- renderUI({
      data <- filtered_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(div(style = "padding: 2rem; text-align: center;", "No players found"))
      }
      
      has_stats <- "total_points" %in% names(data) && !all(is.na(data$total_points))
      display_data <- data
      
      col_defs <- list()
      
      # Player column
      col_defs$player <- reactable::colDef(
        name = "Player",
        width = 280,
        cell = function(value, index) {
          pos <- display_data$position[index]
          status <- display_data$status[index]
          team <- display_data$team_normalized[index]
          opp <- if ("opponent" %in% names(display_data)) display_data$opponent[index] else NA
          ha <- if ("home_away" %in% names(display_data)) display_data$home_away[index] else NA
          
          # Status badge
          status_badge <- NULL
          if (!is.null(status) && !is.na(status) && status != "") {
            status_lower <- tolower(status)
            badge_style <- switch(status_lower,
                                  "expected" = "background-color: #A3BE8C; color: white;",
                                  "possible" = "background-color: #EBCB8B; color: #2E3440;",
                                  "unexpected" = "background-color: #B48EAD; color: white;",
                                  "injured" = "background-color: #BF616A; color: white;",
                                  "suspended" = "background-color: #D08770; color: white;",
                                  "background-color: #4C566A; color: white;")
            status_badge <- htmltools::tags$span(
              style = sprintf("padding: 2px 6px; border-radius: 4px; font-size: 0.65rem; font-weight: 600; margin-left: 8px; %s", badge_style),
              toupper(substr(status_lower, 1, 3))
            )
          }
          
          # Logo
          logo_html <- NULL
          logo_path <- get_soccer_team_logo(team)
          if (!is.null(logo_path) && logo_path != "") {
            logo_html <- htmltools::tags$img(src = logo_path, style = "width: 16px; height: 16px; object-fit: contain; vertical-align: middle;")
          }
          
          # Matchup
          matchup_html <- NULL
          if (!is.na(opp) && opp != "") {
            team_abbr <- get_team_abbreviation(team)
            opp_abbr <- get_team_abbreviation(opp)
            if (!is.na(ha) && toupper(ha) == "H") {
              matchup_html <- htmltools::tagList(
                htmltools::tags$span(style = "font-weight: 700;", team_abbr),
                " v ", htmltools::tags$span(opp_abbr)
              )
            } else {
              matchup_html <- htmltools::tagList(
                htmltools::tags$span(opp_abbr), " v ",
                htmltools::tags$span(style = "font-weight: 700;", team_abbr)
              )
            }
          }
          
          pos_text <- if (!is.na(pos) && pos != "") pos else ""
          
          htmltools::tags$div(
            htmltools::tags$div(style = "font-weight: 600;", value, status_badge),
            htmltools::tags$div(
              style = "margin-top: 3px; font-size: 0.75rem; color: #9ca3af; display: flex; align-items: center; gap: 6px;",
              logo_html,
              if (!is.null(logo_html) && pos_text != "") htmltools::tags$span("Ã‚Â·"),
              if (pos_text != "") htmltools::tags$span(pos_text),
              if (pos_text != "" && !is.null(matchup_html)) htmltools::tags$span("Ã‚Â·"),
              matchup_html
            )
          )
        }
      )
      
      # Hide columns shown in player cell
      col_defs$team_normalized <- reactable::colDef(show = FALSE)
      col_defs$position <- reactable::colDef(show = FALSE)
      col_defs$status <- reactable::colDef(show = FALSE)
      col_defs$logo_path <- reactable::colDef(show = FALSE)
      col_defs$opponent <- reactable::colDef(show = FALSE)
      col_defs$home_away <- reactable::colDef(show = FALSE)
      
      # Salary
      col_defs$salary <- reactable::colDef(
        name = "Salary",
        width = 80,
        align = "center",
        cell = function(value) if (is.na(value)) "-" else sprintf("%.1fM", value)
      )
      
      # Odds columns
      col_defs$implied_team_goals <- reactable::colDef(name = "TmG", width = 55, align = "center",
                                                       cell = function(value) if (is.na(value)) "-" else sprintf("%.2f", value))
      col_defs$implied_opp_goals <- reactable::colDef(name = "OpG", width = 55, align = "center",
                                                      cell = function(value) if (is.na(value)) "-" else sprintf("%.2f", value))
      col_defs$clean_sheet_pct <- reactable::colDef(name = "CS%", width = 50, align = "center",
                                                    cell = function(value) if (is.na(value)) "-" else sprintf("%.0f%%", value))
      
      # Stats columns
      if (has_stats) {
        if ("total_points" %in% names(data)) {
          col_defs$total_points <- reactable::colDef(name = "Pts", width = 55, align = "center",
                                                     cell = function(value) if (is.na(value)) "-" else as.character(round(value)))
        }
        if ("average_points" %in% names(data)) {
          col_defs$average_points <- reactable::colDef(name = "Avg", width = 55, align = "center",
                                                       cell = function(value) if (is.na(value)) "-" else sprintf("%.1f", value))
        }
      }
      
      # Hide other columns
      hidden_cols <- c("team", "player_id", "club_abbrev", "gameweek", "fanteam_name_normalized",
                       "fanteam_club", "fbref_name", "fbref_team", "win_pct", "implied_total",
                       "goals", "sot", "assists", "mins_60", "mins_90", "clean_sheets", 
                       "saves", "goals_conceded", "yellows", "reds", "match_status")
      for (col in hidden_cols) {
        if (col %in% names(display_data)) {
          col_defs[[col]] <- reactable::colDef(show = FALSE)
        }
      }
      
      reactable::reactable(
        display_data,
        searchable = TRUE,
        sortable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        fullWidth = TRUE,
        theme = app_reactable_theme(),
        columns = col_defs,
        defaultColDef = reactable::colDef(vAlign = "center", headerVAlign = "center")
      )
    })
    
  })
}