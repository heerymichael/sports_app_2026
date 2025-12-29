# =============================================================================
# Module: Soccer FanTeam Contests
# 
# Player salary tables for FanTeam Monster contests
# Styled similar to NFL projections module
# Integrates with FanTeam stats_overview data
# =============================================================================

# Soccer card color
SOCCER_CARD_COLOR <- APP_COLORS$sage

# FanTeam Stats Google Sheet ID
FANTEAM_STATS_SHEET_ID <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"

# Cache for stats overview data
FANTEAM_STATS_CACHE <- new.env()

#' Load FanTeam stats overview data from Google Sheet
#' @param force_refresh Force reload from Google Sheets (ignore cache)
#' @return Data frame with player stats
load_fanteam_stats_overview <- function(force_refresh = FALSE) {
  cache_key <- "stats_overview"
  cache_time_key <- "stats_overview_time"
  
  # Check cache (valid for 10 minutes)
  if (!force_refresh && 
      exists(cache_key, envir = FANTEAM_STATS_CACHE) &&
      exists(cache_time_key, envir = FANTEAM_STATS_CACHE)) {
    cache_age <- difftime(Sys.time(), get(cache_time_key, envir = FANTEAM_STATS_CACHE), units = "mins")
    if (cache_age < 10) {
      log_debug("Using cached stats_overview data", level = "DEBUG")
      return(get(cache_key, envir = FANTEAM_STATS_CACHE))
    }
  }
  
  log_debug("Loading stats_overview from Google Sheet...", level = "INFO")
  
  stats <- tryCatch({
    raw_data <- read_sheet(
      FANTEAM_STATS_SHEET_ID,
      sheet = "stats_overview"
    ) %>%
      clean_names()
    
    log_debug("stats_overview columns:", paste(names(raw_data), collapse = ", "), level = "DEBUG")
    
    # Standardize column names (handle various naming conventions)
    # Map FROM sheet column names TO standard names
    col_renames <- c(
      # Player identifier
      "name" = "player",
      # Points - your sheet uses total_pts and avg_pts
      "total_pts" = "total_points",
      "pts" = "total_points",
      "points" = "total_points",
      "avg_pts" = "average_points",
      "avg" = "average_points",
      "ppg" = "average_points",
      # Attack stats
      "g" = "goals",
      "shots_on_target" = "sot",
      "a" = "assists",
      # Playing time - your sheet uses x60_mp and x90_mp
      "x60_mp" = "mins_60",
      "x60_mins" = "mins_60",
      "x15_mp" = "mins_15",
      "x1_mp" = "mins_1",
      "x90_mp" = "mins_90",
      "x90_mins" = "mins_90",
      # Defensive stats - your sheet uses cs and gc
      "cs" = "clean_sheets",
      "sv" = "saves",
      "gc" = "goals_conceded",
      # Cards - your sheet uses yel and red
      "yel" = "yellows",
      "yellow_cards" = "yellows",
      "yc" = "yellows",
      "red" = "reds",
      "red_cards" = "reds",
      "rc" = "reds"
    )
    
    # Apply column renaming - rename columns that exist
    for (old_name in names(col_renames)) {
      new_name <- col_renames[old_name]
      if (old_name %in% names(raw_data) && !new_name %in% names(raw_data)) {
        names(raw_data)[names(raw_data) == old_name] <- new_name
        log_debug(sprintf("Renamed column: %s -> %s", old_name, new_name), level = "DEBUG")
      }
    }
    
    # Convert numeric columns (only if they exist)
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
    # Cache the result
    assign(cache_key, stats, envir = FANTEAM_STATS_CACHE)
    assign(cache_time_key, Sys.time(), envir = FANTEAM_STATS_CACHE)
    log_debug("Loaded", nrow(stats), "player stats from stats_overview", level = "INFO")
    log_debug("Final columns:", paste(names(stats), collapse = ", "), level = "DEBUG")
  }
  
  return(stats)
}

#' Merge FanTeam salary data with stats overview
#' @param salary_data FanTeam salary data (contains weekly salary - DO NOT OVERWRITE)
#' @param stats_data Stats overview data
#' @return Merged data frame with stats added but salary preserved from salary_data
merge_fanteam_with_stats <- function(salary_data, stats_data) {
  if (is.null(salary_data) || nrow(salary_data) == 0) return(salary_data)
  if (is.null(stats_data) || nrow(stats_data) == 0) {
    log_debug("No stats data to merge", level = "WARN")
    return(salary_data)
  }
  
  log_debug("Merging salary data with stats...", level = "DEBUG")
  log_debug("Stats data columns:", paste(names(stats_data), collapse = ", "), level = "DEBUG")
  
  # Check if player column exists in stats data
  if (!"player" %in% names(stats_data)) {
    log_debug("Stats data missing 'player' column, skipping merge", level = "WARN")
    return(salary_data)
  }
  
  # Remove any salary-related columns from stats_data BEFORE any operations
  cols_to_exclude <- c("salary", "price", "cost", "value", "weekly_salary")
  stats_cols_to_drop <- intersect(cols_to_exclude, names(stats_data))
  if (length(stats_cols_to_drop) > 0) {
    stats_data <- stats_data %>% select(-all_of(stats_cols_to_drop))
    log_debug("Dropped salary columns from stats:", paste(stats_cols_to_drop, collapse = ", "), level = "DEBUG")
  }
  
  # Local normalize function - simple version that works without external dependencies
  local_normalize <- function(x) {
    if (is.null(x) || length(x) == 0) return(x)
    x <- tolower(trimws(as.character(x)))
    # Remove accents/diacritics if stringi is available
    if (requireNamespace("stringi", quietly = TRUE)) {
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    }
    # Standardize spacing
    x <- gsub("\\s+", " ", x)
    x
  }
  
  # Normalize player names for matching
  salary_data <- salary_data %>%
    mutate(player_norm = local_normalize(player))
  
  stats_data <- stats_data %>%
    mutate(player_norm = local_normalize(player))
  
  log_debug("Normalized player names for matching", level = "DEBUG")
  
  # Identify which stat columns exist in stats_data
  stat_cols <- c("total_points", "average_points", "goals", "sot", "assists",
                 "mins_60", "mins_90", "clean_sheets", "saves", "goals_conceded", 
                 "yellows", "reds")
  available_stat_cols <- intersect(stat_cols, names(stats_data))
  
  if (length(available_stat_cols) == 0) {
    log_debug("No recognized stat columns found in stats_data", level = "WARN")
    log_debug("Stats columns available:", paste(names(stats_data), collapse = ", "), level = "DEBUG")
    # Return salary_data without player_norm column
    return(salary_data %>% select(-player_norm))
  }
  
  log_debug("Available stat columns for merge:", paste(available_stat_cols, collapse = ", "), level = "INFO")
  log_debug("Preserving weekly salary from salary_data (not from stats)", level = "DEBUG")
  
  # Left join to keep all salary players - ONLY bring in stat columns, not salary
  merged <- tryCatch({
    salary_data %>%
      left_join(
        stats_data %>% 
          select(player_norm, all_of(available_stat_cols)),
        by = "player_norm"
      ) %>%
      select(-player_norm)
  }, error = function(e) {
    log_debug("Error during merge:", e$message, level = "ERROR")
    salary_data %>% select(-player_norm)
  })
  
  # Count how many were matched (use first available stat column)
  if (length(available_stat_cols) > 0 && available_stat_cols[1] %in% names(merged)) {
    first_stat_col <- available_stat_cols[1]
    matched_count <- sum(!is.na(merged[[first_stat_col]]))
    log_debug(sprintf("Stats matched: %d/%d players (%.1f%%)", 
                      matched_count, nrow(merged), 100 * matched_count / nrow(merged)), level = "INFO")
  }
  
  return(merged)
}

#' Soccer FanTeam Contests UI
#' @param id Module namespace ID
soccer_fanteam_contests_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_fanteam_contests_ui() called with id:", id, level = "INFO")
  
  # Get available gameweeks at UI build time
  gameweeks <- get_fanteam_soccer_gameweeks()
  log_debug("UI build - gameweeks available:", paste(gameweeks, collapse = ", "), level = "INFO")
  
  # Build gameweek choices
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
      tags$p(class = "text-muted", "Player salaries and stats for FanTeam Monster soccer contests")
    ),
    
    # Filters card
    ui_card(
      title = "Filters",
      color = SOCCER_CARD_COLOR,
      
      # Row 1: Gameweek, Position, Team
      fluidRow(
        column(4,
               selectInput(ns("gameweek"), "Gameweek",
                           choices = gw_choices,
                           selected = gw_selected
               )
        ),
        column(4,
               selectInput(ns("position"), "Position",
                           choices = c("All Positions" = "all", "GK", "DEF", "MID", "FWD"),
                           selected = "all"
               )
        ),
        column(4,
               shinyWidgets::pickerInput(ns("team"), "Team",
                                         choices = c("All Teams" = "all"),
                                         selected = "all",
                                         options = shinyWidgets::pickerOptions(
                                           liveSearch = TRUE,
                                           size = 10
                                         )
               )
        )
      ),
      
      # Row 2: Status, Sort, Heatmap
      fluidRow(
        column(4,
               shinyWidgets::pickerInput(ns("status"), "Availability",
                                         choices = c("Expected" = "expected",
                                                     "Possible" = "possible",
                                                     "Unexpected" = "unexpected",
                                                     "Injured" = "injured",
                                                     "Suspended" = "suspended"),
                                         selected = c("expected", "possible", "unexpected", "injured", "suspended"),
                                         multiple = TRUE,
                                         options = shinyWidgets::pickerOptions(
                                           actionsBox = TRUE,
                                           selectedTextFormat = "count > 2",
                                           countSelectedText = "{0} statuses"
                                         )
               )
        ),
        column(4,
               selectInput(ns("sort_by"), "Sort By",
                           choices = c("Salary (High to Low)" = "salary_desc",
                                       "Salary (Low to High)" = "salary_asc",
                                       "Total Points" = "points_desc",
                                       "Avg Points" = "avg_desc",
                                       "Player Name" = "player",
                                       "Position" = "position"),
                           selected = "salary_desc"
               )
        ),
        column(4,
               selectInput(ns("heatmap"), "Heatmap",
                           choices = c("None" = "none", 
                                       "Salary" = "salary",
                                       "Total Points" = "total_points",
                                       "Avg Points" = "average_points"),
                           selected = "none"
               )
        )
      ),
      
      # Row 3: Salary Cap
      fluidRow(
        column(4,
               numericInput(ns("salary_cap"), "Salary Cap (€M)",
                            value = 118,
                            min = 50,
                            max = 200,
                            step = 1
               )
        ),
        column(8,
               div(
                 style = "padding-top: 25px;",
                 uiOutput(ns("budget_display"))
               )
        )
      )
    ),
    
    tags$br(),
    
    # Unmatched players alert
    uiOutput(ns("unmatched_alert")),
    
    # Salary table
    uiOutput(ns("salary_table"))
  )
}

#' Soccer FanTeam Contests Server
#' @param id Module namespace ID
soccer_fanteam_contests_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_fanteam_contests_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      salary_data = NULL,
      stats_data = NULL,
      loading = FALSE,
      initialized = FALSE
    )
    
    # =========================================================================
    # DATA LOAD - triggered by gameweek change
    # =========================================================================
    
    load_attempts <- reactiveVal(0)
    
    observe({
      gameweek <- input$gameweek
      
      log_debug(">>> Data load observer triggered (attempt", load_attempts() + 1, ")", level = "DEBUG")
      log_debug(">>>   gameweek:", if(is.null(gameweek)) "NULL" else if(gameweek == "") "''" else paste0("'", gameweek, "'"), level = "DEBUG")
      
      # If inputs aren't ready, retry
      if ((is.null(gameweek) || gameweek == "") && load_attempts() < 10) {
        load_attempts(load_attempts() + 1)
        invalidateLater(200, session)
        return()
      }
      
      if (is.null(gameweek) || gameweek == "") {
        log_debug(">>> No valid gameweek after retries", level = "WARN")
        return()
      }
      
      rv$loading <- TRUE
      log_debug(">>> Loading data for gameweek:", gameweek, level = "INFO")
      
      # Load salary data
      data <- load_fanteam_soccer_with_logos(as.integer(gameweek))
      
      if (!is.null(data) && nrow(data) > 0) {
        # Run player matching against FBref data
        tryCatch({
          # Get FBref player list from cached data
          shot_data <- get_cached_data("shots")
          player_data <- get_cached_data("player_stats")
          
          if ((!is.null(shot_data) && nrow(shot_data) > 0) || 
              (!is.null(player_data) && nrow(player_data) > 0)) {
            fbref_players <- get_fbref_player_list(shot_data, player_data)
            
            if (nrow(fbref_players) > 0) {
              data <- match_fanteam_to_fbref(
                data, 
                fbref_players, 
                gameweek = as.integer(gameweek),
                write_unmatched = TRUE
              )
              log_debug("Player matching complete", level = "INFO")
            }
          }
        }, error = function(e) {
          log_debug("Player matching skipped:", e$message, level = "WARN")
        })
        
        # Load and merge stats overview data
        tryCatch({
          log_debug("Loading stats overview data...", level = "INFO")
          stats_data <- load_fanteam_stats_overview()
          
          if (!is.null(stats_data) && nrow(stats_data) > 0) {
            log_debug(sprintf("Stats loaded: %d rows", nrow(stats_data)), level = "INFO")
            data <- merge_fanteam_with_stats(data, stats_data)
            rv$stats_data <- stats_data
          } else {
            log_debug("No stats data loaded", level = "WARN")
          }
        }, error = function(e) {
          log_debug("Stats merge error:", e$message, level = "ERROR")
        })
        
        rv$salary_data <- data
        rv$initialized <- TRUE
        log_debug(">>> Data loaded:", nrow(data), "players", level = "INFO")
        
        # Update team dropdown with logos
        teams <- sort(unique(data$team_normalized[!is.na(data$team_normalized)]))
        
        if (length(teams) > 0) {
          # Build HTML content for each team (logo + name)
          team_content <- c(
            "All Teams",  # No logo for "All Teams"
            sapply(teams, function(team) {
              logo_path <- get_soccer_team_logo(team)
              if (!is.null(logo_path)) {
                sprintf('<img src="%s" style="width:20px; height:20px; margin-right:8px; vertical-align:middle; object-fit:contain;"> %s', 
                        logo_path, team)
              } else {
                team
              }
            }, USE.NAMES = FALSE)
          )
          
          team_choices <- c("all", teams)
          names(team_choices) <- c("All Teams", teams)
          
          shinyWidgets::updatePickerInput(session, "team",
                                          choices = team_choices,
                                          selected = "all",
                                          choicesOpt = list(content = team_content)
          )
        }
        
      } else {
        log_debug(">>> No data found for gameweek:", gameweek, level = "WARN")
        rv$salary_data <- NULL
      }
      
      rv$loading <- FALSE
    })
    
    # =========================================================================
    # BUDGET DISPLAY
    # =========================================================================
    
    output$budget_display <- renderUI({
      cap <- input$salary_cap
      if (is.null(cap)) cap <- 118
      
      div(
        style = "display: flex; gap: 20px; align-items: center;",
        div(
          style = "font-size: 0.9rem; color: var(--text-muted);",
          HTML(sprintf("Budget: <strong style='color: var(--text-primary);'>€%.0fM</strong>", cap))
        ),
        div(
          style = "font-size: 0.85rem; color: var(--text-muted);",
          "11 players: 1 GK, 3 DEF, 3 MID, 1 FWD + 2 FLEX (DEF/MID/FWD)"
        )
      )
    })
    
    # =========================================================================
    # UNMATCHED PLAYERS ALERT
    # =========================================================================
    
    output$unmatched_alert <- renderUI({
      # Check for unmatched players
      unmatched_count <- tryCatch({
        get_unmatched_count()
      }, error = function(e) 0)
      
      if (unmatched_count == 0) return(NULL)
      
      div(
        style = "background-color: #EBCB8B; color: #2E3440; padding: 12px 16px; border-radius: 8px; margin-bottom: 16px; display: flex; align-items: center; justify-content: space-between;",
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          tags$span(style = "font-size: 1.2rem;", "⚠️"),
          tags$span(
            style = "font-weight: 600;",
            sprintf("%d unmatched player%s", unmatched_count, if(unmatched_count == 1) "" else "s")
          ),
          tags$span(
            style = "font-size: 0.9rem;",
            "— Add corrections in Google Sheet"
          )
        ),
        tags$a(
          href = sprintf("https://docs.google.com/spreadsheets/d/%s/edit#gid=707513073", FANTEAM_MAPPING_SHEET_ID),
          target = "_blank",
          style = "background-color: #2E3440; color: white; padding: 6px 12px; border-radius: 4px; text-decoration: none; font-size: 0.85rem; font-weight: 600;",
          "Open Sheet"
        )
      )
    })
    
    # =========================================================================
    # FILTERED DATA
    # =========================================================================
    
    filtered_data <- reactive({
      req(rv$salary_data)
      
      data <- rv$salary_data
      
      # Position filter
      if (!is.null(input$position) && input$position != "all") {
        data <- data %>% filter(position == input$position)
      }
      
      # Team filter
      if (!is.null(input$team) && input$team != "all") {
        data <- data %>% filter(team_normalized == input$team)
      }
      
      # Status filter (multi-select) - include players with NA status if any filter is selected
      if (!is.null(input$status) && length(input$status) > 0 && "status" %in% names(data)) {
        # Filter to only include selected statuses (or NA/missing status)
        data <- data %>% filter(
          is.na(status) | status == "" | tolower(status) %in% input$status
        )
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
                       "position" = data %>% arrange(match(position, c("GK", "DEF", "MID", "FWD")), desc(salary)),
                       data
        )
      }
      
      log_debug("Filtered data:", nrow(data), "players", level = "DEBUG")
      return(data)
    })
    
    # =========================================================================
    # SALARY TABLE RENDER - Using reactable for consistency
    # =========================================================================
    
    output$salary_table <- renderUI({
      # Show loading state
      if (rv$loading) {
        return(
          ui_card(
            title = "Loading...",
            color = SOCCER_CARD_COLOR,
            div(
              style = "padding: 2rem; text-align: center;",
              tags$p(class = "text-muted", "Loading salary data...")
            )
          )
        )
      }
      
      # Check for data
      data <- filtered_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          ui_card(
            title = "No Data",
            color = SOCCER_CARD_COLOR,
            div(
              style = "padding: 2rem; text-align: center;",
              tags$p(class = "text-muted", "No salary data available for this selection."),
              tags$p(
                class = "text-muted",
                style = "font-size: 0.85rem;",
                "Add CSV files to: data/fanteam_soccer/fanteam_monster_salaries/"
              )
            )
          )
        )
      }
      
      # Check if we have stats data
      has_stats <- "total_points" %in% names(data) && !all(is.na(data$total_points))
      
      # Heatmap setting
      heatmap_col <- input$heatmap
      
      # Prepare display data
      display_data <- data
      
      # Build column definitions for reactable
      col_defs <- list()
      
      # Standard stat column width
      STAT_COL_WIDTH <- 70
      
      # Team logo column - first column (use team_normalized value directly with get_soccer_team_logo)
      col_defs$team_normalized <- reactable::colDef(
        name = "",
        width = 60,
        cell = function(value, index) {
          # Get logo path using the same function as dropdown
          logo <- get_soccer_team_logo(value)
          if (!is.null(logo) && logo != "") {
            htmltools::tags$img(
              src = logo, 
              style = "width: 28px; height: 28px; object-fit: contain;",
              onerror = "this.style.display='none'"
            )
          } else {
            # Show team abbreviation as fallback
            if (!is.null(value) && !is.na(value)) {
              htmltools::tags$span(
                style = "font-size: 0.7rem; color: #6b7280; font-weight: 500;",
                substr(value, 1, 3)
              )
            } else {
              ""
            }
          }
        }
      )
      
      # Player column with position and status on second line
      col_defs$player <- reactable::colDef(
        name = "Player", 
        width = 220,
        cell = function(value, index) {
          pos <- display_data$position[index]
          status <- display_data$status[index]
          
          # Position text
          pos_text <- if (!is.na(pos) && pos != "") pos else ""
          
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
                                  "refuted" = "background-color: #4C566A; color: white;",
                                  "background-color: #4C566A; color: white;"
            )
            status_badge <- htmltools::tags$span(
              style = sprintf("padding: 2px 6px; border-radius: 4px; font-size: 0.65rem; font-weight: 600; margin-left: 8px; %s", badge_style),
              toupper(substr(status_lower, 1, 3))
            )
          }
          
          htmltools::tags$div(
            htmltools::tags$div(style = "font-weight: 600;", value),
            htmltools::tags$div(
              style = "margin-top: 2px;",
              htmltools::tags$span(style = "color: #9ca3af; font-size: 0.75rem; font-weight: 500;", pos_text),
              status_badge
            )
          )
        }
      )
      
      # Hide position column (shown in player cell)
      col_defs$position <- reactable::colDef(show = FALSE)
      
      # Hide logo_path
      col_defs$logo_path <- reactable::colDef(show = FALSE)
      
      # Status badge - hidden (now shown in player column)
      col_defs$status <- reactable::colDef(show = FALSE)
      
      # Salary column with heatmap
      col_defs$salary <- reactable::colDef(
        name = "Salary",
        width = 100,
        align = "center",
        cell = function(value) {
          if (is.na(value)) return("-")
          sprintf("€%.1fM", value)
        },
        style = function(value) {
          if (heatmap_col != "salary" || is.na(value)) return(list())
          get_sequential_heatmap_style(value, min(data$salary, na.rm = TRUE), max(data$salary, na.rm = TRUE))
        }
      )
      
      # Stats columns (only show columns that exist in the data)
      if (has_stats) {
        # Total Points
        if ("total_points" %in% names(data)) {
          col_defs$total_points <- reactable::colDef(
            name = "Pts",
            width = 70,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(round(value)),
            style = function(value) {
              if (heatmap_col != "total_points" || is.na(value)) return(list())
              get_sequential_heatmap_style(value, min(data$total_points, na.rm = TRUE), max(data$total_points, na.rm = TRUE))
            }
          )
        }
        
        # Average Points
        if ("average_points" %in% names(data)) {
          col_defs$average_points <- reactable::colDef(
            name = "Avg",
            width = 70,
            align = "center",
            format = reactable::colFormat(digits = 1),
            cell = function(value) if (is.na(value)) "-" else sprintf("%.1f", value),
            style = function(value) {
              if (heatmap_col != "average_points" || is.na(value)) return(list())
              get_sequential_heatmap_style(value, min(data$average_points, na.rm = TRUE), max(data$average_points, na.rm = TRUE))
            }
          )
        }
        
        # Goals
        if ("goals" %in% names(data)) {
          col_defs$goals <- reactable::colDef(
            name = "G",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # SOT (Shots on Target)
        if ("sot" %in% names(data)) {
          col_defs$sot <- reactable::colDef(
            name = "SOT",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # Assists
        if ("assists" %in% names(data)) {
          col_defs$assists <- reactable::colDef(
            name = "A",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # 60+ minutes
        if ("mins_60" %in% names(data)) {
          col_defs$mins_60 <- reactable::colDef(
            name = "60+",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # 90 minutes
        if ("mins_90" %in% names(data)) {
          col_defs$mins_90 <- reactable::colDef(
            name = "90",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # Clean Sheets
        if ("clean_sheets" %in% names(data)) {
          col_defs$clean_sheets <- reactable::colDef(
            name = "CS",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # Saves
        if ("saves" %in% names(data)) {
          col_defs$saves <- reactable::colDef(
            name = "Sv",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # Goals Conceded
        if ("goals_conceded" %in% names(data)) {
          col_defs$goals_conceded <- reactable::colDef(
            name = "GC",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # Yellows
        if ("yellows" %in% names(data)) {
          col_defs$yellows <- reactable::colDef(
            name = "YC",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
        
        # Reds
        if ("reds" %in% names(data)) {
          col_defs$reds <- reactable::colDef(
            name = "RC",
            width = STAT_COL_WIDTH,
            align = "center",
            cell = function(value) if (is.na(value)) "-" else as.character(as.integer(value))
          )
        }
      }
      
      # Match status indicator
      if ("match_status" %in% names(data)) {
        col_defs$match_status <- reactable::colDef(
          name = "",
          width = 40,
          align = "center",
          cell = function(value, index) {
            if (is.na(value) || value == "unmatched") {
              htmltools::tags$span(title = "Unmatched - no FBref data", style = "color: #BF616A; cursor: help;", "✗")
            } else {
              fbref_name <- display_data$fbref_name[index]
              htmltools::tags$span(title = paste("Matched:", fbref_name), style = "color: #A3BE8C; cursor: help;", "✓")
            }
          }
        )
      }
      
      # Hide other columns we don't want to display
      hidden_cols <- c("team", "player_id", "club_abbrev", "gameweek", "fanteam_name_normalized",
                       "fanteam_club", "fbref_name", "fbref_team")
      for (col in hidden_cols) {
        if (col %in% names(display_data)) {
          col_defs[[col]] <- reactable::colDef(show = FALSE)
        }
      }
      
      # Select and order columns for display
      display_cols <- c("team_normalized", "player", "position", "logo_path", "status", "salary")
      
      # Add stats columns that exist
      if (has_stats) {
        stat_cols_ordered <- c("total_points", "average_points", "goals", "sot", "assists",
                               "mins_60", "mins_90", "clean_sheets", "saves", "goals_conceded", 
                               "yellows", "reds")
        available_stats <- intersect(stat_cols_ordered, names(data))
        display_cols <- c(display_cols, available_stats)
      }
      
      if ("match_status" %in% names(data)) {
        display_cols <- c(display_cols, "match_status")
      }
      
      # Add any remaining columns that exist in data but not in display_cols
      remaining_cols <- setdiff(names(display_data), display_cols)
      display_cols <- c(display_cols, remaining_cols)
      
      # Only select columns that exist
      display_cols <- intersect(display_cols, names(display_data))
      display_data <- display_data[, display_cols, drop = FALSE]
      
      # Build the table
      table <- reactable::reactable(
        display_data,
        searchable = TRUE,
        sortable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        theme = app_reactable_theme(),
        columns = col_defs,
        defaultColDef = reactable::colDef(
          vAlign = "center",
          headerVAlign = "center"
        )
      )
      
      # Build legend dynamically based on available columns
      legend_mapping <- list(
        total_points = "Pts = Total Points",
        average_points = "Avg = Avg Points/Game",
        goals = "G = Goals",
        sot = "SOT = Shots on Target",
        assists = "A = Assists",
        mins_60 = "60+ = Games ≥60 mins",
        mins_90 = "90 = Full games",
        clean_sheets = "CS = Clean Sheets",
        saves = "Sv = Saves",
        goals_conceded = "GC = Goals Conceded",
        yellows = "YC = Yellow Cards",
        reds = "RC = Red Cards"
      )
      
      legend <- if (has_stats) {
        available_legend_items <- legend_mapping[names(legend_mapping) %in% names(data)]
        
        if (length(available_legend_items) > 0) {
          div(
            style = "margin-top: 0.75rem; padding: 0.5rem 0.75rem; background: var(--bg-tertiary); border-radius: 6px; font-size: 0.75rem; color: var(--text-muted);",
            tags$span(style = "font-weight: 600; margin-right: 0.5rem;", "Legend:"),
            lapply(seq_along(available_legend_items), function(i) {
              tagList(
                tags$span(available_legend_items[[i]]),
                if (i < length(available_legend_items)) tags$span(style = "margin: 0 0.4rem;", "·") else NULL
              )
            })
          )
        } else {
          NULL
        }
      } else {
        NULL
      }
      
      # Build card
      ui_card(
        title = sprintf("Player Salaries (%d players)", nrow(data)),
        color = SOCCER_CARD_COLOR,
        tagList(table, legend)
      )
    })
    
  })
}