# =============================================================================
# Module: Soccer Betting
# 
# Displays upcoming matches with:
#   - League toggle buttons with logos
#   - Day toggle buttons (like old app)
#   - Team vs Opponent with logos
#   - PPG Differential (Per Game or Total basis)
#   - Win Odds
#   - Optional diverging heatmap per column
#
# Uses existing normalize_team_names() from soccer_config.R
# =============================================================================

# =============================================================================
# UI
# =============================================================================

soccer_betting_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "page-content",
    
    # Filters Card
    ui_card(
      title = "Betting Filters",
      color = "sage",
      
      # League selector - toggle buttons with logos
      div(
        style = "margin-bottom: var(--space-lg);",
        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: var(--space-sm);",
          tags$label("Select Leagues", class = "control-label", 
                     style = "font-weight: 700; font-size: 0.85rem; text-transform: uppercase; letter-spacing: 0.5px;"),
          div(
            style = "display: flex; gap: 10px; align-items: center;",
            actionButton(
              ns("refresh_data"),
              label = tagList(icon("rotate"), "Refresh"),
              class = "btn-refresh-subtle"
            ),
            uiOutput(ns("data_freshness"))
          )
        ),
        uiOutput(ns("league_buttons"))
      ),
      
      # Day selector - toggle buttons
      div(
        style = "margin-bottom: var(--space-lg);",
        tags$label("Select Days", class = "control-label",
                   style = "font-weight: 700; font-size: 0.85rem; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: var(--space-sm); display: block;"),
        uiOutput(ns("day_buttons"))
      ),
      
      # Bottom row: Points display, heatmap, and diff filter dropdowns
      div(
        style = "display: flex; gap: var(--space-lg); align-items: flex-end; flex-wrap: wrap;",
        div(
          class = "form-group",
          style = "min-width: 140px;",
          tags$label("Points Display", class = "control-label",
                     style = "font-weight: 600; font-size: 0.8rem;"),
          selectizeInput(
            ns("ppg_mode"),
            label = NULL,
            choices = c("Total Points" = "total", "Per Game" = "per_game"),
            selected = "total",
            multiple = FALSE,
            width = "100%"
          )
        ),
        div(
          class = "form-group",
          style = "min-width: 140px;",
          tags$label("Heatmap Column", class = "control-label",
                     style = "font-weight: 600; font-size: 0.8rem;"),
          selectizeInput(
            ns("heatmap_col"),
            label = NULL,
            choices = c("None" = "none", 
                        "Season Diff" = "season_diff",
                        "Last 13 Diff" = "last13_diff", 
                        "Last 6 Diff" = "last6_diff",
                        "Win Odds" = "win_odds"),
            selected = "none",
            multiple = FALSE,
            width = "100%"
          )
        ),
        # Diff filter - elegant paired dropdowns
        div(
          style = "display: flex; gap: var(--space-sm); align-items: flex-end; border-left: 1px solid var(--border); padding-left: var(--space-lg); margin-left: var(--space-sm);",
          div(
            class = "form-group",
            style = "min-width: 120px;",
            tags$label("Filter by Diff", class = "control-label",
                       style = "font-weight: 600; font-size: 0.8rem;"),
            selectizeInput(
              ns("diff_filter_col"),
              label = NULL,
              choices = c("No Filter" = "none",
                          "Season" = "season_diff",
                          "Last 13" = "last13_diff",
                          "Last 6" = "last6_diff"),
              selected = "none",
              multiple = FALSE,
              width = "100%"
            )
          ),
          div(
            class = "form-group",
            style = "min-width: 110px;",
            tags$label("Show", class = "control-label",
                       style = "font-weight: 600; font-size: 0.8rem;"),
            selectizeInput(
              ns("diff_filter_dir"),
              label = NULL,
              choices = c("All" = "all",
                          "Positive +" = "positive",
                          "Negative âˆ’" = "negative"),
              selected = "all",
              multiple = FALSE,
              width = "100%"
            )
          )
        )
      )
    ),
    
    # Main Table Card
    ui_card(
      title = "Upcoming Matches",
      color = "teal",
      
      reactableOutput(ns("betting_table"))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

soccer_betting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================")
    log_debug("Soccer Betting Module: Initializing")
    log_debug("========================================")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    rv <- reactiveValues(
      odds_data = NULL,
      standings_data = NULL,
      last_updated = NULL,
      loading = FALSE,
      load_attempted = FALSE,  # Prevents infinite retry loop on error
      selected_leagues = BETTING_DEFAULT_LEAGUES,
      selected_dates = Sys.Date()  # Default to today only (actual dates, not day names)
    )
    
    # =========================================================================
    # LEAGUE TOGGLE BUTTONS
    # =========================================================================
    
    output$league_buttons <- renderUI({
      # Select All / Deselect All button
      all_selected <- length(rv$selected_leagues) == length(names(BETTING_LEAGUES))
      
      select_all_btn <- div(
        style = "margin-bottom: var(--space-sm);",
        tags$button(
          id = ns("select_all_leagues"),
          style = paste0(
            "display: inline-flex; align-items: center; gap: 6px; ",
            "padding: 6px 12px; border-radius: var(--radius-sm); ",
            "border: none; cursor: pointer; font-family: var(--font-primary); ",
            "font-weight: 600; font-size: 0.75rem; text-transform: uppercase; ",
            "letter-spacing: 0.5px; ",
            "background-color: transparent; ",
            "color: ", if (all_selected) "var(--coral)" else "var(--sage-dark)", "; ",
            "transition: all var(--transition-fast);"
          ),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("toggle_all_leagues"),
            if (all_selected) "deselect" else "select"
          ),
          icon(if (all_selected) "square-minus" else "square-check", style = "font-size: 0.85rem;"),
          span(if (all_selected) "Deselect All" else "Select All")
        )
      )
      
      # Individual league buttons
      buttons <- lapply(names(BETTING_LEAGUES), function(league_name) {
        is_selected <- league_name %in% rv$selected_leagues
        
        # Get league logo
        logo_path <- get_league_logo(league_name)
        
        # Clean minimal style - identical width, grey shadow
        tags$button(
          id = ns(paste0("league_", gsub(" ", "_", league_name))),
          style = paste0(
            "display: inline-flex; align-items: center; justify-content: center; gap: 8px; ",
            "padding: 10px 14px; border-radius: var(--radius-md); ",
            "border: 2px solid ", if (is_selected) "var(--outline)" else "#E0E0E0", "; ",
            "cursor: pointer; font-family: var(--font-primary); ",
            "font-weight: ", if (is_selected) "700" else "500", "; ",
            "font-size: 0.85rem; width: calc(25% - 6px); box-sizing: border-box; ",
            "background-color: var(--bg-white); ",
            "color: ", if (is_selected) "var(--text-primary)" else "var(--text-secondary)", "; ",
            "transition: all var(--transition-fast); ",
            "box-shadow: 2px 2px 0 #CCCCCC;"
          ),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("league_clicked"),
            league_name
          ),
          if (!is.null(logo_path)) {
            tags$img(src = logo_path, style = paste0(
              "width: 25px; height: 25px; object-fit: contain; ",
              "opacity: ", if (is_selected) "1" else "0.6", ";"
            ))
          },
          span(league_name)
        )
      })
      
      tagList(
        select_all_btn,
        div(style = "display: flex; flex-wrap: wrap; gap: 8px;", buttons)
      )
    })
    
    # Handle Select All / Deselect All
    observeEvent(input$toggle_all_leagues, {
      if (input$toggle_all_leagues == "select") {
        rv$selected_leagues <- names(BETTING_LEAGUES)
      } else {
        rv$selected_leagues <- BETTING_DEFAULT_LEAGUES  # Keep at least default
      }
    })
    
    observeEvent(input$league_clicked, {
      league_name <- input$league_clicked
      
      if (league_name %in% rv$selected_leagues) {
        if (length(rv$selected_leagues) > 1) {
          rv$selected_leagues <- setdiff(rv$selected_leagues, league_name)
        } else {
          showNotification("At least one league must be selected", type = "warning", duration = 3)
        }
      } else {
        rv$selected_leagues <- c(rv$selected_leagues, league_name)
      }
    })
    
    # =========================================================================
    # DAY TOGGLE BUTTONS
    # =========================================================================
    
    output$day_buttons <- renderUI({
      buttons <- lapply(0:6, function(i) {
        date <- Sys.Date() + i
        day_abbr <- toupper(substr(weekdays(date), 1, 3))
        day_date <- format(date, "%d %b")
        date_str <- as.character(date)  # For passing to Shiny
        
        is_selected <- date %in% rv$selected_dates
        
        tags$button(
          id = ns(paste0("day_", i)),
          style = paste0(
            "display: inline-flex; flex-direction: column; align-items: center; justify-content: center; ",
            "padding: 10px 18px; margin: 4px; border-radius: var(--radius-md); ",
            "border: 2px solid ", if (is_selected) "var(--outline)" else "#E0E0E0", "; ",
            "cursor: pointer; font-family: var(--font-primary); min-width: 70px; ",
            "background-color: var(--bg-white); ",
            "color: ", if (is_selected) "var(--text-primary)" else "var(--text-secondary)", "; ",
            "transition: all var(--transition-fast); ",
            "box-shadow: 2px 2px 0 #CCCCCC;"
          ),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("date_clicked"),
            date_str
          ),
          div(style = paste0(
            "font-size: 0.95rem; letter-spacing: 0.5px; ",
            "font-weight: ", if (is_selected) "700" else "500", ";"
          ), day_abbr),
          div(style = "font-size: 0.75rem; margin-top: 2px;", day_date)
        )
      })
      
      div(style = "display: flex; flex-wrap: wrap; gap: 4px;", buttons)
    })
    
    observeEvent(input$date_clicked, {
      date_clicked <- as.Date(input$date_clicked)
      
      if (date_clicked %in% rv$selected_dates) {
        if (length(rv$selected_dates) > 1) {
          # setdiff converts Date to numeric, so we need to preserve Date class
          rv$selected_dates <- rv$selected_dates[rv$selected_dates != date_clicked]
        } else {
          showNotification("At least one day must be selected", type = "warning", duration = 3)
        }
      } else {
        rv$selected_dates <- c(rv$selected_dates, date_clicked)
      }
    })
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    observe({
      req(!rv$loading)
      req(!rv$load_attempted)  # Don't retry if we already tried
      
      if (is.null(rv$odds_data)) {
        log_debug("Soccer Betting: Loading initial data")
        load_betting_data()
      }
    })
    
    observeEvent(input$refresh_data, {
      log_debug("Soccer Betting: Manual refresh triggered")
      rv$load_attempted <- FALSE  # Reset flag for manual refresh
      load_betting_data(force_refresh = TRUE)
    })
    
    load_betting_data <- function(force_refresh = FALSE) {
      rv$loading <- TRUE
      rv$load_attempted <- TRUE  # Mark that we've attempted to load
      
      tryCatch({
        data <- fetch_betting_data(
          selected_leagues = names(BETTING_LEAGUES),
          force_refresh = force_refresh
        )
        
        rv$odds_data <- data$odds
        rv$standings_data <- data$standings
        rv$last_updated <- data$timestamp
        
        log_debug("Soccer Betting: Loaded", nrow(rv$odds_data), "matches")
        
      }, error = function(e) {
        log_debug("Soccer Betting: Load error -", e$message, level = "ERROR")
        showNotification(paste("Error loading data:", e$message), type = "error", duration = 5)
        
        # Set empty data to prevent further retries
        rv$odds_data <- tibble(
          match_id = character(),
          DateTime = as.POSIXct(character()),
          home_team = character(),
          away_team = character(),
          odd_home = numeric(),
          odd_draw = numeric(),
          odd_away = numeric(),
          league_name = character()
        )
        rv$standings_data <- tibble()
        
      }, finally = {
        rv$loading <- FALSE
      })
    }
    
    # =========================================================================
    # DATA FRESHNESS INDICATOR
    # =========================================================================
    
    output$data_freshness <- renderUI({
      base_style <- "font-size: 0.75rem; font-style: italic;"
      
      if (rv$loading) {
        return(span(style = paste0(base_style, " color: var(--text-muted);"), "Loading..."))
      }
      
      if (is.null(rv$last_updated)) {
        return(span(style = paste0(base_style, " color: var(--text-muted);"), "No data"))
      }
      
      hours_old <- as.numeric(difftime(Sys.time(), rv$last_updated, units = "hours"))
      
      time_text <- if (hours_old < 1) {
        sprintf("%.0f minutes ago", hours_old * 60)
      } else {
        sprintf("%.1f hours ago", hours_old)
      }
      
      span(
        style = paste0(base_style, " color: var(--text-muted);"),
        title = format(rv$last_updated, "%Y-%m-%d %H:%M"),
        sprintf("Data loaded %s", time_text)
      )
    })
    
    # =========================================================================
    # FILTERED DATA
    # =========================================================================
    
    filtered_data <- reactive({
      req(rv$odds_data)
      
      odds <- rv$odds_data
      standings <- rv$standings_data
      
      # Filter out games that have already started
      current_time <- Sys.time()
      odds <- odds %>% filter(DateTime > current_time)
      
      # Filter by selected leagues
      if (length(rv$selected_leagues) > 0) {
        odds <- odds %>% filter(league_name %in% rv$selected_leagues)
        if (!is.null(standings) && nrow(standings) > 0) {
          standings <- standings %>% filter(league_name %in% rv$selected_leagues)
        }
      }
      
      # Filter by selected dates (actual dates, not day names)
      if (length(rv$selected_dates) > 0) {
        odds <- odds %>%
          filter(as.Date(DateTime) %in% rv$selected_dates)
      }
      
      # PPG mode
      ppg_mode <- input$ppg_mode %||% "total"
      
      # Prepare table with team name normalization and logos
      result <- prepare_betting_table_with_logos(odds, standings, ppg_mode)
      
      # Apply diff filter if set
      diff_col <- input$diff_filter_col %||% "none"
      diff_dir <- input$diff_filter_dir %||% "all"
      
      if (diff_col != "none" && diff_dir != "all" && !is.null(result) && nrow(result) > 0) {
        if (diff_col %in% names(result)) {
          if (diff_dir == "positive") {
            result <- result %>% filter(!is.na(.data[[diff_col]]) & .data[[diff_col]] > 0)
          } else if (diff_dir == "negative") {
            result <- result %>% filter(!is.na(.data[[diff_col]]) & .data[[diff_col]] < 0)
          }
        }
      }
      
      result
    })
    
    # =========================================================================
    # MAIN TABLE
    # =========================================================================
    
    output$betting_table <- renderReactable({
      data <- filtered_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          reactable(
            tibble(Message = "No matches found for selected filters"),
            columns = list(
              Message = colDef(
                align = "center",
                style = list(padding = "40px 20px", color = APP_COLORS$muted, fontStyle = "italic")
              )
            ),
            theme = app_reactable_theme()
          )
        )
      }
      
      # Get heatmap settings
      heatmap_col <- input$heatmap_col %||% "none"
      
      # Calculate ranges for heatmap (symmetric around 0 for diff columns)
      if (nrow(data) > 0) {
        season_max <- max(abs(data$season_diff), na.rm = TRUE)
        last13_max <- max(abs(data$last13_diff), na.rm = TRUE)
        last6_max <- max(abs(data$last6_diff), na.rm = TRUE)
        odds_range <- range(data$win_odds, na.rm = TRUE)
      } else {
        season_max <- last13_max <- last6_max <- 10
        odds_range <- c(1, 5)
      }
      
      # Use same pattern as FanTeam - assign to table_df as data.frame, pass to reactable
      table_df <- as.data.frame(data)
      
      reactable(
        table_df,
        
        theme = reactable::reactableTheme(
          color = APP_COLORS$primary,
          backgroundColor = "white",
          borderColor = "#E8E8E8",
          stripedColor = APP_COLORS$bg_tan,
          highlightColor = "#F0F0F0",
          cellPadding = "10px 14px",
          
          style = list(
            fontFamily = sprintf("'%s', %s", APP_FONT, APP_FONT_FALLBACK),
            fontSize = "17px",  # Increased from 13px
            borderRadius = "6px",
            boxShadow = "0 1px 3px rgba(0,0,0,0.04)"
          ),
          
          headerStyle = list(
            background = "white",
            borderBottom = "2px solid #E0E0E0",
            color = APP_COLORS$muted,
            fontWeight = "600",
            fontSize = "14px",  # Increased from 10px
            textTransform = "uppercase",
            letterSpacing = "0.5px",
            padding = "12px 14px"
          ),
          
          cellStyle = list(borderBottom = "1px solid #F0F0F0")
        ),
        
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100),
        searchable = TRUE,
        filterable = FALSE,
        striped = TRUE,
        highlight = TRUE,
        compact = FALSE,
        
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(fontWeight = "600", fontSize = "14px", textTransform = "uppercase")
        ),
        
        columns = list(
          League = colDef(
            name = "League",
            width = 290,
            align = "left",
            cell = function(value, index) {
              logo <- table_df$league_logo[index]
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                if (!is.null(logo) && nchar(logo) > 0) {
                  tags$img(src = logo, style = "width: 26px; height: 26px; object-fit: contain;")
                },
                span(style = "font-weight: 500; font-size: 16px;", value)
              )
            }
          ),
          
          Team = colDef(
            name = "Team",
            minWidth = 175,
            align = "left",
            cell = function(value, index) {
              logo <- table_df$team_logo[index]
              is_home <- table_df$is_home[index]
              
              border_color <- if (is_home) APP_COLORS$sage else APP_COLORS$coral
              
              div(
                style = sprintf(
                  "display: flex; align-items: center; gap: 12px; padding-left: 10px; border-left: 4px solid %s;",
                  border_color
                ),
                if (!is.null(logo) && nchar(logo) > 0) {
                  tags$img(src = logo, style = "width: 28px; height: 28px; object-fit: contain;")
                },
                span(style = "font-weight: 600; font-size: 17px;", value)
              )
            }
          ),
          
          Opponent = colDef(
            name = "Opponent",
            minWidth = 155,
            align = "left",
            cell = function(value, index) {
              logo <- table_df$opponent_logo[index]
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                if (!is.null(logo) && nchar(logo) > 0) {
                  tags$img(src = logo, style = "width: 26px; height: 26px; object-fit: contain;")
                },
                span(style = "font-weight: 500; font-size: 16px; color: var(--text-secondary);", value)
              )
            }
          ),
          
          win_odds = colDef(
            name = "Win Odds",
            width = 110,
            cell = function(value, index) {
              if (is.na(value)) return(span(style = list(color = APP_COLORS$muted), "-"))
              
              span(
                style = list(fontWeight = "700", fontSize = "17px", color = APP_COLORS$primary),
                sprintf("%.2f", value)
              )
            },
            style = function(value, index) {
              if (heatmap_col == "win_odds" && !is.na(value)) {
                # Single direction: higher odds = more intense color (teal)
                # Normalize value between min and max odds
                t <- (value - odds_range[1]) / (odds_range[2] - odds_range[1])
                t <- max(0, min(1, t))
                
                # Interpolate from white (#FFFFFF) to teal (#8FBCBB)
                r <- round(255 + (143 - 255) * t)
                g <- round(255 + (188 - 255) * t)
                b <- round(255 + (187 - 255) * t)
                
                list(background = sprintf("rgb(%d, %d, %d)", r, g, b))
              } else {
                list()
              }
            }
          ),
          
          season_diff = colDef(
            name = "Season",
            width = 100,
            cell = function(value, index) {
              if (is.na(value)) return(span(style = list(color = APP_COLORS$muted, fontSize = "17px"), "-"))
              
              prefix <- if (value > 0) "+" else ""
              formatted <- sprintf("%s%.1f", prefix, value)
              
              # Color based on value
              text_color <- if (value > 0.2) {
                APP_COLORS$sage_dark
              } else if (value < -0.2) {
                APP_COLORS$coral_dark
              } else {
                APP_COLORS$primary
              }
              
              span(
                style = list(
                  color = text_color, 
                  fontWeight = if (abs(value) > 0.5) "700" else "500", 
                  fontSize = "17px"
                ),
                formatted
              )
            },
            style = function(value, index) {
              if (heatmap_col == "season_diff" && !is.na(value)) {
                bg_style <- get_diverging_heatmap_style(
                  value = value,
                  midpoint = 0,
                  min_val = -season_max,
                  max_val = season_max
                )
                list(background = gsub("background-color: |;", "", bg_style))
              } else {
                list()
              }
            }
          ),
          
          last13_diff = colDef(
            name = "Last 13",
            width = 100,
            cell = function(value, index) {
              if (is.na(value)) return(span(style = list(color = APP_COLORS$muted, fontSize = "17px"), "-"))
              
              prefix <- if (value > 0) "+" else ""
              formatted <- sprintf("%s%.1f", prefix, value)
              
              text_color <- if (value > 0.2) {
                APP_COLORS$sage_dark
              } else if (value < -0.2) {
                APP_COLORS$coral_dark
              } else {
                APP_COLORS$primary
              }
              
              span(
                style = list(
                  color = text_color, 
                  fontWeight = if (abs(value) > 0.5) "700" else "500", 
                  fontSize = "17px"
                ),
                formatted
              )
            },
            style = function(value, index) {
              if (heatmap_col == "last13_diff" && !is.na(value)) {
                bg_style <- get_diverging_heatmap_style(
                  value = value,
                  midpoint = 0,
                  min_val = -last13_max,
                  max_val = last13_max
                )
                list(background = gsub("background-color: |;", "", bg_style))
              } else {
                list()
              }
            }
          ),
          
          last6_diff = colDef(
            name = "Last 6",
            width = 100,
            cell = function(value, index) {
              if (is.na(value)) return(span(style = list(color = APP_COLORS$muted, fontSize = "17px"), "-"))
              
              prefix <- if (value > 0) "+" else ""
              formatted <- sprintf("%s%.1f", prefix, value)
              
              text_color <- if (value > 0.2) {
                APP_COLORS$sage_dark
              } else if (value < -0.2) {
                APP_COLORS$coral_dark
              } else {
                APP_COLORS$primary
              }
              
              span(
                style = list(
                  color = text_color, 
                  fontWeight = if (abs(value) > 0.5) "700" else "500", 
                  fontSize = "17px"
                ),
                formatted
              )
            },
            style = function(value, index) {
              if (heatmap_col == "last6_diff" && !is.na(value)) {
                bg_style <- get_diverging_heatmap_style(
                  value = value,
                  midpoint = 0,
                  min_val = -last6_max,
                  max_val = last6_max
                )
                list(background = gsub("background-color: |;", "", bg_style))
              } else {
                list()
              }
            }
          ),
          
          # Hidden columns for logo data access
          team_logo = colDef(show = FALSE),
          opponent_logo = colDef(show = FALSE),
          league_logo = colDef(show = FALSE),
          is_home = colDef(show = FALSE),
          DateTime = colDef(show = FALSE)
        )
      )
    })
    
  })
}

# =============================================================================
# HELPER: Prepare table with logos and normalized names
# =============================================================================

prepare_betting_table_with_logos <- function(odds_data, standings_data, ppg_mode = "total") {
  if (is.null(odds_data) || nrow(odds_data) == 0) {
    return(tibble(
      League = character(),
      Team = character(),
      Opponent = character(),
      win_odds = numeric(),
      season_diff = numeric(),
      last13_diff = numeric(),
      last6_diff = numeric(),
      is_home = logical(),
      team_logo = character(),
      opponent_logo = character(),
      league_logo = character()
    ))
  }
  
  # Normalize team names using betting-specific function
  # This extends the base normalize_team_names with BBC/Odds API variants
  odds_norm <- odds_data %>%
    mutate(
      home_team_norm = normalize_betting_team_names(home_team),
      away_team_norm = normalize_betting_team_names(away_team)
    )
  
  # Join standings using normalized names
  if (!is.null(standings_data) && nrow(standings_data) > 0) {
    standings_norm <- standings_data %>%
      mutate(team_norm = normalize_betting_team_names(team))
    
    # Join home teams
    odds_with_home <- odds_norm %>%
      left_join(
        standings_norm %>% 
          select(team_norm, league_name, games_played, points, season_ppg, last13_ppg, last6_ppg) %>%
          rename(
            home_games = games_played,
            home_points = points,
            home_season_ppg = season_ppg,
            home_last13_ppg = last13_ppg,
            home_last6_ppg = last6_ppg
          ),
        by = c("home_team_norm" = "team_norm", "league_name" = "league_name")
      )
    
    # Join away teams
    full_joined <- odds_with_home %>%
      left_join(
        standings_norm %>% 
          select(team_norm, league_name, games_played, points, season_ppg, last13_ppg, last6_ppg) %>%
          rename(
            away_games = games_played,
            away_points = points,
            away_season_ppg = season_ppg,
            away_last13_ppg = last13_ppg,
            away_last6_ppg = last6_ppg
          ),
        by = c("away_team_norm" = "team_norm", "league_name" = "league_name")
      )
    
    # Diagnostic: log unmatched teams
    unmatched_home <- full_joined %>% 
      filter(is.na(home_season_ppg)) %>% 
      distinct(league_name, home_team_norm) %>%
      mutate(side = "home")
    unmatched_away <- full_joined %>% 
      filter(is.na(away_season_ppg)) %>% 
      distinct(league_name, away_team_norm) %>%
      mutate(side = "away")
    
    if (nrow(unmatched_home) > 0 || nrow(unmatched_away) > 0) {
      all_unmatched <- unique(c(
        paste0(unmatched_home$league_name, ": ", unmatched_home$home_team_norm),
        paste0(unmatched_away$league_name, ": ", unmatched_away$away_team_norm)
      ))
      standings_teams <- standings_norm %>% distinct(league_name, team_norm)
      
      # Log unmatched and what standings has for those leagues
      for (um in all_unmatched) {
        league <- sub(":.*", "", um)
        avail <- standings_teams %>% filter(league_name == league) %>% pull(team_norm)
        if (length(avail) == 0) {
          log_debug(sprintf("Betting UNMATCHED: %s (BBC returned NO standings for this league)", um))
        } else {
          log_debug(sprintf("Betting UNMATCHED: %s (BBC has: %s)", um, paste(head(avail, 20), collapse = ", ")))
        }
      }
    }
  } else {
    full_joined <- odds_norm %>%
      mutate(
        home_games = NA_real_, away_games = NA_real_,
        home_points = NA_real_, away_points = NA_real_,
        home_season_ppg = NA_real_, away_season_ppg = NA_real_,
        home_last13_ppg = NA_real_, away_last13_ppg = NA_real_,
        home_last6_ppg = NA_real_, away_last6_ppg = NA_real_
      )
  }
  
  # Calculate differentials based on mode
  if (ppg_mode == "per_game") {
    # Per game (PPG difference)
    home_rows <- full_joined %>%
      transmute(
        DateTime = DateTime,
        League = league_name,
        Team = home_team_norm,
        Opponent = away_team_norm,
        win_odds = odd_home,
        season_diff = round(home_season_ppg - away_season_ppg, 2),
        last13_diff = round(home_last13_ppg - away_last13_ppg, 2),
        last6_diff = round(home_last6_ppg - away_last6_ppg, 2),
        is_home = TRUE
      )
    
    away_rows <- full_joined %>%
      transmute(
        DateTime = DateTime,
        League = league_name,
        Team = away_team_norm,
        Opponent = home_team_norm,
        win_odds = odd_away,
        season_diff = round(away_season_ppg - home_season_ppg, 2),
        last13_diff = round(away_last13_ppg - home_last13_ppg, 2),
        last6_diff = round(away_last6_ppg - home_last6_ppg, 2),
        is_home = FALSE
      )
  } else {
    # Total points difference
    home_rows <- full_joined %>%
      transmute(
        DateTime = DateTime,
        League = league_name,
        Team = home_team_norm,
        Opponent = away_team_norm,
        win_odds = odd_home,
        season_diff = round(home_points - away_points, 0),
        last13_diff = round((home_last13_ppg * 13) - (away_last13_ppg * 13), 0),
        last6_diff = round((home_last6_ppg * 6) - (away_last6_ppg * 6), 0),
        is_home = TRUE
      )
    
    away_rows <- full_joined %>%
      transmute(
        DateTime = DateTime,
        League = league_name,
        Team = away_team_norm,
        Opponent = home_team_norm,
        win_odds = odd_away,
        season_diff = round(away_points - home_points, 0),
        last13_diff = round((away_last13_ppg * 13) - (home_last13_ppg * 13), 0),
        last6_diff = round((away_last6_ppg * 6) - (home_last6_ppg * 6), 0),
        is_home = FALSE
      )
  }
  
  # Combine and add logos
  result <- bind_rows(home_rows, away_rows) %>%
    arrange(DateTime, League, Team)
  
  # Helper to safely get logo path
  safe_get_team_logo <- function(team_name) {
    logo <- get_soccer_team_logo(team_name)
    if (is.null(logo) || length(logo) == 0 || is.na(logo)) "" else logo
  }
  
  safe_get_league_logo <- function(league_name) {
    logo <- get_league_logo(league_name)
    if (is.null(logo) || length(logo) == 0 || is.na(logo)) "" else logo
  }
  
  # Add logo columns
  result <- result %>%
    mutate(
      team_logo = vapply(Team, safe_get_team_logo, character(1)),
      opponent_logo = vapply(Opponent, safe_get_team_logo, character(1)),
      league_logo = vapply(League, safe_get_league_logo, character(1))
    )
  
  # Brief debug summary
  if (nrow(result) > 0) {
    teams_with_logos <- sum(nchar(result$team_logo) > 0)
    log_debug(sprintf("Betting: %d rows, %d with team logos (%.0f%%)", 
                      nrow(result), teams_with_logos, 
                      100 * teams_with_logos / nrow(result)))
  }
  
  return(result)
}