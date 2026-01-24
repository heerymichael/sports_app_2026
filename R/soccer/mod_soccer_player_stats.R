# =============================================================================
# Module: Soccer Player Stats
# 
# FanTeam weekly player performance statistics viewer
# Data source: Google Sheet with weekly player statistics (with Parquet caching)
# 
# Features:
#   1. Stats Overview - Season-long summary table, filterable by position
#   2. Stats by GW - Compare selected players across gameweek ranges
#   3. Gameweek Detail - Detailed per-player per-GW statistics
#
# Dependencies: app_themes.R, soccer_config.R, soccer_data_loader.R, helpers.R
# =============================================================================

# Google Sheet ID for FanTeam weekly stats
FANTEAM_STATS_SHEET_ID <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"

# Sheet names within the workbook
FANTEAM_STATS_SHEETS <- list(
  overview = "stats_overview",
  by_gw = "stats_by_gw",
  detail = "gameweek_detail"
)

# Cache max age for player stats (hours)
FANTEAM_STATS_CACHE_HOURS <- 12

# =============================================================================
# DATA LOADING FUNCTIONS (with Parquet caching)
# =============================================================================

#' Initialize FanTeam stats Google Sheet (public access)
init_fanteam_stats_sheets <- function() {
  log_debug("Initializing FanTeam stats sheets (public, no auth)...", level = "INFO")
  googlesheets4::gs4_deauth()
  log_debug("FanTeam stats sheets initialized", level = "INFO")
}

#' Get cache path for FanTeam stats
#' @param data_type Type: "ft_overview", "ft_by_gw", "ft_detail"
get_fanteam_stats_cache_path <- function(data_type) {
  if (!dir.exists(CACHE_DIR)) {
    dir.create(CACHE_DIR, recursive = TRUE)
  }
  
  ext <- if (USE_PARQUET_CACHE && has_arrow()) ".parquet" else ".rds"
  file.path(CACHE_DIR, paste0("fanteam_", data_type, ext))
}

#' Check if FanTeam stats cache is valid
is_fanteam_stats_cache_valid <- function(cache_path) {
  if (!file.exists(cache_path)) {
    return(FALSE)
  }
  
  file_age_hours <- difftime(Sys.time(), file.mtime(cache_path), units = "hours")
  return(as.numeric(file_age_hours) < FANTEAM_STATS_CACHE_HOURS)
}

#' Save FanTeam stats to cache
save_fanteam_stats_cache <- function(data, data_type) {
  cache_path <- get_fanteam_stats_cache_path(data_type)
  
  tryCatch({
    if (USE_PARQUET_CACHE && has_arrow()) {
      arrow::write_parquet(data, cache_path)
      log_debug(sprintf("Saved %d rows to Parquet cache: %s", nrow(data), data_type), level = "INFO")
    } else {
      saveRDS(data, cache_path)
      log_debug(sprintf("Saved %d rows to RDS cache: %s", nrow(data), data_type), level = "INFO")
    }
  }, error = function(e) {
    log_debug(sprintf("Failed to save cache: %s", e$message), level = "WARN")
  })
}

#' Load FanTeam stats from cache
load_fanteam_stats_cache <- function(data_type) {
  cache_path <- get_fanteam_stats_cache_path(data_type)
  
  if (!file.exists(cache_path)) {
    return(NULL)
  }
  
  tryCatch({
    if (grepl("\\.parquet$", cache_path) && has_arrow()) {
      data <- arrow::read_parquet(cache_path)
    } else {
      data <- readRDS(cache_path)
    }
    log_debug(sprintf("Loaded %d rows from cache: %s", nrow(data), data_type), level = "INFO")
    return(as.data.frame(data))
  }, error = function(e) {
    log_debug(sprintf("Failed to load cache: %s", e$message), level = "WARN")
    return(NULL)
  })
}

#' Load stats overview data (with caching)
#' @param force_refresh If TRUE, bypass cache
#' @return Data frame with season-long player stats
load_fanteam_stats_overview <- function(force_refresh = FALSE) {
  log_debug("load_fanteam_stats_overview() called", level = "INFO")
  
  cache_path <- get_fanteam_stats_cache_path("ft_overview")
  
  # 1. Check cache first
  if (!force_refresh && is_fanteam_stats_cache_valid(cache_path)) {
    data <- load_fanteam_stats_cache("ft_overview")
    if (!is.null(data)) {
      return(data)
    }
  }
  
  # 2. Load from Google Sheets
  log_debug("Loading stats overview from Google Sheets...", level = "INFO")
  
  tryCatch({
    init_fanteam_stats_sheets()
    
    data <- googlesheets4::read_sheet(
      FANTEAM_STATS_SHEET_ID,
      sheet = FANTEAM_STATS_SHEETS$overview
    ) %>%
      janitor::clean_names() %>%
      as.data.frame()
    
    # Save to cache
    save_fanteam_stats_cache(data, "ft_overview")
    
    log_debug("Loaded stats overview:", nrow(data), "rows", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug("Error loading stats overview:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load stats by gameweek data (with caching)
#' @param force_refresh If TRUE, bypass cache
#' @return Data frame with per-gameweek player stats
load_fanteam_stats_by_gw <- function(force_refresh = FALSE) {
  log_debug("load_fanteam_stats_by_gw() called", level = "INFO")
  
  cache_path <- get_fanteam_stats_cache_path("ft_by_gw")
  
  # 1. Check cache first
  if (!force_refresh && is_fanteam_stats_cache_valid(cache_path)) {
    data <- load_fanteam_stats_cache("ft_by_gw")
    if (!is.null(data)) {
      return(data)
    }
  }
  
  # 2. Load from Google Sheets
  log_debug("Loading stats by GW from Google Sheets...", level = "INFO")
  
  tryCatch({
    init_fanteam_stats_sheets()
    
    data <- googlesheets4::read_sheet(
      FANTEAM_STATS_SHEET_ID,
      sheet = FANTEAM_STATS_SHEETS$by_gw
    ) %>%
      janitor::clean_names() %>%
      as.data.frame()
    
    # Save to cache
    save_fanteam_stats_cache(data, "ft_by_gw")
    
    log_debug("Loaded stats by GW:", nrow(data), "rows", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug("Error loading stats by GW:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load gameweek detail data (with caching)
#' @param force_refresh If TRUE, bypass cache
#' @return Data frame with detailed per-GW player stats
load_fanteam_stats_detail <- function(force_refresh = FALSE) {
  log_debug("load_fanteam_stats_detail() called", level = "INFO")
  
  cache_path <- get_fanteam_stats_cache_path("ft_detail")
  
  # 1. Check cache first
  if (!force_refresh && is_fanteam_stats_cache_valid(cache_path)) {
    data <- load_fanteam_stats_cache("ft_detail")
    if (!is.null(data)) {
      return(data)
    }
  }
  
  # 2. Load from Google Sheets
  log_debug("Loading gameweek detail from Google Sheets...", level = "INFO")
  
  tryCatch({
    init_fanteam_stats_sheets()
    
    data <- googlesheets4::read_sheet(
      FANTEAM_STATS_SHEET_ID,
      sheet = FANTEAM_STATS_SHEETS$detail
    ) %>%
      janitor::clean_names() %>%
      as.data.frame()
    
    # Save to cache
    save_fanteam_stats_cache(data, "ft_detail")
    
    log_debug("Loaded gameweek detail:", nrow(data), "rows", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug("Error loading gameweek detail:", e$message, level = "ERROR")
    return(NULL)
  })
}

# =============================================================================
# UI
# =============================================================================

#' Soccer Player Stats UI
#' @param id Module namespace ID
soccer_player_stats_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_player_stats_ui() called with id:", id, level = "INFO")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Player Stats"),
      tags$p(class = "text-muted", "Weekly player performance statistics for FanTeam contests")
    ),
    
    # Filters card
    ui_card(
      title = "Filters",
      color = "sage",
      
      fluidRow(
        column(3,
               shinyWidgets::pickerInput(
                 ns("league"),
                 "League",
                 choices = c("Premier League"),
                 selected = "Premier League",
                 options = shinyWidgets::pickerOptions(
                   liveSearch = FALSE,
                   size = 10
                 )
               )
        ),
        column(9,
               div(
                 style = "padding-top: 25px; display: flex; justify-content: flex-end; gap: 0.5rem; align-items: center;",
                 uiOutput(ns("data_status")),
                 tags$button(
                   id = ns("refresh_data"),
                   class = "btn btn-refresh-subtle",
                   type = "button",
                   "Refresh Data"
                 )
               )
        )
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # Section 1: Stats Overview
    # =========================================================================
    ui_card(
      title = "Season Overview",
      color = "sage",
      
      fluidRow(
        column(3,
               shinyWidgets::pickerInput(
                 ns("position_filter"),
                 "Position Filter",
                 choices = c("All", "GK", "DEF", "MID", "FWD"),
                 selected = "All",
                 multiple = TRUE,
                 options = shinyWidgets::pickerOptions(
                   actionsBox = TRUE,
                   noneSelectedText = "All Positions"
                 )
               )
        ),
        column(2,
               numericInput(
                 ns("min_games"),
                 "Min Games",
                 value = 3,
                 min = 1,
                 max = 38,
                 step = 1
               )
        ),
        column(3,
               shinyWidgets::pickerInput(
                 ns("overview_team_filter"),
                 "Team Filter",
                 choices = NULL,
                 selected = NULL,
                 multiple = TRUE,
                 options = shinyWidgets::pickerOptions(
                   actionsBox = TRUE,
                   liveSearch = TRUE,
                   noneSelectedText = "All Teams"
                 )
               )
        ),
        column(4,
               div(
                 style = "padding-top: 25px;",
                 textOutput(ns("overview_count"))
               )
        )
      ),
      
      tags$hr(style = "margin: 0.5rem 0 1rem 0; border-color: var(--bg-secondary);"),
      
      reactableOutput(ns("overview_table"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # Section 2: Stats by Gameweek (Player Comparison)
    # =========================================================================
    ui_card(
      title = "Player Comparison by Gameweek",
      color = "sage",
      
      fluidRow(
        column(5,
               shinyWidgets::pickerInput(
                 ns("compare_players"),
                 "Select Players",
                 choices = NULL,
                 selected = NULL,
                 multiple = TRUE,
                 options = shinyWidgets::pickerOptions(
                   actionsBox = TRUE,
                   liveSearch = TRUE,
                   maxOptions = 10,
                   noneSelectedText = "Select players to compare",
                   selectedTextFormat = "count > 3",
                   countSelectedText = "{0} players selected"
                 )
               )
        ),
        column(3,
               sliderInput(
                 ns("gw_range"),
                 "Gameweek Range",
                 min = 1,
                 max = 38,
                 value = c(1, 10),
                 step = 1
               )
        ),
        column(4,
               div(
                 style = "padding-top: 25px;",
                 shinyWidgets::pickerInput(
                   ns("compare_metric"),
                   "Metric",
                   choices = c(
                     "FanTeam Points" = "total_pts",
                     "Minutes" = "mins_played",
                     "Goals" = "goals",
                     "Assists" = "assists",
                     "Shots on Target" = "sot",
                     "Clean Sheets" = "cs",
                     "Saves" = "saves"
                   ),
                   selected = "total_pts"
                 )
               )
        )
      ),
      
      tags$hr(style = "margin: 0.5rem 0 1rem 0; border-color: var(--bg-secondary);"),
      
      uiOutput(ns("comparison_content"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # Section 3: Gameweek Detail
    # =========================================================================
    ui_card(
      title = "Gameweek Detail",
      color = "sage",
      
      fluidRow(
        column(2,
               shinyWidgets::pickerInput(
                 ns("detail_gw"),
                 "Gameweek",
                 choices = NULL,
                 selected = NULL,
                 options = shinyWidgets::pickerOptions(
                   liveSearch = FALSE,
                   size = 10
                 )
               )
        ),
        column(3,
               shinyWidgets::pickerInput(
                 ns("detail_position"),
                 "Position Filter",
                 choices = c("All", "GK", "DEF", "MID", "FWD"),
                 selected = "All",
                 multiple = TRUE,
                 options = shinyWidgets::pickerOptions(
                   actionsBox = TRUE,
                   noneSelectedText = "All Positions"
                 )
               )
        ),
        column(3,
               shinyWidgets::pickerInput(
                 ns("detail_team"),
                 "Team Filter",
                 choices = NULL,
                 selected = NULL,
                 multiple = TRUE,
                 options = shinyWidgets::pickerOptions(
                   actionsBox = TRUE,
                   liveSearch = TRUE,
                   noneSelectedText = "All Teams"
                 )
               )
        ),
        column(4,
               div(
                 style = "padding-top: 25px;",
                 textOutput(ns("detail_count"))
               )
        )
      ),
      
      tags$hr(style = "margin: 0.5rem 0 1rem 0; border-color: var(--bg-secondary);"),
      
      reactableOutput(ns("detail_table"))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

#' Soccer Player Stats Server
#' @param id Module namespace ID
soccer_player_stats_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_player_stats_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    rv <- reactiveValues(
      overview_data = NULL,
      by_gw_data = NULL,
      detail_data = NULL,
      loading = FALSE,
      initialized = FALSE,
      last_refresh = NULL,
      error_message = NULL
    )
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    # Initial data load
    observe({
      log_debug(">>> Initial data load observer triggered", level = "DEBUG")
      
      if (rv$initialized) return()
      
      rv$loading <- TRUE
      rv$error_message <- NULL
      
      tryCatch({
        log_debug("Loading FanTeam stats data...", level = "INFO")
        
        rv$overview_data <- load_fanteam_stats_overview(force_refresh = FALSE)
        rv$by_gw_data <- load_fanteam_stats_by_gw(force_refresh = FALSE)
        rv$detail_data <- load_fanteam_stats_detail(force_refresh = FALSE)
        
        rv$last_refresh <- Sys.time()
        rv$initialized <- TRUE
        
        log_debug("FanTeam stats data loaded successfully", level = "INFO")
        if (!is.null(rv$overview_data)) log_debug("  Overview:", nrow(rv$overview_data), "rows", level = "INFO")
        if (!is.null(rv$by_gw_data)) log_debug("  By GW:", nrow(rv$by_gw_data), "rows", level = "INFO")
        if (!is.null(rv$detail_data)) log_debug("  Detail:", nrow(rv$detail_data), "rows", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error loading FanTeam stats:", e$message, level = "ERROR")
        rv$error_message <- e$message
      })
      
      rv$loading <- FALSE
    })
    
    # Manual refresh - force reload from Google Sheets
    observeEvent(input$refresh_data, {
      log_debug(">>> Manual refresh triggered", level = "INFO")
      
      rv$loading <- TRUE
      rv$error_message <- NULL
      
      tryCatch({
        rv$overview_data <- load_fanteam_stats_overview(force_refresh = TRUE)
        rv$by_gw_data <- load_fanteam_stats_by_gw(force_refresh = TRUE)
        rv$detail_data <- load_fanteam_stats_detail(force_refresh = TRUE)
        
        rv$last_refresh <- Sys.time()
        
        log_debug("Manual refresh completed", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error during refresh:", e$message, level = "ERROR")
        rv$error_message <- e$message
      })
      
      rv$loading <- FALSE
    })
    
    # =========================================================================
    # DATA STATUS
    # =========================================================================
    
    output$data_status <- renderUI({
      if (rv$loading) {
        return(tags$span(
          style = "color: var(--text-muted); font-size: 0.8rem;",
          "Loading..."
        ))
      }
      
      if (!is.null(rv$error_message)) {
        return(tags$span(
          style = "color: var(--accent-coral); font-size: 0.8rem;",
          icon("exclamation-triangle"),
          " Error loading data"
        ))
      }
      
      if (!is.null(rv$last_refresh)) {
        age <- difftime(Sys.time(), rv$last_refresh, units = "mins")
        age_text <- if (age < 1) {
          "just now"
        } else if (age < 60) {
          sprintf("%.0f min ago", age)
        } else {
          sprintf("%.1f hrs ago", age / 60)
        }
        
        return(tags$span(
          style = "color: var(--text-muted); font-size: 0.8rem;",
          paste("Updated", age_text)
        ))
      }
      
      return(NULL)
    })
    
    # =========================================================================
    # UPDATE LEAGUE PICKER WITH LOGO
    # =========================================================================
    
    observe({
      req(rv$initialized)
      
      leagues <- c("Premier League")
      
      # Build HTML content with league logo
      league_content <- sapply(leagues, function(lg) {
        logo_path <- get_league_logo(lg)
        if (!is.null(logo_path)) {
          sprintf('<img src="%s" style="width:20px; height:20px; margin-right:8px; vertical-align:middle; object-fit:contain;"> %s', 
                  logo_path, lg)
        } else {
          lg
        }
      }, USE.NAMES = FALSE)
      
      shinyWidgets::updatePickerInput(
        session, "league",
        choices = leagues,
        selected = "Premier League",
        choicesOpt = list(content = league_content)
      )
    })
    
    # =========================================================================
    # UPDATE TEAM FILTER CHOICES WITH LOGOS
    # =========================================================================
    
    observe({
      req(rv$overview_data)
      
      # Get teams from data
      teams <- sort(unique(rv$overview_data$team))
      teams <- teams[!is.na(teams) & teams != ""]
      
      if (length(teams) > 0) {
        # Build HTML content with team logos
        team_content <- sapply(teams, function(team) {
          logo_path <- get_soccer_team_logo(team)
          if (!is.null(logo_path)) {
            sprintf('<img src="%s" style="width:20px; height:20px; margin-right:8px; vertical-align:middle; object-fit:contain;"> %s', 
                    logo_path, team)
          } else {
            team
          }
        }, USE.NAMES = FALSE)
        
        # Update overview team filter
        shinyWidgets::updatePickerInput(
          session, "overview_team_filter",
          choices = teams,
          selected = NULL,
          choicesOpt = list(content = team_content)
        )
        
        # Update detail team filter
        shinyWidgets::updatePickerInput(
          session, "detail_team",
          choices = teams,
          selected = NULL,
          choicesOpt = list(content = team_content)
        )
      }
    })
    
    # =========================================================================
    # UPDATE PLAYER CHOICES
    # =========================================================================
    
    observe({
      req(rv$overview_data)
      
      # Get players sorted by total points
      players <- rv$overview_data %>%
        arrange(desc(total_pts)) %>%
        pull(name) %>%
        unique()
      
      players <- players[!is.na(players) & players != ""]
      
      shinyWidgets::updatePickerInput(
        session, "compare_players",
        choices = players,
        selected = NULL
      )
    })
    
    # =========================================================================
    # UPDATE GAMEWEEK CHOICES AND SLIDER
    # =========================================================================
    
    observe({
      req(rv$detail_data)
      
      # Get gameweek column - try common variants
      gw_col <- if ("gw" %in% names(rv$detail_data)) "gw"
      else if ("gameweek" %in% names(rv$detail_data)) "gameweek"
      else NULL
      
      if (!is.null(gw_col)) {
        gws <- sort(unique(as.integer(rv$detail_data[[gw_col]])))
        gws <- gws[!is.na(gws)]
        
        if (length(gws) > 0) {
          min_gw <- min(gws)
          max_gw <- max(gws)
          
          # Update slider
          updateSliderInput(
            session, "gw_range",
            min = min_gw,
            max = max_gw,
            value = c(min_gw, max_gw)
          )
          
          # Update detail dropdown (most recent first)
          shinyWidgets::updatePickerInput(
            session, "detail_gw",
            choices = as.character(rev(gws)),
            selected = as.character(max_gw)
          )
        }
      }
    })
    
    # =========================================================================
    # SECTION 1: OVERVIEW TABLE
    # =========================================================================
    
    filtered_overview <- reactive({
      req(rv$overview_data)
      
      data <- rv$overview_data
      
      # Position filter
      pos_filter <- input$position_filter
      if (!is.null(pos_filter) && !"All" %in% pos_filter && length(pos_filter) > 0) {
        data <- data %>% filter(pos %in% pos_filter)
      }
      
      # Min games filter - count non-zero minute appearances
      min_games <- input$min_games
      if (!is.null(min_games) && min_games > 0) {
        # Use x1_mp (1+ minute appearances) if available, otherwise estimate
        if ("x1_mp" %in% names(data)) {
          data <- data %>% filter(x1_mp >= min_games)
        }
      }
      
      # Team filter
      team_filter <- input$overview_team_filter
      if (!is.null(team_filter) && length(team_filter) > 0) {
        data <- data %>% filter(team %in% team_filter)
      }
      
      return(data)
    })
    
    output$overview_count <- renderText({
      data <- filtered_overview()
      if (is.null(data)) return("")
      sprintf("Showing %d players", nrow(data))
    })
    
    output$overview_table <- renderReactable({
      data <- filtered_overview()
      req(data, nrow(data) > 0)
      
      log_debug("Rendering overview table with", nrow(data), "rows", level = "DEBUG")
      
      # Select and order columns for display
      display_cols <- c("name", "team", "pos", "price", "total_pts", "avg_pts", "ppm",
                        "x1_mp", "mins_played", "goals", "sot", "assists", "cs", "saves")
      
      # Keep only columns that exist
      display_cols <- intersect(display_cols, names(data))
      data <- data %>% select(all_of(display_cols))
      
      # Column definitions
      col_defs <- list(
        name = colDef(
          name = "Player",
          minWidth = 160,
          sticky = "left",
          style = list(fontWeight = 600, background = "#fff")
        ),
        team = colDef(
          name = "Team",
          minWidth = 140,
          cell = function(value) {
            logo <- get_soccer_team_logo(value)
            if (!is.null(logo)) {
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                tags$img(src = logo, style = "width: 20px; height: 20px; object-fit: contain;"),
                span(value)
              )
            } else {
              value
            }
          }
        ),
        pos = colDef(
          name = "Pos",
          minWidth = 55,
          align = "center",
          cell = function(value) {
            color <- get_position_color(value)
            div(
              style = sprintf("background: %s; color: white; padding: 2px 8px; border-radius: 4px; font-size: 0.75rem; font-weight: 700;", color),
              value
            )
          }
        ),
        price = colDef(
          name = "Price",
          minWidth = 70,
          align = "center",
          cell = function(value) {
            sprintf("%.1fM", value / 10)
          }
        ),
        total_pts = colDef(
          name = "Total",
          minWidth = 70,
          align = "center",
          style = function(value) {
            list(fontWeight = 700)
          }
        ),
        avg_pts = colDef(
          name = "Avg",
          minWidth = 60,
          align = "center",
          format = colFormat(digits = 1)
        ),
        ppm = colDef(
          name = "PPM",
          minWidth = 60,
          align = "center",
          format = colFormat(digits = 2)
        ),
        x1_mp = colDef(
          name = "Apps",
          minWidth = 55,
          align = "center"
        ),
        mins_played = colDef(
          name = "Mins",
          minWidth = 60,
          align = "center"
        ),
        goals = colDef(name = "G", minWidth = 45, align = "center"),
        sot = colDef(name = "SoT", minWidth = 50, align = "center"),
        assists = colDef(name = "A", minWidth = 45, align = "center"),
        cs = colDef(name = "CS", minWidth = 45, align = "center"),
        saves = colDef(name = "Sv", minWidth = 45, align = "center")
      )
      
      # Filter to columns that exist
      col_defs <- col_defs[names(col_defs) %in% names(data)]
      
      reactable(
        data,
        theme = app_reactable_theme(),
        columns = col_defs,
        defaultColDef = colDef(
          minWidth = 50,
          align = "center"
        ),
        searchable = TRUE,
        filterable = FALSE,
        sortable = TRUE,
        defaultSorted = "total_pts",
        defaultSortOrder = "desc",
        pagination = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE
      )
    })
    
    # =========================================================================
    # SECTION 2: PLAYER COMPARISON
    # =========================================================================
    
    output$comparison_content <- renderUI({
      players <- input$compare_players
      
      if (is.null(players) || length(players) == 0) {
        return(div(
          style = "text-align: center; padding: 3rem; color: var(--text-muted);",
          icon("users", style = "font-size: 2rem; margin-bottom: 1rem;"),
          tags$p("Select players above to compare their gameweek performance")
        ))
      }
      
      tagList(
        plotOutput(ns("comparison_plot"), height = "400px"),
        tags$hr(style = "margin: 1rem 0; border-color: var(--bg-secondary);"),
        reactableOutput(ns("comparison_table"))
      )
    })
    
    comparison_data <- reactive({
      req(rv$detail_data, input$compare_players, input$gw_range)
      req(length(input$compare_players) > 0)
      
      data <- rv$detail_data
      
      # Get gameweek column
      gw_col <- if ("gw" %in% names(data)) "gw"
      else if ("gameweek" %in% names(data)) "gameweek"
      else return(NULL)
      
      # Filter by selected players and GW range
      data <- data %>%
        filter(name %in% input$compare_players) %>%
        filter(!!sym(gw_col) >= input$gw_range[1] & !!sym(gw_col) <= input$gw_range[2])
      
      return(data)
    })
    
    output$comparison_plot <- renderPlot({
      data <- comparison_data()
      req(data, nrow(data) > 0)
      
      metric <- input$compare_metric
      if (is.null(metric) || !metric %in% names(data)) {
        # Try to find a valid metric
        metric <- intersect(c("total_pts", "pts", "goals", "mins_played"), names(data))[1]
        if (is.null(metric)) return(NULL)
      }
      
      gw_col <- if ("gw" %in% names(data)) "gw" else "gameweek"
      
      # Prepare data for plotting
      plot_data <- data %>%
        mutate(
          gw_num = as.numeric(!!sym(gw_col)),
          metric_value = as.numeric(!!sym(metric))
        ) %>%
        filter(!is.na(gw_num), !is.na(metric_value))
      
      if (nrow(plot_data) == 0) return(NULL)
      
      # Get metric label
      metric_labels <- c(
        "total_pts" = "FanTeam Points",
        "pts" = "Points",
        "mins_played" = "Minutes",
        "goals" = "Goals",
        "assists" = "Assists",
        "sot" = "Shots on Target",
        "cs" = "Clean Sheets",
        "saves" = "Saves"
      )
      metric_label <- if (metric %in% names(metric_labels)) metric_labels[[metric]] else metric
      
      # Create color palette
      n_players <- length(unique(plot_data$name))
      if (n_players <= 8) {
        player_colors <- c(
          APP_COLORS$sage,
          APP_COLORS$coral,
          APP_COLORS$primary,
          "#6B8E23",
          "#4682B4",
          "#9370DB",
          "#20B2AA",
          "#CD853F"
        )[1:n_players]
      } else {
        player_colors <- scales::hue_pal()(n_players)
      }
      names(player_colors) <- unique(plot_data$name)
      
      # Create plot
      ggplot(plot_data, aes(x = gw_num, y = metric_value, color = name, group = name)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 3) +
        scale_color_manual(values = player_colors) +
        scale_x_continuous(breaks = seq(min(plot_data$gw_num), max(plot_data$gw_num), by = 1)) +
        labs(
          y = metric_label,
          x = "Gameweek",
          color = NULL
        ) +
        theme_app() +
        theme(
          legend.position = "top",
          legend.text = element_text(size = 11, face = "bold"),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11)
        )
    }, bg = "transparent")
    
    output$comparison_table <- renderReactable({
      data <- comparison_data()
      req(data, nrow(data) > 0)
      
      metric <- input$compare_metric
      if (is.null(metric) || !metric %in% names(data)) return(NULL)
      
      # Summarize by player
      summary_data <- data %>%
        group_by(name) %>%
        summarise(
          games = n(),
          total = sum(!!sym(metric), na.rm = TRUE),
          avg = mean(!!sym(metric), na.rm = TRUE),
          max = max(!!sym(metric), na.rm = TRUE),
          min = min(!!sym(metric), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(total))
      
      reactable(
        summary_data,
        theme = app_reactable_theme(),
        columns = list(
          name = colDef(name = "Player", minWidth = 150, style = list(fontWeight = 600)),
          games = colDef(name = "GW", minWidth = 60, align = "center"),
          total = colDef(name = "Total", minWidth = 80, align = "center", format = colFormat(digits = 0)),
          avg = colDef(name = "Avg", minWidth = 80, align = "center", format = colFormat(digits = 1)),
          max = colDef(name = "Max", minWidth = 70, align = "center", format = colFormat(digits = 0)),
          min = colDef(name = "Min", minWidth = 70, align = "center", format = colFormat(digits = 0))
        ),
        striped = TRUE,
        compact = TRUE,
        pagination = FALSE
      )
    })
    
    # =========================================================================
    # SECTION 3: GAMEWEEK DETAIL
    # =========================================================================
    
    filtered_detail <- reactive({
      req(rv$detail_data, input$detail_gw)
      
      data <- rv$detail_data
      
      # Get gameweek column
      gw_col <- if ("gw" %in% names(data)) "gw"
      else if ("gameweek" %in% names(data)) "gameweek"
      else return(NULL)
      
      # Filter by selected gameweek
      data <- data %>% filter(as.character(!!sym(gw_col)) == input$detail_gw)
      
      # Position filter
      pos_filter <- input$detail_position
      if (!is.null(pos_filter) && !"All" %in% pos_filter && length(pos_filter) > 0) {
        data <- data %>% filter(pos %in% pos_filter)
      }
      
      # Team filter
      team_filter <- input$detail_team
      if (!is.null(team_filter) && length(team_filter) > 0) {
        data <- data %>% filter(team %in% team_filter)
      }
      
      return(data)
    })
    
    output$detail_count <- renderText({
      data <- filtered_detail()
      if (is.null(data)) return("")
      sprintf("Showing %d players", nrow(data))
    })
    
    output$detail_table <- renderReactable({
      data <- filtered_detail()
      req(data, nrow(data) > 0)
      
      log_debug("Rendering detail table with", nrow(data), "rows", level = "DEBUG")
      
      # Select key columns for display (adjust based on actual columns)
      display_cols <- c("name", "team", "pos", "pts", "mins", "goals", "sot", 
                        "assists", "cs", "saves", "gc", "yel", "og", "bonus")
      
      # Keep only columns that exist
      display_cols <- intersect(display_cols, names(data))
      
      if (length(display_cols) == 0) {
        # Fall back to showing all columns except gw
        display_cols <- setdiff(names(data), c("gw", "gameweek", "scrape_date", "id"))
      }
      
      data <- data %>% select(all_of(display_cols))
      
      # Build column definitions
      col_defs <- list()
      
      if ("name" %in% names(data)) {
        col_defs$name <- colDef(
          name = "Player",
          minWidth = 160,
          sticky = "left",
          style = list(fontWeight = 600, background = "#fff")
        )
      }
      
      if ("team" %in% names(data)) {
        col_defs$team <- colDef(
          name = "Team",
          minWidth = 140,
          cell = function(value) {
            logo <- get_soccer_team_logo(value)
            if (!is.null(logo)) {
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                tags$img(src = logo, style = "width: 20px; height: 20px; object-fit: contain;"),
                span(value)
              )
            } else {
              value
            }
          }
        )
      }
      
      if ("pos" %in% names(data)) {
        col_defs$pos <- colDef(
          name = "Pos",
          minWidth = 55,
          align = "center",
          cell = function(value) {
            color <- get_position_color(value)
            div(
              style = sprintf("background: %s; color: white; padding: 2px 8px; border-radius: 4px; font-size: 0.75rem; font-weight: 700;", color),
              value
            )
          }
        )
      }
      
      if ("pts" %in% names(data)) {
        col_defs$pts <- colDef(name = "Pts", minWidth = 55, align = "center", style = list(fontWeight = 700))
      }
      
      if ("mins" %in% names(data)) {
        col_defs$mins <- colDef(name = "Mins", minWidth = 55, align = "center")
      }
      
      # Standard stat columns
      stat_cols <- c(
        "goals" = "G", "sot" = "SoT", "assists" = "A", "cs" = "CS",
        "saves" = "Sv", "gc" = "GC", "yel" = "Y", "og" = "OG", "bonus" = "Bon"
      )
      
      for (col_name in names(stat_cols)) {
        if (col_name %in% names(data)) {
          col_defs[[col_name]] <- colDef(
            name = stat_cols[[col_name]],
            minWidth = 45,
            align = "center"
          )
        }
      }
      
      reactable(
        data,
        theme = app_reactable_theme(),
        columns = col_defs,
        defaultColDef = colDef(
          minWidth = 50,
          align = "center"
        ),
        searchable = TRUE,
        sortable = TRUE,
        defaultSorted = if ("pts" %in% names(data)) "pts" else names(data)[1],
        defaultSortOrder = "desc",
        pagination = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE
      )
    })
    
  })
}