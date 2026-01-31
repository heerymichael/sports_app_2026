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
                 "Min Apps",
                 value = 1,
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
      
      reactableOutput(ns("overview_table")),
      
      # Comprehensive methodology explanation
      div(
        style = "margin-top: 1.25rem; padding: 1rem 1.25rem; background: var(--bg-secondary); border-radius: 8px; font-size: 0.82rem; color: var(--text-secondary); line-height: 1.6;",
        
        # Section 1: Sortino Ratio
        div(
          style = "margin-bottom: 0.75rem;",
          tags$strong("1. Sortino Ratio — Measuring Quality", style = "color: var(--text-primary); font-size: 0.9rem;"),
          tags$p(
            style = "margin: 0.4rem 0 0 0;",
            "Standard deviation penalizes ALL variance equally, but in DFS, upside variance is ",
            tags$em("good"),
            ". A player who scores 5, 5, 5, 15 has the same SD as one who scores 5, 5, 5, 0 — but they're clearly not equivalent. ",
            tags$strong("Sortino"),
            " fixes this by only measuring ",
            tags$em("downside"),
            " deviation (games below their average). The formula is simply: ",
            tags$code("PPG ÷ Downside Deviation", style = "background: #e8e4df; padding: 1px 4px; border-radius: 3px;"),
            ". Higher = more reliable points without penalizing ceiling games."
          )
        ),
        
        # Section 2: Bayesian Adjustment
        div(
          style = "margin-bottom: 0.75rem;",
          tags$strong("2. Bayesian Adjustment — Handling Small Samples", style = "color: var(--text-primary); font-size: 0.9rem;"),
          tags$p(
            style = "margin: 0.4rem 0 0 0;",
            "A player with 4 games including one 18-point haul looks elite, but that's likely noise. Raw Sortino would overrate them. We apply ",
            tags$strong("Bayesian shrinkage"),
            ": each player's Sortino is pulled toward their ",
            tags$em("position average"),
            " based on sample size. The formula: "
          ),
          tags$p(
            style = "margin: 0.3rem 0; padding-left: 1rem; font-family: monospace; font-size: 0.78rem;",
            "Adjusted = (Games × Raw_Sortino + 10 × Position_Avg) ÷ (Games + 10)"
          ),
          tags$p(
            style = "margin: 0.3rem 0 0 0;",
            "With 5 games, a player keeps ~33% of their individual Sortino; with 20 games, ~67%. This means: ",
            tags$strong("players with few games regress toward 'average for their position'"),
            " while established players keep most of their rating. No arbitrary cutoffs — the math handles it smoothly."
          )
        ),
        
        # Section 3: Floor & Ceiling
        div(
          style = "margin-bottom: 0.75rem;",
          tags$strong("3. Floor & Ceiling — Realistic Range", style = "color: var(--text-primary); font-size: 0.9rem;"),
          tags$p(
            style = "margin: 0.4rem 0 0 0;",
            tags$strong("Floor"),
            " = 2nd worst score; ",
            tags$strong("Ceiling"),
            " = 2nd best score. We ignore one outlier each way to show the ",
            tags$em("realistic"),
            " range rather than freak results. (Players with <4 games show actual min/max.)"
          )
        ),
        
        # Section 4: Cash & GPP Grades
        div(
          style = "margin-bottom: 0.5rem;",
          tags$strong("4. Cash & GPP Grades — Contest-Specific Ratings", style = "color: var(--text-primary); font-size: 0.9rem;"),
          tags$p(
            style = "margin: 0.4rem 0 0 0;",
            "Grades combine metrics using ",
            tags$strong("position-relative percentiles"),
            " — a midfielder competes with midfielders, not goalkeepers. Each grade uses weightings tailored to the contest type:"
          ),
          # Cash weighting box
          div(
            style = "margin: 0.5rem 0 0.3rem 0; padding: 0.5rem 0.75rem; background: #E8EFE2; border-radius: 4px; border-left: 3px solid #A3BE8C;",
            tags$strong("Cash", style = "color: var(--text-primary);"),
            tags$span(" = ", style = "color: var(--text-secondary);"),
            tags$span("40% Floor", style = "font-weight: 600;"),
            tags$span(" + ", style = "color: var(--text-muted);"),
            tags$span("40% Sortino", style = "font-weight: 600;"),
            tags$span(" + ", style = "color: var(--text-muted);"),
            tags$span("20% PPG", style = "font-weight: 600;"),
            tags$br(),
            tags$span("Prioritizes reliability: high floor + consistent scoring. PPG weighted lower because volatile high-scorers can hurt cash lineups.", style = "font-size: 0.75rem; color: var(--text-muted); font-style: italic;")
          ),
          # GPP weighting box
          div(
            style = "margin: 0.3rem 0 0.5rem 0; padding: 0.5rem 0.75rem; background: #FDF8F6; border-radius: 4px; border-left: 3px solid #D08770;",
            tags$strong("GPP", style = "color: var(--text-primary);"),
            tags$span(" = ", style = "color: var(--text-secondary);"),
            tags$span("40% Ceiling", style = "font-weight: 600;"),
            tags$span(" + ", style = "color: var(--text-muted);"),
            tags$span("40% PPG", style = "font-weight: 600;"),
            tags$span(" + ", style = "color: var(--text-muted);"),
            tags$span("20% Sortino", style = "font-weight: 600;"),
            tags$br(),
            tags$span("Prioritizes upside: high ceiling + strong production. Sortino kept at 20% because even in 600-person GPPs you need quality, not pure dart throws.", style = "font-size: 0.75rem; color: var(--text-muted); font-style: italic;")
          ),
          tags$p(
            style = "margin: 0.3rem 0 0 0;",
            "A player can be ",
            tags$strong("Cash A / GPP C"),
            " (reliable but low ceiling) or ",
            tags$strong("Cash C / GPP A"),
            " (volatile but explosive). Look for mismatches to find edges for specific contest types."
          )
        ),
        
        # Color coding key with diverging palette
        div(
          style = "padding-top: 0.5rem; border-top: 1px solid var(--border);",
          tags$strong("Sortino: ", style = "color: var(--text-primary);"),
          tags$span("≥3.0 ", style = "color: #A3BE8C; font-weight: 700;"),
          tags$span("Elite ", style = "color: var(--text-secondary);"),
          tags$span("| 1.5–3.0 ", style = "font-weight: 600;"),
          tags$span("Average ", style = "color: var(--text-secondary);"),
          tags$span("| <1.5 ", style = "color: #D08770; font-weight: 700;"),
          tags$span("Volatile   ", style = "color: var(--text-secondary);"),
          tags$span(" · ", style = "color: var(--border-dark);"),
          tags$strong("Grades: ", style = "color: var(--text-primary);"),
          span(style = "background: #A3BE8C; color: white; padding: 1px 5px; border-radius: 3px; font-size: 0.75rem; font-weight: 700;", "A+"),
          span(style = "background: #BACEA9; color: white; padding: 1px 5px; border-radius: 3px; font-size: 0.75rem; font-weight: 700; margin-left: 2px;", "A"),
          span(style = "background: #D1DFC6; padding: 1px 5px; border-radius: 3px; font-size: 0.75rem; font-weight: 700; margin-left: 2px;", "B+"),
          span(style = "background: #E8EFE2; padding: 1px 5px; border-radius: 3px; font-size: 0.75rem; font-weight: 700; margin-left: 2px;", "B"),
          span(style = "background: #FFFFFF; border: 1px solid #E5E9F0; padding: 1px 5px; border-radius: 3px; font-size: 0.75rem; font-weight: 700; margin-left: 2px;", "C"),
          span(style = "background: #F3E1DB; padding: 1px 5px; border-radius: 3px; font-size: 0.75rem; font-weight: 700; margin-left: 2px;", "D"),
          span(style = "background: #D08770; color: white; padding: 1px 5px; border-radius: 3px; font-size: 0.75rem; font-weight: 700; margin-left: 2px;", "F")
        )
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # Section 2: Player Comparison (4 columns)
    # =========================================================================
    ui_card(
      title = "Player Comparison by Gameweek",
      color = "sage",
      
      # Custom styles for slider
      tags$style(HTML("
        .player-stats-slider .irs--shiny .irs-bar {
          background: var(--accent-sage);
          border-top: 1px solid var(--accent-sage);
          border-bottom: 1px solid var(--accent-sage);
        }
        .player-stats-slider .irs--shiny .irs-from,
        .player-stats-slider .irs--shiny .irs-to,
        .player-stats-slider .irs--shiny .irs-single {
          background: var(--accent-sage);
        }
        .player-stats-slider .irs--shiny .irs-handle {
          border: 2px solid var(--accent-sage);
          background: white;
        }
        .player-stats-slider .irs--shiny .irs-line {
          background: var(--bg-secondary);
        }
      ")),
      
      # Four player selector columns
      fluidRow(
        column(3,
               tags$label("Player 1", class = "control-label", style = "font-weight: 600;"),
               shinyWidgets::pickerInput(
                 ns("compare_team_1"),
                 NULL,
                 choices = NULL,
                 selected = NULL,
                 options = shinyWidgets::pickerOptions(
                   liveSearch = TRUE,
                   noneSelectedText = "Select Team"
                 )
               ),
               shinyWidgets::pickerInput(
                 ns("compare_player_1"),
                 NULL,
                 choices = NULL,
                 selected = NULL,
                 options = shinyWidgets::pickerOptions(
                   liveSearch = TRUE,
                   noneSelectedText = "Select Player"
                 )
               )
        ),
        column(3,
               div(
                 id = ns("player_2_container"),
                 style = "opacity: 0.5;",
                 tags$label("Player 2", class = "control-label", style = "font-weight: 600;"),
                 shinyWidgets::pickerInput(
                   ns("compare_team_2"),
                   NULL,
                   choices = NULL,
                   selected = NULL,
                   options = shinyWidgets::pickerOptions(
                     liveSearch = TRUE,
                     noneSelectedText = "Select Team"
                   )
                 ),
                 shinyWidgets::pickerInput(
                   ns("compare_player_2"),
                   NULL,
                   choices = NULL,
                   selected = NULL,
                   options = shinyWidgets::pickerOptions(
                     liveSearch = TRUE,
                     noneSelectedText = "Select Player"
                   )
                 )
               )
        ),
        column(3,
               div(
                 id = ns("player_3_container"),
                 style = "opacity: 0.5;",
                 tags$label("Player 3", class = "control-label", style = "font-weight: 600;"),
                 shinyWidgets::pickerInput(
                   ns("compare_team_3"),
                   NULL,
                   choices = NULL,
                   selected = NULL,
                   options = shinyWidgets::pickerOptions(
                     liveSearch = TRUE,
                     noneSelectedText = "Select Team"
                   )
                 ),
                 shinyWidgets::pickerInput(
                   ns("compare_player_3"),
                   NULL,
                   choices = NULL,
                   selected = NULL,
                   options = shinyWidgets::pickerOptions(
                     liveSearch = TRUE,
                     noneSelectedText = "Select Player"
                   )
                 )
               )
        ),
        column(3,
               div(
                 id = ns("player_4_container"),
                 style = "opacity: 0.5;",
                 tags$label("Player 4", class = "control-label", style = "font-weight: 600;"),
                 shinyWidgets::pickerInput(
                   ns("compare_team_4"),
                   NULL,
                   choices = NULL,
                   selected = NULL,
                   options = shinyWidgets::pickerOptions(
                     liveSearch = TRUE,
                     noneSelectedText = "Select Team"
                   )
                 ),
                 shinyWidgets::pickerInput(
                   ns("compare_player_4"),
                   NULL,
                   choices = NULL,
                   selected = NULL,
                   options = shinyWidgets::pickerOptions(
                     liveSearch = TRUE,
                     noneSelectedText = "Select Player"
                   )
                 )
               )
        )
      ),
      
      tags$hr(style = "margin: 0.5rem 0 1rem 0; border-color: var(--bg-secondary);"),
      
      # Chart controls row
      fluidRow(
        column(4,
               div(class = "player-stats-slider",
                   sliderInput(
                     ns("gw_range"),
                     "Gameweek Range",
                     min = 1,
                     max = 38,
                     value = c(1, 22),
                     step = 1
                   )
               )
        ),
        column(4,
               shinyWidgets::pickerInput(
                 ns("compare_metric"),
                 "Metric",
                 choices = c(
                   "FanTeam Points" = "pts",
                   "Minutes" = "mins",
                   "Goals" = "goals",
                   "Assists" = "assists",
                   "Shots on Target" = "sot",
                   "Clean Sheets" = "cs",
                   "Saves" = "saves"
                 ),
                 selected = "pts"
               )
        ),
        column(4,
               shinyWidgets::pickerInput(
                 ns("chart_type"),
                 "Chart Type",
                 choices = c(
                   "Line Chart" = "line",
                   "Bar Charts" = "bar"
                 ),
                 selected = "line"
               )
        )
      ),
      
      tags$hr(style = "margin: 0.5rem 0 1rem 0; border-color: var(--bg-secondary);"),
      
      uiOutput(ns("comparison_content"))
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
        if (!is.null(rv$overview_data)) {
          log_debug("  Overview:", nrow(rv$overview_data), "rows", level = "INFO")
          log_debug("  Overview columns:", paste(names(rv$overview_data), collapse = ", "), level = "DEBUG")
          
          # Check for Haaland
          haaland_rows <- rv$overview_data %>% filter(grepl("haaland", tolower(name)))
          if (nrow(haaland_rows) > 0) {
            log_debug("  Found Haaland in overview:", nrow(haaland_rows), "rows", level = "DEBUG")
            log_debug("    x1_mp value:", haaland_rows$x1_mp[1], "type:", class(haaland_rows$x1_mp[1]), level = "DEBUG")
          } else {
            log_debug("  WARNING: Haaland NOT found in overview data!", level = "WARN")
          }
          
          # Check x1_mp column
          if ("x1_mp" %in% names(rv$overview_data)) {
            x1mp_summary <- summary(as.numeric(rv$overview_data$x1_mp))
            log_debug("  x1_mp summary - Min:", x1mp_summary[1], "Max:", x1mp_summary[6], "NAs:", sum(is.na(rv$overview_data$x1_mp)), level = "DEBUG")
          }
        }
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
    
    # Update min_games default to 50% of max appearances
    observe({
      req(rv$overview_data)
      
      if ("x1_mp" %in% names(rv$overview_data)) {
        max_apps <- max(as.numeric(rv$overview_data$x1_mp), na.rm = TRUE)
        default_min <- max(1, floor(max_apps * 0.5))
        
        updateNumericInput(
          session,
          "min_games",
          value = default_min,
          max = max_apps
        )
      }
    }) %>% bindEvent(rv$initialized, once = TRUE)
    
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
        
        # Update all 4 comparison team selectors
        for (i in 1:4) {
          shinyWidgets::updatePickerInput(
            session, paste0("compare_team_", i),
            choices = teams,
            selected = NULL,
            choicesOpt = list(content = team_content)
          )
        }
      }
    })
    
    # =========================================================================
    # UPDATE PLAYER CHOICES BASED ON TEAM SELECTION
    # =========================================================================
    
    # Helper function for updating player dropdown based on team
    update_player_choices <- function(team_input_id, player_input_id) {
      observe({
        req(rv$overview_data)
        
        team <- input[[team_input_id]]
        data <- rv$overview_data
        
        if (!is.null(team) && team != "") {
          data <- data %>% filter(team == !!team)
        }
        
        # Get players sorted by total_pts descending
        players <- data %>%
          arrange(desc(total_pts)) %>%
          pull(name) %>%
          unique()
        
        players <- players[!is.na(players) & players != ""]
        
        shinyWidgets::updatePickerInput(
          session, player_input_id,
          choices = players,
          selected = NULL
        )
      })
    }
    
    update_player_choices("compare_team_1", "compare_player_1")
    update_player_choices("compare_team_2", "compare_player_2")
    update_player_choices("compare_team_3", "compare_player_3")
    update_player_choices("compare_team_4", "compare_player_4")
    
    # =========================================================================
    # UPDATE GAMEWEEK SLIDER
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
        }
      }
    })
    
    # =========================================================================
    # SECTION 1: OVERVIEW TABLE
    # =========================================================================
    
    filtered_overview <- reactive({
      req(rv$overview_data)
      
      data <- rv$overview_data
      
      log_debug("Overview data before filters:", nrow(data), "rows", level = "DEBUG")
      log_debug("Overview columns:", paste(names(data), collapse = ", "), level = "DEBUG")
      
      # Position filter
      pos_filter <- input$position_filter
      if (!is.null(pos_filter) && !"All" %in% pos_filter && length(pos_filter) > 0) {
        data <- data %>% filter(pos %in% pos_filter)
        log_debug("After position filter:", nrow(data), "rows", level = "DEBUG")
      }
      
      # Min games filter - count non-zero minute appearances
      min_games <- input$min_games
      if (!is.null(min_games) && min_games > 0) {
        # Use x1_mp (1+ minute appearances) if available
        if ("x1_mp" %in% names(data)) {
          # Convert to numeric to handle character columns
          data <- data %>% 
            mutate(x1_mp_num = as.numeric(x1_mp)) %>%
            filter(!is.na(x1_mp_num) & x1_mp_num >= min_games) %>%
            select(-x1_mp_num)
          log_debug("After min games filter (x1_mp >=", min_games, "):", nrow(data), "rows", level = "DEBUG")
        }
      }
      
      # Team filter
      team_filter <- input$overview_team_filter
      if (!is.null(team_filter) && length(team_filter) > 0) {
        data <- data %>% filter(team %in% team_filter)
        log_debug("After team filter:", nrow(data), "rows", level = "DEBUG")
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
      
      # Get detail data for std dev and histogram calculations
      detail <- rv$detail_data
      
      # Calculate stats from detail data: Sortino ratio, trimmed floor/ceiling, distribution
      if (!is.null(detail) && nrow(detail) > 0) {
        # Ensure pts column is numeric
        pts_col <- if ("pts" %in% names(detail)) "pts" else if ("total_pts" %in% names(detail)) "total_pts" else NULL
        
        if (!is.null(pts_col)) {
          detail[[pts_col]] <- as.numeric(detail[[pts_col]])
          
          # Calculate stats per player
          player_stats <- detail %>%
            group_by(name) %>%
            summarise(
              n_games = n(),
              player_avg = mean(.data[[pts_col]], na.rm = TRUE),
              # Trimmed floor/ceiling: 2nd worst/best if 4+ games, else min/max
              pts_floor = {
                pts <- sort(.data[[pts_col]][!is.na(.data[[pts_col]])])
                if (length(pts) >= 4) pts[2] else if (length(pts) > 0) pts[1] else NA_real_
              },
              pts_ceiling = {
                pts <- sort(.data[[pts_col]][!is.na(.data[[pts_col]])], decreasing = TRUE)
                if (length(pts) >= 4) pts[2] else if (length(pts) > 0) pts[1] else NA_real_
              },
              # Downside deviation for Sortino (only penalizes scores below average)
              downside_dev = {
                pts <- .data[[pts_col]][!is.na(.data[[pts_col]])]
                avg <- mean(pts, na.rm = TRUE)
                below_avg <- pts[pts < avg]
                if (length(below_avg) > 1) {
                  sqrt(mean((below_avg - avg)^2))
                } else {
                  NA_real_
                }
              },
              pts_list = list(.data[[pts_col]][!is.na(.data[[pts_col]])]),
              .groups = "drop"
            )
          
          # Join to main data
          data <- data %>%
            left_join(player_stats, by = "name")
          
          # Calculate raw Sortino ratio (PPG / Downside Deviation)
          data <- data %>%
            mutate(
              sortino_raw = if_else(
                !is.na(avg_pts) & !is.na(downside_dev) & downside_dev > 0,
                as.numeric(avg_pts) / downside_dev,
                NA_real_
              )
            )
          
          # Bayesian shrinkage: pull Sortino toward position average based on sample size
          # Formula: adjusted = (n * player_sortino + k * pos_avg) / (n + k)
          # k = 10 represents "equivalent games" of prior belief
          k_prior <- 10
          
          # Calculate position averages (only from players with sufficient games)
          pos_averages <- data %>%
            filter(n_games >= 6, !is.na(sortino_raw)) %>%
            group_by(pos) %>%
            summarise(pos_avg_sortino = mean(sortino_raw, na.rm = TRUE), .groups = "drop")
          
          # Join position averages and apply shrinkage
          data <- data %>%
            left_join(pos_averages, by = "pos") %>%
            mutate(
              # If no position average available, use global average
              pos_avg_sortino = if_else(
                is.na(pos_avg_sortino),
                mean(sortino_raw, na.rm = TRUE),
                pos_avg_sortino
              ),
              # Bayesian adjusted Sortino
              sortino = if_else(
                !is.na(sortino_raw) & !is.na(n_games),
                (n_games * sortino_raw + k_prior * pos_avg_sortino) / (n_games + k_prior),
                NA_real_
              )
            )
        }
      }
      
      # If no detail data, add empty columns
      if (!"sortino" %in% names(data)) {
        data$pts_floor <- NA_real_
        data$pts_ceiling <- NA_real_
        data$sortino_raw <- NA_real_
        data$sortino <- NA_real_
        data$n_games <- NA_integer_
        data$pts_list <- list(NULL)
      }
      
      # Calculate Cash and GPP grades using position-relative percentiles
      # Cash: 40% Floor + 40% Sortino + 20% PPG (prioritize reliability)
      # GPP:  40% Ceiling + 40% PPG + 20% Sortino (prioritize upside + production)
      if (all(c("pts_floor", "pts_ceiling", "sortino", "avg_pts", "pos") %in% names(data))) {
        data <- data %>%
          group_by(pos) %>%
          mutate(
            # Position-relative percentiles (0-100)
            pctl_floor = percent_rank(pts_floor) * 100,
            pctl_ceiling = percent_rank(pts_ceiling) * 100,
            pctl_ppg = percent_rank(avg_pts) * 100,
            pctl_sortino = percent_rank(sortino) * 100
          ) %>%
          ungroup() %>%
          mutate(
            # Weighted composite scores
            cash_score = 0.40 * pctl_floor + 0.40 * pctl_sortino + 0.20 * pctl_ppg,
            gpp_score = 0.40 * pctl_ceiling + 0.40 * pctl_ppg + 0.20 * pctl_sortino,
            # Convert to letter grades
            cash_grade = case_when(
              is.na(cash_score) ~ NA_character_,
              cash_score >= 90 ~ "A+",
              cash_score >= 80 ~ "A",
              cash_score >= 70 ~ "B+",
              cash_score >= 60 ~ "B",
              cash_score >= 50 ~ "C+",
              cash_score >= 40 ~ "C",
              cash_score >= 30 ~ "D",
              TRUE ~ "F"
            ),
            gpp_grade = case_when(
              is.na(gpp_score) ~ NA_character_,
              gpp_score >= 90 ~ "A+",
              gpp_score >= 80 ~ "A",
              gpp_score >= 70 ~ "B+",
              gpp_score >= 60 ~ "B",
              gpp_score >= 50 ~ "C+",
              gpp_score >= 40 ~ "C",
              gpp_score >= 30 ~ "D",
              TRUE ~ "F"
            )
          )
      }
      
      # Select columns for display
      display_cols <- c("team", "name", "pos", "x1_mp", "mins_played", "total_pts", "avg_pts", 
                        "pts_floor", "pts_ceiling", "sortino", "cash_grade", "gpp_grade", "pts_list")
      display_cols <- intersect(display_cols, names(data))
      data <- data %>% select(all_of(display_cols))
      
      # Ensure numeric columns are numeric
      numeric_cols <- c("x1_mp", "mins_played", "total_pts", "avg_pts", "pts_floor", "pts_ceiling", "sortino")
      for (col in intersect(numeric_cols, names(data))) {
        data[[col]] <- as.numeric(data[[col]])
      }
      
      # Calculate global x-axis range for histograms (standardized across all players)
      all_pts <- unlist(data$pts_list)
      if (length(all_pts) > 0 && !all(is.na(all_pts))) {
        hist_min <- floor(min(all_pts, na.rm = TRUE) / 5) * 5
        hist_max <- ceiling(max(all_pts, na.rm = TRUE) / 5) * 5
      } else {
        hist_min <- 0
        hist_max <- 20
      }
      
      # Calculate global max bin count across all players for consistent bar heights
      n_bins <- 10
      bin_width <- (hist_max - hist_min) / n_bins
      breaks <- seq(hist_min, hist_max, by = bin_width)
      
      global_max_count <- 1
      for (pts_vec in data$pts_list) {
        pts <- unlist(pts_vec)
        if (!is.null(pts) && length(pts) > 0 && !all(is.na(pts))) {
          h <- hist(pts, breaks = breaks, plot = FALSE)
          global_max_count <- max(global_max_count, max(h$counts))
        }
      }
      
      # Store for use in cell renderer
      table_df <- as.data.frame(data)
      
      # Column definitions
      col_defs <- list(
        team = colDef(
          name = "Team",
          minWidth = 60,
          maxWidth = 80,
          align = "center",
          cell = function(value) {
            logo <- get_soccer_team_logo(value)
            if (!is.null(logo)) {
              tags$img(src = logo, style = "width: 24px; height: 24px; object-fit: contain;")
            } else {
              ""
            }
          }
        ),
        name = colDef(
          name = "Player",
          minWidth = 160,
          align = "left",
          sticky = "left",
          style = list(fontWeight = 600, background = "#fff")
        ),
        pos = colDef(
          name = "Pos",
          minWidth = 55,
          maxWidth = 65,
          align = "center",
          cell = function(value) {
            color <- get_position_color(value)
            div(
              style = sprintf("background: %s; color: white; padding: 2px 8px; border-radius: 4px; font-size: 0.75rem; font-weight: 700;", color),
              value
            )
          }
        ),
        x1_mp = colDef(
          name = "Apps",
          minWidth = 55,
          maxWidth = 65,
          align = "center"
        ),
        mins_played = colDef(
          name = "Mins",
          minWidth = 60,
          maxWidth = 75,
          align = "center"
        ),
        total_pts = colDef(
          name = "Total",
          minWidth = 65,
          maxWidth = 80,
          align = "center",
          style = function(value) list(fontWeight = 700),
          cell = function(value) {
            if (is.null(value) || is.na(value)) return("-")
            sprintf("%.1f", value)
          }
        ),
        avg_pts = colDef(
          name = "PPG",
          minWidth = 60,
          maxWidth = 75,
          align = "center",
          style = function(value) list(fontWeight = 700),
          format = colFormat(digits = 1)
        ),
        pts_floor = colDef(
          name = "Floor",
          minWidth = 55,
          maxWidth = 70,
          align = "center",
          cell = function(value) {
            if (is.null(value) || is.na(value)) return("-")
            sprintf("%.1f", value)
          }
        ),
        pts_ceiling = colDef(
          name = "Ceil",
          minWidth = 55,
          maxWidth = 70,
          align = "center",
          cell = function(value) {
            if (is.null(value) || is.na(value)) return("-")
            sprintf("%.1f", value)
          }
        ),
        sortino = colDef(
          name = "Sortino",
          minWidth = 65,
          maxWidth = 80,
          align = "center",
          cell = function(value) {
            if (is.null(value) || is.na(value)) return("-")
            # Diverging palette: Coral (low) → neutral → Sage (high)
            color <- if (value >= 3.0) {
              "#A3BE8C"  # sage - matches grade A+
            } else if (value < 1.5) {
              "#D08770"  # coral - matches grade F
            } else {
              APP_COLORS$primary
            }
            span(
              style = list(fontWeight = 600, color = color),
              sprintf("%.2f", value)
            )
          }
        ),
        cash_grade = colDef(
          name = "Cash",
          minWidth = 55,
          maxWidth = 65,
          align = "center",
          cell = function(value) {
            if (is.null(value) || is.na(value)) return("-")
            # Diverging palette: Coral (bad) → White (neutral) → Sage (good)
            # Using APP_COLORS: coral=#D08770, sage=#A3BE8C
            bg_color <- switch(value,
                               "A+" = "#A3BE8C",   # sage (full)
                               "A"  = "#BACEA9",   # 75% toward sage
                               "B+" = "#D1DFC6",   # 50% toward sage
                               "B"  = "#E8EFE2",   # 25% toward sage
                               "C+" = "#FFFFFF",   # white (neutral)
                               "C"  = "#F3E1DB",   # 25% toward coral
                               "D"  = "#E8C3B8",   # 50% toward coral
                               "F"  = "#D08770",   # coral (full)
                               "#FFFFFF"
            )
            text_color <- if (value %in% c("A+", "A", "F")) "#FFFFFF" else APP_COLORS$primary
            div(
              style = sprintf(
                "background: %s; color: %s; padding: 2px 6px; border-radius: 4px; font-weight: 700; font-size: 0.8rem;",
                bg_color, text_color
              ),
              value
            )
          }
        ),
        gpp_grade = colDef(
          name = "GPP",
          minWidth = 55,
          maxWidth = 65,
          align = "center",
          cell = function(value) {
            if (is.null(value) || is.na(value)) return("-")
            # Diverging palette: Coral (bad) → White (neutral) → Sage (good)
            bg_color <- switch(value,
                               "A+" = "#A3BE8C",   # sage (full)
                               "A"  = "#BACEA9",   # 75% toward sage
                               "B+" = "#D1DFC6",   # 50% toward sage
                               "B"  = "#E8EFE2",   # 25% toward sage
                               "C+" = "#FFFFFF",   # white (neutral)
                               "C"  = "#F3E1DB",   # 25% toward coral
                               "D"  = "#E8C3B8",   # 50% toward coral
                               "F"  = "#D08770",   # coral (full)
                               "#FFFFFF"
            )
            text_color <- if (value %in% c("A+", "A", "F")) "#FFFFFF" else APP_COLORS$primary
            div(
              style = sprintf(
                "background: %s; color: %s; padding: 2px 6px; border-radius: 4px; font-weight: 700; font-size: 0.8rem;",
                bg_color, text_color
              ),
              value
            )
          }
        ),
        pts_list = colDef(
          name = "Points Distribution",
          minWidth = 180,
          align = "center",
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            pts <- unlist(value)
            if (is.null(pts) || length(pts) == 0 || all(is.na(pts))) {
              return(span(style = list(color = APP_COLORS$muted), "-"))
            }
            
            # Calculate histogram using pre-defined breaks
            h <- hist(pts, breaks = breaks, plot = FALSE)
            counts <- h$counts
            
            # Build bars as divs - heights based on global_max_count for actual game counts
            bar_width_px <- 14
            max_height <- 28
            
            bars <- lapply(seq_along(counts), function(i) {
              bar_height <- round((counts[i] / global_max_count) * max_height)
              
              div(
                style = sprintf(
                  "width: %dpx; height: %dpx; background: %s; border-radius: 2px; margin-right: 2px; display: inline-block; vertical-align: bottom;",
                  bar_width_px,
                  max(bar_height, 0),
                  if (counts[i] > 0) APP_COLORS$sage else "transparent"
                )
              )
            })
            
            div(
              style = "display: flex; align-items: flex-end; justify-content: center; height: 32px; border-bottom: 1px solid #D8D0C4;",
              bars
            )
          }
        )
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
    
    # Reactive to get selected players from 4 individual selectors
    selected_players <- reactive({
      players <- c(
        input$compare_player_1,
        input$compare_player_2,
        input$compare_player_3,
        input$compare_player_4
      )
      players <- players[!is.null(players) & players != "" & !is.na(players)]
      return(players)
    })
    
    output$comparison_content <- renderUI({
      players <- selected_players()
      
      if (length(players) == 0) {
        return(div(
          style = "text-align: center; padding: 3rem; color: var(--text-muted);",
          icon("users", style = "font-size: 2rem; margin-bottom: 1rem;"),
          tags$p("Select players above to compare their gameweek performance")
        ))
      }
      
      tagList(
        ggiraph::girafeOutput(ns("comparison_plot"), height = "450px"),
        tags$hr(style = "margin: 1rem 0; border-color: var(--bg-secondary);"),
        reactableOutput(ns("comparison_table"))
      )
    })
    
    comparison_data <- reactive({
      req(rv$detail_data, input$gw_range)
      
      players <- selected_players()
      req(length(players) > 0)
      
      data <- rv$detail_data
      
      # Get gameweek column
      gw_col <- if ("gw" %in% names(data)) "gw"
      else if ("gameweek" %in% names(data)) "gameweek"
      else return(NULL)
      
      # Filter by selected players and GW range
      data <- data %>%
        filter(name %in% players) %>%
        filter(!!sym(gw_col) >= input$gw_range[1] & !!sym(gw_col) <= input$gw_range[2])
      
      return(data)
    })
    
    output$comparison_plot <- ggiraph::renderGirafe({
      data <- comparison_data()
      req(data, nrow(data) > 0)
      
      metric <- input$compare_metric
      chart_type <- input$chart_type
      
      if (is.null(metric) || !metric %in% names(data)) {
        metric <- intersect(c("pts", "total_pts", "goals", "mins"), names(data))[1]
        if (is.null(metric)) return(NULL)
      }
      
      gw_col <- if ("gw" %in% names(data)) "gw" else "gameweek"
      
      # Prepare data
      plot_data <- data %>%
        mutate(
          gw_num = as.numeric(!!sym(gw_col)),
          metric_value = as.numeric(!!sym(metric))
        ) %>%
        filter(!is.na(gw_num), !is.na(metric_value))
      
      if (nrow(plot_data) == 0) return(NULL)
      
      # Metric labels
      metric_labels <- c(
        "pts" = "FanTeam Points", "total_pts" = "FanTeam Points",
        "mins" = "Minutes", "mins_played" = "Minutes",
        "goals" = "Goals", "assists" = "Assists",
        "sot" = "Shots on Target", "cs" = "Clean Sheets", "saves" = "Saves"
      )
      metric_label <- if (metric %in% names(metric_labels)) metric_labels[[metric]] else metric
      
      # Color palette
      n_players <- length(unique(plot_data$name))
      player_colors <- c(APP_COLORS$sage, APP_COLORS$coral, APP_COLORS$primary,
                         "#6B8E23", "#4682B4", "#9370DB", "#20B2AA", "#CD853F")[1:n_players]
      names(player_colors) <- unique(plot_data$name)
      
      # Build tooltips
      plot_data <- plot_data %>%
        mutate(
          tooltip_text = sprintf("<b>%s</b><br>GW %d: <b>%.0f</b> %s",
                                 name, gw_num, metric_value, metric_label)
        )
      
      # Y-axis limits
      y_max <- max(plot_data$metric_value, na.rm = TRUE) * 1.15
      y_min <- 0
      
      # X-axis breaks
      gw_breaks <- seq(min(plot_data$gw_num), max(plot_data$gw_num), by = 1)
      gw_labels <- paste0("GW", gw_breaks)
      
      # Build plot
      if (is.null(chart_type) || chart_type == "line") {
        p <- ggplot(plot_data, aes(x = gw_num, y = metric_value, color = name, group = name)) +
          geom_hline(yintercept = 0, color = APP_COLORS$primary, linewidth = 0.8) +
          ggiraph::geom_line_interactive(linewidth = 1.5, alpha = 0.8) +
          ggiraph::geom_point_interactive(
            aes(tooltip = tooltip_text, data_id = paste(name, gw_num)),
            size = 4
          ) +
          scale_color_manual(values = player_colors) +
          scale_x_continuous(
            breaks = gw_breaks,
            labels = gw_labels,
            position = "top",
            expand = expansion(mult = c(0.02, 0.02))
          ) +
          scale_y_continuous(limits = c(y_min, y_max), expand = expansion(mult = c(0, 0))) +
          labs(y = metric_label, x = NULL, color = NULL) +
          theme_app_timeseries() +
          theme(
            legend.position = "top",
            legend.text = element_text(size = 11, face = "bold"),
            axis.title.y = element_text(size = 11, face = "bold")
          )
        
      } else {
        # Faceted bar charts
        p <- ggplot(plot_data, aes(x = gw_num, y = metric_value, fill = name)) +
          geom_hline(yintercept = 0, color = APP_COLORS$primary, linewidth = 0.8) +
          ggiraph::geom_col_interactive(
            aes(tooltip = tooltip_text, data_id = paste(name, gw_num)),
            width = 0.7
          ) +
          facet_wrap(~ name, ncol = 2, scales = "fixed") +
          scale_fill_manual(values = player_colors) +
          scale_x_continuous(
            breaks = function(limits) seq(floor(min(limits)), ceiling(max(limits)), by = 2),
            position = "top",
            expand = expansion(mult = c(0.02, 0.02))
          ) +
          scale_y_continuous(limits = c(y_min, y_max), expand = expansion(mult = c(0, 0))) +
          labs(y = metric_label, x = NULL) +
          theme_app_timeseries() +
          theme(
            legend.position = "none",
            strip.text = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 11, face = "bold"),
            panel.spacing = unit(1.5, "lines")
          )
      }
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 10,
        height_svg = 5.5,
        options = list(
          ggiraph::opts_tooltip(
            css = "background-color: white; border: 2px solid #3B3226; border-radius: 6px; padding: 8px 12px; font-family: 'Plus Jakarta Sans', sans-serif; font-size: 12px; box-shadow: 3px 3px 0 rgba(59, 50, 38, 0.25);",
            use_fill = FALSE
          ),
          ggiraph::opts_hover(css = "stroke-width: 3; cursor: pointer;"),
          ggiraph::opts_hover_inv(css = "opacity: 0.3;")
        )
      )
    })
    
    output$comparison_table <- renderReactable({
      data <- comparison_data()
      req(data, nrow(data) > 0)
      
      metric <- input$compare_metric
      if (is.null(metric) || !metric %in% names(data)) {
        metric <- intersect(c("pts", "total_pts", "goals", "mins"), names(data))[1]
        if (is.null(metric)) return(NULL)
      }
      
      # Ensure metric column is numeric
      data <- data %>%
        mutate(metric_val = as.numeric(!!sym(metric)))
      
      # Summarize by player
      summary_data <- data %>%
        group_by(name) %>%
        summarise(
          games = n(),
          total = sum(metric_val, na.rm = TRUE),
          avg = mean(metric_val, na.rm = TRUE),
          max = max(metric_val, na.rm = TRUE),
          min = min(metric_val, na.rm = TRUE),
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
    
  })
}