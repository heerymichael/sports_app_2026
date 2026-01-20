# =============================================================================
# Module: Golf Showdown
# 
# Single-day lineup building for FanTeam Golf Showdown format
# Roster: 6 golfers, 100M salary cap
# Multipliers:
#   - CPT (Captain): 1.25x points (user selected)
#   - UNDERDOG: 1.25x points (automatically assigned to cheapest player)
#   - Other 4: 1.0x points
# All salaries at base rate (no salary multipliers)
# =============================================================================

library(lpSolve)

# =============================================================================
# GOLF MODULE CONSTANTS (if not already defined by Classic module)
# =============================================================================

if (!exists("GOLF_DEFAULT_HEADSHOT")) {
  GOLF_DEFAULT_HEADSHOT <- "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=200&h=146"
}

if (!exists("GOLF_CARD_COLOR")) {
  GOLF_CARD_COLOR <- "gold"
}

if (!exists("GOLF_HEADSHOTS_PATH")) {
  GOLF_HEADSHOTS_PATH <- "data/golf/player_headshots/player_headshots.csv"
}

# =============================================================================
# HEADSHOT LOADING (if not already defined by Classic module)
# =============================================================================

if (!exists("load_golf_headshots")) {
  #' Load golf player headshots from CSV
  load_golf_headshots <- function() {
    log_debug("load_golf_headshots() called", level = "DEBUG")
    
    if (!file.exists(GOLF_HEADSHOTS_PATH)) {
      log_debug("Headshots file not found:", GOLF_HEADSHOTS_PATH, level = "WARN")
      return(NULL)
    }
    
    tryCatch({
      headshots_df <- readr::read_csv(GOLF_HEADSHOTS_PATH, show_col_types = FALSE)
      names(headshots_df) <- tolower(names(headshots_df))
      
      if (all(c("fname", "name", "headshot") %in% names(headshots_df))) {
        headshots_df <- headshots_df %>%
          mutate(
            full_name = trimws(paste(fname, name)),
            headshot_url = headshot,
            match_key = tolower(gsub("[^a-z0-9 ]", " ", full_name)),
            match_key = gsub("\\s+", " ", trimws(match_key))
          ) %>%
          select(full_name, headshot_url, match_key) %>%
          filter(!is.na(full_name) & full_name != "")
        
        return(headshots_df)
      }
      return(NULL)
    }, error = function(e) {
      log_debug("Error loading headshots:", e$message, level = "ERROR")
      return(NULL)
    })
  }
}

if (!exists("join_golf_headshots")) {
  #' Join headshots to player data
  join_golf_headshots <- function(player_df) {
    if (is.null(player_df) || nrow(player_df) == 0) return(player_df)
    
    headshots <- load_golf_headshots()
    
    if (is.null(headshots) || nrow(headshots) == 0) {
      player_df$headshot_url <- GOLF_DEFAULT_HEADSHOT
      return(player_df)
    }
    
    player_df <- player_df %>%
      mutate(
        match_key = tolower(gsub("[^a-z0-9 ]", " ", player_name)),
        match_key = gsub("\\s+", " ", trimws(match_key))
      )
    
    result <- player_df %>%
      left_join(headshots %>% select(match_key, headshot_url), by = "match_key") %>%
      mutate(headshot_url = ifelse(is.na(headshot_url) | headshot_url == "", 
                                   GOLF_DEFAULT_HEADSHOT, headshot_url)) %>%
      select(-match_key)
    
    return(result)
  }
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

# Google Sheet IDs for Showdown
GOLF_SHOWDOWN_SALARIES_SHEET_ID <- "1QZt9Z5NkIdRyCN2BmKsBYh7Efkk0CJOrvdAfxAGwbd4"
GOLF_SHOWDOWN_PROJECTIONS_SHEET_ID <- "1uk-Ptqfg1MDkyTIxZiqvuLFLdE_M8vr269QEelqWMgY"

# =============================================================================
# NAME CORRECTIONS
# Maps variant names to canonical names used in projections
# =============================================================================

GOLF_SHOWDOWN_NAME_CORRECTIONS <- list(
  # McNames - capitalize properly
  "Robert Macintyre" = "Robert MacIntyre",
  "Maverick Mcnealy" = "Maverick McNealy",
  "Denny Mccarthy" = "Denny McCarthy",
  "Max Mcgreevy" = "Max McGreevy",
  
  # Nickname/full name variations
  "Christopher Gotterup" = "Chris Gotterup",
  "chris gotterup" = "Chris Gotterup",
  "Henry Lebioda" = "Hank Lebioda",
  "henry lebioda" = "Hank Lebioda",
  "Cam Davis" = "Cameron Davis",
  "cam davis" = "Cameron Davis",
  "Matt McCarty" = "Matthew McCarty",
  "matt mccarty" = "Matthew McCarty",
  "Matthias Schmid" = "Matti Schmid",
  "matthias schmid" = "Matti Schmid",
  "Zach Bauchou" = "Zachary Bauchou",
  "zach bauchou" = "Zachary Bauchou",
  "Alex Noren" = "Alexander Noren",
  "alex noren" = "Alexander Noren",
  "Nico Echavarria" = "Nicolas Echavarria",
  "nico echavarria" = "Nicolas Echavarria",
  "Kris Ventura" = "Kristoffer Ventura",
  "kris ventura" = "Kristoffer Ventura",
  "Sam Stevens" = "Samuel Stevens",
  "sam stevens" = "Samuel Stevens",
  "Kota Yuta Kaneko" = "Kota Kaneko",
  
  # Hyphenated/spaced name variations
  "Seong-Hyeon Kim" = "Seonghyeon Kim",
  "seong hyeon kim" = "Seonghyeon Kim",
  "Byeong-Hun An" = "Byeong Hun An",
  "byeong hun an" = "Byeong Hun An",
  "Sung-Jae Im" = "Sungjae Im",
  "sung jae im" = "Sungjae Im",
  "Hao-Tong Li" = "Haotong Li",
  "hao tong li" = "Haotong Li",
  "Ze-Cheng Dou" = "Zecheng Dou",
  "ze cheng dou" = "Zecheng Dou",
  "Adrien Dumont" = "Adrien Dumont De Chassart",
  "adrien dumont" = "Adrien Dumont De Chassart",
  "Jordan L Smith" = "Jordan Smith",
  "jordan l smith" = "Jordan Smith",
  
  # J.J. variations
  "JJ Spaun" = "J.J. Spaun",
  "jj spaun" = "J.J. Spaun"
)

#' Apply name corrections to a player name
#' @param name Player name to correct
#' @return Corrected name
apply_golf_name_correction <- function(name) {
  if (is.na(name) || name == "") return(name)
  
  # Try exact match first
  if (name %in% names(GOLF_SHOWDOWN_NAME_CORRECTIONS)) {
    return(GOLF_SHOWDOWN_NAME_CORRECTIONS[[name]])
  }
  
  # Try lowercase match
  name_lower <- tolower(trimws(name))
  if (name_lower %in% names(GOLF_SHOWDOWN_NAME_CORRECTIONS)) {
    return(GOLF_SHOWDOWN_NAME_CORRECTIONS[[name_lower]])
  }
  
  return(name)
}

#' Get available showdown contests from Google Sheets
#' Returns sheet names from the salaries workbook (format: "Sony Open Day 1", etc.)
get_golf_showdown_sheets_gsheet <- function() {
  log_debug("get_golf_showdown_sheets_gsheet() called", level = "DEBUG")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    ss <- googlesheets4::gs4_get(GOLF_SHOWDOWN_SALARIES_SHEET_ID)
    all_sheets <- ss$sheets$name
    
    log_debug("Found showdown sheets:", paste(all_sheets, collapse = ", "), level = "INFO")
    return(all_sheets)
    
  }, error = function(e) {
    log_debug("Error getting showdown sheets:", e$message, level = "ERROR")
    return(character(0))
  })
}

#' Normalize player name for matching between salaries and projections
#' Applies name corrections first, then normalizes
normalize_showdown_name <- function(name) {
  if (is.na(name) || name == "") return("")
  
  # Apply name corrections first
  name <- apply_golf_name_correction(name)
  
  # Lowercase and clean
  name_lower <- tolower(trimws(name))
  
  # Remove special characters, normalize spaces
  name_lower <- gsub("[^a-z0-9 ]", " ", name_lower)
  name_lower <- gsub("\\s+", " ", name_lower)
  name_lower <- trimws(name_lower)
  
  return(name_lower)
}

#' Load showdown salaries from Google Sheets
#' @param sheet_name Name of the sheet to load (e.g., "Sony Open Day 1")
#' @return Data frame with player salaries
load_showdown_salaries <- function(sheet_name) {
  log_debug("load_showdown_salaries() for:", sheet_name, level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    
    data_raw <- googlesheets4::read_sheet(
      GOLF_SHOWDOWN_SALARIES_SHEET_ID,
      sheet = sheet_name
    ) %>%
      janitor::clean_names()
    
    log_debug("Salary columns:", paste(names(data_raw), collapse = ", "), level = "DEBUG")
    
    # Check if we have f_name (first) + name (last) format - need to combine them
    if ("f_name" %in% names(data_raw) && "name" %in% names(data_raw)) {
      log_debug("Combining f_name (first) + name (last) into full name", level = "DEBUG")
      data_raw <- data_raw %>%
        mutate(player_name = trimws(paste(f_name, name)))
    } else {
      # Try single full name column
      name_priority <- c("full_name", "golfer", "player_name", "player")
      name_col <- NULL
      for (col in name_priority) {
        if (col %in% names(data_raw)) {
          name_col <- col
          break
        }
      }
      # Fall back to "name" if it seems to contain full names (has spaces)
      if (is.null(name_col) && "name" %in% names(data_raw)) {
        # Check if name column has spaces (indicating full names)
        sample_names <- head(data_raw$name, 5)
        if (any(grepl(" ", sample_names))) {
          name_col <- "name"
        }
      }
      
      if (!is.null(name_col)) {
        log_debug("Using single name column:", name_col, level = "DEBUG")
        data_raw <- data_raw %>% rename(player_name = !!name_col)
      } else {
        log_debug("No player name column found in salaries", level = "ERROR")
        return(NULL)
      }
    }
    
    # Find salary column - include "price" which FanTeam uses
    salary_col <- intersect(names(data_raw), c("price", "salary", "dk_salary", "sal", "cost"))
    if (length(salary_col) > 0) {
      log_debug("Using salary column:", salary_col[1], level = "DEBUG")
      data_raw <- data_raw %>% mutate(salary = as.numeric(.data[[salary_col[1]]]))
    } else {
      log_debug("No salary column found", level = "ERROR")
      return(NULL)
    }
    
    # Create match key
    data_raw <- data_raw %>%
      mutate(match_key = sapply(player_name, normalize_showdown_name)) %>%
      select(player_name, salary, match_key) %>%
      filter(!is.na(player_name) & player_name != "")
    
    log_debug("Loaded", nrow(data_raw), "salaries", level = "INFO")
    log_debug("Sample names:", paste(head(data_raw$player_name, 5), collapse = ", "), level = "DEBUG")
    return(data_raw)
    
  }, error = function(e) {
    log_debug("Error loading showdown salaries:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load showdown projections from Google Sheets
#' @param sheet_name Name of the sheet to load (e.g., "Sony Open Day 1")
#' @return Data frame with player projections
load_showdown_projections <- function(sheet_name) {
  log_debug("load_showdown_projections() for:", sheet_name, level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    
    data_raw <- googlesheets4::read_sheet(
      GOLF_SHOWDOWN_PROJECTIONS_SHEET_ID,
      sheet = sheet_name
    ) %>%
      janitor::clean_names()
    
    log_debug("Projection columns:", paste(names(data_raw), collapse = ", "), level = "DEBUG")
    
    # Find player name column
    name_col <- intersect(names(data_raw), c("golfer", "player_name", "player", "name", "f_name", "full_name"))
    if (length(name_col) > 0) {
      log_debug("Using projection name column:", name_col[1], level = "DEBUG")
      data_raw <- data_raw %>% rename(player_name = !!name_col[1])
    } else {
      log_debug("No player name column found in projections", level = "ERROR")
      return(NULL)
    }
    
    # Find median/projection column - dk_points is common for DraftKings projections
    median_col <- intersect(names(data_raw), c("dk_points", "median", "projection", "proj", "pts", "fpts", "points", "projected_points"))
    if (length(median_col) > 0) {
      log_debug("Using projection column:", median_col[1], level = "DEBUG")
      data_raw <- data_raw %>% mutate(median = as.numeric(.data[[median_col[1]]]))
    } else {
      log_debug("No median column found", level = "WARN")
      data_raw$median <- NA_real_
    }
    
    # Find ceiling column
    ceiling_col <- intersect(names(data_raw), c("ceiling", "ceil", "upside", "high"))
    if (length(ceiling_col) > 0) {
      data_raw <- data_raw %>% mutate(ceiling = as.numeric(.data[[ceiling_col[1]]]))
    } else {
      # Default ceiling for single day (less variance than full tournament)
      data_raw$ceiling <- data_raw$median * 1.15
    }
    
    # Find ownership columns
    own_col <- intersect(names(data_raw), c("ownership", "own_large", "ownership_large", "dk_ownership", "own"))
    if (length(own_col) > 0) {
      data_raw <- data_raw %>% mutate(own_large = as.numeric(gsub("%", "", .data[[own_col[1]]])))
    } else {
      data_raw$own_large <- NA_real_
    }
    
    own_sm_col <- intersect(names(data_raw), c("own_small", "ownership_small", "fd_ownership"))
    if (length(own_sm_col) > 0) {
      data_raw <- data_raw %>% mutate(own_small = as.numeric(gsub("%", "", .data[[own_sm_col[1]]])))
    } else {
      data_raw$own_small <- NA_real_
    }
    
    # Create match key
    data_raw <- data_raw %>%
      mutate(match_key = sapply(player_name, normalize_showdown_name)) %>%
      select(player_name, median, ceiling, own_large, own_small, match_key) %>%
      filter(!is.na(player_name) & player_name != "")
    
    log_debug("Loaded", nrow(data_raw), "projections", level = "INFO")
    log_debug("Sample names:", paste(head(data_raw$player_name, 5), collapse = ", "), level = "DEBUG")
    return(data_raw)
    
  }, error = function(e) {
    log_debug("Error loading showdown projections:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load and merge showdown tournament data from Google Sheets
#' Combines salaries and projections into a single data frame
#' @param sheet_name Name of the sheet to load (e.g., "Sony Open Day 1")
#' @return Data frame with player projections and salaries for single-day contest
load_showdown_tournament_data <- function(sheet_name) {
  log_debug("load_showdown_tournament_data() for:", sheet_name, level = "INFO")
  
  # Load salaries
  salaries <- load_showdown_salaries(sheet_name)
  if (is.null(salaries)) {
    log_debug("Failed to load salaries", level = "ERROR")
    return(NULL)
  }
  
  # Load projections
  projections <- load_showdown_projections(sheet_name)
  if (is.null(projections)) {
    log_debug("Failed to load projections", level = "ERROR")
    return(NULL)
  }
  
  # Debug: show sample match keys from both sources
  log_debug("Salary match keys sample:", paste(head(salaries$match_key, 5), collapse = ", "), level = "DEBUG")
  log_debug("Projection match keys sample:", paste(head(projections$match_key, 5), collapse = ", "), level = "DEBUG")
  
  # Merge on match_key
  merged <- salaries %>%
    left_join(
      projections %>% select(-player_name),  # Use salary player names as primary
      by = "match_key"
    )
  
  # Calculate blended projection and value
  merged <- merged %>%
    mutate(
      blended = ifelse(!is.na(median), median * 0.7 + ceiling * 0.3, NA_real_),
      value = ifelse(!is.na(salary) & salary > 0 & !is.na(blended), blended / salary, NA_real_)
    ) %>%
    select(
      player_name, salary, median, ceiling, blended, value,
      own_large, own_small
    ) %>%
    filter(!is.na(player_name) & player_name != "")
  
  # Join headshots from CSV file
  merged <- join_golf_headshots(merged)
  
  # Log matching stats
  matched_count <- sum(!is.na(merged$median))
  unmatched_count <- nrow(merged) - matched_count
  log_debug(sprintf("Merged data: %d total, %d matched, %d unmatched", 
                    nrow(merged), matched_count, unmatched_count), level = "INFO")
  
  if (unmatched_count > 0) {
    unmatched_names <- merged %>% filter(is.na(median)) %>% pull(player_name)
    log_debug("Unmatched players:", paste(head(unmatched_names, 10), collapse = ", "), level = "WARN")
  }
  
  return(merged)
}

# =============================================================================
# UI
# =============================================================================

golf_showdown_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("golf_showdown_ui() called with id:", id, level = "INFO")
  
  # Get available showdown sheets from Google Sheets
  sheets <- tryCatch({
    get_golf_showdown_sheets_gsheet()
  }, error = function(e) {
    log_debug("Error getting showdown sheets:", e$message, level = "ERROR")
    character(0)
  })
  
  sheet_choices <- if (length(sheets) > 0) {
    setNames(sheets, sheets)
  } else {
    c("No showdown contests found" = "")
  }
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Golf Showdown"),
      tags$p(class = "text-muted", "Build single-day lineups with Captain and Underdog multipliers")
    ),
    
    # =========================================================================
    # SETTINGS
    # =========================================================================
    ui_card(
      title = "Settings",
      color = GOLF_CARD_COLOR,
      
      fluidRow(
        column(4,
               selectizeInput(ns("contest_select"), "Tournament / Day",
                              choices = sheet_choices,
                              selected = if (length(sheets) > 0) sheets[1] else NULL
               )
        ),
        column(2,
               numericInput(ns("salary_cap"), "Salary Cap", value = 100, min = 50, max = 150, step = 0.5)
        ),
        column(2,
               selectizeInput(ns("projection_type"), "Optimize On",
                              choices = c("Blended" = "blended", "Median" = "median", "Ceiling" = "ceiling"),
                              selected = "blended"
               )
        ),
        column(4,
               div(style = "margin-top: 25px;",
                   actionButton(ns("process_btn"), "Load Data", class = "btn btn-primary w-100", icon = icon("sync"))
               )
        )
      ),
      
      # Multiplier reference
      div(
        class = "alert alert-info mt-3 mb-0",
        style = "padding: 0.5rem 1rem; font-size: 0.85rem;",
        icon("info-circle"), " ",
        tags$strong("CPT"), " (you choose): 1.25x points | ",
        tags$strong("UNDERDOG"), " (auto-cheapest): 1.25x points | ",
        tags$strong("Others"), ": 1.0x points"
      ),
      
      uiOutput(ns("unmatched_alert"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # PLAYER POOL
    # =========================================================================
    ui_card(
      title = "Player Pool",
      color = GOLF_CARD_COLOR,
      
      # Heatmap dropdown row
      div(
        style = "display: flex; justify-content: flex-end; margin-bottom: 0.5rem; align-items: center; gap: 0.5rem;",
        tags$label(style = "font-size: 0.8rem; font-weight: 600; color: var(--text-secondary); margin: 0;", "Heatmap"),
        div(
          style = "width: 120px;",
          selectInput(ns("pool_heatmap"), NULL,
                      choices = c("None" = "none", "Blend" = "blended", "Median" = "median", 
                                  "Ceiling" = "ceiling", "Value" = "value"),
                      selected = "none"
          )
        )
      ),
      
      div(
        class = "player-pool-container",
        style = "max-height: 450px; overflow-y: auto;",
        reactableOutput(ns("player_pool_table"))
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # PLAYER RULES
    # =========================================================================
    ui_card(
      title = "Player Rules & Adjustments",
      color = GOLF_CARD_COLOR,
      
      # Captain Lock Row
      fluidRow(
        column(6,
               selectizeInput(ns("lock_cpt"), "Lock Captain (1.25x)",
                              choices = NULL,
                              options = list(placeholder = "Select Captain (optional - optimizer will choose best if empty)...")
               )
        ),
        column(6,
               div(
                 class = "text-muted small mt-4",
                 icon("lightbulb"), " Underdog (1.25x) is automatically assigned to the cheapest player in your lineup"
               )
        )
      ),
      
      tags$hr(),
      
      # Lock Players Row
      fluidRow(
        column(9,
               selectizeInput(ns("lock_players"), "Lock Players (Always Include)",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players to lock...")
               )
        ),
        column(3,
               div(
                 style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                 actionButton(ns("apply_lock"), "Lock", class = "btn btn-outline-success", style = "width: 100px;")
               )
        )
      ),
      
      # Exclude Players Row
      fluidRow(
        column(9,
               selectizeInput(ns("exclude_players"), "Exclude Players",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players to exclude...")
               )
        ),
        column(3,
               div(
                 style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                 actionButton(ns("apply_exclude"), "Exclude", class = "btn btn-outline-danger", style = "width: 100px;")
               )
        )
      ),
      
      # Applied lock/exclude display
      uiOutput(ns("lock_exclude_display")),
      
      tags$hr(),
      
      # Grouped Boost/Dock
      tags$h6(class = "fw-bold mb-2", icon("users"), " Grouped Boost/Dock"),
      tags$p(class = "text-muted small mb-3", "Apply uniform boost or dock to selected players"),
      
      fluidRow(
        column(5,
               selectizeInput(ns("grouped_players"), "Players",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players...")
               )
        ),
        column(2,
               selectInput(ns("grouped_action"), "Action", choices = c("Boost", "Dock"))
        ),
        column(2,
               numericInput(ns("grouped_pct"), "Adjust %", value = 10, min = 0, max = 100, step = 5)
        ),
        column(3,
               div(style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                   actionButton(ns("add_grouped_rule"), "Add Rule", class = "btn btn-primary", style = "width: 120px; text-align: center;")
               )
        )
      ),
      
      uiOutput(ns("grouped_rules_display")),
      
      tags$hr(),
      
      # Correlation Rules
      tags$h6(class = "fw-bold mb-2", icon("link"), " Correlation Rules"),
      tags$p(class = "text-muted small mb-3", "When trigger player is in lineup, boost/dock target players"),
      
      # Row 1: Trigger
      fluidRow(
        column(4,
               selectizeInput(ns("corr_trigger"), "If This Player Is In Lineup",
                              choices = NULL,
                              options = list(placeholder = "Select trigger player...")
               )
        )
      ),
      
      # Row 2: Boost Targets
      fluidRow(
        column(6,
               selectizeInput(ns("corr_boost_targets"), "Boost These Players",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players to boost...")
               )
        ),
        column(2,
               numericInput(ns("corr_boost_pct"), "Boost %", value = 10, min = 0, max = 100, step = 5)
        )
      ),
      
      # Row 3: Dock Targets
      fluidRow(
        column(6,
               selectizeInput(ns("corr_dock_targets"), "Dock These Players",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players to dock...")
               )
        ),
        column(2,
               numericInput(ns("corr_dock_pct"), "Dock %", value = 10, min = 0, max = 100, step = 5)
        ),
        column(4,
               div(style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                   actionButton(ns("add_corr_rule"), "Add Correlation Rule", class = "btn btn-primary", style = "min-width: 180px; text-align: center;")
               )
        )
      ),
      
      uiOutput(ns("corr_rules_display"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # GENERATE LINEUPS
    # =========================================================================
    ui_card(
      title = "Generate Lineups",
      color = GOLF_CARD_COLOR,
      
      div(
        style = "display: flex; justify-content: center; gap: 2rem; margin-bottom: 1.5rem;",
        div(style = "width: 150px;",
            numericInput(ns("num_lineups"), "# Lineups", value = 10, min = 1, max = 150)
        ),
        div(style = "width: 150px;",
            numericInput(ns("variance_pct"), "Variance %", value = 15, min = 0, max = 50)
        ),
        div(style = "width: 150px;",
            numericInput(ns("min_unique"), "Min Unique", value = 1, min = 0, max = 5)
        )
      ),
      
      div(
        style = "text-align: center; margin-bottom: 1.5rem;",
        actionButton(
          ns("generate_btn"),
          tagList(icon("play"), " GENERATE LINEUPS"),
          class = "btn btn-success btn-lg",
          style = "padding: 1rem 3rem; font-size: 1.2rem; font-weight: 700; text-transform: uppercase; box-shadow: 4px 4px 0 rgba(0,0,0,0.2);"
        )
      ),
      
      uiOutput(ns("lineup_summary_stats")),
      
      tags$hr(),
      
      uiOutput(ns("generated_lineups_grid"))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

golf_showdown_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("golf_showdown_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    rv <- reactiveValues(
      player_data = NULL,
      unmatched_players = NULL,
      generated_lineups = list(),
      optimal_projection = NULL,
      locked_players = character(0),
      excluded_players = character(0),
      grouped_rules = list(),
      corr_rules = list()
    )
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    observeEvent(input$process_btn, {
      req(input$contest_select)
      req(input$contest_select != "")
      
      log_debug(">>> Process button clicked", level = "INFO")
      log_debug(">>> Loading showdown contest:", input$contest_select, level = "INFO")
      
      # Load data from Google Sheets
      showNotification("Loading data from Google Sheets...", type = "message", duration = 2)
      
      player_data <- load_showdown_tournament_data(input$contest_select)
      
      if (is.null(player_data)) {
        showNotification("Failed to load showdown data from Google Sheets", type = "error")
        return()
      }
      
      rv$player_data <- player_data
      
      rv$unmatched_players <- rv$player_data %>%
        filter(is.na(median)) %>%
        pull(player_name)
      
      matched_count <- sum(!is.na(rv$player_data$median))
      showNotification(
        sprintf("Loaded %d golfers (%d with projections)", nrow(rv$player_data), matched_count),
        type = "message"
      )
      
      player_choices <- rv$player_data %>%
        filter(!is.na(median)) %>%
        arrange(desc(blended)) %>%
        pull(player_name)
      
      # Update all player selectors
      updateSelectizeInput(session, "lock_cpt", choices = c("Auto-select best" = "", player_choices))
      updateSelectizeInput(session, "lock_players", choices = player_choices, server = TRUE)
      updateSelectizeInput(session, "exclude_players", choices = player_choices, server = TRUE)
      updateSelectizeInput(session, "grouped_players", choices = player_choices, server = TRUE)
      updateSelectizeInput(session, "corr_trigger", choices = c("Select trigger..." = "", player_choices))
      updateSelectizeInput(session, "corr_boost_targets", choices = player_choices, server = TRUE)
      updateSelectizeInput(session, "corr_dock_targets", choices = player_choices, server = TRUE)
      
      # Reset state
      rv$generated_lineups <- list()
      rv$optimal_projection <- NULL
      rv$locked_players <- character(0)
      rv$excluded_players <- character(0)
      rv$grouped_rules <- list()
      rv$corr_rules <- list()
    })
    
    # =========================================================================
    # UNMATCHED ALERT
    # =========================================================================
    
    output$unmatched_alert <- renderUI({
      unmatched <- rv$unmatched_players
      if (is.null(unmatched) || length(unmatched) == 0) return(NULL)
      
      div(
        class = "alert alert-warning mt-3 mb-0",
        tags$strong(icon("exclamation-triangle"), sprintf(" %d Unmatched: ", length(unmatched))),
        tags$span(
          style = "font-size: 0.85rem;",
          if (length(unmatched) <= 10) {
            paste(unmatched, collapse = ", ")
          } else {
            paste0(paste(head(unmatched, 10), collapse = ", "), " +(", length(unmatched) - 10, " more)")
          }
        )
      )
    })
    
    # =========================================================================
    # LOCK/EXCLUDE
    # =========================================================================
    
    observeEvent(input$apply_lock, {
      rv$locked_players <- unique(c(rv$locked_players, input$lock_players))
      updateSelectizeInput(session, "lock_players", selected = character(0))
    })
    
    observeEvent(input$apply_exclude, {
      rv$excluded_players <- unique(c(rv$excluded_players, input$exclude_players))
      updateSelectizeInput(session, "exclude_players", selected = character(0))
    })
    
    observeEvent(input$remove_locked, {
      rv$locked_players <- setdiff(rv$locked_players, input$remove_locked)
    })
    
    observeEvent(input$remove_excluded, {
      rv$excluded_players <- setdiff(rv$excluded_players, input$remove_excluded)
    })
    
    output$lock_exclude_display <- renderUI({
      locked <- rv$locked_players
      excluded <- rv$excluded_players
      
      if (length(locked) == 0 && length(excluded) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem; margin-top: 1rem;",
        
        # Locked players
        lapply(locked, function(p) {
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-sage); border-radius: 6px;",
            div(
              style = "background: var(--accent-sage); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("lock"), " LOCKED"
            ),
            div(style = "flex: 1; font-weight: 600; font-size: 0.9rem;", p),
            actionButton(
              ns(paste0("remove_locked_", gsub("[^a-zA-Z0-9]", "_", p))),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_locked"), p)
            )
          )
        }),
        
        # Excluded players
        lapply(excluded, function(p) {
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-coral); border-radius: 6px;",
            div(
              style = "background: var(--accent-coral); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("ban"), " EXCLUDED"
            ),
            div(style = "flex: 1; font-weight: 600; font-size: 0.9rem;", p),
            actionButton(
              ns(paste0("remove_excluded_", gsub("[^a-zA-Z0-9]", "_", p))),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_excluded"), p)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # GROUPED RULES
    # =========================================================================
    
    observeEvent(input$add_grouped_rule, {
      req(input$grouped_players)
      req(length(input$grouped_players) > 0)
      
      rule <- list(
        id = paste0("grouped_", length(rv$grouped_rules) + 1, "_", as.integer(Sys.time())),
        players = input$grouped_players,
        action = input$grouped_action,
        pct = input$grouped_pct
      )
      
      rv$grouped_rules <- c(rv$grouped_rules, list(rule))
      updateSelectizeInput(session, "grouped_players", selected = character(0))
      
      log_debug(">>> Added grouped rule:", rule$action, rule$pct, "% for", length(rule$players), "players", level = "INFO")
    })
    
    observeEvent(input$remove_grouped, {
      rv$grouped_rules <- purrr::discard(rv$grouped_rules, ~ .x$id == input$remove_grouped)
    })
    
    output$grouped_rules_display <- renderUI({
      rules <- rv$grouped_rules
      if (length(rules) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem; margin-top: 1rem;",
        
        lapply(rules, function(rule) {
          is_boost <- rule$action == "Boost"
          border_color <- if (is_boost) "var(--accent-sage)" else "var(--accent-coral)"
          badge_bg <- if (is_boost) "var(--accent-sage)" else "var(--accent-coral)"
          
          div(
            style = sprintf("display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid %s; border-radius: 6px;", border_color),
            div(
              style = sprintf("background: %s; color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;", badge_bg),
              if (is_boost) icon("arrow-up") else icon("arrow-down"),
              sprintf(" %s %d%%", rule$action, rule$pct)
            ),
            div(style = "flex: 1; font-size: 0.85rem;", paste(rule$players, collapse = ", ")),
            actionButton(
              ns(paste0("remove_grouped_", rule$id)),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_grouped"), rule$id)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # CORRELATION RULES
    # =========================================================================
    
    observeEvent(input$add_corr_rule, {
      req(input$corr_trigger)
      req(input$corr_trigger != "")
      req(length(input$corr_boost_targets) > 0 || length(input$corr_dock_targets) > 0)
      
      rule <- list(
        id = paste0("corr_", length(rv$corr_rules) + 1, "_", as.integer(Sys.time())),
        trigger = input$corr_trigger,
        boost_targets = input$corr_boost_targets %||% character(0),
        boost_pct = input$corr_boost_pct %||% 0,
        dock_targets = input$corr_dock_targets %||% character(0),
        dock_pct = input$corr_dock_pct %||% 0
      )
      
      rv$corr_rules <- c(rv$corr_rules, list(rule))
      
      updateSelectizeInput(session, "corr_trigger", selected = "")
      updateSelectizeInput(session, "corr_boost_targets", selected = character(0))
      updateSelectizeInput(session, "corr_dock_targets", selected = character(0))
      
      log_debug(">>> Added correlation rule: trigger =", rule$trigger, level = "INFO")
    })
    
    observeEvent(input$remove_corr, {
      rv$corr_rules <- purrr::discard(rv$corr_rules, ~ .x$id == input$remove_corr)
    })
    
    output$corr_rules_display <- renderUI({
      rules <- rv$corr_rules
      if (length(rules) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem; margin-top: 1rem;",
        
        lapply(rules, function(rule) {
          div(
            style = "display: flex; align-items: flex-start; padding: 0.5rem 0.6rem; background: white; border: 2px solid var(--accent-plum, #B48EAD); border-radius: 6px;",
            div(
              style = "background: var(--accent-plum, #B48EAD); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("link"), " CORR"
            ),
            div(
              style = "flex: 1; font-size: 0.85rem; line-height: 1.4;",
              div(tags$strong("If: "), rule$trigger),
              if (length(rule$boost_targets) > 0) {
                div(
                  style = "color: var(--accent-sage);",
                  icon("arrow-up"), sprintf(" +%d%%: ", rule$boost_pct),
                  paste(rule$boost_targets, collapse = ", ")
                )
              },
              if (length(rule$dock_targets) > 0) {
                div(
                  style = "color: var(--accent-coral);",
                  icon("arrow-down"), sprintf(" -%d%%: ", rule$dock_pct),
                  paste(rule$dock_targets, collapse = ", ")
                )
              }
            ),
            actionButton(
              ns(paste0("remove_corr_", rule$id)),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_corr"), rule$id)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # PLAYER POOL TABLE
    # =========================================================================
    
    output$player_pool_table <- renderReactable({
      req(rv$player_data)
      
      players <- rv$player_data %>%
        filter(!is.na(median))
      
      if (nrow(players) == 0) return(NULL)
      
      heatmap_col <- input$pool_heatmap %||% "none"
      
      # Prepare display data with headshot_url
      pool_data <- players %>%
        select(player_name, headshot_url, salary, blended, median, ceiling, 
               any_of(c("value", "own_large", "own_small"))) %>%
        arrange(desc(blended))
      
      # Pre-compute ranges for heatmaps
      blend_range <- range(pool_data$blended, na.rm = TRUE)
      median_range <- range(pool_data$median, na.rm = TRUE)
      ceiling_range <- range(pool_data$ceiling, na.rm = TRUE)
      value_range <- if ("value" %in% names(pool_data)) range(pool_data$value, na.rm = TRUE) else c(0, 1)
      
      # Sequential heatmap helper (white -> teal)
      get_sequential_style <- function(value, min_val, max_val) {
        if (is.na(value)) return(list(verticalAlign = "middle"))
        if (max_val == min_val) return(list(verticalAlign = "middle"))
        
        t <- (value - min_val) / (max_val - min_val)
        t <- max(0, min(1, t))
        r <- round(255 + (143 - 255) * t)
        g <- round(255 + (188 - 255) * t)
        b <- round(255 + (187 - 255) * t)
        list(background = sprintf("rgb(%d, %d, %d)", r, g, b), verticalAlign = "middle")
      }
      
      # Diverging heatmap helper (coral -> white -> teal)
      get_diverging_style <- function(value, midpoint, min_val, max_val) {
        if (is.na(value)) return(list(verticalAlign = "middle"))
        
        if (value < midpoint) {
          if (min_val >= midpoint) min_val <- -1
          t <- (value - min_val) / (midpoint - min_val)
          t <- max(0, min(1, t))
          r <- round(208 + (255 - 208) * t)
          g <- round(135 + (255 - 135) * t)
          b <- round(112 + (255 - 112) * t)
        } else {
          if (max_val <= midpoint) max_val <- 1
          t <- (value - midpoint) / (max_val - midpoint)
          t <- max(0, min(1, t))
          r <- round(255 + (143 - 255) * t)
          g <- round(255 + (188 - 255) * t)
          b <- round(255 + (187 - 255) * t)
        }
        list(background = sprintf("rgb(%d, %d, %d)", r, g, b), verticalAlign = "middle")
      }
      
      reactable(
        pool_data,
        theme = app_reactable_theme(compact = TRUE),
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 50,
        defaultColDef = colDef(
          vAlign = "center",
          style = list(verticalAlign = "middle")
        ),
        columns = list(
          player_name = colDef(
            name = "Golfer",
            sticky = "left",
            style = list(verticalAlign = "middle"),
            cell = function(value, index) {
              headshot_url <- pool_data$headshot_url[index]
              headshot_src <- if (!is.na(headshot_url) && nchar(headshot_url) > 0) headshot_url else GOLF_DEFAULT_HEADSHOT
              
              div(
                style = "display: flex; align-items: center; gap: 0.5rem;",
                div(
                  class = "player-headshot player-headshot--sm",
                  style = "background: #e8e8e8; flex-shrink: 0; width: 32px; height: 32px; border-radius: 50%; overflow: hidden; border: 2px solid var(--text-primary);",
                  tags$img(
                    src = headshot_src,
                    style = "width: 100%; height: 100%; object-fit: cover;",
                    onerror = sprintf("this.src='%s'", GOLF_DEFAULT_HEADSHOT)
                  )
                ),
                span(style = "font-weight: 600;", value)
              )
            }
          ),
          headshot_url = colDef(show = FALSE),
          salary = colDef(
            name = "Salary",
            width = 115,
            align = "center",
            format = colFormat(prefix = "$", suffix = "M", digits = 1),
            style = list(verticalAlign = "middle")
          ),
          blended = colDef(
            name = "Blend",
            width = 105,
            align = "center",
            format = colFormat(digits = 1),
            style = function(value) {
              if (heatmap_col == "blended" && !is.na(value)) {
                get_sequential_style(value, blend_range[1], blend_range[2])
              } else {
                list(fontWeight = 600, verticalAlign = "middle")
              }
            }
          ),
          median = colDef(
            name = "Median",
            width = 105,
            align = "center",
            format = colFormat(digits = 1),
            style = function(value) {
              if (heatmap_col == "median" && !is.na(value)) {
                get_sequential_style(value, median_range[1], median_range[2])
              } else {
                list(verticalAlign = "middle")
              }
            }
          ),
          ceiling = colDef(
            name = "Ceiling",
            width = 105,
            align = "center",
            format = colFormat(digits = 1),
            style = function(value) {
              if (heatmap_col == "ceiling" && !is.na(value)) {
                get_sequential_style(value, ceiling_range[1], ceiling_range[2])
              } else {
                list(verticalAlign = "middle")
              }
            }
          ),
          value = colDef(
            name = "Value",
            width = 105,
            align = "center",
            format = colFormat(digits = 2),
            style = function(value) {
              if (heatmap_col == "value" && !is.na(value)) {
                get_diverging_style(value, 0, value_range[1], value_range[2])
              } else {
                list(verticalAlign = "middle")
              }
            }
          ),
          own_large = colDef(
            name = "Own LG",
            width = 100,
            align = "center",
            format = colFormat(suffix = "%", digits = 1),
            style = list(verticalAlign = "middle")
          ),
          own_small = colDef(
            name = "Own SM",
            width = 100,
            align = "center",
            format = colFormat(suffix = "%", digits = 1),
            style = list(verticalAlign = "middle")
          )
        )
      )
    })
    
    # =========================================================================
    # LINEUP GENERATION - SHOWDOWN SPECIFIC
    # =========================================================================
    
    # Helper: assign roles (CPT, UNDERDOG, regular) to a lineup
    assign_showdown_roles <- function(lineup_df, cpt_player = NULL) {
      # If no CPT specified, choose the one that maximizes total (highest projection)
      if (is.null(cpt_player) || cpt_player == "" || !(cpt_player %in% lineup_df$player_name)) {
        cpt_player <- lineup_df$player_name[which.max(lineup_df$projection)]
      }
      
      # Underdog is always the cheapest player
      underdog_player <- lineup_df$player_name[which.min(lineup_df$salary)]
      
      # If CPT is also the cheapest, underdog goes to second cheapest
      if (cpt_player == underdog_player) {
        salary_order <- order(lineup_df$salary)
        underdog_player <- lineup_df$player_name[salary_order[2]]
      }
      
      # Assign roles
      lineup_df <- lineup_df %>%
        mutate(
          role = case_when(
            player_name == cpt_player ~ "CPT",
            player_name == underdog_player ~ "DOG",
            TRUE ~ "G"
          ),
          multiplier = case_when(
            role == "CPT" ~ 1.25,
            role == "DOG" ~ 1.25,
            TRUE ~ 1.0
          ),
          effective_projection = projection * multiplier
        )
      
      # Sort: CPT first, then DOG, then others by projection
      lineup_df <- lineup_df %>%
        arrange(
          factor(role, levels = c("CPT", "DOG", "G")),
          desc(projection)
        )
      
      return(lineup_df)
    }
    
    observeEvent(input$generate_btn, {
      req(rv$player_data)
      
      log_debug(">>> Generate button clicked", level = "INFO")
      
      projection_type <- input$projection_type %||% "blended"
      num_lineups <- input$num_lineups %||% 10
      variance <- input$variance_pct %||% 15
      salary_cap <- input$salary_cap %||% 100
      min_unique <- input$min_unique %||% 1
      cpt_lock <- input$lock_cpt
      
      # Apply grouped boost/dock rules
      players <- rv$player_data %>%
        filter(!is.na(median)) %>%
        mutate(adjusted_projection = .data[[projection_type]])
      
      for (rule in rv$grouped_rules) {
        adj_factor <- if (rule$action == "Boost") 1 + rule$pct / 100 else 1 - rule$pct / 100
        players <- players %>%
          mutate(adjusted_projection = if_else(
            player_name %in% rule$players,
            adjusted_projection * adj_factor,
            adjusted_projection
          ))
      }
      
      # Expand correlation rules for optimizer
      expanded_corr_rules <- lapply(rv$corr_rules, function(r) {
        list(
          trigger = r$trigger,
          boost_targets = r$boost_targets,
          boost_pct = r$boost_pct,
          dock_targets = r$dock_targets,
          dock_pct = r$dock_pct
        )
      })
      
      # Use classic-style generation (6 players, no multiplier constraints)
      # Then apply showdown roles after
      showNotification("Generating lineups...", type = "message", duration = 2)
      
      lineups_raw <- generate_golf_classic_lineups(
        players = players,
        projection_col = "adjusted_projection",
        num_lineups = num_lineups,
        variance_pct = variance,
        salary_cap = salary_cap,
        locked_players = rv$locked_players,
        excluded_players = rv$excluded_players,
        min_unique = min_unique,
        corr_rules = expanded_corr_rules
      )
      
      # Apply showdown roles to each lineup
      lineups <- lapply(lineups_raw, function(lineup) {
        assign_showdown_roles(lineup, cpt_player = cpt_lock)
      })
      
      # Calculate optimal
      if (length(lineups) > 0) {
        # Find best CPT assignment for optimal
        opt_lineup <- lineups_raw[[1]]  # First is usually optimal
        best_total <- 0
        best_cpt <- NULL
        
        for (p in opt_lineup$player_name) {
          test <- assign_showdown_roles(opt_lineup, cpt_player = p)
          total <- sum(test$effective_projection)
          if (total > best_total) {
            best_total <- total
            best_cpt <- p
          }
        }
        
        # If user locked CPT, use that instead
        if (!is.null(cpt_lock) && cpt_lock != "" && cpt_lock %in% opt_lineup$player_name) {
          opt_with_lock <- assign_showdown_roles(opt_lineup, cpt_player = cpt_lock)
          rv$optimal_projection <- sum(opt_with_lock$effective_projection)
        } else {
          rv$optimal_projection <- best_total
        }
        
        log_debug(">>> Optimal showdown projection:", rv$optimal_projection, level = "INFO")
      } else {
        rv$optimal_projection <- NA
      }
      
      rv$generated_lineups <- lineups
      
      if (length(lineups) > 0) {
        showNotification(sprintf("Generated %d unique lineups", length(lineups)), type = "message")
      } else {
        showNotification("No valid lineups generated", type = "warning")
      }
    })
    
    # =========================================================================
    # SUMMARY STATS
    # =========================================================================
    
    output$lineup_summary_stats <- renderUI({
      n <- length(rv$generated_lineups)
      opt <- rv$optimal_projection
      
      if (n == 0) return(NULL)
      
      projections <- sapply(rv$generated_lineups, function(l) sum(l$effective_projection))
      
      div(
        style = "display: flex; justify-content: center; gap: 2rem; margin-bottom: 1rem;",
        
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Lineups"),
          div(style = "font-size: 1.25rem; font-weight: 700;", n)
        ),
        
        if (!is.na(opt)) {
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Optimal"),
            div(style = "font-size: 1.25rem; font-weight: 700; color: var(--accent-sage);", sprintf("%.1f", opt))
          )
        },
        
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Best"),
          div(style = "font-size: 1.25rem; font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", max(projections)))
        ),
        
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Average"),
          div(style = "font-size: 1.25rem; font-weight: 700;", sprintf("%.1f", mean(projections)))
        )
      )
    })
    
    # =========================================================================
    # GENERATED LINEUPS GRID
    # =========================================================================
    
    output$generated_lineups_grid <- renderUI({
      lineups <- rv$generated_lineups
      opt <- rv$optimal_projection
      
      if (length(lineups) == 0) {
        return(div(class = "text-muted text-center py-4", "No lineups generated yet"))
      }
      
      player_data <- rv$player_data
      
      # Sort by closest to optimal
      projections <- sapply(lineups, function(l) sum(l$effective_projection))
      sorted_indices <- if (!is.na(opt)) order(abs(projections - opt)) else seq_along(lineups)
      
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
        lapply(sorted_indices, function(i) {
          lineup <- lineups[[i]]
          total_proj <- sum(lineup$effective_projection)
          total_sal <- sum(lineup$salary)
          vs_opt <- if (!is.na(opt)) total_proj - opt else NA
          
          div(
            style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 0.75rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
            
            # Header
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem; padding-bottom: 0.4rem; border-bottom: 2px dashed var(--bg-secondary);",
              span(style = "font-weight: 800; text-transform: uppercase; font-size: 0.8rem;", sprintf("Lineup %d", i)),
              div(
                style = "display: flex; gap: 0.4rem; font-size: 0.75rem; align-items: center;",
                span(style = "font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", total_proj)),
                span(style = "color: var(--text-muted);", sprintf("$%.1fM", total_sal)),
                if (!is.na(vs_opt)) {
                  span(
                    style = sprintf(
                      "font-weight: 600; padding: 0.1rem 0.25rem; border-radius: 3px; font-size: 0.65rem; background: %s; color: %s;",
                      if (vs_opt >= -3) "rgba(139, 168, 134, 0.3)" else "rgba(232, 131, 121, 0.3)",
                      if (vs_opt >= -3) "var(--accent-sage)" else "var(--accent-coral)"
                    ),
                    sprintf("%+.1f", vs_opt)
                  )
                }
              )
            ),
            
            # Players
            div(
              style = "display: flex; flex-direction: column; gap: 0.25rem;",
              lapply(1:nrow(lineup), function(j) {
                player_info <- player_data %>% filter(player_name == lineup$player_name[j])
                
                headshot_url <- if (nrow(player_info) > 0 && "headshot_url" %in% names(player_info)) {
                  player_info$headshot_url[1]
                } else {
                  GOLF_DEFAULT_HEADSHOT
                }
                
                # Get role info for styling
                role <- lineup$role[j]
                role_color <- if (role == "CPT") "#B48EAD" else if (role == "DOG") "#EBCB8B" else "var(--text-muted)"
                role_label <- if (role == "CPT") "CPT" else if (role == "DOG") "DOG" else "G"
                mult_label <- if (lineup$multiplier[j] > 1) sprintf("%.2fx", lineup$multiplier[j]) else ""
                
                # Split name
                name_parts <- strsplit(lineup$player_name[j], " ")[[1]]
                first_name <- if (length(name_parts) >= 1) name_parts[1] else ""
                last_name <- if (length(name_parts) >= 2) paste(name_parts[-1], collapse = " ") else ""
                
                div(
                  style = "display: flex; align-items: center; gap: 0.35rem; padding: 0.2rem 0.3rem; background: var(--bg-tertiary); border-radius: 4px;",
                  
                  # Role badge
                  div(
                    style = sprintf(
                      "width: 32px; text-align: center; font-size: 0.6rem; font-weight: 700; padding: 0.15rem 0; border-radius: 3px; %s",
                      if (role %in% c("CPT", "DOG")) sprintf("background: %s; color: white;", role_color) else sprintf("color: %s;", role_color)
                    ),
                    role_label
                  ),
                  
                  # Headshot
                  div(
                    style = "width: 28px; height: 28px; border-radius: 50%; background: #e8e8e8; overflow: hidden; flex-shrink: 0; border: 2px solid var(--text-primary);",
                    tags$img(
                      src = headshot_url,
                      style = "width: 100%; height: 100%; object-fit: cover;",
                      onerror = sprintf("this.src='%s'", GOLF_DEFAULT_HEADSHOT)
                    )
                  ),
                  
                  # Name stacked
                  div(
                    style = "flex: 1; min-width: 0; line-height: 1.1;",
                    div(style = "font-size: 0.55rem; color: var(--text-muted);", first_name),
                    div(style = "font-size: 0.7rem; font-weight: 700; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", last_name)
                  ),
                  
                  # SAL
                  div(
                    style = "text-align: center; width: 40px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "SAL"),
                    div(style = "font-size: 0.7rem; font-weight: 600;", sprintf("%.1f", lineup$salary[j]))
                  ),
                  
                  # Base PROJ
                  div(
                    style = "text-align: center; width: 35px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "BASE"),
                    div(style = "font-size: 0.7rem; font-weight: 600;", sprintf("%.0f", lineup$projection[j]))
                  ),
                  
                  # Effective PROJ (with multiplier)
                  div(
                    style = "text-align: center; width: 40px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "EFF"),
                    div(
                      style = sprintf("font-size: 0.7rem; font-weight: 700; color: %s;", 
                                      if (lineup$multiplier[j] > 1) "var(--accent-coral)" else "inherit"),
                      sprintf("%.0f", lineup$effective_projection[j])
                    )
                  )
                )
              })
            )
          )
        })
      )
    })
    
  })
}

cat("Golf Showdown module loaded: golf_showdown_ui(), golf_showdown_server()\n")
cat("  Format: CPT (1.25x user-selected) + UNDERDOG (1.25x auto-cheapest) + 4 regular\n")