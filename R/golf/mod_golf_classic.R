# =============================================================================
# Module: Golf Classic
# 
# Full tournament lineup building for FanTeam Golf Classic format
# Roster: 6 golfers, 100M salary cap, no position constraints
# =============================================================================

library(lpSolve)

# =============================================================================
# GOLF MODULE CONSTANTS
# =============================================================================

# Default headshot for players without images
GOLF_DEFAULT_HEADSHOT <- "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=200&h=146"

# Card color for Golf UI elements (gold from APP_COLORS)
GOLF_CARD_COLOR <- "gold"  # Named color for ui_card()

# Path to golf headshots CSV file
GOLF_HEADSHOTS_PATH <- "data/golf/player_headshots/player_headshots.csv"

# =============================================================================
# HEADSHOT LOADING FUNCTION
# =============================================================================

#' Load golf player headshots from CSV
#' File format: PlayerID, Name (last), FName (first), Headshot (URL)
#' @return Data frame with full_name and headshot_url columns for joining
load_golf_headshots <- function() {
  log_debug("load_golf_headshots() called", level = "DEBUG")
  
  if (!file.exists(GOLF_HEADSHOTS_PATH)) {
    log_debug("Headshots file not found:", GOLF_HEADSHOTS_PATH, level = "WARN")
    return(NULL)
  }
  
  tryCatch({
    headshots_df <- readr::read_csv(GOLF_HEADSHOTS_PATH, show_col_types = FALSE)
    log_debug("Loaded", nrow(headshots_df), "headshots from CSV", level = "INFO")
    
    # Normalize column names
    names(headshots_df) <- tolower(names(headshots_df))
    
    # Build full name from FName (first) + Name (last)
    if (all(c("fname", "name", "headshot") %in% names(headshots_df))) {
      headshots_df <- headshots_df %>%
        mutate(
          full_name = trimws(paste(fname, name)),
          headshot_url = headshot,
          # Create normalized match key
          match_key = tolower(gsub("[^a-z0-9 ]", " ", full_name)),
          match_key = gsub("\\s+", " ", trimws(match_key))
        ) %>%
        select(full_name, headshot_url, match_key) %>%
        filter(!is.na(full_name) & full_name != "")
      
      log_debug("Processed", nrow(headshots_df), "headshots for matching", level = "INFO")
      return(headshots_df)
    } else {
      log_debug("Missing required columns (fname, name, headshot)", level = "ERROR")
      return(NULL)
    }
    
  }, error = function(e) {
    log_debug("Error loading headshots:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Join headshots to player data
#' @param player_df Data frame with player_name column
#' @return Data frame with headshot_url column added
join_golf_headshots <- function(player_df) {
  if (is.null(player_df) || nrow(player_df) == 0) return(player_df)
  
  log_debug("join_golf_headshots() for", nrow(player_df), "players", level = "DEBUG")
  
  headshots <- load_golf_headshots()
  
  if (is.null(headshots) || nrow(headshots) == 0) {
    log_debug("No headshots available, using defaults", level = "WARN")
    player_df$headshot_url <- GOLF_DEFAULT_HEADSHOT
    return(player_df)
  }
  
  # Create match key for players
  player_df <- player_df %>%
    mutate(
      match_key = tolower(gsub("[^a-z0-9 ]", " ", player_name)),
      match_key = gsub("\\s+", " ", trimws(match_key))
    )
  
  # Join headshots
  result <- player_df %>%
    left_join(
      headshots %>% select(match_key, headshot_url),
      by = "match_key"
    ) %>%
    mutate(
      headshot_url = ifelse(is.na(headshot_url) | headshot_url == "", 
                            GOLF_DEFAULT_HEADSHOT, headshot_url)
    ) %>%
    select(-match_key)
  
  matched <- sum(result$headshot_url != GOLF_DEFAULT_HEADSHOT)
  log_debug(sprintf("Matched headshots: %d of %d players", matched, nrow(result)), level = "INFO")
  
  return(result)
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

# Google Sheet ID for projections (shared with season management)
GOLF_CLASSIC_PROJECTIONS_SHEET_ID <- "1yJJAOv5hzNZagYUG7FLpNmRIRC76L0fJNGPbzK61lbw"

# Google Sheet ID for FanTeam salaries (Golf Classic specific)
GOLF_CLASSIC_SALARIES_SHEET_ID <- "12I3uMfY_V5apa0u6DGcQctIvJETJFonUJwm3aqiuvhI"

# =============================================================================
# NAME CORRECTIONS (shared between Classic and Showdown)
# Maps FanTeam names to projection names
# Add new corrections here as you discover mismatches
# =============================================================================

GOLF_CLASSIC_SHOWDOWN_NAME_CORRECTIONS <- list(
  
  # McNames - FanTeam uses lowercase after "Mc"
  "Robert Macintyre" = "Robert MacIntyre",
  "Maverick Mcnealy" = "Maverick McNealy",
  "Denny Mccarthy" = "Denny McCarthy",
  "Max Mcgreevy" = "Max McGreevy",
  "Matt Mccarty" = "Matthew McCarty",
  
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
  "kota yuta kaneko" = "Kota Kaneko",
  
  # Hyphenated/spaced name variations (Asian names)
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
  
  # Extended names
  "Adrien Dumont" = "Adrien Dumont De Chassart",
  "adrien dumont" = "Adrien Dumont De Chassart",
  "Jordan L Smith" = "Jordan Smith",
  "jordan l smith" = "Jordan Smith",
  
  # J.J. / J.T. variations
  "JJ Spaun" = "J.J. Spaun",
  "jj spaun" = "J.J. Spaun",
  "JT Poston" = "J.T. Poston"
)

#' Apply name corrections for Classic/Showdown matching
#' Shared function used by both modules
#' @param name Player name to correct
#' @return Corrected name (or original if no correction found)
apply_golf_classic_showdown_correction <- function(name) {
  if (is.na(name) || name == "") return(name)
  
  
  # Try exact match first
  if (name %in% names(GOLF_CLASSIC_SHOWDOWN_NAME_CORRECTIONS)) {
    return(GOLF_CLASSIC_SHOWDOWN_NAME_CORRECTIONS[[name]])
  }
  
  # Try lowercase match
  name_lower <- tolower(trimws(name))
  if (name_lower %in% names(GOLF_CLASSIC_SHOWDOWN_NAME_CORRECTIONS)) {
    return(GOLF_CLASSIC_SHOWDOWN_NAME_CORRECTIONS[[name_lower]])
  }
  
  return(name)
}

#' Normalize player name for matching
#' Applies name corrections first, then normalizes
#' @param name Player name to normalize
#' @return Normalized name (lowercase, no special chars)
normalize_classic_name <- function(name) {
  if (is.na(name) || name == "") return("")
  
  # Apply name corrections first (handles FanTeam -> projection name mapping)
  name <- apply_golf_classic_showdown_correction(name)
  
  name_lower <- tolower(trimws(name))
  
  # Handle "LastName, FirstName" format
  if (grepl(",", name_lower)) {
    parts <- strsplit(name_lower, ",")[[1]]
    if (length(parts) == 2) {
      name_lower <- paste(trimws(parts[2]), trimws(parts[1]))
    }
  }
  
  # Remove special characters, normalize spaces
  name_lower <- gsub("[^a-z0-9 ]", " ", name_lower)
  name_lower <- gsub("\\s+", " ", trimws(name_lower))
  
  return(name_lower)
}

#' Load FanTeam salaries for a tournament from the salaries sheet
#' @param tournament_name Name of the sheet/tournament to load
#' @return Data frame with player_name, salary, match_key or NULL if not found
load_classic_fanteam_salaries <- function(tournament_name) {
  log_debug("load_classic_fanteam_salaries() for:", tournament_name, level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    
    # Check if tournament sheet exists in salaries workbook
    ss <- googlesheets4::gs4_get(GOLF_CLASSIC_SALARIES_SHEET_ID)
    available_sheets <- ss$sheets$name
    
    if (!tournament_name %in% available_sheets) {
      log_debug("Tournament not found in salaries sheet:", tournament_name, level = "WARN")
      log_debug("Available sheets:", paste(available_sheets, collapse = ", "), level = "DEBUG")
      return(NULL)
    }
    
    salaries_raw <- googlesheets4::read_sheet(
      GOLF_CLASSIC_SALARIES_SHEET_ID,
      sheet = tournament_name
    ) %>%
      janitor::clean_names()
    
    log_debug("Raw salary columns:", paste(names(salaries_raw), collapse = ", "), level = "DEBUG")
    
    # Check for FanTeam format (f_name + name) or simple format
    if (all(c("f_name", "name") %in% names(salaries_raw))) {
      log_debug("Detected FanTeam export format", level = "INFO")
      salaries <- salaries_raw %>%
        mutate(
          player_name = paste0(f_name, " ", name),
          salary = as.numeric(price)
        )
      # Filter out refuted if column exists
      if ("lineup" %in% names(salaries)) {
        salaries <- salaries %>% filter(lineup != "refuted")
      }
    } else {
      # Simple format - look for name and price columns
      name_col <- intersect(names(salaries_raw), c("player_name", "player", "golfer", "name"))
      price_col <- intersect(names(salaries_raw), c("price", "salary", "sal", "cost"))
      
      if (length(name_col) == 0 || length(price_col) == 0) {
        log_debug("Could not find name or price columns in salaries", level = "ERROR")
        return(NULL)
      }
      
      log_debug("Using name column:", name_col[1], "price column:", price_col[1], level = "DEBUG")
      
      salaries <- salaries_raw %>%
        rename(player_name = !!name_col[1], salary = !!price_col[1]) %>%
        mutate(salary = as.numeric(salary))
    }
    
    # Create match key for joining
    salaries <- salaries %>%
      mutate(match_key = sapply(player_name, normalize_classic_name)) %>%
      select(player_name, salary, match_key) %>%
      filter(!is.na(salary) & !is.na(player_name) & player_name != "")
    
    log_debug("Loaded", nrow(salaries), "FanTeam salaries", level = "INFO")
    return(salaries)
    
  }, error = function(e) {
    log_debug("Error loading FanTeam salaries:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Get available tournaments from Google Sheets
#' Returns sheet names from the projections workbook (full tournaments only, not daily rounds)
get_golf_tournaments_gsheet <- function() {
  log_debug("get_golf_tournaments_gsheet() called", level = "DEBUG")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    ss <- googlesheets4::gs4_get(GOLF_CLASSIC_PROJECTIONS_SHEET_ID)
    tournaments <- ss$sheets$name
    
    # Filter out showdown/daily sheets (typically have "R1", "R2", "Day" in name)
    # Keep full tournament sheets
    full_tournaments <- tournaments[!grepl("R[1-4]|Day|Round", tournaments, ignore.case = TRUE)]
    
    log_debug("Found tournaments:", paste(full_tournaments, collapse = ", "), level = "INFO")
    return(full_tournaments)
    
  }, error = function(e) {
    log_debug("Error getting tournaments:", e$message, level = "ERROR")
    return(character(0))
  })
}

#' Load tournament data from Google Sheets for Classic format
#' IMPORTANT: Starts with SALARY file as base, then adds projections only for
#' players in salary file. This ensures we work with the actual contest field
#' and can identify naming mismatches between sources.
#' @param tournament_name Name of the sheet/tournament to load
#' @return List with: data (player data), unmatched (salary players without projections)
load_golf_tournament_data <- function(tournament_name) {
  log_debug("load_golf_tournament_data() for:", tournament_name, level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    
    # =========================================================================
    # STEP 1: Load SALARIES first (this is the base - actual contest field)
    # =========================================================================
    ft_salaries <- load_classic_fanteam_salaries(tournament_name)
    
    if (is.null(ft_salaries) || nrow(ft_salaries) == 0) {
      log_debug("No FanTeam salaries found for tournament - cannot proceed", level = "ERROR")
      return(list(data = NULL, unmatched = NULL))
    }
    
    log_debug(sprintf("Loaded %d players from salary file", nrow(ft_salaries)), level = "INFO")
    
    # Start with salary data as the base
    data <- ft_salaries %>%
      mutate(player_id = row_number())
    
    # =========================================================================
    # STEP 2: Load PROJECTIONS and join to salary players only
    # =========================================================================
    proj_raw <- tryCatch({
      googlesheets4::read_sheet(
        GOLF_CLASSIC_PROJECTIONS_SHEET_ID,
        sheet = tournament_name
      ) %>%
        janitor::clean_names()
    }, error = function(e) {
      log_debug("Error loading projections sheet:", e$message, level = "ERROR")
      NULL
    })
    
    if (!is.null(proj_raw) && nrow(proj_raw) > 0) {
      log_debug("Projections raw columns:", paste(names(proj_raw), collapse = ", "), level = "DEBUG")
      
      # Find player name column in projections
      name_col <- intersect(names(proj_raw), c("golfer", "player_name", "player", "name"))
      if (length(name_col) > 0) {
        proj_raw <- proj_raw %>% rename(proj_player_name = !!name_col[1])
      } else {
        log_debug("No player name column found in projections", level = "ERROR")
        proj_raw <- NULL
      }
    }
    
    if (!is.null(proj_raw) && nrow(proj_raw) > 0) {
      # Find median projection column
      median_col <- intersect(names(proj_raw), c("median", "dk_points", "projection", "proj", "pts", "fpts", "points"))
      if (length(median_col) > 0) {
        log_debug("Using median column:", median_col[1], level = "DEBUG")
        proj_raw <- proj_raw %>% mutate(median = as.numeric(.data[[median_col[1]]]))
      } else {
        log_debug("No median column found", level = "WARN")
        proj_raw$median <- NA_real_
      }
      
      # Find ceiling projection column
      ceiling_col <- intersect(names(proj_raw), c("ceiling", "ceil", "upside", "high", "dk_ceiling"))
      if (length(ceiling_col) > 0) {
        proj_raw <- proj_raw %>% mutate(ceiling = as.numeric(.data[[ceiling_col[1]]]))
      } else {
        proj_raw$ceiling <- proj_raw$median * 1.2
      }
      
      # Find ownership columns
      own_lg_col <- intersect(names(proj_raw), c("own_large", "ownership_large", "dk_ownership", "ownership", "own"))
      if (length(own_lg_col) > 0) {
        proj_raw <- proj_raw %>% mutate(own_large = as.numeric(gsub("%", "", .data[[own_lg_col[1]]])))
      } else {
        proj_raw$own_large <- NA_real_
      }
      
      own_sm_col <- intersect(names(proj_raw), c("own_small", "ownership_small", "fd_ownership"))
      if (length(own_sm_col) > 0) {
        proj_raw <- proj_raw %>% mutate(own_small = as.numeric(gsub("%", "", .data[[own_sm_col[1]]])))
      } else {
        proj_raw$own_small <- NA_real_
      }
      
      # Create match key for projections
      proj_raw <- proj_raw %>%
        mutate(match_key = sapply(proj_player_name, normalize_classic_name)) %>%
        select(match_key, median, ceiling, own_large, own_small)
      
      log_debug(sprintf("Prepared %d projections for matching", nrow(proj_raw)), level = "INFO")
      
      # Join projections TO salary data (left join keeps all salary players)
      data <- data %>%
        left_join(proj_raw, by = "match_key")
      
      # Count matches and identify unmatched salary players
      matched_count <- sum(!is.na(data$median))
      unmatched_count <- sum(is.na(data$median))
      
      log_debug(sprintf("Matched %d of %d salary players to projections", matched_count, nrow(data)), level = "INFO")
      
      # Get unmatched players (in salary but NOT in projections - naming issues!)
      unmatched_players <- NULL
      if (unmatched_count > 0) {
        unmatched_players <- data %>% 
          filter(is.na(median)) %>% 
          select(player_name, salary) %>%
          arrange(desc(salary))
        
        log_debug(sprintf("WARNING: %d salary players without projection match:", unmatched_count), level = "WARN")
        log_debug(paste(head(unmatched_players$player_name, 10), collapse = ", "), level = "WARN")
      }
      
    } else {
      log_debug("No projections loaded - projection columns will be NA", level = "WARN")
      data$median <- NA_real_
      data$ceiling <- NA_real_
      data$own_large <- NA_real_
      data$own_small <- NA_real_
      unmatched_players <- data %>% select(player_name, salary)
    }
    
    # =========================================================================
    # STEP 3: Calculate derived columns
    # =========================================================================
    data <- data %>%
      mutate(
        # Default ceiling if missing
        ceiling = ifelse(is.na(ceiling) & !is.na(median), median * 1.2, ceiling),
        # Blended projection (median weighted toward ceiling for GPP)
        blended = median * 0.7 + ceiling * 0.3
      )
    
    # Calculate regression-based value metric
    # Fits a line through salary vs projection, value = residual (distance from line)
    # This naturally handles the salary-projection relationship without manual adjustment
    #   positive = projects above the regression line (underpriced for their salary)
    #   negative = projects below the regression line (overpriced for their salary)
    valid_for_value <- data %>% 
      filter(!is.na(blended) & !is.na(salary) & salary > 0)
    
    if (nrow(valid_for_value) >= 3) {
      # Fit linear regression: projection = intercept + slope × salary
      value_model <- lm(blended ~ salary, data = valid_for_value)
      
      log_debug(sprintf("Value regression: intercept=%.2f, slope=%.2f per $1M, R²=%.3f",
                        coef(value_model)[1], coef(value_model)[2], 
                        summary(value_model)$r.squared), level = "INFO")
      
      # Calculate expected projection and residual for ALL players
      data <- data %>%
        mutate(
          expected = predict(value_model, newdata = data.frame(salary = salary)),
          value = ifelse(!is.na(blended) & !is.na(salary) & salary > 0,
                         blended - expected, NA_real_)
        ) %>%
        select(-expected)
    } else {
      log_debug("Not enough valid players for regression, value will be NA", level = "WARN")
      data$value <- NA_real_
    }
    
    # =========================================================================
    # STEP 4: Clean and finalize
    # =========================================================================
    data <- data %>%
      select(
        player_id, player_name, salary, median, ceiling, blended, value,
        own_large, own_small
      ) %>%
      filter(!is.na(player_name) & player_name != "")
    
    # Join headshots from CSV file
    data <- join_golf_headshots(data)
    
    log_debug(sprintf("Final dataset: %d golfers from salary file", nrow(data)), level = "INFO")
    
    # Return both data and unmatched info
    return(list(
      data = data,
      unmatched = if (exists("unmatched_players")) unmatched_players else NULL
    ))
    
  }, error = function(e) {
    log_debug("Error loading tournament data:", e$message, level = "ERROR")
    return(list(data = NULL, unmatched = NULL))
  })
}

# =============================================================================
# UI
# =============================================================================

golf_classic_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("golf_classic_ui() called with id:", id, level = "INFO")
  
  # Get tournaments from Google Sheets
  tournaments <- tryCatch({
    get_golf_tournaments_gsheet()
  }, error = function(e) {
    log_debug("Error getting tournaments:", e$message, level = "ERROR")
    character(0)
  })
  
  contest_choices <- if (length(tournaments) > 0) {
    setNames(tournaments, tournaments)
  } else {
    c("No tournaments found" = "")
  }
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Golf Classic"),
      tags$p(class = "text-muted", "Build 6-golfer lineups for full tournament contests")
    ),
    
    # =========================================================================
    # SETTINGS
    # =========================================================================
    ui_card(
      title = "Settings",
      color = GOLF_CARD_COLOR,
      
      fluidRow(
        column(4,
               selectizeInput(ns("contest_select"), "Tournament",
                              choices = contest_choices,
                              selected = if (length(tournaments) > 0) tournaments[1] else NULL
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

golf_classic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("golf_classic_server() initialized", level = "INFO")
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
      log_debug(">>> Loading tournament:", input$contest_select, level = "INFO")
      
      # Load data from Google Sheets
      showNotification("Loading data from Google Sheets...", type = "message", duration = 2)
      
      result <- load_golf_tournament_data(input$contest_select)
      
      if (is.null(result$data) || nrow(result$data) == 0) {
        showNotification("Failed to load tournament data - check salary file exists", type = "error")
        return()
      }
      
      rv$player_data <- result$data
      
      # Unmatched players are those in salary file but NOT matched to projections
      # This is the key diagnostic for naming format issues
      rv$unmatched_players <- if (!is.null(result$unmatched) && nrow(result$unmatched) > 0) {
        result$unmatched$player_name
      } else {
        character(0)
      }
      
      matched_count <- sum(!is.na(rv$player_data$median))
      showNotification(
        sprintf("Loaded %d golfers from salary file (%d matched to projections)", 
                nrow(rv$player_data), matched_count),
        type = "message"
      )
      
      player_choices <- rv$player_data %>%
        filter(!is.na(median)) %>%
        arrange(desc(blended)) %>%
        pull(player_name)
      
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
        tags$strong(icon("exclamation-triangle"), sprintf(" %d Salary Players Without Projections: ", length(unmatched))),
        tags$p(
          style = "font-size: 0.8rem; margin-top: 0.3rem; margin-bottom: 0;",
          "These players are in the salary file but couldn't be matched to projections. Check for naming format differences."
        ),
        tags$span(
          style = "font-size: 0.85rem; display: block; margin-top: 0.3rem;",
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
            
            # Lock badge
            div(
              style = "background: var(--accent-sage); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("lock"), " LOCKED"
            ),
            
            # Player name
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              p
            ),
            
            # Remove button
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
            
            # Exclude badge
            div(
              style = "background: var(--accent-coral); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("ban"), " EXCLUDED"
            ),
            
            # Player name
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              p
            ),
            
            # Remove button
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
      
      key <- paste0(input$grouped_action, "_", input$grouped_pct %||% 10)
      
      if (key %in% names(rv$grouped_rules)) {
        rv$grouped_rules[[key]]$players <- unique(c(rv$grouped_rules[[key]]$players, input$grouped_players))
      } else {
        rv$grouped_rules[[key]] <- list(
          action = input$grouped_action,
          pct = input$grouped_pct %||% 10,
          players = input$grouped_players
        )
      }
      
      updateSelectizeInput(session, "grouped_players", selected = character(0))
    })
    
    observeEvent(input$remove_grouped_rule, {
      rv$grouped_rules[[input$remove_grouped_rule]] <- NULL
    })
    
    output$grouped_rules_display <- renderUI({
      rules <- rv$grouped_rules
      if (length(rules) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem; margin-top: 1rem;",
        lapply(names(rules), function(key) {
          rule <- rules[[key]]
          is_boost <- rule$action == "Boost"
          
          div(
            style = sprintf(
              "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid %s; border-radius: 6px;",
              if (is_boost) "var(--accent-sage)" else "var(--accent-coral)"
            ),
            
            # Action badge
            div(
              style = sprintf(
                "background: %s; color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem; white-space: nowrap;",
                if (is_boost) "var(--accent-sage)" else "var(--accent-coral)"
              ),
              sprintf("%s %d%%", toupper(rule$action), rule$pct)
            ),
            
            # Player names
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              paste(rule$players, collapse = ", ")
            ),
            
            # Remove button
            actionButton(
              ns(paste0("remove_grouped_", gsub("[^a-zA-Z0-9]", "_", key))),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_grouped_rule"), key)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # CORRELATION RULES (supports multiple boost/dock groups per trigger)
    # =========================================================================
    
    observeEvent(input$add_corr_rule, {
      req(input$corr_trigger)
      req(input$corr_trigger != "")
      
      trigger <- input$corr_trigger
      boost_targets <- input$corr_boost_targets %||% character(0)
      boost_pct <- input$corr_boost_pct %||% 10
      dock_targets <- input$corr_dock_targets %||% character(0)
      dock_pct <- input$corr_dock_pct %||% 10
      
      if (trigger %in% names(rv$corr_rules)) {
        existing <- rv$corr_rules[[trigger]]
        
        # Add boost targets to matching pct group or create new group
        if (length(boost_targets) > 0) {
          matched <- FALSE
          for (i in seq_along(existing$boost_groups)) {
            if (existing$boost_groups[[i]]$pct == boost_pct) {
              existing$boost_groups[[i]]$targets <- unique(c(existing$boost_groups[[i]]$targets, boost_targets))
              matched <- TRUE
              break
            }
          }
          if (!matched) {
            existing$boost_groups <- append(existing$boost_groups, list(list(pct = boost_pct, targets = boost_targets)))
          }
        }
        
        # Add dock targets to matching pct group or create new group
        if (length(dock_targets) > 0) {
          matched <- FALSE
          for (i in seq_along(existing$dock_groups)) {
            if (existing$dock_groups[[i]]$pct == dock_pct) {
              existing$dock_groups[[i]]$targets <- unique(c(existing$dock_groups[[i]]$targets, dock_targets))
              matched <- TRUE
              break
            }
          }
          if (!matched) {
            existing$dock_groups <- append(existing$dock_groups, list(list(pct = dock_pct, targets = dock_targets)))
          }
        }
        
        rv$corr_rules[[trigger]] <- existing
      } else {
        rv$corr_rules[[trigger]] <- list(
          trigger = trigger,
          boost_groups = if (length(boost_targets) > 0) list(list(pct = boost_pct, targets = boost_targets)) else list(),
          dock_groups = if (length(dock_targets) > 0) list(list(pct = dock_pct, targets = dock_targets)) else list()
        )
      }
      
      updateSelectizeInput(session, "corr_trigger", selected = "")
      updateSelectizeInput(session, "corr_boost_targets", selected = character(0))
      updateSelectizeInput(session, "corr_dock_targets", selected = character(0))
    })
    
    observeEvent(input$remove_corr_rule, {
      rv$corr_rules[[input$remove_corr_rule]] <- NULL
    })
    
    output$corr_rules_display <- renderUI({
      rules <- rv$corr_rules
      if (length(rules) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem; margin-top: 1rem;",
        lapply(names(rules), function(trigger) {
          rule <- rules[[trigger]]
          
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-plum); border-radius: 6px;",
            
            # IF badge
            div(
              style = "background: var(--text-primary); color: white; padding: 0.2rem 0.4rem; border-radius: 4px; font-size: 0.7rem; font-weight: 700; margin-right: 0.5rem;",
              "IF"
            ),
            
            # Trigger name
            div(
              style = "font-weight: 600; font-size: 0.9rem; min-width: 120px;",
              trigger
            ),
            
            # Arrow
            span(style = "color: var(--text-muted); font-size: 1.2rem; margin: 0 0.5rem;", HTML("&#8594;")),
            
            # Effects
            div(
              style = "flex: 1; display: flex; flex-wrap: wrap; gap: 0.4rem; align-items: center;",
              
              # Boost groups
              lapply(rule$boost_groups, function(group) {
                div(
                  style = "display: inline-flex; align-items: center; gap: 0.25rem; padding: 0.15rem 0.4rem; background: rgba(139, 168, 134, 0.2); border-radius: 4px;",
                  span(
                    style = "font-weight: 700; color: var(--accent-sage); font-size: 0.75rem;",
                    sprintf("+%d%%", group$pct)
                  ),
                  span(style = "font-size: 0.85rem; font-weight: 500;", paste(group$targets, collapse = ", "))
                )
              }),
              
              # Dock groups
              lapply(rule$dock_groups, function(group) {
                div(
                  style = "display: inline-flex; align-items: center; gap: 0.25rem; padding: 0.15rem 0.4rem; background: rgba(232, 131, 121, 0.2); border-radius: 4px;",
                  span(
                    style = "font-weight: 700; color: var(--accent-coral); font-size: 0.75rem;",
                    sprintf("-%d%%", group$pct)
                  ),
                  span(style = "font-size: 0.85rem; font-weight: 500;", paste(group$targets, collapse = ", "))
                )
              })
            ),
            
            # Remove button
            actionButton(
              ns(paste0("remove_corr_", gsub("[^a-zA-Z0-9]", "_", trigger))),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_corr_rule"), trigger)
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
      
      proj_col <- input$projection_type %||% "blended"
      heatmap_col <- input$pool_heatmap %||% "none"
      
      pool_data <- rv$player_data %>%
        filter(!is.na(.data[[proj_col]])) %>%
        mutate(
          excluded = player_name %in% rv$excluded_players,
          locked = player_name %in% rv$locked_players
        ) %>%
        arrange(desc(.data[[proj_col]]))
      
      # Sequential heatmap: white to teal (for projection columns)
      get_sequential_style <- function(value, col_values, col_name) {
        if (heatmap_col != col_name || is.na(value)) return(list(verticalAlign = "middle"))
        min_val <- min(col_values, na.rm = TRUE)
        max_val <- max(col_values, na.rm = TRUE)
        if (max_val == min_val) return(list(verticalAlign = "middle"))
        
        t <- (value - min_val) / (max_val - min_val)
        t <- max(0, min(1, t))
        r <- round(255 + (143 - 255) * t)
        g <- round(255 + (188 - 255) * t)
        b <- round(255 + (187 - 255) * t)
        list(background = sprintf("rgb(%d, %d, %d)", r, g, b), verticalAlign = "middle")
      }
      
      # Diverging heatmap: coral -> white -> teal (for value column, midpoint = 0)
      get_diverging_style <- function(value, col_values) {
        if (heatmap_col != "value" || is.na(value)) return(list(verticalAlign = "middle"))
        
        midpoint <- 0
        min_val <- min(col_values, na.rm = TRUE)
        max_val <- max(col_values, na.rm = TRUE)
        
        if (value < midpoint) {
          if (min_val >= midpoint) min_val <- -1
          t <- (value - min_val) / (midpoint - min_val)
          t <- max(0, min(1, t))
          # Coral to white
          r <- round(208 + (255 - 208) * t)
          g <- round(135 + (255 - 135) * t)
          b <- round(112 + (255 - 112) * t)
        } else {
          if (max_val <= midpoint) max_val <- 1
          t <- (value - midpoint) / (max_val - midpoint)
          t <- max(0, min(1, t))
          # White to teal
          r <- round(255 + (143 - 255) * t)
          g <- round(255 + (188 - 255) * t)
          b <- round(255 + (187 - 255) * t)
        }
        list(background = sprintf("rgb(%d, %d, %d)", r, g, b), verticalAlign = "middle")
      }
      
      # Pre-compute column values for heatmap scaling
      blend_vals <- pool_data$blended
      med_vals <- pool_data$median
      ceil_vals <- pool_data$ceiling
      value_vals <- pool_data$value
      
      reactable(
        pool_data,
        theme = app_reactable_theme(compact = TRUE),
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        rowStyle = function(index) {
          if (pool_data$excluded[index]) {
            list(opacity = "0.4", textDecoration = "line-through")
          } else if (pool_data$locked[index]) {
            list(background = "#FFF9E6")
          }
        },
        defaultColDef = colDef(
          vAlign = "center",
          style = list(verticalAlign = "middle")
        ),
        columns = list(
          player_id = colDef(show = FALSE),
          player_name = colDef(
            name = "Golfer",
            minWidth = 160,
            style = list(verticalAlign = "middle"),
            cell = function(value, index) {
              headshot_url <- pool_data$headshot_url[index]
              div(
                style = "display: flex; align-items: center; gap: 0.5rem;",
                div(
                  class = "player-headshot player-headshot--sm",
                  style = "background: #e8e8e8; flex-shrink: 0;",
                  tags$img(
                    src = headshot_url,
                    style = "width: 100%; height: 100%; object-fit: cover;",
                    onerror = sprintf("this.src='%s'", GOLF_DEFAULT_HEADSHOT)
                  )
                ),
                span(style = "font-weight: 600;", value)
              )
            }
          ),
          salary = colDef(name = "Salary", format = colFormat(prefix = "$", suffix = "M", digits = 1), width = 115, align = "right", style = list(verticalAlign = "middle")),
          blended = colDef(
            name = "Blend",
            format = colFormat(digits = 1),
            width = 100,
            align = "right",
            style = function(value) get_sequential_style(value, blend_vals, "blended")
          ),
          median = colDef(
            name = "Med",
            format = colFormat(digits = 1),
            width = 95,
            align = "right",
            style = function(value) get_sequential_style(value, med_vals, "median")
          ),
          ceiling = colDef(
            name = "Ceil",
            format = colFormat(digits = 1),
            width = 95,
            align = "right",
            style = function(value) get_sequential_style(value, ceil_vals, "ceiling")
          ),
          value = colDef(
            name = "Value",
            width = 100,
            align = "right",
            cell = function(value) {
              if (is.na(value)) return("-")
              sprintf("%+.2f", value)
            },
            style = function(value) get_diverging_style(value, value_vals)
          ),
          own_large = colDef(name = "Own Lg", format = colFormat(suffix = "%", digits = 1), width = 95, align = "right", style = list(verticalAlign = "middle")),
          own_small = colDef(name = "Own Sm", format = colFormat(suffix = "%", digits = 1), width = 95, align = "right", style = list(verticalAlign = "middle")),
          headshot_url = colDef(show = FALSE),
          cut_odds = colDef(show = FALSE),
          dk_salary = colDef(show = FALSE),
          volatility = colDef(show = FALSE),
          excluded = colDef(show = FALSE),
          locked = colDef(show = FALSE)
        )
      )
    })
    
    # =========================================================================
    # LINEUP GENERATION
    # =========================================================================
    
    # Helper to expand correlation rules for optimizer
    expand_corr_rules_for_optimizer <- function(rules) {
      expanded <- list()
      for (trigger in names(rules)) {
        rule <- rules[[trigger]]
        # Combine all boost groups for this trigger
        all_boost_targets <- character(0)
        all_dock_targets <- character(0)
        boost_pct <- 0
        dock_pct <- 0
        
        # Process boost groups (apply each group separately)
        for (group in rule$boost_groups) {
          expanded <- append(expanded, list(list(
            trigger = trigger,
            boost_targets = group$targets,
            boost_pct = group$pct,
            dock_targets = character(0),
            dock_pct = 0
          )))
        }
        
        # Process dock groups (apply each group separately)
        for (group in rule$dock_groups) {
          expanded <- append(expanded, list(list(
            trigger = trigger,
            boost_targets = character(0),
            boost_pct = 0,
            dock_targets = group$targets,
            dock_pct = group$pct
          )))
        }
      }
      expanded
    }
    
    observeEvent(input$generate_btn, {
      req(rv$player_data)
      
      log_debug(">>> Generate button clicked", level = "INFO")
      
      proj_col <- input$projection_type %||% "blended"
      num_lineups <- input$num_lineups %||% 10
      variance <- input$variance_pct %||% 15
      min_unique <- input$min_unique %||% 1
      salary_cap <- input$salary_cap %||% 100
      
      players_adj <- rv$player_data %>%
        filter(!is.na(.data[[proj_col]])) %>%
        mutate(adjusted_projection = .data[[proj_col]])
      
      # Apply grouped rules
      for (rule in rv$grouped_rules) {
        factor <- if (rule$action == "Boost") (1 + rule$pct / 100) else (1 - rule$pct / 100)
        players_adj <- players_adj %>%
          mutate(adjusted_projection = if_else(
            player_name %in% rule$players,
            adjusted_projection * factor,
            adjusted_projection
          ))
      }
      
      # Expand correlation rules for optimizer
      expanded_corr_rules <- expand_corr_rules_for_optimizer(rv$corr_rules)
      
      # Calculate optimal
      optimal <- optimize_golf_classic_lp(
        players = players_adj,
        projection_col = "adjusted_projection",
        salary_cap = salary_cap,
        locked_players = rv$locked_players,
        excluded_players = rv$excluded_players,
        corr_rules = expanded_corr_rules
      )
      
      rv$optimal_projection <- if (!is.null(optimal)) sum(optimal$projection) else NA
      
      showNotification(sprintf("Generating %d lineups...", num_lineups), type = "message", duration = 2)
      
      lineups <- generate_golf_classic_lineups(
        players = players_adj,
        projection_col = "adjusted_projection",
        num_lineups = num_lineups,
        variance_pct = variance,
        salary_cap = salary_cap,
        locked_players = rv$locked_players,
        excluded_players = rv$excluded_players,
        min_unique = min_unique,
        corr_rules = expanded_corr_rules
      )
      
      rv$generated_lineups <- lineups
      
      if (length(lineups) > 0) {
        showNotification(sprintf("Generated %d unique lineups", length(lineups)), type = "message")
      } else {
        showNotification("No valid lineups generated", type = "warning")
      }
    })
    
    # =========================================================================
    # SUMMARY STATS (order matches columns: Sal, Proj, Own Lg, Own Sm)
    # =========================================================================
    
    output$lineup_summary_stats <- renderUI({
      n <- length(rv$generated_lineups)
      opt <- rv$optimal_projection
      
      if (n == 0) return(NULL)
      
      projections <- sapply(rv$generated_lineups, function(l) sum(l$projection))
      
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
      projections <- sapply(lineups, function(l) sum(l$projection))
      sorted_indices <- if (!is.na(opt)) order(abs(projections - opt)) else seq_along(lineups)
      
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
        lapply(sorted_indices, function(i) {
          lineup <- lineups[[i]]
          total_proj <- sum(lineup$projection)
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
                
                own_lg <- if (nrow(player_info) > 0 && "own_large" %in% names(player_info)) {
                  player_info$own_large[1]
                } else {
                  NA
                }
                
                own_sm <- if (nrow(player_info) > 0 && "own_small" %in% names(player_info)) {
                  player_info$own_small[1]
                } else {
                  NA
                }
                
                # Split name
                name_parts <- strsplit(lineup$player_name[j], " ")[[1]]
                first_name <- if (length(name_parts) >= 1) name_parts[1] else ""
                last_name <- if (length(name_parts) >= 2) paste(name_parts[-1], collapse = " ") else ""
                
                div(
                  style = "display: flex; align-items: center; gap: 0.35rem; padding: 0.2rem 0.3rem; background: var(--bg-tertiary); border-radius: 4px;",
                  
                  # Headshot with dark border
                  div(
                    style = "width: 28px; height: 28px; border-radius: 50%; background: #e8e8e8; overflow: hidden; flex-shrink: 0; border: 2px solid var(--text-primary);",
                    tags$img(
                      src = headshot_url,
                      style = "width: 100%; height: 100%; object-fit: cover;",
                      onerror = sprintf("this.src='%s'", GOLF_DEFAULT_HEADSHOT)
                    )
                  ),
                  
                  # Name stacked (narrower)
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
                  
                  # PROJ
                  div(
                    style = "text-align: center; width: 35px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "PROJ"),
                    div(style = "font-size: 0.7rem; font-weight: 600; color: var(--accent-coral);", sprintf("%.0f", lineup$projection[j]))
                  ),
                  
                  # OWN LG
                  div(
                    style = "text-align: center; width: 35px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "LG"),
                    div(style = "font-size: 0.7rem; font-weight: 600;", if (!is.na(own_lg)) sprintf("%.0f%%", own_lg) else "-")
                  ),
                  
                  # OWN SM
                  div(
                    style = "text-align: center; width: 35px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "SM"),
                    div(style = "font-size: 0.7rem; font-weight: 600;", if (!is.na(own_sm)) sprintf("%.0f%%", own_sm) else "-")
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

cat("Golf Classic module loaded: golf_classic_ui(), golf_classic_server()\n")