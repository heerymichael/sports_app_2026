# =============================================================================
# Soccer Data Loader - UPDATED
# 
# CHANGES:
#   1. Uses new combined Player_Match_Stats sheet (Summary + Possession)
#   2. Uses Arrow/Parquet for ~10x faster cache reads
#   3. Single load function for player data
#
# Dependencies: soccer_config.R, soccer_cache.R, arrow package
# =============================================================================

# =============================================================================
# GOOGLE SHEETS INITIALIZATION
# =============================================================================

#' Initialize Google Sheets authentication
#' @description Sets up googlesheets4 for public sheet access (no auth needed)
init_google_sheets <- function() {
  log_debug("Initializing Google Sheets access (public, no auth)...", level = "INFO")
  googlesheets4::gs4_deauth()
  log_debug("Google Sheets initialized for public access", level = "INFO")
}

# =============================================================================
# GOOGLE DRIVE LOADING (FAST - Parquet files)
# =============================================================================

#' Check if googledrive package is available
has_googledrive <- function() {
  requireNamespace("googledrive", quietly = TRUE)
}

#' Download and read Parquet file from Google Drive
#' @param file_id Google Drive file ID
#' @param data_type Type of data for cache path naming
#' @return Data frame or NULL if failed
load_from_google_drive <- function(file_id, data_type) {
  if (is.null(file_id) || !USE_GOOGLE_DRIVE) {
    return(NULL)
  }
  
  if (!has_googledrive() || !has_arrow()) {
    log_debug("googledrive or arrow package not available", level = "WARN")
    return(NULL)
  }
  
  log_debug(sprintf("Loading %s from Google Drive...", data_type), level = "INFO")
  
  tryCatch({
    # Deauth for public files
    googledrive::drive_deauth()
    
    # Create temp file for download
    temp_file <- tempfile(fileext = ".parquet")
    
    # Download from Drive
    googledrive::drive_download(
      googledrive::as_id(file_id),
      path = temp_file,
      overwrite = TRUE
    )
    
    # Read Parquet
    data <- arrow::read_parquet(temp_file)
    
    # Clean up temp file
    unlink(temp_file)
    
    log_debug(sprintf("Loaded %d rows from Google Drive", nrow(data)), level = "INFO")
    return(as.data.frame(data))
    
  }, error = function(e) {
    log_debug(sprintf("Google Drive load failed: %s", e$message), level = "WARN")
    return(NULL)
  })
}

# =============================================================================
# PARQUET CACHE FUNCTIONS
# =============================================================================

#' Check if arrow package is available
has_arrow <- function() {
  requireNamespace("arrow", quietly = TRUE)
}

#' Get cache file path (Parquet or RDS based on config)
#' @param data_type Type of data (e.g., "player_stats", "shots")
#' @return Full path to cache file
get_cache_path_v2 <- function(data_type) {
  if (!dir.exists(CACHE_DIR)) {
    dir.create(CACHE_DIR, recursive = TRUE)
  }
  
  ext <- if (USE_PARQUET_CACHE && has_arrow()) ".parquet" else ".rds"
  file.path(CACHE_DIR, paste0("soccer_", data_type, ext))
}

#' Check if cache is valid (exists and not expired)
#' @param cache_path Path to cache file
#' @return TRUE if cache is valid
is_cache_valid_v2 <- function(cache_path) {
  if (!file.exists(cache_path)) {
    return(FALSE)
  }
  
  file_age_hours <- difftime(Sys.time(), file.mtime(cache_path), units = "hours")
  return(as.numeric(file_age_hours) < CACHE_MAX_AGE_HOURS)
}

#' Save data to cache (Parquet or RDS)
#' @param data Data frame to cache
#' @param data_type Type of data
save_to_cache_v2 <- function(data, data_type) {
  cache_path <- get_cache_path_v2(data_type)
  
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

#' Load data from cache (Parquet or RDS)
#' @param data_type Type of data
#' @return Data frame or NULL
load_from_cache_v2 <- function(data_type) {
  cache_path <- get_cache_path_v2(data_type)
  
  if (!file.exists(cache_path)) {
    return(NULL)
  }
  
  tryCatch({
    if (grepl("\\.parquet$", cache_path) && has_arrow()) {
      data <- arrow::read_parquet(cache_path)
    } else {
      data <- readRDS(cache_path)
    }
    return(as.data.frame(data))
  }, error = function(e) {
    log_debug(sprintf("Failed to load cache: %s", e$message), level = "WARN")
    return(NULL)
  })
}

# =============================================================================
# PRIMARY DATA LOADING FUNCTION
# =============================================================================

#' Load Player Match Stats (Combined Summary + Possession)
#' @param force_refresh If TRUE, bypass cache and reload from source
#' @return Data frame with combined player match statistics
load_player_match_stats <- function(force_refresh = FALSE) {
  log_debug("========================================", level = "INFO")
  log_debug("load_player_match_stats() called", level = "INFO")
  
  cache_path <- get_cache_path_v2("player_stats")
  
  
  # 1. Check cache first (fastest - instant)
  if (!force_refresh && is_cache_valid_v2(cache_path)) {
    log_debug("Using cached player stats data", level = "INFO")
    data <- load_from_cache_v2("player_stats")
    if (!is.null(data)) {
      log_debug(sprintf("Loaded %d rows from cache", nrow(data)), level = "INFO")
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  # 2. Try Google Drive Parquet (fast - ~5-15 seconds)
  drive_id <- SOCCER_DRIVE_IDS$player_match_stats
  if (!is.null(drive_id)) {
    data <- load_from_google_drive(drive_id, "player_stats")
    if (!is.null(data)) {
      # Normalize team names
      data <- normalize_team_columns(data, c("team", "squad", "home_team", "away_team"))
      # Save to cache for next time
      save_to_cache_v2(data, "player_stats")
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  # 3. Fall back to Google Sheets (slow - ~3 minutes)
  log_debug("Loading player stats from Google Sheets (slow)...", level = "INFO")
  
  tryCatch({
    data <- googlesheets4::read_sheet(
      SOCCER_SHEET_IDS$player_match_stats,
      sheet = "Player_Match_Stats"
    ) %>%
      as.data.frame()
    
    # Normalize team names
    data <- normalize_team_columns(data, c("team", "squad", "home_team", "away_team"))
    
    # Coerce numeric columns (Google Sheets often returns character)
    numeric_cols <- c("minutes", "goals", "assists", "shots", "xg", "sca", "gca", 
                      "touches", "progressive_passes", "progressive_carries",
                      "touches_att_3rd", "touches_att_pen_area", "progressive_passes_received",
                      "gameweek")
    for (col in numeric_cols) {
      if (col %in% names(data)) {
        data[[col]] <- as.numeric(data[[col]])
      }
    }
    
    log_debug(sprintf("Loaded %d rows from Google Sheets", nrow(data)), level = "INFO")
    
    # Save to cache
    save_to_cache_v2(data, "player_stats")
    
    log_debug("========================================", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug(sprintf("Error loading player stats: %s", e$message), level = "ERROR")
    
    # Try to return cached data even if expired
    if (file.exists(cache_path)) {
      log_debug("Falling back to expired cache", level = "WARN")
      return(load_from_cache_v2("player_stats"))
    }
    
    return(NULL)
  })
}

#' Load Shot Data (with caching)
#' @param force_refresh If TRUE, bypass cache and reload from source
#' @return Data frame with individual shot data
load_shot_data <- function(force_refresh = FALSE) {
  log_debug("========================================", level = "INFO")
  log_debug("load_shot_data() called", level = "INFO")
  
  cache_path <- get_cache_path_v2("shots")
  
  # 1. Check cache first (fastest)
  if (!force_refresh && is_cache_valid_v2(cache_path)) {
    log_debug("Using cached shot data", level = "INFO")
    data <- load_from_cache_v2("shots")
    if (!is.null(data)) {
      log_debug(sprintf("Loaded %d rows from cache", nrow(data)), level = "INFO")
      if ("league" %in% names(data)) {
        leagues_in_data <- unique(data$league)
        log_debug("Leagues in cached data:", paste(leagues_in_data, collapse = ", "), level = "INFO")
      }
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  # 2. Try Google Drive Parquet (fast)
  drive_id <- SOCCER_DRIVE_IDS$shots
  if (!is.null(drive_id)) {
    data <- load_from_google_drive(drive_id, "shots")
    if (!is.null(data)) {
      # Normalize team names
      data <- normalize_team_columns(data, c("squad", "home_team", "away_team"))
      # Save to cache
      save_to_cache_v2(data, "shots")
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  # 3. Fall back to Google Sheets (slow)
  log_debug("Loading shot data from Google Sheets (slow)...", level = "INFO")
  
  tryCatch({
    data <- googlesheets4::read_sheet(
      SOCCER_SHEET_IDS$shots,
      sheet = "Shot_Data"
    ) %>%
      as.data.frame()
    
    # Normalize team names
    data <- normalize_team_columns(data, c("squad", "home_team", "away_team"))
    
    # Coerce numeric columns (Google Sheets often returns character)
    numeric_cols <- c("xg_shot", "minute", "gameweek")
    for (col in numeric_cols) {
      if (col %in% names(data)) {
        data[[col]] <- as.numeric(data[[col]])
      }
    }
    
    log_debug(sprintf("Loaded %d rows from Google Sheets", nrow(data)), level = "INFO")
    
    # Log leagues present in fresh data
    if ("league" %in% names(data)) {
      leagues_in_data <- unique(data$league)
      log_debug("Leagues in fresh data:", paste(leagues_in_data, collapse = ", "), level = "INFO")
    }
    
    # Save to cache
    save_to_cache_v2(data, "shots")
    
    log_debug("========================================", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug(sprintf("Error loading shot data: %s", e$message), level = "ERROR")
    
    if (file.exists(cache_path)) {
      log_debug("Falling back to expired cache", level = "WARN")
      return(load_from_cache_v2("shots"))
    }
    
    return(NULL)
  })
}

#' Load Team Goals Data (with caching)
#' @param force_refresh If TRUE, bypass cache and reload from source
#' @return Data frame with match results
load_team_goals <- function(force_refresh = FALSE) {
  log_debug("========================================", level = "INFO")
  log_debug("load_team_goals() called", level = "INFO")
  
  cache_path <- get_cache_path_v2("team_goals")
  
  # 1. Check cache first (fastest)
  if (!force_refresh && is_cache_valid_v2(cache_path)) {
    log_debug("Using cached team goals data", level = "INFO")
    data <- load_from_cache_v2("team_goals")
    if (!is.null(data)) {
      log_debug(sprintf("Loaded %d rows from cache", nrow(data)), level = "INFO")
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  # 2. Try Google Drive Parquet (fast)
  drive_id <- SOCCER_DRIVE_IDS$team_goals
  if (!is.null(drive_id)) {
    data <- load_from_google_drive(drive_id, "team_goals")
    if (!is.null(data)) {
      # Normalize team names
      data <- normalize_team_columns(data, c("home_team", "away_team"))
      # Save to cache
      save_to_cache_v2(data, "team_goals")
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  # 3. Fall back to Google Sheets (slow)
  log_debug("Loading team goals from Google Sheets...", level = "INFO")
  
  tryCatch({
    data <- googlesheets4::read_sheet(
      SOCCER_SHEET_IDS$team_goals,
      sheet = "Team_Goals"
    ) %>%
      as.data.frame()
    
    # Normalize team names
    data <- normalize_team_columns(data, c("home_team", "away_team"))
    
    # Coerce numeric columns (Google Sheets often returns character or list)
    numeric_cols <- c("home_goals", "away_goals", "gameweek")
    for (col in numeric_cols) {
      if (col %in% names(data)) {
        # Handle list columns from Google Sheets
        if (is.list(data[[col]])) {
          data[[col]] <- sapply(data[[col]], function(x) if(length(x) == 0) NA else as.numeric(x[[1]]))
        } else {
          data[[col]] <- as.numeric(data[[col]])
        }
      }
    }
    
    # Ensure character columns are not lists
    char_cols <- c("home_team", "away_team", "league", "match_date")
    for (col in char_cols) {
      if (col %in% names(data) && is.list(data[[col]])) {
        data[[col]] <- sapply(data[[col]], function(x) if(length(x) == 0) NA_character_ else as.character(x[[1]]))
      }
    }
    
    log_debug(sprintf("Loaded %d rows from Google Sheets", nrow(data)), level = "INFO")
    
    # Save to cache
    save_to_cache_v2(data, "team_goals")
    
    log_debug("========================================", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug(sprintf("Error loading team goals: %s", e$message), level = "ERROR")
    
    if (file.exists(cache_path)) {
      log_debug("Falling back to expired cache", level = "WARN")
      return(load_from_cache_v2("team_goals"))
    }
    
    return(NULL)
  })
}

# =============================================================================
# BACKWARD COMPATIBILITY FUNCTIONS
# =============================================================================

#' Load Shooting Summary Data (DEPRECATED - use load_player_match_stats)
#' @description Kept for backward compatibility during transition
load_shooting_summary <- function(force_refresh = FALSE) {
  log_debug("load_shooting_summary() is deprecated - using load_player_match_stats()", level = "WARN")
  return(load_player_match_stats(force_refresh))
}

#' Load Possession Data (DEPRECATED - use load_player_match_stats)
#' @description Kept for backward compatibility during transition
load_possession_data <- function(force_refresh = FALSE) {
  log_debug("load_possession_data() is deprecated - using load_player_match_stats()", level = "WARN")
  return(load_player_match_stats(force_refresh))
}

# =============================================================================
# QUERY FUNCTIONS
# =============================================================================

#' Get available leagues from data
#' @param data Data frame with league column
#' @return Character vector of league names (display format)
get_available_leagues <- function(data) {
  if (is.null(data)) {
    log_debug("get_available_leagues: data is NULL", level = "WARN")
    return(character(0))
  }
  
  if (!"league" %in% names(data)) {
    log_debug("get_available_leagues: no 'league' column in data", level = "WARN")
    log_debug("Available columns:", paste(names(data), collapse = ", "), level = "DEBUG")
    return(character(0))
  }
  
  leagues <- unique(data$league)
  leagues <- leagues[!is.na(leagues) & leagues != ""]
  
  log_debug("get_available_leagues: raw leagues in data:", paste(leagues, collapse = ", "), level = "INFO")
  log_debug("get_available_leagues: LEAGUE_DISPLAY_NAMES keys:", paste(names(LEAGUE_DISPLAY_NAMES), collapse = ", "), level = "DEBUG")
  
  # Convert to display names
  display_names <- LEAGUE_DISPLAY_NAMES[leagues]
  matched <- !is.na(display_names)
  
  if (any(!matched)) {
    unmatched <- leagues[!matched]
    log_debug("get_available_leagues: UNMATCHED leagues:", paste(unmatched, collapse = ", "), level = "WARN")
  }
  
  display_names <- display_names[matched]
  
  log_debug("get_available_leagues: returning", length(display_names), "leagues:", 
            paste(display_names, collapse = ", "), level = "INFO")
  
  return(as.character(display_names))
}

#' Get teams for a specific league
#' @param data Data frame with league and team columns
#' @param league League name (display format)
#' @return Character vector of team names
get_league_teams <- function(data, league) {
  if (is.null(data) || is.null(league)) return(character(0))
  
  # Convert display name to data name
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Get team column - might be 'team' or 'squad'
  team_col <- if ("team" %in% names(data)) "team" else if ("squad" %in% names(data)) "squad" else NULL
  
  if (is.null(team_col)) {
    log_debug("No team column found in data", level = "WARN")
    return(character(0))
  }
  
  teams <- data %>%
    filter(league == league_data_name) %>%
    pull(!!sym(team_col)) %>%
    unique()
  
  # Normalize team names for consistency with stats calculations
  teams <- normalize_team_names(teams)
  
  teams <- teams[!is.na(teams) & teams != ""]
  teams <- sort(unique(teams))
  
  return(teams)
}

#' Get players for a specific team
#' @param data Data frame with team and player columns
#' @param team Team name
#' @return Character vector of player names
get_team_players <- function(data, team) {
  if (is.null(data) || is.null(team)) return(character(0))
  
  team_col <- if ("team" %in% names(data)) "team" else if ("squad" %in% names(data)) "squad" else NULL
  
  if (is.null(team_col)) return(character(0))
  
  players <- data %>%
    filter(!!sym(team_col) == team) %>%
    pull(player) %>%
    unique() %>%
    sort()
  
  players <- players[!is.na(players) & players != ""]
  
  return(players)
}

# =============================================================================
# CACHE MANAGEMENT
# =============================================================================

#' Clear all soccer data caches
#' @description Removes all cached files to force fresh reload
clear_soccer_cache <- function() {
  log_debug("Clearing soccer data cache...", level = "INFO")
  
  cache_files <- list.files(CACHE_DIR, pattern = "^soccer_.*\\.(parquet|rds)$", full.names = TRUE)
  
  if (length(cache_files) > 0) {
    file.remove(cache_files)
    log_debug(sprintf("Removed %d cache files", length(cache_files)), level = "INFO")
  } else {
    log_debug("No cache files to remove", level = "INFO")
  }
}

#' Get cache status
#' @return Data frame with cache file info
get_cache_status <- function() {
  cache_files <- list.files(CACHE_DIR, pattern = "^soccer_.*\\.(parquet|rds)$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    return(data.frame(
      file = character(0),
      size_mb = numeric(0),
      age_hours = numeric(0),
      valid = logical(0)
    ))
  }
  
  info <- file.info(cache_files)
  
  data.frame(
    file = basename(cache_files),
    size_mb = round(info$size / 1024 / 1024, 2),
    age_hours = round(as.numeric(difftime(Sys.time(), info$mtime, units = "hours")), 1),
    valid = as.numeric(difftime(Sys.time(), info$mtime, units = "hours")) < CACHE_MAX_AGE_HOURS
  )
}

# =============================================================================
# PRELOAD FUNCTION
# =============================================================================

#' Preload all soccer data into cache
#' @description Useful for app startup to ensure fast subsequent access
preload_soccer_data <- function() {
  log_debug("Preloading soccer data...", level = "INFO")
  
  start_time <- Sys.time()
  
  # Load all data types
  player_stats <- load_player_match_stats()
  shots <- load_shot_data()
  team_goals <- load_team_goals()
  
  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)
  
  log_debug(sprintf("Preload complete in %.1f seconds", duration), level = "INFO")
  log_debug(sprintf("  Player stats: %d rows", if(is.null(player_stats)) 0 else nrow(player_stats)), level = "INFO")
  log_debug(sprintf("  Shots: %d rows", if(is.null(shots)) 0 else nrow(shots)), level = "INFO")
  log_debug(sprintf("  Team goals: %d rows", if(is.null(team_goals)) 0 else nrow(team_goals)), level = "INFO")
  
  invisible(list(
    player_stats = player_stats,
    shots = shots,
    team_goals = team_goals
  ))
}

# =============================================================================
# DEBUG FUNCTION
# =============================================================================

#' Debug soccer data - print summary of what's available
#' @description Call this to diagnose issues with missing leagues
#' @param force_refresh If TRUE, bypass cache and load fresh from Google Sheets
debug_soccer_data <- function(force_refresh = FALSE) {
  message("\n========================================")
  message("SOCCER DATA DEBUG")
  message("========================================\n")
  
  # Check config
  message("1. CONFIGURATION:")
  message("   LEAGUE_DISPLAY_NAMES keys: ", paste(names(LEAGUE_DISPLAY_NAMES), collapse = ", "))
  message("   LEAGUE_DATA_NAMES keys: ", paste(names(LEAGUE_DATA_NAMES), collapse = ", "))
  message("")
  
  # Check cache status
  message("2. CACHE STATUS:")
  cache_status <- get_cache_status()
  if (nrow(cache_status) == 0) {
    message("   No cache files found")
  } else {
    for (i in seq_len(nrow(cache_status))) {
      message(sprintf("   %s: %.2f MB, %.1f hours old, valid=%s",
                      cache_status$file[i],
                      cache_status$size_mb[i],
                      cache_status$age_hours[i],
                      cache_status$valid[i]))
    }
  }
  message("")
  
  # Load and check shot data
  message("3. SHOT DATA:")
  shot_data <- load_shot_data(force_refresh = force_refresh)
  if (is.null(shot_data)) {
    message("   ERROR: shot_data is NULL")
  } else {
    message(sprintf("   Total rows: %d", nrow(shot_data)))
    message(sprintf("   Columns: %s", paste(names(shot_data), collapse = ", ")))
    
    if ("league" %in% names(shot_data)) {
      leagues <- unique(shot_data$league)
      message(sprintf("   Unique leagues in data: %s", paste(leagues, collapse = ", ")))
      
      # Count per league
      for (lg in leagues) {
        count <- sum(shot_data$league == lg, na.rm = TRUE)
        message(sprintf("     - %s: %d rows", lg, count))
      }
      
      # Check which match LEAGUE_DISPLAY_NAMES
      message("\n   League matching check:")
      for (lg in leagues) {
        matches <- lg %in% names(LEAGUE_DISPLAY_NAMES)
        display <- if (matches) LEAGUE_DISPLAY_NAMES[lg] else "NO MATCH"
        message(sprintf("     '%s' -> '%s'", lg, display))
      }
    } else {
      message("   WARNING: No 'league' column found!")
    }
  }
  message("")
  
  # Check available leagues function
  message("4. GET_AVAILABLE_LEAGUES OUTPUT:")
  available <- get_available_leagues(shot_data)
  message(sprintf("   Returns: %s", paste(available, collapse = ", ")))
  message("")
  
  message("========================================")
  message("END DEBUG")
  message("========================================\n")
  
  invisible(list(
    shot_data = shot_data,
    available_leagues = available
  ))
}