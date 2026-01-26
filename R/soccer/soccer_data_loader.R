# =============================================================================
# Soccer Data Loader
# 
# Loads data from Google Drive parquet files (fast) with fallback to Sheets
# Handles both FBref and Understat data formats
#
# Dependencies: soccer_config.R, arrow, googledrive, googlesheets4
# =============================================================================

# =============================================================================
# GOOGLE SHEETS INITIALIZATION
# =============================================================================

init_google_sheets <- function() {
  log_debug("Initializing Google Sheets access (public, no auth)...", level = "INFO")
  googlesheets4::gs4_deauth()
  log_debug("Google Sheets initialized for public access", level = "INFO")
}

# =============================================================================
# GOOGLE DRIVE LOADING (FAST - Parquet files)
# =============================================================================

has_googledrive <- function() {
  requireNamespace("googledrive", quietly = TRUE)
}

.drive_id_cache <- new.env(parent = emptyenv())

get_drive_file_id <- function(data_type) {
  cache_key <- paste0("drive_id_", data_type)
  if (exists(cache_key, envir = .drive_id_cache)) {
    return(get(cache_key, envir = .drive_id_cache))
  }
  
  filename <- SOCCER_DRIVE_FILES[[data_type]]
  if (is.null(filename)) {
    log_debug(sprintf("No Drive filename configured for: %s", data_type), level = "WARN")
    return(NULL)
  }
  
  tryCatch({
    googledrive::drive_deauth()
    files <- googledrive::drive_find(
      q = sprintf("'%s' in parents and name = '%s'", SOCCER_DRIVE_FOLDER_ID, filename),
      n_max = 1
    )
    
    if (nrow(files) == 0) {
      log_debug(sprintf("Drive file not found: %s", filename), level = "WARN")
      return(NULL)
    }
    
    file_id <- files$id[1]
    assign(cache_key, file_id, envir = .drive_id_cache)
    log_debug(sprintf("Found Drive file %s -> %s", filename, file_id), level = "DEBUG")
    return(file_id)
    
  }, error = function(e) {
    log_debug(sprintf("Drive lookup failed: %s", e$message), level = "WARN")
    return(NULL)
  })
}

load_from_google_drive <- function(data_type) {
  if (!USE_GOOGLE_DRIVE) return(NULL)
  
  if (!has_googledrive() || !has_arrow()) {
    log_debug("googledrive or arrow package not available", level = "WARN")
    return(NULL)
  }
  
  file_id <- get_drive_file_id(data_type)
  if (is.null(file_id)) return(NULL)
  
  log_debug(sprintf("Loading %s from Google Drive...", data_type), level = "INFO")
  
  tryCatch({
    googledrive::drive_deauth()
    temp_file <- tempfile(fileext = ".parquet")
    googledrive::drive_download(googledrive::as_id(file_id), path = temp_file, overwrite = TRUE)
    data <- arrow::read_parquet(temp_file)
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

has_arrow <- function() {
  requireNamespace("arrow", quietly = TRUE)
}

get_cache_path_v2 <- function(data_type) {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  ext <- if (USE_PARQUET_CACHE && has_arrow()) ".parquet" else ".rds"
  file.path(CACHE_DIR, paste0("soccer_", data_type, ext))
}

is_cache_valid_v2 <- function(cache_path) {
  if (!file.exists(cache_path)) return(FALSE)
  file_age_hours <- difftime(Sys.time(), file.mtime(cache_path), units = "hours")
  return(as.numeric(file_age_hours) < CACHE_MAX_AGE_HOURS)
}

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

load_from_cache_v2 <- function(data_type) {
  cache_path <- get_cache_path_v2(data_type)
  if (!file.exists(cache_path)) return(NULL)
  
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
# PLAYER MATCH STATS LOADING
# =============================================================================

load_player_match_stats <- function(force_refresh = FALSE) {
  log_debug("========================================", level = "INFO")
  log_debug("load_player_match_stats() called", level = "INFO")
  
  cache_path <- get_cache_path_v2("player_stats")
  
  # 1. Check cache first (fastest - instant)
  if (!force_refresh && is_cache_valid_v2(cache_path)) {
    log_debug("Using cached player stats data", level = "INFO")
    data <- load_from_cache_v2("player_stats")
    if (!is.null(data) && nrow(data) > 0) {
      log_debug(sprintf("Loaded %d rows from cache", nrow(data)), level = "INFO")
      if ("league" %in% names(data)) {
        log_debug(sprintf("Leagues: %s", paste(unique(data$league), collapse = ", ")), level = "INFO")
      }
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  # 2. Try Google Drive Parquet (fast - ~5-15 seconds)
  data <- load_from_google_drive("player_match_stats")
  if (!is.null(data) && nrow(data) > 0) {
    data <- normalize_team_columns(data, c("team", "squad", "home_team", "away_team", "h_team", "a_team"))
    
    if ("league" %in% names(data)) {
      log_debug(sprintf("Loaded from Drive: %d rows, leagues: %s", 
                        nrow(data), paste(unique(data$league), collapse = ", ")), level = "INFO")
    }
    
    save_to_cache_v2(data, "player_stats")
    log_debug("========================================", level = "INFO")
    return(data)
  }
  
  # 3. Fall back to Google Sheets (slow - ~3 minutes)
  log_debug("Loading player stats from Google Sheets (slow)...", level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    data <- googlesheets4::read_sheet(
      SOCCER_SHEET_IDS$player_match_stats,
      sheet = "Player_Match_Stats"
    ) %>% as.data.frame()
    
    data <- normalize_team_columns(data, c("team", "squad", "home_team", "away_team"))
    
    numeric_cols <- c("minutes", "goals", "assists", "shots", "xg", "sca", "gca", 
                      "touches", "progressive_passes", "progressive_carries",
                      "touches_att_3rd", "touches_att_pen_area", "progressive_passes_received",
                      "gameweek", "time")
    for (col in numeric_cols) {
      if (col %in% names(data)) data[[col]] <- as.numeric(data[[col]])
    }
    
    log_debug(sprintf("Loaded %d rows from Google Sheets", nrow(data)), level = "INFO")
    save_to_cache_v2(data, "player_stats")
    log_debug("========================================", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug(sprintf("Error loading player stats: %s", e$message), level = "ERROR")
    if (file.exists(cache_path)) {
      log_debug("Falling back to expired cache", level = "WARN")
      return(load_from_cache_v2("player_stats"))
    }
    return(NULL)
  })
}

# =============================================================================
# SHOT DATA LOADING - HANDLES BOTH FBREF AND UNDERSTAT FORMATS
# =============================================================================

load_shot_data <- function(force_refresh = FALSE) {
  log_debug("========================================", level = "INFO")
  log_debug("load_shot_data() called", level = "INFO")
  
  cache_path <- get_cache_path_v2("shots")
  
  # 1. Check cache first (fastest)
  if (!force_refresh && is_cache_valid_v2(cache_path)) {
    log_debug("Using cached shot data", level = "INFO")
    data <- load_from_cache_v2("shots")
    if (!is.null(data) && nrow(data) > 0) {
      log_debug(sprintf("Loaded %d rows from cache", nrow(data)), level = "INFO")
      if ("league" %in% names(data)) {
        log_debug(sprintf("Leagues: %s", paste(unique(data$league), collapse = ", ")), level = "INFO")
      }
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  # 2. Try Google Drive Parquet (fast)
  data <- load_from_google_drive("shots")
  if (!is.null(data) && nrow(data) > 0) {
    data <- normalize_shot_data(data)
    
    if ("league" %in% names(data)) {
      log_debug(sprintf("Loaded from Drive: %d rows, leagues: %s", 
                        nrow(data), paste(unique(data$league), collapse = ", ")), level = "INFO")
    }
    
    save_to_cache_v2(data, "shots")
    log_debug("========================================", level = "INFO")
    return(data)
  }
  
  # 3. Fall back to Google Sheets (slow)
  log_debug("Loading shot data from Google Sheets (slow)...", level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    data <- googlesheets4::read_sheet(SOCCER_SHEET_IDS$shots, sheet = "Shot_Data") %>%
      as.data.frame()
    
    data <- normalize_shot_data(data)
    
    log_debug(sprintf("Loaded %d rows from Google Sheets", nrow(data)), level = "INFO")
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

#' Normalize shot data to handle both FBref and Understat formats
normalize_shot_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  # Detect format based on columns present
  is_understat <- "h_a" %in% names(data) || "h_team" %in% names(data)
  is_fbref <- "squad" %in% names(data) || "home_team" %in% names(data)
  
  if (is_understat) {
    log_debug("Detected Understat format, normalizing...", level = "DEBUG")
    data <- normalize_team_columns(data, c("h_team", "a_team"))
    
    # Handle xG column variations
    if ("x_g" %in% names(data) && !"xG" %in% names(data)) {
      data$xG <- as.numeric(data$x_g)
    } else if ("xg" %in% names(data) && !"xG" %in% names(data)) {
      data$xG <- as.numeric(data$xg)
    }
    if ("xG" %in% names(data)) data$xG <- as.numeric(data$xG)
    
    for (col in c("minute", "X", "Y")) {
      if (col %in% names(data)) data[[col]] <- as.numeric(data[[col]])
    }
    
    if (!"match_id" %in% names(data) && "id" %in% names(data)) {
      data$match_id <- data$id
    }
    
  } else if (is_fbref) {
    log_debug("Detected FBref format, normalizing...", level = "DEBUG")
    data <- normalize_team_columns(data, c("squad", "home_team", "away_team"))
    for (col in c("xg_shot", "minute", "gameweek")) {
      if (col %in% names(data)) data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  data
}

# =============================================================================
# TEAM GOALS LOADING
# =============================================================================

load_team_goals <- function(force_refresh = FALSE) {
  log_debug("========================================", level = "INFO")
  log_debug("load_team_goals() called", level = "INFO")
  
  cache_path <- get_cache_path_v2("team_goals")
  
  if (!force_refresh && is_cache_valid_v2(cache_path)) {
    log_debug("Using cached team goals data", level = "INFO")
    data <- load_from_cache_v2("team_goals")
    if (!is.null(data)) {
      log_debug(sprintf("Loaded %d rows from cache", nrow(data)), level = "INFO")
      log_debug("========================================", level = "INFO")
      return(data)
    }
  }
  
  data <- load_from_google_drive("team_goals")
  if (!is.null(data)) {
    data <- normalize_team_columns(data, c("home_team", "away_team"))
    save_to_cache_v2(data, "team_goals")
    log_debug("========================================", level = "INFO")
    return(data)
  }
  
  log_debug("Loading team goals from Google Sheets...", level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    data <- googlesheets4::read_sheet(SOCCER_SHEET_IDS$team_goals, sheet = "Team_Goals") %>%
      as.data.frame()
    
    data <- normalize_team_columns(data, c("home_team", "away_team"))
    
    numeric_cols <- c("home_goals", "away_goals", "gameweek")
    for (col in numeric_cols) {
      if (col %in% names(data)) {
        if (is.list(data[[col]])) {
          data[[col]] <- sapply(data[[col]], function(x) if(length(x) == 0) NA else as.numeric(x[[1]]))
        } else {
          data[[col]] <- as.numeric(data[[col]])
        }
      }
    }
    
    log_debug(sprintf("Loaded %d rows from Google Sheets", nrow(data)), level = "INFO")
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
# BACKWARD COMPATIBILITY
# =============================================================================

load_shooting_summary <- function(force_refresh = FALSE) {
  log_debug("load_shooting_summary() is deprecated - using load_player_match_stats()", level = "WARN")
  return(load_player_match_stats(force_refresh))
}

load_possession_data <- function(force_refresh = FALSE) {
  log_debug("load_possession_data() is deprecated - using load_player_match_stats()", level = "WARN")
  return(load_player_match_stats(force_refresh))
}

# =============================================================================
# QUERY FUNCTIONS
# =============================================================================

get_available_leagues <- function(data) {
  if (is.null(data) || !"league" %in% names(data)) return(character(0))
  
  leagues <- unique(data$league)
  leagues <- leagues[!is.na(leagues) & leagues != ""]
  
  display_names <- LEAGUE_DISPLAY_NAMES[leagues]
  display_names <- display_names[!is.na(display_names)]
  
  return(as.character(display_names))
}

get_league_teams <- function(data, league) {
  if (is.null(data) || is.null(league)) return(character(0))
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  team_col <- if ("team" %in% names(data)) "team" else if ("squad" %in% names(data)) "squad" else NULL
  if (is.null(team_col)) return(character(0))
  
  teams <- data %>%
    filter(league == league_data_name) %>%
    pull(!!sym(team_col)) %>%
    unique()
  
  teams <- normalize_team_names(teams)
  teams <- teams[!is.na(teams) & teams != ""]
  sort(unique(teams))
}

get_team_players <- function(data, team) {
  if (is.null(data) || is.null(team)) return(character(0))
  
  team_col <- if ("team" %in% names(data)) "team" else if ("squad" %in% names(data)) "squad" else NULL
  if (is.null(team_col)) return(character(0))
  
  players <- data %>%
    filter(!!sym(team_col) == team) %>%
    pull(player) %>%
    unique() %>%
    sort()
  
  players[!is.na(players) & players != ""]
}

# =============================================================================
# CACHE MANAGEMENT
# =============================================================================

clear_soccer_cache <- function() {
  log_debug("Clearing soccer data cache...", level = "INFO")
  cache_files <- list.files(CACHE_DIR, pattern = "^soccer_.*\\.(parquet|rds)$", full.names = TRUE)
  if (length(cache_files) > 0) {
    file.remove(cache_files)
    log_debug(sprintf("Removed %d cache files", length(cache_files)), level = "INFO")
  }
}

get_cache_status <- function() {
  cache_files <- list.files(CACHE_DIR, pattern = "^soccer_.*\\.(parquet|rds)$", full.names = TRUE)
  if (length(cache_files) == 0) {
    return(data.frame(file = character(0), size_mb = numeric(0), age_hours = numeric(0), valid = logical(0)))
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
# PRELOAD & DEBUG
# =============================================================================

preload_soccer_data <- function() {
  log_debug("Preloading soccer data...", level = "INFO")
  start_time <- Sys.time()
  player_stats <- load_player_match_stats()
  shots <- load_shot_data()
  team_goals <- load_team_goals()
  duration <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
  log_debug(sprintf("Preload complete in %.1f seconds", duration), level = "INFO")
  invisible(list(player_stats = player_stats, shots = shots, team_goals = team_goals))
}

debug_soccer_data <- function(force_refresh = FALSE) {
  message("\n=== SOCCER DATA DEBUG ===")
  message("Leagues configured: ", paste(names(LEAGUE_DISPLAY_NAMES), collapse = ", "))
  
  shots <- load_shot_data(force_refresh)
  if (!is.null(shots) && "league" %in% names(shots)) {
    message("Shot leagues: ", paste(unique(shots$league), collapse = ", "))
  }
  
  pms <- load_player_match_stats(force_refresh)
  if (!is.null(pms) && "league" %in% names(pms)) {
    message("PMS leagues: ", paste(unique(pms$league), collapse = ", "))
  }
  
  invisible(list(shots = shots, pms = pms))
}