# =============================================================================
# Soccer Shot Share Analysis
# 
# Functions for calculating shot and xG share between players when on field
# together. Uses understat data from Google Drive parquet files.
#
# Dependencies: soccer_config.R, helpers.R, arrow, googledrive
# =============================================================================

# =============================================================================
# UNDERSTAT DATA LOADING - USES DRIVE PARQUET
# =============================================================================

UNDERSTAT_CACHE_DIR <- "data/cache"

#' Load UNDERSTAT shots data for shot share analysis
#' Priority: Local cache -> Google Drive parquet -> Google Sheets
load_shots_for_share <- function(force_refresh = FALSE) {
  log_debug("========================================", level = "INFO")
  log_debug("load_shots_for_share() - Loading UNDERSTAT shots", level = "INFO")
  
  cache_path <- file.path(UNDERSTAT_CACHE_DIR, "understat_shots.parquet")
  
  # 1. Check local cache first
  if (!force_refresh && file.exists(cache_path)) {
    cache_age <- difftime(Sys.time(), file.mtime(cache_path), units = "hours")
    if (cache_age < 24) {
      tryCatch({
        data <- arrow::read_parquet(cache_path)
        log_debug(sprintf("Loaded %d rows from understat cache: %s", nrow(data), basename(cache_path)), level = "INFO")
        if ("league" %in% names(data)) {
          log_debug(sprintf("Using cached understat shots: %d rows, leagues: %s", 
                            nrow(data), paste(unique(data$league), collapse = ", ")), level = "INFO")
        }
        return(as.data.frame(data))
      }, error = function(e) {
        log_debug(sprintf("Cache read failed: %s", e$message), level = "WARN")
      })
    }
  }
  
  # 2. Try Google Drive parquet
  data <- tryCatch({
    googledrive::drive_deauth()
    files <- googledrive::drive_find(
      q = sprintf("'%s' in parents and name = 'shots.parquet'", SOCCER_DRIVE_FOLDER_ID),
      n_max = 1
    )
    
    if (nrow(files) > 0) {
      log_debug("Loading understat shots from Google Drive...", level = "INFO")
      temp_file <- tempfile(fileext = ".parquet")
      googledrive::drive_download(googledrive::as_id(files$id[1]), path = temp_file, overwrite = TRUE)
      df <- arrow::read_parquet(temp_file)
      unlink(temp_file)
      log_debug(sprintf("Loaded %d rows from Drive", nrow(df)), level = "INFO")
      as.data.frame(df)
    } else {
      NULL
    }
  }, error = function(e) {
    log_debug(sprintf("Drive load failed: %s", e$message), level = "WARN")
    NULL
  })
  
  # 3. Fallback to Google Sheets
  if (is.null(data) || nrow(data) == 0) {
    log_debug("Loading understat shots from Google Sheets (slow)...", level = "INFO")
    tryCatch({
      googlesheets4::gs4_deauth()
      data <- googlesheets4::read_sheet("1QHXvdEOOJv2Bjiecvk7ZkOWp-THk4Pzy4IgyttxuPHo", sheet = "Sheet1") %>%
        janitor::clean_names() %>%
        as.data.frame()
    }, error = function(e) {
      log_debug(sprintf("Sheets load failed: %s", e$message), level = "ERROR")
      return(NULL)
    })
  }
  
  if (!is.null(data) && nrow(data) > 0) {
    if ("x_g" %in% names(data) && !"xG" %in% names(data)) data$xG <- as.numeric(data$x_g)
    if ("xG" %in% names(data)) data$xG <- as.numeric(data$xG)
    if ("minute" %in% names(data)) data$minute <- as.numeric(data$minute)
    
    if (!dir.exists(UNDERSTAT_CACHE_DIR)) dir.create(UNDERSTAT_CACHE_DIR, recursive = TRUE)
    tryCatch({
      arrow::write_parquet(data, cache_path)
      log_debug(sprintf("Cached %d rows to %s", nrow(data), basename(cache_path)), level = "INFO")
    }, error = function(e) log_debug(sprintf("Cache write failed: %s", e$message), level = "WARN"))
  }
  
  data
}

#' Load UNDERSTAT player match stats for shot share analysis
#' Priority: Local cache -> Google Drive parquet -> Google Sheets
load_pms_for_share <- function(force_refresh = FALSE) {
  log_debug("========================================", level = "INFO")
  log_debug("load_pms_for_share() - Loading UNDERSTAT PMS", level = "INFO")
  
  cache_path <- file.path(UNDERSTAT_CACHE_DIR, "understat_pms.parquet")
  
  # 1. Check local cache first
  if (!force_refresh && file.exists(cache_path)) {
    cache_age <- difftime(Sys.time(), file.mtime(cache_path), units = "hours")
    if (cache_age < 24) {
      tryCatch({
        data <- arrow::read_parquet(cache_path)
        log_debug(sprintf("Loaded %d rows from understat PMS cache", nrow(data)), level = "INFO")
        return(as.data.frame(data))
      }, error = function(e) {
        log_debug(sprintf("Cache read failed: %s", e$message), level = "WARN")
      })
    }
  }
  
  # 2. Try Google Drive parquet
  data <- tryCatch({
    googledrive::drive_deauth()
    files <- googledrive::drive_find(
      q = sprintf("'%s' in parents and name = 'player_match_stats.parquet'", SOCCER_DRIVE_FOLDER_ID),
      n_max = 1
    )
    
    if (nrow(files) > 0) {
      log_debug("Loading understat PMS from Google Drive...", level = "INFO")
      temp_file <- tempfile(fileext = ".parquet")
      googledrive::drive_download(googledrive::as_id(files$id[1]), path = temp_file, overwrite = TRUE)
      df <- arrow::read_parquet(temp_file)
      unlink(temp_file)
      log_debug(sprintf("Loaded %d rows from Drive", nrow(df)), level = "INFO")
      as.data.frame(df)
    } else {
      NULL
    }
  }, error = function(e) {
    log_debug(sprintf("Drive load failed: %s", e$message), level = "WARN")
    NULL
  })
  
  # 3. Fallback to Google Sheets
  if (is.null(data) || nrow(data) == 0) {
    log_debug("Loading understat PMS from Google Sheets (slow)...", level = "INFO")
    tryCatch({
      googlesheets4::gs4_deauth()
      data <- googlesheets4::read_sheet("1IQFKATmpgLiK7aVbkfgATi_uJBKjt4xSRX065LukHYA", sheet = "Sheet1") %>%
        janitor::clean_names() %>%
        as.data.frame()
    }, error = function(e) {
      log_debug(sprintf("Sheets load failed: %s", e$message), level = "ERROR")
      return(NULL)
    })
  }
  
  if (!is.null(data) && nrow(data) > 0) {
    if (!"match_id" %in% names(data) && "id" %in% names(data)) data$match_id <- data$id
    if ("time" %in% names(data)) data$time <- as.numeric(data$time)
    
    if (!dir.exists(UNDERSTAT_CACHE_DIR)) dir.create(UNDERSTAT_CACHE_DIR, recursive = TRUE)
    tryCatch({
      arrow::write_parquet(data, cache_path)
      log_debug(sprintf("Cached %d rows to %s", nrow(data), basename(cache_path)), level = "INFO")
    }, error = function(e) log_debug(sprintf("Cache write failed: %s", e$message), level = "WARN"))
  }
  
  data
}

# =============================================================================
# LEAGUE DISPLAY HELPERS
# =============================================================================

#' Convert raw league name (EPL) to display name (Premier League)
get_understat_league_display <- function(league_raw) {
  mapping <- c("EPL" = "Premier League", "La_Liga" = "La Liga", "La Liga" = "La Liga",
               "Bundesliga" = "Bundesliga", "Serie_A" = "Serie A", "Serie A" = "Serie A",
               "Ligue_1" = "Ligue 1", "Ligue 1" = "Ligue 1")
  if (league_raw %in% names(mapping)) return(mapping[[league_raw]])
  league_raw
}

#' Convert display name (Premier League) to raw league name (EPL)
#' CRITICAL: Use this before filtering data!
get_understat_league_raw <- function(league_display) {
  mapping <- c(
    "Premier League" = "EPL",
    "La Liga" = "La Liga",
    "Bundesliga" = "Bundesliga",
    "Serie A" = "Serie A",
    "Ligue 1" = "Ligue 1"
  )
  if (league_display %in% names(mapping)) return(mapping[[league_display]])
  league_display
}

get_understat_league_logo <- function(league_name) {
  if (exists("LEAGUE_LOGO_PATHS") && league_name %in% names(LEAGUE_LOGO_PATHS)) {
    return(LEAGUE_LOGO_PATHS[[league_name]])
  }
  logo_paths <- c("Premier League" = "soccer_logos/League Logos/Premier_League.png",
                  "La Liga" = "soccer_logos/League Logos/LaLiga.jpeg",
                  "Bundesliga" = "soccer_logos/League Logos/Bundesliga.png",
                  "Serie A" = "soccer_logos/League Logos/Serie_A.jpeg",
                  "Ligue 1" = "soccer_logos/League Logos/Ligue_1.png")
  if (league_name %in% names(logo_paths)) return(logo_paths[[league_name]])
  ""
}

# =============================================================================
# SHOT SHARE CALCULATIONS
# =============================================================================

#' Get teams for a league from shots data
#' @param shots Shot data
#' @param league_display League DISPLAY name (e.g. "Premier League")
get_shot_share_teams <- function(shots, league_display) {
  if (is.null(shots) || nrow(shots) == 0) return(character(0))
  
  # CRITICAL: Convert display name to raw format for filtering
  raw_league <- get_understat_league_raw(league_display)
  log_debug(sprintf("get_shot_share_teams: display='%s' -> raw='%s'", league_display, raw_league), level = "DEBUG")
  
  # Use .data$ to avoid variable shadowing with column name
  league_shots <- shots %>% filter(.data$league == raw_league)
  log_debug(sprintf("Filtered to %d shots for league %s", nrow(league_shots), raw_league), level = "DEBUG")
  
  if (nrow(league_shots) == 0) return(character(0))
  
  teams <- unique(c(league_shots$h_team, league_shots$a_team))
  teams <- teams[!is.na(teams) & teams != ""]
  sort(teams)
}

#' Get players for a team from shots data
#' @param shots Shot data
#' @param league_display League DISPLAY name (e.g. "Premier League")
#' @param team Team name
get_shot_share_players <- function(shots, league_display, team) {
  if (is.null(shots) || nrow(shots) == 0) return(character(0))
  
  # CRITICAL: Convert display name to raw format for filtering
  raw_league <- get_understat_league_raw(league_display)
  
  # Filter to shots BY the team (h_a indicates if shooter was home or away)
  # h_a = "h" means home team player took the shot
  # h_a = "a" means away team player took the shot
  team_shots <- shots %>%
    filter(.data$league == raw_league) %>%
    filter(
      (grepl(team, .data$h_team, ignore.case = TRUE) & .data$h_a == "h") |
        (grepl(team, .data$a_team, ignore.case = TRUE) & .data$h_a == "a")
    )
  
  if (nrow(team_shots) == 0) return(character(0))
  
  players <- unique(team_shots$player)
  players <- players[!is.na(players) & players != ""]
  sort(players)
}

#' Calculate pair analysis for two players
#' @param shots Shot data
#' @param pms Player match stats
#' @param league_display League DISPLAY name (e.g. "Premier League")
#' @param team Team name
#' @param player1 First player
#' @param player2 Second player
calculate_pair_analysis <- function(shots, pms, league_display, team, player1, player2) {
  if (is.null(shots) || is.null(pms)) return(NULL)
  
  # CRITICAL: Convert display name to raw format for filtering
  raw_league <- get_understat_league_raw(league_display)
  
  # Use .data$ to avoid variable shadowing
  p1_matches <- pms %>% filter(.data$league == raw_league, .data$player == player1) %>% pull(match_id)
  p2_matches <- pms %>% filter(.data$league == raw_league, .data$player == player2) %>% pull(match_id)
  both_matches <- intersect(p1_matches, p2_matches)
  
  if (length(both_matches) == 0) {
    return(list(matches_together = 0, minutes_together = 0, player1_stats = NULL, player2_stats = NULL))
  }
  
  team_shots <- shots %>%
    filter(.data$league == raw_league, .data$match_id %in% both_matches) %>%
    filter(grepl(team, .data$h_team, ignore.case = TRUE) | grepl(team, .data$a_team, ignore.case = TRUE))
  
  calc_player_stats <- function(player_name) {
    player_shots <- team_shots %>% filter(.data$player == player_name)
    list(
      player = player_name,
      shots = nrow(player_shots),
      goals = sum(player_shots$result == "Goal", na.rm = TRUE),
      xG = round(sum(player_shots$xG, na.rm = TRUE), 2),
      shot_share = if (nrow(team_shots) > 0) round(100 * nrow(player_shots) / nrow(team_shots), 1) else 0,
      xG_share = if (sum(team_shots$xG, na.rm = TRUE) > 0) 
        round(100 * sum(player_shots$xG, na.rm = TRUE) / sum(team_shots$xG, na.rm = TRUE), 1) else 0
    )
  }
  
  minutes <- sapply(both_matches, function(mid) {
    p1_time <- pms %>% filter(.data$match_id == mid, .data$player == player1) %>% pull(time)
    p2_time <- pms %>% filter(.data$match_id == mid, .data$player == player2) %>% pull(time)
    if (length(p1_time) > 0 && length(p2_time) > 0) min(p1_time[1], p2_time[1], na.rm = TRUE) else 0
  })
  
  list(
    matches_together = length(both_matches),
    minutes_together = sum(minutes, na.rm = TRUE),
    team_shots = nrow(team_shots),
    team_xG = round(sum(team_shots$xG, na.rm = TRUE), 2),
    player1_stats = calc_player_stats(player1),
    player2_stats = calc_player_stats(player2)
  )
}

message("Soccer Shot Share loaded: load_shots_for_share(), load_pms_for_share(), calculate_pair_analysis()")

# =============================================================================
# HELPER FUNCTIONS FOR UI MODULE
# =============================================================================

#' Get available leagues from shots data
get_shot_share_leagues <- function(shots) {
  if (is.null(shots) || nrow(shots) == 0) return(character(0))
  
  raw_leagues <- unique(shots$league)
  raw_leagues <- raw_leagues[!is.na(raw_leagues) & raw_leagues != ""]
  
  # Convert to display names
  display_leagues <- sapply(raw_leagues, get_understat_league_display)
  sort(unique(display_leagues))
}

#' Enrich player match stats with team info
enrich_player_match_stats <- function(pms, shots) {
  if (is.null(pms) || nrow(pms) == 0) return(pms)
  
  # Add player's team based on h_a flag if not present
  if (!"player_team" %in% names(pms) && "h_a" %in% names(pms)) {
    pms <- pms %>%
      mutate(player_team = ifelse(.data$h_a == "h", .data$h_team, .data$a_team))
  }
  
  pms
}

#' Enrich shots with teammate information
enrich_shots_with_teammates <- function(shots, pms) {
  if (is.null(shots) || nrow(shots) == 0) return(shots)
  if (is.null(pms) || nrow(pms) == 0) return(shots)
  
  # For each shot, find teammates who were on field
  shots$teammates <- lapply(1:nrow(shots), function(i) {
    shot <- shots[i, ]
    
    # Find players in same match, same team (h_a)
    match_players <- pms %>%
      filter(.data$match_id == shot$match_id, .data$h_a == shot$h_a)
    
    # Return teammate names (excluding shooter)
    teammates <- match_players$player[match_players$player != shot$player]
    paste(teammates, collapse = ", ")
  })
  
  shots$teammates <- unlist(shots$teammates)
  shots
}

#' Calculate minutes stats for selected players
#' @param enriched_pms Player match stats
#' @param players Vector of player names to analyze
#' @param team Team name
#' @return List with minutes_together, matches_together, pct_together, avg_minutes_per_match
calculate_minutes_together <- function(enriched_pms, players, team) {
  if (is.null(enriched_pms) || length(players) == 0) {
    return(list(minutes_together = 0, matches_together = 0, pct_together = 0, avg_minutes_per_match = 0))
  }
  
  # Filter to team
  team_pms <- enriched_pms %>%
    filter(grepl(team, .data$h_team, ignore.case = TRUE) | grepl(team, .data$a_team, ignore.case = TRUE))
  
  if (nrow(team_pms) == 0) {
    return(list(minutes_together = 0, matches_together = 0, pct_together = 0, avg_minutes_per_match = 0))
  }
  
  # Find matches where ALL selected players appeared
  player_matches <- lapply(players, function(p) {
    team_pms %>% filter(.data$player == p) %>% pull(match_id) %>% unique()
  })
  
  # Intersection of all players' matches
  common_matches <- Reduce(intersect, player_matches)
  
  if (length(common_matches) == 0) {
    return(list(minutes_together = 0, matches_together = 0, pct_together = 0, avg_minutes_per_match = 0))
  }
  
  # For each common match, calculate overlap minutes
  minutes_per_match <- sapply(common_matches, function(mid) {
    player_times <- sapply(players, function(p) {
      pdata <- team_pms %>% filter(.data$match_id == mid, .data$player == p)
      if (nrow(pdata) > 0 && "time" %in% names(pdata)) {
        as.numeric(pdata$time[1])
      } else {
        0
      }
    })
    # Minutes together = minimum of all players' minutes (conservative estimate)
    min(player_times, na.rm = TRUE)
  })
  
  minutes_together <- sum(minutes_per_match, na.rm = TRUE)
  matches_together <- length(common_matches)
  
  # CORRECT CALCULATION: % of maximum possible minutes (matches * 90)
  max_possible_minutes <- matches_together * 90
  pct_together <- if (max_possible_minutes > 0) {
    (minutes_together / max_possible_minutes) * 100
  } else {
    0
  }
  
  avg_minutes <- if (matches_together > 0) minutes_together / matches_together else 0
  
  list(
    minutes_together = minutes_together,
    matches_together = matches_together,
    pct_together = pct_together,
    avg_minutes_per_match = avg_minutes
  )
}

#' Calculate shot/xG share for selected players
#' @param shots Enriched shot data (filtered to team)
#' @param players Vector of player names
#' @param enriched_pms Player match stats
#' @param include_baseline If TRUE, also calculate baseline (overall) share
#' @return Data frame with share stats per player
calculate_player_share <- function(shots, players, enriched_pms, include_baseline = TRUE) {
  if (is.null(shots) || nrow(shots) == 0 || length(players) == 0) {
    return(NULL)
  }
  
  # Find matches where all players played
  player_matches <- lapply(players, function(p) {
    enriched_pms %>% filter(.data$player == p) %>% pull(match_id) %>% unique()
  })
  common_matches <- Reduce(intersect, player_matches)
  
  # Filter shots to common matches
  together_shots <- shots %>% filter(.data$match_id %in% common_matches)
  
  if (nrow(together_shots) == 0) {
    return(NULL)
  }
  
  # Calculate stats for each player
  results <- lapply(players, function(p) {
    player_shots <- together_shots %>% filter(.data$player == p)
    
    data.frame(
      player = p,
      shots = nrow(player_shots),
      xG = sum(player_shots$xG, na.rm = TRUE),
      goals = sum(player_shots$result == "Goal", na.rm = TRUE),
      shot_share_team = nrow(player_shots) / nrow(together_shots),
      xg_share_team = sum(player_shots$xG, na.rm = TRUE) / sum(together_shots$xG, na.rm = TRUE),
      team_shots = nrow(together_shots),
      team_xg = sum(together_shots$xG, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  results_df <- bind_rows(results)
  
  # Add baseline (overall share) if requested
  if (include_baseline && nrow(shots) > 0) {
    results_df <- results_df %>%
      rowwise() %>%
      mutate(
        baseline_shot_share = {
          player_total <- shots %>% filter(.data$player == player) %>% nrow()
          player_total / nrow(shots)
        },
        baseline_xg_share = {
          player_xg <- shots %>% filter(.data$player == player) %>% pull(xG) %>% sum(na.rm = TRUE)
          player_xg / sum(shots$xG, na.rm = TRUE)
        }
      ) %>%
      ungroup() %>%
      mutate(
        shot_share_diff = shot_share_team - baseline_shot_share,
        xg_share_diff = xg_share_team - baseline_xg_share
      )
  }
  
  results_df
}

#' Get players for a team (generic version using PMS)
get_team_players <- function(pms, team) {
  if (is.null(pms) || nrow(pms) == 0) return(character(0))
  
  players <- pms %>%
    filter(grepl(team, .data$h_team, ignore.case = TRUE) | grepl(team, .data$a_team, ignore.case = TRUE)) %>%
    pull(player) %>%
    unique()
  
  players <- players[!is.na(players) & players != ""]
  sort(players)
}