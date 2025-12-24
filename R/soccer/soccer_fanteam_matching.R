# =============================================================================
# Soccer FanTeam Player Matching
# 
# Reconciles FanTeam player names with FBref data using Google Sheets
# for corrections management
# =============================================================================

# Google Sheet ID for player mapping
FANTEAM_MAPPING_SHEET_ID <- "11ETfAvmX2uGwXv49YuwznLgV1ff9MvOubhOvCk08lpA"

# Cache for corrections (avoid repeated API calls)
CORRECTIONS_CACHE <- new.env()

#' Normalize a player name for matching
#' @param name Player name
#' @return Normalized name (lowercase, no accents, standardized spacing)
normalize_player_name <- function(name) {
  if (is.null(name) || is.na(name) || name == "") return("")
  
  name <- tolower(name)
  
  # Remove accents/diacritics
  name <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
  
  # Remove any remaining non-alphanumeric (except spaces and hyphens)
  name <- gsub("[^a-z0-9 -]", "", name)
  
  # Standardize spacing
  name <- gsub("\\s+", " ", trimws(name))
  
  return(name)
}

#' Load corrections from Google Sheet
#' @param force_refresh Force reload from Google Sheets (ignore cache)
#' @return Data frame with corrections
load_corrections <- function(force_refresh = FALSE) {
  cache_key <- "corrections"
  cache_time_key <- "corrections_time"
  
  # Check cache (valid for 5 minutes)
  if (!force_refresh && 
      exists(cache_key, envir = CORRECTIONS_CACHE) &&
      exists(cache_time_key, envir = CORRECTIONS_CACHE)) {
    cache_age <- difftime(Sys.time(), get(cache_time_key, envir = CORRECTIONS_CACHE), units = "mins")
    if (cache_age < 5) {
      log_debug("Using cached corrections", level = "DEBUG")
      return(get(cache_key, envir = CORRECTIONS_CACHE))
    }
  }
  
  log_debug("Loading corrections from Google Sheet...", level = "INFO")
  
  corrections <- tryCatch({
    read_sheet(
      FANTEAM_MAPPING_SHEET_ID,
      sheet = "corrections",
      col_types = "c"
    ) %>%
      filter(!is.na(fanteam_name) & fanteam_name != "")
  }, error = function(e) {
    log_debug("Error loading corrections:", e$message, level = "WARN")
    data.frame(
      fanteam_name = character(),
      fanteam_club = character(),
      fbref_name = character(),
      fbref_team = character(),
      stringsAsFactors = FALSE
    )
  })
  
  # Cache the result
  assign(cache_key, corrections, envir = CORRECTIONS_CACHE)
  assign(cache_time_key, Sys.time(), envir = CORRECTIONS_CACHE)
  
  log_debug("Loaded", nrow(corrections), "corrections", level = "INFO")
  return(corrections)
}

#' Load unmatched players from Google Sheet
#' @return Data frame with unmatched players
load_unmatched <- function() {
  log_debug("Loading unmatched players from Google Sheet...", level = "DEBUG")
  
  tryCatch({
    read_sheet(
      FANTEAM_MAPPING_SHEET_ID,
      sheet = "unmatched",
      col_types = "c"
    )
  }, error = function(e) {
    log_debug("Error loading unmatched:", e$message, level = "WARN")
    data.frame(
      fanteam_name = character(),
      fanteam_club = character(),
      position = character(),
      salary = character(),
      gameweek = character(),
      date_added = character(),
      resolved = character(),
      stringsAsFactors = FALSE
    )
  })
}

#' Write unmatched players to Google Sheet
#' @param unmatched_df Data frame with unmatched players to add
#' @param gameweek Current gameweek
append_unmatched <- function(unmatched_df, gameweek) {
  if (is.null(unmatched_df) || nrow(unmatched_df) == 0) return(invisible(NULL))
  
  log_debug("Appending", nrow(unmatched_df), "unmatched players to Google Sheet...", level = "INFO")
  
  # Load existing unmatched to avoid duplicates
  existing <- load_unmatched()
  
  # Filter out players already in unmatched (same name + club + gameweek)
  if (nrow(existing) > 0) {
    existing_keys <- paste(existing$fanteam_name, existing$fanteam_club, existing$gameweek, sep = "|||")
    new_keys <- paste(unmatched_df$fanteam_name, unmatched_df$fanteam_club, gameweek, sep = "|||")
    unmatched_df <- unmatched_df[!new_keys %in% existing_keys, ]
  }
  
  if (nrow(unmatched_df) == 0) {
    log_debug("No new unmatched players to add", level = "DEBUG")
    return(invisible(NULL))
  }
  
  # Prepare data for append
  to_append <- unmatched_df %>%
    mutate(
      gameweek = as.character(gameweek),
      date_added = as.character(Sys.Date()),
      resolved = "FALSE"
    ) %>%
    select(fanteam_name, fanteam_club, position, salary, gameweek, date_added, resolved)
  
  tryCatch({
    sheet_append(
      FANTEAM_MAPPING_SHEET_ID,
      to_append,
      sheet = "unmatched"
    )
    log_debug("Successfully appended", nrow(to_append), "unmatched players", level = "INFO")
  }, error = function(e) {
    log_debug("Error appending unmatched:", e$message, level = "ERROR")
  })
  
  invisible(NULL)
}

#' Match FanTeam players to FBref data
#' @param fanteam_data FanTeam salary data (from load_fanteam_soccer_salaries)
#' @param fbref_players FBref player data (unique players from shot/player data)
#' @param gameweek Current gameweek (for logging unmatched)
#' @param write_unmatched Whether to write unmatched to Google Sheet
#' @return FanTeam data with fbref_name and match_status columns added
match_fanteam_to_fbref <- function(fanteam_data, fbref_players, gameweek = NULL, write_unmatched = TRUE) {
  log_debug("========================================", level = "INFO")
  log_debug("match_fanteam_to_fbref() called", level = "INFO")
  log_debug("  FanTeam players:", nrow(fanteam_data), level = "INFO")
  log_debug("  FBref players:", nrow(fbref_players), level = "INFO")
  
  if (is.null(fanteam_data) || nrow(fanteam_data) == 0) {
    return(fanteam_data)
  }
  
  # Load corrections
  corrections <- load_corrections()
  
  # Normalize FBref player names for matching
  fbref_players <- fbref_players %>%
    mutate(
      fbref_name_normalized = normalize_player_name(player),
      fbref_team_normalized = normalize_team_names(team)
    )
  
  # Process each FanTeam player
  fanteam_data <- fanteam_data %>%
    mutate(
      fanteam_name_normalized = normalize_player_name(player),
      fanteam_club = if ("club_abbrev" %in% names(.)) club_abbrev else team
    )
  
  # Initialize result columns
  fanteam_data$fbref_name <- NA_character_
  fanteam_data$fbref_team <- NA_character_
  fanteam_data$match_status <- "unmatched"
  
  for (i in 1:nrow(fanteam_data)) {
    ft_name <- fanteam_data$player[i]
    ft_club <- fanteam_data$fanteam_club[i]
    ft_name_norm <- fanteam_data$fanteam_name_normalized[i]
    ft_team <- fanteam_data$team_normalized[i]
    
    # Step 1: Check corrections table
    correction <- corrections %>%
      filter(
        tolower(fanteam_name) == tolower(ft_name) |
          normalize_player_name(fanteam_name) == ft_name_norm
      )
    
    if (nrow(correction) > 0) {
      # Also check club if provided in corrections
      if (!is.na(correction$fanteam_club[1]) && correction$fanteam_club[1] != "") {
        correction <- correction %>% filter(toupper(fanteam_club) == toupper(ft_club))
      }
      
      if (nrow(correction) > 0) {
        fanteam_data$fbref_name[i] <- correction$fbref_name[1]
        fanteam_data$fbref_team[i] <- correction$fbref_team[1]
        fanteam_data$match_status[i] <- "corrected"
        next
      }
    }
    
    # Step 2: Try exact match on normalized name + team
    exact_match <- fbref_players %>%
      filter(
        fbref_name_normalized == ft_name_norm,
        fbref_team_normalized == ft_team
      )
    
    if (nrow(exact_match) > 0) {
      fanteam_data$fbref_name[i] <- exact_match$player[1]
      fanteam_data$fbref_team[i] <- exact_match$team[1]
      fanteam_data$match_status[i] <- "exact"
      next
    }
    
    # Step 3: Try exact match on name only (if team didn't match)
    name_only_match <- fbref_players %>%
      filter(fbref_name_normalized == ft_name_norm)
    
    if (nrow(name_only_match) == 1) {
      # Only accept if there's exactly one player with this name
      fanteam_data$fbref_name[i] <- name_only_match$player[1]
      fanteam_data$fbref_team[i] <- name_only_match$team[1]
      fanteam_data$match_status[i] <- "name_only"
      next
    }
    
    # No match found - stays as "unmatched"
  }
  
  # Summary
  match_summary <- table(fanteam_data$match_status)
  log_debug("Match results:", level = "INFO")
  for (status in names(match_summary)) {
    log_debug(sprintf("  %s: %d", status, match_summary[status]), level = "INFO")
  }
  
  # Write unmatched to Google Sheet
  if (write_unmatched && !is.null(gameweek)) {
    unmatched <- fanteam_data %>%
      filter(match_status == "unmatched") %>%
      select(
        fanteam_name = player,
        fanteam_club,
        position,
        salary
      ) %>%
      mutate(salary = as.character(salary))
    
    if (nrow(unmatched) > 0) {
      append_unmatched(unmatched, gameweek)
    }
  }
  
  log_debug("========================================", level = "INFO")
  
  return(fanteam_data)
}

#' Get count of unmatched players (for UI alerts)
#' @return Number of unresolved unmatched players
get_unmatched_count <- function() {
  unmatched <- load_unmatched()
  
  if (nrow(unmatched) == 0) return(0)
  
  # Count unresolved
  unresolved <- unmatched %>%
    filter(is.na(resolved) | tolower(resolved) != "true")
  
  return(nrow(unresolved))
}

#' Get unique FBref players for matching
#' @param shot_data Shot data from Google Sheets
#' @param player_data Player match stats from Google Sheets
#' @return Data frame with unique player/team combinations
get_fbref_player_list <- function(shot_data = NULL, player_data = NULL) {
  players <- data.frame(player = character(), team = character(), stringsAsFactors = FALSE)
  
  # From shot data
  if (!is.null(shot_data) && nrow(shot_data) > 0 && "player" %in% names(shot_data)) {
    shot_players <- shot_data %>%
      filter(!is.na(player) & player != "") %>%
      select(player, team) %>%
      distinct()
    players <- bind_rows(players, shot_players)
  }
  
  # From player match stats
  if (!is.null(player_data) && nrow(player_data) > 0 && "player" %in% names(player_data)) {
    player_stats <- player_data %>%
      filter(!is.na(player) & player != "") %>%
      select(player, team) %>%
      distinct()
    players <- bind_rows(players, player_stats)
  }
  
  # Deduplicate
  players <- players %>%
    distinct(player, team)
  
  log_debug("Built FBref player list:", nrow(players), "unique players", level = "INFO")
  
  return(players)
}

#' Clear corrections cache (force reload on next call)
clear_corrections_cache <- function() {
  rm(list = ls(envir = CORRECTIONS_CACHE), envir = CORRECTIONS_CACHE)
  log_debug("Corrections cache cleared", level = "INFO")
}