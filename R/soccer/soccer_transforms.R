# =============================================================================
# Soccer Data Transforms
# 
# Calculation and aggregation functions for soccer analytics
# Dependencies: soccer_config.R
# =============================================================================

# =============================================================================
# TEAM STATISTICS
# =============================================================================

#' Calculate team statistics from player-match data
#' @param shooting_data Shooting summary data
#' @param shot_data Individual shot data
#' @param possession_data Possession data
#' @param league League name (display format)
#' @param selected_team Team name
#' @return List with team stats for and against
calculate_team_stats <- function(shooting_data, shot_data, possession_data, league, selected_team) {
  log_debug("========================================", level = "INFO")
  log_debug("calculate_team_stats() for", selected_team, "in", league, level = "INFO")
  
  # Normalize team name to match data format
  selected_team <- normalize_team_names(selected_team)
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Filter to this league first
  league_shots <- shot_data %>%
    filter(league == league_data_name)
  
  # Normalize team columns for consistent matching
  league_shots <- league_shots %>%
    mutate(
      team_normalized = normalize_team_names(team),
      home_team_normalized = normalize_team_names(home_team),
      away_team_normalized = normalize_team_names(away_team)
    )
  
  # Get matches involving this team (using normalized names)
  team_match_urls <- league_shots %>%
    filter(home_team_normalized == selected_team | away_team_normalized == selected_team) %>%
    pull(match_url) %>%
    unique()
  
  n_matches <- length(team_match_urls)
  log_debug("Team has played", n_matches, "matches", level = "INFO")
  
  if (n_matches == 0) {
    return(list(
      matches_played = 0,
      goals_for = 0, goals_against = 0,
      shots_for = 0, shots_against = 0,
      xg_for = 0, xg_against = 0,
      goals_for_pg = 0, goals_against_pg = 0,
      shots_for_pg = 0, shots_against_pg = 0,
      xg_for_pg = 0, xg_against_pg = 0
    ))
  }
  
  # Calculate stats FOR (shots taken BY this team)
  shots_by_team <- league_shots %>%
    filter(team_normalized == selected_team)
  
  goals_for <- sum(shots_by_team$outcome == "Goal", na.rm = TRUE)
  shots_for <- nrow(shots_by_team)
  xg_for <- sum(shots_by_team$xg_shot, na.rm = TRUE)
  
  # Calculate stats AGAINST (shots by OPPONENTS in matches involving this team)
  shots_against_team <- league_shots %>%
    filter(match_url %in% team_match_urls) %>%
    filter(team_normalized != selected_team)
  
  goals_against <- sum(shots_against_team$outcome == "Goal", na.rm = TRUE)
  shots_against <- nrow(shots_against_team)
  xg_against <- sum(shots_against_team$xg_shot, na.rm = TRUE)
  
  # Per game averages
  stats <- list(
    matches_played = n_matches,
    goals_for = goals_for,
    goals_against = goals_against,
    shots_for = shots_for,
    shots_against = shots_against,
    xg_for = xg_for,
    xg_against = xg_against,
    goals_for_pg = round(goals_for / n_matches, 2),
    goals_against_pg = round(goals_against / n_matches, 2),
    shots_for_pg = round(shots_for / n_matches, 2),
    shots_against_pg = round(shots_against / n_matches, 2),
    xg_for_pg = round(xg_for / n_matches, 2),
    xg_against_pg = round(xg_against / n_matches, 2)
  )
  
  log_debug("Team stats calculated:", level = "INFO")
  log_debug("  Goals:", stats$goals_for, "for,", stats$goals_against, "against", level = "DEBUG")
  log_debug("  xG:", round(stats$xg_for, 1), "for,", round(stats$xg_against, 1), "against", level = "DEBUG")
  log_debug("========================================", level = "INFO")
  
  return(stats)
}

#' Calculate all team stats for league comparison (enhanced version)
#' @param shot_data Individual shot data
#' @param league League name (display format)
#' @param timeframe "season" for whole season, "last6" for last 6 games
#' @param team_goals_data Optional data frame with actual match scores (includes own goals)
#' @return Data frame with per-game stats for all teams including opponent-adjusted metrics
calculate_all_team_stats <- function(shot_data, league, timeframe = "season", team_goals_data = NULL) {
  log_debug("========================================", level = "INFO")
  log_debug("calculate_all_team_stats() for", league, "timeframe:", timeframe, level = "INFO")
  if (!is.null(team_goals_data)) {
    log_debug("Using team_goals_data for actual goal counts", level = "INFO")
  }
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Filter to this league
  league_shots <- shot_data %>%
    filter(league == league_data_name)
  
  # Normalize team names for consistent comparison
  league_shots <- league_shots %>%
    mutate(
      team_normalized = normalize_team_names(team),
      home_team_normalized = normalize_team_names(home_team),
      away_team_normalized = normalize_team_names(away_team)
    )
  
  # Prepare team_goals lookup if provided
  goals_by_match <- NULL
  if (!is.null(team_goals_data) && nrow(team_goals_data) > 0) {
    goals_by_match <- team_goals_data %>%
      filter(league == league_data_name) %>%
      mutate(
        home_team_normalized = normalize_team_names(home_team),
        away_team_normalized = normalize_team_names(away_team),
        home_goals = as.numeric(home_goals),
        away_goals = as.numeric(away_goals)
      ) %>%
      select(match_url, home_team_normalized, away_team_normalized, home_goals, away_goals)
  }
  
  # Get all teams (use normalized names)
  all_teams <- league_shots %>%
    pull(team_normalized) %>%
    unique() %>%
    sort()
  
  all_teams <- all_teams[!is.na(all_teams) & all_teams != ""]
  
  log_debug("Calculating stats for", length(all_teams), "teams", level = "INFO")
  
  # =========================================================================
  # FILTER TO TIMEFRAME IF NEEDED
  # =========================================================================
  
  if (timeframe == "last6") {
    log_debug("Filtering to last 6 games per team...", level = "DEBUG")
    
    # For each team, find their last 6 match URLs (using normalized names)
    last6_match_urls <- lapply(all_teams, function(t) {
      team_matches <- league_shots %>%
        filter(home_team_normalized == t | away_team_normalized == t) %>%
        distinct(match_url, match_date) %>%
        mutate(
          date_extracted = stringr::str_extract(match_date, "[A-Za-z]+ \\d{1,2}, \\d{4}"),
          parsed_date = as.Date(date_extracted, format = "%B %d, %Y")
        ) %>%
        arrange(desc(parsed_date)) %>%
        head(6) %>%
        pull(match_url)
      
      team_matches
    })
    
    # Combine all relevant match URLs
    relevant_match_urls <- unique(unlist(last6_match_urls))
    
    log_debug("Last 6 games filter: using", length(relevant_match_urls), "unique matches", level = "DEBUG")
    
    # Filter shots to only these matches
    league_shots <- league_shots %>%
      filter(match_url %in% relevant_match_urls)
  }
  
  # =========================================================================
  # CALCULATE LEAGUE AVERAGES (for opponent adjustment)
  # =========================================================================
  
  league_avgs <- calculate_league_averages(league_shots)
  
  # =========================================================================
  # CALCULATE PER-TEAM STATS
  # =========================================================================
  
  team_stats <- lapply(all_teams, function(t) {
    # Get match URLs for this team (using normalized names)
    team_match_urls <- league_shots %>%
      filter(home_team_normalized == t | away_team_normalized == t) %>%
      pull(match_url) %>%
      unique()
    
    n_matches <- length(team_match_urls)
    
    if (n_matches == 0) {
      return(data.frame(
        team = t,
        matches = 0,
        goals_for = 0, goals_against = 0,
        shots_for = 0, shots_against = 0,
        xg_for = 0, xg_against = 0,
        stringsAsFactors = FALSE
      ))
    }
    
    # Stats FOR (using normalized team column)
    shots_by_team <- league_shots %>% filter(team_normalized == t)
    shots_for <- nrow(shots_by_team)
    xg_for <- sum(shots_by_team$xg_shot, na.rm = TRUE)
    
    # Stats AGAINST (using normalized team column)
    shots_against_team <- league_shots %>%
      filter(match_url %in% team_match_urls, team_normalized != t)
    shots_against <- nrow(shots_against_team)
    xg_against <- sum(shots_against_team$xg_shot, na.rm = TRUE)
    
    # Goals - use team_goals_data if available (includes own goals)
    if (!is.null(goals_by_match)) {
      team_goals_matches <- goals_by_match %>%
        filter(match_url %in% team_match_urls)
      
      if (nrow(team_goals_matches) > 0) {
        # Sum goals for matches where team is home
        home_matches <- team_goals_matches %>% filter(home_team_normalized == t)
        # Sum goals for matches where team is away
        away_matches <- team_goals_matches %>% filter(away_team_normalized == t)
        
        goals_for <- sum(home_matches$home_goals, na.rm = TRUE) + sum(away_matches$away_goals, na.rm = TRUE)
        goals_against <- sum(home_matches$away_goals, na.rm = TRUE) + sum(away_matches$home_goals, na.rm = TRUE)
      } else {
        # Fallback if no goals data for these matches
        goals_for <- sum(shots_by_team$outcome == "Goal", na.rm = TRUE)
        goals_against <- sum(shots_against_team$outcome == "Goal", na.rm = TRUE)
      }
    } else {
      # No team_goals data, use shot-based counting
      goals_for <- sum(shots_by_team$outcome == "Goal", na.rm = TRUE)
      goals_against <- sum(shots_against_team$outcome == "Goal", na.rm = TRUE)
    }
    
    data.frame(
      team = t,
      matches = n_matches,
      goals_for = goals_for,
      goals_against = goals_against,
      shots_for = shots_for,
      shots_against = shots_against,
      xg_for = xg_for,
      xg_against = xg_against,
      stringsAsFactors = FALSE
    )
  })
  
  stats_df <- bind_rows(team_stats)
  
  # =========================================================================
  # CALCULATE DERIVED METRICS
  # =========================================================================
  
  stats_df <- stats_df %>%
    mutate(
      # Per game metrics
      goals_for_pg = round(goals_for / matches, 2),
      goals_against_pg = round(goals_against / matches, 2),
      goal_diff_pg = round((goals_for - goals_against) / matches, 2),
      shots_for_pg = round(shots_for / matches, 2),
      shots_against_pg = round(shots_against / matches, 2),
      xg_for_pg = round(xg_for / matches, 2),
      xg_against_pg = round(xg_against / matches, 2),
      xg_diff_pg = round((xg_for - xg_against) / matches, 2),
      
      # Shot quality
      xg_per_shot_for = round(xg_for / pmax(shots_for, 1), 3),
      xg_per_shot_against = round(xg_against / pmax(shots_against, 1), 3),
      
      # Opponent-adjusted (vs league average) - Offensive
      goals_vs_opp_avg = round(goals_for_pg - league_avgs$goals_pg, 2),
      shots_vs_opp_avg = round(shots_for_pg - league_avgs$shots_pg, 2),
      xg_vs_opp_avg = round(xg_for_pg - league_avgs$xg_pg, 2),
      
      # Opponent-adjusted (vs league average) - Defensive (negative = better defense)
      goals_against_vs_opp_avg = round(goals_against_pg - league_avgs$goals_pg, 2),
      shots_against_vs_opp_avg = round(shots_against_pg - league_avgs$shots_pg, 2),
      xg_against_vs_opp_avg = round(xg_against_pg - league_avgs$xg_pg, 2),
      
      # Pythagorean win %
      pythag_pct = round(
        goals_for^PYTHAG_EXPONENT / 
          (goals_for^PYTHAG_EXPONENT + goals_against^PYTHAG_EXPONENT) * 100,
        1
      ),
      xpythag_pct = round(
        xg_for^PYTHAG_EXPONENT / 
          (xg_for^PYTHAG_EXPONENT + xg_against^PYTHAG_EXPONENT) * 100,
        1
      ),
      
      # Luck factor
      luck_factor = round(pythag_pct - xpythag_pct, 1),
      
      # Logo path
      logo_path = sapply(team, get_soccer_team_logo)
    )
  
  log_debug("All team stats calculated:", nrow(stats_df), "teams", level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(stats_df)
}

#' Calculate league averages
#' @param league_shots Shot data filtered to a league
#' @return List with per-team-per-game averages
calculate_league_averages <- function(league_shots) {
  # Count unique matches
  n_matches <- league_shots %>%
    distinct(match_url) %>%
    nrow()
  
  if (n_matches == 0) {
    return(list(
      goals_pg = 0,
      shots_pg = 0,
      xg_pg = 0
    ))
  }
  
  total_goals <- sum(league_shots$outcome == "Goal", na.rm = TRUE)
  total_shots <- nrow(league_shots)
  total_xg <- sum(league_shots$xg_shot, na.rm = TRUE)
  
  # Per team per game (divide by 2 since each match has 2 teams)
  list(
    goals_pg = round(total_goals / n_matches / 2, 2),
    shots_pg = round(total_shots / n_matches / 2, 2),
    xg_pg = round(total_xg / n_matches / 2, 2)
  )
}

# =============================================================================
# GAME-BY-GAME STATS
# =============================================================================

#' Calculate game-by-game stats for a team
#' @param shot_data Individual shot data
#' @param league League name (display format)
#' @param selected_team Team name
#' @param team_goals_data Optional data frame with actual match scores (includes own goals)
#' @return Data frame with per-game stats
calculate_game_by_game_stats <- function(shot_data, league, selected_team, team_goals_data = NULL) {
  log_debug("========================================", level = "INFO")
  log_debug("calculate_game_by_game_stats() for", selected_team, "in", league, level = "INFO")
  if (!is.null(team_goals_data)) {
    log_debug("Using team_goals_data for actual goal counts (includes own goals)", level = "INFO")
  }
  
  # Normalize team name to match shot data format
  selected_team <- normalize_team_names(selected_team)
  log_debug("Normalized team name:", selected_team, level = "DEBUG")
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Filter to league
  league_shots <- shot_data %>%
    filter(.data$league == league_data_name)
  
  # Normalize team columns for consistent matching
  league_shots <- league_shots %>%
    mutate(
      home_team_normalized = normalize_team_names(home_team),
      away_team_normalized = normalize_team_names(away_team)
    )
  
  # Prepare team_goals lookup if provided
  goals_lookup <- NULL
  if (!is.null(team_goals_data) && nrow(team_goals_data) > 0) {
    goals_lookup <- team_goals_data %>%
      filter(league == league_data_name) %>%
      mutate(
        home_team_normalized = normalize_team_names(home_team),
        away_team_normalized = normalize_team_names(away_team),
        home_goals = as.numeric(home_goals),
        away_goals = as.numeric(away_goals)
      ) %>%
      select(match_url, home_team_normalized, away_team_normalized, home_goals, away_goals)
  }
  
  # Check available columns for ordering
  has_gameweek <- "gameweek" %in% names(league_shots)
  has_match_date <- "match_date" %in% names(league_shots)
  
  log_debug("Columns available - gameweek:", has_gameweek, ", match_date:", has_match_date, level = "DEBUG")
  
  # --- PRIMARY STRATEGY: Parse actual match_date for chronological ordering ---
  if (has_match_date) {
    log_debug("Parsing match_date column for chronological ordering", level = "INFO")
    
    team_matches <- league_shots %>%
      filter(home_team_normalized == .env$selected_team | away_team_normalized == .env$selected_team) %>%
      distinct(match_url, home_team_normalized, away_team_normalized, match_date) %>%
      rename(home_team = home_team_normalized, away_team = away_team_normalized) %>%
      mutate(
        date_extracted = stringr::str_extract(match_date, "[A-Za-z]+ \\d{1,2}, \\d{4}"),
        parsed_date = as.Date(date_extracted, format = "%B %d, %Y")
      )
    
    n_valid <- sum(!is.na(team_matches$parsed_date))
    log_debug("Successfully parsed", n_valid, "of", nrow(team_matches), "match dates", level = "INFO")
    
    if (n_valid > 0) {
      team_matches <- team_matches %>%
        arrange(parsed_date) %>%
        mutate(match_num = row_number())
    } else {
      has_match_date <- FALSE
    }
  }
  
  # --- FALLBACK: Use gameweek if date parsing unavailable/failed ---
  if (!has_match_date && has_gameweek) {
    log_debug("Using gameweek column for ordering (fallback)", level = "INFO")
    
    team_matches <- league_shots %>%
      filter(home_team_normalized == .env$selected_team | away_team_normalized == .env$selected_team) %>%
      distinct(match_url, home_team_normalized, away_team_normalized, gameweek) %>%
      rename(home_team = home_team_normalized, away_team = away_team_normalized) %>%
      mutate(gameweek_num = suppressWarnings(as.numeric(gameweek))) %>%
      arrange(gameweek_num) %>%
      mutate(match_num = row_number())
  }
  
  # --- LAST RESORT: Use row order ---
  if (!exists("team_matches") || is.null(team_matches)) {
    log_debug("No ordering column found, using row order", level = "WARN")
    
    team_matches <- league_shots %>%
      filter(home_team_normalized == .env$selected_team | away_team_normalized == .env$selected_team) %>%
      distinct(match_url, home_team_normalized, away_team_normalized) %>%
      rename(home_team = home_team_normalized, away_team = away_team_normalized) %>%
      mutate(match_num = row_number())
  }
  
  log_debug("Found", nrow(team_matches), "unique matches for", selected_team, level = "INFO")
  
  if (nrow(team_matches) == 0) {
    return(data.frame(
      match_num = integer(),
      opponent = character(),
      is_home = logical(),
      xg_for = numeric(),
      xg_against = numeric(),
      shots_for = integer(),
      shots_against = integer(),
      goals_for = integer(),
      goals_against = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  # --- CALCULATE STATS FOR EACH MATCH ---
  match_stats <- lapply(seq_len(nrow(team_matches)), function(i) {
    match_info <- team_matches[i, ]
    match_url_i <- match_info$match_url
    match_shots <- league_shots %>% filter(match_url == match_url_i)
    
    opponent <- ifelse(match_info$home_team == selected_team, match_info$away_team, match_info$home_team)
    is_home <- match_info$home_team == selected_team
    
    # Normalize team column for comparison (shot data may have different team name format)
    match_shots <- match_shots %>%
      mutate(team_normalized = normalize_team_names(team))
    
    # Shots FOR - compare normalized names
    shots_for_df <- match_shots %>% filter(team_normalized == .env$selected_team)
    xg_for <- sum(shots_for_df$xg_shot, na.rm = TRUE)
    shots_for <- nrow(shots_for_df)
    
    # Shots AGAINST
    shots_against_df <- match_shots %>% filter(team_normalized != .env$selected_team)
    xg_against <- sum(shots_against_df$xg_shot, na.rm = TRUE)
    shots_against <- nrow(shots_against_df)
    
    # Goals - use team_goals_data if available (includes own goals), otherwise count from shots
    if (!is.null(goals_lookup)) {
      match_goals <- goals_lookup %>% filter(match_url == match_url_i)
      if (nrow(match_goals) > 0) {
        if (is_home) {
          goals_for <- match_goals$home_goals[1]
          goals_against <- match_goals$away_goals[1]
        } else {
          goals_for <- match_goals$away_goals[1]
          goals_against <- match_goals$home_goals[1]
        }
      } else {
        # Fallback to shot-based counting if no match in goals data
        goals_for <- sum(shots_for_df$outcome == "Goal", na.rm = TRUE)
        goals_against <- sum(shots_against_df$outcome == "Goal", na.rm = TRUE)
      }
    } else {
      # No team_goals data provided, use shot-based counting
      goals_for <- sum(shots_for_df$outcome == "Goal", na.rm = TRUE)
      goals_against <- sum(shots_against_df$outcome == "Goal", na.rm = TRUE)
    }
    
    data.frame(
      match_num = match_info$match_num,
      opponent = opponent,
      is_home = is_home,
      xg_for = round(xg_for, 2),
      xg_against = round(xg_against, 2),
      shots_for = shots_for,
      shots_against = shots_against,
      goals_for = goals_for,
      goals_against = goals_against,
      stringsAsFactors = FALSE
    )
  })
  
  result <- bind_rows(match_stats)
  
  log_debug("Game-by-game stats calculated:", nrow(result), "matches", level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(result)
}

# =============================================================================
# PLAYER STATISTICS
# =============================================================================

#' Get player statistics for a team
#' @param player_data Combined player match stats data
#' @param shot_data Individual shot data
#' @param league League name (display format)
#' @param team Team name
#' @return Data frame with player stats
get_player_stats <- function(player_data, shot_data, league, team) {
  log_debug("========================================", level = "INFO")
  log_debug("get_player_stats() for", team, "in", league, level = "INFO")
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Normalize team name for matching
  team_normalized <- normalize_team_names(team)
  
  # Check which team column exists in player_data
  team_col <- if ("team" %in% names(player_data)) "team" else if ("squad" %in% names(player_data)) "squad" else NULL
  
  if (is.null(team_col)) {
    log_debug("No team column found in player_data", level = "WARN")
    return(data.frame())
  }
  
  # Use combined player data
  log_debug("Using combined player match stats", level = "INFO")
  
  # Filter first, then coerce types (Google Sheets often returns character)
  filtered_data <- player_data %>%
    filter(league == league_data_name, !!sym(team_col) == team_normalized)
  
  if (nrow(filtered_data) == 0) {
    log_debug("No data found for team:", team_normalized, level = "WARN")
    return(data.frame())
  }
  
  # Coerce numeric columns
  numeric_cols <- c("minutes", "goals", "assists", "shots", "xg", "sca", "gca", 
                    "touches", "progressive_passes", "progressive_carries")
  for (col in numeric_cols) {
    if (col %in% names(filtered_data)) {
      filtered_data[[col]] <- as.numeric(filtered_data[[col]])
    }
  }
  
  player_stats <- filtered_data %>%
    group_by(player, position) %>%
    summarise(
      matches = n(),
      minutes = sum(minutes, na.rm = TRUE),
      goals = sum(goals, na.rm = TRUE),
      assists = if ("assists" %in% names(.)) sum(assists, na.rm = TRUE) else NA_integer_,
      shots = if ("shots" %in% names(.)) sum(shots, na.rm = TRUE) else 0L,
      xg = if ("xg" %in% names(.)) sum(xg, na.rm = TRUE) else 0,
      sca = if ("sca" %in% names(.)) sum(sca, na.rm = TRUE) else NA_integer_,
      gca = if ("gca" %in% names(.)) sum(gca, na.rm = TRUE) else NA_integer_,
      touches = if ("touches" %in% names(.)) sum(touches, na.rm = TRUE) else NA_integer_,
      progressive_passes = if ("progressive_passes" %in% names(.)) sum(progressive_passes, na.rm = TRUE) else 0L,
      progressive_carries = if ("progressive_carries" %in% names(.)) sum(progressive_carries, na.rm = TRUE) else 0L,
      .groups = "drop"
    ) %>%
    mutate(
      mins_per_match = round(minutes / pmax(matches, 1), 0),
      progressive_actions = progressive_passes + progressive_carries
    )
  
  # Supplement with shot data if available
  if (!is.null(shot_data) && nrow(shot_data) > 0) {
    shot_stats <- shot_data %>%
      filter(league == league_data_name, team == team_normalized) %>%
      group_by(player) %>%
      summarise(
        shot_goals = sum(outcome == "Goal", na.rm = TRUE),
        shot_count = n(),
        shot_xg = sum(xg_shot, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Use shot data to fill in missing values
    if (nrow(shot_stats) > 0) {
      player_stats <- player_stats %>%
        left_join(shot_stats, by = "player") %>%
        mutate(
          shots = coalesce(shots, shot_count, 0L),
          xg = coalesce(xg, shot_xg, 0)
        ) %>%
        select(-any_of(c("shot_goals", "shot_count", "shot_xg")))
    }
  }
  
  log_debug("Player stats calculated:", nrow(player_stats), "players", level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(player_stats)
}

# =============================================================================
# SET PIECE ANALYSIS
# =============================================================================

#' Get set piece specialists (defenders/midfielders with headers from set pieces)
#' @param shot_data Individual shot data
#' @param possession_data Possession data (for position info)
#' @param league League name (display format)
#' @param team Team name (NULL for all teams in league)
#' @return Data frame with set piece header specialists
get_set_piece_specialists <- function(shot_data, player_data, league, team = NULL) {
  log_debug("========================================", level = "INFO")
  log_debug("get_set_piece_specialists() for", league, level = "INFO")
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Filter for set piece shots
  set_piece_shots <- shot_data %>%
    filter(league == league_data_name) %>%
    filter(
      sca_1_type == "Pass (Dead)" | 
        sca_2_type == "Pass (Dead)" |
        grepl("Penalty|Free kick", notes, ignore.case = TRUE)
    )
  
  if (!is.null(team)) {
    team_normalized <- normalize_team_names(team)
    set_piece_shots <- set_piece_shots %>%
      filter(team == team_normalized)
  }
  
  # Get headers specifically
  set_piece_headers <- set_piece_shots %>%
    filter(body_part == "Head")
  
  # Get position info from player data
  team_col <- if ("team" %in% names(player_data)) "team" else if ("squad" %in% names(player_data)) "squad" else "team"
  
  player_positions <- player_data %>%
    filter(league == league_data_name) %>%
    distinct(player, position, !!sym(team_col)) %>%
    group_by(player) %>%
    slice(1) %>%
    ungroup()
  
  # Aggregate by player
  header_stats <- set_piece_headers %>%
    group_by(player, team) %>%
    summarise(
      set_piece_headers = n(),
      sp_header_goals = sum(outcome == "Goal", na.rm = TRUE),
      sp_header_xg = sum(xg_shot, na.rm = TRUE),
      avg_xg_per_header = round(mean(xg_shot, na.rm = TRUE), 3),
      .groups = "drop"
    ) %>%
    left_join(player_positions, by = "player") %>%
    filter(grepl("CB|LB|RB|WB|DM|CM|CDM", position, ignore.case = TRUE) | is.na(position)) %>%
    arrange(desc(set_piece_headers))
  
  log_debug("Found", nrow(header_stats), "set piece header specialists", level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(header_stats)
}

#' Get all set piece shots for a team
#' @param shot_data Individual shot data
#' @param league League name (display format)
#' @param team Team name
#' @return Data frame with set piece shots
get_set_piece_shots <- function(shot_data, league, team = NULL) {
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  set_piece_shots <- shot_data %>%
    filter(league == league_data_name) %>%
    filter(
      sca_1_type == "Pass (Dead)" | 
        sca_2_type == "Pass (Dead)" |
        grepl("Penalty|Free kick", notes, ignore.case = TRUE)
    )
  
  if (!is.null(team)) {
    set_piece_shots <- set_piece_shots %>%
      filter(team == !!team)
  }
  
  return(set_piece_shots)
}