# =============================================================================
# Soccer Data Transforms
# 
# Calculation and aggregation functions for soccer analytics
# Dependencies: soccer_config.R
# =============================================================================

#' Ensure shot data has a team column (copy from squad if needed) and numeric xg
#' @param data Data frame to check
#' @return Data frame with team column guaranteed and xg as numeric
ensure_team_column <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  # Ensure team column exists
  
  if (!"team" %in% names(data) && "squad" %in% names(data)) {
    data$team <- data$squad
  } else if (!"team" %in% names(data)) {
    data$team <- NA_character_
  }
  
  # Ensure xg is numeric (Google Sheets often returns as character)
  if ("xg" %in% names(data) && !is.numeric(data$xg)) {
    data$xg <- suppressWarnings(as.numeric(data$xg))
  }
  
  return(data)
}

#' Derive team column for shot data using player_data lookup
#' @param shot_data Shot data with player column
#' @param player_data Player match stats with team info
#' @return Shot data with team column populated
derive_shot_teams <- function(shot_data, player_data) {
  if (is.null(player_data) || nrow(player_data) == 0) {
    return(shot_data)
  }
  
  # Check if team column already has data
  if ("team" %in% names(shot_data) && !all(is.na(shot_data$team))) {
    return(shot_data)
  }
  
  # Get team lookup from player_data
  team_col <- if ("team" %in% names(player_data)) "team" else if ("squad" %in% names(player_data)) "squad" else NULL
  if (is.null(team_col)) {
    return(shot_data)
  }
  
  player_teams <- player_data %>%
    filter(!is.na(!!sym(team_col))) %>%
    group_by(player, league) %>%
    summarise(derived_team = first(!!sym(team_col)), .groups = "drop")
  
  # Join to add team info
  shot_data <- shot_data %>%
    left_join(player_teams, by = c("player", "league")) %>%
    mutate(team = coalesce(team, squad, derived_team)) %>%
    select(-any_of("derived_team"))
  
  log_debug(sprintf("Derived team for %d of %d shots", 
                    sum(!is.na(shot_data$team)), nrow(shot_data)), level = "DEBUG")
  
  return(shot_data)
}

# =============================================================================
# TEAM STATISTICS
# =============================================================================

#' Calculate team statistics from player-match data
#' @param shooting_data Shooting summary data
#' @param shot_data Individual shot data
#' @param possession_data Possession data
#' @param league League name (display format)
#' @param selected_team Team name
#' @param player_data Optional player data for deriving team from shots
#' @return List with team stats for and against
calculate_team_stats <- function(shooting_data, shot_data, possession_data, league, selected_team, player_data = NULL) {
  log_debug("========================================", level = "INFO")
  log_debug("calculate_team_stats() for", selected_team, "in", league, level = "INFO")
  
  # Normalize team name to match data format
  selected_team <- normalize_team_names(selected_team)
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Derive team column from player_data if needed
  if (!is.null(player_data)) {
    shot_data <- derive_shot_teams(shot_data, player_data)
  }
  
  # Ensure team column exists and xg is numeric
  shot_data <- ensure_team_column(shot_data)
  
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
  xg_for <- sum(as.numeric(shots_by_team$xg), na.rm = TRUE)
  
  # Calculate stats AGAINST (shots by OPPONENTS in matches involving this team)
  shots_against_team <- league_shots %>%
    filter(match_url %in% team_match_urls) %>%
    filter(team_normalized != selected_team)
  
  goals_against <- sum(shots_against_team$outcome == "Goal", na.rm = TRUE)
  shots_against <- nrow(shots_against_team)
  xg_against <- sum(as.numeric(shots_against_team$xg), na.rm = TRUE)
  
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
#' @param player_data Optional player data for deriving team from shots
#' @return Data frame with per-game stats for all teams including opponent-adjusted metrics
calculate_all_team_stats <- function(shot_data, league, timeframe = "season", team_goals_data = NULL, player_data = NULL) {
  log_debug("========================================", level = "INFO")
  log_debug("calculate_all_team_stats() for", league, "timeframe:", timeframe, level = "INFO")
  if (!is.null(team_goals_data)) {
    log_debug("Using team_goals_data for actual goal counts", level = "INFO")
  }
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Derive team column from player_data if needed
  if (!is.null(player_data)) {
    shot_data <- derive_shot_teams(shot_data, player_data)
  }
  
  # Ensure team column exists and xg is numeric
  shot_data <- ensure_team_column(shot_data)
  
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
  
  # Get all teams - try team_normalized first, fallback to home/away teams
  all_teams <- league_shots %>%
    pull(team_normalized) %>%
    unique()
  all_teams <- all_teams[!is.na(all_teams) & all_teams != ""]
  
  # Fallback: get teams from home_team/away_team if team column is empty
  if (length(all_teams) == 0) {
    log_debug("team column empty, using home_team/away_team for team list", level = "DEBUG")
    home_teams <- unique(league_shots$home_team_normalized)
    away_teams <- unique(league_shots$away_team_normalized)
    all_teams <- unique(c(home_teams, away_teams))
    all_teams <- all_teams[!is.na(all_teams) & all_teams != ""]
  }
  
  all_teams <- sort(all_teams)
  
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
    xg_for <- sum(as.numeric(shots_by_team$xg), na.rm = TRUE)
    
    # Stats AGAINST (using normalized team column)
    shots_against_team <- league_shots %>%
      filter(match_url %in% team_match_urls, team_normalized != t)
    shots_against <- nrow(shots_against_team)
    xg_against <- sum(as.numeric(shots_against_team$xg), na.rm = TRUE)
    
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
  
  # =========================================================================
  # CALCULATE TRUE OPPONENT-ADJUSTED METRICS (vs actual opponent strength)
  # =========================================================================
  
  # Build match-opponent lookup from shots
  match_teams <- league_shots %>%
    distinct(match_url, home_team_normalized, away_team_normalized)
  
  # For each team, calculate opponent-adjusted metrics
  opp_adjusted <- lapply(all_teams, function(t) {
    # Get all matches for this team
    team_matches <- match_teams %>%
      filter(home_team_normalized == t | away_team_normalized == t) %>%
      mutate(
        opponent = ifelse(home_team_normalized == t, away_team_normalized, home_team_normalized)
      )
    
    if (nrow(team_matches) == 0) {
      return(data.frame(
        team = t,
        opp_avg_xga = NA_real_,
        opp_avg_xgf = NA_real_,
        xgf_vs_opp_strength = NA_real_,
        xga_vs_opp_strength = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    
    # Get opponents' stats
    opponents <- unique(team_matches$opponent)
    opp_stats <- stats_df %>%
      filter(team %in% opponents) %>%
      summarise(
        opp_avg_xga = mean(xg_against_pg, na.rm = TRUE),  # What opponents typically concede
        opp_avg_xgf = mean(xg_for_pg, na.rm = TRUE)       # What opponents typically create
      )
    
    # Get this team's stats
    team_stats_row <- stats_df %>% filter(team == t)
    
    if (nrow(team_stats_row) == 0) {
      return(data.frame(
        team = t,
        opp_avg_xga = opp_stats$opp_avg_xga,
        opp_avg_xgf = opp_stats$opp_avg_xgf,
        xgf_vs_opp_strength = NA_real_,
        xga_vs_opp_strength = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    
    # Calculate opponent-adjusted metrics
    # Positive xgf_vs_opp_strength = team creates MORE xG than their opponents typically concede
    # Negative xga_vs_opp_strength = team concedes LESS xG than their opponents typically create
    data.frame(
      team = t,
      opp_avg_xga = round(opp_stats$opp_avg_xga, 2),
      opp_avg_xgf = round(opp_stats$opp_avg_xgf, 2),
      xgf_vs_opp_strength = round(team_stats_row$xg_for_pg - opp_stats$opp_avg_xga, 2),
      xga_vs_opp_strength = round(team_stats_row$xg_against_pg - opp_stats$opp_avg_xgf, 2),
      stringsAsFactors = FALSE
    )
  })
  
  opp_adjusted_df <- bind_rows(opp_adjusted)
  
  # Join opponent-adjusted metrics back to main stats
  stats_df <- stats_df %>%
    left_join(opp_adjusted_df, by = "team")
  
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
  total_xg <- sum(as.numeric(league_shots$xg), na.rm = TRUE)
  
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
  
  # Ensure team column exists and xg is numeric
  shot_data <- ensure_team_column(shot_data)
  
  # Filter to league
  league_shots <- shot_data %>%
    filter(.data$league == league_data_name)
  
  # Normalize team columns for consistent matching
  league_shots <- league_shots %>%
    mutate(
      team_normalized = normalize_team_names(team),
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
    
    # Shots FOR - compare normalized names (already normalized in league_shots)
    shots_for_df <- match_shots %>% filter(team_normalized == .env$selected_team)
    xg_for <- sum(as.numeric(shots_for_df$xg), na.rm = TRUE)
    shots_for <- nrow(shots_for_df)
    
    # Shots AGAINST
    shots_against_df <- match_shots %>% filter(team_normalized != .env$selected_team)
    xg_against <- sum(as.numeric(shots_against_df$xg), na.rm = TRUE)
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
  
  # DEBUG: Log available columns
  log_debug("Available columns:", paste(names(player_data), collapse = ", "), level = "INFO")
  
  # Check if touch columns exist
  touch_cols <- c("touches", "touches_att_3rd", "touches_att_pen_area", "touches_def_3rd", "touches_mid_3rd")
  for (col in touch_cols) {
    if (col %in% names(player_data)) {
      log_debug(sprintf("Column %s EXISTS - sample values: %s", col, 
                        paste(head(player_data[[col]], 3), collapse = ", ")), level = "INFO")
    } else {
      log_debug(sprintf("Column %s MISSING", col), level = "WARN")
    }
  }
  
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
  
  # Coerce numeric columns (including new touch columns)
  numeric_cols <- c("minutes", "goals", "assists", "shots", "xg", "sca", "gca", 
                    "touches", "touches_att_3rd", "touches_att_pen_area",
                    "progressive_passes", "progressive_carries")
  for (col in numeric_cols) {
    if (col %in% names(filtered_data)) {
      filtered_data[[col]] <- as.numeric(filtered_data[[col]])
    }
  }
  
  # Add simplified position column
  filtered_data <- filtered_data %>%
    mutate(position_simple = simplify_position(position))
  
  # DEBUG: Log position data for specific players
  diallo_data <- filtered_data %>% filter(grepl("Diallo", player, ignore.case = TRUE))
  if (nrow(diallo_data) > 0) {
    log_debug("=== DIALLO DEBUG ===", level = "INFO")
    log_debug("Raw positions:", paste(unique(diallo_data$position), collapse = ", "), level = "INFO")
    log_debug("Simplified positions:", paste(unique(diallo_data$position_simple), collapse = ", "), level = "INFO")
    log_debug("Touches att_3rd sample:", paste(head(diallo_data$touches_att_3rd, 5), collapse = ", "), level = "INFO")
    log_debug("Touches att_pen sample:", paste(head(diallo_data$touches_att_pen_area, 5), collapse = ", "), level = "INFO")
  }
  
  # First, determine each player's primary position (most common one)
  player_positions <- filtered_data %>%
    group_by(player, position_simple) %>%
    summarise(pos_minutes = sum(minutes, na.rm = TRUE), .groups = "drop") %>%
    group_by(player) %>%
    slice_max(pos_minutes, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(player, position = position_simple)
  
  # Check which columns exist before summarise (safer than checking inside)
  has_assists <- "assists" %in% names(filtered_data)
  has_shots <- "shots" %in% names(filtered_data)
  has_xg <- "xg" %in% names(filtered_data)
  has_sca <- "sca" %in% names(filtered_data)
  has_gca <- "gca" %in% names(filtered_data)
  has_touches <- "touches" %in% names(filtered_data)
  has_touches_att_3rd <- "touches_att_3rd" %in% names(filtered_data)
  has_touches_att_pen <- "touches_att_pen_area" %in% names(filtered_data)
  has_prog_passes <- "progressive_passes" %in% names(filtered_data)
  has_prog_carries <- "progressive_carries" %in% names(filtered_data)
  
  log_debug(sprintf("Column checks: assists=%s, shots=%s, xg=%s, sca=%s, gca=%s, touches=%s, att_3rd=%s, att_pen=%s",
                    has_assists, has_shots, has_xg, has_sca, has_gca, has_touches, has_touches_att_3rd, has_touches_att_pen), level = "INFO")
  
  # Now aggregate stats by player only (no position grouping)
  player_stats <- filtered_data %>%
    mutate(minutes_num = as.numeric(minutes)) %>%
    group_by(player) %>%
    summarise(
      matches = n(),
      minutes = sum(minutes_num, na.rm = TRUE),
      # Appearance counts by minutes threshold - use numeric minutes
      apps_60 = sum(minutes_num >= 60, na.rm = TRUE),
      apps_90 = sum(minutes_num >= 89, na.rm = TRUE),
      # Goals & assists
      goals = sum(as.numeric(goals), na.rm = TRUE),
      assists = if (has_assists) sum(as.numeric(assists), na.rm = TRUE) else NA_real_,
      # Shooting
      shots = if (has_shots) sum(as.numeric(shots), na.rm = TRUE) else 0,
      xg = if (has_xg) sum(as.numeric(xg), na.rm = TRUE) else 0,
      # Shot creation
      sca = if (has_sca) sum(as.numeric(sca), na.rm = TRUE) else NA_real_,
      gca = if (has_gca) sum(as.numeric(gca), na.rm = TRUE) else NA_real_,
      # Touches
      touches = if (has_touches) sum(as.numeric(touches), na.rm = TRUE) else NA_real_,
      touches_att_3rd = if (has_touches_att_3rd) sum(as.numeric(touches_att_3rd), na.rm = TRUE) else NA_real_,
      touches_att_pen = if (has_touches_att_pen) sum(as.numeric(touches_att_pen_area), na.rm = TRUE) else NA_real_,
      # Progressive actions
      progressive_passes = if (has_prog_passes) sum(as.numeric(progressive_passes), na.rm = TRUE) else 0,
      progressive_carries = if (has_prog_carries) sum(as.numeric(progressive_carries), na.rm = TRUE) else 0,
      .groups = "drop"
    ) %>%
    # Join back the primary position
    left_join(player_positions, by = "player") %>%
    mutate(
      mins_per_match = round(minutes / pmax(matches, 1), 0),
      progressive_actions = progressive_passes + progressive_carries
    )
  
  # Supplement with shot data if available
  if (!is.null(shot_data) && nrow(shot_data) > 0) {
    # Ensure team column exists and xg is numeric in shot data
    shot_data <- ensure_team_column(shot_data)
    
    # Get list of players from this team to filter shot data
    team_players <- unique(player_stats$player)
    
    shot_stats <- shot_data %>%
      filter(league == league_data_name, player %in% team_players) %>%
      group_by(player) %>%
      summarise(
        shot_goals = sum(outcome == "Goal", na.rm = TRUE),
        shot_count = n(),
        shot_xg = sum(as.numeric(xg), na.rm = TRUE),
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
#' @param player_data Player match stats (for position info)
#' @param league League name (display format)
#' @param team Team name (NULL for all teams in league)
#' @return Data frame with set piece header specialists
get_set_piece_specialists <- function(shot_data, player_data, league, team = NULL) {
  log_debug("========================================", level = "INFO")
  log_debug("get_set_piece_specialists() for", league, level = "INFO")
  
  league_data_name <- LEAGUE_DATA_NAMES[league]
  if (is.na(league_data_name)) league_data_name <- league
  
  # Get team info for players from player_data
  team_col <- if ("team" %in% names(player_data)) "team" else if ("squad" %in% names(player_data)) "squad" else NULL
  
  if (is.null(team_col)) {
    log_debug("No team column in player_data", level = "WARN")
    return(data.frame())
  }
  
  player_teams <- player_data %>%
    filter(league == league_data_name) %>%
    group_by(player) %>%
    summarise(team = first(!!sym(team_col)), .groups = "drop") %>%
    filter(!is.na(team))
  
  # Ensure team column exists and xg is numeric in shot data
  shot_data <- ensure_team_column(shot_data)
  
  # Filter for set piece shots
  set_piece_shots <- shot_data %>%
    filter(league == league_data_name) %>%
    filter(
      sca_1_type == "Pass (Dead)" | 
        sca_2_type == "Pass (Dead)" |
        grepl("Penalty|Free kick", notes, ignore.case = TRUE)
    ) %>%
    # Join to get team info from player_data
    left_join(player_teams, by = "player", suffix = c("", "_lookup")) %>%
    mutate(team = coalesce(team, team_lookup)) %>%
    select(-any_of("team_lookup"))
  
  if (!is.null(team)) {
    team_normalized <- normalize_team_names(team)
    set_piece_shots <- set_piece_shots %>%
      filter(normalize_team_names(team) == team_normalized)
  }
  
  # Get headers specifically
  set_piece_headers <- set_piece_shots %>%
    filter(body_part == "Head")
  
  player_positions <- player_data %>%
    filter(league == league_data_name) %>%
    mutate(position_simple = simplify_position(position)) %>%
    group_by(player, !!sym(team_col), position_simple) %>%
    summarise(pos_minutes = sum(as.numeric(minutes), na.rm = TRUE), .groups = "drop") %>%
    group_by(player) %>%
    slice_max(pos_minutes, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(player, position = position_simple)
  
  # Aggregate by player
  header_stats <- set_piece_headers %>%
    group_by(player, team) %>%
    summarise(
      set_piece_headers = n(),
      sp_header_goals = sum(outcome == "Goal", na.rm = TRUE),
      sp_header_xg = sum(as.numeric(xg), na.rm = TRUE),
      avg_xg_per_header = round(mean(as.numeric(xg), na.rm = TRUE), 3),
      .groups = "drop"
    ) %>%
    left_join(player_positions, by = "player") %>%
    # Filter for DEF and MID positions (not forwards)
    filter(position %in% c("DEF", "MID") | is.na(position)) %>%
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
  
  # Ensure team column exists and xg is numeric
  shot_data <- ensure_team_column(shot_data)
  
  set_piece_shots <- shot_data %>%
    filter(league == league_data_name) %>%
    filter(
      sca_1_type == "Pass (Dead)" | 
        sca_2_type == "Pass (Dead)" |
        grepl("Penalty|Free kick", notes, ignore.case = TRUE)
    )
  
  if (!is.null(team)) {
    team_normalized <- normalize_team_names(team)
    # Try 'team' column first, then fallback to home_team/away_team
    if ("team" %in% names(set_piece_shots) && !all(is.na(set_piece_shots$team))) {
      set_piece_shots <- set_piece_shots %>%
        filter(normalize_team_names(team) == team_normalized)
    } else if ("home_team" %in% names(set_piece_shots)) {
      # Filter to shots where this team was home or away
      set_piece_shots <- set_piece_shots %>%
        filter(normalize_team_names(home_team) == team_normalized | 
                 normalize_team_names(away_team) == team_normalized)
    }
  }
  
  return(set_piece_shots)
}