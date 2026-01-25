# =============================================================================
# Soccer Betting Data Loader
# 
# Functions for fetching:
#   - Odds from The Odds API
#   - League standings from BBC Sport
#   - Cache management
#
# Dependencies: httr, jsonlite, rvest, dplyr, lubridate, stringr
# =============================================================================

# =============================================================================
# BBC SCRAPER
# =============================================================================

#' Scrape a single BBC league table
#' @param url BBC league table URL
#' @param league_name League name for the output
#' @return Tibble with team standings data
scrape_betting_bbc_table <- function(url, league_name) {
  log_debug("BBC Scraper: Fetching", league_name)
  
  tryCatch({
    page <- rvest::read_html(url)
    
    # Try standard HTML table
    table_data <- page %>%
      rvest::html_element("table") %>%
      rvest::html_table()
    
    if (!is.null(table_data)) {
      result <- process_betting_table(table_data, league_name)
      return(result)
    }
    
    log_debug("BBC Scraper: Could not find table for", league_name, level = "WARN")
    return(empty_betting_standings(league_name))
    
  }, error = function(e) {
    log_debug("BBC Scraper: Error -", e$message, level = "WARN")
    return(empty_betting_standings(league_name))
  })
}

#' Process standard HTML table from BBC
process_betting_table <- function(table_data, league_name) {
  # Clean column names
  colnames(table_data) <- tolower(gsub(" ", "_", colnames(table_data)))
  
  # Convert all columns to character first to avoid list columns from HTML parsing
  table_data <- table_data %>%
    as_tibble() %>%
    mutate(across(everything(), ~ if (is.list(.x)) sapply(.x, function(v) paste(v, collapse = " ")) else as.character(.x)))
  
  result <- table_data %>%
    mutate(
      team = if ("team" %in% names(.)) gsub("^\\d+", "", team) else 
        if ("club" %in% names(.)) gsub("^\\d+", "", club) else NA_character_,
      games_played = if ("played" %in% names(.)) as.numeric(played) else
        if ("pl" %in% names(.)) as.numeric(pl) else
          if ("p" %in% names(.)) as.numeric(p) else NA_real_,
      points = if ("points" %in% names(.)) as.numeric(points) else
        if ("pts" %in% names(.)) as.numeric(pts) else NA_real_,
      form = if ("form,_last_6_games,_oldest_first" %in% names(.)) `form,_last_6_games,_oldest_first` else
        if ("form" %in% names(.)) form else
          if ("last_5" %in% names(.)) last_5 else NA_character_
    ) %>%
    select(any_of(c("team", "games_played", "points", "form"))) %>%
    filter(!is.na(team))
  
  # Ensure team is character (belt and suspenders)
  result$team <- as.character(result$team)
  
  # Calculate form points (last 6 games)
  form_data <- calculate_betting_form_points(result$form)
  
  result <- result %>%
    mutate(
      team = stringr::str_trim(team),
      team_n = tolower(trimws(team)),
      league_name = league_name,
      last6_form_points = form_data$points,
      last6_form_games = form_data$games,
      # For last 13 - we need to calculate from season PPG (approximation)
      # BBC only provides last 6, so we'll estimate last 13 as weighted avg
      season_ppg = if_else(games_played > 0, points / games_played, NA_real_),
      last6_ppg = if_else(last6_form_games > 0, last6_form_points / last6_form_games, NA_real_)
    ) %>%
    # Estimate last 13 PPG as weighted average of season and last 6
    # If games_played >= 13, use: (points - (points - last6_form_points if we had 7 earlier games))
    # Simplified: use weighted combo
    mutate(
      last13_ppg = case_when(
        games_played >= 13 ~ (last6_ppg * 6 + season_ppg * 7) / 13,  # Rough estimate
        games_played >= 6 ~ last6_ppg,  # Use last 6 if < 13 games
        TRUE ~ season_ppg  # Use season if < 6 games
      )
    ) %>%
    select(team, games_played, points, season_ppg, last13_ppg, last6_ppg, 
           last6_form_points, last6_form_games, team_n, league_name)
  
  log_debug("BBC Scraper: Retrieved", nrow(result), "teams from", league_name)
  return(result)
}

#' Calculate points from form string (W=3, D=1, L=0)
calculate_betting_form_points <- function(form_string) {
  points_vector <- numeric(length(form_string))
  games_vector <- numeric(length(form_string))
  
  for (i in seq_along(form_string)) {
    f <- form_string[i]
    
    if (is.na(f) || f == "") {
      points_vector[i] <- NA_real_
      games_vector[i] <- NA_real_
      next
    }
    
    # Count wins, draws, losses from BBC format
    wins <- stringr::str_count(f, "WResult Win|^W|\\sW")
    draws <- stringr::str_count(f, "DResult Draw|^D|\\sD")
    losses <- stringr::str_count(f, "LResult Loss|^L|\\sL")
    
    total_games <- wins + draws + losses
    points <- (wins * 3) + (draws * 1)
    
    points_vector[i] <- points
    games_vector[i] <- total_games
  }
  
  return(list(points = points_vector, games = games_vector))
}

#' Return empty standings table structure
empty_betting_standings <- function(league_name) {
  tibble(
    team = character(),
    games_played = numeric(),
    points = numeric(),
    season_ppg = numeric(),
    last13_ppg = numeric(),
    last6_ppg = numeric(),
    last6_form_points = numeric(),
    last6_form_games = numeric(),
    team_n = character(),
    league_name = character()
  )
}

# =============================================================================
# ODDS API
# =============================================================================

#' Fetch odds for a single league (7-day window)
#' @param slug The Odds API sport slug
#' @param league_name Display name for the league
#' @return Tibble with odds data
fetch_betting_league_odds <- function(slug, league_name) {
  url <- glue::glue("https://api.the-odds-api.com/v4/sports/{slug}/odds")
  
  date_from <- format(Sys.Date(), "%Y-%m-%dT00:00:00Z")
  date_to <- format(Sys.Date() + 7, "%Y-%m-%dT23:59:59Z")
  
  log_debug("Odds API: Fetching", league_name, "(7-day window)")
  
  resp <- tryCatch({
    httr::GET(url, query = list(
      apiKey     = BETTING_API_KEY,
      regions    = "uk,eu",
      markets    = "h2h",
      oddsFormat = "decimal",
      dateFormat = "iso",
      commenceTimeFrom = date_from,
      commenceTimeTo = date_to
    ))
  }, error = function(e) {
    log_debug("Odds API: Request failed -", e$message, level = "WARN")
    return(NULL)
  })
  
  if (is.null(resp) || httr::status_code(resp) != 200) {
    log_debug("Odds API: Failed for", slug, level = "WARN")
    return(empty_betting_odds(league_name))
  }
  
  raw_bytes <- httr::content(resp, as = "raw")
  raw_text <- rawToChar(raw_bytes)
  Encoding(raw_text) <- "UTF-8"
  
  data_list <- tryCatch({
    jsonlite::fromJSON(raw_text, simplifyVector = FALSE)
  }, error = function(e) {
    log_debug("Odds API: JSON parse failed -", e$message, level = "WARN")
    list()
  })
  
  if (length(data_list) == 0) {
    return(empty_betting_odds(league_name))
  }
  
  result <- purrr::map_df(data_list, function(m) {
    if (is.null(m$id) || is.null(m$commence_time) || 
        is.null(m$home_team) || is.null(m$away_team)) {
      return(NULL)
    }
    
    odd_home <- NA_real_
    odd_draw <- NA_real_
    odd_away <- NA_real_
    
    if (length(m$bookmakers) > 0 && 
        length(m$bookmakers[[1]]$markets) > 0 && 
        length(m$bookmakers[[1]]$markets[[1]]$outcomes) > 0) {
      
      outcomes <- m$bookmakers[[1]]$markets[[1]]$outcomes
      for (outcome in outcomes) {
        if (outcome$name == m$home_team) {
          odd_home <- outcome$price
        } else if (outcome$name == m$away_team) {
          odd_away <- outcome$price
        } else if (tolower(outcome$name) == "draw") {
          odd_draw <- outcome$price
        }
      }
    }
    
    tibble(
      match_id = m$id,
      DateTime = lubridate::as_datetime(m$commence_time),
      home_team = m$home_team,
      away_team = m$away_team,
      odd_home = odd_home,
      odd_draw = odd_draw,
      odd_away = odd_away,
      league_name = league_name
    )
  })
  
  log_debug("Odds API: Found", nrow(result), "matches for", league_name)
  return(result)
}

#' Return empty odds table structure
empty_betting_odds <- function(league_name) {
  tibble(
    match_id = character(),
    DateTime = as.POSIXct(character()),
    home_team = character(),
    away_team = character(),
    odd_home = numeric(),
    odd_draw = numeric(),
    odd_away = numeric(),
    league_name = character()
  )
}

# =============================================================================
# CACHE MANAGEMENT
# =============================================================================

#' Check if cached betting data exists and is recent
#' @return Cache data or NULL
check_betting_cache <- function() {
  if (file.exists(BETTING_CACHE_FILE)) {
    tryCatch({
      cache <- readRDS(BETTING_CACHE_FILE)
      hours_old <- as.numeric(difftime(Sys.time(), cache$timestamp, units = "hours"))
      
      if (hours_old < BETTING_CACHE_MAX_AGE_HOURS) {
        log_debug("Betting Cache: Using cached data (", round(hours_old, 1), "h old)")
        return(cache)
      } else {
        log_debug("Betting Cache: Expired (", round(hours_old, 1), "h old)")
      }
    }, error = function(e) {
      log_debug("Betting Cache: Read error -", e$message, level = "WARN")
      return(NULL)
    })
  }
  return(NULL)
}

#' Save betting data to cache
#' @param odds_data Odds dataframe
#' @param standings_data Standings dataframe
#' @param timestamp Cache timestamp
save_betting_cache <- function(odds_data, standings_data, timestamp) {
  # Ensure cache directory exists
  cache_dir <- dirname(BETTING_CACHE_FILE)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  tryCatch({
    saveRDS(list(
      odds = odds_data,
      standings = standings_data,
      timestamp = timestamp
    ), file = BETTING_CACHE_FILE)
    log_debug("Betting Cache: Saved successfully")
    return(TRUE)
  }, error = function(e) {
    log_debug("Betting Cache: Save error -", e$message, level = "WARN")
    return(FALSE)
  })
}

# =============================================================================
# MAIN DATA FETCH FUNCTIONS
# =============================================================================

#' Fetch all betting data (odds + standings)
#' @param selected_leagues Vector of league names to fetch
#' @param force_refresh If TRUE, bypass cache
#' @return List with odds and standings dataframes
fetch_betting_data <- function(selected_leagues = NULL, force_refresh = FALSE) {
  # Default to Premier League if not specified
  if (is.null(selected_leagues)) {
    selected_leagues <- BETTING_DEFAULT_LEAGUES
  }
  
  # Check cache first
  if (!force_refresh) {
    cache <- check_betting_cache()
    if (!is.null(cache)) {
      # Filter to selected leagues
      return(list(
        odds = cache$odds %>% filter(league_name %in% selected_leagues),
        standings = cache$standings %>% filter(league_name %in% selected_leagues),
        timestamp = cache$timestamp
      ))
    }
  }
  
  log_debug("Betting Data: Fetching fresh data for", length(selected_leagues), "leagues")
  
  all_odds <- list()
  all_standings <- list()
  
  for (league_name in names(BETTING_LEAGUES)) {
    league_info <- BETTING_LEAGUES[[league_name]]
    
    # Fetch odds
    odds <- fetch_betting_league_odds(league_info$slug, league_name)
    all_odds[[league_name]] <- odds
    
    Sys.sleep(BETTING_API_CALL_DELAY)
    
    # Fetch standings from BBC
    standings <- tryCatch({
      st <- scrape_betting_bbc_table(league_info$bbc_url, league_name)
      st <- apply_betting_team_mappings(st)
      
      # Validate column types before adding to list
      if (nrow(st) > 0) {
        st$team <- as.character(st$team)
        st$team_n <- as.character(st$team_n)
        st$league_name <- as.character(st$league_name)
      }
      st
    }, error = function(e) {
      log_debug("Betting Data: Standings error for", league_name, "-", e$message, level = "WARN")
      empty_betting_standings(league_name)
    })
    
    all_standings[[league_name]] <- standings
    
    Sys.sleep(0.5)  # Be nice to BBC
  }
  
  # Safely bind rows with type validation
  odds_data <- tryCatch({
    dplyr::bind_rows(all_odds)
  }, error = function(e) {
    log_debug("Betting Data: Error binding odds -", e$message, level = "ERROR")
    # Return only successful leagues
    valid_odds <- Filter(function(x) nrow(x) > 0, all_odds)
    if (length(valid_odds) > 0) {
      dplyr::bind_rows(valid_odds[1])  # At least return first valid
    } else {
      empty_betting_odds("Unknown")
    }
  })
  
  standings_data <- tryCatch({
    # Filter out any standings with list columns before binding
    valid_standings <- lapply(all_standings, function(st) {
      if (is.null(st) || nrow(st) == 0) return(empty_betting_standings(""))
      # Force all columns to atomic types
      st$team <- as.character(st$team)
      st$team_n <- as.character(st$team_n)
      st$league_name <- as.character(st$league_name)
      st
    })
    dplyr::bind_rows(valid_standings)
  }, error = function(e) {
    log_debug("Betting Data: Error binding standings -", e$message, level = "ERROR")
    # Return empty standings on error
    empty_betting_standings("Unknown")
  })
  
  timestamp <- Sys.time()
  
  # Save to cache
  save_betting_cache(odds_data, standings_data, timestamp)
  
  # Return filtered to selected leagues
  return(list(
    odds = odds_data %>% filter(league_name %in% selected_leagues),
    standings = standings_data %>% filter(league_name %in% selected_leagues),
    timestamp = timestamp
  ))
}

# =============================================================================
# TABLE PREPARATION
# =============================================================================

#' Prepare the betting odds table with PPG differentials
#' @param odds_data Odds dataframe
#' @param standings_data Standings dataframe
#' @return Tibble ready for display
prepare_betting_table <- function(odds_data, standings_data) {
  if (nrow(odds_data) == 0) {
    return(tibble(
      Team = character(),
      Opponent = character(),
      `Win Odds` = numeric(),
      `Season Diff` = numeric(),
      `Last 13 Diff` = numeric(),
      `Last 6 Diff` = numeric()
    ))
  }
  
  # Normalize team names
  odds_norm <- odds_data %>%
    mutate(
      home_n = tolower(trimws(home_team)),
      away_n = tolower(trimws(away_team))
    )
  
  # Join home team standings
  if (nrow(standings_data) > 0) {
    home_joined <- fuzzyjoin::stringdist_left_join(
      odds_norm,
      standings_data %>% select(team_n, season_ppg, last13_ppg, last6_ppg),
      by = c("home_n" = "team_n"),
      max_dist = 2,
      distance_col = "dist_home"
    ) %>%
      group_by(match_id) %>%
      slice_min(dist_home, with_ties = FALSE) %>%
      ungroup() %>%
      rename(
        home_season_ppg = season_ppg,
        home_last13_ppg = last13_ppg,
        home_last6_ppg = last6_ppg
      ) %>%
      select(-team_n, -dist_home)
    
    # Join away team standings
    full_joined <- fuzzyjoin::stringdist_left_join(
      home_joined,
      standings_data %>% select(team_n, season_ppg, last13_ppg, last6_ppg),
      by = c("away_n" = "team_n"),
      max_dist = 2,
      distance_col = "dist_away"
    ) %>%
      group_by(match_id) %>%
      slice_min(dist_away, with_ties = FALSE) %>%
      ungroup() %>%
      rename(
        away_season_ppg = season_ppg,
        away_last13_ppg = last13_ppg,
        away_last6_ppg = last6_ppg
      ) %>%
      select(-team_n, -dist_away)
  } else {
    full_joined <- odds_norm %>%
      mutate(
        home_season_ppg = NA_real_, away_season_ppg = NA_real_,
        home_last13_ppg = NA_real_, away_last13_ppg = NA_real_,
        home_last6_ppg = NA_real_, away_last6_ppg = NA_real_
      )
  }
  
  # Create two rows per match (one for each team)
  home_rows <- full_joined %>%
    transmute(
      DateTime = DateTime,
      League = league_name,
      Team = home_team,
      Opponent = away_team,
      `Win Odds` = odd_home,
      `Season Diff` = round(home_season_ppg - away_season_ppg, 2),
      `Last 13 Diff` = round(home_last13_ppg - away_last13_ppg, 2),
      `Last 6 Diff` = round(home_last6_ppg - away_last6_ppg, 2),
      is_home = TRUE
    )
  
  away_rows <- full_joined %>%
    transmute(
      DateTime = DateTime,
      League = league_name,
      Team = away_team,
      Opponent = home_team,
      `Win Odds` = odd_away,
      `Season Diff` = round(away_season_ppg - home_season_ppg, 2),
      `Last 13 Diff` = round(away_last13_ppg - home_last13_ppg, 2),
      `Last 6 Diff` = round(away_last6_ppg - home_last6_ppg, 2),
      is_home = FALSE
    )
  
  # Combine and sort
  result <- bind_rows(home_rows, away_rows) %>%
    arrange(DateTime, Team) %>%
    select(-DateTime)
  
  return(result)
}