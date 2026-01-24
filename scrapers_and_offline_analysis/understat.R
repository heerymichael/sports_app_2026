################################################################################
#
#                         UNDERSTAT R MODULE
#
# A comprehensive R module for scraping data from understat.com
# Adapted from worldfootballR package patterns for long-term maintenance
#
# Author: [Your Name]
# Created: 2025-01
# 
# FUNCTIONS PROVIDED:
#   Core:
#     - .load_understat_page()     - Fetch and parse page
#     - .get_understat_json()      - Extract JSON from script tags
#     - .understat_shooting()      - Common shots extraction helper
#
#   League Level:
#     - understat_league_match_results()  - All match results for league/season
#     - understat_league_season_shots()   - All shots for league/season
#
#   Team Level:
#     - understat_available_teams()       - List available teams
#     - understat_team_meta()             - Team metadata and URLs
#     - understat_team_players_stats()    - Player stats for team season
#     - understat_team_season_shots()     - All shots for team season
#     - understat_team_stats_breakdown()  - Stats by situation/formation/etc
#
#   Match Level:
#     - understat_match_players()         - Player stats for a match
#     - understat_match_shots()           - Shot data for a match
#     - understat_match_stats()           - Match statistics table
#
#   Player Level:
#     - understat_player_shots()          - All shots for a player
#
# USAGE:
#   source("understat.R")
#   
#   # Get EPL match results
#   matches <- understat_league_match_results("EPL", 2025)
#   
#   # Get player stats for a match
#   players <- understat_match_players("https://understat.com/match/12345")
#
################################################################################

# =============================================================================
# DEPENDENCIES
# =============================================================================

if (!require("rvest", quietly = TRUE)) install.packages("rvest")
if (!require("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!require("stringi", quietly = TRUE)) install.packages("stringi")
if (!require("stringr", quietly = TRUE)) install.packages("stringr")
if (!require("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!require("httr", quietly = TRUE)) install.packages("httr")
if (!require("purrr", quietly = TRUE)) install.packages("purrr")
if (!require("readr", quietly = TRUE)) install.packages("readr")
if (!require("withr", quietly = TRUE)) install.packages("withr")

library(rvest)
library(dplyr)
library(stringi)
library(stringr)
library(jsonlite)
library(httr)
library(purrr)
library(readr)
library(withr)

# =============================================================================
# CONFIGURATION
# =============================================================================

# League code mappings (display name -> URL code)
UNDERSTAT_LEAGUES <- list(
  
  "EPL" = "EPL",
  "La liga" = "La_liga",
  "Bundesliga" = "Bundesliga",
  "Serie A" = "Serie_A",
  "Ligue 1" = "Ligue_1",
  "RFPL" = "RFPL"
)

# Default rate limiting
UNDERSTAT_RATE_LIMIT <- 3  # seconds between requests

# =============================================================================
# INTERNAL HELPER FUNCTIONS
# =============================================================================

#' Check if league name is valid
#' @param league League name to check
#' @keywords internal
.check_league_name <- function(league) {
  valid_leagues <- names(UNDERSTAT_LEAGUES)
  if (!league %in% valid_leagues) {
    stop(sprintf("Invalid league '%s'. Valid options: %s", 
                 league, paste(valid_leagues, collapse = ", ")))
  }
  invisible(TRUE)
}

#' Rate-limited page fetch with polite delay
#' @param url URL to fetch
#' @param wait_time Seconds to wait before fetching (default: UNDERSTAT_RATE_LIMIT)
#' @return rvest html document or NULL on error
#' @keywords internal
.load_understat_page <- function(url, wait_time = UNDERSTAT_RATE_LIMIT) {
  tryCatch({
    Sys.sleep(wait_time)
    
    response <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
      httr::timeout(30)
    )
    
    if (httr::status_code(response) != 200) {
      warning(sprintf("HTTP %d for %s", httr::status_code(response), url))
      return(NULL)
    }
    
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    return(rvest::read_html(content))
    
  }, error = function(e) {
    warning(sprintf("Failed to fetch %s: %s", url, e$message))
    return(NULL)
  })
}

#' Alias for .load_understat_page (matches worldfootballR naming)
#' @keywords internal
.load_page <- .load_understat_page

#' Get raw page HTML (returns the page object itself for further processing)
#' Used by functions that need to extract both JSON and HTML elements
#' @param page_url URL to fetch
#' @return rvest html document
#' @keywords internal
.get_understat_json <- function(page_url) {
  .load_understat_page(page_url)
}

#' Extract and parse JSON data embedded in Understat page scripts
#' @param page rvest html document OR page URL
#' @param script_name Name of the JavaScript variable (e.g., "rostersData", "datesData")
#' @return Parsed JSON as data.frame, or NULL if not found
#' @keywords internal
.get_clean_understat_json <- function(page_url, script_name) {
  tryCatch({
    # Fetch page if URL provided
    if (is.character(page_url)) {
      page <- .load_understat_page(page_url)
    } else {
      page <- page_url
    }
    
    if (is.null(page)) return(NULL)
    
    # Get all script tags as character strings
    scripts <- page %>%
      rvest::html_nodes("script") %>%
      as.character()
    
    # Find the script containing our variable
    target_script <- scripts %>%
      stringr::str_subset(script_name)
    
    if (length(target_script) == 0) {
      warning(sprintf("Script variable '%s' not found", script_name))
      return(NULL)
    }
    
    # Decode unicode escape sequences (e.g., \x22 -> ")
    decoded <- target_script %>%
      stringi::stri_unescape_unicode()
    
    # Remove the wrapper to get just the JSON
    # Handle various patterns:
    #   var datesData = JSON.parse('...');
    #   var rostersData	= JSON.parse('...');  (with tab)
    #   \tvar shotsData = JSON.parse('...');
    # Use a flexible pattern that handles tabs, spaces, and newlines
    start_pattern <- sprintf(".*var[\\s\\t]+%s[\\s\\t]*=[\\s\\t]*JSON\\.parse\\('", script_name)
    end_pattern <- "'\\).*"
    
    json_str <- decoded %>%
      gsub(start_pattern, "", ., perl = TRUE) %>%
      gsub(end_pattern, "", ., perl = TRUE)
    
    # Parse JSON
    data <- jsonlite::fromJSON(json_str, simplifyDataFrame = TRUE)
    
    return(data)
    
  }, error = function(e) {
    warning(sprintf("Could not extract %s: %s", script_name, e$message))
    return(NULL)
  })
}

#' Common shooting data extraction helper
#' Used by league, team, and match shot functions
#' @param type_url URL for the page containing shots data
#' @return data.frame of shot data
#' @keywords internal
.understat_shooting <- function(type_url) {
  shots_data <- .get_clean_understat_json(page_url = type_url, script_name = "shotsData")
  
  if (is.null(shots_data) || length(shots_data) == 0) {
    return(data.frame())
  }
  
  return(shots_data)
}

# =============================================================================
# LEAGUE LEVEL FUNCTIONS
# =============================================================================

#' Get Understat season match results
#'
#' Returns match results for all matches played in the selected league season
#'
#' @param league League name. Options: "EPL", "La liga", "Bundesliga", "Serie A", "Ligue 1", "RFPL"
#' @param season_start_year The year the season started (e.g., 2025 for 2025-26)
#'
#' @return data.frame of match results including xG values
#'
#' @examples
#' \dontrun{
#' epl_matches <- understat_league_match_results("EPL", 2025)
#' laliga_matches <- understat_league_match_results("La liga", 2024)
#' }
#'
#' @export
understat_league_match_results <- function(league, season_start_year) {
  .check_league_name(league)
  league_code <- UNDERSTAT_LEAGUES[[league]]
  
  league_url <- sprintf("https://understat.com/league/%s/%s", league_code, season_start_year)
  
  match_results <- .get_clean_understat_json(page_url = league_url, script_name = "datesData")
  
  if (is.null(match_results) || nrow(match_results) == 0) {
    warning("No match data found")
    return(data.frame())
  }
  
  # Filter to completed matches
  match_results <- match_results %>%
    dplyr::filter(.data[["isResult"]] == TRUE)
  
  # Add league column and rename nested columns
  match_results <- cbind(league = league, match_results)
  
  match_results <- match_results %>%
    dplyr::rename(
      match_id = .data[["id"]], 
      home_id = .data[["h.id"]], 
      home_team = .data[["h.title"]], 
      home_abbr = .data[["h.short_title"]], 
      away_id = .data[["a.id"]], 
      away_team = .data[["a.title"]], 
      away_abbr = .data[["a.short_title"]],
      home_goals = .data[["goals.h"]], 
      away_goals = .data[["goals.a"]], 
      home_xG = .data[["xG.h"]], 
      away_xG = .data[["xG.a"]],
      forecast_win = .data[["forecast.w"]], 
      forecast_draw = .data[["forecast.d"]], 
      forecast_loss = .data[["forecast.l"]]
    )
  
  match_results <- match_results %>%
    dplyr::mutate(dplyr::across(
      c("home_goals", "away_goals", "home_xG", "away_xG", 
        "forecast_win", "forecast_draw", "forecast_loss"), 
      as.numeric
    ))
  
  return(match_results)
}

#' Get Understat league season shot locations
#'
#' Returns shooting locations for all matches played in the selected league season
#'
#' @param league League name. Options: "EPL", "La liga", "Bundesliga", "Serie A", "Ligue 1", "RFPL"
#' @param season_start_year The year the season started
#'
#' @return data.frame of shot locations with xG values
#'
#' @examples
#' \dontrun{
#' epl_shots <- understat_league_season_shots("EPL", 2025)
#' }
#'
#' @export
understat_league_season_shots <- function(league, season_start_year) {
  .check_league_name(league)
  league_code <- UNDERSTAT_LEAGUES[[league]]
  
  league_url <- sprintf("https://understat.com/league/%s/%s", league_code, season_start_year)
  
  shots_data <- .understat_shooting(type_url = league_url)
  
  if (nrow(shots_data) == 0) {
    return(shots_data)
  }
  
  shots_data <- cbind(league = league, shots_data)
  
  shots_data <- shots_data %>%
    dplyr::rename(
      home_team = .data[["h_team"]], 
      away_team = .data[["a_team"]], 
      home_goals = .data[["h_goals"]], 
      away_goals = .data[["a_goals"]]
    ) %>%
    dplyr::mutate(dplyr::across(
      c("X", "Y", "xG", "home_goals", "away_goals"), 
      as.numeric
    ))
  
  return(shots_data)
}

# =============================================================================
# TEAM LEVEL FUNCTIONS
# =============================================================================

#' Get Understat available teams
#'
#' Returns all available team names for the selected leagues
#'
#' @param leagues Vector of league names to get teams for
#'
#' @return Character vector of team names (if single league) or named list
#'
#' @examples
#' \dontrun{
#' epl_teams <- understat_available_teams("EPL")
#' all_teams <- understat_available_teams(c("EPL", "La liga"))
#' }
#'
#' @export
understat_available_teams <- function(leagues) {
  teams_list <- list()
  
  for (lg in leagues) {
    if (!lg %in% names(UNDERSTAT_LEAGUES)) {
      warning(sprintf("League '%s' not found", lg))
      next
    }
    
    # Use a representative team URL to get the league's team list
    match_url <- switch(lg,
                        'EPL' = 'https://understat.com/team/Arsenal',
                        'La liga' = 'https://understat.com/team/Barcelona',
                        'Bundesliga' = 'https://understat.com/team/Bayern_Munich',
                        'Serie A' = 'https://understat.com/team/AC_Milan',
                        'Ligue 1' = 'https://understat.com/team/Paris_Saint_Germain',
                        'RFPL' = 'https://understat.com/team/Spartak_Moscow'
    )
    
    team_page <- tryCatch(.load_understat_page(match_url), error = function(e) NULL)
    
    if (is.null(team_page)) next
    
    teams <- team_page %>% 
      rvest::html_nodes(".header-wrapper") %>% 
      rvest::html_text() %>% 
      gsub("([\n\t])|(\\d{4}/\\d{4})", "", .) %>% 
      gsub('(?<!\\s)([[:upper:]])', '(&&)\\1', ., perl = TRUE) %>%
      strsplit("\\(&&\\)", perl = TRUE) %>% 
      unlist() %>% 
      .[2:length(.)]
    
    teams_list[[lg]] <- teams
  }
  
  if (length(leagues) == 1) {
    return(unlist(teams_list, use.names = FALSE))
  }
  
  return(teams_list)
}

#' Get Understat team metadata
#'
#' Retrieve team metadata including available seasons and URLs
#'
#' @param team_names Vector of team names
#'
#' @return data.frame with team_name, year, season, and url columns
#'
#' @examples
#' \dontrun{
#' meta <- understat_team_meta(c("Liverpool", "Manchester City"))
#' }
#'
#' @export
understat_team_meta <- function(team_names) {
  f_possibly <- purrr::possibly(.understat_team_meta_single, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(team_names, f_possibly)
}

#' @keywords internal
.understat_team_meta_single <- function(team_name) {
  main_url <- "https://understat.com"
  
  team_name_url <- stringr::str_replace_all(team_name, " ", "_")
  team_url <- paste(main_url, "team", team_name_url, sep = "/")
  
  team_page <- .load_understat_page(team_url)
  if (is.null(team_page)) return(data.frame())
  
  year_link <- rvest::html_nodes(team_page, "#header :nth-child(2)")
  year_options <- rvest::html_nodes(year_link[2], "option")
  
  team_df <- data.frame(
    team_name = team_name,
    year = as.numeric(rvest::html_attr(year_options, "value")),
    season = rvest::html_text(year_options),
    stringsAsFactors = FALSE
  )
  
  team_df$url <- paste(team_url, team_df$year, sep = "/")
  
  return(team_df)
}

#' Get Understat team player stats
#'
#' Retrieve player statistics for a team season
#'
#' @param team_url Team season URL (from understat_team_meta)
#'
#' @return data.frame of player statistics
#'
#' @examples
#' \dontrun{
#' stats <- understat_team_players_stats("https://understat.com/team/Liverpool/2025")
#' }
#'
#' @export
understat_team_players_stats <- function(team_url) {
  f_possibly <- purrr::possibly(.understat_team_players_stats_single, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(team_url, f_possibly)
}

#' @keywords internal
.understat_team_players_stats_single <- function(team_url) {
  players_data <- .get_clean_understat_json(team_url, "playersData")
  
  if (is.null(players_data) || nrow(players_data) == 0) {
    return(data.frame())
  }
  
  names(players_data)[names(players_data) == "team_title"] <- "team_name"
  names(players_data)[names(players_data) == "id"] <- "player_id"
  
  withr::local_options(list(readr.num_columns = 0))
  players_data <- readr::type_convert(players_data)
  
  return(players_data)
}

#' Get Understat team season shot locations
#'
#' Returns shooting locations for all matches played by a selected team
#'
#' @param team_url Team season URL
#'
#' @return data.frame of shot locations
#'
#' @examples
#' \dontrun{
#' shots <- understat_team_season_shots("https://understat.com/team/Liverpool/2025")
#' }
#'
#' @export
understat_team_season_shots <- function(team_url) {
  shots_df <- .understat_shooting(type_url = team_url)
  
  if (nrow(shots_df) == 0) {
    return(shots_df)
  }
  
  shots_df <- shots_df %>%
    dplyr::rename(
      home_away = .data[["h_a"]], 
      home_team = .data[["h_team"]], 
      away_team = .data[["a_team"]], 
      home_goals = .data[["h_goals"]], 
      away_goals = .data[["a_goals"]]
    ) %>%
    dplyr::mutate(dplyr::across(
      c("minute", "X", "Y", "xG", "home_goals", "away_goals"), 
      as.numeric
    ))
  
  return(shots_df)
}

#' Get Understat team statistics breakdowns
#'
#' Returns stats broken down by situation, formation, game state, timing, etc.
#'
#' @param team_urls Vector of team season URLs
#'
#' @return data.frame with all stat breakdowns
#'
#' @examples
#' \dontrun{
#' breakdown <- understat_team_stats_breakdown("https://understat.com/team/Liverpool/2025")
#' }
#'
#' @export
understat_team_stats_breakdown <- function(team_urls) {
  f_possibly <- purrr::possibly(.understat_team_stats_breakdown_single, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(team_urls, f_possibly)
}

#' @keywords internal
.understat_team_stats_breakdown_single <- function(team_url) {
  data_html <- .load_understat_page(team_url)
  
  if (is.null(data_html)) return(data.frame())
  
  # Get team name from breadcrumb
  team_name <- data_html %>% 
    rvest::html_nodes(".breadcrumb") %>% 
    rvest::html_nodes("li") %>% 
    .[3] %>% 
    rvest::html_text()
  
  # Get selected season
  season_element <- data_html %>% 
    rvest::html_nodes(xpath = '//*[@name="season"]') %>%
    rvest::html_nodes("option")
  season_element <- season_element[grep("selected", season_element)]
  season <- season_element %>% rvest::html_attr("value") %>% as.numeric()
  
  # Get statistics data
  scripts <- data_html %>%
    rvest::html_nodes("script") %>%
    as.character()
  
  stats_script <- scripts %>%
    stringr::str_subset("statisticsData")
  
  if (length(stats_script) == 0) {
    return(data.frame())
  }
  
  data_statistics <- stats_script %>%
    stringi::stri_unescape_unicode() %>%
    gsub(".*var statisticsData = JSON.parse\\('", "", .) %>%
    gsub("'\\);.*", "", .) %>%
    jsonlite::parse_json(simplifyVector = TRUE)
  
  # Parse each stat group
  .parse_stat_group <- function(stat_list, stat_group) {
    x <- stat_list[[stat_group]]
    if (is.null(x)) return(data.frame())
    
    all_names <- names(x)
    
    get_each_stat <- function(stat_name) {
      stat_vals <- x[[stat_name]] %>% 
        as.data.frame()
      
      out_df <- data.frame(
        team_name = team_name, 
        season_start_year = season, 
        stat_group_name = stat_group, 
        stat_name = stat_name, 
        stringsAsFactors = FALSE
      )
      out_df <- dplyr::bind_cols(out_df, stat_vals)
      out_df[, "stat"] <- NULL
      return(out_df)
    }
    
    purrr::map_dfr(all_names, get_each_stat)
  }
  
  stat_groups <- names(data_statistics)
  full_df <- purrr::map_dfr(stat_groups, ~.parse_stat_group(data_statistics, .x))
  
  return(full_df)
}

# =============================================================================
# MATCH LEVEL FUNCTIONS
# =============================================================================

#' Get Understat match player data
#'
#' Returns player values for a selected match
#'
#' @param match_url URL of the match page
#'
#' @return data.frame with data for all players in the match
#'
#' @examples
#' \dontrun{
#' players <- understat_match_players("https://understat.com/match/12345")
#' }
#'
#' @export
understat_match_players <- function(match_url) {
  match_id <- gsub("[^0-9]", "", match_url)
  
  page <- .load_understat_page(match_url)
  if (is.null(page)) return(NULL)
  
  # Get scripts and find rostersData
  scripts <- page %>%
    rvest::html_nodes("script") %>%
    as.character()
  
  # Find script with rostersData (note: may use tab character)
  roster_script <- scripts[grep("rostersData", scripts)]
  
  if (length(roster_script) == 0) {
    warning(sprintf("No rostersData found for match %s", match_id))
    return(NULL)
  }
  
  # Decode and extract - following worldfootballR pattern exactly
  match_player_data <- roster_script %>%
    stringi::stri_unescape_unicode() %>%
    substr(41, nchar(.)) %>%
    substr(0, nchar(.) - 13) %>%
    paste0('[', ., ']') %>%
    unlist() %>%
    stringr::str_subset("\\[\\]", negate = TRUE)
  
  match_player_data <- lapply(match_player_data, jsonlite::fromJSON) %>%
    do.call("rbind", .)
  
  # Process home and away
  match_player_data_home <- do.call(rbind.data.frame, match_player_data$h)
  match_player_data_away <- do.call(rbind.data.frame, match_player_data$a)
  
  match_player_data_rebind <- dplyr::bind_rows(match_player_data_home, match_player_data_away)
  
  # Create clean output
  match_players <- data.frame(
    match_id = as.integer(match_id),
    id = as.integer(match_player_data_rebind[["id"]]),
    team_id = as.integer(match_player_data_rebind[["team_id"]]),
    home_away = as.character(match_player_data_rebind[["h_a"]]),
    player_id = as.integer(match_player_data_rebind[["player_id"]]),
    player = as.character(match_player_data_rebind[["player"]]),
    position = as.character(match_player_data_rebind[["position"]]),
    positionOrder = as.integer(match_player_data_rebind[["positionOrder"]]),
    time_played = as.integer(match_player_data_rebind[["time"]]),
    goals = as.integer(match_player_data_rebind[["goals"]]),
    own_goals = as.integer(match_player_data_rebind[["own_goals"]]),
    shots = as.integer(match_player_data_rebind[["shots"]]),
    xG = as.numeric(match_player_data_rebind[["xG"]]),
    yellow_card = as.integer(match_player_data_rebind[["yellow_card"]]),
    red_card = as.integer(match_player_data_rebind[["red_card"]]),
    roster_in = as.integer(match_player_data_rebind[["roster_in"]]),
    roster_out = as.integer(match_player_data_rebind[["roster_out"]]),
    key_passes = as.integer(match_player_data_rebind[["key_passes"]]),
    assists = as.integer(match_player_data_rebind[["assists"]]),
    xA = as.numeric(match_player_data_rebind[["xA"]]),
    xGChain = as.numeric(match_player_data_rebind[["xGChain"]]),
    xGBuildup = as.numeric(match_player_data_rebind[["xGBuildup"]]),
    stringsAsFactors = FALSE
  )
  
  return(match_players)
}

#' Get Understat match shot locations
#'
#' Returns shooting locations for a selected match
#'
#' @param match_url URL of the match page
#'
#' @return data.frame of shot locations
#'
#' @examples
#' \dontrun{
#' shots <- understat_match_shots("https://understat.com/match/12345")
#' }
#'
#' @export
understat_match_shots <- function(match_url) {
  match_shots_df <- .get_clean_understat_json(page_url = match_url, script_name = "shotsData")
  
  if (is.null(match_shots_df) || nrow(match_shots_df) == 0) {
    return(data.frame())
  }
  
  match_shots_df <- match_shots_df %>%
    dplyr::rename(
      home_away = .data[["h_a"]], 
      home_team = .data[["h_team"]], 
      away_team = .data[["a_team"]], 
      home_goals = .data[["h_goals"]], 
      away_goals = .data[["a_goals"]]
    ) %>%
    dplyr::mutate(dplyr::across(
      c("minute", "X", "Y", "xG", "home_goals", "away_goals"), 
      as.numeric
    ))
  
  return(match_shots_df)
}

#' Get Understat match stats table data
#'
#' Returns the stats values from the match statistics table
#'
#' @param match_url URL of the match page
#'
#' @return data.frame with match statistics
#'
#' @details For draw_chances, home_chances and away_chances, values below 10% 
#' in the browser will be retrieved as NA
#'
#' @examples
#' \dontrun{
#' stats <- understat_match_stats("https://understat.com/match/12345")
#' }
#'
#' @export
understat_match_stats <- function(match_url) {
  page <- .load_understat_page(match_url)
  if (is.null(page)) return(NULL)
  
  match_stats <- page %>%
    rvest::html_elements("div.scheme-block.is-hide[data-scheme='stats']") %>%
    rvest::html_elements(".progress-value") %>%
    rvest::html_text()
  
  if (length(match_stats) < 20) {
    warning("Could not extract match stats")
    return(NULL)
  }
  
  away <- match_stats[seq(1, length(match_stats), by = 2)]
  home <- match_stats[seq(2, length(match_stats), by = 2)]
  
  match_stats_df <- data.frame(
    match_id = as.integer(gsub("[^0-9]", "", match_url)),
    
    home_team = as.character(away[1]),
    home_chances = as.integer(gsub("[^0-9]", "", away[2])) / 100,
    home_goals = as.integer(home[3]),
    home_xG = as.numeric(home[4]),
    home_shots = as.integer(home[5]),
    home_shot_on_target = as.integer(home[6]),
    home_deep = as.integer(home[7]),
    home_PPDA = as.numeric(home[8]),
    home_xPTS = as.numeric(home[9]),
    
    draw_chances = as.integer(gsub("[^0-9]", "", home[2])) / 100,
    
    away_team = home[1],
    away_chances = as.integer(gsub("[^0-9]", "", away[3])) / 100,
    away_goals = as.integer(away[4]),
    away_xG = as.numeric(away[5]),
    away_shots = as.integer(away[6]),
    away_shot_on_target = as.integer(away[7]),
    away_deep = as.integer(away[8]),
    away_PPDA = as.numeric(away[9]),
    away_xPTS = as.numeric(away[10]),
    
    stringsAsFactors = FALSE
  )
  
  return(match_stats_df)
}

# =============================================================================
# PLAYER LEVEL FUNCTIONS
# =============================================================================

#' Get all Understat shot locations for a player
#'
#' Returns shooting locations for a selected player for all matches played
#'
#' @param player_url URL of a player page (e.g., "https://understat.com/player/1234")
#'
#' @return data.frame of shot locations
#'
#' @examples
#' \dontrun{
#' shots <- understat_player_shots("https://understat.com/player/1234")
#' }
#'
#' @export
understat_player_shots <- function(player_url) {
  shots_df <- understat_match_shots(match_url = player_url)
  return(shots_df)
}

# =============================================================================
# UTILITY / TEST FUNCTIONS
# =============================================================================

#' Test the Understat scraper
#'
#' Runs a quick test to verify the scraper is working
#'
#' @param league League to test (default: "EPL")
#' @param season Season to test (default: 2024)
#'
#' @return List with test results
#'
#' @examples
#' \dontrun{
#' test_understat()
#' }
#'
#' @export
test_understat <- function(league = "EPL", season = 2024) {
  message("================================================================================")
  message("  TESTING UNDERSTAT MODULE")
  message("================================================================================\n")
  
  results <- list()
  league_code <- UNDERSTAT_LEAGUES[[league]]
  league_url <- sprintf("https://understat.com/league/%s/%s", league_code, season)
  
  # First, debug the page to see what's there
  message("0. Debugging page structure...")
  debug_info <- understat_debug(league_url)
  message("")
  
  # Test 1: League match results
  message("1. Testing understat_league_match_results()...")
  matches <- tryCatch({
    understat_league_match_results(league, season)
  }, error = function(e) {
    message(sprintf("   [ERROR] %s", e$message))
    NULL
  })
  
  if (!is.null(matches) && nrow(matches) > 0) {
    message(sprintf("   [OK] Found %d matches\n", nrow(matches)))
    results$matches <- matches
  } else {
    message("   [FAIL] Could not get match results\n")
    return(results)
  }
  
  # Test 2: Match players
  message("2. Testing understat_match_players()...")
  test_url <- sprintf("https://understat.com/match/%s", matches$match_id[1])
  
  players <- tryCatch({
    understat_match_players(test_url)
  }, error = function(e) {
    message(sprintf("   [ERROR] %s", e$message))
    NULL
  })
  
  if (!is.null(players) && nrow(players) > 0) {
    message(sprintf("   [OK] Found %d players\n", nrow(players)))
    results$players <- players
  } else {
    message("   [FAIL] Could not get player data\n")
  }
  
  # Test 3: Match shots
  message("3. Testing understat_match_shots()...")
  shots <- tryCatch({
    understat_match_shots(test_url)
  }, error = function(e) {
    message(sprintf("   [ERROR] %s", e$message))
    NULL
  })
  
  if (!is.null(shots) && nrow(shots) > 0) {
    message(sprintf("   [OK] Found %d shots\n", nrow(shots)))
    results$shots <- shots
  } else {
    message("   [FAIL] Could not get shot data\n")
  }
  
  message("================================================================================")
  message("  TEST COMPLETE")
  message("================================================================================")
  
  return(results)
}

#' Print module info
#' @export
understat_info <- function() {
  cat("
================================================================================
                          UNDERSTAT R MODULE
================================================================================

LEAGUE FUNCTIONS:
  understat_league_match_results(league, season_start_year)
  understat_league_season_shots(league, season_start_year)

TEAM FUNCTIONS:
  understat_available_teams(leagues)
  understat_team_meta(team_names)
  understat_team_players_stats(team_url)
  understat_team_season_shots(team_url)
  understat_team_stats_breakdown(team_urls)

MATCH FUNCTIONS:
  understat_match_players(match_url)
  understat_match_shots(match_url)
  understat_match_stats(match_url)

PLAYER FUNCTIONS:
  understat_player_shots(player_url)

AVAILABLE LEAGUES:
  'EPL', 'La liga', 'Bundesliga', 'Serie A', 'Ligue 1', 'RFPL'

EXAMPLES:
  # Get all EPL matches for 2025-26 season
  matches <- understat_league_match_results('EPL', 2025)
  
  # Get player stats for a match
  players <- understat_match_players('https://understat.com/match/12345')
  
  # Get team season stats
  meta <- understat_team_meta('Liverpool')
  stats <- understat_team_players_stats(meta$url[1])

================================================================================
")
}

#' Debug function to inspect page script tags
#' 
#' Use this to diagnose issues with JSON extraction
#'
#' @param url URL to inspect
#' @return List with debug info
#' @export
understat_debug <- function(url) {
  message(sprintf("Debugging: %s\n", url))
  
  page <- .load_understat_page(url, wait_time = 1)
  if (is.null(page)) {
    message("[ERROR] Could not fetch page")
    return(NULL)
  }
  
  # Get scripts
  scripts <- page %>%
    rvest::html_nodes("script") %>%
    as.character()
  
  message(sprintf("Found %d script tags\n", length(scripts)))
  
  # Known variable names to look for
  known_vars <- c("datesData", "rostersData", "shotsData", "playersData", 
                  "statisticsData", "match_info", "teamsData")
  
  found_vars <- list()
  
  for (i in seq_along(scripts)) {
    script <- scripts[[i]]
    script_len <- nchar(script)
    
    if (script_len > 100) {
      for (var_name in known_vars) {
        if (grepl(var_name, script)) {
          message(sprintf("Script %d (%d chars): contains '%s'", i, script_len, var_name))
          
          # Show a snippet around the variable definition
          match_pos <- regexpr(paste0("var[\\s\\t]+", var_name), script, perl = TRUE)
          if (match_pos > 0) {
            snippet_start <- max(1, match_pos - 10)
            snippet_end <- min(script_len, match_pos + 100)
            snippet <- substr(script, snippet_start, snippet_end)
            # Make whitespace visible
            snippet <- gsub("\t", "[TAB]", snippet)
            snippet <- gsub("\n", "[NL]", snippet)
            message(sprintf("  Snippet: %s...\n", snippet))
          }
          
          found_vars[[var_name]] <- i
        }
      }
    }
  }
  
  # Try to extract one of the found variables
  if (length(found_vars) > 0) {
    test_var <- names(found_vars)[1]
    message(sprintf("\nAttempting to extract '%s'...", test_var))
    
    result <- tryCatch({
      .get_clean_understat_json(page, test_var)
    }, error = function(e) {
      message(sprintf("[ERROR] %s", e$message))
      NULL
    })
    
    if (!is.null(result)) {
      if (is.data.frame(result)) {
        message(sprintf("[OK] Extracted data.frame with %d rows, %d cols", nrow(result), ncol(result)))
      } else if (is.list(result)) {
        message(sprintf("[OK] Extracted list with %d elements", length(result)))
      }
    }
  }
  
  return(list(
    scripts_count = length(scripts),
    found_vars = found_vars,
    page = page
  ))
}

message("Understat module loaded. Run understat_info() for help.")