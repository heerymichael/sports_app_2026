################################################################################
# FOOTBALL ODDS SCRAPER
# 
# Downloads betting odds data from football-data.co.uk and writes to Google Sheets
# Run this weekly to build up historical odds for regression analysis
#
# DATA SOURCE:
#   https://www.football-data.co.uk/englandm.php
#   Direct CSV: https://www.football-data.co.uk/mmz4281/2526/E0.csv (2025-26 season)
#
# OUTPUT:
#   Google Sheet with pre-match odds per fixture:
#   - Clean sheet probability (derived from odds)
#   - Implied goals for/against (derived from over/under odds)
#   - 1X2 probabilities
#
# USAGE:
#   Run weekly before the gameweek kicks off to capture pre-match odds
#   The script is incremental - only adds new fixtures not already in the sheet
#
################################################################################

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(googlesheets4)

################################################################################
# CONFIGURATION
################################################################################
# Season configuration - UPDATE THIS EACH SEASON
CURRENT_SEASON <- "2526"  # Format: 2526 for 2025-26 season
SEASON_DISPLAY <- "2025-26"

# Football-data.co.uk URL pattern
# E0 = Premier League, E1 = Championship, etc.
ODDS_URL <- sprintf("https://www.football-data.co.uk/mmz4281/%s/E0.csv", CURRENT_SEASON)

# Google Sheet configuration - same sheet as FanTeam data
GOOGLE_SHEET_ID <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"
ODDS_SHEET_NAME <- "match_odds"

# Team name mapping: football-data.co.uk -> FanTeam names
# Update this if team names don't match between sources
TEAM_NAME_MAP <- c(
  "Man United" = "Manchester United",
  "Man City" = "Manchester City",
  "Spurs" = "Spurs",
  "Tottenham" = "Spurs",
  "Newcastle" = "Newcastle",
  "West Ham" = "West Ham",
  "Wolves" = "Wolves",
  "Nott'm Forest" = "Nottingham Forest",
  "Nottingham Forest" = "Nottingham Forest",
  "Sheffield United" = "Sheffield United",
  "Sheffield Utd" = "Sheffield United",
  "Brighton" = "Brighton",
  "Crystal Palace" = "Crystal Palace",
  "Leicester" = "Leicester",
  "Leeds" = "Leeds",
  "Leeds United" = "Leeds"
)

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Convert decimal odds to implied probability
#' @param odds Decimal odds (e.g., 2.5)
#' @return Implied probability (0-1)
odds_to_prob <- function(odds) {
  if (is.na(odds) || odds <= 0) return(NA_real_)
  1 / odds
}

#' Normalize team name to match FanTeam naming
#' @param team Team name from football-data.co.uk
#' @return Normalized team name
normalize_team_name <- function(team) {
  if (team %in% names(TEAM_NAME_MAP)) {
    return(TEAM_NAME_MAP[team])
  }
  return(team)
}

#' Calculate implied goals from over/under 2.5 odds
#' 
#' Uses a simplified Poisson assumption:
#' P(over 2.5) ≈ 1 - P(0) - P(1) - P(2) where goals ~ Poisson(λ)
#' 
#' This is an approximation - we solve for λ numerically
#' 
#' @param over_odds Decimal odds for over 2.5 goals
#' @param under_odds Decimal odds for under 2.5 goals
#' @return Estimated total match goals (λ for both teams combined)
calculate_implied_total_goals <- function(over_odds, under_odds) {
  if (is.na(over_odds) || is.na(under_odds)) return(NA_real_)
  
  # Convert to probabilities (removing vig by normalizing)
  over_prob_raw <- odds_to_prob(over_odds)
  under_prob_raw <- odds_to_prob(under_odds)
  
  if (is.na(over_prob_raw) || is.na(under_prob_raw)) return(NA_real_)
  
  # Normalize to remove bookmaker margin
  total_prob <- over_prob_raw + under_prob_raw
  over_prob <- over_prob_raw / total_prob
  
  # For Poisson with mean λ:
  # P(X <= 2) = exp(-λ) * (1 + λ + λ²/2)
  # P(X > 2) = 1 - P(X <= 2) = over_prob
  # 
  # Solve numerically for λ
  target_over <- over_prob
  
  # Binary search for λ
  lambda_low <- 0.5
  lambda_high <- 6.0
  
  for (i in 1:50) {
    lambda_mid <- (lambda_low + lambda_high) / 2
    # P(X > 2) for Poisson(lambda_mid)
    p_under <- ppois(2, lambda_mid)
    p_over <- 1 - p_under
    
    if (abs(p_over - target_over) < 0.001) {
      return(lambda_mid)
    }
    
    if (p_over < target_over) {
      lambda_low <- lambda_mid
    } else {
      lambda_high <- lambda_mid
    }
  }
  
  return((lambda_low + lambda_high) / 2)
}

#' Calculate implied goals for each team from 1X2 odds and total goals
#' 
#' Uses the relationship between win probability and goal difference
#' Combined with total goals to estimate each team's expected goals
#' 
#' @param home_odds Decimal odds for home win
#' @param draw_odds Decimal odds for draw
#' @param away_odds Decimal odds for away win
#' @param total_goals Implied total match goals
#' @return List with home_goals and away_goals
calculate_team_goals <- function(home_odds, draw_odds, away_odds, total_goals) {
  if (is.na(total_goals) || is.na(home_odds) || is.na(away_odds)) {
    return(list(home_goals = NA_real_, away_goals = NA_real_))
  }
  
  # Convert to probabilities and normalize
  home_prob_raw <- odds_to_prob(home_odds)
  draw_prob_raw <- odds_to_prob(draw_odds)
  away_prob_raw <- odds_to_prob(away_odds)
  
  total_prob <- home_prob_raw + draw_prob_raw + away_prob_raw
  home_prob <- home_prob_raw / total_prob
  away_prob <- away_prob_raw / total_prob
  
  # Approximate: favorite gets more of the goals
  # Simple heuristic: split total goals based on win probability ratio
  home_share <- home_prob / (home_prob + away_prob)
  away_share <- 1 - home_share
  
  # Adjust for typical home advantage (~0.3 goals)
  # and regression toward 50/50 split
  home_share_adj <- 0.5 + (home_share - 0.5) * 0.8
  
  home_goals <- total_goals * home_share_adj
  away_goals <- total_goals * (1 - home_share_adj)
  
  return(list(home_goals = home_goals, away_goals = away_goals))
}

#' Calculate clean sheet probability from opponent's implied goals
#' 
#' P(CS) ≈ P(opponent scores 0) = exp(-opponent_goals) for Poisson
#' 
#' @param opponent_goals Opponent's implied goals
#' @return Clean sheet probability (0-1)
calculate_cs_prob <- function(opponent_goals) {
  if (is.na(opponent_goals) || opponent_goals < 0) return(NA_real_)
  # P(X = 0) for Poisson(λ) = exp(-λ)
  exp(-opponent_goals)
}

################################################################################
# MAIN FUNCTIONS
################################################################################

#' Download and parse odds data from football-data.co.uk
#' @return Data frame with match odds
download_odds_data <- function() {
  message(sprintf("Downloading odds data from football-data.co.uk..."))
  message(sprintf("URL: %s", ODDS_URL))
  
  tryCatch({
    # Read CSV directly from URL
    raw_data <- read_csv(ODDS_URL, show_col_types = FALSE)
    
    message(sprintf("✓ Downloaded %d matches", nrow(raw_data)))
    
    # Check available columns
    message(sprintf("  Available columns: %s", 
                    paste(names(raw_data)[1:min(15, length(names(raw_data)))], collapse = ", ")))
    
    return(raw_data)
    
  }, error = function(e) {
    message(sprintf("✗ Error downloading data: %s", e$message))
    return(NULL)
  })
}

#' Process raw odds data into analysis-ready format
#' @param raw_data Raw data from football-data.co.uk
#' @return Processed data frame
process_odds_data <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(NULL)
  }
  
  message("Processing odds data...")
  
  # Select and rename relevant columns
  # Try different column name patterns as they vary slightly
  
  # Identify available odds columns
  has_b365 <- all(c("B365H", "B365D", "B365A") %in% names(raw_data))
  has_avg <- all(c("AvgH", "AvgD", "AvgA") %in% names(raw_data))
  has_pinnacle <- all(c("PSH", "PSD", "PSA") %in% names(raw_data))
  
  # Identify over/under columns
  ou_cols <- names(raw_data)[grepl(">2.5|<2.5|Over|Under", names(raw_data), ignore.case = TRUE)]
  message(sprintf("  Over/Under columns found: %s", paste(ou_cols, collapse = ", ")))
  
  # Start building processed data
  processed <- raw_data %>%
    select(
      # Core match info
      date = Date,
      home_team = HomeTeam,
      away_team = AwayTeam,
      # Actual results (for later validation)
      home_goals_actual = FTHG,
      away_goals_actual = FTAG,
      result = FTR,
      # We'll add odds columns dynamically
      everything()
    )
  
  # Add 1X2 odds - prefer Pinnacle (sharp), then Bet365, then Average
  if (has_pinnacle) {
    processed <- processed %>%
      mutate(
        home_win_odds = PSH,
        draw_odds = PSD,
        away_win_odds = PSA
      )
    message("  Using Pinnacle odds for 1X2")
  } else if (has_b365) {
    processed <- processed %>%
      mutate(
        home_win_odds = B365H,
        draw_odds = B365D,
        away_win_odds = B365A
      )
    message("  Using Bet365 odds for 1X2")
  } else if (has_avg) {
    processed <- processed %>%
      mutate(
        home_win_odds = AvgH,
        draw_odds = AvgD,
        away_win_odds = AvgA
      )
    message("  Using Average odds for 1X2")
  }
  
  # Add over/under odds
  if ("B365>2.5" %in% names(raw_data) && "B365<2.5" %in% names(raw_data)) {
    processed <- processed %>%
      mutate(
        over_2_5_odds = `B365>2.5`,
        under_2_5_odds = `B365<2.5`
      )
    message("  Using Bet365 odds for Over/Under 2.5")
  } else if ("Avg>2.5" %in% names(raw_data) && "Avg<2.5" %in% names(raw_data)) {
    processed <- processed %>%
      mutate(
        over_2_5_odds = `Avg>2.5`,
        under_2_5_odds = `Avg<2.5`
      )
    message("  Using Average odds for Over/Under 2.5")
  }
  
  # Calculate derived metrics
  processed <- processed %>%
    rowwise() %>%
    mutate(
      # Normalize team names
      home_team = normalize_team_name(home_team),
      away_team = normalize_team_name(away_team),
      
      # Parse date
      match_date = dmy(date),
      
      # 1X2 probabilities (normalized)
      home_win_prob = {
        total <- odds_to_prob(home_win_odds) + odds_to_prob(draw_odds) + odds_to_prob(away_win_odds)
        odds_to_prob(home_win_odds) / total
      },
      draw_prob = {
        total <- odds_to_prob(home_win_odds) + odds_to_prob(draw_odds) + odds_to_prob(away_win_odds)
        odds_to_prob(draw_odds) / total
      },
      away_win_prob = {
        total <- odds_to_prob(home_win_odds) + odds_to_prob(draw_odds) + odds_to_prob(away_win_odds)
        odds_to_prob(away_win_odds) / total
      },
      
      # Implied total goals
      implied_total_goals = calculate_implied_total_goals(over_2_5_odds, under_2_5_odds),
      
      # Team-specific implied goals
      team_goals_calc = list(calculate_team_goals(home_win_odds, draw_odds, away_win_odds, implied_total_goals)),
      home_implied_goals = team_goals_calc$home_goals,
      away_implied_goals = team_goals_calc$away_goals,
      
      # Clean sheet probabilities
      home_cs_prob = calculate_cs_prob(away_implied_goals),  # Home CS = away team scores 0
      away_cs_prob = calculate_cs_prob(home_implied_goals),  # Away CS = home team scores 0
      
      # Convert to percentages for easier reading
      home_cs_pct = round(home_cs_prob * 100, 1),
      away_cs_pct = round(away_cs_prob * 100, 1),
      home_win_pct = round(home_win_prob * 100, 1),
      away_win_pct = round(away_win_prob * 100, 1),
      draw_pct = round(draw_prob * 100, 1)
    ) %>%
    ungroup() %>%
    select(-team_goals_calc)  # Remove temporary list column
  
  # Select final columns for output
  output <- processed %>%
    select(
      match_date,
      home_team,
      away_team,
      # Derived metrics (key for regression)
      home_implied_goals,
      away_implied_goals,
      home_cs_pct,
      away_cs_pct,
      implied_total_goals,
      # Win probabilities
      home_win_pct,
      draw_pct,
      away_win_pct,
      # Raw odds (for reference)
      home_win_odds,
      draw_odds,
      away_win_odds,
      over_2_5_odds,
      under_2_5_odds,
      # Actual results (for validation after matches complete)
      home_goals_actual,
      away_goals_actual,
      result
    ) %>%
    arrange(match_date)
  
  # Round numeric columns
  output <- output %>%
    mutate(
      across(c(home_implied_goals, away_implied_goals, implied_total_goals), ~round(., 2))
    )
  
  message(sprintf("✓ Processed %d matches", nrow(output)))
  
  return(output)
}

#' Create team-centric view (one row per team per match)
#' This format is easier to join with FanTeam player data
#' @param match_data Processed match odds data
#' @return Data frame with one row per team per match
create_team_view <- function(match_data) {
  if (is.null(match_data) || nrow(match_data) == 0) {
    return(NULL)
  }
  
  message("Creating team-centric view...")
  
  # Home team rows
  home_rows <- match_data %>%
    transmute(
      match_date,
      team = home_team,
      opponent = away_team,
      venue = "Home",
      implied_goals_for = home_implied_goals,
      implied_goals_against = away_implied_goals,
      clean_sheet_pct = home_cs_pct,
      win_pct = home_win_pct,
      implied_total_goals,
      # Actual results
      goals_scored_actual = home_goals_actual,
      goals_conceded_actual = away_goals_actual,
      result_actual = case_when(
        result == "H" ~ "Win",
        result == "D" ~ "Draw",
        result == "A" ~ "Loss",
        TRUE ~ NA_character_
      ),
      clean_sheet_actual = (away_goals_actual == 0)
    )
  
  # Away team rows
  away_rows <- match_data %>%
    transmute(
      match_date,
      team = away_team,
      opponent = home_team,
      venue = "Away",
      implied_goals_for = away_implied_goals,
      implied_goals_against = home_implied_goals,
      clean_sheet_pct = away_cs_pct,
      win_pct = away_win_pct,
      implied_total_goals,
      # Actual results
      goals_scored_actual = away_goals_actual,
      goals_conceded_actual = home_goals_actual,
      result_actual = case_when(
        result == "A" ~ "Win",
        result == "D" ~ "Draw",
        result == "H" ~ "Loss",
        TRUE ~ NA_character_
      ),
      clean_sheet_actual = (home_goals_actual == 0)
    )
  
  # Combine and sort
  team_view <- bind_rows(home_rows, away_rows) %>%
    arrange(match_date, team)
  
  # Add season and scrape metadata
  team_view <- team_view %>%
    mutate(
      season = SEASON_DISPLAY,
      scrape_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  
  message(sprintf("✓ Created %d team-match rows", nrow(team_view)))
  
  return(team_view)
}

#' Get existing matches from Google Sheet to avoid duplicates
#' @param sheet_id Google Sheet ID
#' @param sheet_name Worksheet name
#' @return Vector of existing match identifiers (date_home_away)
get_existing_matches <- function(sheet_id, sheet_name) {
  tryCatch({
    existing <- read_sheet(sheet_id, sheet = sheet_name, col_types = "c")
    
    if (nrow(existing) == 0) {
      return(character(0))
    }
    
    # Create match identifier
    match_ids <- paste(existing$match_date, existing$team, sep = "_")
    
    message(sprintf("  Found %d existing team-match records", length(match_ids)))
    return(match_ids)
    
  }, error = function(e) {
    message(sprintf("  Note: Could not read existing data (%s)", e$message))
    return(character(0))
  })
}

#' Write odds data to Google Sheet
#' @param data Data frame to write
#' @param sheet_id Google Sheet ID
#' @param sheet_name Worksheet name
#' @param incremental If TRUE, only add new matches
write_odds_to_sheet <- function(data, sheet_id, sheet_name, incremental = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    message("  No data to write")
    return(FALSE)
  }
  
  message(sprintf("Writing to Google Sheet: %s", sheet_name))
  
  tryCatch({
    # Check if sheet exists
    sheet_info <- gs4_get(sheet_id)
    existing_sheets <- sheet_info$sheets$name
    
    if (!sheet_name %in% existing_sheets) {
      # Create new sheet
      message(sprintf("  Creating new worksheet: %s", sheet_name))
      sheet_write(data, ss = sheet_id, sheet = sheet_name)
      message(sprintf("  ✓ Wrote %d rows", nrow(data)))
      return(TRUE)
    }
    
    if (incremental) {
      # Get existing matches
      existing_ids <- get_existing_matches(sheet_id, sheet_name)
      
      # Create match IDs for new data
      data <- data %>%
        mutate(match_id = paste(match_date, team, sep = "_"))
      
      # Filter to new matches only
      new_data <- data %>%
        filter(!match_id %in% existing_ids) %>%
        select(-match_id)
      
      if (nrow(new_data) == 0) {
        message("  ✓ No new matches to add (already up to date)")
        return(TRUE)
      }
      
      message(sprintf("  Adding %d new team-match records", nrow(new_data)))
      
      # Read existing to check column order
      existing_headers <- names(read_sheet(sheet_id, sheet = sheet_name, n_max = 1, col_types = "c"))
      
      # Reorder columns to match
      if (all(names(new_data) %in% existing_headers)) {
        new_data <- new_data[, existing_headers]
      }
      
      sheet_append(new_data, ss = sheet_id, sheet = sheet_name)
      
    } else {
      # Full replace
      message("  Replacing all data...")
      range_clear(ss = sheet_id, sheet = sheet_name)
      Sys.sleep(1)
      range_write(ss = sheet_id, sheet = sheet_name, data = data, range = "A1", col_names = TRUE)
    }
    
    message(sprintf("  ✓ Write successful"))
    return(TRUE)
    
  }, error = function(e) {
    message(sprintf("  ✗ Error: %s", e$message))
    return(FALSE)
  })
}

################################################################################
# MAIN EXECUTION
################################################################################

main <- function() {
  message("================================================================================")
  message("          FOOTBALL ODDS SCRAPER")
  message("================================================================================")
  message("")
  message(sprintf("📊 DATA SOURCE: football-data.co.uk (%s Premier League)", SEASON_DISPLAY))
  message(sprintf("   URL: %s", ODDS_URL))
  message("")
  message("📝 OUTPUT:")
  message(sprintf("   Google Sheet: https://docs.google.com/spreadsheets/d/%s/edit", GOOGLE_SHEET_ID))
  message(sprintf("   Worksheet: %s", ODDS_SHEET_NAME))
  message("")
  message("📈 DERIVED METRICS:")
  message("   - Implied goals for/against (from over/under odds)")
  message("   - Clean sheet probability (from implied goals)")
  message("   - Win/draw/loss probabilities (from 1X2 odds)")
  message("================================================================================")
  message("")
  
  # Authenticate with Google Sheets
  message("Authenticating with Google Sheets...")
  tryCatch({
    gs4_auth()
    message("✓ Authentication successful\n")
  }, error = function(e) {
    stop("Failed to authenticate with Google Sheets: ", e$message)
  })
  
  # Download raw data
  raw_data <- download_odds_data()
  
  if (is.null(raw_data)) {
    message("\n✗ Failed to download data. Exiting.")
    return(NULL)
  }
  
  # Process odds
  processed <- process_odds_data(raw_data)
  
  if (is.null(processed)) {
    message("\n✗ Failed to process data. Exiting.")
    return(NULL)
  }
  
  # Create team-centric view
  team_view <- create_team_view(processed)
  
  # Write to Google Sheets (incremental - only new matches)
  message("")
  write_odds_to_sheet(team_view, GOOGLE_SHEET_ID, ODDS_SHEET_NAME, incremental = TRUE)
  
  # Summary
  message("\n")
  message("================================================================================")
  message("                              SUMMARY")
  message("================================================================================")
  message(sprintf("Season: %s", SEASON_DISPLAY))
  message(sprintf("Matches in source: %d", nrow(processed)))
  message(sprintf("Team-match rows created: %d", nrow(team_view)))
  message("")
  
  # Show sample of latest matches
  message("Latest matches with odds:")
  latest <- team_view %>%
    filter(venue == "Home") %>%  # Show each match once
    arrange(desc(match_date)) %>%
    head(5) %>%
    select(match_date, team, opponent, implied_goals_for, clean_sheet_pct)
  
  print(as.data.frame(latest))
  
  message("")
  message(sprintf("Data saved to: https://docs.google.com/spreadsheets/d/%s/edit#gid=0", GOOGLE_SHEET_ID))
  message("================================================================================\n")
  
  return(list(
    raw = raw_data,
    processed = processed,
    team_view = team_view
  ))
}

################################################################################
# RUN
################################################################################

result <- main()