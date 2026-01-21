# =============================================================================
# Module: Golf Season Long Management
# 
# Weekly roster management for FanTeam Season Long contest:
# - Load existing rosters from Google Sheets
# - Match players to weekly tournament projections
# - Recommend best 6 starters from each 10-man roster
# - Track transfers over time
# - TRANSFER PLANNING: Optimize transfers for next week
# 
# FanTeam Rules:
# - 10 players per roster (6 starters + 4 bench)
# - 100M budget
# - Captain (highest projection) = 1.25x points
# - Underdog (cheapest in lineup) = 1.25x points
# - 1 free transfer per gameweek (can bank up to 31)
# - Excess transfers cost -20 points each
# =============================================================================

library(googlesheets4)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Google Sheet IDs
GOLF_ROSTER_SHEET_ID <- "18aDz1kwgeJTqyDRxlwX3COl3gCmzetT6vuTao8l1JQE"
GOLF_PROJECTIONS_SHEET_ID <- "1yJJAOv5hzNZagYUG7FLpNmRIRC76L0fJNGPbzK61lbw"
GOLF_SALARIES_SHEET_ID <- "1_OgRCMmmyGQhLNsAFvK3wvlFIjkMPmvOddIr3PdYhwg"

# Contest configuration
GOLF_SEASON_MANAGEMENT_CONFIG <- list(
  roster_size = 10,
  active_lineup_size = 6,
  bench_size = 4,
  budget = 100,
  num_rosters = 5,
  free_transfers_per_week = 1,
  max_banked_transfers = 31,
  transfer_penalty = -20,
  captain_multiplier = 1.25,
  underdog_multiplier = 1.25
)

# Null coalesce operator
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# =============================================================================
# NAME CORRECTIONS (for matching roster names to projection names)
# =============================================================================

GOLF_MANAGEMENT_NAME_CORRECTIONS <- list(
  # Capitalization fixes
  "Robert Macintyre" = "Robert MacIntyre",
  "Maverick Mcnealy" = "Maverick McNealy",
  "Denny Mccarthy" = "Denny McCarthy",
  "Max Mcgreevy" = "Max McGreevy",
  
  # Name variants
  "Christopher Gotterup" = "Chris Gotterup",
  "Henry Lebioda" = "Hank Lebioda",
  "Kota Yuta Kaneko" = "Kota Kaneko",
  "Cam Davis" = "Cameron Davis",
  "Matt McCarty" = "Matthew McCarty",
  "Matthias Schmid" = "Matti Schmid",
  "Zach Bauchou" = "Zachary Bauchou",
  "Seong-Hyeon Kim" = "Seonghyeon Kim",
  "Byeong-Hun An" = "Byeong Hun An",
  "Sung-Jae Im" = "Sungjae Im",
  "Hao-Tong Li" = "Haotong Li",
  "Ze-Cheng Dou" = "Zecheng Dou",
  "Adrien Dumont" = "Adrien Dumont De Chassart",
  "Alex Noren" = "Alexander Noren",
  "Nico Echavarria" = "Nicolas Echavarria",
  "Kris Ventura" = "Kristoffer Ventura",
  "Jordan L Smith" = "Jordan Smith",
  "Sam Stevens" = "Samuel Stevens"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Normalize player name for matching
normalize_management_name <- function(name) {
  if (is.na(name) || name == "") return("")
  
  # Check direct correction first
  if (name %in% names(GOLF_MANAGEMENT_NAME_CORRECTIONS)) {
    name <- GOLF_MANAGEMENT_NAME_CORRECTIONS[[name]]
  }
  
  # Lowercase and clean
  name_lower <- tolower(trimws(name))
  
  # Check lowercase correction
  if (name_lower %in% tolower(names(GOLF_MANAGEMENT_NAME_CORRECTIONS))) {
    idx <- which(tolower(names(GOLF_MANAGEMENT_NAME_CORRECTIONS)) == name_lower)
    name <- GOLF_MANAGEMENT_NAME_CORRECTIONS[[idx]]
    name_lower <- tolower(name)
  }
  
  # Handle "LastName, FirstName" format
  if (grepl(",", name_lower)) {
    parts <- strsplit(name_lower, ",")[[1]]
    if (length(parts) == 2) {
      name_lower <- paste(trimws(parts[2]), trimws(parts[1]))
    }
  }
  
  # Remove special characters, normalize spaces
  name_lower <- gsub("[^a-z0-9 ]", " ", name_lower)
  name_lower <- gsub("\\s+", " ", name_lower)
  name_lower <- trimws(name_lower)
  
  return(name_lower)
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Get available weeks from roster sheet columns
get_roster_weeks <- function() {
  log_debug("get_roster_weeks() called", level = "DEBUG")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    # Read just the header row to get column names
    roster_data <- googlesheets4::read_sheet(
      GOLF_ROSTER_SHEET_ID,
      range = "1:1"
    )
    
    # Get columns that start with "Week"
    week_cols <- names(roster_data)[grepl("^Week", names(roster_data))]
    
    log_debug("Found week columns:", paste(week_cols, collapse = ", "), level = "INFO")
    return(week_cols)
    
  }, error = function(e) {
    log_debug("Error getting roster weeks:", e$message, level = "ERROR")
    return(c("Week 1"))
  })
}

#' Get available tournaments from projections sheet
get_projection_tournaments <- function() {
  log_debug("get_projection_tournaments() called", level = "DEBUG")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    ss <- googlesheets4::gs4_get(GOLF_PROJECTIONS_SHEET_ID)
    tournaments <- ss$sheets$name
    
    log_debug("Found tournaments:", paste(tournaments, collapse = ", "), level = "INFO")
    return(tournaments)
    
  }, error = function(e) {
    log_debug("Error getting tournaments:", e$message, level = "ERROR")
    return(character(0))
  })
}

#' Get available salary tournaments from salaries sheet
#' Returns all sheet names (tournament names like "American Express", "Farmers Insurance Open", etc.)
get_salary_tournaments <- function() {
  log_debug("get_salary_tournaments() called", level = "DEBUG")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    ss <- googlesheets4::gs4_get(GOLF_SALARIES_SHEET_ID)
    all_sheets <- ss$sheets$name
    
    log_debug("All salary sheets found:", paste(all_sheets, collapse = ", "), level = "DEBUG")
    
    # Filter out common metadata/config sheets
    tournament_sheets <- all_sheets[!grepl("^(template|readme|info|meta|config|ETR|test)$", all_sheets, ignore.case = TRUE)]
    
    log_debug("Found salary tournaments:", paste(tournament_sheets, collapse = ", "), level = "INFO")
    
    # Return empty message if none found
    if (length(tournament_sheets) == 0) {
      log_debug("No salary tournaments found", level = "WARN")
      return(c("No tournaments found" = ""))
    }
    
    return(tournament_sheets)
    
  }, error = function(e) {
    log_debug("Error getting salary tournaments:", e$message, level = "ERROR")
    return(c("Error loading" = ""))
  })
}

#' Load roster data from Google Sheet for a specific week
load_roster_data <- function(week_col = "Week 1") {
  log_debug("load_roster_data() for week:", week_col, level = "INFO")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    roster_data <- googlesheets4::read_sheet(GOLF_ROSTER_SHEET_ID) %>%
      as.data.frame()
    
    log_debug("Raw roster columns:", paste(names(roster_data), collapse = ", "), level = "DEBUG")
    
    # Check that required columns exist
    if (!"Roster" %in% names(roster_data)) {
      log_debug("Missing 'Roster' column", level = "ERROR")
      return(NULL)
    }
    
    if (!week_col %in% names(roster_data)) {
      log_debug("Missing week column:", week_col, level = "ERROR")
      return(NULL)
    }
    
    # Fill down the Roster column (since it only appears on first row of each group)
    current_roster <- NA
    for (i in 1:nrow(roster_data)) {
      if (!is.na(roster_data$Roster[i]) && roster_data$Roster[i] != "") {
        current_roster <- roster_data$Roster[i]
      } else {
        roster_data$Roster[i] <- current_roster
      }
    }
    
    # Select relevant columns and rename
    result <- roster_data %>%
      select(
        roster = Roster,
        slot = Slot,
        player_name = !!sym(week_col)
      ) %>%
      filter(!is.na(player_name) & player_name != "") %>%
      mutate(
        match_key = sapply(player_name, normalize_management_name)
      )
    
    log_debug("Loaded", nrow(result), "roster entries", level = "INFO")
    return(result)
    
  }, error = function(e) {
    log_debug("Error loading roster data:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load projections from Google Sheet for a specific tournament
load_tournament_projections <- function(tournament_name) {
  log_debug("load_tournament_projections() for:", tournament_name, level = "INFO")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    projections_raw <- googlesheets4::read_sheet(
      GOLF_PROJECTIONS_SHEET_ID,
      sheet = tournament_name
    ) %>%
      janitor::clean_names()
    
    log_debug("Raw projection columns:", paste(names(projections_raw), collapse = ", "), level = "DEBUG")
    
    # Map columns flexibly
    projections <- projections_raw
    
    # Find player name column
    name_col <- intersect(names(projections), c("golfer", "player_name", "player", "name"))
    if (length(name_col) > 0) {
      projections <- projections %>% rename(player_name = !!name_col[1])
    } else {
      log_debug("No player name column found", level = "ERROR")
      return(NULL)
    }
    
    # Find projection column (median/points)
    proj_col <- intersect(names(projections), c(
      "dk_points", "median", "projection", "proj", "pts", "fpts",
      "fantasy_points", "points", "projected_points"
    ))
    if (length(proj_col) > 0) {
      log_debug("Using projection column:", proj_col[1], level = "DEBUG")
      projections <- projections %>%
        mutate(projection = as.numeric(.data[[proj_col[1]]]))
    } else {
      log_debug("No projection column found", level = "WARN")
      projections$projection <- NA_real_
    }
    
    # Find salary column (for underdog calculation)
    salary_col <- intersect(names(projections), c(
      "salary", "dk_salary", "price", "sal", "cost"
    ))
    if (length(salary_col) > 0) {
      log_debug("Using salary column:", salary_col[1], level = "DEBUG")
      projections <- projections %>%
        mutate(salary = as.numeric(.data[[salary_col[1]]]))
    } else {
      log_debug("No salary column found - underdog bonus may not work correctly", level = "WARN")
      projections$salary <- NA_real_
    }
    
    # Find ownership column
    own_col <- intersect(names(projections), c(
      "dk_ownership", "ownership", "own", "own_large", "ownership_large"
    ))
    if (length(own_col) > 0) {
      projections <- projections %>%
        mutate(ownership = as.numeric(gsub("%", "", .data[[own_col[1]]])))
    } else {
      projections$ownership <- NA_real_
    }
    
    # Create match key
    projections <- projections %>%
      mutate(match_key = sapply(player_name, normalize_management_name)) %>%
      select(player_name, projection, salary, ownership, match_key) %>%
      filter(!is.na(projection))
    
    log_debug("Loaded", nrow(projections), "projections", level = "INFO")
    return(projections)
    
  }, error = function(e) {
    log_debug("Error loading projections:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load salaries from Google Sheet for a specific tournament
load_tournament_salaries <- function(tournament_name) {
  log_debug("load_tournament_salaries() for:", tournament_name, level = "INFO")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    salaries_raw <- googlesheets4::read_sheet(
      GOLF_SALARIES_SHEET_ID,
      sheet = tournament_name
    ) %>%
      janitor::clean_names()
    
    log_debug("Raw salary columns:", paste(names(salaries_raw), collapse = ", "), level = "DEBUG")
    log_debug("Raw salary rows:", nrow(salaries_raw), level = "DEBUG")
    
    # Check for FanTeam format (f_name + name) or simple format
    if (all(c("f_name", "name") %in% names(salaries_raw))) {
      # FanTeam format - DO NOT FILTER on lineup/availability columns
      # Include ALL players regardless of tournament status
      salaries <- salaries_raw %>%
        mutate(
          player_name = paste0(f_name, " ", name),
          salary = as.numeric(price)
        )
      log_debug("FanTeam format detected, loaded ALL players (no lineup filter)", level = "INFO")
    } else {
      # Simple format
      name_col <- intersect(names(salaries_raw), c("player_name", "player", "golfer", "name"))
      price_col <- intersect(names(salaries_raw), c("salary", "price", "sal", "cost"))
      
      if (length(name_col) == 0 || length(price_col) == 0) {
        log_debug("Could not find name or price columns", level = "ERROR")
        return(NULL)
      }
      
      salaries <- salaries_raw %>%
        rename(player_name = !!name_col[1], salary = !!price_col[1]) %>%
        mutate(salary = as.numeric(salary))
    }
    
    # Create match key - keep ALL players with valid salary
    salaries <- salaries %>%
      mutate(match_key = sapply(player_name, normalize_management_name)) %>%
      select(player_name, salary, match_key) %>%
      filter(!is.na(salary) & !is.na(player_name) & player_name != "")
    
    log_debug("Loaded", nrow(salaries), "player salaries (all players, no availability filter)", level = "INFO")
    return(salaries)
    
  }, error = function(e) {
    log_debug("Error loading salaries:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Match roster players to projections
match_roster_to_projections <- function(roster_data, projections) {
  log_debug("match_roster_to_projections() called", level = "DEBUG")
  
  if (is.null(roster_data) || is.null(projections)) return(NULL)
  
  # Join on match_key
  matched <- roster_data %>%
    left_join(
      projections %>% select(match_key, projection, salary, ownership, proj_name = player_name),
      by = "match_key"
    )
  
  # Log unmatched players
  unmatched <- matched %>% filter(is.na(projection)) %>% pull(player_name)
  if (length(unmatched) > 0) {
    log_debug("Unmatched players:", paste(unmatched, collapse = ", "), level = "WARN")
  }
  
  # Log salary availability
  has_salary <- sum(!is.na(matched$salary))
  log_debug("Players with salary data:", has_salary, "of", nrow(matched), level = "INFO")
  
  matched_count <- sum(!is.na(matched$projection))
  log_debug("Matched", matched_count, "of", nrow(matched), "players", level = "INFO")
  
  return(matched)
}

#' Match roster to salaries (for budget calculation)
match_roster_to_salaries <- function(roster_data, salaries) {
  log_debug("match_roster_to_salaries() called", level = "DEBUG")
  
  if (is.null(roster_data) || is.null(salaries)) return(NULL)
  
  matched <- roster_data %>%
    left_join(
      salaries %>% select(match_key, salary_value = salary, sal_name = player_name),
      by = "match_key"
    )
  
  # Log unmatched
  unmatched <- matched %>% filter(is.na(salary_value)) %>% pull(player_name)
  if (length(unmatched) > 0) {
    log_debug("Players without salary data:", paste(unmatched, collapse = ", "), level = "WARN")
  }
  
  return(matched)
}

#' Select optimal 6 players from a roster considering Captain AND Underdog bonuses
#' Captain = highest projection in lineup (1.25x) - always assigned to top projected
#' Underdog = cheapest salary in lineup (1.25x)
#' Uses brute force over all C(n,6) combinations to find true optimal
select_best_six <- function(roster_players) {
  
  # Separate matched and unmatched players
  matched_players <- roster_players %>% filter(!is.na(projection))
  unmatched_players <- roster_players %>% filter(is.na(projection))
  
  n_matched <- nrow(matched_players)
  
  # If 6 or fewer matched players, they're all starters by default
  if (n_matched <= 6) {
    max_proj <- max(matched_players$projection, na.rm = TRUE)
    min_sal <- if ("salary" %in% names(matched_players) && sum(!is.na(matched_players$salary)) > 0) {
      min(matched_players$salary, na.rm = TRUE)
    } else {
      NA_real_
    }
    
    result <- matched_players %>%
      arrange(desc(projection)) %>%
      mutate(
        lineup_rank = row_number(),
        is_starter = TRUE,
        is_captain = projection == max_proj,
        is_underdog = !is.na(min_sal) & !is.na(salary) & salary == min_sal & !is_captain,
        effective_projection = case_when(
          is_captain ~ projection * 1.25,
          is_underdog ~ projection * 1.25,
          TRUE ~ projection
        ),
        is_unmatched = FALSE
      )
    
    # Add unmatched players at the bottom
    if (nrow(unmatched_players) > 0) {
      unmatched_result <- unmatched_players %>%
        mutate(
          lineup_rank = n_matched + row_number(),
          is_starter = FALSE,
          is_captain = FALSE,
          is_underdog = FALSE,
          effective_projection = NA_real_,
          is_unmatched = TRUE
        )
      result <- bind_rows(result, unmatched_result)
    }
    
    return(result)
  }
  
  # Check salary column exists and has data
  has_salary <- "salary" %in% names(matched_players) && sum(!is.na(matched_players$salary)) > 0
  
  if (!has_salary) {
    # Fallback to simple projection-based selection if no salary data
    matched_players <- matched_players %>%
      arrange(desc(projection)) %>%
      mutate(
        lineup_rank = row_number(),
        is_starter = row_number() <= 6,
        is_captain = row_number() == 1,
        is_underdog = FALSE,
        effective_projection = if_else(is_captain, projection * 1.25, projection),
        is_unmatched = FALSE
      )
    
    if (nrow(unmatched_players) > 0) {
      unmatched_result <- unmatched_players %>%
        mutate(
          lineup_rank = n_matched + row_number(),
          is_starter = FALSE,
          is_captain = FALSE,
          is_underdog = FALSE,
          effective_projection = NA_real_,
          is_unmatched = TRUE
        )
      matched_players <- bind_rows(matched_players, unmatched_result)
    }
    
    return(matched_players)
  }
  
  # Full optimization with salary data
  # Generate all combinations of 6 players from n
  combos <- utils::combn(1:n_matched, 6)
  
  best_total <- -Inf
  best_combo <- NULL
  
  for (i in 1:ncol(combos)) {
    lineup_idx <- combos[, i]
    lineup <- matched_players[lineup_idx, ]
    
    # Captain = highest projection
    max_proj <- max(lineup$projection)
    captain_idx <- which(lineup$projection == max_proj)[1]
    
    # Underdog = cheapest in lineup
    min_sal <- min(lineup$salary, na.rm = TRUE)
    underdog_candidates <- which(lineup$salary == min_sal)
    # Underdog can't be captain
    underdog_idx <- underdog_candidates[underdog_candidates != captain_idx][1]
    if (is.na(underdog_idx)) underdog_idx <- NULL
    
    # Calculate effective total
    total <- sum(lineup$projection)
    total <- total + lineup$projection[captain_idx] * 0.25  # Captain bonus
    if (!is.null(underdog_idx)) {
      total <- total + lineup$projection[underdog_idx] * 0.25  # Underdog bonus
    }
    
    if (total > best_total) {
      best_total <- total
      best_combo <- list(
        lineup_idx = lineup_idx,
        captain_idx = captain_idx,
        underdog_idx = underdog_idx
      )
    }
  }
  
  # Build result
  result <- matched_players %>%
    mutate(
      is_starter = row_number() %in% best_combo$lineup_idx,
      is_captain = FALSE,
      is_underdog = FALSE,
      is_unmatched = FALSE
    )
  
  # Mark captain and underdog
  starter_rows <- which(result$is_starter)
  result$is_captain[starter_rows[best_combo$captain_idx]] <- TRUE
  if (!is.null(best_combo$underdog_idx)) {
    result$is_underdog[starter_rows[best_combo$underdog_idx]] <- TRUE
  }
  
  # Calculate effective projections
  result <- result %>%
    mutate(
      effective_projection = case_when(
        is_captain ~ projection * 1.25,
        is_underdog ~ projection * 1.25,
        TRUE ~ projection
      )
    ) %>%
    arrange(desc(is_starter), desc(effective_projection)) %>%
    mutate(lineup_rank = row_number())
  
  # Add unmatched players at the bottom
  if (nrow(unmatched_players) > 0) {
    unmatched_result <- unmatched_players %>%
      mutate(
        lineup_rank = n_matched + row_number(),
        is_starter = FALSE,
        is_captain = FALSE,
        is_underdog = FALSE,
        effective_projection = NA_real_,
        is_unmatched = TRUE
      )
    result <- bind_rows(result, unmatched_result)
  }
  
  return(result)
}

#' Calculate total lineup projection for a roster
#' Used in transfer optimization
#' Players with projection = 0 are valid (not in tournament) but won't be selected as starters
calculate_lineup_projection <- function(roster_players) {
  # Select best 6 and calculate total
  if (nrow(roster_players) == 0) {
    return(list(total = NA_real_, details = NULL))
  }
  
  # Get players with valid projections (including 0 for players not in tournament)
  # Filter only NA projections, not 0
  best <- roster_players %>%
    filter(!is.na(projection)) %>%
    arrange(desc(projection))
  
  if (nrow(best) < 6) {
    # Not enough players with projections - this shouldn't happen with a full roster
    log_debug(sprintf("calculate_lineup_projection: Only %d players with projections", nrow(best)), level = "WARN")
    return(list(total = NA_real_, details = NULL))
  }
  
  # Take top 6 by projection (players with 0 projection will be at bottom)
  lineup <- best[1:6, ]
  
  # Check if we have any players with actual projections
  if (sum(lineup$projection > 0) == 0) {
    log_debug("calculate_lineup_projection: All 6 starters have 0 projection", level = "WARN")
    return(list(total = 0, details = NULL))
  }
  
  # Assign captain (highest projection) and underdog (cheapest)
  max_proj <- max(lineup$projection)
  captain_idx <- which(lineup$projection == max_proj)[1]
  
  has_salary <- "salary" %in% names(lineup) && sum(!is.na(lineup$salary) & lineup$salary > 0) > 0
  underdog_idx <- NULL
  
  if (has_salary) {
    # For underdog, only consider players with actual salary > 0
    salary_for_underdog <- lineup$salary
    salary_for_underdog[is.na(salary_for_underdog) | salary_for_underdog == 0] <- Inf
    min_sal <- min(salary_for_underdog)
    if (is.finite(min_sal)) {
      underdog_candidates <- which(lineup$salary == min_sal)
      underdog_candidates <- underdog_candidates[underdog_candidates != captain_idx]
      if (length(underdog_candidates) > 0) {
        underdog_idx <- underdog_candidates[1]
      }
    }
  }
  
  # Calculate total with bonuses
  base_total <- sum(lineup$projection)
  captain_bonus <- lineup$projection[captain_idx] * 0.25
  underdog_bonus <- if (!is.null(underdog_idx)) lineup$projection[underdog_idx] * 0.25 else 0
  
  total <- base_total + captain_bonus + underdog_bonus
  
  return(list(
    total = total,
    base = base_total,
    captain_bonus = captain_bonus,
    underdog_bonus = underdog_bonus,
    captain_player = lineup$player_name[captain_idx],
    underdog_player = if (!is.null(underdog_idx)) lineup$player_name[underdog_idx] else NA
  ))
}

#' Find optimal transfers for a single roster with specific transfer count
#' @param roster_players Data frame with current roster (player_name, salary, projection, match_key)
#' @param available_pool Data frame with all available players (player_name, salary, projection, match_key)
#' @param current_budget Remaining budget after selling current team at new prices
#' @param n_transfers Exact number of transfers to evaluate
#' @param free_transfers Number of free transfers available
#' @return Best transfer recommendation for exactly n_transfers
find_optimal_transfers_n <- function(roster_players, available_pool, current_budget, 
                                     n_transfers = 1, free_transfers = 1) {
  
  log_debug(sprintf("find_optimal_transfers_n() - n=%d, budget=%.1f, free=%d", 
                    n_transfers, current_budget, free_transfers), level = "DEBUG")
  
  config <- GOLF_SEASON_MANAGEMENT_CONFIG
  penalty <- config$transfer_penalty
  
  # Current lineup projection
  current_proj <- calculate_lineup_projection(roster_players)$total
  
  # Debug: show roster player projections sorted
  player_projs <- roster_players %>% 
    arrange(desc(projection)) %>%
    mutate(info = sprintf("%s: $%.1f, %.1f pts", player_name, salary, projection))
  
  # Find the 6th best projection (minimum needed to make starting lineup)
  sorted_projs <- sort(roster_players$projection, decreasing = TRUE)
  min_starter_proj <- if (length(sorted_projs) >= 6) sorted_projs[6] else 0
  
  log_debug(sprintf("Current roster (sorted by proj): %s", paste(head(player_projs$info, 10), collapse = " | ")), level = "DEBUG")
  log_debug(sprintf("Current lineup projection: %.1f | Min starter projection: %.1f", current_proj, min_starter_proj), level = "DEBUG")
  
  # Show how many available players could actually improve the lineup
  better_than_min <- available_pool %>% filter(projection > min_starter_proj)
  log_debug(sprintf("Players in pool with proj > %.1f (min starter): %d", min_starter_proj, nrow(better_than_min)), level = "DEBUG")
  
  if (nrow(better_than_min) > 0) {
    top_better <- better_than_min %>% arrange(desc(projection)) %>% head(5)
    log_debug(sprintf("Top 5 who could improve: %s", 
                      paste(sprintf("%s $%.1f %.1f pts", top_better$player_name, top_better$salary, top_better$projection), collapse = ", ")), 
              level = "DEBUG")
  }
  
  if (is.na(current_proj)) {
    log_debug("Cannot calculate current projection - returning NULL", level = "WARN")
    return(NULL)
  }
  
  # Get players NOT on roster for transfers in
  roster_keys <- roster_players$match_key
  transfer_pool <- available_pool %>%
    filter(!match_key %in% roster_keys) %>%
    filter(!is.na(projection) & !is.na(salary))
  
  if (nrow(transfer_pool) == 0) {
    log_debug("No players available for transfer", level = "WARN")
    return(NULL)
  }
  
  # Calculate penalty for this transfer count
  excess_transfers <- max(0, n_transfers - free_transfers)
  total_penalty <- excess_transfers * penalty
  
  best_net_gain <- -Inf
  best_transfer <- NULL
  
  # Get roster indices
  n_roster <- nrow(roster_players)
  
  if (n_transfers == 1) {
    # Single transfer: evaluate all combinations
    log_debug("Evaluating 1 transfer - checking all roster slots", level = "DEBUG")
    
    # Show current lineup info
    starters <- roster_players %>% arrange(desc(projection)) %>% head(6)
    min_starter_proj <- min(starters$projection)
    log_debug(sprintf("Current starters (min proj %.0f): %s", min_starter_proj,
                      paste(sprintf("%s(%.0f)", starters$player_name, starters$projection), collapse = ", ")), 
              level = "DEBUG")
    
    transfers_evaluated <- 0
    transfers_improved <- 0
    
    for (i in 1:n_roster) {
      player_out <- roster_players[i, ]
      budget_after_sale <- current_budget + player_out$salary
      
      # Find players we can afford
      affordable <- transfer_pool %>%
        filter(salary <= budget_after_sale) %>%
        arrange(desc(projection))
      
      if (nrow(affordable) == 0) {
        log_debug(sprintf("No affordable players when selling %s ($%.1f, %.0f pts)", 
                          player_out$player_name, player_out$salary, player_out$projection), level = "DEBUG")
        next
      }
      
      # Try top candidates
      for (j in 1:min(nrow(affordable), 30)) {
        player_in <- affordable[j, ]
        transfers_evaluated <- transfers_evaluated + 1
        
        # Build new roster
        new_roster <- roster_players[-i, ] %>%
          bind_rows(player_in %>% select(player_name, salary, projection, match_key))
        
        new_proj <- calculate_lineup_projection(new_roster)$total
        
        if (is.na(new_proj)) next
        
        gross_gain <- new_proj - current_proj
        net_gain <- gross_gain + total_penalty
        
        # Log significant improvements
        if (net_gain > 0 && transfers_improved < 5) {
          transfers_improved <- transfers_improved + 1
          log_debug(sprintf("Beneficial: %s(%.0f)→%s(%.0f) | cur=%.0f new=%.0f net=+%.1f", 
                            player_out$player_name, player_out$projection,
                            player_in$player_name, player_in$projection,
                            current_proj, new_proj, net_gain), level = "DEBUG")
        }
        
        if (net_gain > best_net_gain) {
          best_net_gain <- net_gain
          best_transfer <- list(
            out = list(player_out),
            in_ = list(player_in),
            current_proj = current_proj,
            new_proj = new_proj,
            gross_gain = gross_gain,
            penalty = total_penalty,
            net_gain = net_gain,
            budget_used = player_in$salary - player_out$salary
          )
        }
      }
    }
    
    log_debug(sprintf("1-transfer: evaluated %d swaps, %d would improve lineup", 
                      transfers_evaluated, transfers_improved), level = "DEBUG")
    
  } else if (n_transfers == 2) {
    # Double transfer: try ALL combinations of outgoing players
    log_debug("Evaluating 2 transfers - trying ALL out-player combinations", level = "DEBUG")
    
    # Show current starters
    starters <- roster_players %>% arrange(desc(projection)) %>% head(6)
    min_starter_proj <- min(starters$projection)
    log_debug(sprintf("Current starters (min proj %.0f): %s", min_starter_proj,
                      paste(sprintf("%s(%.0f)", starters$player_name, starters$projection), collapse = ", ")), 
              level = "DEBUG")
    
    bench <- roster_players %>% arrange(desc(projection)) %>% tail(4)
    log_debug(sprintf("Current bench: %s", 
                      paste(sprintf("%s($%.1f/%.0f)", bench$player_name, bench$salary, bench$projection), collapse = ", ")), 
              level = "DEBUG")
    
    # Get ALL pairs of players to swap out - no prioritization, try everything
    out_combos <- combn(1:n_roster, 2, simplify = FALSE)
    
    log_debug(sprintf("Trying ALL %d out-player pairs (including high-value players)", length(out_combos)), level = "DEBUG")
    
    # Get top incoming candidates - need to consider we might be selling expensive players
    max_possible_budget <- current_budget + sum(sort(roster_players$salary, decreasing = TRUE)[1:2])
    
    in_candidates <- transfer_pool %>%
      filter(salary <= max_possible_budget) %>%
      arrange(desc(projection)) %>%
      head(50)  # More candidates since we might have bigger budget
    
    log_debug(sprintf("Max possible budget (if selling 2 most expensive): $%.1f", max_possible_budget), level = "DEBUG")
    log_debug(sprintf("Top incoming candidates (%d players): %s", nrow(in_candidates),
                      paste(head(sprintf("%s($%.1f/%.0f)", in_candidates$player_name, in_candidates$salary, in_candidates$projection), 8), collapse = ", ")), 
              level = "DEBUG")
    
    combos_tried <- 0
    combos_with_valid_in <- 0
    
    for (out_idx in out_combos) {
      combos_tried <- combos_tried + 1
      
      budget_after_sales <- current_budget + sum(roster_players$salary[out_idx])
      
      # Try multiple combinations of incoming players
      affordable_in <- in_candidates %>% filter(salary <= budget_after_sales)
      
      if (nrow(affordable_in) < 2) next
      
      # Try pairs of incoming players - more combinations for high-budget scenarios
      max_in_to_try <- min(nrow(affordable_in), 25)
      in_pairs <- combn(1:max_in_to_try, 2, simplify = FALSE)
      
      for (in_idx in in_pairs) {
        player_in_1 <- affordable_in[in_idx[1], ]
        player_in_2 <- affordable_in[in_idx[2], ]
        
        # Check budget
        total_in_cost <- player_in_1$salary + player_in_2$salary
        if (total_in_cost > budget_after_sales) next
        
        combos_with_valid_in <- combos_with_valid_in + 1
        
        # Build new roster
        new_roster <- roster_players[-out_idx, ]
        new_roster <- bind_rows(new_roster, 
                                player_in_1 %>% select(player_name, salary, projection, match_key),
                                player_in_2 %>% select(player_name, salary, projection, match_key))
        
        new_proj <- calculate_lineup_projection(new_roster)$total
        
        if (is.na(new_proj)) next
        
        gross_gain <- new_proj - current_proj
        net_gain <- gross_gain + total_penalty
        budget_change <- total_in_cost - sum(roster_players$salary[out_idx])
        
        # Log first few valid attempts AND any that beat current best
        if (combos_with_valid_in <= 5 || (net_gain > best_net_gain && net_gain > 0)) {
          out_names <- paste(roster_players$player_name[out_idx], collapse = "+")
          in_names <- paste(c(player_in_1$player_name, player_in_2$player_name), collapse = "+")
          log_debug(sprintf("2-transfer combo: OUT[%s] IN[%s] | cur=%.0f new=%.0f gross=%.1f pen=%.0f net=%.1f%s", 
                            out_names, in_names, current_proj, new_proj, gross_gain, total_penalty, net_gain,
                            if(net_gain > best_net_gain && net_gain > 0) " *** NEW BEST ***" else ""), 
                    level = "DEBUG")
        }
        
        if (net_gain > best_net_gain) {
          best_net_gain <- net_gain
          best_transfer <- list(
            out = list(roster_players[out_idx[1], ], roster_players[out_idx[2], ]),
            in_ = list(player_in_1, player_in_2),
            current_proj = current_proj,
            new_proj = new_proj,
            gross_gain = gross_gain,
            penalty = total_penalty,
            net_gain = net_gain,
            budget_used = budget_change
          )
        }
      }
    }
    
    log_debug(sprintf("2-transfer: tried %d out-pairs, %d total in-combos evaluated, best_net=%.1f", 
                      combos_tried, combos_with_valid_in, best_net_gain), level = "DEBUG")
    
    if (is.null(best_transfer) && best_net_gain <= 0) {
      log_debug("No beneficial 2-transfer found - all combinations result in same or worse lineup", level = "INFO")
    }
    
  } else {
    # 3+ transfers: try ALL combinations of both outgoing AND incoming players
    log_debug(sprintf("Evaluating %d transfers - exhaustive search", n_transfers), level = "DEBUG")
    
    # Show current starters (top 6 by projection)
    starters <- roster_players %>% arrange(desc(projection)) %>% head(6)
    min_starter_proj <- min(starters$projection)
    log_debug(sprintf("Current starters (min proj %.0f): %s", min_starter_proj,
                      paste(sprintf("%s(%.0f)", starters$player_name, starters$projection), collapse = ", ")), 
              level = "DEBUG")
    
    bench <- roster_players %>% arrange(desc(projection)) %>% tail(4)
    log_debug(sprintf("Current bench: %s", 
                      paste(sprintf("%s($%.1f/%.0f)", bench$player_name, bench$salary, bench$projection), collapse = ", ")), 
              level = "DEBUG")
    
    # Get ALL combinations of outgoing players
    out_combos <- combn(1:n_roster, n_transfers, simplify = FALSE)
    
    log_debug(sprintf("Trying ALL %d out-player combinations", length(out_combos)), level = "DEBUG")
    
    # Get top incoming candidates - need enough for combinations
    max_possible_budget <- current_budget + sum(sort(roster_players$salary, decreasing = TRUE)[1:n_transfers])
    
    # For combinations, we need more candidates but not too many (computational limits)
    n_candidates <- switch(as.character(n_transfers),
                           "3" = 25,  # C(25,3) = 2,300 combos per out-combo
                           "4" = 20,  # C(20,4) = 4,845 combos per out-combo  
                           "5" = 15,  # C(15,5) = 3,003 combos per out-combo
                           20)        # default
    
    in_candidates <- transfer_pool %>%
      filter(salary <= max_possible_budget) %>%
      arrange(desc(projection)) %>%
      head(n_candidates)
    
    log_debug(sprintf("Max possible budget: $%.1f | Top %d incoming candidates", 
                      max_possible_budget, nrow(in_candidates)), level = "DEBUG")
    
    if (nrow(in_candidates) < n_transfers) {
      log_debug(sprintf("Not enough affordable candidates: need %d, have %d", 
                        n_transfers, nrow(in_candidates)), level = "WARN")
    } else {
      # Pre-generate all incoming combinations
      in_combos <- combn(1:nrow(in_candidates), n_transfers, simplify = FALSE)
      
      # Pre-calculate total salary for each incoming combo for faster filtering
      in_combo_salaries <- sapply(in_combos, function(idx) sum(in_candidates$salary[idx]))
      in_combo_projs <- sapply(in_combos, function(idx) sum(in_candidates$projection[idx]))
      
      log_debug(sprintf("Generated %d incoming combinations", length(in_combos)), level = "DEBUG")
      
      combos_tried <- 0
      combos_evaluated <- 0
      
      for (out_idx in out_combos) {
        combos_tried <- combos_tried + 1
        
        # Budget after selling these players
        budget_after_sales <- current_budget + sum(roster_players$salary[out_idx])
        
        # Filter to affordable incoming combinations
        affordable_mask <- in_combo_salaries <= budget_after_sales
        
        if (!any(affordable_mask)) next
        
        # Get indices of affordable combos, sorted by total projection (descending)
        affordable_indices <- which(affordable_mask)
        affordable_indices <- affordable_indices[order(in_combo_projs[affordable_indices], decreasing = TRUE)]
        
        # Try top N affordable combinations (limit for performance)
        max_in_to_try <- min(length(affordable_indices), 50)
        
        for (in_combo_idx in affordable_indices[1:max_in_to_try]) {
          in_idx <- in_combos[[in_combo_idx]]
          combos_evaluated <- combos_evaluated + 1
          
          # Build new roster
          new_roster <- roster_players[-out_idx, ]
          for (k in 1:n_transfers) {
            new_roster <- bind_rows(new_roster, 
                                    in_candidates[in_idx[k], ] %>% select(player_name, salary, projection, match_key))
          }
          
          # Calculate actual lineup projection
          new_proj <- calculate_lineup_projection(new_roster)$total
          
          if (is.na(new_proj)) next
          
          gross_gain <- new_proj - current_proj
          net_gain <- gross_gain + total_penalty
          budget_change <- sum(in_candidates$salary[in_idx]) - sum(roster_players$salary[out_idx])
          
          # Log improvements
          if (net_gain > best_net_gain && net_gain > 0) {
            out_names <- paste(roster_players$player_name[out_idx], collapse = "+")
            in_names <- paste(in_candidates$player_name[in_idx], collapse = "+")
            log_debug(sprintf("%d-transfer NEW BEST: OUT[%s] IN[%s] | gross=%.1f pen=%.0f net=%.1f", 
                              n_transfers, out_names, in_names, gross_gain, total_penalty, net_gain), 
                      level = "DEBUG")
          }
          
          if (net_gain > best_net_gain) {
            best_net_gain <- net_gain
            
            out_list <- lapply(out_idx, function(i) roster_players[i, ])
            in_list <- lapply(in_idx, function(i) in_candidates[i, ])
            
            best_transfer <- list(
              out = out_list,
              in_ = in_list,
              current_proj = current_proj,
              new_proj = new_proj,
              gross_gain = gross_gain,
              penalty = total_penalty,
              net_gain = net_gain,
              budget_used = budget_change
            )
          }
        }
      }
      
      log_debug(sprintf("%d-transfer: tried %d out-combos, evaluated %d total roster combinations, best_net=%.1f", 
                        n_transfers, combos_tried, combos_evaluated, best_net_gain), level = "DEBUG")
    }
  }
  
  if (!is.null(best_transfer)) {
    log_debug(sprintf("Best %d-transfer: +%.1f pts (gross: +%.1f, penalty: %.0f)", 
                      n_transfers, best_transfer$net_gain, best_transfer$gross_gain, 
                      best_transfer$penalty), level = "INFO")
  } else {
    log_debug(sprintf("No beneficial %d-transfer found", n_transfers), level = "INFO")
  }
  
  return(best_transfer)
}


# =============================================================================
# UI
# =============================================================================

golf_season_management_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("golf_season_management_ui() called with id:", id, level = "INFO")
  
  # Get available weeks and tournaments
  weeks <- tryCatch(get_roster_weeks(), error = function(e) c("Week 1"))
  tournaments <- tryCatch(get_projection_tournaments(), error = function(e) character(0))
  salary_tournaments <- tryCatch(get_salary_tournaments(), error = function(e) c("No tournaments found" = ""))
  
  # Debug: log what we got for salary tournaments
  log_debug("UI init - salary_tournaments:", paste(salary_tournaments, collapse = ", "), level = "INFO")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Season Long Management"),
      tags$p(class = "text-muted", "Manage your 5 rosters - optimizes CPT (highest proj) + DOG (cheapest) bonuses")
    ),
    
    # =========================================================================
    # OPENING POSITION (renamed from Current Week Settings)
    # =========================================================================
    ui_card(
      title = "Opening Position",
      color = GOLF_CARD_COLOR,
      
      fluidRow(
        column(3,
               selectizeInput(ns("week_select"), "Roster Week",
                              choices = weeks,
                              selected = if (length(weeks) > 0) weeks[length(weeks)] else "Week 1"
               )
        ),
        column(5,
               selectizeInput(ns("tournament_select"), "Tournament (Projections)",
                              choices = if (length(tournaments) > 0) tournaments else c("No tournaments found" = ""),
                              selected = if (length(tournaments) > 0) tournaments[1] else NULL
               )
        ),
        column(4,
               div(style = "margin-top: 25px;",
                   actionButton(ns("load_btn"), "Load & Match", class = "btn btn-primary w-100", icon = icon("sync"))
               )
        )
      ),
      
      # Status message
      uiOutput(ns("status_message"))
    ),
    
    tags$br(),
    
    
    # =========================================================================
    # CURRENT LINEUPS (renamed from Recommended Lineups)
    # =========================================================================
    ui_card(
      title = "Current Lineups",
      color = GOLF_CARD_COLOR,
      
      uiOutput(ns("rosters_display"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # TRANSFER PLANNING
    # =========================================================================
    ui_card(
      title = "Transfer Planning",
      color = GOLF_CARD_COLOR,
      
      # Transfer settings row
      fluidRow(
        column(5,
               selectizeInput(ns("next_tournament_select"), "Next Week Tournament (Salaries & Projections)",
                              choices = if (length(salary_tournaments) > 0) salary_tournaments else c("No tournaments found" = ""),
                              selected = if (length(salary_tournaments) > 0) salary_tournaments[1] else NULL
               )
        ),
        column(3,
               div(style = "margin-top: 25px;",
                   actionButton(ns("load_transfer_data_btn"), "Load Transfer Data", 
                                class = "btn btn-secondary w-100", icon = icon("download"))
               )
        )
      ),
      
      # Transfer data status
      uiOutput(ns("transfer_data_status")),
      
      tags$hr(),
      
      # Free transfers per roster - individual buttons to avoid timeout
      div(
        style = "margin-bottom: 1rem;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem;",
          tags$h5(style = "margin: 0; font-weight: 700;", "Free Transfers Available"),
          div(
            style = "display: flex; gap: 0.5rem; align-items: center;",
            uiOutput(ns("cache_status")),
            actionButton(ns("clear_cache_btn"), "Clear Cache", 
                         class = "btn btn-outline-secondary btn-sm", 
                         icon = icon("trash-alt"),
                         style = "padding: 0.2rem 0.5rem; font-size: 0.7rem;")
          )
        ),
        tags$p(class = "text-muted", style = "font-size: 0.8rem; margin-bottom: 0.75rem;",
               "Set how many free transfers each roster has banked, then click Calculate. Each transfer above free costs -20 pts."
        ),
        
        # Grid layout: 5 roster rows with free transfer input + calculate button
        div(
          style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 0.5rem;",
          
          # Roster 1
          div(
            style = "display: flex; flex-direction: column; gap: 0.25rem; padding: 0.5rem; background: var(--bg-secondary); border-radius: 6px;",
            tags$label(style = "font-size: 0.75rem; font-weight: 600;", "Roster 1"),
            div(
              style = "display: flex; gap: 0.25rem; align-items: center;",
              numericInput(ns("transfers_r1_free"), NULL, value = 1, min = 0, max = 31, width = "60px"),
              actionButton(ns("calc_r1_btn"), "Calc", class = "btn btn-primary btn-sm", icon = icon("calculator"), style = "padding: 0.25rem 0.5rem;")
            ),
            uiOutput(ns("r1_status"))
          ),
          
          # Roster 2
          div(
            style = "display: flex; flex-direction: column; gap: 0.25rem; padding: 0.5rem; background: var(--bg-secondary); border-radius: 6px;",
            tags$label(style = "font-size: 0.75rem; font-weight: 600;", "Roster 2"),
            div(
              style = "display: flex; gap: 0.25rem; align-items: center;",
              numericInput(ns("transfers_r2_free"), NULL, value = 1, min = 0, max = 31, width = "60px"),
              actionButton(ns("calc_r2_btn"), "Calc", class = "btn btn-primary btn-sm", icon = icon("calculator"), style = "padding: 0.25rem 0.5rem;")
            ),
            uiOutput(ns("r2_status"))
          ),
          
          # Roster 3
          div(
            style = "display: flex; flex-direction: column; gap: 0.25rem; padding: 0.5rem; background: var(--bg-secondary); border-radius: 6px;",
            tags$label(style = "font-size: 0.75rem; font-weight: 600;", "Roster 3"),
            div(
              style = "display: flex; gap: 0.25rem; align-items: center;",
              numericInput(ns("transfers_r3_free"), NULL, value = 1, min = 0, max = 31, width = "60px"),
              actionButton(ns("calc_r3_btn"), "Calc", class = "btn btn-primary btn-sm", icon = icon("calculator"), style = "padding: 0.25rem 0.5rem;")
            ),
            uiOutput(ns("r3_status"))
          ),
          
          # Roster 4
          div(
            style = "display: flex; flex-direction: column; gap: 0.25rem; padding: 0.5rem; background: var(--bg-secondary); border-radius: 6px;",
            tags$label(style = "font-size: 0.75rem; font-weight: 600;", "Roster 4"),
            div(
              style = "display: flex; gap: 0.25rem; align-items: center;",
              numericInput(ns("transfers_r4_free"), NULL, value = 1, min = 0, max = 31, width = "60px"),
              actionButton(ns("calc_r4_btn"), "Calc", class = "btn btn-primary btn-sm", icon = icon("calculator"), style = "padding: 0.25rem 0.5rem;")
            ),
            uiOutput(ns("r4_status"))
          ),
          
          # Roster 5
          div(
            style = "display: flex; flex-direction: column; gap: 0.25rem; padding: 0.5rem; background: var(--bg-secondary); border-radius: 6px;",
            tags$label(style = "font-size: 0.75rem; font-weight: 600;", "Roster 5"),
            div(
              style = "display: flex; gap: 0.25rem; align-items: center;",
              numericInput(ns("transfers_r5_free"), NULL, value = 1, min = 0, max = 31, width = "60px"),
              actionButton(ns("calc_r5_btn"), "Calc", class = "btn btn-primary btn-sm", icon = icon("calculator"), style = "padding: 0.25rem 0.5rem;")
            ),
            uiOutput(ns("r5_status"))
          )
        )
      ),
      
      tags$hr(),
      
      # Transfer recommendations display - full width per roster
      uiOutput(ns("transfer_recommendations"))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

golf_season_management_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("golf_season_management_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # SESSION CACHE KEY (for persisting data across navigation)
    # =========================================================================
    cache_key <- "golf_season_management_cache"
    
    # Initialize session cache if not exists
    if (is.null(session$userData[[cache_key]])) {
      session$userData[[cache_key]] <- list(
        roster_data = NULL,
        projections = NULL,
        matched_data = NULL,
        next_projections = NULL,
        next_salaries = NULL,
        transfer_data_loaded = FALSE,
        transfer_results = NULL,
        roster_budgets = list(),
        roster_calc_status = list(r1 = FALSE, r2 = FALSE, r3 = FALSE, r4 = FALSE, r5 = FALSE),
        selected_week = NULL,
        selected_tournament = NULL,
        selected_next_tournament = NULL
      )
    }
    
    # Restore from cache
    cache <- session$userData[[cache_key]]
    
    # =========================================================================
    # REACTIVE VALUES (initialized from cache)
    # =========================================================================
    rv <- reactiveValues(
      roster_data = cache$roster_data,
      projections = cache$projections,
      matched_data = cache$matched_data,
      
      # Transfer planning
      next_projections = cache$next_projections,
      next_salaries = cache$next_salaries,
      current_salaries = NULL,
      transfer_data_loaded = cache$transfer_data_loaded,
      transfer_results = cache$transfer_results,
      roster_budgets = cache$roster_budgets,
      
      # Per-roster calculation status
      roster_calc_status = cache$roster_calc_status
    )
    
    # =========================================================================
    # CACHE SYNC - Save to session whenever key data changes
    # =========================================================================
    observe({
      session$userData[[cache_key]]$roster_data <- rv$roster_data
      session$userData[[cache_key]]$projections <- rv$projections
      session$userData[[cache_key]]$matched_data <- rv$matched_data
      session$userData[[cache_key]]$next_projections <- rv$next_projections
      session$userData[[cache_key]]$next_salaries <- rv$next_salaries
      session$userData[[cache_key]]$transfer_data_loaded <- rv$transfer_data_loaded
      session$userData[[cache_key]]$transfer_results <- rv$transfer_results
      session$userData[[cache_key]]$roster_budgets <- rv$roster_budgets
      session$userData[[cache_key]]$roster_calc_status <- rv$roster_calc_status
    })
    
    # Save dropdown selections to cache
    observeEvent(input$week_select, {
      session$userData[[cache_key]]$selected_week <- input$week_select
    }, ignoreNULL = FALSE)
    
    observeEvent(input$tournament_select, {
      session$userData[[cache_key]]$selected_tournament <- input$tournament_select
    }, ignoreNULL = FALSE)
    
    observeEvent(input$next_tournament_select, {
      session$userData[[cache_key]]$selected_next_tournament <- input$next_tournament_select
    }, ignoreNULL = FALSE)
    
    # Restore dropdown selections on init (with slight delay for UI to render)
    observe({
      # Only run once on init
      isolate({
        cache <- session$userData[[cache_key]]
        
        if (!is.null(cache$selected_week)) {
          updateSelectizeInput(session, "week_select", selected = cache$selected_week)
        }
        if (!is.null(cache$selected_tournament)) {
          updateSelectizeInput(session, "tournament_select", selected = cache$selected_tournament)
        }
        if (!is.null(cache$selected_next_tournament)) {
          updateSelectizeInput(session, "next_tournament_select", selected = cache$selected_next_tournament)
        }
      })
    }) |> bindEvent(TRUE, once = TRUE)
    
    # =========================================================================
    # LOAD CURRENT WEEK DATA (Opening Position)
    # =========================================================================
    observeEvent(input$load_btn, {
      req(input$week_select, input$tournament_select)
      req(input$tournament_select != "")
      
      log_debug(">>> Load button clicked", level = "INFO")
      log_debug(">>> Week:", input$week_select, level = "INFO")
      log_debug(">>> Tournament:", input$tournament_select, level = "INFO")
      
      showNotification("Loading roster data...", type = "message", duration = 2)
      
      # Load roster data
      roster_data <- load_roster_data(input$week_select)
      if (is.null(roster_data)) {
        showNotification("Failed to load roster data", type = "error")
        return()
      }
      rv$roster_data <- roster_data
      
      # Load projections
      showNotification("Loading projections...", type = "message", duration = 2)
      projections <- load_tournament_projections(input$tournament_select)
      if (is.null(projections)) {
        showNotification("Failed to load projections", type = "error")
        return()
      }
      rv$projections <- projections
      
      # Match data
      matched <- match_roster_to_projections(roster_data, projections)
      rv$matched_data <- matched
      
      matched_count <- sum(!is.na(matched$projection))
      unmatched_count <- nrow(matched) - matched_count
      
      showNotification(
        sprintf("Matched %d of %d players to projections", matched_count, nrow(matched)),
        type = if (unmatched_count > 0) "warning" else "message"
      )
      
    })
    
    # =========================================================================
    # LOAD TRANSFER DATA (separate from calculation)
    # =========================================================================
    observeEvent(input$load_transfer_data_btn, {
      req(input$next_tournament_select)
      req(input$next_tournament_select != "")
      
      tournament_name <- input$next_tournament_select
      
      log_debug(">>> Load transfer data clicked", level = "INFO")
      log_debug(">>> Tournament:", tournament_name, level = "INFO")
      
      showNotification("Loading next week data...", type = "message", duration = 2)
      
      # Load projections for the tournament
      next_proj <- load_tournament_projections(tournament_name)
      if (is.null(next_proj)) {
        showNotification("Failed to load projections - check if tournament exists in projections sheet", type = "error")
        return()
      }
      rv$next_projections <- next_proj
      
      # Load salaries for the tournament
      next_sal <- load_tournament_salaries(tournament_name)
      if (is.null(next_sal)) {
        showNotification("Failed to load salaries - check if tournament exists in salaries sheet", type = "error")
        return()
      }
      rv$next_salaries <- next_sal
      
      rv$transfer_data_loaded <- TRUE
      
      # Reset calculation status and results for new data
      rv$roster_calc_status <- list(r1 = FALSE, r2 = FALSE, r3 = FALSE, r4 = FALSE, r5 = FALSE)
      rv$transfer_results <- NULL
      rv$roster_budgets <- list()
      
      showNotification(
        sprintf("Loaded %d projections and %d salaries for %s", 
                nrow(next_proj), nrow(next_sal), tournament_name),
        type = "message"
      )
    })
    
    # =========================================================================
    # CALCULATE TRANSFERS - HELPER FUNCTION (single roster)
    # =========================================================================
    calculate_roster_transfers <- function(roster_idx) {
      # Get roster name from the available rosters
      rosters <- unique(rv$roster_data$roster)
      if (roster_idx > length(rosters)) {
        showNotification(sprintf("Roster %d not found", roster_idx), type = "error")
        return()
      }
      
      roster_name <- rosters[roster_idx]
      
      log_debug(sprintf(">>> Calculating transfers for Roster %d: %s", roster_idx, roster_name), level = "INFO")
      showNotification(sprintf("Calculating transfers for %s...", roster_name), type = "message", duration = 3)
      
      # Get transfer data
      next_proj <- rv$next_projections
      next_sal <- rv$next_salaries
      
      # Initialize results list if not already done
      if (is.null(rv$transfer_results)) {
        rv$transfer_results <- list()
      }
      if (is.null(rv$roster_budgets)) {
        rv$roster_budgets <- list()
      }
      
      # Get roster players
      roster_players <- rv$roster_data %>%
        filter(roster == roster_name)
      
      log_debug(sprintf("Roster %s has %d players", roster_name, nrow(roster_players)), level = "INFO")
      
      # Match to salaries
      roster_with_sal <- roster_players %>%
        left_join(
          next_sal %>% select(match_key, salary),
          by = "match_key"
        )
      
      # Log unmatched salary players
      unmatched_sal <- roster_with_sal %>% filter(is.na(salary)) %>% pull(player_name)
      if (length(unmatched_sal) > 0) {
        log_debug(sprintf("Roster %s: %d players without salary match: %s", 
                          roster_name, length(unmatched_sal), paste(unmatched_sal, collapse = ", ")), 
                  level = "WARN")
      }
      
      # Match to projections
      roster_with_proj <- roster_with_sal %>%
        left_join(
          next_proj %>% select(match_key, projection),
          by = "match_key"
        )
      
      # Players without projections are NOT in the tournament
      not_in_tournament <- roster_with_proj %>% filter(is.na(projection)) %>% pull(player_name)
      in_tournament <- roster_with_proj %>% filter(!is.na(projection)) %>% pull(player_name)
      
      if (length(not_in_tournament) > 0) {
        log_debug(sprintf("Roster %s: %d players NOT in tournament: %s", 
                          roster_name, length(not_in_tournament), paste(not_in_tournament, collapse = ", ")), 
                  level = "INFO")
      }
      
      # Calculate budget
      team_value <- sum(roster_with_proj$salary, na.rm = TRUE)
      remaining_budget <- GOLF_SEASON_MANAGEMENT_CONFIG$budget - team_value
      
      # Count players without salary match
      players_without_salary <- roster_with_proj %>% filter(is.na(salary))
      missing_salary_names <- if (nrow(players_without_salary) > 0) players_without_salary$player_name else NULL
      
      # Warning flag
      salary_warning <- remaining_budget > 10
      
      rv$roster_budgets[[roster_name]] <- list(
        team_value = team_value,
        remaining_budget = remaining_budget,
        players_in_tournament = length(in_tournament),
        players_not_in_tournament = length(not_in_tournament),
        not_in_tournament_names = if (length(not_in_tournament) > 0) not_in_tournament else NULL,
        missing_salary_count = nrow(players_without_salary),
        missing_salary_names = missing_salary_names,
        salary_warning = salary_warning
      )
      
      if (salary_warning) {
        log_debug(sprintf("WARNING Roster %s: Available budget $%.1fM > $10M - check salary file!", 
                          roster_name, remaining_budget), level = "WARN")
      }
      
      log_debug(sprintf("Roster %s: %d/10 in tournament, Team value = $%.1fM, Remaining = $%.1fM", 
                        roster_name, length(in_tournament), team_value, remaining_budget), level = "INFO")
      
      # Get free transfers for this roster
      free_transfers <- input[[paste0("transfers_r", roster_idx, "_free")]] %||% 1
      
      log_debug(sprintf("Roster %d: %d free transfers available", roster_idx, free_transfers), level = "INFO")
      
      # Build available pool
      available_pool <- next_proj %>%
        left_join(
          next_sal %>% select(match_key, pool_salary = salary),
          by = "match_key"
        ) %>%
        mutate(salary = coalesce(pool_salary, salary)) %>%
        filter(!is.na(projection) & !is.na(salary))
      
      log_debug(sprintf("Available pool: %d players", nrow(available_pool)), level = "INFO")
      
      # Prepare roster for optimization
      roster_for_opt <- roster_with_proj %>%
        mutate(
          projection = coalesce(projection, 0),
          salary = coalesce(salary, 0)
        ) %>%
        select(player_name, salary, projection, match_key)
      
      # Diagnostic: show current lineup
      current_lineup <- calculate_lineup_projection(roster_for_opt)
      log_debug(sprintf("Roster %s current lineup projection: %.1f", roster_name, current_lineup$total), level = "INFO")
      
      # Calculate optimal transfers for 1-5 transfers
      transfer_scenarios <- list()
      
      for (n_trans in 1:5) {
        log_debug(sprintf("Roster %s: Calculating %d-transfer scenario...", roster_name, n_trans), level = "INFO")
        
        transfer_result <- find_optimal_transfers_n(
          roster_players = roster_for_opt,
          available_pool = available_pool,
          current_budget = remaining_budget,
          n_transfers = n_trans,
          free_transfers = free_transfers
        )
        
        transfer_scenarios[[as.character(n_trans)]] <- transfer_result
      }
      
      # Summary log
      log_debug(sprintf("=== %s TRANSFER SUMMARY ===", roster_name), level = "INFO")
      for (n in 1:5) {
        sc <- transfer_scenarios[[as.character(n)]]
        if (is.null(sc)) {
          log_debug(sprintf("  %d transfer(s): No beneficial option", n), level = "INFO")
        } else {
          log_debug(sprintf("  %d transfer(s): net=%+.1f (gross=%+.1f, penalty=%.0f)", 
                            n, sc$net_gain, sc$gross_gain, sc$penalty), level = "INFO")
        }
      }
      
      # Store results
      rv$transfer_results[[roster_name]] <- list(
        free_transfers = free_transfers,
        scenarios = transfer_scenarios
      )
      
      # Update calculation status
      status_key <- paste0("r", roster_idx)
      rv$roster_calc_status[[status_key]] <- TRUE
      
      showNotification(sprintf("%s transfer analysis complete!", roster_name), type = "message")
    }
    
    # =========================================================================
    # INDIVIDUAL ROSTER CALCULATE BUTTONS
    # =========================================================================
    observeEvent(input$calc_r1_btn, {
      req(rv$transfer_data_loaded, rv$roster_data, rv$next_projections, rv$next_salaries)
      calculate_roster_transfers(1)
    })
    
    observeEvent(input$calc_r2_btn, {
      req(rv$transfer_data_loaded, rv$roster_data, rv$next_projections, rv$next_salaries)
      calculate_roster_transfers(2)
    })
    
    observeEvent(input$calc_r3_btn, {
      req(rv$transfer_data_loaded, rv$roster_data, rv$next_projections, rv$next_salaries)
      calculate_roster_transfers(3)
    })
    
    observeEvent(input$calc_r4_btn, {
      req(rv$transfer_data_loaded, rv$roster_data, rv$next_projections, rv$next_salaries)
      calculate_roster_transfers(4)
    })
    
    observeEvent(input$calc_r5_btn, {
      req(rv$transfer_data_loaded, rv$roster_data, rv$next_projections, rv$next_salaries)
      calculate_roster_transfers(5)
    })
    
    # =========================================================================
    # PER-ROSTER STATUS OUTPUTS
    # =========================================================================
    output$r1_status <- renderUI({
      if (rv$roster_calc_status$r1) {
        div(style = "font-size: 0.65rem; color: var(--accent-sage); font-weight: 600;", icon("check"), "Done")
      } else {
        div(style = "font-size: 0.65rem; color: var(--text-muted);", "Pending")
      }
    })
    
    output$r2_status <- renderUI({
      if (rv$roster_calc_status$r2) {
        div(style = "font-size: 0.65rem; color: var(--accent-sage); font-weight: 600;", icon("check"), "Done")
      } else {
        div(style = "font-size: 0.65rem; color: var(--text-muted);", "Pending")
      }
    })
    
    output$r3_status <- renderUI({
      if (rv$roster_calc_status$r3) {
        div(style = "font-size: 0.65rem; color: var(--accent-sage); font-weight: 600;", icon("check"), "Done")
      } else {
        div(style = "font-size: 0.65rem; color: var(--text-muted);", "Pending")
      }
    })
    
    output$r4_status <- renderUI({
      if (rv$roster_calc_status$r4) {
        div(style = "font-size: 0.65rem; color: var(--accent-sage); font-weight: 600;", icon("check"), "Done")
      } else {
        div(style = "font-size: 0.65rem; color: var(--text-muted);", "Pending")
      }
    })
    
    output$r5_status <- renderUI({
      if (rv$roster_calc_status$r5) {
        div(style = "font-size: 0.65rem; color: var(--accent-sage); font-weight: 600;", icon("check"), "Done")
      } else {
        div(style = "font-size: 0.65rem; color: var(--text-muted);", "Pending")
      }
    })
    
    # =========================================================================
    # CACHE STATUS INDICATOR
    # =========================================================================
    output$cache_status <- renderUI({
      # Count how many rosters have been calculated
      calc_count <- sum(unlist(rv$roster_calc_status))
      
      if (calc_count > 0) {
        div(
          style = "font-size: 0.7rem; color: var(--accent-sage); font-weight: 500;",
          icon("database"), sprintf(" %d/5 cached", calc_count)
        )
      } else if (rv$transfer_data_loaded) {
        div(
          style = "font-size: 0.7rem; color: var(--text-muted);",
          icon("database"), " Ready"
        )
      } else {
        NULL
      }
    })
    
    # =========================================================================
    # CLEAR CACHE HANDLER
    # =========================================================================
    observeEvent(input$clear_cache_btn, {
      # Reset all cached data
      rv$roster_data <- NULL
      rv$projections <- NULL
      rv$matched_data <- NULL
      rv$next_projections <- NULL
      rv$next_salaries <- NULL
      rv$transfer_data_loaded <- FALSE
      rv$transfer_results <- NULL
      rv$roster_budgets <- list()
      rv$roster_calc_status <- list(r1 = FALSE, r2 = FALSE, r3 = FALSE, r4 = FALSE, r5 = FALSE)
      
      # Clear session cache
      session$userData[[cache_key]] <- list(
        roster_data = NULL,
        projections = NULL,
        matched_data = NULL,
        next_projections = NULL,
        next_salaries = NULL,
        transfer_data_loaded = FALSE,
        transfer_results = NULL,
        roster_budgets = list(),
        roster_calc_status = list(r1 = FALSE, r2 = FALSE, r3 = FALSE, r4 = FALSE, r5 = FALSE),
        selected_week = NULL,
        selected_tournament = NULL,
        selected_next_tournament = NULL
      )
      
      showNotification("Cache cleared - reload data to start fresh", type = "message")
    })
    
    # =========================================================================
    # STATUS MESSAGE
    # =========================================================================
    output$status_message <- renderUI({
      if (is.null(rv$matched_data)) {
        return(div(
          class = "text-muted mt-3",
          icon("info-circle"), " Select week and tournament, then click Load & Match"
        ))
      }
      
      total_count <- nrow(rv$matched_data)
      matched_count <- sum(!is.na(rv$matched_data$projection))
      unmatched_count <- total_count - matched_count
      
      if (unmatched_count == 0) {
        div(
          class = "alert alert-success mt-3 mb-0",
          icon("check-circle"), sprintf(" All %d players matched! CPT + DOG (1.25x each) optimized.", total_count)
        )
      } else {
        div(
          class = "alert alert-warning mt-3 mb-0",
          icon("exclamation-triangle"),
          sprintf(" %d of %d players matched. %d not playing this week (shown in coral).",
                  matched_count, total_count, unmatched_count)
        )
      }
    })
    
    # =========================================================================
    # TRANSFER DATA STATUS
    # =========================================================================
    output$transfer_data_status <- renderUI({
      if (!rv$transfer_data_loaded) {
        return(div(
          class = "text-muted mt-2 mb-0",
          icon("info-circle"), " Click 'Load Transfer Data' to load next week projections and salaries"
        ))
      }
      
      proj_count <- if (!is.null(rv$next_projections)) nrow(rv$next_projections) else 0
      sal_count <- if (!is.null(rv$next_salaries)) nrow(rv$next_salaries) else 0
      calc_count <- sum(unlist(rv$roster_calc_status))
      
      # Show different message if we have cached calculations
      if (calc_count > 0) {
        div(
          class = "alert alert-success mt-2 mb-0",
          style = "padding: 0.5rem;",
          icon("check-circle"), 
          sprintf(" Transfer data loaded: %d projections, %d salaries", proj_count, sal_count),
          span(style = "margin-left: 0.5rem; font-weight: 600; color: var(--accent-sage);",
               sprintf("(%d/5 rosters calculated)", calc_count))
        )
      } else {
        div(
          class = "alert alert-success mt-2 mb-0",
          style = "padding: 0.5rem;",
          icon("check-circle"), 
          sprintf(" Transfer data loaded: %d projections, %d salaries", proj_count, sal_count)
        )
      }
    })
    
    # =========================================================================
    # ROSTERS DISPLAY (Current Lineups)
    # =========================================================================
    output$rosters_display <- renderUI({
      if (is.null(rv$matched_data)) {
        return(div(
          class = "text-muted text-center py-4",
          "Load data to see current lineups"
        ))
      }
      
      # Get unique rosters
      rosters <- unique(rv$matched_data$roster)
      
      # Create a card for each roster
      create_roster_card <- function(roster_name) {
        roster_players <- rv$matched_data %>%
          filter(roster == roster_name) %>%
          select_best_six()
        
        # Calculate totals for starters (using effective projections with bonuses)
        starters <- roster_players %>% filter(is_starter & !is_unmatched)
        total_projection <- sum(starters$effective_projection, na.rm = TRUE)
        
        # Count issues
        unmatched_count <- sum(roster_players$is_unmatched)
        
        div(
          style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
          
          # Header
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px dashed var(--bg-secondary);",
            span(style = "font-weight: 800; text-transform: uppercase; font-size: 0.9rem;", roster_name),
            div(
              style = "text-align: right;",
              div(style = "font-size: 0.65rem; text-transform: uppercase; color: var(--text-muted);", "Projected"),
              div(style = "font-size: 1.1rem; font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", total_projection))
            )
          ),
          
          # Player list
          div(
            style = "display: flex; flex-direction: column; gap: 0.25rem;",
            lapply(1:nrow(roster_players), function(i) {
              player <- roster_players[i, ]
              is_bench <- !player$is_starter
              is_captain <- player$is_captain && !player$is_unmatched
              is_underdog <- player$is_underdog && !player$is_unmatched
              is_unmatched <- player$is_unmatched
              
              div(
                style = sprintf(
                  "display: flex; align-items: center; justify-content: space-between; padding: 0.3rem 0.5rem; border-radius: 4px; background: %s;",
                  if (is_unmatched) "var(--accent-coral-light, #F5DDD5)" 
                  else if (is_captain) "#E8E0F0"
                  else if (is_underdog) "#FDF6E3"
                  else if (is_bench) "var(--bg-secondary)" 
                  else "white"
                ),
                
                # Rank + Name
                div(
                  style = "display: flex; align-items: center; gap: 0.5rem;",
                  span(
                    style = sprintf(
                      "width: 18px; height: 18px; border-radius: 50%%; background: %s; color: white; font-size: 0.65rem; display: flex; align-items: center; justify-content: center; font-weight: 600;",
                      if (is_captain) "#B48EAD"
                      else if (is_underdog) "#EBCB8B"
                      else if (is_unmatched) "var(--accent-coral)"
                      else if (is_bench) "var(--text-muted)"
                      else "var(--accent-sage)"
                    ),
                    if (is_captain) "C" 
                    else if (is_underdog) "D"
                    else player$lineup_rank
                  ),
                  span(
                    style = sprintf(
                      "font-size: 0.8rem; font-weight: %s; %s",
                      if (is_captain || is_underdog) "600" else "500",
                      if (is_unmatched) "color: var(--accent-coral);" else ""
                    ),
                    player$player_name
                  )
                ),
                
                # Projection / Effective
                div(
                  style = "text-align: right; width: 50px;",
                  div(style = "font-size: 0.5rem; color: var(--text-muted);", 
                      if ((is_captain || is_underdog) && !is_unmatched) "EFF" else "PROJ"),
                  div(
                    style = sprintf(
                      "font-size: 0.75rem; font-weight: 600; %s",
                      if (is_unmatched) "color: var(--accent-coral);" 
                      else if (is_captain) "color: #B48EAD;"
                      else if (is_underdog) "color: #EBCB8B;"
                      else if (is_bench) "" 
                      else "color: var(--accent-coral);"
                    ),
                    if (is_unmatched) {
                      "N/A"
                    } else if (is_captain || is_underdog) {
                      sprintf("%.1f", player$effective_projection)
                    } else {
                      sprintf("%.1f", player$projection)
                    }
                  )
                ),
                
                # Ownership (if available and matched)
                if (!is_unmatched && !is.na(player$ownership)) {
                  div(
                    style = "text-align: right; width: 40px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "OWN"),
                    div(style = "font-size: 0.7rem; color: var(--text-secondary);", sprintf("%.0f%%", player$ownership))
                  )
                }
              )
            })
          ),
          
          # Footer legend
          div(
            style = "margin-top: 0.5rem; padding-top: 0.5rem; border-top: 1px solid var(--border); font-size: 0.65rem; color: var(--text-muted); display: flex; flex-wrap: wrap; gap: 0.75rem;",
            span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: #B48EAD; margin-right: 4px;"), "CPT (1.25x)"),
            span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: #EBCB8B; margin-right: 4px;"), "DOG (1.25x)"),
            span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: var(--accent-sage); margin-right: 4px;"), "Starter"),
            span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: var(--text-muted); margin-right: 4px;"), "Bench"),
            if (unmatched_count > 0) {
              span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: var(--accent-coral); margin-right: 4px;"), "Not Playing")
            }
          )
        )
      }
      
      # Layout: 3 columns for 5 rosters
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
        lapply(rosters, create_roster_card)
      )
    })
    
    # =========================================================================
    # TRANSFER RECOMMENDATIONS DISPLAY
    # =========================================================================
    output$transfer_recommendations <- renderUI({
      # Check if any rosters have been calculated
      has_results <- !is.null(rv$transfer_results) && length(rv$transfer_results) > 0
      
      if (!has_results) {
        return(div(
          class = "text-muted text-center py-4",
          icon("info-circle"), " Load transfer data and click 'Calc' for each roster"
        ))
      }
      
      rosters <- names(rv$transfer_results)
      
      # Helper to create a single transfer scenario display
      create_scenario_cell <- function(n_trans, transfer_result, free_transfers) {
        # Calculate penalty for this scenario
        excess_transfers <- max(0, n_trans - free_transfers)
        penalty <- excess_transfers * -20
        
        if (is.null(transfer_result)) {
          # No beneficial transfer found
          div(
            style = "background: var(--bg-secondary); border-radius: 6px; padding: 0.5rem; height: 100%; opacity: 0.6;",
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.25rem;",
              span(style = "font-weight: 700; font-size: 0.8rem;", sprintf("%d Transfer%s", n_trans, if(n_trans > 1) "s" else "")),
              span(style = "font-size: 0.7rem; color: var(--text-muted);", 
                   if(excess_transfers > 0) sprintf("(-%d pts)", excess_transfers * 20) else "FREE")
            ),
            div(style = "font-size: 0.7rem; color: var(--text-muted); font-style: italic;", "No beneficial option")
          )
        } else {
          # Has a recommendation
          out_names <- sapply(transfer_result$out, function(x) x$player_name)
          in_names <- sapply(transfer_result$in_, function(x) x$player_name)
          
          gain_color <- if (transfer_result$net_gain > 0) "var(--accent-sage)" else "var(--accent-coral)"
          border_color <- if (transfer_result$net_gain > 0) "var(--accent-sage)" else "var(--border)"
          
          div(
            style = sprintf("background: white; border: 2px solid %s; border-radius: 6px; padding: 0.5rem; height: 100%%;", border_color),
            
            # Header with transfer count and net gain
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.25rem;",
              div(
                span(style = "font-weight: 700; font-size: 0.8rem;", sprintf("%d Transfer%s", n_trans, if(n_trans > 1) "s" else "")),
                if (excess_transfers > 0) {
                  span(style = "font-size: 0.65rem; color: var(--accent-coral); margin-left: 0.25rem;", 
                       sprintf("(-%d)", excess_transfers * 20))
                }
              ),
              span(
                style = sprintf("font-size: 0.9rem; font-weight: 700; color: %s;", gain_color),
                sprintf("%+.1f pts", transfer_result$net_gain)
              )
            ),
            
            # Transfer details - compact list
            div(
              style = "font-size: 0.65rem;",
              lapply(seq_along(out_names), function(i) {
                div(
                  style = "display: flex; align-items: center; gap: 0.2rem; margin-bottom: 0.1rem; white-space: nowrap; overflow: hidden;",
                  span(style = "color: var(--accent-coral); font-weight: 600; flex-shrink: 0;", "OUT"),
                  span(style = "overflow: hidden; text-overflow: ellipsis;", out_names[i]),
                  span(style = "color: var(--text-muted); flex-shrink: 0;", "→"),
                  span(style = "color: var(--accent-sage); font-weight: 600; flex-shrink: 0;", "IN"),
                  span(style = "overflow: hidden; text-overflow: ellipsis;", in_names[i])
                )
              })
            ),
            
            # Budget change
            div(
              style = "font-size: 0.6rem; color: var(--text-muted); margin-top: 0.25rem; padding-top: 0.25rem; border-top: 1px dashed var(--border);",
              sprintf("Gross: %+.1f | Budget: %+.1fM", transfer_result$gross_gain, -transfer_result$budget_used)
            )
          )
        }
      }
      
      # Helper to create full-width roster card with all 5 scenarios
      create_roster_transfer_card <- function(roster_name) {
        result_data <- rv$transfer_results[[roster_name]]
        budget_info <- rv$roster_budgets[[roster_name]]
        
        if (is.null(result_data)) {
          return(div(
            style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem; margin-bottom: 1rem;",
            tags$h5(style = "margin: 0;", roster_name),
            div(class = "text-muted", "No transfer data available")
          ))
        }
        
        free_transfers <- result_data$free_transfers
        scenarios <- result_data$scenarios
        
        # Find best net gain across all scenarios
        best_scenario <- NULL
        best_gain <- -Inf
        for (n in 1:5) {
          sc <- scenarios[[as.character(n)]]
          if (!is.null(sc) && sc$net_gain > best_gain) {
            best_gain <- sc$net_gain
            best_scenario <- n
          }
        }
        
        div(
          style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem; margin-bottom: 1rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
          
          # Header row
          div(
            style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px dashed var(--bg-secondary);",
            
            # Left: Roster name and budget info
            div(
              span(style = "font-weight: 800; text-transform: uppercase; font-size: 1rem;", roster_name),
              div(
                style = "display: flex; gap: 1rem; font-size: 0.7rem; color: var(--text-muted); margin-top: 0.25rem;",
                span(sprintf("Team: $%.1fM", budget_info$team_value)),
                span(sprintf("Budget: $%.1fM", budget_info$remaining_budget)),
                span(sprintf("In Tournament: %d/10", budget_info$players_in_tournament)),
                span(style = "font-weight: 600;", sprintf("Free Transfers: %d", free_transfers))
              ),
              # Show players not in tournament
              if (!is.null(budget_info$not_in_tournament_names) && length(budget_info$not_in_tournament_names) > 0) {
                div(
                  style = "font-size: 0.65rem; color: var(--accent-coral); margin-top: 0.25rem;",
                  tags$strong("Not playing: "),
                  paste(budget_info$not_in_tournament_names, collapse = ", ")
                )
              }
            ),
            
            # Right: Best recommendation badge
            if (!is.null(best_scenario) && best_gain > 0) {
              div(
                style = "background: var(--accent-sage); color: white; padding: 0.25rem 0.5rem; border-radius: 4px; font-size: 0.7rem; font-weight: 600;",
                sprintf("Best: %d transfer%s → %+.1f pts", best_scenario, if(best_scenario > 1) "s" else "", best_gain)
              )
            } else {
              div(
                style = "background: var(--text-muted); color: white; padding: 0.25rem 0.5rem; border-radius: 4px; font-size: 0.7rem;",
                "No beneficial transfers"
              )
            }
          ),
          
          # Warning if salary data looks incomplete
          if (!is.null(budget_info$salary_warning) && budget_info$salary_warning) {
            div(
              style = "background: var(--accent-coral); color: white; padding: 0.4rem 0.5rem; border-radius: 4px; margin-bottom: 0.75rem; font-size: 0.7rem; font-weight: 600;",
              icon("exclamation-triangle"),
              sprintf(" Budget $%.1fM > $10M - check salary file for missing players!", budget_info$remaining_budget)
            )
          },
          
          # 5 scenario cells in a row
          div(
            style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 0.5rem;",
            lapply(1:5, function(n) {
              create_scenario_cell(n, scenarios[[as.character(n)]], free_transfers)
            })
          )
        )
      }
      
      # Create full-width cards for each roster
      div(
        lapply(rosters, create_roster_transfer_card)
      )
    })
    
    
  })
}

cat("Golf Season Management module loaded: golf_season_management_ui(), golf_season_management_server()\n")
cat("  Optimizes lineup selection with CPT (highest proj, 1.25x) + DOG (cheapest, 1.25x) bonuses\n")
cat("  Transfer Planning: Shows best 1-5 transfers for each roster\n")