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

#' Get available salary weeks from salaries sheet
get_salary_weeks <- function() {
  log_debug("get_salary_weeks() called", level = "DEBUG")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    ss <- googlesheets4::gs4_get(GOLF_SALARIES_SHEET_ID)
    # Filter to only "Week N" sheets
    weeks <- ss$sheets$name[grepl("^Week \\d+$", ss$sheets$name)]
    
    log_debug("Found salary weeks:", paste(weeks, collapse = ", "), level = "INFO")
    return(weeks)
    
  }, error = function(e) {
    log_debug("Error getting salary weeks:", e$message, level = "ERROR")
    return(c("Week 1"))
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

#' Load salaries from Google Sheet for a specific week
load_week_salaries <- function(week_name = "Week 1") {
  log_debug("load_week_salaries() for:", week_name, level = "INFO")
  
  tryCatch({
    # Deauth for public sheet access (required for shinyapps.io)
    googlesheets4::gs4_deauth()
    
    salaries_raw <- googlesheets4::read_sheet(
      GOLF_SALARIES_SHEET_ID,
      sheet = week_name
    ) %>%
      janitor::clean_names()
    
    log_debug("Raw salary columns:", paste(names(salaries_raw), collapse = ", "), level = "DEBUG")
    
    # Check for FanTeam format (f_name + name) or simple format
    if (all(c("f_name", "name") %in% names(salaries_raw))) {
      # FanTeam format
      salaries <- salaries_raw %>%
        filter(lineup != "refuted") %>%
        mutate(
          player_name = paste0(f_name, " ", name),
          salary = as.numeric(price)
        )
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
    
    # Create match key
    salaries <- salaries %>%
      mutate(match_key = sapply(player_name, normalize_management_name)) %>%
      select(player_name, salary, match_key) %>%
      filter(!is.na(salary))
    
    log_debug("Loaded", nrow(salaries), "player salaries", level = "INFO")
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
  
  # Generate all combinations of 6 from n matched players
  combos <- combn(n_matched, 6)
  
  best_total <- -Inf
  best_combo <- NULL
  
  # Evaluate each combination
  for (i in 1:ncol(combos)) {
    idx <- combos[, i]
    lineup <- matched_players[idx, ]
    
    # Skip if any salary is NA (can't determine underdog)
    if (any(is.na(lineup$salary))) next
    
    # Captain = highest projection, Underdog = lowest salary
    captain_proj <- max(lineup$projection)
    underdog_proj <- lineup$projection[which.min(lineup$salary)]
    
    # Calculate effective total with bonuses
    total <- sum(lineup$projection)
    total <- total + captain_proj * 0.25      # Captain bonus
    total <- total + underdog_proj * 0.25     # Underdog bonus
    
    if (total > best_total) {
      best_total <- total
      best_combo <- idx
    }
  }
  
  # If no valid combo found (all had NA salaries), fall back to projection-only
  if (is.null(best_combo)) {
    log_debug("No valid lineup with salary data, falling back to projection-only", level = "WARN")
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
          lineup_rank = nrow(matched_players) + row_number(),
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
  
  # Build result with best lineup
  starter_indices <- best_combo
  bench_indices <- setdiff(1:n_matched, best_combo)
  
  starters <- matched_players[starter_indices, ]
  captain_player <- starters$player_name[which.max(starters$projection)]
  underdog_player <- starters$player_name[which.min(starters$salary)]
  
  # Arrange starters by projection (captain first)
  starters <- starters %>%
    arrange(desc(projection)) %>%
    mutate(
      lineup_rank = row_number(),
      is_starter = TRUE,
      is_captain = player_name == captain_player,
      is_underdog = player_name == underdog_player,
      effective_projection = case_when(
        is_captain ~ projection * 1.25,
        is_underdog ~ projection * 1.25,
        TRUE ~ projection
      ),
      is_unmatched = FALSE
    )
  
  # Add bench players
  if (length(bench_indices) > 0) {
    bench <- matched_players[bench_indices, ] %>%
      arrange(desc(projection)) %>%
      mutate(
        lineup_rank = 6 + row_number(),
        is_starter = FALSE,
        is_captain = FALSE,
        is_underdog = FALSE,
        effective_projection = projection,
        is_unmatched = FALSE
      )
    starters <- bind_rows(starters, bench)
  }
  
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
    starters <- bind_rows(starters, unmatched_result)
  }
  
  return(starters)
}


# =============================================================================
# TRANSFER OPTIMIZATION FUNCTIONS
# =============================================================================

#' Calculate effective lineup projection for a set of 10 players
#' Accounts for captain (1.25x on highest proj) and underdog (1.25x on cheapest)
calculate_lineup_projection <- function(players_df) {
  # players_df should have: player_name, projection, salary
  # Need at least 6 players with projections
  
  valid_players <- players_df %>% filter(!is.na(projection))
  
  if (nrow(valid_players) < 6) {
    return(list(total = NA, starters = NULL))
  }
  
  # Find best 6 using the same logic as select_best_six
  n <- nrow(valid_players)
  
  if (n == 6) {
    starters <- valid_players
  } else {
    # Try all combinations
    combos <- combn(n, 6)
    best_total <- -Inf
    best_idx <- NULL
    
    for (i in 1:ncol(combos)) {
      idx <- combos[, i]
      lineup <- valid_players[idx, ]
      
      if (any(is.na(lineup$salary))) next
      
      captain_proj <- max(lineup$projection)
      underdog_proj <- lineup$projection[which.min(lineup$salary)]
      
      total <- sum(lineup$projection) + captain_proj * 0.25 + underdog_proj * 0.25
      
      if (total > best_total) {
        best_total <- total
        best_idx <- idx
      }
    }
    
    if (is.null(best_idx)) {
      # Fallback to top 6 by projection
      starters <- valid_players %>% arrange(desc(projection)) %>% head(6)
      best_total <- sum(starters$projection) + max(starters$projection) * 0.25
    } else {
      starters <- valid_players[best_idx, ]
    }
  }
  
  # Calculate final projection
  captain_proj <- max(starters$projection)
  underdog_proj <- if (any(!is.na(starters$salary))) {
    starters$projection[which.min(starters$salary)]
  } else {
    0
  }
  
  total <- sum(starters$projection) + captain_proj * 0.25 + underdog_proj * 0.25
  
  return(list(total = total, starters = starters))
}

#' Find optimal transfers for a single roster
#' @param roster_players Data frame with current roster (player_name, salary, projection, match_key)
#' @param available_pool Data frame with all available players (player_name, salary, projection, match_key)
#' @param current_budget Remaining budget after selling current team at new prices
#' @param max_transfers Maximum number of transfers to evaluate
#' @param free_transfers Number of free transfers available
#' @return List with transfer recommendations for 1 to max_transfers
find_optimal_transfers <- function(roster_players, available_pool, current_budget, 
                                   max_transfers = 3, free_transfers = 1) {
  
  log_debug("find_optimal_transfers() - budget:", current_budget, "max:", max_transfers, 
            "free:", free_transfers, level = "DEBUG")
  
  config <- GOLF_SEASON_MANAGEMENT_CONFIG
  penalty <- config$transfer_penalty
  
  # Current lineup projection
  current_proj <- calculate_lineup_projection(roster_players)$total
  
  if (is.na(current_proj)) {
    log_debug("Cannot calculate current projection", level = "WARN")
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
  
  results <- list()
  
  # Evaluate each transfer count from 1 to max_transfers
  for (n_transfers in 1:min(max_transfers, nrow(roster_players))) {
    
    log_debug("Evaluating", n_transfers, "transfer(s)...", level = "DEBUG")
    
    # Calculate penalty
    excess_transfers <- max(0, n_transfers - free_transfers)
    total_penalty <- excess_transfers * penalty
    
    best_net_gain <- -Inf
    best_transfer <- NULL
    
    # For computational efficiency, use different strategies based on transfer count
    if (n_transfers == 1) {
      # Single transfer: evaluate all combinations
      for (i in 1:nrow(roster_players)) {
        player_out <- roster_players[i, ]
        budget_after_sale <- current_budget + player_out$salary
        
        # Find best player to bring in
        affordable <- transfer_pool %>%
          filter(salary <= budget_after_sale)
        
        if (nrow(affordable) == 0) next
        
        for (j in 1:nrow(affordable)) {
          player_in <- affordable[j, ]
          
          # Build new roster
          new_roster <- roster_players[-i, ] %>%
            bind_rows(player_in %>% select(player_name, salary, projection, match_key))
          
          new_proj <- calculate_lineup_projection(new_roster)$total
          
          if (is.na(new_proj)) next
          
          net_gain <- new_proj - current_proj + total_penalty
          
          if (net_gain > best_net_gain) {
            best_net_gain <- net_gain
            best_transfer <- list(
              out = list(player_out),
              in_ = list(player_in),
              current_proj = current_proj,
              new_proj = new_proj,
              gross_gain = new_proj - current_proj,
              penalty = total_penalty,
              net_gain = net_gain,
              budget_used = player_in$salary - player_out$salary
            )
          }
        }
      }
      
    } else if (n_transfers == 2) {
      # Double transfer: evaluate top candidates
      # Strategy: rank single transfer value, then try combinations
      
      single_values <- data.frame()
      
      for (i in 1:nrow(roster_players)) {
        player_out <- roster_players[i, ]
        budget_after_sale <- current_budget + player_out$salary
        
        affordable <- transfer_pool %>%
          filter(salary <= budget_after_sale) %>%
          arrange(desc(projection))
        
        if (nrow(affordable) > 0) {
          # Best replacement for this player
          best_in <- affordable[1, ]
          
          new_roster <- roster_players[-i, ] %>%
            bind_rows(best_in %>% select(player_name, salary, projection, match_key))
          
          new_proj <- calculate_lineup_projection(new_roster)$total
          
          if (!is.na(new_proj)) {
            single_values <- bind_rows(single_values, data.frame(
              out_idx = i,
              out_name = player_out$player_name,
              out_salary = player_out$salary,
              in_name = best_in$player_name,
              in_salary = best_in$salary,
              in_proj = best_in$projection,
              in_key = best_in$match_key,
              value = new_proj - current_proj,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      if (nrow(single_values) >= 2) {
        # Sort by value
        single_values <- single_values %>% arrange(desc(value))
        
        # Try top combinations
        for (a in 1:min(5, nrow(single_values) - 1)) {
          for (b in (a + 1):min(6, nrow(single_values))) {
            t1 <- single_values[a, ]
            t2 <- single_values[b, ]
            
            # Skip if same player out or in
            if (t1$out_idx == t2$out_idx) next
            if (t1$in_key == t2$in_key) next
            
            # Check budget
            budget_change <- (t1$in_salary - t1$out_salary) + (t2$in_salary - t2$out_salary)
            if (budget_change > 0) next  # Would exceed budget
            
            # Build new roster
            new_roster <- roster_players[-c(t1$out_idx, t2$out_idx), ]
            
            player_in_1 <- transfer_pool %>% filter(match_key == t1$in_key) %>% head(1)
            player_in_2 <- transfer_pool %>% filter(match_key == t2$in_key) %>% head(1)
            
            new_roster <- new_roster %>%
              bind_rows(player_in_1 %>% select(player_name, salary, projection, match_key)) %>%
              bind_rows(player_in_2 %>% select(player_name, salary, projection, match_key))
            
            new_proj <- calculate_lineup_projection(new_roster)$total
            
            if (is.na(new_proj)) next
            
            net_gain <- new_proj - current_proj + total_penalty
            
            if (net_gain > best_net_gain) {
              best_net_gain <- net_gain
              best_transfer <- list(
                out = list(roster_players[t1$out_idx, ], roster_players[t2$out_idx, ]),
                in_ = list(player_in_1, player_in_2),
                current_proj = current_proj,
                new_proj = new_proj,
                gross_gain = new_proj - current_proj,
                penalty = total_penalty,
                net_gain = net_gain,
                budget_used = budget_change
              )
            }
          }
        }
      }
      
    } else {
      # 3+ transfers: use greedy approach with refinement
      # Start with top N single transfers
      
      single_values <- data.frame()
      
      for (i in 1:nrow(roster_players)) {
        player_out <- roster_players[i, ]
        budget_after_sale <- current_budget + player_out$salary
        
        affordable <- transfer_pool %>%
          filter(salary <= budget_after_sale) %>%
          arrange(desc(projection))
        
        if (nrow(affordable) > 0) {
          best_in <- affordable[1, ]
          
          single_values <- bind_rows(single_values, data.frame(
            out_idx = i,
            out_name = player_out$player_name,
            out_salary = player_out$salary,
            in_name = best_in$player_name,
            in_salary = best_in$salary,
            in_proj = best_in$projection,
            in_key = best_in$match_key,
            proj_gain = best_in$projection - ifelse(is.na(player_out$projection), 0, player_out$projection),
            stringsAsFactors = FALSE
          ))
        }
      }
      
      if (nrow(single_values) >= n_transfers) {
        single_values <- single_values %>% arrange(desc(proj_gain))
        
        # Try taking top N transfers
        top_transfers <- single_values[1:n_transfers, ]
        
        # Check for duplicates
        if (length(unique(top_transfers$out_idx)) == n_transfers && 
            length(unique(top_transfers$in_key)) == n_transfers) {
          
          # Check budget
          budget_change <- sum(top_transfers$in_salary) - sum(top_transfers$out_salary)
          
          if (budget_change <= 0) {
            # Build new roster
            new_roster <- roster_players[-top_transfers$out_idx, ]
            
            for (k in 1:n_transfers) {
              player_in <- transfer_pool %>% filter(match_key == top_transfers$in_key[k]) %>% head(1)
              new_roster <- bind_rows(new_roster, player_in %>% select(player_name, salary, projection, match_key))
            }
            
            new_proj <- calculate_lineup_projection(new_roster)$total
            
            if (!is.na(new_proj)) {
              net_gain <- new_proj - current_proj + total_penalty
              
              if (net_gain > best_net_gain) {
                best_net_gain <- net_gain
                
                out_list <- lapply(1:n_transfers, function(k) roster_players[top_transfers$out_idx[k], ])
                in_list <- lapply(1:n_transfers, function(k) {
                  transfer_pool %>% filter(match_key == top_transfers$in_key[k]) %>% head(1)
                })
                
                best_transfer <- list(
                  out = out_list,
                  in_ = in_list,
                  current_proj = current_proj,
                  new_proj = new_proj,
                  gross_gain = new_proj - current_proj,
                  penalty = total_penalty,
                  net_gain = net_gain,
                  budget_used = budget_change
                )
              }
            }
          }
        }
      }
    }
    
    results[[paste0("transfer_", n_transfers)]] <- best_transfer
    
    if (!is.null(best_transfer)) {
      log_debug(sprintf("Best %d-transfer: +%.1f pts (gross: +%.1f, penalty: %.0f)", 
                        n_transfers, best_transfer$net_gain, best_transfer$gross_gain, 
                        best_transfer$penalty), level = "INFO")
    }
  }
  
  return(results)
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
  salary_weeks <- tryCatch(get_salary_weeks(), error = function(e) c("Week 1", "Week 2"))
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Season Long Management"),
      tags$p(class = "text-muted", "Manage your 5 rosters - optimizes CPT (highest proj) + DOG (cheapest) bonuses")
    ),
    
    # =========================================================================
    # CURRENT WEEK SETTINGS
    # =========================================================================
    ui_card(
      title = "Current Week Settings",
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
    # ROSTER LINEUPS
    # =========================================================================
    ui_card(
      title = "Recommended Lineups",
      color = GOLF_CARD_COLOR,
      
      uiOutput(ns("rosters_display"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # TRANSFER PLANNING (NEW SECTION)
    # =========================================================================
    ui_card(
      title = "Transfer Planning",
      color = GOLF_CARD_COLOR,
      
      # Transfer settings row
      fluidRow(
        column(3,
               selectizeInput(ns("next_tournament_select"), "Next Week Tournament",
                              choices = if (length(tournaments) > 0) tournaments else c("No tournaments found" = ""),
                              selected = if (length(tournaments) > 1) tournaments[2] else NULL
               )
        ),
        column(2,
               selectizeInput(ns("next_salary_week"), "Salary Week",
                              choices = salary_weeks,
                              selected = if (length(salary_weeks) > 1) salary_weeks[2] else salary_weeks[1]
               )
        ),
        column(2,
               numericInput(ns("max_transfers"), "Max Transfers",
                            value = 3, min = 1, max = 10, step = 1
               )
        ),
        column(2,
               div(style = "margin-top: 25px;",
                   actionButton(ns("calc_transfers_btn"), "Calculate", 
                                class = "btn btn-primary w-100", icon = icon("calculator"))
               )
        )
      ),
      
      tags$hr(),
      
      # Roster-specific transfers available
      div(
        style = "margin-bottom: 1rem;",
        tags$h5(style = "margin-bottom: 0.5rem; font-weight: 700;", "Transfers Available per Roster"),
        tags$p(class = "text-muted", style = "font-size: 0.8rem; margin-bottom: 0.75rem;",
               "1 free transfer per week. Excess transfers cost -20 points each."
        ),
        fluidRow(
          column(2, numericInput(ns("transfers_r1"), "Roster 1", value = 1, min = 0, max = 31)),
          column(2, numericInput(ns("transfers_r2"), "Roster 2", value = 1, min = 0, max = 31)),
          column(2, numericInput(ns("transfers_r3"), "Roster 3", value = 1, min = 0, max = 31)),
          column(2, numericInput(ns("transfers_r4"), "Roster 4", value = 1, min = 0, max = 31)),
          column(2, numericInput(ns("transfers_r5"), "Roster 5", value = 1, min = 0, max = 31))
        )
      ),
      
      tags$hr(),
      
      # Transfer recommendations display
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
    # REACTIVE VALUES
    # =========================================================================
    rv <- reactiveValues(
      roster_data = NULL,
      projections = NULL,
      matched_data = NULL,
      
      # Transfer planning
      next_projections = NULL,
      next_salaries = NULL,
      current_salaries = NULL,
      transfer_results = NULL,
      roster_budgets = list()
    )
    
    # =========================================================================
    # LOAD CURRENT WEEK DATA
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
    # CALCULATE TRANSFERS
    # =========================================================================
    observeEvent(input$calc_transfers_btn, {
      req(input$next_tournament_select, input$next_salary_week)
      req(input$next_tournament_select != "")
      req(rv$roster_data)
      
      log_debug(">>> Calculate transfers clicked", level = "INFO")
      log_debug(">>> Next tournament:", input$next_tournament_select, level = "INFO")
      log_debug(">>> Salary week:", input$next_salary_week, level = "INFO")
      
      showNotification("Loading next week data...", type = "message", duration = 2)
      
      # Load next week projections
      next_proj <- load_tournament_projections(input$next_tournament_select)
      if (is.null(next_proj)) {
        showNotification("Failed to load next week projections", type = "error")
        return()
      }
      rv$next_projections <- next_proj
      
      # Load next week salaries
      next_sal <- load_week_salaries(input$next_salary_week)
      if (is.null(next_sal)) {
        showNotification("Failed to load next week salaries", type = "error")
        return()
      }
      rv$next_salaries <- next_sal
      
      # Also load current week salaries for budget tracking
      current_week_num <- as.numeric(gsub("Week ", "", input$week_select))
      current_sal_week <- paste0("Week ", current_week_num)
      current_sal <- load_week_salaries(current_sal_week)
      rv$current_salaries <- current_sal
      
      # Get transfers available per roster
      transfers_available <- list(
        r1 = input$transfers_r1 %||% 1,
        r2 = input$transfers_r2 %||% 1,
        r3 = input$transfers_r3 %||% 1,
        r4 = input$transfers_r4 %||% 1,
        r5 = input$transfers_r5 %||% 1
      )
      
      max_transfers <- input$max_transfers %||% 3
      
      showNotification("Calculating optimal transfers...", type = "message", duration = 3)
      
      # Process each roster
      rosters <- unique(rv$roster_data$roster)
      results <- list()
      budgets <- list()
      
      for (roster_name in rosters) {
        log_debug("Processing roster:", roster_name, level = "INFO")
        
        # Get roster players
        roster_players <- rv$roster_data %>%
          filter(roster == roster_name)
        
        # Match to next week salaries
        roster_with_next_sal <- roster_players %>%
          left_join(
            next_sal %>% select(match_key, next_salary = salary),
            by = "match_key"
          )
        
        # Match to next week projections
        roster_with_proj <- roster_with_next_sal %>%
          left_join(
            next_proj %>% select(match_key, projection, proj_salary = salary),
            by = "match_key"
          ) %>%
          mutate(
            # Use next week salary from salaries sheet, fallback to projection salary
            salary = coalesce(next_salary, proj_salary)
          )
        
        # Calculate budget
        team_value <- sum(roster_with_proj$salary, na.rm = TRUE)
        remaining_budget <- GOLF_SEASON_MANAGEMENT_CONFIG$budget - team_value
        
        budgets[[roster_name]] <- list(
          team_value = team_value,
          remaining_budget = remaining_budget
        )
        
        log_debug(sprintf("Roster %s: Team value = %.1fM, Remaining = %.1fM", 
                          roster_name, team_value, remaining_budget), level = "INFO")
        
        # Get free transfers for this roster
        roster_idx <- which(rosters == roster_name)
        free_transfers <- transfers_available[[paste0("r", roster_idx)]]
        
        # Build available pool (players with projections and salaries)
        available_pool <- next_proj %>%
          left_join(
            next_sal %>% select(match_key, pool_salary = salary),
            by = "match_key"
          ) %>%
          mutate(salary = coalesce(pool_salary, salary)) %>%
          filter(!is.na(projection) & !is.na(salary))
        
        # Find optimal transfers
        roster_for_opt <- roster_with_proj %>%
          select(player_name, salary, projection, match_key)
        
        transfer_result <- find_optimal_transfers(
          roster_players = roster_for_opt,
          available_pool = available_pool,
          current_budget = remaining_budget,
          max_transfers = max_transfers,
          free_transfers = free_transfers
        )
        
        results[[roster_name]] <- transfer_result
      }
      
      rv$transfer_results <- results
      rv$roster_budgets <- budgets
      
      showNotification("Transfer analysis complete!", type = "message")
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
    # ROSTERS DISPLAY
    # =========================================================================
    output$rosters_display <- renderUI({
      if (is.null(rv$matched_data)) {
        return(div(
          class = "text-muted text-center py-4",
          "Load data to see recommended lineups"
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
              
              # Determine row background
              row_bg <- if (is_unmatched) {
                "rgba(208, 135, 112, 0.25)"  # Light coral for unmatched
              } else if (is_captain) {
                "rgba(180, 142, 173, 0.25)"  # Light plum for captain
              } else if (is_underdog) {
                "rgba(235, 203, 139, 0.25)"  # Light gold for underdog
              } else if (is_bench) {
                "var(--bg-secondary)"
              } else {
                "var(--bg-tertiary)"
              }
              
              # Determine badge text and color
              badge_text <- if (is_unmatched) {
                "?"
              } else if (is_captain) {
                "CPT"
              } else if (is_underdog) {
                "DOG"
              } else {
                player$lineup_rank
              }
              
              badge_bg <- if (is_unmatched) {
                "background: var(--accent-coral); color: white;"
              } else if (is_captain) {
                "background: #B48EAD; color: white;"
              } else if (is_underdog) {
                "background: #EBCB8B; color: var(--text-primary);"
              } else if (is_bench) {
                "background: var(--text-muted); color: white;"
              } else {
                "background: var(--accent-sage); color: white;"
              }
              
              div(
                style = sprintf(
                  "display: flex; align-items: center; gap: 0.4rem; padding: 0.3rem 0.4rem; background: %s; border-radius: 4px; %s",
                  row_bg,
                  if (is_bench && !is_unmatched) "opacity: 0.6;" else ""
                ),
                
                # Rank/Role badge
                div(
                  style = sprintf(
                    "width: 28px; height: 24px; border-radius: 4px; display: flex; align-items: center; justify-content: center; font-size: 0.55rem; font-weight: 700; %s",
                    badge_bg
                  ),
                  badge_text
                ),
                
                # Player name
                div(
                  style = sprintf(
                    "flex: 1; font-weight: 600; font-size: 0.8rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; %s",
                    if (is_unmatched) "font-style: italic;" else ""
                  ),
                  player$player_name
                ),
                
                # Salary (show for starters to explain underdog)
                if (!is_unmatched && player$is_starter) {
                  div(
                    style = "text-align: right; width: 40px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "SAL"),
                    div(
                      style = sprintf("font-size: 0.7rem; %s", if (is_underdog) "color: #EBCB8B; font-weight: 700;" else "color: var(--text-secondary);"),
                      if ("salary" %in% names(player) && !is.na(player$salary)) sprintf("$%.1f", player$salary) else "Ã¢â‚¬â€"
                    )
                  )
                },
                
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
      if (is.null(rv$transfer_results)) {
        return(div(
          class = "text-muted text-center py-4",
          icon("info-circle"), " Select next week tournament and salaries, then click Calculate"
        ))
      }
      
      rosters <- names(rv$transfer_results)
      
      # Helper to create transfer recommendation card
      create_transfer_card <- function(roster_name) {
        results <- rv$transfer_results[[roster_name]]
        budget_info <- rv$roster_budgets[[roster_name]]
        
        if (is.null(results)) {
          return(div(
            style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem;",
            tags$h5(style = "margin: 0;", roster_name),
            div(class = "text-muted", "No transfer data available")
          ))
        }
        
        # Budget summary
        budget_div <- div(
          style = "background: var(--bg-secondary); border-radius: 6px; padding: 0.5rem; margin-bottom: 0.75rem;",
          div(
            style = "display: flex; justify-content: space-between; font-size: 0.75rem;",
            span("Team Value:"),
            span(style = "font-weight: 600;", sprintf("$%.1fM", budget_info$team_value))
          ),
          div(
            style = "display: flex; justify-content: space-between; font-size: 0.75rem;",
            span("Available Budget:"),
            span(
              style = sprintf("font-weight: 600; color: %s;", 
                              if (budget_info$remaining_budget >= 0) "var(--accent-sage)" else "var(--accent-coral)"),
              sprintf("$%.1fM", budget_info$remaining_budget)
            )
          )
        )
        
        # Create transfer recommendation rows
        create_transfer_row <- function(n_transfers, transfer_data) {
          if (is.null(transfer_data)) {
            return(div(
              style = "padding: 0.4rem; background: var(--bg-secondary); border-radius: 4px; margin-bottom: 0.25rem; opacity: 0.5;",
              div(style = "font-size: 0.7rem; font-weight: 600;", sprintf("%d Transfer(s)", n_transfers)),
              div(style = "font-size: 0.65rem; color: var(--text-muted);", "No beneficial transfer found")
            ))
          }
          
          # Build OUT Ã¢â€ â€™ IN text
          out_names <- sapply(transfer_data$out, function(x) x$player_name)
          in_names <- sapply(transfer_data$in_, function(x) x$player_name)
          
          # Determine color based on net gain
          gain_color <- if (transfer_data$net_gain > 0) "var(--accent-sage)" else "var(--accent-coral)"
          
          div(
            style = "padding: 0.5rem; background: white; border: 1px solid var(--border); border-radius: 6px; margin-bottom: 0.5rem;",
            
            # Header row
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.25rem;",
              span(style = "font-size: 0.75rem; font-weight: 700;", sprintf("%d Transfer(s)", n_transfers)),
              span(
                style = sprintf("font-size: 0.85rem; font-weight: 700; color: %s;", gain_color),
                sprintf("%+.1f pts", transfer_data$net_gain)
              )
            ),
            
            # Transfer details
            div(
              style = "font-size: 0.7rem;",
              lapply(seq_along(out_names), function(i) {
                div(
                  style = "display: flex; align-items: center; gap: 0.25rem; margin-bottom: 0.1rem;",
                  span(style = "color: var(--accent-coral); font-weight: 600;", "OUT:"),
                  span(out_names[i]),
                  span(style = "color: var(--text-muted);", "Ã¢â€ â€™"),
                  span(style = "color: var(--accent-sage); font-weight: 600;", "IN:"),
                  span(in_names[i])
                )
              })
            ),
            
            # Stats row
            div(
              style = "display: flex; gap: 1rem; margin-top: 0.25rem; font-size: 0.6rem; color: var(--text-muted);",
              span(sprintf("Gross: %+.1f", transfer_data$gross_gain)),
              if (transfer_data$penalty < 0) {
                span(style = "color: var(--accent-coral);", sprintf("Penalty: %.0f", transfer_data$penalty))
              },
              span(sprintf("Budget: %+.1fM", -transfer_data$budget_used))
            )
          )
        }
        
        # Build the card
        div(
          style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
          
          # Header
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px dashed var(--bg-secondary);",
            span(style = "font-weight: 800; text-transform: uppercase; font-size: 0.9rem;", roster_name),
            div(
              style = "font-size: 0.65rem; text-transform: uppercase; color: var(--text-muted);",
              "Transfer Analysis"
            )
          ),
          
          budget_div,
          
          # Transfer recommendations
          div(
            lapply(1:length(results), function(i) {
              n <- as.numeric(gsub("transfer_", "", names(results)[i]))
              create_transfer_row(n, results[[i]])
            })
          )
        )
      }
      
      # Layout: 3 columns
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
        lapply(rosters, create_transfer_card)
      )
    })
    
    
  })
}

cat("Golf Season Management module loaded: golf_season_management_ui(), golf_season_management_server()\n")
cat("  Optimizes lineup selection with CPT (highest proj, 1.25x) + DOG (cheapest, 1.25x) bonuses\n")
cat("  Transfer Planning: Analyzes optimal 1-N transfers for next week\n")