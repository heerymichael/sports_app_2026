# =============================================================================
# Golf Configuration
# 
# Configuration for Golf Season Long module:
# - Contest rules (Underdog Scramble)
# - Name matching utilities
# - Weighted projection calculations
# - Roster validation
# =============================================================================

# -----------------------------------------------------------------------------
# Contest Configuration - Underdog Scramble
# -----------------------------------------------------------------------------

GOLF_SCRAMBLE_CONFIG <- list(
  # Roster structure
  roster_size = 10,           # Total players in roster
  bench_size = 4,             # Players on bench
  active_size = 6,            # Active lineup (roster - bench)
  
  # Budget
  budget = 100,               # 100M budget
  
  # Scoring multipliers
  captain_multiplier = 1.25,   # Captain scores 1.25x
  underdog_multiplier = 1.25,  # Cheapest player scores 1.25x
  
  # Transfer rules
  free_transfers_per_week = 1,
  max_saved_transfers = 31,
  transfer_penalty = -20,      # Points deducted per extra transfer
  
  # Season
  total_gameweeks = 32,
  
  # Payout
  payout_percentage = 10       # Top 10% paid
)

# -----------------------------------------------------------------------------
# Quarter Weighting Profiles
# -----------------------------------------------------------------------------

#' Get weighting profile for projections
#' @param profile Character: "early_season", "mid_season", "late_season", "equal"
#' @return Named numeric vector of weights (Q1, Q2, Q3, Q4)
get_quarter_weights <- function(profile = "early_season") {
  weights <- switch(profile,
                    "early_season" = c(Q1 = 0.35, Q2 = 0.30, Q3 = 0.20, Q4 = 0.15),
                    "mid_season" = c(Q1 = 0.20, Q2 = 0.30, Q3 = 0.30, Q4 = 0.20),
                    "late_season" = c(Q1 = 0.15, Q2 = 0.20, Q3 = 0.30, Q4 = 0.35),
                    "equal" = c(Q1 = 0.25, Q2 = 0.25, Q3 = 0.25, Q4 = 0.25),
                    # Default to early season
                    c(Q1 = 0.35, Q2 = 0.30, Q3 = 0.20, Q4 = 0.15)
  )
  
  # Ensure weights sum to 1
  weights / sum(weights)
}

#' Calculate weighted projection score
#' @param q1 Quarter 1 projection
#' @param q2 Quarter 2 projection
#' @param q3 Quarter 3 projection
#' @param q4 Quarter 4 projection
#' @param weights Named vector of quarter weights
#' @return Weighted projection score
calculate_weighted_projection <- function(q1, q2, q3, q4, weights = NULL) {
  if (is.null(weights)) {
    weights <- get_quarter_weights("early_season")
  }
  
  # Handle NA values
  q1 <- ifelse(is.na(q1), 0, q1)
  q2 <- ifelse(is.na(q2), 0, q2)
  q3 <- ifelse(is.na(q3), 0, q3)
  q4 <- ifelse(is.na(q4), 0, q4)
  
  weighted_score <- q1 * weights["Q1"] + 
    q2 * weights["Q2"] + 
    q3 * weights["Q3"] + 
    q4 * weights["Q4"]
  
  round(weighted_score, 1)
}

# -----------------------------------------------------------------------------
# Name Matching Utilities
# -----------------------------------------------------------------------------

#' Normalize golfer name for matching
#' @param name Character string of player name
#' @return Normalized name for matching
normalize_golfer_name <- function(name) {
  if (is.na(name) || name == "") return("")
  
  # Convert to lowercase
  name <- tolower(trimws(name))
  
  # Handle "Last, First" format -> "First Last"
  if (grepl(",", name)) {
    parts <- strsplit(name, ",")[[1]]
    if (length(parts) == 2) {
      name <- paste(trimws(parts[2]), trimws(parts[1]))
    }
  }
  
  # Remove accents/diacritics
  name <- iconv(name, to = "ASCII//TRANSLIT")
  
  # Remove common suffixes
  name <- gsub("\\s+(jr|sr|ii|iii|iv)\\.?$", "", name)
  
  # Remove punctuation including hyphens and periods
  name <- gsub("[^a-z0-9\\s]", " ", name)
  
  # Collapse multiple spaces
  name <- gsub("\\s+", " ", name)
  
  name <- trimws(name)
  
  # Apply name corrections
  if (exists("GOLF_NAME_CORRECTIONS") && name %in% names(GOLF_NAME_CORRECTIONS)) {
    name <- GOLF_NAME_CORRECTIONS[[name]]
  }
  
  name
}

#' Create matching key from name
#' @param name Character string of player name
#' @return Simplified key for fuzzy matching
create_name_key <- function(name) {
  normalized <- normalize_golfer_name(name)
  
  # Split into parts
  parts <- strsplit(normalized, " ")[[1]]
  
  # If we have first and last name, use first 3 chars of each
  if (length(parts) >= 2) {
    first <- substr(parts[1], 1, 3)
    last <- substr(parts[length(parts)], 1, 4)
    key <- paste0(first, "_", last)
  } else if (length(parts) == 1) {
    key <- substr(parts[1], 1, 6)
  } else {
    key <- ""
  }
  
  key
}

# =============================================================================
# GOLF NAME RECONCILIATION
# =============================================================================
#
# This section handles name mismatches between salary files and rankings files.
# 
# HOW TO ADD NEW CORRECTIONS:
# ---------------------------
# 1. Find the unmatched player in the app (shown in "Unmatched Players" debug table)
# 2. Look up their name in the rankings file
# 3. Add a new line below in the format:
#    "salary name" = "rankings name"
#
# IMPORTANT: Names should be LOWERCASE with spaces (no hyphens, no periods)
# The system automatically normalizes names before lookup, so:
#   - "Sung-Jae Im" becomes "sung jae im"
#   - "J.T. Poston" becomes "jt poston"
#
# EXAMPLE:
# If salary file has "Alexander Noren" but rankings has "Alex Noren":
#   "alexander noren" = "alex noren"
#
# =============================================================================

GOLF_NAME_CORRECTIONS <- list(
  
  
  # ---------------------------------------------------------------------------
  # ABBREVIATED/NICKNAME DIFFERENCES
  # Salary file uses full name, rankings uses nickname or vice versa
  # ---------------------------------------------------------------------------
  
  "alexander noren"      = "alex noren",
  "christopher gotterup" = "chris gotterup",
  "nicolas echavarria"   = "nico echavarria",
  "sam stevens"          = "samuel stevens",
  "henry lebioda"        = "hank lebioda",
  "kristoffer ventura"   = "kris ventura",
  "matthias schmid"      = "matti schmid",
  "zach bauchou"         = "zachary bauchou",
  
  
  # ---------------------------------------------------------------------------
  # SPACING/FORMATTING DIFFERENCES
  # Names with hyphens, spaces, or different formatting between sources
  # Note: Hyphens are converted to spaces during normalization
  # ---------------------------------------------------------------------------
  
  "byeong hun an"        = "byeong hun an",
  "hao tong li"          = "haotong li",
  "sung jae im"          = "sungjae im",
  "ze cheng dou"         = "zecheng dou",
  "seong hyeon kim"      = "seonghyeon kim",
  
  
  # ---------------------------------------------------------------------------
  # SUFFIX/PREFIX DIFFERENCES
  # Names with Jr, III, middle initials, etc.
  # ---------------------------------------------------------------------------
  
  "jordan l smith"       = "jordan smith",
  
  
  # ---------------------------------------------------------------------------
  # EXTENDED NAME DIFFERENCES
  # Names where one source has additional parts
  # ---------------------------------------------------------------------------
  
  "adrien dumont"        = "adrien dumont de chassart"
  
  
  # ---------------------------------------------------------------------------
  # ADD NEW CORRECTIONS BELOW THIS LINE
  # ---------------------------------------------------------------------------
  # Format: "salary name lowercase" = "rankings name lowercase"
  # 
  # Example:
  # "jon rahm"             = "jonathan rahm",
  # "tiger woods"          = "eldrick woods"
  # ---------------------------------------------------------------------------
  
)

#' Apply name corrections
#' @param names Vector of names to correct
#' @return Corrected names
apply_golf_name_corrections <- function(names) {
  corrections <- GOLF_NAME_CORRECTIONS
  
  sapply(names, function(name) {
    if (name %in% names(corrections)) {
      corrections[[name]]
    } else {
      name
    }
  }, USE.NAMES = FALSE)
}

#' Match salary names to rankings names
#' @param salary_df Data frame with salary data
#' @param rankings_df Data frame with rankings data
#' @param salary_name_col Name column in salary data
#' @param rankings_name_col Name column in rankings data
#' @return Salary data frame with matched rankings info
match_golfer_names <- function(salary_df, rankings_df, 
                               salary_name_col = "Name",
                               salary_fname_col = "FName",
                               rankings_name_col = "Golfer") {
  
  # Create full name from salary data (First Last format)
  salary_df <- salary_df %>%
    mutate(
      full_name = paste(!!sym(salary_fname_col), !!sym(salary_name_col)),
      full_name = trimws(full_name),
      name_normalized = normalize_golfer_name(full_name),
      name_key = sapply(full_name, create_name_key)
    )
  
  # Create normalized names for rankings
  rankings_df <- rankings_df %>%
    mutate(
      name_normalized = normalize_golfer_name(!!sym(rankings_name_col)),
      name_key = sapply(!!sym(rankings_name_col), create_name_key)
    )
  
  # Try exact match on normalized names first
  matched <- salary_df %>%
    left_join(
      rankings_df %>% select(-any_of("name_key")),
      by = "name_normalized",
      suffix = c("", "_rankings")
    )
  
  # For unmatched, try key-based matching
  unmatched_mask <- is.na(matched$Rank)
  if (any(unmatched_mask)) {
    unmatched <- matched %>% filter(unmatched_mask) %>% select(names(salary_df))
    
    # Match on key
    key_matched <- unmatched %>%
      left_join(
        rankings_df %>% select(-any_of("name_normalized")),
        by = "name_key",
        suffix = c("", "_rankings")
      )
    
    # Combine
    matched <- bind_rows(
      matched %>% filter(!unmatched_mask),
      key_matched
    )
  }
  
  # Report matching stats
  match_rate <- sum(!is.na(matched$Rank)) / nrow(matched) * 100
  if (exists("log_debug")) {
    log_debug(sprintf("Name matching: %.1f%% matched (%d/%d)", 
                      match_rate, sum(!is.na(matched$Rank)), nrow(matched)), 
              level = "INFO")
  }
  
  matched
}

# -----------------------------------------------------------------------------
# Value Calculations
# -----------------------------------------------------------------------------

#' Calculate points per million value
#' @param projection Projected points
#' @param price Player price/salary
#' @return Points per million
calculate_value <- function(projection, price) {
  if (is.na(projection) || is.na(price) || price == 0) {
    return(NA)
  }
  round(projection / price, 2)
}

#' Calculate value vs average
#' @param player_value Player's value score
#' @param all_values Vector of all player values
#' @return Percentage above/below average
calculate_value_vs_avg <- function(player_value, all_values) {
  avg_value <- mean(all_values, na.rm = TRUE)
  if (is.na(player_value) || avg_value == 0) {
    return(NA)
  }
  round((player_value - avg_value) / avg_value * 100, 1)
}

# -----------------------------------------------------------------------------
# Roster Validation
# -----------------------------------------------------------------------------

#' Validate roster for Underdog Scramble rules
#' @param roster Data frame of selected players
#' @param config List of contest configuration
#' @return List with valid (logical) and message (character)
validate_scramble_roster <- function(roster, config = GOLF_SCRAMBLE_CONFIG) {
  
  messages <- c()
  valid <- TRUE
  
  # Check roster size
  if (nrow(roster) != config$roster_size) {
    valid <- FALSE
    messages <- c(messages, sprintf(
      "Roster must have exactly %d players (currently %d)",
      config$roster_size, nrow(roster)
    ))
  }
  
  # Check budget
  total_salary <- sum(roster$Price, na.rm = TRUE)
  if (total_salary > config$budget) {
    valid <- FALSE
    messages <- c(messages, sprintf(
      "Over budget: %.1fM / %.0fM",
      total_salary, config$budget
    ))
  }
  
  # Check for captain selection
  if (!"is_captain" %in% names(roster) || sum(roster$is_captain) != 1) {
    valid <- FALSE
    messages <- c(messages, "Exactly one player must be designated as Captain")
  }
  
  if (length(messages) == 0) {
    messages <- "Roster is valid"
  }
  
  list(
    valid = valid,
    message = paste(messages, collapse = "; "),
    total_salary = total_salary,
    remaining_budget = config$budget - total_salary
  )
}

#' Identify underdog (cheapest player)
#' @param roster Data frame of selected players
#' @return PlayerID of cheapest player
identify_underdog <- function(roster) {
  if (nrow(roster) == 0) return(NULL)
  
  cheapest <- roster %>%
    arrange(Price) %>%
    slice(1)
  
  cheapest$PlayerID
}

# -----------------------------------------------------------------------------
# Transfer Analysis
# -----------------------------------------------------------------------------

#' Calculate transfer value
#' @param player_out Player being transferred out
#' @param player_in Player being transferred in
#' @param weeks_remaining Gameweeks remaining
#' @return List with transfer details and value
calculate_transfer_value <- function(player_out, player_in, weeks_remaining) {
  
  # Projected points difference over remaining season
  proj_diff <- player_in$weighted_proj - player_out$weighted_proj
  
  # Normalize by weeks remaining
  weekly_diff <- proj_diff / max(weeks_remaining, 1)
  
  # Salary difference (positive = savings)
  salary_diff <- player_out$Price - player_in$Price
  
  list(
    player_out = player_out$full_name,
    player_in = player_in$full_name,
    proj_difference = round(proj_diff, 1),
    weekly_uplift = round(weekly_diff, 2),
    salary_change = salary_diff,
    recommended = proj_diff > 20  # Simple threshold
  )
}

# -----------------------------------------------------------------------------
# Logging Helper
# -----------------------------------------------------------------------------

#' Log golf module messages
golf_log <- function(..., level = "INFO") {
  if (exists("log_debug")) {
    log_debug(paste0("[GOLF] ", ...), level = level)
  } else {
    cat(sprintf("[%s] [GOLF] %s\n", level, paste0(...)))
  }
}

# =============================================================================
# GOLF PLAYER HEADSHOTS
# =============================================================================
#
# Headshot images are stored in data/golf_2025/player_headshots/
# The headshot mapping file links player names to image URLs
#
# Expected file: data/golf_2025/player_headshots/player-headshots.csv
# Expected columns: Name (or Golfer), HeadshotURL (or similar)
# =============================================================================

# Global cache for headshot data
GOLF_HEADSHOTS_CACHE <- NULL

#' Load golf player headshots from CSV
#' @param force_refresh Logical, whether to reload even if cached
#' @return Data frame with player names and headshot URLs
load_golf_headshots <- function(force_refresh = FALSE) {
  
  # Return cached if available
  if (!force_refresh && !is.null(GOLF_HEADSHOTS_CACHE)) {
    return(GOLF_HEADSHOTS_CACHE)
  }
  
  # Try multiple possible file paths and names
  possible_paths <- c(
    "data/golf_2025/player_headshots/player-headshots.csv",
    "data/golf_2025/player_headshots/player_headshots.csv",
    "data/golf_2025/player_headshots/headshots.csv"
  )
  
  headshots_path <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      headshots_path <- path
      break
    }
  }
  
  if (is.null(headshots_path)) {
    golf_log("No headshots file found in data/golf_2025/player_headshots/", level = "WARN")
    return(data.frame(name_normalized = character(), headshot_url = character()))
  }
  
  tryCatch({
    headshots_df <- read_csv(headshots_path, show_col_types = FALSE)
    golf_log("Loaded headshots file:", headshots_path, "-", nrow(headshots_df), "entries", level = "INFO")
    
    # Normalize column names to lowercase for easier matching
    names(headshots_df) <- tolower(names(headshots_df))
    
    # Check for the specific format: Name (last), FName (first), Headshot (URL)
    if (all(c("name", "fname", "headshot") %in% names(headshots_df))) {
      # Format: Name=Last, FName=First, Headshot=URL (like salary file format)
      result <- headshots_df %>%
        mutate(
          # Combine first and last name
          player_name = paste(fname, name),
          player_name = trimws(player_name),
          headshot_url = headshot,
          # Normalize for matching
          name_normalized = sapply(player_name, function(n) {
            n <- tolower(trimws(n))
            n <- gsub("[^a-z0-9 ]", " ", n)
            n <- gsub("\\s+", " ", n)
            trimws(n)
          })
        ) %>%
        filter(!is.na(headshot_url), headshot_url != "") %>%
        select(player_name, name_normalized, headshot_url)
      
      golf_log("Processed headshots with Name/FName format:", nrow(result), "entries", level = "INFO")
      
    } else {
      # Fallback: Try to find name and URL columns generically
      
      # Find name column
      name_col <- NULL
      for (col in c("golfer", "player", "player_name", "playername", "name")) {
        if (col %in% names(headshots_df)) {
          name_col <- col
          break
        }
      }
      
      # Find URL column
      url_col <- NULL
      for (col in c("headshot", "headshoturl", "headshot_url", "url", "image", "image_url", "imageurl", "photo", "photo_url")) {
        if (col %in% names(headshots_df)) {
          url_col <- col
          break
        }
      }
      
      if (is.null(name_col) || is.null(url_col)) {
        golf_log("Could not identify name/URL columns. Found:", paste(names(headshots_df), collapse = ", "), level = "ERROR")
        return(data.frame(name_normalized = character(), headshot_url = character()))
      }
      
      # Normalize and standardize
      result <- headshots_df %>%
        select(player_name = !!sym(name_col), headshot_url = !!sym(url_col)) %>%
        mutate(
          name_normalized = sapply(player_name, function(n) {
            n <- tolower(trimws(n))
            n <- gsub("[^a-z0-9 ]", " ", n)
            n <- gsub("\\s+", " ", n)
            trimws(n)
          })
        ) %>%
        filter(!is.na(headshot_url), headshot_url != "")
      
      golf_log("Processed headshots with generic format:", nrow(result), "entries", level = "INFO")
    }
    
    # Cache the result
    GOLF_HEADSHOTS_CACHE <<- result
    
    return(result)
    
  }, error = function(e) {
    golf_log("Error loading headshots:", e$message, level = "ERROR")
    return(data.frame(name_normalized = character(), headshot_url = character()))
  })
}

#' Get headshot URL for a golfer
#' @param player_name Character, the player's name
#' @return Character, the headshot URL or NULL if not found
get_golf_headshot <- function(player_name) {
  if (is.na(player_name) || player_name == "") return(NULL)
  
  headshots <- load_golf_headshots()
  if (nrow(headshots) == 0) return(NULL)
  
  # Normalize the input name
  name_normalized <- tolower(trimws(player_name))
  name_normalized <- gsub("[^a-z0-9 ]", " ", name_normalized)
  name_normalized <- gsub("\\s+", " ", name_normalized)
  name_normalized <- trimws(name_normalized)
  
  # Look up
  match <- headshots %>% filter(name_normalized == !!name_normalized)
  
  if (nrow(match) > 0) {
    return(match$headshot_url[1])
  }
  
  # Try partial matching on last name
  name_parts <- strsplit(name_normalized, " ")[[1]]
  if (length(name_parts) >= 1) {
    last_name <- name_parts[length(name_parts)]
    partial_match <- headshots %>% 
      filter(grepl(last_name, name_normalized, fixed = TRUE))
    
    if (nrow(partial_match) == 1) {
      return(partial_match$headshot_url[1])
    }
  }
  
  return(NULL)
}

#' Create HTML for player cell with headshot
#' @param player_name Character, the player's name
#' @param price Numeric, optional price to display
#' @param show_headshot Logical, whether to show headshot
#' @return HTML string for the cell
create_golf_player_cell <- function(player_name, price = NULL, show_headshot = TRUE) {
  headshot_url <- if (show_headshot) get_golf_headshot(player_name) else NULL
  
  headshot_html <- if (!is.null(headshot_url)) {
    sprintf(
      '<img src="%s" style="width: 32px; height: 32px; border-radius: 50%%; object-fit: cover; margin-right: 8px; border: 2px solid var(--border-color);" onerror="this.style.display=\'none\'">',
      headshot_url
    )
  } else {
    # Placeholder circle with initials
    initials <- toupper(substr(gsub("^(.).*\\s+(.).*$", "\\1\\2", player_name), 1, 2))
    sprintf(
      '<div style="width: 32px; height: 32px; border-radius: 50%%; background: var(--bg-secondary); display: inline-flex; align-items: center; justify-content: center; margin-right: 8px; font-size: 0.75rem; font-weight: 600; color: var(--text-secondary); border: 2px solid var(--border-color);">%s</div>',
      initials
    )
  }
  
  price_html <- if (!is.null(price)) {
    sprintf('<span style="color: var(--text-secondary); font-size: 0.85rem; margin-left: 4px;">$%.1fM</span>', price)
  } else {
    ""
  }
  
  sprintf(
    '<div style="display: flex; align-items: center;">%s<span style="font-weight: 500;">%s</span>%s</div>',
    headshot_html,
    htmltools::htmlEscape(player_name),
    price_html
  )
}