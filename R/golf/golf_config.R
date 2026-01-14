# =============================================================================
# Golf Configuration
# 
# Centralized configuration for Golf modules including:
# - Contest rules and structure
# - Player name corrections for data matching
# - Data loading functions with headshot support
# =============================================================================
library(tidyverse)
library(janitor)

# =============================================================================
# UI THEMING
# =============================================================================

GOLF_CARD_COLOR <- "gold"  # Consistent card header color for all Golf modules

# =============================================================================
# CONTEST CONFIGURATIONS
# =============================================================================

#' Classic (Full Tournament) Configuration
#' 6 golfers, 100M salary cap
GOLF_CLASSIC_STRUCTURE <- list(
  roster_size = 6,
  salary_cap = 100,
  slots = c("G1", "G2", "G3", "G4", "G5", "G6"),
  slot_labels = c(
    G1 = "Golfer", G2 = "Golfer", G3 = "Golfer",
    G4 = "Golfer", G5 = "Golfer", G6 = "Golfer"
  )
)

#' Showdown (Single Day) Configuration
#' 6 golfers: 1 CPT (1.5x), 1 VICE (1.25x), 4 FLEX
GOLF_SHOWDOWN_STRUCTURE <- list(
  roster_size = 6,
  salary_cap = 100,
  slots = c("CPT", "VICE", "FLEX1", "FLEX2", "FLEX3", "FLEX4"),
  multipliers = list(
    CPT = list(points = 1.5, salary = 1.5),
    VICE = list(points = 1.25, salary = 1.25),
    FLEX = list(points = 1.0, salary = 1.0)
  ),
  slot_labels = c(
    CPT = "CPT", VICE = "VICE",
    FLEX1 = "FLEX", FLEX2 = "FLEX", FLEX3 = "FLEX", FLEX4 = "FLEX"
  )
)

# =============================================================================
# NAME CORRECTIONS
# FanTeam name -> Projection/Headshot name
# =============================================================================

GOLF_NAME_CORRECTIONS <- list(
  # Capitalization fixes (FanTeam -> standard)
  "Robert Macintyre" = "Robert MacIntyre",
  "Maverick Mcnealy" = "Maverick McNealy",
  "Denny Mccarthy" = "Denny McCarthy",
  "Max Mcgreevy" = "Max McGreevy",
  
  # Name variants (FanTeam salaries -> Projection names)
  "Christopher Gotterup" = "Chris Gotterup",
  "Henry Lebioda" = "Hank Lebioda",
  "Kota Yuta Kaneko" = "Kota Kaneko",
  "Cam Davis" = "Cameron Davis",
  "Matt McCarty" = "Matthew McCarty",
  "Matthias Schmid" = "Matti Schmid",
  "Zach Bauchou" = "Zachary Bauchou",
  
  # Full name handling
  "Seong-Hyeon Kim" = "Seonghyeon Kim",
  "Adrien Dumont" = "Adrien Dumont De Chassart"
)

#' Apply name corrections to a player name
#' @param name Player name to correct
#' @return Corrected name or original if no correction exists
correct_golf_name <- function(name) {
  correction <- GOLF_NAME_CORRECTIONS[[name]]
  if (!is.null(correction)) return(correction)
  
  name
}

# =============================================================================
# HEADSHOT LOADING
# =============================================================================

#' Load golf player headshots
#' @return Data frame with player_name, headshot_url
load_golf_headshots <- function() {
  headshot_path <- "data/golf/player_headshots/player_headshots.csv"
  
  if (!file.exists(headshot_path)) {
    log_debug("Golf headshots file not found:", headshot_path, level = "WARN")
    return(NULL)
  }
  
  tryCatch({
    headshots <- read_csv(headshot_path, show_col_types = FALSE) %>%
      clean_names() %>%
      mutate(
        player_name = paste0(f_name, " ", name),
        headshot_url = headshot
      ) %>%
      select(player_name, headshot_url)
    
    log_debug("Loaded", nrow(headshots), "golf headshots", level = "INFO")
    return(headshots)
  }, error = function(e) {
    log_debug("Error loading golf headshots:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Default headshot URL for missing players
GOLF_DEFAULT_HEADSHOT <- "https://datagolf.com/static/players/headshot_default.png"

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Get available golf contests
#' @param year Year folder (default 2025)
#' @param contest_type "classic" or "showdown"
#' @return Vector of contest names
get_available_golf_contests <- function(year = "2025", contest_type = "classic") {
  log_debug("get_available_golf_contests() for year:", year, "type:", contest_type, level = "DEBUG")
  
  salary_dir <- sprintf("data/golf/fanteam_salaries/%s", year)
  
  if (!dir.exists(salary_dir)) {
    log_debug("Golf salary directory not found:", salary_dir, level = "WARN")
    return(character(0))
  }
  
  pattern <- if (contest_type == "classic") {
    "_classic_salaries\\.csv$"
  } else {
    "_day_\\d+_salaries\\.csv$"
  }
  
  files <- list.files(salary_dir, pattern = pattern)
  
  if (length(files) == 0) {
    log_debug("No", contest_type, "files found", level = "DEBUG")
    return(character(0))
  }
  
  contests <- if (contest_type == "classic") {
    gsub("_classic_salaries\\.csv$", "", files)
  } else {
    unique(gsub("_day_\\d+_salaries\\.csv$", "", files))
  }
  
  log_debug("Found contests:", paste(contests, collapse = ", "), level = "INFO")
  return(contests)
}

#' Get available showdown days for a tournament
#' @param year Year folder
#' @param tournament Tournament name
#' @return Vector of day numbers
get_showdown_days <- function(year = "2025", tournament) {
  salary_dir <- sprintf("data/golf/fanteam_salaries/%s", year)
  pattern <- sprintf("^%s_day_(\\d+)_salaries\\.csv$", tournament)
  files <- list.files(salary_dir, pattern = pattern)
  
  if (length(files) == 0) return(integer(0))
  
  days <- as.integer(gsub(pattern, "\\1", files))
  sort(days)
}

#' Load golf salaries from FanTeam file
#' @param year Year folder
#' @param contest Contest identifier
#' @return Data frame with player_id, player_name, salary
load_golf_salaries <- function(year = "2025", contest) {
  log_debug("load_golf_salaries() for contest:", contest, level = "DEBUG")
  
  # Determine file path - updated to new location
  if (grepl("_classic$", contest)) {
    file_path <- sprintf("data/golf/fanteam_salaries/%s/%s_salaries.csv", year, contest)
  } else if (grepl("_day_\\d+$", contest)) {
    file_path <- sprintf("data/golf/fanteam_salaries/%s/%s_salaries.csv", year, contest)
  } else {
    file_path <- sprintf("data/golf/fanteam_salaries/%s/%s_classic_salaries.csv", year, contest)
  }
  
  if (!file.exists(file_path)) {
    log_debug("Salary file not found:", file_path, level = "ERROR")
    return(NULL)
  }
  
  log_debug("Loading salaries from:", file_path, level = "INFO")
  
  tryCatch({
    salaries <- read_csv(file_path, show_col_types = FALSE) %>%
      clean_names() %>%
      filter(lineup != "refuted") %>%
      mutate(
        player_name = paste0(f_name, " ", name),
        player_id = player_id,
        salary = price
      ) %>%
      select(player_id, player_name, salary)
    
    log_debug("Loaded", nrow(salaries), "golfers", level = "INFO")
    return(salaries)
  }, error = function(e) {
    log_debug("Error loading salaries:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Parse projections from uploaded CSV (DraftKings format)
#' @param projections_df Data frame from uploaded file
#' @return Cleaned projections data frame
parse_golf_projections <- function(projections_df) {
  log_debug("parse_golf_projections() called with", nrow(projections_df), "rows", level = "DEBUG")
  
  tryCatch({
    projections <- projections_df %>%
      clean_names() %>%
      rename_with(~ gsub("^x_", "", .), everything())
    
    # Log available columns for debugging
    log_debug("Available columns:", paste(names(projections), collapse = ", "), level = "DEBUG")
    
    # Map column names flexibly
    projections <- projections %>%
      rename(
        player_name = any_of(c("golfer", "player", "name")),
        median = any_of(c("dk_points", "points", "proj", "median", "projection")),
        ceiling = any_of(c("dk_ceiling", "ceiling", "ceil")),
        cut_odds = any_of(c("make_cut_odds", "cut_odds", "make_cut")),
        dk_salary = any_of(c("dk_salary", "salary")),
        own_large = any_of(c("dk_ownership", "dk_large_ownership", "own_large", "ownership_large", "large_ownership")),
        own_small = any_of(c("small_field_dk_ownership", "dk_small_ownership", "own_small", "ownership_small", "small_ownership")),
        etr_value = any_of(c("dk_value", "value", "etr_value"))
      )
    
    projections <- projections %>%
      mutate(
        own_large = if ("own_large" %in% names(.)) {
          as.numeric(gsub("%", "", own_large))
        } else NA_real_,
        own_small = if ("own_small" %in% names(.)) {
          as.numeric(gsub("%", "", own_small))
        } else NA_real_,
        cut_odds = if ("cut_odds" %in% names(.)) {
          as.numeric(gsub("%", "", cut_odds))
        } else NA_real_,
        etr_value = if ("etr_value" %in% names(.)) {
          as.numeric(etr_value)
        } else NA_real_,
        median = as.numeric(median),
        ceiling = as.numeric(ceiling),
        blended = (median + ceiling) / 2
      ) %>%
      select(player_name, median, ceiling, blended, 
             any_of(c("own_large", "own_small", "cut_odds", "dk_salary", "volatility", "etr_value")))
    
    log_debug("Parsed", nrow(projections), "golfers", level = "INFO")
    return(projections)
  }, error = function(e) {
    log_debug("Error parsing projections:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Match projections and headshots to salaries by name
#' @param salaries Data frame from load_golf_salaries()
#' @param projections Data frame from parse_golf_projections()
#' @param headshots Data frame from load_golf_headshots() (optional)
#' @return Merged data frame with projection and headshot info
match_golf_players <- function(salaries, projections, headshots = NULL) {
  log_debug("match_golf_players() called", level = "DEBUG")
  log_debug("  Salaries:", nrow(salaries), "golfers", level = "DEBUG")
  log_debug("  Projections:", nrow(projections), "golfers", level = "DEBUG")
  
  normalize_name <- function(name) {
    name %>%
      tolower() %>%
      stringr::str_replace_all("[^a-z0-9\\s]", "") %>%
      stringr::str_squish()
  }
  
  # Apply corrections and create match keys
  salaries <- salaries %>%
    mutate(
      corrected_name = sapply(player_name, correct_golf_name),
      match_key = normalize_name(corrected_name)
    )
  
  projections <- projections %>%
    mutate(match_key = normalize_name(player_name))
  
  # Match projections
  matched <- salaries %>%
    left_join(
      projections %>% select(-player_name),
      by = "match_key"
    )
  
  # Match headshots if provided
  if (!is.null(headshots) && nrow(headshots) > 0) {
    headshots <- headshots %>%
      mutate(match_key = normalize_name(player_name))
    
    matched <- matched %>%
      left_join(
        headshots %>% select(match_key, headshot_url),
        by = "match_key"
      )
  } else {
    matched$headshot_url <- NA_character_
  }
  
  # Clean up headshots and calculate value
  matched <- matched %>%
    mutate(
      headshot_url = if_else(is.na(headshot_url), GOLF_DEFAULT_HEADSHOT, headshot_url)
    )
  
  # Use ETR's value if available in projections, otherwise calculate our own
  has_etr_value <- "etr_value" %in% names(matched) && sum(!is.na(matched$etr_value)) > 0
  
  if (has_etr_value) {
    log_debug("Using ETR value column from projections", level = "INFO")
    matched <- matched %>%
      mutate(value = etr_value) %>%
      select(-any_of("etr_value"))
  } else {
    log_debug("Calculating salary-adjusted value", level = "INFO")
    # Calculate salary-adjusted value (higher-priced players have higher baseline)
    matched <- matched %>%
      mutate(
        pts_per_dollar = if_else(!is.na(blended) & salary > 0, blended / salary, NA_real_)
      )
    
    avg_salary <- mean(matched$salary, na.rm = TRUE)
    avg_pts_per_dollar <- mean(matched$pts_per_dollar, na.rm = TRUE)
    
    # 15% higher expectation per unit above avg salary
    salary_premium_factor <- 0.15
    
    matched <- matched %>%
      mutate(
        salary_ratio = (salary - avg_salary) / avg_salary,
        expected_pts_per_dollar = avg_pts_per_dollar * (1 + salary_premium_factor * salary_ratio),
        value = pts_per_dollar - expected_pts_per_dollar
      ) %>%
      select(-pts_per_dollar, -salary_ratio, -expected_pts_per_dollar)
  }
  
  # Ensure expected columns exist
  if (!"own_large" %in% names(matched)) matched$own_large <- NA_real_
  if (!"own_small" %in% names(matched)) matched$own_small <- NA_real_
  if (!"cut_odds" %in% names(matched)) matched$cut_odds <- NA_real_
  if (!"volatility" %in% names(matched)) matched$volatility <- NA_real_
  
  # Remove match helper columns and any leftover temp columns
  matched <- matched %>%
    select(-any_of(c("corrected_name", "match_key", "etr_value")))
  
  # Log results
  matched_proj <- sum(!is.na(matched$median))
  matched_headshot <- sum(matched$headshot_url != GOLF_DEFAULT_HEADSHOT)
  
  log_debug("Matched projections:", matched_proj, "of", nrow(matched), level = "INFO")
  log_debug("Matched headshots:", matched_headshot, "of", nrow(matched), level = "INFO")
  
  return(matched)
}

#' Get human-readable label for contest
#' @param contest Contest identifier
#' @return Display label
get_golf_contest_label <- function(contest) {
  label <- contest %>%
    gsub("_", " ", .) %>%
    tools::toTitleCase()
  
  label <- gsub("Classic$", "(Classic)", label)
  label <- gsub("Day (\\d+)$", "(Day \\1)", label)
  label
}

# =============================================================================
# GOOGLE SHEETS DATA LOADING
# =============================================================================

# Google Sheet IDs for golf data
GOLF_PROJECTIONS_SHEET_ID <- "1yJJAOv5hzNZagYUG7FLpNmRIRC76L0fJNGPbzK61lbw"
GOLF_SALARIES_SHEET_ID <- "12I3uMfY_V5apa0u6DGcQctIvJETJFonUJwm3aqiuvhI"

#' Get available tournaments from Google Sheets
#' @return Character vector of tournament names (sheet names)
get_golf_tournaments_gsheet <- function() {
  log_debug("get_golf_tournaments_gsheet() called", level = "DEBUG")
  
  tryCatch({
    ss <- googlesheets4::gs4_get(GOLF_PROJECTIONS_SHEET_ID)
    tournaments <- ss$sheets$name
    log_debug("Found tournaments:", paste(tournaments, collapse = ", "), level = "INFO")
    return(tournaments)
  }, error = function(e) {
    log_debug("Error getting tournaments from Google Sheets:", e$message, level = "ERROR")
    return(character(0))
  })
}

#' Load projections from Google Sheet for a specific tournament
#' @param tournament Tournament name (sheet name)
#' @return Data frame with projections
load_golf_projections_gsheet <- function(tournament) {
  log_debug("load_golf_projections_gsheet() for tournament:", tournament, level = "DEBUG")
  
  tryCatch({
    projections <- googlesheets4::read_sheet(
      GOLF_PROJECTIONS_SHEET_ID,
      sheet = tournament
    ) %>%
      janitor::clean_names()
    
    log_debug("Loaded", nrow(projections), "rows from projections sheet", level = "INFO")
    
    # Use the existing parse function to standardize column names
    projections <- parse_golf_projections(projections)
    
    return(projections)
  }, error = function(e) {
    log_debug("Error loading projections from Google Sheets:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load salaries from Google Sheet for a specific tournament
#' @param tournament Tournament name (sheet name)
#' @return Data frame with player_id, player_name, salary
load_golf_salaries_gsheet <- function(tournament) {
  log_debug("load_golf_salaries_gsheet() for tournament:", tournament, level = "DEBUG")
  
  tryCatch({
    salaries_raw <- googlesheets4::read_sheet(
      GOLF_SALARIES_SHEET_ID,
      sheet = tournament
    ) %>%
      janitor::clean_names()
    
    log_debug("Raw salary columns:", paste(names(salaries_raw), collapse = ", "), level = "DEBUG")
    
    # Check if we have f_name + name format (FanTeam style) or single player_name
    if (all(c("f_name", "name") %in% names(salaries_raw))) {
      # FanTeam format: concatenate first and last name
      salaries <- salaries_raw %>%
        filter(lineup != "refuted") %>%
        mutate(
          player_name = paste0(f_name, " ", name),
          salary = as.numeric(price)
        )
    } else {
      # Simple format: rename columns flexibly
      salaries <- salaries_raw %>%
        rename(
          player_name = any_of(c("player_name", "player", "golfer", "name")),
          salary = any_of(c("salary", "price", "sal"))
        ) %>%
        mutate(salary = as.numeric(salary))
    }
    
    # Create player_id if not present
    if (!"player_id" %in% names(salaries)) {
      salaries$player_id <- seq_len(nrow(salaries))
    }
    
    salaries <- salaries %>%
      select(player_id, player_name, salary)
    
    log_debug("Loaded", nrow(salaries), "golfers from salaries sheet", level = "INFO")
    return(salaries)
  }, error = function(e) {
    log_debug("Error loading salaries from Google Sheets:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load and merge tournament data from Google Sheets
#' @param tournament Tournament name
#' @return Merged data frame with salaries, projections, headshots
load_golf_tournament_data <- function(tournament) {
  log_debug("load_golf_tournament_data() for tournament:", tournament, level = "DEBUG")
  
  salaries <- load_golf_salaries_gsheet(tournament)
  projections <- load_golf_projections_gsheet(tournament)
  headshots <- load_golf_headshots()
  
  if (is.null(salaries)) {
    log_debug("Failed to load salaries", level = "ERROR")
    return(NULL)
  }
  
  if (is.null(projections)) {
    log_debug("Failed to load projections", level = "ERROR")
    return(NULL)
  }
  
  # Use existing match function
  player_data <- match_golf_players(salaries, projections, headshots)
  
  return(player_data)
}

cat("Golf config loaded: GOLF_CARD_COLOR, GOLF_CLASSIC_STRUCTURE, GOLF_SHOWDOWN_STRUCTURE, headshot support, Google Sheets functions\n")