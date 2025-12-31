# =============================================================================
# Soccer FanTeam Data Loader
# 
# Functions to load FanTeam Monster salary data for soccer DFS
# =============================================================================

# Data directory
FANTEAM_SOCCER_DIR <- "data/fanteam_soccer/fanteam_monster_salaries"

# Club abbreviation to full team name mapping
FANTEAM_CLUB_MAPPING <- c(
  # Premier League
  "ARS" = "Arsenal",
  "AV" = "Aston Villa",
  "AVL" = "Aston Villa",
  "BOU" = "Bournemouth",
  "BRE" = "Brentford",
  "BHA" = "Brighton",
  "BUR" = "Burnley",
  "CHE" = "Chelsea",
  "CRY" = "Crystal Palace",
  "EVE" = "Everton",
  "FUL" = "Fulham",
  "IPS" = "Ipswich Town",
  "LEE" = "Leeds United",
  "LEI" = "Leicester City",
  "LIV" = "Liverpool",
  "MCI" = "Manchester City",
  "MUN" = "Manchester United",
  "NEW" = "Newcastle United",
  "NFO" = "Nottingham Forest",
  "NTG" = "Nottingham Forest",
  "SOU" = "Southampton",
  "SUN" = "Sunderland",
  "TOT" = "Tottenham",
  "WHU" = "West Ham",
  "WOL" = "Wolves",
  # Championship
  "BIR" = "Birmingham City",
  "BLK" = "Blackburn Rovers",
  "BRC" = "Bristol City",
  "CAR" = "Cardiff City",
  "COV" = "Coventry City",
  "DER" = "Derby County",
  "HUL" = "Hull City",
  "LUT" = "Luton Town",
  "MID" = "Middlesbrough",
  "MIL" = "Millwall",
  "NOR" = "Norwich City",
  "PLY" = "Plymouth Argyle",
  "PNE" = "Preston North End",
  "QPR" = "Queens Park Rangers",
  "SHU" = "Sheffield United",
  "SHW" = "Sheffield Wednesday",
  "STK" = "Stoke City",
  "SWA" = "Swansea City",
  "WAT" = "Watford",
  "WBA" = "West Brom"
)

#' Get available gameweeks from FanTeam Monster files
#' @return Vector of gameweek numbers (sorted descending - newest first)
get_fanteam_soccer_gameweeks <- function() {
  log_debug("get_fanteam_soccer_gameweeks() called", level = "INFO")
  
  if (!dir.exists(FANTEAM_SOCCER_DIR)) {
    log_debug("Directory not found:", FANTEAM_SOCCER_DIR, level = "WARN")
    return(c())
  }
  
  # List CSV files
  files <- list.files(FANTEAM_SOCCER_DIR, pattern = "\\.csv$", full.names = FALSE)
  log_debug("Found files:", paste(files, collapse = ", "), level = "DEBUG")
  
  if (length(files) == 0) {
    log_debug("No CSV files found", level = "WARN")
    return(c())
  }
  
  # Extract gameweek numbers from filenames
  # Supported formats:
  #   - gw1.csv, gw12.csv
  #   - gameweek_1.csv
  #   - week_1.csv  
  #   - 1043294_players_20251223205455.csv (FanTeam export - use file order/date)
  gameweeks <- c()
  fanteam_files <- c()
  
  for (file in files) {
    gw <- NA
    
    # Pattern: gw1.csv, gw12.csv
    if (grepl("^gw(\\d+)\\.csv$", file, ignore.case = TRUE)) {
      gw <- as.integer(gsub("^gw(\\d+)\\.csv$", "\\1", file, ignore.case = TRUE))
    }
    # Pattern: gameweek_1.csv
    else if (grepl("gameweek_(\\d+)\\.csv$", file, ignore.case = TRUE)) {
      gw <- as.integer(gsub(".*gameweek_(\\d+)\\.csv$", "\\1", file, ignore.case = TRUE))
    }
    # Pattern: week_1.csv
    else if (grepl("week_(\\d+)\\.csv$", file, ignore.case = TRUE)) {
      gw <- as.integer(gsub(".*week_(\\d+)\\.csv$", "\\1", file, ignore.case = TRUE))
    }
    # Pattern: FanTeam export format (tournament_players_timestamp.csv)
    else if (grepl("^\\d+_players_\\d+\\.csv$", file)) {
      fanteam_files <- c(fanteam_files, file)
    }
    
    if (!is.na(gw)) {
      gameweeks <- c(gameweeks, gw)
    }
  }
  
  # If we have FanTeam format files but no numbered ones, create synthetic GW numbers
  if (length(gameweeks) == 0 && length(fanteam_files) > 0) {
    # Sort by timestamp in filename (descending = newest first)
    fanteam_files <- sort(fanteam_files, decreasing = TRUE)
    gameweeks <- seq_along(fanteam_files)
    
    # Store the mapping for later use
    assign("FANTEAM_FILE_MAPPING", setNames(fanteam_files, as.character(gameweeks)), 
           envir = .GlobalEnv)
    log_debug("Created file mapping for", length(fanteam_files), "FanTeam exports", level = "INFO")
  }
  
  gameweeks <- sort(unique(gameweeks), decreasing = TRUE)
  log_debug("Available gameweeks:", paste(gameweeks, collapse = ", "), level = "INFO")
  
  return(gameweeks)
}

#' Get filename for a gameweek
#' @param gameweek Gameweek number
#' @return Filename or NULL
get_fanteam_filename <- function(gameweek) {
  # Check if we have a FanTeam file mapping
  if (exists("FANTEAM_FILE_MAPPING", envir = .GlobalEnv)) {
    mapping <- get("FANTEAM_FILE_MAPPING", envir = .GlobalEnv)
    if (as.character(gameweek) %in% names(mapping)) {
      return(mapping[[as.character(gameweek)]])
    }
  }
  
  # Try standard patterns
  patterns <- c(
    sprintf("gw%d.csv", gameweek),
    sprintf("GW%d.csv", gameweek),
    sprintf("gameweek_%d.csv", gameweek),
    sprintf("week_%d.csv", gameweek)
  )
  
  for (pattern in patterns) {
    if (file.exists(file.path(FANTEAM_SOCCER_DIR, pattern))) {
      return(pattern)
    }
  }
  
  return(NULL)
}

#' Load FanTeam Monster salary data for a specific gameweek
#' @param gameweek Gameweek number
#' @return Data frame with player salaries and positions, or NULL if not found
load_fanteam_soccer_salaries <- function(gameweek) {
  log_debug("========================================", level = "INFO")
  log_debug("load_fanteam_soccer_salaries() called", level = "INFO")
  log_debug("  Gameweek:", gameweek, level = "INFO")
  
  if (!dir.exists(FANTEAM_SOCCER_DIR)) {
    log_debug("Directory not found:", FANTEAM_SOCCER_DIR, level = "ERROR")
    return(NULL)
  }
  
  # Get filename for this gameweek
  filename <- get_fanteam_filename(gameweek)
  
  if (is.null(filename)) {
    # Try to find any matching file
    files <- list.files(FANTEAM_SOCCER_DIR, pattern = "\\.csv$", full.names = FALSE)
    if (length(files) > 0) {
      # Use the first file as fallback
      filename <- files[min(gameweek, length(files))]
    }
  }
  
  if (is.null(filename)) {
    log_debug("No file found for gameweek:", gameweek, level = "ERROR")
    return(NULL)
  }
  
  file_path <- file.path(FANTEAM_SOCCER_DIR, filename)
  
  if (!file.exists(file_path)) {
    log_debug("File not found:", file_path, level = "ERROR")
    return(NULL)
  }
  
  log_debug("Loading file:", file_path, level = "INFO")
  
  # Load the data
  data <- tryCatch({
    read_csv(file_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
      clean_names()
  }, error = function(e) {
    log_debug("Error reading file:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(data) || nrow(data) == 0) {
    log_debug("No data loaded", level = "WARN")
    return(NULL)
  }
  
  log_debug("Loaded", nrow(data), "rows", level = "INFO")
  log_debug("Columns:", paste(names(data), collapse = ", "), level = "DEBUG")
  
  # Process FanTeam format
  # Expected columns: tournament, player_id, name, f_name, club, lineup, position, price
  
  if (all(c("name", "f_name", "club", "position", "price") %in% names(data))) {
    log_debug("Detected FanTeam export format", level = "INFO")
    
    data <- data %>%
      mutate(
        # Combine first name and last name
        player = paste(f_name, name),
        # Map club abbreviation to full team name
        team = case_when(
          club %in% names(FANTEAM_CLUB_MAPPING) ~ FANTEAM_CLUB_MAPPING[club],
          TRUE ~ club
        ),
        # Standardize positions
        position = case_when(
          tolower(position) == "goalkeeper" ~ "GK",
          tolower(position) == "defender" ~ "DEF",
          tolower(position) == "midfielder" ~ "MID",
          tolower(position) == "forward" ~ "FWD",
          TRUE ~ toupper(substr(position, 1, 3))
        ),
        # Convert price to numeric (already in millions)
        salary = as.numeric(price),
        # Keep lineup status
        status = lineup
      ) %>%
      select(player, team, position, salary, status, 
             player_id = player_id, club_abbrev = club)
    
  } else {
    # Try generic format
    log_debug("Using generic format parsing", level = "INFO")
    
    # Rename columns to standard names
    data <- data %>%
      rename_with(~ case_when(
        . %in% c("name", "player_name") ~ "player",
        . %in% c("club", "team_name") ~ "team",
        . %in% c("pos") ~ "position",
        . %in% c("price", "cost") ~ "salary",
        TRUE ~ .
      ))
    
    # Standardize positions if present
    if ("position" %in% names(data)) {
      data <- data %>%
        mutate(position = case_when(
          tolower(position) %in% c("goalkeeper", "gk", "g") ~ "GK",
          tolower(position) %in% c("defender", "def", "d") ~ "DEF",
          tolower(position) %in% c("midfielder", "mid", "m") ~ "MID",
          tolower(position) %in% c("forward", "fwd", "f", "attacker", "att") ~ "FWD",
          TRUE ~ toupper(position)
        ))
    }
    
    # Convert salary to numeric
    if ("salary" %in% names(data)) {
      data$salary <- as.numeric(gsub("[^0-9.]", "", as.character(data$salary)))
    }
  }
  
  # Add gameweek column
  data$gameweek <- as.integer(gameweek)
  
  # Ensure required columns exist
  required_cols <- c("player", "team", "position", "salary")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    log_debug("Missing required columns:", paste(missing_cols, collapse = ", "), level = "WARN")
  }
  
  log_debug("Data processed:", nrow(data), "players", level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(data)
}

#' Load FanTeam salary data with team logos
#' @param gameweek Gameweek number
#' @return Data frame with salaries and logo paths
load_fanteam_soccer_with_logos <- function(gameweek) {
  log_debug("load_fanteam_soccer_with_logos() called", level = "INFO")
  
  data <- load_fanteam_soccer_salaries(gameweek)
  
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Add team logos using existing soccer config
  if ("team" %in% names(data)) {
    data <- data %>%
      mutate(
        team_normalized = normalize_team_names(team),
        logo_path = sapply(team_normalized, function(t) {
          logo <- get_soccer_team_logo(t)
          if (is.null(logo)) "" else logo
        })
      )
  }
  
  return(data)
}

#' Get unique teams from current data
#' @param data FanTeam salary data
#' @return Vector of unique team names
get_fanteam_teams <- function(data) {
  if (is.null(data) || !"team_normalized" %in% names(data)) {
    return(c())
  }
  sort(unique(data$team_normalized[!is.na(data$team_normalized)]))
}