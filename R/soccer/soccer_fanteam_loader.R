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

#' Get available gameweeks from Google Sheets
#' Reads worksheet names from the salaries Google Sheet to find week_N tabs
#' @return Vector of gameweek numbers (sorted descending - newest first)
get_fanteam_soccer_gameweeks <- function() {
  log_debug("get_fanteam_soccer_gameweeks() called", level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    sheets <- googlesheets4::sheet_names(FANTEAM_MATCHUPS_SHEET_IDS$salaries)
    log_debug("Found sheets in salaries workbook:", paste(sheets, collapse = ", "), level = "DEBUG")
    
    # Filter to week_N pattern and extract numbers
    week_sheets <- sheets[grepl("^week_\\d+$", sheets, ignore.case = TRUE)]
    if (length(week_sheets) == 0) {
      log_debug("No week_N sheets found in Google Sheets", level = "WARN")
      return(c())
    }
    
    gws <- as.integer(gsub("^week_(\\d+)$", "\\1", week_sheets, ignore.case = TRUE))
    gws <- sort(unique(gws[!is.na(gws)]), decreasing = TRUE)
    log_debug("Gameweeks from Google Sheets:", paste(gws, collapse = ", "), level = "INFO")
    return(gws)
  }, error = function(e) {
    log_debug("Google Sheets lookup failed:", e$message, level = "ERROR")
    return(c())
  })
}

#' Load FanTeam Monster salary data for a specific gameweek from Google Sheets
#' @param gameweek Gameweek number
#' @return Data frame with player salaries and positions, or NULL if not found
load_fanteam_soccer_salaries <- function(gameweek) {
  log_debug("========================================", level = "INFO")
  log_debug("load_fanteam_soccer_salaries() called", level = "INFO")
  log_debug("  Gameweek:", gameweek, level = "INFO")
  
  sheet_name <- sprintf("week_%d", as.integer(gameweek))
  
  data <- tryCatch({
    googlesheets4::gs4_deauth()
    raw <- googlesheets4::read_sheet(
      FANTEAM_MATCHUPS_SHEET_IDS$salaries,
      sheet = sheet_name
    ) %>% janitor::clean_names()
    
    log_debug("Loaded", nrow(raw), "rows from Google Sheets (salaries,", sheet_name, ")", level = "INFO")
    log_debug("Columns:", paste(names(raw), collapse = ", "), level = "DEBUG")
    
    # Detect garbage headers (x1, x2, x3...) - means real headers are in a data row
    unnamed_cols <- sum(grepl("^x\\d+$", names(raw)))
    if (unnamed_cols >= ncol(raw) / 2) {
      log_debug("Salaries: Detected unnamed columns (", unnamed_cols, "/", ncol(raw),
                ") - re-reading with skip=1", level = "WARN")
      raw <- googlesheets4::read_sheet(
        FANTEAM_MATCHUPS_SHEET_IDS$salaries,
        sheet = sheet_name,
        skip = 1
      ) %>% janitor::clean_names()
      log_debug("Salaries: Re-read columns:", paste(names(raw), collapse = ", "), level = "INFO")
    }
    
    # Flatten any list columns (common googlesheets4 issue with mixed types/empty cells)
    for (col in names(raw)) {
      if (is.list(raw[[col]])) {
        raw[[col]] <- sapply(raw[[col]], function(x) if (is.null(x) || length(x) == 0) NA else x[[1]])
      }
    }
    
    raw
  }, error = function(e) {
    log_debug("Google Sheets salaries load failed:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(data) || nrow(data) == 0) {
    log_debug("No salary data loaded for GW:", gameweek, level = "WARN")
    return(NULL)
  }
  
  # Process FanTeam format
  # Expected columns: tournament, player_id, name, f_name, club, lineup, position, price
  if (all(c("name", "f_name", "club", "position", "price") %in% names(data))) {
    log_debug("Detected FanTeam export format", level = "INFO")
    
    data <- data %>%
      mutate(
        player = paste(f_name, name),
        team = case_when(
          club %in% names(FANTEAM_CLUB_MAPPING) ~ FANTEAM_CLUB_MAPPING[club],
          TRUE ~ club
        ),
        position = case_when(
          tolower(position) == "goalkeeper" ~ "GK",
          tolower(position) == "defender" ~ "DEF",
          tolower(position) == "midfielder" ~ "MID",
          tolower(position) == "forward" ~ "FWD",
          TRUE ~ toupper(substr(position, 1, 3))
        ),
        salary = as.numeric(price),
        status = lineup
      ) %>%
      select(player, team, position, salary, status, 
             player_id = player_id, club_abbrev = club)
    
  } else {
    # Try generic format
    log_debug("Using generic format parsing", level = "INFO")
    
    data <- data %>%
      rename_with(~ case_when(
        . %in% c("name", "player_name") ~ "player",
        . %in% c("club", "team_name") ~ "team",
        . %in% c("pos") ~ "position",
        . %in% c("price", "cost") ~ "salary",
        TRUE ~ .
      ))
    
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