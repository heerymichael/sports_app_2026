# =============================================================================
# Utility Functions
# Helper functions for the Sports Analytics app
# =============================================================================

# Enable debug mode - set to FALSE in production
DEBUG_MODE <- TRUE

#' Debug logging function
#' @param ... Messages to log
#' @param level Log level: "INFO", "WARN", "ERROR", "DEBUG"
log_debug <- function(..., level = "DEBUG") {
  if (DEBUG_MODE || level %in% c("ERROR", "WARN", "INFO")) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    msg <- paste(..., collapse = " ")
    cat(sprintf("[%s] [%s] %s\n", timestamp, level, msg))
  }
}

#' Null coalescing operator
#' @param a First value
#' @param b Fallback value if a is NULL or empty
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}

#' Format salary for display
#' @param salary Numeric salary value
#' @return Formatted string
format_salary <- function(salary) {
  sprintf("$%s", format(salary, big.mark = ","))
}

#' Format projection for display
#' @param projection Numeric projection value
#' @return Formatted string
format_projection <- function(projection) {
  sprintf("%.1f", projection)
}

#' Get team logo URL
#' @param team Team abbreviation (e.g., "KC", "NYG")
#' @param variant Logo variant: "regular", "bw" (black/white), or "webp"
#' @return URL path to logo
get_team_logo <- function(team, variant = "webp") {
  if (is.null(team) || team == "") return(NULL)
  
  switch(variant,
         "bw" = sprintf("nfl_logos/%s_bw.png", team),
         "png" = sprintf("nfl_logos/%s.png", team),
         "webp" = sprintf("nfl_logos/%s.webp", team),
         sprintf("nfl_logos/%s.webp", team)  # default to webp
  )
}

#' Create player headshot HTML element (Stabilo style)
#' @param headshot_url URL to player headshot image
#' @param team_color Background color (hex)
#' @param size Size of container ("small" or "normal")
#' @param position Player position (to detect DST)
#' @param team Team abbreviation (for DST logo)
#' @return HTML div with circular headshot
create_headshot_html <- function(headshot_url, team_color, size = "normal", position = NULL, team = NULL) {
  
  # Default values if missing
  team_color <- team_color %||% "#E0E0E0"
  
  # Map size param to CSS class modifier
  size_class <- switch(size,
                       "tiny" = "player-headshot--xs",
                       "small" = "player-headshot--sm",
                       "player-headshot--md"  # default/normal
  )
  
  # Default fallback image
  fallback_url <- "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
  
  # Use team logo for DST positions
  if (!is.null(position) && position == "DST" && !is.null(team)) {
    headshot_url <- get_team_logo(team, "webp")
  } else {
    # Handle NULL, NA, or empty string
    if (is.null(headshot_url) || is.na(headshot_url) || headshot_url == "") {
      headshot_url <- fallback_url
    }
  }
  
  # Use CSS classes for sizing, only inline style for dynamic team color
  div(
    class = paste("player-headshot", size_class),
    style = sprintf("background-color: %s;", team_color),
    tags$img(
      src = headshot_url,
      onerror = sprintf("this.src='%s'", fallback_url)
    )
  )
}

#' Create matchup display HTML (Stabilo style)
#' @param team Player's team
#' @param opponent Opponent team  
#' @param home Is player's team at home (TRUE/FALSE)
#' @param show_logos Whether to show team logos (default FALSE)
#' @return HTML div with matchup
create_matchup_html <- function(team, opponent = NULL, home = TRUE, show_logos = FALSE) {
  
  # If no opponent data, just show team
  if (is.null(opponent) || is.na(opponent) || opponent == "") {
    return(
      div(
        style = "font-size: 0.75rem; color: var(--text-muted); margin-top: 0.15rem;",
        span(style = "font-weight: 700;", team)
      )
    )
  }
  
  # Convert home to boolean
  if (identical(home, FALSE) || home == 0 || home == "FALSE" || home == "false") {
    is_home <- FALSE
  } else {
    is_home <- TRUE
  }
  
  # Determine which team is away and which is home
  if (!is_home) {
    away_team <- team
    home_team <- opponent
  } else {
    away_team <- opponent
    home_team <- team
  }
  
  # Always display as: AWAY @ HOME, bold player's team
  div(
    style = "font-size: 0.75rem; color: var(--text-muted); margin-top: 0.15rem;",
    span(style = if (away_team == team) "font-weight: 700; color: var(--text-primary);" else "", away_team),
    span(" @ "),
    span(style = if (home_team == team) "font-weight: 700; color: var(--text-primary);" else "", home_team)
  )
}

#' Get full team name from abbreviation
#' @param abbr Team abbreviation
#' @return Full team name
#' @note Uses NFL_TEAM_NAMES from nfl_config.R (available at runtime)
get_team_full_name <- function(abbr) {
  # Use centralized team data from nfl_config.R
  # NFL_TEAM_NAMES is available after global.R sources all files
  if (exists("NFL_TEAM_NAMES")) {
    return(unname(NFL_TEAM_NAMES[abbr]))
  }
  # Fallback if called before config loaded
  abbr
}

#' Get available seasons from data folder
#' @return Vector of available seasons
get_available_seasons <- function() {
  log_debug("========================================", level = "INFO")
  log_debug("get_available_seasons() called", level = "INFO")
  log_debug("Current working directory:", getwd(), level = "INFO")
  
  # Try multiple possible path patterns
  possible_paths <- c(
    "data/projections",      # New structure: data/projections/2025/
    "projections"            # Old structure: projections/ (files directly)
  )
  
  proj_path <- NULL
  path_type <- NULL
  
  for (path in possible_paths) {
    log_debug("Checking path:", path, level = "INFO")
    if (dir.exists(path)) {
      proj_path <- path
      log_debug("  Found!", level = "INFO")
      break
    } else {
      log_debug("  Not found", level = "DEBUG")
    }
  }
  
  if (is.null(proj_path)) {
    log_debug("No projection directory found!", level = "ERROR")
    log_debug("Contents of current directory:", level = "INFO")
    current_files <- list.files(".", all.files = FALSE)
    log_debug(paste(current_files, collapse = ", "), level = "INFO")
    return(character(0))
  }
  
  # Check if this path has year subfolders or files directly
  contents <- list.files(proj_path, full.names = FALSE)
  log_debug("Contents of", proj_path, ":", paste(contents, collapse = ", "), level = "INFO")
  
  # Check for year subdirectories (e.g., 2025)
  subdirs <- list.dirs(proj_path, recursive = FALSE, full.names = FALSE)
  log_debug("Subdirectories found:", paste(subdirs, collapse = ", "), level = "INFO")
  
  if (length(subdirs) > 0) {
    # New structure: data/projections/2025/
    seasons <- as.numeric(subdirs)
    seasons <- seasons[!is.na(seasons)]
    path_type <- "year_subfolders"
    log_debug("Structure type: year subfolders", level = "INFO")
  } else {
    # Old structure: projections/week_1_projections.csv (no year folders)
    # In this case, we don't have multiple seasons - just return current year or detect from files
    csv_files <- list.files(proj_path, pattern = "week_.*_projections\\.csv", full.names = FALSE)
    if (length(csv_files) > 0) {
      seasons <- c(2025)  # Default to current season
      path_type <- "flat_files"
      log_debug("Structure type: flat files (no year folders), defaulting to 2025", level = "INFO")
    } else {
      log_debug("No projection files found in", proj_path, level = "ERROR")
      return(character(0))
    }
  }
  
  seasons <- sort(seasons, decreasing = TRUE)
  
  # Store path type for later use
  options(sports_analytics_path_type = path_type)
  options(sports_analytics_base_path = proj_path)
  
  log_debug("Available seasons:", paste(seasons, collapse = ", "), level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(seasons)
}

# =============================================================================
# PLAYOFF WEEK CONFIGURATION
# =============================================================================

# Playoff week identifiers in order
NFL_PLAYOFF_WEEKS <- c("wild_card", "divisional_round", "conference", "super_bowl")

# Display labels for playoff weeks
NFL_PLAYOFF_LABELS <- c(
  "wild_card" = "Wild Card",
  "divisional_round" = "Divisional Round",
  "conference" = "Conference Championships",
  "super_bowl" = "Super Bowl"
)

#' Check if a week identifier is a playoff week
#' @param week Week identifier (numeric or string)
#' @return TRUE if playoff week
is_playoff_week <- function(week) {
  as.character(week) %in% NFL_PLAYOFF_WEEKS
}

#' Get display label for a week
#' @param week Week identifier (numeric or string)
#' @return Display label
get_week_label <- function(week) {
  week_str <- as.character(week)
  if (week_str %in% names(NFL_PLAYOFF_LABELS)) {
    return(NFL_PLAYOFF_LABELS[[week_str]])
  }
  paste("Week", week)
}

#' Build file prefix for a week (handles both regular and playoff weeks)
#' @param week Week identifier (numeric or string)
#' @return File prefix (e.g., "week_15" or "wild_card")
get_week_file_prefix <- function(week) {
  if (is_playoff_week(week)) {
    return(as.character(week))
  }
  paste0("week_", week)
}

#' Get available weeks for a season
#' @param season Year
#' @return Vector of available weeks (numeric for regular season, character for playoffs)
get_available_weeks <- function(season) {
  log_debug("get_available_weeks() called for season:", season, level = "INFO")
  
  # Get stored path info or detect again
  path_type <- getOption("sports_analytics_path_type", NULL)
  base_path <- getOption("sports_analytics_base_path", NULL)
  
  log_debug("Path type:", path_type, level = "DEBUG")
  log_debug("Base path:", base_path, level = "DEBUG")
  
  # Determine the correct path based on structure
  if (!is.null(path_type) && path_type == "year_subfolders" && !is.null(base_path)) {
    # New structure: data/projections/2025/
    proj_path <- paste0(base_path, "/", season)
  } else if (!is.null(path_type) && path_type == "flat_files" && !is.null(base_path)) {
    # Old structure: projections/ (files directly)
    proj_path <- base_path
  } else {
    # Fallback: try to detect
    if (dir.exists(paste0("data/projections/", season))) {
      proj_path <- paste0("data/projections/", season)
    } else if (dir.exists("data/projections")) {
      proj_path <- "data/projections"
    } else if (dir.exists("projections")) {
      proj_path <- "projections"
    } else {
      log_debug("Could not determine projections path", level = "ERROR")
      return(character(0))
    }
  }
  
  log_debug("Looking for weeks at:", proj_path, level = "INFO")
  log_debug("Path exists:", dir.exists(proj_path), level = "INFO")
  
  if (!dir.exists(proj_path)) {
    log_debug("Path does not exist:", proj_path, level = "ERROR")
    return(character(0))
  }
  
  # List ALL projection files
  proj_files <- list.files(
    proj_path, 
    pattern = ".*_projections\\.csv", 
    full.names = FALSE
  )
  
  log_debug("Found projection files:", paste(proj_files, collapse = ", "), level = "INFO")
  
  if (length(proj_files) == 0) {
    log_debug("No projection files found", level = "WARN")
    return(character(0))
  }
  
  # Extract regular season weeks (numeric)
  regular_weeks <- as.numeric(gsub(".*week_(\\d+)_projections\\.csv", "\\1", proj_files))
  regular_weeks <- sort(regular_weeks[!is.na(regular_weeks)], decreasing = TRUE)
  
  # Check for playoff weeks
  playoff_weeks <- c()
  for (pw in NFL_PLAYOFF_WEEKS) {
    pattern <- paste0("^", pw, "_projections\\.csv$")
    if (any(grepl(pattern, proj_files))) {
      playoff_weeks <- c(playoff_weeks, pw)
    }
  }
  
  # Combine: playoff weeks first (in order), then regular weeks (descending)
  all_weeks <- c(playoff_weeks, as.character(regular_weeks))
  
  log_debug("Available weeks:", paste(all_weeks, collapse = ", "), level = "INFO")
  
  return(all_weeks)
}