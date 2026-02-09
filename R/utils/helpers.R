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
  if (exists("NFL_TEAM_NAMES")) {
    return(unname(NFL_TEAM_NAMES[abbr]))
  }
  abbr
}

#' Get available seasons from NFL_SHEET_IDS config
#' @return Vector of available seasons (numeric, descending)
get_available_seasons <- function() {
  log_debug("========================================", level = "INFO")
  log_debug("get_available_seasons() called", level = "INFO")
  
  if (!exists("NFL_SHEET_IDS") || length(NFL_SHEET_IDS) == 0) {
    log_debug("NFL_SHEET_IDS not defined or empty!", level = "ERROR")
    return(character(0))
  }
  
  seasons <- as.numeric(names(NFL_SHEET_IDS))
  seasons <- sort(seasons[!is.na(seasons)], decreasing = TRUE)
  
  log_debug("Available seasons:", paste(seasons, collapse = ", "), level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(seasons)
}

# =============================================================================
# PLAYOFF WEEK CONFIGURATION
# =============================================================================

NFL_PLAYOFF_WEEKS <- c("super_bowl", "conference_games", "divisional_round", "wild_card")

NFL_PLAYOFF_LABELS <- c(
  "super_bowl" = "Super Bowl",
  "conference_games" = "Conference Games",
  "divisional_round" = "Divisional Round",
  "wild_card" = "Wild Card"
)

#' Check if a week identifier is a playoff week
is_playoff_week <- function(week) {
  as.character(week) %in% NFL_PLAYOFF_WEEKS
}

#' Get display label for a week
get_week_label <- function(week) {
  week_str <- as.character(week)
  if (week_str %in% names(NFL_PLAYOFF_LABELS)) {
    return(NFL_PLAYOFF_LABELS[[week_str]])
  }
  paste("Week", week)
}

#' Build sheet name prefix for a week (handles both regular and playoff weeks)
#' @param week Week identifier (numeric or string)
#' @return Sheet name prefix (e.g., "week_15" or "wild_card")
get_week_file_prefix <- function(week) {
  if (is_playoff_week(week)) {
    return(as.character(week))
  }
  paste0("week_", week)
}

#' Get available weeks for a season by reading Google Sheets worksheet names
#' @param season Year
#' @return Vector of available weeks (numeric for regular season, character for playoffs)
get_available_weeks <- function(season) {
  log_debug("get_available_weeks() called for season:", season, level = "INFO")
  
  # Get Google Sheet IDs for this season
  sheet_ids <- get_nfl_sheet_ids(season)
  if (is.null(sheet_ids)) {
    log_debug("No Google Sheet IDs for season:", season, level = "ERROR")
    return(character(0))
  }
  
  # Get all worksheet names from the projections sheet
  proj_sheets <- get_nfl_sheet_names(sheet_ids$projections)
  log_debug("Found projection sheets:", paste(proj_sheets, collapse = ", "), level = "INFO")
  
  if (length(proj_sheets) == 0) {
    log_debug("No projection sheets found", level = "WARN")
    return(character(0))
  }
  
  # =========================================================================
  # Worksheet names ARE the week identifiers directly:
  #   week_1, week_2, ..., week_18, wild_card, divisional_round, etc.
  # There is NO "_projections" suffix on worksheet names.
  # =========================================================================
  
  # Extract regular season weeks (numeric) - match "week_N" pattern exactly
  regular_weeks <- as.numeric(gsub("^week_(\\d+)$", "\\1", proj_sheets))
  regular_weeks <- sort(regular_weeks[!is.na(regular_weeks)], decreasing = TRUE)
  
  # Check for playoff weeks - match directly against sheet names
  playoff_weeks <- c()
  for (pw in NFL_PLAYOFF_WEEKS) {
    if (pw %in% proj_sheets) {
      playoff_weeks <- c(playoff_weeks, pw)
    }
  }
  
  # Combine: playoff weeks first (in order), then regular weeks (descending)
  all_weeks <- c(playoff_weeks, as.character(regular_weeks))
  
  log_debug("Available weeks:", paste(all_weeks, collapse = ", "), level = "INFO")
  
  return(all_weeks)
}