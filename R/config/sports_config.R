# =============================================================================
# Sports Configuration
# 
# Defines available sports, their sections, and display properties
# Note: APP_COLORS is sourced from app_themes.R in global.R before this file
#
# ARCHIVE NOTE (2026-01-24):
# - team_dashboard and player_dashboard removed from soccer sections
# - These modules relied on FBref data which is no longer available
# - Module files retained but not active: mod_soccer_team_dashboard.R,
#   mod_soccer_player_dashboard.R
# =============================================================================

#' Get all available sports
#' @return Named list of sport configurations (order determines nav display order)
get_sports_config <- function() {
  list(
    # Soccer is first/default
    soccer = list(
      id = "soccer",
      name = "Soccer",
      icon = "soccer.png",
      color = APP_COLORS$sage,
      color_light = "#C5D4B8",
      icon_scale = 1.2,
      sections = c("matchups", "player_stats", "handbuild", "showdown", "shot_share", "betting"),
      default_section = "matchups"
    ),
    
    golf = list(
      id = "golf",
      name = "Golf",
      icon = "golf.png",
      color = APP_COLORS$gold,
      color_light = "#F5E0B8",
      icon_scale = 1.0,
      sections = c("this_week", "season_management", "classic", "showdown"),
      default_section = "this_week"
    ),
    
    nfl = list(
      id = "nfl",
      name = "NFL",
      icon = "nfl.png",
      color = APP_COLORS$coral,
      color_light = "#E8B8A8",
      icon_scale = 1.2,
      sections = c("ffpc_bestball", "handbuild", "showdown", "projections", "fanteam_playoffs"),
      default_section = "ffpc_bestball"
    ),
    
    nhl = list(
      id = "nhl",
      name = "Ice Hockey",
      icon = "ice_hockey.png",
      color = APP_COLORS$frost,
      color_light = "#A3C1D9",
      icon_scale = 1.2,
      sections = c("dashboard", "projections", "handbuild"),
      default_section = "projections"
    ),
    
    f1 = list(
      id = "f1",
      name = "Formula 1",
      icon = "f1.png",
      color = APP_COLORS$coral,
      color_light = "#E8B8A8",
      icon_scale = 1.2,
      sections = c("dashboard"),
      default_section = "dashboard"
    )
  )
}

#' Get all section definitions
#' @return Named list of section configurations
get_sections_config <- function() {
  list(
    # =========================================================================
    # Soccer sections - ACTIVE
    # =========================================================================
    player_stats = list(
      id = "player_stats",
      name = "Player Stats",
      icon = "bar-chart-2"
    ),
    
    matchups = list(
      id = "matchups",
      name = "Match Ups",
      icon = "calendar"
    ),
    
    # Note: 'handbuild' is shared across sports (soccer, nfl, nhl)
    # The page container resolves to {sport}_handbuild_ui/server
    handbuild = list(
      id = "handbuild",
      name = "Handbuild",
      icon = "edit-3"
    ),
    
    # Note: 'showdown' is shared across sports (soccer, golf, nfl)
    # The page container resolves to {sport}_showdown_ui/server
    showdown = list(
      id = "showdown",
      name = "Showdown",
      icon = "crosshair"
    ),
    
    shot_share = list(
      id = "shot_share",
      name = "Shot Share",
      icon = "pie-chart"
    ),
    
    betting = list(
      id = "betting",
      name = "Betting",
      icon = "trending-up"
    ),
    
    # Soccer sections - ARCHIVED (legacy)
    # team_dashboard = list(id = "team_dashboard", name = "Team", icon = "users"),
    # player_dashboard = list(id = "player_dashboard", name = "Player", icon = "user"),
    # fanteam_contests = list(id = "fanteam_contests", name = "FanTeam", icon = "trophy"),
    
    # =========================================================================
    # NFL sections
    # =========================================================================
    ffpc_bestball = list(
      id = "ffpc_bestball",
      name = "FFPC Bestball",
      icon = "trophy"
    ),
    
    # handbuild already defined above (shared)
    
    # showdown already defined above (shared)
    
    projections = list(
      id = "projections",
      name = "Projections",
      icon = "table"
    ),
    
    fanteam_playoffs = list(
      id = "fanteam_playoffs",
      name = "FT Playoffs",
      icon = "award"
    ),
    
    # =========================================================================
    # Golf sections
    # =========================================================================
    this_week = list(
      id = "this_week",
      name = "This Week",
      icon = "calendar"
    ),
    
    season_management = list(
      id = "season_management",
      name = "Season Mgmt",
      icon = "layers"
    ),
    
    classic = list(
      id = "classic",
      name = "Classic",
      icon = "award"
    ),
    
    # showdown already defined above (shared)
    
    # =========================================================================
    # NHL sections
    # =========================================================================
    dashboard = list(
      id = "dashboard",
      name = "Dashboard",
      icon = "grid"
    ),
    
    # projections already defined above (shared)
    # handbuild already defined above (shared)
    
    # =========================================================================
    # Common sections (available to multiple sports)
    # =========================================================================
    optimizer = list(
      id = "optimizer",
      name = "Optimizer",
      icon = "sliders"
    ),
    
    performance = list(
      id = "performance",
      name = "Performance",
      icon = "trending-up"
    ),
    
    ownership = list(
      id = "ownership",
      name = "Ownership",
      icon = "percent"
    )
  )
}

#' Get sections available for a specific sport
#' @param sport_id Character, the sport identifier
#' @return List of section configs for that sport
get_sport_sections <- function(sport_id) {
  sports <- get_sports_config()
  sections <- get_sections_config()
  
  sport <- sports[[sport_id]]
  if (is.null(sport)) return(list())
  
  # Return only sections available for this sport
  sections[sport$sections]
}

#' Get sport config by ID
#' @param sport_id Character, the sport identifier
#' @return Sport configuration list or NULL
get_sport <- function(sport_id) {
  sports <- get_sports_config()
  sports[[sport_id]]
}