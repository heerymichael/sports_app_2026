# =============================================================================
# Sports Configuration
# 
# Defines available sports, their sections, and display properties
# Note: APP_COLORS is sourced from app_themes.R in global.R before this file
# =============================================================================

#' Get all available sports
#' @return Named list of sport configurations
get_sports_config <- function() {
  list(
    nfl = list(
      id = "nfl",
      name = "NFL",
      icon = "nfl.png",
      color = APP_COLORS$coral,
      color_light = "#E8B8A8",
      icon_scale = 1.2,  # 20% larger than base
      sections = c("projections", "handbuild", "optimiser", "ffpc_bestball"),
      default_section = "projections"
    ),
    
    soccer = list(
      id = "soccer",
      name = "Soccer",
      icon = "soccer.png",
      color = APP_COLORS$sage,
      color_light = "#C5D4B8",
      icon_scale = 1.2,  # 20% larger than base
      sections = c("team_dashboard", "player_dashboard", "fanteam_contests", "projections", "performance"),
      default_section = "team_dashboard"
    ),
    
    golf = list(
      id = "golf",
      name = "Golf",
      icon = "golf.png",
      color = APP_COLORS$gold,
      color_light = "#F5E0B8",
      icon_scale = 1.0,  # Base size (taller icon)
      sections = c("dashboard", "optimizer", "projections"),
      default_section = "dashboard"
    ),
    
    f1 = list(
      id = "f1",
      name = "Formula 1",
      icon = "f1.png",
      color = "#E5383B",
      color_light = "#F2A3A5",
      icon_scale = 1.2,
      sections = c("dashboard", "projections"),
      default_section = "dashboard"
    ),
    
    nhl = list(
      id = "nhl",
      name = "Ice Hockey",
      icon = "ice_hockey.png",
      color = APP_COLORS$frost,
      color_light = "#A3C1D9",
      icon_scale = 1.2,
      sections = c("dashboard", "projections"),
      default_section = "dashboard"
    )
  )
}

#' Get section definitions
#' @return Named list of section configurations
get_sections_config <- function() {
  list(
    # NFL sections
    projections = list(
      id = "projections",
      name = "Projections",
      icon = "table"
    ),
    
    handbuild = list(
      id = "handbuild",
      name = "Handbuild",
      icon = "edit"
    ),
    
    optimiser = list(
      id = "optimiser",
      name = "Optimiser",
      icon = "sliders"
    ),
    
    ffpc_bestball = list(
      id = "ffpc_bestball",
      name = "FFPC Bestball",
      icon = "trophy"
    ),
    
    # Soccer sections
    team_dashboard = list(
      id = "team_dashboard",
      name = "Team",
      icon = "users"
    ),
    
    player_dashboard = list(
      id = "player_dashboard",
      name = "Players",
      icon = "user"
    ),
    
    fanteam_contests = list(
      id = "fanteam_contests",
      name = "FanTeam",
      icon = "trophy"
    ),
    
    # Common sections
    dashboard = list(
      id = "dashboard",
      name = "Dashboard",
      icon = "grid"
    ),
    
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