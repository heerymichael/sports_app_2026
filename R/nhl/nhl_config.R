# =============================================================================
# NHL Configuration
# 
# Team mappings, position definitions, FanTeam Classic scoring rules,
# and projection calculations
# =============================================================================

# =============================================================================
# NHL TEAM ABBREVIATIONS
# =============================================================================

NHL_TEAMS <- c(
  "ANA" = "Anaheim Ducks",
  "ARI" = "Arizona Coyotes",
  "BOS" = "Boston Bruins",
  "BUF" = "Buffalo Sabres",
  "CAR" = "Carolina Hurricanes",
  "CBJ" = "Columbus Blue Jackets",
  "CGY" = "Calgary Flames",
  "CHI" = "Chicago Blackhawks",
  "COL" = "Colorado Avalanche",
  "DAL" = "Dallas Stars",
  "DET" = "Detroit Red Wings",
  "EDM" = "Edmonton Oilers",
  "FLA" = "Florida Panthers",
  "LAK" = "Los Angeles Kings",
  "MIN" = "Minnesota Wild",
  "MTL" = "Montreal Canadiens",
  "NJD" = "New Jersey Devils",
  "NSH" = "Nashville Predators",
  "NYI" = "New York Islanders",
  "NYR" = "New York Rangers",
  "OTT" = "Ottawa Senators",
  "PHI" = "Philadelphia Flyers",
  "PIT" = "Pittsburgh Penguins",
  "SEA" = "Seattle Kraken",
  "SJS" = "San Jose Sharks",
  "STL" = "St. Louis Blues",
  "TBL" = "Tampa Bay Lightning",
  "TOR" = "Toronto Maple Leafs",
  "UTA" = "Utah Hockey Club",
  "VAN" = "Vancouver Canucks",
  "VGK" = "Vegas Golden Knights",
  "WPG" = "Winnipeg Jets",
  "WSH" = "Washington Capitals"
)

# Alternate abbreviations used by different sources
NHL_TEAM_ALIASES <- list(
  "MON" = "MTL",
  "TB" = "TBL",
  "SJ" = "SJS",
  "LA" = "LAK",
  "NJ" = "NJD",
  "CLB" = "CBJ",
  "WAS" = "WSH",
  "WIN" = "WPG",
  "VEG" = "VGK",
  "PHX" = "ARI",
  "AZ" = "ARI",
  # Rotowire specific
  "TBL" = "TBL",
  "SJS" = "SJS"
)

#' Normalize NHL team abbreviation
#' @param abbr Team abbreviation from any source
#' @return Standardized 3-letter abbreviation
normalize_nhl_team <- function(abbr) {
  if (is.null(abbr) || is.na(abbr)) return(NA_character_)
  
  abbr <- toupper(trimws(abbr))
  
  # Check if it's an alias
  if (abbr %in% names(NHL_TEAM_ALIASES)) {
    return(NHL_TEAM_ALIASES[[abbr]])
  }
  
  # Check if it's already valid
  if (abbr %in% names(NHL_TEAMS)) {
    return(abbr)
  }
  
  return(abbr)
}

#' Get NHL team logo path
#' @param team Team abbreviation
#' @return Path to team logo SVG
get_nhl_team_logo <- function(team) {
  if (is.null(team) || is.na(team) || team == "") {
    return(NULL)
  }
  
  team <- normalize_nhl_team(team)
  
  # Special case for Washington (uses secondary logo)
  if (team == "WSH") {
    return("nhl_logos/WSH_secondary_light.svg")
  }
  
  sprintf("nhl_logos/%s_light.svg", team)
}

# =============================================================================
# POSITION DEFINITIONS
# =============================================================================

# FanTeam NHL positions
# Note: FanTeam uses "winger" which we normalize to "W"
# Projections may use LW/RW which we keep for display but treat as W for slot assignment
NHL_POSITIONS <- c("C", "LW", "RW", "W", "D", "G")

# Position display order
NHL_POSITION_ORDER <- c("C" = 1, "LW" = 2, "RW" = 3, "W" = 4, "D" = 5, "G" = 6)

# Position colors (for badges) - using frost theme for hockey
NHL_POSITION_COLORS <- list(
  "C" = list(bg = "#E8B8A8", text = "#3B3226"),   # Coral light - Centers
  "LW" = list(bg = "#C5D4B8", text = "#3B3226"),  # Sage light - Left Wing
  "RW" = list(bg = "#C5D4B8", text = "#3B3226"),  # Sage light - Right Wing
  "W" = list(bg = "#C5D4B8", text = "#3B3226"),   # Sage light - Wing (generic)
  "D" = list(bg = "#A3C1D9", text = "#3B3226"),   # Frost - Defense
  "G" = list(bg = "#F5E0B8", text = "#3B3226"),   # Gold light - Goalie
  "FLEX" = list(bg = "#D8D0C4", text = "#3B3226") # Neutral - Flex
)

# Position mapping from different sources
NHL_POSITION_ALIASES <- list(
  "CENTER" = "C",
  "CENTRE" = "C",
  "LEFT WING" = "LW",
  "RIGHT WING" = "RW",
  "WINGER" = "W",       # FanTeam uses generic "winger" - keep as W
  "DEFENSE" = "D",
  "DEFENDER" = "D",     # FanTeam uses "defender"
  "DEFENSEMAN" = "D",
  "GOALIE" = "G",
  "GOALKEEPER" = "G",   # FanTeam uses "goalkeeper"
  "GOALTENDER" = "G",
  "F" = "C",            # Generic forward maps to C
  "LW" = "LW",          # Keep specific wing positions from projections
  "RW" = "RW"
)

#' Normalize NHL position
#' @param pos Position from any source
#' @return Standardized position abbreviation
normalize_nhl_position <- function(pos) {
  if (is.null(pos) || is.na(pos)) return(NA_character_)
  
  pos <- toupper(trimws(pos))
  
  # Check if it's an alias
  if (pos %in% names(NHL_POSITION_ALIASES)) {
    return(NHL_POSITION_ALIASES[[pos]])
  }
  
  # Check if it's already valid
  if (pos %in% NHL_POSITIONS) {
    return(pos)
  }
  
  return(pos)
}

#' Check if position is a winger (LW or RW)
#' @param pos Position
#' @return Boolean
is_winger <- function(pos) {
  pos <- normalize_nhl_position(pos)
  return(pos %in% c("LW", "RW"))
}

# =============================================================================
# FANTEAM CLASSIC SCORING (NHL)
# =============================================================================

# FanTeam NHL Classic scoring system
NHL_FANTEAM_SCORING <- list(
  # Skater scoring
  skater = list(
    goal = 8.5,
    assist = 5.0,
    sh_bonus = 2.0,            # Short handed point bonus (per G or A)
    sog = 1.5,                 # Shot on goal
    sog_5_plus_bonus = 3.0,    # 5+ shots bonus
    block = 1.3,               # Blocked shot
    block_3_plus_bonus = 3.0,  # 3+ blocks bonus
    points_3_plus_bonus = 3.0, # 3+ points bonus
    hat_trick_bonus = 3.0,     # Hat trick bonus
    shootout_goal = 1.5        # Shootout goal
  ),
  
  # Goalie scoring
  goalie = list(
    win = 6.0,
    ot_win = 6.0,              # OT or shootout win (same as regular win)
    ot_loss = 2.0,             # Overtime loss
    save = 0.7,
    saves_35_plus_bonus = 3.0, # 35+ saves bonus
    goal_against = -3.5,
    shutout = 4.0              # Must play full game
  )
)

#' Calculate FanTeam fantasy points for a skater (MEDIAN projection)
#' @param projections Named list or data frame row with projection stats
#' @return Numeric fantasy points (median)
calculate_nhl_median_fpts <- function(projections) {
  scoring <- NHL_FANTEAM_SCORING$skater
  
  pts <- 0
  
  # Goals
  goals <- projections$G %||% 0
  if (is.na(goals)) goals <- 0
  pts <- pts + (goals * scoring$goal)
  
  # Assists
  assists <- projections$A %||% 0
  if (is.na(assists)) assists <- 0
  pts <- pts + (assists * scoring$assist)
  
  # Short handed bonus (for SH goals and assists)
  sh_g <- projections$SH_G %||% 0
  sh_a <- projections$SH_A %||% 0
  if (is.na(sh_g)) sh_g <- 0
  if (is.na(sh_a)) sh_a <- 0
  pts <- pts + (sh_g * scoring$sh_bonus) + (sh_a * scoring$sh_bonus)
  
  # Shots on goal
  sog <- projections$SOG %||% 0
  if (is.na(sog)) sog <- 0
  pts <- pts + (sog * scoring$sog)
  # 5+ shots bonus (probability-weighted)
  if (sog >= 3) {
    prob_5_plus <- pnorm(4.5, mean = sog, sd = max(sog * 0.5, 1), lower.tail = FALSE)
    pts <- pts + (prob_5_plus * scoring$sog_5_plus_bonus)
  }
  
  # Blocked shots
  blocks <- projections$BS %||% 0
  if (is.na(blocks)) blocks <- 0
  pts <- pts + (blocks * scoring$block)
  # 3+ blocks bonus (probability-weighted)
  if (blocks >= 1.5) {
    prob_3_plus <- pnorm(2.5, mean = blocks, sd = max(blocks * 0.6, 0.8), lower.tail = FALSE)
    pts <- pts + (prob_3_plus * scoring$block_3_plus_bonus)
  }
  
  # 3+ points bonus (probability-weighted)
  total_points <- goals + assists
  if (total_points >= 1) {
    prob_3_plus_pts <- pnorm(2.5, mean = total_points, sd = max(total_points * 0.7, 0.5), lower.tail = FALSE)
    pts <- pts + (prob_3_plus_pts * scoring$points_3_plus_bonus)
  }
  
  # Hat trick bonus (probability-weighted)
  if (goals >= 0.5) {
    prob_hat_trick <- pnorm(2.5, mean = goals, sd = max(goals * 0.8, 0.4), lower.tail = FALSE)
    pts <- pts + (prob_hat_trick * scoring$hat_trick_bonus)
  }
  
  return(round(pts, 2))
}

#' Calculate FanTeam fantasy points for a skater (CEILING projection)
#' @param projections Named list or data frame row with projection stats
#' @return Numeric fantasy points (ceiling - approximately 85th percentile)
calculate_nhl_ceiling_fpts <- function(projections) {
  scoring <- NHL_FANTEAM_SCORING$skater
  
  pts <- 0
  
  # For ceiling, we use ~1.3-1.5x multiplier on counting stats
  # representing roughly the 85th percentile outcome
  
  # Goals (ceiling ~1.5x for high-variance stat)
  goals_base <- projections$G %||% 0
  if (is.na(goals_base)) goals_base <- 0
  goals <- goals_base * 1.5
  pts <- pts + (goals * scoring$goal)
  
  # Assists (ceiling ~1.4x)
  assists_base <- projections$A %||% 0
  if (is.na(assists_base)) assists_base <- 0
  assists <- assists_base * 1.4
  pts <- pts + (assists * scoring$assist)
  
  # Short handed bonus
  sh_g <- (projections$SH_G %||% 0) * 1.5
  sh_a <- (projections$SH_A %||% 0) * 1.5
  if (is.na(sh_g)) sh_g <- 0
  if (is.na(sh_a)) sh_a <- 0
  pts <- pts + (sh_g * scoring$sh_bonus) + (sh_a * scoring$sh_bonus)
  
  # Shots on goal (ceiling ~1.3x, less variable)
  sog_base <- projections$SOG %||% 0
  if (is.na(sog_base)) sog_base <- 0
  sog <- sog_base * 1.3
  pts <- pts + (sog * scoring$sog)
  # Higher probability of 5+ shots in ceiling scenario
  if (sog >= 4) {
    pts <- pts + scoring$sog_5_plus_bonus * 0.75
  } else if (sog >= 3) {
    pts <- pts + scoring$sog_5_plus_bonus * 0.4
  }
  
  # Blocked shots (ceiling ~1.4x)
  blocks_base <- projections$BS %||% 0
  if (is.na(blocks_base)) blocks_base <- 0
  blocks <- blocks_base * 1.4
  pts <- pts + (blocks * scoring$block)
  # Higher probability of 3+ blocks in ceiling
  if (blocks >= 2) {
    pts <- pts + scoring$block_3_plus_bonus * 0.65
  } else if (blocks >= 1.5) {
    pts <- pts + scoring$block_3_plus_bonus * 0.35
  }
  
  # 3+ points bonus (higher probability in ceiling)
  total_points <- goals + assists
  if (total_points >= 2.5) {
    pts <- pts + scoring$points_3_plus_bonus * 0.7
  } else if (total_points >= 1.5) {
    pts <- pts + scoring$points_3_plus_bonus * 0.35
  }
  
  # Hat trick bonus (possible in ceiling for high-goal players)
  if (goals >= 2) {
    pts <- pts + scoring$hat_trick_bonus * 0.25
  } else if (goals >= 1.2) {
    pts <- pts + scoring$hat_trick_bonus * 0.1
  }
  
  return(round(pts, 2))
}

#' Calculate FanTeam fantasy points for a goalie (MEDIAN projection)
#' @param projections Named list with goalie projection stats
#' @return Numeric fantasy points (median)
calculate_nhl_goalie_median_fpts <- function(projections) {
  scoring <- NHL_FANTEAM_SCORING$goalie
  
  pts <- 0
  
  # Win points (projected wins * win value)
  win <- projections$W %||% 0
  if (is.na(win)) win <- 0
  pts <- pts + (win * scoring$win)
  
  # OT Loss points
  otl <- projections$OTL %||% 0
  if (is.na(otl)) otl <- 0
  pts <- pts + (otl * scoring$ot_loss)
  
  # Save points
  saves <- projections$SV %||% 0
  if (is.na(saves)) saves <- 0
  pts <- pts + (saves * scoring$save)
  
  # 35+ saves bonus (probability-weighted)
  if (saves >= 25) {
    prob_35_plus <- pnorm(34.5, mean = saves, sd = max(saves * 0.25, 4), lower.tail = FALSE)
    pts <- pts + (prob_35_plus * scoring$saves_35_plus_bonus)
  }
  
  # Goals against (negative points)
  ga <- projections$GA %||% 0
  if (is.na(ga)) ga <- 0
  pts <- pts + (ga * scoring$goal_against)
  
  # Shutout points (probability-weighted)
  shutout_proj <- projections$SO %||% 0
  if (is.na(shutout_proj)) shutout_proj <- 0
  pts <- pts + (shutout_proj * scoring$shutout)
  
  return(round(pts, 2))
}

#' Calculate FanTeam fantasy points for a goalie (CEILING projection)
#' @param projections Named list with goalie projection stats
#' @return Numeric fantasy points (ceiling)
calculate_nhl_goalie_ceiling_fpts <- function(projections) {
  scoring <- NHL_FANTEAM_SCORING$goalie
  
  pts <- 0
  
  # In ceiling scenario, assume win is more likely
  win_base <- projections$W %||% 0
  if (is.na(win_base)) win_base <- 0
  # Boost win probability for ceiling
  win <- min(win_base * 1.4, 1.0)
  pts <- pts + (win * scoring$win)
  
  # OT Loss (less likely in ceiling)
  otl <- (projections$OTL %||% 0) * 0.7
  if (is.na(otl)) otl <- 0
  pts <- pts + (otl * scoring$ot_loss)
  
  # More saves in ceiling scenario
  saves_base <- projections$SV %||% 0
  if (is.na(saves_base)) saves_base <- 0
  saves <- saves_base * 1.25
  pts <- pts + (saves * scoring$save)
  
  # Higher chance of 35+ saves
  if (saves >= 28) {
    pts <- pts + scoring$saves_35_plus_bonus * 0.6
  } else if (saves >= 22) {
    pts <- pts + scoring$saves_35_plus_bonus * 0.25
  }
  
  # Fewer goals against in ceiling
  ga_base <- projections$GA %||% 0
  if (is.na(ga_base)) ga_base <- 0
  ga <- ga_base * 0.65
  pts <- pts + (ga * scoring$goal_against)
  
  # Better shutout chance in ceiling
  shutout_base <- projections$SO %||% 0
  if (is.na(shutout_base)) shutout_base <- 0
  shutout <- min(shutout_base * 2.5, 0.4)
  pts <- pts + (shutout * scoring$shutout)
  
  return(round(pts, 2))
}

# =============================================================================
# LINEUP CONFIGURATION (FanTeam NHL)
# =============================================================================

# FanTeam NHL Classic lineup slots (9 players)
# 1 GK, 2 D, 3 W, 2 C, 1 FLEX
NHL_LINEUP_CLASSIC <- list(
  G = list(position = "G", label = "GK", required = TRUE, eligible = c("G")),
  D1 = list(position = "D", label = "D1", required = TRUE, eligible = c("D")),
  D2 = list(position = "D", label = "D2", required = TRUE, eligible = c("D")),
  W1 = list(position = "W", label = "W1", required = TRUE, eligible = c("LW", "RW", "W")),
  W2 = list(position = "W", label = "W2", required = TRUE, eligible = c("LW", "RW", "W")),
  W3 = list(position = "W", label = "W3", required = TRUE, eligible = c("LW", "RW", "W")),
  C1 = list(position = "C", label = "C1", required = TRUE, eligible = c("C")),
  C2 = list(position = "C", label = "C2", required = TRUE, eligible = c("C")),
  FLEX = list(position = "FLEX", label = "FLEX", required = TRUE, eligible = c("C", "LW", "RW", "W", "D"))
)

# FanTeam NHL Classic (Limited) lineup slots (6 players)
# 1 GK, 2 D, 2 W, 1 C - for 2-game slates
NHL_LINEUP_CLASSIC_LIMITED <- list(
  G = list(position = "G", label = "GK", required = TRUE, eligible = c("G")),
  D1 = list(position = "D", label = "D1", required = TRUE, eligible = c("D")),
  D2 = list(position = "D", label = "D2", required = TRUE, eligible = c("D")),
  W1 = list(position = "W", label = "W1", required = TRUE, eligible = c("LW", "RW", "W")),
  W2 = list(position = "W", label = "W2", required = TRUE, eligible = c("LW", "RW", "W")),
  C1 = list(position = "C", label = "C1", required = TRUE, eligible = c("C"))
)

# Display order for lineup slots
NHL_LINEUP_ORDER_CLASSIC <- c("G", "D1", "D2", "W1", "W2", "W3", "C1", "C2", "FLEX")
NHL_LINEUP_ORDER_LIMITED <- c("G", "D1", "D2", "W1", "W2", "C1")

# Roster format options
NHL_ROSTER_FORMATS <- list(
  classic = list(
    id = "classic",
    name = "Classic (9 players)",
    description = "1 GK, 2 D, 3 W, 2 C, 1 FLEX",
    slots = NHL_LINEUP_CLASSIC,
    order = NHL_LINEUP_ORDER_CLASSIC,
    roster_size = 9
  ),
  classic_limited = list(
    id = "classic_limited", 
    name = "Classic Limited (6 players)",
    description = "1 GK, 2 D, 2 W, 1 C",
    slots = NHL_LINEUP_CLASSIC_LIMITED,
    order = NHL_LINEUP_ORDER_LIMITED,
    roster_size = 6
  )
)

# Legacy aliases for backwards compatibility
NHL_LINEUP_SLOTS <- NHL_LINEUP_CLASSIC
NHL_LINEUP_ORDER <- NHL_LINEUP_ORDER_CLASSIC
NHL_ROSTER_SIZE <- 9

# Salary cap (typical FanTeam budget)
NHL_SALARY_CAP <- 100.0

# =============================================================================
# NAME NORMALIZATION
# =============================================================================

#' Normalize player name for matching
#' @param name Player name
#' @return Normalized name (lowercase, no accents, standardized)
normalize_nhl_name <- function(name) {
  if (is.null(name) || is.na(name)) return(NA_character_)
  
  name <- trimws(name)
  
  # Convert to lowercase
  name <- tolower(name)
  
  # Remove accents
  name <- stringi::stri_trans_general(name, "Latin-ASCII")
  
  # Remove common suffixes
  name <- gsub("\\s+(jr\\.?|sr\\.?|ii|iii|iv)$", "", name, ignore.case = TRUE)
  
  # Remove periods and extra spaces
  name <- gsub("\\.", "", name)
  name <- gsub("\\s+", " ", name)
  
  # Handle common name variations
  name <- gsub("^(alexander|aleksandr|alex)\\s", "alex ", name)
  name <- gsub("^(michael|mike)\\s", "mike ", name)
  name <- gsub("^(nicholas|nick)\\s", "nick ", name)
  name <- gsub("^(christopher|chris)\\s", "chris ", name)
  name <- gsub("^(matthew|matt)\\s", "matt ", name)
  name <- gsub("^(william|will|bill)\\s", "will ", name)
  name <- gsub("^(robert|rob|bob)\\s", "rob ", name)
  name <- gsub("^(anthony|tony)\\s", "tony ", name)
  
  return(trimws(name))
}

#' Match FanTeam players to projections
#' @param fanteam_data Data frame with FanTeam player data
#' @param projections_data Data frame with projections
#' @return List with matched data and unmatched players
match_nhl_players <- function(fanteam_data, projections_data) {
  log_debug("========================================", level = "INFO")
  log_debug("match_nhl_players() called", level = "INFO")
  log_debug("FanTeam players:", nrow(fanteam_data), level = "INFO")
  log_debug("Projection players:", nrow(projections_data), level = "INFO")
  log_debug("Projection columns:", paste(names(projections_data), collapse = ", "), level = "DEBUG")
  
  # Create normalized name columns
  fanteam_data <- fanteam_data %>%
    mutate(
      name_normalized = sapply(player_name, normalize_nhl_name),
      team_normalized = sapply(team, normalize_nhl_team)
    )
  
  projections_data <- projections_data %>%
    mutate(
      name_normalized = sapply(player_name, normalize_nhl_name),
      team_normalized = sapply(team, normalize_nhl_team)
    )
  
  # First pass: exact name + team match
  # Select only columns we want to join (exclude player_name, team, position which we keep from FanTeam)
  proj_cols_to_join <- setdiff(names(projections_data), c("player_name", "team", "position"))
  
  proj_for_join <- projections_data %>%
    select(all_of(proj_cols_to_join)) %>%
    rename_with(~paste0("proj_", .), -c(name_normalized, team_normalized))
  
  log_debug("Columns for join:", paste(names(proj_for_join), collapse = ", "), level = "DEBUG")
  
  matched_data <- fanteam_data %>%
    left_join(proj_for_join, by = c("name_normalized", "team_normalized"))
  
  log_debug("Matched data columns:", paste(names(matched_data), collapse = ", "), level = "DEBUG")
  
  # Track unmatched - use proj_G if exists, otherwise check any proj_ column
  proj_check_col <- if ("proj_G" %in% names(matched_data)) "proj_G" else {
    proj_cols <- grep("^proj_", names(matched_data), value = TRUE)
    if (length(proj_cols) > 0) proj_cols[1] else NULL
  }
  
  if (is.null(proj_check_col)) {
    log_debug("WARNING: No proj_ columns found after join!", level = "WARN")
    return(list(
      matched = matched_data,
      unmatched = fanteam_data %>% select(player_name, team, position, salary),
      match_rate = 0
    ))
  }
  
  log_debug("Using column for match check:", proj_check_col, level = "DEBUG")
  
  unmatched_fanteam <- matched_data %>%
    filter(is.na(.data[[proj_check_col]])) %>%
    select(player_name, team, position, salary) %>%
    distinct()
  
  # Second pass: name-only match for unmatched (different team abbreviations)
  if (nrow(unmatched_fanteam) > 0) {
    log_debug("Attempting name-only match for", nrow(unmatched_fanteam), "players", level = "INFO")
    
    for (i in 1:nrow(unmatched_fanteam)) {
      player_name_norm <- normalize_nhl_name(unmatched_fanteam$player_name[i])
      
      # Find in projections by name only
      proj_match <- projections_data %>%
        filter(name_normalized == player_name_norm)
      
      if (nrow(proj_match) == 1) {
        # Update matched_data for this player
        match_idx <- which(matched_data$name_normalized == player_name_norm)
        if (length(match_idx) > 0) {
          for (col in names(projections_data)) {
            if (col %in% c("player_name", "team", "position", "name_normalized", "team_normalized")) next
            proj_col <- paste0("proj_", col)
            if (proj_col %in% names(matched_data)) {
              matched_data[match_idx, proj_col] <- proj_match[[col]][1]
            }
          }
        }
      }
    }
  }
  
  # Final unmatched
  final_unmatched <- matched_data %>%
    filter(is.na(.data[[proj_check_col]]) | .data[[proj_check_col]] == 0) %>%
    filter(position != "G") %>%  # Goalies often not in skater projections
    select(player_name, team, position, salary) %>%
    distinct()
  
  # Matched count (excluding goalies for skater projections)
  matched_count <- matched_data %>%
    filter((!is.na(.data[[proj_check_col]]) & .data[[proj_check_col]] > 0) | position == "G") %>%
    nrow()
  
  log_debug("Matched players:", matched_count, level = "INFO")
  log_debug("Unmatched players:", nrow(final_unmatched), level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(list(
    matched = matched_data,
    unmatched = final_unmatched,
    match_rate = matched_count / nrow(fanteam_data) * 100
  ))
}

message("NHL config loaded: NHL_TEAMS, NHL_POSITIONS, NHL_FANTEAM_SCORING (Classic), NHL_LINEUP_SLOTS (9 players)")

#' Match FanTeam goalies to goalie projections
#' @param fanteam_data Data frame with FanTeam goalie data
#' @param projections_data Data frame with goalie projections
#' @return List with matched data and unmatched players
match_nhl_goalies <- function(fanteam_data, projections_data) {
  log_debug("========================================", level = "INFO")
  log_debug("match_nhl_goalies() called", level = "INFO")
  log_debug("FanTeam goalies:", nrow(fanteam_data), level = "INFO")
  log_debug("Projection goalies:", nrow(projections_data), level = "INFO")
  
  # Create normalized name columns
  fanteam_data <- fanteam_data %>%
    mutate(
      name_normalized = sapply(player_name, normalize_nhl_name),
      team_normalized = sapply(team, normalize_nhl_team)
    )
  
  projections_data <- projections_data %>%
    mutate(
      name_normalized = sapply(player_name, normalize_nhl_name),
      team_normalized = sapply(team, normalize_nhl_team)
    )
  
  # First pass: exact name + team match
  matched_data <- fanteam_data %>%
    left_join(
      projections_data %>%
        select(-player_name, -team, -position) %>%
        rename_with(~paste0("proj_", .), -c(name_normalized, team_normalized)),
      by = c("name_normalized", "team_normalized")
    )
  
  # Track unmatched
  unmatched_fanteam <- matched_data %>%
    filter(is.na(proj_W)) %>%
    select(player_name, team, position, salary) %>%
    distinct()
  
  # Second pass: name-only match for unmatched (different team abbreviations)
  if (nrow(unmatched_fanteam) > 0) {
    log_debug("Attempting name-only match for", nrow(unmatched_fanteam), "goalies", level = "INFO")
    
    for (i in 1:nrow(unmatched_fanteam)) {
      player_name_norm <- normalize_nhl_name(unmatched_fanteam$player_name[i])
      
      # Find in projections by name only
      proj_match <- projections_data %>%
        filter(name_normalized == player_name_norm)
      
      if (nrow(proj_match) == 1) {
        match_idx <- which(matched_data$name_normalized == player_name_norm)
        if (length(match_idx) > 0) {
          for (col in names(projections_data)) {
            if (col %in% c("player_name", "team", "position", "name_normalized", "team_normalized")) next
            proj_col <- paste0("proj_", col)
            if (proj_col %in% names(matched_data)) {
              matched_data[match_idx, proj_col] <- proj_match[[col]][1]
            }
          }
        }
      }
    }
  }
  
  # Final unmatched
  final_unmatched <- matched_data %>%
    filter(is.na(proj_W) | proj_W == 0) %>%
    select(player_name, team, position, salary) %>%
    distinct()
  
  matched_count <- matched_data %>%
    filter(!is.na(proj_W) & proj_W > 0) %>%
    nrow()
  
  log_debug("Matched goalies:", matched_count, level = "INFO")
  log_debug("Unmatched goalies:", nrow(final_unmatched), level = "INFO")
  log_debug("========================================", level = "INFO")
  
  return(list(
    matched = matched_data,
    unmatched = final_unmatched,
    match_rate = matched_count / nrow(fanteam_data) * 100
  ))
}