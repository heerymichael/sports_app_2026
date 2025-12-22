# =============================================================================
# NFL Configuration
# 
# Centralized configuration for NFL modules including:
# - Team information and colors
# - Player name corrections for data matching
# - Lineup structure constants
# - Position settings
# =============================================================================

# =============================================================================
# UI THEMING
# =============================================================================

NFL_CARD_COLOR <- "coral"  # Consistent card header color for all NFL modules

# =============================================================================
# LINEUP STRUCTURE
# =============================================================================

NFL_LINEUP_STRUCTURE <- list(
  slots = c("QB", "RB1", "RB2", "WR1", "WR2", "WR3", "TE", "FLEX", "DST"),
  positions = c("QB", "RB", "WR", "TE", "DST"),
  flex_positions = c("RB", "WR", "TE"),
  total_players = 9,
  
  # Position requirements for optimization
  requirements = list(
    QB = list(min = 1, max = 1),
    RB = list(min = 2, max = 3),
    WR = list(min = 3, max = 4),
    TE = list(min = 1, max = 2),
    DST = list(min = 1, max = 1)
  ),
  
  # Display labels
  slot_labels = c(
    QB = "QB", RB1 = "RB", RB2 = "RB",
    WR1 = "WR", WR2 = "WR", WR3 = "WR",
    TE = "TE", FLEX = "FLX", DST = "DST"
  )
)

# Default salary caps by slate
NFL_SALARY_CAPS <- list(
  main = 130,
  late = 120
)

# =============================================================================
# TEAM INFORMATION
# =============================================================================

# Full team names
NFL_TEAM_NAMES <- c(
  "ARI" = "Arizona Cardinals",
  "ATL" = "Atlanta Falcons",
  "BAL" = "Baltimore Ravens",
  "BUF" = "Buffalo Bills",
  "CAR" = "Carolina Panthers",
  "CHI" = "Chicago Bears",
  "CIN" = "Cincinnati Bengals",
  "CLE" = "Cleveland Browns",
  "DAL" = "Dallas Cowboys",
  "DEN" = "Denver Broncos",
  "DET" = "Detroit Lions",
  "GB" = "Green Bay Packers",
  "HOU" = "Houston Texans",
  "IND" = "Indianapolis Colts",
  "JAX" = "Jacksonville Jaguars",
  "KC" = "Kansas City Chiefs",
  "LAC" = "Los Angeles Chargers",
  "LAR" = "Los Angeles Rams",
  "LV" = "Las Vegas Raiders",
  "MIA" = "Miami Dolphins",
  "MIN" = "Minnesota Vikings",
  "NE" = "New England Patriots",
  "NO" = "New Orleans Saints",
  "NYG" = "New York Giants",
  "NYJ" = "New York Jets",
  "PHI" = "Philadelphia Eagles",
  "PIT" = "Pittsburgh Steelers",
  "SEA" = "Seattle Seahawks",
  "SF" = "San Francisco 49ers",
  "TB" = "Tampa Bay Buccaneers",
  "TEN" = "Tennessee Titans",
  "WAS" = "Washington Commanders"
)

# Team abbreviation list (for validation)
NFL_TEAMS <- names(NFL_TEAM_NAMES)

# =============================================================================
# PLAYER NAME CORRECTIONS
# 
# Maps common name variations to the correct name for headshot/data matching
# Format: "incorrect_name" = "correct_name"
# =============================================================================

NFL_PLAYER_NAME_CORRECTIONS <- list(
  # Common suffix variations
  "Ken Walker" = "Kenneth Walker III",
  "Kenneth Walker III" = "Kenneth Walker",  # Reverse mapping for salary files
  "Luther Burden III" = "Luther Burden",
  "Calvin Austin III" = "Calvin Austin",
  "Ollie Gordon II" = "Ollie Gordon",
  
  # Punctuation variations
  "Amon-Ra St. Brown" = "Amon-Ra St Brown",
  "A.J. Brown" = "AJ Brown",
  "J.K. Dobbins" = "JK Dobbins",
  "T.J. Hockenson" = "TJ Hockenson",
  "D.K. Metcalf" = "D.K. Metcalf",
  "DK Metcalf" = "D.K. Metcalf",
  
  # Nickname variations
  "Hollywood Brown" = "Marquise Brown",
  "Gabe Davis" = "Gabriel Davis",
  
  # Case variations
  "DeMario Douglas" = "Demario Douglas",
  "KaVontae Turpin" = "Kavontae Turpin"
)

#' Apply name corrections to player names
#' @param names Vector of player names
#' @return Vector with corrected names
correct_player_names <- function(names) {
  sapply(names, function(name) {
    if (name %in% names(NFL_PLAYER_NAME_CORRECTIONS)) {
      NFL_PLAYER_NAME_CORRECTIONS[[name]]
    } else {
      name
    }
  }, USE.NAMES = FALSE)
}

#' Get full team name from abbreviation
#' @param abbr Team abbreviation (e.g., "KC")
#' @return Full team name or NA if not found
get_nfl_team_name <- function(abbr) {
  if (is.null(abbr) || is.na(abbr) || abbr == "") return(NA)
  NFL_TEAM_NAMES[abbr]
}

#' Validate team abbreviation
#' @param team Team abbreviation to check
#' @return TRUE if valid NFL team
is_valid_nfl_team <- function(team) {
  !is.null(team) && !is.na(team) && team %in% NFL_TEAMS
}

# =============================================================================
# POSITION STYLING
# =============================================================================

# Position badge colors (using CSS variables where possible)
NFL_POSITION_COLORS <- list(
  QB = list(bg = "#E8D5D5", text = "#8B4513"),
  RB = list(bg = "#D5E8D5", text = "#2E8B2E"),
  WR = list(bg = "#D5D5E8", text = "#4169E1"),
  TE = list(bg = "#E8E0D5", text = "#CD853F"),
  DST = list(bg = "#D5D5D5", text = "#2F4F4F")
)

#' Get position badge styling
#' @param position Position abbreviation
#' @return List with bg and text colors
get_position_colors <- function(position) {
  if (position %in% names(NFL_POSITION_COLORS)) {
    NFL_POSITION_COLORS[[position]]
  } else {
    list(bg = "#E0E0E0", text = "#3B3226")
  }
}

# =============================================================================
# COLUMN DEFINITIONS
# =============================================================================

# Standard projection columns
NFL_PROJECTION_COLUMNS <- c(
  "player", "team", "position", "salary",
  "full", "ceiling", "blended", "value"
)

# Display column names
NFL_COLUMN_LABELS <- c(
  player = "Player",
  team = "Team",
  position = "Pos",
  salary = "Salary",
  full = "Proj",
  ceiling = "Ceiling",
  blended = "Blended",
  value = "Value"
)

# =============================================================================
# EXPORT MESSAGE
# =============================================================================

message("NFL config loaded: NFL_LINEUP_STRUCTURE, NFL_TEAM_NAMES, NFL_PLAYER_NAME_CORRECTIONS")