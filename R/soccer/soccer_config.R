# =============================================================================
# Soccer Configuration
# 
# Team name mappings, logos, abbreviations, and Google Sheets IDs
# UPDATED: Added Understat league mappings (EPL, La_Liga, etc.)
# =============================================================================

# Google Sheet IDs
SOCCER_SHEET_IDS <- list(
  epl_shooting = "1hQrhyL9K7hJwJAOCxiMCRe-zjxVi7EL8lQzIw9DSSzE",
  possession = "1CjS5HMFIFtVeXHVOQfe_8oj-cQpdfIfZKZdq5yhAXLk",
  shots = "1oxQ6rk_B_r2QUNZxGssmDEopKqJD0otMt_r1hPOufT0",
  player_match_stats = "12MXPMsuI4S7EiTPnVpaqx5-riXoK37cEtPo-MFOf1fA",
  team_goals = "1gUCVxBFR3kwE259ZccWBLRn7iAlV0HpZL7OccbGsWuo"
)

# Google Drive settings
USE_GOOGLE_DRIVE <- TRUE
SOCCER_DRIVE_FOLDER_ID <- "1APlkMnjX3RjxPOzEnYWP5DYYCH_AcUM8"
SOCCER_DRIVE_FILES <- list(
  player_match_stats = "player_match_stats.parquet",
  shots = "shots.parquet",
  team_goals = "team_goals.parquet"
)

USE_PARQUET_CACHE <- TRUE
CACHE_DIR <- "data/cache"
CACHE_MAX_AGE_HOURS <- 24
PYTHAG_EXPONENT <- 1.35

# =============================================================================
# LEAGUE NAME MAPPINGS - INCLUDES BOTH FBREF AND UNDERSTAT FORMATS
# =============================================================================

LEAGUE_DISPLAY_NAMES <- c(
  # FBref format
  "Premier-League" = "Premier League",
  "Serie-A" = "Serie A",
  "La-Liga" = "La Liga",
  "Bundesliga" = "Bundesliga",
  "Championship" = "Championship",
  # Understat format
  "EPL" = "Premier League",
  "La_Liga" = "La Liga",
  "La Liga" = "La Liga",
  "Serie_A" = "Serie A",
  "Serie A" = "Serie A",
  "Ligue_1" = "Ligue 1",
  "Ligue 1" = "Ligue 1"
)

LEAGUE_DATA_NAMES <- c(
  "Premier League" = "Premier-League",
  "Serie A" = "Serie-A",
  "La Liga" = "La-Liga",
  "Bundesliga" = "Bundesliga",
  "Championship" = "Championship",
  "Ligue 1" = "Ligue_1"
)

UNDERSTAT_LEAGUE_RAW <- c(
  "Premier League" = "EPL",
  "La Liga" = "La_Liga",
  "Bundesliga" = "Bundesliga",
  "Serie A" = "Serie_A",
  "Ligue 1" = "Ligue_1"
)

# =============================================================================
# LEAGUE LOGO PATHS
# =============================================================================

LEAGUE_LOGO_PATHS <- c(
  "Premier League" = "soccer_logos/League Logos/Premier_League.png",
  "Serie A" = "soccer_logos/League Logos/Serie_A.jpeg",
  "La Liga" = "soccer_logos/League Logos/LaLiga.jpeg",
  "Bundesliga" = "soccer_logos/League Logos/Bundesliga.png",
  "Championship" = "soccer_logos/League Logos/Championship.png",
  "Ligue 1" = "soccer_logos/League Logos/Ligue_1.png"
)

# =============================================================================
# TEAM NAME NORMALIZATION
# =============================================================================

TEAM_NAME_MAPPING <- c(
  "Arsenal" = "Arsenal", "Aston Villa" = "Aston Villa", "Bournemouth" = "Bournemouth",
  "AFC Bournemouth" = "Bournemouth", "Brentford" = "Brentford", "Brighton" = "Brighton",
  "Brighton & Hove Albion" = "Brighton", "Brighton and Hove Albion" = "Brighton",
  "Chelsea" = "Chelsea", "Crystal Palace" = "Crystal Palace", "Everton" = "Everton",
  "Fulham" = "Fulham", "Ipswich" = "Ipswich Town", "Ipswich Town" = "Ipswich Town",
  "Leicester" = "Leicester City", "Leicester City" = "Leicester City", "Liverpool" = "Liverpool",
  "Manchester Utd" = "Manchester United", "Manchester United" = "Manchester United",
  "Man Utd" = "Manchester United", "Man United" = "Manchester United",
  "Newcastle Utd" = "Newcastle United", "Newcastle United" = "Newcastle United",
  "Nott'ham Forest" = "Nottingham Forest", "Nottingham Forest" = "Nottingham Forest",
  "Southampton" = "Southampton", "Tottenham" = "Tottenham", "Tottenham Hotspur" = "Tottenham",
  "Spurs" = "Tottenham", "West Ham" = "West Ham", "West Ham United" = "West Ham",
  "Wolves" = "Wolves", "Wolverhampton Wanderers" = "Wolves", "Manchester City" = "Manchester City",
  "Man City" = "Manchester City",
  "Inter" = "Inter Milan", "Inter Milan" = "Inter Milan", "Internazionale" = "Inter Milan",
  "Hellas Verona" = "Verona", "Verona" = "Verona", "Milan" = "AC Milan", "AC Milan" = "AC Milan",
  "Monza" = "Monza", "Venezia" = "Venezia", "Empoli" = "Empoli", "Juventus" = "Juventus",
  "Napoli" = "Napoli", "Roma" = "Roma", "AS Roma" = "Roma", "Lazio" = "Lazio",
  "Atalanta" = "Atalanta", "Fiorentina" = "Fiorentina", "Bologna" = "Bologna",
  "Torino" = "Torino", "Udinese" = "Udinese", "Sassuolo" = "Sassuolo", "Lecce" = "Lecce",
  "Genoa" = "Genoa", "Cagliari" = "Cagliari", "Parma" = "Parma", "Como" = "Como",
  "Betis" = "Real Betis", "Real Betis" = "Real Betis", "Atletico Madrid" = "Atletico Madrid",
  "Athletic Club" = "Athletic Bilbao", "Athletic Bilbao" = "Athletic Bilbao",
  "Alaves" = "Alaves", "Espanyol" = "Espanyol", "Villarreal" = "Villarreal",
  "Rayo Vallecano" = "Rayo Vallecano", "Sevilla" = "Sevilla", "Las Palmas" = "Las Palmas",
  "Leganes" = "Leganes", "Valladolid" = "Real Valladolid", "Real Valladolid" = "Real Valladolid",
  "Real Madrid" = "Real Madrid", "Barcelona" = "Barcelona", "Real Sociedad" = "Real Sociedad",
  "Valencia" = "Valencia", "Celta Vigo" = "Celta Vigo", "Celta" = "Celta Vigo",
  "Getafe" = "Getafe", "Osasuna" = "Osasuna", "Mallorca" = "Mallorca", "Girona" = "Girona",
  "Bayern Munich" = "Bayern Munich", "Bayern" = "Bayern Munich",
  "Borussia Dortmund" = "Borussia Dortmund", "Dortmund" = "Borussia Dortmund",
  "RB Leipzig" = "RB Leipzig", "Leipzig" = "RB Leipzig",
  "Bayer Leverkusen" = "Bayer Leverkusen", "Leverkusen" = "Bayer Leverkusen",
  "Eintracht Frankfurt" = "Eintracht Frankfurt", "Frankfurt" = "Eintracht Frankfurt",
  "Union Berlin" = "Union Berlin", "SC Freiburg" = "SC Freiburg", "Freiburg" = "SC Freiburg",
  "Wolfsburg" = "Wolfsburg", "Mainz" = "Mainz", "Mainz 05" = "Mainz",
  "Borussia Monchengladbach" = "Borussia Monchengladbach", "M'Gladbach" = "Borussia Monchengladbach",
  "Hoffenheim" = "Hoffenheim", "Werder Bremen" = "Werder Bremen", "Bremen" = "Werder Bremen",
  "VfB Stuttgart" = "VfB Stuttgart", "Stuttgart" = "VfB Stuttgart", "Augsburg" = "Augsburg",
  "Heidenheim" = "Heidenheim", "Bochum" = "Bochum", "Holstein Kiel" = "Holstein Kiel",
  "FC St Pauli" = "FC St Pauli", "St. Pauli" = "FC St Pauli",
  "Paris Saint-Germain" = "Paris Saint-Germain", "PSG" = "Paris Saint-Germain",
  "Paris S-G" = "Paris Saint-Germain", "Monaco" = "Monaco", "AS Monaco" = "Monaco",
  "Marseille" = "Marseille", "Olympique Marseille" = "Marseille", "Lyon" = "Lyon",
  "Olympique Lyon" = "Lyon", "Lille" = "Lille", "Nice" = "Nice", "Lens" = "Lens",
  "Rennes" = "Rennes", "Strasbourg" = "Strasbourg", "Nantes" = "Nantes",
  "Toulouse" = "Toulouse", "Montpellier" = "Montpellier", "Brest" = "Brest",
  "Reims" = "Reims", "Le Havre" = "Le Havre", "Auxerre" = "Auxerre",
  "Saint-Etienne" = "Saint-Etienne", "Angers" = "Angers"
)

# =============================================================================
# TEAM ABBREVIATIONS
# =============================================================================

TEAM_ABBREVIATIONS <- c(
  "Arsenal" = "ARS", "Aston Villa" = "AVL", "Bournemouth" = "BOU", "Brentford" = "BRE",
  "Brighton" = "BHA", "Chelsea" = "CHE", "Crystal Palace" = "CRY", "Everton" = "EVE",
  "Fulham" = "FUL", "Ipswich Town" = "IPS", "Leicester City" = "LEI", "Liverpool" = "LIV",
  "Manchester United" = "MUN", "Manchester City" = "MCI", "Newcastle United" = "NEW",
  "Nottingham Forest" = "NFO", "Southampton" = "SOU", "Tottenham" = "TOT",
  "West Ham" = "WHU", "Wolves" = "WOL",
  "AC Milan" = "MIL", "Inter Milan" = "INT", "Juventus" = "JUV", "Napoli" = "NAP",
  "Roma" = "ROM", "Lazio" = "LAZ", "Atalanta" = "ATA", "Fiorentina" = "FIO",
  "Bologna" = "BOL", "Torino" = "TOR", "Udinese" = "UDI", "Sassuolo" = "SAS",
  "Verona" = "VER", "Lecce" = "LEC", "Genoa" = "GEN", "Cagliari" = "CAG",
  "Empoli" = "EMP", "Monza" = "MON", "Venezia" = "VEN", "Parma" = "PAR", "Como" = "COM",
  "Real Madrid" = "RMA", "Barcelona" = "BAR", "Atletico Madrid" = "ATM",
  "Real Sociedad" = "RSO", "Real Betis" = "BET", "Athletic Bilbao" = "ATH",
  "Valencia" = "VAL", "Villarreal" = "VIL", "Sevilla" = "SEV", "Celta Vigo" = "CEL",
  "Getafe" = "GET", "Osasuna" = "OSA", "Mallorca" = "MAL", "Rayo Vallecano" = "RAY",
  "Girona" = "GIR", "Alaves" = "ALA", "Las Palmas" = "LPA", "Espanyol" = "ESP",
  "Leganes" = "LEG", "Real Valladolid" = "VLL",
  "Bayern Munich" = "BAY", "Borussia Dortmund" = "BVB", "RB Leipzig" = "RBL",
  "Bayer Leverkusen" = "B04", "Eintracht Frankfurt" = "SGE", "Union Berlin" = "FCU",
  "SC Freiburg" = "SCF", "Wolfsburg" = "WOB", "Mainz" = "M05",
  "Borussia Monchengladbach" = "BMG", "Hoffenheim" = "TSG", "Werder Bremen" = "SVW",
  "VfB Stuttgart" = "VFB", "Augsburg" = "FCA", "Heidenheim" = "HDH", "Bochum" = "BOC",
  "Holstein Kiel" = "KIE", "FC St Pauli" = "STP",
  "Paris Saint-Germain" = "PSG", "Monaco" = "MON", "Marseille" = "OM", "Lyon" = "OL",
  "Lille" = "LIL", "Nice" = "NIC", "Lens" = "LEN", "Rennes" = "REN", "Strasbourg" = "STR",
  "Nantes" = "NAN", "Toulouse" = "TOU", "Montpellier" = "MTP", "Brest" = "BRE29",
  "Reims" = "REI", "Le Havre" = "HAV", "Auxerre" = "AUX", "Saint-Etienne" = "STE", "Angers" = "ANG"
)

# =============================================================================
# TEAM LOGO PATHS
# =============================================================================

TEAM_LOGO_PATHS <- c(
  "Arsenal" = "soccer_logos/Premier League/arsenal.svg",
  "Aston Villa" = "soccer_logos/Premier League/aston_villa.svg",
  "Bournemouth" = "soccer_logos/Premier League/bournemouth.svg",
  "Brentford" = "soccer_logos/Premier League/brentford.svg",
  "Brighton" = "soccer_logos/Premier League/brighton.svg",
  "Chelsea" = "soccer_logos/Premier League/chelsea.svg",
  "Crystal Palace" = "soccer_logos/Premier League/crystal_palace.svg",
  "Everton" = "soccer_logos/Premier League/everton.svg",
  "Fulham" = "soccer_logos/Premier League/fulham.svg",
  "Ipswich Town" = "soccer_logos/Premier League/ipswich_town.svg",
  "Leicester City" = "soccer_logos/Premier League/leicester_city.svg",
  "Liverpool" = "soccer_logos/Premier League/liverpool.svg",
  "Manchester United" = "soccer_logos/Premier League/manchester_united.svg",
  "Manchester City" = "soccer_logos/Premier League/manchester_city.svg",
  "Newcastle United" = "soccer_logos/Premier League/newcastle_united.svg",
  "Nottingham Forest" = "soccer_logos/Premier League/nottingham_forest.svg",
  "Southampton" = "soccer_logos/Premier League/southampton.svg",
  "Tottenham" = "soccer_logos/Premier League/tottenham.svg",
  "West Ham" = "soccer_logos/Premier League/west_ham.svg",
  "Wolves" = "soccer_logos/Premier League/wolverhampton.svg"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

get_team_abbreviation <- function(team_name) {
  if (is.null(team_name) || length(team_name) == 0) return("???")
  if (length(team_name) > 1) return(sapply(team_name, get_team_abbreviation, USE.NAMES = FALSE))
  if (is.na(team_name) || team_name == "") return("???")
  if (team_name %in% names(TEAM_ABBREVIATIONS)) return(unname(TEAM_ABBREVIATIONS[team_name]))
  return(toupper(substr(team_name, 1, 3)))
}

normalize_team_names <- function(team_names) {
  sapply(team_names, function(name) {
    if (is.na(name) || name == "") return(name)
    if (name %in% names(TEAM_NAME_MAPPING)) return(TEAM_NAME_MAPPING[name])
    return(name)
  }, USE.NAMES = FALSE)
}

normalize_team_columns <- function(df, team_cols) {
  for (col in team_cols) {
    if (col %in% names(df)) df[[col]] <- normalize_team_names(df[[col]])
  }
  return(df)
}

get_soccer_team_logo <- function(team_name) {
  if (is.null(team_name) || is.na(team_name) || team_name == "") return(NULL)
  normalized <- normalize_team_names(team_name)
  if (normalized %in% names(TEAM_LOGO_PATHS)) return(unname(TEAM_LOGO_PATHS[normalized]))
  return(NULL)
}

get_league_logo <- function(league_name) {
  if (is.null(league_name) || is.na(league_name) || league_name == "") return(NULL)
  if (league_name %in% names(LEAGUE_LOGO_PATHS)) return(unname(LEAGUE_LOGO_PATHS[league_name]))
  return(NULL)
}

simplify_position <- function(position) {
  pos_map <- c("GK" = "GK", "DF" = "DEF", "CB" = "DEF", "LB" = "DEF", "RB" = "DEF",
               "WB" = "MID", "LWB" = "MID", "RWB" = "MID", "MF" = "MID", "DM" = "MID",
               "CM" = "MID", "AM" = "MID", "LM" = "MID", "RM" = "MID",
               "FW" = "FWD", "LW" = "FWD", "RW" = "FWD", "ST" = "FWD", "CF" = "FWD", "Sub" = "SUB")
  sapply(position, function(pos) {
    if (is.null(pos) || is.na(pos) || pos == "") return(NA_character_)
    primary <- toupper(trimws(strsplit(as.character(pos), ",")[[1]][1]))
    if (primary %in% names(pos_map)) return(unname(pos_map[primary]))
    return(NA_character_)
  }, USE.NAMES = FALSE)
}

get_position_color <- function(position) {
  colors <- c("GK" = "#f59e0b", "DEF" = "#3b82f6", "MID" = "#22c55e", "FWD" = "#ef4444")
  if (position %in% names(colors)) return(colors[position])
  return("#6b7280")
}