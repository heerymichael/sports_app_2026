# =============================================================================
# Soccer Configuration
# 
# Team name mappings, logos, abbreviations, and Google Sheets IDs
# Extracted from soccer_data_loader.R for maintainability
# =============================================================================

# Google Sheet IDs
SOCCER_SHEET_IDS <- list(
  epl_shooting = "1hQrhyL9K7hJwJAOCxiMCRe-zjxVi7EL8lQzIw9DSSzE",
  possession = "1CjS5HMFIFtVeXHVOQfe_8oj-cQpdfIfZKZdq5yhAXLk",
  shots = "1oxQ6rk_B_r2QUNZxGssmDEopKqJD0otMt_r1hPOufT0",
  # Combined player stats (from scraper)
  player_match_stats = "12MXPMsuI4S7EiTPnVpaqx5-riXoK37cEtPo-MFOf1fA",
  # Team goals with actual match scores (includes own goals)
  team_goals = "1gUCVxBFR3kwE259ZccWBLRn7iAlV0HpZL7OccbGsWuo"
)

# Use Parquet format for faster cache reads (requires arrow package)
USE_PARQUET_CACHE <- TRUE

# Cache settings
CACHE_DIR <- "data/cache"
CACHE_MAX_AGE_HOURS <- 24  # Refresh from Google Sheets if cache older than this

# Pythagorean exponent for soccer (well-validated value)
PYTHAG_EXPONENT <- 1.35

# =============================================================================
# LEAGUE NAME MAPPINGS
# =============================================================================

# League name mapping (from data to display)
LEAGUE_DISPLAY_NAMES <- c(
  "Premier-League" = "Premier League",
  "Serie-A" = "Serie A",
  "La-Liga" = "La Liga",
  "Bundesliga" = "Bundesliga",
  "Championship" = "Championship"
)

# Reverse mapping for filtering
LEAGUE_DATA_NAMES <- c(
  "Premier League" = "Premier-League",
  "Serie A" = "Serie-A",
  "La Liga" = "La-Liga",
  "Bundesliga" = "Bundesliga",
  "Championship" = "Championship"
)

# =============================================================================
# TEAM NAME NORMALIZATION
# =============================================================================

# Canonical team names - all variants map to these
TEAM_NAME_MAPPING <- c(
  # Premier League
  "Brighton" = "Brighton",
  "Brighton & Hove Albion" = "Brighton",
  "Brighton and Hove Albion" = "Brighton",
  "Ipswich" = "Ipswich Town",
  "Ipswich Town" = "Ipswich Town",
  "Leicester" = "Leicester City",
  "Leicester City" = "Leicester City",
  "Manchester Utd" = "Manchester United",
  "Manchester United" = "Manchester United",
  "Man Utd" = "Manchester United",
  "Man United" = "Manchester United",
  "Newcastle Utd" = "Newcastle United",
  "Newcastle United" = "Newcastle United",
  "Nott'ham Forest" = "Nottingham Forest",
  "Nottingham Forest" = "Nottingham Forest",
  "Notts Forest" = "Nottingham Forest",
  "Southampton" = "Southampton",
  "Tottenham" = "Tottenham",
  "Tottenham Hotspur" = "Tottenham",
  "Spurs" = "Tottenham",
  "West Ham" = "West Ham",
  "West Ham United" = "West Ham",
  "Wolves" = "Wolves",
  "Wolverhampton Wanderers" = "Wolves",
  "Wolverhampton" = "Wolves",
  "Manchester City" = "Manchester City",
  "Man City" = "Manchester City",
  
  # Serie A
  "Inter" = "Inter Milan",
  "Inter Milan" = "Inter Milan",
  "Internazionale" = "Inter Milan",
  "Hellas Verona" = "Verona",
  "Verona" = "Verona",
  "Milan" = "AC Milan",
  "AC Milan" = "AC Milan",
  "Monza" = "Monza",
  "Venezia" = "Venezia",
  "Empoli" = "Empoli",
  
  # La Liga
  "Betis" = "Real Betis",
  "Real Betis" = "Real Betis",
  "AtlÃƒÂ©tico Madrid" = "Atletico Madrid",
  "Atletico Madrid" = "Atletico Madrid",
  "AtlÃƒÂ©tico" = "Atletico Madrid",
  "Athletic Club" = "Athletic Bilbao",
  "Athletic Bilbao" = "Athletic Bilbao",
  "AlavÃƒÂ©s" = "Alaves",
  "Alaves" = "Alaves",
  "Oviedo" = "Real Oviedo",
  "Real Oviedo" = "Real Oviedo",
  "Espanyol" = "Espanyol",
  "Villarreal" = "Villarreal",
  "Rayo Vallecano" = "Rayo Vallecano",
  "Sevilla" = "Sevilla",
  "Las Palmas" = "Las Palmas",
  "LeganÃƒÂ©s" = "Leganes",
  "Leganes" = "Leganes",
  "Valladolid" = "Real Valladolid",
  "Real Valladolid" = "Real Valladolid",
  
  # Bundesliga
  "Bayern Munich" = "Bayern Munich",
  "Bayern MÃ¼nchen" = "Bayern Munich",
  "FC Bayern MÃ¼nchen" = "Bayern Munich",
  "Bayern" = "Bayern Munich",
  "Borussia Dortmund" = "Borussia Dortmund",
  "Dortmund" = "Borussia Dortmund",
  "BVB" = "Borussia Dortmund",
  "RB Leipzig" = "RB Leipzig",
  "Leipzig" = "RB Leipzig",
  "Bayer Leverkusen" = "Bayer Leverkusen",
  "Leverkusen" = "Bayer Leverkusen",
  "Bayer 04 Leverkusen" = "Bayer Leverkusen",
  "Eintracht Frankfurt" = "Eintracht Frankfurt",
  "Frankfurt" = "Eintracht Frankfurt",
  "Union Berlin" = "Union Berlin",
  "1. FC Union Berlin" = "Union Berlin",
  "SC Freiburg" = "SC Freiburg",
  "Freiburg" = "SC Freiburg",
  "Wolfsburg" = "Wolfsburg",
  "VfL Wolfsburg" = "Wolfsburg",
  "Mainz" = "Mainz",
  "Mainz 05" = "Mainz",
  "1. FSV Mainz 05" = "Mainz",
  "Borussia Monchengladbach" = "Borussia Monchengladbach",
  "Borussia MÃ¶nchengladbach" = "Borussia Monchengladbach",
  "Gladbach" = "Borussia Monchengladbach",
  "Hoffenheim" = "Hoffenheim",
  "TSG Hoffenheim" = "Hoffenheim",
  "Werder Bremen" = "Werder Bremen",
  "Bremen" = "Werder Bremen",
  "SV Werder Bremen" = "Werder Bremen",
  "Augsburg" = "Augsburg",
  "FC Augsburg" = "Augsburg",
  "Stuttgart" = "Stuttgart",
  "VfB Stuttgart" = "Stuttgart",
  "Bochum" = "Bochum",
  "VfL Bochum" = "Bochum",
  "Heidenheim" = "Heidenheim",
  "1. FC Heidenheim" = "Heidenheim",
  "Darmstadt" = "Darmstadt",
  "SV Darmstadt 98" = "Darmstadt",
  "Koln" = "Koln",
  "KÃ¶ln" = "Koln",
  "1. FC KÃ¶ln" = "Koln",
  
  # Championship
  "Leeds United" = "Leeds United",
  "Leeds" = "Leeds United",
  "Leicester City" = "Leicester City",
  "Leicester" = "Leicester City",
  "Southampton" = "Southampton",
  "West Brom" = "West Brom",
  "West Bromwich Albion" = "West Brom",
  "Norwich City" = "Norwich City",
  "Norwich" = "Norwich City",
  "Middlesbrough" = "Middlesbrough",
  "Boro" = "Middlesbrough",
  "Coventry City" = "Coventry City",
  "Coventry" = "Coventry City",
  "Bristol City" = "Bristol City",
  "Sunderland" = "Sunderland",
  "Watford" = "Watford",
  "Sheffield United" = "Sheffield United",
  "Sheff Utd" = "Sheffield United",
  "Burnley" = "Burnley",
  "Luton Town" = "Luton Town",
  "Luton" = "Luton Town",
  "Stoke City" = "Stoke City",
  "Stoke" = "Stoke City",
  "Hull City" = "Hull City",
  "Hull" = "Hull City",
  "Queens Park Rangers" = "Queens Park Rangers",
  "QPR" = "Queens Park Rangers",
  "Blackburn Rovers" = "Blackburn Rovers",
  "Blackburn" = "Blackburn Rovers",
  "Preston North End" = "Preston North End",
  "Preston" = "Preston North End",
  "Swansea City" = "Swansea City",
  "Swansea" = "Swansea City",
  "Cardiff City" = "Cardiff City",
  "Cardiff" = "Cardiff City",
  "Plymouth Argyle" = "Plymouth Argyle",
  "Plymouth" = "Plymouth Argyle",
  "Millwall" = "Millwall",
  "Sheffield Wednesday" = "Sheffield Wednesday",
  "Sheff Wed" = "Sheffield Wednesday",
  "Birmingham City" = "Birmingham City",
  "Birmingham" = "Birmingham City"
)

# =============================================================================
# TEAM ABBREVIATIONS
# =============================================================================

TEAM_ABBREVIATIONS <- c(
  # Premier League
  "Arsenal" = "ARS",
  "Aston Villa" = "AVL",
  "Bournemouth" = "BOU",
  "Brentford" = "BRE",
  "Brighton" = "BHA",
  "Burnley" = "BUR",
  "Chelsea" = "CHE",
  "Crystal Palace" = "CRY",
  "Everton" = "EVE",
  "Fulham" = "FUL",
  "Ipswich Town" = "IPS",
  "Leeds United" = "LEE",
  "Leicester City" = "LEI",
  "Liverpool" = "LIV",
  "Manchester City" = "MCI",
  "Manchester United" = "MUN",
  "Newcastle United" = "NEW",
  "Nottingham Forest" = "NFO",
  "Southampton" = "SOU",
  "Tottenham" = "TOT",
  "West Ham" = "WHU",
  "Wolves" = "WOL",
  
  # Serie A
  "AC Milan" = "MIL",
  "Atalanta" = "ATA",
  "Bologna" = "BOL",
  "Cagliari" = "CAG",
  "Como" = "COM",
  "Empoli" = "EMP",
  "Fiorentina" = "FIO",
  "Genoa" = "GEN",
  "Verona" = "VER",
  "Inter Milan" = "INT",
  "Juventus" = "JUV",
  "Lazio" = "LAZ",
  "Lecce" = "LEC",
  "Monza" = "MON",
  "Napoli" = "NAP",
  "Parma" = "PAR",
  "Roma" = "ROM",
  "Torino" = "TOR",
  "Udinese" = "UDI",
  "Venezia" = "VEN",
  
  # La Liga
  "Alaves" = "ALA",
  "Athletic Bilbao" = "ATH",
  "Atletico Madrid" = "ATM",
  "Barcelona" = "BAR",
  "Celta Vigo" = "CEL",
  "Espanyol" = "ESP",
  "Getafe" = "GET",
  "Girona" = "GIR",
  "Las Palmas" = "LPA",
  "Leganes" = "LEG",
  "Mallorca" = "MLL",
  "Osasuna" = "OSA",
  "Rayo Vallecano" = "RAY",
  "Real Betis" = "BET",
  "Real Madrid" = "RMA",
  "Real Oviedo" = "OVI",
  "Real Sociedad" = "RSO",
  "Real Valladolid" = "VLL",
  "Sevilla" = "SEV",
  "Valencia" = "VAL",
  "Villarreal" = "VIL",
  
  # Bundesliga
  "Bayern Munich" = "BAY",
  "Borussia Dortmund" = "BVB",
  "RB Leipzig" = "RBL",
  "Bayer Leverkusen" = "LEV",
  "Eintracht Frankfurt" = "SGE",
  "Union Berlin" = "UNB",
  "SC Freiburg" = "FRE",
  "Wolfsburg" = "WOB",
  "Mainz" = "M05",
  "Borussia Monchengladbach" = "BMG",
  "Hoffenheim" = "HOF",
  "Werder Bremen" = "SVW",
  "Augsburg" = "AUG",
  "Stuttgart" = "VFB",
  "Bochum" = "BOC",
  "Heidenheim" = "HDH",
  "Darmstadt" = "DAR",
  "Koln" = "KOE",
  
  # Championship
  "Leeds United" = "LEE",
  "Leicester City" = "LEI",
  "Southampton" = "SOU",
  "West Brom" = "WBA",
  "Norwich City" = "NOR",
  "Middlesbrough" = "MID",
  "Coventry City" = "COV",
  "Bristol City" = "BRC",
  "Sunderland" = "SUN",
  "Watford" = "WAT",
  "Sheffield United" = "SHU",
  "Burnley" = "BUR",
  "Luton Town" = "LUT",
  "Stoke City" = "STK",
  "Hull City" = "HUL",
  "Queens Park Rangers" = "QPR",
  "Blackburn Rovers" = "BLK",
  "Preston North End" = "PNE",
  "Swansea City" = "SWA",
  "Cardiff City" = "CAR",
  "Plymouth Argyle" = "PLY",
  "Millwall" = "MIL",
  "Sheffield Wednesday" = "SHW",
  "Birmingham City" = "BIR"
)

# =============================================================================
# TEAM LOGO PATHS
# =============================================================================

# Map canonical team names to logo file paths (relative to www/)
# Only include teams with actual SVG files
TEAM_LOGO_PATHS <- c(
  # Premier League
  "Arsenal" = "soccer_logos/Premier League/arsenal.svg",
  "Aston Villa" = "soccer_logos/Premier League/aston_villa.svg",
  "Bournemouth" = "soccer_logos/Premier League/bournemouth.svg",
  "Brentford" = "soccer_logos/Premier League/brentford.svg",
  "Brighton" = "soccer_logos/Premier League/brighton.svg",
  "Burnley" = "soccer_logos/Premier League/burnley.svg",
  "Chelsea" = "soccer_logos/Premier League/chelsea.svg",
  "Crystal Palace" = "soccer_logos/Premier League/crystal_palace.svg",
  "Everton" = "soccer_logos/Premier League/everton.svg",
  "Fulham" = "soccer_logos/Premier League/fulham.svg",
  "Ipswich Town" = "soccer_logos/Premier League/ipswich_town.svg",
  "Leeds United" = "soccer_logos/Premier League/leeds.svg",
  "Leicester City" = "soccer_logos/Premier League/leicester_city.svg",
  "Liverpool" = "soccer_logos/Premier League/liverpool.svg",
  "Manchester City" = "soccer_logos/Premier League/man_city.svg",
  "Manchester United" = "soccer_logos/Premier League/man_united.svg",
  "Newcastle United" = "soccer_logos/Premier League/newcastle_united.svg",
  "Nottingham Forest" = "soccer_logos/Premier League/nottingham_forest.svg",
  "Southampton" = "soccer_logos/Premier League/southampton.svg",
  "Sunderland" = "soccer_logos/Premier League/sunderland.svg",
  "Tottenham" = "soccer_logos/Premier League/spurs.svg",
  "West Ham" = "soccer_logos/Premier League/west_ham.svg",
  "Wolves" = "soccer_logos/Premier League/wolves.svg",
  
  # Serie A
  "AC Milan" = "soccer_logos/Serie A/ac_milan.svg",
  "Atalanta" = "soccer_logos/Serie A/atalanta.svg",
  "Bologna" = "soccer_logos/Serie A/bologna.svg",
  "Cagliari" = "soccer_logos/Serie A/cagliari.svg",
  "Como" = "soccer_logos/Serie A/como.svg",
  "Cremonese" = "soccer_logos/Serie A/cremonese.svg",
  "Empoli" = "soccer_logos/Serie A/empoli.svg",
  "Fiorentina" = "soccer_logos/Serie A/fiorentina.svg",
  "Genoa" = "soccer_logos/Serie A/genoa.svg",
  "Inter Milan" = "soccer_logos/Serie A/inter_milan.svg",
  "Juventus" = "soccer_logos/Serie A/juventus.svg",
  "Lazio" = "soccer_logos/Serie A/lazio.svg",
  "Lecce" = "soccer_logos/Serie A/lecce.svg",
  "Monza" = "soccer_logos/Serie A/monza.svg",
  "Napoli" = "soccer_logos/Serie A/napoli.svg",
  "Parma" = "soccer_logos/Serie A/parma.svg",
  "Pisa" = "soccer_logos/Serie A/pisa.svg",
  "Roma" = "soccer_logos/Serie A/roma.svg",
  "Sassuolo" = "soccer_logos/Serie A/sassuolo.svg",
  "Torino" = "soccer_logos/Serie A/torino.svg",
  "Udinese" = "soccer_logos/Serie A/udinese.svg",
  "Venezia" = "soccer_logos/Serie A/venezia.svg",
  "Verona" = "soccer_logos/Serie A/hellas_verona.svg",
  
  # La Liga
  "Alaves" = "soccer_logos/La Liga/alaves.svg",
  "Athletic Bilbao" = "soccer_logos/La Liga/athletic_bilbao.svg",
  "Atletico Madrid" = "soccer_logos/La Liga/atletico_madrid.svg",
  "Barcelona" = "soccer_logos/La Liga/barcelona.svg",
  "Celta Vigo" = "soccer_logos/La Liga/celta_vigo.svg",
  "Elche" = "soccer_logos/La Liga/elche.svg",
  "Espanyol" = "soccer_logos/La Liga/espanyol.svg",
  "Getafe" = "soccer_logos/La Liga/getafe.svg",
  "Girona" = "soccer_logos/La Liga/girona.svg",
  "Las Palmas" = "soccer_logos/La Liga/las_palmas.svg",
  "Leganes" = "soccer_logos/La Liga/leganes.svg",
  "Levante" = "soccer_logos/La Liga/levante.svg",
  "Mallorca" = "soccer_logos/La Liga/mallorca.svg",
  "Osasuna" = "soccer_logos/La Liga/osasuna.svg",
  "Rayo Vallecano" = "soccer_logos/La Liga/rayo_vallecano.svg",
  "Real Betis" = "soccer_logos/La Liga/real_betis.svg",
  "Real Madrid" = "soccer_logos/La Liga/real_madrid.svg",
  "Real Oviedo" = "soccer_logos/La Liga/real_oviedo.svg",
  "Real Sociedad" = "soccer_logos/La Liga/real_sociedad.svg",
  "Real Valladolid" = "soccer_logos/La Liga/real_valladolid.svg",
  "Sevilla" = "soccer_logos/La Liga/sevilla.svg",
  "Valencia" = "soccer_logos/La Liga/valencia.svg",
  "Villarreal" = "soccer_logos/La Liga/villarreal.svg",
  
  # Bundesliga
  "Bayern Munich" = "soccer_logos/Bundesliga/bayern_munich.svg",
  "Borussia Dortmund" = "soccer_logos/Bundesliga/borussia_dortmund.svg",
  "RB Leipzig" = "soccer_logos/Bundesliga/rb_leipzig.svg",
  "Bayer Leverkusen" = "soccer_logos/Bundesliga/bayer_leverkusen.svg",
  "Eintracht Frankfurt" = "soccer_logos/Bundesliga/eintracht_frankfurt.svg",
  "Union Berlin" = "soccer_logos/Bundesliga/union_berlin.svg",
  "SC Freiburg" = "soccer_logos/Bundesliga/sc_freiburg.svg",
  "Wolfsburg" = "soccer_logos/Bundesliga/wolfsburg.svg",
  "Mainz" = "soccer_logos/Bundesliga/mainz.svg",
  "Borussia Monchengladbach" = "soccer_logos/Bundesliga/borussia_monchengladbach.svg",
  "Hoffenheim" = "soccer_logos/Bundesliga/hoffenheim.svg",
  "Werder Bremen" = "soccer_logos/Bundesliga/werder_bremen.svg",
  "Augsburg" = "soccer_logos/Bundesliga/augsburg.svg",
  "Stuttgart" = "soccer_logos/Bundesliga/stuttgart.svg",
  "Bochum" = "soccer_logos/Bundesliga/bochum.svg",
  "Heidenheim" = "soccer_logos/Bundesliga/heidenheim.svg",
  "Darmstadt" = "soccer_logos/Bundesliga/darmstadt.svg",
  "Koln" = "soccer_logos/Bundesliga/koln.svg",
  
  # Championship
  "Leeds United" = "soccer_logos/Championship/leeds_united.svg",
  "Leicester City" = "soccer_logos/Championship/leicester_city.svg",
  "Southampton" = "soccer_logos/Championship/southampton.svg",
  "West Brom" = "soccer_logos/Championship/west_brom.svg",
  "Norwich City" = "soccer_logos/Championship/norwich_city.svg",
  "Middlesbrough" = "soccer_logos/Championship/middlesbrough.svg",
  "Coventry City" = "soccer_logos/Championship/coventry_city.svg",
  "Bristol City" = "soccer_logos/Championship/bristol_city.svg",
  "Sunderland" = "soccer_logos/Championship/sunderland.svg",
  "Watford" = "soccer_logos/Championship/watford.svg",
  "Sheffield United" = "soccer_logos/Championship/sheffield_united.svg",
  "Burnley" = "soccer_logos/Championship/burnley.svg",
  "Luton Town" = "soccer_logos/Championship/luton_town.svg",
  "Stoke City" = "soccer_logos/Championship/stoke_city.svg",
  "Hull City" = "soccer_logos/Championship/hull_city.svg",
  "Queens Park Rangers" = "soccer_logos/Championship/qpr.svg",
  "Blackburn Rovers" = "soccer_logos/Championship/blackburn_rovers.svg",
  "Preston North End" = "soccer_logos/Championship/preston_north_end.svg",
  "Swansea City" = "soccer_logos/Championship/swansea_city.svg",
  "Cardiff City" = "soccer_logos/Championship/cardiff_city.svg",
  "Plymouth Argyle" = "soccer_logos/Championship/plymouth_argyle.svg",
  "Millwall" = "soccer_logos/Championship/millwall.svg",
  "Sheffield Wednesday" = "soccer_logos/Championship/sheffield_wednesday.svg",
  "Birmingham City" = "soccer_logos/Championship/birmingham_city.svg"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get abbreviation for a soccer team
#' @param team_name Canonical team name (can be a vector)
#' @return 3-letter abbreviation or first 3 letters if not found
get_team_abbreviation <- function(team_name) {
  # Handle NULL or empty
  if (is.null(team_name) || length(team_name) == 0) return("???")
  
  # Handle vector input - vectorize the function
  if (length(team_name) > 1) {
    return(sapply(team_name, get_team_abbreviation, USE.NAMES = FALSE))
  }
  
  # Handle NA or empty string (single value)
  if (is.na(team_name) || team_name == "") return("???")
  
  # Look up in abbreviations
  if (team_name %in% names(TEAM_ABBREVIATIONS)) {
    return(unname(TEAM_ABBREVIATIONS[team_name]))
  }
  
  # Fallback: first 3 letters uppercase
  return(toupper(substr(team_name, 1, 3)))
}

#' Normalize team names to canonical form
#' @param team_names Vector of team names
#' @return Vector of normalized team names
normalize_team_names <- function(team_names) {
  sapply(team_names, function(name) {
    if (is.na(name) || name == "") return(name)
    
    # Check if we have a mapping
    if (name %in% names(TEAM_NAME_MAPPING)) {
      return(TEAM_NAME_MAPPING[name])
    }
    
    # Return original if no mapping
    return(name)
  }, USE.NAMES = FALSE)
}

#' Apply team name normalization to a data frame
#' @param df Data frame with team columns
#' @param team_cols Vector of column names to normalize
#' @return Data frame with normalized team names
normalize_team_columns <- function(df, team_cols) {
  for (col in team_cols) {
    if (col %in% names(df)) {
      df[[col]] <- normalize_team_names(df[[col]])
    }
  }
  return(df)
}

#' Get logo path for a soccer team
#' @param team_name Team name (will be normalized)
#' @return Logo path or NULL if not found
get_soccer_team_logo <- function(team_name) {
  if (is.null(team_name) || is.na(team_name) || team_name == "") return(NULL)
  
  # Normalize the team name first
  normalized <- normalize_team_names(team_name)
  
  if (normalized %in% names(TEAM_LOGO_PATHS)) {
    return(TEAM_LOGO_PATHS[normalized])
  }
  
  return(NULL)
}