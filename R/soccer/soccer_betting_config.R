# =============================================================================
# Soccer Betting Configuration
# 
# League definitions, team mappings, API configuration
# Dependencies: None (standalone config file)
# =============================================================================

# =============================================================================
# API CONFIGURATION
# =============================================================================

# The Odds API key
BETTING_API_KEY <- "fb80ddefba388dcb27e65ba5046a027e"
BETTING_API_CALL_DELAY <- 0.5

# Cache settings
BETTING_CACHE_FILE <- "data/cache/betting_cache.rds"
BETTING_CACHE_MAX_AGE_HOURS <- 12

# =============================================================================
# LEAGUE DEFINITIONS
# =============================================================================

BETTING_LEAGUES <- list(
  # England
  "Premier League" = list(
    slug = "soccer_epl",
    bbc_url = "https://www.bbc.co.uk/sport/football/premier-league/table"
  ),
  "Championship" = list(
    slug = "soccer_efl_champ",
    bbc_url = "https://www.bbc.co.uk/sport/football/championship/table"
  ),
  "League One" = list(
    slug = "soccer_england_league1",
    bbc_url = "https://www.bbc.co.uk/sport/football/league-one/table"
  ),
  "League Two" = list(
    slug = "soccer_england_league2",
    bbc_url = "https://www.bbc.co.uk/sport/football/league-two/table"
  ),
  
  # Scotland
  "Scottish Premiership" = list(
    slug = "soccer_spl",
    bbc_url = "https://www.bbc.co.uk/sport/football/scottish-premiership/table"
  ),
  
  # Top 5 Leagues
  "La Liga" = list(
    slug = "soccer_spain_la_liga",
    bbc_url = "https://www.bbc.co.uk/sport/football/spanish-la-liga/table"
  ),
  "Serie A" = list(
    slug = "soccer_italy_serie_a",
    bbc_url = "https://www.bbc.co.uk/sport/football/italian-serie-a/table"
  ),
  "Bundesliga" = list(
    slug = "soccer_germany_bundesliga",
    bbc_url = "https://www.bbc.co.uk/sport/football/german-bundesliga/table"
  ),
  "Ligue 1" = list(
    slug = "soccer_france_ligue_one",
    bbc_url = "https://www.bbc.co.uk/sport/football/french-ligue-one/table"
  ),
  
  # Other European Leagues
  "Eredivisie" = list(
    slug = "soccer_netherlands_eredivisie",
    bbc_url = "https://www.bbc.co.uk/sport/football/dutch-eredivisie/table"
  ),
  "Belgian Pro League" = list(
    slug = "soccer_belgium_first_div",
    bbc_url = "https://www.bbc.co.uk/sport/football/belgian-pro-league/table"
  ),
  "Primeira Liga" = list(
    slug = "soccer_portugal_primeira_liga",
    bbc_url = "https://www.bbc.co.uk/sport/football/portuguese-primeira-liga/table"
  ),
  "Austrian Bundesliga" = list(
    slug = "soccer_austria_bundesliga",
    bbc_url = "https://www.bbc.co.uk/sport/football/austrian-bundesliga/table"
  ),
  "Swiss Super League" = list(
    slug = "soccer_switzerland_superleague",
    bbc_url = "https://www.bbc.co.uk/sport/football/swiss-super-league/table"
  )
)

# Default leagues to show
BETTING_DEFAULT_LEAGUES <- c("Premier League")

# =============================================================================
# ADDITIONAL TEAM NAME MAPPINGS FOR BBC AND ODDS API
# 
# These are added to the main TEAM_NAME_MAPPING in soccer_config.R
# The prepare_betting_table_with_logos() function uses normalize_team_names()
# which references TEAM_NAME_MAPPING
# =============================================================================

# Additional mappings to add to TEAM_NAME_MAPPING in soccer_config.R
# These handle BBC and Odds API specific name variants
# Maps to canonical names that exist in TEAM_LOGO_PATHS
BETTING_EXTRA_MAPPINGS <- c(
  # Premier League - BBC variants
  "Nott'm Forest" = "Nottingham Forest",
  "Newcastle" = "Newcastle United",
  
  # Premier League - Odds API variants (map to TEAM_LOGO_PATHS canonical names)
  "Tottenham Hotspur" = "Tottenham",
  "West Ham United" = "West Ham",
  "Wolverhampton Wanderers" = "Wolves",
  "Brighton and Hove Albion" = "Brighton",
  "AFC Bournemouth" = "Bournemouth",
  "Ipswich" = "Ipswich Town",
  "Leicester" = "Leicester City",
  
  # Championship - BBC variants
  "Sheffield Utd" = "Sheffield United",
  "Sheffield Wed" = "Sheffield Wednesday",
  "Coventry" = "Coventry City",
  "Norwich" = "Norwich City",
  "Swansea" = "Swansea City",
  "Cardiff" = "Cardiff City",
  "Stoke" = "Stoke City",
  "Hull" = "Hull City",
  "Blackburn" = "Blackburn Rovers",
  "Preston" = "Preston North End",
  "Plymouth" = "Plymouth Argyle",
  "Leeds" = "Leeds United",
  "Luton" = "Luton Town",
  "Oxford" = "Oxford United",
  "Bristol" = "Bristol City",
  
  # Championship - Odds API variants  
  "West Bromwich Albion" = "West Brom",
  "Middlesbrough FC" = "Middlesbrough",
  "Wrexham AFC" = "Wrexham",
  "Sheffield Wednesday FC" = "Sheffield Wednesday",
  "Bristol City FC" = "Bristol City",
  "Sunderland AFC" = "Sunderland",
  "QPR" = "Queens Park Rangers",
  
  # La Liga - BBC variants
  "Ath Bilbao" = "Athletic Bilbao",
  "Ath Madrid" = "Atletico Madrid",
  "Celta" = "Celta Vigo",
  "Sociedad" = "Real Sociedad",
  "Betis" = "Real Betis",
  "Valladolid" = "Real Valladolid",
  
  # La Liga - Odds API variants
  "Celta de Vigo" = "Celta Vigo",
  "CA Osasuna" = "Osasuna",
  "Athletic Club" = "Athletic Bilbao",
  "CD Leganes" = "Leganes",
  "UD Las Palmas" = "Las Palmas",
  "Deportivo Alaves" = "Alaves",
  "RCD Mallorca" = "Mallorca",
  "Getafe CF" = "Getafe",
  "Real Madrid CF" = "Real Madrid",
  "FC Barcelona" = "Barcelona",
  "Valencia CF" = "Valencia",
  "Villarreal CF" = "Villarreal",
  "Sevilla FC" = "Sevilla",
  "Girona FC" = "Girona",
  "Real Sociedad de Futbol" = "Real Sociedad",
  "Atletico Madrid" = "Atletico Madrid",
  
  # Serie A - BBC variants
  "Parma Calcio 1913" = "Parma",
  
  # Serie A - Odds API variants
  "Inter" = "Inter Milan",
  "Internazionale" = "Inter Milan",
  "FC Internazionale Milano" = "Inter Milan",
  "Milan" = "AC Milan",
  "AS Roma" = "Roma",
  "Atalanta BC" = "Atalanta",
  "SS Lazio" = "Lazio",
  "SSC Napoli" = "Napoli",
  "Juventus FC" = "Juventus",
  "ACF Fiorentina" = "Fiorentina",
  "Torino FC" = "Torino",
  "Genoa CFC" = "Genoa",
  "Bologna FC 1909" = "Bologna",
  "Cagliari Calcio" = "Cagliari",
  "Udinese Calcio" = "Udinese",
  "US Lecce" = "Lecce",
  "Hellas Verona" = "Verona",
  "Hellas Verona FC" = "Verona",
  "Empoli FC" = "Empoli",
  "AC Monza" = "Monza",
  "Venezia FC" = "Venezia",
  "Como 1907" = "Como",
  "Parma Calcio" = "Parma",
  
  # Bundesliga - BBC variants
  "Bor M'gladbach" = "Borussia Monchengladbach",
  "B Dortmund" = "Borussia Dortmund",
  "B Leverkusen" = "Bayer Leverkusen",
  "E Frankfurt" = "Eintracht Frankfurt",
  
  # Bundesliga - Odds API variants
  "RasenBallsport Leipzig" = "RB Leipzig",
  "Bayern Munchen" = "Bayern Munich",
  "FC Bayern Munchen" = "Bayern Munich",
  "FC Bayern Munich" = "Bayern Munich",
  "FSV Mainz 05" = "Mainz",
  "1. FSV Mainz 05" = "Mainz",
  "FC Heidenheim" = "Heidenheim",
  "1. FC Heidenheim 1846" = "Heidenheim",
  "VfB Stuttgart" = "Stuttgart",
  "VfL Wolfsburg" = "Wolfsburg",
  "TSG Hoffenheim" = "Hoffenheim",
  "TSG 1899 Hoffenheim" = "Hoffenheim",
  "FC Koln" = "Koln",
  "1. FC Koln" = "Koln",
  "1. FC KÃ¶ln" = "Koln",
  "FC KÃ¶ln" = "Koln",
  "1. FC Köln" = "Koln",
  "FC Köln" = "Koln",
  "FC St. Pauli" = "FC St Pauli",
  "FC St Pauli 1910" = "FC St Pauli",
  "SV Werder Bremen" = "Werder Bremen",
  "FC Augsburg" = "Augsburg",
  "VfL Bochum" = "Bochum",
  "VfL Bochum 1848" = "Bochum",
  "1. FC Union Berlin" = "Union Berlin",
  "Borussia Monchengladbach" = "Borussia Monchengladbach",
  "Borussia Mönchengladbach" = "Borussia Monchengladbach",
  # Note: "SC Freiburg" is already canonical in TEAM_LOGO_PATHS
  
  # Ligue 1
  "Paris Saint-Germain" = "PSG",
  "Paris Saint Germain" = "PSG",
  "Paris SG" = "PSG",
  "AS Monaco" = "Monaco",
  "AS Monaco FC" = "Monaco",
  "OGC Nice" = "Nice",
  "Olympique Lyonnais" = "Lyon",
  "Olympique Lyon" = "Lyon",
  "Olympique de Marseille" = "Marseille",
  "Olympique Marseille" = "Marseille",
  "RC Lens" = "Lens",
  "Stade Rennais FC" = "Rennes",
  "Stade Rennais" = "Rennes",
  "LOSC Lille" = "Lille",
  "Lille OSC" = "Lille",
  "Stade Brestois 29" = "Brest",
  "Stade de Reims" = "Reims",
  "FC Nantes" = "Nantes",
  "Toulouse FC" = "Toulouse",
  "RC Strasbourg Alsace" = "Strasbourg",
  "AJ Auxerre" = "Auxerre",
  "Montpellier HSC" = "Montpellier",
  "Le Havre AC" = "Le Havre",
  "Angers SCO" = "Angers",
  "AS Saint-Etienne" = "Saint-Etienne",
  
  # Scottish Premiership
  "St Mirren" = "St. Mirren",
  "St Johnstone" = "St. Johnstone",
  "Heart of Midlothian" = "Hearts",
  "Dundee FC" = "Dundee",
  "Ross County FC" = "Ross County",
  "Kilmarnock FC" = "Kilmarnock",
  "Hibernian FC" = "Hibernian",
  "St Johnstone FC" = "St. Johnstone",
  "Motherwell FC" = "Motherwell",
  "Aberdeen FC" = "Aberdeen",
  "Celtic FC" = "Celtic",
  "Rangers FC" = "Rangers",
  "Falkirk F.C." = "Falkirk",
  "Falkirk FC" = "Falkirk",
  
  # Belgian Pro League
  "Cercle Brugge KSV" = "Cercle Brugge",
  "Club Brugge KV" = "Club Brugge",
  "KRC Genk" = "Genk",
  "RSC Anderlecht" = "Anderlecht",
  "R. Antwerp FC" = "Antwerp",
  "Royal Antwerp" = "Antwerp",
  "KAA Gent" = "Gent",
  "KV Mechelen" = "Mechelen",
  "Sint-Truidense VV" = "Sint-Truiden",
  "OH Leuven" = "Leuven",
  "Sporting Charleroi" = "Charleroi",
  "Standard Liege" = "Standard Liege",
  "Standard de Liege" = "Standard Liege",
  "Union Saint-Gilloise" = "Union SG",
  "Royale Union Saint-Gilloise" = "Union SG",
  "FCV Dender EH" = "Dender",
  "STVV" = "Sint-Truiden",
  "Beerschot VA" = "Beerschot",
  "Beerschot V.A." = "Beerschot",
  
  # Primeira Liga (Portugal)
  "Rio Ave FC" = "Rio Ave",
  "CF Estrela" = "Estrela",
  "CF Estrela da Amadora" = "Estrela",
  "Estrela da Amadora" = "Estrela",
  "Estrela Amadora" = "Estrela",
  "SL Benfica" = "Benfica",
  "Sporting CP" = "Sporting",
  "Sporting Lisbon" = "Sporting",
  "FC Porto" = "Porto",
  "SC Braga" = "Braga",
  "Vitoria SC" = "Vitoria",
  "Vitoria de Guimaraes" = "Vitoria",
  "Gil Vicente FC" = "Gil Vicente",
  "CD Santa Clara" = "Santa Clara",
  "Moreirense FC" = "Moreirense",
  "Boavista FC" = "Boavista",
  "Casa Pia AC" = "Casa Pia",
  "GD Estoril Praia" = "Estoril",
  "Estoril Praia" = "Estoril",
  "FC Arouca" = "Arouca",
  "FC Famalicao" = "Famalicao",
  "Famalicão" = "Famalicao",
  "AVS Futebol SAD" = "AVS",
  "SC Farense" = "Farense",
  "CD Nacional" = "Nacional",
  "CD Tondela" = "Tondela",
  "FC Alverca" = "Alverca",
  
  # Swiss Super League
  "FC Basel" = "Basel",
  "FC Basel 1893" = "Basel",
  "FC Zurich" = "Zurich",
  "FC Zürich" = "Zurich",
  "FC St Gallen" = "St Gallen",
  "FC St. Gallen" = "St Gallen",
  "FC St. Gallen 1879" = "St Gallen",
  "FC Lugano" = "Lugano",
  "FC Luzern" = "Luzern",
  "FC Lausanne-Sport" = "Lausanne",
  "Lausanne-Sport" = "Lausanne",
  "BSC Young Boys" = "Young Boys",
  "Yverdon Sport FC" = "Yverdon",
  "Yverdon-Sport" = "Yverdon",
  "FC Sion" = "Sion",
  "FC Winterthur" = "Winterthur",
  "Grasshopper Club Zurich" = "Grasshoppers",
  "Grasshoppers" = "Grasshoppers",
  
  # Eredivisie (Netherlands)
  "FC Utrecht" = "Utrecht",
  "Heracles Almelo" = "Heracles",
  "Sparta Rotterdam" = "Sparta",
  "FC Twente" = "Twente",
  "FC Twente Enschede" = "Twente",
  "FC Groningen" = "Groningen",
  "SC Heerenveen" = "Heerenveen",
  "Fortuna Sittard" = "Fortuna Sittard",
  "RKC Waalwijk" = "Waalwijk",
  "NEC Nijmegen" = "NEC",
  "NEC" = "NEC",
  "Go Ahead Eagles" = "Go Ahead Eagles",
  "PEC Zwolle" = "PEC Zwolle",
  "NAC Breda" = "NAC Breda",
  "Willem II" = "Willem II",
  "Almere City FC" = "Almere City",
  "AZ Alkmaar" = "AZ",
  "AZ" = "AZ",
  "PSV Eindhoven" = "PSV",
  "PSV" = "PSV",
  "Ajax Amsterdam" = "Ajax",
  "Ajax" = "Ajax",
  "Feyenoord Rotterdam" = "Feyenoord",
  
  # La Liga 2 / Segunda (if appearing in odds)
  "Real Oviedo" = "Real Oviedo",
  "Real Sporting de Gijon" = "Sporting Gijon",
  "Sporting Gijon" = "Sporting Gijon",
  "Racing Santander" = "Racing Santander",
  "Real Zaragoza" = "Zaragoza",
  "Cadiz CF" = "Cadiz",
  "Elche CF" = "Elche",
  "Granada CF" = "Granada"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Apply extra betting mappings to team name
#' This extends the base normalize_team_names() with betting-specific variants
normalize_betting_team_name <- function(team_name) {
  if (is.null(team_name) || is.na(team_name) || team_name == "") return(team_name)
  
  # First check BETTING_EXTRA_MAPPINGS
  if (team_name %in% names(BETTING_EXTRA_MAPPINGS)) {
    team_name <- unname(BETTING_EXTRA_MAPPINGS[team_name])
  }
  
  # Then apply base normalization from soccer_config.R
  normalize_team_names(team_name)
}

#' Vectorized version
normalize_betting_team_names <- function(team_names) {
  sapply(team_names, normalize_betting_team_name, USE.NAMES = FALSE)
}

#' Apply team name mappings to standings dataframe
#' @param standings Standings tibble with 'team' column
#' @return Standings with team names normalized
apply_betting_team_mappings <- function(standings) {
  if (is.null(standings) || nrow(standings) == 0) return(standings)
  
  standings %>%
    mutate(
      team = normalize_betting_team_names(team),
      team_n = tolower(trimws(team))
    )
}

#' Get list of available betting leagues
get_betting_leagues <- function() {
  names(BETTING_LEAGUES)
}