# =============================================================================
# Soccer Betting Configuration
# 
# League definitions, team mappings, API configuration
# Dependencies: soccer_config.R (for normalize_team_names, get_soccer_team_logo)
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
# LEAGUE DEFINITIONS (17 leagues total)
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
  "Scottish Championship" = list(
    slug = "soccer_scotland_championship",
    bbc_url = "https://www.bbc.co.uk/sport/football/scottish-championship/table"
  ),
  "Scottish League One" = list(
    slug = "soccer_scotland_league_one",
    bbc_url = "https://www.bbc.co.uk/sport/football/scottish-league-one/table"
  ),
  "Scottish League Two" = list(
    slug = "soccer_scotland_league_two",
    bbc_url = "https://www.bbc.co.uk/sport/football/scottish-league-two/table"
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

# Default selected leagues
BETTING_DEFAULT_LEAGUES <- c("Premier League")

# =============================================================================
# BETTING EXTRA MAPPINGS
# Additional mappings to handle BBC and Odds API specific name variants
# Maps to canonical names that exist in TEAM_LOGO_PATHS (soccer_config.R)
# =============================================================================

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
  
  # Championship - Odds API variants
  "West Bromwich Albion" = "West Brom",
  "Queens Park Rangers" = "Queens Park Rangers",
  "Middlesbrough FC" = "Middlesbrough",
  "Sunderland AFC" = "Sunderland",
  "Burnley FC" = "Burnley",
  "Watford FC" = "Watford",
  "Millwall FC" = "Millwall",
  
  # League One / League Two - common BBC variants
  "Wrexham" = "Wrexham",
  "Birmingham" = "Birmingham City",
  "Bolton" = "Bolton Wanderers",
  "Stockport" = "Stockport County",
  "Wigan" = "Wigan Athletic",
  "Charlton" = "Charlton Athletic",
  "Reading" = "Reading FC",
  "Peterborough" = "Peterborough United",
  "Bristol Rovers" = "Bristol Rovers",
  "Cambridge" = "Cambridge United",
  "Exeter" = "Exeter City",
  "Northampton" = "Northampton Town",
  "Mansfield" = "Mansfield Town",
  "Rotherham" = "Rotherham United",
  "Barnsley" = "Barnsley FC",
  "Huddersfield" = "Huddersfield Town",
  "Leyton Orient" = "Leyton Orient",
  "Wycombe" = "Wycombe Wanderers",
  "Burton" = "Burton Albion",
  "Shrewsbury" = "Shrewsbury Town",
  "Lincoln" = "Lincoln City",
  "Stevenage" = "Stevenage FC",
  "Crawley" = "Crawley Town",
  "Port Vale" = "Port Vale",
  "Fleetwood" = "Fleetwood Town",
  
  # League One / League Two - Odds API variants (often append FC/Town)
  "Barnsley FC" = "Barnsley FC",
  "Bromley FC" = "Bromley FC",
  "Bromley" = "Bromley FC",
  "Chesterfield FC" = "Chesterfield FC",
  "Chesterfield" = "Chesterfield FC",
  "Harrogate Town" = "Harrogate Town",
  "Harrogate" = "Harrogate Town",
  "Notts County" = "Notts County",
  "AFC Wimbledon" = "AFC Wimbledon",
  "Wimbledon" = "AFC Wimbledon",
  "Cheltenham Town" = "Cheltenham Town",
  "Cheltenham" = "Cheltenham Town",
  "Colchester United" = "Colchester United",
  "Colchester" = "Colchester United",
  "Crewe Alexandra" = "Crewe Alexandra",
  "Crewe" = "Crewe Alexandra",
  "Doncaster Rovers" = "Doncaster Rovers",
  "Doncaster" = "Doncaster Rovers",
  "Gillingham FC" = "Gillingham FC",
  "Gillingham" = "Gillingham FC",
  "Grimsby Town" = "Grimsby Town",
  "Grimsby" = "Grimsby Town",
  "MK Dons" = "Milton Keynes Dons",
  "Milton Keynes Dons" = "Milton Keynes Dons",
  "Morecambe FC" = "Morecambe FC",
  "Morecambe" = "Morecambe FC",
  "Newport County" = "Newport County",
  "Newport" = "Newport County",
  "Salford City" = "Salford City",
  "Salford" = "Salford City",
  "Swindon Town" = "Swindon Town",
  "Swindon" = "Swindon Town",
  "Tranmere Rovers" = "Tranmere Rovers",
  "Tranmere" = "Tranmere Rovers",
  "Walsall FC" = "Walsall FC",
  "Walsall" = "Walsall FC",
  "Accrington Stanley" = "Accrington Stanley",
  "Accrington" = "Accrington Stanley",
  "Barrow AFC" = "Barrow AFC",
  "Barrow" = "Barrow AFC",
  "Bradford City" = "Bradford City",
  "Bradford" = "Bradford City",
  "Carlisle United" = "Carlisle United",
  "Carlisle" = "Carlisle United",
  
  # La Liga - BBC/Odds API variants
  "Athletic Club" = "Athletic Bilbao",
  "AtlÃ©tico Madrid" = "Atletico Madrid",
  "Atletico de Madrid" = "Atletico Madrid",
  "Celta de Vigo" = "Celta Vigo",
  "CA Osasuna" = "Osasuna",
  "Deportivo Alaves" = "Alaves",
  "Deportivo AlavÃ©s" = "Alaves",
  "UD Las Palmas" = "Las Palmas",
  "CD Leganes" = "Leganes",
  "CD LeganÃ©s" = "Leganes",
  "Rayo" = "Rayo Vallecano",
  "Real Valladolid CF" = "Real Valladolid",
  "RCD Espanyol" = "Espanyol",
  "RCD Mallorca" = "Mallorca",
  "Girona FC" = "Girona",
  "Villarreal CF" = "Villarreal",
  "Valencia CF" = "Valencia",
  "Sevilla FC" = "Sevilla",
  "RC Celta" = "Celta Vigo",
  "Getafe CF" = "Getafe",
  "Elche CF" = "Elche",
  "Elche" = "Elche",
  
  # Serie A - BBC/Odds API variants
  "AC Milan" = "AC Milan",
  "Milan" = "AC Milan",
  "Inter" = "Inter Milan",
  "FC Internazionale Milano" = "Inter Milan",
  "Hellas Verona" = "Verona",
  "Hellas Verona FC" = "Verona",
  "AS Roma" = "Roma",
  "Atalanta BC" = "Atalanta",
  "SSC Napoli" = "Napoli",
  "SS Lazio" = "Lazio",
  "ACF Fiorentina" = "Fiorentina",
  "Bologna FC 1909" = "Bologna",
  "Torino FC" = "Torino",
  "Udinese Calcio" = "Udinese",
  "US Sassuolo Calcio" = "Sassuolo",
  "US Lecce" = "Lecce",
  "Genoa CFC" = "Genoa",
  "Cagliari Calcio" = "Cagliari",
  "Parma Calcio 1913" = "Parma",
  "Como 1907" = "Como",
  "Empoli FC" = "Empoli",
  "Venezia FC" = "Venezia",
  "AC Monza" = "Monza",
  
  # Bundesliga - BBC/Odds API variants
  "RasenBallsport Leipzig" = "RB Leipzig",
  "Bayern MÃ¼nchen" = "Bayern Munich",
  "FC Bayern Munich" = "Bayern Munich",
  "FC Bayern MÃ¼nchen" = "Bayern Munich",
  "Bor. M'gladbach" = "Borussia Monchengladbach",
  "Borussia M'gladbach" = "Borussia Monchengladbach",
  "Borussia MÃ¶nchengladbach" = "Borussia Monchengladbach",
  "M'gladbach" = "Borussia Monchengladbach",
  "M'Gladbach" = "Borussia Monchengladbach",
  "Monchengladbach" = "Borussia Monchengladbach",
  "Gladbach" = "Borussia Monchengladbach",
  "B. Monchengladbach" = "Borussia Monchengladbach",
  "Bor Monchengladbach" = "Borussia Monchengladbach",
  "Bor. Monchengladbach" = "Borussia Monchengladbach",
  "Borussia Monchengladbach" = "Borussia Monchengladbach",
  "Bor. Dortmund" = "Borussia Dortmund",
  "Dortmund" = "Borussia Dortmund",
  "Bayer 04 Leverkusen" = "Bayer Leverkusen",
  "Leverkusen" = "Bayer Leverkusen",
  "Eintracht Frankfurt" = "Eintracht Frankfurt",
  "Frankfurt" = "Eintracht Frankfurt",
  "E. Frankfurt" = "Eintracht Frankfurt",
  "FSV Mainz 05" = "Mainz",
  "1. FSV Mainz 05" = "Mainz",
  "Mainz 05" = "Mainz",
  "FC Heidenheim" = "Heidenheim",
  "1. FC Heidenheim 1846" = "Heidenheim",
  "1. FC Heidenheim" = "Heidenheim",
  "SC Freiburg" = "SC Freiburg",
  "Freiburg" = "SC Freiburg",
  "VfB Stuttgart" = "VfB Stuttgart",
  "Stuttgart" = "VfB Stuttgart",
  "VfL Wolfsburg" = "Wolfsburg",
  "TSG Hoffenheim" = "Hoffenheim",
  "TSG 1899 Hoffenheim" = "Hoffenheim",
  "FC Koln" = "Koln",
  "1. FC KÃ¶ln" = "Koln",
  "1. FC Koln" = "Koln",
  "Cologne" = "Koln",
  "FC St. Pauli" = "FC St Pauli",
  "FC St Pauli 1910" = "FC St Pauli",
  "St Pauli" = "FC St Pauli",
  "SV Werder Bremen" = "Werder Bremen",
  "Bremen" = "Werder Bremen",
  "FC Augsburg" = "Augsburg",
  "VfL Bochum" = "Bochum",
  "VfL Bochum 1848" = "Bochum",
  "1. FC Union Berlin" = "Union Berlin",
  "Holstein Kiel" = "Holstein Kiel",
  "Kiel" = "Holstein Kiel",
  
  # Ligue 1 - BBC/Odds API variants
  "Paris Saint-Germain" = "Paris Saint-Germain",
  "Paris Saint Germain" = "Paris Saint-Germain",
  "Paris SG" = "Paris Saint-Germain",
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
  
  # Scottish Premiership - BBC/Odds API variants
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
  
  # Eredivisie (Netherlands)
  "Ajax Amsterdam" = "Ajax",
  "AFC Ajax" = "Ajax",
  "Feyenoord Rotterdam" = "Feyenoord",
  "PSV Eindhoven" = "PSV",
  "AZ Alkmaar" = "AZ",
  "FC Twente" = "Twente",
  "FC Utrecht" = "Utrecht",
  "Sparta Rotterdam" = "Sparta Rotterdam",
  "NEC Nijmegen" = "NEC",
  "SC Heerenveen" = "Heerenveen",
  "Fortuna Sittard" = "Fortuna Sittard",
  "Go Ahead Eagles" = "Go Ahead Eagles",
  "PEC Zwolle" = "PEC Zwolle",
  "RKC Waalwijk" = "RKC Waalwijk",
  "Heracles Almelo" = "Heracles",
  "FC Groningen" = "Groningen",
  "Willem II" = "Willem II",
  "NAC Breda" = "NAC Breda",
  "Almere City FC" = "Almere City",
  "FC Volendam" = "Volendam",
  "Volendam" = "Volendam",
  
  # Belgian Pro League - BBC uses these exact names
  "Club Brugge KV" = "Club Brugge",
  "Club Bruges" = "Club Brugge",
  "RSC Anderlecht" = "Anderlecht",
  "Royal Antwerp FC" = "Antwerp",
  "Royal Antwerp" = "Antwerp",
  "KRC Genk" = "Genk",
  "Racing Genk" = "Genk",
  "KAA Gent" = "Gent",
  "AA Gent" = "Gent",
  "Union SG" = "Union Saint-Gilloise",
  "Union St. Gilloise" = "Union Saint-Gilloise",
  "Union St-Gilloise" = "Union Saint-Gilloise",
  "Royale Union SG" = "Union Saint-Gilloise",
  "Cercle Brugge KSV" = "Cercle Brugge",
  "Cercle Bruges" = "Cercle Brugge",
  "Standard Liege" = "Standard Liege",
  "Standard de LiÃ¨ge" = "Standard Liege",
  "Standard de Liege" = "Standard Liege",
  "R Standard Liege" = "Standard Liege",
  "Standard" = "Standard Liege",
  "OH Leuven" = "OH Leuven",
  "Oud-Heverlee Leuven" = "OH Leuven",
  "Leuven" = "OH Leuven",
  "KV Mechelen" = "Mechelen",
  "Charleroi" = "Sporting Charleroi",
  "R Charleroi SC" = "Sporting Charleroi",
  "Royal Charleroi" = "Sporting Charleroi",
  "STVV" = "Sint-Truiden",
  "Sint-Truidense VV" = "Sint-Truiden",
  "St Truiden" = "Sint-Truiden",
  "St-Truiden" = "Sint-Truiden",
  "KV Kortrijk" = "Kortrijk",
  "KVC Westerlo" = "Westerlo",
  "RWD Molenbeek" = "Molenbeek",
  "RWDM" = "Molenbeek",
  "FCV Dender EH" = "Dender",
  "Dender EH" = "Dender",
  "Beerschot VA" = "Beerschot",
  "SV Zulte Waregem" = "Zulte Waregem",
  "RAAL" = "RAAL La Louviere",
  "La Louviere" = "RAAL La Louviere",
  
  # Primeira Liga (Portugal)
  "SL Benfica" = "Benfica",
  "FC Porto" = "Porto",
  "Sporting CP" = "Sporting Lisbon",
  "Sporting Lisbon" = "Sporting Lisbon",
  "SC Braga" = "Braga",
  "Sp. Braga" = "Braga",
  "Sp Braga" = "Braga",
  "Sporting Braga" = "Braga",
  "Braga" = "Braga",
  "Vitoria SC" = "Vitoria Guimaraes",
  "VitÃ³ria SC" = "Vitoria Guimaraes",
  "Vitoria Guimaraes" = "Vitoria Guimaraes",
  "Guimaraes" = "Vitoria Guimaraes",
  "Vitoria de Guimaraes" = "Vitoria Guimaraes",
  "Moreirense FC" = "Moreirense",
  "Moreirense" = "Moreirense",
  "Famalicao" = "Famalicao",
  "Famalicão" = "Famalicao",
  "FC FamalicÃ£o" = "Famalicao",
  "Casa Pia AC" = "Casa Pia",
  "Casa Pia" = "Casa Pia",
  "Rio Ave FC" = "Rio Ave",
  "Rio Ave" = "Rio Ave",
  "Santa Clara" = "Santa Clara",
  "Gil Vicente FC" = "Gil Vicente",
  "Gil Vicente" = "Gil Vicente",
  "Arouca" = "Arouca",
  "FC Arouca" = "Arouca",
  "GD Estoril Praia" = "Estoril",
  "Estoril" = "Estoril",
  "Boavista FC" = "Boavista",
  "Boavista" = "Boavista",
  "Farense" = "Farense",
  "SC Farense" = "Farense",
  "Nacional" = "Nacional",
  "CD Nacional" = "Nacional",
  "AVS" = "AVS",
  "AVS Futebol SAD" = "AVS",
  "Estrela Amadora" = "Estrela Amadora",
  "CF Estrela" = "Estrela Amadora",
  "CF Estrela Amadora" = "Estrela Amadora",
  "E. Amadora" = "Estrela Amadora",
  "Estrela da Amadora" = "Estrela Amadora",
  "CF Os Belenenses" = "Estrela Amadora",
  "Estrela" = "Estrela Amadora",
  "Alverca" = "Alverca",
  "FC Alverca" = "Alverca",
  
  # Austrian Bundesliga
  "Red Bull Salzburg" = "Salzburg",
  "FC Red Bull Salzburg" = "Salzburg",
  "RB Salzburg" = "Salzburg",
  "SK Sturm Graz" = "Sturm Graz",
  "Sturm Graz" = "Sturm Graz",
  "SK Rapid Wien" = "Rapid Vienna",
  "Rapid Wien" = "Rapid Vienna",
  "Rapid" = "Rapid Vienna",
  "LASK" = "LASK",
  "LASK Linz" = "LASK",
  "Austria Wien" = "Austria Vienna",
  "FK Austria Wien" = "Austria Vienna",
  "Austria Vienna" = "Austria Vienna",
  "Wolfsberger AC" = "Wolfsberg",
  "WAC" = "Wolfsberg",
  "TSV Hartberg" = "Hartberg",
  "SCR Altach" = "Altach",
  "Rheindorf Altach" = "Altach",
  "WSG Tirol" = "WSG Tirol",
  "Wattens" = "WSG Tirol",
  "Austria Klagenfurt" = "Austria Klagenfurt",
  "SK Austria Klagenfurt" = "Austria Klagenfurt",
  "Blau-Weiss Linz" = "Blau-Weiss Linz",
  "BW Linz" = "Blau-Weiss Linz",
  "Grazer AK" = "GAK",
  "GAK 1902" = "GAK",
  
  # Swiss Super League - BBC uses these exact names
  "BSC Young Boys" = "Young Boys",
  "FC Basel 1893" = "Basel",
  "FC Basel" = "Basel",
  "FC Lugano" = "Lugano",
  "FC Zurich" = "Zurich",
  "FC ZÃ¼rich" = "Zurich",
  "Zurich" = "Zurich",
  "Servette FC" = "Servette",
  "FC St. Gallen" = "St.Gallen",
  "FC St Gallen" = "St.Gallen",
  "St Gallen" = "St.Gallen",
  "St. Gallen" = "St.Gallen",
  "FC Lausanne-Sport" = "Lausanne-Sport",
  "Lausanne" = "Lausanne-Sport",
  "FC Luzern" = "Luzern",
  "Lucerne" = "Luzern",
  "FC Sion" = "Sion",
  "Grasshopper Club Zurich" = "Grasshoppers",
  "Grasshopper Club ZÃ¼rich" = "Grasshoppers",
  "Grasshopper ZÃ¼rich" = "Grasshoppers",
  "Grasshopper Zurich" = "Grasshoppers",
  "GC Zurich" = "Grasshoppers",
  "FC Winterthur" = "Winterthur",
  "Yverdon Sport FC" = "Yverdon",
  "FC Thun" = "Thun"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Apply extra betting mappings to team name
#' This extends the base normalize_team_names() with betting-specific variants
normalize_betting_team_name <- function(team_name) {
  if (is.null(team_name) || is.na(team_name) || team_name == "") return(team_name)
  
  # Strip diacritics/accents to ASCII (handles UTF-8 vs mojibake mismatches)
  # e.g. "Atlético" → "Atletico", "Zürich" → "Zurich"
  team_name <- stringi::stri_trans_general(team_name, "Latin-ASCII")
  
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