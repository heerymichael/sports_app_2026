################################################################################
# SOCCER SHOWDOWN CONFIG
#
# Constants, scoring rules, regression coefficients, and data loading functions
# for the FanTeam Soccer Showdown module.
#
# FanTeam Showdown Format:
#   - 5 players, 1 CPT (1.5x points)
#   - Budget: 53M (configurable)
#   - No position restrictions
#   - CS stacking penalty: 2+ GK/DEF from same club on CS
#     Player1 = unaffected, Player2 = -1p, Player3 = -2p, Player4 = -3p
#   - Safety net for non-starters
#   - Max 10 entries per user
################################################################################

library(googlesheets4)

# =============================================================================
# GOOGLE SHEET IDs
# =============================================================================

SHOWDOWN_SALARY_SHEET_ID <- "1qWxFSP9KxihvQFOO4jzTfAqwz8-F3xRZ2cZf9NSedpc"
NOTONLYFPL_SHEET_ID      <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"

# =============================================================================
# SHOWDOWN CONTEST RULES
# =============================================================================

SHOWDOWN_RULES <- list(
  roster_size    = 5,
  salary_cap     = 53,
  cpt_multiplier = 1.5,
  max_entries     = 10
)

# CS stacking penalty: 2+ GK/DEF from same club who get a clean sheet
# Player1 = 0, Player2 = -1, Player3 = -2, Player4 = -3
SHOWDOWN_CS_STACK_PENALTY <- c(0, -1, -2, -3)

# =============================================================================
# FANTEAM SCORING RULES (Monster format)
# =============================================================================

SHOWDOWN_SCORING <- list(
  # Minutes
  mins_60       =  2,    # 60+ minutes played
  mins_sub      =  1,    # < 60 minutes (came on as sub)
  
  
  # Goals (by position)
  goal_gk       = 10,
  goal_def      = 10,
  goal_mid      =  9,
  goal_fwd      =  8,
  
  # Clean sheet (by position) - requires 60+ mins
  cs_gk         =  6,
  cs_def        =  6,
  cs_mid        =  1,
  cs_fwd        =  0,
  
  # Other
  assist        =  5,
  shot_on_target =  1,
  yellow_card   = -1,
  red_card      = -3,
  
  
  # GK/DEF concede penalty: -1 per 2 goals conceded (requires 60+ mins)
  goals_conceded_per_2 = -1,
  
  # GK specific
  save          =  1,    # per save
  penalty_save  =  5
)

#' Get goal points for a position
get_goal_points <- function(position) {
  switch(toupper(position),
         "GK"  = SHOWDOWN_SCORING$goal_gk,
         "DEF" = SHOWDOWN_SCORING$goal_def,
         "MID" = SHOWDOWN_SCORING$goal_mid,
         "FWD" = SHOWDOWN_SCORING$goal_fwd,
         SHOWDOWN_SCORING$goal_mid  # default
  )
}

#' Get clean sheet points for a position
get_cs_points <- function(position) {
  switch(toupper(position),
         "GK"  = SHOWDOWN_SCORING$cs_gk,
         "DEF" = SHOWDOWN_SCORING$cs_def,
         "MID" = SHOWDOWN_SCORING$cs_mid,
         "FWD" = SHOWDOWN_SCORING$cs_fwd,
         0  # default
  )
}

# =============================================================================
# REGRESSION COEFFICIENTS
# Validated on 3,460 Premier League team-match observations (5 seasons)
# =============================================================================

SHOWDOWN_COEFFICIENTS <- list(
  goals = list(
    intercept        = 0.27,
    implied_goals_for = 0.82,
    venue_away       = 0.03
  ),
  shots = list(
    intercept        = 6.3,
    implied_goals_for = 5.0,
    implied_total    = -0.84,
    venue_away       = -0.55
  ),
  sot = list(
    intercept        = 1.8,
    implied_goals_for = 1.87,
    venue_away       = -0.08
  ),
  yellows = list(
    intercept            = 1.7,
    implied_goals_against = 0.15,
    implied_goals_for    = -0.18,
    venue_away           = 0.14
  )
)

# =============================================================================
# POSITION STAT SHARES
# How team-level stats are distributed across position groups
# Based on empirical analysis of Premier League data
# =============================================================================

SHOWDOWN_STAT_SHARES <- list(
  goals = c(GK = 0.00, DEF = 0.10, MID = 0.45, FWD = 0.45),
  assists = c(GK = 0.02, DEF = 0.15, MID = 0.55, FWD = 0.28),
  shots_on_target = c(GK = 0.00, DEF = 0.08, MID = 0.42, FWD = 0.50),
  yellow_cards = c(GK = 0.03, DEF = 0.38, MID = 0.42, FWD = 0.17)
)

# Typical starters per position (used for per-player share calculation)
TYPICAL_STARTERS <- c(GK = 1, DEF = 4, MID = 3, FWD = 3)

# =============================================================================
# TEAM ABBREVIATION MAPPING
# Maps various team name formats to standard 3-letter abbreviations
# =============================================================================

TEAM_ABBREV_MAP <- c(
  # Full names -> abbreviations
  "Arsenal" = "ARS", "Aston Villa" = "AVL", "Bournemouth" = "BOU",
  
  "Brentford" = "BRE", "Brighton" = "BHA", "Chelsea" = "CHE",
  "Crystal Palace" = "CRY", "Everton" = "EVE", "Fulham" = "FUL",
  "Ipswich" = "IPS", "Leicester" = "LEI", "Liverpool" = "LIV",
  "Manchester City" = "MCI", "Manchester United" = "MUN",
  "Newcastle" = "NEW", "Nottingham Forest" = "NFO",
  "Southampton" = "SOU", "Tottenham" = "TOT",
  "West Ham" = "WHU", "Wolves" = "WOL",
  "Wolverhampton" = "WOL", "Sunderland" = "SUN",
  "Leeds" = "LEE", "Burnley" = "BUR",
  # Common short forms
  "Man City" = "MCI", "Man Utd" = "MUN", "Man United" = "MUN",
  "Spurs" = "TOT", "Nott'm Forest" = "NFO",
  "West Ham United" = "WHU", "Newcastle United" = "NEW",
  "Brighton & Hove Albion" = "BHA", "Wolverhampton Wanderers" = "WOL",
  "AFC Bournemouth" = "BOU", "Ipswich Town" = "IPS",
  "Leicester City" = "LEI", "Southampton FC" = "SOU",
  "Crystal Palace FC" = "CRY", "Nottingham Forest FC" = "NFO"
)

# Reverse mapping: abbreviation -> display name
TEAM_DISPLAY_NAMES <- c(
  ARS = "Arsenal", AVL = "Aston Villa", BOU = "Bournemouth",
  BRE = "Brentford", BHA = "Brighton", CHE = "Chelsea",
  CRY = "Crystal Palace", EVE = "Everton", FUL = "Fulham",
  IPS = "Ipswich", LEI = "Leicester", LIV = "Liverpool",
  MCI = "Man City", MUN = "Man United", NEW = "Newcastle",
  NFO = "Nott. Forest", SOU = "Southampton", TOT = "Tottenham",
  WHU = "West Ham", WOL = "Wolves"
)

# =============================================================================
# NAME MATCHING / NORMALIZATION
# =============================================================================

#' Normalize a player name for matching
#' Strips accents, lowercases, removes punctuation
normalize_name <- function(name) {
  if (is.na(name) || name == "") return("")
  n <- tolower(trimws(name))
  n <- iconv(n, to = "ASCII//TRANSLIT")
  n <- gsub("[^a-z ]", "", n)
  n <- gsub("\\s+", " ", n)
  trimws(n)
}

# Known name corrections: FanTeam name -> NotOnlyFPL name
SHOWDOWN_NAME_CORRECTIONS <- list(
  "Casemiro"           = "Casemiro",
  "Pablo"              = "Pablo Fornals",
  "Rayan"              = "Rayan Ait-Nouri",
  "Fernandes"          = "Bruno Fernandes",  # disambiguation via first name + club
  "Mateus Fernandes"   = "Mateus Fernandes"
)

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Get available showdown match sheets from Google Sheets
#' @return Character vector of sheet names matching gw_NN_XXX_YYY pattern
get_showdown_sheets <- function() {
  tryCatch({
    log_debug("Fetching showdown sheet names...", level = "INFO")
    sheets <- googlesheets4::sheet_names(SHOWDOWN_SALARY_SHEET_ID)
    # Filter for gameweek match pattern
    gw_sheets <- grep("^gw_\\d+_[A-Z]+_[A-Z]+$", sheets, value = TRUE)
    log_debug(sprintf("Found %d showdown sheets", length(gw_sheets)), level = "INFO")
    return(gw_sheets)
  }, error = function(e) {
    log_debug(paste("Error fetching showdown sheets:", e$message), level = "ERROR")
    return(character(0))
  })
}

#' Parse a sheet name into components
#' @param sheet_name e.g. "gw_26_MUN_WHU"
#' @return list(gameweek, home_abbrev, away_abbrev, display_label)
parse_sheet_name <- function(sheet_name) {
  parts <- strsplit(sheet_name, "_")[[1]]
  if (length(parts) < 4) return(NULL)
  
  gw <- as.integer(parts[2])
  home <- parts[3]
  away <- parts[4]
  
  home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[home]), TEAM_DISPLAY_NAMES[home], home)
  away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[away]), TEAM_DISPLAY_NAMES[away], away)
  
  list(
    gameweek     = gw,
    home_abbrev  = home,
    away_abbrev  = away,
    display_label = sprintf("GW%d: %s vs %s", gw, home_name, away_name)
  )
}

#' Load showdown player data from Google Sheet
#' @param sheet_name The specific sheet to load (e.g. "gw_26_MUN_WHU")
#' @return Data frame with player salary and lineup data
load_showdown_players <- function(sheet_name) {
  tryCatch({
    log_debug(sprintf("Loading showdown data from sheet: %s", sheet_name), level = "INFO")
    
    raw <- googlesheets4::read_sheet(
      SHOWDOWN_SALARY_SHEET_ID,
      sheet = sheet_name,
      col_types = "c"  # read all as character initially
    )
    
    if (nrow(raw) == 0) {
      log_debug("Empty sheet returned", level = "WARN")
      return(NULL)
    }
    
    # Standardize column names
    names(raw) <- tolower(trimws(names(raw)))
    
    # Expected columns from the uploaded sample
    df <- data.frame(
      tournament_id = as.integer(raw$tournament %||% raw$tournamentid %||% NA),
      player_id     = as.integer(raw$playerid %||% raw$player_id %||% NA),
      name          = as.character(raw$name %||% ""),
      first_name    = as.character(raw$fname %||% raw$firstname %||% ""),
      club          = as.character(raw$club %||% ""),
      lineup        = tolower(trimws(as.character(raw$lineup %||% "unknown"))),
      position      = toupper(trimws(as.character(raw$position %||% ""))),
      price         = as.numeric(raw$price %||% 0),
      stringsAsFactors = FALSE
    )
    
    # Normalize position names
    df$position <- gsub("goalkeeper", "GK", df$position, ignore.case = TRUE)
    df$position <- gsub("defender", "DEF", df$position, ignore.case = TRUE)
    df$position <- gsub("midfielder", "MID", df$position, ignore.case = TRUE)
    df$position <- gsub("forward", "FWD", df$position, ignore.case = TRUE)
    
    # Create full display name
    df$display_name <- ifelse(
      df$first_name != "" & df$first_name != df$name,
      paste(df$first_name, df$name),
      df$name
    )
    
    # Map club to abbreviation
    df$club_abbrev <- sapply(df$club, function(c) {
      abbr <- TEAM_ABBREV_MAP[c]
      if (!is.na(abbr)) return(unname(abbr))
      # Try matching as-is (might already be abbreviation)
      if (nchar(c) <= 3) return(toupper(c))
      return(c)
    })
    
    # Normalized name for matching
    df$name_normalized <- sapply(df$display_name, normalize_name)
    
    log_debug(sprintf("Loaded %d players from %s", nrow(df), sheet_name), level = "INFO")
    return(df)
    
  }, error = function(e) {
    log_debug(paste("Error loading showdown data:", e$message), level = "ERROR")
    return(NULL)
  })
}

#' Load player summary stats from NotOnlyFPL stats_overview
#' @return Data frame with per-player season averages
load_notonlyfpl_summary <- function() {
  tryCatch({
    log_debug("Loading NotOnlyFPL stats_overview...", level = "INFO")
    
    raw <- googlesheets4::read_sheet(
      NOTONLYFPL_SHEET_ID,
      sheet = "stats_overview",
      col_types = "c"
    )
    
    if (nrow(raw) == 0) return(NULL)
    
    names(raw) <- tolower(trimws(names(raw)))
    
    # Build summary data frame - flexibly match column names
    find_col <- function(patterns) {
      for (p in patterns) {
        match <- grep(p, names(raw), ignore.case = TRUE, value = TRUE)
        if (length(match) > 0) return(match[1])
      }
      return(NA)
    }
    
    col_name     <- find_col(c("^name$", "player"))
    col_team     <- find_col(c("^team$", "^club$"))
    col_pos      <- find_col(c("^pos", "^position"))
    col_pts      <- find_col(c("total.*pts", "total.*points", "^pts$", "^points$"))
    col_avg_pts  <- find_col(c("avg.*pts", "average.*pts", "ppg", "pts.*avg"))
    col_matches  <- find_col(c("^mp$", "matches", "apps", "appearances", "^gp$"))
    col_goals    <- find_col(c("^goals$", "^g$", "^gls$"))
    col_assists  <- find_col(c("^assists$", "^a$", "^ast$"))
    col_shots    <- find_col(c("^shots$", "^sh$"))
    col_sot      <- find_col(c("shot.*target", "^sot$", "^sht$"))
    col_mins     <- find_col(c("^mins$", "^minutes$", "^min$"))
    col_cs       <- find_col(c("clean.*sheet", "^cs$"))
    col_saves    <- find_col(c("^saves$", "^sv$"))
    col_gc       <- find_col(c("goals.*conc", "^gc$"))
    col_yellows  <- find_col(c("yellow", "^yc$", "^y$"))
    col_reds     <- find_col(c("red.*card", "^rc$", "^r$"))
    col_60       <- find_col(c("60", "sixty"))
    col_90       <- find_col(c("90", "ninety"))
    
    safe_num <- function(x) {
      if (is.null(x) || all(is.na(x))) return(rep(NA_real_, length(x)))
      suppressWarnings(as.numeric(x))
    }
    
    df <- data.frame(
      name         = as.character(raw[[col_name]]),
      team         = if (!is.na(col_team)) as.character(raw[[col_team]]) else NA,
      position     = if (!is.na(col_pos)) toupper(trimws(as.character(raw[[col_pos]]))) else NA,
      total_pts    = if (!is.na(col_pts)) safe_num(raw[[col_pts]]) else NA,
      avg_pts      = if (!is.na(col_avg_pts)) safe_num(raw[[col_avg_pts]]) else NA,
      matches      = if (!is.na(col_matches)) safe_num(raw[[col_matches]]) else NA,
      goals        = if (!is.na(col_goals)) safe_num(raw[[col_goals]]) else NA,
      assists      = if (!is.na(col_assists)) safe_num(raw[[col_assists]]) else NA,
      shots        = if (!is.na(col_shots)) safe_num(raw[[col_shots]]) else NA,
      sot          = if (!is.na(col_sot)) safe_num(raw[[col_sot]]) else NA,
      minutes      = if (!is.na(col_mins)) safe_num(raw[[col_mins]]) else NA,
      clean_sheets = if (!is.na(col_cs)) safe_num(raw[[col_cs]]) else NA,
      saves        = if (!is.na(col_saves)) safe_num(raw[[col_saves]]) else NA,
      goals_conc   = if (!is.na(col_gc)) safe_num(raw[[col_gc]]) else NA,
      yellows      = if (!is.na(col_yellows)) safe_num(raw[[col_yellows]]) else NA,
      reds         = if (!is.na(col_reds)) safe_num(raw[[col_reds]]) else NA,
      apps_60      = if (!is.na(col_60)) safe_num(raw[[col_60]]) else NA,
      apps_90      = if (!is.na(col_90)) safe_num(raw[[col_90]]) else NA,
      stringsAsFactors = FALSE
    )
    
    # Calculate per-game averages
    df$mins_per_game <- ifelse(!is.na(df$matches) & df$matches > 0,
                               round(df$minutes / df$matches, 0), NA)
    df$goals_per_game <- ifelse(!is.na(df$matches) & df$matches > 0,
                                round(df$goals / df$matches, 2), NA)
    df$assists_per_game <- ifelse(!is.na(df$matches) & df$matches > 0,
                                  round(df$assists / df$matches, 2), NA)
    df$shots_per_game <- ifelse(!is.na(df$matches) & df$matches > 0,
                                round(df$shots / df$matches, 2), NA)
    df$sot_per_game <- ifelse(!is.na(df$matches) & df$matches > 0,
                              round(df$sot / df$matches, 2), NA)
    df$saves_per_game <- ifelse(!is.na(df$matches) & df$matches > 0,
                                round(df$saves / df$matches, 2), NA)
    df$yellows_per_game <- ifelse(!is.na(df$matches) & df$matches > 0,
                                  round(df$yellows / df$matches, 2), NA)
    
    # Normalized name for matching
    df$name_normalized <- sapply(df$name, normalize_name)
    
    log_debug(sprintf("Loaded %d players from NotOnlyFPL stats_overview", nrow(df)), level = "INFO")
    return(df)
    
  }, error = function(e) {
    log_debug(paste("Error loading NotOnlyFPL data:", e$message), level = "ERROR")
    return(NULL)
  })
}

# =============================================================================
# ODDS -> TEAM STATS ENGINE
# =============================================================================

#' Calculate implied goals from result probabilities and total goals
#' Uses conservative model validated on 3,460 observations
#'
#' @param home_win Home win probability (0-100)
#' @param draw Draw probability (0-100)
#' @param away_win Away win probability (0-100)
#' @param total_goals Expected total match goals
#' @return list(home_goals, away_goals)
calculate_implied_goals <- function(home_win, draw, away_win, total_goals) {
  # Normalize percentages
  total_pct <- home_win + draw + away_win
  if (total_pct == 0) total_pct <- 100
  
  hw <- home_win / total_pct
  dr <- draw / total_pct
  aw <- away_win / total_pct
  
  # Conservative model: sensitivity = 0.15
  # Higher win% -> higher share of goals, but capped conservatively
  win_loss_diff <- hw - aw
  home_share <- 0.5 + (win_loss_diff * 0.15)
  home_share <- max(0.30, min(0.70, home_share))
  
  # Draw pulls toward 50/50 split
  draw_pull <- dr * 0.1
  home_share <- home_share * (1 - draw_pull) + 0.5 * draw_pull
  
  list(
    home_goals = round(total_goals * home_share, 2),
    away_goals = round(total_goals * (1 - home_share), 2)
  )
}

#' Calculate clean sheet probability using Poisson distribution
#' @param goals_against Expected goals against
#' @return Clean sheet percentage (0-100)
calculate_cs_pct <- function(goals_against) {
  if (is.na(goals_against) || goals_against < 0) return(NA_real_)
  round(exp(-goals_against) * 100, 1)
}

#' Predict team stats from implied goals using regression coefficients
#' @param goals_for Implied goals for this team
#' @param goals_against Implied goals against this team
#' @param is_away Logical, TRUE if away team
#' @return list(goals, shots, sot, yellow_cards, cs_pct)
predict_team_stats <- function(goals_for, goals_against, is_away = FALSE) {
  away_flag <- if (is_away) 1 else 0
  total <- goals_for + goals_against
  coef <- SHOWDOWN_COEFFICIENTS
  
  list(
    goals = max(0, coef$goals$intercept +
                  coef$goals$implied_goals_for * goals_for +
                  coef$goals$venue_away * away_flag),
    shots = max(5, coef$shots$intercept +
                  coef$shots$implied_goals_for * goals_for +
                  coef$shots$implied_total * total +
                  coef$shots$venue_away * away_flag),
    sot = max(1, coef$sot$intercept +
                coef$sot$implied_goals_for * goals_for +
                coef$sot$venue_away * away_flag),
    yellow_cards = max(0.5, coef$yellows$intercept +
                         coef$yellows$implied_goals_against * goals_against +
                         coef$yellows$implied_goals_for * goals_for +
                         coef$yellows$venue_away * away_flag),
    cs_pct = calculate_cs_pct(goals_against)
  )
}

# =============================================================================
# BIVARIATE POISSON SIMULATION ENGINE
# Holgate (1964) formulation with draw-probability calibration
# =============================================================================

#' Bivariate Poisson probability mass function
#'
#' Computes P(X=x, Y=y) where (X,Y) follow a bivariate Poisson distribution
#' with marginal means mu_x, mu_y and covariance parameter rho.
#'
#' Uses the Holgate (1964) formulation:
#'   X = Z1 + Z3,  Y = Z2 + Z3
#'   Z1 ~ Poi(mu_x - rho), Z2 ~ Poi(mu_y - rho), Z3 ~ Poi(rho)
#'
#' @param x Non-negative integer (home goals/shots)
#' @param y Non-negative integer (away goals/shots)
#' @param mu_x Expected value of X (home team)
#' @param mu_y Expected value of Y (away team)
#' @param rho Covariance parameter (0 = independent Poisson)
#' @return Probability P(X=x, Y=y)
bpois_pmf <- function(x, y, mu_x, mu_y, rho = 0) {
  # Clamp rho to valid range [0, min(mu_x, mu_y))
  rho <- max(0, min(rho, min(mu_x, mu_y) - 1e-6))
  
  # Independent Poisson shortcut
  if (rho < 1e-8) return(dpois(x, mu_x) * dpois(y, mu_y))
  
  l1 <- mu_x - rho  # independent home component
  l2 <- mu_y - rho  # independent away component
  l3 <- rho          # shared component
  
  k_max <- min(x, y)
  s <- 0
  for (k in 0:k_max) {
    s <- s + (l1^(x - k) * l2^(y - k) * l3^k) /
      (factorial(x - k) * factorial(y - k) * factorial(k))
  }
  
  exp(-(l1 + l2 + l3)) * s
}

#' Calibrate correlation parameter to match target draw probability
#'
#' Finds rho such that sum of P(k,k) for k=0..N equals the user's draw%.
#' Note: bivariate Poisson can only INCREASE draw probability vs independent
#' Poisson (since rho >= 0). If independent Poisson already matches or
#' exceeds the target, returns 0.
#'
#' @param mu_home Expected home goals
#' @param mu_away Expected away goals
#' @param target_draw_pct Target draw probability (0-100 scale)
#' @param precision_goals Max goals to sum over (default 10)
#' @return Optimal rho value (>= 0)
calibrate_rho <- function(mu_home, mu_away, target_draw_pct, precision_goals = 10) {
  target <- target_draw_pct / 100
  
  # Independent Poisson draw probability
  indep_draw <- sum(dpois(0:precision_goals, mu_home) * dpois(0:precision_goals, mu_away))
  
  # If independent Poisson already meets target, no correlation needed
  if (indep_draw >= target - 0.002) return(0)
  
  # Maximum possible rho (must keep lambda1, lambda2 > 0)
  max_rho <- min(mu_home, mu_away) - 0.01
  if (max_rho <= 0.001) return(0)
  
  # Objective: P(draw | rho) - target = 0
  draw_prob_diff <- function(rho) {
    p_draw <- 0
    for (k in 0:precision_goals) {
      p_draw <- p_draw + bpois_pmf(k, k, mu_home, mu_away, rho)
    }
    p_draw - target
  }
  
  # Check if max rho can even reach the target
  max_draw_val <- tryCatch(draw_prob_diff(max_rho) + target, error = function(e) 0)
  if (max_draw_val < target) return(max_rho)  # Use max available
  
  tryCatch({
    result <- uniroot(draw_prob_diff, c(0, max_rho), tol = 0.0005)
    max(0, result$root)
  }, error = function(e) 0)
}

#' Generate bivariate Poisson probability matrix (0 to max_val+)
#'
#' Produces a (max_val+1) x (max_val+1) matrix where the last row/column
#' captures all overflow probability (i.e., "4+" means "4 or more").
#'
#' @param mu_x Expected value for row variable (home team)
#' @param mu_y Expected value for column variable (away team)
#' @param rho Correlation parameter
#' @param max_val Maximum explicit value (default 4 for "0-4+")
#' @param precision Upper limit for internal computation (default 12)
#' @return Named list with matrix, labels, and result probabilities
generate_bivariate_matrix <- function(mu_x, mu_y, rho = 0, max_val = 4, precision = 12) {
  n <- precision + 1
  full_grid <- matrix(0, nrow = n, ncol = n)
  
  for (i in 0:precision) {
    for (j in 0:precision) {
      full_grid[i + 1, j + 1] <- bpois_pmf(i, j, mu_x, mu_y, rho)
    }
  }
  
  # Aggregate into 0..max_val-1, max_val+ buckets
  display_n <- max_val + 1
  display_grid <- matrix(0, nrow = display_n, ncol = display_n)
  
  for (i in 1:display_n) {
    for (j in 1:display_n) {
      if (i < display_n && j < display_n) {
        display_grid[i, j] <- full_grid[i, j]
      } else if (i == display_n && j < display_n) {
        display_grid[i, j] <- sum(full_grid[display_n:n, j])
      } else if (i < display_n && j == display_n) {
        display_grid[i, j] <- sum(full_grid[i, display_n:n])
      } else {
        display_grid[i, j] <- sum(full_grid[display_n:n, display_n:n])
      }
    }
  }
  
  labels <- c(as.character(0:(max_val - 1)), paste0(max_val, "+"))
  
  # Result probabilities from full grid
  home_win <- 0; draw <- 0; away_win <- 0
  for (i in 0:precision) {
    for (j in 0:precision) {
      p <- full_grid[i + 1, j + 1]
      if (i > j) home_win <- home_win + p
      else if (i == j) draw <- draw + p
      else away_win <- away_win + p
    }
  }
  
  list(
    matrix = display_grid,
    row_labels = labels,
    col_labels = labels,
    result_probs = list(
      home_win = round(home_win * 100, 1),
      draw = round(draw * 100, 1),
      away_win = round(away_win * 100, 1)
    )
  )
}

#' Generate score matrix from user's adjusted odds
#'
#' @param mu_home Implied home goals
#' @param mu_away Implied away goals
#' @param draw_pct User's draw probability (0-100)
#' @param max_goals Grid cutoff (default 4)
#' @return Bivariate matrix result plus rho and lambda values
generate_score_matrix <- function(mu_home, mu_away, draw_pct, max_goals = 4) {
  rho <- calibrate_rho(mu_home, mu_away, draw_pct)
  result <- generate_bivariate_matrix(mu_home, mu_away, rho,
                                      max_val = max_goals, precision = 12)
  result$rho <- round(rho, 4)
  result$mu_home <- mu_home
  result$mu_away <- mu_away
  result
}

#' Generate shots-on-target matrix
#'
#' Uses predicted SoT as Poisson parameters. SoT correlation is derived
#' from goal correlation scaled by approximate conversion rate (~30%).
#'
#' @param sot_home Predicted home SoT
#' @param sot_away Predicted away SoT
#' @param goal_rho Goal correlation parameter
#' @param max_sot Grid cutoff (default 4)
#' @return Bivariate matrix result
generate_sot_matrix <- function(sot_home, sot_away, goal_rho = 0, max_sot = 4) {
  # SoT correlation: attenuated from goal correlation
  sot_rho <- goal_rho * 0.3
  sot_rho <- min(sot_rho, min(sot_home, sot_away) - 0.01)
  sot_rho <- max(0, sot_rho)
  
  generate_bivariate_matrix(sot_home, sot_away, sot_rho,
                            max_val = max_sot, precision = 15)
}

#' Generate a binned bivariate Poisson matrix for total shots
#'
#' Since total shots per team are typically 8-15, individual-value Poisson cells
#' would produce a huge grid. Instead we bin into defined ranges and sum
#' probabilities within each bin.
#'
#' @param shots_home Predicted home shots
#' @param shots_away Predicted away shots
#' @param goal_rho Goal correlation parameter (attenuated for shots)
#' @param bins List of bin definitions, each with label and range (min, max)
#' @return Named list: matrix, row_labels, col_labels, result_probs (N/A for shots)
generate_shots_matrix <- function(shots_home, shots_away, goal_rho = 0,
                                  bins = NULL) {
  # Default bins: 0-4, 5-7, 8-10, 11-13, 14+
  
  if (is.null(bins)) {
    bins <- list(
      list(label = "0-4",  range = c(0, 4)),
      list(label = "5-7",  range = c(5, 7)),
      list(label = "8-10", range = c(8, 10)),
      list(label = "11-13", range = c(11, 13)),
      list(label = "14+",  range = c(14, 25))
    )
  }
  
  # Shots correlation: very weak compared to goals
  shots_rho <- goal_rho * 0.15
  shots_rho <- min(shots_rho, min(shots_home, shots_away) - 0.01)
  shots_rho <- max(0, shots_rho)
  
  # Compute full fine-grained grid up to precision
  precision <- 25
  n <- precision + 1
  full_grid <- matrix(0, nrow = n, ncol = n)
  
  for (i in 0:precision) {
    for (j in 0:precision) {
      full_grid[i + 1, j + 1] <- bpois_pmf(i, j, shots_home, shots_away, shots_rho)
    }
  }
  
  # Aggregate into bins
  nb <- length(bins)
  display_grid <- matrix(0, nrow = nb, ncol = nb)
  
  for (bi in 1:nb) {
    r_min <- bins[[bi]]$range[1]
    r_max <- min(bins[[bi]]$range[2], precision)
    for (bj in 1:nb) {
      c_min <- bins[[bj]]$range[1]
      c_max <- min(bins[[bj]]$range[2], precision)
      display_grid[bi, bj] <- sum(full_grid[(r_min:r_max) + 1, (c_min:c_max) + 1])
    }
  }
  
  labels <- sapply(bins, function(b) b$label)
  
  list(
    matrix = display_grid,
    row_labels = labels,
    col_labels = labels,
    result_probs = NULL  # Not meaningful for shots
  )
}

#' Get team logo path from 3-letter abbreviation
#'
#' Reverse-looks up the abbreviation in TEAM_ABBREVIATIONS (soccer_config.R)
#' to find the canonical name, then retrieves the logo path.
#'
#' @param abbrev 3-letter team abbreviation (e.g., "MUN", "LIV")
#' @return Logo path string or NULL
get_logo_from_abbrev <- function(abbrev) {
  if (is.na(abbrev) || abbrev == "") return(NULL)
  canonical <- names(TEAM_ABBREVIATIONS)[match(abbrev, TEAM_ABBREVIATIONS)]
  if (is.na(canonical)) return(NULL)
  get_soccer_team_logo(canonical)
}

# =============================================================================
# PLAYER PROJECTION ENGINE
# =============================================================================

#' Project individual player fantasy points
#'
#' Allocates team-level predicted stats to individual players based on:
#' 1. Position-based share of team output
#' 2. Number of starters at that position
#' 3. Individual performance adjustment (if NotOnlyFPL data available)
#'
#' @param player Data frame row: position, club_abbrev, price
#' @param team_stats Output from predict_team_stats()
#' @param opp_stats Output from predict_team_stats() for opponent
#' @param starters_at_pos Number of starters at this player's position
#' @param player_summary Optional: matched NotOnlyFPL data for individual adjustment
#' @param pos_avg_pts Optional: average PPG for this position group (for adjustment)
#' @return list(projected_pts, breakdown)
project_player_points <- function(player, team_stats, opp_stats,
                                  starters_at_pos = NULL,
                                  player_summary = NULL,
                                  pos_avg_pts = NULL) {
  
  pos <- toupper(player$position)
  if (!pos %in% c("GK", "DEF", "MID", "FWD")) pos <- "MID"
  
  # Default starters at position if not provided
  if (is.null(starters_at_pos) || is.na(starters_at_pos) || starters_at_pos == 0) {
    starters_at_pos <- TYPICAL_STARTERS[pos]
  }
  
  # Per-player share = position share / number of starters at position
  g_share   <- SHOWDOWN_STAT_SHARES$goals[pos] / starters_at_pos
  a_share   <- SHOWDOWN_STAT_SHARES$assists[pos] / starters_at_pos
  sot_share <- SHOWDOWN_STAT_SHARES$shots_on_target[pos] / starters_at_pos
  y_share   <- SHOWDOWN_STAT_SHARES$yellow_cards[pos] / starters_at_pos
  
  # Individual adjustment factor
  # If we have this player's historical PPG and the position average,
  # scale their share accordingly
  adj_factor <- 1.0
  if (!is.null(player_summary) && !is.null(pos_avg_pts)) {
    player_ppg <- player_summary$avg_pts
    if (!is.na(player_ppg) && !is.na(pos_avg_pts) && pos_avg_pts > 0) {
      # Regress toward mean: don't let adjustment be too extreme
      raw_adj <- player_ppg / pos_avg_pts
      adj_factor <- 0.5 + 0.5 * raw_adj  # blend 50% individual, 50% average
      adj_factor <- max(0.3, min(2.0, adj_factor))
    }
  }
  
  # Apply individual adjustment to offensive shares
  g_share   <- g_share * adj_factor
  a_share   <- a_share * adj_factor
  sot_share <- sot_share * adj_factor
  
  # Minutes probability
  prob_60 <- 0.85  # default
  if (!is.null(player_summary) && !is.na(player_summary$mins_per_game)) {
    mpg <- player_summary$mins_per_game
    if (mpg >= 80) prob_60 <- 0.95
    else if (mpg >= 60) prob_60 <- 0.80
    else if (mpg >= 45) prob_60 <- 0.50
    else prob_60 <- 0.25
  }
  
  # Points breakdown
  mins_pts   <- prob_60 * SHOWDOWN_SCORING$mins_60 + (1 - prob_60) * SHOWDOWN_SCORING$mins_sub
  goal_pts   <- team_stats$goals * g_share * get_goal_points(pos)
  assist_pts <- team_stats$goals * 0.65 * a_share * SHOWDOWN_SCORING$assist  # ~65% of goals are assisted
  sot_pts    <- team_stats$sot * sot_share * SHOWDOWN_SCORING$shot_on_target
  cs_pts     <- (team_stats$cs_pct / 100) * get_cs_points(pos) * prob_60
  yellow_pts <- team_stats$yellow_cards * y_share * SHOWDOWN_SCORING$yellow_card
  
  # Goals conceded penalty (GK/DEF only, requires 60+ mins)
  gc_pts <- 0
  if (pos %in% c("GK", "DEF")) {
    # Expected goals conceded = opponent's predicted goals
    exp_gc <- opp_stats$goals
    gc_pts <- floor(exp_gc / 2) * SHOWDOWN_SCORING$goals_conceded_per_2 * prob_60
  }
  
  # GK saves bonus
  save_pts <- 0
  if (pos == "GK") {
    # Estimate saves as ~70% of opponent SoT
    exp_saves <- opp_stats$sot * 0.70
    save_pts <- exp_saves * SHOWDOWN_SCORING$save
  }
  
  total <- mins_pts + goal_pts + assist_pts + sot_pts + cs_pts + yellow_pts + gc_pts + save_pts
  
  list(
    projected_pts = round(max(0, total), 1),
    breakdown = list(
      minutes  = round(mins_pts, 2),
      goals    = round(goal_pts, 2),
      assists  = round(assist_pts, 2),
      sot      = round(sot_pts, 2),
      cs       = round(cs_pts, 2),
      yellows  = round(yellow_pts, 2),
      gc       = round(gc_pts, 2),
      saves    = round(save_pts, 2)
    )
  )
}

# =============================================================================
# LINEUP OPTIMIZER (LP)
# =============================================================================

#' Optimize a showdown lineup using linear programming
#'
#' @param players Data frame with columns: display_name, position, price,
#'   projected_pts, club_abbrev, player_id
#' @param salary_cap Budget constraint
#' @param roster_size Number of players to select
#' @param cpt_multiplier Captain multiplier
#' @param locked_cpt Optional: player_id to lock as captain
#' @param excluded_ids Optional: vector of player_ids to exclude
#' @param required_ids Optional: vector of player_ids that must be in lineup
#' @return List with lineup data frame and metadata
optimize_showdown_lineup <- function(players, salary_cap = 53,
                                     roster_size = 5, cpt_multiplier = 1.5,
                                     locked_cpt = NULL, excluded_ids = NULL,
                                     required_ids = NULL) {
  
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("lpSolve package required for optimization")
  }
  
  # Filter excluded players
  if (!is.null(excluded_ids)) {
    players <- players[!players$player_id %in% excluded_ids, ]
  }
  
  n <- nrow(players)
  if (n < roster_size) {
    warning("Not enough eligible players for a full lineup")
    return(NULL)
  }
  
  # Strategy: try each player as captain, solve LP for remaining 4
  # Pick the combination that maximizes total effective points
  
  best_total <- -Inf
  best_lineup <- NULL
  
  # Candidate captains: either locked or all eligible players
  cpt_candidates <- if (!is.null(locked_cpt)) {
    which(players$player_id == locked_cpt)
  } else {
    1:n
  }
  
  for (cpt_idx in cpt_candidates) {
    cpt_row <- players[cpt_idx, ]
    cpt_effective_pts <- cpt_row$projected_pts * cpt_multiplier
    remaining_budget <- salary_cap - cpt_row$price
    
    # Remaining players (exclude captain)
    remaining <- players[-cpt_idx, ]
    nr <- nrow(remaining)
    
    if (nr < (roster_size - 1)) next
    
    # Filter required players
    flex_size <- roster_size - 1
    
    # LP: maximize points subject to salary and roster size
    obj <- remaining$projected_pts
    
    # Constraints
    const_mat <- rbind(
      remaining$price,      # salary constraint
      rep(1, nr)            # roster size
    )
    const_dir <- c("<=", "=")
    const_rhs <- c(remaining_budget, flex_size)
    
    # Add required player constraints
    if (!is.null(required_ids)) {
      req_ids <- setdiff(required_ids, cpt_row$player_id)
      for (rid in req_ids) {
        req_row <- rep(0, nr)
        req_idx <- which(remaining$player_id == rid)
        if (length(req_idx) > 0) {
          req_row[req_idx] <- 1
          const_mat <- rbind(const_mat, req_row)
          const_dir <- c(const_dir, "=")
          const_rhs <- c(const_rhs, 1)
        }
      }
    }
    
    result <- tryCatch({
      lpSolve::lp("max", obj, const_mat, const_dir, const_rhs, all.bin = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(result) || result$status != 0) next
    
    selected <- which(result$solution == 1)
    flex_pts <- sum(remaining$projected_pts[selected])
    total_pts <- cpt_effective_pts + flex_pts
    
    if (total_pts > best_total) {
      best_total <- total_pts
      flex_df <- remaining[selected, ]
      flex_df$role <- "FLEX"
      flex_df$effective_pts <- flex_df$projected_pts
      
      cpt_df <- cpt_row
      cpt_df$role <- "CPT"
      cpt_df$effective_pts <- cpt_effective_pts
      
      best_lineup <- rbind(cpt_df, flex_df)
    }
  }
  
  if (is.null(best_lineup)) return(NULL)
  
  list(
    lineup     = best_lineup,
    total_pts  = round(best_total, 1),
    total_salary = sum(best_lineup$price),
    remaining  = salary_cap - sum(best_lineup$price)
  )
}

#' Generate multiple diverse lineups
#' @param players Player pool with projections
#' @param n_lineups Number of lineups to generate
#' @param variance_pct Percentage variance to add for diversity (0-50)
#' @param ... Additional args passed to optimize_showdown_lineup
#' @return List of lineup results
generate_showdown_lineups <- function(players, n_lineups = 5, variance_pct = 10, ...) {
  results <- list()
  seen_combos <- list()
  attempts <- 0
  max_attempts <- n_lineups * 5
  
  while (length(results) < n_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Add random variance to projections
    jittered <- players
    if (variance_pct > 0) {
      noise <- runif(nrow(jittered), 1 - variance_pct/100, 1 + variance_pct/100)
      jittered$projected_pts <- jittered$projected_pts * noise
    }
    
    result <- optimize_showdown_lineup(jittered, ...)
    if (is.null(result)) next
    
    # Check for uniqueness
    combo_key <- paste(sort(result$lineup$player_id), collapse = "-")
    if (combo_key %in% seen_combos) next
    
    seen_combos <- c(seen_combos, combo_key)
    
    # Recalculate with original projections
    result$lineup$projected_pts <- players$projected_pts[match(result$lineup$player_id, players$player_id)]
    result$lineup$effective_pts <- ifelse(
      result$lineup$role == "CPT",
      result$lineup$projected_pts * SHOWDOWN_RULES$cpt_multiplier,
      result$lineup$projected_pts
    )
    result$total_pts <- sum(result$lineup$effective_pts)
    
    results[[length(results) + 1]] <- result
  }
  
  # Sort by total points
  pts <- sapply(results, function(r) r$total_pts)
  results <- results[order(pts, decreasing = TRUE)]
  
  results
}