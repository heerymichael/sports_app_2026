# =============================================================================
# Soccer Ratings Engine
# 
# Ownership estimation and player grading for FanTeam DFS contests
#
# Features:
#   - Ownership estimation from salary, form, matchup quality, position
#   - Cash grades (reliability + matchup + value)
#   - GPP grades (ceiling + leverage + matchup)
#   - Captain grades (ceiling + captain leverage)
#   - Calibration against historical ownership data (GW20+)
#
# FanTeam Monster Scoring Rules (encoded for reference):
#   ALL:  1 Appearance, 1 60+min, 3 Assist, -1 Yellow, -3 Red,
#         -2 Own Goal / Pen Miss / Caused Pen / Caused scoring FK
#   GK:   0.5/Save, 4 CS(60+min), -1/2 goals conceded, 8 Goal, 1 SOT, 5 Pen Save
#   DEF:  4 CS(60+min), -1/2 goals conceded, 6 Goal, 0.6 SOT
#   MID:  1 CS(60+min), 1 Full Match, 5 Goal, 0.4 SOT
#   FWD:  1 Full Match, 4 Goal, 0.4 SOT
#   Captain: 2x all points
#   NOTE: Positive/Negative Impact (+/-0.3) excluded from modelling.
#         Depends on team winning during player's time on pitch and cannot
#         be reliably projected. Flagged for future calibration.
#
# Column convention: uses `pos` (GK/DEF/MID/FWD) throughout.
# The handbuild module maps salary loader's `position` -> `pos` before calling.
#
# Data Sources:
#   - Google Sheet: Historical ownership (OWNERSHIP_SHEET_ID)
#   - FanTeam salaries (local CSV via soccer_fanteam_loader.R)
#   - Odds report (local CSV via mod_soccer_matchups.R load_fanteam_odds())
#   - Player stats (Google Sheets via mod_soccer_player_stats.R)
#
# Depends on: helpers.R, soccer_config.R, soccer_fanteam_loader.R
# Sourced AFTER: soccer_fanteam_matching.R, BEFORE: mod_soccer_handbuild.R
# =============================================================================

# =============================================================================
# CONSTANTS
# =============================================================================

# Google Sheet for historical ownership data
OWNERSHIP_SHEET_ID <- "1uI0wkZVLBL9hgMDfoszuZYDx-n9P03BJPZ4vP1KAMjY"

# Rating tier definitions (checked top-down: first match wins)
RATING_TIERS <- list(
  elite = list(
    min_score = 85,
    cash    = "Cash Lock",
    gpp     = "Smash Play",
    captain = "Crown Him",
    emoji_cash = "\U0001F512",
    emoji_gpp  = "\U0001F680",
    emoji_cpt  = "\U0001F451",
    color   = "#2E7D32",
    bg      = "#E8F5E9"
  ),
  strong = list(
    min_score = 70,
    cash    = "Strong Play",
    gpp     = "Leverage Gold",
    captain = "Top Captain",
    emoji_cash = "\U0001F4AA",
    emoji_gpp  = "\U0001F3AF",
    emoji_cpt  = "\u2B50",
    color   = "#1565C0",
    bg      = "#E3F2FD"
  ),
  good = list(
    min_score = 55,
    cash    = "Solid",
    gpp     = "Sneaky Good",
    captain = "Live Longshot",
    emoji_cash = "\u2705",
    emoji_gpp  = "\U0001F50D",
    emoji_cpt  = "\U0001F4A1",
    color   = "#6A1B9A",
    bg      = "#F3E5F5"
  ),
  neutral = list(
    min_score = 40,
    cash    = "Playable",
    gpp     = "Meh",
    captain = "Risky Captain",
    emoji_cash = "\u2796",
    emoji_gpp  = "\U0001F937",
    emoji_cpt  = "\u2796",
    color   = "#7A7A7A",
    bg      = "#F5F5F5"
  ),
  weak = list(
    min_score = 25,
    cash    = "Thin Ice",
    gpp     = "Chalk Trap",
    captain = "Fade Captain",
    emoji_cash = "\u26A0\uFE0F",
    emoji_gpp  = "\U0001F480",
    emoji_cpt  = "\u274C",
    color   = "#E65100",
    bg      = "#FFF3E0"
  ),
  avoid = list(
    min_score = 0,
    cash    = "Hard Pass",
    gpp     = "Dead Money",
    captain = "No",
    emoji_cash = "\u274C",
    emoji_gpp  = "\u274C",
    emoji_cpt  = "\u274C",
    color   = "#C62828",
    bg      = "#FFEBEE"
  )
)

# =============================================================================
# DATA LOADING - Historical Ownership
# =============================================================================

#' Load historical ownership data from Google Sheet
#' @return Combined data frame across all available week_* tabs, or NULL
load_ownership_history <- function() {
  log_debug("========================================", level = "INFO")
  log_debug("load_ownership_history() called", level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    
    # Discover available week sheets
    sheet_meta <- googlesheets4::gs4_get(OWNERSHIP_SHEET_ID)
    all_sheets <- sheet_meta$sheets$name
    week_sheets <- all_sheets[grepl("^week[-_]\\d+$", all_sheets, ignore.case = TRUE)]
    
    log_debug("Found ownership sheets:", paste(week_sheets, collapse = ", "), level = "INFO")
    
    if (length(week_sheets) == 0) {
      log_debug("No ownership week sheets found", level = "WARN")
      return(NULL)
    }
    
    # Load each sheet
    all_data <- lapply(week_sheets, function(sn) {
      tryCatch({
        gw_num <- as.integer(gsub("\\D", "", sn))
        
        raw <- googlesheets4::read_sheet(
          OWNERSHIP_SHEET_ID, sheet = sn, col_types = "c"
        )
        
        log_debug(sprintf("  %s: %d rows, raw cols: %s",
                          sn, nrow(raw), paste(names(raw), collapse = ", ")), level = "DEBUG")
        
        # Pre-clean_names fix: Cyrillic С (U+0421) in "С %" captain column
        # Replace Cyrillic С with Latin C before clean_names mangles it
        names(raw) <- gsub("\u0421", "C", names(raw))
        
        # Also catch any column that looks like just "C" or "C%" for captain
        # by renaming before clean_names
        raw_names_upper <- toupper(trimws(names(raw)))
        cpt_idx <- which(raw_names_upper %in% c("C %", "C%", "C", "CPT %", "CPT%", "CAPTAIN %"))
        if (length(cpt_idx) == 1) names(raw)[cpt_idx] <- "captain_pct"
        
        # Now clean
        raw <- raw %>% janitor::clean_names()
        
        log_debug(sprintf("  %s: %d rows, cleaned cols: %s",
                          sn, nrow(raw), paste(names(raw), collapse = ", ")), level = "DEBUG")
        
        # Standardize column names (handle many variants)
        col_map <- c(
          # Ownership variants
          "own"          = "own_pct",
          "own_percent"  = "own_pct",
          "ownership"    = "own_pct",
          "ownership_percent" = "own_pct",
          "o"            = "own_pct",
          # Captain variants
          "c"            = "captain_pct",
          "c_percent"    = "captain_pct",
          "captain"      = "captain_pct",
          "cap"          = "captain_pct",
          "cap_percent"  = "captain_pct",
          # Player name variants
          "name"         = "player",
          "player_name"  = "player",
          # Position variants
          "position"     = "pos",
          # Salary/price variants
          "salary"       = "price",
          "cost"         = "price",
          # Score variants
          "pts"          = "score",
          "points"       = "score",
          "total_pts"    = "score",
          "fpts"         = "score"
        )
        for (old in names(col_map)) {
          new <- col_map[old]
          if (old %in% names(raw) && !new %in% names(raw)) {
            names(raw)[names(raw) == old] <- new
          }
        }
        
        # Parse helper
        parse_num <- function(x) {
          x <- gsub("[^0-9.\\-]", "", as.character(x))
          x[x == "" | is.na(x)] <- NA_character_
          as.numeric(x)
        }
        
        raw <- raw %>% mutate(
          gameweek    = gw_num,
          price       = if ("price" %in% names(.)) parse_num(price) else NA_real_,
          own_pct     = if ("own_pct" %in% names(.)) parse_num(own_pct) else NA_real_,
          captain_pct = if ("captain_pct" %in% names(.)) parse_num(captain_pct) else NA_real_,
          form        = if ("form" %in% names(.)) parse_num(form) else NA_real_,
          score       = if ("score" %in% names(.)) parse_num(score) else NA_real_
        )
        
        # Normalize position if present
        if ("pos" %in% names(raw)) {
          raw <- raw %>% mutate(
            pos = case_when(
              toupper(pos) %in% c("GK", "GKP", "GOALKEEPER") ~ "GK",
              toupper(pos) %in% c("DEF", "DEFENDER", "D")    ~ "DEF",
              toupper(pos) %in% c("MID", "MIDFIELDER", "M")  ~ "MID",
              toupper(pos) %in% c("FWD", "FOR", "FORWARD", "F", "ATT", "ST", "STRIKER") ~ "FWD",
              TRUE ~ toupper(pos)
            )
          )
        }
        
        available <- intersect(
          c("player", "pos", "team", "price", "own_pct", "captain_pct",
            "form", "score", "gameweek"),
          names(raw)
        )
        raw %>% select(all_of(available))
        
      }, error = function(e) {
        log_debug(sprintf("  Error loading %s: %s", sn, e$message), level = "WARN")
        NULL
      })
    })
    
    result <- bind_rows(Filter(Negate(is.null), all_data))
    log_debug(sprintf("Combined ownership: %d rows, %d weeks",
                      nrow(result), length(unique(result$gameweek))), level = "INFO")
    log_debug("========================================", level = "INFO")
    return(result)
    
  }, error = function(e) {
    log_debug("Error in load_ownership_history():", e$message, level = "ERROR")
    NULL
  })
}


# =============================================================================
# MATCHUP QUALITY
# =============================================================================

#' Calculate position-specific matchup quality (0-100) per player
#'
#' What matters by position (driven by the scoring rules):
#'   GK:  CS probability (4pts) + save volume (0.5/save) - goals conceded
#'   DEF: CS probability (4pts) + team attack for set-piece goals (6pts) - conceded
#'   MID: Team implied goals (5pts/goal) + SOT (0.4/SOT) + small CS (1pt)
#'   FWD: Team implied goals (4pts/goal) + SOT (0.4/SOT)
#'
#' @param players Data frame with at least: pos, team_normalized
#' @param matchup_context Data frame per team: team_normalized, implied_goals,
#'   implied_opp_goals, cs_prob
#' @return Input data with matchup_quality column added
calculate_matchup_quality <- function(players, matchup_context) {
  if (is.null(matchup_context) || nrow(matchup_context) == 0) {
    return(players %>% mutate(matchup_quality = 50))
  }
  
  # Baselines (EPL season averages for calibration)
  avg_goals <- 1.35
  avg_cs    <- 28
  
  team_mq <- matchup_context %>%
    mutate(
      # 0-100 scales centered on EPL average
      atk_score  = pmin(100, pmax(0, 50 + (implied_goals - avg_goals) * 30)),
      cs_score   = pmin(100, pmax(0, 50 + (cs_prob - avg_cs) * 1.8)),
      opp_score  = pmin(100, pmax(0, 50 + (implied_opp_goals - avg_goals) * 25)),
      # Save opportunity: opponent attacking volume
      save_opp   = pmin(100, pmax(0, 50 + (implied_opp_goals - avg_goals) * 20)),
      # Concede penalty: how badly does conceding hurt?
      concede_risk = pmin(100, pmax(0, 50 - (implied_opp_goals - avg_goals) * 25))
    ) %>%
    select(team_normalized, atk_score, cs_score, opp_score, save_opp, concede_risk)
  
  if (!"team_normalized" %in% names(players)) {
    return(players %>% mutate(matchup_quality = 50))
  }
  
  players %>%
    left_join(team_mq, by = "team_normalized") %>%
    mutate(
      matchup_quality = case_when(
        # GK: CS + saves volume - concede risk
        pos == "GK"  ~ 0.40 * coalesce(cs_score, 50) +
          0.30 * coalesce(save_opp, 50) +
          0.30 * coalesce(concede_risk, 50),
        # DEF: CS dominant + some attack upside (set pieces)
        pos == "DEF" ~ 0.55 * coalesce(cs_score, 50) +
          0.15 * coalesce(atk_score, 50) +
          0.30 * coalesce(concede_risk, 50),
        # MID: Attack dominant + small CS
        pos == "MID" ~ 0.65 * coalesce(atk_score, 50) +
          0.10 * coalesce(cs_score, 50) +
          0.25 * coalesce(opp_score, 50),
        # FWD: Pure attack
        pos == "FWD" ~ 0.80 * coalesce(atk_score, 50) +
          0.20 * coalesce(opp_score, 50),
        TRUE ~ 50
      ),
      matchup_quality = round(pmin(100, pmax(0, matchup_quality)), 1)
    ) %>%
    select(-any_of(c("atk_score", "cs_score", "opp_score", "save_opp", "concede_risk")))
}


# =============================================================================
# OWNERSHIP ESTIMATION
# =============================================================================

#' Estimate ownership for a slate of players
#'
#' Builds a weekly APPEAL SCORE from matchup quality, recent form, salary
#' value, salary tier, and picked_by (as a weak recognition prior only).
#' Distributes ownership within hard position budgets using exponential
#' concentration, producing realistic DFS-like distributions.
#'
#' Key insight: `picked_by` is a SEASON-LONG stat that barely moves week to
#' week. DFS contest ownership is driven by THIS WEEK's fixture, form, and
#' value. The appeal-score approach weights weekly signals 70% vs structural
#' signals 30%.
#'
#' When historical ownership data is available, the exponential steepness
#' is calibrated per position to match observed concentration patterns.
#'
#' @param players Data frame with: player, pos, salary.
#'   Optional: form, matchup_quality, picked_by, ppg, pts_ceiling
#' @param historical Output of load_ownership_history() for calibration, or NULL
#' @return Input df with added: est_own_pct, est_captain_pct, own_bucket
estimate_ownership <- function(players, historical = NULL) {
  log_debug("estimate_ownership():", nrow(players), "players", level = "INFO")
  
  if (nrow(players) == 0 || !"salary" %in% names(players)) {
    return(players %>% mutate(est_own_pct = 1, est_captain_pct = 0, own_bucket = "Unknown"))
  }
  
  # ---- Position budgets ----
  # GK=1 slot, DEF=4, MID=3, FWD=3 in an 11-player lineup
  pos_budgets <- c(GK = 100, DEF = 400, MID = 300, FWD = 300)
  
  # ---- Parse inputs ----
  result <- players %>%
    mutate(
      pb_num   = as.numeric(gsub("[^0-9.]", "", as.character(picked_by))),
      pb_num   = coalesce(pb_num, 0),
      mq_safe  = coalesce(matchup_quality, 50),
      form_num = coalesce(as.numeric(form), 0),
      ppg_safe = coalesce(ppg, 0),
      value    = if_else(salary > 0, ppg_safe / salary, 0)
    )
  
  has_pb <- sum(result$pb_num > 0, na.rm = TRUE) > 10
  
  log_debug(sprintf("  picked_by: %d non-zero values, max=%.1f, median=%.1f",
                    sum(result$pb_num > 0, na.rm = TRUE),
                    max(c(result$pb_num, 0), na.rm = TRUE),
                    median(c(result$pb_num[result$pb_num > 0], 0), na.rm = TRUE)),
            level = "INFO")
  
  # ---- Calibrate exponential steepness from historical data ----
  # Default k=6 gives roughly: top player ~35-45%, #2-3 ~15-25%, mid ~2-6%
  # If we have actual ownership data, find k that best matches observed concentration
  pos_k <- c(GK = 6, DEF = 6, MID = 6, FWD = 6)  # defaults
  
  if (!is.null(historical) && nrow(historical) > 0) {
    hist_valid <- historical %>% filter(!is.na(own_pct), own_pct > 0)
    
    if (nrow(hist_valid) > 50) {
      log_debug("Calibrating steepness from", nrow(hist_valid), "historical observations", level = "INFO")
      
      # For each position, measure how concentrated real ownership is
      # Use the ratio of max_ownership / median_ownership as a concentration metric
      for (p in c("GK", "DEF", "MID", "FWD")) {
        pos_hist <- hist_valid %>%
          filter(pos == p | (p == "FWD" & pos %in% c("FOR", "FWD", "ST")))
        
        if (nrow(pos_hist) < 10) next
        
        # Concentration per gameweek: top player share of position total
        gw_conc <- pos_hist %>%
          group_by(gw) %>%
          summarise(
            top_share = max(own_pct, na.rm = TRUE) / sum(own_pct, na.rm = TRUE),
            max_own   = max(own_pct, na.rm = TRUE),
            n         = n(),
            .groups   = "drop"
          )
        
        if (nrow(gw_conc) < 2) next
        
        # Target: match the average top-player share
        avg_top_share <- mean(gw_conc$top_share, na.rm = TRUE)
        avg_n <- round(mean(gw_conc$n, na.rm = TRUE))
        
        # Search for k that produces this concentration
        # For n players with uniform appeal percentiles, top share ≈ exp(k) / sum(exp(i/n * k))
        best_k <- 6
        best_err <- Inf
        for (test_k in seq(3, 12, by = 0.5)) {
          pctls <- seq(0, 1, length.out = max(avg_n, 5))
          shares <- exp(pctls * test_k)
          sim_top <- max(shares) / sum(shares)
          err <- abs(sim_top - avg_top_share)
          if (err < best_err) {
            best_err <- err
            best_k <- test_k
          }
        }
        
        pos_k[p] <- best_k
        log_debug(sprintf("  %s: calibrated k=%.1f (target top_share=%.1f%%, n=%d GWs)",
                          p, best_k, avg_top_share * 100, nrow(gw_conc)),
                  level = "INFO")
      }
    }
  }
  
  # ---- Compute appeal score (position percentiles) ----
  # Weekly signals (70%): matchup + form + value
  # Structural signals (30%): salary tier + picked_by recognition
  result <- result %>%
    group_by(pos) %>%
    mutate(
      mq_pctl     = percent_rank(mq_safe),       # fixture attractiveness
      form_pctl   = percent_rank(form_num),       # recent hot streak
      value_pctl  = percent_rank(value),          # PPG per million
      salary_pctl = percent_rank(salary),         # premium name recognition
      pb_pctl     = if_else(rep(has_pb, n()),
                            percent_rank(pb_num),
                            0.5),                 # season-long quality prior
      
      # Weighted appeal — weekly signals dominate
      appeal = 0.30 * mq_pctl +     # This week's fixture (biggest DFS driver)
        0.25 * form_pctl +    # Recent scoring form
        0.15 * value_pctl +   # Points-per-million efficiency
        0.15 * salary_pctl +  # Higher salary = more recognition
        0.15 * pb_pctl,       # Season-long quality prior (weak)
      
      # Exponential concentration with position-calibrated steepness
      k = pos_k[first(pos)],
      appeal_exp = exp(appeal * k),
      budget = pos_budgets[first(pos)],
      share = appeal_exp / sum(appeal_exp, na.rm = TRUE),
      est_own_pct = round(pmin(65, pmax(0.1, share * budget)), 1)
    ) %>%
    ungroup()
  
  # ---- Historical level calibration ----
  # Gently blend with historical averages by pos×salary_bucket to correct levels
  # 80% model (preserves weekly differentiation) + 20% historical avg (corrects levels)
  if (!is.null(historical) && nrow(historical) > 0) {
    hist_bench <- tryCatch({
      historical %>%
        filter(!is.na(own_pct), own_pct > 0) %>%
        mutate(
          sal_bucket = cut(price, breaks = c(0, 6, 8, 10, 12, 15, Inf),
                           labels = c("budget", "low", "mid", "mid_high", "premium", "elite"))
        ) %>%
        group_by(pos, sal_bucket) %>%
        summarise(avg_own = mean(own_pct, na.rm = TRUE), .groups = "drop")
    }, error = function(e) NULL)
    
    if (!is.null(hist_bench) && nrow(hist_bench) > 0) {
      log_debug("Level-calibrating against", nrow(hist_bench), "historical buckets", level = "INFO")
      result <- result %>%
        mutate(sal_bucket = cut(salary, breaks = c(0, 6, 8, 10, 12, 15, Inf),
                                labels = c("budget", "low", "mid", "mid_high", "premium", "elite"))) %>%
        left_join(hist_bench, by = c("pos", "sal_bucket")) %>%
        mutate(
          est_own_pct = if_else(!is.na(avg_own),
                                round(0.80 * est_own_pct + 0.20 * avg_own, 1),
                                est_own_pct)
        ) %>%
        select(-sal_bucket, -avg_own)
    }
  }
  
  # ---- Captain ownership ----
  # 1 captain slot = 100% total. Very concentrated on high-ceiling MID/FWD.
  result <- result %>%
    mutate(
      ceil_safe = coalesce(as.numeric(pts_ceiling), 0),
      cpt_pos_mult = case_when(
        pos == "GK"  ~ 0.02,
        pos == "DEF" ~ 0.08,
        pos == "MID" ~ 1.00,
        pos == "FWD" ~ 1.20,
        TRUE ~ 0.05
      ),
      # Captain appeal: combine ownership + ceiling + position suitability
      cpt_raw = (est_own_pct / 100) * cpt_pos_mult *
        (1 + coalesce(percent_rank(ceil_safe), 0.5)),
      cpt_exp = exp(cpt_raw * 6),
      cpt_total = sum(cpt_exp, na.rm = TRUE),
      est_captain_pct = round(pmin(40, pmax(0, (cpt_exp / cpt_total) * 100)), 1)
    )
  
  # ---- Assign ownership buckets ----
  result <- result %>%
    mutate(
      own_bucket = case_when(
        est_own_pct >= 20 ~ "Chalk",
        est_own_pct >= 8  ~ "Popular",
        est_own_pct >= 2  ~ "Moderate",
        est_own_pct >= 0.5 ~ "Low",
        TRUE ~ "Ghost"
      )
    ) %>%
    select(-any_of(c(
      "pb_num", "mq_safe", "form_num", "ppg_safe", "value",
      "mq_pctl", "form_pctl", "value_pctl", "salary_pctl", "pb_pctl",
      "appeal", "k", "appeal_exp", "budget", "share",
      "ceil_safe", "cpt_pos_mult", "cpt_raw", "cpt_exp", "cpt_total"
    )))
  
  # ---- Diagnostic logging ----
  total_own <- sum(result$est_own_pct, na.rm = TRUE)
  log_debug(sprintf("Ownership total: %.0f%% across %d players", total_own, nrow(result)), level = "INFO")
  
  for (p in c("GK", "DEF", "MID", "FWD")) {
    pos_data <- result %>% filter(pos == p)
    if (nrow(pos_data) == 0) next
    top3 <- pos_data %>% arrange(desc(est_own_pct)) %>% head(3)
    log_debug(sprintf("  %s: n=%d, total=%.0f%%, top3: %s",
                      p, nrow(pos_data),
                      sum(pos_data$est_own_pct, na.rm = TRUE),
                      paste(sprintf("%s=%.1f%%", top3$player, top3$est_own_pct), collapse = ", ")),
              level = "INFO")
  }
  
  log_debug(sprintf("Ownership buckets: Chalk=%d, Popular=%d, Moderate=%d, Low=%d, Ghost=%d",
                    sum(result$own_bucket == "Chalk", na.rm = TRUE),
                    sum(result$own_bucket == "Popular", na.rm = TRUE),
                    sum(result$own_bucket == "Moderate", na.rm = TRUE),
                    sum(result$own_bucket == "Low", na.rm = TRUE),
                    sum(result$own_bucket == "Ghost", na.rm = TRUE)), level = "INFO")
  return(result)
}


# =============================================================================
# PLAYER RATINGS
# =============================================================================

#' Rate all players for Cash, GPP, and Captain
#'
#' Combines historical performance, matchup quality, salary value, form,
#' and ownership leverage into three composite scores, mapped to word grades.
#'
#' @param players Data frame with columns from salary loader + ownership + matchup.
#'   Required: player, pos, salary, est_own_pct, matchup_quality
#'   Optional: ppg, sortino, pts_floor, pts_ceiling, form, n_games, est_captain_pct
#' @return Input df with: cash_score, cash_rating, cash_color, cash_bg,
#'   gpp_score, gpp_rating, ..., captain_score, captain_rating, ...
rate_players <- function(players) {
  log_debug("rate_players():", nrow(players), "players", level = "INFO")
  
  # Ensure necessary columns exist with safe defaults
  result <- players %>%
    mutate(
      ppg_val     = coalesce(as.numeric(ppg), 0),
      sortino_val = coalesce(as.numeric(sortino), 0),
      floor_val   = coalesce(as.numeric(pts_floor), 0),
      ceil_val    = coalesce(as.numeric(pts_ceiling), 0),
      form_val    = coalesce(as.numeric(form), ppg_val),
      n_games_val = coalesce(as.numeric(n_games), 0),
      mq_val      = coalesce(matchup_quality, 50),
      own_val     = coalesce(est_own_pct, 5),
      cpt_own_val = coalesce(est_captain_pct, 0),
      # Confidence: shrink grades toward 50 for thin sample sizes
      conf        = pmin(1, n_games_val / 10)
    )
  
  # Position-relative percentiles
  result <- result %>%
    group_by(pos) %>%
    mutate(
      p_ppg     = percent_rank(ppg_val) * 100,
      p_sortino = percent_rank(sortino_val) * 100,
      p_floor   = percent_rank(floor_val) * 100,
      p_ceiling = percent_rank(ceil_val) * 100,
      p_form    = percent_rank(form_val) * 100,
      p_matchup = percent_rank(mq_val) * 100,
      p_value   = percent_rank(ppg_val / pmax(salary, 3)) * 100
    ) %>%
    ungroup()
  
  # ---- CASH SCORE ----
  # Cash = floor reliability + matchup quality + salary value + form
  # Position-aware: GK/DEF matchup-heavy (CS worth 4pts), MID/FWD form-driven
  result <- result %>%
    mutate(
      cash_raw = case_when(
        pos == "GK"  ~ 0.25 * p_floor + 0.15 * p_sortino + 0.30 * p_matchup + 0.15 * p_value + 0.15 * p_form,
        pos == "DEF" ~ 0.25 * p_floor + 0.15 * p_sortino + 0.30 * p_matchup + 0.15 * p_value + 0.15 * p_form,
        pos == "MID" ~ 0.25 * p_floor + 0.15 * p_sortino + 0.25 * p_matchup + 0.15 * p_value + 0.20 * p_form,
        pos == "FWD" ~ 0.20 * p_floor + 0.15 * p_sortino + 0.20 * p_matchup + 0.20 * p_value + 0.25 * p_form,
        TRUE         ~ 0.25 * p_floor + 0.15 * p_sortino + 0.25 * p_matchup + 0.15 * p_value + 0.20 * p_form
      ),
      cash_score = round(pmin(100, pmax(0, conf * cash_raw + (1 - conf) * 50)), 1)
    )
  
  # ---- GPP SCORE ----
  # GPP = ceiling + leverage (good projection but low owned) + matchup + form
  # Leverage is THE key GPP concept: high projection rank vs low ownership rank
  result <- result %>%
    group_by(pos) %>%
    mutate(
      p_own = percent_rank(own_val) * 100,
      # High quality + low owned = positive leverage (Leverage Gold)
      # Low quality + high owned = negative leverage (Chalk Trap)
      leverage_raw = (0.50 * p_ceiling + 0.50 * p_form) - p_own,
      p_leverage = percent_rank(leverage_raw) * 100
    ) %>%
    ungroup() %>%
    mutate(
      gpp_raw = 0.25 * p_ceiling + 0.30 * p_leverage + 0.25 * p_matchup + 0.20 * p_form,
      gpp_score = round(pmin(100, pmax(0, conf * gpp_raw + (1 - conf) * 50)), 1)
    )
  
  # ---- CAPTAIN SCORE ----
  # Captain = ceiling (2x magnifies upside) + captain pool leverage + matchup
  # GKs capped upside (CS max 8pts before 2x). Best captains: high-ceiling MID/FWD
  result <- result %>%
    group_by(pos) %>%
    mutate(
      p_cpt_own = percent_rank(cpt_own_val) * 100,
      cpt_leverage_raw = (0.60 * p_ceiling + 0.40 * p_form) - p_cpt_own,
      p_cpt_leverage = percent_rank(cpt_leverage_raw) * 100
    ) %>%
    ungroup() %>%
    mutate(
      # Position modifier: GKs penalized, DEFs moderate, MID/FWD natural captains
      pos_mod = case_when(
        pos == "GK"  ~ -20,
        pos == "DEF" ~ -5,
        pos == "MID" ~ 5,
        pos == "FWD" ~ 8,
        TRUE ~ 0
      ),
      cpt_raw = 0.35 * p_ceiling + 0.30 * p_cpt_leverage + 0.20 * p_matchup + 0.15 * p_form + pos_mod,
      captain_score = round(pmin(100, pmax(0, conf * cpt_raw + (1 - conf) * 35)), 1)
    )
  
  # ---- MAP TO LABELS + COLORS ----
  result <- result %>%
    mutate(
      cash_rating   = map_score_to_label(cash_score, "cash"),
      cash_color    = map_score_to_color(cash_score),
      cash_bg       = map_score_to_bg(cash_score),
      gpp_rating    = map_score_to_label(gpp_score, "gpp"),
      gpp_color     = map_score_to_color(gpp_score),
      gpp_bg        = map_score_to_bg(gpp_score),
      captain_rating = map_score_to_label(captain_score, "captain"),
      captain_color  = map_score_to_color(captain_score),
      captain_bg     = map_score_to_bg(captain_score)
    )
  
  # Clean working columns
  result <- result %>%
    select(-any_of(c(
      "ppg_val", "sortino_val", "floor_val", "ceil_val", "form_val",
      "n_games_val", "mq_val", "own_val", "cpt_own_val", "conf",
      "p_ppg", "p_sortino", "p_floor", "p_ceiling", "p_form",
      "p_matchup", "p_value", "p_own", "p_cpt_own",
      "leverage_raw", "p_leverage", "cpt_leverage_raw", "p_cpt_leverage",
      "pos_mod", "cash_raw", "gpp_raw", "cpt_raw"
    )))
  
  # Log distribution
  for (type in c("cash_rating", "gpp_rating", "captain_rating")) {
    dist <- table(result[[type]])
    log_debug(sprintf("  %s: %s", type,
                      paste(names(dist), dist, sep = "=", collapse = ", ")), level = "DEBUG")
  }
  
  return(result)
}


# =============================================================================
# LABEL / COLOR MAPPING HELPERS
# =============================================================================

#' Map score to word label
#' @param score Numeric vector (0-100)
#' @param type "cash", "gpp", or "captain"
map_score_to_label <- function(score, type = "cash") {
  sapply(score, function(s) {
    if (is.na(s)) return(NA_character_)
    for (tier in RATING_TIERS) {
      if (s >= tier$min_score) return(tier[[type]])
    }
    RATING_TIERS$avoid[[type]]
  })
}

#' Map score to hex color
map_score_to_color <- function(score) {
  sapply(score, function(s) {
    if (is.na(s)) return("#7A7A7A")
    for (tier in RATING_TIERS) {
      if (s >= tier$min_score) return(tier$color)
    }
    RATING_TIERS$avoid$color
  })
}

#' Map score to hex background color
map_score_to_bg <- function(score) {
  sapply(score, function(s) {
    if (is.na(s)) return("#F5F5F5")
    for (tier in RATING_TIERS) {
      if (s >= tier$min_score) return(tier$bg)
    }
    RATING_TIERS$avoid$bg
  })
}

#' Get emoji for a rating type and score
get_rating_emoji <- function(score, type = "cash") {
  emoji_key <- paste0("emoji_", switch(type, cash = "cash", gpp = "gpp", captain = "cpt", "cash"))
  sapply(score, function(s) {
    if (is.na(s)) return("")
    for (tier in RATING_TIERS) {
      if (s >= tier$min_score) return(tier[[emoji_key]])
    }
    RATING_TIERS$avoid[[emoji_key]]
  })
}


# =============================================================================
# RATING GUIDE HTML (for UI display in handbuild module)
# =============================================================================

#' Generate the rating methodology guide as HTML
#' @return shiny tagList suitable for rendering inside a ui_card
generate_rating_guide_html <- function() {
  
  tier_rows <- lapply(RATING_TIERS, function(tier) {
    tags$tr(
      style = sprintf("background: %s;", tier$bg),
      tags$td(style = "padding: 0.35rem 0.5rem; text-align: center; font-weight: 600; font-size: 0.75rem; color: var(--text-muted);",
              sprintf("%d+", tier$min_score)),
      tags$td(style = sprintf("padding: 0.35rem 0.5rem; font-weight: 700; font-size: 0.8rem; color: %s; white-space: nowrap;", tier$color),
              paste(tier$emoji_cash, tier$cash)),
      tags$td(style = sprintf("padding: 0.35rem 0.5rem; font-weight: 700; font-size: 0.8rem; color: %s; white-space: nowrap;", tier$color),
              paste(tier$emoji_gpp, tier$gpp)),
      tags$td(style = sprintf("padding: 0.35rem 0.5rem; font-weight: 700; font-size: 0.8rem; color: %s; white-space: nowrap;", tier$color),
              paste(tier$emoji_cpt, tier$captain))
    )
  })
  
  tagList(
    # Grade table
    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 2px solid var(--outline); border-radius: 6px; overflow: hidden;",
      tags$thead(
        tags$tr(
          style = "background: var(--text-primary); color: white;",
          tags$th(style = "padding: 0.4rem 0.5rem; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.5px;", "Score"),
          tags$th(style = "padding: 0.4rem 0.5rem; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.5px;", "Cash"),
          tags$th(style = "padding: 0.4rem 0.5rem; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.5px;", "GPP"),
          tags$th(style = "padding: 0.4rem 0.5rem; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.5px;", "Captain")
        )
      ),
      tags$tbody(tier_rows)
    ),
    
    # Methodology explanation
    tags$div(
      style = "margin-top: 0.75rem; padding: 0.6rem; background: var(--bg-secondary); border-radius: 6px;",
      tags$p(
        style = "font-size: 0.8rem; color: var(--text-secondary); margin: 0 0 0.5rem 0; line-height: 1.5;",
        tags$strong("How ratings work:"),
        " Each player gets a composite score (0-100) for Cash, GPP, and Captain suitability. ",
        "Scores combine historical performance (floor, ceiling, Sortino ratio), ",
        "this week's matchup quality (odds-derived), salary value, current form, ",
        "and estimated ownership (for GPP leverage). All grades are ", tags$strong("position-relative"),
        ": a 'Cash Lock' GK is elite among GKs."
      ),
      tags$p(
        style = "font-size: 0.75rem; color: var(--text-muted); margin: 0 0 0.3rem 0;",
        tags$strong("Cash:"), " Floor + Sortino (reliability) + matchup quality + salary value. ",
        "GK/DEF matchup-heavy (CS is worth 4pts). MID/FWD more form-driven."
      ),
      tags$p(
        style = "font-size: 0.75rem; color: var(--text-muted); margin: 0 0 0.3rem 0;",
        tags$strong("GPP:"), " Ceiling upside + leverage (good projection but low-owned) + matchup. ",
        "Rewards contrarian picks with genuine ceiling. Penalises chalk traps."
      ),
      tags$p(
        style = "font-size: 0.75rem; color: var(--text-muted); margin: 0 0 0.3rem 0;",
        tags$strong("Captain (2x):"), " Ceiling (magnified by 2x multiplier) + captain-pool leverage + matchup. ",
        "GKs heavily penalised (capped upside). MID/FWD natural captain picks."
      ),
      tags$p(
        style = "font-size: 0.75rem; color: var(--text-muted); margin: 0; font-style: italic;",
        "Ownership estimates calibrated against GW20+ actuals. Target accuracy: \u00B110pp. ",
        "Positive/Negative Impact scoring (\u00B10.3) excluded from analysis."
      )
    )
  )
}