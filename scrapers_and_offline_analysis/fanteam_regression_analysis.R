################################################################################
# FANTEAM FANTASY OPPORTUNITY SCORE - EMPIRICAL WEIGHT CALIBRATION
# 
# Purpose: Analyze historical FanTeam data to validate/calibrate position weights
# 
# Run frequency: End of season, or mid-season checkpoint
# Data required: 
#   - gameweek_detail sheet (from scrape_notonlyfpl.R)
#   - match_odds sheet (from scrape_football_odds.R)
#
# Current production weights (as of Jan 2026):
#   GK:  CS=0.80, GF=0.20
#   DEF: CS=0.50, GF=0.50
#   MID: CS=0.25, GF=0.75
#   FWD: CS=0.10, GF=0.90
#
# INTERPRETATION NOTES:
#   - R² will be LOW (3-10%) - this is expected! Odds explain team opportunity,
#     not individual player performance
#   - Need p-value < 0.05 for coefficients to be meaningful
#   - Minimum ~500 observations per position for reliable estimates
#   - Best run with 2+ full seasons of data
#
# History:
#   - Jan 2026: Initial analysis with 16 GWs. CS% coefficients not significant.
#               Implied GF significant for DEF/MID/FWD. Kept theoretical weights
#               with modest adjustment toward goals.
################################################################################

library(tidyverse)
library(googlesheets4)

# =============================================================================
# CONFIGURATION
# =============================================================================

SHEET_ID <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"

# Current production weights (update these when you change the app)
CURRENT_WEIGHTS <- list(
  GK  = c(cs = 0.80, gf = 0.20),
  DEF = c(cs = 0.50, gf = 0.50),
  MID = c(cs = 0.25, gf = 0.75),
  FWD = c(cs = 0.10, gf = 0.90)
)

# Minimum sample size per position for reliable regression
MIN_SAMPLE_SIZE <- 300

# =============================================================================
# MAIN ANALYSIS FUNCTION
# =============================================================================

run_calibration_analysis <- function(sheet_id = SHEET_ID, save_results = TRUE) {
  
  message("================================================================================")
  message("    FANTEAM OPPORTUNITY SCORE - EMPIRICAL WEIGHT CALIBRATION")
  message(sprintf("    Run date: %s", Sys.Date()))
  message("================================================================================\n")
  
  # =============================================================================
  # LOAD DATA
  # =============================================================================
  
  message("=== Loading Data ===\n")
  
  gs4_auth(email = "Heerymichael@hotmail.com")
  
  gw_detail <- read_sheet(sheet_id, sheet = "gameweek_detail")
  message(sprintf("  gameweek_detail: %d rows", nrow(gw_detail)))
  
  match_odds <- read_sheet(sheet_id, sheet = "match_odds")
  message(sprintf("  match_odds: %d rows", nrow(match_odds)))
  
  # =============================================================================
  # DATA CLEANING
  # =============================================================================
  
  message("\n=== Cleaning Data ===\n")
  
  # Clean gameweek_detail - convert character columns to numeric
  gw_clean <- gw_detail %>%
    mutate(
      # Convert numeric columns
      total_pts = as.numeric(Total.Pts),
      goals = as.numeric(Goals),
      assists = as.numeric(Assists),
      cs = as.numeric(CS),
      saves = as.numeric(Saves),
      goals_conceded = as.numeric(GC),
      mins_played = as.numeric(Mins.Played),
      price = as.numeric(Price....),
      
      # Standardize position names
      position = case_when(
        Pos == "FOR" ~ "FWD",
        TRUE ~ Pos
      ),
      
      team_clean = str_trim(Team),
      gameweek = as.integer(gameweek)
    ) %>%
    filter(!is.na(total_pts), mins_played > 0) %>%
    select(
      player_id = ID,
      player_name = Name,
      team = team_clean,
      position,
      price,
      total_pts,
      goals,
      assists,
      cs,
      saves,
      goals_conceded,
      mins_played,
      gameweek
    )
  
  message(sprintf("  Cleaned gw_detail: %d rows (filtered mins > 0)", nrow(gw_clean)))
  
  # Clean match_odds
  odds_clean <- match_odds %>%
    mutate(
      team_clean = str_trim(team),
      match_date = as.Date(match_date),
      implied_gf = as.numeric(implied_goals_for),
      implied_ga = as.numeric(implied_goals_against),
      cs_pct = as.numeric(clean_sheet_pct),
      cs_actual = as.logical(clean_sheet_actual),
      goals_actual = as.numeric(goals_scored_actual)
    ) %>%
    filter(!is.na(implied_gf), !is.na(cs_pct)) %>%
    select(
      match_date,
      team = team_clean,
      opponent,
      venue,
      implied_gf,
      implied_ga,
      cs_pct,
      goals_actual,
      cs_actual
    )
  
  message(sprintf("  Cleaned match_odds: %d rows", nrow(odds_clean)))
  
  # =============================================================================
  # MAP GAMEWEEKS TO DATES
  # =============================================================================
  
  message("\n=== Mapping Gameweeks to Dates ===\n")
  
  unique_dates <- sort(unique(odds_clean$match_date))
  
  # Group dates into gameweeks (dates close together = same GW)
  date_to_gw <- tibble(match_date = unique_dates) %>%
    arrange(match_date) %>%
    mutate(
      date_gap = c(0, diff(match_date)),
      new_gw = date_gap > 3 | row_number() == 1,
      gameweek = cumsum(new_gw)
    ) %>%
    select(match_date, gameweek)
  
  gw_summary <- date_to_gw %>% 
    group_by(gameweek) %>% 
    summarise(from = min(match_date), to = max(match_date), .groups = "drop")
  
  message(sprintf("  Mapped %d gameweeks from %s to %s", 
                  nrow(gw_summary), 
                  min(gw_summary$from), 
                  max(gw_summary$to)))
  
  # Add gameweek to odds data
  odds_with_gw <- odds_clean %>%
    left_join(date_to_gw, by = "match_date")
  
  # =============================================================================
  # TEAM NAME RECONCILIATION (will need updates as teams change)
  # =============================================================================
  
  message("\n=== Team Name Reconciliation ===\n")
  
  gw_teams <- sort(unique(gw_clean$team))
  odds_teams <- sort(unique(odds_with_gw$team))
  
  # Check if teams already match
  if (setequal(gw_teams, odds_teams)) {
    message("✓ All teams matched automatically!")
    gw_clean$team_std <- gw_clean$team
    odds_with_gw$team_std <- odds_with_gw$team
  } else {
    # Show mismatches for manual resolution
    missing_in_odds <- setdiff(gw_teams, odds_teams)
    missing_in_gw <- setdiff(odds_teams, gw_teams)
    
    if (length(missing_in_odds) > 0) {
      message("⚠️  Teams in gw_detail but not in odds:")
      print(missing_in_odds)
    }
    
    if (length(missing_in_gw) > 0) {
      message("⚠️  Teams in odds but not in gw_detail:")
      print(missing_in_gw)
    }
    
    message("\n  Add mappings to team_mapping if needed")
    
    # Basic pass-through for now
    gw_clean$team_std <- gw_clean$team
    odds_with_gw$team_std <- odds_with_gw$team
  }
  
  # =============================================================================
  # JOIN PLAYER STATS WITH MATCH ODDS
  # =============================================================================
  
  message("\n=== Joining Player Stats with Match Odds ===\n")
  
  player_with_odds <- gw_clean %>%
    left_join(
      odds_with_gw %>% 
        select(team_std, gameweek, implied_gf, implied_ga, cs_pct, cs_actual, goals_actual) %>%
        distinct(),  # Avoid many-to-many
      by = c("team_std", "gameweek"),
      relationship = "many-to-one"
    )
  
  joined_count <- sum(!is.na(player_with_odds$implied_gf))
  message(sprintf("Successfully joined: %d / %d rows (%.1f%%)", 
                  joined_count, nrow(player_with_odds),
                  100 * joined_count / nrow(player_with_odds)))
  
  analysis_data <- player_with_odds %>%
    filter(!is.na(implied_gf), !is.na(cs_pct))
  
  message(sprintf("\nAnalysis dataset: %d rows", nrow(analysis_data)))
  message("\nRows by position:")
  print(table(analysis_data$position))
  
  # =============================================================================
  # DATA QUALITY CHECK
  # =============================================================================
  
  message("\n=== Data Quality Check ===\n")
  
  n_gameweeks <- length(unique(analysis_data$gameweek))
  n_teams <- length(unique(analysis_data$team_std))
  
  message(sprintf("  Gameweeks: %d", n_gameweeks))
  message(sprintf("  Teams: %d", n_teams))
  message(sprintf("  Total player-GWs: %d", nrow(analysis_data)))
  
  pos_counts <- table(analysis_data$position)
  for (pos in names(pos_counts)) {
    status <- if (pos_counts[pos] >= MIN_SAMPLE_SIZE) "✓" else "⚠️ LOW"
    message(sprintf("  %s: %d %s", pos, pos_counts[pos], status))
  }
  
  if (n_gameweeks < 20) {
    message("\n⚠️  WARNING: Less than 20 gameweeks - results will be noisy")
    message("   Recommend re-running at end of season for more reliable estimates")
  }
  
  # =============================================================================
  # REGRESSION ANALYSIS BY POSITION
  # =============================================================================
  
  message("\n================================================================================")
  message("    REGRESSION ANALYSIS: Points ~ CS% + Implied Goals")
  message("================================================================================\n")
  
  # Normalize predictors for comparable coefficients
  analysis_data <- analysis_data %>%
    mutate(
      cs_pct_scaled = cs_pct / 100,
      implied_gf_scaled = (implied_gf - min(implied_gf)) / (max(implied_gf) - min(implied_gf))
    )
  
  positions <- c("GK", "DEF", "MID", "FWD")
  results <- list()
  
  for (pos in positions) {
    message(sprintf("\n--- %s ---", pos))
    
    pos_data <- analysis_data %>% filter(position == pos)
    n_obs <- nrow(pos_data)
    message(sprintf("  N = %d player-gameweeks", n_obs))
    
    if (n_obs < 50) {
      message("  ⚠️  Insufficient data for regression")
      next
    }
    
    # Basic stats
    message(sprintf("  Avg points: %.2f", mean(pos_data$total_pts, na.rm = TRUE)))
    message(sprintf("  Avg implied GF: %.2f", mean(pos_data$implied_gf, na.rm = TRUE)))
    message(sprintf("  Avg CS%%: %.1f%%", mean(pos_data$cs_pct, na.rm = TRUE)))
    
    # Run regression
    model <- lm(total_pts ~ cs_pct_scaled + implied_gf_scaled, data = pos_data)
    model_summary <- summary(model)
    
    message("\n  Regression coefficients:")
    print(model_summary$coefficients)
    
    # Extract coefficients and p-values
    coef_cs <- coef(model)["cs_pct_scaled"]
    coef_gf <- coef(model)["implied_gf_scaled"]
    pval_cs <- model_summary$coefficients["cs_pct_scaled", "Pr(>|t|)"]
    pval_gf <- model_summary$coefficients["implied_gf_scaled", "Pr(>|t|)"]
    
    # Calculate relative weights
    total <- abs(coef_cs) + abs(coef_gf)
    weight_cs <- abs(coef_cs) / total
    weight_gf <- abs(coef_gf) / total
    
    # Flag significance
    sig_cs <- if (pval_cs < 0.05) "✓" else ""
    sig_gf <- if (pval_gf < 0.05) "✓" else ""
    
    message(sprintf("\n  Empirical weights: CS=%.2f %s, GF=%.2f %s", 
                    weight_cs, sig_cs, weight_gf, sig_gf))
    
    r_squared <- model_summary$r.squared
    message(sprintf("  R² = %.3f", r_squared))
    
    # Note if sample is low
    if (n_obs < MIN_SAMPLE_SIZE) {
      message(sprintf("  ⚠️  Sample below %d - interpret with caution", MIN_SAMPLE_SIZE))
    }
    
    results[[pos]] <- list(
      n = n_obs,
      model = model,
      coef_cs = coef_cs,
      coef_gf = coef_gf,
      pval_cs = pval_cs,
      pval_gf = pval_gf,
      weight_cs = weight_cs,
      weight_gf = weight_gf,
      r_squared = r_squared,
      significant_cs = pval_cs < 0.05,
      significant_gf = pval_gf < 0.05
    )
  }
  
  # =============================================================================
  # SUMMARY: CURRENT vs EMPIRICAL WEIGHTS
  # =============================================================================
  
  message("\n================================================================================")
  message("    SUMMARY: CURRENT PRODUCTION vs EMPIRICAL WEIGHTS")
  message("================================================================================\n")
  
  current_df <- tibble(
    position = names(CURRENT_WEIGHTS),
    current_cs = sapply(CURRENT_WEIGHTS, function(x) x["cs"]),
    current_gf = sapply(CURRENT_WEIGHTS, function(x) x["gf"])
  )
  
  empirical_df <- tibble(
    position = names(results),
    emp_cs = sapply(results, function(x) x$weight_cs),
    emp_gf = sapply(results, function(x) x$weight_gf),
    r_squared = sapply(results, function(x) x$r_squared),
    n = sapply(results, function(x) x$n),
    sig_cs = sapply(results, function(x) x$significant_cs),
    sig_gf = sapply(results, function(x) x$significant_gf)
  )
  
  comparison <- current_df %>%
    left_join(empirical_df, by = "position") %>%
    mutate(
      cs_diff = emp_cs - current_cs,
      gf_diff = emp_gf - current_gf
    )
  
  message("Position | Current (CS/GF) | Empirical (CS/GF) | Significant? | R² | N")
  message("---------|-----------------|-------------------|--------------|-----|-----")
  
  for (i in 1:nrow(comparison)) {
    row <- comparison[i, ]
    sig_label <- paste0(
      if(row$sig_cs) "CS " else "",
      if(row$sig_gf) "GF" else ""
    )
    if (sig_label == "") sig_label <- "neither"
    
    message(sprintf("%-8s | %.2f / %.2f       | %.2f / %.2f         | %-12s | %.3f | %d",
                    row$position,
                    row$current_cs, row$current_gf,
                    row$emp_cs, row$emp_gf,
                    sig_label,
                    row$r_squared, row$n))
  }
  
  # =============================================================================
  # CS% CALIBRATION CHECK
  # =============================================================================
  
  message("\n================================================================================")
  message("    BONUS: Clean Sheet Odds Calibration (Predicted vs Actual)")
  message("================================================================================\n")
  
  cs_analysis <- analysis_data %>%
    group_by(team_std, gameweek) %>%
    summarise(
      cs_pct = first(cs_pct),
      cs_actual = first(cs_actual),
      .groups = "drop"
    ) %>%
    mutate(cs_actual_num = as.numeric(cs_actual))
  
  cs_calibration <- cs_analysis %>%
    mutate(cs_bin = cut(cs_pct, breaks = c(0, 20, 30, 40, 50, 100), 
                        labels = c("0-20%", "20-30%", "30-40%", "40-50%", "50%+"))) %>%
    group_by(cs_bin) %>%
    summarise(
      n = n(),
      predicted_pct = mean(cs_pct),
      actual_pct = mean(cs_actual_num, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(calibration_error = actual_pct - predicted_pct)
  
  print(cs_calibration)
  
  # =============================================================================
  # RECOMMENDATIONS
  # =============================================================================
  
  message("\n================================================================================")
  message("    RECOMMENDATIONS")
  message("================================================================================\n")
  
  # Check if any empirical weights differ significantly from current
  sig_differences <- comparison %>%
    filter(abs(cs_diff) > 0.15, sig_cs | sig_gf)
  
  if (nrow(sig_differences) > 0) {
    message("⚠️  Significant differences detected for:")
    for (i in 1:nrow(sig_differences)) {
      row <- sig_differences[i, ]
      message(sprintf("   %s: Current CS=%.2f, Empirical CS=%.2f (diff=%.2f)",
                      row$position, row$current_cs, row$emp_cs, row$cs_diff))
    }
    message("\n   Consider updating weights if this persists with more data")
  } else {
    message("✓ Current weights are reasonably aligned with empirical data")
    message("  No changes recommended at this time")
  }
  
  message(sprintf("\n  Data covers %d gameweeks", n_gameweeks))
  if (n_gameweeks < 30) {
    message("  → Re-run at end of season for more confident estimates")
  }
  
  # =============================================================================
  # SAVE RESULTS
  # =============================================================================
  
  if (save_results) {
    message("\n=== Saving Results ===\n")
    
    # Save comparison with timestamp
    output_file <- sprintf("fanteam_weights_analysis_%s.csv", Sys.Date())
    write_csv(comparison, output_file)
    message(sprintf("✓ Saved to %s", output_file))
  }
  
  message("\n================================================================================")
  message("    ANALYSIS COMPLETE")
  message("================================================================================\n")
  
  return(list(
    comparison = comparison,
    results = results,
    cs_calibration = cs_calibration,
    n_gameweeks = n_gameweeks,
    analysis_data = analysis_data
  ))
  
}

# =============================================================================
# RUN ANALYSIS
# =============================================================================

# Uncomment to run:
# output <- run_calibration_analysis()