################################################################################
# FANTEAM SHOTS/GOALS CALIBRATION
# 
# Uses existing 5-season historical data from Google Sheets (match_odds)
# to derive empirical coefficients for predicting shots/SOT/goals from odds
#
# Data source: Google Sheet 1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg
# Sheet: match_odds
#
# Columns used:
#   Inputs (pre-match odds):
#     - win_pct: Team's win probability (0-100)
#     - draw_pct: Draw probability (0-100)
#     - implied_goals_for: Team's expected goals
#     - implied_goals_against: Opponent's expected goals
#     - implied_total_goals: Total match expected goals
#     - venue: "Home" or "Away"
#
#   Outputs (actual results):
#     - goals_scored: Actual goals
#     - shots: Actual shots
#     - shots_on_target: Actual SOT
#
# Run: source("fanteam_shots_calibration.R"); results <- run_calibration()
################################################################################

library(dplyr)
library(tidyr)
library(googlesheets4)
library(ggplot2)

################################################################################
# CONFIGURATION
################################################################################

SHEET_ID <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"
SHEET_NAME <- "match_odds"

################################################################################
# DATA LOADING
################################################################################

load_match_odds <- function(sheet_id = SHEET_ID, sheet_name = SHEET_NAME) {
  message("=== Loading Historical Data from Google Sheets ===\n")
  
  gs4_auth(email = "Heerymichael@hotmail.com")
  
  data <- read_sheet(sheet_id, sheet = sheet_name)
  
  message(sprintf("✓ Loaded %d team-match observations", nrow(data)))
  message(sprintf("  Seasons: %s", paste(unique(data$season), collapse = ", ")))
  message(sprintf("  Date range: %s to %s", min(data$match_date), max(data$match_date)))
  
  # Check for required columns
  required <- c("win_pct", "implied_goals_for", "implied_total_goals", 
                "venue", "shots", "shots_on_target", "goals_scored")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # Clean data
  data <- data %>%
    filter(
      !is.na(shots),
      !is.na(shots_on_target),
      !is.na(goals_scored),
      !is.na(win_pct),
      !is.na(implied_total_goals)
    ) %>%
    mutate(
      win_pct = as.numeric(win_pct),
      draw_pct = as.numeric(draw_pct),
      implied_goals_for = as.numeric(implied_goals_for),
      implied_goals_against = as.numeric(implied_goals_against),
      implied_total_goals = as.numeric(implied_total_goals),
      shots = as.numeric(shots),
      shots_on_target = as.numeric(shots_on_target),
      goals_scored = as.numeric(goals_scored),
      is_home = venue == "Home"
    )
  
  message(sprintf("  After cleaning: %d observations with complete data\n", nrow(data)))
  
  return(data)
}

################################################################################
# SUMMARY STATISTICS
################################################################################

print_summary_stats <- function(data) {
  message("=== Summary Statistics ===\n")
  
  overall <- data %>%
    summarise(
      n = n(),
      avg_goals = mean(goals_scored),
      avg_shots = mean(shots),
      avg_sot = mean(shots_on_target),
      sot_ratio = mean(shots_on_target / shots),
      avg_win_pct = mean(win_pct),
      avg_implied_gf = mean(implied_goals_for),
      avg_implied_total = mean(implied_total_goals)
    )
  
  message("Overall averages:")
  message(sprintf("  Goals scored: %.2f", overall$avg_goals))
  message(sprintf("  Shots: %.1f", overall$avg_shots))
  message(sprintf("  Shots on target: %.1f", overall$avg_sot))
  message(sprintf("  SOT ratio: %.1f%%", overall$sot_ratio * 100))
  message(sprintf("  Win %%: %.1f%%", overall$avg_win_pct))
  message(sprintf("  Implied GF: %.2f", overall$avg_implied_gf))
  message(sprintf("  Implied total: %.2f\n", overall$avg_implied_total))
  
  by_venue <- data %>%
    group_by(venue) %>%
    summarise(
      n = n(),
      avg_goals = mean(goals_scored),
      avg_shots = mean(shots),
      avg_sot = mean(shots_on_target),
      sot_ratio = mean(shots_on_target / shots),
      .groups = "drop"
    )
  
  message("By venue:")
  for (i in 1:nrow(by_venue)) {
    v <- by_venue[i, ]
    message(sprintf("  %s: Goals=%.2f, Shots=%.1f, SOT=%.1f (ratio=%.1f%%)", 
                    v$venue, v$avg_goals, v$avg_shots, v$avg_sot, v$sot_ratio * 100))
  }
  message("")
  
  return(list(overall = overall, by_venue = by_venue))
}

################################################################################
# REGRESSION MODELS
################################################################################

run_regressions <- function(data) {
  message("================================================================================")
  message("    REGRESSION ANALYSIS")
  message("================================================================================\n")
  
  results <- list()
  
  # Split data for separate home/away models
  home_data <- data %>% filter(venue == "Home")
  away_data <- data %>% filter(venue == "Away")
  
  message(sprintf("Home observations: %d", nrow(home_data)))
  message(sprintf("Away observations: %d\n", nrow(away_data)))
  
  # ==========================================================================
  # MODEL 1: SHOTS ~ win_pct + implied_total_goals (separate home/away)
  # ==========================================================================
  
  message("--- SHOTS MODELS ---\n")
  
  # Home shots
  home_shots_model <- lm(shots ~ win_pct + implied_total_goals, data = home_data)
  message("HOME SHOTS: shots ~ win_pct + implied_total_goals")
  print(summary(home_shots_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(home_shots_model)$r.squared))
  
  results$home_shots <- list(
    model = home_shots_model,
    intercept = coef(home_shots_model)["(Intercept)"],
    win_pct = coef(home_shots_model)["win_pct"],
    total_goals = coef(home_shots_model)["implied_total_goals"],
    r_squared = summary(home_shots_model)$r.squared
  )
  
  # Away shots
  away_shots_model <- lm(shots ~ win_pct + implied_total_goals, data = away_data)
  message("AWAY SHOTS: shots ~ win_pct + implied_total_goals")
  print(summary(away_shots_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(away_shots_model)$r.squared))
  
  results$away_shots <- list(
    model = away_shots_model,
    intercept = coef(away_shots_model)["(Intercept)"],
    win_pct = coef(away_shots_model)["win_pct"],
    total_goals = coef(away_shots_model)["implied_total_goals"],
    r_squared = summary(away_shots_model)$r.squared
  )
  
  # ==========================================================================
  # MODEL 2: SOT ~ win_pct + implied_total_goals
  # ==========================================================================
  
  message("--- SHOTS ON TARGET MODELS ---\n")
  
  # Home SOT
  home_sot_model <- lm(shots_on_target ~ win_pct + implied_total_goals, data = home_data)
  message("HOME SOT: shots_on_target ~ win_pct + implied_total_goals")
  print(summary(home_sot_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(home_sot_model)$r.squared))
  
  results$home_sot <- list(
    model = home_sot_model,
    intercept = coef(home_sot_model)["(Intercept)"],
    win_pct = coef(home_sot_model)["win_pct"],
    total_goals = coef(home_sot_model)["implied_total_goals"],
    r_squared = summary(home_sot_model)$r.squared
  )
  
  # Away SOT
  away_sot_model <- lm(shots_on_target ~ win_pct + implied_total_goals, data = away_data)
  message("AWAY SOT: shots_on_target ~ win_pct + implied_total_goals")
  print(summary(away_sot_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(away_sot_model)$r.squared))
  
  results$away_sot <- list(
    model = away_sot_model,
    intercept = coef(away_sot_model)["(Intercept)"],
    win_pct = coef(away_sot_model)["win_pct"],
    total_goals = coef(away_sot_model)["implied_total_goals"],
    r_squared = summary(away_sot_model)$r.squared
  )
  
  # ==========================================================================
  # MODEL 3: GOALS ~ win_pct + draw_pct + implied_total_goals
  # ==========================================================================
  
  message("--- GOALS MODELS ---\n")
  
  # Home goals
  home_goals_model <- lm(goals_scored ~ win_pct + draw_pct + implied_total_goals, data = home_data)
  message("HOME GOALS: goals_scored ~ win_pct + draw_pct + implied_total_goals")
  print(summary(home_goals_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(home_goals_model)$r.squared))
  
  results$home_goals <- list(
    model = home_goals_model,
    intercept = coef(home_goals_model)["(Intercept)"],
    win_pct = coef(home_goals_model)["win_pct"],
    draw_pct = coef(home_goals_model)["draw_pct"],
    total_goals = coef(home_goals_model)["implied_total_goals"],
    r_squared = summary(home_goals_model)$r.squared
  )
  
  # Away goals
  away_goals_model <- lm(goals_scored ~ win_pct + draw_pct + implied_total_goals, data = away_data)
  message("AWAY GOALS: goals_scored ~ win_pct + draw_pct + implied_total_goals")
  print(summary(away_goals_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(away_goals_model)$r.squared))
  
  results$away_goals <- list(
    model = away_goals_model,
    intercept = coef(away_goals_model)["(Intercept)"],
    win_pct = coef(away_goals_model)["win_pct"],
    draw_pct = coef(away_goals_model)["draw_pct"],
    total_goals = coef(away_goals_model)["implied_total_goals"],
    r_squared = summary(away_goals_model)$r.squared
  )
  
  # ==========================================================================
  # SOT RATIOS (for simple conversion)
  # ==========================================================================
  
  message("--- SOT RATIOS ---\n")
  
  home_sot_ratio <- mean(home_data$shots_on_target / home_data$shots)
  away_sot_ratio <- mean(away_data$shots_on_target / away_data$shots)
  overall_sot_ratio <- mean(data$shots_on_target / data$shots)
  
  message(sprintf("Home SOT ratio: %.3f (%.1f%%)", home_sot_ratio, home_sot_ratio * 100))
  message(sprintf("Away SOT ratio: %.3f (%.1f%%)", away_sot_ratio, away_sot_ratio * 100))
  message(sprintf("Overall SOT ratio: %.3f (%.1f%%)\n", overall_sot_ratio, overall_sot_ratio * 100))
  
  results$sot_ratios <- list(
    home = home_sot_ratio,
    away = away_sot_ratio,
    overall = overall_sot_ratio
  )
  
  return(results)
}

################################################################################
# VALIDATION
################################################################################

validate_models <- function(data, results) {
  message("================================================================================")
  message("    MODEL VALIDATION")
  message("================================================================================\n")
  
  # Add predictions
  home_data <- data %>% filter(venue == "Home")
  away_data <- data %>% filter(venue == "Away")
  
  home_data <- home_data %>%
    mutate(
      pred_shots = predict(results$home_shots$model, newdata = .),
      pred_sot = predict(results$home_sot$model, newdata = .),
      pred_goals = predict(results$home_goals$model, newdata = .)
    )
  
  away_data <- away_data %>%
    mutate(
      pred_shots = predict(results$away_shots$model, newdata = .),
      pred_sot = predict(results$away_sot$model, newdata = .),
      pred_goals = predict(results$away_goals$model, newdata = .)
    )
  
  combined <- bind_rows(home_data, away_data)
  
  # Calculate errors
  message("Mean Absolute Error (MAE):")
  message(sprintf("  Shots: %.2f (avg actual: %.1f)", 
                  mean(abs(combined$pred_shots - combined$shots)),
                  mean(combined$shots)))
  message(sprintf("  SOT: %.2f (avg actual: %.1f)", 
                  mean(abs(combined$pred_sot - combined$shots_on_target)),
                  mean(combined$shots_on_target)))
  message(sprintf("  Goals: %.2f (avg actual: %.2f)", 
                  mean(abs(combined$pred_goals - combined$goals_scored)),
                  mean(combined$goals_scored)))
  
  # Correlations
  message("\nCorrelation (predicted vs actual):")
  message(sprintf("  Shots: %.3f", cor(combined$pred_shots, combined$shots)))
  message(sprintf("  SOT: %.3f", cor(combined$pred_sot, combined$shots_on_target)))
  message(sprintf("  Goals: %.3f\n", cor(combined$pred_goals, combined$goals_scored)))
  
  return(combined)
}

################################################################################
# GENERATE CODE
################################################################################

generate_coefficient_code <- function(results) {
  message("================================================================================")
  message("    GENERATED R CODE FOR mod_soccer_fanteam_contests.R")
  message("================================================================================\n")
  
  hs <- results$home_shots
  as <- results$away_shots
  hsot <- results$home_sot
  asot <- results$away_sot
  hg <- results$home_goals
  ag <- results$away_goals
  sot <- results$sot_ratios
  
  # Check if time-weighted
  weighting_note <- if (!is.null(results$half_life_years)) {
    sprintf("Time-weighted with %.1f-year half-life", results$half_life_years)
  } else {
    "Equal-weighted (all seasons)"
  }
  
  code <- sprintf('
# =============================================================================
# EMPIRICAL COEFFICIENTS - Derived from 5 seasons of Premier League data
# Generated: %s
# Source: Google Sheet match_odds (football-data.co.uk historical data)
# Weighting: %s
# =============================================================================

FANTEAM_COEFFICIENTS <- list(
  # HOME SHOTS: shots = intercept + (win_pct * coef) + (total_goals * coef)
  # R² = %.4f
  home_shots = list(
    intercept = %.4f,
    win_pct = %.6f,
    total_goals = %.4f
  ),
  
  # AWAY SHOTS
  # R² = %.4f
  away_shots = list(
    intercept = %.4f,
    win_pct = %.6f,
    total_goals = %.4f
  ),
  
  # HOME SOT
  # R² = %.4f
  home_sot = list(
    intercept = %.4f,
    win_pct = %.6f,
    total_goals = %.4f
  ),
  
  # AWAY SOT
  # R² = %.4f
  away_sot = list(
    intercept = %.4f,
    win_pct = %.6f,
    total_goals = %.4f
  ),
  
  # HOME GOALS: goals = intercept + (win_pct * coef) + (draw_pct * coef) + (total * coef)
  # R² = %.4f
  home_goals = list(
    intercept = %.4f,
    win_pct = %.6f,
    draw_pct = %.6f,
    total_goals = %.4f
  ),
  
  # AWAY GOALS
  # R² = %.4f
  away_goals = list(
    intercept = %.4f,
    win_pct = %.6f,
    draw_pct = %.6f,
    total_goals = %.4f
  ),
  
  # SOT ratios (for quick conversion if needed)
  sot_ratio = list(
    home = %.4f,
    away = %.4f,
    overall = %.4f
  )
)

# =============================================================================
# CALCULATION FUNCTIONS
# =============================================================================

calc_shots <- function(win_pct, total_goals, is_home) {
  c <- if (is_home) FANTEAM_COEFFICIENTS$home_shots else FANTEAM_COEFFICIENTS$away_shots
  c$intercept + (win_pct * c$win_pct) + (total_goals * c$total_goals)
}

calc_sot <- function(win_pct, total_goals, is_home) {
  c <- if (is_home) FANTEAM_COEFFICIENTS$home_sot else FANTEAM_COEFFICIENTS$away_sot
  c$intercept + (win_pct * c$win_pct) + (total_goals * c$total_goals)
}

calc_goals <- function(win_pct, draw_pct, total_goals, is_home) {
  c <- if (is_home) FANTEAM_COEFFICIENTS$home_goals else FANTEAM_COEFFICIENTS$away_goals
  c$intercept + (win_pct * c$win_pct) + (draw_pct * c$draw_pct) + (total_goals * c$total_goals)
}
',
Sys.Date(),
weighting_note,
hs$r_squared, hs$intercept, hs$win_pct, hs$total_goals,
as$r_squared, as$intercept, as$win_pct, as$total_goals,
hsot$r_squared, hsot$intercept, hsot$win_pct, hsot$total_goals,
asot$r_squared, asot$intercept, asot$win_pct, asot$total_goals,
hg$r_squared, hg$intercept, hg$win_pct, hg$draw_pct, hg$total_goals,
ag$r_squared, ag$intercept, ag$win_pct, ag$draw_pct, ag$total_goals,
sot$home, sot$away, sot$overall
  )

cat(code)

return(code)
}

################################################################################
# EXAMPLE PREDICTIONS
################################################################################

print_example_predictions <- function(results) {
  message("\n================================================================================")
  message("    EXAMPLE PREDICTIONS")
  message("================================================================================\n")
  
  # Example: Liverpool (big favorite) vs Burnley
  message("Example 1: Liverpool (Home) vs Burnley")
  message("  Inputs: win_pct=75, draw_pct=15, total_goals=3.2")
  
  hs <- results$home_shots
  hsot <- results$home_sot
  hg <- results$home_goals
  
  pred_shots <- hs$intercept + (75 * hs$win_pct) + (3.2 * hs$total_goals)
  pred_sot <- hsot$intercept + (75 * hsot$win_pct) + (3.2 * hsot$total_goals)
  pred_goals <- hg$intercept + (75 * hg$win_pct) + (15 * hg$draw_pct) + (3.2 * hg$total_goals)
  
  message(sprintf("  Predicted: Shots=%.1f, SOT=%.1f, Goals=%.2f\n", pred_shots, pred_sot, pred_goals))
  
  # Example: Burnley (big underdog away)
  message("Example 2: Burnley (Away) vs Liverpool")
  message("  Inputs: win_pct=10, draw_pct=15, total_goals=3.2")
  
  as <- results$away_shots
  asot <- results$away_sot
  ag <- results$away_goals
  
  pred_shots <- as$intercept + (10 * as$win_pct) + (3.2 * as$total_goals)
  pred_sot <- asot$intercept + (10 * asot$win_pct) + (3.2 * asot$total_goals)
  pred_goals <- ag$intercept + (10 * ag$win_pct) + (15 * ag$draw_pct) + (3.2 * ag$total_goals)
  
  message(sprintf("  Predicted: Shots=%.1f, SOT=%.1f, Goals=%.2f\n", pred_shots, pred_sot, pred_goals))
  
  # Example: Even match
  message("Example 3: Chelsea (Home) vs Arsenal (even match)")
  message("  Inputs: win_pct=35, draw_pct=30, total_goals=2.5")
  
  pred_shots <- hs$intercept + (35 * hs$win_pct) + (2.5 * hs$total_goals)
  pred_sot <- hsot$intercept + (35 * hsot$win_pct) + (2.5 * hsot$total_goals)
  pred_goals <- hg$intercept + (35 * hg$win_pct) + (30 * hg$draw_pct) + (2.5 * hg$total_goals)
  
  message(sprintf("  Predicted: Shots=%.1f, SOT=%.1f, Goals=%.2f\n", pred_shots, pred_sot, pred_goals))
}

################################################################################
# TIME-WEIGHTED REGRESSION
################################################################################

run_weighted_regressions <- function(data, half_life_years = 2) {
  message("================================================================================")
  message(sprintf("    TIME-WEIGHTED REGRESSION (half-life = %.1f years)", half_life_years))
  message("================================================================================\n")
  
  # Calculate weights based on exponential decay
  data <- data %>%
    mutate(
      days_ago = as.numeric(Sys.Date() - as.Date(match_date)),
      # Exponential decay: weight = 0.5^(years_ago / half_life)
      weight = 0.5 ^ (days_ago / (half_life_years * 365))
    )
  
  message(sprintf("Weight range: %.3f (oldest) to %.3f (newest)", 
                  min(data$weight), max(data$weight)))
  message(sprintf("Effective sample size: %.0f (vs %d actual)\n", 
                  sum(data$weight), nrow(data)))
  
  results <- list()
  
  home_data <- data %>% filter(venue == "Home")
  away_data <- data %>% filter(venue == "Away")
  
  message(sprintf("Home observations: %d (effective: %.0f)", 
                  nrow(home_data), sum(home_data$weight)))
  message(sprintf("Away observations: %d (effective: %.0f)\n", 
                  nrow(away_data), sum(away_data$weight)))
  
  # ==========================================================================
  # WEIGHTED SHOTS MODELS
  # ==========================================================================
  
  message("--- WEIGHTED SHOTS MODELS ---\n")
  
  # Home shots (weighted)
  home_shots_model <- lm(shots ~ win_pct + implied_total_goals, 
                         data = home_data, weights = weight)
  message("HOME SHOTS (weighted): shots ~ win_pct + implied_total_goals")
  print(summary(home_shots_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(home_shots_model)$r.squared))
  
  results$home_shots <- list(
    model = home_shots_model,
    intercept = coef(home_shots_model)["(Intercept)"],
    win_pct = coef(home_shots_model)["win_pct"],
    total_goals = coef(home_shots_model)["implied_total_goals"],
    r_squared = summary(home_shots_model)$r.squared
  )
  
  # Away shots (weighted)
  away_shots_model <- lm(shots ~ win_pct + implied_total_goals, 
                         data = away_data, weights = weight)
  message("AWAY SHOTS (weighted): shots ~ win_pct + implied_total_goals")
  print(summary(away_shots_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(away_shots_model)$r.squared))
  
  results$away_shots <- list(
    model = away_shots_model,
    intercept = coef(away_shots_model)["(Intercept)"],
    win_pct = coef(away_shots_model)["win_pct"],
    total_goals = coef(away_shots_model)["implied_total_goals"],
    r_squared = summary(away_shots_model)$r.squared
  )
  
  # ==========================================================================
  # WEIGHTED SOT MODELS
  # ==========================================================================
  
  message("--- WEIGHTED SOT MODELS ---\n")
  
  home_sot_model <- lm(shots_on_target ~ win_pct + implied_total_goals, 
                       data = home_data, weights = weight)
  message("HOME SOT (weighted): shots_on_target ~ win_pct + implied_total_goals")
  print(summary(home_sot_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(home_sot_model)$r.squared))
  
  results$home_sot <- list(
    model = home_sot_model,
    intercept = coef(home_sot_model)["(Intercept)"],
    win_pct = coef(home_sot_model)["win_pct"],
    total_goals = coef(home_sot_model)["implied_total_goals"],
    r_squared = summary(home_sot_model)$r.squared
  )
  
  away_sot_model <- lm(shots_on_target ~ win_pct + implied_total_goals, 
                       data = away_data, weights = weight)
  message("AWAY SOT (weighted): shots_on_target ~ win_pct + implied_total_goals")
  print(summary(away_sot_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(away_sot_model)$r.squared))
  
  results$away_sot <- list(
    model = away_sot_model,
    intercept = coef(away_sot_model)["(Intercept)"],
    win_pct = coef(away_sot_model)["win_pct"],
    total_goals = coef(away_sot_model)["implied_total_goals"],
    r_squared = summary(away_sot_model)$r.squared
  )
  
  # ==========================================================================
  # WEIGHTED GOALS MODELS
  # ==========================================================================
  
  message("--- WEIGHTED GOALS MODELS ---\n")
  
  home_goals_model <- lm(goals_scored ~ win_pct + draw_pct + implied_total_goals, 
                         data = home_data, weights = weight)
  message("HOME GOALS (weighted): goals_scored ~ win_pct + draw_pct + implied_total_goals")
  print(summary(home_goals_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(home_goals_model)$r.squared))
  
  results$home_goals <- list(
    model = home_goals_model,
    intercept = coef(home_goals_model)["(Intercept)"],
    win_pct = coef(home_goals_model)["win_pct"],
    draw_pct = coef(home_goals_model)["draw_pct"],
    total_goals = coef(home_goals_model)["implied_total_goals"],
    r_squared = summary(home_goals_model)$r.squared
  )
  
  away_goals_model <- lm(goals_scored ~ win_pct + draw_pct + implied_total_goals, 
                         data = away_data, weights = weight)
  message("AWAY GOALS (weighted): goals_scored ~ win_pct + draw_pct + implied_total_goals")
  print(summary(away_goals_model)$coefficients)
  message(sprintf("R² = %.4f\n", summary(away_goals_model)$r.squared))
  
  results$away_goals <- list(
    model = away_goals_model,
    intercept = coef(away_goals_model)["(Intercept)"],
    win_pct = coef(away_goals_model)["win_pct"],
    draw_pct = coef(away_goals_model)["draw_pct"],
    total_goals = coef(away_goals_model)["implied_total_goals"],
    r_squared = summary(away_goals_model)$r.squared
  )
  
  # ==========================================================================
  # WEIGHTED SOT RATIOS
  # ==========================================================================
  
  message("--- WEIGHTED SOT RATIOS ---\n")
  
  home_sot_ratio <- weighted.mean(home_data$shots_on_target / home_data$shots, home_data$weight)
  away_sot_ratio <- weighted.mean(away_data$shots_on_target / away_data$shots, away_data$weight)
  overall_sot_ratio <- weighted.mean(data$shots_on_target / data$shots, data$weight)
  
  message(sprintf("Home SOT ratio (weighted): %.3f (%.1f%%)", home_sot_ratio, home_sot_ratio * 100))
  message(sprintf("Away SOT ratio (weighted): %.3f (%.1f%%)", away_sot_ratio, away_sot_ratio * 100))
  message(sprintf("Overall SOT ratio (weighted): %.3f (%.1f%%)\n", overall_sot_ratio, overall_sot_ratio * 100))
  
  results$sot_ratios <- list(
    home = home_sot_ratio,
    away = away_sot_ratio,
    overall = overall_sot_ratio
  )
  
  results$half_life_years <- half_life_years
  
  return(results)
}

################################################################################
# COMPARE WEIGHTED VS UNWEIGHTED
################################################################################

compare_coefficients <- function(unweighted, weighted) {
  message("\n================================================================================")
  message("    COMPARISON: UNWEIGHTED vs TIME-WEIGHTED COEFFICIENTS")
  message("================================================================================\n")
  
  compare_one <- function(name, uw, w) {
    message(sprintf("--- %s ---", toupper(name)))
    message(sprintf("  Intercept:   %.4f → %.4f (%.1f%% change)", 
                    uw$intercept, w$intercept, 
                    (w$intercept - uw$intercept) / abs(uw$intercept) * 100))
    message(sprintf("  Win %%:      %.6f → %.6f (%.1f%% change)", 
                    uw$win_pct, w$win_pct,
                    (w$win_pct - uw$win_pct) / abs(uw$win_pct) * 100))
    message(sprintf("  Total goals: %.4f → %.4f (%.1f%% change)", 
                    uw$total_goals, w$total_goals,
                    (w$total_goals - uw$total_goals) / abs(uw$total_goals) * 100))
    message(sprintf("  R²:          %.4f → %.4f\n", uw$r_squared, w$r_squared))
  }
  
  compare_one("Home Shots", unweighted$home_shots, weighted$home_shots)
  compare_one("Away Shots", unweighted$away_shots, weighted$away_shots)
  compare_one("Home SOT", unweighted$home_sot, weighted$home_sot)
  compare_one("Away SOT", unweighted$away_sot, weighted$away_sot)
  
  message("--- HOME GOALS ---")
  message(sprintf("  Intercept:   %.4f → %.4f", 
                  unweighted$home_goals$intercept, weighted$home_goals$intercept))
  message(sprintf("  Win %%:      %.6f → %.6f", 
                  unweighted$home_goals$win_pct, weighted$home_goals$win_pct))
  message(sprintf("  Draw %%:     %.6f → %.6f", 
                  unweighted$home_goals$draw_pct, weighted$home_goals$draw_pct))
  message(sprintf("  Total goals: %.4f → %.4f\n", 
                  unweighted$home_goals$total_goals, weighted$home_goals$total_goals))
  
  message("--- AWAY GOALS ---")
  message(sprintf("  Intercept:   %.4f → %.4f", 
                  unweighted$away_goals$intercept, weighted$away_goals$intercept))
  message(sprintf("  Win %%:      %.6f → %.6f", 
                  unweighted$away_goals$win_pct, weighted$away_goals$win_pct))
  message(sprintf("  Draw %%:     %.6f → %.6f", 
                  unweighted$away_goals$draw_pct, weighted$away_goals$draw_pct))
  message(sprintf("  Total goals: %.4f → %.4f\n", 
                  unweighted$away_goals$total_goals, weighted$away_goals$total_goals))
  
  message("--- SOT RATIOS ---")
  message(sprintf("  Home:    %.4f → %.4f", unweighted$sot_ratios$home, weighted$sot_ratios$home))
  message(sprintf("  Away:    %.4f → %.4f", unweighted$sot_ratios$away, weighted$sot_ratios$away))
  message(sprintf("  Overall: %.4f → %.4f\n", unweighted$sot_ratios$overall, weighted$sot_ratios$overall))
}

################################################################################
# MAIN
################################################################################

run_calibration <- function(use_time_weighting = TRUE, half_life_years = 2) {
  message("================================================================================")
  message("    FANTEAM SHOTS/GOALS CALIBRATION")
  message(sprintf("    Run date: %s", Sys.Date()))
  message(sprintf("    Time weighting: %s", if(use_time_weighting) sprintf("YES (half-life = %.1f years)", half_life_years) else "NO"))
  message("================================================================================\n")
  
  # Load data
  data <- load_match_odds()
  
  # Summary stats
  stats <- print_summary_stats(data)
  
  # Run unweighted regressions first (for comparison)
  message("\n--- RUNNING UNWEIGHTED REGRESSIONS ---\n")
  unweighted_results <- run_regressions(data)
  
  if (use_time_weighting) {
    # Run weighted regressions
    weighted_results <- run_weighted_regressions(data, half_life_years)
    
    # Compare
    compare_coefficients(unweighted_results, weighted_results)
    
    # Use weighted for final output
    results <- weighted_results
    
    # Validate weighted model
    validated_data <- validate_models(data, results)
  } else {
    results <- unweighted_results
    validated_data <- validate_models(data, results)
  }
  
  # Generate code
  code <- generate_coefficient_code(results)
  
  # Print examples
  print_example_predictions(results)
  
  message("\n================================================================================")
  message("    CALIBRATION COMPLETE")
  message("================================================================================\n")
  message("Copy the FANTEAM_COEFFICIENTS code above into mod_soccer_fanteam_contests.R")
  message("Replace the existing calc_shots() function with the new empirical version.\n")
  
  return(list(
    data = data,
    stats = stats,
    unweighted_results = if(use_time_weighting) unweighted_results else NULL,
    results = results,
    validated_data = validated_data,
    coefficient_code = code
  ))
}

################################################################################
# RUN
################################################################################

# Default: Time-weighted with 2-year half-life
output <- run_calibration()

# With custom half-life (e.g., 1.5 years for more recency bias)
# output <- run_calibration(use_time_weighting = TRUE, half_life_years = 1.5)

# Without time weighting (equal weight all seasons)
# output <- run_calibration(use_time_weighting = FALSE)