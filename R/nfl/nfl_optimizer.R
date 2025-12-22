# =============================================================================
# NFL Optimizer
# 
# Linear programming optimization functions for NFL DFS lineup building
# Used by: mod_nfl_handbuild.R, mod_nfl_optimiser.R
# 
# Dependencies: lpSolve
# =============================================================================

library(lpSolve)

# =============================================================================
# SINGLE LINEUP OPTIMIZATION
# =============================================================================

#' Optimize a single lineup using linear programming
#' 
#' @param players Data frame with player data (must include: player, position, team, salary, and projection column)
#' @param projection_col Name of column to optimize on (e.g., "blended", "adjusted_blended")
#' @param salary_cap Maximum total salary allowed
#' @param locked_players Character vector of player names that must be included
#' @param excluded_players Character vector of player names to exclude
#' @return Data frame with optimal lineup (9 players) or NULL if no valid lineup found
#' 
#' @examples
#' optimal <- optimize_lineup_lp(
#'   players = player_data,
#'   projection_col = "blended",
#'   salary_cap = 130,
#'   locked_players = c("Patrick Mahomes"),
#'   excluded_players = c("Travis Kelce")
#' )
optimize_lineup_lp <- function(players, projection_col, salary_cap,
                               locked_players = NULL, excluded_players = NULL) {
  
  log_debug(">>> optimize_lineup_lp() called", level = "DEBUG")
  log_debug(">>>   projection_col:", projection_col, level = "DEBUG")
  log_debug(">>>   salary_cap:", salary_cap, level = "DEBUG")
  log_debug(">>>   locked_players:", length(locked_players %||% c()), level = "DEBUG")
  log_debug(">>>   excluded_players:", length(excluded_players %||% c()), level = "DEBUG")
  
  # Filter excluded players
  available <- players
  if (!is.null(excluded_players) && length(excluded_players) > 0) {
    available <- available %>% filter(!(player %in% excluded_players))
    log_debug(">>>   After exclusions:", nrow(available), "players", level = "DEBUG")
  }
  
  n <- nrow(available)
  if (n == 0) {
    log_debug(">>> No players available after exclusions", level = "WARN")
    return(NULL)
  }
  
  # Verify locked players exist in available pool
  if (!is.null(locked_players) && length(locked_players) > 0) {
    missing <- setdiff(locked_players, available$player)
    if (length(missing) > 0) {
      log_debug(">>> Locked players not found:", paste(missing, collapse = ", "), level = "WARN")
      return(NULL)
    }
  }
  
  # Verify projection column exists
  if (!(projection_col %in% names(available))) {
    log_debug(">>> Projection column not found:", projection_col, level = "ERROR")
    return(NULL)
  }
  
  # Objective function (maximize projection)
  objective <- available[[projection_col]]
  
  # Build constraint matrix
  # Constraints:
  # 1. QB = 1
  # 2. RB >= 2
  # 3. WR >= 3
  # 4. TE >= 1
  # 5. DST = 1
  # 6. RB + WR + TE >= 6 (to allow FLEX)
  # 7. Total players = 9
  # 8. Salary <= cap
  
  constraint_matrix <- rbind(
    as.numeric(available$position == "QB"),        # QB = 1
    as.numeric(available$position == "RB"),        # RB >= 2
    as.numeric(available$position == "WR"),        # WR >= 3
    as.numeric(available$position == "TE"),        # TE >= 1
    as.numeric(available$position == "DST"),       # DST = 1
    as.numeric(available$position %in% c("RB", "WR", "TE")),  # FLEX total >= 6
    rep(1, n),                                      # Total = 9
    available$salary                                # Salary <= cap
  )
  
  constraint_dirs <- c("==", ">=", ">=", ">=", "==", ">=", "==", "<=")
  constraint_rhs <- c(1, 2, 3, 1, 1, 6, 9, salary_cap)
  
  # Add constraints for locked players (each must be selected)
  if (!is.null(locked_players) && length(locked_players) > 0) {
    for (locked_player in locked_players) {
      if (locked_player %in% available$player) {
        constraint_matrix <- rbind(
          constraint_matrix,
          as.numeric(available$player == locked_player)
        )
        constraint_dirs <- c(constraint_dirs, "==")
        constraint_rhs <- c(constraint_rhs, 1)
      }
    }
  }
  
  # Solve the LP problem
  solution <- tryCatch({
    lp(
      direction = "max",
      objective.in = objective,
      const.mat = constraint_matrix,
      const.dir = constraint_dirs,
      const.rhs = constraint_rhs,
      all.bin = TRUE
    )
  }, error = function(e) {
    log_debug(">>> LP solve error:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(solution) || solution$status != 0) {
    log_debug(">>> No feasible solution found (status:", solution$status %||% "NULL", ")", level = "WARN")
    return(NULL)
  }
  
  # Extract selected players
  lineup <- available[solution$solution == 1, ] %>%
    mutate(
      projection = .data[[projection_col]],
      value = projection / salary
    ) %>%
    select(player, position, team, salary, projection, value,
           any_of(c("opponent", "home", "headshot_url", "team_bg_color"))) %>%
    arrange(match(position, c("QB", "RB", "WR", "TE", "DST")))
  
  log_debug(">>> Optimal lineup found:", sum(lineup$projection), "projected points,",
            sum(lineup$salary), "salary", level = "INFO")
  
  lineup
}

# =============================================================================
# MULTIPLE LINEUP GENERATION
# =============================================================================

#' Generate multiple unique lineups with variance and stacking rules
#' 
#' @param players Data frame with player data
#' @param num_lineups Number of lineups to generate
#' @param salary_cap Maximum salary
#' @param variance_pct Variance percentage to apply to projections (0-50)
#' @param locked_players Players that must be in every lineup
#' @param adjustments Named list of player adjustments (player_name -> percentage)
#' @param stacking_rules List of conditional stacking rule objects
#' @param stack_game Game key for game stack (e.g., "KC_BUF")
#' @param min_game_players Minimum players from selected game
#' @return List of lineup data frames
#' 
#' @examples
#' lineups <- generate_lineups_with_variance(
#'   players = player_data,
#'   num_lineups = 10,
#'   salary_cap = 130,
#'   variance_pct = 15,
#'   locked_players = c("Patrick Mahomes"),
#'   stacking_rules = list(rule1)
#' )
generate_lineups_with_variance <- function(players, num_lineups, salary_cap,
                                           variance_pct = 10, locked_players = NULL,
                                           adjustments = list(),
                                           stacking_rules = list(),
                                           stack_game = "",
                                           min_game_players = 4) {
  
  log_debug(">>> generate_lineups_with_variance() called", level = "INFO")
  log_debug(">>>   num_lineups:", num_lineups, level = "INFO")
  log_debug(">>>   variance_pct:", variance_pct, level = "INFO")
  log_debug(">>>   stacking_rules:", length(stacking_rules), level = "INFO")
  log_debug(">>>   stack_game:", stack_game, level = "INFO")
  
  lineups <- list()
  lineup_signatures <- character(0)
  
  # Apply adjustments to create base projections
  players_base <- players %>%
    mutate(
      base_projection = sapply(1:n(), function(i) {
        adj_pct <- adjustments[[player[i]]] %||% 0
        blended[i] * (1 + adj_pct / 100)
      })
    )
  
  # Parse game stack if applicable
  game_teams <- if (stack_game != "") {
    strsplit(stack_game, "_")[[1]]
  } else {
    NULL
  }
  
  attempts <- 0
  max_attempts <- num_lineups * 50  # Allow many attempts due to constraints
  
  while (length(lineups) < num_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Apply random variance - fresh for each lineup attempt
    set.seed(Sys.time() + attempts)
    
    players_varied <- players_base %>%
      mutate(
        variance_mult = 1 + runif(n(), -variance_pct/100, variance_pct/100),
        varied_projection = base_projection * variance_mult
      )
    
    # Optimize with varied projections
    lineup <- optimize_lineup_lp(
      players = players_varied,
      projection_col = "varied_projection",
      salary_cap = salary_cap,
      locked_players = locked_players
    )
    
    if (is.null(lineup)) next
    
    # Check stacking rules
    if (!check_stacking_rules(lineup, players_base, stacking_rules, game_teams, min_game_players)) {
      next
    }
    
    # Check for duplicate lineups using signature
    sig <- paste(sort(lineup$player), collapse = "|")
    if (sig %in% lineup_signatures) next
    
    # Restore base projections (with adjustments applied)
    lineup <- lineup %>%
      mutate(projection = players_base$base_projection[match(player, players_base$player)])
    
    lineups[[length(lineups) + 1]] <- lineup
    lineup_signatures <- c(lineup_signatures, sig)
    
    if (length(lineups) %% 5 == 0) {
      log_debug(">>>   Generated", length(lineups), "lineups after", attempts, "attempts", level = "DEBUG")
    }
  }
  
  log_debug(">>> Generated", length(lineups), "unique lineups in", attempts, "attempts", level = "INFO")
  
  lineups
}

# =============================================================================
# STACKING RULE VALIDATION
# =============================================================================

#' Check if lineup meets conditional stacking rules
#' 
#' @param lineup Lineup data frame
#' @param players Full player data (for opponent lookup)
#' @param stacking_rules List of conditional stacking rules
#' @param game_teams Teams in selected game (for game stack)
#' @param min_game_players Minimum players from selected game
#' @return TRUE if lineup passes all applicable stacking rules
check_stacking_rules <- function(lineup, players, stacking_rules, game_teams, min_game_players) {
  
  # Check game stack requirement first (applies to all lineups)
  if (!is.null(game_teams) && length(game_teams) >= 2) {
    game_players <- lineup %>%
      filter(team %in% game_teams) %>%
      nrow()
    
    if (game_players < min_game_players) {
      return(FALSE)
    }
  }
  
  # If no conditional rules, lineup passes
  if (length(stacking_rules) == 0) return(TRUE)
  
  # Get QB info
  qb_row <- lineup %>% filter(position == "QB")
  if (nrow(qb_row) == 0) return(FALSE)
  
  qb_name <- qb_row$player[1]
  qb_team <- qb_row$team[1]
  
  # Get opponent from player data
  qb_player_data <- players %>% filter(player == qb_name)
  qb_opponent <- if (nrow(qb_player_data) > 0 && "opponent" %in% names(qb_player_data)) {
    qb_player_data$opponent[1]
  } else {
    ""
  }
  
  # Find applicable rule for this QB
  applicable_rule <- NULL
  for (rule in stacking_rules) {
    if (qb_name %in% rule$qbs) {
      applicable_rule <- rule
      break
    }
  }
  
  # If no rule applies to this QB, lineup passes
  if (is.null(applicable_rule)) return(TRUE)
  
  # Check same-team stack requirement
  if (applicable_rule$same_team_min > 0) {
    same_team_players <- lineup %>%
      filter(
        position %in% applicable_rule$same_team_positions,
        team == qb_team
      ) %>%
      nrow()
    
    if (same_team_players < applicable_rule$same_team_min) {
      return(FALSE)
    }
  }
  
  # Check opponent stack requirement (bring-back)
  if (applicable_rule$opp_min > 0 && qb_opponent != "") {
    opp_players <- lineup %>%
      filter(
        position %in% applicable_rule$opp_positions,
        team == qb_opponent
      ) %>%
      nrow()
    
    if (opp_players < applicable_rule$opp_min) {
      return(FALSE)
    }
  }
  
  TRUE
}

# =============================================================================
# STACKING RULE BUILDER
# =============================================================================

#' Create a stacking rule object
#' 
#' @param qbs Character vector of QB names this rule applies to
#' @param same_team_min Minimum same-team players required
#' @param same_team_positions Positions to count for same-team stack
#' @param opp_min Minimum opponent players required (bring-back)
#' @param opp_positions Positions to count for opponent stack
#' @return Stacking rule list object
create_stacking_rule <- function(qbs, 
                                 same_team_min = 0, 
                                 same_team_positions = c("WR", "TE"),
                                 opp_min = 0,
                                 opp_positions = c("WR")) {
  list(
    id = paste0("rule_", sample(1000:9999, 1)),
    qbs = qbs,
    same_team_min = same_team_min,
    same_team_positions = same_team_positions,
    opp_min = opp_min,
    opp_positions = opp_positions
  )
}

# =============================================================================
# LINEUP UTILITIES
# =============================================================================

#' Calculate lineup statistics
#' 
#' @param lineup Lineup data frame
#' @param adjustments Named list of player adjustments
#' @return List with total_salary, total_projection, adjusted_projection
calculate_lineup_stats <- function(lineup, adjustments = list()) {
  if (is.null(lineup) || nrow(lineup) == 0) {
    return(list(
      total_salary = 0,
      total_projection = 0,
      adjusted_projection = 0,
      player_count = 0
    ))
  }
  
  total_salary <- sum(lineup$salary, na.rm = TRUE)
  total_projection <- sum(lineup$projection, na.rm = TRUE)
  
  # Calculate adjusted projection
  adjusted_projection <- sum(sapply(1:nrow(lineup), function(i) {
    adj_pct <- adjustments[[lineup$player[i]]] %||% 0
    lineup$projection[i] * (1 + adj_pct / 100)
  }))
  
  list(
    total_salary = total_salary,
    total_projection = total_projection,
    adjusted_projection = adjusted_projection,
    player_count = nrow(lineup)
  )
}

#' Apply percentage adjustments to player projections
#' 
#' @param players Player data frame with blended column
#' @param adjustments Named list of player adjustments (player_name -> percentage)
#' @return Player data frame with adjusted_blended column added
apply_projection_adjustments <- function(players, adjustments) {
  players %>%
    mutate(
      adjusted_blended = sapply(1:n(), function(i) {
        adj_pct <- adjustments[[player[i]]] %||% 0
        blended[i] * (1 + adj_pct / 100)
      })
    )
}

# =============================================================================
# EXPORT MESSAGE
# =============================================================================

message("NFL optimizer loaded: optimize_lineup_lp(), generate_lineups_with_variance(), check_stacking_rules()")