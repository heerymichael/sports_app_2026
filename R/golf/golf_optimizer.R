# =============================================================================
# Golf Optimizer
# 
# Linear programming optimization for golf DFS lineups
# Supports both Classic (full tournament) and Showdown (single day) formats
# 
# Features:
# - Grouped boost/dock (applied before optimization)
# - Correlation rules (trigger player -> boost/dock targets)
# - Variance-based lineup generation
# - Uniqueness constraints
# 
# Dependencies: lpSolve
# =============================================================================

library(lpSolve)

# =============================================================================
# CLASSIC OPTIMIZATION (6 golfers, no multipliers)
# =============================================================================

#' Optimize a classic golf lineup using linear programming
#' 
#' @param players Data frame with player_id, player_name, salary, and projection column
#' @param projection_col Name of column to optimize on
#' @param salary_cap Maximum total salary allowed (default 100)
#' @param locked_players Character vector of player names that must be included
#' @param excluded_players Character vector of player names to exclude
#' @param corr_rules List of correlation rules (trigger, boost_targets, boost_pct, dock_targets, dock_pct)
#' @return Data frame with optimal lineup (6 golfers) or NULL if no valid lineup found
optimize_golf_classic_lp <- function(players, 
                                     projection_col = "blended",
                                     salary_cap = 100,
                                     locked_players = NULL, 
                                     excluded_players = NULL,
                                     corr_rules = list()) {
  
  log_debug(">>> optimize_golf_classic_lp() called", level = "DEBUG")
  log_debug(">>>   projection_col:", projection_col, level = "DEBUG")
  log_debug(">>>   salary_cap:", salary_cap, level = "DEBUG")
  log_debug(">>>   locked_players:", length(locked_players %||% c()), level = "DEBUG")
  log_debug(">>>   excluded_players:", length(excluded_players %||% c()), level = "DEBUG")
  log_debug(">>>   corr_rules:", length(corr_rules), level = "DEBUG")
  
  # Filter excluded players
  available <- players
  if (!is.null(excluded_players) && length(excluded_players) > 0) {
    available <- available %>% filter(!(player_name %in% excluded_players))
    log_debug(">>>   After exclusions:", nrow(available), "players", level = "DEBUG")
  }
  
  # Filter out players without projections
  available <- available %>% 
    filter(!is.na(.data[[projection_col]]) & .data[[projection_col]] > 0)
  
  n <- nrow(available)
  if (n < 6) {
    log_debug(">>> Not enough players after filtering:", n, level = "WARN")
    return(NULL)
  }
  
  # Verify locked players exist
  if (!is.null(locked_players) && length(locked_players) > 0) {
    missing <- setdiff(locked_players, available$player_name)
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
  
  # Objective: maximize projection
  objective <- available[[projection_col]]
  
  # Constraints: Total = 6, Salary <= cap
  constraint_matrix <- rbind(
    rep(1, n),
    available$salary
  )
  constraint_dirs <- c("==", "<=")
  constraint_rhs <- c(6, salary_cap)
  
  # Add locked player constraints
  if (!is.null(locked_players) && length(locked_players) > 0) {
    for (locked_player in locked_players) {
      if (locked_player %in% available$player_name) {
        constraint_matrix <- rbind(
          constraint_matrix,
          as.numeric(available$player_name == locked_player)
        )
        constraint_dirs <- c(constraint_dirs, "==")
        constraint_rhs <- c(constraint_rhs, 1)
      }
    }
  }
  
  # First solve without correlation adjustments
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
    log_debug(">>> No feasible solution found", level = "WARN")
    return(NULL)
  }
  
  # Check if any trigger players are in the solution
  selected_players <- available$player_name[solution$solution == 1]
  
  if (length(corr_rules) > 0) {
    trigger_matched_rules <- purrr::keep(corr_rules, ~ .x$trigger %in% selected_players)
    
    if (length(trigger_matched_rules) > 0) {
      log_debug(">>> Trigger players found, re-optimizing with correlation boosts", level = "DEBUG")
      
      # Apply boosts/docks to projection
      boosted_proj <- available[[projection_col]]
      
      for (rule in trigger_matched_rules) {
        if (length(rule$boost_targets) > 0 && rule$boost_pct > 0) {
          boost_mask <- available$player_name %in% rule$boost_targets
          boosted_proj[boost_mask] <- boosted_proj[boost_mask] * (1 + rule$boost_pct / 100)
        }
        if (length(rule$dock_targets) > 0 && rule$dock_pct > 0) {
          dock_mask <- available$player_name %in% rule$dock_targets
          boosted_proj[dock_mask] <- boosted_proj[dock_mask] * (1 - rule$dock_pct / 100)
        }
      }
      
      # Force trigger players to be included
      forced_triggers <- unique(sapply(trigger_matched_rules, function(r) r$trigger))
      force_mat <- constraint_matrix
      force_dirs <- constraint_dirs
      force_rhs <- constraint_rhs
      
      for (trigger in forced_triggers) {
        force_mat <- rbind(force_mat, as.numeric(available$player_name == trigger))
        force_dirs <- c(force_dirs, "==")
        force_rhs <- c(force_rhs, 1)
      }
      
      # Re-solve with boosted projections and forced triggers
      solution <- tryCatch({
        lp(
          direction = "max",
          objective.in = boosted_proj,
          const.mat = force_mat,
          const.dir = force_dirs,
          const.rhs = force_rhs,
          all.bin = TRUE
        )
      }, error = function(e) {
        log_debug(">>> LP re-solve error:", e$message, level = "ERROR")
        return(NULL)
      })
      
      if (is.null(solution) || solution$status != 0) {
        log_debug(">>> No feasible solution with correlation constraints", level = "WARN")
        return(NULL)
      }
    }
  }
  
  # Extract selected players
  lineup <- available[solution$solution == 1, ] %>%
    mutate(
      projection = .data[[projection_col]],
      value = projection / salary,
      slot = paste0("G", row_number())
    )
  
  # Apply correlation adjustments to final projections for display
  if (length(corr_rules) > 0) {
    selected_names <- lineup$player_name
    for (rule in corr_rules) {
      if (rule$trigger %in% selected_names) {
        if (length(rule$boost_targets) > 0 && rule$boost_pct > 0) {
          lineup <- lineup %>%
            mutate(projection = if_else(
              player_name %in% rule$boost_targets,
              projection * (1 + rule$boost_pct / 100),
              projection
            ))
        }
        if (length(rule$dock_targets) > 0 && rule$dock_pct > 0) {
          lineup <- lineup %>%
            mutate(projection = if_else(
              player_name %in% rule$dock_targets,
              projection * (1 - rule$dock_pct / 100),
              projection
            ))
        }
      }
    }
  }
  
  lineup <- lineup %>%
    select(player_id, player_name, salary, projection, value, slot, headshot_url,
           any_of(c("median", "ceiling", "ownership", "cut_odds"))) %>%
    arrange(desc(projection))
  
  log_debug(">>> Optimal lineup:", sum(lineup$projection), "pts,",
            sum(lineup$salary), "salary", level = "INFO")
  
  lineup
}

#' Generate multiple classic lineups with variance and correlation rules
#' 
#' @param players Data frame with player data
#' @param projection_col Projection column to use
#' @param num_lineups Number of unique lineups
#' @param variance_pct Percentage variance (e.g., 15 for Â±15%)
#' @param salary_cap Maximum salary
#' @param locked_players Players to lock in
#' @param excluded_players Players to exclude
#' @param min_unique Minimum different players between lineups
#' @param corr_rules List of correlation rules
#' @return List of lineup data frames
generate_golf_classic_lineups <- function(players,
                                          projection_col = "blended",
                                          num_lineups = 10,
                                          variance_pct = 15,
                                          salary_cap = 100,
                                          locked_players = NULL,
                                          excluded_players = NULL,
                                          min_unique = 1,
                                          corr_rules = list()) {
  
  log_debug("generate_golf_classic_lineups() called", level = "INFO")
  log_debug("  num_lineups:", num_lineups, "variance_pct:", variance_pct, level = "INFO")
  log_debug("  corr_rules:", length(corr_rules), level = "INFO")
  
  lineups <- list()
  lineup_signatures <- character(0)
  previous_lineups <- list()
  
  # Filter once
  available <- players
  if (!is.null(excluded_players) && length(excluded_players) > 0) {
    available <- available %>% filter(!(player_name %in% excluded_players))
  }
  
  available <- available %>%
    filter(!is.na(.data[[projection_col]]) & .data[[projection_col]] > 0) %>%
    filter(!is.na(salary) & salary > 0)
  
  n <- nrow(available)
  if (n < 6) {
    log_debug("Not enough players:", n, level = "WARN")
    return(list())
  }
  
  base_projection <- available[[projection_col]]
  
  # Pre-generate variances
  set.seed(as.integer(Sys.time()))
  max_attempts <- num_lineups * 10
  
  # Main generation loop
  attempts <- 0
  
  while (length(lineups) < num_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Apply variance
    if (variance_pct > 0) {
      variance_factors <- runif(n, 1 - variance_pct/100, 1 + variance_pct/100)
      varied_proj <- base_projection * variance_factors
    } else {
      varied_proj <- base_projection
    }
    
    # Build constraint matrix
    constraint_matrix <- rbind(rep(1, n), available$salary)
    constraint_dirs <- c("==", "<=")
    constraint_rhs <- c(6, salary_cap)
    
    # Add locked player constraints
    if (!is.null(locked_players) && length(locked_players) > 0) {
      for (locked_player in locked_players) {
        if (locked_player %in% available$player_name) {
          constraint_matrix <- rbind(
            constraint_matrix,
            as.numeric(available$player_name == locked_player)
          )
          constraint_dirs <- c(constraint_dirs, "==")
          constraint_rhs <- c(constraint_rhs, 1)
        }
      }
    }
    
    # Add uniqueness constraints from previous lineups
    for (prev_lineup in previous_lineups) {
      overlap_vector <- as.numeric(available$player_name %in% prev_lineup)
      constraint_matrix <- rbind(constraint_matrix, overlap_vector)
      constraint_dirs <- c(constraint_dirs, "<=")
      constraint_rhs <- c(constraint_rhs, 6 - min_unique)
    }
    
    # First solve
    result <- tryCatch({
      lpSolve::lp(
        direction = "max",
        objective.in = varied_proj,
        const.mat = constraint_matrix,
        const.dir = constraint_dirs,
        const.rhs = constraint_rhs,
        all.bin = TRUE
      )
    }, error = function(e) NULL)
    
    if (is.null(result) || result$status != 0) next
    if (sum(result$solution) != 6) next
    
    selected_indices <- which(result$solution == 1)
    selected_names <- available$player_name[selected_indices]
    
    # Check for correlation rules
    if (length(corr_rules) > 0) {
      trigger_matched_rules <- purrr::keep(corr_rules, ~ .x$trigger %in% selected_names)
      
      if (length(trigger_matched_rules) > 0) {
        # Apply boosts/docks to varied projections
        boosted_proj <- varied_proj
        
        for (rule in trigger_matched_rules) {
          if (length(rule$boost_targets) > 0 && rule$boost_pct > 0) {
            boost_mask <- available$player_name %in% rule$boost_targets
            boosted_proj[boost_mask] <- boosted_proj[boost_mask] * (1 + rule$boost_pct / 100)
          }
          if (length(rule$dock_targets) > 0 && rule$dock_pct > 0) {
            dock_mask <- available$player_name %in% rule$dock_targets
            boosted_proj[dock_mask] <- boosted_proj[dock_mask] * (1 - rule$dock_pct / 100)
          }
        }
        
        # Force trigger players
        forced_triggers <- unique(sapply(trigger_matched_rules, function(r) r$trigger))
        force_mat <- constraint_matrix
        force_dirs <- constraint_dirs
        force_rhs <- constraint_rhs
        
        for (trigger in forced_triggers) {
          force_mat <- rbind(force_mat, as.numeric(available$player_name == trigger))
          force_dirs <- c(force_dirs, "==")
          force_rhs <- c(force_rhs, 1)
        }
        
        # Re-solve with boosted projections
        result <- tryCatch({
          lpSolve::lp(
            direction = "max",
            objective.in = boosted_proj,
            const.mat = force_mat,
            const.dir = force_dirs,
            const.rhs = force_rhs,
            all.bin = TRUE
          )
        }, error = function(e) NULL)
        
        if (is.null(result) || result$status != 0) next
        if (sum(result$solution) != 6) next
        
        selected_indices <- which(result$solution == 1)
        selected_names <- available$player_name[selected_indices]
      }
    }
    
    # Check signature for uniqueness
    sig <- paste(sort(selected_names), collapse = "|")
    if (sig %in% lineup_signatures) next
    
    # Build lineup data frame
    lineup <- available[selected_indices, ] %>%
      mutate(
        projection = base_projection[selected_indices],
        value = projection / salary,
        slot = paste0("G", row_number())
      )
    
    # Apply correlation adjustments to final projections
    if (length(corr_rules) > 0) {
      for (rule in corr_rules) {
        if (rule$trigger %in% lineup$player_name) {
          if (length(rule$boost_targets) > 0 && rule$boost_pct > 0) {
            lineup <- lineup %>%
              mutate(projection = if_else(
                player_name %in% rule$boost_targets,
                projection * (1 + rule$boost_pct / 100),
                projection
              ))
          }
          if (length(rule$dock_targets) > 0 && rule$dock_pct > 0) {
            lineup <- lineup %>%
              mutate(projection = if_else(
                player_name %in% rule$dock_targets,
                projection * (1 - rule$dock_pct / 100),
                projection
              ))
          }
        }
      }
    }
    
    lineup <- lineup %>% arrange(desc(projection))
    
    lineups[[length(lineups) + 1]] <- lineup
    lineup_signatures <- c(lineup_signatures, sig)
    previous_lineups[[length(previous_lineups) + 1]] <- selected_names
  }
  
  log_debug("Generated", length(lineups), "lineups in", attempts, "attempts", level = "INFO")
  lineups
}

# =============================================================================
# SHOWDOWN OPTIMIZATION (CPT/VICE/FLEX with multipliers)
# =============================================================================

#' Optimize a showdown golf lineup using linear programming
#' 
#' @param players Data frame with player data
#' @param projection_col Projection column to use
#' @param salary_cap Maximum effective salary (after multipliers)
#' @param locked_slots Named list of locked slots (e.g., list(CPT = "Player Name"))
#' @param excluded_players Character vector of excluded names
#' @param corr_rules List of correlation rules
#' @return Data frame with optimal lineup or NULL
optimize_golf_showdown_lp <- function(players,
                                      projection_col = "blended",
                                      salary_cap = 100,
                                      locked_slots = list(),
                                      excluded_players = c(),
                                      corr_rules = list()) {
  
  log_debug("optimize_golf_showdown_lp() called", level = "DEBUG")
  log_debug("  projection_col:", projection_col, "salary_cap:", salary_cap, level = "DEBUG")
  
  # Filter excluded
  if (length(excluded_players) > 0) {
    players <- players %>% filter(!(player_name %in% excluded_players))
  }
  
  # Filter NA
  players <- players %>%
    filter(!is.na(.data[[projection_col]]) & .data[[projection_col]] > 0) %>%
    filter(!is.na(salary) & salary > 0)
  
  n <- nrow(players)
  if (n < 6) {
    log_debug("Not enough players:", n, level = "WARN")
    return(NULL)
  }
  
  base_proj <- players[[projection_col]]
  
  # Decision variables: [p1_cpt, p1_vice, p1_flex, p2_cpt, ...]
  n_vars <- n * 3
  
  # Objective: maximize effective projection
  objective <- numeric(n_vars)
  for (i in 1:n) {
    objective[(i-1)*3 + 1] <- base_proj[i] * 1.5   # CPT
    objective[(i-1)*3 + 2] <- base_proj[i] * 1.25  # VICE
    objective[(i-1)*3 + 3] <- base_proj[i]         # FLEX
  }
  
  # Build constraints
  constraint_list <- list()
  constraint_dirs <- c()
  constraint_rhs <- c()
  
  # 1. Salary constraint
  salary_constraint <- numeric(n_vars)
  for (i in 1:n) {
    base_salary <- players$salary[i]
    salary_constraint[(i-1)*3 + 1] <- base_salary * 1.5
    salary_constraint[(i-1)*3 + 2] <- base_salary * 1.25
    salary_constraint[(i-1)*3 + 3] <- base_salary
  }
  constraint_list[[1]] <- salary_constraint
  constraint_dirs <- c(constraint_dirs, "<=")
  constraint_rhs <- c(constraint_rhs, salary_cap)
  
  # 2-4. Slot constraints
  cpt_constraint <- numeric(n_vars)
  vice_constraint <- numeric(n_vars)
  flex_constraint <- numeric(n_vars)
  for (i in 1:n) {
    cpt_constraint[(i-1)*3 + 1] <- 1
    vice_constraint[(i-1)*3 + 2] <- 1
    flex_constraint[(i-1)*3 + 3] <- 1
  }
  constraint_list[[2]] <- cpt_constraint
  constraint_list[[3]] <- vice_constraint
  constraint_list[[4]] <- flex_constraint
  constraint_dirs <- c(constraint_dirs, "==", "==", "==")
  constraint_rhs <- c(constraint_rhs, 1, 1, 4)
  
  # 5. Player uniqueness
  for (i in 1:n) {
    player_constraint <- numeric(n_vars)
    player_constraint[(i-1)*3 + 1] <- 1
    player_constraint[(i-1)*3 + 2] <- 1
    player_constraint[(i-1)*3 + 3] <- 1
    constraint_list[[length(constraint_list) + 1]] <- player_constraint
    constraint_dirs <- c(constraint_dirs, "<=")
    constraint_rhs <- c(constraint_rhs, 1)
  }
  
  # 6. Locked slots
  for (slot_name in names(locked_slots)) {
    player_name <- locked_slots[[slot_name]]
    if (!is.null(player_name) && player_name != "") {
      player_idx <- which(players$player_name == player_name)
      if (length(player_idx) == 1) {
        lock_coef <- numeric(n_vars)
        if (slot_name == "CPT") lock_coef[(player_idx-1)*3 + 1] <- 1
        else if (slot_name == "VICE") lock_coef[(player_idx-1)*3 + 2] <- 1
        else if (grepl("^FLEX", slot_name)) lock_coef[(player_idx-1)*3 + 3] <- 1
        constraint_list[[length(constraint_list) + 1]] <- lock_coef
        constraint_dirs <- c(constraint_dirs, "==")
        constraint_rhs <- c(constraint_rhs, 1)
      }
    }
  }
  
  const_mat <- do.call(rbind, constraint_list)
  
  # Solve
  result <- tryCatch({
    lp(
      direction = "max",
      objective.in = objective,
      const.mat = const_mat,
      const.dir = constraint_dirs,
      const.rhs = constraint_rhs,
      all.bin = TRUE
    )
  }, error = function(e) {
    log_debug("LP error:", e$message, level = "ERROR")
    NULL
  })
  
  if (is.null(result) || result$status != 0) {
    log_debug("LP optimization failed", level = "WARN")
    return(NULL)
  }
  
  # Extract lineup
  solution <- result$solution
  lineup <- list()
  flex_count <- 0
  
  for (i in 1:n) {
    if (solution[(i-1)*3 + 1] == 1) {
      lineup[["CPT"]] <- players[i, ] %>%
        mutate(slot = "CPT", effective_salary = salary * 1.5, effective_projection = .data[[projection_col]] * 1.5)
    } else if (solution[(i-1)*3 + 2] == 1) {
      lineup[["VICE"]] <- players[i, ] %>%
        mutate(slot = "VICE", effective_salary = salary * 1.25, effective_projection = .data[[projection_col]] * 1.25)
    } else if (solution[(i-1)*3 + 3] == 1) {
      flex_count <- flex_count + 1
      slot_name <- paste0("FLEX", flex_count)
      lineup[[slot_name]] <- players[i, ] %>%
        mutate(slot = slot_name, effective_salary = salary, effective_projection = .data[[projection_col]])
    }
  }
  
  if (length(lineup) == 6) {
    lineup_df <- bind_rows(lineup) %>%
      arrange(match(slot, c("CPT", "VICE", "FLEX1", "FLEX2", "FLEX3", "FLEX4")))
    
    # Apply correlation rules to final projections
    if (length(corr_rules) > 0) {
      for (rule in corr_rules) {
        if (rule$trigger %in% lineup_df$player_name) {
          if (length(rule$boost_targets) > 0 && rule$boost_pct > 0) {
            lineup_df <- lineup_df %>%
              mutate(effective_projection = if_else(
                player_name %in% rule$boost_targets,
                effective_projection * (1 + rule$boost_pct / 100),
                effective_projection
              ))
          }
          if (length(rule$dock_targets) > 0 && rule$dock_pct > 0) {
            lineup_df <- lineup_df %>%
              mutate(effective_projection = if_else(
                player_name %in% rule$dock_targets,
                effective_projection * (1 - rule$dock_pct / 100),
                effective_projection
              ))
          }
        }
      }
    }
    
    log_debug("Optimal showdown lineup:", round(sum(lineup_df$effective_projection), 1), "pts", level = "INFO")
    return(lineup_df)
  }
  
  return(NULL)
}

#' Generate multiple showdown lineups with variance
generate_golf_showdown_lineups <- function(players,
                                           projection_col = "blended",
                                           num_lineups = 10,
                                           variance_pct = 15,
                                           salary_cap = 100,
                                           locked_slots = list(),
                                           excluded_players = c(),
                                           corr_rules = list()) {
  
  log_debug("generate_golf_showdown_lineups() called", level = "INFO")
  log_debug("  num_lineups:", num_lineups, "variance_pct:", variance_pct, level = "INFO")
  
  lineups <- list()
  lineup_signatures <- character(0)
  
  # Filter once
  if (length(excluded_players) > 0) {
    players <- players %>% filter(!(player_name %in% excluded_players))
  }
  
  players <- players %>%
    filter(!is.na(.data[[projection_col]]) & .data[[projection_col]] > 0) %>%
    filter(!is.na(salary) & salary > 0)
  
  n <- nrow(players)
  if (n < 6) return(list())
  
  n_vars <- n * 3
  base_proj <- players[[projection_col]]
  
  # Pre-build constraints
  salary_coef <- numeric(n_vars)
  cpt_coef <- numeric(n_vars)
  vice_coef <- numeric(n_vars)
  flex_coef <- numeric(n_vars)
  
  for (i in 1:n) {
    salary_coef[(i-1)*3 + 1] <- players$salary[i] * 1.5
    salary_coef[(i-1)*3 + 2] <- players$salary[i] * 1.25
    salary_coef[(i-1)*3 + 3] <- players$salary[i]
    cpt_coef[(i-1)*3 + 1] <- 1
    vice_coef[(i-1)*3 + 2] <- 1
    flex_coef[(i-1)*3 + 3] <- 1
  }
  
  player_coefs <- matrix(0, nrow = n, ncol = n_vars)
  for (i in 1:n) {
    player_coefs[i, (i-1)*3 + 1] <- 1
    player_coefs[i, (i-1)*3 + 2] <- 1
    player_coefs[i, (i-1)*3 + 3] <- 1
  }
  
  base_constraints <- rbind(salary_coef, cpt_coef, vice_coef, flex_coef, player_coefs)
  base_dirs <- c("<=", "==", "==", "==", rep("<=", n))
  base_rhs <- c(salary_cap, 1, 1, 4, rep(1, n))
  
  # Add locked constraints
  if (length(locked_slots) > 0) {
    for (slot_name in names(locked_slots)) {
      player_name <- locked_slots[[slot_name]]
      if (!is.null(player_name) && player_name != "") {
        player_idx <- which(players$player_name == player_name)
        if (length(player_idx) == 1) {
          lock_coef <- numeric(n_vars)
          if (slot_name == "CPT") lock_coef[(player_idx-1)*3 + 1] <- 1
          else if (slot_name == "VICE") lock_coef[(player_idx-1)*3 + 2] <- 1
          else if (grepl("^FLEX", slot_name)) lock_coef[(player_idx-1)*3 + 3] <- 1
          base_constraints <- rbind(base_constraints, lock_coef)
          base_dirs <- c(base_dirs, "==")
          base_rhs <- c(base_rhs, 1)
        }
      }
    }
  }
  
  # Main loop
  max_attempts <- num_lineups * 10
  attempts <- 0
  
  while (length(lineups) < num_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Apply variance
    if (variance_pct > 0) {
      variance_factors <- runif(n, 1 - variance_pct/100, 1 + variance_pct/100)
      varied_proj <- base_proj * variance_factors
    } else {
      varied_proj <- base_proj
    }
    
    objective <- numeric(n_vars)
    for (i in 1:n) {
      objective[(i-1)*3 + 1] <- varied_proj[i] * 1.5
      objective[(i-1)*3 + 2] <- varied_proj[i] * 1.25
      objective[(i-1)*3 + 3] <- varied_proj[i]
    }
    
    result <- tryCatch({
      lpSolve::lp(
        direction = "max",
        objective.in = objective,
        const.mat = base_constraints,
        const.dir = base_dirs,
        const.rhs = base_rhs,
        all.bin = TRUE
      )
    }, error = function(e) NULL)
    
    if (is.null(result) || result$status != 0) next
    
    solution <- result$solution
    selected_names <- character(0)
    selected_slots <- character(0)
    selected_indices <- integer(0)
    selected_mults <- numeric(0)
    
    for (i in 1:n) {
      if (solution[(i-1)*3 + 1] > 0.5) {
        selected_indices <- c(selected_indices, i)
        selected_slots <- c(selected_slots, "CPT")
        selected_mults <- c(selected_mults, 1.5)
      } else if (solution[(i-1)*3 + 2] > 0.5) {
        selected_indices <- c(selected_indices, i)
        selected_slots <- c(selected_slots, "VICE")
        selected_mults <- c(selected_mults, 1.25)
      } else if (solution[(i-1)*3 + 3] > 0.5) {
        selected_indices <- c(selected_indices, i)
        selected_slots <- c(selected_slots, "FLEX")
        selected_mults <- c(selected_mults, 1.0)
      }
    }
    
    if (length(selected_indices) != 6) next
    
    selected_names <- players$player_name[selected_indices]
    
    sig <- paste(sort(selected_names), collapse = "|")
    if (sig %in% lineup_signatures) next
    
    # Assign FLEX numbers
    flex_count <- 0
    for (j in seq_along(selected_slots)) {
      if (selected_slots[j] == "FLEX") {
        flex_count <- flex_count + 1
        selected_slots[j] <- paste0("FLEX", flex_count)
      }
    }
    
    lineup <- data.frame(
      player_id = players$player_id[selected_indices],
      player_name = selected_names,
      salary = players$salary[selected_indices],
      projection = base_proj[selected_indices],
      slot = selected_slots,
      effective_salary = players$salary[selected_indices] * selected_mults,
      effective_projection = base_proj[selected_indices] * selected_mults,
      stringsAsFactors = FALSE
    )
    
    for (col in c("median", "ceiling", "ownership", "cut_odds", "headshot_url")) {
      if (col %in% names(players)) lineup[[col]] <- players[[col]][selected_indices]
    }
    
    # Apply correlation rules
    if (length(corr_rules) > 0) {
      for (rule in corr_rules) {
        if (rule$trigger %in% lineup$player_name) {
          if (length(rule$boost_targets) > 0 && rule$boost_pct > 0) {
            lineup$effective_projection[lineup$player_name %in% rule$boost_targets] <-
              lineup$effective_projection[lineup$player_name %in% rule$boost_targets] * (1 + rule$boost_pct / 100)
          }
          if (length(rule$dock_targets) > 0 && rule$dock_pct > 0) {
            lineup$effective_projection[lineup$player_name %in% rule$dock_targets] <-
              lineup$effective_projection[lineup$player_name %in% rule$dock_targets] * (1 - rule$dock_pct / 100)
          }
        }
      }
    }
    
    slot_order <- c("CPT", "VICE", "FLEX1", "FLEX2", "FLEX3", "FLEX4")
    lineup <- lineup[order(match(lineup$slot, slot_order)), ]
    
    lineups[[length(lineups) + 1]] <- lineup
    lineup_signatures <- c(lineup_signatures, sig)
  }
  
  log_debug("Generated", length(lineups), "showdown lineups in", attempts, "attempts", level = "INFO")
  lineups
}

cat("Golf optimizer loaded: optimize_golf_classic_lp(), generate_golf_classic_lineups()\n")
cat("                       optimize_golf_showdown_lp(), generate_golf_showdown_lineups()\n")
cat("                       Supports: grouped rules, correlation rules, variance\n")