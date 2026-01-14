# =============================================================================
# Module: NFL Showdown
# 
# Single-game slate lineup building with CPT/VICE/FLEX format
# Roster: 1 CPT (1.5x pts, 1.5x salary), 1 VICE (1.25x pts, 1.25x salary), 4 FLEX
# =============================================================================

library(lpSolve)

# =============================================================================
# SHOWDOWN CONFIGURATION
# =============================================================================

NFL_SHOWDOWN_STRUCTURE <- list(
  slots = c("CPT", "VICE", "FLEX1", "FLEX2", "FLEX3", "FLEX4"),
  total_players = 6,
  
  # Multipliers for scoring and salary
  multipliers = list(
    CPT = list(points = 1.5, salary = 1.5),
    VICE = list(points = 1.25, salary = 1.25),
    FLEX = list(points = 1.0, salary = 1.0)
  ),
  
  # Display labels
  slot_labels = c(
    CPT = "CPT", VICE = "VICE",
    FLEX1 = "FLEX", FLEX2 = "FLEX", FLEX3 = "FLEX", FLEX4 = "FLEX"
  )
)

# Default salary cap for showdown
NFL_SHOWDOWN_SALARY_CAP <- 85

#' Get available showdown slates for a season and week
#' @param season Year
#' @param week Week number
#' @return Vector of slate identifiers (e.g., "showdown_sf_sea")
get_available_showdown_slates <- function(season, week) {
  log_debug("get_available_showdown_slates() for season:", season, "week:", week, level = "DEBUG")
  
  # Get the file prefix (handles both regular weeks and playoff rounds)
  week_prefix <- get_week_file_prefix(week)
  
  # Look for showdown files in the salary folder
  salary_dir <- sprintf("data/fanteam_salaries/%s", season)
  
  if (!dir.exists(salary_dir)) {
    log_debug("Salary directory not found:", salary_dir, level = "WARN")
    return(character(0))
  }
  
  # Pattern: week_X_showdown_TEAM1_TEAM2.csv or wild_card_showdown_TEAM1_TEAM2.csv
  pattern <- sprintf("^%s_showdown_.+\\.csv$", week_prefix)
  files <- list.files(salary_dir, pattern = pattern)
  
  if (length(files) == 0) {
    log_debug("No showdown files found for week", week, level = "DEBUG")
    return(character(0))
  }
  
  # Extract slate names from filenames
  slates <- gsub(sprintf("^%s_", week_prefix), "", files)
  slates <- gsub("\\.csv$", "", slates)
  
  log_debug("Found showdown slates:", paste(slates, collapse = ", "), level = "INFO")
  return(slates)
}

#' Get human-readable label for showdown slate
#' @param slate Slate identifier (e.g., "showdown_sf_sea")
#' @return Display label
get_showdown_slate_label <- function(slate) {
  # Extract team names from slate ID
  # Format: showdown_team1_team2
  parts <- strsplit(gsub("^showdown_", "", slate), "_")[[1]]
  
  if (length(parts) >= 2) {
    team1 <- toupper(parts[1])
    team2 <- toupper(parts[2])
    return(sprintf("%s @ %s", team1, team2))
  }
  
  # Fallback
  gsub("_", " ", tools::toTitleCase(gsub("showdown_", "", slate)))
}

#' Load showdown slate data
#' @param season Year
#' @param week Week number or playoff identifier
#' @param slate Showdown slate identifier
#' @return Data frame with player data including CPT/VICE salary adjustments
load_showdown_data <- function(season, week, slate) {
  log_debug("========================================", level = "INFO")
  log_debug("load_showdown_data() called", level = "INFO")
  log_debug("  Season:", season, level = "INFO")
  log_debug("  Week:", week, level = "INFO")
  log_debug("  Slate:", slate, level = "INFO")
  
  # Get the file prefix (handles both regular weeks and playoff rounds)
  week_prefix <- get_week_file_prefix(week)
  
  # Projection file (use regular week projections)
  proj_file <- NULL
  proj_paths_to_try <- c(
    sprintf("data/projections/%s/%s_projections.csv", season, week_prefix),
    sprintf("projections/%s_projections.csv", week_prefix)
  )
  
  for (path in proj_paths_to_try) {
    if (file.exists(path)) {
      proj_file <- path
      log_debug("  Found projection file:", path, level = "INFO")
      break
    }
  }
  
  if (is.null(proj_file)) {
    log_debug("Projections file not found!", level = "ERROR")
    return(NULL)
  }
  
  # Salary file (showdown-specific)
  salary_file <- sprintf("data/fanteam_salaries/%s/%s_%s.csv", season, week_prefix, slate)
  
  if (!file.exists(salary_file)) {
    log_debug("Showdown salary file not found:", salary_file, level = "ERROR")
    return(NULL)
  }
  
  log_debug("Using files:", level = "INFO")
  log_debug("  Projection:", proj_file, level = "INFO")
  log_debug("  Salary:", salary_file, level = "INFO")
  
  # Load projections
  projections <- tryCatch({
    read_csv(proj_file, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>% 
      clean_names() %>%
      {
        if ("pos" %in% names(.)) {
          select(., player, team, pos, full_ppr_proj, dk_ceiling) %>%
            rename(position = pos, full = full_ppr_proj, ceiling = dk_ceiling)
        } else {
          select(., player, team, position, full, dk_ceiling) %>%
            rename(ceiling = dk_ceiling)
        }
      } %>%
      filter(position != "K") %>%
      mutate(
        team = if_else(team == "LA", "LAR", team),
        player = if_else(player == "LA DST", "LAR DST", player),
        blended = (full + ceiling) / 2
      )
  }, error = function(e) {
    log_debug("Error reading projections:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(projections)) return(NULL)
  
  log_debug("Projections loaded:", nrow(projections), "rows", level = "INFO")
  
  # Load salaries
  salaries <- tryCatch({
    read_csv(salary_file, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>% 
      clean_names() %>% 
      filter(lineup != "refuted") %>%
      filter(position != "kicker") %>%
      mutate(name = str_remove(name, regex("\\s+Jr\\.?$", ignore_case = TRUE))) %>% 
      mutate(name = str_remove(name, regex("\\s+Sr\\.?$", ignore_case = TRUE))) %>% 
      mutate(player = paste0(f_name, " ", name)) %>% 
      rename(salary = price, team = club) %>% 
      select(player, team, position, salary) %>% 
      mutate(position = case_when(
        position == "quarterback" ~ "QB",
        position == "running_back" ~ "RB",
        position == "wide_receiver" ~ "WR",
        position == "tight_end" ~ "TE",
        position == "defense_special" ~ "DST",
        TRUE ~ position
      )) %>% 
      mutate(player = case_when(
        position == "DST" ~ paste0(team, " ", position),
        TRUE ~ player
      )) %>% 
      # Name corrections for matching
      mutate(player = case_when(
        player == "Amon-Ra St. Brown" ~ "Amon-Ra St Brown",
        player == "A.J. Brown" ~ "AJ Brown",
        player == "J.K. Dobbins" ~ "JK Dobbins",
        player == "Kenneth Walker III" ~ "Kenneth Walker",
        player == "Luther Burden III" ~ "Luther Burden",
        player == "DeMario Douglas" ~ "Demario Douglas",
        player == "Calvin Austin III" ~ "Calvin Austin",
        player == "Hollywood Brown" ~ "Marquise Brown",
        player == "KaVontae Turpin" ~ "Kavontae Turpin",
        player == "Ollie Gordon II" ~ "Ollie Gordon",
        player == "T.J. Hockenson" ~ "TJ Hockenson",
        TRUE ~ player
      ))
  }, error = function(e) {
    log_debug("Error reading salaries:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(salaries)) return(NULL)
  
  log_debug("Salaries loaded:", nrow(salaries), "rows", level = "INFO")
  
  # Get teams in this showdown
  showdown_teams <- unique(salaries$team)
  log_debug("Teams in showdown:", paste(showdown_teams, collapse = " vs "), level = "INFO")
  
  # Filter projections to showdown teams only
  projections_filtered <- projections %>%
    filter(team %in% showdown_teams)
  
  # Combine salaries with projections
  combined <- tryCatch({
    result <- left_join(salaries, projections_filtered, 
                        by = c("player", "team", "position")) %>% 
      mutate(
        blended = (full + ceiling) / 2,
        # Base salary - this is what's in the file
        base_salary = salary,
        # Calculate CPT and VICE salaries
        cpt_salary = salary * 1.5,
        vice_salary = salary * 1.25,
        # Calculate CPT and VICE projections (using blended = avg of full and ceiling)
        cpt_projection = blended * 1.5,
        vice_projection = blended * 1.25,
        # Value calculations
        value = blended / salary,
        cpt_value = cpt_projection / cpt_salary,
        vice_value = vice_projection / vice_salary,
        season = as.character(season),
        week = as.integer(week),
        slate = slate
      ) %>% 
      filter(!is.na(full))
    
    # Add headshots using the standard helper function
    if (nrow(result) > 0) {
      result <- add_headshot_info(result)
    }
    
    result
  }, error = function(e) {
    log_debug("Error combining data:", e$message, level = "ERROR")
    return(NULL)
  })
  
  log_debug("Combined data:", nrow(combined), "players", level = "INFO")
  return(combined)
}

#' Optimize showdown lineup using linear programming
#' Uses BLENDED projection (average of full and ceiling) as the optimization target
#' @param players Data frame with player data
#' @param salary_cap Maximum salary (from UI input)
#' @param locked_slots List with slot -> player mappings for locked players
#' @param excluded_players Vector of player names to exclude
#' @param adjustments List of player -> adjustment % mappings
#' @return Optimal lineup data frame with slot assignments
optimize_showdown_lineup_lp <- function(players, 
                                        salary_cap = 50,
                                        locked_slots = list(),
                                        excluded_players = c(),
                                        adjustments = list()) {
  
  log_debug("optimize_showdown_lineup_lp() called", level = "DEBUG")
  log_debug("  salary_cap:", salary_cap, level = "DEBUG")
  log_debug("  locked_slots:", length(locked_slots), level = "DEBUG")
  log_debug("  excluded_players:", length(excluded_players), level = "DEBUG")
  log_debug("  adjustments:", length(adjustments), level = "DEBUG")
  
  # Filter out excluded players
  if (length(excluded_players) > 0) {
    players <- players %>% filter(!player %in% excluded_players)
  }
  
  # Apply adjustments to create adjusted blended projection
  players <- players %>%
    mutate(
      adj_factor = sapply(player, function(p) 1 + (adjustments[[p]] %||% 0) / 100),
      adjusted_blended = blended * adj_factor
    )
  
  # Remove NA projections
  players <- players %>%
    filter(!is.na(adjusted_blended) & adjusted_blended > 0) %>%
    filter(!is.na(salary) & salary > 0)
  
  n <- nrow(players)
  if (n < 6) {
    log_debug("Not enough players for showdown lineup", level = "WARN")
    return(NULL)
  }
  
  # Decision variables: for each player, can be CPT, VICE, or FLEX
  # Structure: [player1_cpt, player1_vice, player1_flex, player2_cpt, ...]
  # Total vars = n * 3
  
  n_vars <- n * 3
  
  # Build objective function (maximize total projected points using ADJUSTED BLENDED)
  objective <- numeric(n_vars)
  for (i in 1:n) {
    base_proj <- players$adjusted_blended[i]
    objective[(i-1)*3 + 1] <- base_proj * 1.5   # CPT
    objective[(i-1)*3 + 2] <- base_proj * 1.25  # VICE
    objective[(i-1)*3 + 3] <- base_proj         # FLEX
  }
  
  # Constraint matrices
  constraint_list <- list()
  constraint_dirs <- c()
  constraint_rhs <- c()
  
  # 1. Salary constraint: sum of effective salaries <= cap
  salary_constraint <- numeric(n_vars)
  for (i in 1:n) {
    base_salary <- players$salary[i]
    salary_constraint[(i-1)*3 + 1] <- base_salary * 1.5   # CPT
    salary_constraint[(i-1)*3 + 2] <- base_salary * 1.25  # VICE
    salary_constraint[(i-1)*3 + 3] <- base_salary         # FLEX
  }
  constraint_list[[1]] <- salary_constraint
  constraint_dirs <- c(constraint_dirs, "<=")
  constraint_rhs <- c(constraint_rhs, salary_cap)
  
  # Count locked slots by type
  locked_cpt <- sum(names(locked_slots) == "CPT")
  locked_vice <- sum(names(locked_slots) == "VICE")
  locked_flex <- sum(grepl("^FLEX", names(locked_slots)))
  
  # 2. Exactly 1 CPT
  cpt_constraint <- numeric(n_vars)
  for (i in 1:n) {
    cpt_constraint[(i-1)*3 + 1] <- 1
  }
  constraint_list[[2]] <- cpt_constraint
  constraint_dirs <- c(constraint_dirs, "==")
  constraint_rhs <- c(constraint_rhs, 1)
  
  # 3. Exactly 1 VICE
  vice_constraint <- numeric(n_vars)
  for (i in 1:n) {
    vice_constraint[(i-1)*3 + 2] <- 1
  }
  constraint_list[[3]] <- vice_constraint
  constraint_dirs <- c(constraint_dirs, "==")
  constraint_rhs <- c(constraint_rhs, 1)
  
  # 4. Exactly 4 FLEX
  flex_constraint <- numeric(n_vars)
  for (i in 1:n) {
    flex_constraint[(i-1)*3 + 3] <- 1
  }
  constraint_list[[4]] <- flex_constraint
  constraint_dirs <- c(constraint_dirs, "==")
  constraint_rhs <- c(constraint_rhs, 4)
  
  # 5. Each player can only be in one slot (CPT + VICE + FLEX <= 1 for each player)
  for (i in 1:n) {
    player_constraint <- numeric(n_vars)
    player_constraint[(i-1)*3 + 1] <- 1  # CPT
    player_constraint[(i-1)*3 + 2] <- 1  # VICE
    player_constraint[(i-1)*3 + 3] <- 1  # FLEX
    constraint_list[[length(constraint_list) + 1]] <- player_constraint
    constraint_dirs <- c(constraint_dirs, "<=")
    constraint_rhs <- c(constraint_rhs, 1)
  }
  
  # 6. Locked player constraints
  if (length(locked_slots) > 0) {
    for (slot_name in names(locked_slots)) {
      player_name <- locked_slots[[slot_name]]
      if (!is.null(player_name) && player_name != "") {
        player_idx <- which(players$player == player_name)
        if (length(player_idx) == 1) {
          lock_constraint <- numeric(n_vars)
          
          if (slot_name == "CPT") {
            # Lock as CPT
            lock_constraint[(player_idx-1)*3 + 1] <- 1
            constraint_list[[length(constraint_list) + 1]] <- lock_constraint
            constraint_dirs <- c(constraint_dirs, "==")
            constraint_rhs <- c(constraint_rhs, 1)
          } else if (slot_name == "VICE") {
            # Lock as VICE
            lock_constraint[(player_idx-1)*3 + 2] <- 1
            constraint_list[[length(constraint_list) + 1]] <- lock_constraint
            constraint_dirs <- c(constraint_dirs, "==")
            constraint_rhs <- c(constraint_rhs, 1)
          } else if (grepl("^FLEX", slot_name)) {
            # Lock as FLEX (any FLEX slot)
            lock_constraint[(player_idx-1)*3 + 3] <- 1
            constraint_list[[length(constraint_list) + 1]] <- lock_constraint
            constraint_dirs <- c(constraint_dirs, "==")
            constraint_rhs <- c(constraint_rhs, 1)
          }
          
          log_debug("  Locked", player_name, "in slot", slot_name, level = "DEBUG")
        }
      }
    }
  }
  
  # Build constraint matrix
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
    log_debug("LP optimization failed with status:", result$status, level = "WARN")
    return(NULL)
  }
  
  # Extract lineup
  solution <- result$solution
  lineup <- list()
  
  flex_count <- 0
  for (i in 1:n) {
    if (solution[(i-1)*3 + 1] == 1) {
      # CPT
      lineup[["CPT"]] <- players[i, ] %>%
        mutate(
          slot = "CPT",
          effective_salary = salary * 1.5,
          effective_projection = adjusted_blended * 1.5
        )
    } else if (solution[(i-1)*3 + 2] == 1) {
      # VICE
      lineup[["VICE"]] <- players[i, ] %>%
        mutate(
          slot = "VICE",
          effective_salary = salary * 1.25,
          effective_projection = adjusted_blended * 1.25
        )
    } else if (solution[(i-1)*3 + 3] == 1) {
      # FLEX
      flex_count <- flex_count + 1
      slot_name <- paste0("FLEX", flex_count)
      lineup[[slot_name]] <- players[i, ] %>%
        mutate(
          slot = slot_name,
          effective_salary = salary,
          effective_projection = adjusted_blended
        )
    }
  }
  
  # Combine into data frame
  if (length(lineup) == 6) {
    lineup_df <- bind_rows(lineup) %>%
      arrange(match(slot, c("CPT", "VICE", "FLEX1", "FLEX2", "FLEX3", "FLEX4")))
    
    log_debug("Optimal lineup found with projection:", 
              round(sum(lineup_df$effective_projection), 1), level = "INFO")
    return(lineup_df)
  }
  
  return(NULL)
}

#' Generate multiple showdown lineups with variance - OPTIMIZED
generate_showdown_lineups_with_variance <- function(players,
                                                    num_lineups = 5,
                                                    variance_pct = 15,
                                                    salary_cap = 50,
                                                    adjustments = list(),
                                                    locked_slots = list(),
                                                    excluded_players = c(),
                                                    stacking_rules = list()) {
  
  
  log_debug("generate_showdown_lineups_with_variance() - OPTIMIZED", level = "INFO")
  log_debug("  num_lineups:", num_lineups, "variance_pct:", variance_pct, level = "INFO")
  
  lineups <- list()
  lineup_signatures <- character(0)
  
  # Filter excluded players once
  if (length(excluded_players) > 0) {
    players <- players[!players$player %in% excluded_players, ]
  }
  
  # Remove NA projections once
  players <- players[!is.na(players$blended) & players$blended > 0, ]
  players <- players[!is.na(players$salary) & players$salary > 0, ]
  
  n <- nrow(players)
  if (n < 6) return(list())
  
  n_vars <- n * 3
  
  # Pre-calculate adjustment factors ONCE (avoid sapply in loop)
  adj_factors <- rep(1, n)
  if (length(adjustments) > 0) {
    for (i in seq_len(n)) {
      adj <- adjustments[[players$player[i]]]
      if (!is.null(adj)) adj_factors[i] <- 1 + adj / 100
    }
  }
  base_projection <- players$blended * adj_factors
  
  # =========================================================================
  # BUILD CONSTRAINT MATRIX ONCE (this is the key optimization)
  # =========================================================================
  
  # Salary constraint coefficients
  salary_coef <- numeric(n_vars)
  for (i in 1:n) {
    salary_coef[(i-1)*3 + 1] <- players$salary[i] * 1.5
    salary_coef[(i-1)*3 + 2] <- players$salary[i] * 1.25
    salary_coef[(i-1)*3 + 3] <- players$salary[i]
  }
  
  # Slot constraints (CPT=1, VICE=1, FLEX=4)
  cpt_coef <- numeric(n_vars)
  vice_coef <- numeric(n_vars)
  flex_coef <- numeric(n_vars)
  for (i in 1:n) {
    cpt_coef[(i-1)*3 + 1] <- 1
    vice_coef[(i-1)*3 + 2] <- 1
    flex_coef[(i-1)*3 + 3] <- 1
  }
  
  # Player uniqueness constraints
  player_coefs <- matrix(0, nrow = n, ncol = n_vars)
  for (i in 1:n) {
    player_coefs[i, (i-1)*3 + 1] <- 1
    player_coefs[i, (i-1)*3 + 2] <- 1
    player_coefs[i, (i-1)*3 + 3] <- 1
  }
  
  # Build base constraint matrix
  base_constraints <- rbind(salary_coef, cpt_coef, vice_coef, flex_coef, player_coefs)
  base_dirs <- c("<=", "==", "==", "==", rep("<=", n))
  base_rhs <- c(salary_cap, 1, 1, 4, rep(1, n))
  
  # Add locked player constraints
  if (length(locked_slots) > 0) {
    for (slot_name in names(locked_slots)) {
      player_name <- locked_slots[[slot_name]]
      if (!is.null(player_name) && player_name != "") {
        player_idx <- which(players$player == player_name)
        if (length(player_idx) == 1) {
          lock_coef <- numeric(n_vars)
          if (slot_name == "CPT") {
            lock_coef[(player_idx-1)*3 + 1] <- 1
          } else if (slot_name == "VICE") {
            lock_coef[(player_idx-1)*3 + 2] <- 1
          } else if (grepl("^FLEX", slot_name)) {
            lock_coef[(player_idx-1)*3 + 3] <- 1
          }
          base_constraints <- rbind(base_constraints, lock_coef)
          base_dirs <- c(base_dirs, "==")
          base_rhs <- c(base_rhs, 1)
        }
      }
    }
  }
  
  # =========================================================================
  # FAST STACKING RULES CHECK (pre-compute indices)
  # =========================================================================
  check_stacking_rules <- function(lineup_players, cpt_player, rules) {
    if (length(rules) == 0) return(TRUE)
    for (rule in rules) {
      if (rule$cpt_player == cpt_player) {
        if (!any(rule$required_players %in% lineup_players)) return(FALSE)
      }
    }
    TRUE
  }
  
  # =========================================================================
  # PRE-GENERATE RANDOM VARIANCE
  # =========================================================================
  set.seed(as.integer(Sys.time()))
  max_attempts <- num_lineups * 50
  all_variances <- matrix(
    runif(n * max_attempts, 1 - variance_pct/100, 1 + variance_pct/100),
    nrow = n, ncol = max_attempts
  )
  
  # =========================================================================
  # MAIN LOOP - Optimized
  # =========================================================================
  attempts <- 0
  
  while (length(lineups) < num_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Apply variance to get varied projections
    varied_proj <- base_projection * all_variances[, attempts]
    
    # Build objective (only this changes per iteration)
    objective <- numeric(n_vars)
    for (i in 1:n) {
      objective[(i-1)*3 + 1] <- varied_proj[i] * 1.5
      objective[(i-1)*3 + 2] <- varied_proj[i] * 1.25
      objective[(i-1)*3 + 3] <- varied_proj[i]
    }
    
    # Solve LP
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
    
    # Extract solution
    solution <- result$solution
    selected_indices <- integer(0)
    selected_slots <- character(0)
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
    
    selected_players <- players$player[selected_indices]
    cpt_player <- selected_players[selected_slots == "CPT"]
    
    # Check stacking rules
    if (!check_stacking_rules(selected_players, cpt_player, stacking_rules)) next
    
    # Check for duplicates
    sig <- paste0(sort(selected_players), collapse = "|")
    if (sig %in% lineup_signatures) next
    
    # Assign FLEX numbers
    flex_count <- 0
    for (j in seq_along(selected_slots)) {
      if (selected_slots[j] == "FLEX") {
        flex_count <- flex_count + 1
        selected_slots[j] <- paste0("FLEX", flex_count)
      }
    }
    
    # Build result dataframe
    lineup <- data.frame(
      player = selected_players,
      position = players$position[selected_indices],
      team = players$team[selected_indices],
      salary = players$salary[selected_indices],
      blended = players$blended[selected_indices],
      ceiling = players$ceiling[selected_indices],
      headshot_url = players$headshot_url[selected_indices],
      team_bg_color = players$team_bg_color[selected_indices],
      slot = selected_slots,
      effective_salary = players$salary[selected_indices] * selected_mults,
      effective_projection = base_projection[selected_indices] * selected_mults,
      stringsAsFactors = FALSE
    )
    
    # Sort by slot order
    slot_order <- c("CPT", "VICE", "FLEX1", "FLEX2", "FLEX3", "FLEX4")
    lineup <- lineup[order(match(lineup$slot, slot_order)), ]
    
    lineups[[length(lineups) + 1]] <- lineup
    lineup_signatures <- c(lineup_signatures, sig)
  }
  
  log_debug("Generated", length(lineups), "lineups in", attempts, "attempts", level = "INFO")
  lineups
}

# =============================================================================
# SHOWDOWN MODULE UI
# =============================================================================

nfl_showdown_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("nfl_showdown_ui() called with id:", id, level = "INFO")
  
  # Get available seasons
  seasons <- get_available_seasons()
  
  if (length(seasons) > 0) {
    weeks <- get_available_weeks(seasons[1])
    showdown_slates <- if (length(weeks) > 0) {
      get_available_showdown_slates(seasons[1], weeks[1])
    } else {
      character(0)
    }
  } else {
    weeks <- c()
    showdown_slates <- c()
  }
  
  # Build choices
  season_choices <- if (length(seasons) > 0) {
    setNames(as.character(seasons), as.character(seasons))
  } else {
    c("No data" = "")
  }
  
  week_choices <- if (length(weeks) > 0) {
    setNames(weeks, sapply(weeks, get_week_label))
  } else {
    c("No weeks" = "")
  }
  
  slate_choices <- if (length(showdown_slates) > 0) {
    setNames(showdown_slates, sapply(showdown_slates, get_showdown_slate_label))
  } else {
    c("No showdown slates" = "")
  }
  
  tagList(
    # Enable shinyjs for button state management
    shinyjs::useShinyjs(),
    
    # Page header
    div(
      class = "page-header",
      tags$h2("NFL Showdown"),
      tags$p(class = "text-muted", "Build single-game showdown lineups with CPT and VICE captain roles")
    ),
    
    # ==========================================================================
    # SETTINGS CARD
    # ==========================================================================
    ui_card(
      title = "Settings",
      color = NFL_CARD_COLOR,
      
      # CSS for showdown-specific styling
      tags$style(HTML("
        .salary-cap-input input.form-control {
          height: 38px !important;
        }
        
        /* Position filter buttons - INACTIVE state (raised with shadow) */
        .btn-position-filter {
          padding: 6px 16px !important;
          font-size: 0.85rem !important;
          font-weight: 600 !important;
          border: 2px solid #3B3226 !important;
          border-radius: 6px !important;
          background: #ffffff !important;
          color: #3B3226 !important;
          box-shadow: 3px 3px 0px #3B3226 !important;
          transition: all 0.1s ease !important;
          position: relative !important;
          top: 0 !important;
          left: 0 !important;
          cursor: pointer !important;
          outline: none !important;
        }
        
        .btn-position-filter:hover:not(.active) {
          background: #f5f5f5 !important;
        }
        
        .btn-position-filter:focus {
          outline: none !important;
        }
        
        /* ACTIVE state - Match btn-primary (dusty mauve) */
        .btn-position-filter.active {
          background: #9B8A9E !important;
          color: #ffffff !important;
          border-color: #3B3226 !important;
          box-shadow: none !important;
          top: 3px !important;
          left: 3px !important;
        }
        
        .btn-position-filter.active:hover {
          background: #8A7A8D !important;
        }
        
        .btn-position-filter.active:focus {
          box-shadow: inset 0 2px 4px rgba(0,0,0,0.2) !important;
        }
      ")),
      
      fluidRow(
        column(3,
               selectInput(ns("season"), "Season",
                           choices = season_choices,
                           selected = if (length(seasons) > 0) as.character(seasons[1]) else NULL
               )
        ),
        column(3,
               selectInput(ns("week"), "Week",
                           choices = week_choices,
                           selected = if (length(weeks) > 0) as.character(weeks[1]) else NULL
               )
        ),
        column(3,
               selectInput(ns("slate"), "Showdown Slate",
                           choices = slate_choices,
                           selected = if (length(showdown_slates) > 0) showdown_slates[1] else NULL
               )
        ),
        column(3,
               div(
                 class = "salary-cap-input",
                 numericInput(ns("salary_cap"), "Salary Cap",
                              value = NFL_SHOWDOWN_SALARY_CAP, min = 30, max = 100, step = 0.5
                 )
               )
        )
      )
    ),
    
    tags$br(),
    
    # ==========================================================================
    # PROJECTIONS TABLE
    # ==========================================================================
    ui_card(
      title = "Player Projections",
      color = NFL_CARD_COLOR,
      
      # Scrollable container with fixed height
      div(
        style = "max-height: 420px; overflow-y: auto; border: 1px solid var(--border-light); border-radius: 6px;",
        uiOutput(ns("projections_table"))
      )
    ),
    
    tags$br(),
    
    # ==========================================================================
    # PROJECTION ADJUSTMENTS
    # ==========================================================================
    ui_card(
      title = "Projection Adjustments",
      color = NFL_CARD_COLOR,
      
      div(
        style = "margin-bottom: 0.75rem; font-size: 0.85rem; color: var(--text-muted);",
        "Boost or dock player projections as a percentage. Applied before multipliers."
      ),
      
      fluidRow(
        column(4,
               selectInput(ns("adj_position"), "Position",
                           choices = c("All" = "all", "QB", "RB", "WR", "TE", "DST"),
                           selected = "all"
               )
        ),
        column(8,
               selectizeInput(ns("adj_player"), "Select Player",
                              choices = c("Select player..." = ""),
                              selected = "",
                              options = list(placeholder = "Choose player to adjust...")
               )
        )
      ),
      
      fluidRow(
        column(6,
               numericInput(ns("adj_pct"), "Adjustment %",
                            value = 0, min = -50, max = 100, step = 5
               ),
               div(
                 style = "font-size: 0.75rem; color: var(--text-muted); margin-top: -0.5rem;",
                 "e.g. +10% boost or -15% dock"
               )
        ),
        column(6,
               div(
                 style = "padding-top: 1.65rem; display: flex; gap: 0.5rem;",
                 actionButton(ns("apply_adj"), "Apply", class = "btn-primary", style = "flex: 1;"),
                 actionButton(ns("clear_adj"), "Clear All", class = "btn-secondary", style = "flex: 1;")
               )
        )
      ),
      
      uiOutput(ns("adjustments_display"))
    ),
    
    tags$br(),
    
    # ==========================================================================
    # OPTIMAL LINEUP (with headshots and better styling)
    # ==========================================================================
    ui_card(
      title = "Optimal Lineup",
      color = NFL_CARD_COLOR,
      
      div(
        style = "margin-bottom: 1rem;",
        actionButton(ns("calc_optimal"), "Calculate Optimal Lineup",
                     class = "btn-primary", style = "width: 100%;")
      ),
      
      uiOutput(ns("optimal_lineup_display"))
    ),
    
    tags$br(),
    
    # ==========================================================================
    # BUILD YOUR LINEUP (styled like handbuild)
    # ==========================================================================
    ui_card(
      title = "Build Your Lineup",
      color = NFL_CARD_COLOR,
      
      fluidRow(
        # Left: Player Pool
        column(7,
               # Position filter tabs
               div(
                 style = "display: flex; gap: 0.5rem; margin-bottom: 0.75rem; flex-wrap: wrap;",
                 actionButton(ns("filter_all"), "ALL", class = "btn-position-filter active"),
                 actionButton(ns("filter_qb"), "QB", class = "btn-position-filter"),
                 actionButton(ns("filter_rb"), "RB", class = "btn-position-filter"),
                 actionButton(ns("filter_wr"), "WR", class = "btn-position-filter"),
                 actionButton(ns("filter_te"), "TE", class = "btn-position-filter"),
                 actionButton(ns("filter_dst"), "DST", class = "btn-position-filter")
               ),
               
               # Player pool header
               uiOutput(ns("player_pool_header")),
               
               # Player pool table
               div(
                 style = "max-height: 450px; overflow-y: auto; border: 1px solid var(--border-light); border-top: none; border-radius: 0 0 6px 6px;",
                 uiOutput(ns("player_pool"))
               )
        ),
        
        # Right: Lineup Slots
        column(5,
               div(
                 class = "showdown-lineup-builder",
                 
                 # Lineup summary
                 uiOutput(ns("lineup_summary")),
                 
                 # Slots
                 div(
                   style = "margin-top: 0.75rem;",
                   uiOutput(ns("lineup_slots_display"))
                 ),
                 
                 # Actions
                 div(
                   style = "margin-top: 1rem; display: flex; gap: 0.5rem;",
                   actionButton(ns("clear_lineup"), "Clear All", class = "btn-secondary", style = "flex: 1;"),
                   actionButton(ns("fill_optimal"), "Fill Optimal", class = "btn-primary", style = "flex: 1;")
                 )
               )
        )
      )
    ),
    
    tags$br(),
    
    # ==========================================================================
    # GENERATE LINEUPS
    # ==========================================================================
    ui_card(
      title = "Generate Lineups",
      color = NFL_CARD_COLOR,
      
      fluidRow(
        column(4,
               numericInput(ns("num_lineups"), "Number of Lineups",
                            value = 6, min = 1, max = 50, step = 1)
        ),
        column(4,
               numericInput(ns("variance_pct"), "Variance %",
                            value = 15, min = 0, max = 50, step = 5)
        ),
        column(4,
               div(
                 style = "padding-top: 1.65rem;",
                 actionButton(ns("generate_lineups"), "Generate",
                              class = "btn-primary", style = "width: 100%;")
               )
        )
      ),
      
      # Stacking Rules Section
      div(
        style = "margin-top: 1.25rem; padding-top: 1rem; border-top: 1px solid var(--bg-secondary);",
        
        div(
          style = "font-weight: 700; font-size: 0.9rem; margin-bottom: 0.75rem;",
          "Stacking Rules"
        ),
        
        div(
          style = "font-size: 0.8rem; color: var(--text-muted); margin-bottom: 0.75rem;",
          "Create rules like: If player X is CPT, include at least one of the specified teammates."
        ),
        
        # Add rule form
        fluidRow(
          column(3,
                 selectizeInput(ns("stack_if_player"), "If CPT is...",
                                choices = c("Select player..." = ""),
                                selected = "",
                                options = list(placeholder = "Select CPT...")
                 )
          ),
          column(4,
                 selectizeInput(ns("stack_own_team"), "Own Team",
                                choices = c(),
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "Select teammates...")
                 )
          ),
          column(4,
                 selectizeInput(ns("stack_opposition"), "Opposition",
                                choices = c(),
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "Select opponents...")
                 )
          ),
          column(1,
                 div(
                   style = "padding-top: 1.65rem;",
                   actionButton(ns("add_stack_rule"), icon("plus"), class = "btn-secondary", style = "width: 100%;")
                 )
          )
        ),
        
        div(
          style = "font-size: 0.75rem; color: var(--text-muted); margin-top: -0.5rem; margin-bottom: 0.75rem;",
          "Include at least one player from either dropdown when CPT is selected player."
        ),
        
        # Display active rules
        uiOutput(ns("stack_rules_display"))
      ),
      
      uiOutput(ns("generated_lineups_output"))
    )
  )
}

# =============================================================================
# SHOWDOWN MODULE SERVER
# =============================================================================

nfl_showdown_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("nfl_showdown_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      player_data = NULL,
      optimal_lineup = NULL,
      lineup_slots = list(
        CPT = NULL, VICE = NULL,
        FLEX1 = NULL, FLEX2 = NULL, FLEX3 = NULL, FLEX4 = NULL
      ),
      generated_lineups = NULL,
      locked_players_for_display = c(),
      projection_adjustments = list(),
      position_filter = "all",
      pool_sort = list(col = "ceiling", dir = "desc"),
      proj_sort_col = "ceiling",
      proj_sort_dir = "desc",
      stacking_rules = list(),
      lineups_page = 1,
      initialized = FALSE
    )
    
    # =========================================================================
    # UPDATE WEEKS WHEN SEASON CHANGES
    # =========================================================================
    observeEvent(input$season, {
      req(input$season, input$season != "", input$season != "No data")
      
      log_debug(">>> Season changed to:", input$season, level = "INFO")
      
      weeks <- get_available_weeks(input$season)
      
      if (length(weeks) > 0) {
        week_choices <- setNames(weeks, sapply(weeks, get_week_label))
        updateSelectInput(session, "week", choices = week_choices, selected = weeks[1])
      } else {
        updateSelectInput(session, "week", choices = c("No weeks" = ""), selected = NULL)
      }
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # UPDATE SLATES WHEN WEEK CHANGES
    # =========================================================================
    observeEvent(input$week, {
      req(input$season, input$week, input$season != "", input$week != "")
      
      log_debug(">>> Week changed to:", input$week, level = "INFO")
      
      slates <- get_available_showdown_slates(input$season, input$week)
      
      if (length(slates) > 0) {
        slate_choices <- setNames(slates, sapply(slates, get_showdown_slate_label))
        updateSelectInput(session, "slate", choices = slate_choices, selected = slates[1])
      } else {
        updateSelectInput(session, "slate", choices = c("No showdown slates" = ""), selected = NULL)
      }
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # LOAD DATA - Use observeEvent on slate changes
    # =========================================================================
    observeEvent(input$slate, {
      season <- input$season
      week <- input$week
      slate <- input$slate
      
      log_debug(">>> Data load triggered for slate:", slate, level = "DEBUG")
      
      # Skip if no valid slate selected
      if (is.null(slate) || slate == "" || slate == "No showdown slates") {
        log_debug(">>> No valid slate selected, skipping load", level = "DEBUG")
        rv$player_data <- NULL
        return()
      }
      
      req(season, week)
      req(season != "", week != "")
      
      log_debug(">>> LOADING SHOWDOWN DATA", level = "INFO")
      log_debug(">>>   Season:", season, level = "INFO")
      log_debug(">>>   Week:", week, level = "INFO")
      log_debug(">>>   Slate:", slate, level = "INFO")
      
      tryCatch({
        data <- load_showdown_data(season, week, slate)
        
        if (!is.null(data) && nrow(data) > 0) {
          rv$player_data <- data
          log_debug(">>> Loaded", nrow(data), "players", level = "INFO")
          
          # Reset state
          rv$optimal_lineup <- NULL
          rv$generated_lineups <- NULL
          rv$lineup_slots <- list(
            CPT = NULL, VICE = NULL,
            FLEX1 = NULL, FLEX2 = NULL, FLEX3 = NULL, FLEX4 = NULL
          )
        } else {
          log_debug(">>> No data returned", level = "WARN")
          rv$player_data <- NULL
        }
        
        rv$initialized <- TRUE
        
      }, error = function(e) {
        log_debug(">>> Error loading data:", e$message, level = "ERROR")
        rv$player_data <- NULL
      })
    }, ignoreNULL = TRUE, ignoreInit = FALSE)
    
    # =========================================================================
    # POSITION FILTER BUTTONS
    # =========================================================================
    
    # Helper to update button active states via shinyjs
    update_position_buttons <- function(active_pos) {
      positions <- c("all", "QB", "RB", "WR", "TE", "DST")
      for (pos in positions) {
        btn_id <- paste0("filter_", tolower(pos))
        if (tolower(pos) == tolower(active_pos)) {
          shinyjs::addClass(id = btn_id, class = "active")
        } else {
          shinyjs::removeClass(id = btn_id, class = "active")
        }
      }
    }
    
    observeEvent(input$filter_all, { 
      rv$position_filter <- "all"
      update_position_buttons("all")
    }, ignoreInit = TRUE)
    observeEvent(input$filter_qb, { 
      rv$position_filter <- "QB"
      update_position_buttons("QB")
    }, ignoreInit = TRUE)
    observeEvent(input$filter_rb, { 
      rv$position_filter <- "RB"
      update_position_buttons("RB")
    }, ignoreInit = TRUE)
    observeEvent(input$filter_wr, { 
      rv$position_filter <- "WR"
      update_position_buttons("WR")
    }, ignoreInit = TRUE)
    observeEvent(input$filter_te, { 
      rv$position_filter <- "TE"
      update_position_buttons("TE")
    }, ignoreInit = TRUE)
    observeEvent(input$filter_dst, { 
      rv$position_filter <- "DST"
      update_position_buttons("DST")
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # PROJECTION ADJUSTMENTS
    # =========================================================================
    
    # Update player dropdown based on position filter
    observe({
      req(rv$player_data)
      
      data <- rv$player_data
      adjustments <- rv$projection_adjustments
      
      if (!is.null(input$adj_position) && input$adj_position != "all") {
        data <- data %>% filter(position == input$adj_position)
      }
      
      data <- data %>% arrange(desc(blended))
      
      if (nrow(data) == 0) {
        choices <- c("No players" = "")
      } else {
        labels <- sapply(1:nrow(data), function(i) {
          player_name <- data$player[i]
          adj_pct <- adjustments[[player_name]]
          adj_str <- if (!is.null(adj_pct) && adj_pct != 0) {
            sprintf(" [%+.0f%%]", adj_pct)
          } else ""
          sprintf("%s (%s) - %.1f pts%s", 
                  player_name, data$position[i], data$blended[i], adj_str)
        })
        choices <- setNames(data$player, labels)
        choices <- c("Select player..." = "", choices)
      }
      
      updateSelectizeInput(session, "adj_player", choices = choices)
    })
    
    # Apply adjustment
    observeEvent(input$apply_adj, {
      req(input$adj_player != "")
      
      player_name <- input$adj_player
      adj_pct <- input$adj_pct %||% 0
      
      log_debug(">>> Applying adjustment:", player_name, "=", adj_pct, "%", level = "INFO")
      
      if (adj_pct == 0) {
        rv$projection_adjustments[[player_name]] <- NULL
      } else {
        rv$projection_adjustments[[player_name]] <- adj_pct
      }
      
      updateSelectizeInput(session, "adj_player", selected = "")
      updateNumericInput(session, "adj_pct", value = 0)
    })
    
    # Clear all adjustments
    observeEvent(input$clear_adj, {
      rv$projection_adjustments <- list()
    })
    
    # Display adjustments
    # =========================================================================
    # PROJECTIONS TABLE
    # =========================================================================
    output$projections_table <- renderUI({
      req(rv$player_data)
      
      # Get sort settings
      sort_col <- rv$proj_sort_col %||% "ceiling"
      sort_dir <- rv$proj_sort_dir %||% "desc"
      
      data <- rv$player_data
      
      # Apply sorting
      if (sort_dir == "desc") {
        data <- data[order(-data[[sort_col]]), ]
      } else {
        data <- data[order(data[[sort_col]]), ]
      }
      
      if (nrow(data) == 0) {
        return(div(
          style = "padding: 2rem; text-align: center; color: var(--text-muted);",
          "No players found"
        ))
      }
      
      # Sort indicator helper
      sort_indicator <- function(col) {
        if (sort_col == col) {
          if (sort_dir == "desc") " \u25BC" else " \u25B2"
        } else ""
      }
      
      # Header style helper
      header_style <- function(col, width) {
        base <- sprintf("text-align: center; padding: 0.5rem; width: %s; font-weight: 700; font-size: 0.7rem; text-transform: uppercase; cursor: pointer;", width)
        if (sort_col == col) {
          paste0(base, " color: var(--accent-plum);")
        } else {
          paste0(base, " color: var(--text-muted);")
        }
      }
      
      div(
        style = "overflow-x: auto;",
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 0.85rem;",
          
          # Header
          tags$thead(
            style = "position: sticky; top: 0; background: var(--bg-secondary); z-index: 1;",
            tags$tr(
              tags$th(style = "text-align: left; padding: 0.5rem 0.75rem; font-weight: 700; font-size: 0.7rem; text-transform: uppercase; color: var(--text-muted); width: 180px;", "Player"),
              tags$th(
                style = header_style("salary", "90px"),
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("proj_sort_salary")),
                HTML(paste0("Salary", sort_indicator("salary")))
              ),
              tags$th(
                style = header_style("full", "80px"),
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("proj_sort_full")),
                HTML(paste0("Proj", sort_indicator("full")))
              ),
              tags$th(
                style = header_style("ceiling", "80px"),
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("proj_sort_ceiling")),
                HTML(paste0("Ceil", sort_indicator("ceiling")))
              ),
              tags$th(
                style = header_style("blended", "85px"),
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("proj_sort_blended")),
                HTML(paste0("Blend", sort_indicator("blended")))
              ),
              tags$th(
                style = header_style("value", "75px"),
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("proj_sort_value")),
                HTML(paste0("Value", sort_indicator("value")))
              )
            )
          ),
          
          # Body
          tags$tbody(
            lapply(1:nrow(data), function(i) {
              row <- data[i, ]
              tags$tr(
                style = if (i %% 2 == 0) "background: var(--bg-tertiary);" else "",
                
                # Player cell with headshot
                tags$td(
                  style = "padding: 0.4rem 0.75rem;",
                  div(
                    style = "display: flex; align-items: center; gap: 0.5rem;",
                    create_headshot_html(row$headshot_url, row$team_bg_color, "tiny", row$position, row$team),
                    div(
                      div(style = "font-weight: 600; font-size: 0.85rem;", row$player),
                      div(
                        style = "font-size: 0.75rem; color: var(--text-muted);",
                        HTML(sprintf("<span style='font-weight: 600;'>%s</span> &middot; %s", row$position, row$team))
                      )
                    )
                  )
                ),
                
                # Salary
                tags$td(style = "text-align: center; padding: 0.4rem; font-weight: 600;", sprintf("$%.1f", row$salary)),
                
                # Projection
                tags$td(style = "text-align: center; padding: 0.4rem;", sprintf("%.1f", row$full)),
                
                # Ceiling
                tags$td(style = "text-align: center; padding: 0.4rem; color: var(--accent-teal); font-weight: 600;", sprintf("%.1f", row$ceiling)),
                
                # Blended
                tags$td(style = "text-align: center; padding: 0.4rem;", sprintf("%.1f", row$blended)),
                
                # Value
                tags$td(style = "text-align: center; padding: 0.4rem;", sprintf("%.2f", row$value))
              )
            })
          )
        )
      )
    })
    
    # Projections table sort observers
    observeEvent(input$proj_sort_salary, {
      if (rv$proj_sort_col == "salary") {
        rv$proj_sort_dir <- if (rv$proj_sort_dir == "desc") "asc" else "desc"
      } else {
        rv$proj_sort_col <- "salary"
        rv$proj_sort_dir <- "desc"
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$proj_sort_full, {
      if (rv$proj_sort_col == "full") {
        rv$proj_sort_dir <- if (rv$proj_sort_dir == "desc") "asc" else "desc"
      } else {
        rv$proj_sort_col <- "full"
        rv$proj_sort_dir <- "desc"
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$proj_sort_ceiling, {
      if (rv$proj_sort_col == "ceiling") {
        rv$proj_sort_dir <- if (rv$proj_sort_dir == "desc") "asc" else "desc"
      } else {
        rv$proj_sort_col <- "ceiling"
        rv$proj_sort_dir <- "desc"
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$proj_sort_blended, {
      if (rv$proj_sort_col == "blended") {
        rv$proj_sort_dir <- if (rv$proj_sort_dir == "desc") "asc" else "desc"
      } else {
        rv$proj_sort_col <- "blended"
        rv$proj_sort_dir <- "desc"
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$proj_sort_value, {
      if (rv$proj_sort_col == "value") {
        rv$proj_sort_dir <- if (rv$proj_sort_dir == "desc") "asc" else "desc"
      } else {
        rv$proj_sort_col <- "value"
        rv$proj_sort_dir <- "desc"
      }
    }, ignoreInit = TRUE)
    
    output$adjustments_display <- renderUI({
      adjustments <- rv$projection_adjustments
      
      if (length(adjustments) == 0) {
        return(div(
          style = "text-align: center; padding: 0.5rem; color: var(--text-muted); font-size: 0.85rem; font-style: italic;",
          "No adjustments applied"
        ))
      }
      
      adj_df <- tibble(
        player = names(adjustments),
        adj_pct = unlist(adjustments)
      ) %>% arrange(desc(adj_pct))
      
      div(
        style = "margin-top: 0.75rem; padding-top: 0.75rem; border-top: 1px solid var(--bg-secondary);",
        div(
          style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted); margin-bottom: 0.5rem;",
          sprintf("Active Adjustments (%d)", nrow(adj_df))
        ),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 0.5rem;",
          lapply(1:nrow(adj_df), function(i) {
            adj_pct <- adj_df$adj_pct[i]
            
            div(
              style = sprintf("display: inline-flex; align-items: center; gap: 0.3rem; padding: 0.25rem 0.5rem; background: %s; border-radius: 4px; font-size: 0.8rem;",
                              if (adj_pct > 0) "rgba(139, 168, 134, 0.2)" else "rgba(232, 131, 121, 0.2)"),
              span(style = "font-weight: 600;", adj_df$player[i]),
              span(style = sprintf("font-weight: 700; color: %s;", 
                                   if (adj_pct > 0) "var(--accent-sage)" else "var(--accent-coral)"), 
                   sprintf("%+.0f%%", adj_pct)),
              tags$button(
                type = "button",
                class = "btn",
                style = "padding: 0.15rem 0.35rem; min-width: auto; font-size: 0.65rem; background-color: #5C4E3D; color: white; border: none; border-radius: 4px; margin-left: 0.25rem;",
                onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                                  ns("remove_adj"), gsub("'", "\\\\'", adj_df$player[i])),
                icon("times")
              )
            )
          })
        )
      )
    })
    
    # Remove individual adjustment
    observeEvent(input$remove_adj, {
      req(input$remove_adj)
      rv$projection_adjustments[[input$remove_adj]] <- NULL
    })
    
    # =========================================================================
    # STACKING RULES
    # =========================================================================
    
    # Update "If CPT is..." dropdown with all players
    observe({
      req(rv$player_data)
      
      data <- rv$player_data %>% 
        arrange(desc(ceiling))
      
      choices <- setNames(
        data$player,
        sprintf("%s (%s) - %.1f ceil", data$player, data$position, data$ceiling)
      )
      choices <- c("Select player..." = "", choices)
      
      updateSelectizeInput(session, "stack_if_player", choices = choices)
    })
    
    # Update "Own Team" and "Opposition" dropdowns based on selected CPT
    observeEvent(input$stack_if_player, {
      req(rv$player_data)
      
      if (is.null(input$stack_if_player) || input$stack_if_player == "") {
        updateSelectizeInput(session, "stack_own_team", choices = c())
        updateSelectizeInput(session, "stack_opposition", choices = c())
        return()
      }
      
      # Get the CPT player's team
      cpt_player <- input$stack_if_player
      cpt_data <- rv$player_data %>% filter(player == cpt_player)
      
      if (nrow(cpt_data) == 0) return()
      
      cpt_team <- cpt_data$team[1]
      
      # Get all teams in this slate
      all_teams <- unique(rv$player_data$team)
      opp_team <- setdiff(all_teams, cpt_team)
      
      # Own team players (excluding the CPT player)
      own_team <- rv$player_data %>%
        filter(team == cpt_team, player != cpt_player) %>%
        arrange(desc(ceiling))
      
      if (nrow(own_team) > 0) {
        own_choices <- setNames(
          own_team$player,
          sprintf("%s (%s) %.1f", own_team$player, own_team$position, own_team$ceiling)
        )
      } else {
        own_choices <- c()
      }
      
      # Opposition players
      opposition <- rv$player_data %>%
        filter(team %in% opp_team) %>%
        arrange(desc(ceiling))
      
      if (nrow(opposition) > 0) {
        opp_choices <- setNames(
          opposition$player,
          sprintf("%s (%s) %.1f", opposition$player, opposition$position, opposition$ceiling)
        )
      } else {
        opp_choices <- c()
      }
      
      updateSelectizeInput(session, "stack_own_team", choices = own_choices, selected = NULL)
      updateSelectizeInput(session, "stack_opposition", choices = opp_choices, selected = NULL)
    }, ignoreNULL = FALSE)
    
    # Add stacking rule
    observeEvent(input$add_stack_rule, {
      req(input$stack_if_player != "")
      
      # Combine selections from both dropdowns
      own_team_players <- input$stack_own_team %||% c()
      opposition_players <- input$stack_opposition %||% c()
      all_required <- c(own_team_players, opposition_players)
      
      if (length(all_required) == 0) {
        showNotification("Select at least one player from Own Team or Opposition", type = "warning", duration = 3)
        return()
      }
      
      rule_id <- paste0("rule_", length(rv$stacking_rules) + 1, "_", as.integer(Sys.time()))
      
      new_rule <- list(
        id = rule_id,
        cpt_player = input$stack_if_player,
        own_team = own_team_players,
        opposition = opposition_players,
        required_players = all_required
      )
      
      rv$stacking_rules[[rule_id]] <- new_rule
      
      log_debug(">>> Added stacking rule:", input$stack_if_player, "->", 
                paste(all_required, collapse = ", "), level = "INFO")
      
      # Reset inputs
      updateSelectizeInput(session, "stack_if_player", selected = "")
      updateSelectizeInput(session, "stack_own_team", selected = NULL)
      updateSelectizeInput(session, "stack_opposition", selected = NULL)
      
      showNotification("Stacking rule added", type = "message", duration = 2)
    })
    
    # Remove stacking rule
    observeEvent(input$remove_stack_rule, {
      req(input$remove_stack_rule)
      rv$stacking_rules[[input$remove_stack_rule]] <- NULL
    })
    
    # Clear all stacking rules
    observeEvent(input$clear_stack_rules, {
      rv$stacking_rules <- list()
      showNotification("All stacking rules cleared", type = "message", duration = 2)
    })
    
    # Display stacking rules
    output$stack_rules_display <- renderUI({
      rules <- rv$stacking_rules
      
      if (length(rules) == 0) {
        return(div(
          style = "text-align: center; padding: 0.5rem; color: var(--text-muted); font-size: 0.85rem; font-style: italic;",
          "No stacking rules defined"
        ))
      }
      
      div(
        style = "margin-top: 0.5rem;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem;",
          div(
            style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);",
            sprintf("Active Rules (%d)", length(rules))
          ),
          actionButton(ns("clear_stack_rules"), "Clear All", class = "btn-link", 
                       style = "padding: 0; font-size: 0.75rem; color: var(--accent-coral);")
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 0.4rem;",
          lapply(names(rules), function(rule_id) {
            rule <- rules[[rule_id]]
            
            # Format own team and opposition separately
            own_str <- if (length(rule$own_team) > 0) paste(rule$own_team, collapse = " / ") else NULL
            opp_str <- if (length(rule$opposition) > 0) paste(rule$opposition, collapse = " / ") else NULL
            
            div(
              style = "display: flex; align-items: center; gap: 0.5rem; padding: 0.4rem 0.6rem; background: var(--bg-tertiary); border-radius: 6px; border-left: 3px solid #FFD700;",
              
              # CPT badge
              div(
                style = "background: #FFD700; color: #8B6914; padding: 0.1rem 0.3rem; border-radius: 3px; font-size: 0.65rem; font-weight: 700;",
                "CPT"
              ),
              
              # CPT player name
              span(style = "font-weight: 600; font-size: 0.8rem;", rule$cpt_player),
              
              # Arrow
              span(style = "color: var(--text-muted); font-size: 0.9rem;", HTML("&rarr;")),
              
              # Required players with labels
              div(
                style = "flex: 1; font-size: 0.8rem;",
                if (!is.null(own_str)) {
                  span(
                    style = "color: var(--accent-teal);",
                    own_str
                  )
                },
                if (!is.null(own_str) && !is.null(opp_str)) {
                  span(style = "color: var(--text-muted); margin: 0 0.3rem;", "|")
                },
                if (!is.null(opp_str)) {
                  span(
                    style = "color: var(--accent-coral);",
                    opp_str
                  )
                }
              ),
              
              # Remove button (matching lineup slot X button style exactly)
              tags$button(
                type = "button",
                class = "btn",
                style = "padding: 0.15rem 0.35rem; min-width: auto; font-size: 0.65rem; background-color: #5C4E3D; color: white; border: none; border-radius: 4px;",
                onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                                  ns("remove_stack_rule"), rule_id),
                icon("times")
              )
            )
          })
        )
      )
    })
    
    # =========================================================================
    # CALCULATE OPTIMAL LINEUP
    # =========================================================================
    observeEvent(input$calc_optimal, {
      req(rv$player_data)
      
      log_debug(">>> Calculating optimal showdown lineup", level = "INFO")
      
      salary_cap <- input$salary_cap %||% NFL_SHOWDOWN_SALARY_CAP
      adjustments <- rv$projection_adjustments
      
      # Optimize using blended projection with adjustments
      optimal <- optimize_showdown_lineup_lp(
        players = rv$player_data,
        salary_cap = salary_cap,
        adjustments = adjustments
      )
      
      if (!is.null(optimal)) {
        rv$optimal_lineup <- optimal
        showNotification(
          sprintf("Optimal lineup: %.1f pts", sum(optimal$effective_projection)), 
          type = "message", duration = 3
        )
      } else {
        showNotification("Could not find valid lineup under salary cap", type = "warning", duration = 5)
      }
    })
    
    # Display optimal lineup (with headshots and larger fonts)
    output$optimal_lineup_display <- renderUI({
      lineup <- rv$optimal_lineup
      player_data <- rv$player_data
      
      if (is.null(lineup)) {
        return(div(
          style = "text-align: center; padding: 2rem; color: var(--text-muted);",
          "Click 'Calculate Optimal Lineup' to find the best lineup"
        ))
      }
      
      total_salary <- sum(lineup$effective_salary)
      total_projection <- sum(lineup$effective_projection)
      salary_cap <- input$salary_cap %||% NFL_SHOWDOWN_SALARY_CAP
      
      div(
        # Summary stats (larger)
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem; margin-bottom: 1rem; padding: 1rem; background: var(--bg-tertiary); border-radius: 8px;",
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Projection"),
            div(style = "font-size: 1.5rem; font-weight: 700; color: var(--accent-teal);", 
                sprintf("%.1f pts", total_projection))
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Salary"),
            div(style = "font-size: 1.5rem; font-weight: 700;", 
                sprintf("$%.1f", total_salary))
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Remaining"),
            div(style = sprintf("font-size: 1.5rem; font-weight: 700; color: %s;",
                                if (salary_cap - total_salary < 0) "var(--accent-coral)" else "var(--accent-sage)"), 
                sprintf("$%.1f", salary_cap - total_salary))
          )
        ),
        
        # Lineup rows with headshots
        div(
          style = "display: flex; flex-direction: column; gap: 0.5rem;",
          lapply(1:nrow(lineup), function(i) {
            p <- lineup[i, ]
            
            # Slot badge color
            slot_color <- case_when(
              p$slot == "CPT" ~ "#FFD700",
              p$slot == "VICE" ~ "#C0C0C0",
              TRUE ~ "#E5E9F0"
            )
            slot_text <- case_when(
              p$slot == "CPT" ~ "#8B6914",
              p$slot == "VICE" ~ "#4A4A4A",
              TRUE ~ "#3B3226"
            )
            
            div(
              style = "display: flex; align-items: center; padding: 0.5rem 0.75rem; background: white; border: 2px solid var(--text-primary); border-radius: 6px;",
              
              # Slot badge
              div(
                style = sprintf("min-width: 50px; padding: 0.3rem 0.5rem; background: %s; color: %s; border-radius: 4px; font-size: 0.75rem; font-weight: 700; text-align: center;", slot_color, slot_text),
                p$slot
              ),
              
              # Headshot
              div(
                style = "margin-left: 0.5rem;",
                create_headshot_html(
                  p$headshot_url, 
                  p$team_bg_color, 
                  "small", 
                  p$position, 
                  p$team
                )
              ),
              
              # Player info
              div(
                style = "flex: 1; margin-left: 0.75rem;",
                div(
                  style = "font-weight: 700; font-size: 1rem;",
                  p$player
                ),
                div(
                  style = "font-size: 0.8rem; color: var(--text-muted);",
                  HTML(sprintf("%s &middot; %s", p$position, p$team))
                )
              ),
              
              # Salary
              div(
                style = "text-align: right; margin-right: 1rem;",
                div(style = "font-size: 0.75rem; color: var(--text-muted);", "Salary"),
                div(style = "font-weight: 600; font-size: 0.95rem;", sprintf("$%.1f", p$effective_salary))
              ),
              
              # Projection
              div(
                style = "text-align: right;",
                div(style = "font-size: 0.75rem; color: var(--text-muted);", "Proj"),
                div(style = "font-weight: 700; font-size: 1rem; color: var(--accent-teal);", 
                    sprintf("%.1f", p$effective_projection))
              )
            )
          })
        )
      )
    })
    
    # =========================================================================
    # PLAYER POOL SORTING
    # =========================================================================
    observeEvent(input$pool_sort_salary, {
      if (rv$pool_sort$col == "salary") {
        rv$pool_sort$dir <- if (rv$pool_sort$dir == "desc") "asc" else "desc"
      } else {
        rv$pool_sort <- list(col = "salary", dir = "asc")
      }
    })
    
    observeEvent(input$pool_sort_blended, {
      if (rv$pool_sort$col == "blended") {
        rv$pool_sort$dir <- if (rv$pool_sort$dir == "desc") "asc" else "desc"
      } else {
        rv$pool_sort <- list(col = "blended", dir = "desc")
      }
    })
    
    observeEvent(input$pool_sort_ceiling, {
      if (rv$pool_sort$col == "ceiling") {
        rv$pool_sort$dir <- if (rv$pool_sort$dir == "desc") "asc" else "desc"
      } else {
        rv$pool_sort <- list(col = "ceiling", dir = "desc")
      }
    })
    
    observeEvent(input$pool_sort_value, {
      if (rv$pool_sort$col == "value") {
        rv$pool_sort$dir <- if (rv$pool_sort$dir == "desc") "asc" else "desc"
      } else {
        rv$pool_sort <- list(col = "value", dir = "desc")
      }
    })
    
    # =========================================================================
    # PLAYER POOL HEADER (sortable)
    # =========================================================================
    output$player_pool_header <- renderUI({
      sort_col <- rv$pool_sort$col
      sort_dir <- rv$pool_sort$dir
      
      # Helper for sort indicator
      sort_indicator <- function(col) {
        if (sort_col == col) {
          if (sort_dir == "desc") " â–¼" else " â–²"
        } else {
          ""
        }
      }
      
      # Header styles
      header_style <- "text-align: center; cursor: pointer; user-select: none;"
      active_style <- "text-align: center; cursor: pointer; user-select: none; color: var(--accent-plum); font-weight: 800;"
      
      div(
        style = "display: grid; grid-template-columns: 1fr 65px 65px 55px 50px 90px; gap: 0.25rem; padding: 0.5rem 0.75rem; background: var(--bg-secondary); border: 1px solid var(--border-light); border-radius: 6px 6px 0 0; font-weight: 700; font-size: 0.65rem; text-transform: uppercase; color: var(--text-muted);",
        span(style = "text-align: left;", "Player"),
        span(
          style = if (sort_col == "salary") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("pool_sort_salary")),
          HTML(paste0("Salary", sort_indicator("salary")))
        ),
        span(
          style = if (sort_col == "blended") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("pool_sort_blended")),
          HTML(paste0("Blend", sort_indicator("blended")))
        ),
        span(
          style = if (sort_col == "ceiling") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("pool_sort_ceiling")),
          HTML(paste0("Ceil", sort_indicator("ceiling")))
        ),
        span(
          style = if (sort_col == "value") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("pool_sort_value")),
          HTML(paste0("Val", sort_indicator("value")))
        ),
        span(style = "text-align: center;", "Add")
      )
    })
    
    # =========================================================================
    # PLAYER POOL RENDER
    # =========================================================================
    output$player_pool <- renderUI({
      req(rv$player_data)
      
      data <- rv$player_data
      pos_filter <- rv$position_filter
      sort_col <- rv$pool_sort$col
      sort_dir <- rv$pool_sort$dir
      adjustments <- rv$projection_adjustments
      stats <- lineup_stats()
      
      # Get locked players
      lineup_players <- unlist(lapply(rv$lineup_slots, function(slot) {
        if (!is.null(slot)) slot$player else NULL
      }))
      
      # Filter by position
      if (pos_filter != "all") {
        data <- data %>% filter(position == pos_filter)
      }
      
      # Apply adjustments for display
      data <- data %>%
        mutate(
          adj_pct = sapply(player, function(p) adjustments[[p]] %||% 0),
          adj_blended = blended * (1 + adj_pct / 100),
          is_locked = player %in% lineup_players,
          can_afford = salary <= stats$remaining_salary
        )
      
      # Sort (locked players at bottom)
      if (sort_dir == "desc") {
        data <- data %>% arrange(is_locked, desc(.data[[sort_col]]))
      } else {
        data <- data %>% arrange(is_locked, .data[[sort_col]])
      }
      
      if (nrow(data) == 0) {
        return(div(style = "text-align: center; padding: 2rem; color: var(--text-muted);", "No players found"))
      }
      
      div(
        lapply(1:nrow(data), function(i) {
          p <- data[i, ]
          is_locked <- p$is_locked
          can_afford <- p$can_afford
          escaped_name <- gsub("'", "\\\\'", p$player)
          
          # Determine row style
          row_style <- if (is_locked) {
            "opacity: 0.4; background: var(--bg-secondary);"
          } else if (!can_afford) {
            "opacity: 0.7;"
          } else {
            "background: white;"
          }
          
          div(
            style = sprintf("display: grid; grid-template-columns: 1fr 65px 65px 55px 50px 90px; gap: 0.25rem; padding: 0.4rem 0.75rem; align-items: center; border-bottom: 1px solid var(--bg-secondary); %s", row_style),
            
            # Player cell with headshot - Name on top, Position Â· Team below
            div(
              style = "display: flex; align-items: center; gap: 0.5rem; min-width: 0;",
              create_headshot_html(p$headshot_url, p$team_bg_color, "tiny", p$position, p$team),
              div(
                style = "min-width: 0; overflow: hidden;",
                div(
                  style = "font-weight: 600; font-size: 0.85rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
                  p$player
                ),
                div(
                  style = "font-size: 0.75rem; color: var(--text-muted);",
                  HTML(sprintf("%s &middot; %s", p$position, p$team))
                )
              )
            ),
            
            # Salary
            div(
              style = sprintf("text-align: center; font-weight: 600; font-size: 0.85rem; %s",
                              if (!can_afford && !is_locked) "color: var(--accent-coral);" else ""),
              sprintf("$%.1f", p$salary)
            ),
            
            # Blended
            div(
              style = "text-align: center; font-size: 0.85rem; font-weight: 600; color: var(--accent-teal);",
              sprintf("%.1f", p$adj_blended)
            ),
            
            # Ceiling
            div(
              style = "text-align: center; font-size: 0.85rem; color: var(--text-secondary);",
              sprintf("%.1f", p$ceiling)
            ),
            
            # Value
            div(
              style = "text-align: center; font-size: 0.85rem;",
              sprintf("%.2f", p$value)
            ),
            
            # Add buttons (CPT / VICE / FLEX)
            if (!is_locked) {
              div(
                style = "display: flex; gap: 0.3rem; justify-content: center;",
                tags$button(
                  type = "button",
                  class = "btn btn-sm",
                  style = "padding: 4px 10px; font-size: 0.75rem; background: #FFD700; border: 2px solid #3B3226; font-weight: 700; border-radius: 4px; box-shadow: 2px 2px 0px #3B3226; position: relative;",
                  onclick = sprintf("Shiny.setInputValue('%s', {player: '%s', role: 'CPT'}, {priority: 'event'})", 
                                    ns("add_player_click"), escaped_name),
                  "C"
                ),
                tags$button(
                  type = "button",
                  class = "btn btn-sm",
                  style = "padding: 4px 10px; font-size: 0.75rem; background: #C0C0C0; border: 2px solid #3B3226; font-weight: 700; border-radius: 4px; box-shadow: 2px 2px 0px #3B3226; position: relative;",
                  onclick = sprintf("Shiny.setInputValue('%s', {player: '%s', role: 'VICE'}, {priority: 'event'})", 
                                    ns("add_player_click"), escaped_name),
                  "V"
                ),
                tags$button(
                  type = "button",
                  class = "btn btn-sm",
                  style = "padding: 4px 10px; font-size: 0.75rem; background: #ffffff; border: 2px solid #3B3226; font-weight: 600; border-radius: 4px; box-shadow: 2px 2px 0px #3B3226; position: relative;",
                  onclick = sprintf("Shiny.setInputValue('%s', {player: '%s', role: 'FLEX'}, {priority: 'event'})", 
                                    ns("add_player_click"), escaped_name),
                  "F"
                )
              )
            } else {
              div(
                style = "text-align: center; font-size: 0.75rem; color: var(--accent-sage); font-weight: 600;",
                "Added"
              )
            }
          )
        })
      )
    })
    
    # Handle add to lineup clicks
    observeEvent(input$add_player_click, {
      req(input$add_player_click)
      req(rv$player_data)
      
      click_info <- input$add_player_click
      player_name <- click_info$player
      role <- click_info$role
      
      log_debug(">>> Adding player:", player_name, "as", role, level = "INFO")
      
      player_row <- rv$player_data %>% filter(player == player_name)
      if (nrow(player_row) == 0) return()
      
      # Check if already in lineup
      lineup_players <- unlist(lapply(rv$lineup_slots, function(slot) {
        if (!is.null(slot)) slot$player else NULL
      }))
      
      if (player_name %in% lineup_players) {
        showNotification("Player already in lineup", type = "warning", duration = 2)
        return()
      }
      
      # Check if slot is available
      if (role == "CPT" && !is.null(rv$lineup_slots$CPT)) {
        showNotification("CPT slot is full", type = "warning", duration = 2)
        return()
      }
      if (role == "VICE" && !is.null(rv$lineup_slots$VICE)) {
        showNotification("VICE slot is full", type = "warning", duration = 2)
        return()
      }
      
      # Create player slot data
      adjustments <- rv$projection_adjustments
      adj_factor <- 1 + (adjustments[[player_name]] %||% 0) / 100
      
      create_slot <- function(slot_name, multiplier) {
        list(
          player = player_row$player[1],
          position = player_row$position[1],
          team = player_row$team[1],
          salary = player_row$salary[1],
          blended = player_row$blended[1],
          adjusted_blended = player_row$blended[1] * adj_factor,
          headshot_url = player_row$headshot_url[1],
          team_bg_color = player_row$team_bg_color[1],
          effective_salary = player_row$salary[1] * multiplier,
          effective_projection = player_row$blended[1] * adj_factor * multiplier,
          slot = slot_name
        )
      }
      
      if (role == "CPT") {
        rv$lineup_slots$CPT <- create_slot("CPT", 1.5)
      } else if (role == "VICE") {
        rv$lineup_slots$VICE <- create_slot("VICE", 1.25)
      } else {
        # Find first empty FLEX slot
        for (slot_name in c("FLEX1", "FLEX2", "FLEX3", "FLEX4")) {
          if (is.null(rv$lineup_slots[[slot_name]])) {
            rv$lineup_slots[[slot_name]] <- create_slot(slot_name, 1.0)
            break
          }
        }
      }
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # LINEUP STATS
    # =========================================================================
    lineup_stats <- reactive({
      slots <- rv$lineup_slots
      salary_cap <- input$salary_cap %||% NFL_SHOWDOWN_SALARY_CAP
      
      filled <- sum(sapply(slots, function(s) !is.null(s)))
      total_salary <- sum(sapply(slots, function(s) if (!is.null(s)) s$effective_salary else 0))
      total_projection <- sum(sapply(slots, function(s) if (!is.null(s)) s$effective_projection else 0))
      
      list(
        filled = filled,
        total = 6,
        total_salary = total_salary,
        total_projection = total_projection,
        remaining_salary = salary_cap - total_salary
      )
    })
    
    # =========================================================================
    # LINEUP SUMMARY
    # =========================================================================
    output$lineup_summary <- renderUI({
      stats <- lineup_stats()
      salary_cap <- input$salary_cap %||% NFL_SHOWDOWN_SALARY_CAP
      remaining <- stats$remaining_salary
      empty_slots <- 6 - stats$filled
      avg_per_slot <- if (empty_slots > 0) remaining / empty_slots else 0
      
      div(
        style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 0.4rem; padding: 0.5rem; background: var(--bg-tertiary); border-radius: 6px; border: 2px solid var(--outline);",
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted);", "Players"),
          div(style = "font-weight: 700; font-size: 0.9rem;", sprintf("%d / %d", stats$filled, stats$total))
        ),
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted);", "Salary"),
          div(style = "font-weight: 700; font-size: 0.9rem;", sprintf("$%.1f", stats$total_salary))
        ),
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted);", "Remaining"),
          div(style = sprintf("font-weight: 700; font-size: 0.9rem; color: %s;", 
                              if (remaining < 0) "var(--accent-coral)" else "var(--accent-sage)"),
              sprintf("$%.1f", remaining))
        ),
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted);", "Projection"),
          div(style = "font-weight: 700; font-size: 0.9rem; color: var(--accent-teal);", 
              sprintf("%.1f", stats$total_projection))
        )
      )
    })
    
    # =========================================================================
    # LINEUP SLOTS DISPLAY (with headshots like handbuild)
    # =========================================================================
    output$lineup_slots_display <- renderUI({
      slots <- rv$lineup_slots
      slot_order <- c("CPT", "VICE", "FLEX1", "FLEX2", "FLEX3", "FLEX4")
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem;",
        lapply(slot_order, function(slot_name) {
          slot <- slots[[slot_name]]
          
          # Slot label and color
          display_label <- if (grepl("^FLEX", slot_name)) "FLX" else slot_name
          slot_color <- case_when(
            slot_name == "CPT" ~ "#FFD700",
            slot_name == "VICE" ~ "#C0C0C0",
            TRUE ~ "var(--text-primary)"
          )
          text_color <- case_when(
            slot_name == "CPT" ~ "#8B6914",
            slot_name == "VICE" ~ "#4A4A4A",
            TRUE ~ "white"
          )
          
          if (is.null(slot)) {
            # Empty slot
            div(
              style = "display: flex; align-items: center; padding: 0.35rem 0.5rem; background: var(--bg-secondary); border-radius: 4px;",
              div(
                style = sprintf("min-width: 40px; padding: 0.2rem 0.35rem; background: %s; color: %s; border-radius: 4px; font-size: 0.65rem; font-weight: 700; text-align: center;", slot_color, text_color),
                display_label
              ),
              span(style = "flex: 1; padding-left: 0.5rem; color: var(--text-muted); font-style: italic; font-size: 0.8rem;", "Empty")
            )
          } else {
            # Filled slot
            div(
              style = "display: flex; align-items: center; padding: 0.35rem 0.5rem; background: white; border: 2px solid var(--text-primary); border-radius: 4px;",
              
              # Slot badge
              div(
                style = sprintf("min-width: 40px; padding: 0.2rem 0.35rem; background: %s; color: %s; border-radius: 4px; font-size: 0.65rem; font-weight: 700; text-align: center;", slot_color, text_color),
                display_label
              ),
              
              # Headshot
              div(
                style = "margin-left: 0.4rem;",
                create_headshot_html(slot$headshot_url, slot$team_bg_color, "tiny", slot$position, slot$team)
              ),
              
              # Player info
              div(
                style = "flex: 1; padding-left: 0.4rem; min-width: 0;",
                div(style = "font-weight: 600; font-size: 0.8rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", slot$player),
                div(
                  style = "font-size: 0.7rem; color: var(--text-muted);",
                  HTML(sprintf("%s &middot; %s", slot$position, slot$team))
                )
              ),
              
              # Salary and projection
              div(
                style = "text-align: right; padding-right: 0.4rem;",
                div(style = "font-weight: 600; font-size: 0.75rem;", sprintf("$%.1f", slot$effective_salary)),
                div(style = "font-size: 0.7rem; color: var(--accent-teal); font-weight: 600;", 
                    sprintf("%.1f", slot$effective_projection))
              ),
              
              # Remove button
              actionButton(
                ns(paste0("remove_", slot_name)),
                icon("times"),
                class = "btn-secondary",
                style = "padding: 0.15rem 0.35rem; min-width: auto; font-size: 0.65rem;"
              )
            )
          }
        })
      )
    })
    
    # Handle remove buttons
    observeEvent(input$remove_CPT, { rv$lineup_slots$CPT <- NULL }, ignoreInit = TRUE)
    observeEvent(input$remove_VICE, { rv$lineup_slots$VICE <- NULL }, ignoreInit = TRUE)
    observeEvent(input$remove_FLEX1, { rv$lineup_slots$FLEX1 <- NULL }, ignoreInit = TRUE)
    observeEvent(input$remove_FLEX2, { rv$lineup_slots$FLEX2 <- NULL }, ignoreInit = TRUE)
    observeEvent(input$remove_FLEX3, { rv$lineup_slots$FLEX3 <- NULL }, ignoreInit = TRUE)
    observeEvent(input$remove_FLEX4, { rv$lineup_slots$FLEX4 <- NULL }, ignoreInit = TRUE)
    
    # Clear lineup
    observeEvent(input$clear_lineup, {
      rv$lineup_slots <- list(
        CPT = NULL, VICE = NULL,
        FLEX1 = NULL, FLEX2 = NULL, FLEX3 = NULL, FLEX4 = NULL
      )
    })
    
    # Fill with optimal
    observeEvent(input$fill_optimal, {
      req(rv$optimal_lineup)
      
      lineup <- rv$optimal_lineup
      adjustments <- rv$projection_adjustments
      
      # Reset slots
      new_slots <- list(
        CPT = NULL, VICE = NULL,
        FLEX1 = NULL, FLEX2 = NULL, FLEX3 = NULL, FLEX4 = NULL
      )
      
      for (i in 1:nrow(lineup)) {
        p <- lineup[i, ]
        slot_name <- p$slot
        adj_factor <- 1 + (adjustments[[p$player]] %||% 0) / 100
        
        new_slots[[slot_name]] <- list(
          player = p$player,
          position = p$position,
          team = p$team,
          salary = p$salary,
          blended = p$blended,
          adjusted_blended = p$blended * adj_factor,
          headshot_url = p$headshot_url,
          team_bg_color = p$team_bg_color,
          effective_salary = p$effective_salary,
          effective_projection = p$effective_projection,
          slot = slot_name
        )
      }
      
      rv$lineup_slots <- new_slots
      showNotification("Loaded optimal lineup", type = "message", duration = 2)
    })
    
    # =========================================================================
    # GENERATE LINEUPS
    # =========================================================================
    observeEvent(input$generate_lineups, {
      req(rv$player_data)
      
      log_debug(">>> Generating showdown lineups", level = "INFO")
      
      salary_cap <- input$salary_cap %||% NFL_SHOWDOWN_SALARY_CAP
      num_lineups <- input$num_lineups %||% 6
      variance_pct <- input$variance_pct %||% 15
      adjustments <- rv$projection_adjustments
      stacking_rules <- rv$stacking_rules
      
      # Get locked players from current lineup
      locked_slots <- list()
      for (slot_name in names(rv$lineup_slots)) {
        slot <- rv$lineup_slots[[slot_name]]
        if (!is.null(slot)) {
          locked_slots[[slot_name]] <- slot$player
        }
      }
      
      # Log locked slots
      if (length(locked_slots) > 0) {
        log_debug(">>> Locked slots:", paste(names(locked_slots), "=", unlist(locked_slots), collapse = ", "), level = "INFO")
      }
      
      withProgress(message = "Generating lineups...", value = 0, {
        lineups <- generate_showdown_lineups_with_variance(
          players = rv$player_data,
          num_lineups = num_lineups,
          variance_pct = variance_pct,
          salary_cap = salary_cap,
          adjustments = adjustments,
          locked_slots = locked_slots,
          stacking_rules = stacking_rules
        )
        
        incProgress(1)
        
        if (length(lineups) > 0) {
          # Store locked player names for display
          rv$locked_players_for_display <- unlist(locked_slots)
          rv$generated_lineups <- lineups
          
          if (length(lineups) < num_lineups) {
            showNotification(
              sprintf("Generated %d of %d requested lineups. Try relaxing stacking rules or adjusting salary cap.", 
                      length(lineups), num_lineups), 
              type = "warning", duration = 5
            )
          } else {
            showNotification(sprintf("Generated %d lineups", length(lineups)), type = "message", duration = 3)
          }
        } else {
          showNotification("Could not generate valid lineups", type = "warning", duration = 5)
        }
      })
    })
    
    # Pagination handlers
    observeEvent(input$prev_page, {
      if (rv$lineups_page > 1) {
        rv$lineups_page <- rv$lineups_page - 1
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$next_page, {
      total_pages <- ceiling(length(rv$generated_lineups) / 12)
      if (rv$lineups_page < total_pages) {
        rv$lineups_page <- rv$lineups_page + 1
      }
    }, ignoreInit = TRUE)
    
    # Reset page when new lineups generated
    observeEvent(rv$generated_lineups, {
      rv$lineups_page <- 1
    }, ignoreInit = TRUE)
    
    # Display generated lineups (in rows of 3 with headshots) - WITH PAGINATION
    output$generated_lineups_output <- renderUI({
      lineups <- rv$generated_lineups
      salary_cap <- input$salary_cap %||% NFL_SHOWDOWN_SALARY_CAP
      current_page <- rv$lineups_page
      
      if (is.null(lineups) || length(lineups) == 0) {
        return(NULL)
      }
      
      # Calculate projections and sort lineups by projection (highest first)
      lineup_projections <- sapply(lineups, function(lu) sum(lu$effective_projection))
      sorted_order <- order(lineup_projections, decreasing = TRUE)
      lineups <- lineups[sorted_order]
      lineup_projections <- lineup_projections[sorted_order]
      best_projection <- max(lineup_projections)
      
      # Pagination settings
      lineups_per_page <- 12  # 4 rows of 3
      total_pages <- ceiling(length(lineups) / lineups_per_page)
      current_page <- min(max(1, current_page), total_pages)
      
      start_idx <- (current_page - 1) * lineups_per_page + 1
      end_idx <- min(current_page * lineups_per_page, length(lineups))
      page_lineups <- lineups[start_idx:end_idx]
      
      div(
        style = "margin-top: 1.5rem;",
        
        # Summary stats
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem; margin-bottom: 1rem; padding: 1rem; background: var(--bg-tertiary); border-radius: 8px;",
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Lineups"),
            div(style = "font-size: 1.25rem; font-weight: 700;", length(lineups))
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Best Projection"),
            div(style = "font-size: 1.25rem; font-weight: 700; color: var(--accent-teal);", sprintf("%.1f", best_projection))
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Average"),
            div(style = "font-size: 1.25rem; font-weight: 700;", sprintf("%.1f", mean(lineup_projections)))
          )
        ),
        
        # Pagination controls (if more than 1 page)
        if (total_pages > 1) {
          div(
            style = "display: flex; justify-content: center; align-items: center; gap: 0.5rem; margin-bottom: 1rem;",
            if (current_page > 1) {
              actionButton(ns("prev_page"), icon("chevron-left"), class = "btn-sm btn-secondary")
            },
            span(style = "font-size: 0.85rem; color: var(--text-muted);", 
                 sprintf("Page %d of %d (showing %d-%d of %d)", current_page, total_pages, start_idx, end_idx, length(lineups))),
            if (current_page < total_pages) {
              actionButton(ns("next_page"), icon("chevron-right"), class = "btn-sm btn-secondary")
            }
          )
        },
        
        # Lineups grid - 3 columns
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
          
          lapply(seq_along(page_lineups), function(i) {
            global_idx <- start_idx + i - 1  # Actual lineup number
            lu <- page_lineups[[i]]
            total_salary <- sum(lu$effective_salary)
            total_projection <- sum(lu$effective_projection)
            delta <- total_projection - best_projection
            
            div(
              style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 0.75rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
              
              # Header
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px dashed var(--bg-secondary);",
                tags$span(
                  style = "font-weight: 800; text-transform: uppercase; font-size: 0.8rem;",
                  sprintf("Lineup %d", global_idx)
                ),
                div(
                  style = "display: flex; gap: 0.5rem; align-items: center;",
                  tags$span(style = "font-weight: 700; color: var(--accent-teal);", sprintf("%.1f", total_projection)),
                  tags$span(style = "font-size: 0.8rem; color: var(--text-muted);", sprintf("$%.1f", total_salary))
                )
              ),
              
              # Players with headshots
              div(
                style = "display: flex; flex-direction: column; gap: 0.3rem;",
                lapply(1:nrow(lu), function(j) {
                  p <- lu[j, ]
                  slot_color <- case_when(
                    p$slot == "CPT" ~ "#FFD700",
                    p$slot == "VICE" ~ "#C0C0C0",
                    TRUE ~ "#E5E9F0"
                  )
                  slot_label <- if (grepl("^FLEX", p$slot)) "F" else substr(p$slot, 1, 1)
                  
                  # Create very light team color for row background
                  team_color <- p$team_bg_color %||% "#E0E0E0"
                  # Convert hex to RGB and create very light version (10% opacity blend with white)
                  hex_to_light_bg <- function(hex) {
                    hex <- gsub("#", "", hex)
                    r <- strtoi(substr(hex, 1, 2), 16)
                    g <- strtoi(substr(hex, 3, 4), 16)
                    b <- strtoi(substr(hex, 5, 6), 16)
                    # Blend with white at 90% white, 10% color
                    r_light <- round(255 * 0.92 + r * 0.08)
                    g_light <- round(255 * 0.92 + g * 0.08)
                    b_light <- round(255 * 0.92 + b * 0.08)
                    sprintf("rgb(%d, %d, %d)", r_light, g_light, b_light)
                  }
                  row_bg <- tryCatch(hex_to_light_bg(team_color), error = function(e) "#F5F5F5")
                  
                  div(
                    style = sprintf("display: flex; align-items: center; gap: 0.4rem; padding: 0.25rem 0.4rem; background: %s; border-radius: 4px;", row_bg),
                    
                    # Slot badge (small)
                    div(
                      style = sprintf("min-width: 20px; padding: 0.1rem 0.25rem; background: %s; border-radius: 3px; font-size: 0.6rem; font-weight: 700; text-align: center;", slot_color),
                      slot_label
                    ),
                    
                    # Mini headshot - matching create_headshot_html pattern with shadow
                    div(
                      style = sprintf("width: 28px; height: 28px; border-radius: 50%%; background: %s; border: 1px solid var(--outline); box-shadow: 1px 1px 0 rgba(59,50,38,0.15); display: flex; align-items: center; justify-content: center; overflow: hidden; flex-shrink: 0;", 
                                      p$team_bg_color %||% "#E0E0E0"),
                      if (p$position == "DST") {
                        tags$img(
                          src = sprintf("nfl_logos/%s.webp", p$team),
                          style = "width: 24px; height: 24px; object-fit: contain; border-radius: 50%;",
                          onerror = "this.style.display='none'"
                        )
                      } else {
                        tags$img(
                          src = p$headshot_url %||% "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png",
                          style = "width: 24px; height: 24px; object-fit: cover; border-radius: 50%;",
                          onerror = "this.src='https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png'"
                        )
                      }
                    ),
                    
                    # Player name Â· Team Â· Position (position slightly heavier)
                    div(
                      style = "flex: 1; min-width: 0; overflow: hidden;",
                      tags$span(
                        style = "font-weight: 600; font-size: 0.75rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
                        HTML(sprintf("%s <span style='color: var(--text-muted); font-weight: 400;'>&middot; %s &middot;</span> <span style='color: var(--text-muted); font-weight: 600;'>%s</span>", 
                                     p$player, p$team, p$position))
                      )
                    ),
                    
                    # Projection
                    tags$span(
                      style = "font-size: 0.75rem; color: var(--accent-teal); font-weight: 600;",
                      sprintf("%.0f", p$effective_projection)
                    )
                  )
                })
              ),
              
              # Footer with Use button
              div(
                style = "margin-top: 0.75rem; display: flex; justify-content: flex-end;",
                tags$button(
                  type = "button",
                  class = "btn btn-sm btn-outline-primary",
                  style = "padding: 4px 12px; font-size: 0.75rem;",
                  onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("use_generated_click"), global_idx),
                  "Use This Lineup"
                )
              )
            )
          })
        )
      )
    })
    
    # Handle "Use" buttons for generated lineups
    observeEvent(input$use_generated_click, {
      req(rv$generated_lineups)
      
      i <- input$use_generated_click
      
      # Sort lineups by projection (same order as display)
      lineups <- rv$generated_lineups
      lineup_projections <- sapply(lineups, function(lu) sum(lu$effective_projection))
      sorted_order <- order(lineup_projections, decreasing = TRUE)
      sorted_lineups <- lineups[sorted_order]
      
      if (i < 1 || i > length(sorted_lineups)) return()
      
      lu <- sorted_lineups[[i]]
      adjustments <- rv$projection_adjustments
      
      new_slots <- list(
        CPT = NULL, VICE = NULL,
        FLEX1 = NULL, FLEX2 = NULL, FLEX3 = NULL, FLEX4 = NULL
      )
      
      for (j in 1:nrow(lu)) {
        p <- lu[j, ]
        slot_name <- p$slot
        adj_factor <- 1 + (adjustments[[p$player]] %||% 0) / 100
        
        new_slots[[slot_name]] <- list(
          player = p$player,
          position = p$position,
          team = p$team,
          salary = p$salary,
          blended = p$blended,
          adjusted_blended = p$blended * adj_factor,
          headshot_url = p$headshot_url,
          team_bg_color = p$team_bg_color,
          effective_salary = p$effective_salary,
          effective_projection = p$effective_projection,
          slot = slot_name
        )
      }
      
      rv$lineup_slots <- new_slots
      showNotification(sprintf("Loaded lineup #%d", i), type = "message", duration = 2)
    }, ignoreInit = TRUE)
  })
}