# =============================================================================
# NHL Lineup Optimizer
# 
# LP (Linear Programming) based optimization for FanTeam NHL lineups
# Uses lpSolve package for guaranteed optimal solutions
# =============================================================================

library(lpSolve)

#' Optimize NHL lineup using linear programming
#' 
#' @param players Data frame with playerid, player_name, team, position, salary, and projection column
#' @param projection_col Name of column to optimize (e.g., "fpts_blended", "fpts_median", "fpts_ceiling")
#' @param salary_cap Maximum salary budget
#' @param roster_format Either "classic" (9 players) or "classic_limited" (6 players)
#' @param locked_ids Vector of player IDs that must be in lineup
#' @param excluded_ids Vector of player IDs to exclude
#' @return Data frame of selected players or NULL if infeasible
optimize_nhl_lineup_lp <- function(players, 
                                   projection_col = "fpts_blended",
                                   salary_cap = 100,
                                   roster_format = "classic",
                                   locked_ids = c(),
                                   excluded_ids = c()) {
  
  # Get roster configuration
  if (roster_format == "classic") {
    # 1 G, 2 D, 3 W, 2 C, 1 FLEX (skater)
    n_goalies <- 1
    n_defense <- 2
    n_wingers <- 3
    n_centers <- 2
    n_flex <- 1  # Can be C, W, or D
    roster_size <- 9
  } else {
    # classic_limited: 1 G, 2 D, 2 W, 1 C
    n_goalies <- 1
    n_defense <- 2
    n_wingers <- 2
    n_centers <- 1
    n_flex <- 0
    roster_size <- 6
  }
  
  # Filter out excluded players
  if (length(excluded_ids) > 0) {
    players <- players %>% filter(!playerid %in% excluded_ids)
  }
  
  # Ensure projection column exists and handle NAs
  if (!projection_col %in% names(players)) {
    log_debug("Projection column not found:", projection_col, level = "ERROR")
    return(NULL)
  }
  
  players <- players %>%
    filter(!is.na(.data[[projection_col]])) %>%
    filter(.data[[projection_col]] > 0) %>%
    filter(!is.na(salary) & salary > 0)
  
  if (nrow(players) < roster_size) {
    log_debug("Not enough players:", nrow(players), "need", roster_size, level = "ERROR")
    return(NULL)
  }
  
  n <- nrow(players)
  
  # Normalize positions - W includes LW and RW
  players <- players %>%
    mutate(
      is_goalie = position == "G",
      is_defense = position == "D",
      is_winger = position %in% c("LW", "RW", "W"),
      is_center = position == "C",
      is_skater = position %in% c("C", "LW", "RW", "W", "D")  # For FLEX
    )
  
  # Objective: maximize projections
  obj <- players[[projection_col]]
  
  # Build constraint matrix
  # Constraints:
  # 1. Salary cap
  # 2. Exactly n_goalies goalies
  # 3. At least n_defense defensemen
  # 4. At least n_wingers wingers
  # 5. At least n_centers centers
  # 6. Total roster size
  # 7. Locked players (if any)
  
  constraints <- list()
  directions <- c()
  rhs <- c()
  
  # 1. Salary cap: sum(salary * x) <= cap
  constraints[[1]] <- players$salary
  directions <- c(directions, "<=")
  rhs <- c(rhs, salary_cap)
  
  # 2. Goalies: exactly n_goalies
  constraints[[2]] <- as.numeric(players$is_goalie)
  directions <- c(directions, "==")
  rhs <- c(rhs, n_goalies)
  
  if (roster_format == "classic") {
    # For classic format with FLEX:
    # We need at least n_defense D, n_wingers W, n_centers C
    # Plus 1 FLEX which can be any skater
    # Total skaters = n_defense + n_wingers + n_centers + n_flex = 8
    
    # 3. At least n_defense defensemen
    constraints[[3]] <- as.numeric(players$is_defense)
    directions <- c(directions, ">=")
    rhs <- c(rhs, n_defense)
    
    # 4. At least n_wingers wingers
    constraints[[4]] <- as.numeric(players$is_winger)
    directions <- c(directions, ">=")
    rhs <- c(rhs, n_wingers)
    
    # 5. At least n_centers centers
    constraints[[5]] <- as.numeric(players$is_center)
    directions <- c(directions, ">=")
    rhs <- c(rhs, n_centers)
    
    # 6. Total players = roster_size
    constraints[[6]] <- rep(1, n)
    directions <- c(directions, "==")
    rhs <- c(rhs, roster_size)
    
  } else {
    # For classic_limited (no FLEX):
    # Exactly n_defense D, n_wingers W, n_centers C
    
    # 3. Exactly n_defense defensemen
    constraints[[3]] <- as.numeric(players$is_defense)
    directions <- c(directions, "==")
    rhs <- c(rhs, n_defense)
    
    # 4. Exactly n_wingers wingers
    constraints[[4]] <- as.numeric(players$is_winger)
    directions <- c(directions, "==")
    rhs <- c(rhs, n_wingers)
    
    # 5. Exactly n_centers centers
    constraints[[5]] <- as.numeric(players$is_center)
    directions <- c(directions, "==")
    rhs <- c(rhs, n_centers)
    
    # 6. Total players = roster_size
    constraints[[6]] <- rep(1, n)
    directions <- c(directions, "==")
    rhs <- c(rhs, roster_size)
  }
  
  # Add locked player constraints
  if (length(locked_ids) > 0) {
    for (pid in locked_ids) {
      lock_constraint <- as.numeric(players$playerid == pid)
      if (sum(lock_constraint) > 0) {
        constraints[[length(constraints) + 1]] <- lock_constraint
        directions <- c(directions, "==")
        rhs <- c(rhs, 1)
      }
    }
  }
  
  # Build constraint matrix
  const_mat <- do.call(rbind, constraints)
  
  # Solve LP
  result <- tryCatch({
    lp(
      direction = "max",
      objective.in = obj,
      const.mat = const_mat,
      const.dir = directions,
      const.rhs = rhs,
      all.bin = TRUE
    )
  }, error = function(e) {
    log_debug("LP solve error:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(result) || result$status != 0) {
    log_debug("LP optimization failed or infeasible, status:", result$status, level = "WARN")
    return(NULL)
  }
  
  # Extract selected players
  selected_indices <- which(result$solution == 1)
  selected_players <- players[selected_indices, ]
  
  log_debug("LP optimization successful, selected", nrow(selected_players), "players", level = "INFO")
  log_debug("Total projection:", round(result$objval, 1), level = "INFO")
  log_debug("Total salary:", sum(selected_players$salary), level = "INFO")
  
  return(selected_players)
}


#' Generate multiple lineups with variance
#' 
#' @param players Data frame of players
#' @param n_lineups Number of lineups to generate
#' @param projection_col Base projection column
#' @param variance_pct Percentage variance to apply (e.g., 15 for +/- 15%)
#' @param salary_cap Salary cap
#' @param roster_format Roster format
#' @param min_unique Minimum unique players between lineups
#' @param locked_ids Players to lock in all lineups
#' @param excluded_ids Players to exclude from all lineups
#' @return List of lineup data frames
generate_nhl_lineups_with_variance <- function(players,
                                               n_lineups = 5,
                                               projection_col = "fpts_blended",
                                               variance_pct = 15,
                                               salary_cap = 100,
                                               roster_format = "classic",
                                               min_unique = 1,
                                               locked_ids = c(),
                                               excluded_ids = c()) {
  
  lineups <- list()
  all_lineup_player_ids <- list()
  max_attempts <- n_lineups * 50
  attempts <- 0
  
  while (length(lineups) < n_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Apply variance to projections
    players_varied <- players %>%
      mutate(
        variance_factor = runif(n(), 1 - variance_pct/100, 1 + variance_pct/100),
        proj_varied = .data[[projection_col]] * variance_factor
      )
    
    # Optimize with varied projections
    lineup <- optimize_nhl_lineup_lp(
      players = players_varied,
      projection_col = "proj_varied",
      salary_cap = salary_cap,
      roster_format = roster_format,
      locked_ids = locked_ids,
      excluded_ids = excluded_ids
    )
    
    if (is.null(lineup) || nrow(lineup) == 0) {
      next
    }
    
    # Check uniqueness constraint
    current_ids <- lineup$playerid
    
    if (length(all_lineup_player_ids) == 0) {
      # First lineup always valid
      lineups[[length(lineups) + 1]] <- lineup
      all_lineup_player_ids[[length(all_lineup_player_ids) + 1]] <- current_ids
    } else {
      # Check against all previous lineups
      meets_unique <- TRUE
      for (prev_ids in all_lineup_player_ids) {
        common <- length(intersect(current_ids, prev_ids))
        unique_count <- length(current_ids) - common
        if (unique_count < min_unique) {
          meets_unique <- FALSE
          break
        }
      }
      
      if (meets_unique) {
        lineups[[length(lineups) + 1]] <- lineup
        all_lineup_player_ids[[length(all_lineup_player_ids) + 1]] <- current_ids
      }
    }
  }
  
  if (length(lineups) < n_lineups) {
    log_debug("Only generated", length(lineups), "of", n_lineups, "lineups after", attempts, "attempts", level = "WARN")
  }
  
  return(lineups)
}


#' Assign players to lineup slots
#' 
#' @param lineup Data frame of selected players from LP
#' @param roster_format Roster format
#' @return Named list with players assigned to slots
assign_nhl_lineup_slots <- function(lineup, roster_format = "classic") {
  
  if (roster_format == "classic") {
    slot_order <- c("G", "D1", "D2", "W1", "W2", "W3", "C1", "C2", "FLEX")
  } else {
    slot_order <- c("G", "D1", "D2", "W1", "W2", "C1")
  }
  
  slots <- setNames(vector("list", length(slot_order)), slot_order)
  used_ids <- c()
  
  # Normalize position groupings
  lineup <- lineup %>%
    mutate(
      pos_group = case_when(
        position == "G" ~ "G",
        position == "D" ~ "D",
        position %in% c("LW", "RW", "W") ~ "W",
        position == "C" ~ "C",
        TRUE ~ position
      )
    )
  
  # Fill goalies
  goalies <- lineup %>% filter(pos_group == "G" & !playerid %in% used_ids)
  if (nrow(goalies) >= 1) {
    slots$G <- as.list(goalies[1, ])
    used_ids <- c(used_ids, goalies$playerid[1])
  }
  
  # Fill defense
  defense <- lineup %>% filter(pos_group == "D" & !playerid %in% used_ids) %>% arrange(desc(fpts_blended))
  for (i in 1:min(2, nrow(defense))) {
    slot_name <- paste0("D", i)
    if (slot_name %in% slot_order) {
      slots[[slot_name]] <- as.list(defense[i, ])
      used_ids <- c(used_ids, defense$playerid[i])
    }
  }
  
  # Fill wingers
  n_wingers <- if (roster_format == "classic") 3 else 2
  wingers <- lineup %>% filter(pos_group == "W" & !playerid %in% used_ids) %>% arrange(desc(fpts_blended))
  for (i in 1:min(n_wingers, nrow(wingers))) {
    slot_name <- paste0("W", i)
    if (slot_name %in% slot_order) {
      slots[[slot_name]] <- as.list(wingers[i, ])
      used_ids <- c(used_ids, wingers$playerid[i])
    }
  }
  
  # Fill centers
  n_centers <- if (roster_format == "classic") 2 else 1
  centers <- lineup %>% filter(pos_group == "C" & !playerid %in% used_ids) %>% arrange(desc(fpts_blended))
  for (i in 1:min(n_centers, nrow(centers))) {
    slot_name <- paste0("C", i)
    if (slot_name %in% slot_order) {
      slots[[slot_name]] <- as.list(centers[i, ])
      used_ids <- c(used_ids, centers$playerid[i])
    }
  }
  
  # Fill FLEX (classic only) with remaining skater
  if (roster_format == "classic" && "FLEX" %in% slot_order) {
    remaining <- lineup %>% filter(!playerid %in% used_ids & pos_group != "G")
    if (nrow(remaining) >= 1) {
      slots$FLEX <- as.list(remaining[1, ])
    }
  }
  
  return(slots)
}


cat("NHL optimizer loaded: optimize_nhl_lineup_lp(), generate_nhl_lineups_with_variance(), assign_nhl_lineup_slots()\n")