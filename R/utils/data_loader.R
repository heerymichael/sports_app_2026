# =============================================================================
# Data Loader
# Functions to load projections and salary data for NFL DFS
# =============================================================================

#' Load data for a specific season, week, and slate
#' @param season Year (e.g., 2025)
#' @param week Week number or playoff identifier (e.g., "wild_card", "divisional_round")
#' @param slate Slate type ("main" or "late")
#' @return Data frame with combined projections and salaries
load_week_data <- function(season, week, slate = "main") {
  log_debug("========================================", level = "INFO")
  log_debug("load_week_data() called", level = "INFO")
  log_debug("  Season:", season, level = "INFO")
  log_debug("  Week:", week, level = "INFO")
  log_debug("  Slate:", slate, level = "INFO")
  
  # Get the file prefix (handles both regular weeks and playoff rounds)
  week_prefix <- get_week_file_prefix(week)
  log_debug("  Week prefix:", week_prefix, level = "DEBUG")
  
  # Determine file paths based on structure
  # Try new structure first (data/projections/2025/), then old structure (projections/)
  
  # Projection file
  proj_file <- NULL
  proj_paths_to_try <- c(
    sprintf("data/projections/%s/%s_projections.csv", season, week_prefix),
    sprintf("projections/%s_projections.csv", week_prefix)
  )
  
  for (path in proj_paths_to_try) {
    log_debug("  Trying projection path:", path, level = "DEBUG")
    if (file.exists(path)) {
      proj_file <- path
      log_debug("  Found projection file:", path, level = "INFO")
      break
    }
  }
  
  if (is.null(proj_file)) {
    log_debug("Projections file not found! Tried:", level = "ERROR")
    for (path in proj_paths_to_try) {
      log_debug("  -", path, level = "ERROR")
    }
    return(NULL)
  }
  
  # Salary file
  salary_file <- NULL
  if (slate == "late") {
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_late.csv", season, week_prefix),
      sprintf("data/fanteam_salaries/%s/%s_fumble.csv", season, week_prefix),
      sprintf("fanteam_salaries/%s_late.csv", week_prefix),
      sprintf("fanteam_salaries/%s_fumble.csv", week_prefix)
    )
  } else if (slate %in% c("two_game_slate", "three_game_slate")) {
    # Custom slate files (e.g., week_15_two_game_slate.csv)
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_%s.csv", season, week_prefix, slate),
      sprintf("fanteam_salaries/%s_%s.csv", week_prefix, slate)
    )
  } else {
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_main.csv", season, week_prefix),
      sprintf("fanteam_salaries/%s_main.csv", week_prefix)
    )
  }
  
  for (path in salary_paths_to_try) {
    log_debug("  Trying salary path:", path, level = "DEBUG")
    if (file.exists(path)) {
      salary_file <- path
      log_debug("  Found salary file:", path, level = "INFO")
      break
    }
  }
  
  if (is.null(salary_file)) {
    log_debug("Salary file not found! Tried:", level = "ERROR")
    for (path in salary_paths_to_try) {
      log_debug("  -", path, level = "ERROR")
    }
    return(NULL)
  }
  
  log_debug("Using files:", level = "INFO")
  log_debug("  Projection:", proj_file, level = "INFO")
  log_debug("  Salary:", salary_file, level = "INFO")
  
  # Load schedule for opponent info
  schedule <- tryCatch({
    log_debug("Loading NFL schedule from nflreadr...", level = "DEBUG")
    sched <- nflreadr::load_schedules(seasons = as.numeric(season)) %>%
      filter(week == !!week) %>%
      mutate(
        home_team = if_else(home_team == "LA", "LAR", home_team),
        away_team = if_else(away_team == "LA", "LAR", away_team)
      ) %>%
      select(week, home_team, away_team)
    log_debug("Loaded schedule with", nrow(sched), "games", level = "DEBUG")
    sched
  }, error = function(e) {
    log_debug("Could not load schedule:", e$message, level = "WARN")
    NULL
  })
  
  # Create opponent lookup
  opponent_lookup <- if (!is.null(schedule) && nrow(schedule) > 0) {
    bind_rows(
      schedule %>% mutate(team = home_team, opponent = away_team, home = TRUE),
      schedule %>% mutate(team = away_team, opponent = home_team, home = FALSE)
    ) %>%
      select(team, opponent, home)
  } else {
    log_debug("No schedule data, opponent info will be empty", level = "WARN")
    NULL
  }
  
  # Load projections
  log_debug("Reading projections CSV...", level = "DEBUG")
  projections <- tryCatch({
    read_csv(proj_file, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>% 
      clean_names()
  }, error = function(e) {
    log_debug("Error reading projections:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(projections)) return(NULL)
  
  log_debug("Projections loaded:", nrow(projections), "rows", level = "INFO")
  log_debug("Projections columns:", paste(names(projections), collapse = ", "), level = "DEBUG")
  
  # Handle different column formats
  projections_clean <- tryCatch({
    projections %>% 
      {
        if ("pos" %in% names(.)) {
          log_debug("Using new format (pos, full_ppr_proj, dk_ceiling)", level = "DEBUG")
          select(., player, team, pos, full_ppr_proj, dk_ceiling) %>%
            rename(position = pos, full = full_ppr_proj, ceiling = dk_ceiling)
        } else {
          log_debug("Using old format (position, full, dk_ceiling)", level = "DEBUG")
          select(., player, team, position, full, dk_ceiling) %>%
            rename(ceiling = dk_ceiling)
        }
      } %>%
      filter(position != "K") %>%
      mutate(team = case_when(
        team == "LA" ~ "LAR",
        TRUE ~ team
      )) %>% 
      mutate(player = case_when(
        player == "LA DST" ~ "LAR DST",
        TRUE ~ player
      ))
  }, error = function(e) {
    log_debug("Error processing projections:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(projections_clean)) return(NULL)
  
  log_debug("Projections cleaned:", nrow(projections_clean), "rows", level = "INFO")
  
  # Load salaries
  log_debug("Reading salaries CSV...", level = "DEBUG")
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
        player == "C.J. Stroud" ~ "CJ Stroud",
        TRUE ~ player
      ))
  }, error = function(e) {
    log_debug("Error reading salaries:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(salaries)) return(NULL)
  
  log_debug("Salaries loaded:", nrow(salaries), "rows", level = "INFO")
  
  # Combine salaries with projections
  log_debug("Joining salaries with projections...", level = "DEBUG")
  combined <- tryCatch({
    result <- left_join(salaries, projections_clean, 
                        by = c("player", "team", "position")) %>% 
      mutate(
        blended = (full + ceiling) / 2,
        value = blended / salary
      ) %>% 
      filter(!is.na(full)) %>%
      mutate(
        season = as.character(season),
        week = as.integer(week),
        slate = slate
      )
    
    # Add opponent info
    if (!is.null(opponent_lookup)) {
      result <- result %>%
        left_join(opponent_lookup, by = "team") %>%
        mutate(
          opponent = if_else(is.na(opponent), "", opponent),
          home = if_else(is.na(home), TRUE, home)
        )
    } else {
      result <- result %>%
        mutate(opponent = "", home = TRUE)
    }
    
    result
  }, error = function(e) {
    log_debug("Error combining data:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(combined)) return(NULL)
  
  log_debug("Combined data:", nrow(combined), "rows", level = "INFO")
  log_debug("Columns:", paste(names(combined), collapse = ", "), level = "DEBUG")
  log_debug("========================================", level = "INFO")
  
  return(combined)
}

#' Load data with headshots
#' @param season Year
#' @param week Week number
#' @param slate Slate type
#' @return Data frame with projections, salaries, and headshots
load_week_data_with_headshots <- function(season, week, slate = "main") {
  log_debug("load_week_data_with_headshots() called", level = "INFO")
  
  data <- load_week_data(season, week, slate)
  
  if (is.null(data) || nrow(data) == 0) {
    log_debug("No data returned from load_week_data()", level = "WARN")
    return(NULL)
  }
  
  # Apply name mapping if exists
  data <- apply_player_mapping(data)
  
  # Add headshots
  tryCatch({
    log_debug("Adding headshot info...", level = "INFO")
    headshots <- get_player_headshots()
    data <- add_headshot_info(data, headshots)
    log_debug("Headshots added successfully", level = "INFO")
  }, error = function(e) {
    log_debug("Could not load headshots:", e$message, level = "WARN")
    data <- data %>%
      mutate(
        headshot_url = "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png",
        team_bg_color = "#E0E0E0"
      )
  })
  
  log_debug("Returning", nrow(data), "players with headshots", level = "INFO")
  return(data)
}

#' Check which slates are available for a week
#' @param season Year
#' @param week Week number or playoff identifier
#' @return Vector of available slates
get_available_slates <- function(season, week) {
  log_debug("get_available_slates() for season:", season, "week:", week, level = "DEBUG")
  
  # Get the file prefix (handles both regular weeks and playoff rounds)
  week_prefix <- get_week_file_prefix(week)
  
  slates <- c()
  
  # Try multiple path patterns for main slate
  main_paths <- c(
    sprintf("data/fanteam_salaries/%s/%s_main.csv", season, week_prefix),
    sprintf("fanteam_salaries/%s_main.csv", week_prefix)
  )
  
  for (path in main_paths) {
    if (file.exists(path)) {
      slates <- c(slates, "main")
      log_debug("  Main slate available at:", path, level = "DEBUG")
      break
    }
  }
  
  # Try multiple path patterns for late slate
  late_paths <- c(
    sprintf("data/fanteam_salaries/%s/%s_fumble.csv", season, week_prefix),
    sprintf("data/fanteam_salaries/%s/%s_late.csv", season, week_prefix),
    sprintf("fanteam_salaries/%s_fumble.csv", week_prefix),
    sprintf("fanteam_salaries/%s_late.csv", week_prefix)
  )
  
  for (path in late_paths) {
    if (file.exists(path)) {
      slates <- c(slates, "late")
      log_debug("  Late slate available at:", path, level = "DEBUG")
      break
    }
  }
  
  # Check for custom slates (two_game_slate, three_game_slate, etc.)
  custom_slate_patterns <- c(
    "two_game_slate",
    "three_game_slate"
  )
  
  for (slate_name in custom_slate_patterns) {
    custom_paths <- c(
      sprintf("data/fanteam_salaries/%s/%s_%s.csv", season, week_prefix, slate_name),
      sprintf("fanteam_salaries/%s_%s.csv", week_prefix, slate_name)
    )
    
    for (path in custom_paths) {
      if (file.exists(path)) {
        slates <- c(slates, slate_name)
        log_debug("  Custom slate available:", slate_name, "at:", path, level = "DEBUG")
        break
      }
    }
  }
  
  log_debug("Available slates:", paste(slates, collapse = ", "), level = "DEBUG")
  return(slates)
}

#' Get display label for slate
#' @param slate Slate identifier
#' @return Human-readable label
get_slate_label <- function(slate) {
  labels <- c(
    "main" = "Main",
    "late" = "Late",
    "two_game_slate" = "2-Game",
    "three_game_slate" = "3-Game"
  )
  
  if (slate %in% names(labels)) {
    return(labels[[slate]])
  }
  
  # Convert snake_case to Title Case as fallback
  gsub("_", " ", tools::toTitleCase(slate))
}

#' Get unmatched players - those with projections but not in salary data
#' @param season Year
#' @param week Week number or playoff identifier
#' @param slate Slate type
#' @param min_projection Minimum projection threshold (default 3)
#' @return Data frame of unmatched players with their projections
get_unmatched_players <- function(season, week, slate = "main", min_projection = 3) {
  log_debug("get_unmatched_players() for season:", season, "week:", week, "slate:", slate, level = "INFO")
  
  # Get the file prefix (handles both regular weeks and playoff rounds)
  week_prefix <- get_week_file_prefix(week)
  
  # Load projections
  proj_file <- NULL
  proj_paths_to_try <- c(
    sprintf("data/projections/%s/%s_projections.csv", season, week_prefix),
    sprintf("projections/%s_projections.csv", week_prefix)
  )
  
  for (path in proj_paths_to_try) {
    if (file.exists(path)) {
      proj_file <- path
      break
    }
  }
  
  if (is.null(proj_file)) {
    log_debug("Projections file not found", level = "WARN")
    return(NULL)
  }
  
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
  
  # Load salaries
  salary_file <- NULL
  if (slate == "late") {
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_late.csv", season, week_prefix),
      sprintf("data/fanteam_salaries/%s/%s_fumble.csv", season, week_prefix),
      sprintf("fanteam_salaries/%s_late.csv", week_prefix),
      sprintf("fanteam_salaries/%s_fumble.csv", week_prefix)
    )
  } else if (slate %in% c("two_game_slate", "three_game_slate")) {
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_%s.csv", season, week_prefix, slate),
      sprintf("fanteam_salaries/%s_%s.csv", week_prefix, slate)
    )
  } else {
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_main.csv", season, week_prefix),
      sprintf("fanteam_salaries/%s_main.csv", week_prefix)
    )
  }
  
  for (path in salary_paths_to_try) {
    if (file.exists(path)) {
      salary_file <- path
      break
    }
  }
  
  if (is.null(salary_file)) {
    log_debug("Salary file not found", level = "WARN")
    return(NULL)
  }
  
  # Get teams in this slate (for filtering projections to slate teams only)
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
      # Apply same name corrections as in load_week_data
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
        player == "C.J. Stroud" ~ "CJ Stroud",
        TRUE ~ player
      ))
  }, error = function(e) {
    log_debug("Error reading salaries:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(salaries)) return(NULL)
  
  # Get teams in the slate
  slate_teams <- unique(salaries$team)
  log_debug("Teams in slate:", paste(slate_teams, collapse = ", "), level = "DEBUG")
  
  # Filter projections to only teams in this slate
  projections_in_slate <- projections %>%
    filter(team %in% slate_teams)
  
  # Get salary player names for matching
  salary_players <- salaries$player
  
  # Find unmatched players with projections >= threshold
  unmatched <- projections_in_slate %>%
    filter(blended >= min_projection) %>%
    filter(!player %in% salary_players) %>%
    arrange(desc(blended)) %>%
    select(player, team, position, full, ceiling, blended)
  
  log_debug("Found", nrow(unmatched), "unmatched players with projection >=", min_projection, level = "INFO")
  
  return(unmatched)
}

#' Get teams in a slate
#' @param season Year
#' @param week Week number or playoff identifier
#' @param slate Slate type
#' @return Vector of team abbreviations in the slate
get_slate_teams <- function(season, week, slate = "main") {
  log_debug("get_slate_teams() for season:", season, "week:", week, "slate:", slate, level = "DEBUG")
  
  # Get the file prefix (handles both regular weeks and playoff rounds)
  week_prefix <- get_week_file_prefix(week)
  
  # Determine salary file path
  salary_file <- NULL
  if (slate == "late") {
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_late.csv", season, week_prefix),
      sprintf("data/fanteam_salaries/%s/%s_fumble.csv", season, week_prefix),
      sprintf("fanteam_salaries/%s_late.csv", week_prefix),
      sprintf("fanteam_salaries/%s_fumble.csv", week_prefix)
    )
  } else if (slate %in% c("two_game_slate", "three_game_slate")) {
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_%s.csv", season, week_prefix, slate),
      sprintf("fanteam_salaries/%s_%s.csv", week_prefix, slate)
    )
  } else {
    salary_paths_to_try <- c(
      sprintf("data/fanteam_salaries/%s/%s_main.csv", season, week_prefix),
      sprintf("fanteam_salaries/%s_main.csv", week_prefix)
    )
  }
  
  for (path in salary_paths_to_try) {
    if (file.exists(path)) {
      salary_file <- path
      break
    }
  }
  
  if (is.null(salary_file)) {
    log_debug("Salary file not found for slate:", slate, level = "WARN")
    return(character(0))
  }
  
  teams <- tryCatch({
    read_csv(salary_file, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>% 
      clean_names() %>%
      filter(lineup != "refuted") %>%
      pull(club) %>%
      unique() %>%
      sort()
  }, error = function(e) {
    log_debug("Error reading salary file:", e$message, level = "ERROR")
    return(character(0))
  })
  
  log_debug("Teams in slate:", paste(teams, collapse = ", "), level = "DEBUG")
  return(teams)
}