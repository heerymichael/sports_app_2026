# =============================================================================
# Module: NFL Fanteam Playoffs
# 
# Hand builder for Fanteam NFL Playoff contests
# Matches the handbuild UI style with clickable player pool rows
# =============================================================================

library(dplyr)
library(readr)
library(stringr)

# =============================================================================
# CONFIGURATION
# =============================================================================

FANTEAM_PLAYOFFS_CONFIG <- list(
  salary_cap = 130,
  roster_slots = c("QB", "RB1", "RB2", "WR1", "WR2", "WR3", "TE", "FLEX", "DST"),
  flex_positions = c("RB", "WR", "TE")
)

FANTEAM_PLAYOFF_ROUNDS <- c(
  "Super Bowl" = "super_bowl",
  "Conference Games" = "conference_games", 
  "Divisional Round" = "divisional_round",
  "Wild Card" = "wild_card"
)

# =============================================================================
# DATA LOADING
# =============================================================================

get_available_playoff_rounds <- function(season) {
  log_debug("get_available_playoff_rounds() for season:", season, level = "DEBUG")
  
  salary_dir <- sprintf("data/fanteam_salaries/%s", season)
  if (!dir.exists(salary_dir)) return(character(0))
  
  available <- c()
  for (round_id in FANTEAM_PLAYOFF_ROUNDS) {
    # Check multiple file patterns including alternate naming (conference_games -> conference_round)
    alternate_round_id <- gsub("_games$", "_round", round_id)
    patterns <- c(
      sprintf("%s/playoff_contest_%s.csv", salary_dir, round_id),
      sprintf("%s/playoff_contest_%s.csv", salary_dir, alternate_round_id),
      sprintf("%s/%s_main.csv", salary_dir, round_id),
      sprintf("%s/%s.csv", salary_dir, round_id)
    )
    
    for (pattern in patterns) {
      if (file.exists(pattern)) {
        available <- c(available, round_id)
        break
      }
    }
  }
  
  if (length(available) == 0) return(character(0))
  
  FANTEAM_PLAYOFF_ROUNDS[FANTEAM_PLAYOFF_ROUNDS %in% available]
}

load_fanteam_playoffs_data <- function(season, round) {
  log_debug("========================================", level = "INFO")
  log_debug("load_fanteam_playoffs_data()", level = "INFO")
  log_debug("  Season:", season, "Round:", round, level = "INFO")
  
  # Try multiple salary file patterns (including alternate naming conventions)
  salary_paths_to_try <- c(
    sprintf("data/fanteam_salaries/%s/playoff_contest_%s.csv", season, round),
    sprintf("data/fanteam_salaries/%s/playoff_contest_%s.csv", season, gsub("_games$", "_round", round)),  # conference_games -> conference_round
    sprintf("data/fanteam_salaries/%s/%s_main.csv", season, round),
    sprintf("data/fanteam_salaries/%s/%s.csv", season, round)
  )
  
  salary_file <- NULL
  for (path in salary_paths_to_try) {
    if (file.exists(path)) {
      salary_file <- path
      break
    }
  }
  
  # Try multiple projection file patterns (including alternate naming conventions)
  alternate_round <- gsub("_games$", "_round", round)
  proj_paths_to_try <- c(
    sprintf("data/projections/%s/%s_projections.csv", season, round),
    sprintf("data/projections/%s/%s_projections.csv", season, alternate_round),  # conference_games -> conference_round
    sprintf("projections/%s_projections.csv", round),
    sprintf("projections/%s_projections.csv", alternate_round),
    sprintf("data/projections/%s_projections.csv", round),
    sprintf("data/projections/%s_projections.csv", alternate_round)
  )
  
  proj_file <- NULL
  for (path in proj_paths_to_try) {
    if (file.exists(path)) {
      proj_file <- path
      break
    }
  }
  
  log_debug("  Salary file:", if (is.null(salary_file)) "NOT FOUND" else salary_file, level = "INFO")
  log_debug("  Projection file:", if (is.null(proj_file)) "NOT FOUND" else proj_file, level = "INFO")
  
  if (is.null(salary_file)) {
    log_debug("  Tried salary paths:", paste(salary_paths_to_try, collapse = ", "), level = "WARN")
    log_debug("Salary file not found!", level = "ERROR")
    return(NULL)
  }
  
  if (is.null(proj_file)) {
    log_debug("  Tried projection paths:", paste(proj_paths_to_try, collapse = ", "), level = "WARN")
    log_debug("Projection file not found - will proceed without projections", level = "WARN")
  }
  
  # Load salaries
  salaries <- tryCatch({
    read_csv(salary_file, show_col_types = FALSE) %>% janitor::clean_names()
  }, error = function(e) {
    log_debug("Error loading salaries:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(salaries)) return(NULL)
  
  log_debug("  Salary columns:", paste(names(salaries), collapse = ", "), level = "INFO")
  
  # Process salary column names
  # Handle FName + Name -> player (clean_names converts FName to f_name)
  # Using EXACT same approach as data_loader.R lines 173-177
  if ("f_name" %in% names(salaries) && "name" %in% names(salaries)) {
    log_debug("  Found f_name and name columns, combining...", level = "INFO")
    
    # Remove Jr./Sr. suffixes from last name (same as data_loader.R)
    salaries <- salaries %>%
      mutate(name = str_remove(name, regex("\\s+Jr\\.?$", ignore_case = TRUE))) %>%
      mutate(name = str_remove(name, regex("\\s+Sr\\.?$", ignore_case = TRUE))) %>%
      mutate(player = if_else(
        is.na(f_name) | f_name == "",
        name,  # DST case: use name only
        paste0(f_name, " ", name)
      )) %>%
      select(-f_name, -name)
  } else if ("name" %in% names(salaries)) {
    salaries <- salaries %>% rename(player = name)
  }
  
  log_debug("  After player creation, columns:", paste(names(salaries), collapse = ", "), level = "INFO")
  
  # Rename columns (clean_names makes them lowercase)
  if ("price" %in% names(salaries)) salaries <- salaries %>% rename(salary = price)
  if ("club" %in% names(salaries)) {
    log_debug("  Renaming club -> team", level = "INFO")
    salaries <- salaries %>% rename(team = club)
  }
  
  log_debug("  After renaming, columns:", paste(names(salaries), collapse = ", "), level = "INFO")
  
  # Map position names using case_when for reliability
  if ("position" %in% names(salaries)) {
    salaries <- salaries %>%
      mutate(position = case_when(
        position == "quarterback" ~ "QB",
        position == "running_back" ~ "RB",
        position == "wide_receiver" ~ "WR",
        position == "tight_end" ~ "TE",
        position == "defense_special" ~ "DST",
        position == "defense" ~ "DST",
        TRUE ~ toupper(position)
      ))
    
    log_debug("  After position mapping, unique positions:", paste(unique(salaries$position), collapse = ", "), level = "INFO")
    
    # Debug: Count DST players after position mapping
    dst_count <- sum(salaries$position == "DST")
    log_debug("  DST players after position mapping:", dst_count, level = "INFO")
  }
  
  # Apply NFL name corrections from nfl_config.R BEFORE creating join_name
  # Using EXACT SAME case_when corrections as data_loader.R for consistency
  log_debug("  Applying name corrections (same as data_loader.R)...", level = "INFO")
  
  # Debug: Show before correction
  walker_before <- salaries %>% filter(grepl("Walker|Mims", player, ignore.case = TRUE)) %>% pull(player)
  if (length(walker_before) > 0) {
    log_debug("  Sample players BEFORE correction:", paste(walker_before, collapse = ", "), level = "INFO")
  }
  
  # Apply exact same corrections as data_loader.R lines 193-207
  salaries <- salaries %>%
    mutate(player = case_when(
      player == "Amon-Ra St. Brown" ~ "Amon-Ra St Brown",
      player == "A.J. Brown" ~ "AJ Brown",
      player == "J.K. Dobbins" ~ "JK Dobbins",
      player == "Kenneth Walker III" ~ "Kenneth Walker",
      player == "Kenneth Walker" ~ "Kenneth Walker",  # Already correct
      player == "Luther Burden III" ~ "Luther Burden",
      player == "DeMario Douglas" ~ "Demario Douglas",
      player == "Calvin Austin III" ~ "Calvin Austin",
      player == "Hollywood Brown" ~ "Marquise Brown",
      player == "KaVontae Turpin" ~ "Kavontae Turpin",
      player == "Ollie Gordon II" ~ "Ollie Gordon",
      player == "T.J. Hockenson" ~ "TJ Hockenson",
      player == "C.J. Stroud" ~ "CJ Stroud",
      player == "D.K. Metcalf" ~ "DK Metcalf",
      player == "Marvin Mims Jr" ~ "Marvin Mims",
      player == "Marvin Mims Jr." ~ "Marvin Mims",
      TRUE ~ player
    ))
  
  walker_after <- salaries %>% filter(grepl("Walker|Mims", player, ignore.case = TRUE)) %>% pull(player)
  if (length(walker_after) > 0) {
    log_debug("  Sample players AFTER correction:", paste(walker_after, collapse = ", "), level = "INFO")
  }
  
  # Normalize DST player names for matching: "DEF/ST Bills" -> "BUF DST"
  # This allows joining with projections that have "NE DST", "DEN DST" etc.
  # MUST happen AFTER name corrections
  if ("player" %in% names(salaries) && "team" %in% names(salaries)) {
    salaries <- salaries %>%
      mutate(
        # Create a join_name for matching projections
        join_name = if_else(
          position == "DST",
          paste0(team, " DST"),  # Convert "DEF/ST Bills" to "BUF DST"
          player  # Use corrected player name
        )
      )
  }
  
  log_debug("  Salaries processed:", nrow(salaries), "players", level = "INFO")
  log_debug("  Sample names:", paste(head(salaries$player, 3), collapse = ", "), level = "INFO")
  if ("team" %in% names(salaries)) {
    log_debug("  Teams found:", paste(head(unique(salaries$team), 6), collapse = ", "), level = "INFO")
  } else {
    log_debug("  WARNING: No team column found!", level = "WARN")
    log_debug("  Available columns:", paste(names(salaries), collapse = ", "), level = "WARN")
  }
  if ("join_name" %in% names(salaries)) {
    # Show sample join names (should be corrected names now)
    sample_join <- head(salaries$join_name, 5)
    log_debug("  Sample join_names:", paste(sample_join, collapse = ", "), level = "INFO")
    # Show DST join names
    dst_names <- salaries %>% filter(position == "DST") %>% pull(join_name) %>% head(3)
    log_debug("  DST join_names:", paste(dst_names, collapse = ", "), level = "INFO")
  }
  
  # Load projections
  projections <- NULL
  if (!is.null(proj_file) && file.exists(proj_file)) {
    projections <- tryCatch({
      read_csv(proj_file, show_col_types = FALSE) %>% janitor::clean_names()
    }, error = function(e) NULL)
    
    if (!is.null(projections)) {
      log_debug("  Projection columns (raw):", paste(names(projections), collapse = ", "), level = "INFO")
      
      # Rename projection columns
      if ("full_ppr_proj" %in% names(projections)) {
        projections <- projections %>% rename(full = full_ppr_proj)
        log_debug("  Renamed full_ppr_proj -> full", level = "INFO")
      } else if ("dk_proj" %in% names(projections)) {
        projections <- projections %>% rename(full = dk_proj)
        log_debug("  Renamed dk_proj -> full", level = "INFO")
      } else {
        log_debug("  WARNING: No full_ppr_proj or dk_proj column found!", level = "WARN")
      }
      
      if ("dk_ceiling" %in% names(projections)) {
        projections <- projections %>% rename(ceiling = dk_ceiling)
        log_debug("  Renamed dk_ceiling -> ceiling", level = "INFO")
      }
      
      log_debug("  Projection columns (after rename):", paste(names(projections), collapse = ", "), level = "INFO")
      log_debug("  Sample proj names:", paste(head(projections$player, 3), collapse = ", "), level = "INFO")
    }
  }
  
  log_debug("  Sample salary names (after correction):", paste(head(salaries$player, 5), collapse = ", "), level = "INFO")
  
  # Apply same name corrections to projections for consistency
  # IMPORTANT: Fix LA/LAR mismatch for DST - salary has LAR, projection has LA
  if (!is.null(projections)) {
    projections <- projections %>%
      mutate(player = case_when(
        player == "LA DST" ~ "LAR DST",  # Match salary file team abbreviation
        TRUE ~ player
      ))
    log_debug("  Sample proj names:", paste(head(projections$player, 5), collapse = ", "), level = "INFO")
    
    # Show DST names in projections for debugging
    proj_dst <- projections %>% filter(grepl("DST", player)) %>% pull(player)
    log_debug("  Projection DST names:", paste(proj_dst, collapse = ", "), level = "INFO")
  }
  
  # Join - use join_name for matching (handles DST name differences)
  if (!is.null(projections)) {
    # Only select projection-specific columns (not team, which we already have from salaries)
    log_debug("  Projection columns available:", paste(names(projections), collapse = ", "), level = "INFO")
    proj_cols_to_use <- intersect(names(projections), c("full", "ceiling", "floor", "blended", "opp"))
    log_debug("  Joining projection cols:", paste(proj_cols_to_use, collapse = ", "), level = "INFO")
    
    if (length(proj_cols_to_use) == 0) {
      log_debug("  WARNING: No projection columns found to join!", level = "WARN")
    }
    
    # Create join_name in projections too
    projections <- projections %>% mutate(join_name = player)
    
    combined <- salaries %>%
      left_join(
        projections %>% select(join_name, all_of(proj_cols_to_use)),
        by = "join_name"
      )
    
    n_matched <- sum(!is.na(combined$full))
    log_debug("  Matched:", n_matched, "of", nrow(combined), level = "INFO")
    
    # Filter to only teams that have players with projections
    teams_with_projections <- combined %>%
      filter(!is.na(full) & full > 0) %>%
      pull(team) %>%
      unique()
    
    log_debug("  Teams with projections:", paste(teams_with_projections, collapse = ", "), level = "INFO")
    
    combined <- combined %>%
      filter(team %in% teams_with_projections)
    
    log_debug("  After team filter:", nrow(combined), "players", level = "INFO")
    
    # Debug DST matching specifically
    dst_combined <- combined %>% filter(position == "DST")
    log_debug("  DST players after join:", nrow(dst_combined), level = "INFO")
    if (nrow(dst_combined) > 0) {
      dst_with_proj <- dst_combined %>% filter(!is.na(full) & full > 0)
      log_debug("  DST with projections:", nrow(dst_with_proj), "of", nrow(dst_combined), level = "INFO")
      if (nrow(dst_with_proj) == 0) {
        # Show what DST join_names we have vs what projections have
        log_debug("  DST join_names in salary:", paste(head(salaries %>% filter(position == "DST") %>% pull(join_name), 5), collapse = ", "), level = "WARN")
        log_debug("  DST join_names in proj:", paste(head(projections %>% filter(grepl("DST", player)) %>% pull(join_name), 5), collapse = ", "), level = "WARN")
      } else {
        log_debug("  DST projections:", paste(sprintf("%s=%.1f", dst_with_proj$player, dst_with_proj$full), collapse = ", "), level = "INFO")
      }
    }
    
    # =========================================================================
    # WARN ABOUT UNMATCHED HIGH-PROJECTION PLAYERS
    # =========================================================================
    # Find players in projections with >=5 points that didn't match any salary player
    salary_players <- salaries$join_name
    unmatched_projections <- projections %>%
      filter(!join_name %in% salary_players) %>%
      filter(full >= 5) %>%
      arrange(desc(full))
    
    # Store unmatched for UI warning (will be attached as attribute)
    unmatched_warning <- NULL
    
    if (nrow(unmatched_projections) > 0) {
      log_debug("  ========================================", level = "WARN")
      log_debug(sprintf("  WARNING: %d unmatched PROJECTION players (>=5 pts, not in salary file):", nrow(unmatched_projections)), level = "WARN")
      
      unmatched_names <- c()
      for (i in 1:min(nrow(unmatched_projections), 15)) {
        p <- unmatched_projections[i, ]
        log_debug(sprintf("    - %s (%.1f pts)", p$player, p$full), level = "WARN")
        unmatched_names <- c(unmatched_names, sprintf("%s (%.1f pts)", p$player, p$full))
      }
      if (nrow(unmatched_projections) > 15) {
        log_debug(sprintf("    ... and %d more", nrow(unmatched_projections) - 15), level = "WARN")
      }
      log_debug("  Consider adding to data/player_name_mapping.csv or NFL_PLAYER_NAME_CORRECTIONS", level = "WARN")
      log_debug("  ========================================", level = "WARN")
      
      unmatched_warning <- list(
        count = nrow(unmatched_projections),
        players = unmatched_names
      )
    }
    
    # Also warn about salary players who didn't get projections (non-DST only)
    unmatched_salary <- combined %>%
      filter(is.na(full) | full == 0) %>%
      filter(position != "DST") %>%
      filter(salary >= 8) %>%  # Only warn about expensive players
      arrange(desc(salary))
    
    if (nrow(unmatched_salary) > 0) {
      log_debug("  ========================================", level = "WARN")
      log_debug(sprintf("  WARNING: %d SALARY players (>=£8) missing projections:", nrow(unmatched_salary)), level = "WARN")
      
      salary_unmatched_names <- c()
      for (i in 1:min(nrow(unmatched_salary), 10)) {
        p <- unmatched_salary[i, ]
        log_debug(sprintf("    - %s (%s, £%.1f)", p$player, p$position, p$salary), level = "WARN")
        salary_unmatched_names <- c(salary_unmatched_names, sprintf("%s (%s, £%.1f)", p$player, p$position, p$salary))
      }
      if (nrow(unmatched_salary) > 10) {
        log_debug(sprintf("    ... and %d more", nrow(unmatched_salary) - 10), level = "WARN")
      }
      log_debug("  ========================================", level = "WARN")
      
      # Add to warning or create new
      if (is.null(unmatched_warning)) {
        unmatched_warning <- list(count = 0, players = c(), salary_count = nrow(unmatched_salary), salary_players = salary_unmatched_names)
      } else {
        unmatched_warning$salary_count <- nrow(unmatched_salary)
        unmatched_warning$salary_players <- salary_unmatched_names
      }
    }
    
    # Attach warning as attribute
    if (!is.null(unmatched_warning)) {
      attr(combined, "unmatched_warning") <- unmatched_warning
    }
    
    # Clean up join_name column
    combined <- combined %>% select(-join_name)
  } else {
    combined <- salaries %>% select(-any_of("join_name"))
  }
  
  # Ensure required columns exist
  if (!"team" %in% names(combined)) combined$team <- "UNK"
  if (!"full" %in% names(combined)) combined$full <- 0
  if (!"ceiling" %in% names(combined)) combined$ceiling <- 0
  
  # Calculate derived columns
  combined <- combined %>%
    mutate(
      projection = coalesce(full, ceiling, 0),
      value = if_else(salary > 0, projection / salary, 0)
    )
  
  # Add headshots using the shared headshot system
  combined <- tryCatch({
    log_debug("  Adding headshot info...", level = "INFO")
    result <- add_headshot_info(combined)
    log_debug("  Headshots added successfully", level = "INFO")
    result
  }, error = function(e) {
    log_debug("  Could not load headshots:", e$message, level = "WARN")
    combined %>%
      mutate(
        headshot_url = "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png",
        team_bg_color = "#E0E0E0"
      )
  })
  
  # Ensure headshot columns exist (in case add_headshot_info didn't add them)
  if (!"headshot_url" %in% names(combined)) {
    combined$headshot_url <- "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
  }
  if (!"team_bg_color" %in% names(combined)) {
    combined$team_bg_color <- "#E0E0E0"
  }
  
  log_debug("  Final:", nrow(combined), "players", level = "INFO")
  log_debug("  Final teams:", paste(head(unique(combined$team), 6), collapse = ", "), level = "INFO")
  
  # Log position counts
  pos_counts <- combined %>% count(position) %>% mutate(info = paste0(position, ":", n))
  log_debug("  Positions:", paste(pos_counts$info, collapse = ", "), level = "INFO")
  
  # CRITICAL: Log DST players with their projections
  dst_final <- combined %>% filter(position == "DST")
  if (nrow(dst_final) > 0) {
    log_debug("  ========================================", level = "INFO")
    log_debug("  FINAL DST PLAYERS:", nrow(dst_final), level = "INFO")
    for (i in 1:min(nrow(dst_final), 10)) {
      d <- dst_final[i, ]
      log_debug(sprintf("    %s: £%.1f, proj=%.1f, full=%s, ceiling=%s", 
                        d$player, d$salary, d$projection, 
                        ifelse(is.na(d$full), "NA", sprintf("%.1f", d$full)),
                        ifelse(is.na(d$ceiling), "NA", sprintf("%.1f", d$ceiling))), level = "INFO")
    }
    log_debug("  ========================================", level = "INFO")
  } else {
    log_debug("  ======== WARNING: NO DST PLAYERS IN FINAL DATA! ========", level = "WARN")
  }
  
  log_debug("========================================", level = "INFO")
  
  combined
}

# =============================================================================
# UI
# =============================================================================

nfl_fanteam_playoffs_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("nfl_fanteam_playoffs_ui() called", level = "INFO")
  
  tagList(
    shinyjs::useShinyjs(),
    
    div(class = "page-header",
        tags$h2("Fanteam Playoffs"),
        tags$p(class = "text-muted", "NFL Playoff lineup builder • £130 salary cap")
    ),
    
    # Season/Round + Stats row
    fluidRow(
      column(2, selectizeInput(ns("season"), "Season", choices = c("2025" = "2025"), selected = "2025")),
      column(2, selectizeInput(ns("round"), "Round", choices = c("Conference Games" = "conference_games"), selected = "conference_games")),
      column(2, ui_value_box(value = textOutput(ns("salary_used")), label = "Salary Used", color = "coral")),
      column(2, ui_value_box(value = textOutput(ns("salary_remaining")), label = "Remaining", color = "sage")),
      column(2, ui_value_box(value = textOutput(ns("players_selected")), label = "Players", color = "teal")),
      column(2, ui_value_box(value = textOutput(ns("total_projection")), label = "Projected Pts", color = "yellow"))
    ),
    
    tags$br(),
    
    # Unmatched players warning (shown when there are name mismatches)
    uiOutput(ns("unmatched_warning")),
    
    # Build Your Lineup card
    fluidRow(
      column(12,
             ui_card(
               title = "Build Your Lineup",
               color = NFL_CARD_COLOR,
               
               fluidRow(
                 # Left: Player Pool
                 column(7,
                        # Position filter buttons
                        div(
                          class = "player-pool-filters",
                          style = "display: flex; gap: 0.5rem; margin-bottom: 0.5rem; align-items: center;",
                          actionButton(ns("filter_all"), "ALL", class = "btn-position-filter active"),
                          actionButton(ns("filter_qb"), "QB", class = "btn-position-filter"),
                          actionButton(ns("filter_rb"), "RB", class = "btn-position-filter"),
                          actionButton(ns("filter_wr"), "WR", class = "btn-position-filter"),
                          actionButton(ns("filter_te"), "TE", class = "btn-position-filter"),
                          actionButton(ns("filter_dst"), "DST", class = "btn-position-filter")
                        ),
                        
                        # Team filter row
                        div(
                          style = "display: flex; gap: 0.75rem; margin-bottom: 0.75rem; align-items: center;",
                          div(style = "width: 180px;",
                              selectizeInput(ns("filter_team"), NULL, choices = c("All Teams" = "all"), selected = "all")
                          ),
                          div(style = "flex: 1;")
                        ),
                        
                        # Player pool container
                        div(
                          class = "player-pool-container",
                          style = "height: 485px; overflow-y: auto; border: 2px solid var(--outline); border-radius: 8px; background: var(--bg-white);",
                          uiOutput(ns("player_pool_header")),
                          uiOutput(ns("player_pool"))
                        )
                 ),
                 
                 # Right: Lineup
                 column(5,
                        uiOutput(ns("lineup_display")),
                        div(
                          style = "display: flex; gap: 0.5rem; margin-top: 0.75rem;",
                          actionButton(ns("optimize_remaining"), "Autocomplete", class = "btn-primary", style = "flex: 1; font-size: 0.85rem;"),
                          actionButton(ns("clear_lineup"), "Clear", class = "btn-secondary", style = "flex: 1; font-size: 0.85rem;")
                        )
                 )
               )
             )
      )
    ),
    
    tags$br(),
    
    # ==========================================================================
    # RULES CARD
    # ==========================================================================
    fluidRow(
      column(12,
             ui_card(
               title = "Player Rules",
               color = NFL_CARD_COLOR,
               
               # Lock Players Row
               fluidRow(
                 column(9,
                        selectizeInput(ns("lock_players"), "Lock Players (Always Include)",
                                       choices = NULL, multiple = TRUE,
                                       options = list(placeholder = "Select players to lock in every lineup...")
                        )
                 ),
                 column(3,
                        div(
                          style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                          actionButton(ns("apply_lock"), "Lock", class = "btn btn-outline-success", style = "width: 100px;")
                        )
                 )
               ),
               
               # Exclude Players Row
               fluidRow(
                 column(9,
                        selectizeInput(ns("exclude_players"), "Exclude Players",
                                       choices = NULL, multiple = TRUE,
                                       options = list(placeholder = "Select players to exclude from all lineups...")
                        )
                 ),
                 column(3,
                        div(
                          style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                          actionButton(ns("apply_exclude"), "Exclude", class = "btn btn-outline-danger", style = "width: 100px;")
                        )
                 )
               ),
               
               # Applied lock/exclude display
               uiOutput(ns("lock_exclude_display")),
               
               tags$hr(),
               
               # Team Max Rules
               tags$h6(class = "fw-bold mb-2", icon("users"), " Team Max Rules"),
               tags$p(class = "text-muted small mb-3", "Limit maximum players from specific teams"),
               
               fluidRow(
                 column(5,
                        selectizeInput(ns("team_max_team"), "Team",
                                       choices = NULL,
                                       options = list(placeholder = "Select team...")
                        )
                 ),
                 column(3,
                        numericInput(ns("team_max_value"), "Max Players", value = 4, min = 1, max = 9, step = 1)
                 ),
                 column(4,
                        div(style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                            actionButton(ns("add_team_max_rule"), "Add Rule", class = "btn btn-primary", style = "width: 120px;")
                        )
                 )
               ),
               
               uiOutput(ns("team_max_rules_display"))
             )
      )
    ),
    
    tags$br(),
    
    # ==========================================================================
    # GENERATE LINEUPS CARD
    # ==========================================================================
    ui_card(
      title = "Generate Lineups",
      color = NFL_CARD_COLOR,
      
      div(
        style = "display: flex; justify-content: center; gap: 2rem; margin-bottom: 1.5rem;",
        div(style = "width: 150px;",
            numericInput(ns("num_lineups"), "# Lineups", value = 10, min = 1, max = 50)
        ),
        div(style = "width: 150px;",
            numericInput(ns("variance_pct"), "Variance %", value = 15, min = 0, max = 50)
        )
      ),
      
      div(
        style = "text-align: center; margin-bottom: 1.5rem;",
        actionButton(
          ns("generate"),
          tagList(icon("play"), " GENERATE LINEUPS"),
          class = "btn btn-target-highlight btn-lg",
          style = "padding: 1rem 3rem; font-size: 1.2rem; font-weight: 700; text-transform: uppercase; box-shadow: 4px 4px 0 rgba(0,0,0,0.2);"
        )
      ),
      
      uiOutput(ns("lineup_summary_stats")),
      
      tags$hr(),
      
      uiOutput(ns("results_panel"))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

nfl_fanteam_playoffs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("nfl_fanteam_playoffs_server() initialized", level = "INFO")
    
    rv <- reactiveValues(
      player_data = NULL,
      lineup = list(QB = NULL, RB1 = NULL, RB2 = NULL, WR1 = NULL, WR2 = NULL, WR3 = NULL, TE = NULL, FLEX = NULL, DST = NULL),
      selected_players = character(0),
      position_filter = "all",
      unmatched_warning = NULL,
      # Rules
      locked_players = character(0),
      excluded_players = character(0),
      team_max_rules = list(),  # List of list(team = "KC", max = 3)
      # Generated lineups
      generated_lineups = NULL
    )
    
    initialized <- reactiveVal(FALSE)
    
    # Initialize dropdowns
    observe({
      if (initialized()) return()
      
      seasons <- get_available_seasons()
      if (length(seasons) == 0) seasons <- c("2025")
      
      updateSelectizeInput(session, "season", choices = setNames(as.character(seasons), as.character(seasons)), selected = seasons[1])
      
      rounds <- get_available_playoff_rounds(seasons[1])
      if (length(rounds) == 0) rounds <- c("Conference Games" = "conference_games")
      
      updateSelectizeInput(session, "round", choices = rounds, selected = rounds[1])
      initialized(TRUE)
    })
    
    # Update rounds when season changes
    observeEvent(input$season, {
      req(input$season, input$season != "", initialized())
      rounds <- get_available_playoff_rounds(input$season)
      if (length(rounds) == 0) rounds <- c("Conference Games" = "conference_games")
      updateSelectizeInput(session, "round", choices = rounds, selected = rounds[1])
    }, ignoreInit = TRUE)
    
    # Load data when round changes
    observeEvent(input$round, {
      req(input$season, input$round, input$season != "", input$round != "")
      
      tryCatch({
        data <- load_fanteam_playoffs_data(input$season, input$round)
        
        if (!is.null(data)) {
          # Extract unmatched warning if present
          rv$unmatched_warning <- attr(data, "unmatched_warning")
          rv$player_data <- data
          
          # Update team filter with logos
          if ("team" %in% names(data)) {
            teams <- sort(unique(data$team))
            
            updateSelectizeInput(session, "filter_team", 
                                 choices = setNames(c("all", teams), c("All Teams", teams)),
                                 selected = "all",
                                 options = list(
                                   placeholder = "All Teams",
                                   render = I("{
                  option: function(item, escape) {
                    if (item.value === 'all') {
                      return '<div class=\"option\" style=\"padding: 6px 8px; display: flex; align-items: center;\">' +
                        '<span style=\"font-weight: 600;\">All Teams</span></div>';
                    }
                    var logoUrl = 'https://a.espncdn.com/i/teamlogos/nfl/500/' + escape(item.value).toLowerCase() + '.png';
                    return '<div class=\"option\" style=\"padding: 4px 8px; display: flex; align-items: center; gap: 8px;\">' +
                      '<img src=\"' + logoUrl + '\" style=\"width: 20px; height: 20px; object-fit: contain;\">' +
                      '<span style=\"font-weight: 600;\">' + escape(item.label) + '</span></div>';
                  },
                  item: function(item, escape) {
                    if (item.value === 'all') {
                      return '<div style=\"display: flex; align-items: center;\">' +
                        '<span style=\"font-weight: 600;\">All Teams</span></div>';
                    }
                    var logoUrl = 'https://a.espncdn.com/i/teamlogos/nfl/500/' + escape(item.value).toLowerCase() + '.png';
                    return '<div style=\"display: flex; align-items: center; gap: 6px;\">' +
                      '<img src=\"' + logoUrl + '\" style=\"width: 18px; height: 18px; object-fit: contain;\">' +
                      '<span style=\"font-weight: 600;\">' + escape(item.label) + '</span></div>';
                  }
                }")
                                 )
            )
          }
          
          # Clear lineup
          rv$lineup <- list(QB = NULL, RB1 = NULL, RB2 = NULL, WR1 = NULL, WR2 = NULL, WR3 = NULL, TE = NULL, FLEX = NULL, DST = NULL)
          rv$selected_players <- character(0)
          
          # Clear rules
          rv$locked_players <- character(0)
          rv$excluded_players <- character(0)
          rv$team_max_rules <- list()
          rv$generated_lineups <- NULL
          
          # Update player dropdowns for rules section
          if ("player" %in% names(data) && "position" %in% names(data) && "team" %in% names(data)) {
            player_choices <- data %>%
              arrange(position, player) %>%
              mutate(label = sprintf("%s (%s, %s)", player, position, team)) %>%
              { setNames(.$player, .$label) }
            
            updateSelectizeInput(session, "lock_players", choices = player_choices, selected = character(0))
            updateSelectizeInput(session, "exclude_players", choices = player_choices, selected = character(0))
          }
          
          # Update team dropdown for team max rules
          if ("team" %in% names(data)) {
            teams <- sort(unique(data$team))
            updateSelectizeInput(session, "team_max_team", choices = setNames(teams, teams), selected = character(0))
          }
        }
      }, error = function(e) log_debug("Error:", e$message, level = "ERROR"))
    })
    
    # Position filter buttons
    update_position_buttons <- function(active) {
      for (pos in c("all", "QB", "RB", "WR", "TE", "DST")) {
        btn <- paste0("filter_", tolower(pos))
        if (tolower(pos) == tolower(active)) shinyjs::addClass(id = btn, class = "active")
        else shinyjs::removeClass(id = btn, class = "active")
      }
    }
    
    observeEvent(input$filter_all, { rv$position_filter <- "all"; update_position_buttons("all") })
    observeEvent(input$filter_qb, { rv$position_filter <- "QB"; update_position_buttons("QB") })
    observeEvent(input$filter_rb, { rv$position_filter <- "RB"; update_position_buttons("RB") })
    observeEvent(input$filter_wr, { rv$position_filter <- "WR"; update_position_buttons("WR") })
    observeEvent(input$filter_te, { rv$position_filter <- "TE"; update_position_buttons("TE") })
    observeEvent(input$filter_dst, { rv$position_filter <- "DST"; update_position_buttons("DST") })
    
    # Filtered data
    filtered_data <- reactive({
      req(rv$player_data)
      
      data <- rv$player_data
      
      # Position filter
      if (rv$position_filter != "all") {
        data <- data %>% filter(position == rv$position_filter)
      }
      
      # Team filter
      if (!is.null(input$filter_team) && input$filter_team != "all") {
        data <- data %>% filter(team == input$filter_team)
      }
      
      # Exclude selected players
      data <- data %>% filter(!player %in% rv$selected_players)
      
      # Exclude excluded players (from rules)
      if (length(rv$excluded_players) > 0) {
        data <- data %>% filter(!player %in% rv$excluded_players)
      }
      
      # Sort by projection
      data %>% arrange(desc(projection))
    })
    
    # Lineup stats
    lineup_stats <- reactive({
      salary <- sum(sapply(rv$lineup, function(p) if (!is.null(p)) p$salary[1] else 0))
      proj <- sum(sapply(rv$lineup, function(p) if (!is.null(p)) p$projection[1] else 0))
      n <- sum(!sapply(rv$lineup, is.null))
      
      list(
        salary_used = salary,
        remaining = FANTEAM_PLAYOFFS_CONFIG$salary_cap - salary,
        players = n,
        projection = proj
      )
    })
    
    # Player pool header
    output$player_pool_header <- renderUI({
      div(
        class = "player-pool-header",
        style = "display: grid; grid-template-columns: 1fr 70px 60px 60px 55px; gap: 0.5rem; padding: 0.5rem 0.75rem; background: var(--bg-secondary); border-bottom: 2px solid var(--outline); font-weight: 700; font-size: 0.65rem; text-transform: uppercase; color: var(--text-muted); position: sticky; top: 0; z-index: 10;",
        span(style = "text-align: left;", "Player"),
        span(style = "text-align: center;", "Salary"),
        span(style = "text-align: center;", "Proj"),
        span(style = "text-align: center;", "Ceil"),
        span(style = "text-align: center;", "Val")
      )
    })
    
    # Player pool rows (like handbuild)
    output$player_pool <- renderUI({
      req(rv$player_data)
      
      data <- filtered_data()
      stats <- lineup_stats()
      
      if (nrow(data) == 0) {
        return(div(style = "padding: 2rem; text-align: center; color: var(--text-muted);", "No players available"))
      }
      
      player_rows <- lapply(1:nrow(data), function(i) {
        p <- data[i, ]
        can_afford <- p$salary[1] <= stats$remaining
        
        if (!can_afford) {
          row_class <- "player-pool-row player-pool-row--unaffordable"
          row_style <- "opacity: 0.7; cursor: not-allowed;"
          clickable <- FALSE
        } else {
          row_class <- "player-pool-row player-pool-row--available"
          row_style <- "cursor: pointer;"
          clickable <- TRUE
        }
        
        # Get team and headshot safely
        player_team <- if ("team" %in% names(p) && !is.na(p$team[1])) p$team[1] else "???"
        headshot_url <- if ("headshot_url" %in% names(p) && !is.na(p$headshot_url[1])) {
          p$headshot_url[1]
        } else {
          "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
        }
        team_bg <- if ("team_bg_color" %in% names(p) && !is.na(p$team_bg_color[1])) {
          p$team_bg_color[1]
        } else {
          "#E0E0E0"
        }
        
        row_content <- div(
          class = row_class,
          style = sprintf("display: grid; grid-template-columns: 1fr 70px 60px 60px 55px; gap: 0.5rem; padding: 0.4rem 0.75rem; align-items: center; border-bottom: 1px solid var(--bg-secondary); %s", row_style),
          `data-player` = p$player[1],
          
          # Player cell: Headshot + Name/Team stacked (no position badge)
          div(
            style = "display: flex; align-items: center; gap: 0.5rem;",
            create_headshot_html(headshot_url, team_bg, "tiny", p$position[1], player_team),
            div(
              div(style = "font-weight: 600; font-size: 0.8rem; line-height: 1.2;", p$player[1]),
              div(style = "font-size: 0.65rem; color: var(--text-muted); line-height: 1.1;", player_team)
            )
          ),
          
          # Salary
          div(
            style = sprintf("text-align: center; font-weight: 600; font-size: 0.85rem; %s",
                            if (!can_afford) "color: var(--accent-red);" else ""),
            sprintf("£%.1f", p$salary[1])
          ),
          
          # Projection
          div(style = "text-align: center; font-size: 0.85rem; color: var(--text-secondary);",
              sprintf("%.1f", p$projection[1])),
          
          # Ceiling
          div(style = "text-align: center; font-size: 0.85rem; color: var(--text-secondary);",
              sprintf("%.1f", if ("ceiling" %in% names(p) && !is.na(p$ceiling[1])) p$ceiling[1] else 0)),
          
          # Value
          div(style = "text-align: center; font-size: 0.85rem; font-weight: 600; color: var(--text-muted);",
              sprintf("%.2f", p$value[1]))
        )
        
        if (clickable) {
          escaped_name <- gsub("'", "\\\\'", p$player[1])
          tags$div(
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("add_player_click"), escaped_name),
            row_content
          )
        } else {
          row_content
        }
      })
      
      tagList(player_rows)
    })
    
    # Add player from click
    observeEvent(input$add_player_click, {
      req(input$add_player_click != "", rv$player_data)
      
      player_name <- input$add_player_click
      player_row <- rv$player_data %>% filter(player == player_name)
      
      if (nrow(player_row) == 0) return()
      
      pos <- player_row$position[1]
      slot <- find_available_slot_playoffs(pos, rv$lineup)
      
      if (!is.null(slot)) {
        rv$lineup[[slot]] <- player_row
        rv$selected_players <- c(rv$selected_players, player_name)
      }
    })
    
    # Lineup display
    output$lineup_display <- renderUI({
      slots <- FANTEAM_PLAYOFFS_CONFIG$roster_slots
      
      slot_cards <- lapply(slots, function(slot) {
        player <- rv$lineup[[slot]]
        display_label <- gsub("[0-9]", "", slot)
        if (slot == "FLEX") display_label <- "FLX"
        
        if (is.null(player)) {
          div(
            class = "lineup-slot lineup-slot--empty",
            create_position_badge(display_label, "small"),
            tags$span(style = "flex: 1; padding-left: 0.5rem; color: var(--text-muted); font-style: italic; font-size: 0.8rem;", "Empty")
          )
        } else {
          player_team <- if ("team" %in% names(player)) player$team[1] else "???"
          
          div(
            class = "lineup-slot lineup-slot--filled",
            create_position_badge(display_label, "small"),
            div(style = "margin-left: 0.3rem;",
                create_headshot_html(player$headshot_url[1], player$team_bg_color[1], "small", player$position[1], player_team)
            ),
            div(
              style = "flex: 1; padding-left: 0.3rem;",
              div(style = "font-weight: 600; font-size: 0.85rem;", player$player[1]),
              div(style = "font-size: 0.75rem; color: var(--text-muted);",
                  sprintf("%s • £%.1f • %.1f pts", player_team, player$salary[1], player$projection[1]))
            ),
            actionButton(
              ns(paste0("remove_", slot)), icon("times"), class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_player"), slot)
            )
          )
        }
      })
      
      tagList(slot_cards)
    })
    
    # Remove player
    observeEvent(input$remove_player, {
      slot <- input$remove_player
      req(slot)
      
      player <- rv$lineup[[slot]]
      if (!is.null(player)) {
        rv$selected_players <- setdiff(rv$selected_players, player$player[1])
        rv$lineup[[slot]] <- NULL
      }
    })
    
    # Clear lineup
    observeEvent(input$clear_lineup, {
      rv$lineup <- list(QB = NULL, RB1 = NULL, RB2 = NULL, WR1 = NULL, WR2 = NULL, WR3 = NULL, TE = NULL, FLEX = NULL, DST = NULL)
      rv$selected_players <- character(0)
    })
    
    # Autocomplete - uses LP optimizer from nfl_optimizer.R
    observeEvent(input$optimize_remaining, {
      req(rv$player_data)
      
      log_debug(">>> Autocomplete triggered (using LP optimizer)", level = "INFO")
      
      # Get currently selected players to lock them
      locked <- rv$selected_players
      excluded <- rv$excluded_players
      team_max_rules <- rv$team_max_rules
      
      log_debug(">>> Locked players:", length(locked), level = "INFO")
      log_debug(">>> Excluded players:", length(excluded), level = "INFO")
      log_debug(">>> Team max rules:", length(team_max_rules), level = "INFO")
      
      # Convert team_max_rules to list format expected by optimizer
      team_max_list <- if (length(team_max_rules) > 0) {
        lapply(team_max_rules, function(r) list(team = r$team, max = r$max))
      } else {
        NULL
      }
      
      # Use the LP optimizer
      optimal <- tryCatch({
        optimize_lineup_lp(
          players = rv$player_data,
          projection_col = "projection",
          salary_cap = FANTEAM_PLAYOFFS_CONFIG$salary_cap,
          locked_players = if (length(locked) > 0) locked else NULL,
          excluded_players = if (length(excluded) > 0) excluded else NULL,
          team_max_rules = team_max_list
        )
      }, error = function(e) {
        log_debug(">>> Optimizer error:", e$message, level = "ERROR")
        NULL
      })
      
      if (is.null(optimal)) {
        log_debug(">>> No optimal lineup found", level = "WARN")
        showNotification("Could not find valid lineup", type = "warning")
        return()
      }
      
      log_debug(">>> Optimal lineup found:", sum(optimal$projection), "pts,", sum(optimal$salary), "salary", level = "INFO")
      
      # Clear current lineup and rebuild with optimal
      rv$lineup <- list(QB = NULL, RB1 = NULL, RB2 = NULL, WR1 = NULL, WR2 = NULL, WR3 = NULL, TE = NULL, FLEX = NULL, DST = NULL)
      rv$selected_players <- character(0)
      
      # Assign players to slots
      rb_count <- 0
      wr_count <- 0
      flex_filled <- FALSE
      
      for (i in 1:nrow(optimal)) {
        p <- optimal[i, ]
        # Need to get full player data row for headshots etc
        player_row <- rv$player_data %>% filter(player == p$player)
        
        if (nrow(player_row) == 0) next
        
        pos <- p$position
        
        slot <- if (pos == "QB") {
          "QB"
        } else if (pos == "RB") {
          rb_count <- rb_count + 1
          if (rb_count <= 2) paste0("RB", rb_count) else if (!flex_filled) { flex_filled <- TRUE; "FLEX" } else NULL
        } else if (pos == "WR") {
          wr_count <- wr_count + 1
          if (wr_count <= 3) paste0("WR", wr_count) else if (!flex_filled) { flex_filled <- TRUE; "FLEX" } else NULL
        } else if (pos == "TE") {
          if (is.null(rv$lineup[["TE"]])) "TE" else if (!flex_filled) { flex_filled <- TRUE; "FLEX" } else NULL
        } else if (pos == "DST") {
          "DST"
        } else {
          NULL
        }
        
        if (!is.null(slot)) {
          rv$lineup[[slot]] <- player_row
          rv$selected_players <- c(rv$selected_players, p$player)
          log_debug(">>>  ", slot, ":", p$player, "(", p$position, ") £", p$salary, "->", p$projection, "pts", level = "INFO")
        }
      }
      
      log_debug(">>> Autocomplete done", level = "INFO")
    })
    
    # =========================================================================
    # RULES: LOCK/EXCLUDE
    # =========================================================================
    
    # Apply lock
    observeEvent(input$apply_lock, {
      req(input$lock_players)
      rv$locked_players <- unique(c(rv$locked_players, input$lock_players))
      updateSelectizeInput(session, "lock_players", selected = character(0))
    })
    
    # Apply exclude
    observeEvent(input$apply_exclude, {
      req(input$exclude_players)
      rv$excluded_players <- unique(c(rv$excluded_players, input$exclude_players))
      updateSelectizeInput(session, "exclude_players", selected = character(0))
    })
    
    # Remove locked
    observeEvent(input$remove_locked, {
      rv$locked_players <- setdiff(rv$locked_players, input$remove_locked)
    })
    
    # Remove excluded
    observeEvent(input$remove_excluded, {
      rv$excluded_players <- setdiff(rv$excluded_players, input$remove_excluded)
    })
    
    # Lock/Exclude display
    output$lock_exclude_display <- renderUI({
      locked <- rv$locked_players
      excluded <- rv$excluded_players
      player_data <- rv$player_data
      
      if (length(locked) == 0 && length(excluded) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 1rem;",
        
        # Locked players
        lapply(locked, function(p) {
          player_info <- if (!is.null(player_data)) player_data %>% filter(player == p) else NULL
          pos_team <- if (!is.null(player_info) && nrow(player_info) > 0) {
            sprintf("%s, %s", player_info$position[1], player_info$team[1])
          } else ""
          
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-sage); border-radius: 6px;",
            div(
              style = "background: var(--accent-sage); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("lock"), " LOCKED"
            ),
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              p,
              if (pos_team != "") span(style = "font-weight: 400; color: var(--text-muted); font-size: 0.8rem; margin-left: 0.5rem;", paste0("(", pos_team, ")"))
            ),
            actionButton(
              ns(paste0("remove_locked_", gsub("[^a-zA-Z0-9]", "_", p))),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_locked"), p)
            )
          )
        }),
        
        # Excluded players
        lapply(excluded, function(p) {
          player_info <- if (!is.null(player_data)) player_data %>% filter(player == p) else NULL
          pos_team <- if (!is.null(player_info) && nrow(player_info) > 0) {
            sprintf("%s, %s", player_info$position[1], player_info$team[1])
          } else ""
          
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-coral); border-radius: 6px;",
            div(
              style = "background: var(--accent-coral); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("ban"), " EXCLUDED"
            ),
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              p,
              if (pos_team != "") span(style = "font-weight: 400; color: var(--text-muted); font-size: 0.8rem; margin-left: 0.5rem;", paste0("(", pos_team, ")"))
            ),
            actionButton(
              ns(paste0("remove_excluded_", gsub("[^a-zA-Z0-9]", "_", p))),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_excluded"), p)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # RULES: TEAM MAX
    # =========================================================================
    
    # Add team max rule
    observeEvent(input$add_team_max_rule, {
      req(input$team_max_team, input$team_max_value)
      
      rule_id <- paste0("rule_", as.integer(Sys.time()))
      new_rule <- list(
        id = rule_id,
        team = input$team_max_team,
        max = input$team_max_value
      )
      
      rv$team_max_rules[[rule_id]] <- new_rule
      updateSelectizeInput(session, "team_max_team", selected = character(0))
    })
    
    # Remove team max rule
    observeEvent(input$remove_team_max_rule, {
      rule_id <- input$remove_team_max_rule
      rv$team_max_rules <- rv$team_max_rules[names(rv$team_max_rules) != rule_id]
    })
    
    # Team max rules display
    output$team_max_rules_display <- renderUI({
      rules <- rv$team_max_rules
      
      if (length(rules) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 1rem;",
        lapply(rules, function(rule) {
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-frost); border-radius: 6px;",
            div(
              style = "background: var(--accent-frost); color: var(--text-primary); padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("users"), " MAX"
            ),
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              sprintf("%s: max %d players", rule$team, rule$max)
            ),
            actionButton(
              ns(paste0("remove_team_max_", rule$id)),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_team_max_rule"), rule$id)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # GENERATE LINEUPS
    # =========================================================================
    
    observeEvent(input$generate, {
      req(rv$player_data)
      
      log_debug(">>> Generate lineups triggered", level = "INFO")
      
      locked <- rv$locked_players
      excluded <- rv$excluded_players
      team_max_rules <- rv$team_max_rules
      
      log_debug(">>> Locked:", length(locked), "Excluded:", length(excluded), "Team rules:", length(team_max_rules), level = "INFO")
      
      showNotification("Generating lineups...", type = "message", id = "gen_notif", duration = NULL)
      
      tryCatch({
        # Prepare player data - need blended column for generate_lineups_with_variance
        players_for_gen <- rv$player_data %>%
          mutate(blended = projection)  # Use projection as blended
        
        # Convert team_max_rules to list format expected by optimizer
        team_max_list <- if (length(team_max_rules) > 0) {
          lapply(team_max_rules, function(r) list(team = r$team, max = r$max))
        } else {
          NULL
        }
        
        lineups <- generate_lineups_with_variance(
          players = players_for_gen,
          num_lineups = input$num_lineups %||% 10,
          salary_cap = FANTEAM_PLAYOFFS_CONFIG$salary_cap,
          variance_pct = input$variance_pct %||% 15,
          locked_players = if (length(locked) > 0) locked else NULL,
          excluded_players = if (length(excluded) > 0) excluded else NULL,
          adjustments = list(),
          stacking_rules = list(),
          stack_game = "",
          min_game_players = 4,
          corr_rules = list(),
          team_max_rules = team_max_list
        )
        
        rv$generated_lineups <- lineups
        
        removeNotification("gen_notif")
        showNotification(
          sprintf("Generated %d lineups", length(lineups)),
          type = "message", duration = 3
        )
        
        log_debug(">>> Generated", length(lineups), "lineups", level = "INFO")
        
      }, error = function(e) {
        log_debug(">>> Generation error:", e$message, level = "ERROR")
        removeNotification("gen_notif")
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Lineup summary stats
    output$lineup_summary_stats <- renderUI({
      lineups <- rv$generated_lineups
      if (is.null(lineups) || length(lineups) == 0) return(NULL)
      
      n <- length(lineups)
      projections <- sapply(lineups, function(l) sum(l$projection))
      salaries <- sapply(lineups, function(l) sum(l$salary))
      
      div(
        style = "display: flex; justify-content: center; gap: 3rem; margin-bottom: 1rem;",
        div(
          style = "text-align: center;",
          div(style = "font-size: 1.5rem; font-weight: 700; color: var(--text-primary);", n),
          div(style = "font-size: 0.8rem; color: var(--text-muted);", "Lineups")
        ),
        div(
          style = "text-align: center;",
          div(style = "font-size: 1.5rem; font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", mean(projections))),
          div(style = "font-size: 0.8rem; color: var(--text-muted);", "Avg Projection")
        ),
        div(
          style = "text-align: center;",
          div(style = "font-size: 1.5rem; font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", max(projections))),
          div(style = "font-size: 0.8rem; color: var(--text-muted);", "Best Lineup")
        ),
        div(
          style = "text-align: center;",
          div(style = "font-size: 1.5rem; font-weight: 700; color: var(--accent-frost);", sprintf("£%.1f", mean(salaries))),
          div(style = "font-size: 0.8rem; color: var(--text-muted);", "Avg Salary")
        )
      )
    })
    
    # Results panel - single column, players in rows of 2
    output$results_panel <- renderUI({
      lineups <- rv$generated_lineups
      
      if (is.null(lineups) || length(lineups) == 0) {
        return(
          div(
            class = "text-muted text-center py-4",
            "No lineups generated yet"
          )
        )
      }
      
      # Sort by projection
      lineup_projections <- sapply(lineups, function(l) sum(l$projection))
      sorted_indices <- order(lineup_projections, decreasing = TRUE)
      
      player_data <- rv$player_data
      locked <- rv$locked_players
      
      div(
        # Lineup cards - single column
        div(
          style = "display: flex; flex-direction: column; gap: 1rem; max-width: 600px; margin: 0 auto;",
          lapply(sorted_indices, function(i) {
            lineup <- lineups[[i]]
            salary <- sum(lineup$salary)
            proj <- sum(lineup$projection)
            
            # Add headshot info from player_data
            if (!is.null(player_data)) {
              lineup <- lineup %>%
                left_join(
                  player_data %>% select(player, headshot_url, team_bg_color),
                  by = "player",
                  suffix = c("", "_pd")
                ) %>%
                mutate(
                  headshot_url = coalesce(headshot_url, headshot_url_pd, "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"),
                  team_bg_color = coalesce(team_bg_color, team_bg_color_pd, "#E0E0E0")
                ) %>%
                select(-any_of(c("headshot_url_pd", "team_bg_color_pd")))
            }
            
            div(
              style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
              
              # Header
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px dashed var(--bg-secondary);",
                tags$span(
                  style = "font-weight: 800; text-transform: uppercase; font-size: 0.9rem;",
                  sprintf("Lineup %d", i)
                ),
                div(
                  style = "display: flex; gap: 1rem; font-size: 0.9rem;",
                  tags$span(style = "font-weight: 700; color: var(--accent-coral);", sprintf("%.1f pts", proj)),
                  tags$span(style = "color: var(--text-muted);", sprintf("£%.1f", salary))
                )
              ),
              
              # Players - 2 per row
              div(
                style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 0.5rem;",
                lapply(1:nrow(lineup), function(j) {
                  player <- lineup[j, ]
                  
                  headshot_url <- player$headshot_url[1] %||% "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
                  team_bg <- player$team_bg_color[1] %||% "#E0E0E0"
                  is_locked <- player$player[1] %in% locked
                  opacity_style <- if (is_locked) "opacity: 0.6;" else ""
                  
                  div(
                    style = sprintf(
                      "display: flex; align-items: center; gap: 0.5rem; padding: 0.4rem 0.5rem; background: var(--bg-tertiary); border-radius: 6px; %s",
                      opacity_style
                    ),
                    
                    # Position badge
                    tags$span(
                      style = "background: var(--text-primary); color: white; padding: 0.15rem 0.35rem; border-radius: 4px; font-weight: 700; font-size: 0.7rem; min-width: 32px; text-align: center;",
                      player$position[1]
                    ),
                    
                    # Headshot
                    div(
                      style = sprintf(
                        "width: 28px; height: 28px; border-radius: 50%%; background: %s; overflow: hidden; flex-shrink: 0;",
                        team_bg
                      ),
                      if (player$position[1] == "DST") {
                        tags$img(
                          src = sprintf("nfl_logos/%s.webp", player$team[1]),
                          style = "width: 100%; height: 100%; object-fit: contain;",
                          onerror = "this.style.display='none'"
                        )
                      } else {
                        tags$img(
                          src = headshot_url,
                          style = "width: 100%; height: 100%; object-fit: cover;",
                          onerror = "this.src='https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png'"
                        )
                      }
                    ),
                    
                    # Player name
                    tags$span(
                      style = "flex: 1; font-weight: 600; font-size: 0.8rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
                      player$player[1]
                    ),
                    
                    # Projection
                    tags$span(
                      style = "font-weight: 700; font-size: 0.8rem; color: var(--accent-coral);",
                      sprintf("%.1f", player$projection[1])
                    )
                  )
                })
              )
            )
          })
        )
      )
    })
    
    # Stats outputs
    output$salary_used <- renderText(sprintf("£%.1f", lineup_stats()$salary_used))
    output$salary_remaining <- renderText(sprintf("£%.1f", lineup_stats()$remaining))
    output$players_selected <- renderText(sprintf("%d / 9", lineup_stats()$players))
    output$total_projection <- renderText(sprintf("%.1f", lineup_stats()$projection))
    
    # Unmatched players warning UI
    output$unmatched_warning <- renderUI({
      warning <- rv$unmatched_warning
      if (is.null(warning)) return(NULL)
      
      # Build warning message
      messages <- c()
      
      if (!is.null(warning$count) && warning$count > 0) {
        messages <- c(messages, sprintf("%d Unmatched Players", warning$count))
      }
      
      if (length(messages) == 0) return(NULL)
      
      # Combine player lists for display
      all_players <- c()
      if (!is.null(warning$players)) all_players <- c(all_players, warning$players)
      if (!is.null(warning$salary_players)) all_players <- c(all_players, warning$salary_players)
      
      div(
        class = "alert alert-warning",
        style = "margin-bottom: 1rem; padding: 0.75rem 1rem; border-radius: 8px; background: #FFF3CD; border: 1px solid #FFECB5; display: flex; align-items: flex-start; gap: 0.75rem;",
        
        icon("exclamation-triangle", style = "color: #856404; margin-top: 0.2rem;"),
        
        div(
          style = "flex: 1;",
          div(
            style = "font-weight: 600; color: #856404; margin-bottom: 0.25rem;",
            paste(messages, collapse = " • "),
            span(style = "font-weight: 400; margin-left: 0.5rem;", "(projection ≥ 5pts but not in salary file)")
          ),
          div(
            style = "font-size: 0.85rem; color: #664d03;",
            paste(all_players, collapse = ", ")
          ),
          tags$a(
            href = "#",
            style = "font-size: 0.8rem; color: #533f03; margin-top: 0.25rem; display: inline-block;",
            onclick = "Shiny.setInputValue('toggle_warning_details', Math.random()); return false;",
            "View players"
          )
        )
      )
    })
  })
}

# =============================================================================
# HELPER
# =============================================================================

find_available_slot_playoffs <- function(position, lineup) {
  slots <- switch(position,
                  "QB" = "QB",
                  "RB" = c("RB1", "RB2"),
                  "WR" = c("WR1", "WR2", "WR3"),
                  "TE" = "TE",
                  "DST" = "DST",
                  character(0)
  )
  
  for (slot in slots) {
    if (is.null(lineup[[slot]])) return(slot)
  }
  
  if (position %in% c("RB", "WR", "TE") && is.null(lineup[["FLEX"]])) return("FLEX")
  
  NULL
}