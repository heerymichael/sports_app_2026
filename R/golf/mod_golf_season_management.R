# =============================================================================
# Module: Golf Season Long Management
# 
# Weekly roster management for FanTeam Season Long contest:
# - Load existing rosters from Google Sheets
# - Match players to weekly tournament projections
# - Recommend best 6 starters from each 10-man roster
# - Track transfers over time
# =============================================================================

library(googlesheets4)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Google Sheet IDs
GOLF_ROSTER_SHEET_ID <- "18aDz1kwgeJTqyDRxlwX3COl3gCmzetT6vuTao8l1JQE"
GOLF_PROJECTIONS_SHEET_ID <- "1yJJAOv5hzNZagYUG7FLpNmRIRC76L0fJNGPbzK61lbw"

# Null coalesce operator
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# =============================================================================
# NAME CORRECTIONS (for matching roster names to projection names)
# =============================================================================

GOLF_MANAGEMENT_NAME_CORRECTIONS <- list(
  # Capitalization fixes
  "Robert Macintyre" = "Robert MacIntyre",
  "Maverick Mcnealy" = "Maverick McNealy",
  "Denny Mccarthy" = "Denny McCarthy",
  "Max Mcgreevy" = "Max McGreevy",
  
  # Name variants
  "Christopher Gotterup" = "Chris Gotterup",
  "Henry Lebioda" = "Hank Lebioda",
  "Kota Yuta Kaneko" = "Kota Kaneko",
  "Cam Davis" = "Cameron Davis",
  "Matt McCarty" = "Matthew McCarty",
  "Matthias Schmid" = "Matti Schmid",
  "Zach Bauchou" = "Zachary Bauchou",
  "Seong-Hyeon Kim" = "Seonghyeon Kim",
  "Byeong-Hun An" = "Byeong Hun An",
  "Sung-Jae Im" = "Sungjae Im",
  "Hao-Tong Li" = "Haotong Li",
  "Ze-Cheng Dou" = "Zecheng Dou",
  "Adrien Dumont" = "Adrien Dumont De Chassart",
  "Alex Noren" = "Alexander Noren",
  "Nico Echavarria" = "Nicolas Echavarria",
  "Kris Ventura" = "Kristoffer Ventura",
  "Jordan L Smith" = "Jordan Smith",
  "Sam Stevens" = "Samuel Stevens"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Normalize player name for matching
normalize_management_name <- function(name) {
  if (is.na(name) || name == "") return("")
  
  # Check direct correction first
  if (name %in% names(GOLF_MANAGEMENT_NAME_CORRECTIONS)) {
    name <- GOLF_MANAGEMENT_NAME_CORRECTIONS[[name]]
  }
  
  # Lowercase and clean
  name_lower <- tolower(trimws(name))
  
  # Check lowercase correction
  if (name_lower %in% tolower(names(GOLF_MANAGEMENT_NAME_CORRECTIONS))) {
    idx <- which(tolower(names(GOLF_MANAGEMENT_NAME_CORRECTIONS)) == name_lower)
    name <- GOLF_MANAGEMENT_NAME_CORRECTIONS[[idx]]
    name_lower <- tolower(name)
  }
  
  # Handle "LastName, FirstName" format
  if (grepl(",", name_lower)) {
    parts <- strsplit(name_lower, ",")[[1]]
    if (length(parts) == 2) {
      name_lower <- paste(trimws(parts[2]), trimws(parts[1]))
    }
  }
  
  # Remove special characters, normalize spaces
  name_lower <- gsub("[^a-z0-9 ]", " ", name_lower)
  name_lower <- gsub("\\s+", " ", name_lower)
  name_lower <- trimws(name_lower)
  
  return(name_lower)
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Get available weeks from roster sheet columns
get_roster_weeks <- function() {
  log_debug("get_roster_weeks() called", level = "DEBUG")
  
  tryCatch({
    # Read just the header row to get column names
    roster_data <- googlesheets4::read_sheet(
      GOLF_ROSTER_SHEET_ID,
      range = "1:1"
    )
    
    # Get columns that start with "Week"
    week_cols <- names(roster_data)[grepl("^Week", names(roster_data))]
    
    log_debug("Found week columns:", paste(week_cols, collapse = ", "), level = "INFO")
    return(week_cols)
    
  }, error = function(e) {
    log_debug("Error getting roster weeks:", e$message, level = "ERROR")
    return(c("Week 1"))
  })
}

#' Get available tournaments from projections sheet
get_projection_tournaments <- function() {
  log_debug("get_projection_tournaments() called", level = "DEBUG")
  
  tryCatch({
    ss <- googlesheets4::gs4_get(GOLF_PROJECTIONS_SHEET_ID)
    tournaments <- ss$sheets$name
    
    log_debug("Found tournaments:", paste(tournaments, collapse = ", "), level = "INFO")
    return(tournaments)
    
  }, error = function(e) {
    log_debug("Error getting tournaments:", e$message, level = "ERROR")
    return(character(0))
  })
}

#' Load roster data from Google Sheet for a specific week
load_roster_data <- function(week_col = "Week 1") {
  log_debug("load_roster_data() for week:", week_col, level = "INFO")
  
  tryCatch({
    roster_data <- googlesheets4::read_sheet(GOLF_ROSTER_SHEET_ID) %>%
      as.data.frame()
    
    log_debug("Raw roster columns:", paste(names(roster_data), collapse = ", "), level = "DEBUG")
    
    # Check that required columns exist
    if (!"Roster" %in% names(roster_data)) {
      log_debug("Missing 'Roster' column", level = "ERROR")
      return(NULL)
    }
    
    if (!week_col %in% names(roster_data)) {
      log_debug("Missing week column:", week_col, level = "ERROR")
      return(NULL)
    }
    
    # Fill down the Roster column (since it only appears on first row of each group)
    current_roster <- NA
    for (i in 1:nrow(roster_data)) {
      if (!is.na(roster_data$Roster[i]) && roster_data$Roster[i] != "") {
        current_roster <- roster_data$Roster[i]
      } else {
        roster_data$Roster[i] <- current_roster
      }
    }
    
    # Select relevant columns and rename
    result <- roster_data %>%
      select(
        roster = Roster,
        slot = Slot,
        player_name = !!sym(week_col)
      ) %>%
      filter(!is.na(player_name) & player_name != "") %>%
      mutate(
        match_key = sapply(player_name, normalize_management_name)
      )
    
    log_debug("Loaded", nrow(result), "roster entries", level = "INFO")
    return(result)
    
  }, error = function(e) {
    log_debug("Error loading roster data:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load projections from Google Sheet for a specific tournament
load_tournament_projections <- function(tournament_name) {
  log_debug("load_tournament_projections() for:", tournament_name, level = "INFO")
  
  tryCatch({
    projections_raw <- googlesheets4::read_sheet(
      GOLF_PROJECTIONS_SHEET_ID,
      sheet = tournament_name
    ) %>%
      janitor::clean_names()
    
    log_debug("Raw projection columns:", paste(names(projections_raw), collapse = ", "), level = "DEBUG")
    
    # Map columns flexibly
    projections <- projections_raw
    
    # Find player name column
    name_col <- intersect(names(projections), c("golfer", "player_name", "player", "name"))
    if (length(name_col) > 0) {
      projections <- projections %>% rename(player_name = !!name_col[1])
    } else {
      log_debug("No player name column found", level = "ERROR")
      return(NULL)
    }
    
    # Find projection column (median/points)
    proj_col <- intersect(names(projections), c(
      "dk_points", "median", "projection", "proj", "pts", "fpts",
      "fantasy_points", "points", "projected_points"
    ))
    if (length(proj_col) > 0) {
      log_debug("Using projection column:", proj_col[1], level = "DEBUG")
      projections <- projections %>%
        mutate(projection = as.numeric(.data[[proj_col[1]]]))
    } else {
      log_debug("No projection column found", level = "WARN")
      projections$projection <- NA_real_
    }
    
    # Find salary column (for underdog calculation)
    salary_col <- intersect(names(projections), c(
      "salary", "dk_salary", "price", "sal", "cost"
    ))
    if (length(salary_col) > 0) {
      log_debug("Using salary column:", salary_col[1], level = "DEBUG")
      projections <- projections %>%
        mutate(salary = as.numeric(.data[[salary_col[1]]]))
    } else {
      log_debug("No salary column found - underdog bonus may not work correctly", level = "WARN")
      projections$salary <- NA_real_
    }
    
    # Find ownership column
    own_col <- intersect(names(projections), c(
      "dk_ownership", "ownership", "own", "own_large", "ownership_large"
    ))
    if (length(own_col) > 0) {
      projections <- projections %>%
        mutate(ownership = as.numeric(gsub("%", "", .data[[own_col[1]]])))
    } else {
      projections$ownership <- NA_real_
    }
    
    # Create match key
    projections <- projections %>%
      mutate(match_key = sapply(player_name, normalize_management_name)) %>%
      select(player_name, projection, salary, ownership, match_key) %>%
      filter(!is.na(projection))
    
    log_debug("Loaded", nrow(projections), "projections", level = "INFO")
    return(projections)
    
  }, error = function(e) {
    log_debug("Error loading projections:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Match roster players to projections
match_roster_to_projections <- function(roster_data, projections) {
  log_debug("match_roster_to_projections() called", level = "DEBUG")
  
  if (is.null(roster_data) || is.null(projections)) return(NULL)
  
  # Join on match_key
  matched <- roster_data %>%
    left_join(
      projections %>% select(match_key, projection, salary, ownership, proj_name = player_name),
      by = "match_key"
    )
  
  # Log unmatched players
  unmatched <- matched %>% filter(is.na(projection)) %>% pull(player_name)
  if (length(unmatched) > 0) {
    log_debug("Unmatched players:", paste(unmatched, collapse = ", "), level = "WARN")
  }
  
  # Log salary availability
  has_salary <- sum(!is.na(matched$salary))
  log_debug("Players with salary data:", has_salary, "of", nrow(matched), level = "INFO")
  
  matched_count <- sum(!is.na(matched$projection))
  log_debug("Matched", matched_count, "of", nrow(matched), "players", level = "INFO")
  
  return(matched)
}

#' Select optimal 6 players from a roster considering Captain AND Underdog bonuses
#' Captain = highest projection in lineup (1.25x) - always assigned to top projected
#' Underdog = cheapest salary in lineup (1.25x)
#' Uses brute force over all C(n,6) combinations to find true optimal
select_best_six <- function(roster_players) {
  
  # Separate matched and unmatched players
  matched_players <- roster_players %>% filter(!is.na(projection))
  unmatched_players <- roster_players %>% filter(is.na(projection))
  
  n_matched <- nrow(matched_players)
  
  # If 6 or fewer matched players, they're all starters by default
  if (n_matched <= 6) {
    max_proj <- max(matched_players$projection, na.rm = TRUE)
    min_sal <- if ("salary" %in% names(matched_players) && sum(!is.na(matched_players$salary)) > 0) {
      min(matched_players$salary, na.rm = TRUE)
    } else {
      NA_real_
    }
    
    result <- matched_players %>%
      arrange(desc(projection)) %>%
      mutate(
        lineup_rank = row_number(),
        is_starter = TRUE,
        is_captain = projection == max_proj,
        is_underdog = !is.na(min_sal) & !is.na(salary) & salary == min_sal & !is_captain,
        effective_projection = case_when(
          is_captain ~ projection * 1.25,
          is_underdog ~ projection * 1.25,
          TRUE ~ projection
        ),
        is_unmatched = FALSE
      )
    
    # Add unmatched players at the bottom
    if (nrow(unmatched_players) > 0) {
      unmatched_result <- unmatched_players %>%
        mutate(
          lineup_rank = n_matched + row_number(),
          is_starter = FALSE,
          is_captain = FALSE,
          is_underdog = FALSE,
          effective_projection = NA_real_,
          is_unmatched = TRUE
        )
      result <- bind_rows(result, unmatched_result)
    }
    
    return(result)
  }
  
  # Check salary column exists and has data
  has_salary <- "salary" %in% names(matched_players) && sum(!is.na(matched_players$salary)) > 0
  
  if (!has_salary) {
    # Fallback to simple projection-based selection if no salary data
    matched_players <- matched_players %>%
      arrange(desc(projection)) %>%
      mutate(
        lineup_rank = row_number(),
        is_starter = row_number() <= 6,
        is_captain = row_number() == 1,
        is_underdog = FALSE,
        effective_projection = if_else(is_captain, projection * 1.25, projection),
        is_unmatched = FALSE
      )
    
    if (nrow(unmatched_players) > 0) {
      unmatched_result <- unmatched_players %>%
        mutate(
          lineup_rank = n_matched + row_number(),
          is_starter = FALSE,
          is_captain = FALSE,
          is_underdog = FALSE,
          effective_projection = NA_real_,
          is_unmatched = TRUE
        )
      matched_players <- bind_rows(matched_players, unmatched_result)
    }
    
    return(matched_players)
  }
  
  # Generate all combinations of 6 from n matched players
  combos <- combn(n_matched, 6)
  
  best_total <- -Inf
  best_combo <- NULL
  
  # Evaluate each combination
  for (i in 1:ncol(combos)) {
    idx <- combos[, i]
    lineup <- matched_players[idx, ]
    
    # Skip if any salary is NA (can't determine underdog)
    if (any(is.na(lineup$salary))) next
    
    # Captain = highest projection, Underdog = lowest salary
    captain_proj <- max(lineup$projection)
    underdog_proj <- lineup$projection[which.min(lineup$salary)]
    
    # Calculate effective total with bonuses
    total <- sum(lineup$projection)
    total <- total + captain_proj * 0.25      # Captain bonus
    total <- total + underdog_proj * 0.25     # Underdog bonus
    
    if (total > best_total) {
      best_total <- total
      best_combo <- idx
    }
  }
  
  # If no valid combo found (all had NA salaries), fall back to projection-only
  if (is.null(best_combo)) {
    log_debug("No valid lineup with salary data, falling back to projection-only", level = "WARN")
    matched_players <- matched_players %>%
      arrange(desc(projection)) %>%
      mutate(
        lineup_rank = row_number(),
        is_starter = row_number() <= 6,
        is_captain = row_number() == 1,
        is_underdog = FALSE,
        effective_projection = if_else(is_captain, projection * 1.25, projection),
        is_unmatched = FALSE
      )
    
    if (nrow(unmatched_players) > 0) {
      unmatched_result <- unmatched_players %>%
        mutate(
          lineup_rank = nrow(matched_players) + row_number(),
          is_starter = FALSE,
          is_captain = FALSE,
          is_underdog = FALSE,
          effective_projection = NA_real_,
          is_unmatched = TRUE
        )
      matched_players <- bind_rows(matched_players, unmatched_result)
    }
    
    return(matched_players)
  }
  
  # Build result with best lineup
  starter_indices <- best_combo
  bench_indices <- setdiff(1:n_matched, best_combo)
  
  starters <- matched_players[starter_indices, ]
  captain_player <- starters$player_name[which.max(starters$projection)]
  underdog_player <- starters$player_name[which.min(starters$salary)]
  
  # Arrange starters by projection (captain first)
  starters <- starters %>%
    arrange(desc(projection)) %>%
    mutate(
      lineup_rank = row_number(),
      is_starter = TRUE,
      is_captain = player_name == captain_player,
      is_underdog = player_name == underdog_player,
      effective_projection = case_when(
        is_captain ~ projection * 1.25,
        is_underdog ~ projection * 1.25,
        TRUE ~ projection
      ),
      is_unmatched = FALSE
    )
  
  # Add bench players
  if (length(bench_indices) > 0) {
    bench <- matched_players[bench_indices, ] %>%
      arrange(desc(projection)) %>%
      mutate(
        lineup_rank = 6 + row_number(),
        is_starter = FALSE,
        is_captain = FALSE,
        is_underdog = FALSE,
        effective_projection = projection,
        is_unmatched = FALSE
      )
    starters <- bind_rows(starters, bench)
  }
  
  # Add unmatched players at the bottom
  if (nrow(unmatched_players) > 0) {
    unmatched_result <- unmatched_players %>%
      mutate(
        lineup_rank = n_matched + row_number(),
        is_starter = FALSE,
        is_captain = FALSE,
        is_underdog = FALSE,
        effective_projection = NA_real_,
        is_unmatched = TRUE
      )
    starters <- bind_rows(starters, unmatched_result)
  }
  
  return(starters)
}

# =============================================================================
# UI
# =============================================================================

golf_season_management_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("golf_season_management_ui() called with id:", id, level = "INFO")
  
  # Get available weeks and tournaments
  weeks <- tryCatch(get_roster_weeks(), error = function(e) c("Week 1"))
  tournaments <- tryCatch(get_projection_tournaments(), error = function(e) character(0))
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Season Long Management"),
      tags$p(class = "text-muted", "Manage your 5 rosters - optimizes CPT (highest proj) + DOG (cheapest) bonuses")
    ),
    
    # =========================================================================
    # SETTINGS
    # =========================================================================
    ui_card(
      title = "Settings",
      color = GOLF_CARD_COLOR,
      
      fluidRow(
        column(3,
               selectizeInput(ns("week_select"), "Week",
                              choices = weeks,
                              selected = if (length(weeks) > 0) weeks[length(weeks)] else "Week 1"
               )
        ),
        column(5,
               selectizeInput(ns("tournament_select"), "Tournament",
                              choices = if (length(tournaments) > 0) tournaments else c("No tournaments found" = ""),
                              selected = if (length(tournaments) > 0) tournaments[1] else NULL
               )
        ),
        column(4,
               div(style = "margin-top: 25px;",
                   actionButton(ns("load_btn"), "Load & Match", class = "btn btn-primary w-100", icon = icon("sync"))
               )
        )
      ),
      
      # Status message
      uiOutput(ns("status_message"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # ROSTER LINEUPS
    # =========================================================================
    ui_card(
      title = "Recommended Lineups",
      color = GOLF_CARD_COLOR,
      
      uiOutput(ns("rosters_display"))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

golf_season_management_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("golf_season_management_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    rv <- reactiveValues(
      roster_data = NULL,
      projections = NULL,
      matched_data = NULL
    )
    
    # =========================================================================
    # LOAD DATA
    # =========================================================================
    observeEvent(input$load_btn, {
      req(input$week_select, input$tournament_select)
      req(input$tournament_select != "")
      
      log_debug(">>> Load button clicked", level = "INFO")
      log_debug(">>> Week:", input$week_select, level = "INFO")
      log_debug(">>> Tournament:", input$tournament_select, level = "INFO")
      
      showNotification("Loading roster data...", type = "message", duration = 2)
      
      # Load roster data
      roster_data <- load_roster_data(input$week_select)
      if (is.null(roster_data)) {
        showNotification("Failed to load roster data", type = "error")
        return()
      }
      rv$roster_data <- roster_data
      
      # Load projections
      showNotification("Loading projections...", type = "message", duration = 2)
      projections <- load_tournament_projections(input$tournament_select)
      if (is.null(projections)) {
        showNotification("Failed to load projections", type = "error")
        return()
      }
      rv$projections <- projections
      
      # Match data
      matched <- match_roster_to_projections(roster_data, projections)
      rv$matched_data <- matched
      
      matched_count <- sum(!is.na(matched$projection))
      unmatched_count <- nrow(matched) - matched_count
      
      showNotification(
        sprintf("Matched %d of %d players to projections", matched_count, nrow(matched)),
        type = if (unmatched_count > 0) "warning" else "message"
      )
    })
    
    # =========================================================================
    # STATUS MESSAGE
    # =========================================================================
    output$status_message <- renderUI({
      if (is.null(rv$matched_data)) {
        return(div(
          class = "text-muted mt-3",
          icon("info-circle"), " Select week and tournament, then click Load & Match"
        ))
      }
      
      total_count <- nrow(rv$matched_data)
      matched_count <- sum(!is.na(rv$matched_data$projection))
      unmatched_count <- total_count - matched_count
      
      if (unmatched_count == 0) {
        div(
          class = "alert alert-success mt-3 mb-0",
          icon("check-circle"), sprintf(" All %d players matched! CPT + DOG (1.25x each) optimized.", total_count)
        )
      } else {
        div(
          class = "alert alert-warning mt-3 mb-0",
          icon("exclamation-triangle"),
          sprintf(" %d of %d players matched. %d not playing this week (shown in coral).",
                  matched_count, total_count, unmatched_count)
        )
      }
    })
    
    # =========================================================================
    # ROSTERS DISPLAY
    # =========================================================================
    output$rosters_display <- renderUI({
      if (is.null(rv$matched_data)) {
        return(div(
          class = "text-muted text-center py-4",
          "Load data to see recommended lineups"
        ))
      }
      
      # Get unique rosters
      rosters <- unique(rv$matched_data$roster)
      
      # Create a card for each roster
      create_roster_card <- function(roster_name) {
        roster_players <- rv$matched_data %>%
          filter(roster == roster_name) %>%
          select_best_six()
        
        # Calculate totals for starters (using effective projections with bonuses)
        starters <- roster_players %>% filter(is_starter & !is_unmatched)
        total_projection <- sum(starters$effective_projection, na.rm = TRUE)
        
        # Count issues
        unmatched_count <- sum(roster_players$is_unmatched)
        
        div(
          style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
          
          # Header
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px dashed var(--bg-secondary);",
            span(style = "font-weight: 800; text-transform: uppercase; font-size: 0.9rem;", roster_name),
            div(
              style = "text-align: right;",
              div(style = "font-size: 0.65rem; text-transform: uppercase; color: var(--text-muted);", "Projected"),
              div(style = "font-size: 1.1rem; font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", total_projection))
            )
          ),
          
          # Player list
          div(
            style = "display: flex; flex-direction: column; gap: 0.25rem;",
            lapply(1:nrow(roster_players), function(i) {
              player <- roster_players[i, ]
              is_bench <- !player$is_starter
              is_captain <- player$is_captain && !player$is_unmatched
              is_underdog <- player$is_underdog && !player$is_unmatched
              is_unmatched <- player$is_unmatched
              
              # Determine row background
              row_bg <- if (is_unmatched) {
                "rgba(208, 135, 112, 0.25)"  # Light coral for unmatched
              } else if (is_captain) {
                "rgba(180, 142, 173, 0.25)"  # Light plum for captain
              } else if (is_underdog) {
                "rgba(235, 203, 139, 0.25)"  # Light gold for underdog
              } else if (is_bench) {
                "var(--bg-secondary)"
              } else {
                "var(--bg-tertiary)"
              }
              
              # Determine badge text and color
              badge_text <- if (is_unmatched) {
                "?"
              } else if (is_captain) {
                "CPT"
              } else if (is_underdog) {
                "DOG"
              } else {
                player$lineup_rank
              }
              
              badge_bg <- if (is_unmatched) {
                "background: var(--accent-coral); color: white;"
              } else if (is_captain) {
                "background: #B48EAD; color: white;"
              } else if (is_underdog) {
                "background: #EBCB8B; color: var(--text-primary);"
              } else if (is_bench) {
                "background: var(--text-muted); color: white;"
              } else {
                "background: var(--accent-sage); color: white;"
              }
              
              div(
                style = sprintf(
                  "display: flex; align-items: center; gap: 0.4rem; padding: 0.3rem 0.4rem; background: %s; border-radius: 4px; %s",
                  row_bg,
                  if (is_bench && !is_unmatched) "opacity: 0.6;" else ""
                ),
                
                # Rank/Role badge
                div(
                  style = sprintf(
                    "width: 28px; height: 24px; border-radius: 4px; display: flex; align-items: center; justify-content: center; font-size: 0.55rem; font-weight: 700; %s",
                    badge_bg
                  ),
                  badge_text
                ),
                
                # Player name
                div(
                  style = sprintf(
                    "flex: 1; font-weight: 600; font-size: 0.8rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; %s",
                    if (is_unmatched) "font-style: italic;" else ""
                  ),
                  player$player_name
                ),
                
                # Salary (show for starters to explain underdog)
                if (!is_unmatched && player$is_starter) {
                  div(
                    style = "text-align: right; width: 40px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "SAL"),
                    div(
                      style = sprintf("font-size: 0.7rem; %s", if (is_underdog) "color: #EBCB8B; font-weight: 700;" else "color: var(--text-secondary);"),
                      if ("salary" %in% names(player) && !is.na(player$salary)) sprintf("$%.1f", player$salary) else "—"
                    )
                  )
                },
                
                # Projection / Effective
                div(
                  style = "text-align: right; width: 50px;",
                  div(style = "font-size: 0.5rem; color: var(--text-muted);", 
                      if ((is_captain || is_underdog) && !is_unmatched) "EFF" else "PROJ"),
                  div(
                    style = sprintf(
                      "font-size: 0.75rem; font-weight: 600; %s",
                      if (is_unmatched) "color: var(--accent-coral);" 
                      else if (is_captain) "color: #B48EAD;"
                      else if (is_underdog) "color: #EBCB8B;"
                      else if (is_bench) "" 
                      else "color: var(--accent-coral);"
                    ),
                    if (is_unmatched) {
                      "N/A"
                    } else if (is_captain || is_underdog) {
                      sprintf("%.1f", player$effective_projection)
                    } else {
                      sprintf("%.1f", player$projection)
                    }
                  )
                ),
                
                # Ownership (if available and matched)
                if (!is_unmatched && !is.na(player$ownership)) {
                  div(
                    style = "text-align: right; width: 40px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "OWN"),
                    div(style = "font-size: 0.7rem; color: var(--text-secondary);", sprintf("%.0f%%", player$ownership))
                  )
                }
              )
            })
          ),
          
          # Footer legend
          div(
            style = "margin-top: 0.5rem; padding-top: 0.5rem; border-top: 1px solid var(--border); font-size: 0.65rem; color: var(--text-muted); display: flex; flex-wrap: wrap; gap: 0.75rem;",
            span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: #B48EAD; margin-right: 4px;"), "CPT (1.25x)"),
            span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: #EBCB8B; margin-right: 4px;"), "DOG (1.25x)"),
            span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: var(--accent-sage); margin-right: 4px;"), "Starter"),
            span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: var(--text-muted); margin-right: 4px;"), "Bench"),
            if (unmatched_count > 0) {
              span(tags$span(style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: var(--accent-coral); margin-right: 4px;"), "Not Playing")
            }
          )
        )
      }
      
      # Layout: 3 columns for 5 rosters
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
        lapply(rosters, create_roster_card)
      )
    })
    
    
  })
}

cat("Golf Season Management module loaded: golf_season_management_ui(), golf_season_management_server()\n")
cat("  Optimizes lineup selection with CPT (highest proj, 1.25x) + DOG (cheapest, 1.25x) bonuses\n")