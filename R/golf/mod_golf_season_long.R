# =============================================================================
# Module: Golf Season Long
# 
# FanTeam Season Long roster management:
# - 5 rosters with independent weight profiles
# - 10 golfers per team: 6 starters + 4 bench
# - Budget: 100M per roster
# - Google Sheets integration for salary and ETR rankings data
# =============================================================================

library(lpSolve)
library(googlesheets4)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Google Sheet configuration
GOLF_SEASON_LONG_SHEET_ID <- "1_OgRCMmmyGQhLNsAFvK3wvlFIjkMPmvOddIr3PdYhwg"
GOLF_SALARY_SHEET_NAME <- "Week 1"
GOLF_RANKINGS_SHEET_NAME <- "ETR Opening Rankings"

# Contest configuration
GOLF_SEASON_LONG_CONFIG <- list(
  roster_size = 10,
  active_lineup_size = 6,
  bench_size = 4,
  budget = 100,
  num_rosters = 5
)

# Null coalesce operator
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# =============================================================================
# NAME CORRECTIONS
# =============================================================================

GOLF_SEASON_LONG_NAME_CORRECTIONS <- list(
  "Robert Macintyre" = "Robert MacIntyre",
  "Maverick Mcnealy" = "Maverick McNealy",
  "Denny Mccarthy" = "Denny McCarthy",
  "Max Mcgreevy" = "Max McGreevy",
  "Christopher Gotterup" = "Chris Gotterup",
  "chris gotterup" = "Chris Gotterup",
  "Henry Lebioda" = "Hank Lebioda",
  "henry lebioda" = "Hank Lebioda",
  "Kota Yuta Kaneko" = "Kota Kaneko",
  "Cam Davis" = "Cameron Davis",
  "cam davis" = "Cameron Davis",
  "Matt McCarty" = "Matthew McCarty",
  "matt mccarty" = "Matthew McCarty",
  "Matthias Schmid" = "Matti Schmid",
  "matthias schmid" = "Matti Schmid",
  "Zach Bauchou" = "Zachary Bauchou",
  "zach bauchou" = "Zachary Bauchou",
  "Seong-Hyeon Kim" = "Seonghyeon Kim",
  "seong hyeon kim" = "Seonghyeon Kim",
  "Byeong-Hun An" = "Byeong Hun An",
  "byeong hun an" = "Byeong Hun An",
  "Sung-Jae Im" = "Sungjae Im",
  "sung jae im" = "Sungjae Im",
  "Hao-Tong Li" = "Haotong Li",
  "hao tong li" = "Haotong Li",
  "Ze-Cheng Dou" = "Zecheng Dou",
  "ze cheng dou" = "Zecheng Dou",
  "Adrien Dumont" = "Adrien Dumont De Chassart",
  "adrien dumont" = "Adrien Dumont De Chassart",
  "Alex Noren" = "Alexander Noren",
  "alex noren" = "Alexander Noren",
  "Nico Echavarria" = "Nicolas Echavarria",
  "nico echavarria" = "Nicolas Echavarria",
  "Kris Ventura" = "Kristoffer Ventura",
  "kris ventura" = "Kristoffer Ventura",
  "Jordan L Smith" = "Jordan Smith",
  "jordan l smith" = "Jordan Smith",
  "Sam Stevens" = "Samuel Stevens",
  "sam stevens" = "Samuel Stevens"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get quarter weights based on profile
get_quarter_weights <- function(profile = "equal") {
  weights <- switch(profile,
                    "early_season" = c(Q1 = 0.35, Q2 = 0.30, Q3 = 0.20, Q4 = 0.15),
                    "mid_season" = c(Q1 = 0.25, Q2 = 0.25, Q3 = 0.25, Q4 = 0.25),
                    "late_season" = c(Q1 = 0.15, Q2 = 0.20, Q3 = 0.30, Q4 = 0.35),
                    "equal" = c(Q1 = 0.25, Q2 = 0.25, Q3 = 0.25, Q4 = 0.25),
                    c(Q1 = 0.25, Q2 = 0.25, Q3 = 0.25, Q4 = 0.25)
  )
  return(weights)
}

#' Calculate weighted projection from quarter values
calculate_weighted_projection <- function(q1, q2, q3, q4, weights) {
  q1 <- ifelse(is.na(q1), 0, q1)
  q2 <- ifelse(is.na(q2), 0, q2)
  q3 <- ifelse(is.na(q3), 0, q3)
  q4 <- ifelse(is.na(q4), 0, q4)
  
  weighted <- (q1 * weights["Q1"] + q2 * weights["Q2"] + 
                 q3 * weights["Q3"] + q4 * weights["Q4"]) * 4
  return(weighted)
}

#' Normalize player name for matching
normalize_golf_name <- function(name) {
  if (is.na(name) || name == "") return("")
  
  if (name %in% names(GOLF_SEASON_LONG_NAME_CORRECTIONS)) {
    name <- GOLF_SEASON_LONG_NAME_CORRECTIONS[[name]]
  }
  
  name <- tolower(trimws(name))
  
  if (name %in% names(GOLF_SEASON_LONG_NAME_CORRECTIONS)) {
    name <- GOLF_SEASON_LONG_NAME_CORRECTIONS[[name]]
    name <- tolower(name)
  }
  
  if (grepl(",", name)) {
    parts <- strsplit(name, ",")[[1]]
    if (length(parts) == 2) {
      name <- paste(trimws(parts[2]), trimws(parts[1]))
    }
  }
  
  name <- gsub("[^a-z0-9 ]", " ", name)
  name <- gsub("\\s+", " ", name)
  name <- trimws(name)
  
  return(name)
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Load salary data from Google Sheets
load_salary_from_sheets <- function(sheet_id = GOLF_SEASON_LONG_SHEET_ID, 
                                    sheet_name = GOLF_SALARY_SHEET_NAME) {
  log_debug("load_salary_from_sheets() called", level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    
    data <- googlesheets4::read_sheet(sheet_id, sheet = sheet_name) %>%
      as.data.frame()
    
    log_debug("Loaded salary data:", nrow(data), "rows", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug("Error loading salary sheet:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Load rankings data from Google Sheets
load_rankings_from_sheets <- function(sheet_id = GOLF_SEASON_LONG_SHEET_ID,
                                      sheet_name = GOLF_RANKINGS_SHEET_NAME) {
  log_debug("load_rankings_from_sheets() called", level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    
    # First try reading with headers
    data <- googlesheets4::read_sheet(sheet_id, sheet = sheet_name) %>%
      as.data.frame()
    
    # Check if first column looks like a name (header detection failed)
    first_col <- names(data)[1]
    if (grepl("^[A-Z][a-z]+ [A-Z][a-z]+", first_col)) {
      # First column name looks like a player name - headers are missing
      log_debug("Detected missing headers, adding column names", level = "INFO")
      
      # Re-read without headers
      data <- googlesheets4::read_sheet(sheet_id, sheet = sheet_name, 
                                        col_names = FALSE) %>%
        as.data.frame()
      
      # Assign standard column names based on column count
      n_cols <- ncol(data)
      if (n_cols >= 10) {
        names(data) <- c("Golfer", "Rank", "ADP", "Events", 
                         "Round 1 Projection", "Round 2 Projection", 
                         "Round 3 Projection", "Round 4 Projection",
                         "Total Projected Points", "ID")[1:n_cols]
      } else if (n_cols >= 6) {
        names(data) <- c("Golfer", "Rank", 
                         "Round 1 Projection", "Round 2 Projection", 
                         "Round 3 Projection", "Round 4 Projection")[1:n_cols]
      } else {
        names(data) <- c("Golfer", "Rank", "Total Projected Points")[1:n_cols]
      }
      
      log_debug("Assigned columns:", paste(names(data), collapse = ", "), level = "DEBUG")
    }
    
    log_debug("Loaded rankings data:", nrow(data), "rows", level = "INFO")
    return(data)
    
  }, error = function(e) {
    log_debug("Error loading rankings sheet:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' Merge salary and rankings data
merge_salary_rankings <- function(salary_df, rankings_df) {
  log_debug("merge_salary_rankings() called", level = "INFO")
  log_debug("Salary columns:", paste(names(salary_df), collapse = ", "), level = "DEBUG")
  log_debug("Rankings columns:", paste(names(rankings_df), collapse = ", "), level = "DEBUG")
  
  # Detect and build full_name column
  sal_cols <- names(salary_df)
  
  # Check for first name + last name pattern
  fname_col <- intersect(c("FName", "f_name", "fname", "first_name"), sal_cols)[1]
  lname_col <- intersect(c("Name", "name", "lname", "last_name"), sal_cols)[1]
  
  if (!is.na(fname_col) && !is.na(lname_col)) {
    log_debug("Using first/last name columns:", fname_col, "+", lname_col, level = "DEBUG")
    salary_df <- salary_df %>%
      mutate(full_name = trimws(paste(.data[[fname_col]], .data[[lname_col]])))
  } else {
    # Try single name column
    name_col <- intersect(c("Player", "player", "Golfer", "golfer", "full_name"), sal_cols)[1]
    if (!is.na(name_col)) {
      log_debug("Using single name column:", name_col, level = "DEBUG")
      salary_df <- salary_df %>% mutate(full_name = .data[[name_col]])
    } else {
      log_debug("Could not find name column in salary data", level = "ERROR")
      return(NULL)
    }
  }
  
  # Detect price column
  price_col <- intersect(c("Price", "price", "Salary", "salary"), sal_cols)[1]
  if (!is.na(price_col)) {
    salary_df$Price <- as.numeric(salary_df[[price_col]])
  }
  
  # Create PlayerID if not present
  id_col <- intersect(c("PlayerID", "player_id", "id"), sal_cols)[1]
  if (!is.na(id_col)) {
    salary_df$PlayerID <- salary_df[[id_col]]
  } else {
    salary_df$PlayerID <- seq_len(nrow(salary_df))
  }
  
  # Normalize names
  salary_df <- salary_df %>%
    mutate(name_normalized = sapply(full_name, normalize_golf_name))
  
  # Clean rankings - detect golfer column
  log_debug("Rankings columns:", paste(names(rankings_df), collapse = ", "), level = "DEBUG")
  
  golfer_candidates <- c("Golfer", "golfer", "Player", "player", "Name", "name")
  golfer_col_matches <- intersect(golfer_candidates, names(rankings_df))
  
  if (length(golfer_col_matches) == 0) {
    # Try to find any column with "name" or "golfer" in it
    name_cols <- names(rankings_df)[grepl("name|golfer|player", names(rankings_df), ignore.case = TRUE)]
    if (length(name_cols) > 0) {
      golfer_col <- name_cols[1]
      log_debug("Using detected name column:", golfer_col, level = "INFO")
    } else {
      log_debug("Could not find golfer name column in rankings data", level = "ERROR")
      return(NULL)
    }
  } else {
    golfer_col <- golfer_col_matches[1]
    log_debug("Using golfer column:", golfer_col, level = "DEBUG")
  }
  
  # Create golfer_name column explicitly before normalizing
  rankings_df$golfer_name <- rankings_df[[golfer_col]]
  rankings_df$name_normalized <- sapply(rankings_df$golfer_name, normalize_golf_name)
  
  # Detect projection columns (handle various naming conventions)
  rank_cols <- names(rankings_df)
  
  # Find Rank column
  rank_col <- intersect(c("Rank", "rank", "ETR Rank", "etr_rank"), rank_cols)[1]
  if (!is.na(rank_col) && rank_col != "Rank") {
    rankings_df$Rank <- rankings_df[[rank_col]]
  }
  
  # Find round projection columns
  r1_col <- rank_cols[grepl("round.*1|r1|rd.*1", rank_cols, ignore.case = TRUE)][1]
  r2_col <- rank_cols[grepl("round.*2|r2|rd.*2", rank_cols, ignore.case = TRUE)][1]
  r3_col <- rank_cols[grepl("round.*3|r3|rd.*3", rank_cols, ignore.case = TRUE)][1]
  r4_col <- rank_cols[grepl("round.*4|r4|rd.*4", rank_cols, ignore.case = TRUE)][1]
  total_col <- rank_cols[grepl("total|projected.*points|proj.*pts", rank_cols, ignore.case = TRUE)][1]
  
  # Rename to standard names
  if (!is.na(r1_col)) rankings_df$`Round 1 Projection` <- as.numeric(rankings_df[[r1_col]])
  if (!is.na(r2_col)) rankings_df$`Round 2 Projection` <- as.numeric(rankings_df[[r2_col]])
  if (!is.na(r3_col)) rankings_df$`Round 3 Projection` <- as.numeric(rankings_df[[r3_col]])
  if (!is.na(r4_col)) rankings_df$`Round 4 Projection` <- as.numeric(rankings_df[[r4_col]])
  if (!is.na(total_col)) rankings_df$`Total Projected Points` <- as.numeric(rankings_df[[total_col]])
  
  # Coerce Rank
  if ("Rank" %in% names(rankings_df)) {
    rankings_df$Rank <- as.numeric(rankings_df$Rank)
  }
  
  log_debug("Detected columns - Rank:", !is.na(rank_col), 
            "R1:", !is.na(r1_col), "R2:", !is.na(r2_col),
            "R3:", !is.na(r3_col), "R4:", !is.na(r4_col),
            "Total:", !is.na(total_col), level = "DEBUG")
  
  # Merge
  merged <- salary_df %>%
    left_join(
      rankings_df %>% select(
        name_normalized,
        any_of(c("Rank", "Round 1 Projection", "Round 2 Projection",
                 "Round 3 Projection", "Round 4 Projection",
                 "Total Projected Points"))
      ),
      by = "name_normalized"
    )
  
  matched <- sum(!is.na(merged$Rank))
  log_debug(sprintf("Matched %d of %d players (%.1f%%)", 
                    matched, nrow(merged), matched/nrow(merged)*100), level = "INFO")
  
  return(merged)
}

# =============================================================================
# OPTIMIZER
# =============================================================================

#' Optimize a single roster using LP
optimize_golf_roster <- function(players, weights, locked_players = NULL, 
                                 budget = 100, roster_size = 10) {
  
  # Check required columns exist
  required_cols <- c("Round 1 Projection", "Round 2 Projection",
                     "Round 3 Projection", "Round 4 Projection", "Price", "PlayerID")
  missing_cols <- setdiff(required_cols, names(players))
  
  if (length(missing_cols) > 0) {
    log_debug("Missing columns for optimization:", paste(missing_cols, collapse = ", "), level = "ERROR")
    return(NULL)
  }
  
  # Calculate weighted projections using the provided weights
  players <- players %>%
    mutate(
      weighted_proj = calculate_weighted_projection(
        `Round 1 Projection`, `Round 2 Projection`,
        `Round 3 Projection`, `Round 4 Projection`,
        weights
      )
    ) %>%
    filter(!is.na(Price), !is.na(weighted_proj), weighted_proj > 0)
  
  n <- nrow(players)
  if (n < roster_size) {
    log_debug(sprintf("Not enough valid players: %d available, %d required", n, roster_size), level = "WARN")
    return(NULL)
  }
  
  # Objective: maximize weighted projection
  obj <- players$weighted_proj
  
  # Constraints matrix
  con <- rbind(
    players$Price,           # Budget constraint
    rep(1, n)                # Roster size constraint
  )
  
  dir <- c("<=", "==")
  rhs <- c(budget, roster_size)
  
  # Add lock constraints
  if (!is.null(locked_players) && length(locked_players) > 0) {
    for (pid in locked_players) {
      lock_row <- as.numeric(players$PlayerID == pid)
      if (sum(lock_row) > 0) {
        con <- rbind(con, lock_row)
        dir <- c(dir, "==")
        rhs <- c(rhs, 1)
      } else {
        log_debug(sprintf("  Warning: Locked player ID %s not found in available players", pid), level = "WARN")
      }
    }
  }
  
  # Solve
  result <- tryCatch({
    lpSolve::lp("max", obj, con, dir, rhs, all.bin = TRUE)
  }, error = function(e) {
    log_debug("LP solve error:", e$message, level = "ERROR")
    return(NULL)
  })
  
  if (is.null(result) || result$status != 0) {
    log_debug(sprintf("LP solver failed with status: %s", if(is.null(result)) "NULL" else result$status), level = "WARN")
    return(NULL)
  }
  
  selected <- players[result$solution == 1, ]
  return(selected)
}

# =============================================================================
# UI
# =============================================================================

golf_season_long_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("golf_season_long_ui() called", level = "INFO")
  
  weight_choices <- c(
    "Equal Weights" = "equal",
    "Early Season (Q1/Q2 Heavy)" = "early_season",
    "Mid Season (Balanced)" = "mid_season",
    "Late Season (Q3/Q4 Heavy)" = "late_season"
  )
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Season Long"),
      tags$p(class = "text-muted", 
             "Build optimized rosters for FanTeam Season Long. Budget: 100M | Roster: 10 players")
    ),
    
    # =========================================================================
    # ROSTER SELECTION
    # =========================================================================
    ui_card(
      title = "Roster Selection",
      color = GOLF_CARD_COLOR,
      
      # Generate all 5 roster weight inputs
      lapply(1:5, function(i) {
        fluidRow(
          column(3,
                 div(style = "display: flex; align-items: center; height: 38px; font-weight: 600;",
                     paste("Roster", i))
          ),
          column(9,
                 selectizeInput(ns(paste0("weights_", i)), NULL, choices = weight_choices, selected = "equal")
          )
        )
      })
    ),
    
    tags$br(),
    
    # =========================================================================
    # PLAYER POOL
    # =========================================================================
    ui_card(
      title = "Player Pool",
      color = GOLF_CARD_COLOR,
      
      # Unmatched players alert
      uiOutput(ns("unmatched_alert")),
      
      # Player pool table - scrollable container, no pagination
      div(
        class = "player-pool-container",
        style = "max-height: 500px; overflow-y: auto;",
        reactableOutput(ns("player_pool_table"))
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # PLAYER LOCKS & GENERATION SETTINGS
    # =========================================================================
    ui_card(
      title = "Roster Rules & Generation",
      color = GOLF_CARD_COLOR,
      
      # Minimum selection input
      div(
        style = "display: flex; align-items: center; flex-wrap: wrap; gap: 0.5rem; font-size: 1rem; margin-bottom: 1rem;",
        span(style = "font-weight: 500;", "Select"),
        div(
          style = "min-width: 200px; flex: 1; max-width: 300px;",
          selectizeInput(ns("lock_player"), NULL,
                         choices = NULL,
                         options = list(placeholder = "select player..."))
        ),
        span(style = "font-weight: 500;", "in at least"),
        div(
          style = "width: 60px;",
          selectizeInput(ns("lock_count"), NULL,
                         choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5"),
                         selected = "1")
        ),
        span(style = "font-weight: 500;", "of five lineups."),
        actionButton(ns("add_lock"), "Add", 
                     class = "btn-primary",
                     style = "margin-left: 0.5rem;")
      ),
      
      # Maximum selection input
      div(
        style = "display: flex; align-items: center; flex-wrap: wrap; gap: 0.5rem; font-size: 1rem;",
        span(style = "font-weight: 500;", "Select"),
        div(
          style = "min-width: 200px; flex: 1; max-width: 300px;",
          selectizeInput(ns("max_player"), NULL,
                         choices = NULL,
                         options = list(placeholder = "select player..."))
        ),
        span(style = "font-weight: 500;", "in at most"),
        div(
          style = "width: 60px;",
          selectizeInput(ns("max_count"), NULL,
                         choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4"),
                         selected = "2")
        ),
        span(style = "font-weight: 500;", "of five lineups."),
        actionButton(ns("add_max"), "Add", 
                     class = "btn-secondary",
                     style = "margin-left: 0.5rem;")
      ),
      
      # Applied locks/maxes display
      tags$div(style = "margin-top: 1rem;",
               uiOutput(ns("locks_display"))
      ),
      
      # Divider
      tags$hr(style = "margin: 1.5rem 0; border-color: var(--border);"),
      
      # Generation settings
      fluidRow(
        column(4,
               numericInput(ns("min_unique_players"), "Min Unique Players",
                            value = 20, min = 10, max = 40, step = 1),
               tags$p(class = "text-muted", style = "font-size: 0.8rem; margin-top: -10px;",
                      "Unique players across all rosters")
        ),
        column(4,
               numericInput(ns("global_max_exposure"), "Max Appearances (All Players)",
                            value = 5, min = 1, max = 5, step = 1),
               tags$p(class = "text-muted", style = "font-size: 0.8rem; margin-top: -10px;",
                      "Hard cap for any player")
        ),
        column(4,
               tags$p(class = "text-muted", style = "margin-top: 25px; font-size: 0.85rem;",
                      "The stricter of the two rules will apply.")
        )
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # GENERATE & CLEAR BUTTONS
    # =========================================================================
    div(
      style = "text-align: center; margin: 2rem 0; display: flex; justify-content: center; gap: 1rem;",
      actionButton(ns("generate_btn"), "GENERATE ROSTERS", 
                   class = "btn-primary btn-lg",
                   style = "padding: 1rem 3rem; font-size: 1.1rem; font-weight: 700;"),
      actionButton(ns("clear_btn"), "CLEAR ROSTERS", 
                   class = "btn-secondary btn-lg",
                   style = "padding: 1rem 2rem; font-size: 1.1rem; font-weight: 700;")
    ),
    
    tags$br(),
    
    # =========================================================================
    # ROSTERS DISPLAY
    # =========================================================================
    uiOutput(ns("rosters_display"))
  )
}

# =============================================================================
# SERVER
# =============================================================================

golf_season_long_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("golf_season_long_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      player_data = NULL,
      unmatched_players = NULL,  # FanTeam players without projections
      locked_players = list(),   # list of {id, name, min_count}
      max_players = list(),      # list of {id, name, max_count}
      rosters = list(NULL, NULL, NULL, NULL, NULL),
      data_loaded = FALSE
    )
    
    # =========================================================================
    # AUTO-LOAD DATA ON INIT
    # =========================================================================
    observe({
      if (rv$data_loaded) return()
      
      log_debug("Auto-loading data...", level = "INFO")
      
      salary_data <- load_salary_from_sheets()
      rankings_data <- load_rankings_from_sheets()
      
      req(salary_data, rankings_data)
      
      merged <- merge_salary_rankings(salary_data, rankings_data)
      
      if (is.null(merged)) {
        log_debug("Merge failed - check column detection", level = "ERROR")
        return()
      }
      
      # Add FanTeam rank (by salary descending)
      merged <- merged %>%
        arrange(desc(Price)) %>%
        mutate(FanTeamRank = row_number())
      
      rv$player_data <- merged
      rv$data_loaded <- TRUE
      
      # Track unmatched players: FanTeam players without ETR projections
      rv$unmatched_players <- merged %>%
        filter(is.na(Rank)) %>%
        pull(full_name)
      
      matched_count <- sum(!is.na(merged$Rank))
      log_debug(sprintf("Matched %d of %d FanTeam players, %d without projections",
                        matched_count, nrow(merged), length(rv$unmatched_players)), level = "INFO")
      
      # Update lock/max player choices (only matched players with projections)
      if (nrow(merged) > 0) {
        matched_players <- merged %>% filter(!is.na(Rank))
        player_choices <- setNames(matched_players$PlayerID, matched_players$full_name)
        updateSelectizeInput(session, "lock_player", choices = player_choices)
        updateSelectizeInput(session, "max_player", choices = player_choices)
      }
      
      log_debug("Data loaded:", nrow(merged), "players", level = "INFO")
    })
    
    # =========================================================================
    # UNMATCHED ALERT
    # =========================================================================
    output$unmatched_alert <- renderUI({
      unmatched <- rv$unmatched_players
      if (is.null(unmatched) || length(unmatched) == 0) return(NULL)
      
      div(
        class = "alert alert-warning mb-3",
        style = "margin-bottom: 1rem;",
        tags$strong(icon("exclamation-triangle"), sprintf(" %d FanTeam Players Without Projections: ", length(unmatched))),
        tags$span(
          style = "font-size: 0.85rem;",
          if (length(unmatched) <= 10) {
            paste(unmatched, collapse = ", ")
          } else {
            paste0(paste(head(unmatched, 10), collapse = ", "), " +(", length(unmatched) - 10, " more)")
          }
        )
      )
    })
    
    # =========================================================================
    # PLAYER POOL TABLE
    # =========================================================================
    output$player_pool_table <- renderReactable({
      req(rv$player_data)
      
      data <- rv$player_data
      
      # Ensure FanTeamRank exists (create if missing)
      if (!"FanTeamRank" %in% names(data)) {
        data <- data %>%
          arrange(desc(Price)) %>%
          mutate(FanTeamRank = row_number())
      }
      
      # Build clean display dataframe - include headshot_url for display
      # Filter to players with projections and create display columns
      pool_df <- data %>%
        filter(!is.na(Rank)) %>%
        transmute(
          PlayerID = PlayerID,
          Golfer = full_name,
          headshot_url = if ("headshot_url" %in% names(data)) headshot_url else NA_character_,
          FT_Salary = Price,
          FT_Rank = FanTeamRank,
          ETR_Rank = Rank,
          RankDiffPct = ifelse(Rank > 0, round((FanTeamRank - Rank) / Rank * 100, 1), NA_real_)
        ) %>%
        arrange(FT_Rank)  # Sort by FanTeam salary rank
      
      # Pre-compute column values for heatmap scaling
      rank_diff_pct_vals <- pool_df$RankDiffPct[!is.na(pool_df$RankDiffPct)]
      
      reactable(
        pool_df,
        theme = app_reactable_theme(compact = TRUE),
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        columns = list(
          PlayerID = colDef(show = FALSE),
          headshot_url = colDef(show = FALSE),
          Golfer = colDef(
            name = "Golfer",
            minWidth = 200,
            sticky = "left",
            style = list(verticalAlign = "middle"),
            cell = function(value, index) {
              # Get headshot URL for this player
              headshot <- pool_df$headshot_url[index]
              headshot_src <- if (!is.na(headshot) && nchar(headshot) > 0) headshot else GOLF_DEFAULT_HEADSHOT
              
              div(
                style = "display: flex; align-items: center; gap: 0.5rem;",
                div(
                  class = "player-headshot player-headshot--sm",
                  style = "background: #e8e8e8; flex-shrink: 0; width: 32px; height: 32px; border-radius: 50%; overflow: hidden;",
                  tags$img(
                    src = headshot_src,
                    style = "width: 100%; height: 100%; object-fit: cover;",
                    onerror = sprintf("this.src='%s'", GOLF_DEFAULT_HEADSHOT)
                  )
                ),
                span(style = "font-weight: 600;", value)
              )
            }
          ),
          FT_Salary = colDef(
            name = "FT Salary", 
            format = colFormat(prefix = "$", suffix = "M", digits = 1), 
            width = 95, 
            align = "right",
            style = list(fontWeight = 600, verticalAlign = "middle")
          ),
          FT_Rank = colDef(
            name = "FT Rk",
            width = 70,
            align = "center",
            style = list(verticalAlign = "middle")
          ),
          ETR_Rank = colDef(
            name = "ETR Rk", 
            width = 70, 
            align = "center",
            cell = function(value) if (is.na(value)) "-" else value,
            style = list(verticalAlign = "middle")
          ),
          RankDiffPct = colDef(
            name = "Rank ÃŽâ€%", 
            width = 90, 
            align = "center",
            cell = function(value) {
              if (is.na(value)) return("-")
              sprintf("%+.1f%%", value)
            },
            style = function(value) {
              if (is.na(value)) return(list(verticalAlign = "middle"))
              
              # Diverging heatmap: coral (negative/overpriced) -> white (0) -> teal (positive/underpriced)
              min_val <- min(rank_diff_pct_vals, na.rm = TRUE)
              max_val <- max(rank_diff_pct_vals, na.rm = TRUE)
              midpoint <- 0
              
              if (value < midpoint) {
                # Coral to white (overpriced relative to projection)
                if (min_val >= midpoint) min_val <- -50
                t <- (value - min_val) / (midpoint - min_val)
                t <- max(0, min(1, t))
                r <- round(208 + (255 - 208) * t)
                g <- round(135 + (255 - 135) * t)
                b <- round(112 + (255 - 112) * t)
              } else {
                # White to teal (underpriced relative to projection)
                if (max_val <= midpoint) max_val <- 50
                t <- (value - midpoint) / (max_val - midpoint)
                t <- max(0, min(1, t))
                r <- round(255 + (143 - 255) * t)
                g <- round(255 + (188 - 255) * t)
                b <- round(255 + (187 - 255) * t)
              }
              list(background = sprintf("rgb(%d, %d, %d)", r, g, b), fontWeight = 600, verticalAlign = "middle")
            }
          )
        )
      )
    })
    
    # =========================================================================
    # LOCKS DISPLAY (Min and Max)
    # =========================================================================
    output$locks_display <- renderUI({
      if (length(rv$locked_players) == 0 && length(rv$max_players) == 0) return(NULL)
      
      num_rosters <- GOLF_SEASON_LONG_CONFIG$num_rosters
      
      # Min selection tags (green/sage)
      lock_tags <- lapply(seq_along(rv$locked_players), function(i) {
        lock <- rv$locked_players[[i]]
        player_name <- lock$name
        min_count <- lock$min_count
        
        div(
          style = "display: inline-flex; align-items: center; padding: 0.4rem 0.8rem; background: white; border: 2px solid var(--accent-sage); border-radius: 6px; margin: 0.25rem;",
          div(
            style = "background: var(--accent-sage); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.5rem;",
            "MIN"
          ),
          span(style = "font-weight: 600; margin-right: 0.5rem;", player_name),
          span(class = "text-muted", style = "font-size: 0.85rem;", 
               sprintf("(at least %d of %d)", min_count, num_rosters)),
          actionButton(ns(paste0("remove_lock_", i)), icon("times"), 
                       class = "btn-secondary",
                       style = "padding: 0.1rem 0.4rem; min-width: auto; font-size: 0.7rem; margin-left: 0.5rem;",
                       onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", 
                                         ns("remove_lock"), i))
        )
      })
      
      # Max selection tags (coral)
      max_tags <- lapply(seq_along(rv$max_players), function(i) {
        max_rule <- rv$max_players[[i]]
        player_name <- max_rule$name
        max_count <- max_rule$max_count
        
        div(
          style = "display: inline-flex; align-items: center; padding: 0.4rem 0.8rem; background: white; border: 2px solid var(--accent-coral); border-radius: 6px; margin: 0.25rem;",
          div(
            style = "background: var(--accent-coral); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.5rem;",
            "MAX"
          ),
          span(style = "font-weight: 600; margin-right: 0.5rem;", player_name),
          span(class = "text-muted", style = "font-size: 0.85rem;", 
               sprintf("(at most %d of %d)", max_count, num_rosters)),
          actionButton(ns(paste0("remove_max_", i)), icon("times"), 
                       class = "btn-secondary",
                       style = "padding: 0.1rem 0.4rem; min-width: auto; font-size: 0.7rem; margin-left: 0.5rem;",
                       onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", 
                                         ns("remove_max"), i))
        )
      })
      
      div(c(lock_tags, max_tags))
    })
    
    # =========================================================================
    # ADD LOCK
    # =========================================================================
    observeEvent(input$add_lock, {
      req(rv$player_data)
      req(input$lock_player)
      
      pid <- input$lock_player
      min_count <- as.numeric(input$lock_count)
      
      # Get player name
      player_name <- rv$player_data %>%
        filter(PlayerID == pid) %>%
        pull(full_name)
      
      if (length(player_name) == 0) return()
      
      # Check if already locked
      existing_names <- sapply(rv$locked_players, function(x) x$name)
      if (player_name[1] %in% existing_names) {
        log_debug("Player already locked:", player_name[1], level = "WARN")
        return()
      }
      
      # Add to locked players
      rv$locked_players <- c(rv$locked_players, list(
        list(id = pid, name = player_name[1], min_count = min_count)
      ))
      
      # Clear selection
      updateSelectizeInput(session, "lock_player", selected = "")
      
      log_debug("Locked:", player_name[1], "min", min_count, "rosters", level = "INFO")
    })
    
    # =========================================================================
    # REMOVE LOCK
    # =========================================================================
    observeEvent(input$remove_lock, {
      idx <- input$remove_lock
      if (idx > 0 && idx <= length(rv$locked_players)) {
        removed <- rv$locked_players[[idx]]$name
        rv$locked_players <- rv$locked_players[-idx]
        log_debug("Removed lock:", removed, level = "INFO")
      }
    })
    
    # =========================================================================
    # ADD MAX
    # =========================================================================
    observeEvent(input$add_max, {
      req(rv$player_data)
      req(input$max_player)
      
      pid <- input$max_player
      max_count <- as.numeric(input$max_count)
      
      # Get player name
      player_name <- rv$player_data %>%
        filter(PlayerID == pid) %>%
        pull(full_name)
      
      if (length(player_name) == 0) return()
      
      # Check if already has max rule
      existing_names <- sapply(rv$max_players, function(x) x$name)
      if (player_name[1] %in% existing_names) {
        log_debug("Player already has max rule:", player_name[1], level = "WARN")
        return()
      }
      
      # Add to max players
      rv$max_players <- c(rv$max_players, list(
        list(id = pid, name = player_name[1], max_count = max_count)
      ))
      
      # Clear selection
      updateSelectizeInput(session, "max_player", selected = "")
      
      log_debug("Max rule:", player_name[1], "max", max_count, "rosters", level = "INFO")
    })
    
    # =========================================================================
    # REMOVE MAX
    # =========================================================================
    observeEvent(input$remove_max, {
      idx <- input$remove_max
      if (idx > 0 && idx <= length(rv$max_players)) {
        removed <- rv$max_players[[idx]]$name
        rv$max_players <- rv$max_players[-idx]
        log_debug("Removed max rule:", removed, level = "INFO")
      }
    })
    
    # =========================================================================
    # CLEAR ROSTERS
    # =========================================================================
    observeEvent(input$clear_btn, {
      log_debug("Clearing all rosters...", level = "INFO")
      num_rosters <- GOLF_SEASON_LONG_CONFIG$num_rosters
      rv$rosters <- replicate(num_rosters, NULL, simplify = FALSE)
    })
    
    # =========================================================================
    # GENERATE ROSTERS
    # =========================================================================
    observeEvent(input$generate_btn, {
      req(rv$player_data)
      
      num_rosters <- GOLF_SEASON_LONG_CONFIG$num_rosters
      log_debug(sprintf("========== GENERATING %d ROSTERS ==========", num_rosters), level = "INFO")
      
      # Get weights for each roster dynamically and log them
      weights_list <- lapply(1:num_rosters, function(i) {
        weight_input <- input[[paste0("weights_", i)]]
        weights <- get_quarter_weights(weight_input %||% "equal")
        log_debug(sprintf("Roster %d weights (%s): Q1=%.2f, Q2=%.2f, Q3=%.2f, Q4=%.2f", 
                          i, weight_input %||% "equal", 
                          weights["Q1"], weights["Q2"], weights["Q3"], weights["Q4"]), level = "INFO")
        weights
      })
      
      # Build lock lists per roster based on min_count
      locks_per_roster <- replicate(num_rosters, NULL, simplify = FALSE)
      
      if (length(rv$locked_players) > 0) {
        log_debug(sprintf("Processing %d lock rules:", length(rv$locked_players)), level = "INFO")
        for (lock in rv$locked_players) {
          pid <- lock$id
          min_count <- as.numeric(lock$min_count)
          log_debug(sprintf("  - %s: min %d rosters", lock$name, min_count), level = "INFO")
          
          # Assign to rosters based on minimum count
          for (i in 1:min(min_count, num_rosters)) {
            locks_per_roster[[i]] <- c(locks_per_roster[[i]], pid)
          }
        }
      }
      
      # Build max constraints per player
      max_constraints <- list()
      if (length(rv$max_players) > 0) {
        log_debug(sprintf("Processing %d max rules:", length(rv$max_players)), level = "INFO")
        for (max_rule in rv$max_players) {
          pid <- max_rule$id
          max_count <- as.numeric(max_rule$max_count)
          log_debug(sprintf("  - %s: max %d rosters", max_rule$name, max_count), level = "INFO")
          max_constraints[[as.character(pid)]] <- max_count
        }
      }
      
      # Get min unique players constraint and calculate auto max exposure
      min_unique <- input$min_unique_players %||% 20
      global_max <- input$global_max_exposure %||% 5
      total_slots <- num_rosters * GOLF_SEASON_LONG_CONFIG$roster_size
      
      # Calculate max appearances per player to achieve min unique target
      # e.g., 50 slots / 25 unique = max 2 appearances each
      auto_max_from_unique <- floor(total_slots / min_unique)
      auto_max_from_unique <- max(1, auto_max_from_unique)
      
      # Use the stricter (lower) of the two limits
      effective_max_exposure <- min(auto_max_from_unique, global_max)
      
      log_debug(sprintf("Min unique players target: %d", min_unique), level = "INFO")
      log_debug(sprintf("Global max exposure setting: %d", global_max), level = "INFO")
      log_debug(sprintf("Auto max from unique target: %d", auto_max_from_unique), level = "INFO")
      log_debug(sprintf("Effective max exposure: %d (using stricter rule)", effective_max_exposure), level = "INFO")
      
      # Track player usage across rosters for uniqueness constraint
      player_usage <- list()
      all_rosters <- list()
      
      # Generate each roster sequentially
      for (i in 1:num_rosters) {
        log_debug(sprintf("--- Generating Roster %d ---", i), level = "INFO")
        
        # Log locks for this roster
        if (!is.null(locks_per_roster[[i]]) && length(locks_per_roster[[i]]) > 0) {
          lock_names <- rv$player_data %>% 
            filter(PlayerID %in% locks_per_roster[[i]]) %>% 
            pull(full_name)
          log_debug(sprintf("  Locked players: %s", paste(lock_names, collapse = ", ")), level = "INFO")
        }
        
        # Calculate which players are blocked due to max constraint OR auto exposure limit
        blocked_players <- c()
        
        if (length(all_rosters) > 0) {
          # First check manual max constraints
          if (length(max_constraints) > 0) {
            for (pid_str in names(max_constraints)) {
              pid <- as.numeric(pid_str)
              max_count <- max_constraints[[pid_str]]
              
              # Count how many times this player appears in previous rosters
              current_count <- sum(vapply(all_rosters, function(r) {
                if (is.null(r)) return(0L)
                as.integer(sum(r$PlayerID == pid))
              }, integer(1)))
              
              if (current_count >= max_count) {
                blocked_players <- c(blocked_players, pid)
              }
            }
          }
          
          # Then check auto exposure limit for ALL players (for diversity)
          for (pid_str in names(player_usage)) {
            pid <- as.numeric(pid_str)
            if (player_usage[[pid_str]] >= effective_max_exposure) {
              if (!pid %in% blocked_players) {
                blocked_players <- c(blocked_players, pid)
              }
            }
          }
        }
        
        # Log blocked players
        if (length(blocked_players) > 0) {
          blocked_names <- rv$player_data %>% 
            filter(PlayerID %in% blocked_players) %>% 
            pull(full_name)
          log_debug(sprintf("  Blocked (exposure limit): %s", paste(blocked_names, collapse = ", ")), level = "INFO")
        }
        
        # Filter out blocked players
        available_players <- rv$player_data
        if (length(blocked_players) > 0) {
          available_players <- available_players %>%
            filter(!PlayerID %in% blocked_players)
        }
        
        roster <- optimize_golf_roster(
          players = available_players,
          weights = weights_list[[i]],
          locked_players = locks_per_roster[[i]],
          budget = GOLF_SEASON_LONG_CONFIG$budget,
          roster_size = GOLF_SEASON_LONG_CONFIG$roster_size
        )
        
        all_rosters[[i]] <- roster
        rv$rosters[[i]] <- roster
        
        if (!is.null(roster)) {
          # Track player usage
          for (pid in roster$PlayerID) {
            player_usage[[as.character(pid)]] <- (player_usage[[as.character(pid)]] %||% 0) + 1
          }
          
          log_debug(sprintf("  Result: %d players, $%.1fM salary, %.1f projected", 
                            nrow(roster), sum(roster$Price), sum(roster$weighted_proj)), 
                    level = "INFO")
        } else {
          log_debug("  Result: NO VALID ROSTER FOUND", level = "WARN")
        }
      }
      
      # Log uniqueness stats
      unique_count <- length(player_usage)
      log_debug("========== SUMMARY ==========", level = "INFO")
      log_debug(sprintf("Effective max exposure: %d rosters per player", effective_max_exposure), level = "INFO")
      log_debug(sprintf("Total unique players: %d (target: %d)", unique_count, min_unique), level = "INFO")
      
      if (unique_count < min_unique) {
        log_debug("WARNING: Did not meet minimum unique players target", level = "WARN")
      }
    })
    
    # =========================================================================
    # ROSTERS DISPLAY
    # =========================================================================
    output$rosters_display <- renderUI({
      # Check if any rosters exist
      has_rosters <- any(sapply(rv$rosters, function(x) !is.null(x)))
      if (!has_rosters) return(NULL)
      
      num_rosters <- GOLF_SEASON_LONG_CONFIG$num_rosters
      
      # Create roster card function
      create_roster_card <- function(i) {
        roster <- rv$rosters[[i]]
        
        ui_card(
          title = paste("Roster", i),
          color = GOLF_CARD_COLOR,
          
          if (is.null(roster)) {
            div(class = "text-muted", style = "text-align: center; padding: 2rem;",
                "No valid roster found")
          } else {
            # Sort roster by salary descending
            roster_sorted <- roster %>% arrange(desc(Price))
            
            tagList(
              # Summary stats
              div(
                style = "display: flex; justify-content: space-between; margin-bottom: 1rem; padding: 0.5rem; background: var(--bg-secondary); border-radius: 6px;",
                div(
                  tags$small(class = "text-muted", "Salary"),
                  div(style = "font-weight: 700;", sprintf("%.1fM", sum(roster_sorted$Price)))
                ),
                div(
                  tags$small(class = "text-muted", "Projected"),
                  div(style = "font-weight: 700;", sprintf("%.1f", sum(roster_sorted$weighted_proj)))
                )
              ),
              
              # Player list (sorted by salary, showing salary and ETR rank)
              div(
                style = "font-size: 0.85rem;",
                lapply(1:nrow(roster_sorted), function(j) {
                  player <- roster_sorted[j, ]
                  etr_rank <- if (!is.na(player$Rank)) player$Rank else "-"
                  # Use player's headshot if available
                  headshot_url <- if ("headshot_url" %in% names(player) && !is.na(player$headshot_url)) {
                    player$headshot_url
                  } else {
                    GOLF_DEFAULT_HEADSHOT
                  }
                  
                  div(
                    style = "display: flex; align-items: center; padding: 0.35rem 0; border-bottom: 1px solid var(--border); gap: 0.4rem;",
                    # Headshot
                    div(
                      style = "width: 26px; height: 26px; border-radius: 50%; background: #e8e8e8; flex-shrink: 0; overflow: hidden;",
                      tags$img(
                        src = headshot_url,
                        style = "width: 100%; height: 100%; object-fit: cover;",
                        onerror = sprintf("this.src='%s'", GOLF_DEFAULT_HEADSHOT)
                      )
                    ),
                    # Name
                    span(style = "font-weight: 600; flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", 
                         player$full_name),
                    # Salary
                    span(style = "color: var(--text-secondary); font-weight: 500; width: 50px; text-align: right; flex-shrink: 0;", 
                         sprintf("$%.1fM", player$Price)),
                    # ETR Rank
                    span(style = "color: var(--text-muted); font-size: 0.75rem; width: 30px; text-align: right; flex-shrink: 0;", 
                         sprintf("#%s", etr_rank))
                  )
                })
              )
            )
          }
        )
      }
      
      # Layout: 3 rosters in first row, 2 in second row (for 5 total)
      tagList(
        fluidRow(
          lapply(1:min(3, num_rosters), function(i) {
            column(4, create_roster_card(i))
          })
        ),
        if (num_rosters > 3) {
          fluidRow(
            style = "margin-top: 1rem;",
            lapply(4:num_rosters, function(i) {
              column(4, create_roster_card(i))
            })
          )
        }
      )
    })
    
  })
}