# =============================================================================
# Module: NHL Handbuild
# 
# Manual lineup construction for FanTeam Classic format
# 9 players: 1 GK, 2 D, 3 W, 2 C, 1 FLEX
# =============================================================================

# Load lpSolve for optimization
library(lpSolve)

# =============================================================================
# LP OPTIMIZER FUNCTIONS
# =============================================================================

#' Optimize NHL lineup using linear programming
optimize_nhl_lineup_lp <- function(players, 
                                   projection_col = "fpts_blended",
                                   salary_cap = 100,
                                   roster_format = "classic",
                                   locked_ids = c(),
                                   excluded_ids = c()) {
  
  # Debug logging
  if (exists("log_debug")) {
    log_debug("LP optimizer called:", level = "DEBUG")
    log_debug("  - projection_col:", projection_col, level = "DEBUG")
    log_debug("  - salary_cap:", salary_cap, level = "DEBUG")
    log_debug("  - roster_format:", roster_format, level = "DEBUG")
    log_debug("  - input rows:", nrow(players), level = "DEBUG")
  }
  
  if (roster_format == "classic") {
    n_goalies <- 1
    n_defense <- 2
    n_wingers <- 3
    n_centers <- 2
    roster_size <- 9
  } else {
    n_goalies <- 1
    n_defense <- 2
    n_wingers <- 2
    n_centers <- 1
    roster_size <- 6
  }
  
  if (length(excluded_ids) > 0) {
    players <- players %>% filter(!playerid %in% excluded_ids)
  }
  
  if (!projection_col %in% names(players)) {
    if (exists("log_debug")) log_debug("Projection column not found:", projection_col, level = "ERROR")
    return(NULL)
  }
  
  if (!"salary" %in% names(players)) {
    if (exists("log_debug")) log_debug("salary column not found", level = "ERROR")
    return(NULL)
  }
  
  if (!"playerid" %in% names(players)) {
    if (exists("log_debug")) log_debug("playerid column not found", level = "ERROR")
    return(NULL)
  }
  
  players <- players %>%
    filter(!is.na(.data[[projection_col]])) %>%
    filter(.data[[projection_col]] > 0) %>%
    filter(!is.na(salary) & salary > 0)
  
  if (nrow(players) < roster_size) return(NULL)
  
  n <- nrow(players)
  
  # Check we have enough players at each position
  n_g <- sum(players$position == "G")
  n_d <- sum(players$position == "D")
  n_w <- sum(players$position %in% c("LW", "RW", "W"))
  n_c <- sum(players$position == "C")
  
  if (exists("log_debug")) {
    log_debug(sprintf("  Position counts: G=%d, D=%d, W=%d, C=%d", n_g, n_d, n_w, n_c), level = "DEBUG")
  }
  
  if (roster_format == "classic") {
    if (n_g < 1 || n_d < 2 || n_w < 3 || n_c < 2) {
      if (exists("log_debug")) log_debug("Not enough players at required positions", level = "WARN")
      return(NULL)
    }
  }
  
  players <- players %>%
    mutate(
      is_goalie = position == "G",
      is_defense = position == "D",
      is_winger = position %in% c("LW", "RW", "W"),
      is_center = position == "C",
      is_skater = position %in% c("C", "LW", "RW", "W", "D")
    )
  
  obj <- players[[projection_col]]
  
  constraints <- list()
  directions <- c()
  rhs <- c()
  
  # Salary cap
  constraints[[1]] <- players$salary
  directions <- c(directions, "<=")
  rhs <- c(rhs, salary_cap)
  
  # Goalies exactly n_goalies
  constraints[[2]] <- as.numeric(players$is_goalie)
  directions <- c(directions, "==")
  rhs <- c(rhs, n_goalies)
  
  if (roster_format == "classic") {
    # At least n_defense defensemen
    constraints[[3]] <- as.numeric(players$is_defense)
    directions <- c(directions, ">=")
    rhs <- c(rhs, n_defense)
    
    # At least n_wingers wingers
    constraints[[4]] <- as.numeric(players$is_winger)
    directions <- c(directions, ">=")
    rhs <- c(rhs, n_wingers)
    
    # At least n_centers centers
    constraints[[5]] <- as.numeric(players$is_center)
    directions <- c(directions, ">=")
    rhs <- c(rhs, n_centers)
    
    # Total = roster_size
    constraints[[6]] <- rep(1, n)
    directions <- c(directions, "==")
    rhs <- c(rhs, roster_size)
  } else {
    # Exact counts for limited format
    constraints[[3]] <- as.numeric(players$is_defense)
    directions <- c(directions, "==")
    rhs <- c(rhs, n_defense)
    
    constraints[[4]] <- as.numeric(players$is_winger)
    directions <- c(directions, "==")
    rhs <- c(rhs, n_wingers)
    
    constraints[[5]] <- as.numeric(players$is_center)
    directions <- c(directions, "==")
    rhs <- c(rhs, n_centers)
    
    constraints[[6]] <- rep(1, n)
    directions <- c(directions, "==")
    rhs <- c(rhs, roster_size)
  }
  
  # Locked players must be selected
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
  
  const_mat <- do.call(rbind, constraints)
  
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
    if (exists("log_debug")) log_debug("LP error:", e$message, level = "ERROR")
    NULL
  })
  
  if (exists("log_debug")) {
    if (is.null(result)) {
      log_debug("LP result is NULL", level = "WARN")
    } else {
      log_debug("LP status:", result$status, "(0=success)", level = "DEBUG")
    }
  }
  
  if (is.null(result) || result$status != 0) return(NULL)
  
  selected_indices <- which(result$solution == 1)
  players[selected_indices, ]
}

#' Generate multiple lineups with variance
generate_nhl_lineups_with_variance <- function(players,
                                               n_lineups = 5,
                                               projection_col = "fpts_blended",
                                               variance_pct = 15,
                                               salary_cap = 100,
                                               roster_format = "classic",
                                               min_unique = 1,
                                               locked_ids = c(),
                                               excluded_ids = c()) {
  
  if (exists("log_debug")) {
    log_debug("generate_nhl_lineups_with_variance called", level = "INFO")
    log_debug("  - n_lineups:", n_lineups, level = "INFO")
    log_debug("  - variance_pct:", variance_pct, level = "INFO")
    log_debug("  - min_unique:", min_unique, level = "INFO")
  }
  
  lineups <- list()
  all_lineup_player_ids <- list()
  max_attempts <- n_lineups * 50
  attempts <- 0
  
  while (length(lineups) < n_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    players_varied <- players %>%
      mutate(
        variance_factor = runif(n(), 1 - variance_pct/100, 1 + variance_pct/100),
        proj_varied = .data[[projection_col]] * variance_factor
      )
    
    lineup <- optimize_nhl_lineup_lp(
      players = players_varied,
      projection_col = "proj_varied",
      salary_cap = salary_cap,
      roster_format = roster_format,
      locked_ids = locked_ids,
      excluded_ids = excluded_ids
    )
    
    if (is.null(lineup) || nrow(lineup) == 0) next
    
    current_ids <- lineup$playerid
    
    if (length(all_lineup_player_ids) == 0) {
      lineups[[length(lineups) + 1]] <- lineup
      all_lineup_player_ids[[length(all_lineup_player_ids) + 1]] <- current_ids
    } else {
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
  
  if (exists("log_debug")) {
    log_debug("  Generated", length(lineups), "lineups after", attempts, "attempts", level = "INFO")
  }
  
  lineups
}

#' Assign players to lineup slots
assign_nhl_lineup_slots <- function(lineup, roster_format = "classic") {
  
  if (roster_format == "classic") {
    slot_order <- c("G", "D1", "D2", "W1", "W2", "W3", "C1", "C2", "FLEX")
  } else {
    slot_order <- c("G", "D1", "D2", "W1", "W2", "C1")
  }
  
  slots <- setNames(vector("list", length(slot_order)), slot_order)
  used_ids <- c()
  
  if (!"fpts_blended" %in% names(lineup)) {
    lineup <- lineup %>% mutate(fpts_blended = 0)
  }
  
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
  
  # Fill FLEX with remaining skater
  if (roster_format == "classic" && "FLEX" %in% slot_order) {
    remaining <- lineup %>% filter(!playerid %in% used_ids & pos_group != "G")
    if (nrow(remaining) >= 1) {
      slots$FLEX <- as.list(remaining[1, ])
    }
  }
  
  slots
}

# =============================================================================
# UI FUNCTION
# =============================================================================

nhl_handbuild_ui <- function(id) {
  
  ns <- NS(id)
  log_debug("nhl_handbuild_ui() called", level = "INFO")
  
  tagList(
    # CRITICAL: Enable shinyjs for button state management
    shinyjs::useShinyjs(),
    
    # CSS for position filter buttons - MATCH Generate Optimal button styling
    tags$style(HTML("
      /* Player pool scrolls with fixed height to match lineup card */
      .nhl-player-pool-container {
        height: 580px !important;
        max-height: 580px !important;
        overflow-y: auto !important;
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
      
      /* Player pool styles */
      .nhl-pool-header {
        display: grid;
        grid-template-columns: 1fr 65px 55px 55px 50px;
        padding: 8px 12px;
        background: var(--bg-secondary);
        font-weight: 600;
        font-size: 0.8rem;
        color: var(--text-muted);
        border-bottom: 2px solid var(--border);
        position: sticky;
        top: 0;
        z-index: 10;
      }
      
      .nhl-pool-header > div { text-align: center; }
      .nhl-pool-header > div:first-child { text-align: left; }
      
      /* Sortable header styling */
      .nhl-sort-header {
        cursor: pointer;
        user-select: none;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 4px;
      }
      
      .nhl-sort-header:first-child {
        justify-content: flex-start;
      }
      
      .nhl-sort-header:hover {
        color: var(--text-primary);
      }
      
      .nhl-sort-icon {
        font-size: 0.7rem;
        opacity: 0.6;
      }
      
      .nhl-sort-header.active .nhl-sort-icon {
        opacity: 1;
        color: var(--accent-coral);
      }
      
      .nhl-pool-row {
        display: grid;
        grid-template-columns: 1fr 65px 55px 55px 50px;
        align-items: center;
        padding: 10px 12px;
        border-bottom: 1px solid var(--border);
        cursor: pointer;
        transition: background 0.15s ease;
        font-size: 0.9rem;
      }
      
      .nhl-pool-row:hover:not(.nhl-pool-row--locked):not(.nhl-pool-row--unaffordable) {
        background: var(--bg-tan);
      }
      
      .nhl-pool-row--locked {
        opacity: 0.4;
        cursor: not-allowed;
        text-decoration: line-through;
        background: var(--bg-secondary);
      }
      
      .nhl-pool-row--unaffordable {
        opacity: 0.5;
        cursor: not-allowed;
      }
      
      .nhl-pool-row--unaffordable .nhl-salary {
        color: var(--accent-coral) !important;
      }
      
      .nhl-player-cell {
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      
      .nhl-player-name { font-weight: 600; font-size: 0.9rem; }
      .nhl-player-meta { font-size: 0.75rem; color: var(--text-muted); }
      .nhl-salary { text-align: center; font-weight: 600; }
      .nhl-median { text-align: center; font-weight: 600; color: var(--accent-sage); }
      .nhl-ceiling { text-align: center; font-weight: 600; color: var(--accent-coral); }
      .nhl-value { text-align: center; color: var(--text-muted); font-size: 0.85rem; }
      
      /* Generated lineups grid */
      .nhl-lineups-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
        gap: 16px;
        align-items: start;
      }
      
      .nhl-lineup-card {
        background: var(--bg-tertiary);
        border-radius: 8px;
        padding: 12px;
        border: 1px solid var(--border);
        min-height: auto;
      }
      
      .nhl-lineup-card-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        margin-bottom: 10px;
        padding-bottom: 8px;
        border-bottom: 1px solid var(--border);
      }
      
      .nhl-lineup-card-title { 
        font-weight: 700; 
        font-size: 1rem; 
      }
      
      .nhl-lineup-card-stats { 
        font-size: 0.75rem; 
        color: var(--text-muted); 
        text-align: right;
      }
      
      /* Delta to optimal - PROMINENT display */
      .nhl-lineup-delta {
        font-size: 1.1rem;
        font-weight: 800;
        color: #D08770;
        margin-bottom: 2px;
      }
      
      .nhl-lineup-delta.positive {
        color: #A3BE8C;
      }
      
      .nhl-lineup-players {
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      
      .nhl-lineup-row {
        display: grid;
        grid-template-columns: 35px 1fr 55px 65px;
        gap: 6px;
        align-items: center;
        padding: 4px 0;
        font-size: 0.8rem;
        border-bottom: 1px solid var(--bg-secondary);
      }
      
      .nhl-lineup-row:last-child { border-bottom: none; }
      
      .nhl-pos-badge {
        padding: 2px 6px;
        border-radius: 4px;
        font-size: 0.7rem;
        font-weight: 600;
        text-align: center;
      }
      
      .nhl-player-pool-container::-webkit-scrollbar { width: 8px; }
      .nhl-player-pool-container::-webkit-scrollbar-track { background: var(--bg-secondary); border-radius: 4px; }
      .nhl-player-pool-container::-webkit-scrollbar-thumb { background: var(--border); border-radius: 4px; }
      .nhl-player-pool-container::-webkit-scrollbar-thumb:hover { background: var(--text-muted); }
    ")),
    
    # Settings Card with z-index for dropdown visibility
    div(
      style = "position: relative; z-index: 100;",
      ui_card(
        title = "Lineup Settings",
        color = "sky",
        fluidRow(
          column(3,
                 selectInput(
                   ns("roster_format"),
                   "Roster Format",
                   choices = c(
                     "Classic (9 players)" = "classic",
                     "Classic Limited (6 players)" = "classic_limited"
                   ),
                   selected = "classic"
                 )
          ),
          column(3,
                 numericInput(
                   ns("salary_cap"),
                   "Salary Cap",
                   value = 100,
                   min = 50,
                   max = 150,
                   step = 1
                 )
          ),
          column(6,
                 div(
                   style = "padding-top: 25px;",
                   uiOutput(ns("data_status"))
                 )
          )
        )
      )
    ),
    
    # Unmatched Players Alert
    uiOutput(ns("unmatched_alert")),
    
    # Main content - Player Pool LEFT, Lineup RIGHT
    fluidRow(
      # LEFT: Player pool (wider)
      column(
        7,
        ui_card(
          title = "Player Pool",
          color = "sky",
          # Position filter buttons - use actionButton with custom class
          div(
            class = "position-filter-container",
            style = "display: flex; gap: 6px; margin-bottom: 16px; flex-wrap: wrap;",
            actionButton(ns("pos_all"), "ALL", class = "btn-position-filter active"),
            actionButton(ns("pos_c"), "C", class = "btn-position-filter"),
            actionButton(ns("pos_w"), "W", class = "btn-position-filter"),
            actionButton(ns("pos_d"), "D", class = "btn-position-filter"),
            actionButton(ns("pos_g"), "G", class = "btn-position-filter"),
            actionButton(ns("pos_flex"), "FLEX", class = "btn-position-filter")
          ),
          # Scrollable player pool container - height matches lineup card via flexbox
          div(
            class = "nhl-player-pool-container",
            style = "border: 1px solid var(--border); border-radius: 8px;",
            uiOutput(ns("player_pool_rows"))
          )
        )
      ),
      
      # RIGHT: Lineup builder
      column(
        5,
        ui_card(
          title = "Your Lineup",
          color = "sky",
          
          # Generate Optimal button at TOP
          div(
            style = "margin-bottom: 12px;",
            actionButton(
              ns("generate_optimal"),
              "Generate Optimal",
              class = "btn btn-primary",
              icon = icon("bolt"),
              style = "width: 100%; font-weight: 600;"
            ),
            tags$small(
              class = "text-muted",
              style = "display: block; margin-top: 4px; text-align: center;",
              "Uses blended median/ceiling projection"
            )
          ),
          
          # Summary stats row
          div(
            class = "lineup-summary",
            style = "display: flex; justify-content: space-between; margin-bottom: 16px; padding: 12px; background: var(--bg-tan); border-radius: 8px;",
            div(
              div(style = "font-size: 0.75rem; color: var(--text-muted);", "SALARY"),
              div(style = "font-weight: 700;",
                  textOutput(ns("total_salary"), inline = TRUE),
                  span(" / "),
                  textOutput(ns("salary_cap_display"), inline = TRUE)
              )
            ),
            div(
              div(style = "font-size: 0.75rem; color: var(--text-muted);", "MEDIAN"),
              div(style = "font-weight: 700; color: var(--accent-sage);",
                  textOutput(ns("total_median"), inline = TRUE))
            ),
            div(
              div(style = "font-size: 0.75rem; color: var(--text-muted);", "CEILING"),
              div(style = "font-weight: 700; color: var(--accent-coral);",
                  textOutput(ns("total_ceiling"), inline = TRUE))
            ),
            div(
              div(style = "font-size: 0.75rem; color: var(--text-muted);", "REMAINING"),
              div(style = "font-weight: 700;",
                  textOutput(ns("remaining_salary"), inline = TRUE))
            )
          ),
          
          # Lineup slots
          uiOutput(ns("lineup_slots")),
          
          # Optimizer controls
          tags$hr(style = "margin: 16px 0; border-color: var(--border);"),
          
          div(
            style = "padding: 12px; background: var(--bg-secondary); border-radius: 8px;",
            tags$h5(style = "margin: 0 0 12px 0; font-weight: 600;", "Lineup Generator"),
            fluidRow(
              column(4,
                     selectInput(
                       ns("variance_pct"),
                       "Variance %",
                       choices = c("5" = 5, "10" = 10, "15" = 15, "20" = 20, "25" = 25),
                       selected = 15
                     )
              ),
              column(4,
                     numericInput(
                       ns("num_lineups"),
                       "# Lineups",
                       value = 5,
                       min = 1,
                       max = 50,
                       step = 1
                     )
              ),
              column(4,
                     numericInput(
                       ns("min_unique"),
                       "Min Unique",
                       value = 1,
                       min = 0,
                       max = 9,
                       step = 1
                     )
              )
            ),
            div(
              style = "display: flex; gap: 8px; margin-top: 8px;",
              actionButton(
                ns("generate_lineups"),
                "Generate Lineups",
                class = "btn btn-primary",
                icon = icon("magic"),
                style = "flex: 1;"
              ),
              actionButton(
                ns("clear_lineup"),
                icon("trash"),
                class = "btn btn-outline-secondary"
              )
            )
          ),
          
          # Export button
          div(
            style = "margin-top: 12px;",
            actionButton(
              ns("export_lineup"),
              "Export Lineup",
              class = "btn btn-outline-primary",
              icon = icon("download"),
              style = "width: 100%;"
            )
          )
        )
      )
    ),
    
    # Generated Lineups - FULL WIDTH
    fluidRow(
      column(
        12,
        uiOutput(ns("generated_lineups_output"))
      )
    )
  )
}

nhl_handbuild_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("nhl_handbuild_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      player_data = NULL,
      unmatched_players = NULL,
      match_rate = NULL,
      selected_position = "all",
      roster_format = "classic",
      lineup_slots = NULL,
      generated_lineups = NULL,
      optimal_projection = NULL,
      data_source = NULL,
      sort_column = "fpts_blended",
      sort_direction = "desc"
    )
    
    # Load shared data from projections module
    observe({
      if (!is.null(session$userData$nhl_player_data) && is.null(rv$player_data)) {
        log_debug(">>> Loading shared player data from projections module", level = "INFO")
        rv$player_data <- session$userData$nhl_player_data
        rv$unmatched_players <- session$userData$nhl_unmatched
        rv$match_rate <- session$userData$nhl_match_rate
        rv$data_source <- "projections"
        log_debug(">>> Loaded", nrow(rv$player_data), "players from shared data", level = "INFO")
      }
    })
    
    # Initialize lineup slots based on roster format
    observe({
      format_id <- input$roster_format %||% "classic"
      rv$roster_format <- format_id
      
      format_config <- NHL_ROSTER_FORMATS[[format_id]]
      slot_names <- format_config$order
      
      rv$lineup_slots <- setNames(
        lapply(slot_names, function(x) NULL),
        slot_names
      )
    })
    
    # Data status output
    output$data_status <- renderUI({
      if (is.null(rv$player_data)) {
        div(
          style = "color: var(--accent-coral); font-weight: 500;",
          icon("exclamation-triangle"),
          " Please load data in Projections section first"
        )
      } else {
        div(
          style = "color: var(--accent-sage); font-weight: 500;",
          icon("check-circle"),
          sprintf(" %d players loaded (%.0f%% matched)", 
                  nrow(rv$player_data), rv$match_rate %||% 0)
        )
      }
    })
    
    # Salary cap display
    output$salary_cap_display <- renderText({
      sprintf("$%.0f", input$salary_cap %||% 100)
    })
    
    # Unmatched alert
    output$unmatched_alert <- renderUI({
      if (is.null(rv$unmatched_players) || nrow(rv$unmatched_players) == 0) return(NULL)
      
      top_unmatched <- rv$unmatched_players %>% arrange(desc(salary)) %>% head(5)
      
      div(
        class = "alert alert-warning",
        style = "margin-top: 16px; border-left: 4px solid #EBCB8B;",
        tags$strong(icon("exclamation-triangle"),
                    sprintf(" %d unmatched players", nrow(rv$unmatched_players))),
        tags$span(style = "margin-left: 8px;",
                  paste(sapply(1:nrow(top_unmatched), function(i) {
                    sprintf("%s ($%.1f)", top_unmatched$player_name[i], top_unmatched$salary[i])
                  }), collapse = ", "),
                  if (nrow(rv$unmatched_players) > 5) "..."
        )
      )
    })
    
    # Helper to update button active states via shinyjs
    update_position_buttons <- function(active_pos) {
      positions <- c("all", "C", "W", "D", "G", "FLEX")
      for (pos in positions) {
        btn_id <- paste0("pos_", tolower(pos))
        if (tolower(pos) == tolower(active_pos)) {
          shinyjs::addClass(id = btn_id, class = "active")
        } else {
          shinyjs::removeClass(id = btn_id, class = "active")
        }
      }
    }
    
    # Position filter handlers
    observeEvent(input$pos_all, { 
      rv$selected_position <- "all"
      update_position_buttons("all")
    })
    observeEvent(input$pos_c, { 
      rv$selected_position <- "C"
      update_position_buttons("C")
    })
    observeEvent(input$pos_w, { 
      rv$selected_position <- "W"
      update_position_buttons("W")
    })
    observeEvent(input$pos_d, { 
      rv$selected_position <- "D"
      update_position_buttons("D")
    })
    observeEvent(input$pos_g, { 
      rv$selected_position <- "G"
      update_position_buttons("G")
    })
    observeEvent(input$pos_flex, { 
      rv$selected_position <- "FLEX"
      update_position_buttons("FLEX")
    })
    
    # Get locked player IDs
    get_locked_players <- reactive({
      req(rv$lineup_slots)
      unlist(lapply(rv$lineup_slots, function(slot) {
        if (!is.null(slot)) slot$playerid else NULL
      }))
    })
    
    # Calculate lineup stats
    lineup_stats <- reactive({
      req(rv$lineup_slots)
      
      total_salary <- sum(sapply(rv$lineup_slots, function(s) if (!is.null(s)) s$salary else 0))
      total_median <- sum(sapply(rv$lineup_slots, function(s) if (!is.null(s)) s$fpts_median %||% 0 else 0))
      total_ceiling <- sum(sapply(rv$lineup_slots, function(s) if (!is.null(s)) s$fpts_ceiling %||% 0 else 0))
      cap <- as.numeric(input$salary_cap) %||% 100
      
      list(
        total_salary = total_salary,
        total_median = total_median,
        total_ceiling = total_ceiling,
        remaining = cap - total_salary
      )
    })
    
    # Filtered player data with sorting
    filtered_players <- reactive({
      req(rv$player_data)
      
      data <- rv$player_data
      
      if (!"fpts_blended" %in% names(data)) {
        data <- data %>%
          mutate(fpts_blended = (fpts_median + fpts_ceiling) / 2)
      }
      
      if (rv$selected_position == "C") {
        data <- data %>% filter(position == "C")
      } else if (rv$selected_position == "W") {
        data <- data %>% filter(position %in% c("LW", "RW", "W"))
      } else if (rv$selected_position == "D") {
        data <- data %>% filter(position == "D")
      } else if (rv$selected_position == "G") {
        data <- data %>% filter(position == "G")
      } else if (rv$selected_position == "FLEX") {
        data <- data %>% filter(position %in% c("C", "LW", "RW", "W", "D"))
      }
      
      # Apply sorting
      sort_col <- rv$sort_column %||% "fpts_blended"
      sort_dir <- rv$sort_direction %||% "desc"
      
      if (sort_col %in% names(data)) {
        if (sort_dir == "desc") {
          data <- data %>% arrange(desc(.data[[sort_col]]))
        } else {
          data <- data %>% arrange(.data[[sort_col]])
        }
      }
      
      data
    })
    
    # Sort column click handlers
    observeEvent(input$sort_player, {
      if (rv$sort_column == "player_name") {
        rv$sort_direction <- if (rv$sort_direction == "asc") "desc" else "asc"
      } else {
        rv$sort_column <- "player_name"
        rv$sort_direction <- "asc"
      }
    })
    
    observeEvent(input$sort_salary, {
      if (rv$sort_column == "salary") {
        rv$sort_direction <- if (rv$sort_direction == "desc") "asc" else "desc"
      } else {
        rv$sort_column <- "salary"
        rv$sort_direction <- "desc"
      }
    })
    
    observeEvent(input$sort_median, {
      if (rv$sort_column == "fpts_median") {
        rv$sort_direction <- if (rv$sort_direction == "desc") "asc" else "desc"
      } else {
        rv$sort_column <- "fpts_median"
        rv$sort_direction <- "desc"
      }
    })
    
    observeEvent(input$sort_ceiling, {
      if (rv$sort_column == "fpts_ceiling") {
        rv$sort_direction <- if (rv$sort_direction == "desc") "asc" else "desc"
      } else {
        rv$sort_column <- "fpts_ceiling"
        rv$sort_direction <- "desc"
      }
    })
    
    observeEvent(input$sort_value, {
      if (rv$sort_column == "value_median") {
        rv$sort_direction <- if (rv$sort_direction == "desc") "asc" else "desc"
      } else {
        rv$sort_column <- "value_median"
        rv$sort_direction <- "desc"
      }
    })
    
    # Player pool rows - HTML based with sortable headers
    output$player_pool_rows <- renderUI({
      req(filtered_players())
      
      data <- filtered_players()
      locked_ids <- get_locked_players()
      stats <- lineup_stats()
      cap <- as.numeric(input$salary_cap) %||% 100
      
      # Sort indicators
      sort_col <- rv$sort_column %||% "fpts_blended"
      sort_dir <- rv$sort_direction %||% "desc"
      
      get_sort_icon <- function(col) {
        if (sort_col == col) {
          if (sort_dir == "desc") "▼" else "▲"
        } else {
          "▽"
        }
      }
      
      get_header_class <- function(col) {
        if (sort_col == col) "nhl-sort-header active" else "nhl-sort-header"
      }
      
      header <- div(
        class = "nhl-pool-header",
        div(
          class = get_header_class("player_name"),
          onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("sort_player")),
          span("PLAYER"),
          span(class = "nhl-sort-icon", get_sort_icon("player_name"))
        ),
        div(
          class = get_header_class("salary"),
          onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("sort_salary")),
          span("SAL"),
          span(class = "nhl-sort-icon", get_sort_icon("salary"))
        ),
        div(
          class = get_header_class("fpts_median"),
          onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("sort_median")),
          span("MED"),
          span(class = "nhl-sort-icon", get_sort_icon("fpts_median"))
        ),
        div(
          class = get_header_class("fpts_ceiling"),
          onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("sort_ceiling")),
          span("CEIL"),
          span(class = "nhl-sort-icon", get_sort_icon("fpts_ceiling"))
        ),
        div(
          class = get_header_class("value_median"),
          onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("sort_value")),
          span("VAL"),
          span(class = "nhl-sort-icon", get_sort_icon("value_median"))
        )
      )
      
      player_rows <- lapply(1:nrow(data), function(i) {
        p <- data[i, ]
        
        is_locked <- p$playerid %in% locked_ids
        can_afford <- (stats$total_salary + p$salary) <= cap
        clickable <- !is_locked && can_afford
        
        row_class <- "nhl-pool-row"
        if (is_locked) row_class <- paste(row_class, "nhl-pool-row--locked")
        if (!can_afford && !is_locked) row_class <- paste(row_class, "nhl-pool-row--unaffordable")
        
        pos_display <- p$pos_display %||% p$position
        
        row_content <- div(
          class = row_class,
          div(
            class = "nhl-player-cell",
            div(class = "nhl-player-name", p$player_name),
            div(class = "nhl-player-meta", sprintf("%s • %s", pos_display, p$team))
          ),
          div(class = "nhl-salary", sprintf("$%.1f", p$salary)),
          div(class = "nhl-median", if (is.na(p$fpts_median) || p$fpts_median == 0) "—" else sprintf("%.1f", p$fpts_median)),
          div(class = "nhl-ceiling", if (is.na(p$fpts_ceiling) || p$fpts_ceiling == 0) "—" else sprintf("%.1f", p$fpts_ceiling)),
          div(class = "nhl-value", if (is.na(p$value_median) || p$value_median == 0) "—" else sprintf("%.2f", p$value_median))
        )
        
        if (clickable) {
          tags$div(
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              ns("add_player_click"), p$playerid),
            style = "cursor: pointer;",
            row_content
          )
        } else {
          row_content
        }
      })
      
      tagList(header, player_rows)
    })
    
    # Handle player click to add to lineup
    observeEvent(input$add_player_click, {
      req(input$add_player_click)
      req(rv$player_data)
      req(rv$lineup_slots)
      
      player_id <- input$add_player_click
      player <- rv$player_data %>% filter(playerid == player_id)
      if (nrow(player) == 0) return()
      player <- player[1, ]
      
      locked <- get_locked_players()
      if (player_id %in% locked) {
        showNotification("Player already in lineup", type = "warning", duration = 2)
        return()
      }
      
      stats <- lineup_stats()
      cap <- as.numeric(input$salary_cap) %||% 100
      if (stats$total_salary + player$salary > cap) {
        showNotification("Would exceed salary cap", type = "warning", duration = 2)
        return()
      }
      
      pos <- player$position
      format_config <- NHL_ROSTER_FORMATS[[rv$roster_format]]
      slot_order <- format_config$order
      
      slot_name <- NULL
      
      if (pos == "G") {
        if ("G" %in% slot_order && is.null(rv$lineup_slots$G)) slot_name <- "G"
      } else if (pos == "D") {
        if ("D1" %in% slot_order && is.null(rv$lineup_slots$D1)) slot_name <- "D1"
        else if ("D2" %in% slot_order && is.null(rv$lineup_slots$D2)) slot_name <- "D2"
        else if ("FLEX" %in% slot_order && is.null(rv$lineup_slots$FLEX)) slot_name <- "FLEX"
      } else if (pos %in% c("LW", "RW", "W")) {
        if ("W1" %in% slot_order && is.null(rv$lineup_slots$W1)) slot_name <- "W1"
        else if ("W2" %in% slot_order && is.null(rv$lineup_slots$W2)) slot_name <- "W2"
        else if ("W3" %in% slot_order && is.null(rv$lineup_slots$W3)) slot_name <- "W3"
        else if ("FLEX" %in% slot_order && is.null(rv$lineup_slots$FLEX)) slot_name <- "FLEX"
      } else if (pos == "C") {
        if ("C1" %in% slot_order && is.null(rv$lineup_slots$C1)) slot_name <- "C1"
        else if ("C2" %in% slot_order && is.null(rv$lineup_slots$C2)) slot_name <- "C2"
        else if ("FLEX" %in% slot_order && is.null(rv$lineup_slots$FLEX)) slot_name <- "FLEX"
      }
      
      if (!is.null(slot_name)) {
        rv$lineup_slots[[slot_name]] <- list(
          playerid = player$playerid,
          player_name = player$player_name,
          team = player$team,
          position = player$position,
          salary = player$salary,
          fpts_median = player$fpts_median %||% 0,
          fpts_ceiling = player$fpts_ceiling %||% 0
        )
      } else {
        showNotification(paste("No available slot for", pos), type = "warning", duration = 2)
      }
    })
    
    # Lineup slots UI
    output$lineup_slots <- renderUI({
      req(rv$lineup_slots)
      
      format_config <- NHL_ROSTER_FORMATS[[rv$roster_format]]
      slot_order <- format_config$order
      slot_configs <- format_config$slots
      
      create_slot_ui <- function(slot_name, slot_data, pos_label) {
        pos_color <- NHL_POSITION_COLORS[[pos_label]]$bg %||% "#E5E9F0"
        
        div(
          style = sprintf(
            "display: flex; align-items: center; gap: 10px; padding: 8px 10px; margin-bottom: 6px; background: %s; border-radius: 6px; border-left: 4px solid %s; font-size: 0.9rem;",
            if (!is.null(slot_data)) "var(--bg-tan)" else "var(--bg-primary)",
            pos_color
          ),
          tags$span(
            style = sprintf("background: %s; padding: 2px 8px; border-radius: 4px; font-size: 0.8rem; font-weight: 600; min-width: 30px; text-align: center;", pos_color),
            pos_label
          ),
          if (!is.null(slot_data)) {
            tagList(
              div(
                style = "flex: 1;",
                tags$strong(slot_data$player_name),
                tags$span(style = "color: var(--text-muted); margin-left: 6px; font-size: 0.85rem;",
                          sprintf("%s • $%.1f", slot_data$team, slot_data$salary))
              ),
              div(
                style = "text-align: right; min-width: 80px;",
                tags$span(style = "color: var(--accent-sage); font-weight: 600;",
                          sprintf("%.1f", slot_data$fpts_median)),
                tags$span(style = "color: var(--text-muted); margin: 0 2px;", "/"),
                tags$span(style = "color: var(--accent-coral); font-weight: 600;",
                          sprintf("%.1f", slot_data$fpts_ceiling))
              ),
              actionButton(
                ns(paste0("remove_", slot_name)),
                icon("times"),
                class = "btn btn-sm btn-link",
                style = "padding: 2px 6px; color: var(--text-muted);"
              )
            )
          } else {
            div(style = "flex: 1; color: var(--text-muted); font-style: italic;", "Empty")
          }
        )
      }
      
      slots <- rv$lineup_slots
      slot_uis <- lapply(slot_order, function(s) {
        slot_config <- slot_configs[[s]]
        pos_label <- slot_config$display %||% s
        create_slot_ui(s, slots[[s]], pos_label)
      })
      
      do.call(tagList, slot_uis)
    })
    
    # Dynamic remove button handlers
    observe({
      req(rv$lineup_slots)
      format_config <- NHL_ROSTER_FORMATS[[rv$roster_format]]
      
      lapply(format_config$order, function(slot_name) {
        observeEvent(input[[paste0("remove_", slot_name)]], {
          rv$lineup_slots[[slot_name]] <- NULL
        }, ignoreInit = TRUE)
      })
    })
    
    # Summary outputs
    output$total_salary <- renderText({ sprintf("$%.1f", lineup_stats()$total_salary) })
    output$remaining_salary <- renderText({ sprintf("$%.1f", lineup_stats()$remaining) })
    output$total_median <- renderText({ sprintf("%.1f", lineup_stats()$total_median) })
    output$total_ceiling <- renderText({ sprintf("%.1f", lineup_stats()$total_ceiling) })
    
    # Clear lineup
    observeEvent(input$clear_lineup, {
      format_config <- NHL_ROSTER_FORMATS[[rv$roster_format]]
      rv$lineup_slots <- setNames(lapply(format_config$order, function(x) NULL), format_config$order)
      rv$generated_lineups <- NULL
      rv$optimal_projection <- NULL
    })
    
    # Generate optimal lineup using LP optimization
    observeEvent(input$generate_optimal, {
      req(rv$player_data)
      req(rv$lineup_slots)
      
      tryCatch({
        data <- rv$player_data
        cap <- as.numeric(input$salary_cap) %||% 100
        
        if (!"fpts_blended" %in% names(data)) {
          data <- data %>% mutate(fpts_blended = (fpts_median + fpts_ceiling) / 2)
        }
        
        # Get locked and excluded players
        locked_ids <- get_locked_players()
        
        # Run LP optimization
        optimal_lineup <- optimize_nhl_lineup_lp(
          players = data,
          projection_col = "fpts_blended",
          salary_cap = cap,
          roster_format = rv$roster_format,
          locked_ids = locked_ids,
          excluded_ids = c()
        )
        
        if (is.null(optimal_lineup) || nrow(optimal_lineup) == 0) {
          showNotification("Could not find feasible lineup", type = "warning", duration = 3)
          return()
        }
        
        # Assign to slots
        slots <- assign_nhl_lineup_slots(optimal_lineup, rv$roster_format)
        
        # Convert to expected format
        for (slot_name in names(slots)) {
          if (!is.null(slots[[slot_name]])) {
            p <- slots[[slot_name]]
            rv$lineup_slots[[slot_name]] <- list(
              playerid = p$playerid,
              player_name = p$player_name,
              team = p$team,
              position = p$position,
              salary = p$salary,
              fpts_median = p$fpts_median %||% 0,
              fpts_ceiling = p$fpts_ceiling %||% 0
            )
          }
        }
        
        # Store optimal projection for comparison
        new_stats <- lineup_stats()
        rv$optimal_projection <- new_stats$total_median
        
        showNotification("Optimal lineup generated", type = "message", duration = 2)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    })
    
    # Generate multiple lineups using LP optimization
    observeEvent(input$generate_lineups, {
      req(rv$player_data)
      
      variance <- as.numeric(input$variance_pct) %||% 15
      num_lineups <- as.numeric(input$num_lineups) %||% 5
      min_unique <- as.numeric(input$min_unique) %||% 1
      
      log_debug(">>> Generate Lineups clicked", level = "INFO")
      log_debug(">>>   variance:", variance, level = "INFO")
      log_debug(">>>   num_lineups:", num_lineups, level = "INFO")
      log_debug(">>>   min_unique:", min_unique, level = "INFO")
      
      showNotification(
        sprintf("Generating %d lineups (variance: %d%%, min unique: %d)", 
                num_lineups, variance, min_unique),
        type = "message", duration = 2
      )
      
      tryCatch({
        data <- rv$player_data
        cap <- as.numeric(input$salary_cap) %||% 100
        format_config <- NHL_ROSTER_FORMATS[[rv$roster_format]]
        slot_order <- format_config$order
        roster_size <- length(slot_order)
        
        log_debug(">>>   salary_cap:", cap, level = "INFO")
        log_debug(">>>   roster_format:", rv$roster_format, level = "INFO")
        log_debug(">>>   player_data rows:", nrow(data), level = "INFO")
        
        if (!"fpts_blended" %in% names(data)) {
          data <- data %>% mutate(fpts_blended = (fpts_median + fpts_ceiling) / 2)
        }
        
        # Get locked players from current lineup
        locked_ids <- get_locked_players()
        
        withProgress(message = "Generating lineups...", value = 0, {
          # Use LP optimizer
          lp_lineups <- generate_nhl_lineups_with_variance(
            players = data,
            n_lineups = num_lineups,
            projection_col = "fpts_blended",
            variance_pct = variance,
            salary_cap = cap,
            roster_format = rv$roster_format,
            min_unique = min_unique,
            locked_ids = locked_ids,
            excluded_ids = c()
          )
          
          setProgress(value = 0.8, detail = "Processing results...")
          
          log_debug(">>>   LP returned", length(lp_lineups), "lineups", level = "INFO")
          
          if (length(lp_lineups) == 0) {
            showNotification("Could not generate any valid lineups", type = "error", duration = 3)
            return()
          }
          
          # Convert LP results to display format
          generated <- list()
          max_projection <- 0
          
          for (i in seq_along(lp_lineups)) {
            lineup_df <- lp_lineups[[i]]
            
            # Assign to slots
            slots <- assign_nhl_lineup_slots(lineup_df, rv$roster_format)
            
            # Convert to list format
            lineup_list <- list()
            for (slot_name in slot_order) {
              if (!is.null(slots[[slot_name]])) {
                p <- slots[[slot_name]]
                lineup_list[[slot_name]] <- list(
                  playerid = p$playerid,
                  player_name = p$player_name,
                  team = p$team,
                  position = p$position,
                  salary = p$salary,
                  fpts_median = p$fpts_median %||% 0,
                  fpts_ceiling = p$fpts_ceiling %||% 0
                )
              }
            }
            
            total_salary <- sum(lineup_df$salary)
            total_median <- sum(lineup_df$fpts_median, na.rm = TRUE)
            total_ceiling <- sum(lineup_df$fpts_ceiling, na.rm = TRUE)
            total_blended <- (total_median + total_ceiling) / 2
            
            if (total_blended > max_projection) {
              max_projection <- total_blended
            }
            
            generated[[i]] <- list(
              lineup = lineup_list,
              total_salary = total_salary,
              total_median = total_median,
              total_ceiling = total_ceiling,
              total_blended = total_blended,
              slot_order = slot_order
            )
          }
          
          setProgress(value = 0.9, detail = "Calculating differences...")
          
          # Find optimal and calculate differences
          best_idx <- which.max(sapply(generated, function(lu) lu$total_blended %||% 0))
          optimal_lineup <- generated[[best_idx]]
          optimal_player_ids <- unlist(lapply(optimal_lineup$lineup, function(p) p$playerid))
          
          # Add player difference count
          for (i in seq_along(generated)) {
            lineup_player_ids <- unlist(lapply(generated[[i]]$lineup, function(p) p$playerid))
            players_different <- length(setdiff(lineup_player_ids, optimal_player_ids)) + 
              length(setdiff(optimal_player_ids, lineup_player_ids))
            generated[[i]]$players_different <- players_different / 2
          }
          
          rv$optimal_projection <- max_projection
          rv$generated_lineups <- generated
          
          setProgress(value = 1, detail = "Done!")
        })
        
        showNotification(sprintf("Generated %d lineup(s)", length(rv$generated_lineups)), type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
        log_debug("Generate lineups error:", e$message, level = "ERROR")
      })
    })
    
    # Generated lineups output with PROMINENT delta display
    output$generated_lineups_output <- renderUI({
      req(rv$generated_lineups)
      
      lineups <- rv$generated_lineups
      if (length(lineups) == 0) return(NULL)
      
      # Find the best lineup projection for comparison
      best_projection <- max(sapply(lineups, function(lu) lu$total_blended %||% 0))
      
      ui_card(
        title = sprintf("Generated Lineups (%d)", length(lineups)),
        color = "sky",
        div(
          class = "nhl-lineups-grid",
          lapply(seq_along(lineups), function(i) {
            lu <- lineups[[i]]
            
            # Calculate delta to optimal
            delta <- lu$total_blended - best_projection
            delta_class <- if (delta >= 0) "nhl-lineup-delta positive" else "nhl-lineup-delta"
            delta_text <- if (delta >= 0) sprintf("+%.1f pts", delta) else sprintf("%.1f pts", delta)
            players_diff <- lu$players_different %||% 0
            
            div(
              class = "nhl-lineup-card",
              div(
                class = "nhl-lineup-card-header",
                div(class = "nhl-lineup-card-title", sprintf("Lineup #%d", i)),
                div(
                  style = "text-align: right;",
                  # PROMINENT delta display with players different
                  div(
                    style = "display: flex; align-items: center; justify-content: flex-end; gap: 12px;",
                    div(class = delta_class, delta_text),
                    div(
                      style = "font-size: 0.9rem; font-weight: 600; color: #7A7A7A;",
                      sprintf("%d diff", as.integer(players_diff))
                    )
                  ),
                  div(class = "nhl-lineup-card-stats",
                      sprintf("$%.1f | Med: %.1f | Ceil: %.1f", 
                              lu$total_salary, lu$total_median, lu$total_ceiling))
                )
              ),
              # Player rows container
              div(
                class = "nhl-lineup-players",
                lapply(lu$slot_order, function(slot_name) {
                  player <- lu$lineup[[slot_name]]
                  if (is.null(player)) {
                    # Show empty slot
                    div(
                      class = "nhl-lineup-row",
                      div(class = "nhl-pos-badge", style = "background: #E5E9F0;", substr(slot_name, 1, 2)),
                      div(style = "font-style: italic; color: #999;", "(empty)"),
                      div("—"),
                      div("—")
                    )
                  } else {
                    pos_color <- NHL_POSITION_COLORS[[player$position]]$bg %||% "#E5E9F0"
                    
                    div(
                      class = "nhl-lineup-row",
                      div(class = "nhl-pos-badge", style = sprintf("background: %s;", pos_color), substr(slot_name, 1, 2)),
                      div(style = "font-weight: 600; font-size: 0.8rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", player$player_name),
                      div(style = "font-size: 0.75rem; text-align: center;", sprintf("$%.1f", player$salary)),
                      div(
                        style = "text-align: right; font-size: 0.75rem;",
                        tags$span(style = "color: var(--accent-sage);", sprintf("%.0f", player$fpts_median)),
                        "/",
                        tags$span(style = "color: var(--accent-coral);", sprintf("%.0f", player$fpts_ceiling))
                      )
                    )
                  }
                })
              ),
              div(
                style = "margin-top: 8px; text-align: right;",
                actionButton(
                  ns(paste0("use_lineup_", i)),
                  "Use",
                  class = "btn btn-sm btn-outline-primary",
                  style = "padding: 2px 10px; font-size: 0.75rem;"
                )
              )
            )
          })
        )
      )
    })
    
    # Handle "Use This Lineup" buttons
    observe({
      req(rv$generated_lineups)
      
      lapply(seq_along(rv$generated_lineups), function(i) {
        observeEvent(input[[paste0("use_lineup_", i)]], {
          rv$lineup_slots <- rv$generated_lineups[[i]]$lineup
          showNotification(sprintf("Loaded lineup #%d", i), type = "message", duration = 2)
        }, ignoreInit = TRUE)
      })
    })
    
    # Export lineup
    observeEvent(input$export_lineup, {
      req(rv$lineup_slots)
      format_config <- NHL_ROSTER_FORMATS[[rv$roster_format]]
      
      lineup_data <- do.call(rbind, lapply(format_config$order, function(slot_name) {
        slot <- rv$lineup_slots[[slot_name]]
        if (!is.null(slot)) {
          data.frame(Slot = slot_name, Player = slot$player_name, Team = slot$team,
                     Pos = slot$position, Salary = slot$salary, Median = slot$fpts_median,
                     Ceiling = slot$fpts_ceiling, stringsAsFactors = FALSE)
        } else {
          data.frame(Slot = slot_name, Player = "(empty)", Team = "", Pos = "",
                     Salary = 0, Median = 0, Ceiling = 0, stringsAsFactors = FALSE)
        }
      }))
      
      if (all(lineup_data$Player == "(empty)")) {
        showNotification("No players in lineup", type = "warning")
        return()
      }
      
      stats <- lineup_stats()
      cap <- as.numeric(input$salary_cap) %||% 100
      
      output_text <- paste(
        sprintf("NHL FanTeam %s Lineup", format_config$name),
        sprintf("Salary: $%.1f / $%.0f", stats$total_salary, cap),
        sprintf("Median: %.1f | Ceiling: %.1f", stats$total_median, stats$total_ceiling),
        "", paste(capture.output(print(lineup_data, row.names = FALSE)), collapse = "\n"),
        sep = "\n"
      )
      
      showModal(modalDialog(title = "Lineup Export", pre(output_text), footer = modalButton("Close"), easyClose = TRUE))
    })
  })
}