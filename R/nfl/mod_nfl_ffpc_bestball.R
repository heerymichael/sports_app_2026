# =============================================================================
# Module: NFL FFPC Bestball
# 
# Draft board for FFPC Bestball drafts with:
# - CSV rankings import
# - Snake draft pick highlighting
# - Value calculations (ADP vs pick position)
# - Player removal tracking
# - Team filtering
# 
# Styled in Stabilo illustrated aesthetic
# =============================================================================

#' NFL FFPC Bestball UI
#' @param id Module namespace ID
nfl_ffpc_bestball_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("nfl_ffpc_bestball_ui() called with id:", id, level = "INFO")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("FFPC Bestball Draft Board"),
      tags$p(class = "text-muted", "Track your draft position and find value picks in real-time")
    ),
    
    # Combined Controls Card
    fluidRow(
      column(12,
             ui_card(
               title = "Draft Settings",
               color = NFL_CARD_COLOR,
               
               fluidRow(
                 column(3,
                        selectInput(ns("competition"), "Competition",
                                    choices = c("Superflex" = "Superflex", "Classic" = "Classic"),
                                    selected = "Superflex")
                 ),
                 column(3,
                        numericInput(ns("draft_spot"), "My Draft Spot",
                                     value = 1, min = 1, max = 12, step = 1)
                 ),
                 column(3,
                        selectizeInput(ns("team_filter"), "Team",
                                       choices = c("All Teams" = "all"),
                                       selected = "all",
                                       options = list(
                                         render = I("{
                           option: function(item, escape) {
                             if (item.value === 'all') {
                               return '<div class=\"option\">' +
                                 '<span style=\"display: inline-block; width: 24px;\"></span>' +
                                 '<span>' + escape(item.label) + '</span>' +
                               '</div>';
                             }
                             return '<div class=\"option\" style=\"display: flex; align-items: center; gap: 8px;\">' +
                               '<img src=\"nfl_logos/' + escape(item.value) + '.webp\" ' +
                                 'style=\"width: 24px; height: 24px; object-fit: contain;\" ' +
                                 'onerror=\"this.style.display=\\'none\\'\">' +
                               '<span>' + escape(item.label) + '</span>' +
                             '</div>';
                           },
                           item: function(item, escape) {
                             if (item.value === 'all') {
                               return '<div class=\"item\">' + escape(item.label) + '</div>';
                             }
                             return '<div class=\"item\" style=\"display: flex; align-items: center; gap: 8px;\">' +
                               '<img src=\"nfl_logos/' + escape(item.value) + '.webp\" ' +
                                 'style=\"width: 20px; height: 20px; object-fit: contain;\" ' +
                                 'onerror=\"this.style.display=\\'none\\'\">' +
                               '<span>' + escape(item.label) + '</span>' +
                             '</div>';
                           }
                         }")
                                       )
                        )
                 ),
                 column(3,
                        selectInput(ns("position_filter"), "Position",
                                    choices = c("All" = "all", "QB", "RB", "WR", "TE"),
                                    selected = "all")
                 )
               )
             )
      )
    ),
    
    # Stats and Reset Row
    fluidRow(
      column(12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 1rem;",
               
               # Stats display
               div(
                 style = "display: flex; gap: 2rem; align-items: center;",
                 div(
                   class = "draft-stat",
                   span(class = "draft-stat-label", "Current Pick: "),
                   span(class = "draft-stat-value", textOutput(ns("current_pick"), inline = TRUE))
                 ),
                 div(
                   class = "draft-stat",
                   span(class = "draft-stat-label", "Players Remaining: "),
                   span(class = "draft-stat-value", textOutput(ns("players_remaining"), inline = TRUE))
                 ),
                 div(
                   class = "draft-stat",
                   span(class = "draft-stat-label", "My Next Pick: "),
                   span(class = "draft-stat-value", style = "color: var(--accent-coral); font-weight: 700;",
                        textOutput(ns("my_next_pick"), inline = TRUE))
                 ),
                 div(
                   class = "draft-stat",
                   span(class = "draft-stat-label", "Drafts Tracked: "),
                   span(class = "draft-stat-value", textOutput(ns("drafts_tracked"), inline = TRUE))
                 )
               ),
               
               # Reset button
               actionButton(ns("reset_board"), "Reset Board", 
                            class = "btn-secondary",
                            icon = icon("refresh"))
             )
      )
    ),
    
    tags$br(),
    
    # Target selector - separate from my_team to avoid re-render issues
    uiOutput(ns("target_selector_container")),
    
    # My Team container - shows drafted picks by position
    uiOutput(ns("my_team_container")),
    
    # Draft board table
    uiOutput(ns("draft_board_container"))
  )
}

#' NFL FFPC Bestball Server
#' @param id Module namespace ID
nfl_ffpc_bestball_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # INITIALIZATION
    # =========================================================================
    log_debug("========================================", level = "INFO")
    log_debug("nfl_ffpc_bestball_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      draft_data = NULL,
      drafted_players = character(0),
      my_picks = list(           # Track my picks by position
        QB = data.frame(),
        RB = data.frame(),
        WR = data.frame(),
        TE = data.frame()
      ),
      targets = list(            # Target players (semi-transparent cards)
        QB = data.frame(),
        RB = data.frame(),
        WR = data.frame(),
        TE = data.frame()
      ),
      headshots_cache = NULL,
      draft_history = NULL,      # Historical draft counts per player
      draft_pairs = NULL,        # Historical player pairing counts
      total_drafts = 0,          # Total number of drafts for percentage calc
      initialized = FALSE
    )
    
    # Load headshots once at startup
    observe({
      if (is.null(rv$headshots_cache)) {
        log_debug(">>> Loading headshots cache...", level = "INFO")
        tryCatch({
          rv$headshots_cache <- get_player_headshots()
          if (!is.null(rv$headshots_cache)) {
            log_debug(">>> Headshots cache loaded:", nrow(rv$headshots_cache), "players", level = "INFO")
          }
        }, error = function(e) {
          log_debug(">>> Error loading headshots:", e$message, level = "WARN")
          rv$headshots_cache <- data.frame()
        })
      }
    }) |> bindEvent(TRUE, once = TRUE)
    
    # =========================================================================
    # DRAFT HISTORY LOADING
    # Loads historical draft data filtered by competition type
    # Google Sheet: https://docs.google.com/spreadsheets/d/1BVOJ_FIfJLB1V6722LqwxmeyEtYGkS6qM2uxUhTEuP0
    # =========================================================================
    
    FFPC_HISTORY_SHEET_ID <- "1BVOJ_FIfJLB1V6722LqwxmeyEtYGkS6qM2uxUhTEuP0"
    
    observe({
      comp <- input$competition
      req(comp)
      
      log_debug(">>> Loading draft history for competition:", comp, level = "INFO")
      
      tryCatch({
        # Deauth for public sheet access (required for shinyapps.io)
        googlesheets4::gs4_deauth()
        
        # Load from Google Sheet
        raw_data <- googlesheets4::read_sheet(
          FFPC_HISTORY_SHEET_ID,
          sheet = "Sheet1"
        )
        
        if (is.null(raw_data) || nrow(raw_data) == 0) {
          log_debug(">>> No draft history data found", level = "WARN")
          rv$draft_history <- data.frame(player = character(0), times_drafted = integer(0))
          rv$draft_pairs <- data.frame(player_1 = character(0), player_2 = character(0), times_paired = integer(0))
          return()
        }
        
        log_debug(">>> Raw draft history loaded:", nrow(raw_data), "drafts", level = "INFO")
        
        # Clean column names
        raw_data <- raw_data %>% janitor::clean_names()
        
        # Filter by competition type (case-insensitive match)
        # comp is now "Superflex" or "Classic" directly from the input
        filtered <- raw_data %>% 
          filter(tolower(competition) == tolower(comp))
        
        log_debug(">>> Filtered to", nrow(filtered), "drafts for", comp, level = "INFO")
        
        if (nrow(filtered) == 0) {
          rv$draft_history <- data.frame(player = character(0), times_drafted = integer(0))
          rv$draft_pairs <- data.frame(player_1 = character(0), player_2 = character(0), times_paired = integer(0))
          rv$total_drafts <- 0
          return()
        }
        
        # Store total drafts for percentage calculations
        rv$total_drafts <- nrow(filtered)
        
        # Create unique draft ID from league_number + date
        filtered <- filtered %>%
          mutate(draft_id = paste(league_number, date, sep = "_"))
        
        # Identify round columns (columns starting with "x" followed by number, or just numbers)
        round_cols <- names(filtered)[grepl("^x?\\d+$", names(filtered))]
        
        # If no round columns found, try looking for columns 7 onwards
        if (length(round_cols) == 0) {
          # Assume columns after draft_spot are rounds
          meta_cols <- c("competition", "stake", "date", "league_number", "type", "draft_spot", "draft_id")
          round_cols <- setdiff(names(filtered), meta_cols)
        }
        
        log_debug(">>> Found round columns:", paste(round_cols, collapse = ", "), level = "DEBUG")
        
        # Pivot to long format: one row per player pick
        picks_long <- filtered %>%
          select(draft_id, all_of(round_cols)) %>%
          tidyr::pivot_longer(
            cols = all_of(round_cols),
            names_to = "round",
            values_to = "player"
          ) %>%
          filter(!is.na(player) & player != "") %>%
          mutate(player = trimws(as.character(player)))
        
        log_debug(">>> Total picks found:", nrow(picks_long), level = "INFO")
        
        # Count times each player was drafted
        rv$draft_history <- picks_long %>%
          group_by(player) %>%
          summarise(times_drafted = n(), .groups = "drop") %>%
          arrange(desc(times_drafted))
        
        log_debug(">>> Unique players drafted:", nrow(rv$draft_history), level = "INFO")
        
        # Count player pairings (players drafted together in same draft)
        # This creates all pairs within each draft
        rv$draft_pairs <- picks_long %>%
          inner_join(picks_long, by = "draft_id", suffix = c("_1", "_2")) %>%
          filter(player_1 < player_2) %>%  # Avoid duplicates and self-pairs
          group_by(player_1, player_2) %>%
          summarise(times_paired = n(), .groups = "drop") %>%
          arrange(desc(times_paired))
        
        log_debug(">>> Player pairs calculated:", nrow(rv$draft_pairs), level = "INFO")
        log_debug(">>> Draft history loaded successfully for", comp, level = "INFO")
        
      }, error = function(e) {
        log_debug(">>> Error loading draft history:", e$message, level = "WARN")
        rv$draft_history <- data.frame(player = character(0), times_drafted = integer(0))
        rv$draft_pairs <- data.frame(player_1 = character(0), player_2 = character(0), times_paired = integer(0))
        rv$total_drafts <- 0
      })
    })
    
    # =========================================================================
    # PLAYER NAME CORRECTIONS
    # Hard-coded mapping for headshot matching
    # =========================================================================
    player_name_corrections <- list(
      "Ken Walker" = "Kenneth Walker III",
      "DK Metcalf" = "D.K. Metcalf",
      "Gabe Davis" = "Gabriel Davis",
      "A.J. Brown" = "AJ Brown",
      "J.K. Dobbins" = "JK Dobbins",
      "T.J. Hockenson" = "TJ Hockenson",
      "Hollywood Brown" = "Marquise Brown"
    )
    
    # =========================================================================
    # HEATMAP COLOR FUNCTION
    # Diverging scale: Coral (negative) -> White (0) -> Teal (positive)
    # Matches the projections table value column style
    # =========================================================================
    get_value_heatmap_color <- function(pct_value, all_values = NULL) {
      if (is.na(pct_value)) return("")
      
      # Diverging scale: Coral (low) -> White (0) -> Teal (high)
      # Coral: #D08770 = rgb(208, 135, 112)
      # White: #FFFFFF = rgb(255, 255, 255)
      # Teal:  #8FBCBB = rgb(143, 188, 187)
      
      midpoint <- 0
      
      # Set reasonable bounds for the scale
      
      min_val <- if (!is.null(all_values)) min(all_values, na.rm = TRUE) else -25
      
      max_val <- if (!is.null(all_values)) max(all_values, na.rm = TRUE) else 25
      
      # Ensure we have some range
      
      if (min_val >= midpoint) min_val <- -25
      if (max_val <= midpoint) max_val <- 25
      
      if (pct_value < midpoint) {
        # Below midpoint: interpolate from coral to white
        t <- (pct_value - min_val) / (midpoint - min_val)
        t <- max(0, min(1, t))
        
        r <- round(208 + (255 - 208) * t)
        g <- round(135 + (255 - 135) * t)
        b <- round(112 + (255 - 112) * t)
      } else {
        # At or above midpoint: interpolate from white to teal
        t <- (pct_value - midpoint) / (max_val - midpoint)
        t <- max(0, min(1, t))
        
        r <- round(255 + (143 - 255) * t)
        g <- round(255 + (188 - 255) * t)
        b <- round(255 + (187 - 255) * t)
      }
      
      sprintf("rgb(%d, %d, %d)", r, g, b)
    }
    
    
    # =========================================================================
    # LOAD RANKINGS FROM GOOGLE SHEETS
    # Sheet: https://docs.google.com/spreadsheets/d/1dHS68tTc1WOz9X32FgIR1q_9VmP-g2XsIhaQ_dqdqw4
    # Worksheets: "Superflex" or "Classic" based on competition selection
    # =========================================================================
    FFPC_RANKINGS_SHEET_ID <- "1dHS68tTc1WOz9X32FgIR1q_9VmP-g2XsIhaQ_dqdqw4"
    
    observe({
      comp <- input$competition
      req(comp)
      
      log_debug(">>> Loading rankings for competition:", comp, level = "INFO")
      
      tryCatch({
        # Deauth for public sheet access (required for shinyapps.io)
        googlesheets4::gs4_deauth()
        
        # Load from Google Sheet - use competition value as worksheet name
        raw_data <- googlesheets4::read_sheet(
          FFPC_RANKINGS_SHEET_ID,
          sheet = comp  # "Superflex" or "Classic"
        )
        
        if (is.null(raw_data) || nrow(raw_data) == 0) {
          log_debug(">>> No rankings data found for", comp, level = "WARN")
          showNotification(paste("No data found in", comp, "worksheet"), type = "warning", duration = 5)
          return()
        }
        
        log_debug(">>> Raw data loaded:", nrow(raw_data), "rows,", ncol(raw_data), "columns", level = "INFO")
        log_debug(">>> Columns found:", paste(names(raw_data), collapse = ", "), level = "DEBUG")
        
        # Clean column names
        raw_data <- raw_data %>% janitor::clean_names()
        
        # Validate required columns (check for common variants)
        col_names <- names(raw_data)
        
        # Find name column
        name_col <- col_names[col_names %in% c("name", "player", "player_name")][1]
        position_col <- col_names[col_names %in% c("position", "pos")][1]
        team_col <- col_names[col_names %in% c("team")][1]
        adp_col <- col_names[col_names %in% c("adp", "rank", "ranking")][1]
        etr_col <- col_names[col_names %in% c("etr_rank", "etr", "rank")][1]
        
        if (is.na(name_col) || is.na(position_col) || is.na(team_col) || is.na(adp_col)) {
          log_debug(">>> Missing required columns. Found:", paste(col_names, collapse = ", "), level = "ERROR")
          showNotification("Missing required columns (name, position, team, adp)", type = "error", duration = 10)
          return()
        }
        
        # Process rankings
        rankings <- raw_data %>%
          rename(
            Name = !!sym(name_col),
            Position = !!sym(position_col),
            Team = !!sym(team_col),
            ADP = !!sym(adp_col)
          )
        
        # Add ETR rank if available
        if (!is.na(etr_col) && etr_col != adp_col) {
          rankings <- rankings %>% rename(ETR_Rank = !!sym(etr_col))
        }
        
        rankings <- rankings %>%
          mutate(ADP = as.numeric(ADP)) %>%
          filter(!is.na(ADP)) %>%
          arrange(ADP) %>%
          mutate(
            REMOVE = "\u2716",
            ADP_RANK = row_number(),
            PLAYER = as.character(Name),
            POSITION = as.character(Position),
            TEAM = as.character(Team),
            ETR_RANK = if ("ETR_Rank" %in% names(.)) as.numeric(ETR_Rank) else ADP_RANK
          ) %>%
          select(REMOVE, ADP_RANK, ADP, ETR_RANK, PLAYER, TEAM, POSITION)
        
        log_debug(">>> Rankings processed:", nrow(rankings), "players", level = "INFO")
        
        # Apply name corrections for headshot matching
        if (length(player_name_corrections) > 0) {
          for (old_name in names(player_name_corrections)) {
            new_name <- player_name_corrections[[old_name]]
            matched <- sum(rankings$PLAYER == old_name)
            if (matched > 0) {
              rankings$PLAYER[rankings$PLAYER == old_name] <- new_name
              log_debug(">>>   Corrected:", old_name, "->", new_name, level = "DEBUG")
            }
          }
        }
        
        # Match with headshots
        if (!is.null(rv$headshots_cache) && nrow(rv$headshots_cache) > 0) {
          log_debug(">>> Matching headshots...", level = "INFO")
          
          rankings$headshot_url <- sapply(rankings$PLAYER, function(name) {
            match_row <- rv$headshots_cache[rv$headshots_cache$player == name, ]
            if (nrow(match_row) > 0 && !is.na(match_row$headshot_url[1])) {
              match_row$headshot_url[1]
            } else {
              "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
            }
          })
          
          rankings$team_bg_color <- sapply(rankings$TEAM, function(team) {
            get_team_bg_color(team)
          })
          
          matched_count <- sum(rankings$headshot_url != "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png")
          log_debug(">>> Matched headshots:", matched_count, "of", nrow(rankings), level = "INFO")
        } else {
          rankings$headshot_url <- "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
          rankings$team_bg_color <- "#E0E0E0"
        }
        
        rankings$drafted <- FALSE
        rv$draft_data <- rankings
        rv$drafted_players <- character(0)
        rv$initialized <- TRUE
        
        # Reset my picks and targets when competition changes
        rv$my_picks <- list(
          QB = data.frame(),
          RB = data.frame(),
          WR = data.frame(),
          TE = data.frame()
        )
        rv$targets <- list(
          QB = data.frame(),
          RB = data.frame(),
          WR = data.frame(),
          TE = data.frame()
        )
        
        # Update team filter choices
        teams <- sort(unique(rankings$TEAM[rankings$TEAM != "" & !is.na(rankings$TEAM)]))
        updateSelectInput(session, "team_filter",
                          choices = c("All Teams" = "all", setNames(teams, teams)))
        
        log_debug(">>> Draft board initialized successfully for", comp, level = "INFO")
        showNotification(paste(comp, "rankings loaded! Click X to remove drafted players."), 
                         type = "message", duration = 5)
        
      }, error = function(e) {
        log_debug(">>> Error loading rankings:", e$message, level = "ERROR")
        showNotification(paste("Error loading rankings:", e$message), type = "error", duration = 10)
      })
    })
    
    # =========================================================================
    # SNAKE DRAFT PICK CALCULATOR
    # =========================================================================
    calculate_my_picks <- function(draft_spot, num_teams, total_original, players_remaining) {
      picks <- integer(0)
      picks_light_before <- integer(0)
      picks_light_after <- integer(0)
      
      total_drafted <- total_original - players_remaining
      
      for (round in 1:30) {
        if (round %% 2 == 1) {
          # Odd rounds: pick in order
          pick_num <- (round - 1) * num_teams + draft_spot
        } else {
          # Even rounds: snake back
          pick_num <- (round - 1) * num_teams + (num_teams + 1 - draft_spot)
        }
        
        board_position <- pick_num - total_drafted
        
        if (board_position > 0 && board_position <= players_remaining) {
          picks <- c(picks, board_position)
          
          # Highlight 2 picks before and after
          if (board_position - 2 > 0) picks_light_before <- c(picks_light_before, board_position - 2)
          if (board_position - 1 > 0) picks_light_before <- c(picks_light_before, board_position - 1)
          if (board_position + 1 <= players_remaining) picks_light_after <- c(picks_light_after, board_position + 1)
          if (board_position + 2 <= players_remaining) picks_light_after <- c(picks_light_after, board_position + 2)
        }
      }
      
      list(
        my_picks = unique(picks),
        light_before = unique(picks_light_before),
        light_after = unique(picks_light_after)
      )
    }
    
    # =========================================================================
    # CALCULATE "DRAFT BY" ROUND FOR TARGET PLAYERS
    # Returns the last pick you have before the player's ADP rank
    # =========================================================================
    calculate_draft_by_round <- function(player_adp, draft_spot, num_teams, total_drafted = 0) {
      # Get all my pick positions (overall pick numbers)
      my_overall_picks <- integer(0)
      
      for (round in 1:30) {
        if (round %% 2 == 1) {
          pick_num <- (round - 1) * num_teams + draft_spot
        } else {
          pick_num <- (round - 1) * num_teams + (num_teams + 1 - draft_spot)
        }
        my_overall_picks <- c(my_overall_picks, pick_num)
      }
      
      # Find the last pick before the player's ADP
      # player_adp is already an overall rank
      picks_before_adp <- my_overall_picks[my_overall_picks < player_adp]
      
      if (length(picks_before_adp) == 0) {
        return(list(round = 1, pick = draft_spot, overall = my_overall_picks[1], urgent = TRUE))
      }
      
      last_pick <- max(picks_before_adp)
      round_num <- ceiling(last_pick / num_teams)
      pick_in_round <- ((last_pick - 1) %% num_teams) + 1
      
      # Check if this is the very next pick (urgent)
      current_overall <- total_drafted + 1
      is_urgent <- (last_pick - current_overall) < 3
      
      list(round = round_num, pick = pick_in_round, overall = last_pick, urgent = is_urgent)
    }
    
    # =========================================================================
    # STATS OUTPUTS
    # =========================================================================
    # Default number of teams (FFPC standard)
    # =========================================================================
    NUM_TEAMS <- 12
    
    # =========================================================================
    output$current_pick <- renderText({
      req(rv$draft_data)
      data <- rv$draft_data
      num_teams <- NUM_TEAMS
      total_original <- nrow(data)
      remaining <- sum(!data$drafted)
      current_pick <- total_original - remaining + 1
      
      # Calculate round and pick within round
      round_num <- ceiling(current_pick / num_teams)
      pick_in_round <- ((current_pick - 1) %% num_teams) + 1
      
      sprintf("%d.%02d (%d)", round_num, pick_in_round, current_pick)
    })
    
    output$players_remaining <- renderText({
      req(rv$draft_data)
      data <- rv$draft_data
      remaining <- sum(!data$drafted)
      as.character(remaining)
    })
    
    output$my_next_pick <- renderText({
      req(rv$draft_data, input$draft_spot)
      
      data <- rv$draft_data
      total_original <- nrow(data)
      remaining <- sum(!data$drafted)
      
      pick_info <- calculate_my_picks(input$draft_spot, NUM_TEAMS, total_original, remaining)
      
      if (length(pick_info$my_picks) > 0) {
        # Convert board position to overall pick number
        total_drafted <- total_original - remaining
        overall_pick <- total_drafted + pick_info$my_picks[1]
        round_num <- ceiling(overall_pick / NUM_TEAMS)
        pick_in_round <- ((overall_pick - 1) %% NUM_TEAMS) + 1
        sprintf("%d.%02d (#%d on board)", round_num, pick_in_round, pick_info$my_picks[1])
      } else {
        "-"
      }
    })
    
    output$drafts_tracked <- renderText({
      as.character(rv$total_drafts %||% 0)
    })
    
    # =========================================================================
    # HELPER: Create player card for My Team display
    # =========================================================================
    create_ffpc_player_card <- function(player_row, qb_team = NULL, is_target = FALSE, draft_by_info = NULL) {
      # Split name into first and last
      name_parts <- strsplit(player_row$PLAYER, " ")[[1]]
      first_name <- name_parts[1]
      surname <- if (length(name_parts) > 1) paste(name_parts[-1], collapse = " ") else ""
      
      # Check if stacked with QB (for non-QB positions)
      is_qb_stacked <- !is.null(qb_team) && 
        player_row$POSITION != "QB" && 
        player_row$TEAM == qb_team
      
      # Card class - add target modifier if applicable
      card_class <- if (is_target) "ffpc-player-card ffpc-player-card--target" else "ffpc-player-card"
      
      # Remove input name depends on whether it's a target or actual pick
      remove_input <- if (is_target) "remove_target" else "remove_my_pick"
      
      div(
        class = card_class,
        
        # Headshot (restored - no badge)
        div(
          class = "ffpc-player-card__headshot",
          style = sprintf("background-color: %s;", player_row$team_bg_color %||% "#E0E0E0"),
          tags$img(
            src = player_row$headshot_url %||% "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png",
            onerror = "this.src='https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png'"
          )
        ),
        
        # Name section
        div(
          class = "ffpc-player-card__name",
          div(class = "ffpc-player-card__firstname", first_name),
          div(class = "ffpc-player-card__surname", surname)
        ),
        
        # Draft-by indicator for targets (positioned to left of other indicators)
        if (is_target && !is.null(draft_by_info)) {
          div(
            class = paste("ffpc-draft-by-indicator", if (draft_by_info$urgent) "ffpc-draft-by-indicator--urgent" else ""),
            sprintf("%d.%02d", draft_by_info$round, draft_by_info$pick)
          )
        },
        
        # Indicators section - QB left, weeks stacked vertically right
        div(
          class = "ffpc-player-card__indicators",
          # QB Stack indicator (only for non-QB)
          if (player_row$POSITION != "QB") {
            tags$span(
              class = paste("ffpc-indicator--qb-stack", if (is_qb_stacked) "active" else ""),
              "QB"
            )
          },
          # Week correlation indicators - in vertical container
          div(
            class = "ffpc-player-card__weeks",
            tags$span(class = "ffpc-indicator--week", "15"),
            tags$span(class = "ffpc-indicator--week", "16"),
            tags$span(class = "ffpc-indicator--week", "17")
          )
        ),
        
        # Team logo (same size as headshot - 40px, with team background color)
        div(
          class = "ffpc-player-card__team-logo",
          style = sprintf("background-color: %s;", player_row$team_bg_color %||% "#E0E0E0"),
          tags$img(
            src = sprintf("nfl_logos/%s.webp", player_row$TEAM),
            onerror = "this.style.display='none'"
          )
        ),
        
        # Remove button (hidden by default, shows on hover)
        tags$button(
          class = "ffpc-player-card__remove",
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
            ns(remove_input),
            player_row$PLAYER
          ),
          icon("times")
        )
      )
    }
    
    # =========================================================================
    # TARGET SELECTOR - Separate to avoid re-render issues
    # =========================================================================
    output$target_selector_container <- renderUI({
      # Only render once we have draft data
      if (is.null(rv$draft_data)) {
        return(NULL)
      }
      
      tagList(
        # Inline style to force width
        tags$style(HTML("
          .target-input-row { display: flex; align-items: start; gap: 12px; }
          .target-input-row .shiny-input-container { flex: 1; margin-bottom: 0; }
          .target-input-row .selectize-control { width: 100% !important; }
          .target-selector-card { margin-bottom: 1.5rem !important; }
        ")),
        div(
          class = "target-selector-card",
          tags$label(
            style = "font-weight: 700; font-size: 0.9rem; display: block; margin-bottom: 0.5rem;", 
            "Add Targets"
          ),
          div(
            class = "target-input-row",
            selectizeInput(
              ns("add_target"),
              label = NULL,
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(
                placeholder = "Search and select players...",
                plugins = list('remove_button'),
                maxItems = 10
              )
            ),
            actionButton(
              ns("confirm_targets"),
              "Add Selected",
              class = "btn-primary",
              style = "padding: 0.5rem 1rem; white-space: nowrap; flex-shrink: 0;"
            )
          )
        )
      )
    })
    
    # =========================================================================
    # MY TEAM CONTAINER - COLUMNS layout with TARGETS
    # =========================================================================
    output$my_team_container <- renderUI({
      log_debug(">>> Rendering my team container", level = "DEBUG")
      
      # Get QB team for stack highlighting (from picks or targets)
      qb_team <- NULL
      if (nrow(rv$my_picks$QB) > 0) {
        qb_team <- rv$my_picks$QB$TEAM[1]
      } else if (nrow(rv$targets$QB) > 0) {
        qb_team <- rv$targets$QB$TEAM[1]
      }
      
      # Count total picks (not including targets)
      total_picks <- nrow(rv$my_picks$QB) + nrow(rv$my_picks$RB) + 
        nrow(rv$my_picks$WR) + nrow(rv$my_picks$TE)
      
      # Count total targets
      total_targets <- nrow(rv$targets$QB) + nrow(rv$targets$RB) + 
        nrow(rv$targets$WR) + nrow(rv$targets$TE)
      
      # Get draft context for calculating draft-by rounds
      draft_spot <- input$draft_spot %||% 1
      total_drafted <- if (!is.null(rv$draft_data)) sum(rv$draft_data$drafted) else 0
      
      # Helper to calculate draft-by for a target row
      get_draft_by <- function(target_row) {
        calculate_draft_by_round(
          player_adp = target_row$ADP_RANK,
          draft_spot = draft_spot,
          num_teams = NUM_TEAMS,
          total_drafted = total_drafted
        )
      }
      
      div(
        class = "my-team-container",
        
        # Header - simplified, targets dropdown is separate
        div(
          class = "my-team-header",
          tags$span("My Team"),
          div(
            style = "display: flex; gap: 1rem; font-size: 0.85rem; font-weight: 500;",
            tags$span(sprintf("%d drafted", total_picks)),
            if (total_targets > 0) tags$span(style = "color: var(--accent-coral);", sprintf("%d targets", total_targets))
          )
        ),
        
        # Body with position COLUMNS
        div(
          class = "my-team-body",
          div(
            class = "my-team-columns",
            
            # QB Column
            div(
              class = "my-team-column",
              div(
                class = "my-team-column__label",
                tags$span(class = "position-badge", "QB")
              ),
              div(
                class = "my-team-column__cards",
                # Actual picks first
                if (nrow(rv$my_picks$QB) > 0) {
                  lapply(1:nrow(rv$my_picks$QB), function(i) {
                    create_ffpc_player_card(rv$my_picks$QB[i, ], qb_team, is_target = FALSE)
                  })
                },
                # Then targets (semi-transparent) with draft-by info
                if (nrow(rv$targets$QB) > 0) {
                  lapply(1:nrow(rv$targets$QB), function(i) {
                    draft_by <- get_draft_by(rv$targets$QB[i, ])
                    create_ffpc_player_card(rv$targets$QB[i, ], qb_team, is_target = TRUE, draft_by_info = draft_by)
                  })
                },
                # Empty state only if both are empty
                if (nrow(rv$my_picks$QB) == 0 && nrow(rv$targets$QB) == 0) {
                  tags$span(class = "my-team-column__empty", "-")
                }
              )
            ),
            
            # RB Column
            div(
              class = "my-team-column",
              div(
                class = "my-team-column__label",
                tags$span(class = "position-badge", "RB")
              ),
              div(
                class = "my-team-column__cards",
                if (nrow(rv$my_picks$RB) > 0) {
                  lapply(1:nrow(rv$my_picks$RB), function(i) {
                    create_ffpc_player_card(rv$my_picks$RB[i, ], qb_team, is_target = FALSE)
                  })
                },
                if (nrow(rv$targets$RB) > 0) {
                  lapply(1:nrow(rv$targets$RB), function(i) {
                    draft_by <- get_draft_by(rv$targets$RB[i, ])
                    create_ffpc_player_card(rv$targets$RB[i, ], qb_team, is_target = TRUE, draft_by_info = draft_by)
                  })
                },
                if (nrow(rv$my_picks$RB) == 0 && nrow(rv$targets$RB) == 0) {
                  tags$span(class = "my-team-column__empty", "-")
                }
              )
            ),
            
            # WR Column
            div(
              class = "my-team-column",
              div(
                class = "my-team-column__label",
                tags$span(class = "position-badge", "WR")
              ),
              div(
                class = "my-team-column__cards",
                if (nrow(rv$my_picks$WR) > 0) {
                  lapply(1:nrow(rv$my_picks$WR), function(i) {
                    create_ffpc_player_card(rv$my_picks$WR[i, ], qb_team, is_target = FALSE)
                  })
                },
                if (nrow(rv$targets$WR) > 0) {
                  lapply(1:nrow(rv$targets$WR), function(i) {
                    draft_by <- get_draft_by(rv$targets$WR[i, ])
                    create_ffpc_player_card(rv$targets$WR[i, ], qb_team, is_target = TRUE, draft_by_info = draft_by)
                  })
                },
                if (nrow(rv$my_picks$WR) == 0 && nrow(rv$targets$WR) == 0) {
                  tags$span(class = "my-team-column__empty", "-")
                }
              )
            ),
            
            # TE Column
            div(
              class = "my-team-column",
              div(
                class = "my-team-column__label",
                tags$span(class = "position-badge", "TE")
              ),
              div(
                class = "my-team-column__cards",
                if (nrow(rv$my_picks$TE) > 0) {
                  lapply(1:nrow(rv$my_picks$TE), function(i) {
                    create_ffpc_player_card(rv$my_picks$TE[i, ], qb_team, is_target = FALSE)
                  })
                },
                if (nrow(rv$targets$TE) > 0) {
                  lapply(1:nrow(rv$targets$TE), function(i) {
                    draft_by <- get_draft_by(rv$targets$TE[i, ])
                    create_ffpc_player_card(rv$targets$TE[i, ], qb_team, is_target = TRUE, draft_by_info = draft_by)
                  })
                },
                if (nrow(rv$my_picks$TE) == 0 && nrow(rv$targets$TE) == 0) {
                  tags$span(class = "my-team-column__empty", "-")
                }
              )
            )
          )
        )
      )
    })
    
    # =========================================================================
    # DRAFT BOARD TABLE
    # =========================================================================
    output$draft_board_container <- renderUI({
      log_debug(">>> Rendering draft board container", level = "DEBUG")
      
      if (is.null(rv$draft_data)) {
        return(
          ui_card(
            color = NFL_CARD_COLOR,
            div(
              style = "text-align: center; padding: 3rem;",
              tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 3rem; color: var(--text-muted); margin-bottom: 1rem;"),
              tags$h4(style = "color: var(--text-muted);", "Loading Rankings..."),
              tags$p(class = "text-muted", "Fetching player rankings from Google Sheets")
            )
          )
        )
      }
      
      # Read reactive dependencies
      draft_spot <- input$draft_spot %||% 1
      num_teams <- NUM_TEAMS
      team_filter <- input$team_filter %||% "all"
      position_filter <- input$position_filter %||% "all"
      
      # Force re-render when targets change - MUST access rv$targets directly
      # to create proper reactive dependency
      target_qb <- rv$targets$QB
      target_rb <- rv$targets$RB  
      target_wr <- rv$targets$WR
      target_te <- rv$targets$TE
      
      # Build target_names from the accessed data
      target_names <- c(
        if (nrow(target_qb) > 0) target_qb$PLAYER else character(0),
        if (nrow(target_rb) > 0) target_rb$PLAYER else character(0),
        if (nrow(target_wr) > 0) target_wr$PLAYER else character(0),
        if (nrow(target_te) > 0) target_te$PLAYER else character(0)
      )
      
      log_debug(">>> Draft board render - target_names:", paste(target_names, collapse = ", "), level = "INFO")
      
      data <- rv$draft_data
      total_original <- nrow(data)
      available <- data[!data$drafted, ]
      
      if (nrow(available) == 0) {
        return(
          ui_card(
            title = "Draft Complete",
            color = NFL_CARD_COLOR,
            div(
              style = "text-align: center; padding: 2rem;",
              tags$h3("-"),
              actionButton(ns("reset_board"), "Start New Draft", class = "btn-primary")
            )
          )
        )
      }
      
      log_debug(">>> Building table with", nrow(available), "available players", level = "DEBUG")
      
      # Add calculated columns
      total_drafted <- sum(data$drafted)
      available <- available %>%
        mutate(
          BOARD_POSITION = row_number(),
          ACTUAL_PICK = BOARD_POSITION + total_drafted,
          ADP_PCT_VALUE = round(((ACTUAL_PICK - ADP) / ADP) * 100, 1),
          ETR_PCT_VALUE = round(((ACTUAL_PICK - ETR_RANK) / ETR_RANK) * 100, 1)
        )
      
      # Calculate pick highlighting
      players_remaining <- nrow(available)
      pick_info <- calculate_my_picks(draft_spot, num_teams, total_original, players_remaining)
      
      # PRE-COMPUTE: Build ownership percentage lookup
      own_pct_lookup <- numeric(0)
      if (!is.null(rv$draft_history) && nrow(rv$draft_history) > 0 && rv$total_drafts > 0) {
        own_pct_lookup <- setNames(
          round((rv$draft_history$times_drafted / rv$total_drafts) * 100, 0),
          rv$draft_history$player
        )
      }
      
      # Get position-specific picks (first two of each position)
      qb1_player <- NULL
      qb2_player <- NULL
      rb1_player <- NULL
      rb2_player <- NULL
      wr1_player <- NULL
      wr2_player <- NULL
      te1_player <- NULL
      te2_player <- NULL
      
      if (nrow(rv$my_picks$QB) >= 1) {
        qb_picks <- rv$my_picks$QB[order(rv$my_picks$QB$ADP), ]
        qb1_player <- qb_picks$PLAYER[1]
        if (nrow(qb_picks) >= 2) qb2_player <- qb_picks$PLAYER[2]
      }
      if (nrow(rv$my_picks$RB) >= 1) {
        rb_picks <- rv$my_picks$RB[order(rv$my_picks$RB$ADP), ]
        rb1_player <- rb_picks$PLAYER[1]
        if (nrow(rb_picks) >= 2) rb2_player <- rb_picks$PLAYER[2]
      }
      if (nrow(rv$my_picks$WR) >= 1) {
        wr_picks <- rv$my_picks$WR[order(rv$my_picks$WR$ADP), ]
        wr1_player <- wr_picks$PLAYER[1]
        if (nrow(wr_picks) >= 2) wr2_player <- wr_picks$PLAYER[2]
      }
      if (nrow(rv$my_picks$TE) >= 1) {
        te_picks <- rv$my_picks$TE[order(rv$my_picks$TE$ADP), ]
        te1_player <- te_picks$PLAYER[1]
        if (nrow(te_picks) >= 2) te2_player <- te_picks$PLAYER[2]
      }
      
      # PRE-COMPUTE: Build pairing COUNT lookups
      # Helper to build lookup for a player
      build_pair_lookup <- function(anchor_player) {
        if (is.null(anchor_player)) return(list(lookup = NULL, total = 0))
        if (is.null(rv$draft_pairs) || nrow(rv$draft_pairs) == 0) return(list(lookup = NULL, total = 0))
        if (is.null(rv$draft_history) || nrow(rv$draft_history) == 0) return(list(lookup = NULL, total = 0))
        
        # Get how many drafts included this anchor player
        anchor_row <- rv$draft_history[rv$draft_history$player == anchor_player, ]
        anchor_total <- if (nrow(anchor_row) > 0) anchor_row$times_drafted[1] else 0
        
        # Get all pairings with anchor player
        pairs <- rv$draft_pairs %>%
          filter(player_1 == anchor_player | player_2 == anchor_player) %>%
          mutate(other_player = if_else(player_1 == anchor_player, player_2, player_1))
        
        if (nrow(pairs) > 0) {
          lookup <- setNames(pairs$times_paired, pairs$other_player)
        } else {
          lookup <- NULL
        }
        
        list(lookup = lookup, total = anchor_total)
      }
      
      qb1_result <- build_pair_lookup(qb1_player)
      qb2_result <- build_pair_lookup(qb2_player)
      rb1_result <- build_pair_lookup(rb1_player)
      rb2_result <- build_pair_lookup(rb2_player)
      wr1_result <- build_pair_lookup(wr1_player)
      wr2_result <- build_pair_lookup(wr2_player)
      te1_result <- build_pair_lookup(te1_player)
      te2_result <- build_pair_lookup(te2_player)
      
      # PRE-COMPUTE: Convert pick vectors to sets for O(1) lookup
      my_picks_set <- pick_info$my_picks
      near_picks_set <- c(pick_info$light_before, pick_info$light_after)
      
      # Helper to render fraction HTML
      render_fraction <- function(lookup, total, player_name) {
        if (is.null(lookup) || total == 0) return("-")
        paired_count <- lookup[player_name]
        if (is.na(paired_count) || paired_count == 0) return("-")
        HTML(sprintf('<span class="fraction"><span class="fraction__num">%d</span><span class="fraction__slash">/</span><span class="fraction__denom">%d</span></span>', 
                     paired_count, total))
      }
      
      # Build table rows (optimized)
      table_rows <- lapply(1:nrow(available), function(i) {
        row <- available[i, ]
        
        # Fast target check using pre-computed set
        is_target <- row$PLAYER %in% target_names
        
        # Determine row highlighting (using pre-computed sets)
        is_my_pick <- i %in% my_picks_set
        is_near_pick <- i %in% near_picks_set
        is_filtered_out <- (team_filter != "all" && row$TEAM != team_filter) ||
          (position_filter != "all" && row$POSITION != position_filter)
        
        # Row style
        row_style <- if (is_my_pick) {
          "background-color: var(--accent-coral-light);"
        } else if (is_near_pick) {
          "background-color: var(--accent-yellow-light);"
        } else {
          ""
        }
        
        if (is_filtered_out) {
          row_style <- paste0(row_style, " opacity: 0.35;")
        }
        
        # Calculate pick display
        overall_pick <- total_drafted + i
        round_num <- ceiling(overall_pick / num_teams)
        pick_in_round <- ((overall_pick - 1) %% num_teams) + 1
        pick_display <- sprintf("%d.%02d", round_num, pick_in_round)
        
        # Fast ownership lookup using pre-computed named vector
        own_pct <- own_pct_lookup[row$PLAYER]
        if (is.na(own_pct)) own_pct <- 0
        
        tags$tr(
          style = row_style,
          
          # Remove button (marks player as drafted, auto-adds to my team if at my pick)
          tags$td(
            style = "text-align: center; padding: 0.5rem;",
            tags$button(
              class = paste("btn", if (is_target) "btn-target-highlight" else "btn-secondary"),
              style = paste0(
                "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem; cursor: pointer;",
                if (is_target) " background-color: #D08770 !important; color: white !important;" else ""
              ),
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", 
                                ns("remove_player"), 
                                jsonlite::toJSON(list(
                                  player = row$PLAYER,
                                  position = row$POSITION,
                                  team = row$TEAM,
                                  headshot_url = row$headshot_url,
                                  team_bg_color = row$team_bg_color,
                                  board_position = i
                                ), auto_unbox = TRUE)),
              icon("times")
            )
          ),
          
          # Pick
          tags$td(
            style = paste0("text-align: center; font-weight: 600; padding: 0.5rem;",
                           if (is_my_pick) " font-weight: 800;" else ""),
            pick_display
          ),
          
          # Combined ADP column
          tags$td(
            style = "text-align: center; padding: 0.5rem;",
            div(
              div(
                style = "font-weight: 700; font-size: 1.1rem; line-height: 1.2;",
                sprintf("%.1f", row$ADP)
              ),
              div(
                style = "font-size: 0.75rem; color: var(--text-muted); line-height: 1.2;",
                sprintf("ADP: %d | ETR: %d", row$ADP_RANK, row$ETR_RANK)
              )
            )
          ),
          
          # Player with headshot, team, and position
          tags$td(
            style = "padding: 0.5rem;",
            div(
              style = "display: flex; align-items: center; gap: 0.75rem;",
              create_headshot_html(row$headshot_url, row$team_bg_color, "small", row$POSITION, row$TEAM),
              div(
                div(
                  style = "font-weight: 700; font-size: 1rem; line-height: 1.2;",
                  row$PLAYER
                ),
                div(
                  style = "font-size: 0.75rem; color: var(--text-muted); line-height: 1.2;",
                  sprintf("%s ", row$TEAM),
                  tags$span(
                    style = "font-weight: 700;",
                    row$POSITION
                  )
                )
              )
            )
          ),
          
          # Own% from draft history
          tags$td(
            style = "text-align: center; font-weight: 600; padding: 0.5rem;",
            if (own_pct > 0) sprintf("%d%%", own_pct) else "-"
          ),
          
          # Fixed pairing columns: QB1, QB2, RB1, RB2, WR1, WR2, TE1, TE2
          tags$td(style = "text-align: center; padding: 0.5rem;", 
                  render_fraction(qb1_result$lookup, qb1_result$total, row$PLAYER)),
          tags$td(style = "text-align: center; padding: 0.5rem;", 
                  render_fraction(qb2_result$lookup, qb2_result$total, row$PLAYER)),
          tags$td(style = "text-align: center; padding: 0.5rem;", 
                  render_fraction(rb1_result$lookup, rb1_result$total, row$PLAYER)),
          tags$td(style = "text-align: center; padding: 0.5rem;", 
                  render_fraction(rb2_result$lookup, rb2_result$total, row$PLAYER)),
          tags$td(style = "text-align: center; padding: 0.5rem;", 
                  render_fraction(wr1_result$lookup, wr1_result$total, row$PLAYER)),
          tags$td(style = "text-align: center; padding: 0.5rem;", 
                  render_fraction(wr2_result$lookup, wr2_result$total, row$PLAYER)),
          tags$td(style = "text-align: center; padding: 0.5rem;", 
                  render_fraction(te1_result$lookup, te1_result$total, row$PLAYER)),
          tags$td(style = "text-align: center; padding: 0.5rem;", 
                  render_fraction(te2_result$lookup, te2_result$total, row$PLAYER)),
          
          # ADP % Value
          tags$td(
            style = sprintf("text-align: center; font-weight: 600; padding: 0.5rem; 
                    background-color: %s; border-left: 2px solid var(--bg-white);",
                            get_value_heatmap_color(row$ADP_PCT_VALUE, available$ADP_PCT_VALUE)),
            sprintf("%+.1f%%", row$ADP_PCT_VALUE)
          ),
          
          # ETR % Value
          tags$td(
            style = sprintf("text-align: center; font-weight: 600; padding: 0.5rem; 
                    background-color: %s; border-left: 2px solid var(--bg-white);",
                            get_value_heatmap_color(row$ETR_PCT_VALUE, available$ETR_PCT_VALUE)),
            sprintf("%+.1f%%", row$ETR_PCT_VALUE)
          )
        )
      })
      
      # Build complete table
      ui_card(
        title = sprintf("Draft Board (%d players remaining)", nrow(available)),
        color = NFL_CARD_COLOR,
        
        div(
          style = "overflow-x: auto;",
          tags$table(
            class = "draft-board-table",
            style = "width: 100%; border-collapse: collapse;",
            
            # Header
            tags$thead(
              tags$tr(
                style = "background-color: var(--accent-teal); color: var(--text-primary);",
                tags$th(style = "width: 50px; padding: 0.75rem; text-align: center;", ""),
                tags$th(style = "width: 70px; padding: 0.75rem; text-align: center;", "Pick"),
                tags$th(style = "width: 130px; padding: 0.75rem; text-align: center;", "ADP"),
                tags$th(style = "min-width: 200px; padding: 0.75rem; text-align: left;", "Player"),
                tags$th(style = "width: 50px; padding: 0.75rem; text-align: center; font-size: 0.75rem;", "Own%"),
                # Fixed pairing column headers: QB1, QB2, RB1, RB2, WR1, WR2, TE1, TE2
                tags$th(style = "width: 48px; padding: 0.5rem; text-align: center; font-size: 0.7rem;", "w/QB1"),
                tags$th(style = "width: 48px; padding: 0.5rem; text-align: center; font-size: 0.7rem;", "w/QB2"),
                tags$th(style = "width: 48px; padding: 0.5rem; text-align: center; font-size: 0.7rem;", "w/RB1"),
                tags$th(style = "width: 48px; padding: 0.5rem; text-align: center; font-size: 0.7rem;", "w/RB2"),
                tags$th(style = "width: 48px; padding: 0.5rem; text-align: center; font-size: 0.7rem;", "w/WR1"),
                tags$th(style = "width: 48px; padding: 0.5rem; text-align: center; font-size: 0.7rem;", "w/WR2"),
                tags$th(style = "width: 48px; padding: 0.5rem; text-align: center; font-size: 0.7rem;", "w/TE1"),
                tags$th(style = "width: 48px; padding: 0.5rem; text-align: center; font-size: 0.7rem;", "w/TE2"),
                tags$th(style = "width: 80px; padding: 0.75rem; text-align: center; background-color: var(--accent-teal-dark);", "ADP Value"),
                tags$th(style = "width: 80px; padding: 0.75rem; text-align: center; background-color: var(--accent-teal-dark);", "ETR Value")
              )
            ),
            
            # Body
            tags$tbody(table_rows)
          )
        )
      )
    })
    
    # =========================================================================
    # REMOVE PLAYER
    # Auto-adds to my team if removed at a "my pick" position
    # Also removes from targets if the player was targeted
    # =========================================================================
    observeEvent(input$remove_player, {
      req(rv$draft_data, input$remove_player, input$draft_spot)
      
      pick_data <- input$remove_player
      player_name <- pick_data$player
      board_position <- pick_data$board_position
      position <- pick_data$position
      
      log_debug(">>> Removing player:", player_name, "at board position:", board_position, level = "INFO")
      
      # Get current draft state
      data <- rv$draft_data
      total_original <- nrow(data)
      players_remaining <- sum(!data$drafted)
      draft_spot <- input$draft_spot
      num_teams <- NUM_TEAMS
      
      # Calculate my pick positions
      pick_info <- calculate_my_picks(draft_spot, num_teams, total_original, players_remaining)
      is_my_pick <- board_position %in% pick_info$my_picks
      
      log_debug(">>> My pick positions:", paste(pick_info$my_picks, collapse = ","), level = "DEBUG")
      log_debug(">>> Is my pick:", is_my_pick, level = "DEBUG")
      
      # If this is at my pick position and it's a trackable position, add to my team
      if (is_my_pick && position %in% c("QB", "RB", "WR", "TE")) {
        player_row <- data[data$PLAYER == player_name, ]
        
        if (nrow(player_row) > 0) {
          # Check if already added (shouldn't happen but safety check)
          if (!(player_name %in% rv$my_picks[[position]]$PLAYER)) {
            rv$my_picks[[position]] <- rbind(rv$my_picks[[position]], player_row)
            log_debug(">>> Added to my team:", player_name, "Position:", position, level = "INFO")
            showNotification(paste(player_name, "added to your team!"), type = "message", duration = 2)
          }
        }
      }
      
      # ALWAYS remove from targets if this player was targeted (regardless of whose pick)
      if (position %in% c("QB", "RB", "WR", "TE") && 
          nrow(rv$targets[[position]]) > 0 &&
          player_name %in% rv$targets[[position]]$PLAYER) {
        rv$targets[[position]] <- rv$targets[[position]][rv$targets[[position]]$PLAYER != player_name, ]
        log_debug(">>> Removed from targets (now drafted by someone):", player_name, level = "INFO")
        showNotification(paste(player_name, "removed from targets (drafted)"), type = "warning", duration = 2)
      }
      
      # Mark as drafted
      data$drafted[data$PLAYER == player_name] <- TRUE
      rv$drafted_players <- c(rv$drafted_players, player_name)
      rv$draft_data <- data
      
      log_debug(">>> Players remaining:", sum(!data$drafted), level = "DEBUG")
    })
    
    # =========================================================================
    # REMOVE MY PICK
    # =========================================================================
    observeEvent(input$remove_my_pick, {
      req(input$remove_my_pick)
      
      player_name <- input$remove_my_pick
      log_debug(">>> Removing from my picks:", player_name, level = "INFO")
      
      # Find which position group the player is in
      for (pos in c("QB", "RB", "WR", "TE")) {
        if (nrow(rv$my_picks[[pos]]) > 0 && player_name %in% rv$my_picks[[pos]]$PLAYER) {
          rv$my_picks[[pos]] <- rv$my_picks[[pos]][rv$my_picks[[pos]]$PLAYER != player_name, ]
          log_debug(">>> Removed from", pos, level = "INFO")
          
          # Optional: Add back to draft board (uncomment if wanted)
          # data <- rv$draft_data
          # data$drafted[data$PLAYER == player_name] <- FALSE
          # rv$drafted_players <- setdiff(rv$drafted_players, player_name)
          # rv$draft_data <- data
          
          showNotification(paste(player_name, "removed from your team"), type = "message", duration = 2)
          return()
        }
      }
    })
    
    # =========================================================================
    # UPDATE TARGET DROPDOWN CHOICES
    # Populates choices when draft data changes, preserves current selection
    # =========================================================================
    observe({
      req(rv$draft_data)
      
      # Get available players for targets dropdown (undrafted QB/RB/WR/TE only)
      available <- rv$draft_data %>%
        filter(!drafted, POSITION %in% c("QB", "RB", "WR", "TE")) %>%
        arrange(ADP_RANK)
      
      if (nrow(available) > 0) {
        # Create named choices: "Name (POS - TEAM)"
        target_choices <- setNames(
          available$PLAYER,
          sprintf("%s (%s - %s)", available$PLAYER, available$POSITION, available$TEAM)
        )
        
        # Preserve current selection if valid
        current_selection <- isolate(input$add_target)
        valid_selection <- if (!is.null(current_selection)) {
          current_selection[current_selection %in% available$PLAYER]
        } else {
          character(0)
        }
        
        updateSelectizeInput(
          session, 
          "add_target", 
          choices = target_choices,
          selected = valid_selection,
          server = FALSE
        )
      }
    })
    
    # =========================================================================
    # ADD TARGETS - Triggered by confirm_targets button
    # =========================================================================
    observeEvent(input$confirm_targets, {
      # Get selected players from the selectizeInput
      player_names <- input$add_target
      
      # Ignore empty selections
      if (is.null(player_names) || length(player_names) == 0 || all(player_names == "")) {
        showNotification("Select players first", type = "warning", duration = 2)
        return()
      }
      
      req(rv$draft_data)
      
      # Handle multiple selections
      added_count <- 0
      
      for (player_name in player_names) {
        if (player_name == "" || player_name == "Select a target...") next
        
        log_debug(">>> Adding target:", player_name, level = "INFO")
        
        # Get player row from draft data
        data <- rv$draft_data
        player_row <- data[data$PLAYER == player_name, ]
        
        if (nrow(player_row) == 0) {
          log_debug(">>> Target player not found:", player_name, level = "WARN")
          next
        }
        
        position <- player_row$POSITION[1]
        
        # Check if it's a trackable position
        if (!(position %in% c("QB", "RB", "WR", "TE"))) {
          next
        }
        
        # Check if already a target or already drafted to my team
        if (player_name %in% rv$targets[[position]]$PLAYER) {
          next
        }
        
        if (player_name %in% rv$my_picks[[position]]$PLAYER) {
          next
        }
        
        # Add to targets
        rv$targets[[position]] <- rbind(rv$targets[[position]], player_row)
        log_debug(">>> Added target:", player_name, "Position:", position, level = "INFO")
        added_count <- added_count + 1
      }
      
      if (added_count > 0) {
        showNotification(sprintf("%d target(s) added", added_count), type = "message", duration = 2)
      }
      
      # Clear the selection after adding (always, even if none added)
      updateSelectizeInput(session, "add_target", selected = character(0))
    })
    
    # =========================================================================
    # REMOVE TARGET
    # =========================================================================
    observeEvent(input$remove_target, {
      req(input$remove_target)
      
      player_name <- input$remove_target
      log_debug(">>> Removing target:", player_name, level = "INFO")
      
      # Find which position group the target is in
      for (pos in c("QB", "RB", "WR", "TE")) {
        if (nrow(rv$targets[[pos]]) > 0 && player_name %in% rv$targets[[pos]]$PLAYER) {
          rv$targets[[pos]] <- rv$targets[[pos]][rv$targets[[pos]]$PLAYER != player_name, ]
          log_debug(">>> Removed target from", pos, level = "INFO")
          showNotification(paste(player_name, "removed from targets"), type = "message", duration = 2)
          return()
        }
      }
    })
    
    # =========================================================================
    # RESET BOARD
    # =========================================================================
    observeEvent(input$reset_board, {
      req(rv$draft_data)
      
      log_debug(">>> Resetting draft board", level = "INFO")
      
      data <- rv$draft_data
      data$drafted <- FALSE
      rv$draft_data <- data
      rv$drafted_players <- character(0)
      
      # Reset my picks
      rv$my_picks <- list(
        QB = data.frame(),
        RB = data.frame(),
        WR = data.frame(),
        TE = data.frame()
      )
      
      # Reset targets
      rv$targets <- list(
        QB = data.frame(),
        RB = data.frame(),
        WR = data.frame(),
        TE = data.frame()
      )
      
      showNotification("Draft board reset!", type = "message", duration = 2)
    })
    
  })
}