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
    
    # Controls row
    fluidRow(
      # Import Rankings Card
      column(4,
             ui_card(
               title = "Import Rankings",
               color = NFL_CARD_COLOR,
               
               fileInput(ns("rankings_file"), "Select Rankings CSV",
                         accept = c(".csv"),
                         placeholder = "No file selected"),
               tags$p(
                 class = "text-muted",
                 style = "font-size: 0.85rem; margin-top: 0.5rem;",
                 "Expected columns: Name, Position, Team, ADP, ETR_Rank"
               )
             )
      ),
      
      # Draft Settings Card
      column(4,
             ui_card(
               title = "Draft Settings",
               color = NFL_CARD_COLOR,
               
               fluidRow(
                 column(6,
                        selectInput(ns("competition"), "Competition",
                                    choices = c("Superflex" = "superflex", "Classic" = "classic"),
                                    selected = "superflex")
                 ),
                 column(6,
                        numericInput(ns("num_teams"), "Number of Teams",
                                     value = 12, min = 8, max = 14, step = 1)
                 )
               ),
               fluidRow(
                 column(6,
                        numericInput(ns("draft_spot"), "My Draft Spot",
                                     value = 1, min = 1, max = 12, step = 1)
                 ),
                 column(6,
                        # Placeholder for future settings
                 )
               )
             )
      ),
      
      # Filters Card
      column(4,
             ui_card(
               title = "Filters",
               color = NFL_CARD_COLOR,
               
               fluidRow(
                 column(6,
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
                 column(6,
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
        comp_label <- if (comp == "superflex") "Superflex" else "Classic"
        filtered <- raw_data %>% 
          filter(tolower(competition) == tolower(comp_label))
        
        log_debug(">>> Filtered to", nrow(filtered), "drafts for", comp_label, level = "INFO")
        
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
        log_debug(">>> Draft history loaded successfully for", comp_label, level = "INFO")
        
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
    # LOAD RANKINGS FILE
    # =========================================================================
    observeEvent(input$rankings_file, {
      req(input$rankings_file)
      
      log_debug(">>> Rankings file upload triggered", level = "INFO")
      log_debug(">>>   File name:", input$rankings_file$name, level = "INFO")
      log_debug(">>>   File size:", input$rankings_file$size, "bytes", level = "INFO")
      
      tryCatch({
        raw_data <- read.csv(input$rankings_file$datapath, stringsAsFactors = FALSE)
        log_debug(">>> Raw data loaded:", nrow(raw_data), "rows,", ncol(raw_data), "columns", level = "INFO")
        log_debug(">>> Columns found:", paste(names(raw_data), collapse = ", "), level = "DEBUG")
        
        # Validate required columns
        required_cols <- c("Name", "Position", "Team", "ADP")
        missing_cols <- setdiff(required_cols, names(raw_data))
        
        if (length(missing_cols) > 0) {
          log_debug(">>> Missing required columns:", paste(missing_cols, collapse = ", "), level = "ERROR")
          showNotification(
            paste("Missing columns:", paste(missing_cols, collapse = ", ")),
            type = "error", duration = 10
          )
          return()
        }
        
        # Process rankings
        rankings <- raw_data %>%
          arrange(ADP) %>%
          mutate(
            REMOVE = "Ã¢Å“â€¢",
            ADP_RANK = row_number(),
            PLAYER = Name,
            POSITION = Position,
            TEAM = Team,
            ETR_RANK = if ("ETR_Rank" %in% names(.)) ETR_Rank else ADP_RANK
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
        
        # Update team filter choices
        teams <- sort(unique(rankings$TEAM[rankings$TEAM != "" & !is.na(rankings$TEAM)]))
        updateSelectInput(session, "team_filter",
                          choices = c("All Teams" = "all", setNames(teams, teams)))
        
        log_debug(">>> Draft board initialized successfully", level = "INFO")
        showNotification("Rankings loaded! Click x to remove drafted players.", 
                         type = "message", duration = 5)
        
      }, error = function(e) {
        log_debug(">>> Error loading rankings:", e$message, level = "ERROR")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
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
    # STATS OUTPUTS
    # =========================================================================
    output$current_pick <- renderText({
      req(rv$draft_data)
      data <- rv$draft_data
      total_original <- nrow(data)
      remaining <- sum(!data$drafted)
      current_pick <- total_original - remaining + 1
      as.character(current_pick)
    })
    
    output$players_remaining <- renderText({
      req(rv$draft_data)
      data <- rv$draft_data
      remaining <- sum(!data$drafted)
      as.character(remaining)
    })
    
    output$my_next_pick <- renderText({
      req(rv$draft_data, input$draft_spot, input$num_teams)
      
      data <- rv$draft_data
      total_original <- nrow(data)
      remaining <- sum(!data$drafted)
      
      pick_info <- calculate_my_picks(input$draft_spot, input$num_teams, total_original, remaining)
      
      if (length(pick_info$my_picks) > 0) {
        # Convert board position to overall pick number
        total_drafted <- total_original - remaining
        overall_pick <- total_drafted + pick_info$my_picks[1]
        round_num <- ceiling(overall_pick / input$num_teams)
        pick_in_round <- ((overall_pick - 1) %% input$num_teams) + 1
        sprintf("%d.%02d (#%d on board)", round_num, pick_in_round, pick_info$my_picks[1])
      } else {
        "-"
      }
    })
    
    output$drafts_tracked <- renderText({
      as.character(rv$total_drafts %||% 0)
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
              tags$i(class = "fa fa-upload", style = "font-size: 3rem; color: var(--text-muted); margin-bottom: 1rem;"),
              tags$h4(style = "color: var(--text-muted);", "No Rankings Loaded"),
              tags$p(class = "text-muted", "Upload a CSV file with your player rankings to get started")
            )
          )
        )
      }
      
      # Read reactive dependencies
      draft_spot <- input$draft_spot %||% 1
      num_teams <- input$num_teams %||% 12
      team_filter <- input$team_filter %||% "all"
      position_filter <- input$position_filter %||% "all"
      
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
              tags$h3("Ã°Å¸Å½â€° All players have been drafted!"),
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
      
      # Build table rows
      table_rows <- lapply(1:nrow(available), function(i) {
        row <- available[i, ]
        
        # Determine row highlighting
        is_my_pick <- i %in% pick_info$my_picks
        is_near_pick <- i %in% pick_info$light_before || i %in% pick_info$light_after
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
        
        # Look up ownership percentage from draft history
        own_pct <- 0
        if (!is.null(rv$draft_history) && nrow(rv$draft_history) > 0 && rv$total_drafts > 0) {
          player_history <- rv$draft_history %>% filter(player == row$PLAYER)
          if (nrow(player_history) > 0) {
            own_pct <- round((player_history$times_drafted[1] / rv$total_drafts) * 100, 0)
          }
        }
        
        tags$tr(
          style = row_style,
          
          # Remove button
          tags$td(
            style = "text-align: center; padding: 0.5rem;",
            tags$button(
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem; cursor: pointer;",
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", 
                                ns("remove_player"), 
                                jsonlite::toJSON(row$PLAYER, auto_unbox = TRUE)),
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
          
          # Player with headshot
          tags$td(
            style = "padding: 0.5rem;",
            div(
              style = "display: flex; align-items: center; gap: 0.75rem;",
              create_headshot_html(row$headshot_url, row$team_bg_color, "small", row$POSITION, row$TEAM),
              div(
                style = "font-weight: 600;",
                row$PLAYER
              )
            )
          ),
          
          # Team
          tags$td(
            style = paste0("text-align: center; font-weight: 600; padding: 0.5rem;",
                           if (team_filter != "all" && row$TEAM == team_filter) 
                             " background-color: var(--accent-sky);" else ""),
            row$TEAM
          ),
          
          # Position
          tags$td(
            style = "text-align: center; padding: 0.5rem;",
            span(class = "position-badge", row$POSITION)
          ),
          
          # Own% from draft history
          tags$td(
            style = "text-align: center; font-weight: 600; padding: 0.5rem;",
            if (own_pct > 0) sprintf("%d%%", own_pct) else "-"
          ),
          
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
                tags$th(style = "width: 70px; padding: 0.75rem; text-align: center;", "Team"),
                tags$th(style = "width: 80px; padding: 0.75rem; text-align: center;", "Pos"),
                tags$th(style = "width: 70px; padding: 0.75rem; text-align: center;", "Own%"),
                tags$th(style = "width: 100px; padding: 0.75rem; text-align: center; background-color: var(--accent-teal-dark);", "ADP Value"),
                tags$th(style = "width: 100px; padding: 0.75rem; text-align: center; background-color: var(--accent-teal-dark);", "ETR Value")
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
    # =========================================================================
    observeEvent(input$remove_player, {
      req(rv$draft_data, input$remove_player)
      
      player_name <- input$remove_player
      log_debug(">>> Removing player:", player_name, level = "INFO")
      
      data <- rv$draft_data
      data$drafted[data$PLAYER == player_name] <- TRUE
      rv$drafted_players <- c(rv$drafted_players, player_name)
      rv$draft_data <- data
      
      log_debug(">>> Players remaining:", sum(!data$drafted), level = "DEBUG")
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
      
      showNotification("Draft board reset!", type = "message", duration = 2)
    })
    
  })
}