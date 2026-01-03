# =============================================================================
# Module: NFL Handbuild
# 
# Manual lineup building with optimization support
# Features:
#   (a) Calculate optimal lineup for a slate (standard + adjusted)
#   (b) Partially build a lineup by locking players
#   (c) Generate multiple optimized lineups with stacking rules
#   (d) Compare each to the true optimal
# =============================================================================

library(lpSolve)

#' NFL Handbuild UI
#' @param id Module namespace ID
nfl_handbuild_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("nfl_handbuild_ui() called with id:", id, level = "INFO")
  
  # Get available seasons at UI build time
  seasons <- get_available_seasons()
  
  if (length(seasons) > 0) {
    weeks <- get_available_weeks(seasons[1])
    slates <- get_available_slates(seasons[1], weeks[1])
  } else {
    weeks <- c()
    slates <- c("main")
  }
  
  # Build choices
  season_choices <- if (length(seasons) > 0) {
    setNames(as.character(seasons), as.character(seasons))
  } else {
    c("No data" = "")
  }
  
  week_choices <- if (length(weeks) > 0) {
    setNames(as.character(weeks), paste("Week", weeks))
  } else {
    c("No weeks" = "")
  }
  
  # Build slate choices with proper labels for all available slates
  slate_choices <- if (length(slates) > 0) {
    setNames(slates, sapply(slates, get_slate_label))
  } else {
    c("Main" = "main")
  }
  
  tagList(
    # Enable shinyjs for button state management
    shinyjs::useShinyjs(),
    
    # Page header
    div(
      class = "page-header",
      tags$h2("NFL Handbuild"),
      tags$p(class = "text-muted", "Build lineups manually with optimization assistance")
    ),
    
    # CSS for position filter buttons
    tags$style(HTML("
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
    
    # ==========================================================================
    # FILTERS CARD
    # ==========================================================================
    ui_card(
      title = "Settings",
      color = NFL_CARD_COLOR,
      
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
               selectInput(ns("slate"), "Slate",
                           choices = slate_choices,
                           selected = slates[1]
               )
        ),
        column(3,
               numericInput(ns("salary_cap"), "Salary Cap",
                            value = 130, min = 50, max = 200, step = 0.1
               )
        )
      ),
      
      # Unmatched players alert
      uiOutput(ns("unmatched_alert"))
    ),
    
    tags$br(),
    
    # ==========================================================================
    # PROJECTION ADJUSTMENTS (Boost/Dock as %)
    # ==========================================================================
    ui_card(
      title = "Projection Adjustments",
      color = NFL_CARD_COLOR,
      
      div(
        style = "margin-bottom: 0.75rem; font-size: 0.85rem; color: var(--text-muted);",
        "Boost or dock player projections as a percentage. Applied to the blended projection."
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
                 actionButton(ns("apply_adj"), "Apply",
                              class = "btn-primary",
                              style = "flex: 1;"
                 ),
                 actionButton(ns("clear_adj"), "Clear All",
                              class = "btn-secondary",
                              style = "flex: 1;"
                 )
               )
        )
      ),
      
      # Show current adjustments
      uiOutput(ns("adjustments_display"))
    ),
    
    tags$br(),
    
    # ==========================================================================
    # OPTIMAL LINEUPS (Full Width - Shows both standard and adjusted)
    # ==========================================================================
    fluidRow(
      column(12,
             ui_card(
               title = "Optimal Lineups",
               color = NFL_CARD_COLOR,
               
               div(
                 style = "margin-bottom: 1rem;",
                 actionButton(ns("calc_optimal"), "Calculate Optimal Lineups",
                              class = "btn-primary",
                              style = "width: 100%;"
                 )
               ),
               
               uiOutput(ns("optimal_comparison"))
             )
      )
    ),
    
    tags$br(),
    
    # ==========================================================================
    # BUILD YOUR LINEUP (Full Width - Below Optimal)
    # ==========================================================================
    fluidRow(
      column(12,
             ui_card(
               title = "Build Your Lineup",
               color = NFL_CARD_COLOR,
               
               # Row with Player Pool and Lineup side by side
               fluidRow(
                 # Left side: Player Pool
                 column(7,
                        # Position filter tabs
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
                        # Team filter + heatmap toggle row
                        div(
                          style = "display: flex; gap: 0.75rem; margin-bottom: 0.75rem; align-items: center;",
                          div(
                            class = "pool-team-filter",
                            style = "width: 180px;",
                            selectizeInput(
                              ns("filter_team"),
                              label = NULL,
                              choices = c("All Teams" = "all"),
                              selected = "all",
                              options = list(
                                placeholder = "All Teams",
                                render = I("{
                                  option: function(item, escape) {
                                    if (item.value === 'all') {
                                      return '<div class=\"option\" style=\"padding: 6px 8px; display: flex; align-items: center;\">' +
                                        '<span style=\"font-weight: 600;\">All Teams</span></div>';
                                    }
                                    return '<div class=\"option\" style=\"padding: 4px 8px; display: flex; align-items: center; gap: 8px;\">' +
                                      '<img src=\"' + escape(item.logo) + '\" style=\"width: 20px; height: 20px; object-fit: contain;\">' +
                                      '<span style=\"font-weight: 600;\">' + escape(item.label) + '</span></div>';
                                  },
                                  item: function(item, escape) {
                                    if (item.value === 'all') {
                                      return '<div style=\"display: flex; align-items: center;\">' +
                                        '<span style=\"font-weight: 600;\">All Teams</span></div>';
                                    }
                                    return '<div style=\"display: flex; align-items: center; gap: 6px;\">' +
                                      '<img src=\"' + escape(item.logo) + '\" style=\"width: 18px; height: 18px; object-fit: contain;\">' +
                                      '<span style=\"font-weight: 600;\">' + escape(item.label) + '</span></div>';
                                  }
                                }")
                              )
                            )
                          ),
                          div(style = "flex: 1;"),
                          tags$label(
                            class = "pool-heatmap-label",
                            style = "display: flex; align-items: center; gap: 6px; cursor: pointer; user-select: none; font-size: 0.8rem; font-weight: 600; color: var(--text-secondary);",
                            tags$input(
                              type = "checkbox",
                              id = ns("pool_heatmap"),
                              style = "width: 16px; height: 16px; accent-color: var(--accent-plum); cursor: pointer; margin: 0;"
                            ),
                            "Heatmap"
                          )
                        ),
                        # Player pool list - height matches lineup display
                        div(
                          class = "player-pool-container",
                          style = "height: 485px; overflow-y: auto; border: 2px solid var(--outline); border-radius: 8px; background: var(--bg-white);",
                          uiOutput(ns("player_pool_header")),
                          uiOutput(ns("player_pool"))
                        )
                 ),
                 
                 # Right side: Lineup display only (no generate variations here)
                 column(5,
                        # Current lineup display
                        uiOutput(ns("lineup_display"))
                 )
               ),
               
               # =======================================================
               # GENERATE VARIATIONS - Full width below
               # =======================================================
               div(
                 style = "margin-top: 1rem; padding: 0.75rem; background: var(--bg-tertiary); border-radius: 8px; border: 1px solid var(--outline);",
                 
                 # Section header
                 div(
                   style = "font-weight: 700; font-size: 0.85rem; color: var(--text-primary); margin-bottom: 0.5rem;",
                   "Generate Variations"
                 ),
                 
                 # Two column layout: Instructions left, Settings right
                 fluidRow(
                   column(6,
                          # Completion instructions
                          textAreaInput(
                            ns("completion_instructions"),
                            label = NULL,
                            value = "",
                            placeholder = "Describe how to complete lineups (e.g., stack WRs with QB, bring-back from opponent...)",
                            rows = 2
                          )
                   ),
                   column(6,
                          # Settings row: # Lineups, Variance, Game Stack
                          div(
                            style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 0.5rem;",
                            div(
                              style = "font-size: 0.75rem;",
                              numericInput(ns("num_lineups"), "# Lineups", value = 10, min = 1, max = 50, step = 1)
                            ),
                            div(
                              style = "font-size: 0.75rem;",
                              numericInput(ns("variance_pct"), "Variance %", value = 15, min = 0, max = 50, step = 5)
                            ),
                            div(
                              style = "font-size: 0.75rem;",
                              selectInput(ns("stack_game"), "Game Stack", choices = c("None" = ""))
                            )
                          )
                   )
                 ),
                 
                 # Collapsible advanced stacking rules
                 tags$details(
                   style = "margin-top: 0.5rem;",
                   tags$summary(
                     style = "font-size: 0.75rem; font-weight: 600; color: var(--text-muted); cursor: pointer; user-select: none;",
                     "Advanced Stacking Rules"
                   ),
                   div(
                     style = "padding: 0.5rem 0;",
                     fluidRow(
                       column(6,
                              # QB selection + Add rule
                              div(
                                style = "display: grid; grid-template-columns: 2fr 1fr; gap: 0.5rem; margin-bottom: 0.5rem;",
                                selectizeInput(ns("rule_qbs"), "If QB is...",
                                               choices = c("Loading..." = ""),
                                               multiple = TRUE,
                                               options = list(placeholder = "Select QB(s)")
                                ),
                                div(
                                  style = "padding-top: 1.5rem;",
                                  actionButton(ns("add_rule"), "Add", class = "btn-primary btn-sm", style = "width: 100%;")
                                )
                              )
                       ),
                       column(3,
                              # Same team stack
                              div(style = "font-size: 0.7rem; font-weight: 600; color: var(--text-muted);", "Same Team"),
                              div(
                                style = "display: flex; align-items: center; gap: 0.25rem;",
                                numericInput(ns("rule_same_min"), NULL, value = 1, min = 0, max = 4, step = 1, width = "50px"),
                                checkboxGroupInput(ns("rule_same_pos"), NULL, choices = c("RB", "WR", "TE"), selected = c("WR", "TE"), inline = TRUE)
                              )
                       ),
                       column(3,
                              # Opponent stack
                              div(style = "font-size: 0.7rem; font-weight: 600; color: var(--text-muted);", "Opponent"),
                              div(
                                style = "display: flex; align-items: center; gap: 0.25rem;",
                                numericInput(ns("rule_opp_min"), NULL, value = 0, min = 0, max = 3, step = 1, width = "50px"),
                                checkboxGroupInput(ns("rule_opp_pos"), NULL, choices = c("RB", "WR", "TE"), selected = c("WR"), inline = TRUE)
                              )
                       )
                     ),
                     # Min game players + current rules
                     div(
                       style = "display: flex; align-items: center; gap: 0.5rem; margin-top: 0.5rem;",
                       span(style = "font-size: 0.7rem; color: var(--text-muted);", "Min players from game:"),
                       numericInput(ns("min_game_players"), NULL, value = 4, min = 2, max = 6, step = 1, width = "60px"),
                       div(style = "flex: 1;"),
                       actionButton(ns("clear_rules"), "Clear Rules", class = "btn-secondary btn-sm")
                     ),
                     uiOutput(ns("stacking_rules_display"))
                   )
                 ),
                 
                 # Action buttons - full width row
                 div(
                   style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 0.5rem; margin-top: 0.75rem;",
                   actionButton(ns("autocomplete"), "Autocomplete", class = "btn-secondary", style = "font-size: 0.85rem;"),
                   actionButton(ns("clear_all"), "Clear", class = "btn-secondary", style = "font-size: 0.85rem;"),
                   actionButton(ns("generate"), "Generate", class = "btn-primary", style = "font-size: 0.85rem; font-weight: 700;")
                 )
               )
             )
      )
    ),
    
    tags$br(),
    
    # ==========================================================================
    # GENERATED LINEUPS (Full Width)
    # ==========================================================================
    fluidRow(
      column(12,
             uiOutput(ns("results_panel"))
      )
    )
  )
}


#' NFL Handbuild Server
#' @param id Module namespace ID
nfl_handbuild_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("nfl_handbuild_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    rv <- reactiveValues(
      player_data = NULL,           # All players for current week/slate
      optimal_lineup = NULL,        # The true optimal lineup (no adjustments)
      adjusted_optimal_lineup = NULL, # Optimal with adjustments applied
      optimal_projection = 0,       # Total optimal projection
      adjusted_optimal_projection = 0, # Total adjusted optimal projection
      lineup_slots = list(          # Current manual lineup
        QB = NULL, RB1 = NULL, RB2 = NULL,
        WR1 = NULL, WR2 = NULL, WR3 = NULL,
        TE = NULL, FLEX = NULL, DST = NULL
      ),
      generated_lineups = NULL,     # Generated lineup variations
      projection_adjustments = list(),  # Player projection adjustments (name -> pct)
      stacking_rules = list(),      # Conditional stacking rules
      available_games = NULL,       # Games for game stack selection
      position_filter = "all",      # Current position filter for player pool
      team_filter = "all",          # Current team filter for player pool
      pool_sort = list(col = "value", dir = "desc"),  # Player pool sort state
      completion_instructions = "", # User instructions for lineup generation
      unmatched_players = NULL,     # Players with projections but no salary data
      loading = FALSE,
      initialized = FALSE
    )
    
    # =========================================================================
    # UPDATE SLATE CHOICES WHEN WEEK CHANGES
    # =========================================================================
    
    observeEvent(c(input$season, input$week), {
      req(input$season, input$week)
      req(input$season != "", input$week != "")
      req(input$season != "No data", input$week != "No weeks")
      
      log_debug(">>> Updating slates for season:", input$season, "week:", input$week, level = "INFO")
      
      slates <- get_available_slates(input$season, as.numeric(input$week))
      
      if (length(slates) > 0) {
        slate_choices <- setNames(slates, sapply(slates, get_slate_label))
        
        # Preserve current selection if still valid
        current_slate <- isolate(input$slate)
        new_selected <- if (!is.null(current_slate) && current_slate %in% slates) {
          current_slate
        } else {
          slates[1]
        }
        
        updateSelectInput(session, "slate", choices = slate_choices, selected = new_selected)
        log_debug(">>> Updated slate choices:", paste(names(slate_choices), collapse = ", "), level = "INFO")
      }
    }, ignoreInit = FALSE)
    
    # =========================================================================
    # DATA LOADING
    # Uses retry mechanism for dynamic module timing, then req() for validation
    # =========================================================================
    
    load_attempts <- reactiveVal(0)
    
    observe({
      # Read inputs first to establish reactive dependencies
      season <- input$season
      week <- input$week
      slate <- input$slate
      
      log_debug(">>> Data load observer triggered (attempt", load_attempts() + 1, ")", level = "DEBUG")
      log_debug(">>>   season:", if(is.null(season)) "NULL" else if(season == "") "''" else paste0("'", season, "'"), level = "DEBUG")
      log_debug(">>>   week:", if(is.null(week)) "NULL" else if(week == "") "''" else paste0("'", week, "'"), level = "DEBUG")
      log_debug(">>>   slate:", if(is.null(slate)) "NULL" else if(slate == "") "''" else paste0("'", slate, "'"), level = "DEBUG")
      
      # If inputs aren't ready and we haven't tried too many times, schedule a retry
      if ((is.null(season) || season == "" || is.null(week) || week == "" || is.null(slate) || slate == "") && 
          load_attempts() < 10) {
        load_attempts(load_attempts() + 1)
        log_debug(">>> Inputs not ready, scheduling retry in 200ms", level = "DEBUG")
        invalidateLater(200, session)
        return()
      }
      
      # Reset retry counter once we have values
      if (load_attempts() > 0 && !is.null(season) && season != "") {
        log_debug(">>> Inputs ready after", load_attempts(), "retries", level = "INFO")
        load_attempts(0)
      }
      
      # Use req() to wait for valid inputs - this maintains reactive dependency
      req(season, week, slate)
      req(season != "", week != "", slate != "")
      req(season != "No data", week != "No weeks")  # Check for placeholder labels
      
      log_debug(">>> DATA LOAD TRIGGERED for:", season, "/", week, "/", slate, level = "INFO")
      
      tryCatch({
        # Use the same function as projections module
        data <- load_week_data_with_headshots(
          season, 
          as.numeric(week), 
          slate
        )
        
        if (!is.null(data) && nrow(data) > 0) {
          rv$player_data <- data
          log_debug(">>> Loaded", nrow(data), "players", level = "INFO")
          
          # Extract available games for game stack
          if ("opponent" %in% names(data) && "team" %in% names(data)) {
            games <- data %>%
              filter(!is.na(opponent), opponent != "") %>%
              mutate(
                game = if_else(home, 
                               paste0(opponent, " @ ", team),
                               paste0(team, " @ ", opponent)),
                game_key = if_else(home,
                                   paste0(pmin(team, opponent), "_", pmax(team, opponent)),
                                   paste0(pmin(team, opponent), "_", pmax(team, opponent)))
              ) %>%
              distinct(game, game_key) %>%
              arrange(game)
            
            rv$available_games <- games
            
            # Update game selector
            game_choices <- setNames(games$game_key, games$game)
            updateSelectInput(session, "stack_game", 
                              choices = c("No game requirement" = "", game_choices))
          }
          
          # Update QB selector for stacking rules
          qbs <- data %>%
            filter(position == "QB") %>%
            arrange(desc(blended)) %>%
            mutate(label = sprintf("%s (%s) - %.1f pts", player, team, blended))
          
          if (nrow(qbs) > 0) {
            qb_choices <- setNames(qbs$player, qbs$label)
            updateSelectizeInput(session, "rule_qbs", choices = qb_choices)
          }
          
          # Update team filter selector with logos
          teams <- data %>%
            distinct(team) %>%
            arrange(team) %>%
            mutate(logo = sapply(team, function(t) get_team_logo(t, "webp")))
          
          if (nrow(teams) > 0) {
            team_options <- lapply(1:nrow(teams), function(i) {
              list(value = teams$team[i], label = teams$team[i], logo = teams$logo[i])
            })
            team_options <- c(list(list(value = "all", label = "All Teams", logo = "")), team_options)
            
            updateSelectizeInput(session, "filter_team", 
                                 choices = setNames(c("all", teams$team), c("All Teams", teams$team)),
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
                                 ))
          }
          
          # Clear previous state when data changes
          rv$optimal_lineup <- NULL
          rv$adjusted_optimal_lineup <- NULL
          rv$optimal_projection <- 0
          rv$adjusted_optimal_projection <- 0
          rv$generated_lineups <- NULL
          rv$stacking_rules <- list()  # Clear stacking rules too
          
          # Reset lineup slots
          rv$lineup_slots <- list(
            QB = NULL, RB1 = NULL, RB2 = NULL,
            WR1 = NULL, WR2 = NULL, WR3 = NULL,
            TE = NULL, FLEX = NULL, DST = NULL
          )
          
          # Check for unmatched players
          unmatched <- tryCatch({
            get_unmatched_players(season, as.numeric(week), slate, min_projection = 3)
          }, error = function(e) {
            log_debug(">>> Error checking unmatched players:", e$message, level = "WARN")
            NULL
          })
          
          rv$unmatched_players <- unmatched
          if (!is.null(unmatched) && nrow(unmatched) > 0) {
            log_debug(">>> Found", nrow(unmatched), "unmatched players with projection >= 3", level = "INFO")
          }
        } else {
          log_debug(">>> No data returned", level = "WARN")
        }
        
        rv$initialized <- TRUE
        
      }, error = function(e) {
        log_debug(">>> Error loading data:", e$message, level = "ERROR")
        rv$player_data <- NULL
      })
    })
    
    # =========================================================================
    # UPDATE WEEKS when season changes
    # =========================================================================
    observeEvent(input$season, {
      log_debug(">>> Season changed to:", input$season, level = "INFO")
      
      # Skip if empty or placeholder
      if (is.null(input$season) || input$season == "" || input$season == "No data") {
        log_debug(">>> Season is empty/placeholder, skipping week update", level = "DEBUG")
        return()
      }
      
      weeks <- get_available_weeks(input$season)
      log_debug(">>> Weeks found:", paste(weeks, collapse = ", "), level = "INFO")
      
      if (length(weeks) > 0) {
        week_choices <- setNames(as.character(weeks), paste("Week", weeks))
        updateSelectInput(session, "week",
                          choices = week_choices,
                          selected = as.character(weeks[1])
        )
        log_debug(">>> Week dropdown updated, selected:", weeks[1], level = "INFO")
      } else {
        updateSelectInput(session, "week",
                          choices = c("No weeks" = ""),
                          selected = NULL
        )
        log_debug(">>> No weeks available for this season", level = "WARN")
      }
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # UPDATE SALARY CAP when slate changes
    # =========================================================================
    observeEvent(input$slate, {
      log_debug(">>> Slate changed to:", input$slate, level = "INFO")
      
      # Set salary cap based on slate: 130 for main, 120 for late
      new_cap <- if (input$slate == "main") 130 else 120
      
      updateNumericInput(session, "salary_cap", value = new_cap)
      log_debug(">>> Salary cap updated to:", new_cap, level = "INFO")
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # PROJECTION ADJUSTMENTS (as %)
    # =========================================================================
    
    # Update adjustment player dropdown based on position filter
    observe({
      req(rv$player_data)
      
      data <- rv$player_data
      adjustments <- rv$projection_adjustments
      
      # Filter by position
      if (!is.null(input$adj_position) && input$adj_position != "all") {
        data <- data %>% filter(position == input$adj_position)
      }
      
      # Sort by projection
      data <- data %>% arrange(desc(blended))
      
      if (nrow(data) == 0) {
        choices <- c("No players" = "")
      } else {
        # Show current adjustment if exists
        labels <- sapply(1:nrow(data), function(i) {
          player_name <- data$player[i]
          adj_pct <- adjustments[[player_name]]
          adj_str <- if (!is.null(adj_pct) && adj_pct != 0) {
            sprintf(" [%+.0f%%]", adj_pct)
          } else {
            ""
          }
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
      
      # Update or remove adjustment
      if (adj_pct == 0) {
        rv$projection_adjustments[[player_name]] <- NULL
      } else {
        rv$projection_adjustments[[player_name]] <- adj_pct
      }
      
      # Reset inputs
      updateSelectizeInput(session, "adj_player", selected = "")
      updateNumericInput(session, "adj_pct", value = 0)
    })
    
    # Clear all adjustments
    observeEvent(input$clear_adj, {
      log_debug(">>> Clearing all adjustments", level = "INFO")
      rv$projection_adjustments <- list()
    })
    
    # Display current adjustments
    output$adjustments_display <- renderUI({
      adjustments <- rv$projection_adjustments
      player_data <- rv$player_data
      
      if (length(adjustments) == 0) {
        return(
          div(
            style = "text-align: center; padding: 0.5rem; color: var(--text-muted); font-size: 0.85rem; font-style: italic;",
            "No adjustments applied"
          )
        )
      }
      
      # Sort by adjustment value
      adj_df <- tibble(
        player = names(adjustments),
        adj_pct = unlist(adjustments)
      ) %>%
        arrange(desc(adj_pct))
      
      # Get base projections to show adjusted values
      if (!is.null(player_data)) {
        adj_df <- adj_df %>%
          left_join(player_data %>% select(player, blended), by = "player") %>%
          mutate(
            adj_value = blended * (1 + adj_pct / 100),
            display = sprintf("%s: %.1f ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€šÃ‚Â ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â‚¬Å¾Ã‚Â¢ %.1f (%+.0f%%)", player, blended, adj_value, adj_pct)
          )
      } else {
        adj_df <- adj_df %>%
          mutate(display = sprintf("%s: %+.0f%%", player, adj_pct))
      }
      
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
            color <- if (adj_pct > 0) "var(--accent-sage)" else "var(--accent-coral)"
            
            div(
              style = sprintf("display: inline-flex; align-items: center; gap: 0.3rem; padding: 0.25rem 0.5rem; background: %s; border-radius: 4px; font-size: 0.8rem;",
                              if (adj_pct > 0) "rgba(139, 168, 134, 0.2)" else "rgba(232, 131, 121, 0.2)"
              ),
              span(style = "font-weight: 600;", adj_df$player[i]),
              span(style = sprintf("font-weight: 700; color: %s;", color), sprintf("%+.0f%%", adj_pct)),
              # Remove button
              tags$button(
                type = "button",
                style = "background: none; border: none; cursor: pointer; padding: 0 0.2rem; font-size: 0.9rem; color: var(--text-muted);",
                onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                                  ns("remove_adj"), gsub("'", "\\\\'", adj_df$player[i])),
                "x"
              )
            )
          })
        )
      )
    })
    
    # Unmatched players alert
    output$unmatched_alert <- renderUI({
      unmatched <- rv$unmatched_players
      
      if (is.null(unmatched) || nrow(unmatched) == 0) {
        return(NULL)
      }
      
      # Create collapsible alert with player list
      div(
        style = "margin-top: 1rem; padding: 0.75rem; background: rgba(232, 131, 121, 0.1); border: 1px solid var(--accent-coral); border-radius: 8px;",
        
        # Header with count
        div(
          style = "display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.5rem;",
          tags$span(style = "font-size: 1.1rem;", HTML("&#9888;")),
          tags$span(
            style = "font-weight: 700; color: var(--accent-coral); font-size: 0.85rem;",
            sprintf("%d Unmatched Player%s", nrow(unmatched), if(nrow(unmatched) > 1) "s" else "")
          ),
          tags$span(
            style = "font-size: 0.8rem; color: var(--text-muted);",
            HTML("(projection &#8805; 3pts but not in salary file)")
          )
        ),
        
        # Collapsible player list
        tags$details(
          style = "cursor: pointer;",
          tags$summary(
            style = "font-size: 0.8rem; color: var(--text-secondary); font-weight: 600; user-select: none;",
            "View players"
          ),
          div(
            style = "margin-top: 0.5rem; max-height: 200px; overflow-y: auto;",
            tags$table(
              style = "width: 100%; font-size: 0.8rem; border-collapse: collapse;",
              tags$thead(
                tags$tr(
                  style = "border-bottom: 1px solid var(--outline);",
                  tags$th(style = "text-align: left; padding: 0.25rem 0.5rem; font-weight: 600;", "Player"),
                  tags$th(style = "text-align: left; padding: 0.25rem 0.5rem; font-weight: 600;", "Team"),
                  tags$th(style = "text-align: left; padding: 0.25rem 0.5rem; font-weight: 600;", "Pos"),
                  tags$th(style = "text-align: right; padding: 0.25rem 0.5rem; font-weight: 600;", "Proj")
                )
              ),
              tags$tbody(
                lapply(1:min(nrow(unmatched), 20), function(i) {
                  tags$tr(
                    style = if(i %% 2 == 0) "background: rgba(0,0,0,0.02);" else "",
                    tags$td(style = "padding: 0.25rem 0.5rem;", unmatched$player[i]),
                    tags$td(style = "padding: 0.25rem 0.5rem;", unmatched$team[i]),
                    tags$td(style = "padding: 0.25rem 0.5rem;", unmatched$position[i]),
                    tags$td(style = "padding: 0.25rem 0.5rem; text-align: right; font-weight: 600;", 
                            sprintf("%.1f", unmatched$blended[i]))
                  )
                }),
                if (nrow(unmatched) > 20) {
                  tags$tr(
                    tags$td(
                      colspan = 4,
                      style = "padding: 0.25rem 0.5rem; font-style: italic; color: var(--text-muted);",
                      sprintf("... and %d more", nrow(unmatched) - 20)
                    )
                  )
                }
              )
            )
          )
        )
      )
    })
    
    # Remove single adjustment
    observeEvent(input$remove_adj, {
      player_name <- input$remove_adj
      log_debug(">>> Removing adjustment for:", player_name, level = "INFO")
      rv$projection_adjustments[[player_name]] <- NULL
    })
    
    # =========================================================================
    # CONDITIONAL STACKING RULES MANAGEMENT
    # =========================================================================
    
    # Add a new stacking rule
    observeEvent(input$add_rule, {
      req(input$rule_qbs)
      
      qbs <- input$rule_qbs
      same_min <- input$rule_same_min %||% 0
      same_pos <- input$rule_same_pos %||% c()
      opp_min <- input$rule_opp_min %||% 0
      opp_pos <- input$rule_opp_pos %||% c()
      
      # Validate rule makes sense
      if (same_min == 0 && opp_min == 0) {
        showNotification("Rule must require at least 1 same-team or opponent player", type = "warning")
        return()
      }
      
      if (same_min > 0 && length(same_pos) == 0) {
        showNotification("Select positions for same-team stack", type = "warning")
        return()
      }
      
      if (opp_min > 0 && length(opp_pos) == 0) {
        showNotification("Select positions for opponent stack", type = "warning")
        return()
      }
      
      # Create rule
      rule <- list(
        id = paste0("rule_", length(rv$stacking_rules) + 1, "_", sample(1000:9999, 1)),
        qbs = qbs,
        same_team_min = same_min,
        same_team_positions = same_pos,
        opp_min = opp_min,
        opp_positions = opp_pos
      )
      
      rv$stacking_rules[[rule$id]] <- rule
      
      log_debug(">>> Added stacking rule:", rule$id, "for QBs:", paste(qbs, collapse = ", "), level = "INFO")
      
      # Reset inputs
      updateSelectizeInput(session, "rule_qbs", selected = character(0))
      updateNumericInput(session, "rule_same_min", value = 1)
      updateNumericInput(session, "rule_opp_min", value = 0)
      
      showNotification(sprintf("Added rule for %d QB(s)", length(qbs)), type = "message", duration = 2)
    })
    
    # Clear all rules
    observeEvent(input$clear_rules, {
      rv$stacking_rules <- list()
      showNotification("All stacking rules cleared", type = "message", duration = 2)
    })
    
    # Remove single rule
    observeEvent(input$remove_rule, {
      rule_id <- input$remove_rule
      log_debug(">>> Removing stacking rule:", rule_id, level = "INFO")
      rv$stacking_rules[[rule_id]] <- NULL
    })
    
    # Display stacking rules
    output$stacking_rules_display <- renderUI({
      rules <- rv$stacking_rules
      player_data <- rv$player_data
      
      if (length(rules) == 0) {
        return(
          div(
            style = "text-align: center; padding: 0.75rem; color: var(--text-muted); font-size: 0.85rem; font-style: italic; background: var(--bg-secondary); border-radius: 6px;",
            "No conditional stacking rules defined. Lineups will be generated without QB-specific stacking requirements."
          )
        )
      }
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.5rem;",
        lapply(names(rules), function(rule_id) {
          rule <- rules[[rule_id]]
          
          # Get QB info for display
          qb_display <- if (!is.null(player_data)) {
            qb_info <- player_data %>% 
              filter(player %in% rule$qbs) %>%
              mutate(display = paste0(player, " (", team, ")"))
            paste(qb_info$display, collapse = ", ")
          } else {
            paste(rule$qbs, collapse = ", ")
          }
          
          # Build rule description
          same_desc <- if (rule$same_team_min > 0) {
            sprintf("%d+ %s (same team)", rule$same_team_min, paste(rule$same_team_positions, collapse = "/"))
          } else {
            NULL
          }
          
          opp_desc <- if (rule$opp_min > 0) {
            sprintf("%d+ %s (opponent)", rule$opp_min, paste(rule$opp_positions, collapse = "/"))
          } else {
            NULL
          }
          
          stack_desc <- paste(c(same_desc, opp_desc), collapse = " + ")
          
          div(
            style = "display: flex; align-items: center; gap: 0.75rem; padding: 0.5rem 0.75rem; background: white; border: 2px solid var(--accent-yellow); border-radius: 6px;",
            
            # QB indicator
            div(
              style = "background: var(--text-primary); color: white; padding: 0.2rem 0.4rem; border-radius: 4px; font-size: 0.7rem; font-weight: 700;",
              "IF QB"
            ),
            
            # QB names
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.85rem;",
              qb_display
            ),
            
            # Arrow
            span(style = "color: var(--text-muted); font-size: 1.2rem;", "ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€šÃ‚Â ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â‚¬Å¾Ã‚Â¢"),
            
            # Stack requirement
            div(
              style = "flex: 1; font-size: 0.85rem; color: var(--text-secondary);",
              stack_desc
            ),
            
            # Remove button
            tags$button(
              type = "button",
              style = "background: none; border: none; cursor: pointer; padding: 0.25rem 0.5rem; font-size: 1rem; color: var(--accent-coral);",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                                ns("remove_rule"), rule_id),
              "x"
            )
          )
        })
      )
    })
    
    # =========================================================================
    # HELPER: Apply adjustments to projections
    # =========================================================================
    
    get_adjusted_projection <- function(player_name, base_proj, adjustments) {
      adj_pct <- adjustments[[player_name]] %||% 0
      base_proj * (1 + adj_pct / 100)
    }
    
    get_adjusted_players <- function(players, adjustments) {
      players %>%
        mutate(
          adjusted_blended = sapply(1:n(), function(i) {
            get_adjusted_projection(player[i], blended[i], adjustments)
          })
        )
    }
    
    # =========================================================================
    # HELPER FUNCTIONS
    # =========================================================================
    
    # Clear all lineup slots
    clear_lineup_slots <- function() {
      rv$lineup_slots <- list(
        QB = NULL, RB1 = NULL, RB2 = NULL,
        WR1 = NULL, WR2 = NULL, WR3 = NULL,
        TE = NULL, FLEX = NULL, DST = NULL
      )
    }
    
    # Get currently locked players
    get_locked_players <- function() {
      slots <- rv$lineup_slots
      players <- c()
      for (slot in slots) {
        if (!is.null(slot)) {
          players <- c(players, slot$player[1])
        }
      }
      players
    }
    
    # Check if a position can still be added to the lineup
    can_add_position <- function(position) {
      slots <- rv$lineup_slots
      
      if (position == "QB") {
        return(is.null(slots$QB))
      } else if (position == "RB") {
        # RB can go in RB1, RB2, or FLEX
        return(is.null(slots$RB1) || is.null(slots$RB2) || is.null(slots$FLEX))
      } else if (position == "WR") {
        # WR can go in WR1, WR2, WR3, or FLEX
        return(is.null(slots$WR1) || is.null(slots$WR2) || is.null(slots$WR3) || is.null(slots$FLEX))
      } else if (position == "TE") {
        # TE can go in TE or FLEX
        return(is.null(slots$TE) || is.null(slots$FLEX))
      } else if (position == "DST") {
        return(is.null(slots$DST))
      }
      
      return(FALSE)
    }
    
    # Calculate lineup stats (with adjusted projections for comparison)
    lineup_stats <- reactive({
      slots <- rv$lineup_slots
      adjustments <- rv$projection_adjustments
      filled <- Filter(Negate(is.null), slots)
      
      if (length(filled) == 0) {
        return(list(
          total_salary = 0,
          remaining_salary = input$salary_cap %||% 130,
          total_projection = 0,        # Raw projection
          adjusted_projection = 0,     # With adjustments applied
          filled_count = 0
        ))
      }
      
      total_salary <- sum(sapply(filled, function(x) x$salary[1]))
      
      # Calculate raw projection
      total_proj <- sum(sapply(filled, function(x) x$blended[1]))
      
      # Calculate adjusted projection (for comparison)
      adjusted_proj <- sum(sapply(filled, function(x) {
        player_name <- x$player[1]
        base_proj <- x$blended[1]
        get_adjusted_projection(player_name, base_proj, adjustments)
      }))
      
      list(
        total_salary = total_salary,
        remaining_salary = (input$salary_cap %||% 130) - total_salary,
        total_projection = total_proj,
        adjusted_projection = adjusted_proj,
        filled_count = length(filled)
      )
    })
    
    # =========================================================================
    # OPTIMAL LINEUP CALCULATION (Both standard and adjusted)
    # =========================================================================
    
    observeEvent(input$calc_optimal, {
      log_debug(">>> Calculate optimal triggered", level = "INFO")
      
      req(rv$player_data)
      
      tryCatch({
        # Standard optimal (no adjustments)
        optimal <- optimize_lineup_lp(
          players = rv$player_data,
          projection_col = "blended",
          salary_cap = input$salary_cap %||% 120,
          locked_players = NULL,
          excluded_players = NULL
        )
        
        if (!is.null(optimal)) {
          rv$optimal_lineup <- optimal
          rv$optimal_projection <- sum(optimal$projection)
          log_debug(">>> Standard optimal:", rv$optimal_projection, "pts", level = "INFO")
        }
        
        # Adjusted optimal (with adjustments applied)
        if (length(rv$projection_adjustments) > 0) {
          adjusted_players <- get_adjusted_players(rv$player_data, rv$projection_adjustments)
          
          adjusted_optimal <- optimize_lineup_lp(
            players = adjusted_players,
            projection_col = "adjusted_blended",
            salary_cap = input$salary_cap %||% 120,
            locked_players = NULL,
            excluded_players = NULL
          )
          
          if (!is.null(adjusted_optimal)) {
            rv$adjusted_optimal_lineup <- adjusted_optimal
            rv$adjusted_optimal_projection <- sum(adjusted_optimal$projection)
            log_debug(">>> Adjusted optimal:", rv$adjusted_optimal_projection, "pts", level = "INFO")
          }
        } else {
          rv$adjusted_optimal_lineup <- NULL
          rv$adjusted_optimal_projection <- 0
        }
        
        showNotification(
          sprintf("Optimal lineup: %.1f pts", rv$optimal_projection),
          type = "message", duration = 3
        )
        
      }, error = function(e) {
        log_debug(">>> Error calculating optimal:", e$message, level = "ERROR")
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # =========================================================================
    # OUTPUT: Optimal Comparison (Two lineups side by side)
    # =========================================================================
    
    output$optimal_comparison <- renderUI({
      if (is.null(rv$optimal_lineup)) {
        return(
          div(
            style = "text-align: center; padding: 2rem; color: var(--text-muted);",
            "Click 'Calculate Optimal Lineups' to see the best possible lineup"
          )
        )
      }
      
      optimal <- rv$optimal_lineup
      adjusted_optimal <- rv$adjusted_optimal_lineup
      adjustments <- rv$projection_adjustments
      player_data <- rv$player_data
      has_adjustments <- length(adjustments) > 0
      
      # Helper function to render a lineup
      render_lineup <- function(lineup, title, is_adjusted = FALSE, projection_col = "projection") {
        if (is.null(lineup)) return(NULL)
        
        div(
          style = "flex: 1; min-width: 350px;",
          
          # Title
          div(
            style = "font-weight: 700; font-size: 1rem; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px solid var(--bg-secondary);",
            title
          ),
          
          # Summary stats
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 0.75rem; margin-bottom: 1rem;",
            div(
              style = "text-align: center; padding: 0.5rem; background: var(--bg-tertiary); border-radius: 6px;",
              div(style = "font-size: 0.7rem; text-transform: uppercase; color: var(--text-muted);", "Projection"),
              div(style = sprintf("font-size: 1.25rem; font-weight: 700; color: %s;", 
                                  if (is_adjusted) "var(--accent-coral)" else "var(--accent-teal)"),
                  sprintf("%.1f", sum(lineup[[projection_col]])))
            ),
            div(
              style = "text-align: center; padding: 0.5rem; background: var(--bg-tertiary); border-radius: 6px;",
              div(style = "font-size: 0.7rem; text-transform: uppercase; color: var(--text-muted);", "Salary"),
              div(style = "font-size: 1.25rem; font-weight: 700;",
                  sprintf("$%.1f", sum(lineup$salary)))
            )
          ),
          
          # Player list
          div(
            style = "font-size: 0.8rem;",
            lapply(1:nrow(lineup), function(i) {
              player_info <- player_data %>% filter(player == lineup$player[i])
              headshot_url <- if (nrow(player_info) > 0 && "headshot_url" %in% names(player_info)) {
                player_info$headshot_url[1]
              } else {
                "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
              }
              team_bg <- if (nrow(player_info) > 0 && "team_bg_color" %in% names(player_info)) {
                player_info$team_bg_color[1]
              } else {
                "#E0E0E0"
              }
              
              # Check if player has adjustment
              player_adj <- adjustments[[lineup$player[i]]]
              has_adj <- !is.null(player_adj) && player_adj != 0
              
              div(
                style = "display: flex; align-items: center; padding: 0.3rem 0; border-bottom: 1px solid var(--bg-secondary);",
                # Mini headshot
                div(
                  style = sprintf("width: 36px; height: 36px; border-radius: 50%%; background: %s; overflow: hidden; margin-right: 0.6rem; flex-shrink: 0;", team_bg),
                  if (lineup$position[i] == "DST") {
                    tags$img(
                      src = sprintf("nfl_logos/%s.webp", lineup$team[i]),
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
                span(
                  class = "position-badge",
                  style = "margin-right: 0.5rem;",
                  lineup$position[i]
                ),
                div(
                  style = "flex: 1;",
                  div(style = "font-weight: 700; font-size: 0.95rem;", lineup$player[i]),
                  div(
                    style = "font-size: 0.75rem; color: var(--text-muted);",
                    span(style = "font-weight: 600; color: var(--text-secondary);", lineup$team[i]),
                    if (!is.null(player_info$opponent[1])) span(style = "margin: 0 0.2rem;", if(isTRUE(player_info$home[1])) "vs" else "@"),
                    if (!is.null(player_info$opponent[1])) span(player_info$opponent[1])
                  )
                ),
                if (has_adj) {
                  span(
                    style = sprintf("font-size: 0.7rem; padding: 0.1rem 0.3rem; border-radius: 3px; margin-right: 0.3rem; background: %s; color: %s;",
                                    if (player_adj > 0) "rgba(139, 168, 134, 0.2)" else "rgba(232, 131, 121, 0.2)",
                                    if (player_adj > 0) "var(--accent-sage)" else "var(--accent-coral)"),
                    sprintf("%+.0f%%", player_adj)
                  )
                },
                span(style = "color: var(--text-muted);", sprintf("$%.1f", lineup$salary[i]))
              )
            })
          )
        )
      }
      
      div(
        style = "display: flex; gap: 2rem; flex-wrap: wrap;",
        
        # Standard Optimal
        render_lineup(optimal, "Standard Optimal", FALSE, "projection"),
        
        # Adjusted Optimal (only if adjustments exist)
        if (has_adjustments && !is.null(adjusted_optimal)) {
          render_lineup(adjusted_optimal, "Adjusted Optimal", TRUE, "projection")
        } else if (has_adjustments) {
          div(
            style = "flex: 1; min-width: 350px; padding: 2rem; text-align: center; color: var(--text-muted); background: var(--bg-tertiary); border-radius: 8px;",
            "Click 'Calculate Optimal' to generate adjusted lineup"
          )
        }
      )
    })
    
    # =========================================================================
    # POSITION FILTER BUTTONS
    # =========================================================================
    
    # Update active button styling via JS
    observe({
      session$sendCustomMessage("updatePositionFilter", list(
        ns = session$ns(""),
        active = rv$position_filter
      ))
    })
    
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
    })
    observeEvent(input$filter_qb, { 
      rv$position_filter <- "QB"
      update_position_buttons("QB")
    })
    observeEvent(input$filter_rb, { 
      rv$position_filter <- "RB"
      update_position_buttons("RB")
    })
    observeEvent(input$filter_wr, { 
      rv$position_filter <- "WR"
      update_position_buttons("WR")
    })
    observeEvent(input$filter_te, { 
      rv$position_filter <- "TE"
      update_position_buttons("TE")
    })
    observeEvent(input$filter_dst, { 
      rv$position_filter <- "DST"
      update_position_buttons("DST")
    })
    
    # Team filter
    observeEvent(input$filter_team, {
      rv$team_filter <- input$filter_team %||% "all"
    }, ignoreNULL = FALSE)
    
    # =========================================================================
    # PLAYER POOL SORT HANDLERS
    # =========================================================================
    
    observeEvent(input$pool_sort_salary, {
      if (rv$pool_sort$col == "salary") {
        rv$pool_sort$dir <- if (rv$pool_sort$dir == "desc") "asc" else "desc"
      } else {
        rv$pool_sort <- list(col = "salary", dir = "asc")
      }
    })
    
    observeEvent(input$pool_sort_proj, {
      if (rv$pool_sort$col == "full") {
        rv$pool_sort$dir <- if (rv$pool_sort$dir == "desc") "asc" else "desc"
      } else {
        rv$pool_sort <- list(col = "full", dir = "desc")
      }
    })
    
    observeEvent(input$pool_sort_ceiling, {
      if (rv$pool_sort$col == "ceiling") {
        rv$pool_sort$dir <- if (rv$pool_sort$dir == "desc") "asc" else "desc"
      } else {
        rv$pool_sort <- list(col = "ceiling", dir = "desc")
      }
    })
    
    observeEvent(input$pool_sort_blended, {
      if (rv$pool_sort$col == "blended") {
        rv$pool_sort$dir <- if (rv$pool_sort$dir == "desc") "asc" else "desc"
      } else {
        rv$pool_sort <- list(col = "blended", dir = "desc")
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
    # PLAYER POOL HEADER (with sort indicators)
    # =========================================================================
    
    output$player_pool_header <- renderUI({
      sort_col <- rv$pool_sort$col
      sort_dir <- rv$pool_sort$dir
      
      # Helper for sort indicator
      sort_indicator <- function(col) {
        if (sort_col == col) {
          if (sort_dir == "desc") " ÃƒÂ¢Ã¢â‚¬â€œÃ‚Â¼" else " ÃƒÂ¢Ã¢â‚¬â€œÃ‚Â²"
        } else {
          ""
        }
      }
      
      # Header style
      header_style <- "text-align: center; cursor: pointer; user-select: none;"
      active_style <- "text-align: center; cursor: pointer; user-select: none; color: var(--accent-plum); font-weight: 800;"
      
      div(
        class = "player-pool-header",
        style = "display: grid; grid-template-columns: 1fr 65px 55px 55px 60px 50px; gap: 0.25rem; padding: 0.5rem 0.75rem; background: var(--bg-secondary); border-bottom: 2px solid var(--outline); font-weight: 700; font-size: 0.65rem; text-transform: uppercase; color: var(--text-muted); position: sticky; top: 0; z-index: 10;",
        
        span(style = "text-align: left;", "Player"),
        span(
          style = if (sort_col == "salary") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("pool_sort_salary")),
          HTML(paste0("Salary", sort_indicator("salary")))
        ),
        span(
          style = if (sort_col == "full") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("pool_sort_proj")),
          HTML(paste0("Proj", sort_indicator("full")))
        ),
        span(
          style = if (sort_col == "ceiling") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("pool_sort_ceiling")),
          HTML(paste0("Ceil", sort_indicator("ceiling")))
        ),
        span(
          style = if (sort_col == "blended") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("pool_sort_blended")),
          HTML(paste0("Blend", sort_indicator("blended")))
        ),
        span(
          style = if (sort_col == "value") active_style else header_style,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("pool_sort_value")),
          HTML(paste0("Val", sort_indicator("value")))
        )
      )
    })
    
    # =========================================================================
    # PLAYER POOL RENDER
    # =========================================================================
    
    output$player_pool <- renderUI({
      req(rv$player_data)
      
      data <- rv$player_data
      stats <- lineup_stats()
      locked <- get_locked_players()
      pos_filter <- rv$position_filter
      team_filter <- rv$team_filter %||% "all"
      sort_col <- rv$pool_sort$col
      sort_dir <- rv$pool_sort$dir
      show_heatmap <- isTRUE(input$pool_heatmap)
      
      # Get which positions can still be added
      positions_available <- c(
        QB = can_add_position("QB"),
        RB = can_add_position("RB"),
        WR = can_add_position("WR"),
        TE = can_add_position("TE"),
        DST = can_add_position("DST")
      )
      
      # Filter by position
      if (pos_filter != "all") {
        data <- data %>% filter(position == pos_filter)
      }
      
      # Filter by team
      if (team_filter != "all") {
        data <- data %>% filter(team == team_filter)
      }
      
      # Add status columns
      data <- data %>% 
        mutate(
          is_locked = player %in% locked,
          can_afford = salary <= stats$remaining_salary,
          position_available = positions_available[position]
        )
      
      # Heatmap color function for value column (diverging: coral -> white -> teal)
      get_value_heatmap <- function(value, all_values) {
        if (!show_heatmap) return("")
        
        midpoint <- 1.0
        min_val <- min(all_values, na.rm = TRUE)
        max_val <- max(all_values, na.rm = TRUE)
        
        if (value < midpoint) {
          # Below midpoint: coral to white
          if (min_val >= midpoint) min_val <- 0.5
          t <- (value - min_val) / (midpoint - min_val)
          t <- max(0, min(1, t))
          r <- round(208 + (255 - 208) * t)
          g <- round(135 + (255 - 135) * t)
          b <- round(112 + (255 - 112) * t)
        } else {
          # Above midpoint: white to teal
          if (max_val <= midpoint) max_val <- 2.0
          t <- (value - midpoint) / (max_val - midpoint)
          t <- max(0, min(1, t))
          r <- round(255 + (143 - 255) * t)
          g <- round(255 + (188 - 255) * t)
          b <- round(255 + (187 - 255) * t)
        }
        
        sprintf("background-color: rgb(%d, %d, %d); border-radius: 4px;", r, g, b)
      }
      
      # Get all values for heatmap scaling
      all_values <- data$value
      
      # Sort by selected column (unavailable players at bottom)
      if (sort_dir == "desc") {
        data <- data %>% arrange(is_locked, !position_available, !can_afford, desc(.data[[sort_col]]))
      } else {
        data <- data %>% arrange(is_locked, !position_available, !can_afford, .data[[sort_col]])
      }
      
      if (nrow(data) == 0) {
        return(div(
          style = "padding: 2rem; text-align: center; color: var(--text-muted);",
          "No players available"
        ))
      }
      
      # Create clickable rows
      player_rows <- lapply(1:nrow(data), function(i) {
        p <- data[i, ]
        is_locked <- p$is_locked
        can_afford <- p$can_afford
        pos_available <- p$position_available
        
        # Determine row state
        if (is_locked) {
          row_class <- "player-pool-row player-pool-row--locked"
          row_style <- "opacity: 0.4; cursor: not-allowed; background: var(--bg-secondary);"
          clickable <- FALSE
        } else if (!pos_available) {
          row_class <- "player-pool-row player-pool-row--position-full"
          row_style <- "opacity: 0.6; cursor: not-allowed;"
          clickable <- FALSE
        } else if (!can_afford) {
          row_class <- "player-pool-row player-pool-row--unaffordable"
          row_style <- "opacity: 0.7; cursor: not-allowed;"
          clickable <- FALSE
        } else {
          row_class <- "player-pool-row player-pool-row--available"
          row_style <- "cursor: pointer;"
          clickable <- TRUE
        }
        
        # Create the row
        row_content <- div(
          class = row_class,
          style = sprintf("display: grid; grid-template-columns: 1fr 65px 55px 55px 60px 50px; gap: 0.25rem; padding: 0.4rem 0.75rem; align-items: center; border-bottom: 1px solid var(--bg-secondary); %s", row_style),
          `data-player` = p$player,
          
          # Player cell: Headshot + Name + Matchup
          div(
            style = "display: flex; align-items: center; gap: 0.5rem;",
            create_headshot_html(p$headshot_url, p$team_bg_color, "tiny", p$position, p$team),
            div(
              span(style = "font-weight: 600; font-size: 0.8rem; line-height: 1.2;", p$player),
              create_matchup_html(p$team, p$opponent, p$home)
            )
          ),
          
          # Salary - red only when unaffordable is the limiting factor
          div(
            style = sprintf("text-align: center; font-weight: 600; font-size: 0.8rem; %s",
                            if (!can_afford && !is_locked && pos_available) "color: var(--accent-red);" else ""),
            sprintf("$%.1f", p$salary)
          ),
          
          # Proj (full)
          div(
            style = "text-align: center; font-size: 0.8rem; color: var(--text-secondary);",
            sprintf("%.1f", p$full)
          ),
          
          # Ceiling
          div(
            style = "text-align: center; font-size: 0.8rem; color: var(--text-secondary);",
            sprintf("%.1f", p$ceiling)
          ),
          
          # Blended
          div(
            style = "text-align: center; font-size: 0.8rem; color: var(--accent-plum); font-weight: 600;",
            sprintf("%.1f", p$blended)
          ),
          
          # Value (with optional heatmap - fills cell)
          div(
            style = sprintf("text-align: center; font-size: 0.75rem; font-weight: 600; padding: 0.25rem 0.15rem; margin: 0 2px; %s%s",
                            if (show_heatmap) "color: var(--text-primary); " else "color: var(--text-muted); ",
                            get_value_heatmap(p$value, all_values)),
            sprintf("%.2f", p$value)
          )
        )
        
        # Wrap clickable rows in an actionButton-style div with onclick
        if (clickable) {
          # Escape apostrophes in player names for JavaScript
          escaped_name <- gsub("'", "\\\\'", p$player)
          tags$div(
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              session$ns("add_player_click"), escaped_name),
            row_content
          )
        } else {
          row_content
        }
      })
      
      tagList(player_rows)
    })
    
    # =========================================================================
    # ADD PLAYER FROM POOL CLICK
    # =========================================================================
    
    observeEvent(input$add_player_click, {
      req(input$add_player_click != "")
      req(rv$player_data)
      
      player_name <- input$add_player_click
      log_debug(">>> Adding player from pool:", player_name, level = "INFO")
      
      player_row <- rv$player_data %>% filter(player == player_name)
      if (nrow(player_row) == 0) return()
      
      # Check if already in lineup
      locked <- get_locked_players()
      if (player_name %in% locked) {
        showNotification("Player already in lineup", type = "warning", duration = 2)
        return()
      }
      
      # Check salary cap
      stats <- lineup_stats()
      if (stats$total_salary + player_row$salary[1] > (input$salary_cap %||% 130)) {
        showNotification("Would exceed salary cap", type = "warning", duration = 3)
        return()
      }
      
      pos <- player_row$position[1]
      assigned <- FALSE
      
      # Assign to appropriate slot
      if (pos == "QB" && is.null(rv$lineup_slots$QB)) {
        rv$lineup_slots$QB <- player_row
        assigned <- TRUE
      } else if (pos == "RB") {
        if (is.null(rv$lineup_slots$RB1)) {
          rv$lineup_slots$RB1 <- player_row
          assigned <- TRUE
        } else if (is.null(rv$lineup_slots$RB2)) {
          rv$lineup_slots$RB2 <- player_row
          assigned <- TRUE
        } else if (is.null(rv$lineup_slots$FLEX)) {
          rv$lineup_slots$FLEX <- player_row
          assigned <- TRUE
        }
      } else if (pos == "WR") {
        if (is.null(rv$lineup_slots$WR1)) {
          rv$lineup_slots$WR1 <- player_row
          assigned <- TRUE
        } else if (is.null(rv$lineup_slots$WR2)) {
          rv$lineup_slots$WR2 <- player_row
          assigned <- TRUE
        } else if (is.null(rv$lineup_slots$WR3)) {
          rv$lineup_slots$WR3 <- player_row
          assigned <- TRUE
        } else if (is.null(rv$lineup_slots$FLEX)) {
          rv$lineup_slots$FLEX <- player_row
          assigned <- TRUE
        }
      } else if (pos == "TE") {
        if (is.null(rv$lineup_slots$TE)) {
          rv$lineup_slots$TE <- player_row
          assigned <- TRUE
        } else if (is.null(rv$lineup_slots$FLEX)) {
          rv$lineup_slots$FLEX <- player_row
          assigned <- TRUE
        }
      } else if (pos == "DST" && is.null(rv$lineup_slots$DST)) {
        rv$lineup_slots$DST <- player_row
        assigned <- TRUE
      }
      
      if (!assigned) {
        showNotification(sprintf("No available slot for %s", player_name), type = "warning")
      }
    })
    
    # =========================================================================
    # REMOVE PLAYER HANDLERS
    # =========================================================================
    
    lapply(c("QB", "RB1", "RB2", "WR1", "WR2", "WR3", "TE", "FLEX", "DST"), function(slot) {
      observeEvent(input[[paste0("remove_", slot)]], {
        log_debug(">>> Removing player from slot:", slot, level = "DEBUG")
        rv$lineup_slots[[slot]] <- NULL
      }, ignoreInit = TRUE)
    })
    
    # =========================================================================
    # CLEAR ALL
    # =========================================================================
    
    observeEvent(input$clear_all, {
      log_debug(">>> Clear all triggered", level = "INFO")
      clear_lineup_slots()
      rv$generated_lineups <- NULL
      showNotification("Lineup cleared", type = "message", duration = 2)
    })
    
    # =========================================================================
    # AUTOCOMPLETE
    # =========================================================================
    
    observeEvent(input$autocomplete, {
      log_debug(">>> Autocomplete triggered", level = "INFO")
      
      req(rv$player_data)
      
      locked <- get_locked_players()
      adjustments <- rv$projection_adjustments
      
      # Use adjusted projections if any adjustments exist
      if (length(adjustments) > 0) {
        players_to_use <- get_adjusted_players(rv$player_data, adjustments)
        proj_col <- "adjusted_blended"
      } else {
        players_to_use <- rv$player_data
        proj_col <- "blended"
      }
      
      tryCatch({
        optimal <- optimize_lineup_lp(
          players = players_to_use,
          projection_col = proj_col,
          salary_cap = input$salary_cap %||% 120,
          locked_players = locked,
          excluded_players = NULL
        )
        
        if (is.null(optimal)) {
          showNotification("Could not complete lineup", type = "warning")
          return()
        }
        
        # Clear and refill all slots
        clear_lineup_slots()
        
        for (i in 1:nrow(optimal)) {
          player_name <- optimal$player[i]
          player_row <- rv$player_data %>% filter(player == player_name)
          
          if (nrow(player_row) == 0) next
          
          pos <- player_row$position[1]
          
          if (pos == "QB" && is.null(rv$lineup_slots$QB)) {
            rv$lineup_slots$QB <- player_row
          } else if (pos == "RB") {
            if (is.null(rv$lineup_slots$RB1)) {
              rv$lineup_slots$RB1 <- player_row
            } else if (is.null(rv$lineup_slots$RB2)) {
              rv$lineup_slots$RB2 <- player_row
            } else if (is.null(rv$lineup_slots$FLEX)) {
              rv$lineup_slots$FLEX <- player_row
            }
          } else if (pos == "WR") {
            if (is.null(rv$lineup_slots$WR1)) {
              rv$lineup_slots$WR1 <- player_row
            } else if (is.null(rv$lineup_slots$WR2)) {
              rv$lineup_slots$WR2 <- player_row
            } else if (is.null(rv$lineup_slots$WR3)) {
              rv$lineup_slots$WR3 <- player_row
            } else if (is.null(rv$lineup_slots$FLEX)) {
              rv$lineup_slots$FLEX <- player_row
            }
          } else if (pos == "TE") {
            if (is.null(rv$lineup_slots$TE)) {
              rv$lineup_slots$TE <- player_row
            } else if (is.null(rv$lineup_slots$FLEX)) {
              rv$lineup_slots$FLEX <- player_row
            }
          } else if (pos == "DST" && is.null(rv$lineup_slots$DST)) {
            rv$lineup_slots$DST <- player_row
          }
        }
        
        showNotification("Lineup autocompleted!", type = "message", duration = 2)
        
      }, error = function(e) {
        log_debug(">>> Autocomplete error:", e$message, level = "ERROR")
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # =========================================================================
    # GENERATE LINEUPS WITH STACKING RULES
    # =========================================================================
    
    observeEvent(input$generate, {
      log_debug(">>> Generate lineups triggered", level = "INFO")
      
      req(rv$player_data)
      
      locked <- get_locked_players()
      adjustments <- rv$projection_adjustments
      stacking_rules <- rv$stacking_rules
      
      # Capture completion instructions for display with results
      rv$completion_instructions <- input$completion_instructions %||% ""
      
      showNotification("Generating lineups...", type = "message", id = "gen_notif", duration = NULL)
      
      tryCatch({
        lineups <- generate_lineups_with_stacking(
          players = rv$player_data,
          num_lineups = input$num_lineups %||% 10,
          salary_cap = input$salary_cap %||% 120,
          variance_pct = input$variance_pct %||% 15,
          locked_players = locked,
          adjustments = adjustments,
          stacking_rules = stacking_rules,
          stack_game = input$stack_game %||% "",
          min_game_players = input$min_game_players %||% 4
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
    
    # =========================================================================
    # OUTPUT: Lineup Display
    # =========================================================================
    
    output$lineup_display <- renderUI({
      log_debug(">>> Rendering lineup display", level = "DEBUG")
      
      slots <- rv$lineup_slots
      stats <- lineup_stats()
      optimal_proj <- rv$optimal_projection
      adjustments <- rv$projection_adjustments
      
      # Get QB team for stack highlighting
      qb_team <- if (!is.null(slots$QB)) slots$QB$team[1] else NULL
      
      # Create slot row helper with headshots
      create_slot_row <- function(slot_name, label, data) {
        if (is.null(data)) {
          div(
            style = "display: flex; align-items: center; padding: 0.3rem 0.4rem; background: var(--bg-secondary); border-radius: 4px; margin-bottom: 0.3rem;",
            span(
              style = "background: var(--text-primary); color: white; padding: 0.15rem 0.3rem; border-radius: 4px; font-weight: 700; font-size: 0.65rem; min-width: 32px; text-align: center;",
              label
            ),
            span(style = "flex: 1; padding-left: 0.5rem; color: var(--text-muted); font-style: italic; font-size: 0.8rem;", "Empty")
          )
        } else {
          # Check if player stacks with QB
          is_stack <- !is.null(qb_team) && data$team[1] == qb_team && slot_name != "QB"
          row_bg <- if (is_stack) "background: #FFF9E6;" else "background: white;"
          
          # Check if player has adjustment
          player_adj <- adjustments[[data$player[1]]]
          has_adj <- !is.null(player_adj) && player_adj != 0
          
          div(
            style = sprintf("display: flex; align-items: center; padding: 0.3rem 0.4rem; %s border: 2px solid var(--text-primary); border-radius: 4px; margin-bottom: 0.3rem;", row_bg),
            # Position badge
            span(
              style = "background: var(--text-primary); color: white; padding: 0.15rem 0.3rem; border-radius: 4px; font-weight: 700; font-size: 0.65rem; min-width: 32px; text-align: center;",
              label
            ),
            # Headshot
            div(
              style = "margin-left: 0.3rem;",
              create_headshot_html(
                data$headshot_url[1], 
                data$team_bg_color[1], 
                "small", 
                data$position[1], 
                data$team[1]
              )
            ),
            # Player info
            div(
              style = "flex: 1; padding-left: 0.3rem;",
              div(style = "font-weight: 600; font-size: 0.8rem; line-height: 1.2;", data$player[1]),
              create_matchup_html(data$team[1], data$opponent[1], data$home[1])
            ),
            # Adjustment badge if applicable
            if (has_adj) {
              span(
                style = sprintf("font-size: 0.65rem; padding: 0.1rem 0.25rem; border-radius: 3px; margin-right: 0.3rem; background: %s; color: %s;",
                                if (player_adj > 0) "rgba(139, 168, 134, 0.2)" else "rgba(232, 131, 121, 0.2)",
                                if (player_adj > 0) "var(--accent-sage)" else "var(--accent-coral)"),
                sprintf("%+.0f%%", player_adj)
              )
            },
            # Salary and projection
            div(
              style = "text-align: right; padding-right: 0.3rem;",
              div(style = "font-weight: 600; font-size: 0.75rem;", sprintf("$%.1f", data$salary[1])),
              div(style = "font-size: 0.65rem; color: var(--accent-coral); font-weight: 600;", 
                  sprintf("%.1f pts", data$blended[1]))
            ),
            # Remove button
            actionButton(
              ns(paste0("remove_", slot_name)),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;"
            )
          )
        }
      }
      
      # Difference vs optimal (using ADJUSTED projection)
      diff_vs_optimal <- if (optimal_proj > 0) {
        optimal_proj - stats$adjusted_projection
      } else {
        NA
      }
      
      # Check if there are any adjustments affecting this lineup
      has_adjustments <- stats$adjusted_projection != stats$total_projection
      
      # Calculate average salary left per remaining player
      empty_slots <- 9 - stats$filled_count
      avg_salary_left <- if (empty_slots > 0) stats$remaining_salary / empty_slots else 0
      
      tagList(
        # Summary row - 4 column header with remaining integrated
        div(
          style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 0.4rem; margin-bottom: 0.5rem; padding: 0.4rem 0.5rem; background: var(--bg-tertiary); border-radius: 6px; border: 2px solid var(--outline);",
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted); letter-spacing: 0.5px;", "Salary Used"),
            div(style = "font-weight: 700; font-size: 0.9rem;", sprintf("$%.1f", stats$total_salary))
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted); letter-spacing: 0.5px;", "Remaining"),
            div(
              style = sprintf("font-weight: 700; font-size: 0.9rem; color: %s;",
                              if (stats$remaining_salary < 0) "var(--accent-coral)" else "var(--accent-teal)"),
              sprintf("$%.1f (%d/9)", stats$remaining_salary, stats$filled_count)
            )
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted); letter-spacing: 0.5px;", 
                if (has_adjustments) "Adj Projection" else "Projection"),
            div(
              style = sprintf("font-weight: 700; font-size: 0.9rem; color: %s;", 
                              if (has_adjustments) "var(--accent-coral)" else "var(--accent-plum)"
              ),
              sprintf("%.1f pts", stats$adjusted_projection)
            )
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted); letter-spacing: 0.5px;", "Avg $/Player"),
            div(
              style = sprintf("font-weight: 700; font-size: 0.9rem; color: %s;",
                              if (empty_slots == 0) "var(--text-muted)" else if (avg_salary_left < 10) "var(--accent-coral)" else "var(--text-primary)"),
              if (empty_slots > 0) sprintf("$%.1f", avg_salary_left) else HTML("&mdash;")
            )
          )
        ),
        
        # Slots
        create_slot_row("QB", "QB", slots$QB),
        create_slot_row("RB1", "RB", slots$RB1),
        create_slot_row("RB2", "RB", slots$RB2),
        create_slot_row("WR1", "WR", slots$WR1),
        create_slot_row("WR2", "WR", slots$WR2),
        create_slot_row("WR3", "WR", slots$WR3),
        create_slot_row("TE", "TE", slots$TE),
        create_slot_row("FLEX", "FLX", slots$FLEX),
        create_slot_row("DST", "DST", slots$DST)
      )
    })
    
    # =========================================================================
    # OUTPUT: Results Panel
    # =========================================================================
    
    output$results_panel <- renderUI({
      log_debug(">>> Rendering results panel", level = "DEBUG")
      
      lineups <- rv$generated_lineups
      optimal_proj <- rv$optimal_projection
      adjustments <- rv$projection_adjustments
      
      if (is.null(lineups) || length(lineups) == 0) {
        return(
          ui_card(
            title = "Generated Lineups",
            color = NFL_CARD_COLOR,
            div(
              style = "text-align: center; padding: 3rem; color: var(--text-muted);",
              tags$p("No lineups generated yet."),
              tags$p(style = "font-size: 0.85rem;", 
                     "1. Optionally add players to lock them in"),
              tags$p(style = "font-size: 0.85rem;", 
                     "2. Configure stacking rules"),
              tags$p(style = "font-size: 0.85rem;", 
                     "3. Click 'Generate Lineups' to create variations")
            )
          )
        )
      }
      
      # Calculate adjusted projections for each lineup
      lineup_projections <- sapply(lineups, function(lineup) {
        sum(sapply(1:nrow(lineup), function(j) {
          player_name <- lineup$player[j]
          base_proj <- lineup$projection[j]
          get_adjusted_projection(player_name, base_proj, adjustments)
        }))
      })
      
      sorted_indices <- order(lineup_projections, decreasing = TRUE)
      
      # Stats (using adjusted projections)
      avg_proj <- mean(lineup_projections)
      best_proj <- max(lineup_projections)
      
      # Check if adjustments are active
      has_adjustments <- length(adjustments) > 0
      
      # Get locked players for opacity styling
      locked_players <- get_locked_players()
      
      # Get completion instructions
      completion_instructions <- rv$completion_instructions
      
      ui_card(
        title = sprintf("Generated Lineups (%d)%s", length(lineups), 
                        if (has_adjustments) " - Projections Adjusted" else ""),
        color = NFL_CARD_COLOR,
        
        # Show completion instructions if provided
        if (!is.null(completion_instructions) && nchar(trimws(completion_instructions)) > 0) {
          div(
            style = "background: var(--bg-tertiary); border-left: 3px solid var(--accent-plum); padding: 0.75rem 1rem; margin-bottom: 1rem; border-radius: 0 6px 6px 0;",
            div(
              style = "font-size: 0.7rem; text-transform: uppercase; font-weight: 600; color: var(--text-muted); margin-bottom: 0.25rem;",
              "Completion Instructions"
            ),
            div(
              style = "font-size: 0.85rem; color: var(--text-secondary); font-style: italic;",
              completion_instructions
            )
          )
        },
        
        # Summary stats
        div(
          style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 1rem; margin-bottom: 1.5rem; padding-bottom: 1rem; border-bottom: 2px solid var(--bg-secondary);",
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", 
                if (has_adjustments) "Best (Adj)" else "Best"),
            div(style = "font-size: 1.25rem; font-weight: 700; color: var(--accent-sage);", sprintf("%.1f", best_proj))
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", 
                if (has_adjustments) "Avg (Adj)" else "Average"),
            div(style = "font-size: 1.25rem; font-weight: 700;", sprintf("%.1f", avg_proj))
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Optimal"),
            div(style = "font-size: 1.25rem; font-weight: 700; color: var(--accent-teal);", 
                if (optimal_proj > 0) sprintf("%.1f", optimal_proj) else "--Ã‚Â")
          ),
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Best vs Opt"),
            div(
              style = sprintf("font-size: 1.25rem; font-weight: 700; color: %s;",
                              if (optimal_proj > 0) "var(--accent-coral)" else "var(--text-muted)"
              ),
              if (optimal_proj > 0) sprintf("%+.1f", best_proj - optimal_proj) else "--Ã‚Â"
            )
          )
        ),
        
        # Lineup cards - grid layout for full width
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
          lapply(sorted_indices, function(i) {
            lineup <- lineups[[i]]
            salary <- sum(lineup$salary)
            
            # Calculate adjusted projection for this lineup
            adj_proj <- sum(sapply(1:nrow(lineup), function(j) {
              player_name <- lineup$player[j]
              base_proj <- lineup$projection[j]
              get_adjusted_projection(player_name, base_proj, adjustments)
            }))
            
            vs_optimal <- if (optimal_proj > 0) adj_proj - optimal_proj else NA
            
            # Get player data for headshots
            player_data <- rv$player_data
            
            div(
              style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
              
              # Header
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px dashed var(--bg-secondary);",
                span(style = "font-weight: 800; text-transform: uppercase; font-size: 0.85rem;", sprintf("Lineup %d", i)),
                div(
                  style = "display: flex; gap: 0.75rem; font-size: 0.85rem;",
                  span(style = "font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", adj_proj)),
                  span(style = "color: var(--text-muted);", sprintf("$%.1f", salary)),
                  if (!is.na(vs_optimal)) {
                    span(
                      style = sprintf("font-weight: 600; color: %s;",
                                      if (vs_optimal > -3) "var(--accent-sage)" else "var(--accent-coral)"
                      ),
                      sprintf("%+.1f", vs_optimal)
                    )
                  }
                )
              ),
              
              # Players - vertically stacked
              div(
                style = "display: flex; flex-direction: column; gap: 0.3rem; font-size: 0.8rem;",
                lapply(1:nrow(lineup), function(j) {
                  # Get headshot info
                  player_info <- player_data %>% filter(player == lineup$player[j])
                  headshot_url <- if (nrow(player_info) > 0 && "headshot_url" %in% names(player_info)) {
                    player_info$headshot_url[1]
                  } else {
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
                  }
                  team_bg <- if (nrow(player_info) > 0 && "team_bg_color" %in% names(player_info)) {
                    player_info$team_bg_color[1]
                  } else {
                    "#E0E0E0"
                  }
                  opponent <- if (nrow(player_info) > 0 && "opponent" %in% names(player_info)) {
                    player_info$opponent[1]
                  } else {
                    ""
                  }
                  home <- if (nrow(player_info) > 0 && "home" %in% names(player_info)) {
                    player_info$home[1]
                  } else {
                    TRUE
                  }
                  
                  # Check if this player is locked (already in user's lineup)
                  is_locked <- lineup$player[j] %in% locked_players
                  opacity_style <- if (is_locked) "opacity: 0.5;" else ""
                  
                  # Check if player has adjustment
                  player_adj <- adjustments[[lineup$player[j]]]
                  has_adj <- !is.null(player_adj) && player_adj != 0
                  
                  div(
                    style = sprintf("display: flex; align-items: center; gap: 0.3rem; padding: 0.2rem 0.3rem; background: var(--bg-tertiary); border-radius: 4px; %s", opacity_style),
                    # Mini headshot
                    div(
                      style = sprintf("width: 26px; height: 26px; border-radius: 50%%; background: %s; overflow: hidden; flex-shrink: 0;", team_bg),
                      if (lineup$position[j] == "DST") {
                        tags$img(
                          src = sprintf("nfl_logos/%s.webp", lineup$team[j]),
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
                    span(
                      class = "position-badge",
                      lineup$position[j]
                    ),
                    div(
                      style = "flex: 1; min-width: 0;",
                      div(style = "font-weight: 600; font-size: 0.8rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
                          lineup$player[j]),
                      div(
                        style = "font-size: 0.65rem; color: var(--text-muted);",
                        span(style = "font-weight: 600; color: var(--text-secondary);", lineup$team[j]),
                        span(style = "margin: 0 0.15rem;", if(isTRUE(home)) "vs" else "@"),
                        span(opponent)
                      )
                    ),
                    if (has_adj) {
                      span(
                        style = sprintf("font-size: 0.55rem; padding: 0.05rem 0.15rem; border-radius: 2px; background: %s; color: %s;",
                                        if (player_adj > 0) "rgba(139, 168, 134, 0.3)" else "rgba(232, 131, 121, 0.3)",
                                        if (player_adj > 0) "var(--accent-sage)" else "var(--accent-coral)"),
                        sprintf("%+.0f%%", player_adj)
                      )
                    },
                    span(
                      style = "font-size: 0.75rem; font-weight: 600; color: var(--text-muted);",
                      sprintf("$%.1f", lineup$salary[j])
                    )
                  )
                })
              )
            )
          })
        )
      )
    })
    
  })
}


# =============================================================================
# OPTIMIZATION FUNCTIONS
# =============================================================================

#' Optimize a single lineup using linear programming
#' @param players Data frame with player data
#' @param projection_col Column to optimize on
#' @param salary_cap Maximum salary
#' @param locked_players Players to lock in
#' @param excluded_players Players to exclude
#' @return Data frame with optimized lineup or NULL
optimize_lineup_lp <- function(players, projection_col, salary_cap,
                               locked_players = NULL, excluded_players = NULL) {
  
  # Filter excluded
  available <- players
  if (!is.null(excluded_players) && length(excluded_players) > 0) {
    available <- available %>% filter(!(player %in% excluded_players))
  }
  
  n <- nrow(available)
  if (n == 0) return(NULL)
  
  # Verify locked players exist
  if (!is.null(locked_players) && length(locked_players) > 0) {
    missing <- setdiff(locked_players, available$player)
    if (length(missing) > 0) return(NULL)
  }
  
  # Objective function
  objective <- available[[projection_col]]
  
  # Constraints
  constraint_matrix <- rbind(
    as.numeric(available$position == "QB"),        # QB = 1
    as.numeric(available$position == "RB"),        # RB >= 2
    as.numeric(available$position == "WR"),        # WR >= 3
    as.numeric(available$position == "TE"),        # TE >= 1
    as.numeric(available$position == "DST"),       # DST = 1
    as.numeric(available$position %in% c("RB", "WR", "TE")),  # FLEX total >= 6
    rep(1, n),                                      # Total = 9
    available$salary                                # Salary <= cap
  )
  
  constraint_dirs <- c("==", ">=", ">=", ">=", "==", ">=", "==", "<=")
  constraint_rhs <- c(1, 2, 3, 1, 1, 6, 9, salary_cap)
  
  # Add locked player constraints
  if (!is.null(locked_players) && length(locked_players) > 0) {
    for (player in locked_players) {
      if (player %in% available$player) {
        constraint_matrix <- rbind(
          constraint_matrix,
          as.numeric(available$player == player)
        )
        constraint_dirs <- c(constraint_dirs, "==")
        constraint_rhs <- c(constraint_rhs, 1)
      }
    }
  }
  
  # Solve
  solution <- lp(
    direction = "max",
    objective.in = objective,
    const.mat = constraint_matrix,
    const.dir = constraint_dirs,
    const.rhs = constraint_rhs,
    all.bin = TRUE
  )
  
  if (solution$status != 0) return(NULL)
  
  # Extract lineup
  lineup <- available[solution$solution == 1, ] %>%
    mutate(
      projection = .data[[projection_col]],
      value = projection / salary
    ) %>%
    select(player, position, team, salary, projection, value,
           any_of(c("opponent", "home", "headshot_url", "team_bg_color"))) %>%
    arrange(match(position, c("QB", "RB", "WR", "TE", "DST")))
  
  lineup
}


#' Generate multiple lineups with variance and conditional stacking rules
#' @param players Data frame with player data
#' @param num_lineups Number of lineups to generate
#' @param salary_cap Maximum salary
#' @param variance_pct Variance percentage
#' @param locked_players Players to lock in
#' @param adjustments List of player adjustments (name -> pct)
#' @param stacking_rules List of conditional stacking rules
#' @param stack_game Game key for game stack
#' @param min_game_players Min players from selected game
#' @return List of lineup data frames
generate_lineups_with_stacking <- function(players, num_lineups, salary_cap,
                                           variance_pct = 10, locked_players = NULL,
                                           adjustments = list(),
                                           stacking_rules = list(),
                                           stack_game = "",
                                           min_game_players = 4) {
  
  lineups <- list()
  lineup_signatures <- character(0)
  
  # Apply adjustments to create base projections
  players_base <- players %>%
    mutate(
      base_projection = sapply(1:n(), function(i) {
        adj_pct <- adjustments[[player[i]]] %||% 0
        blended[i] * (1 + adj_pct / 100)
      })
    )
  
  # Parse game stack if applicable
  game_teams <- if (stack_game != "") {
    strsplit(stack_game, "_")[[1]]
  } else {
    NULL
  }
  
  attempts <- 0
  max_attempts <- num_lineups * 50
  
  while (length(lineups) < num_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Apply random variance - FRESH for each lineup
    set.seed(Sys.time() + attempts)
    
    players_varied <- players_base %>%
      mutate(
        variance_mult = 1 + runif(n(), -variance_pct/100, variance_pct/100),
        varied_projection = base_projection * variance_mult
      )
    
    # Optimize with varied projections
    lineup <- optimize_lineup_lp(
      players = players_varied,
      projection_col = "varied_projection",
      salary_cap = salary_cap,
      locked_players = locked_players
    )
    
    if (is.null(lineup)) next
    
    # Check stacking rules (conditional + game stack)
    if (!check_stacking_rules(lineup, players_base, stacking_rules, game_teams, min_game_players)) {
      next
    }
    
    # Check for duplicates
    sig <- paste(sort(lineup$player), collapse = "|")
    if (sig %in% lineup_signatures) next
    
    # Restore base projections (with adjustments applied)
    lineup <- lineup %>%
      mutate(projection = players_base$base_projection[match(player, players_base$player)])
    
    lineups[[length(lineups) + 1]] <- lineup
    lineup_signatures <- c(lineup_signatures, sig)
  }
  
  lineups
}


#' Check if lineup meets conditional stacking rules
#' @param lineup Lineup data frame
#' @param players Full player data
#' @param stacking_rules List of conditional stacking rules
#' @param game_teams Teams in selected game (for game stack)
#' @param min_game_players Min players from selected game
#' @return TRUE if lineup passes all applicable stacking rules
check_stacking_rules <- function(lineup, players, stacking_rules, game_teams, min_game_players) {
  
  # Check game stack requirement first (applies to all lineups)
  if (!is.null(game_teams) && length(game_teams) >= 2) {
    game_players <- lineup %>%
      filter(team %in% game_teams) %>%
      nrow()
    
    if (game_players < min_game_players) {
      return(FALSE)
    }
  }
  
  # If no conditional rules, lineup passes
  if (length(stacking_rules) == 0) return(TRUE)
  
  # Get QB info
  qb_row <- lineup %>% filter(position == "QB")
  if (nrow(qb_row) == 0) return(FALSE)
  
  qb_name <- qb_row$player[1]
  qb_team <- qb_row$team[1]
  
  # Get opponent from player data
  qb_player_data <- players %>% filter(player == qb_name)
  qb_opponent <- if (nrow(qb_player_data) > 0 && "opponent" %in% names(qb_player_data)) {
    qb_player_data$opponent[1]
  } else {
    ""
  }
  
  # Find applicable rule for this QB
  applicable_rule <- NULL
  for (rule in stacking_rules) {
    if (qb_name %in% rule$qbs) {
      applicable_rule <- rule
      break
    }
  }
  
  # If no rule applies to this QB, lineup passes
  if (is.null(applicable_rule)) return(TRUE)
  
  # Check same-team stack requirement
  if (applicable_rule$same_team_min > 0) {
    same_team_players <- lineup %>%
      filter(
        position %in% applicable_rule$same_team_positions,
        team == qb_team
      ) %>%
      nrow()
    
    if (same_team_players < applicable_rule$same_team_min) {
      return(FALSE)
    }
  }
  
  # Check opponent stack requirement
  if (applicable_rule$opp_min > 0 && qb_opponent != "") {
    opp_players <- lineup %>%
      filter(
        position %in% applicable_rule$opp_positions,
        team == qb_opponent
      ) %>%
      nrow()
    
    if (opp_players < applicable_rule$opp_min) {
      return(FALSE)
    }
  }
  
  TRUE
}