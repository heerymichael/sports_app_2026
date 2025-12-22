# =============================================================================
# Module: NFL Projections
# 
# Player projection tables with filtering, sorting, and headshots
# Styled in Stabilo illustrated aesthetic
# =============================================================================

#' NFL Projections UI
#' @param id Module namespace ID
nfl_projections_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("nfl_projections_ui() called with id:", id, level = "INFO")
  
  # Get available seasons at UI build time
  seasons <- get_available_seasons()
  log_debug("UI build - seasons available:", paste(seasons, collapse = ", "), level = "INFO")
  
  # Get weeks for first season (if available)
  if (length(seasons) > 0) {
    weeks <- get_available_weeks(seasons[1])
    slates <- get_available_slates(seasons[1], weeks[1])
  } else {
    weeks <- c()
    slates <- c("main")
  }
  
  log_debug("UI build - weeks available:", paste(weeks, collapse = ", "), level = "INFO")
  
  # Build season choices
  if (length(seasons) > 0) {
    season_choices <- setNames(as.character(seasons), as.character(seasons))
    season_selected <- as.character(seasons[1])
  } else {
    season_choices <- c("No data found" = "")
    season_selected <- NULL
  }
  
  # Build week choices
  if (length(weeks) > 0) {
    week_choices <- setNames(as.character(weeks), paste("Week", weeks))
    week_selected <- as.character(weeks[1])
  } else {
    week_choices <- c("No weeks found" = "")
    week_selected <- NULL
  }
  
  # Build slate choices
  slate_choices <- c("Main" = "main", "Late" = "late")[c("main", "late") %in% slates]
  if (length(slate_choices) == 0) slate_choices <- c("Main" = "main")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("NFL Projections"),
      tags$p(class = "text-muted", "Player projections and salary analysis for FanTeam DFS")
    ),
    
    # Filters card
    ui_card(
      title = "Filters",
      color = NFL_CARD_COLOR,
      
      # Row 1: Season, Week, Slate
      fluidRow(
        column(4, 
               selectInput(ns("season"), "Season",
                           choices = season_choices,
                           selected = season_selected
               )
        ),
        column(4,
               selectInput(ns("week"), "Week",
                           choices = week_choices,
                           selected = week_selected
               )
        ),
        column(4,
               selectInput(ns("slate"), "Slate",
                           choices = slate_choices,
                           selected = "main"
               )
        )
      ),
      
      # Row 2: Position, Team, Heatmap (aligned with row above)
      fluidRow(
        column(4,
               selectInput(ns("position"), "Position",
                           choices = c("All Positions" = "all", "QB", "RB", "WR", "TE", "DST"),
                           selected = "all"
               )
        ),
        column(4,
               # Team selector with logo rendering
               selectizeInput(ns("team"), "Team",
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
        column(4,
               selectInput(ns("heatmap"), "Heatmap",
                           choices = c("None" = "none", "Projection" = "full", "Ceiling" = "ceiling", 
                                       "Blended" = "blended", "Salary" = "salary", "Value" = "value"),
                           selected = "none"
               )
        )
      )
    ),
    
    tags$br(),
    
    # Projections table
    uiOutput(ns("projections_table"))
  )
}

#' NFL Projections Server
#' @param id Module namespace ID
nfl_projections_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("nfl_projections_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      player_data = NULL,
      loading = FALSE,
      initialized = FALSE
    )
    
    # Sort state
    sort_col <- reactiveVal("salary")
    sort_dir <- reactiveVal("desc")
    
    # =========================================================================
    # DATA LOAD - triggered by input changes
    # Uses req() to wait for valid inputs while maintaining reactive dependencies
    # Also includes a retry mechanism for timing issues with dynamic UI
    # =========================================================================
    
    # Retry counter for initial load
    load_attempts <- reactiveVal(0)
    
    observe({
      # Read inputs first to establish reactive dependencies
      season <- input$season
      week <- input$week
      slate <- input$slate
      
      # Log current state
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
      
      # req() stops execution but KEEPS reactive dependency
      req(season, week, slate)
      req(season != "", week != "", slate != "")
      req(season != "No data found", week != "No weeks found")
      
      log_debug(">>> DATA LOAD TRIGGERED", level = "INFO")
      log_debug(">>>   Season:", season, level = "INFO")
      log_debug(">>>   Week:", week, level = "INFO")
      log_debug(">>>   Slate:", slate, level = "INFO")
      
      rv$loading <- TRUE
      
      tryCatch({
        rv$player_data <- load_week_data_with_headshots(
          season, 
          as.numeric(week), 
          slate
        )
        
        if (!is.null(rv$player_data)) {
          log_debug(">>> Data loaded successfully:", nrow(rv$player_data), "players", level = "INFO")
        } else {
          log_debug(">>> Data load returned NULL", level = "WARN")
        }
        
      }, error = function(e) {
        log_debug(">>> Error loading data:", e$message, level = "ERROR")
        showNotification(paste("Error loading data:", e$message), type = "error")
        rv$player_data <- NULL
      })
      
      rv$loading <- FALSE
      rv$initialized <- TRUE
    })
    
    # =========================================================================
    # UPDATE WEEKS when season changes
    # =========================================================================
    observeEvent(input$season, {
      log_debug(">>> Season changed to:", input$season, level = "INFO")
      
      # Skip if empty or placeholder
      if (is.null(input$season) || input$season == "" || input$season == "No data found") {
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
                          choices = c("No weeks found" = ""),
                          selected = NULL
        )
        log_debug(">>> No weeks available for this season", level = "WARN")
      }
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # UPDATE SLATES when week changes
    # =========================================================================
    observeEvent(input$week, {
      log_debug(">>> Week changed to:", input$week, level = "INFO")
      
      # Skip if empty
      if (is.null(input$season) || input$season == "" ||
          is.null(input$week) || input$week == "") {
        log_debug(">>> Season or week empty, skipping slate update", level = "DEBUG")
        return()
      }
      
      slates <- get_available_slates(input$season, as.numeric(input$week))
      log_debug(">>> Slates found:", paste(slates, collapse = ", "), level = "INFO")
      
      if (length(slates) > 0) {
        slate_choices <- c("Main" = "main", "Late" = "late")[c("main", "late") %in% slates]
        updateSelectInput(session, "slate",
                          choices = slate_choices,
                          selected = if ("main" %in% slates) "main" else slates[1]
        )
      }
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # UPDATE TEAM DROPDOWN when data loads
    # =========================================================================
    observe({
      log_debug(">>> Updating team dropdown", level = "DEBUG")
      
      if (is.null(rv$player_data) || nrow(rv$player_data) == 0) {
        updateSelectizeInput(session, "team",
                             choices = c("All Teams" = "all"),
                             selected = "all"
        )
        return()
      }
      
      teams <- sort(unique(rv$player_data$team))
      team_choices <- c("All Teams" = "all")
      
      for (team in teams) {
        full_name <- get_team_full_name(team)
        if (is.na(full_name)) full_name <- team
        team_choices[full_name] <- team
      }
      
      log_debug(">>> Team choices:", length(team_choices) - 1, "teams", level = "DEBUG")
      
      updateSelectizeInput(session, "team",
                           choices = team_choices,
                           selected = "all"
      )
    })
    
    # =========================================================================
    # FILTERED DATA
    # =========================================================================
    filtered_data <- reactive({
      log_debug(">>> filtered_data() reactive triggered", level = "DEBUG")
      
      if (is.null(rv$player_data) || nrow(rv$player_data) == 0) {
        log_debug(">>> No player data available", level = "DEBUG")
        return(NULL)
      }
      
      data <- rv$player_data
      initial_count <- nrow(data)
      
      # Position filter
      if (!is.null(input$position) && input$position != "all") {
        data <- data %>% filter(position == input$position)
        log_debug(">>> Position filter applied:", input$position, "->", nrow(data), "rows", level = "DEBUG")
      }
      
      # Team filter
      if (!is.null(input$team) && input$team != "all") {
        data <- data %>% filter(team == input$team)
        log_debug(">>> Team filter applied:", input$team, "->", nrow(data), "rows", level = "DEBUG")
      }
      
      # Apply sorting
      col <- sort_col()
      dir <- sort_dir()
      
      if (dir == "desc") {
        data <- data %>% arrange(desc(.data[[col]]))
      } else {
        data <- data %>% arrange(.data[[col]])
      }
      
      log_debug(">>> Filtered data:", nrow(data), "of", initial_count, "rows", level = "DEBUG")
      data
    })
    
    # =========================================================================
    # SORT HANDLERS
    # =========================================================================
    observeEvent(input$sort_salary, {
      log_debug(">>> Sort by salary clicked", level = "DEBUG")
      if (sort_col() == "salary") {
        sort_dir(if (sort_dir() == "desc") "asc" else "desc")
      } else {
        sort_col("salary")
        sort_dir("desc")
      }
    })
    
    observeEvent(input$sort_projection, {
      log_debug(">>> Sort by projection clicked", level = "DEBUG")
      if (sort_col() == "full") {
        sort_dir(if (sort_dir() == "desc") "asc" else "desc")
      } else {
        sort_col("full")
        sort_dir("desc")
      }
    })
    
    observeEvent(input$sort_ceiling, {
      log_debug(">>> Sort by ceiling clicked", level = "DEBUG")
      if (sort_col() == "ceiling") {
        sort_dir(if (sort_dir() == "desc") "asc" else "desc")
      } else {
        sort_col("ceiling")
        sort_dir("desc")
      }
    })
    
    observeEvent(input$sort_blended, {
      log_debug(">>> Sort by blended clicked", level = "DEBUG")
      if (sort_col() == "blended") {
        sort_dir(if (sort_dir() == "desc") "asc" else "desc")
      } else {
        sort_col("blended")
        sort_dir("desc")
      }
    })
    
    observeEvent(input$sort_value, {
      log_debug(">>> Sort by value clicked", level = "DEBUG")
      if (sort_col() == "value") {
        sort_dir(if (sort_dir() == "desc") "asc" else "desc")
      } else {
        sort_col("value")
        sort_dir("desc")
      }
    })
    
    # =========================================================================
    # PROJECTIONS TABLE
    # =========================================================================
    output$projections_table <- renderUI({
      log_debug(">>> Rendering projections table", level = "DEBUG")
      
      if (rv$loading) {
        return(
          div(
            class = "card",
            style = "padding: 3rem; text-align: center;",
            tags$p(style = "font-size: 1.1rem; color: var(--text-muted);", "Loading player data...")
          )
        )
      }
      
      data <- filtered_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          div(
            class = "card",
            style = "padding: 3rem; text-align: center;",
            tags$p(style = "font-size: 1.1rem; color: var(--text-muted);", 
                   "No players found. Select a season and week above.")
          )
        )
      }
      
      log_debug(">>> Building table with", nrow(data), "rows", level = "DEBUG")
      
      # Sort indicator helper
      sort_indicator <- function(col_name) {
        if (sort_col() == col_name) {
          if (sort_dir() == "desc") " â–¼" else " â–²"
        } else {
          ""
        }
      }
      
      # Heatmap color helper - diverging scale for value, sequential for others
      get_heatmap_color <- function(value, col_name) {
        if (is.null(input$heatmap) || input$heatmap == "none" || input$heatmap != col_name) {
          return("")
        }
        
        col_values <- data[[col_name]]
        
        # For "value" column, use diverging scale with midpoint at 1.0
        if (col_name == "value") {
          # Diverging scale: Coral (low) -> White (1.0) -> Teal (high)
          # Coral: #D08770 = rgb(208, 135, 112)
          # White: #FFFFFF = rgb(255, 255, 255)
          # Teal:  #8FBCBB = rgb(143, 188, 187)
          
          midpoint <- 1.0
          
          if (value < midpoint) {
            # Below midpoint: interpolate from coral to white
            # Use range from minimum to midpoint
            min_val <- min(col_values, na.rm = TRUE)
            if (min_val >= midpoint) min_val <- 0.5  # fallback
            
            # Normalize: 0 = min_val (full coral), 1 = midpoint (white)
            t <- (value - min_val) / (midpoint - min_val)
            t <- max(0, min(1, t))  # clamp to 0-1
            
            # Interpolate coral -> white
            r <- round(208 + (255 - 208) * t)
            g <- round(135 + (255 - 135) * t)
            b <- round(112 + (255 - 112) * t)
            
          } else {
            # At or above midpoint: interpolate from white to teal
            max_val <- max(col_values, na.rm = TRUE)
            if (max_val <= midpoint) max_val <- 2.0  # fallback
            
            # Normalize: 0 = midpoint (white), 1 = max_val (full teal)
            t <- (value - midpoint) / (max_val - midpoint)
            t <- max(0, min(1, t))  # clamp to 0-1
            
            # Interpolate white -> teal
            r <- round(255 + (143 - 255) * t)
            g <- round(255 + (188 - 255) * t)
            b <- round(255 + (187 - 255) * t)
          }
          
          return(sprintf("background-color: rgb(%d, %d, %d);", r, g, b))
        }
        
        # For other columns, use sequential scale (white -> teal)
        min_val <- min(col_values, na.rm = TRUE)
        max_val <- max(col_values, na.rm = TRUE)
        
        if (max_val == min_val) return("")
        
        # Normalize 0-1
        t <- (value - min_val) / (max_val - min_val)
        
        # Interpolate white -> teal light (#A3D1D1 = rgb(163, 209, 209))
        r <- round(255 + (163 - 255) * t)
        g <- round(255 + (209 - 255) * t)
        b <- round(255 + (209 - 255) * t)
        
        sprintf("background-color: rgb(%d, %d, %d);", r, g, b)
      }
      
      # Build table
      ui_card(
        title = sprintf("Player Projections (%d players)", nrow(data)),
        color = NFL_CARD_COLOR,
        
        div(
          style = "overflow-x: auto;",
          tags$table(
            class = "projections-table",
            style = "width: 100%; border-collapse: collapse;",
            
            # Header
            tags$thead(
              tags$tr(
                tags$th(style = "text-align: left; padding: 0.75rem 1rem;", "Player"),
                tags$th(style = "text-align: center; padding: 0.75rem; width: 70px;", "Pos"),
                tags$th(
                  style = "text-align: center; padding: 0.75rem; width: 100px; cursor: pointer;",
                  onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("sort_salary")),
                  HTML(paste0("Salary", sort_indicator("salary")))
                ),
                tags$th(
                  style = "text-align: center; padding: 0.75rem; width: 100px; cursor: pointer;",
                  onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("sort_projection")),
                  HTML(paste0("Proj", sort_indicator("full")))
                ),
                tags$th(
                  style = "text-align: center; padding: 0.75rem; width: 100px; cursor: pointer;",
                  onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("sort_ceiling")),
                  HTML(paste0("Ceiling", sort_indicator("ceiling")))
                ),
                tags$th(
                  style = "text-align: center; padding: 0.75rem; width: 100px; cursor: pointer;",
                  onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("sort_blended")),
                  HTML(paste0("Blended", sort_indicator("blended")))
                ),
                tags$th(
                  style = "text-align: center; padding: 0.75rem; width: 90px; cursor: pointer;",
                  onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("sort_value")),
                  HTML(paste0("Value", sort_indicator("value")))
                )
              )
            ),
            
            # Body
            tags$tbody(
              lapply(1:nrow(data), function(i) {
                row <- data[i, ]
                tags$tr(
                  # Player cell with headshot
                  tags$td(
                    style = "padding: 0.75rem 1rem;",
                    div(
                      style = "display: flex; align-items: center; gap: 0.75rem;",
                      create_headshot_html(
                        row$headshot_url, 
                        row$team_bg_color, 
                        "small", 
                        row$position, 
                        row$team
                      ),
                      div(
                        div(style = "font-weight: 600;", row$player),
                        create_matchup_html(row$team, row$opponent, row$home)
                      )
                    )
                  ),
                  
                  # Position
                  tags$td(
                    style = "text-align: center; padding: 0.75rem;",
                    span(
                      class = "position-badge",
                      row$position
                    )
                  ),
                  
                  # Salary
                  tags$td(
                    style = sprintf("text-align: center; padding: 0.75rem; font-weight: 600; %s", 
                                    get_heatmap_color(row$salary, "salary")),
                    sprintf("$%.1f", row$salary)
                  ),
                  
                  # Projection
                  tags$td(
                    style = sprintf("text-align: center; padding: 0.75rem; font-weight: 600; %s",
                                    get_heatmap_color(row$full, "full")),
                    sprintf("%.1f", row$full)
                  ),
                  
                  # Ceiling
                  tags$td(
                    style = sprintf("text-align: center; padding: 0.75rem; font-weight: 600; %s",
                                    get_heatmap_color(row$ceiling, "ceiling")),
                    sprintf("%.1f", row$ceiling)
                  ),
                  
                  # Blended
                  tags$td(
                    style = sprintf("text-align: center; padding: 0.75rem; font-weight: 700; color: var(--accent-coral); %s",
                                    get_heatmap_color(row$blended, "blended")),
                    sprintf("%.1f", row$blended)
                  ),
                  
                  # Value
                  tags$td(
                    style = sprintf("text-align: center; padding: 0.75rem; font-weight: 600; %s",
                                    get_heatmap_color(row$value, "value")),
                    sprintf("%.2f", row$value)
                  )
                )
              })
            )
          )
        )
      )
    })
    
    # =========================================================================
    # DEBUG: Log reactive state (commented out to prevent flicker)
    # Uncomment temporarily when debugging reactive issues
    # =========================================================================
    # observe({
    #   invalidateLater(10000, session)  # Every 10 seconds
    #   isolate({
    #     log_debug("--- Periodic State Check ---", level = "DEBUG")
    #     log_debug("  initialized:", rv$initialized, level = "DEBUG")
    #     log_debug("  input$season:", input$season, level = "DEBUG")
    #     log_debug("  input$week:", input$week, level = "DEBUG")
    #     log_debug("  input$slate:", input$slate, level = "DEBUG")
    #     log_debug("  player_data rows:", if(is.null(rv$player_data)) "NULL" else nrow(rv$player_data), level = "DEBUG")
    #     log_debug("----------------------------", level = "DEBUG")
    #   })
    # })
    
  })
}