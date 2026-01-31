# =============================================================================
# Module: Soccer Handbuild
# 
# Manual lineup building with optimization support for FanTeam contests
# 
# Features:
#   - Player pool table combining historical stats and weekly context
#   - Filters: Position, salary range, team, min Sortino, grade threshold
#   - Selection UI: Pick players, track budget/slots
#   - Simple optimizer (max projected pts within constraints)
#
# Data Sources:
#   - Historical: Player Stats module (PPG, Sortino, Floor, Ceiling, Grades)
#   - Weekly: Match Ups module (Salary, Opponent, Opp DEF rating, H/A)
# =============================================================================

library(lpSolve)

# =============================================================================
# UI FUNCTION
# =============================================================================

#' Soccer Handbuild UI
#' @param id Module namespace ID
soccer_handbuild_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_handbuild_ui() called with id:", id, level = "INFO")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Soccer Handbuild"),
      tags$p(class = "text-muted", "Build lineups manually with optimization assistance")
    ),
    
    # ==========================================================================
    # FILTERS CARD
    # ==========================================================================
    ui_card(
      title = "Filters",
      color = "sage",
      
      fluidRow(
        column(3,
               selectInput(
                 ns("contest_type"),
                 "Contest Type",
                 choices = c("FanTeam Classic" = "classic", "FanTeam Showdown" = "showdown"),
                 selected = "classic"
               )
        ),
        column(3,
               shinyWidgets::pickerInput(
                 ns("position_filter"),
                 "Position",
                 choices = c("All", "GK", "DEF", "MID", "FWD"),
                 selected = "All",
                 multiple = TRUE,
                 options = shinyWidgets::pickerOptions(
                   actionsBox = TRUE,
                   noneSelectedText = "All Positions"
                 )
               )
        ),
        column(3,
               shinyWidgets::pickerInput(
                 ns("team_filter"),
                 "Team",
                 choices = NULL,
                 selected = NULL,
                 multiple = TRUE,
                 options = shinyWidgets::pickerOptions(
                   actionsBox = TRUE,
                   liveSearch = TRUE,
                   noneSelectedText = "All Teams"
                 )
               )
        ),
        column(3,
               div(
                 style = "padding-top: 25px;",
                 actionButton(
                   ns("refresh_data"),
                   "Refresh Data",
                   icon = icon("refresh"),
                   class = "btn-secondary"
                 )
               )
        )
      ),
      
      fluidRow(
        column(3,
               sliderInput(
                 ns("salary_range"),
                 "Salary Range (M)",
                 min = 3,
                 max = 15,
                 value = c(3, 15),
                 step = 0.5
               )
        ),
        column(3,
               sliderInput(
                 ns("min_sortino"),
                 "Min Sortino",
                 min = 0,
                 max = 5,
                 value = 0,
                 step = 0.25
               )
        ),
        column(3,
               selectInput(
                 ns("min_cash_grade"),
                 "Min Cash Grade",
                 choices = c("Any" = "", "A+" = "A+", "A" = "A", "B+" = "B+", "B" = "B", "C+" = "C+", "C" = "C"),
                 selected = ""
               )
        ),
        column(3,
               selectInput(
                 ns("min_gpp_grade"),
                 "Min GPP Grade",
                 choices = c("Any" = "", "A+" = "A+", "A" = "A", "B+" = "B+", "B" = "B", "C+" = "C+", "C" = "C"),
                 selected = ""
               )
        )
      )
    ),
    
    # ==========================================================================
    # MAIN CONTENT: Player Pool + Lineup Builder
    # ==========================================================================
    fluidRow(
      # Left: Player Pool (8 cols)
      column(
        8,
        ui_card(
          title = "Player Pool",
          color = "sage",
          
          # Pool stats
          div(
            style = "margin-bottom: 1rem;",
            uiOutput(ns("pool_stats"))
          ),
          
          # Player table
          reactableOutput(ns("player_pool_table"))
        )
      ),
      
      # Right: Lineup Builder (4 cols)
      column(
        4,
        ui_card(
          title = "Lineup Builder",
          color = "sage",
          
          # Budget tracker
          div(
            style = "margin-bottom: 1rem; padding: 0.75rem; background: var(--bg-secondary); border-radius: 6px;",
            fluidRow(
              column(6,
                     div(
                       style = "text-align: center;",
                       tags$span("Budget", style = "font-size: 0.75rem; color: var(--text-muted);"),
                       tags$div(
                         style = "font-size: 1.5rem; font-weight: 700;",
                         textOutput(ns("budget_remaining"), inline = TRUE),
                         tags$span("M", style = "font-size: 0.9rem; font-weight: 500;")
                       )
                     )
              ),
              column(6,
                     div(
                       style = "text-align: center;",
                       tags$span("Slots", style = "font-size: 0.75rem; color: var(--text-muted);"),
                       tags$div(
                         style = "font-size: 1.5rem; font-weight: 700;",
                         textOutput(ns("slots_filled"), inline = TRUE)
                       )
                     )
              )
            )
          ),
          
          # Lineup slots
          uiOutput(ns("lineup_slots")),
          
          # Action buttons
          div(
            style = "margin-top: 1rem; display: flex; gap: 0.5rem;",
            actionButton(
              ns("clear_lineup"),
              "Clear",
              icon = icon("trash"),
              class = "btn-secondary",
              style = "flex: 1;"
            ),
            actionButton(
              ns("optimize_lineup"),
              "Optimize",
              icon = icon("magic"),
              class = "btn-primary",
              style = "flex: 1;"
            )
          ),
          
          # Projected points
          div(
            style = "margin-top: 1rem; padding: 0.75rem; background: var(--accent-sage); color: white; border-radius: 6px; text-align: center;",
            tags$span("Projected Points", style = "font-size: 0.75rem; opacity: 0.9;"),
            tags$div(
              style = "font-size: 1.75rem; font-weight: 700;",
              textOutput(ns("projected_points"), inline = TRUE)
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # GENERATED LINEUPS SECTION
    # ==========================================================================
    tags$br(),
    
    ui_card(
      title = "Generated Lineups",
      color = "sage",
      collapsed = TRUE,
      
      fluidRow(
        column(3,
               numericInput(
                 ns("num_lineups"),
                 "Number of Lineups",
                 value = 5,
                 min = 1,
                 max = 20,
                 step = 1
               )
        ),
        column(3,
               sliderInput(
                 ns("variance"),
                 "Variance",
                 min = 0,
                 max = 100,
                 value = 20,
                 step = 5,
                 post = "%"
               )
        ),
        column(3,
               div(
                 style = "padding-top: 25px;",
                 actionButton(
                   ns("generate_lineups"),
                   "Generate Lineups",
                   icon = icon("bolt"),
                   class = "btn-primary"
                 )
               )
        )
      ),
      
      tags$hr(style = "margin: 1rem 0;"),
      
      uiOutput(ns("generated_lineups_output"))
    )
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

#' Soccer Handbuild Server
#' @param id Module namespace ID
soccer_handbuild_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # INITIALIZATION
    # =========================================================================
    log_debug("========================================", level = "INFO")
    log_debug("soccer_handbuild_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    rv <- reactiveValues(
      player_data = NULL,          # Combined player pool data
      historical_stats = NULL,     # From Player Stats module
      weekly_context = NULL,       # From Match Ups module
      lineup_slots = list(),       # Current lineup selections
      generated_lineups = NULL,    # Generated lineup results
      initialized = FALSE,
      loading = FALSE,
      error_message = NULL
    )
    
    # =========================================================================
    # CONSTANTS
    # =========================================================================
    SALARY_CAP <- 100  # FanTeam salary cap in millions
    
    LINEUP_STRUCTURE <- list(
      classic = list(
        slots = c("GK", "DEF", "DEF", "DEF", "DEF", "MID", "MID", "MID", "FWD", "FWD", "FWD"),
        total = 11
      ),
      showdown = list(
        slots = c("CAPTAIN", "FLEX", "FLEX", "FLEX", "FLEX", "FLEX"),
        total = 6
      )
    )
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    observe({
      log_debug(">>> Initial data load observer triggered", level = "DEBUG")
      
      if (rv$initialized) return()
      
      rv$loading <- TRUE
      rv$error_message <- NULL
      
      tryCatch({
        log_debug("Loading player pool data...", level = "INFO")
        
        # TODO: Load and combine data from Player Stats and Match Ups modules
        # For now, create placeholder structure
        rv$player_data <- data.frame(
          name = character(),
          team = character(),
          pos = character(),
          salary = numeric(),
          ppg = numeric(),
          sortino = numeric(),
          floor = numeric(),
          ceiling = numeric(),
          cash_grade = character(),
          gpp_grade = character(),
          opponent = character(),
          home_away = character(),
          stringsAsFactors = FALSE
        )
        
        rv$initialized <- TRUE
        rv$last_refresh <- Sys.time()
        
        log_debug("Player pool data initialized (placeholder)", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error loading player pool:", e$message, level = "ERROR")
        rv$error_message <- e$message
      })
      
      rv$loading <- FALSE
    })
    
    # Manual refresh
    observeEvent(input$refresh_data, {
      log_debug(">>> Manual refresh triggered", level = "INFO")
      rv$initialized <- FALSE
    })
    
    # =========================================================================
    # FILTERED DATA
    # =========================================================================
    
    filtered_pool <- reactive({
      req(rv$player_data)
      
      data <- rv$player_data
      
      log_debug("Filtering player pool, starting with", nrow(data), "players", level = "DEBUG")
      
      # Position filter
      pos_filter <- input$position_filter
      if (!is.null(pos_filter) && !"All" %in% pos_filter && length(pos_filter) > 0) {
        data <- data %>% filter(pos %in% pos_filter)
      }
      
      # Team filter
      team_filter <- input$team_filter
      if (!is.null(team_filter) && length(team_filter) > 0) {
        data <- data %>% filter(team %in% team_filter)
      }
      
      # Salary filter
      salary_range <- input$salary_range
      if (!is.null(salary_range)) {
        data <- data %>% filter(salary >= salary_range[1] & salary <= salary_range[2])
      }
      
      # Sortino filter
      min_sortino <- input$min_sortino
      if (!is.null(min_sortino) && min_sortino > 0) {
        data <- data %>% filter(sortino >= min_sortino)
      }
      
      # Grade filters
      min_cash <- input$min_cash_grade
      if (!is.null(min_cash) && min_cash != "") {
        grade_order <- c("A+", "A", "B+", "B", "C+", "C", "D", "F")
        min_idx <- which(grade_order == min_cash)
        data <- data %>% filter(cash_grade %in% grade_order[1:min_idx])
      }
      
      min_gpp <- input$min_gpp_grade
      if (!is.null(min_gpp) && min_gpp != "") {
        grade_order <- c("A+", "A", "B+", "B", "C+", "C", "D", "F")
        min_idx <- which(grade_order == min_gpp)
        data <- data %>% filter(gpp_grade %in% grade_order[1:min_idx])
      }
      
      log_debug("Filtered to", nrow(data), "players", level = "DEBUG")
      
      return(data)
    })
    
    # =========================================================================
    # POOL STATS
    # =========================================================================
    
    output$pool_stats <- renderUI({
      data <- filtered_pool()
      
      n_players <- if (!is.null(data)) nrow(data) else 0
      n_teams <- if (!is.null(data) && nrow(data) > 0) length(unique(data$team)) else 0
      
      div(
        style = "display: flex; gap: 1.5rem;",
        tags$span(
          style = "color: var(--text-muted); font-size: 0.85rem;",
          tags$strong(n_players), " players"
        ),
        tags$span(
          style = "color: var(--text-muted); font-size: 0.85rem;",
          tags$strong(n_teams), " teams"
        )
      )
    })
    
    # =========================================================================
    # PLAYER POOL TABLE
    # =========================================================================
    
    output$player_pool_table <- renderReactable({
      data <- filtered_pool()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          reactable(
            data.frame(Message = "No players match the current filters"),
            theme = app_reactable_theme()
          )
        )
      }
      
      reactable(
        data,
        theme = app_reactable_theme(),
        columns = list(
          name = colDef(name = "Player", minWidth = 140),
          team = colDef(name = "Team", minWidth = 80),
          pos = colDef(name = "Pos", minWidth = 50, align = "center"),
          salary = colDef(name = "Salary", minWidth = 60, align = "center",
                          cell = function(value) sprintf("%.1fM", value)),
          ppg = colDef(name = "PPG", minWidth = 55, align = "center",
                       format = colFormat(digits = 1)),
          sortino = colDef(name = "Sortino", minWidth = 60, align = "center",
                           format = colFormat(digits = 2)),
          floor = colDef(name = "Floor", minWidth = 55, align = "center",
                         format = colFormat(digits = 1)),
          ceiling = colDef(name = "Ceil", minWidth = 55, align = "center",
                           format = colFormat(digits = 1)),
          cash_grade = colDef(name = "Cash", minWidth = 50, align = "center"),
          gpp_grade = colDef(name = "GPP", minWidth = 50, align = "center"),
          opponent = colDef(name = "Opp", minWidth = 70),
          home_away = colDef(name = "H/A", minWidth = 40, align = "center")
        ),
        searchable = TRUE,
        sortable = TRUE,
        defaultSorted = "ppg",
        defaultSortOrder = "desc",
        pagination = TRUE,
        defaultPageSize = 20,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(20, 50, 100),
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        selection = "multiple",
        onClick = "select"
      )
    })
    
    # =========================================================================
    # LINEUP BUILDER
    # =========================================================================
    
    output$budget_remaining <- renderText({
      # TODO: Calculate from lineup slots
      sprintf("%.1f", SALARY_CAP)
    })
    
    output$slots_filled <- renderText({
      contest <- input$contest_type %||% "classic"
      structure <- LINEUP_STRUCTURE[[contest]]
      filled <- length(rv$lineup_slots)
      sprintf("%d / %d", filled, structure$total)
    })
    
    output$lineup_slots <- renderUI({
      contest <- input$contest_type %||% "classic"
      structure <- LINEUP_STRUCTURE[[contest]]
      
      # Create slot UI for each position
      slot_uis <- lapply(seq_along(structure$slots), function(i) {
        pos <- structure$slots[i]
        player <- rv$lineup_slots[[i]]
        
        if (is.null(player)) {
          # Empty slot
          div(
            style = "display: flex; align-items: center; padding: 0.5rem; margin-bottom: 0.5rem; background: var(--bg-secondary); border: 2px dashed var(--border); border-radius: 6px;",
            div(
              style = sprintf("background: %s; color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;", 
                              get_position_color(pos)),
              pos
            ),
            tags$span("Empty", style = "color: var(--text-muted); font-style: italic;")
          )
        } else {
          # Filled slot
          div(
            style = "display: flex; align-items: center; padding: 0.5rem; margin-bottom: 0.5rem; background: white; border: 2px solid var(--accent-sage); border-radius: 6px;",
            div(
              style = sprintf("background: %s; color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
                              get_position_color(player$pos)),
              player$pos
            ),
            div(
              style = "flex: 1;",
              tags$strong(player$name, style = "font-size: 0.85rem;"),
              tags$span(sprintf(" %.1fM", player$salary), style = "color: var(--text-muted); font-size: 0.75rem;")
            ),
            actionButton(
              ns(paste0("remove_", i)),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("remove_slot"), i)
            )
          )
        }
      })
      
      tagList(slot_uis)
    })
    
    output$projected_points <- renderText({
      # TODO: Calculate from lineup
      "0.0"
    })
    
    # Clear lineup
    observeEvent(input$clear_lineup, {
      log_debug("Clearing lineup", level = "INFO")
      rv$lineup_slots <- list()
    })
    
    # Remove slot
    observeEvent(input$remove_slot, {
      slot_idx <- input$remove_slot
      log_debug("Removing player from slot", slot_idx, level = "INFO")
      rv$lineup_slots[[slot_idx]] <- NULL
    })
    
    # =========================================================================
    # OPTIMIZATION (Placeholder)
    # =========================================================================
    
    observeEvent(input$optimize_lineup, {
      log_debug("Optimize lineup clicked", level = "INFO")
      # TODO: Implement LP optimization
      showNotification("Optimization coming soon!", type = "message")
    })
    
    observeEvent(input$generate_lineups, {
      log_debug("Generate lineups clicked", level = "INFO")
      # TODO: Implement multi-lineup generation
      showNotification("Multi-lineup generation coming soon!", type = "message")
    })
    
    # =========================================================================
    # GENERATED LINEUPS OUTPUT
    # =========================================================================
    
    output$generated_lineups_output <- renderUI({
      if (is.null(rv$generated_lineups)) {
        return(
          div(
            style = "text-align: center; padding: 2rem; color: var(--text-muted);",
            tags$p("No lineups generated yet."),
            tags$p("Configure your settings and click 'Generate Lineups' to create optimized lineups.")
          )
        )
      }
      
      # TODO: Render generated lineups
      div("Generated lineups will appear here")
    })
    
  })
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get position color (consistent with app theme)
#' @param pos Position abbreviation
#' @return Hex color string
get_position_color <- function(pos) {
  colors <- c(
    "GK" = "#5C9A9A",    # Teal
    "DEF" = "#6B8E6B",   # Green
    "MID" = "#8B7355",   # Brown
    "FWD" = "#BF7460",   # Coral
    "CAPTAIN" = "#EBCB8B", # Gold
    "FLEX" = "#7A7A7A"   # Gray
  )
  
  if (pos %in% names(colors)) {
    return(colors[[pos]])
  }
  return(APP_COLORS$muted)
}