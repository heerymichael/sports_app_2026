# =============================================================================
# Module: Soccer FanTeam Contests
# 
# Player salary tables for FanTeam Monster contests
# Styled similar to NFL projections module
# =============================================================================

# Soccer card color
SOCCER_CARD_COLOR <- APP_COLORS$sage

#' Soccer FanTeam Contests UI
#' @param id Module namespace ID
soccer_fanteam_contests_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_fanteam_contests_ui() called with id:", id, level = "INFO")
  
  # Get available gameweeks at UI build time
  gameweeks <- get_fanteam_soccer_gameweeks()
  log_debug("UI build - gameweeks available:", paste(gameweeks, collapse = ", "), level = "INFO")
  
  # Build gameweek choices
  if (length(gameweeks) > 0) {
    gw_choices <- setNames(as.character(gameweeks), paste("Gameweek", gameweeks))
    gw_selected <- as.character(gameweeks[1])
  } else {
    gw_choices <- c("No data found" = "")
    gw_selected <- NULL
  }
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("FanTeam Contests"),
      tags$p(class = "text-muted", "Player salaries for FanTeam Monster soccer contests")
    ),
    
    # Filters card
    ui_card(
      title = "Filters",
      color = SOCCER_CARD_COLOR,
      
      # Row 1: Gameweek, Position, Team
      fluidRow(
        column(4,
               selectInput(ns("gameweek"), "Gameweek",
                           choices = gw_choices,
                           selected = gw_selected
               )
        ),
        column(4,
               selectInput(ns("position"), "Position",
                           choices = c("All Positions" = "all", "GK", "DEF", "MID", "FWD"),
                           selected = "all"
               )
        ),
        column(4,
               shinyWidgets::pickerInput(ns("team"), "Team",
                                         choices = c("All Teams" = "all"),
                                         selected = "all",
                                         options = shinyWidgets::pickerOptions(
                                           liveSearch = TRUE,
                                           size = 10
                                         )
               )
        )
      ),
      
      # Row 2: Status, Sort, Heatmap
      fluidRow(
        column(4,
               selectInput(ns("status"), "Availability",
                           choices = c("All Players" = "all", 
                                       "Expected" = "expected",
                                       "Possible" = "possible",
                                       "Unexpected" = "unexpected",
                                       "Injured" = "injured",
                                       "Suspended" = "suspended"),
                           selected = "all"
               )
        ),
        column(4,
               selectInput(ns("sort_by"), "Sort By",
                           choices = c("Salary (High to Low)" = "salary_desc",
                                       "Salary (Low to High)" = "salary_asc",
                                       "Player Name" = "player",
                                       "Position" = "position"),
                           selected = "salary_desc"
               )
        ),
        column(4,
               selectInput(ns("heatmap"), "Heatmap",
                           choices = c("None" = "none", "Salary" = "salary"),
                           selected = "none"
               )
        )
      ),
      
      # Row 3: Salary Cap
      fluidRow(
        column(4,
               numericInput(ns("salary_cap"), "Salary Cap (â‚¬M)",
                            value = 118,
                            min = 50,
                            max = 200,
                            step = 1
               )
        ),
        column(8,
               div(
                 style = "padding-top: 25px;",
                 uiOutput(ns("budget_display"))
               )
        )
      )
    ),
    
    tags$br(),
    
    # Unmatched players alert
    uiOutput(ns("unmatched_alert")),
    
    # Salary table
    uiOutput(ns("salary_table"))
  )
}

#' Soccer FanTeam Contests Server
#' @param id Module namespace ID
soccer_fanteam_contests_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_fanteam_contests_server() initialized", level = "INFO")
    log_debug("Module namespace:", id, level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      salary_data = NULL,
      loading = FALSE,
      initialized = FALSE
    )
    
    # =========================================================================
    # DATA LOAD - triggered by gameweek change
    # =========================================================================
    
    load_attempts <- reactiveVal(0)
    
    observe({
      gameweek <- input$gameweek
      
      log_debug(">>> Data load observer triggered (attempt", load_attempts() + 1, ")", level = "DEBUG")
      log_debug(">>>   gameweek:", if(is.null(gameweek)) "NULL" else if(gameweek == "") "''" else paste0("'", gameweek, "'"), level = "DEBUG")
      
      # If inputs aren't ready, retry
      if ((is.null(gameweek) || gameweek == "") && load_attempts() < 10) {
        load_attempts(load_attempts() + 1)
        invalidateLater(200, session)
        return()
      }
      
      if (is.null(gameweek) || gameweek == "") {
        log_debug(">>> No valid gameweek after retries", level = "WARN")
        return()
      }
      
      rv$loading <- TRUE
      log_debug(">>> Loading data for gameweek:", gameweek, level = "INFO")
      
      # Load salary data
      data <- load_fanteam_soccer_with_logos(as.integer(gameweek))
      
      if (!is.null(data) && nrow(data) > 0) {
        # Run player matching against FBref data
        tryCatch({
          # Get FBref player list from cached data
          shot_data <- get_cached_data("shots")
          player_data <- get_cached_data("player_stats")
          
          if ((!is.null(shot_data) && nrow(shot_data) > 0) || 
              (!is.null(player_data) && nrow(player_data) > 0)) {
            fbref_players <- get_fbref_player_list(shot_data, player_data)
            
            if (nrow(fbref_players) > 0) {
              data <- match_fanteam_to_fbref(
                data, 
                fbref_players, 
                gameweek = as.integer(gameweek),
                write_unmatched = TRUE
              )
              log_debug("Player matching complete", level = "INFO")
            }
          }
        }, error = function(e) {
          log_debug("Player matching skipped:", e$message, level = "WARN")
        })
        
        rv$salary_data <- data
        rv$initialized <- TRUE
        log_debug(">>> Data loaded:", nrow(data), "players", level = "INFO")
        
        # Update team dropdown with logos
        teams <- sort(unique(data$team_normalized[!is.na(data$team_normalized)]))
        
        if (length(teams) > 0) {
          # Build HTML content for each team (logo + name)
          team_content <- c(
            "All Teams",  # No logo for "All Teams"
            sapply(teams, function(team) {
              logo_path <- get_soccer_team_logo(team)
              if (!is.null(logo_path)) {
                sprintf('<img src="%s" style="width:20px; height:20px; margin-right:8px; vertical-align:middle; object-fit:contain;"> %s', 
                        logo_path, team)
              } else {
                team
              }
            }, USE.NAMES = FALSE)
          )
          
          team_choices <- c("all", teams)
          names(team_choices) <- c("All Teams", teams)
          
          shinyWidgets::updatePickerInput(session, "team",
                                          choices = team_choices,
                                          selected = "all",
                                          choicesOpt = list(content = team_content)
          )
        }
        
      } else {
        log_debug(">>> No data found for gameweek:", gameweek, level = "WARN")
        rv$salary_data <- NULL
      }
      
      rv$loading <- FALSE
    })
    
    # =========================================================================
    # BUDGET DISPLAY
    # =========================================================================
    
    output$budget_display <- renderUI({
      cap <- input$salary_cap
      if (is.null(cap)) cap <- 118
      
      div(
        style = "display: flex; gap: 20px; align-items: center;",
        div(
          style = "font-size: 0.9rem; color: var(--text-muted);",
          HTML(sprintf("Budget: <strong style='color: var(--text-primary);'>â‚¬%.0fM</strong>", cap))
        ),
        div(
          style = "font-size: 0.85rem; color: var(--text-muted);",
          "11 players: 1 GK, 3 DEF, 3 MID, 1 FWD + 2 FLEX (DEF/MID/FWD)"
        )
      )
    })
    
    # =========================================================================
    # UNMATCHED PLAYERS ALERT
    # =========================================================================
    
    output$unmatched_alert <- renderUI({
      # Check for unmatched players
      unmatched_count <- tryCatch({
        get_unmatched_count()
      }, error = function(e) 0)
      
      if (unmatched_count == 0) return(NULL)
      
      div(
        style = "background-color: #EBCB8B; color: #2E3440; padding: 12px 16px; border-radius: 8px; margin-bottom: 16px; display: flex; align-items: center; justify-content: space-between;",
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          tags$span(style = "font-size: 1.2rem;", "âš ï¸"),
          tags$span(
            style = "font-weight: 600;",
            sprintf("%d unmatched player%s", unmatched_count, if(unmatched_count == 1) "" else "s")
          ),
          tags$span(
            style = "font-size: 0.9rem;",
            "â€” Add corrections in Google Sheet"
          )
        ),
        tags$a(
          href = sprintf("https://docs.google.com/spreadsheets/d/%s/edit#gid=707513073", FANTEAM_MAPPING_SHEET_ID),
          target = "_blank",
          style = "background-color: #2E3440; color: white; padding: 6px 12px; border-radius: 4px; text-decoration: none; font-size: 0.85rem; font-weight: 600;",
          "Open Sheet"
        )
      )
    })
    
    # =========================================================================
    # FILTERED DATA
    # =========================================================================
    
    filtered_data <- reactive({
      req(rv$salary_data)
      
      data <- rv$salary_data
      
      # Position filter
      if (!is.null(input$position) && input$position != "all") {
        data <- data %>% filter(position == input$position)
      }
      
      # Team filter
      if (!is.null(input$team) && input$team != "all") {
        data <- data %>% filter(team_normalized == input$team)
      }
      
      # Status filter
      if (!is.null(input$status) && input$status != "all" && "status" %in% names(data)) {
        data <- data %>% filter(status == input$status)
      }
      
      # Sort
      sort_by <- input$sort_by
      if (!is.null(sort_by)) {
        data <- switch(sort_by,
                       "salary_desc" = data %>% arrange(desc(salary)),
                       "salary_asc" = data %>% arrange(salary),
                       "player" = data %>% arrange(player),
                       "position" = data %>% arrange(match(position, c("GK", "DEF", "MID", "FWD")), desc(salary)),
                       data
        )
      }
      
      log_debug("Filtered data:", nrow(data), "players", level = "DEBUG")
      return(data)
    })
    
    # =========================================================================
    # SALARY TABLE RENDER
    # =========================================================================
    
    output$salary_table <- renderUI({
      # Show loading state
      if (rv$loading) {
        return(
          ui_card(
            title = "Loading...",
            color = SOCCER_CARD_COLOR,
            div(
              style = "padding: 2rem; text-align: center;",
              tags$p(class = "text-muted", "Loading salary data...")
            )
          )
        )
      }
      
      # Check for data
      data <- filtered_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          ui_card(
            title = "No Data",
            color = SOCCER_CARD_COLOR,
            div(
              style = "padding: 2rem; text-align: center;",
              tags$p(class = "text-muted", "No salary data available for this selection."),
              tags$p(
                class = "text-muted",
                style = "font-size: 0.85rem;",
                "Add CSV files to: data/fanteam_soccer/fanteam_monster_salaries/"
              )
            )
          )
        )
      }
      
      # Heatmap function
      heatmap_col <- input$heatmap
      
      get_heatmap_color <- function(value, col_name) {
        if (is.null(heatmap_col) || heatmap_col == "none") return("")
        if (heatmap_col != col_name) return("")
        if (is.na(value)) return("")
        
        col_values <- data[[col_name]]
        col_values <- col_values[!is.na(col_values)]
        
        if (length(col_values) == 0) return("")
        
        min_val <- min(col_values, na.rm = TRUE)
        max_val <- max(col_values, na.rm = TRUE)
        
        if (max_val == min_val) return("")
        
        # Normalize 0-1
        t <- (value - min_val) / (max_val - min_val)
        
        # Interpolate white -> sage light
        r <- round(255 + (197 - 255) * t)
        g <- round(255 + (212 - 255) * t)
        b <- round(255 + (184 - 255) * t)
        
        sprintf("background-color: rgb(%d, %d, %d);", r, g, b)
      }
      
      # Status badge helper
      get_status_badge <- function(status) {
        if (is.null(status) || is.na(status)) return("")
        
        status <- tolower(status)
        
        badge_style <- switch(status,
                              "expected" = "background-color: #A3BE8C; color: white;",
                              "possible" = "background-color: #EBCB8B; color: #2E3440;",
                              "unexpected" = "background-color: #B48EAD; color: white;",
                              "injured" = "background-color: #BF616A; color: white;",
                              "suspended" = "background-color: #D08770; color: white;",
                              "refuted" = "background-color: #4C566A; color: white;",
                              "background-color: #4C566A; color: white;"
        )
        
        tags$span(
          style = sprintf("padding: 2px 6px; border-radius: 4px; font-size: 0.7rem; font-weight: 600; %s", badge_style),
          toupper(substr(status, 1, 3))
        )
      }
      
      # Build table
      ui_card(
        title = sprintf("Player Salaries (%d players)", nrow(data)),
        color = SOCCER_CARD_COLOR,
        
        div(
          style = "overflow-x: auto;",
          tags$table(
            class = "projections-table",
            style = "width: 100%; border-collapse: collapse;",
            
            # Header
            tags$thead(
              tags$tr(
                tags$th(style = "text-align: left; padding: 0.75rem 1rem;", "Player"),
                tags$th(style = "text-align: center; padding: 0.75rem; width: 60px;", "Pos"),
                tags$th(style = "text-align: center; padding: 0.75rem; width: 100px;", "Team"),
                tags$th(style = "text-align: center; padding: 0.75rem; width: 80px;", "Status"),
                tags$th(style = "text-align: center; padding: 0.75rem; width: 100px;", "Salary"),
                tags$th(style = "text-align: center; padding: 0.75rem; width: 50px;", "")
              )
            ),
            
            # Body
            tags$tbody(
              lapply(1:nrow(data), function(i) {
                row <- data[i, ]
                
                # Team logo
                logo_html <- if (!is.null(row$logo_path) && !is.na(row$logo_path) && row$logo_path != "") {
                  tags$img(
                    src = row$logo_path,
                    style = "width: 20px; height: 20px; object-fit: contain; vertical-align: middle;",
                    onerror = "this.style.display='none'"
                  )
                } else {
                  ""
                }
                
                tags$tr(
                  # Player name
                  tags$td(
                    style = "padding: 0.75rem 1rem; font-weight: 500;",
                    row$player
                  ),
                  
                  # Position badge
                  tags$td(
                    style = "text-align: center; padding: 0.75rem;",
                    span(
                      class = "position-badge",
                      row$position
                    )
                  ),
                  
                  # Team with logo
                  tags$td(
                    style = "text-align: center; padding: 0.75rem;",
                    div(
                      style = "display: flex; align-items: center; justify-content: center; gap: 6px;",
                      logo_html,
                      span(get_team_abbreviation(row$team_normalized))
                    )
                  ),
                  
                  # Status badge
                  tags$td(
                    style = "text-align: center; padding: 0.75rem;",
                    if ("status" %in% names(row)) get_status_badge(row$status) else ""
                  ),
                  
                  # Salary
                  tags$td(
                    style = sprintf("text-align: center; padding: 0.75rem; font-weight: 600; %s",
                                    get_heatmap_color(row$salary, "salary")),
                    if (!is.na(row$salary)) sprintf("â‚¬%.1fM", row$salary) else "-"
                  ),
                  
                  # Match status indicator
                  tags$td(
                    style = "text-align: center; padding: 0.75rem;",
                    if ("match_status" %in% names(row)) {
                      status <- row$match_status
                      if (is.na(status) || status == "unmatched") {
                        tags$span(title = "Unmatched - no FBref data", style = "color: #BF616A; cursor: help;", "âœ—")
                      } else {
                        tags$span(title = paste("Matched:", row$fbref_name), style = "color: #A3BE8C; cursor: help;", "âœ“")
                      }
                    } else {
                      ""
                    }
                  )
                )
              })
            )
          )
        )
      )
    })
    
  })
}