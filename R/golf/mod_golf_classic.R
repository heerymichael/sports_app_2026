# =============================================================================
# Module: Golf Classic
# 
# Full tournament lineup building for FanTeam Golf Classic format
# Roster: 6 golfers, 100M salary cap, no position constraints
# =============================================================================

library(lpSolve)

# =============================================================================
# UI
# =============================================================================

golf_classic_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("golf_classic_ui() called with id:", id, level = "INFO")
  
  contests <- get_available_golf_contests(year = "2025", contest_type = "classic")
  contest_choices <- if (length(contests) > 0) {
    setNames(contests, sapply(contests, get_golf_contest_label))
  } else {
    c("No contests found" = "")
  }
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Golf Classic"),
      tags$p(class = "text-muted", "Build 6-golfer lineups for full tournament contests")
    ),
    
    # =========================================================================
    # SETTINGS
    # =========================================================================
    ui_card(
      title = "Settings",
      color = GOLF_CARD_COLOR,
      
      fluidRow(
        column(3,
               selectizeInput(ns("contest_select"), "Contest",
                              choices = contest_choices,
                              selected = if (length(contests) > 0) contests[1] else NULL
               )
        ),
        column(3,
               fileInput(ns("projections_upload"), "Upload Projections (CSV)", accept = ".csv")
        ),
        column(2,
               numericInput(ns("salary_cap"), "Salary Cap", value = 100, min = 50, max = 150, step = 0.5)
        ),
        column(2,
               selectizeInput(ns("projection_type"), "Optimize On",
                              choices = c("Blended" = "blended", "Median" = "median", "Ceiling" = "ceiling"),
                              selected = "blended"
               )
        ),
        column(2,
               div(style = "margin-top: 25px;",
                   actionButton(ns("process_btn"), "Load Data", class = "btn btn-primary w-100", icon = icon("sync"))
               )
        )
      ),
      
      uiOutput(ns("unmatched_alert"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # PLAYER POOL
    # =========================================================================
    ui_card(
      title = "Player Pool",
      color = GOLF_CARD_COLOR,
      
      # Heatmap dropdown row
      div(
        style = "display: flex; justify-content: flex-end; margin-bottom: 0.5rem; align-items: center; gap: 0.5rem;",
        tags$label(style = "font-size: 0.8rem; font-weight: 600; color: var(--text-secondary); margin: 0;", "Heatmap"),
        div(
          style = "width: 120px;",
          selectInput(ns("pool_heatmap"), NULL,
                      choices = c("None" = "none", "Blend" = "blended", "Median" = "median", 
                                  "Ceiling" = "ceiling", "Value" = "value"),
                      selected = "none"
          )
        )
      ),
      
      div(
        class = "player-pool-container",
        style = "max-height: 450px; overflow-y: auto;",
        reactableOutput(ns("player_pool_table"))
      )
    ),
    
    tags$br(),
    
    # =========================================================================
    # PLAYER RULES
    # =========================================================================
    ui_card(
      title = "Player Rules & Adjustments",
      color = GOLF_CARD_COLOR,
      
      # Lock Players Row
      fluidRow(
        column(9,
               selectizeInput(ns("lock_players"), "Lock Players (Always Include)",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players to lock...")
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
                              options = list(placeholder = "Select players to exclude...")
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
      
      # Grouped Boost/Dock
      tags$h6(class = "fw-bold mb-2", icon("users"), " Grouped Boost/Dock"),
      tags$p(class = "text-muted small mb-3", "Apply uniform boost or dock to selected players"),
      
      fluidRow(
        column(5,
               selectizeInput(ns("grouped_players"), "Players",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players...")
               )
        ),
        column(2,
               selectInput(ns("grouped_action"), "Action", choices = c("Boost", "Dock"))
        ),
        column(2,
               numericInput(ns("grouped_pct"), "Adjust %", value = 10, min = 0, max = 100, step = 5)
        ),
        column(3,
               div(style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                   actionButton(ns("add_grouped_rule"), "Add Rule", class = "btn btn-primary", style = "width: 120px; text-align: center;")
               )
        )
      ),
      
      uiOutput(ns("grouped_rules_display")),
      
      tags$hr(),
      
      # Correlation Rules
      tags$h6(class = "fw-bold mb-2", icon("link"), " Correlation Rules"),
      tags$p(class = "text-muted small mb-3", "When trigger player is in lineup, boost/dock target players"),
      
      # Row 1: Trigger
      fluidRow(
        column(4,
               selectizeInput(ns("corr_trigger"), "If This Player Is In Lineup",
                              choices = NULL,
                              options = list(placeholder = "Select trigger player...")
               )
        )
      ),
      
      # Row 2: Boost Targets
      fluidRow(
        column(6,
               selectizeInput(ns("corr_boost_targets"), "Boost These Players",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players to boost...")
               )
        ),
        column(2,
               numericInput(ns("corr_boost_pct"), "Boost %", value = 10, min = 0, max = 100, step = 5)
        )
      ),
      
      # Row 3: Dock Targets
      fluidRow(
        column(6,
               selectizeInput(ns("corr_dock_targets"), "Dock These Players",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Select players to dock...")
               )
        ),
        column(2,
               numericInput(ns("corr_dock_pct"), "Dock %", value = 10, min = 0, max = 100, step = 5)
        ),
        column(4,
               div(style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                   actionButton(ns("add_corr_rule"), "Add Correlation Rule", class = "btn btn-primary", style = "min-width: 180px; text-align: center;")
               )
        )
      ),
      
      uiOutput(ns("corr_rules_display"))
    ),
    
    tags$br(),
    
    # =========================================================================
    # GENERATE LINEUPS
    # =========================================================================
    ui_card(
      title = "Generate Lineups",
      color = GOLF_CARD_COLOR,
      
      div(
        style = "display: flex; justify-content: center; gap: 2rem; margin-bottom: 1.5rem;",
        div(style = "width: 150px;",
            numericInput(ns("num_lineups"), "# Lineups", value = 10, min = 1, max = 150)
        ),
        div(style = "width: 150px;",
            numericInput(ns("variance_pct"), "Variance %", value = 15, min = 0, max = 50)
        ),
        div(style = "width: 150px;",
            numericInput(ns("min_unique"), "Min Unique", value = 1, min = 0, max = 5)
        )
      ),
      
      div(
        style = "text-align: center; margin-bottom: 1.5rem;",
        actionButton(
          ns("generate_btn"),
          tagList(icon("play"), " GENERATE LINEUPS"),
          class = "btn btn-success btn-lg",
          style = "padding: 1rem 3rem; font-size: 1.2rem; font-weight: 700; text-transform: uppercase; box-shadow: 4px 4px 0 rgba(0,0,0,0.2);"
        )
      ),
      
      uiOutput(ns("lineup_summary_stats")),
      
      tags$hr(),
      
      uiOutput(ns("generated_lineups_grid"))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

golf_classic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("golf_classic_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    rv <- reactiveValues(
      salaries = NULL,
      projections = NULL,
      headshots = NULL,
      player_data = NULL,
      unmatched_players = NULL,
      generated_lineups = list(),
      optimal_projection = NULL,
      locked_players = character(0),
      excluded_players = character(0),
      grouped_rules = list(),
      corr_rules = list()
    )
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    observeEvent(input$process_btn, {
      req(input$contest_select)
      req(input$contest_select != "")
      
      log_debug(">>> Process button clicked", level = "INFO")
      
      salaries <- load_golf_salaries(year = "2025", contest = input$contest_select)
      
      if (is.null(salaries)) {
        showNotification("Failed to load salary data", type = "error")
        return()
      }
      
      rv$salaries <- salaries
      rv$headshots <- load_golf_headshots()
      
      if (!is.null(input$projections_upload)) {
        proj_raw <- tryCatch({
          read_csv(input$projections_upload$datapath, show_col_types = FALSE)
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
          NULL
        })
        
        if (!is.null(proj_raw)) {
          rv$projections <- parse_golf_projections(proj_raw)
        }
      }
      
      if (!is.null(rv$salaries) && !is.null(rv$projections)) {
        rv$player_data <- match_golf_players(rv$salaries, rv$projections, rv$headshots)
        
        rv$unmatched_players <- rv$player_data %>%
          filter(is.na(median)) %>%
          pull(player_name)
        
        matched_count <- sum(!is.na(rv$player_data$median))
        showNotification(
          sprintf("Loaded %d golfers (%d with projections)", nrow(rv$player_data), matched_count),
          type = "message"
        )
        
        player_choices <- rv$player_data %>%
          filter(!is.na(median)) %>%
          arrange(desc(blended)) %>%
          pull(player_name)
        
        updateSelectizeInput(session, "lock_players", choices = player_choices, server = TRUE)
        updateSelectizeInput(session, "exclude_players", choices = player_choices, server = TRUE)
        updateSelectizeInput(session, "grouped_players", choices = player_choices, server = TRUE)
        updateSelectizeInput(session, "corr_trigger", choices = c("Select trigger..." = "", player_choices))
        updateSelectizeInput(session, "corr_boost_targets", choices = player_choices, server = TRUE)
        updateSelectizeInput(session, "corr_dock_targets", choices = player_choices, server = TRUE)
        
      } else if (!is.null(rv$salaries)) {
        normalize_name <- function(name) {
          name %>%
            tolower() %>%
            stringr::str_replace_all("[^a-z0-9\\s]", "") %>%
            stringr::str_squish()
        }
        
        if (!is.null(rv$headshots)) {
          rv$player_data <- rv$salaries %>%
            mutate(match_key = normalize_name(player_name)) %>%
            left_join(
              rv$headshots %>%
                mutate(match_key = normalize_name(player_name)) %>%
                select(match_key, headshot_url),
              by = "match_key"
            ) %>%
            mutate(headshot_url = if_else(is.na(headshot_url), GOLF_DEFAULT_HEADSHOT, headshot_url)) %>%
            select(-match_key)
        } else {
          rv$player_data <- rv$salaries %>%
            mutate(headshot_url = GOLF_DEFAULT_HEADSHOT)
        }
        
        rv$unmatched_players <- rv$player_data$player_name
        showNotification("Loaded salaries - upload projections to continue", type = "warning")
      }
      
      # Reset state
      rv$generated_lineups <- list()
      rv$optimal_projection <- NULL
      rv$locked_players <- character(0)
      rv$excluded_players <- character(0)
      rv$grouped_rules <- list()
      rv$corr_rules <- list()
    })
    
    # =========================================================================
    # UNMATCHED ALERT
    # =========================================================================
    
    output$unmatched_alert <- renderUI({
      unmatched <- rv$unmatched_players
      if (is.null(unmatched) || length(unmatched) == 0) return(NULL)
      
      div(
        class = "alert alert-warning mt-3 mb-0",
        tags$strong(icon("exclamation-triangle"), sprintf(" %d Unmatched: ", length(unmatched))),
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
    # LOCK/EXCLUDE
    # =========================================================================
    
    observeEvent(input$apply_lock, {
      rv$locked_players <- unique(c(rv$locked_players, input$lock_players))
      updateSelectizeInput(session, "lock_players", selected = character(0))
    })
    
    observeEvent(input$apply_exclude, {
      rv$excluded_players <- unique(c(rv$excluded_players, input$exclude_players))
      updateSelectizeInput(session, "exclude_players", selected = character(0))
    })
    
    observeEvent(input$remove_locked, {
      rv$locked_players <- setdiff(rv$locked_players, input$remove_locked)
    })
    
    observeEvent(input$remove_excluded, {
      rv$excluded_players <- setdiff(rv$excluded_players, input$remove_excluded)
    })
    
    output$lock_exclude_display <- renderUI({
      locked <- rv$locked_players
      excluded <- rv$excluded_players
      
      if (length(locked) == 0 && length(excluded) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem; margin-top: 1rem;",
        
        # Locked players
        lapply(locked, function(p) {
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-sage); border-radius: 6px;",
            
            # Lock badge
            div(
              style = "background: var(--accent-sage); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("lock"), " LOCKED"
            ),
            
            # Player name
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              p
            ),
            
            # Remove button
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
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-coral); border-radius: 6px;",
            
            # Exclude badge
            div(
              style = "background: var(--accent-coral); color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
              icon("ban"), " EXCLUDED"
            ),
            
            # Player name
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              p
            ),
            
            # Remove button
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
    # GROUPED RULES
    # =========================================================================
    
    observeEvent(input$add_grouped_rule, {
      req(input$grouped_players)
      req(length(input$grouped_players) > 0)
      
      key <- paste0(input$grouped_action, "_", input$grouped_pct %||% 10)
      
      if (key %in% names(rv$grouped_rules)) {
        rv$grouped_rules[[key]]$players <- unique(c(rv$grouped_rules[[key]]$players, input$grouped_players))
      } else {
        rv$grouped_rules[[key]] <- list(
          action = input$grouped_action,
          pct = input$grouped_pct %||% 10,
          players = input$grouped_players
        )
      }
      
      updateSelectizeInput(session, "grouped_players", selected = character(0))
    })
    
    observeEvent(input$remove_grouped_rule, {
      rv$grouped_rules[[input$remove_grouped_rule]] <- NULL
    })
    
    output$grouped_rules_display <- renderUI({
      rules <- rv$grouped_rules
      if (length(rules) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem; margin-top: 1rem;",
        lapply(names(rules), function(key) {
          rule <- rules[[key]]
          is_boost <- rule$action == "Boost"
          
          div(
            style = sprintf(
              "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid %s; border-radius: 6px;",
              if (is_boost) "var(--accent-sage)" else "var(--accent-coral)"
            ),
            
            # Action badge
            div(
              style = sprintf(
                "background: %s; color: white; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem; white-space: nowrap;",
                if (is_boost) "var(--accent-sage)" else "var(--accent-coral)"
              ),
              sprintf("%s %d%%", toupper(rule$action), rule$pct)
            ),
            
            # Player names
            div(
              style = "flex: 1; font-weight: 600; font-size: 0.9rem;",
              paste(rule$players, collapse = ", ")
            ),
            
            # Remove button
            actionButton(
              ns(paste0("remove_grouped_", gsub("[^a-zA-Z0-9]", "_", key))),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_grouped_rule"), key)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # CORRELATION RULES (supports multiple boost/dock groups per trigger)
    # =========================================================================
    
    observeEvent(input$add_corr_rule, {
      req(input$corr_trigger)
      req(input$corr_trigger != "")
      
      trigger <- input$corr_trigger
      boost_targets <- input$corr_boost_targets %||% character(0)
      boost_pct <- input$corr_boost_pct %||% 10
      dock_targets <- input$corr_dock_targets %||% character(0)
      dock_pct <- input$corr_dock_pct %||% 10
      
      if (trigger %in% names(rv$corr_rules)) {
        existing <- rv$corr_rules[[trigger]]
        
        # Add boost targets to matching pct group or create new group
        if (length(boost_targets) > 0) {
          matched <- FALSE
          for (i in seq_along(existing$boost_groups)) {
            if (existing$boost_groups[[i]]$pct == boost_pct) {
              existing$boost_groups[[i]]$targets <- unique(c(existing$boost_groups[[i]]$targets, boost_targets))
              matched <- TRUE
              break
            }
          }
          if (!matched) {
            existing$boost_groups <- append(existing$boost_groups, list(list(pct = boost_pct, targets = boost_targets)))
          }
        }
        
        # Add dock targets to matching pct group or create new group
        if (length(dock_targets) > 0) {
          matched <- FALSE
          for (i in seq_along(existing$dock_groups)) {
            if (existing$dock_groups[[i]]$pct == dock_pct) {
              existing$dock_groups[[i]]$targets <- unique(c(existing$dock_groups[[i]]$targets, dock_targets))
              matched <- TRUE
              break
            }
          }
          if (!matched) {
            existing$dock_groups <- append(existing$dock_groups, list(list(pct = dock_pct, targets = dock_targets)))
          }
        }
        
        rv$corr_rules[[trigger]] <- existing
      } else {
        rv$corr_rules[[trigger]] <- list(
          trigger = trigger,
          boost_groups = if (length(boost_targets) > 0) list(list(pct = boost_pct, targets = boost_targets)) else list(),
          dock_groups = if (length(dock_targets) > 0) list(list(pct = dock_pct, targets = dock_targets)) else list()
        )
      }
      
      updateSelectizeInput(session, "corr_trigger", selected = "")
      updateSelectizeInput(session, "corr_boost_targets", selected = character(0))
      updateSelectizeInput(session, "corr_dock_targets", selected = character(0))
    })
    
    observeEvent(input$remove_corr_rule, {
      rv$corr_rules[[input$remove_corr_rule]] <- NULL
    })
    
    output$corr_rules_display <- renderUI({
      rules <- rv$corr_rules
      if (length(rules) == 0) return(NULL)
      
      div(
        style = "display: flex; flex-direction: column; gap: 0.4rem; margin-top: 1rem;",
        lapply(names(rules), function(trigger) {
          rule <- rules[[trigger]]
          
          div(
            style = "display: flex; align-items: center; padding: 0.4rem 0.6rem; background: white; border: 2px solid var(--accent-plum); border-radius: 6px;",
            
            # IF badge
            div(
              style = "background: var(--text-primary); color: white; padding: 0.2rem 0.4rem; border-radius: 4px; font-size: 0.7rem; font-weight: 700; margin-right: 0.5rem;",
              "IF"
            ),
            
            # Trigger name
            div(
              style = "font-weight: 600; font-size: 0.9rem; min-width: 120px;",
              trigger
            ),
            
            # Arrow
            span(style = "color: var(--text-muted); font-size: 1.2rem; margin: 0 0.5rem;", HTML("&#8594;")),
            
            # Effects
            div(
              style = "flex: 1; display: flex; flex-wrap: wrap; gap: 0.4rem; align-items: center;",
              
              # Boost groups
              lapply(rule$boost_groups, function(group) {
                div(
                  style = "display: inline-flex; align-items: center; gap: 0.25rem; padding: 0.15rem 0.4rem; background: rgba(139, 168, 134, 0.2); border-radius: 4px;",
                  span(
                    style = "font-weight: 700; color: var(--accent-sage); font-size: 0.75rem;",
                    sprintf("+%d%%", group$pct)
                  ),
                  span(style = "font-size: 0.85rem; font-weight: 500;", paste(group$targets, collapse = ", "))
                )
              }),
              
              # Dock groups
              lapply(rule$dock_groups, function(group) {
                div(
                  style = "display: inline-flex; align-items: center; gap: 0.25rem; padding: 0.15rem 0.4rem; background: rgba(232, 131, 121, 0.2); border-radius: 4px;",
                  span(
                    style = "font-weight: 700; color: var(--accent-coral); font-size: 0.75rem;",
                    sprintf("-%d%%", group$pct)
                  ),
                  span(style = "font-size: 0.85rem; font-weight: 500;", paste(group$targets, collapse = ", "))
                )
              })
            ),
            
            # Remove button
            actionButton(
              ns(paste0("remove_corr_", gsub("[^a-zA-Z0-9]", "_", trigger))),
              icon("times"),
              class = "btn-secondary",
              style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_corr_rule"), trigger)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # PLAYER POOL TABLE
    # =========================================================================
    
    output$player_pool_table <- renderReactable({
      req(rv$player_data)
      
      proj_col <- input$projection_type %||% "blended"
      heatmap_col <- input$pool_heatmap %||% "none"
      
      pool_data <- rv$player_data %>%
        filter(!is.na(.data[[proj_col]])) %>%
        mutate(
          excluded = player_name %in% rv$excluded_players,
          locked = player_name %in% rv$locked_players
        ) %>%
        arrange(desc(.data[[proj_col]]))
      
      # Sequential heatmap: white to teal (for projection columns)
      get_sequential_style <- function(value, col_values, col_name) {
        if (heatmap_col != col_name || is.na(value)) return(list(verticalAlign = "middle"))
        min_val <- min(col_values, na.rm = TRUE)
        max_val <- max(col_values, na.rm = TRUE)
        if (max_val == min_val) return(list(verticalAlign = "middle"))
        
        t <- (value - min_val) / (max_val - min_val)
        t <- max(0, min(1, t))
        r <- round(255 + (143 - 255) * t)
        g <- round(255 + (188 - 255) * t)
        b <- round(255 + (187 - 255) * t)
        list(background = sprintf("rgb(%d, %d, %d)", r, g, b), verticalAlign = "middle")
      }
      
      # Diverging heatmap: coral -> white -> teal (for value column, midpoint = 0)
      get_diverging_style <- function(value, col_values) {
        if (heatmap_col != "value" || is.na(value)) return(list(verticalAlign = "middle"))
        
        midpoint <- 0
        min_val <- min(col_values, na.rm = TRUE)
        max_val <- max(col_values, na.rm = TRUE)
        
        if (value < midpoint) {
          if (min_val >= midpoint) min_val <- -1
          t <- (value - min_val) / (midpoint - min_val)
          t <- max(0, min(1, t))
          # Coral to white
          r <- round(208 + (255 - 208) * t)
          g <- round(135 + (255 - 135) * t)
          b <- round(112 + (255 - 112) * t)
        } else {
          if (max_val <= midpoint) max_val <- 1
          t <- (value - midpoint) / (max_val - midpoint)
          t <- max(0, min(1, t))
          # White to teal
          r <- round(255 + (143 - 255) * t)
          g <- round(255 + (188 - 255) * t)
          b <- round(255 + (187 - 255) * t)
        }
        list(background = sprintf("rgb(%d, %d, %d)", r, g, b), verticalAlign = "middle")
      }
      
      # Pre-compute column values for heatmap scaling
      blend_vals <- pool_data$blended
      med_vals <- pool_data$median
      ceil_vals <- pool_data$ceiling
      value_vals <- pool_data$value
      
      reactable(
        pool_data,
        theme = app_reactable_theme(compact = TRUE),
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        rowStyle = function(index) {
          if (pool_data$excluded[index]) {
            list(opacity = "0.4", textDecoration = "line-through")
          } else if (pool_data$locked[index]) {
            list(background = "#FFF9E6")
          }
        },
        defaultColDef = colDef(
          vAlign = "center",
          style = list(verticalAlign = "middle")
        ),
        columns = list(
          player_id = colDef(show = FALSE),
          player_name = colDef(
            name = "Golfer",
            minWidth = 160,
            style = list(verticalAlign = "middle"),
            cell = function(value, index) {
              headshot_url <- pool_data$headshot_url[index]
              div(
                style = "display: flex; align-items: center; gap: 0.5rem;",
                div(
                  class = "player-headshot player-headshot--sm",
                  style = "background: #e8e8e8; flex-shrink: 0;",
                  tags$img(
                    src = headshot_url,
                    style = "width: 100%; height: 100%; object-fit: cover;",
                    onerror = sprintf("this.src='%s'", GOLF_DEFAULT_HEADSHOT)
                  )
                ),
                span(style = "font-weight: 600;", value)
              )
            }
          ),
          salary = colDef(name = "Salary", format = colFormat(prefix = "$", suffix = "M", digits = 1), width = 115, align = "right", style = list(verticalAlign = "middle")),
          blended = colDef(
            name = "Blend",
            format = colFormat(digits = 1),
            width = 100,
            align = "right",
            style = function(value) get_sequential_style(value, blend_vals, "blended")
          ),
          median = colDef(
            name = "Med",
            format = colFormat(digits = 1),
            width = 95,
            align = "right",
            style = function(value) get_sequential_style(value, med_vals, "median")
          ),
          ceiling = colDef(
            name = "Ceil",
            format = colFormat(digits = 1),
            width = 95,
            align = "right",
            style = function(value) get_sequential_style(value, ceil_vals, "ceiling")
          ),
          value = colDef(
            name = "Value",
            width = 100,
            align = "right",
            cell = function(value) {
              if (is.na(value)) return("-")
              sprintf("%+.2f", value)
            },
            style = function(value) get_diverging_style(value, value_vals)
          ),
          own_large = colDef(name = "Own Lg", format = colFormat(suffix = "%", digits = 1), width = 95, align = "right", style = list(verticalAlign = "middle")),
          own_small = colDef(name = "Own Sm", format = colFormat(suffix = "%", digits = 1), width = 95, align = "right", style = list(verticalAlign = "middle")),
          headshot_url = colDef(show = FALSE),
          cut_odds = colDef(show = FALSE),
          dk_salary = colDef(show = FALSE),
          volatility = colDef(show = FALSE),
          excluded = colDef(show = FALSE),
          locked = colDef(show = FALSE)
        )
      )
    })
    
    # =========================================================================
    # LINEUP GENERATION
    # =========================================================================
    
    # Helper to expand correlation rules for optimizer
    expand_corr_rules_for_optimizer <- function(rules) {
      expanded <- list()
      for (trigger in names(rules)) {
        rule <- rules[[trigger]]
        # Combine all boost groups for this trigger
        all_boost_targets <- character(0)
        all_dock_targets <- character(0)
        boost_pct <- 0
        dock_pct <- 0
        
        # Process boost groups (apply each group separately)
        for (group in rule$boost_groups) {
          expanded <- append(expanded, list(list(
            trigger = trigger,
            boost_targets = group$targets,
            boost_pct = group$pct,
            dock_targets = character(0),
            dock_pct = 0
          )))
        }
        
        # Process dock groups (apply each group separately)
        for (group in rule$dock_groups) {
          expanded <- append(expanded, list(list(
            trigger = trigger,
            boost_targets = character(0),
            boost_pct = 0,
            dock_targets = group$targets,
            dock_pct = group$pct
          )))
        }
      }
      expanded
    }
    
    observeEvent(input$generate_btn, {
      req(rv$player_data)
      
      log_debug(">>> Generate button clicked", level = "INFO")
      
      proj_col <- input$projection_type %||% "blended"
      num_lineups <- input$num_lineups %||% 10
      variance <- input$variance_pct %||% 15
      min_unique <- input$min_unique %||% 1
      salary_cap <- input$salary_cap %||% 100
      
      players_adj <- rv$player_data %>%
        filter(!is.na(.data[[proj_col]])) %>%
        mutate(adjusted_projection = .data[[proj_col]])
      
      # Apply grouped rules
      for (rule in rv$grouped_rules) {
        factor <- if (rule$action == "Boost") (1 + rule$pct / 100) else (1 - rule$pct / 100)
        players_adj <- players_adj %>%
          mutate(adjusted_projection = if_else(
            player_name %in% rule$players,
            adjusted_projection * factor,
            adjusted_projection
          ))
      }
      
      # Expand correlation rules for optimizer
      expanded_corr_rules <- expand_corr_rules_for_optimizer(rv$corr_rules)
      
      # Calculate optimal
      optimal <- optimize_golf_classic_lp(
        players = players_adj,
        projection_col = "adjusted_projection",
        salary_cap = salary_cap,
        locked_players = rv$locked_players,
        excluded_players = rv$excluded_players,
        corr_rules = expanded_corr_rules
      )
      
      rv$optimal_projection <- if (!is.null(optimal)) sum(optimal$projection) else NA
      
      showNotification(sprintf("Generating %d lineups...", num_lineups), type = "message", duration = 2)
      
      lineups <- generate_golf_classic_lineups(
        players = players_adj,
        projection_col = "adjusted_projection",
        num_lineups = num_lineups,
        variance_pct = variance,
        salary_cap = salary_cap,
        locked_players = rv$locked_players,
        excluded_players = rv$excluded_players,
        min_unique = min_unique,
        corr_rules = expanded_corr_rules
      )
      
      rv$generated_lineups <- lineups
      
      if (length(lineups) > 0) {
        showNotification(sprintf("Generated %d unique lineups", length(lineups)), type = "message")
      } else {
        showNotification("No valid lineups generated", type = "warning")
      }
    })
    
    # =========================================================================
    # SUMMARY STATS (order matches columns: Sal, Proj, Own Lg, Own Sm)
    # =========================================================================
    
    output$lineup_summary_stats <- renderUI({
      n <- length(rv$generated_lineups)
      opt <- rv$optimal_projection
      
      if (n == 0) return(NULL)
      
      projections <- sapply(rv$generated_lineups, function(l) sum(l$projection))
      
      div(
        style = "display: flex; justify-content: center; gap: 2rem; margin-bottom: 1rem;",
        
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Lineups"),
          div(style = "font-size: 1.25rem; font-weight: 700;", n)
        ),
        
        if (!is.na(opt)) {
          div(
            style = "text-align: center;",
            div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Optimal"),
            div(style = "font-size: 1.25rem; font-weight: 700; color: var(--accent-sage);", sprintf("%.1f", opt))
          )
        },
        
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Best"),
          div(style = "font-size: 1.25rem; font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", max(projections)))
        ),
        
        div(
          style = "text-align: center;",
          div(style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);", "Average"),
          div(style = "font-size: 1.25rem; font-weight: 700;", sprintf("%.1f", mean(projections)))
        )
      )
    })
    
    # =========================================================================
    # GENERATED LINEUPS GRID
    # =========================================================================
    
    output$generated_lineups_grid <- renderUI({
      lineups <- rv$generated_lineups
      opt <- rv$optimal_projection
      
      if (length(lineups) == 0) {
        return(div(class = "text-muted text-center py-4", "No lineups generated yet"))
      }
      
      player_data <- rv$player_data
      
      # Sort by closest to optimal
      projections <- sapply(lineups, function(l) sum(l$projection))
      sorted_indices <- if (!is.na(opt)) order(abs(projections - opt)) else seq_along(lineups)
      
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem;",
        lapply(sorted_indices, function(i) {
          lineup <- lineups[[i]]
          total_proj <- sum(lineup$projection)
          total_sal <- sum(lineup$salary)
          vs_opt <- if (!is.na(opt)) total_proj - opt else NA
          
          div(
            style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 0.75rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
            
            # Header
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem; padding-bottom: 0.4rem; border-bottom: 2px dashed var(--bg-secondary);",
              span(style = "font-weight: 800; text-transform: uppercase; font-size: 0.8rem;", sprintf("Lineup %d", i)),
              div(
                style = "display: flex; gap: 0.4rem; font-size: 0.75rem; align-items: center;",
                span(style = "font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", total_proj)),
                span(style = "color: var(--text-muted);", sprintf("$%.1fM", total_sal)),
                if (!is.na(vs_opt)) {
                  span(
                    style = sprintf(
                      "font-weight: 600; padding: 0.1rem 0.25rem; border-radius: 3px; font-size: 0.65rem; background: %s; color: %s;",
                      if (vs_opt >= -3) "rgba(139, 168, 134, 0.3)" else "rgba(232, 131, 121, 0.3)",
                      if (vs_opt >= -3) "var(--accent-sage)" else "var(--accent-coral)"
                    ),
                    sprintf("%+.1f", vs_opt)
                  )
                }
              )
            ),
            
            # Players
            div(
              style = "display: flex; flex-direction: column; gap: 0.25rem;",
              lapply(1:nrow(lineup), function(j) {
                player_info <- player_data %>% filter(player_name == lineup$player_name[j])
                
                headshot_url <- if (nrow(player_info) > 0 && "headshot_url" %in% names(player_info)) {
                  player_info$headshot_url[1]
                } else {
                  GOLF_DEFAULT_HEADSHOT
                }
                
                own_lg <- if (nrow(player_info) > 0 && "own_large" %in% names(player_info)) {
                  player_info$own_large[1]
                } else {
                  NA
                }
                
                own_sm <- if (nrow(player_info) > 0 && "own_small" %in% names(player_info)) {
                  player_info$own_small[1]
                } else {
                  NA
                }
                
                # Split name
                name_parts <- strsplit(lineup$player_name[j], " ")[[1]]
                first_name <- if (length(name_parts) >= 1) name_parts[1] else ""
                last_name <- if (length(name_parts) >= 2) paste(name_parts[-1], collapse = " ") else ""
                
                div(
                  style = "display: flex; align-items: center; gap: 0.35rem; padding: 0.2rem 0.3rem; background: var(--bg-tertiary); border-radius: 4px;",
                  
                  # Headshot with dark border
                  div(
                    style = "width: 28px; height: 28px; border-radius: 50%; background: #e8e8e8; overflow: hidden; flex-shrink: 0; border: 2px solid var(--text-primary);",
                    tags$img(
                      src = headshot_url,
                      style = "width: 100%; height: 100%; object-fit: cover;",
                      onerror = sprintf("this.src='%s'", GOLF_DEFAULT_HEADSHOT)
                    )
                  ),
                  
                  # Name stacked (narrower)
                  div(
                    style = "flex: 1; min-width: 0; line-height: 1.1;",
                    div(style = "font-size: 0.55rem; color: var(--text-muted);", first_name),
                    div(style = "font-size: 0.7rem; font-weight: 700; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", last_name)
                  ),
                  
                  # SAL
                  div(
                    style = "text-align: center; width: 40px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "SAL"),
                    div(style = "font-size: 0.7rem; font-weight: 600;", sprintf("%.1f", lineup$salary[j]))
                  ),
                  
                  # PROJ
                  div(
                    style = "text-align: center; width: 35px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "PROJ"),
                    div(style = "font-size: 0.7rem; font-weight: 600; color: var(--accent-coral);", sprintf("%.0f", lineup$projection[j]))
                  ),
                  
                  # OWN LG
                  div(
                    style = "text-align: center; width: 35px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "LG"),
                    div(style = "font-size: 0.7rem; font-weight: 600;", if (!is.na(own_lg)) sprintf("%.0f%%", own_lg) else "-")
                  ),
                  
                  # OWN SM
                  div(
                    style = "text-align: center; width: 35px;",
                    div(style = "font-size: 0.5rem; color: var(--text-muted);", "SM"),
                    div(style = "font-size: 0.7rem; font-weight: 600;", if (!is.na(own_sm)) sprintf("%.0f%%", own_sm) else "-")
                  )
                )
              })
            )
          )
        })
      )
    })
    
  })
}

cat("Golf Classic module loaded: golf_classic_ui(), golf_classic_server()\n")