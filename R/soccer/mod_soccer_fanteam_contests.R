# =============================================================================
# Module: Soccer FanTeam Contests - v18 SHOTS + CS% + FIXED BUTTON WIDTHS
# 
# Layout:
#   1. Team header with logos
#   2. HOME | DRAW | AWAY inputs (with native spinners)
#   3. [Probability Bar] [GOALS input] side by side
#   4. Market Implied | My View results (Result, Shots/SOT, Home CS%, Away CS%)
#
# v18 Changes:
#   - Match cards now show: Result, Home Shots, Away Shots, Home CS%, Away CS%
#   - Fixed button widths: View buttons 100px, Position buttons 55px
#   - ggiraph interactive plots with hover tooltips
#   - Value plot: tooltip shows team logo, name, FOS, Salary, CS%, xG
#   - Adjustments plot: y-axis labels with team logo + CAPS name (ggtext)
#   - Plot container height: 630px (~66% taller)
#
# v15-17 Changes:
#   - FOS vs Salary scatter plot (Fantasy Opportunity Score)
#   - Position-specific FOS weights (GK: 80% CS, FWD: 90% GF, etc.)
#   - Dumbbell plot showing baseline vs adjusted FOS per team
# =============================================================================

# =============================================================================
# EMPIRICAL COEFFICIENTS - Derived from 5 seasons of Premier League data
# Generated: 2026-01-16
# Source: Google Sheet match_odds (football-data.co.uk historical data)
# Weighting: Time-weighted with 2.0-year half-life
# =============================================================================

FANTEAM_COEFFICIENTS <- list(
  # HOME SHOTS: shots = intercept + (win_pct * coef) + (total_goals * coef)
  # RÃƒâ€šÃ‚Â² = 0.2940
  home_shots = list(
    intercept = 3.9941,
    win_pct = 0.150385,
    total_goals = 1.1583
  ),
  
  # AWAY SHOTS
  # RÃƒâ€šÃ‚Â² = 0.2570
  away_shots = list(
    intercept = 4.4395,
    win_pct = 0.152701,
    total_goals = 0.8070
  ),
  
  # HOME SOT
  # RÃƒâ€šÃ‚Â² = 0.1883
  home_sot = list(
    intercept = 0.2532,
    win_pct = 0.051147,
    total_goals = 0.7963
  ),
  
  # AWAY SOT
  # RÃƒâ€šÃ‚Â² = 0.1988
  away_sot = list(
    intercept = 0.5083,
    win_pct = 0.060438,
    total_goals = 0.5695
  ),
  
  # HOME GOALS: goals = intercept + (win_pct * coef) + (draw_pct * coef) + (total * coef)
  # RÃƒâ€šÃ‚Â² = 0.1431
  home_goals = list(
    intercept = -1.3634,
    win_pct = 0.024723,
    draw_pct = 0.020101,
    total_goals = 0.4762
  ),
  
  # AWAY GOALS
  # RÃƒâ€šÃ‚Â² = 0.1373
  away_goals = list(
    intercept = -0.5941,
    win_pct = 0.025267,
    draw_pct = 0.004321,
    total_goals = 0.3474
  ),
  
  # SOT ratios (for quick conversion if needed)
  sot_ratio = list(
    home = 0.3511,
    away = 0.3614,
    overall = 0.3562
  )
)

# =============================================================================
# ODDS DATA LOADING
# =============================================================================

#' Load odds report from CSV file in fanteam_monster_salaries folder
#' @param gameweek Optional gameweek number to match specific file
#' @return Data frame with odds data or NULL if not found
load_fanteam_odds <- function(gameweek = NULL) {
  if (!exists("FANTEAM_SOCCER_DIR") || !dir.exists(FANTEAM_SOCCER_DIR)) {
    log_debug("Odds: Directory not found or FANTEAM_SOCCER_DIR not defined", level = "DEBUG")
    return(NULL)
  }
  
  # Look for exact pattern: week_X_odds_report.csv
  all_files <- list.files(FANTEAM_SOCCER_DIR, pattern = "\\.csv$", full.names = FALSE)
  
  selected_file <- NULL
  if (!is.null(gameweek)) {
    # Exact match: week_19_odds_report.csv
    exact_pattern <- sprintf("week_%d_odds_report.csv", gameweek)
    if (exact_pattern %in% all_files) {
      selected_file <- exact_pattern
    }
  }
  
  # If no gameweek specified or no exact match, find most recent odds_report file
  if (is.null(selected_file)) {
    odds_report_files <- all_files[grepl("^week_\\d+_odds_report\\.csv$", all_files)]
    if (length(odds_report_files) > 0) {
      # Sort by week number descending
      weeks <- as.integer(gsub("^week_(\\d+)_odds_report\\.csv$", "\\1", odds_report_files))
      selected_file <- odds_report_files[order(weeks, decreasing = TRUE)[1]]
    }
  }
  
  if (is.null(selected_file)) {
    log_debug("Odds: No odds_report file found", level = "DEBUG")
    return(NULL)
  }
  
  file_path <- file.path(FANTEAM_SOCCER_DIR, selected_file)
  log_debug("Odds: Loading file:", selected_file, level = "INFO")
  
  odds_data <- tryCatch({
    raw_lines <- readLines(file_path, n = 3, encoding = "UTF-8")
    skip_rows <- 0
    if (length(raw_lines) > 1) {
      first_line <- raw_lines[1]
      if (grepl("Implied Goals", first_line, ignore.case = TRUE) || 
          nchar(gsub(",", "", first_line)) < 10) {
        skip_rows <- 1
        log_debug("Odds: Skipping header row", level = "DEBUG")
      }
    }
    
    read_csv(file_path, skip = skip_rows, show_col_types = FALSE, 
             locale = locale(encoding = "UTF-8")) %>%
      clean_names()
  }, error = function(e) {
    log_debug("Odds: Error reading file:", e$message, level = "WARN")
    return(NULL)
  })
  
  if (is.null(odds_data) || nrow(odds_data) == 0) return(NULL)
  
  log_debug("Odds: Raw columns after clean_names:", paste(names(odds_data), collapse = ", "), level = "DEBUG")
  
  # Standardize column names - handle multiple variants
  col_renames <- c(
    "team" = "odds_team",
    "opponent" = "odds_opponent", 
    "h_a" = "home_away",
    # Win % variants
    "win" = "win_pct",
    "win_percent" = "win_pct",
    # Draw % variants  
    "draw" = "draw_pct",
    "draw_percent" = "draw_pct",
    # Clean Sheet % variants
    "clean_sheet" = "clean_sheet_pct",
    "clean_sheet_percent" = "clean_sheet_pct",
    # Implied goals
    "scored" = "implied_team_goals",
    "allowed" = "implied_opp_goals",
    "total" = "implied_total"
  )
  
  for (old_name in names(col_renames)) {
    new_name <- col_renames[old_name]
    if (old_name %in% names(odds_data) && !new_name %in% names(odds_data)) {
      names(odds_data)[names(odds_data) == old_name] <- new_name
      log_debug(sprintf("Odds: Renamed %s -> %s", old_name, new_name), level = "DEBUG")
    }
  }
  
  log_debug("Odds: Final columns:", paste(names(odds_data), collapse = ", "), level = "DEBUG")
  
  # Normalize team names - trim whitespace first
  if ("odds_team" %in% names(odds_data)) {
    odds_data <- odds_data %>%
      mutate(
        odds_team = trimws(odds_team),
        odds_opponent = trimws(odds_opponent),
        odds_team_normalized = normalize_team_names(odds_team),
        odds_opponent_normalized = normalize_team_names(odds_opponent)
      )
    log_debug("Odds: Teams:", paste(unique(odds_data$odds_team_normalized), collapse = ", "), level = "DEBUG")
  } else {
    log_debug("Odds: WARNING - no odds_team column found!", level = "WARN")
  }
  
  return(odds_data)
}

soccer_fanteam_contests_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("soccer_fanteam_contests_ui() called with id:", id, level = "INFO")
  
  tagList(
    # Enable shinyjs for button active state toggling
    shinyjs::useShinyjs(),
    
    # JavaScript for bar updates, goals stepper, and auto-balancing
    tags$head(tags$script(HTML("
      $(document).ready(function() {
        console.log('FanTeam module JS loaded');
        
        // Auto-balance probability inputs when one changes
        $(document).on('change', '.prob-inputs input[type=number]', function() {
          var changed = $(this);
          var container = changed.closest('.prob-inputs');
          var inputs = container.find('input[type=number]');
          
          // Get all three inputs
          var homeInput = inputs.eq(0);
          var drawInput = inputs.eq(1);
          var awayInput = inputs.eq(2);
          
          var homeVal = parseFloat(homeInput.val()) || 0;
          var drawVal = parseFloat(drawInput.val()) || 0;
          var awayVal = parseFloat(awayInput.val()) || 0;
          
          var total = homeVal + drawVal + awayVal;
          
          if (total !== 100 && total > 0) {
            // Figure out which input changed
            var changedIdx = inputs.index(changed);
            var changedVal = parseFloat(changed.val()) || 0;
            
            // Get the other two inputs
            var others = inputs.not(changed);
            var other1 = others.eq(0);
            var other2 = others.eq(1);
            var other1Val = parseFloat(other1.val()) || 0;
            var other2Val = parseFloat(other2.val()) || 0;
            var othersTotal = other1Val + other2Val;
            
            // Calculate remaining to distribute
            var remaining = 100 - changedVal;
            
            if (remaining < 0) remaining = 0;
            if (remaining > 100) remaining = 100;
            
            // Distribute proportionally among the other two
            if (othersTotal > 0) {
              var new1 = Math.round(remaining * (other1Val / othersTotal));
              var new2 = remaining - new1;
              other1.val(Math.max(0, new1));
              other2.val(Math.max(0, new2));
            } else {
              // Split evenly if both are zero
              other1.val(Math.round(remaining / 2));
              other2.val(remaining - Math.round(remaining / 2));
            }
            
            // Trigger change on the adjusted inputs to update Shiny
            other1.trigger('change');
            other2.trigger('change');
          }
        });
        
        // Goals stepper buttons
        $(document).on('click', '.stepper-btn', function(e) {
          e.preventDefault();
          var btn = $(this);
          var targetId = btn.attr('data-target');
          var input = $('#' + targetId);
          var isUp = btn.hasClass('goals-up');
          
          var current = parseFloat(input.val()) || 2.5;
          var step = 0.1;
          var newVal = isUp ? (current + step) : Math.max(0.5, current - step);
          newVal = Math.round(newVal * 10) / 10;
          
          input.val(newVal).trigger('change');
        });
      });
      
      Shiny.addCustomMessageHandler('updateFanteamMatch', function(msg) {
        // Normalize percentages for bar width
        var total = msg.home + msg.draw + msg.away;
        var normHome = total > 0 ? (msg.home / total * 100) : 33.3;
        var normDraw = total > 0 ? (msg.draw / total * 100) : 33.3;
        var normAway = total > 0 ? (msg.away / total * 100) : 33.3;
        
        var barHome = document.getElementById(msg.bar_home_id);
        var barDraw = document.getElementById(msg.bar_draw_id);
        var barAway = document.getElementById(msg.bar_away_id);
        
        if (barHome) { barHome.style.width = normHome.toFixed(1) + '%'; }
        if (barDraw) { barDraw.style.width = normDraw.toFixed(1) + '%'; }
        if (barAway) { barAway.style.width = normAway.toFixed(1) + '%'; }
        
        // Update my view values - Result, Shots, and CS%
        var myResult = document.getElementById(msg.my_result_id);
        var myHomeShots = document.getElementById(msg.my_home_shots_id);
        var myAwayShots = document.getElementById(msg.my_away_shots_id);
        var myHomeCS = document.getElementById(msg.my_home_cs_id);
        var myAwayCS = document.getElementById(msg.my_away_cs_id);
        var homeShotsArrow = document.getElementById(msg.home_shots_arrow_id);
        var awayShotsArrow = document.getElementById(msg.away_shots_arrow_id);
        var homeCSArrow = document.getElementById(msg.home_cs_arrow_id);
        var awayCSArrow = document.getElementById(msg.away_cs_arrow_id);
        
        if (myResult) myResult.innerText = msg.my_result;
        if (myHomeShots) myHomeShots.innerText = msg.my_home_shots;
        if (myAwayShots) myAwayShots.innerText = msg.my_away_shots;
        if (myHomeCS) myHomeCS.innerText = msg.my_home_cs;
        if (myAwayCS) myAwayCS.innerText = msg.my_away_cs;
        
        if (homeShotsArrow) {
          homeShotsArrow.className = 'change-arrow' + (msg.home_shots_arrow === 'up' ? ' up' : (msg.home_shots_arrow === 'down' ? ' down' : ''));
          homeShotsArrow.innerHTML = msg.home_shots_arrow === 'up' ? '\u25B2' : (msg.home_shots_arrow === 'down' ? '\u25BC' : '');
        }
        if (awayShotsArrow) {
          awayShotsArrow.className = 'change-arrow' + (msg.away_shots_arrow === 'up' ? ' up' : (msg.away_shots_arrow === 'down' ? ' down' : ''));
          awayShotsArrow.innerHTML = msg.away_shots_arrow === 'up' ? '\u25B2' : (msg.away_shots_arrow === 'down' ? '\u25BC' : '');
        }
        if (homeCSArrow) {
          homeCSArrow.className = 'change-arrow' + (msg.home_cs_arrow === 'up' ? ' up' : (msg.home_cs_arrow === 'down' ? ' down' : ''));
          homeCSArrow.innerHTML = msg.home_cs_arrow === 'up' ? '\u25B2' : (msg.home_cs_arrow === 'down' ? '\u25BC' : '');
        }
        if (awayCSArrow) {
          awayCSArrow.className = 'change-arrow' + (msg.away_cs_arrow === 'up' ? ' up' : (msg.away_cs_arrow === 'down' ? ' down' : ''));
          awayCSArrow.innerHTML = msg.away_cs_arrow === 'up' ? '\u25B2' : (msg.away_cs_arrow === 'down' ? '\u25BC' : '');
        }
      });
    "))),
    
    # Page header
    div(class = "page-header",
        tags$h2("FanTeam Projections"),
        tags$p(class = "text-muted", "Adjust match probabilities to generate custom projections")
    ),
    
    # FILTERS CARD
    ui_card(title = "Contest Selection", color = "sage",
            fluidRow(
              column(3, shinyWidgets::pickerInput(ns("gameweek"), "Gameweek", choices = c("Loading..." = ""), selected = NULL)),
              column(4, shinyWidgets::pickerInput(ns("slate"), "Slate", choices = c("Classic (Full)" = "classic"), selected = "classic")),
              column(5, div(style = "padding-top: 25px; text-align: right;",
                            tags$button(id = ns("refresh_data"), class = "btn btn-refresh-subtle", type = "button", "Refresh")
              ))
            )
    ),
    
    tags$br(),
    
    # MATCH ODDS CARD
    ui_card(title = "Your View", color = "sage",
            tags$p(class = "text-muted", style = "font-size: 0.85rem; margin-bottom: 1rem;",
                   "Adjust win/draw/loss probabilities and total goals."
            ),
            div(id = ns("match_grid_wrapper"), 
                style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 1.25rem;",
                uiOutput(ns("matches_ui"))
            ),
            div(style = "margin-top: 1.5rem; text-align: center;",
                actionButton(ns("calculate"), "Calculate Projections", class = "btn-primary")
            )
    ),
    
    tags$br(),
    
    # PROJECTIONS CARD  
    ui_card(title = "Projected Points", color = "sage",
            # Row 0: Table/Plot toggle
            div(style = "display: flex; align-items: center; gap: 0.5rem; margin-bottom: 1rem;",
                actionButton(ns("output_table"), "Table", class = "btn-position-filter btn-output-toggle active"),
                actionButton(ns("output_plot"), "Plot", class = "btn-position-filter btn-output-toggle")
            ),
            # Plot controls container (hidden when table is selected - default)
            div(id = ns("plot_controls"), style = "display: none;",
                # Chart controls row 1
                div(style = "display: flex; align-items: center; gap: 1.5rem; margin-bottom: 0.75rem; flex-wrap: wrap;",
                    # View toggle
                    div(style = "display: flex; align-items: center; gap: 0.5rem;",
                        span(style = "font-weight: 600; color: #3B3226; margin-right: 0.25rem;", "View:"),
                        actionButton(ns("view_value"), "Value", class = "btn-position-filter btn-view-toggle active"),
                        actionButton(ns("view_delta"), "Adjustments", class = "btn-position-filter btn-view-toggle")
                    ),
                    # Position filter
                    div(style = "display: flex; align-items: center; gap: 0.5rem;",
                        span(style = "font-weight: 600; color: #3B3226; margin-right: 0.25rem;", "Position:"),
                        actionButton(ns("pos_gk"), "GK", class = "btn-position-filter btn-pos-toggle"),
                        actionButton(ns("pos_def"), "DEF", class = "btn-position-filter btn-pos-toggle"),
                        actionButton(ns("pos_mid"), "MID", class = "btn-position-filter btn-pos-toggle"),
                        actionButton(ns("pos_fwd"), "FWD", class = "btn-position-filter btn-pos-toggle active")
                    ),
                    # Team highlight selector
                    div(style = "display: flex; align-items: center; gap: 0.5rem;",
                        span(style = "font-weight: 600; color: #3B3226; margin-right: 0.25rem;", "Highlight:"),
                        shinyWidgets::pickerInput(
                          ns("highlight_team"), 
                          label = NULL,
                          choices = c("None" = ""),
                          selected = "",
                          options = list(
                            style = "btn-outline-secondary",
                            size = 10,
                            `live-search` = TRUE
                          ),
                          width = "180px"
                        )
                    )
                ),
                # Chart controls row 2 - FOS weight presets
                div(style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem; flex-wrap: wrap;",
                    span(style = "font-weight: 600; color: #3B3226; font-size: 0.9rem;", "FOS Weights:"),
                    # Preset buttons
                    div(style = "display: flex; align-items: center; gap: 0.5rem;",
                        actionButton(ns("fos_cs_heavy"), "CS Heavy", class = "btn-position-filter btn-fos-toggle", 
                                     title = "80% Clean Sheet / 20% Goals"),
                        actionButton(ns("fos_balanced"), "Balanced", class = "btn-position-filter btn-fos-toggle",
                                     title = "50% Clean Sheet / 50% Goals"),
                        actionButton(ns("fos_attack_lean"), "Attack Lean", class = "btn-position-filter btn-fos-toggle",
                                     title = "25% Clean Sheet / 75% Goals"),
                        actionButton(ns("fos_attack_heavy"), "Attack Heavy", class = "btn-position-filter btn-fos-toggle active",
                                     title = "10% Clean Sheet / 90% Goals")
                    ),
                    # Current weights display
                    div(style = "display: flex; align-items: center; gap: 0.25rem; padding: 6px 12px; background: #F5F3F0; border-radius: 6px; font-size: 0.85rem;",
                        span(style = "color: #5C4E3D;", "CS:"),
                        span(style = "font-weight: 600; color: #3B3226;", textOutput(ns("fos_cs_weight_display"), inline = TRUE)),
                        span(style = "color: #5C4E3D; margin-left: 8px;", "Goals:"),
                        span(style = "font-weight: 600; color: #3B3226;", textOutput(ns("fos_gf_weight_display"), inline = TRUE))
                    )
                ),
                # Chart output with external axis labels wrapper (hidden by default since table is default view)
                uiOutput(ns("chart_wrapper"))
            ),
            # Table output container (visible by default)
            div(id = ns("table_container"),
                reactableOutput(ns("projections_table"))
            )
    ),
    
    # CSS - Part 1: Grid wrapper rules (need ns() interpolation)
    tags$style(HTML(sprintf("
      #%s > .shiny-html-output { display: contents; }
      #%s { display: grid !important; grid-template-columns: 1fr 1fr !important; gap: 1.25rem !important; }
      @media (max-width: 900px) { #%s { grid-template-columns: 1fr !important; } }
    ", ns("match_grid_wrapper"), ns("match_grid_wrapper"), ns("match_grid_wrapper")))),
    
    # NOTE: Styles use generic classes from styles.css
    
  )
}

soccer_fanteam_contests_server <- function(id, soccer_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_fanteam_contests_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    rv <- reactiveValues(
      salaries = NULL, odds = NULL, matches = NULL,
      projections = NULL, initialized = FALSE,
      output_view = "table",      # "table" or "plot"
      chart_view = "value",      # "value" or "delta"
      chart_position = "FWD",    # "GK", "DEF", "MID", "FWD"
      fos_preset = "attack_heavy" # "cs_heavy", "balanced", "attack_lean", "attack_heavy"
    )
    
    # =========================================================================
    # CHART FILTER BUTTON OBSERVERS
    # =========================================================================
    
    # Helper to update output toggle button active states
    update_output_buttons <- function(active_output) {
      outputs <- c("table", "plot")
      for (o in outputs) {
        btn_id <- paste0("output_", o)
        if (o == active_output) {
          shinyjs::addClass(id = btn_id, class = "active")
        } else {
          shinyjs::removeClass(id = btn_id, class = "active")
        }
      }
      # Show/hide containers
      if (active_output == "table") {
        shinyjs::hide(id = "plot_controls")
        shinyjs::show(id = "table_container")
      } else {
        shinyjs::show(id = "plot_controls")
        shinyjs::hide(id = "table_container")
      }
    }
    
    # Output toggle button observers
    observeEvent(input$output_table, {
      rv$output_view <- "table"
      update_output_buttons("table")
    })
    observeEvent(input$output_plot, {
      rv$output_view <- "plot"
      update_output_buttons("plot")
    })
    
    # Helper to update view button active states
    update_view_buttons <- function(active_view) {
      views <- c("value", "delta")
      for (v in views) {
        btn_id <- paste0("view_", v)
        if (v == active_view) {
          shinyjs::addClass(id = btn_id, class = "active")
        } else {
          shinyjs::removeClass(id = btn_id, class = "active")
        }
      }
    }
    
    # Helper to update position button active states
    update_pos_buttons <- function(active_pos) {
      positions <- c("gk", "def", "mid", "fwd")
      for (p in positions) {
        btn_id <- paste0("pos_", p)
        if (tolower(p) == tolower(active_pos)) {
          shinyjs::addClass(id = btn_id, class = "active")
        } else {
          shinyjs::removeClass(id = btn_id, class = "active")
        }
      }
    }
    
    # View button observers
    observeEvent(input$view_value, {
      rv$chart_view <- "value"
      update_view_buttons("value")
    })
    observeEvent(input$view_delta, {
      rv$chart_view <- "delta"
      update_view_buttons("delta")
    })
    
    # FOS preset definitions
    fos_presets <- list(
      cs_heavy = c(cs = 0.80, gf = 0.20),
      balanced = c(cs = 0.50, gf = 0.50),
      attack_lean = c(cs = 0.25, gf = 0.75),
      attack_heavy = c(cs = 0.10, gf = 0.90)
    )
    
    # Helper to update FOS button active states
    update_fos_buttons <- function(active_preset) {
      presets <- c("cs_heavy", "balanced", "attack_lean", "attack_heavy")
      for (preset in presets) {
        btn_id <- paste0("fos_", preset)
        if (preset == active_preset) {
          shinyjs::addClass(id = btn_id, class = "active")
        } else {
          shinyjs::removeClass(id = btn_id, class = "active")
        }
      }
    }
    
    # Position button observers - auto-select appropriate FOS preset
    observeEvent(input$pos_gk, {
      rv$chart_position <- "GK"
      update_pos_buttons("gk")
      rv$fos_preset <- "cs_heavy"
      update_fos_buttons("cs_heavy")
    })
    observeEvent(input$pos_def, {
      rv$chart_position <- "DEF"
      update_pos_buttons("def")
      rv$fos_preset <- "balanced"
      update_fos_buttons("balanced")
    })
    observeEvent(input$pos_mid, {
      rv$chart_position <- "MID"
      update_pos_buttons("mid")
      rv$fos_preset <- "attack_lean"
      update_fos_buttons("attack_lean")
    })
    observeEvent(input$pos_fwd, {
      rv$chart_position <- "FWD"
      update_pos_buttons("fwd")
      rv$fos_preset <- "attack_heavy"
      update_fos_buttons("attack_heavy")
    })
    
    # FOS preset button observers
    observeEvent(input$fos_cs_heavy, {
      rv$fos_preset <- "cs_heavy"
      update_fos_buttons("cs_heavy")
    })
    observeEvent(input$fos_balanced, {
      rv$fos_preset <- "balanced"
      update_fos_buttons("balanced")
    })
    observeEvent(input$fos_attack_lean, {
      rv$fos_preset <- "attack_lean"
      update_fos_buttons("attack_lean")
    })
    observeEvent(input$fos_attack_heavy, {
      rv$fos_preset <- "attack_heavy"
      update_fos_buttons("attack_heavy")
    })
    
    # FOS weights display outputs
    output$fos_cs_weight_display <- renderText({
      preset <- rv$fos_preset %||% "attack_heavy"
      weights <- fos_presets[[preset]]
      paste0(round(weights["cs"] * 100), "%")
    })
    
    output$fos_gf_weight_display <- renderText({
      preset <- rv$fos_preset %||% "attack_heavy"
      weights <- fos_presets[[preset]]
      paste0(round(weights["gf"] * 100), "%")
    })
    
    # Reactive FOS weights based on preset
    fos_weights_reactive <- reactive({
      preset <- rv$fos_preset %||% "attack_heavy"
      fos_presets[[preset]]
    })
    
    # Initialize gameweeks
    observe({
      if (rv$initialized) return()
      log_debug("Initializing gameweek choices...", level = "INFO")
      gameweeks <- tryCatch(get_fanteam_soccer_gameweeks(), error = function(e) c())
      if (length(gameweeks) > 0) {
        shinyWidgets::updatePickerInput(session, "gameweek",
                                        choices = setNames(gameweeks, paste("Gameweek", gameweeks)),
                                        selected = gameweeks[1]
        )
      }
      rv$initialized <- TRUE
    })
    
    # Update slates
    observeEvent(input$gameweek, {
      req(input$gameweek)
      gw <- as.integer(input$gameweek)
      slate_choices <- c("Classic (Full)" = "classic")
      if (dir.exists(FANTEAM_SOCCER_DIR)) {
        pattern <- sprintf("week_%d_.*_vs_.*\\.csv", gw)
        for (f in list.files(FANTEAM_SOCCER_DIR, pattern = pattern)) {
          match_info <- gsub(sprintf("week_%d_(.*)_vs_(.*)\\.csv", gw), "\\1 vs \\2", f)
          slate_choices <- c(slate_choices, setNames(f, gsub("_", " ", match_info)))
        }
      }
      shinyWidgets::updatePickerInput(session, "slate", choices = slate_choices, selected = "classic")
    })
    
    # Load data - ALL logic inlined to avoid reactive scope issues
    observeEvent(list(input$gameweek, input$slate), {
      req(input$gameweek)
      gw <- as.integer(input$gameweek)
      current_slate <- input$slate
      log_debug("Loading data for GW:", gw, "Slate:", current_slate, level = "INFO")
      
      # Load salaries
      salaries_data <- tryCatch({
        if (is.null(current_slate) || current_slate == "classic") {
          load_fanteam_soccer_with_logos(gw)
        } else {
          fp <- file.path(FANTEAM_SOCCER_DIR, current_slate)
          if (file.exists(fp)) {
            read_csv(fp, show_col_types = FALSE) %>% clean_names() %>%
              mutate(
                player = paste(f_name, name),
                team = ifelse(club %in% names(FANTEAM_CLUB_MAPPING), FANTEAM_CLUB_MAPPING[club], club),
                position = case_when(tolower(position) == "goalkeeper" ~ "GK", tolower(position) == "defender" ~ "DEF",
                                     tolower(position) == "midfielder" ~ "MID", tolower(position) == "forward" ~ "FWD", TRUE ~ "MID"),
                salary = as.numeric(price), team_normalized = normalize_team_names(team),
                logo_path = sapply(team_normalized, function(t) get_soccer_team_logo(t) %||% "")
              )
          } else NULL
        }
      }, error = function(e) { log_debug("Error loading salaries:", e$message, level = "ERROR"); NULL })
      
      rv$salaries <- salaries_data
      
      if (!is.null(salaries_data)) {
        log_debug("Loaded", nrow(salaries_data), "players", level = "INFO")
      }
      
      # Load odds using existing function
      odds_data <- tryCatch({
        od <- load_fanteam_odds(gw)
        if (!is.null(od)) {
          log_debug("Loaded odds with columns:", paste(names(od), collapse = ", "), level = "INFO")
          log_debug("Odds has", nrow(od), "rows", level = "INFO")
        }
        od
      }, error = function(e) { 
        log_debug("Error loading odds:", e$message, level = "ERROR")
        NULL 
      })
      
      rv$odds <- odds_data
      
      # Build matches - ALL INLINED
      teams <- if (!is.null(salaries_data) && nrow(salaries_data) > 0) {
        unique(salaries_data$team_normalized[!is.na(salaries_data$team_normalized)])
      } else NULL
      
      if (is.null(teams) || length(teams) == 0) { 
        rv$matches <- NULL
        return()
      }
      
      # Filter for slate if needed
      if (!is.null(current_slate) && current_slate != "classic") {
        mi <- gsub("week_\\d+_(.*)_vs_(.*)\\.csv", "\\1|\\2", current_slate)
        st <- normalize_team_names(gsub("_", " ", strsplit(mi, "\\|")[[1]]))
        teams <- intersect(teams, st)
      }
      
      log_debug("Building matches for", length(teams), "teams", level = "INFO")
      
      # Build matches from odds data (team-centric format)
      if (!is.null(odds_data) && nrow(odds_data) > 0 && "odds_team_normalized" %in% names(odds_data)) {
        log_debug("Using odds data to build matches", level = "INFO")
        
        # Filter to teams in our slate and home teams only (to avoid duplicates)
        home_odds <- odds_data %>%
          filter(
            odds_team_normalized %in% teams,
            home_away == "Home" | home_away == "H"
          )
        
        log_debug("Found", nrow(home_odds), "home team rows in odds", level = "INFO")
        log_debug("Available columns:", paste(names(home_odds), collapse = ", "), level = "INFO")
        log_debug("Has win_pct:", "win_pct" %in% names(home_odds), 
                  "Has draw_pct:", "draw_pct" %in% names(home_odds),
                  "Has implied_team_goals:", "implied_team_goals" %in% names(home_odds), level = "INFO")
        
        if (nrow(home_odds) > 0) {
          # Add default columns if missing (avoids errors in transmute)
          if (!"win_pct" %in% names(home_odds)) home_odds$win_pct <- 40
          if (!"draw_pct" %in% names(home_odds)) home_odds$draw_pct <- 30
          if (!"implied_team_goals" %in% names(home_odds)) home_odds$implied_team_goals <- 1.3
          if (!"implied_opp_goals" %in% names(home_odds)) home_odds$implied_opp_goals <- 1.2
          
          matches_data <- home_odds %>%
            transmute(
              match_id = row_number(),
              home_team = odds_team_normalized,
              away_team = odds_opponent_normalized,
              market_home_win = as.numeric(win_pct),
              market_draw = as.numeric(draw_pct),
              market_away_win = 100 - market_home_win - market_draw,
              market_home_goals = as.numeric(implied_team_goals),
              market_away_goals = as.numeric(implied_opp_goals),
              market_total = market_home_goals + market_away_goals
            )
          
          rv$matches <- matches_data
          
          log_debug("Built", nrow(matches_data), "matches from odds", level = "INFO")
          if (nrow(matches_data) > 0) {
            log_debug("First match:", matches_data$home_team[1], "vs", matches_data$away_team[1], 
                      "H:", round(matches_data$market_home_win[1]), "D:", round(matches_data$market_draw[1]), 
                      "A:", round(matches_data$market_away_win[1]), 
                      "Goals:", matches_data$market_home_goals[1], "-", matches_data$market_away_goals[1], level = "INFO")
          }
          return()
        }
      }
      
      # Fallback: create matches from team pairs
      log_debug("No odds data - creating default matches from team pairs", level = "WARN")
      n <- length(teams)
      if (n < 2) { rv$matches <- NULL; return() }
      
      rv$matches <- data.frame(
        match_id = seq_len(floor(n/2)), 
        home_team = teams[seq(1, n-1, 2)], 
        away_team = teams[seq(2, n, 2)],
        market_home_win = 40, market_draw = 30, market_away_win = 30, 
        market_total = 2.5, market_home_goals = 1.3, market_away_goals = 1.2,
        stringsAsFactors = FALSE
      )
    })
    
    # ==========================================================================
    # CALCULATIONS
    # Goals distributed based on win probabilities:
    #   home_share = home% + 0.5*draw%
    #   home_goals = total * home_share / (home_share + away_share)
    # Shots estimated from win% and total goals
    # ==========================================================================
    
    calc_goals_from_probs <- function(home_pct, draw_pct, away_pct, total_goals) {
      home_share <- home_pct + 0.5 * draw_pct
      away_share <- away_pct + 0.5 * draw_pct
      total_share <- home_share + away_share
      
      if (total_share > 0) {
        home_goals <- total_goals * home_share / total_share
        away_goals <- total_goals * away_share / total_share
      } else {
        home_goals <- total_goals / 2
        away_goals <- total_goals / 2
      }
      
      list(home = home_goals, away = away_goals)
    }
    
    calc_shots <- function(win_pct, total_goals, is_home) {
      c <- if (is_home) FANTEAM_COEFFICIENTS$home_shots else FANTEAM_COEFFICIENTS$away_shots
      c$intercept + (win_pct * c$win_pct) + (total_goals * c$total_goals)
    }
    
    calc_sot <- function(win_pct, total_goals, is_home) {
      c <- if (is_home) FANTEAM_COEFFICIENTS$home_sot else FANTEAM_COEFFICIENTS$away_sot
      c$intercept + (win_pct * c$win_pct) + (total_goals * c$total_goals)
    }
    
    # Render matches
    output$matches_ui <- renderUI({
      req(rv$matches)
      matches <- rv$matches
      if (nrow(matches) == 0) return(tags$p(class = "text-muted", "No matches available."))
      
      log_debug("Rendering", nrow(matches), "match cards", level = "INFO")
      
      tagList(
        lapply(1:nrow(matches), function(i) {
          m <- matches[i, ]
          home_logo <- get_soccer_team_logo(m$home_team)
          away_logo <- get_soccer_team_logo(m$away_team)
          
          # Market shots and SOT using empirical models
          mkt_hs <- calc_shots(m$market_home_win, m$market_total, TRUE)
          mkt_as <- calc_shots(m$market_away_win, m$market_total, FALSE)
          mkt_hsot <- calc_sot(m$market_home_win, m$market_total, TRUE)
          mkt_asot <- calc_sot(m$market_away_win, m$market_total, FALSE)
          
          # Market clean sheet % (Poisson: CS% = exp(-goals_against) * 100)
          mkt_hcs <- exp(-m$market_away_goals) * 100
          mkt_acs <- exp(-m$market_home_goals) * 100
          
          # Normalize bar widths
          bar_total <- m$market_home_win + m$market_draw + m$market_away_win
          norm_h <- if (bar_total > 0) m$market_home_win / bar_total * 100 else 33.3
          norm_d <- if (bar_total > 0) m$market_draw / bar_total * 100 else 33.3
          norm_a <- if (bar_total > 0) m$market_away_win / bar_total * 100 else 33.3
          
          div(class = "match-card",
              # Row 1: Team header
              div(class = "match-card__header",
                  div(class = "team-cell",
                      if (!is.null(home_logo)) tags$img(src = home_logo, class = "team-logo"),
                      span(m$home_team)
                  ),
                  div(class = "team-cell",
                      span(m$away_team),
                      if (!is.null(away_logo)) tags$img(src = away_logo, class = "team-logo")
                  )
              ),
              
              # Row 2: Controls - [Probs section] [Goals stepper]
              div(class = "match-card__controls",
                  # Left: HOME/DRAW/AWAY inputs + bar
                  div(class = "prob-section",
                      div(class = "prob-inputs",
                          div(class = "form-group-inline",
                              span(class = "form-label-sm", "HOME"),
                              numericInput(ns(paste0("home_", i)), NULL, round(m$market_home_win), 0, 100, 1)
                          ),
                          div(class = "form-group-inline",
                              span(class = "form-label-sm", "DRAW"),
                              numericInput(ns(paste0("draw_", i)), NULL, round(m$market_draw), 0, 100, 1)
                          ),
                          div(class = "form-group-inline",
                              span(class = "form-label-sm", "AWAY"),
                              numericInput(ns(paste0("away_", i)), NULL, round(m$market_away_win), 0, 100, 1)
                          )
                      ),
                      div(class = "prob-bar",
                          div(class = "prob-bar__segment home", id = ns(paste0("bar_h_", i)),
                              style = sprintf("width:%.1f%%;", norm_h)),
                          div(class = "prob-bar__segment draw", id = ns(paste0("bar_d_", i)),
                              style = sprintf("width:%.1f%%;", norm_d)),
                          div(class = "prob-bar__segment away", id = ns(paste0("bar_a_", i)),
                              style = sprintf("width:%.1f%%;", norm_a))
                      )
                  ),
                  # Right: Goals stepper - up arrow, number, down arrow
                  div(class = "stepper-section",
                      span(class = "form-label-sm", "GOALS"),
                      div(class = "number-stepper",
                          tags$button(type = "button", class = "stepper-btn goals-up", 
                                      `data-target` = ns(paste0("total_", i)),
                                      span(class = "spinner-arrow up")),
                          numericInput(ns(paste0("total_", i)), NULL, round(m$market_total, 1), 0.5, 8, 0.1),
                          tags$button(type = "button", class = "stepper-btn goals-down", 
                                      `data-target` = ns(paste0("total_", i)),
                                      span(class = "spinner-arrow down"))
                      )
                  )
              ),
              
              # Row 3: Results
              div(class = "results-grid",
                  div(class = "results-grid__col",
                      div(class = "results-grid__header", "Market Implied"),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Result:"),
                          span(class = "results-grid__value", sprintf("%.1f - %.1f", m$market_home_goals, m$market_away_goals))
                      ),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Home Shots:"),
                          span(class = "results-grid__value", sprintf("%.1f (%.1f)", mkt_hs, mkt_hsot))
                      ),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Away Shots:"),
                          span(class = "results-grid__value", sprintf("%.1f (%.1f)", mkt_as, mkt_asot))
                      ),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Home CS%:"),
                          span(class = "results-grid__value", sprintf("%.0f%%", mkt_hcs))
                      ),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Away CS%:"),
                          span(class = "results-grid__value", sprintf("%.0f%%", mkt_acs))
                      )
                  ),
                  div(class = "results-grid__col",
                      div(class = "results-grid__header", "My View"),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Result:"),
                          span(class = "results-grid__value", id = ns(paste0("my_res_", i)),
                               sprintf("%.1f - %.1f", m$market_home_goals, m$market_away_goals))
                      ),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Home Shots:"),
                          span(style = "display: flex; align-items: center; justify-content: flex-end; flex: 1;",
                               span(class = "change-arrow", id = ns(paste0("arr_hs_", i))),
                               span(class = "results-grid__value", id = ns(paste0("my_hs_", i)), 
                                    sprintf("%.1f (%.1f)", mkt_hs, mkt_hsot))
                          )
                      ),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Away Shots:"),
                          span(style = "display: flex; align-items: center; justify-content: flex-end; flex: 1;",
                               span(class = "change-arrow", id = ns(paste0("arr_as_", i))),
                               span(class = "results-grid__value", id = ns(paste0("my_as_", i)), 
                                    sprintf("%.1f (%.1f)", mkt_as, mkt_asot))
                          )
                      ),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Home CS%:"),
                          span(style = "display: flex; align-items: center; justify-content: flex-end; flex: 1;",
                               span(class = "change-arrow", id = ns(paste0("arr_hcs_", i))),
                               span(class = "results-grid__value", id = ns(paste0("my_hcs_", i)), 
                                    sprintf("%.0f%%", mkt_hcs))
                          )
                      ),
                      div(class = "results-grid__row", 
                          span(class = "results-grid__label", "Away CS%:"),
                          span(style = "display: flex; align-items: center; justify-content: flex-end; flex: 1;",
                               span(class = "change-arrow", id = ns(paste0("arr_acs_", i))),
                               span(class = "results-grid__value", id = ns(paste0("my_acs_", i)), 
                                    sprintf("%.0f%%", mkt_acs))
                          )
                      )
                  )
              )
          )
        })
      )
    })
    
    # Input observers - create a reactive for each possible match index
    # These will only trigger once the corresponding inputs exist
    lapply(1:20, function(i) {  # Support up to 20 matches
      local({
        idx <- i
        observe({
          # Only run if we have matches and this index exists
          req(rv$matches)
          if (idx > nrow(rv$matches)) return()
          
          m <- rv$matches[idx, ]
          
          # Get current input values
          h <- input[[paste0("home_", idx)]]
          d <- input[[paste0("draw_", idx)]]
          a <- input[[paste0("away_", idx)]]
          t <- input[[paste0("total_", idx)]]
          
          # Require all inputs to have values
          req(h, d, a, t)
          
          # Calculate market reference values
          mkt_hs <- calc_shots(m$market_home_win, m$market_total, TRUE)
          mkt_as <- calc_shots(m$market_away_win, m$market_total, FALSE)
          mkt_hcs <- exp(-m$market_away_goals) * 100
          mkt_acs <- exp(-m$market_home_goals) * 100
          
          # Calculate goals using correct distribution
          goals <- calc_goals_from_probs(h, d, a, t)
          my_hg <- goals$home
          my_ag <- goals$away
          
          # Calculate shots and SOT using empirical models
          my_hs <- calc_shots(h, t, TRUE)
          my_as <- calc_shots(a, t, FALSE)
          my_hsot <- calc_sot(h, t, TRUE)
          my_asot <- calc_sot(a, t, FALSE)
          
          # Calculate CS% (Poisson: CS% = exp(-goals_against) * 100)
          my_hcs <- exp(-my_ag) * 100
          my_acs <- exp(-my_hg) * 100
          
          # Determine arrows
          hs_arr <- if (my_hs > mkt_hs + 0.1) "up" else if (my_hs < mkt_hs - 0.1) "down" else "none"
          as_arr <- if (my_as > mkt_as + 0.1) "up" else if (my_as < mkt_as - 0.1) "down" else "none"
          hcs_arr <- if (my_hcs > mkt_hcs + 1) "up" else if (my_hcs < mkt_hcs - 1) "down" else "none"
          acs_arr <- if (my_acs > mkt_acs + 1) "up" else if (my_acs < mkt_acs - 1) "down" else "none"
          
          # Send update to JavaScript
          session$sendCustomMessage("updateFanteamMatch", list(
            bar_home_id = ns(paste0("bar_h_", idx)),
            bar_draw_id = ns(paste0("bar_d_", idx)),
            bar_away_id = ns(paste0("bar_a_", idx)),
            my_result_id = ns(paste0("my_res_", idx)),
            my_home_shots_id = ns(paste0("my_hs_", idx)),
            my_away_shots_id = ns(paste0("my_as_", idx)),
            my_home_cs_id = ns(paste0("my_hcs_", idx)),
            my_away_cs_id = ns(paste0("my_acs_", idx)),
            home_shots_arrow_id = ns(paste0("arr_hs_", idx)),
            away_shots_arrow_id = ns(paste0("arr_as_", idx)),
            home_cs_arrow_id = ns(paste0("arr_hcs_", idx)),
            away_cs_arrow_id = ns(paste0("arr_acs_", idx)),
            home = round(h), draw = round(d), away = round(a),
            my_result = sprintf("%.1f - %.1f", my_hg, my_ag),
            my_home_shots = sprintf("%.1f (%.1f)", my_hs, my_hsot),
            my_away_shots = sprintf("%.1f (%.1f)", my_as, my_asot),
            my_home_cs = sprintf("%.0f%%", my_hcs),
            my_away_cs = sprintf("%.0f%%", my_acs),
            home_shots_arrow = hs_arr, away_shots_arrow = as_arr,
            home_cs_arrow = hcs_arr, away_cs_arrow = acs_arr
          ))
        })
      })
    })
    
    # Calculate projections
    observeEvent(input$calculate, {
      req(rv$matches, rv$salaries)
      matches <- rv$matches
      
      # Build lookup for ADJUSTED projections (user inputs)
      team_lookup_adj <- list()
      # Build lookup for BASELINE projections (market odds)
      team_lookup_mkt <- list()
      
      for (i in 1:nrow(matches)) {
        m <- matches[i, ]
        
        # User adjusted values
        h <- input[[paste0("home_", i)]] %||% m$market_home_win
        d <- input[[paste0("draw_", i)]] %||% m$market_draw
        a <- input[[paste0("away_", i)]] %||% m$market_away_win
        t <- input[[paste0("total_", i)]] %||% m$market_total
        
        # Market baseline values
        mkt_h <- m$market_home_win
        mkt_d <- m$market_draw
        mkt_a <- m$market_away_win
        mkt_t <- m$market_total
        
        # Adjusted goals
        goals_adj <- calc_goals_from_probs(h, d, a, t)
        team_lookup_adj[[m$home_team]] <- list(
          gf = goals_adj$home, ga = goals_adj$away,
          cs = exp(-goals_adj$away) * 100, shots = calc_shots(h, t, TRUE)
        )
        team_lookup_adj[[m$away_team]] <- list(
          gf = goals_adj$away, ga = goals_adj$home,
          cs = exp(-goals_adj$home) * 100, shots = calc_shots(a, t, FALSE)
        )
        
        # Market baseline goals
        goals_mkt <- calc_goals_from_probs(mkt_h, mkt_d, mkt_a, mkt_t)
        team_lookup_mkt[[m$home_team]] <- list(
          gf = goals_mkt$home, ga = goals_mkt$away,
          cs = exp(-goals_mkt$away) * 100, shots = calc_shots(mkt_h, mkt_t, TRUE)
        )
        team_lookup_mkt[[m$away_team]] <- list(
          gf = goals_mkt$away, ga = goals_mkt$home,
          cs = exp(-goals_mkt$home) * 100, shots = calc_shots(mkt_a, mkt_t, FALSE)
        )
      }
      
      rv$projections <- rv$salaries %>%
        mutate(
          # Adjusted projections
          proj_gf = sapply(team_normalized, function(t) if (t %in% names(team_lookup_adj)) team_lookup_adj[[t]]$gf else NA_real_),
          proj_cs = sapply(team_normalized, function(t) if (t %in% names(team_lookup_adj)) team_lookup_adj[[t]]$cs else NA_real_),
          proj_pts = case_when(
            position == "GK" ~ 2 + (proj_cs/100 * 4) - 0.5 * sapply(team_normalized, function(t) if (t %in% names(team_lookup_adj)) team_lookup_adj[[t]]$ga else 1),
            position == "DEF" ~ 2 + (proj_cs/100 * 4) + 0.3 * proj_gf,
            position == "MID" ~ 2 + 0.8 * proj_gf + 0.1 * sapply(team_normalized, function(t) if (t %in% names(team_lookup_adj)) team_lookup_adj[[t]]$shots else 10),
            position == "FWD" ~ 2 + 1.2 * proj_gf + 0.15 * sapply(team_normalized, function(t) if (t %in% names(team_lookup_adj)) team_lookup_adj[[t]]$shots else 10),
            TRUE ~ 2
          ),
          value = proj_pts / salary,
          
          # Baseline projections (from market odds)
          base_gf = sapply(team_normalized, function(t) if (t %in% names(team_lookup_mkt)) team_lookup_mkt[[t]]$gf else NA_real_),
          base_cs = sapply(team_normalized, function(t) if (t %in% names(team_lookup_mkt)) team_lookup_mkt[[t]]$cs else NA_real_),
          base_pts = case_when(
            position == "GK" ~ 2 + (base_cs/100 * 4) - 0.5 * sapply(team_normalized, function(t) if (t %in% names(team_lookup_mkt)) team_lookup_mkt[[t]]$ga else 1),
            position == "DEF" ~ 2 + (base_cs/100 * 4) + 0.3 * base_gf,
            position == "MID" ~ 2 + 0.8 * base_gf + 0.1 * sapply(team_normalized, function(t) if (t %in% names(team_lookup_mkt)) team_lookup_mkt[[t]]$shots else 10),
            position == "FWD" ~ 2 + 1.2 * base_gf + 0.15 * sapply(team_normalized, function(t) if (t %in% names(team_lookup_mkt)) team_lookup_mkt[[t]]$shots else 10),
            TRUE ~ 2
          ),
          base_value = base_pts / salary,
          
          # Deltas
          delta_pts = proj_pts - base_pts,
          delta_value = value - base_value
        ) %>% filter(!is.na(proj_pts)) %>% arrange(desc(proj_pts))
      
      # Update team highlight dropdown with logos
      teams <- sort(unique(rv$projections$team_normalized))
      if (length(teams) > 0) {
        team_content <- c(
          "None",  # First option - no highlight
          sapply(teams, function(team) {
            logo_path <- get_soccer_team_logo(team)
            if (!is.null(logo_path) && logo_path != "") {
              sprintf('<img src="%s" style="width:20px;height:20px;margin-right:8px;vertical-align:middle;object-fit:contain;">%s', 
                      logo_path, team)
            } else {
              team
            }
          }, USE.NAMES = FALSE)
        )
        
        shinyWidgets::updatePickerInput(
          session, "highlight_team",
          choices = c("None" = "", setNames(teams, teams)),
          selected = "",
          choicesOpt = list(content = team_content)
        )
      }
    })
    
    # =========================================================================
    # PROJECTION CHART (FOS vs Salary scatter, or Delta bar)
    # =========================================================================
    
    # Position counts - max players per position per team
    pos_counts <- c("GK" = 2, "DEF" = 5, "MID" = 5, "FWD" = 3)
    
    # Chart wrapper with external axis labels
    output$chart_wrapper <- renderUI({
      chart_view <- rv$chart_view
      
      if (chart_view == "value") {
        # VALUE VIEW: Scatter with external axis labels and arrows
        div(
          style = "width: 100%; box-sizing: border-box;",
          
          # Main content area with Y-axis label and plot
          div(
            style = "display: flex; align-items: center; width: 100%;",
            
            # Y-axis label (left side, rotated)
            div(
              style = "flex-shrink: 0; width: 50px; display: flex; align-items: center; justify-content: center;",
              div(
                style = "transform: rotate(-90deg); transform-origin: center; white-space: nowrap;",
                span(
                  style = "font-family: 'Fjalla One', sans-serif; font-size: 16px; color: #666; font-weight: 400;",
                  "AVG SALARY",
                  span(style = "font-size: 20px; margin-left: 6px; vertical-align: middle;", HTML("&#8594;"))
                )
              )
            ),
            
            # Plot area
            div(
              style = "flex: 1; min-width: 0; padding-right: 50px;",
              ggiraph::girafeOutput(session$ns("projection_chart"), height = "580px", width = "100%")
            )
          ),
          
          # X-axis label below
          div(
            style = "display: flex; justify-content: center; padding: 8px 50px 0 50px;",
            span(
              style = "font-family: 'Fjalla One', sans-serif; font-size: 16px; color: #666; font-weight: 400;",
              "FANTASY OPPORTUNITY SCORE",
              span(style = "font-size: 20px; margin-left: 6px; vertical-align: middle;", HTML("&#8594;"))
            )
          )
        )
      } else {
        # ADJUSTMENTS VIEW: Dumbbell with x-axis label at top
        div(
          style = "width: 100%; box-sizing: border-box;",
          
          # X-axis label at top with arrow
          div(
            style = "display: flex; justify-content: flex-end; padding: 0 20px 12px 0;",
            span(
              style = "font-family: 'Fjalla One', sans-serif; font-size: 16px; color: #666; font-weight: 400;",
              "FANTASY OPPORTUNITY SCORE",
              span(style = "font-size: 20px; margin-left: 6px; vertical-align: middle;", HTML("&#8594;"))
            )
          ),
          
          # Plot area
          ggiraph::girafeOutput(session$ns("projection_chart"), height = "600px", width = "100%")
        )
      }
    })
    
    output$projection_chart <- ggiraph::renderGirafe({
      # Check if projections exist
      if (is.null(rv$projections) || nrow(rv$projections) == 0) {
        # Return a placeholder message
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Click 'Calculate Projections' to generate chart",
                   size = 5, color = "#7A7A7A", fontface = "italic") +
          theme_void() +
          theme(plot.background = element_rect(fill = "transparent", color = NA))
        return(ggiraph::girafe(ggobj = p, width_svg = 8, height_svg = 6))
      }
      
      pos_filter <- rv$chart_position
      chart_view <- rv$chart_view
      max_for_pos <- pos_counts[pos_filter]
      
      if (chart_view == "value") {
        # =====================================================================
        # FOS vs SALARY SCATTER PLOT (Interactive)
        # =====================================================================
        
        # Build team-level data with top N players per position
        plot_data <- rv$projections %>%
          filter(position == pos_filter, !is.na(salary), !is.na(proj_gf), !is.na(proj_cs)) %>%
          group_by(team_normalized) %>%
          arrange(desc(salary)) %>%
          mutate(pos_rank = row_number()) %>%
          filter(pos_rank <= max_for_pos) %>%
          summarise(
            avg_salary = mean(salary, na.rm = TRUE),
            implied_team_goals = mean(proj_gf, na.rm = TRUE),
            clean_sheet_pct = mean(proj_cs, na.rm = TRUE),
            n_players = n(),
            .groups = "drop"
          ) %>%
          filter(!is.na(avg_salary), !is.na(clean_sheet_pct), !is.na(implied_team_goals))
        
        if (nrow(plot_data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = paste("No", pos_filter, "data available"),
                     size = 5, color = "#7A7A7A", fontface = "italic") +
            theme_void() +
            theme(plot.background = element_rect(fill = "transparent", color = NA))
          return(ggiraph::girafe(ggobj = p, width_svg = 8, height_svg = 6))
        }
        
        # Normalize goals to 0-100 scale
        gf_min <- min(plot_data$implied_team_goals, na.rm = TRUE)
        gf_max <- max(plot_data$implied_team_goals, na.rm = TRUE)
        gf_range <- gf_max - gf_min
        if (gf_range < 0.01) gf_range <- 1
        
        # Get current FOS weights from slider
        current_weights <- fos_weights_reactive()
        
        # Calculate FOS with user-defined weights and add logo paths
        highlighted_team <- input$highlight_team
        
        plot_data <- plot_data %>%
          mutate(
            gf_normalized = ((implied_team_goals - gf_min) / gf_range) * 100,
            fantasy_opp_score = (clean_sheet_pct * current_weights["cs"]) + 
              (gf_normalized * current_weights["gf"]),
            team_abbr = toupper(get_team_abbreviation(team_normalized)),
            logo_path = sapply(team_normalized, function(t) get_soccer_team_logo(t) %||% ""),
            is_highlighted = !is.null(highlighted_team) && highlighted_team != "" & team_normalized == highlighted_team,
            # Build HTML tooltip
            tooltip_html = paste0(
              "<div style='padding:8px;min-width:160px;'>",
              ifelse(logo_path != "", 
                     paste0("<img src='", logo_path, "' style='width:32px;height:32px;vertical-align:middle;margin-right:8px;'>"), ""),
              "<strong style='font-size:14px;'>", toupper(team_normalized), "</strong><br>",
              "<span style='color:#5C4E3D;'>FOS: <strong>", sprintf("%.1f", fantasy_opp_score), "</strong></span><br>",
              "<span style='color:#5C4E3D;'>Salary: <strong>Ãƒâ€šÃ‚Â£", sprintf("%.2fM", avg_salary), "</strong></span><br>",
              "<span style='color:#5C4E3D;'>CS%: <strong>", sprintf("%.0f%%", clean_sheet_pct), "</strong></span><br>",
              "<span style='color:#5C4E3D;'>xG: <strong>", sprintf("%.2f", implied_team_goals), "</strong></span>",
              "</div>"
            )
          )
        
        # Calculate averages for reference lines
        avg_salary <- mean(plot_data$avg_salary, na.rm = TRUE)
        avg_fos <- mean(plot_data$fantasy_opp_score, na.rm = TRUE)
        
        # Axis ranges with padding
        x_range <- max(plot_data$fantasy_opp_score, na.rm = TRUE) - min(plot_data$fantasy_opp_score, na.rm = TRUE)
        y_range <- max(plot_data$avg_salary, na.rm = TRUE) - min(plot_data$avg_salary, na.rm = TRUE)
        if (x_range < 0.1) x_range <- 10
        if (y_range < 0.1) y_range <- 1
        
        x_min <- min(plot_data$fantasy_opp_score, na.rm = TRUE) - (x_range * 0.15)
        x_max <- max(plot_data$fantasy_opp_score, na.rm = TRUE) + (x_range * 0.15)
        y_min <- min(plot_data$avg_salary, na.rm = TRUE) - (y_range * 0.15)
        y_max <- max(plot_data$avg_salary, na.rm = TRUE) + (y_range * 0.15)
        
        # Build interactive scatter plot (tooltips on hover)
        # Draw non-highlighted points first, highlighted on top
        p <- ggplot(plot_data, aes(x = fantasy_opp_score, y = avg_salary)) +
          # Reference lines at averages
          geom_hline(yintercept = avg_salary, linetype = "dashed", 
                     color = "#7A7A7A", linewidth = 0.6) +
          geom_vline(xintercept = avg_fos, linetype = "dashed", 
                     color = "#7A7A7A", linewidth = 0.6) +
          # Non-highlighted points (grey, no border)
          ggiraph::geom_point_interactive(
            data = filter(plot_data, !is_highlighted),
            aes(tooltip = tooltip_html, data_id = team_normalized),
            size = 14, shape = 19, color = "#8A8A8A", alpha = 0.65
          ) +
          # Team abbreviations for non-highlighted
          geom_text(
            data = filter(plot_data, !is_highlighted),
            aes(label = team_abbr),
            size = 3.5, fontface = "bold", color = "#3B3226", vjust = 0.4
          ) +
          # Highlighted point (gold fill, dark border, on top)
          ggiraph::geom_point_interactive(
            data = filter(plot_data, is_highlighted),
            aes(tooltip = tooltip_html, data_id = team_normalized),
            size = 14, shape = 21, fill = "#C9A227", color = "#3B3226", stroke = 2
          ) +
          # Team abbreviation for highlighted
          geom_text(
            data = filter(plot_data, is_highlighted),
            aes(label = team_abbr),
            size = 3.5, fontface = "bold", color = "#3B3226", vjust = 0.4
          ) +
          # Axes (no titles - external labels)
          scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0),
                             labels = function(x) sprintf("%.0f", x)) +
          scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0),
                             labels = function(y) sprintf("Ãƒâ€šÃ‚Â£%.1fM", y)) +
          labs(x = NULL, y = NULL) +
          theme_minimal(base_size = 14) +
          theme(
            text = element_text(family = "sans"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "#E5E9F0", linewidth = 0.5),
            axis.text = element_text(color = "#3B3226", size = 11),
            plot.margin = margin(15, 20, 15, 15)
          )
        
        # Return girafe object
        ggiraph::girafe(
          ggobj = p,
          width_svg = 10,
          height_svg = 7,
          options = list(
            ggiraph::opts_tooltip(
              css = "background-color:#FFFFFF;border:2px solid #3B3226;border-radius:8px;box-shadow:3px 3px 0 #3B3226;padding:0;font-family:sans-serif;",
              opacity = 1,
              use_fill = FALSE
            ),
            ggiraph::opts_hover(
              css = "fill:#C9A227;fill-opacity:0.9;cursor:pointer;"
            ),
            ggiraph::opts_selection(type = "none")
          )
        )
        
      } else {
        # =====================================================================
        # DUMBBELL PLOT - FOS change from market baseline (with logo y-axis)
        # =====================================================================
        
        # Build team-level deltas
        plot_data <- rv$projections %>%
          filter(position == pos_filter, !is.na(salary), !is.na(proj_gf), !is.na(proj_cs),
                 !is.na(base_gf), !is.na(base_cs)) %>%
          group_by(team_normalized) %>%
          arrange(desc(salary)) %>%
          mutate(pos_rank = row_number()) %>%
          filter(pos_rank <= max_for_pos) %>%
          summarise(
            avg_salary = mean(salary, na.rm = TRUE),
            adj_gf = mean(proj_gf, na.rm = TRUE),
            adj_cs = mean(proj_cs, na.rm = TRUE),
            base_gf_avg = mean(base_gf, na.rm = TRUE),
            base_cs_avg = mean(base_cs, na.rm = TRUE),
            .groups = "drop"
          )
        
        if (nrow(plot_data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = paste("No", pos_filter, "data available"),
                     size = 5, color = "#7A7A7A", fontface = "italic") +
            theme_void() +
            theme(plot.background = element_rect(fill = "transparent", color = NA))
          return(ggiraph::girafe(ggobj = p, width_svg = 8, height_svg = 6))
        }
        
        # Normalize goals and calculate FOS for both adjusted and baseline
        gf_all <- c(plot_data$adj_gf, plot_data$base_gf_avg)
        gf_min <- min(gf_all, na.rm = TRUE)
        gf_max <- max(gf_all, na.rm = TRUE)
        gf_range <- gf_max - gf_min
        if (gf_range < 0.01) gf_range <- 1
        
        # Get current FOS weights from slider
        current_weights <- fos_weights_reactive()
        
        highlighted_team <- input$highlight_team
        
        plot_data <- plot_data %>%
          mutate(
            adj_gf_norm = ((adj_gf - gf_min) / gf_range) * 100,
            base_gf_norm = ((base_gf_avg - gf_min) / gf_range) * 100,
            adj_fos = (adj_cs * current_weights["cs"]) + 
              (adj_gf_norm * current_weights["gf"]),
            base_fos = (base_cs_avg * current_weights["cs"]) + 
              (base_gf_norm * current_weights["gf"]),
            delta_fos = adj_fos - base_fos,
            is_positive = delta_fos >= 0,
            team_name_caps = toupper(team_normalized),
            logo_path = sapply(team_normalized, function(t) get_soccer_team_logo(t) %||% ""),
            is_highlighted = !is.null(highlighted_team) && highlighted_team != "" & team_normalized == highlighted_team,
            # Y-axis label with logo for ggtext
            y_label = ifelse(
              logo_path != "",
              paste0("<img src='", logo_path, "' width='16'/>&nbsp;&nbsp;**", team_name_caps, "**"),
              paste0("**", team_name_caps, "**")
            )
          ) %>%
          arrange(desc(adj_fos))
        
        # Create ordered factor for y-axis with logo labels
        plot_data <- plot_data %>%
          mutate(y_label = factor(y_label, levels = rev(y_label)))
        
        # Calculate x-axis range
        x_min <- min(c(plot_data$base_fos, plot_data$adj_fos), na.rm = TRUE)
        x_max <- max(c(plot_data$base_fos, plot_data$adj_fos), na.rm = TRUE)
        x_range <- x_max - x_min
        if (x_range < 1) x_range <- 10
        x_min <- x_min - (x_range * 0.12)
        x_max <- x_max + (x_range * 0.12)
        
        # Build dumbbell plot with conditional coloring
        p <- ggplot(plot_data) +
          # Segments - gold for positive, frost for negative (with transparency)
          geom_segment(
            data = filter(plot_data, is_positive & !is_highlighted),
            aes(x = base_fos, xend = adj_fos, y = y_label, yend = y_label),
            color = "#C9A227", alpha = 0.5, linewidth = 2.5
          ) +
          geom_segment(
            data = filter(plot_data, !is_positive & !is_highlighted),
            aes(x = base_fos, xend = adj_fos, y = y_label, yend = y_label),
            color = "#4A90A4", alpha = 0.5, linewidth = 2.5
          ) +
          # Highlighted segment (darker, thicker)
          geom_segment(
            data = filter(plot_data, is_positive & is_highlighted),
            aes(x = base_fos, xend = adj_fos, y = y_label, yend = y_label),
            color = "#C9A227", linewidth = 3.5
          ) +
          geom_segment(
            data = filter(plot_data, !is_positive & is_highlighted),
            aes(x = base_fos, xend = adj_fos, y = y_label, yend = y_label),
            color = "#4A90A4", linewidth = 3.5
          ) +
          # All baseline dots - grey
          ggiraph::geom_point_interactive(
            data = filter(plot_data, !is_highlighted),
            aes(x = base_fos, y = y_label,
                tooltip = paste0(
                  "<div style='padding:6px;font-family:sans-serif;'>",
                  "<strong>Market FOS:</strong> ", sprintf("%.1f", base_fos),
                  "</div>"
                ),
                data_id = paste0(team_normalized, "_base")),
            color = "#8A8A8A", size = 6
          ) +
          # Highlighted baseline dot - grey with border
          ggiraph::geom_point_interactive(
            data = filter(plot_data, is_highlighted),
            aes(x = base_fos, y = y_label,
                tooltip = paste0(
                  "<div style='padding:6px;font-family:sans-serif;'>",
                  "<strong>Market FOS:</strong> ", sprintf("%.1f", base_fos),
                  "</div>"
                ),
                data_id = paste0(team_normalized, "_base")),
            shape = 21, fill = "#8A8A8A", color = "#3B3226", stroke = 2, size = 7
          ) +
          # Adjusted dots - gold if positive, frost if negative, with dark border
          ggiraph::geom_point_interactive(
            data = filter(plot_data, is_positive & !is_highlighted),
            aes(x = adj_fos, y = y_label,
                tooltip = paste0(
                  "<div style='padding:6px;font-family:sans-serif;'>",
                  "<strong>Your FOS:</strong> ", sprintf("%.1f", adj_fos), "<br>",
                  "<strong>Change:</strong> ", sprintf("%+.1f", delta_fos),
                  "</div>"
                ),
                data_id = paste0(team_normalized, "_adj")),
            shape = 21, fill = "#C9A227", color = "#3B3226", stroke = 1.5, size = 6
          ) +
          ggiraph::geom_point_interactive(
            data = filter(plot_data, !is_positive & !is_highlighted),
            aes(x = adj_fos, y = y_label,
                tooltip = paste0(
                  "<div style='padding:6px;font-family:sans-serif;'>",
                  "<strong>Your FOS:</strong> ", sprintf("%.1f", adj_fos), "<br>",
                  "<strong>Change:</strong> ", sprintf("%+.1f", delta_fos),
                  "</div>"
                ),
                data_id = paste0(team_normalized, "_adj")),
            shape = 21, fill = "#4A90A4", color = "#3B3226", stroke = 1.5, size = 6
          ) +
          # Highlighted adjusted dots - larger with thicker border
          ggiraph::geom_point_interactive(
            data = filter(plot_data, is_positive & is_highlighted),
            aes(x = adj_fos, y = y_label,
                tooltip = paste0(
                  "<div style='padding:6px;font-family:sans-serif;'>",
                  "<strong>Your FOS:</strong> ", sprintf("%.1f", adj_fos), "<br>",
                  "<strong>Change:</strong> ", sprintf("%+.1f", delta_fos),
                  "</div>"
                ),
                data_id = paste0(team_normalized, "_adj")),
            shape = 21, fill = "#C9A227", color = "#3B3226", stroke = 2.5, size = 8
          ) +
          ggiraph::geom_point_interactive(
            data = filter(plot_data, !is_positive & is_highlighted),
            aes(x = adj_fos, y = y_label,
                tooltip = paste0(
                  "<div style='padding:6px;font-family:sans-serif;'>",
                  "<strong>Your FOS:</strong> ", sprintf("%.1f", adj_fos), "<br>",
                  "<strong>Change:</strong> ", sprintf("%+.1f", delta_fos),
                  "</div>"
                ),
                data_id = paste0(team_normalized, "_adj")),
            shape = 21, fill = "#4A90A4", color = "#3B3226", stroke = 2.5, size = 8
          ) +
          # X-axis at top (no title - external label)
          scale_x_continuous(
            limits = c(x_min, x_max),
            expand = c(0, 0),
            labels = function(x) sprintf("%.0f", x),
            position = "top"
          ) +
          labs(x = NULL, y = NULL) +
          theme_minimal(base_size = 14) +
          theme(
            text = element_text(family = "sans"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#E5E9F0", linewidth = 0.5),
            axis.text.x.top = element_text(color = "#3B3226", size = 11),
            axis.text.y = ggtext::element_markdown(color = "#3B3226", size = 10),
            plot.margin = margin(15, 20, 15, 80)  # Extra left margin for logos
          )
        
        # Return girafe object
        ggiraph::girafe(
          ggobj = p,
          width_svg = 10,
          height_svg = 7,
          options = list(
            ggiraph::opts_tooltip(
              css = "background-color:#FFFFFF;border:2px solid #3B3226;border-radius:6px;box-shadow:2px 2px 0 #3B3226;font-family:sans-serif;",
              opacity = 1
            ),
            ggiraph::opts_hover(
              css = "fill-opacity:0.8;cursor:pointer;"
            ),
            ggiraph::opts_selection(type = "none")
          )
        )
      }
    })
    
    # =========================================================================
    # PROJECTIONS TABLE (Team-level comparison: Market vs My Adjusted)
    # =========================================================================
    
    output$projections_table <- renderReactable({
      req(rv$matches)
      matches <- rv$matches
      
      if (nrow(matches) == 0) return(NULL)
      
      # Build team-level data from matches
      table_data <- lapply(1:nrow(matches), function(i) {
        m <- matches[i, ]
        
        # User adjusted values
        h <- input[[paste0("home_", i)]] %||% m$market_home_win
        d <- input[[paste0("draw_", i)]] %||% m$market_draw
        a <- input[[paste0("away_", i)]] %||% m$market_away_win
        t <- input[[paste0("total_", i)]] %||% m$market_total
        
        # Market baseline values
        mkt_h <- m$market_home_win
        mkt_d <- m$market_draw
        mkt_a <- m$market_away_win
        mkt_t <- m$market_total
        
        # Calculate adjusted goals
        goals_adj <- calc_goals_from_probs(h, d, a, t)
        
        # Calculate market goals
        goals_mkt <- calc_goals_from_probs(mkt_h, mkt_d, mkt_a, mkt_t)
        
        # Shots calculations
        adj_home_shots <- calc_shots(h, t, TRUE)
        adj_away_shots <- calc_shots(a, t, FALSE)
        adj_home_sot <- calc_sot(h, t, TRUE)
        adj_away_sot <- calc_sot(a, t, FALSE)
        
        mkt_home_shots <- calc_shots(mkt_h, mkt_t, TRUE)
        mkt_away_shots <- calc_shots(mkt_a, mkt_t, FALSE)
        mkt_home_sot <- calc_sot(mkt_h, mkt_t, TRUE)
        mkt_away_sot <- calc_sot(mkt_a, mkt_t, FALSE)
        
        # Clean sheet % (Poisson: CS% = exp(-goals_against))
        adj_home_cs <- exp(-goals_adj$away) * 100
        adj_away_cs <- exp(-goals_adj$home) * 100
        mkt_home_cs <- exp(-goals_mkt$away) * 100
        mkt_away_cs <- exp(-goals_mkt$home) * 100
        
        # Return both home and away team rows
        list(
          # Home team
          data.frame(
            team = m$home_team,
            logo = get_soccer_team_logo(m$home_team) %||% "",
            is_home = TRUE,
            opponent = m$away_team,
            # Market columns - store raw goals for win/loss calc
            mkt_team_goals = goals_mkt$home,
            mkt_opp_goals = goals_mkt$away,
            mkt_shots = mkt_home_shots,
            mkt_sot = mkt_home_sot,
            mkt_shots_conceded = mkt_away_shots,
            mkt_sot_conceded = mkt_away_sot,
            mkt_cs = mkt_home_cs,
            mkt_opp_cs = mkt_away_cs,
            # Adjusted columns
            adj_team_goals = goals_adj$home,
            adj_opp_goals = goals_adj$away,
            adj_shots = adj_home_shots,
            adj_sot = adj_home_sot,
            adj_shots_conceded = adj_away_shots,
            adj_sot_conceded = adj_away_sot,
            adj_cs = adj_home_cs,
            adj_opp_cs = adj_away_cs,
            stringsAsFactors = FALSE
          ),
          # Away team
          data.frame(
            team = m$away_team,
            logo = get_soccer_team_logo(m$away_team) %||% "",
            is_home = FALSE,
            opponent = m$home_team,
            # Market columns
            mkt_team_goals = goals_mkt$away,
            mkt_opp_goals = goals_mkt$home,
            mkt_shots = mkt_away_shots,
            mkt_sot = mkt_away_sot,
            mkt_shots_conceded = mkt_home_shots,
            mkt_sot_conceded = mkt_home_sot,
            mkt_cs = mkt_away_cs,
            mkt_opp_cs = mkt_home_cs,
            # Adjusted columns
            adj_team_goals = goals_adj$away,
            adj_opp_goals = goals_adj$home,
            adj_shots = adj_away_shots,
            adj_sot = adj_away_sot,
            adj_shots_conceded = adj_home_shots,
            adj_sot_conceded = adj_home_sot,
            adj_cs = adj_away_cs,
            adj_opp_cs = adj_home_cs,
            stringsAsFactors = FALSE
          )
        )
      })
      
      # Flatten the list
      table_df <- do.call(rbind, unlist(table_data, recursive = FALSE))
      
      # Sort by team name
      table_df <- table_df[order(table_df$team), ]
      
      reactable(
        table_df,
        theme = app_reactable_theme(compact = TRUE),
        defaultColDef = colDef(
          align = "center",
          minWidth = 70
        ),
        columns = list(
          team = colDef(
            name = "Team",
            minWidth = 204,
            align = "left",
            style = list(borderRight = "2px solid #E5E9F0"),
            headerStyle = list(borderRight = "2px solid #E5E9F0"),
            cell = function(value, index) {
              logo <- table_df$logo[index]
              is_home <- table_df$is_home[index]
              opponent <- table_df$opponent[index]
              opp_abbr <- toupper(get_team_abbreviation(opponent))
              prefix <- if (is_home) "vs" else "@"
              
              div(
                style = "display: flex; align-items: center; gap: 10px; padding: 4px 0;",
                if (!is.null(logo) && logo != "") {
                  tags$img(src = logo, style = "width: 29px; height: 29px; object-fit: contain;")
                },
                div(
                  style = "display: flex; flex-direction: column; line-height: 1.3;",
                  span(style = "font-weight: 600; font-size: 0.9rem;", value),
                  span(style = "font-size: 0.75rem; color: #7A7A7A;", paste(prefix, opp_abbr))
                )
              )
            }
          ),
          logo = colDef(show = FALSE),
          is_home = colDef(show = FALSE),
          opponent = colDef(show = FALSE),
          mkt_opp_goals = colDef(show = FALSE),
          adj_opp_goals = colDef(show = FALSE),
          # Hide other market columns (data still available for delta calc)
          mkt_shots = colDef(show = FALSE),
          mkt_sot = colDef(show = FALSE),
          mkt_shots_conceded = colDef(show = FALSE),
          mkt_sot_conceded = colDef(show = FALSE),
          mkt_cs = colDef(show = FALSE),
          mkt_opp_cs = colDef(show = FALSE),
          # Market result column (no superscript)
          mkt_team_goals = colDef(
            name = "Mkt Result", 
            minWidth = 80,
            align = "center",
            cell = function(value, index) {
              sprintf("%.1f - %.1f", value, table_df$mkt_opp_goals[index])
            }
          ),
          # Adjusted result column (no superscript)
          adj_team_goals = colDef(
            name = "My Result", 
            minWidth = 80,
            align = "center",
            style = list(borderRight = "2px solid #E5E9F0"),
            headerStyle = list(borderRight = "2px solid #E5E9F0"),
            cell = function(value, index) {
              sprintf("%.1f - %.1f", value, table_df$adj_opp_goals[index])
            }
          ),
          # Stat columns with delta superscripts - fixed width number, superscript after
          adj_shots = colDef(
            name = "Sh",
            minWidth = 75,
            align = "center",
            cell = function(value, index) {
              mkt <- table_df$mkt_shots[index]
              delta <- value - mkt
              if (abs(delta) < 0.05) {
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.1f", value)))
              } else {
                color <- if (delta > 0) "#A3BE8C" else "#D08770"
                sign <- if (delta > 0) "+" else ""
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.1f", value)),
                    tags$sup(style = sprintf("font-size:0.7em;font-weight:700;color:%s;", color),
                             sprintf("%s%.1f", sign, delta)))
              }
            }
          ),
          adj_sot = colDef(
            name = "SoT",
            minWidth = 75,
            align = "center",
            cell = function(value, index) {
              mkt <- table_df$mkt_sot[index]
              delta <- value - mkt
              if (abs(delta) < 0.05) {
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.1f", value)))
              } else {
                color <- if (delta > 0) "#A3BE8C" else "#D08770"
                sign <- if (delta > 0) "+" else ""
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.1f", value)),
                    tags$sup(style = sprintf("font-size:0.7em;font-weight:700;color:%s;", color),
                             sprintf("%s%.1f", sign, delta)))
              }
            }
          ),
          adj_shots_conceded = colDef(
            name = "ShC",
            minWidth = 75,
            align = "center",
            cell = function(value, index) {
              mkt <- table_df$mkt_shots_conceded[index]
              delta <- value - mkt
              if (abs(delta) < 0.05) {
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.1f", value)))
              } else {
                # For shots conceded, MORE is bad (coral), LESS is good (sage)
                color <- if (delta > 0) "#D08770" else "#A3BE8C"
                sign <- if (delta > 0) "+" else ""
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.1f", value)),
                    tags$sup(style = sprintf("font-size:0.7em;font-weight:700;color:%s;", color),
                             sprintf("%s%.1f", sign, delta)))
              }
            }
          ),
          adj_sot_conceded = colDef(
            name = "SoTC",
            minWidth = 75,
            align = "center",
            cell = function(value, index) {
              mkt <- table_df$mkt_sot_conceded[index]
              delta <- value - mkt
              if (abs(delta) < 0.05) {
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.1f", value)))
              } else {
                # For shots on target conceded, MORE is bad (coral), LESS is good (sage)
                color <- if (delta > 0) "#D08770" else "#A3BE8C"
                sign <- if (delta > 0) "+" else ""
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.1f", value)),
                    tags$sup(style = sprintf("font-size:0.7em;font-weight:700;color:%s;", color),
                             sprintf("%s%.1f", sign, delta)))
              }
            }
          ),
          adj_cs = colDef(
            name = "CS%",
            minWidth = 70,
            align = "center",
            cell = function(value, index) {
              mkt <- table_df$mkt_cs[index]
              delta <- value - mkt
              if (abs(delta) < 0.5) {
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.0f%%", value)))
              } else {
                color <- if (delta > 0) "#A3BE8C" else "#D08770"
                sign <- if (delta > 0) "+" else ""
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.0f%%", value)),
                    tags$sup(style = sprintf("font-size:0.7em;font-weight:700;color:%s;", color),
                             sprintf("%s%.0f", sign, delta)))
              }
            }
          ),
          adj_opp_cs = colDef(
            name = "OppCS%",
            minWidth = 75,
            align = "center",
            cell = function(value, index) {
              mkt <- table_df$mkt_opp_cs[index]
              delta <- value - mkt
              if (abs(delta) < 0.5) {
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.0f%%", value)))
              } else {
                # For opponent CS%, MORE is bad for us (coral), LESS is good (sage)
                color <- if (delta > 0) "#D08770" else "#A3BE8C"
                sign <- if (delta > 0) "+" else ""
                div(style = "display:inline-block;",
                    span(style = "display:inline-block;width:32px;text-align:right;", sprintf("%.0f%%", value)),
                    tags$sup(style = sprintf("font-size:0.7em;font-weight:700;color:%s;", color),
                             sprintf("%s%.0f", sign, delta)))
              }
            }
          )
        ),
        striped = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        compact = TRUE,
        pagination = FALSE,
        rownames = FALSE
      )
    })
    
    observeEvent(input$refresh_data, {
      rv$salaries <- NULL; rv$odds <- NULL; rv$matches <- NULL; rv$projections <- NULL
      shinyWidgets::updatePickerInput(session, "gameweek", selected = input$gameweek)
    })
  })
}