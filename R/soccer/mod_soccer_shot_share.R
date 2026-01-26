# =============================================================================
# Module: Soccer Shot Share
# 
# Analyze shot and xG share between players when on the field together.
# Features:
#   - League logos in dropdown
#   - Minutes together calculation
#   - Baseline vs Together comparison with delta columns
#
# Dependencies: app_themes.R, soccer_config.R, soccer_shot_share.R, helpers.R
# =============================================================================

# =============================================================================
# UI
# =============================================================================

#' Soccer Shot Share UI
#' @param id Module namespace ID
soccer_shot_share_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for this module
    tags$style(HTML(sprintf("
      .shot-share-filters {
        display: grid;
        grid-template-columns: 1fr 1fr 1fr 1fr 1fr;
        gap: 15px;
        align-items: end;
      }
      .shot-share-filters .form-group {
        margin-bottom: 0;
      }
      .shot-share-filters label {
        font-size: 0.85rem;
        font-weight: 600;
        color: var(--text-secondary);
        margin-bottom: 5px;
      }
      .shot-share-action {
        display: flex;
        align-items: center;
        gap: 15px;
        margin-top: 15px;
      }
      .league-option {
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .league-option img {
        width: 20px;
        height: 20px;
        object-fit: contain;
      }
      .stat-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 15px;
      }
      .stat-box {
        text-align: center;
        padding: 20px 15px;
        background: var(--bg-tertiary);
        border-radius: 8px;
      }
      .stat-box .stat-label {
        font-size: 0.85rem;
        color: var(--text-muted);
        margin-bottom: 8px;
      }
      .stat-box .stat-value {
        font-size: 2rem;
        font-weight: 700;
        line-height: 1.2;
      }
      .stat-box .stat-subtext {
        font-size: 0.85rem;
        color: var(--text-muted);
        margin-top: 5px;
      }
      @media (max-width: 1200px) {
        .shot-share-filters {
          grid-template-columns: 1fr 1fr;
        }
        .stat-grid {
          grid-template-columns: repeat(2, 1fr);
        }
      }
    ", APP_COLORS$sage))),
    
    # Page header
    div(
      class = "page-header",
      tags$h2("Shot Share Analysis"),
      tags$p(class = "text-muted", 
             "Analyze how shots and xG are distributed between players when they're on the field together")
    ),
    
    # Filters card
    ui_card(
      title = "Player Selection",
      color = "sage",
      
      # Filter inputs in grid
      div(
        class = "shot-share-filters",
        div(
          pickerInput(
            ns("league"),
            "League",
            choices = c("Loading..." = ""),
            selected = NULL,
            options = list(
              `live-search` = FALSE,
              size = 10
            )
          )
        ),
        div(
          selectInput(
            ns("team"),
            "Team",
            choices = c("Select league first" = ""),
            selected = NULL
          )
        ),
        div(
          selectInput(
            ns("player1"),
            "Player 1",
            choices = c("Select team first" = ""),
            selected = NULL
          )
        ),
        div(
          selectInput(
            ns("player2"),
            "Player 2 (optional)",
            choices = c("None" = ""),
            selected = ""
          )
        ),
        div(
          selectInput(
            ns("player3"),
            "Player 3 (optional)",
            choices = c("None" = ""),
            selected = ""
          )
        )
      ),
      
      # Action button and status
      div(
        class = "shot-share-action",
        actionButton(
          ns("analyze"),
          "Analyze Shot Share",
          icon = icon("chart-pie"),
          class = "btn-primary"
        ),
        uiOutput(ns("status_message"))
      )
    ),
    
    # Results section
    uiOutput(ns("results_section"))
  )
}

# =============================================================================
# SERVER
# =============================================================================

#' Soccer Shot Share Server
#' @param id Module namespace ID
soccer_shot_share_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("soccer_shot_share_server() initialized", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      shots = NULL,
      pms = NULL,
      enriched_pms = NULL,
      enriched_shots = NULL,
      analysis_results = NULL,
      minutes_stats = NULL,
      data_loaded = FALSE,
      data_loading = FALSE,
      error_message = NULL
    )
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    observe({
      req(!rv$data_loaded, !rv$data_loading)
      
      log_debug(">>> Starting understat data load...", level = "INFO")
      rv$data_loading <- TRUE
      
      tryCatch({
        # Load understat shots
        rv$shots <- load_shots_for_share()
        
        if (is.null(rv$shots) || nrow(rv$shots) == 0) {
          rv$error_message <- "Failed to load shots data"
          rv$data_loading <- FALSE
          return()
        }
        log_debug(sprintf(">>> Loaded %d shots", nrow(rv$shots)), level = "INFO")
        
        # Load understat PMS
        rv$pms <- load_pms_for_share()
        
        if (is.null(rv$pms) || nrow(rv$pms) == 0) {
          rv$error_message <- "Failed to load player match stats"
          rv$data_loading <- FALSE
          return()
        }
        log_debug(sprintf(">>> Loaded %d PMS records", nrow(rv$pms)), level = "INFO")
        
        # Enrich PMS
        rv$enriched_pms <- enrich_player_match_stats(rv$pms, rv$shots)
        
        # Get leagues with display names
        leagues <- get_shot_share_leagues(rv$shots)
        log_debug(sprintf(">>> Available leagues: %s", paste(leagues, collapse = ", ")), level = "INFO")
        
        # Create choices with logo content
        league_choices <- as.character(leagues)
        
        # Build content with logos
        league_content <- sapply(league_choices, function(lg) {
          logo_path <- get_understat_league_logo(lg)
          if (!is.null(logo_path) && logo_path != "") {
            sprintf('<div class="league-option"><img src="%s" alt="%s"> %s</div>', logo_path, lg, lg)
          } else {
            lg
          }
        })
        
        # Update league dropdown with logos
        updatePickerInput(
          session, "league",
          choices = league_choices,
          selected = NULL,
          choicesOpt = list(
            content = league_content
          )
        )
        
        rv$data_loaded <- TRUE
        rv$data_loading <- FALSE
        rv$error_message <- NULL
        
        log_debug(">>> Data load complete", level = "INFO")
        
      }, error = function(e) {
        log_debug(sprintf(">>> ERROR: %s", e$message), level = "ERROR")
        rv$error_message <- paste("Error loading data:", e$message)
        rv$data_loading <- FALSE
      })
    })
    
    # =========================================================================
    # CASCADING DROPDOWNS
    # =========================================================================
    
    observeEvent(input$league, {
      league <- input$league
      req(league, league != "", rv$shots)
      
      log_debug(sprintf(">>> League changed to: %s", league), level = "DEBUG")
      
      # Pass display name - get_shot_share_teams converts internally
      teams <- get_shot_share_teams(rv$shots, league)
      log_debug(sprintf(">>> Teams found: %d", length(teams)), level = "DEBUG")
      
      updateSelectInput(session, "team", choices = c("Select team" = "", teams), selected = "")
      updateSelectInput(session, "player1", choices = c("Select team first" = ""), selected = "")
      updateSelectInput(session, "player2", choices = c("None" = ""), selected = "")
      updateSelectInput(session, "player3", choices = c("None" = ""), selected = "")
      
      rv$analysis_results <- NULL
      rv$minutes_stats <- NULL
    })
    
    observeEvent(input$team, {
      team <- input$team
      league <- input$league
      req(team, team != "", league, league != "", rv$shots)
      
      log_debug(sprintf(">>> Team changed to: %s", team), level = "DEBUG")
      
      # Use get_shot_share_players with league filter
      players <- get_shot_share_players(rv$shots, league, team)
      
      updateSelectInput(session, "player1", 
                        choices = c("Select player" = "", setNames(players, players)), 
                        selected = "")
      updateSelectInput(session, "player2", 
                        choices = c("None" = "", setNames(players, players)), 
                        selected = "")
      updateSelectInput(session, "player3", 
                        choices = c("None" = "", setNames(players, players)), 
                        selected = "")
      
      rv$analysis_results <- NULL
      rv$minutes_stats <- NULL
      rv$enriched_shots <- NULL
    })
    
    # =========================================================================
    # ANALYSIS
    # =========================================================================
    
    observeEvent(input$analyze, {
      log_debug(">>> Analyze clicked", level = "INFO")
      
      req(input$team, input$team != "")
      req(input$player1, input$player1 != "")
      req(rv$enriched_pms)
      
      players <- c(input$player1)
      if (!is.null(input$player2) && input$player2 != "") {
        players <- c(players, input$player2)
      }
      if (!is.null(input$player3) && input$player3 != "") {
        players <- c(players, input$player3)
      }
      
      log_debug(sprintf(">>> Players: %s", paste(players, collapse = ", ")), level = "INFO")
      
      # Calculate minutes
      tryCatch({
        rv$minutes_stats <- calculate_minutes_together(
          enriched_pms = rv$enriched_pms,
          players = players,
          team = input$team
        )
        log_debug(sprintf(">>> Minutes together: %.0f", rv$minutes_stats$minutes_together), level = "INFO")
      }, error = function(e) {
        log_debug(sprintf(">>> Minutes error: %s", e$message), level = "WARN")
        rv$minutes_stats <- NULL
      })
      
      # Enrich shots if needed
      if (is.null(rv$enriched_shots)) {
        log_debug(">>> Enriching shots...", level = "INFO")
        
        team_shots <- rv$shots %>%
          filter(h_team == input$team | a_team == input$team) %>%
          mutate(player_team = ifelse(h_a == "h", h_team, a_team)) %>%
          filter(player_team == input$team)
        
        rv$enriched_shots <- enrich_shots_with_teammates(team_shots, rv$enriched_pms)
      }
      
      # Calculate share
      tryCatch({
        # Get raw league for filtering
        raw_league <- get_understat_league_raw(input$league)
        
        results <- calculate_player_share(
          shots = rv$enriched_shots,
          players = players,
          team = input$team,
          league = raw_league,
          enriched_pms = rv$enriched_pms
        )
        
        if (is.null(results)) {
          rv$error_message <- "No shots found with all selected players on field together"
          rv$analysis_results <- NULL
        } else {
          rv$analysis_results <- results
          rv$error_message <- NULL
        }
        
      }, error = function(e) {
        log_debug(sprintf(">>> Analysis error: %s", e$message), level = "ERROR")
        rv$error_message <- paste("Analysis error:", e$message)
        rv$analysis_results <- NULL
      })
    })
    
    # Clear on team change
    observeEvent(input$team, {
      rv$enriched_shots <- NULL
      rv$analysis_results <- NULL
      rv$minutes_stats <- NULL
    })
    
    # =========================================================================
    # STATUS MESSAGE
    # =========================================================================
    
    output$status_message <- renderUI({
      if (rv$data_loading) {
        div(style = "color: var(--text-muted); font-style: italic;",
            icon("spinner", class = "fa-spin"), " Loading data...")
      } else if (!is.null(rv$error_message)) {
        div(style = sprintf("color: %s; font-weight: 600;", APP_COLORS$coral),
            icon("exclamation-triangle"), " ", rv$error_message)
      } else if (rv$data_loaded) {
        div(style = sprintf("color: %s;", APP_COLORS$sage),
            icon("check-circle"), " Data loaded")
      }
    })
    
    # =========================================================================
    # RESULTS SECTION
    # =========================================================================
    
    output$results_section <- renderUI({
      req(rv$analysis_results)
      
      results <- rv$analysis_results
      minutes <- rv$minutes_stats
      has_baseline <- "baseline_shot_share" %in% names(results)
      
      tagList(
        # Summary stats
        ui_card(
          title = "Partnership Summary",
          color = "sage",
          
          div(
            class = "stat-grid",
            # Minutes together
            div(class = "stat-box",
                div(class = "stat-label", "Minutes Together"),
                div(class = "stat-value", style = sprintf("color: %s;", APP_COLORS$primary),
                    if (!is.null(minutes)) sprintf("%.0f", minutes$minutes_together) else "—"),
                div(class = "stat-subtext",
                    if (!is.null(minutes)) sprintf("%.1f%% of team minutes", minutes$pct_together) else "")
            ),
            # Matches together
            div(class = "stat-box",
                div(class = "stat-label", "Matches Together"),
                div(class = "stat-value", style = sprintf("color: %s;", APP_COLORS$frost),
                    results$matches_together[1]),
                div(class = "stat-subtext",
                    if (!is.null(minutes) && minutes$matches_together > 0) 
                      sprintf("Avg %.0f min/match", minutes$avg_minutes_per_match) else "")
            ),
            # Team shots
            div(class = "stat-box",
                div(class = "stat-label", "Team Shots (Together)"),
                div(class = "stat-value", style = sprintf("color: %s;", APP_COLORS$sage),
                    results$team_shots[1]),
                div(class = "stat-subtext", sprintf("%.1f xG", results$team_xg[1]))
            ),
            # Goals
            div(class = "stat-box",
                div(class = "stat-label", "Goals (Selected)"),
                div(class = "stat-value", style = sprintf("color: %s;", APP_COLORS$coral),
                    sum(results$goals)),
                div(class = "stat-subtext",
                    sprintf("%d shots, %.1f xG", sum(results$shots), sum(results$xG)))
            )
          )
        ),
        
        # Breakdown table
        ui_card(
          title = "Player Breakdown: Baseline vs Together",
          color = "sage",
          
          if (has_baseline) {
            div(
              style = "margin-bottom: 15px; padding: 12px; background: var(--bg-secondary); border-radius: 6px; font-size: 0.9rem;",
              tags$strong("How to read this table:"),
              tags$ul(
                style = "margin: 8px 0 0 0; padding-left: 20px;",
                tags$li(tags$strong("Baseline"), " = Player's overall share across ALL team matches"),
                tags$li(tags$strong("Together"), " = Player's share ONLY when selected players are all on field"),
                tags$li(tags$strong("Δ"), " = Difference (",
                        tags$span(style = sprintf("color: %s;", APP_COLORS$sage), "+green"),
                        " = higher together, ",
                        tags$span(style = sprintf("color: %s;", APP_COLORS$coral), "-red"),
                        " = lower)")
              )
            )
          },
          
          reactableOutput(ns("results_table"))
        ),
        
        # Charts
        ui_card(
          title = "Share Visualization (When Playing Together)",
          color = "sage",
          
          fluidRow(
            column(6,
                   div(style = "text-align: center; margin-bottom: 10px; font-weight: 600;", "Shot Share"),
                   plotOutput(ns("shot_share_plot"), height = "280px")
            ),
            column(6,
                   div(style = "text-align: center; margin-bottom: 10px; font-weight: 600;", "xG Share"),
                   plotOutput(ns("xg_share_plot"), height = "280px")
            )
          )
        )
      )
    })
    
    # =========================================================================
    # RESULTS TABLE
    # =========================================================================
    
    output$results_table <- renderReactable({
      req(rv$analysis_results)
      
      results <- rv$analysis_results
      has_baseline <- "baseline_shot_share" %in% names(results)
      
      if (has_baseline) {
        display_data <- results %>%
          select(player, shots, xG, goals,
                 baseline_shot_share, shot_share_team, shot_share_diff,
                 baseline_xg_share, xg_share_team, xg_share_diff) %>%
          mutate(xG = round(xG, 2))
        
        col_defs <- list(
          player = colDef(name = "Player", minWidth = 150, style = list(fontWeight = 600)),
          shots = colDef(name = "Shots", minWidth = 60, align = "center"),
          xG = colDef(name = "xG", minWidth = 60, align = "center", format = colFormat(digits = 2)),
          goals = colDef(name = "Goals", minWidth = 60, align = "center"),
          baseline_shot_share = colDef(
            name = "Shot % (Baseline)", minWidth = 110, align = "center",
            format = colFormat(percent = TRUE, digits = 1),
            style = list(color = APP_COLORS$muted)
          ),
          shot_share_team = colDef(
            name = "Shot % (Together)", minWidth = 115, align = "center",
            format = colFormat(percent = TRUE, digits = 1),
            style = function(value) {
              get_sequential_heatmap_style(value, 0, max(results$shot_share_team))
            }
          ),
          shot_share_diff = colDef(
            name = "Δ Shot %", minWidth = 80, align = "center",
            cell = function(value) {
              if (is.na(value)) return("")
              color <- if (value > 0.005) APP_COLORS$sage 
              else if (value < -0.005) APP_COLORS$coral 
              else APP_COLORS$muted
              div(style = sprintf("color: %s; font-weight: 600;", color),
                  sprintf("%s%.1f%%", if (value > 0) "+" else "", value * 100))
            }
          ),
          baseline_xg_share = colDef(
            name = "xG % (Baseline)", minWidth = 105, align = "center",
            format = colFormat(percent = TRUE, digits = 1),
            style = list(color = APP_COLORS$muted)
          ),
          xg_share_team = colDef(
            name = "xG % (Together)", minWidth = 110, align = "center",
            format = colFormat(percent = TRUE, digits = 1),
            style = function(value) {
              get_sequential_heatmap_style(value, 0, max(results$xg_share_team))
            }
          ),
          xg_share_diff = colDef(
            name = "Δ xG %", minWidth = 75, align = "center",
            cell = function(value) {
              if (is.na(value)) return("")
              color <- if (value > 0.005) APP_COLORS$sage 
              else if (value < -0.005) APP_COLORS$coral 
              else APP_COLORS$muted
              div(style = sprintf("color: %s; font-weight: 600;", color),
                  sprintf("%s%.1f%%", if (value > 0) "+" else "", value * 100))
            }
          )
        )
      } else {
        display_data <- results %>%
          select(player, shots, xG, goals, shot_share_team, xg_share_team) %>%
          mutate(xG = round(xG, 2))
        
        col_defs <- list(
          player = colDef(name = "Player", minWidth = 180, style = list(fontWeight = 600)),
          shots = colDef(name = "Shots", minWidth = 70, align = "center"),
          xG = colDef(name = "xG", minWidth = 70, align = "center", format = colFormat(digits = 2)),
          goals = colDef(name = "Goals", minWidth = 70, align = "center"),
          shot_share_team = colDef(
            name = "Shot % (Team)", minWidth = 120, align = "center",
            format = colFormat(percent = TRUE, digits = 1)
          ),
          xg_share_team = colDef(
            name = "xG % (Team)", minWidth = 120, align = "center",
            format = colFormat(percent = TRUE, digits = 1)
          )
        )
      }
      
      reactable(
        display_data,
        theme = app_reactable_theme(),
        columns = col_defs,
        striped = TRUE,
        compact = TRUE,
        pagination = FALSE
      )
    })
    
    # =========================================================================
    # CHARTS
    # =========================================================================
    
    output$shot_share_plot <- renderPlot({
      req(rv$analysis_results)
      results <- rv$analysis_results
      
      plot_data <- results %>%
        select(player, shot_share_team) %>%
        mutate(share_pct = shot_share_team * 100)
      
      other_share <- (1 - sum(results$shot_share_team)) * 100
      if (other_share > 0) {
        plot_data <- rbind(plot_data, 
                           data.frame(player = "Other", shot_share_team = 1 - sum(results$shot_share_team), share_pct = other_share))
      }
      
      n_players <- nrow(results)
      colors <- c(APP_COLORS$sage, APP_COLORS$coral, APP_COLORS$frost)[1:n_players]
      if (other_share > 0) colors <- c(colors, "#CCCCCC")
      
      plot_data$player <- factor(plot_data$player, levels = rev(plot_data$player))
      
      ggplot(plot_data, aes(x = "", y = share_pct, fill = player)) +
        geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = rev(colors)) +
        labs(fill = NULL) +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 11),
          plot.background = element_rect(fill = "transparent", color = NA)
        ) +
        geom_text(aes(label = sprintf("%.0f%%", share_pct)),
                  position = position_stack(vjust = 0.5),
                  color = "white", fontface = "bold", size = 4)
    }, bg = "transparent")
    
    output$xg_share_plot <- renderPlot({
      req(rv$analysis_results)
      results <- rv$analysis_results
      
      plot_data <- results %>%
        select(player, xg_share_team) %>%
        mutate(share_pct = xg_share_team * 100)
      
      other_share <- (1 - sum(results$xg_share_team)) * 100
      if (other_share > 0) {
        plot_data <- rbind(plot_data,
                           data.frame(player = "Other", xg_share_team = 1 - sum(results$xg_share_team), share_pct = other_share))
      }
      
      n_players <- nrow(results)
      colors <- c(APP_COLORS$sage, APP_COLORS$coral, APP_COLORS$frost)[1:n_players]
      if (other_share > 0) colors <- c(colors, "#CCCCCC")
      
      plot_data$player <- factor(plot_data$player, levels = rev(plot_data$player))
      
      ggplot(plot_data, aes(x = "", y = share_pct, fill = player)) +
        geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = rev(colors)) +
        labs(fill = NULL) +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 11),
          plot.background = element_rect(fill = "transparent", color = NA)
        ) +
        geom_text(aes(label = sprintf("%.0f%%", share_pct)),
                  position = position_stack(vjust = 0.5),
                  color = "white", fontface = "bold", size = 4)
    }, bg = "transparent")
    
  })
}