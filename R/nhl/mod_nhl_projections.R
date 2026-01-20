# =============================================================================
# Module: NHL Projections
# 
# File upload for FanTeam salary and projections CSV, with name reconciliation
# and projections table display with median and ceiling projections
# =============================================================================

nhl_projections_ui <- function(id) {
  ns <- NS(id)
  log_debug("nhl_projections_ui() called", level = "INFO")
  
  tagList(
    # NOTE: Styles use generic classes from styles.css
    
    # File Upload Card
    ui_card(
      title = "Upload Slate Files", 
      color = "sky",
      fluidRow(
        column(4,
               fileInput(
                 ns("fanteam_file"),
                 "FanTeam Salary File (CSV)",
                 accept = c(".csv"),
                 placeholder = "Select FanTeam export..."
               )
        ),
        column(4,
               fileInput(
                 ns("skater_projections_file"),
                 "Skater Projections (CSV)",
                 accept = c(".csv"),
                 placeholder = "Select skater projections..."
               )
        ),
        column(4,
               fileInput(
                 ns("goalie_projections_file"),
                 "Goalie Projections (CSV)",
                 accept = c(".csv"),
                 placeholder = "Select goalie projections..."
               )
        )
      ),
      fluidRow(
        column(12,
               actionButton(
                 ns("process_files"),
                 "Process Files",
                 class = "btn-primary",
                 icon = icon("sync")
               ),
               span(style = "margin-left: 16px;", textOutput(ns("file_status"), inline = TRUE))
        )
      )
    ),
    
    # Unmatched Players Alert
    uiOutput(ns("unmatched_alert")),
    
    # Filter Card
    uiOutput(ns("filter_card")),
    
    # Projections Table
    ui_card(
      title = "Player Projections",
      color = "sky",
      div(
        style = "min-height: 400px;",
        reactableOutput(ns("projections_table"))
      )
    )
  )
}

nhl_projections_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialization logging
    log_debug("========================================", level = "INFO")
    log_debug("nhl_projections_server() initialized", level = "INFO")
    log_debug("Session token:", session$token, level = "DEBUG")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      fanteam_raw = NULL,
      skater_projections_raw = NULL,
      goalie_projections_raw = NULL,
      player_data = NULL,
      unmatched_players = NULL,
      match_rate = NULL,
      available_teams = NULL,
      available_positions = NULL
    )
    
    # File status output
    output$file_status <- renderText({
      if (is.null(rv$player_data)) {
        files_loaded <- sum(!is.null(rv$fanteam_raw), 
                            !is.null(rv$skater_projections_raw), 
                            !is.null(rv$goalie_projections_raw))
        sprintf("Upload files (%d/3 loaded)", files_loaded)
      } else {
        sprintf("Loaded %d players (%.1f%% matched)", 
                nrow(rv$player_data), 
                rv$match_rate %||% 0)
      }
    })
    
    # Process FanTeam file upload
    observeEvent(input$fanteam_file, {
      req(input$fanteam_file)
      log_debug(">>> FanTeam file uploaded:", input$fanteam_file$name, level = "INFO")
      
      tryCatch({
        fanteam_data <- read_csv(
          input$fanteam_file$datapath,
          show_col_types = FALSE,
          locale = locale(encoding = "UTF-8")
        )
        
        log_debug("FanTeam columns:", paste(names(fanteam_data), collapse = ", "), level = "DEBUG")
        log_debug("FanTeam rows:", nrow(fanteam_data), level = "INFO")
        
        # Standardize column names
        fanteam_clean <- fanteam_data %>%
          rename_with(tolower) %>%
          rename_with(~gsub("\\s+", "_", .))
        
        # Map to standard columns
        if ("name" %in% names(fanteam_clean) && "fname" %in% names(fanteam_clean)) {
          fanteam_clean <- fanteam_clean %>%
            mutate(player_name = paste(fname, name, sep = " "))
        } else if ("name" %in% names(fanteam_clean)) {
          fanteam_clean <- fanteam_clean %>%
            rename(player_name = name)
        }
        
        if ("club" %in% names(fanteam_clean)) {
          fanteam_clean <- fanteam_clean %>%
            rename(team = club)
        }
        
        if ("price" %in% names(fanteam_clean)) {
          fanteam_clean <- fanteam_clean %>%
            rename(salary = price)
        }
        
        # Normalize positions
        if ("position" %in% names(fanteam_clean)) {
          fanteam_clean <- fanteam_clean %>%
            mutate(position = sapply(position, normalize_nhl_position))
        }
        
        # Normalize teams
        if ("team" %in% names(fanteam_clean)) {
          fanteam_clean <- fanteam_clean %>%
            mutate(team = sapply(team, normalize_nhl_team))
        }
        
        # Keep relevant columns
        rv$fanteam_raw <- fanteam_clean %>%
          select(
            any_of(c("playerid", "player_name", "team", "position", "salary", "lineup"))
          ) %>%
          filter(!is.na(player_name), player_name != "")
        
        log_debug("FanTeam processed:", nrow(rv$fanteam_raw), "players", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error reading FanTeam file:", e$message, level = "ERROR")
        showNotification(
          paste("Error reading FanTeam file:", e$message),
          type = "error"
        )
      })
    })
    
    # Process Skater Projections file upload
    observeEvent(input$skater_projections_file, {
      req(input$skater_projections_file)
      log_debug(">>> Skater projections file uploaded:", input$skater_projections_file$name, level = "INFO")
      
      tryCatch({
        proj_data <- read_csv(
          input$skater_projections_file$datapath,
          show_col_types = FALSE,
          locale = locale(encoding = "UTF-8"),
          skip = 1
        )
        
        log_debug("Skater projections columns (raw):", paste(names(proj_data), collapse = ", "), level = "DEBUG")
        log_debug("Skater projections rows:", nrow(proj_data), level = "INFO")
        
        col_names <- names(proj_data)
        g_cols <- grep("^G(\\.{3}\\d+)?$", col_names, value = TRUE)
        a_cols <- grep("^A(\\.{3}\\d+)?$", col_names, value = TRUE)
        
        if (length(g_cols) >= 1) names(proj_data)[names(proj_data) == g_cols[1]] <- "G"
        if (length(a_cols) >= 1) names(proj_data)[names(proj_data) == a_cols[1]] <- "A"
        if (length(g_cols) >= 2) names(proj_data)[names(proj_data) == g_cols[2]] <- "PP_G"
        if (length(a_cols) >= 2) names(proj_data)[names(proj_data) == a_cols[2]] <- "PP_A"
        if (length(g_cols) >= 3) names(proj_data)[names(proj_data) == g_cols[3]] <- "SH_G"
        if (length(a_cols) >= 3) names(proj_data)[names(proj_data) == a_cols[3]] <- "SH_A"
        
        proj_clean <- proj_data %>%
          rename_with(~gsub("\\s+", "_", .))
        
        if (names(proj_clean)[1] == "" || grepl("^X", names(proj_clean)[1])) {
          names(proj_clean)[1] <- "player_name"
        }
        
        name_map <- c("Player_Name" = "player_name", "Team" = "team", "Pos" = "position")
        for (old_name in names(name_map)) {
          if (old_name %in% names(proj_clean)) {
            proj_clean <- proj_clean %>% rename(!!name_map[old_name] := !!old_name)
          }
        }
        
        if ("position" %in% names(proj_clean)) {
          proj_clean <- proj_clean %>%
            mutate(position = sapply(position, normalize_nhl_position))
        }
        
        if ("team" %in% names(proj_clean)) {
          proj_clean <- proj_clean %>%
            mutate(team = sapply(team, normalize_nhl_team))
        }
        
        if ("+/-" %in% names(proj_clean)) {
          proj_clean <- proj_clean %>% rename(plus_minus = `+/-`)
        }
        
        all_numeric <- c("G", "A", "Pts", "plus_minus", "PIM", "SOG", "GWG", 
                         "PP_G", "PP_A", "SH_G", "SH_A", "Hits", "BS")
        for (col in all_numeric) {
          if (col %in% names(proj_clean)) {
            proj_clean <- proj_clean %>%
              mutate(!!col := as.numeric(!!sym(col)))
          }
        }
        
        rv$skater_projections_raw <- proj_clean %>%
          filter(!is.na(player_name), player_name != "")
        
        log_debug("Skater projections processed:", nrow(rv$skater_projections_raw), "players", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error reading skater projections file:", e$message, level = "ERROR")
        showNotification(paste("Error reading skater projections file:", e$message), type = "error")
      })
    })
    
    # Process Goalie Projections file upload
    observeEvent(input$goalie_projections_file, {
      req(input$goalie_projections_file)
      log_debug(">>> Goalie projections file uploaded:", input$goalie_projections_file$name, level = "INFO")
      
      tryCatch({
        proj_data <- read_csv(
          input$goalie_projections_file$datapath,
          show_col_types = FALSE,
          locale = locale(encoding = "UTF-8"),
          skip = 1
        )
        
        proj_clean <- proj_data %>%
          rename_with(~gsub("\\s+", "_", .)) %>%
          rename_with(~gsub("\\.", "", .))
        
        if (names(proj_clean)[1] == "" || grepl("^X", names(proj_clean)[1])) {
          names(proj_clean)[1] <- "player_name"
        }
        
        name_map <- c("Player_Name" = "player_name", "Team" = "team", "Pos" = "position")
        for (old_name in names(name_map)) {
          if (old_name %in% names(proj_clean)) {
            proj_clean <- proj_clean %>% rename(!!name_map[old_name] := !!old_name)
          }
        }
        
        if ("team" %in% names(proj_clean)) {
          proj_clean <- proj_clean %>%
            mutate(team = sapply(team, normalize_nhl_team))
        }
        
        proj_clean <- proj_clean %>% mutate(position = "G")
        
        goalie_numeric <- c("W", "L", "OTL", "GA", "SA", "SV", "SO")
        for (col in goalie_numeric) {
          if (col %in% names(proj_clean)) {
            proj_clean <- proj_clean %>%
              mutate(!!col := as.numeric(!!sym(col)))
          }
        }
        
        sv_pct_cols <- c("SV%", "SV_pct", "SVpct")
        for (col in sv_pct_cols) {
          if (col %in% names(proj_clean)) {
            proj_clean <- proj_clean %>%
              rename(SV_pct = !!col) %>%
              mutate(SV_pct = as.numeric(SV_pct))
            break
          }
        }
        
        rv$goalie_projections_raw <- proj_clean %>%
          filter(!is.na(player_name), player_name != "")
        
        log_debug("Goalie projections processed:", nrow(rv$goalie_projections_raw), "players", level = "INFO")
        
      }, error = function(e) {
        log_debug("Error reading goalie projections file:", e$message, level = "ERROR")
        showNotification(paste("Error reading goalie projections file:", e$message), type = "error")
      })
    })
    
    # Process files button
    observeEvent(input$process_files, {
      log_debug(">>> Process files button clicked", level = "INFO")
      
      req(rv$fanteam_raw)
      
      if (is.null(rv$skater_projections_raw) && is.null(rv$goalie_projections_raw)) {
        showNotification("Please upload at least one projections file", type = "warning")
        return()
      }
      
      tryCatch({
        fanteam_skaters <- rv$fanteam_raw %>% filter(position != "G")
        fanteam_goalies <- rv$fanteam_raw %>% filter(position == "G")
        
        all_player_data <- NULL
        all_unmatched <- NULL
        total_matched <- 0
        total_players <- nrow(rv$fanteam_raw)
        
        # Process skaters
        if (!is.null(rv$skater_projections_raw) && nrow(fanteam_skaters) > 0) {
          skater_match <- match_nhl_players(fanteam_skaters, rv$skater_projections_raw)
          
          skater_data <- skater_match$matched %>%
            rowwise() %>%
            mutate(
              fpts_median = calculate_nhl_median_fpts(list(
                G = proj_G %||% 0, A = proj_A %||% 0,
                SH_G = proj_SH_G %||% 0, SH_A = proj_SH_A %||% 0,
                SOG = proj_SOG %||% 0, BS = proj_BS %||% 0
              )),
              fpts_ceiling = calculate_nhl_ceiling_fpts(list(
                G = proj_G %||% 0, A = proj_A %||% 0,
                SH_G = proj_SH_G %||% 0, SH_A = proj_SH_A %||% 0,
                SOG = proj_SOG %||% 0, BS = proj_BS %||% 0
              ))
            ) %>%
            ungroup()
          
          all_player_data <- skater_data
          all_unmatched <- skater_match$unmatched
          total_matched <- total_matched + sum(!is.na(skater_data$proj_G) & skater_data$proj_G > 0)
        }
        
        # Process goalies
        if (!is.null(rv$goalie_projections_raw) && nrow(fanteam_goalies) > 0) {
          goalie_match <- match_nhl_goalies(fanteam_goalies, rv$goalie_projections_raw)
          
          goalie_data <- goalie_match$matched %>%
            rowwise() %>%
            mutate(
              fpts_median = calculate_nhl_goalie_median_fpts(list(
                W = proj_W %||% 0, OTL = proj_OTL %||% 0,
                SV = proj_SV %||% 0, GA = proj_GA %||% 0, SO = proj_SO %||% 0
              )),
              fpts_ceiling = calculate_nhl_goalie_ceiling_fpts(list(
                W = proj_W %||% 0, OTL = proj_OTL %||% 0,
                SV = proj_SV %||% 0, GA = proj_GA %||% 0, SO = proj_SO %||% 0
              ))
            ) %>%
            ungroup()
          
          if (is.null(all_player_data)) {
            all_player_data <- goalie_data
          } else {
            skater_cols <- setdiff(names(all_player_data), names(goalie_data))
            goalie_cols <- setdiff(names(goalie_data), names(all_player_data))
            for (col in skater_cols) goalie_data[[col]] <- NA
            for (col in goalie_cols) all_player_data[[col]] <- NA
            all_player_data <- bind_rows(all_player_data, goalie_data)
          }
          
          if (!is.null(goalie_match$unmatched) && nrow(goalie_match$unmatched) > 0) {
            if (is.null(all_unmatched)) {
              all_unmatched <- goalie_match$unmatched
            } else {
              all_unmatched <- bind_rows(all_unmatched, goalie_match$unmatched)
            }
          }
          
          total_matched <- total_matched + sum(!is.na(goalie_data$proj_W) & goalie_data$proj_W > 0)
        }
        
        # Final processing
        player_data <- all_player_data %>%
          mutate(
            value_median = ifelse(salary > 0, fpts_median / salary, 0),
            value_ceiling = ifelse(salary > 0, fpts_ceiling / salary, 0),
            fpts_blended = (fpts_median + fpts_ceiling) / 2,
            pos_display = case_when(
              position %in% c("LW", "RW", "W") ~ "W",
              TRUE ~ position
            ),
            # Get full team name for display
            team_full = NHL_TEAMS[team]
          ) %>%
          arrange(desc(fpts_blended))
        
        rv$player_data <- player_data
        rv$unmatched_players <- all_unmatched
        rv$match_rate <- (total_matched / total_players) * 100
        
        # Store in session for sharing with handbuild module
        session$userData$nhl_player_data <- player_data
        session$userData$nhl_unmatched <- all_unmatched
        session$userData$nhl_match_rate <- rv$match_rate
        
        rv$available_teams <- sort(unique(player_data$team[!is.na(player_data$team)]))
        rv$available_positions <- c("C", "W", "D", "G")
        
        showNotification(
          sprintf("Processed %d players (%.1f%% matched)", nrow(rv$player_data), rv$match_rate),
          type = "message"
        )
        
      }, error = function(e) {
        log_debug("Error processing files:", e$message, level = "ERROR")
        showNotification(paste("Error processing files:", e$message), type = "error")
      })
    })
    
    # Unmatched players alert
    output$unmatched_alert <- renderUI({
      req(rv$unmatched_players)
      if (nrow(rv$unmatched_players) == 0) return(NULL)
      
      top_unmatched <- rv$unmatched_players %>% arrange(desc(salary)) %>% head(5)
      unmatched_text <- paste(sapply(1:nrow(top_unmatched), function(i) {
        sprintf("%s ($%.1f)", top_unmatched$player_name[i], top_unmatched$salary[i])
      }), collapse = ", ")
      if (nrow(rv$unmatched_players) > 5) unmatched_text <- paste0(unmatched_text, " ...")
      
      div(
        class = "alert alert-warning",
        style = "margin-top: 16px; border-left: 4px solid #EBCB8B; display: flex; align-items: center; gap: 8px;",
        tags$strong(icon("exclamation-triangle"), sprintf(" %d unmatched players", nrow(rv$unmatched_players))),
        tags$span(style = "margin-left: 8px;", unmatched_text)
      )
    })
    
    # Filter card
    output$filter_card <- renderUI({
      req(rv$player_data)
      
      ui_card(
        title = "Filters",
        color = "sky",
        fluidRow(
          column(3,
                 selectInput(ns("filter_position"), "Position",
                             choices = c("All" = "all", "C" = "C", "W" = "W", "D" = "D", "G" = "G"),
                             selected = "all")
          ),
          column(3,
                 selectInput(ns("filter_team"), "Team",
                             choices = c("All Teams" = "all", setNames(rv$available_teams, rv$available_teams)),
                             selected = "all")
          ),
          column(3,
                 numericInput(ns("filter_min_salary"), "Min Salary", value = 0, min = 0, max = 30, step = 0.5)
          ),
          column(3,
                 numericInput(ns("filter_max_salary"), "Max Salary", value = 100, min = 0, max = 100, step = 0.5)
          )
        )
      )
    })
    
    # Filtered data
    filtered_data <- reactive({
      req(rv$player_data)
      
      data <- rv$player_data
      
      if (!is.null(input$filter_position) && input$filter_position != "all") {
        if (input$filter_position == "W") {
          data <- data %>% filter(position %in% c("LW", "RW", "W"))
        } else {
          data <- data %>% filter(position == input$filter_position)
        }
      }
      
      if (!is.null(input$filter_team) && input$filter_team != "all") {
        data <- data %>% filter(team == input$filter_team)
      }
      
      if (!is.null(input$filter_min_salary) && !is.na(input$filter_min_salary)) {
        data <- data %>% filter(salary >= input$filter_min_salary)
      }
      if (!is.null(input$filter_max_salary) && !is.na(input$filter_max_salary)) {
        data <- data %>% filter(salary <= input$filter_max_salary)
      }
      
      data
    })
    
    # Projections table - Position appended to player, logo + full team name, 60px stat columns, NO color formatting
    output$projections_table <- renderReactable({
      req(filtered_data())
      
      tryCatch({
        data <- filtered_data()
        
        # Replace NAs
        data$fpts_median[is.na(data$fpts_median)] <- 0
        data$fpts_ceiling[is.na(data$fpts_ceiling)] <- 0
        data$fpts_blended[is.na(data$fpts_blended)] <- 0
        data$value_median[is.na(data$value_median)] <- 0
        
        # Build table data - NO separate POS column
        table_data <- data.frame(
          Player = as.character(data$player_name),
          Pos = as.character(data$pos_display),
          Team = as.character(data$team),
          TeamFull = as.character(data$team_full),
          Salary = as.numeric(data$salary),
          Median = round(as.numeric(data$fpts_median), 1),
          Ceiling = round(as.numeric(data$fpts_ceiling), 1),
          Blended = round(as.numeric(data$fpts_blended), 1),
          Value = round(as.numeric(data$value_median), 2),
          stringsAsFactors = FALSE
        )
        
        reactable::reactable(
          table_data,
          striped = TRUE,
          highlight = TRUE,
          searchable = TRUE,
          defaultPageSize = 25,
          defaultSorted = list(Blended = "desc"),
          columns = list(
            # Player column with position appended - reduced width
            Player = colDef(
              name = "PLAYER",
              minWidth = 160,
              headerStyle = list(fontWeight = 700, fontSize = "0.85rem"),
              cell = function(value, index) {
                pos <- table_data$Pos[index]
                div(
                  class = "player-cell player-cell--row",
                  span(class = "player-name", value),
                  span(class = "position-badge position-badge--sm", pos)
                )
              }
            ),
            # Hide the separate Pos column
            Pos = colDef(show = FALSE),
            # Team with logo + full name - increased width (+25px)
            Team = colDef(
              name = "TEAM",
              minWidth = 165,
              headerStyle = list(fontWeight = 700, fontSize = "0.85rem"),
              cell = function(value, index) {
                if (is.na(value) || value == "") return("â€”")
                team_full <- table_data$TeamFull[index]
                if (is.na(team_full)) team_full <- value
                logo_path <- sprintf("nhl_logos/%s_light.svg", value)
                div(
                  class = "team-cell",
                  tags$img(src = logo_path, class = "team-logo team-logo--lg", 
                           onerror = "this.style.display='none'"),
                  span(class = "team-name", team_full)
                )
              }
            ),
            # Hide TeamFull (used for display only)
            TeamFull = colDef(show = FALSE),
            # Salary - reduced 5px (175 - 5 = 170)
            Salary = colDef(
              name = "SALARY",
              width = 170,
              align = "center",
              headerStyle = list(fontWeight = 700, fontSize = "0.85rem"),
              cell = function(value) {
                div(class = "stat-cell", sprintf("$%.1f", value))
              }
            ),
            # Median - reduced 5px (150 - 5 = 145)
            Median = colDef(
              name = "MEDIAN",
              width = 145,
              align = "center",
              headerStyle = list(fontWeight = 700, fontSize = "0.85rem"),
              cell = function(value) {
                if (value > 0) div(class = "stat-cell", sprintf("%.1f", value)) else "â€”"
              }
            ),
            # Ceiling - reduced 5px (150 - 5 = 145)
            Ceiling = colDef(
              name = "CEILING",
              width = 145,
              align = "center",
              headerStyle = list(fontWeight = 700, fontSize = "0.85rem"),
              cell = function(value) {
                if (value > 0) div(class = "stat-cell", sprintf("%.1f", value)) else "â€”"
              }
            ),
            # Blended - reduced 5px (150 - 5 = 145)
            Blended = colDef(
              name = "BLEND",
              width = 145,
              align = "center",
              headerStyle = list(fontWeight = 700, fontSize = "0.85rem"),
              cell = function(value) {
                if (value > 0) div(class = "stat-cell", sprintf("%.1f", value)) else "â€”"
              }
            ),
            # Value - reduced 5px (150 - 5 = 145)
            Value = colDef(
              name = "VALUE",
              width = 145,
              align = "center",
              headerStyle = list(fontWeight = 700, fontSize = "0.85rem"),
              cell = function(value) {
                if (value > 0) div(class = "stat-cell stat-cell--sm", sprintf("%.2f", value)) else "â€”"
              }
            )
          )
        )
        
      }, error = function(e) {
        log_debug(">>> ERROR in renderReactable:", e$message, level = "ERROR")
        NULL
      })
    })
    
    # Return player data for use by handbuild module
    return(reactive({ rv$player_data }))
  })
}