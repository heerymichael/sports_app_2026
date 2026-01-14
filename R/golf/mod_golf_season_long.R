# =============================================================================
# Module: Golf Season Long
# 
# Season-long roster management for Underdog Scramble contest:
# - 3 roster management slots
# - Initial roster building with salary/rankings merge
# - Weighted projections (Q1-Q4)
# - Transfer analysis and tracking
# - Captain selection and underdog identification
# 
# Styled in Stabilo illustrated aesthetic
# =============================================================================

# Null coalesce operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# Card color for golf
GOLF_CARD_COLOR <- "gold"

# =============================================================================
# UI
# =============================================================================

#' Golf Season Long UI
#' @param id Module namespace ID
golf_season_long_ui <- function(id) {
  ns <- NS(id)
  
  log_debug("golf_season_long_ui() called with id:", id, level = "INFO")
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("Season Long - Underdog Scramble"),
      tags$p(class = "text-muted", 
             "Manage your season-long golf rosters across 32 gameweeks")
    ),
    
    # Top controls row
    fluidRow(
      # Data Import Card
      column(3,
             ui_card(
               title = "Data Import",
               color = GOLF_CARD_COLOR,
               
               fileInput(ns("salary_file"), "Salary File (CSV)",
                         accept = ".csv",
                         placeholder = "week_1.csv"),
               fileInput(ns("rankings_file"), "Rankings File (CSV)",
                         accept = ".csv",
                         placeholder = "rankings.csv"),
               actionButton(ns("load_data"), "Load & Merge Data",
                            class = "btn-primary btn-block",
                            icon = icon("upload"))
             )
      ),
      
      # Roster Selection Card
      column(3,
             ui_card(
               title = "Roster Selection",
               color = GOLF_CARD_COLOR,
               
               selectInput(ns("active_roster"), "Active Roster",
                           choices = c("Roster 1" = "1", 
                                       "Roster 2" = "2", 
                                       "Roster 3" = "3"),
                           selected = "1"),
               tags$hr(),
               div(
                 style = "display: flex; gap: 0.5rem;",
                 actionButton(ns("save_roster"), "Save", 
                              class = "btn-outline-primary", 
                              style = "flex: 1;"),
                 actionButton(ns("clear_roster"), "Clear", 
                              class = "btn-outline-danger", 
                              style = "flex: 1;")
               )
             )
      ),
      
      # Projection Settings Card
      column(3,
             ui_card(
               title = "Projection Weights",
               color = GOLF_CARD_COLOR,
               
               selectInput(ns("weight_profile"), "Weight Profile",
                           choices = c("Early Season (Q1/Q2 Heavy)" = "early_season",
                                       "Mid Season (Balanced)" = "mid_season",
                                       "Late Season (Q3/Q4 Heavy)" = "late_season",
                                       "Equal Weights" = "equal"),
                           selected = "early_season"),
               uiOutput(ns("weight_display"))
             )
      ),
      
      # Budget Summary Card
      column(3,
             ui_card(
               title = "Budget Status",
               color = GOLF_CARD_COLOR,
               
               div(class = "budget-summary",
                   div(class = "budget-row",
                       span(class = "budget-label", "Budget:"),
                       span(class = "budget-value", "100.0M")
                   ),
                   div(class = "budget-row",
                       span(class = "budget-label", "Spent:"),
                       span(class = "budget-value", textOutput(ns("spent_budget"), inline = TRUE))
                   ),
                   div(class = "budget-row budget-remaining",
                       span(class = "budget-label", "Remaining:"),
                       span(class = "budget-value", textOutput(ns("remaining_budget"), inline = TRUE))
                   ),
                   div(class = "budget-row",
                       span(class = "budget-label", "Players:"),
                       span(class = "budget-value", textOutput(ns("roster_count"), inline = TRUE))
                   )
               )
             )
      )
    ),
    
    # Main content: Player Pool and Roster
    fluidRow(
      # Player Pool (Left side - wider)
      column(7,
             ui_card(
               title = "Player Pool",
               color = GOLF_CARD_COLOR,
               
               # Filters row
               fluidRow(
                 column(4,
                        textInput(ns("search_player"), "Search Player", 
                                  placeholder = "Enter name...")
                 ),
                 column(3,
                        selectInput(ns("sort_by"), "Sort By",
                                    choices = c("Weighted Projection" = "weighted_proj",
                                                "Total Projection" = "Total Projected Points",
                                                "Price" = "Price",
                                                "Value (Pts/M)" = "value",
                                                "Rank" = "Rank",
                                                "ADP" = "ADP"),
                                    selected = "weighted_proj")
                 ),
                 column(2,
                        selectInput(ns("sort_order"), "Order",
                                    choices = c("Descending" = "desc", "Ascending" = "asc"),
                                    selected = "desc")
                 ),
                 column(3,
                        sliderInput(ns("price_range"), "Price Range",
                                    min = 7.5, max = 16, value = c(7.5, 16), step = 0.5)
                 )
               ),
               
               # Player table
               div(style = "margin-top: 1rem;",
                   reactableOutput(ns("player_pool_table"), height = "500px")
               )
             )
      ),
      
      # Roster Builder (Right side)
      column(5,
             ui_card(
               title = "Current Roster",
               color = GOLF_CARD_COLOR,
               
               # Roster summary header
               div(
                 class = "roster-header",
                 style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 1rem;",
                 div(
                   tags$strong("Projected Points: "),
                   textOutput(ns("total_projection"), inline = TRUE)
                 ),
                 div(
                   actionButton(ns("optimize_roster"), "Optimize", 
                                class = "btn-sm btn-outline-primary",
                                icon = icon("magic"))
                 )
               ),
               
               # Active lineup section
               tags$h5("Active Lineup", style = "margin-top: 1rem; color: var(--text-primary);"),
               reactableOutput(ns("active_lineup_table"), height = "200px"),
               
               # Bench section
               tags$h5("Bench (4 players)", style = "margin-top: 1rem; color: var(--text-secondary);"),
               reactableOutput(ns("bench_table"), height = "140px"),
               
               # Captain & Underdog info
               div(
                 class = "roster-multipliers",
                 style = "margin-top: 1rem; padding: 0.75rem; background: var(--bg-secondary); border-radius: 8px;",
                 fluidRow(
                   column(6,
                          div(
                            tags$strong("Captain (1.25x): "),
                            textOutput(ns("captain_name"), inline = TRUE)
                          )
                   ),
                   column(6,
                          div(
                            tags$strong("Underdog (1.25x): "),
                            textOutput(ns("underdog_name"), inline = TRUE)
                          )
                   )
                 )
               )
             )
      )
    ),
    
    # Transfer Analysis Section
    fluidRow(
      column(12,
             ui_card(
               title = "Transfer Analysis",
               color = GOLF_CARD_COLOR,
               collapsed = TRUE,
               
               fluidRow(
                 column(4,
                        selectInput(ns("transfer_out"), "Transfer Out",
                                    choices = NULL),
                        tags$p(class = "text-muted", "Select a player from your roster")
                 ),
                 column(4,
                        selectInput(ns("transfer_in"), "Transfer In",
                                    choices = NULL),
                        tags$p(class = "text-muted", "Select a replacement from the pool")
                 ),
                 column(4,
                        numericInput(ns("weeks_remaining"), "Weeks Remaining",
                                     value = 32, min = 1, max = 32, step = 1),
                        numericInput(ns("free_transfers"), "Free Transfers Available",
                                     value = 1, min = 0, max = 32, step = 1)
                 )
               ),
               
               # Transfer analysis output
               div(
                 style = "margin-top: 1rem;",
                 uiOutput(ns("transfer_analysis_output"))
               )
             )
      )
    ),
    
    # Unmatched players (for debugging)
    fluidRow(
      column(12,
             ui_card(
               title = "Unmatched Players (Debug)",
               color = GOLF_CARD_COLOR,
               collapsed = TRUE,
               
               reactableOutput(ns("unmatched_table"))
             )
      )
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

#' Golf Season Long Server
#' @param id Module namespace ID
golf_season_long_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # Initialization
    # =========================================================================
    
    log_debug("========================================", level = "INFO")
    log_debug("golf_season_long_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Reactive values
    rv <- reactiveValues(
      # Raw data
      salary_data = NULL,
      rankings_data = NULL,
      merged_data = NULL,
      
      # Rosters (list of 3)
      rosters = list(
        `1` = data.frame(),
        `2` = data.frame(),
        `3` = data.frame()
      ),
      
      # Current roster state
      current_roster = data.frame(),
      captain_id = NULL,
      
      # Transfer tracking
      transfers_used = list(`1` = 0, `2` = 0, `3` = 0),
      
      # Status
      data_loaded = FALSE
    )
    
    # =========================================================================
    # Data Loading
    # =========================================================================
    
    # Load and merge data when button clicked
    observeEvent(input$load_data, {
      log_debug(">>> Load data button clicked", level = "INFO")
      
      req(input$salary_file, input$rankings_file)
      
      tryCatch({
        # Read salary file
        salary_path <- input$salary_file$datapath
        salary_df <- read_csv(salary_path, show_col_types = FALSE)
        log_debug("Salary data loaded:", nrow(salary_df), "rows", level = "INFO")
        
        # Read rankings file
        rankings_path <- input$rankings_file$datapath
        rankings_df <- read_csv(rankings_path, show_col_types = FALSE)
        log_debug("Rankings data loaded:", nrow(rankings_df), "rows", level = "INFO")
        
        # Store raw data
        rv$salary_data <- salary_df
        rv$rankings_data <- rankings_df
        
        # Merge the data
        merged <- merge_salary_rankings(salary_df, rankings_df)
        
        if (!is.null(merged) && nrow(merged) > 0) {
          rv$merged_data <- merged
          rv$data_loaded <- TRUE
          log_debug("Data merged successfully:", nrow(merged), "players", level = "INFO")
          
          showNotification(
            sprintf("Loaded %d players (%d matched with rankings)", 
                    nrow(merged), sum(!is.na(merged$Rank))),
            type = "message",
            duration = 3
          )
        }
        
      }, error = function(e) {
        log_debug("Error loading data:", e$message, level = "ERROR")
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # =========================================================================
    # Data Processing Functions
    # =========================================================================
    
    #' Merge salary and rankings data
    merge_salary_rankings <- function(salary_df, rankings_df) {
      
      # Name corrections mapping
      name_corrections <- list(
        "alexander noren" = "alex noren",
        "christopher gotterup" = "chris gotterup",
        "nicolas echavarria" = "nico echavarria",
        "sam stevens" = "samuel stevens",
        "henry lebioda" = "hank lebioda",
        "kristoffer ventura" = "kris ventura",
        "matthias schmid" = "matti schmid",
        "zach bauchou" = "zachary bauchou",
        "byeong hun an" = "byeong hun an",
        "hao tong li" = "haotong li",
        "sung jae im" = "sungjae im",
        "ze cheng dou" = "zecheng dou",
        "seong hyeon kim" = "seonghyeon kim",
        "jordan l smith" = "jordan smith",
        "adrien dumont" = "adrien dumont de chassart"
      )
      
      # Normalize function with corrections
      normalize_and_correct <- function(name) {
        if (is.na(name) || name == "") return("")
        
        name <- tolower(trimws(name))
        
        # Handle "Last, First" format
        if (grepl(",", name)) {
          parts <- strsplit(name, ",")[[1]]
          if (length(parts) == 2) {
            name <- paste(trimws(parts[2]), trimws(parts[1]))
          }
        }
        
        # Remove punctuation including hyphens
        name <- gsub("[^a-z0-9 ]", " ", name)
        name <- gsub("\\s+", " ", name)
        name <- trimws(name)
        
        # Apply corrections
        if (name %in% names(name_corrections)) {
          name <- name_corrections[[name]]
        }
        
        name
      }
      
      # Clean salary data
      salary_df <- salary_df %>%
        mutate(
          full_name = paste(FName, Name),
          full_name = trimws(full_name),
          name_normalized = sapply(full_name, normalize_and_correct),
          Price = as.numeric(Price)
        )
      
      # Clean rankings data  
      rankings_df <- rankings_df %>%
        mutate(
          name_normalized = sapply(Golfer, normalize_and_correct),
          `Round 1 Projection` = as.numeric(`Round 1 Projection`),
          `Round 2 Projection` = as.numeric(`Round 2 Projection`),
          `Round 3 Projection` = as.numeric(`Round 3 Projection`),
          `Round 4 Projection` = as.numeric(`Round 4 Projection`),
          `Total Projected Points` = as.numeric(`Total Projected Points`),
          Rank = as.numeric(Rank),
          ADP = as.numeric(ADP)
        )
      
      # Join on normalized name
      merged <- salary_df %>%
        left_join(
          rankings_df %>% 
            select(name_normalized, Golfer, ADP, Rank, 
                   `Total Events Played`,
                   `Round 1 Projection`, `Round 2 Projection`,
                   `Round 3 Projection`, `Round 4 Projection`,
                   `Total Projected Points`),
          by = "name_normalized"
        )
      
      # Log match rate
      matched_count <- sum(!is.na(merged$Rank))
      log_debug(sprintf("Matched %d/%d players (%.1f%%)", 
                        matched_count, nrow(merged),
                        matched_count/nrow(merged)*100), level = "INFO")
      
      merged
    }
    
    # =========================================================================
    # Reactive Calculations
    # =========================================================================
    
    # Calculate weighted projections based on selected profile
    player_pool <- reactive({
      req(rv$merged_data)
      
      weights <- get_quarter_weights(input$weight_profile %||% "early_season")
      
      df <- rv$merged_data %>%
        mutate(
          # Calculate weighted projection
          weighted_proj = calculate_weighted_projection(
            `Round 1 Projection`,
            `Round 2 Projection`,
            `Round 3 Projection`,
            `Round 4 Projection`,
            weights
          ),
          # Calculate value
          value = round(weighted_proj / Price, 2),
          # Rank by weighted projection for those with data
          weighted_rank = dense_rank(desc(weighted_proj))
        ) %>%
        # Remove players already in roster
        filter(!PlayerID %in% rv$current_roster$PlayerID)
      
      # Apply filters
      if (!is.null(input$search_player) && input$search_player != "") {
        search_term <- tolower(input$search_player)
        df <- df %>%
          filter(grepl(search_term, tolower(full_name)))
      }
      
      if (!is.null(input$price_range)) {
        df <- df %>%
          filter(Price >= input$price_range[1], Price <= input$price_range[2])
      }
      
      # Apply sorting
      sort_col <- input$sort_by %||% "weighted_proj"
      sort_desc <- (input$sort_order %||% "desc") == "desc"
      
      if (sort_col %in% names(df)) {
        if (sort_desc) {
          df <- df %>% arrange(desc(.data[[sort_col]]))
        } else {
          df <- df %>% arrange(.data[[sort_col]])
        }
      }
      
      df
    })
    
    # Current roster with calculations
    current_roster_calc <- reactive({
      roster <- rv$current_roster
      
      if (is.null(roster) || nrow(roster) == 0) {
        return(data.frame())
      }
      
      weights <- get_quarter_weights(input$weight_profile %||% "early_season")
      
      roster %>%
        mutate(
          weighted_proj = calculate_weighted_projection(
            `Round 1 Projection`,
            `Round 2 Projection`,
            `Round 3 Projection`,
            `Round 4 Projection`,
            weights
          ),
          is_captain = PlayerID == rv$captain_id,
          is_underdog = PlayerID == identify_underdog(roster),
          # Apply multipliers
          adjusted_proj = case_when(
            is_captain | is_underdog ~ weighted_proj * 1.25,
            TRUE ~ weighted_proj
          )
        )
    })
    
    # =========================================================================
    # Weight Display
    # =========================================================================
    
    output$weight_display <- renderUI({
      weights <- get_quarter_weights(input$weight_profile %||% "early_season")
      
      div(
        class = "weight-display",
        style = "font-size: 0.85rem; margin-top: 0.5rem;",
        div(sprintf("Q1: %.0f%% | Q2: %.0f%% | Q3: %.0f%% | Q4: %.0f%%",
                    weights["Q1"]*100, weights["Q2"]*100, 
                    weights["Q3"]*100, weights["Q4"]*100))
      )
    })
    
    # =========================================================================
    # Budget Outputs
    # =========================================================================
    
    output$spent_budget <- renderText({
      roster <- current_roster_calc()
      if (nrow(roster) == 0) return("0.0M")
      sprintf("%.1fM", sum(roster$Price, na.rm = TRUE))
    })
    
    output$remaining_budget <- renderText({
      roster <- current_roster_calc()
      spent <- if (nrow(roster) == 0) 0 else sum(roster$Price, na.rm = TRUE)
      remaining <- GOLF_SCRAMBLE_CONFIG$budget - spent
      sprintf("%.1fM", remaining)
    })
    
    output$roster_count <- renderText({
      roster <- current_roster_calc()
      sprintf("%d / %d", nrow(roster), GOLF_SCRAMBLE_CONFIG$roster_size)
    })
    
    output$total_projection <- renderText({
      roster <- current_roster_calc()
      if (nrow(roster) == 0) return("0.0")
      sprintf("%.1f", sum(roster$adjusted_proj, na.rm = TRUE))
    })
    
    # =========================================================================
    # Captain & Underdog
    # =========================================================================
    
    output$captain_name <- renderText({
      roster <- current_roster_calc()
      captain <- roster %>% filter(is_captain)
      if (nrow(captain) == 0) return("Not selected")
      captain$full_name[1]
    })
    
    output$underdog_name <- renderText({
      roster <- current_roster_calc()
      if (nrow(roster) == 0) return("N/A")
      
      cheapest <- roster %>% 
        arrange(Price) %>% 
        slice(1)
      
      sprintf("%s (%.1fM)", cheapest$full_name[1], cheapest$Price[1])
    })
    
    # =========================================================================
    # Player Pool Table
    # =========================================================================
    
    output$player_pool_table <- renderReactable({
      pool <- player_pool()
      req(pool, nrow(pool) > 0)
      
      # Load headshots
      headshots <- load_golf_headshots()
      
      # Select display columns
      display_df <- pool %>%
        mutate(
          # Create normalized name for headshot lookup
          name_lookup = sapply(full_name, function(n) {
            n <- tolower(trimws(n))
            n <- gsub("[^a-z0-9 ]", " ", n)
            gsub("\\s+", " ", trimws(n))
          })
        ) %>%
        left_join(
          headshots %>% select(name_normalized, headshot_url),
          by = c("name_lookup" = "name_normalized")
        ) %>%
        select(
          PlayerID,
          headshot_url,
          Player = full_name,
          Price,
          Proj = weighted_proj,
          Value = value,
          Rank,
          Q1 = `Round 1 Projection`,
          Q2 = `Round 2 Projection`,
          Q3 = `Round 3 Projection`,
          Q4 = `Round 4 Projection`,
          Total = `Total Projected Points`
        )
      
      reactable(
        display_df,
        theme = app_reactable_theme(),
        searchable = FALSE,
        pagination = TRUE,
        defaultPageSize = 15,
        selection = "single",
        onClick = "select",
        columns = list(
          PlayerID = colDef(show = FALSE),
          headshot_url = colDef(show = FALSE),
          Player = colDef(
            name = "Player",
            minWidth = 180,
            sticky = "left",
            html = TRUE,
            cell = function(value, index) {
              headshot <- display_df$headshot_url[index]
              
              if (!is.na(headshot) && headshot != "") {
                img_html <- sprintf(
                  '<img src="%s" style="width: 28px; height: 28px; border-radius: 50%%; object-fit: cover; margin-right: 8px; border: 1px solid var(--border-color);" onerror="this.style.display=\'none\'">',
                  headshot
                )
              } else {
                # Placeholder with initials
                initials <- toupper(substr(gsub("^(.).*\\s+(.).*$", "\\1\\2", value), 1, 2))
                img_html <- sprintf(
                  '<div style="width: 28px; height: 28px; border-radius: 50%%; background: var(--bg-secondary); display: inline-flex; align-items: center; justify-content: center; margin-right: 8px; font-size: 0.7rem; font-weight: 600; color: var(--text-secondary); flex-shrink: 0;">%s</div>',
                  initials
                )
              }
              
              sprintf(
                '<div style="display: flex; align-items: center;">%s<span style="font-weight: 500;">%s</span></div>',
                img_html,
                htmltools::htmlEscape(value)
              )
            }
          ),
          Price = colDef(
            name = "Price",
            format = colFormat(suffix = "M", digits = 1),
            width = 70,
            align = "right"
          ),
          Proj = colDef(
            name = "Proj",
            format = colFormat(digits = 1),
            width = 70,
            align = "right",
            style = function(value) {
              if (is.na(value)) return(list())
              color <- if (value > 300) APP_COLORS$teal else if (value > 200) APP_COLORS$sage else NULL
              list(fontWeight = if (!is.null(color)) "600" else "400",
                   color = color)
            }
          ),
          Value = colDef(
            name = "Val",
            format = colFormat(digits = 1),
            width = 60,
            align = "right"
          ),
          Rank = colDef(
            name = "Rank",
            width = 60,
            align = "center",
            na = "-"
          ),
          Q1 = colDef(name = "Q1", width = 60, format = colFormat(digits = 0), na = "-"),
          Q2 = colDef(name = "Q2", width = 60, format = colFormat(digits = 0), na = "-"),
          Q3 = colDef(name = "Q3", width = 60, format = colFormat(digits = 0), na = "-"),
          Q4 = colDef(name = "Q4", width = 60, format = colFormat(digits = 0), na = "-"),
          Total = colDef(name = "Tot", width = 70, format = colFormat(digits = 0), na = "-")
        )
      )
    })
    
    # Add selected player to roster
    observeEvent(input$player_pool_table__selected, {
      selected_idx <- input$player_pool_table__selected
      req(selected_idx)
      
      pool <- player_pool()
      selected_player <- pool[selected_idx, ]
      
      # Check if roster is full
      if (nrow(rv$current_roster) >= GOLF_SCRAMBLE_CONFIG$roster_size) {
        showNotification("Roster is full (10 players)", type = "warning")
        return()
      }
      
      # Check budget
      current_spent <- sum(rv$current_roster$Price, na.rm = TRUE)
      if (current_spent + selected_player$Price > GOLF_SCRAMBLE_CONFIG$budget) {
        showNotification(
          sprintf("Cannot afford %s (%.1fM). Budget remaining: %.1fM",
                  selected_player$full_name,
                  selected_player$Price,
                  GOLF_SCRAMBLE_CONFIG$budget - current_spent),
          type = "warning"
        )
        return()
      }
      
      # Add to roster
      rv$current_roster <- bind_rows(rv$current_roster, selected_player)
      log_debug("Added to roster:", selected_player$full_name, level = "INFO")
      
      showNotification(
        sprintf("Added %s (%.1fM)", selected_player$full_name, selected_player$Price),
        type = "message",
        duration = 2
      )
    })
    
    # =========================================================================
    # Roster Tables
    # =========================================================================
    
    # Active lineup table (first 6 players by projection)
    output$active_lineup_table <- renderReactable({
      roster <- current_roster_calc()
      if (nrow(roster) == 0) {
        return(reactable(
          data.frame(Message = "Add players from the pool"),
          theme = app_reactable_theme()
        ))
      }
      
      # Load headshots
      headshots <- load_golf_headshots()
      
      # Get top 6 by adjusted projection for active lineup
      active <- roster %>%
        arrange(desc(adjusted_proj)) %>%
        head(6) %>%
        mutate(
          slot = row_number(),
          name_lookup = sapply(full_name, function(n) {
            n <- tolower(trimws(n))
            n <- gsub("[^a-z0-9 ]", " ", n)
            gsub("\\s+", " ", trimws(n))
          })
        ) %>%
        left_join(
          headshots %>% select(name_normalized, headshot_url),
          by = c("name_lookup" = "name_normalized")
        ) %>%
        select(
          slot,
          headshot_url,
          Player = full_name,
          Price,
          Proj = adjusted_proj,
          is_captain,
          is_underdog,
          PlayerID
        )
      
      reactable(
        active,
        theme = app_reactable_theme(),
        compact = TRUE,
        selection = "single",
        onClick = "select",
        columns = list(
          slot = colDef(name = "#", width = 35, align = "center"),
          headshot_url = colDef(show = FALSE),
          Player = colDef(
            name = "Player",
            minWidth = 160,
            html = TRUE,
            cell = function(value, index) {
              row <- active[index, ]
              headshot <- row$headshot_url
              
              # Build headshot/placeholder
              if (!is.na(headshot) && headshot != "") {
                img_html <- sprintf(
                  '<img src="%s" style="width: 24px; height: 24px; border-radius: 50%%; object-fit: cover; margin-right: 6px; border: 1px solid var(--border-color);" onerror="this.style.display=\'none\'">',
                  headshot
                )
              } else {
                initials <- toupper(substr(gsub("^(.).*\\s+(.).*$", "\\1\\2", value), 1, 2))
                img_html <- sprintf(
                  '<div style="width: 24px; height: 24px; border-radius: 50%%; background: var(--bg-secondary); display: inline-flex; align-items: center; justify-content: center; margin-right: 6px; font-size: 0.65rem; font-weight: 600; color: var(--text-secondary); flex-shrink: 0;">%s</div>',
                  initials
                )
              }
              
              # Build badges
              badges <- c()
              if (row$is_captain) badges <- c(badges, '<span style="background: var(--accent-gold); color: white; padding: 1px 4px; border-radius: 3px; font-size: 0.65rem; font-weight: 600; margin-left: 4px;">C</span>')
              if (row$is_underdog) badges <- c(badges, '<span style="background: var(--accent-coral); color: white; padding: 1px 4px; border-radius: 3px; font-size: 0.65rem; font-weight: 600; margin-left: 4px;">U</span>')
              badges_html <- paste(badges, collapse = "")
              
              sprintf(
                '<div style="display: flex; align-items: center;">%s<span style="font-weight: 500;">%s</span>%s</div>',
                img_html,
                htmltools::htmlEscape(value),
                badges_html
              )
            }
          ),
          Price = colDef(
            name = "Price",
            format = colFormat(suffix = "M", digits = 1),
            width = 65,
            align = "right"
          ),
          Proj = colDef(
            name = "Proj",
            format = colFormat(digits = 1),
            width = 60,
            align = "right"
          ),
          is_captain = colDef(show = FALSE),
          is_underdog = colDef(show = FALSE),
          PlayerID = colDef(show = FALSE)
        )
      )
    })
    
    # Bench table (remaining 4 players)
    output$bench_table <- renderReactable({
      roster <- current_roster_calc()
      if (nrow(roster) <= 6) {
        return(reactable(
          data.frame(Message = "Need 10 players total, 4 on bench"),
          theme = app_reactable_theme()
        ))
      }
      
      # Load headshots
      headshots <- load_golf_headshots()
      
      # Get bottom 4 by adjusted projection for bench
      bench <- roster %>%
        arrange(desc(adjusted_proj)) %>%
        tail(4) %>%
        mutate(
          name_lookup = sapply(full_name, function(n) {
            n <- tolower(trimws(n))
            n <- gsub("[^a-z0-9 ]", " ", n)
            gsub("\\s+", " ", trimws(n))
          })
        ) %>%
        left_join(
          headshots %>% select(name_normalized, headshot_url),
          by = c("name_lookup" = "name_normalized")
        ) %>%
        select(
          headshot_url,
          Player = full_name,
          Price,
          Proj = adjusted_proj,
          is_underdog,
          PlayerID
        )
      
      reactable(
        bench,
        theme = app_reactable_theme(),
        compact = TRUE,
        selection = "single",
        onClick = "select",
        columns = list(
          headshot_url = colDef(show = FALSE),
          Player = colDef(
            name = "Player",
            minWidth = 160,
            html = TRUE,
            cell = function(value, index) {
              row <- bench[index, ]
              headshot <- row$headshot_url
              
              # Build headshot/placeholder
              if (!is.na(headshot) && headshot != "") {
                img_html <- sprintf(
                  '<img src="%s" style="width: 24px; height: 24px; border-radius: 50%%; object-fit: cover; margin-right: 6px; border: 1px solid var(--border-color);" onerror="this.style.display=\'none\'">',
                  headshot
                )
              } else {
                initials <- toupper(substr(gsub("^(.).*\\s+(.).*$", "\\1\\2", value), 1, 2))
                img_html <- sprintf(
                  '<div style="width: 24px; height: 24px; border-radius: 50%%; background: var(--bg-secondary); display: inline-flex; align-items: center; justify-content: center; margin-right: 6px; font-size: 0.65rem; font-weight: 600; color: var(--text-secondary); flex-shrink: 0;">%s</div>',
                  initials
                )
              }
              
              # Build badge for underdog
              badge_html <- ""
              if (row$is_underdog) {
                badge_html <- '<span style="background: var(--accent-coral); color: white; padding: 1px 4px; border-radius: 3px; font-size: 0.65rem; font-weight: 600; margin-left: 4px;">U</span>'
              }
              
              sprintf(
                '<div style="display: flex; align-items: center;">%s<span style="font-weight: 500;">%s</span>%s</div>',
                img_html,
                htmltools::htmlEscape(value),
                badge_html
              )
            }
          ),
          Price = colDef(
            name = "Price",
            format = colFormat(suffix = "M", digits = 1),
            width = 65,
            align = "right"
          ),
          Proj = colDef(
            name = "Proj",
            format = colFormat(digits = 1),
            width = 60,
            align = "right"
          ),
          is_underdog = colDef(show = FALSE),
          PlayerID = colDef(show = FALSE)
        )
      )
    })
    
    # =========================================================================
    # Roster Actions
    # =========================================================================
    
    # Remove player from active lineup
    observeEvent(input$active_lineup_table__selected, {
      selected_idx <- input$active_lineup_table__selected
      req(selected_idx)
      
      roster <- current_roster_calc()
      active <- roster %>%
        arrange(desc(adjusted_proj)) %>%
        head(6)
      
      player_to_remove <- active[selected_idx, ]
      
      # Remove from roster
      rv$current_roster <- rv$current_roster %>%
        filter(PlayerID != player_to_remove$PlayerID)
      
      # Clear captain if removed
      if (!is.null(rv$captain_id) && rv$captain_id == player_to_remove$PlayerID) {
        rv$captain_id <- NULL
      }
      
      showNotification(
        sprintf("Removed %s", player_to_remove$full_name),
        type = "message",
        duration = 2
      )
    })
    
    # Remove player from bench
    observeEvent(input$bench_table__selected, {
      selected_idx <- input$bench_table__selected
      req(selected_idx)
      
      roster <- current_roster_calc()
      bench <- roster %>%
        arrange(desc(adjusted_proj)) %>%
        tail(4)
      
      player_to_remove <- bench[selected_idx, ]
      
      # Remove from roster
      rv$current_roster <- rv$current_roster %>%
        filter(PlayerID != player_to_remove$PlayerID)
      
      showNotification(
        sprintf("Removed %s", player_to_remove$full_name),
        type = "message",
        duration = 2
      )
    })
    
    # Set captain (double-click or separate action)
    observeEvent(input$set_captain, {
      # This would be triggered by a UI action to set captain
      # For now, captain can be set by clicking a dedicated button
    })
    
    # Clear roster
    observeEvent(input$clear_roster, {
      rv$current_roster <- data.frame()
      rv$captain_id <- NULL
      showNotification("Roster cleared", type = "message")
    })
    
    # Save roster to slot
    observeEvent(input$save_roster, {
      slot <- input$active_roster
      rv$rosters[[slot]] <- rv$current_roster
      showNotification(sprintf("Roster %s saved (%d players)", 
                               slot, nrow(rv$current_roster)), 
                       type = "message")
    })
    
    # Load roster when slot changes
    observeEvent(input$active_roster, {
      slot <- input$active_roster
      saved_roster <- rv$rosters[[slot]]
      
      if (!is.null(saved_roster) && nrow(saved_roster) > 0) {
        rv$current_roster <- saved_roster
        log_debug("Loaded roster", slot, "with", nrow(saved_roster), "players", level = "INFO")
      } else {
        rv$current_roster <- data.frame()
      }
    })
    
    # =========================================================================
    # LP Optimization
    # =========================================================================
    
    observeEvent(input$optimize_roster, {
      req(rv$merged_data)
      
      pool <- player_pool()
      if (is.null(pool) || nrow(pool) == 0) {
        showNotification("No player data available", type = "warning")
        return()
      }
      
      # Calculate weighted projections for all players
      weights <- get_quarter_weights(input$weight_profile %||% "early_season")
      
      all_players <- rv$merged_data %>%
        mutate(
          weighted_proj = calculate_weighted_projection(
            `Round 1 Projection`,
            `Round 2 Projection`,
            `Round 3 Projection`,
            `Round 4 Projection`,
            weights
          )
        ) %>%
        filter(!is.na(weighted_proj), weighted_proj > 0) %>%
        filter(!is.na(Price), Price > 0)
      
      n <- nrow(all_players)
      config <- GOLF_SCRAMBLE_CONFIG
      
      # Objective: maximize weighted projection
      obj <- all_players$weighted_proj
      
      # Constraints
      # 1. Salary cap
      constraints <- list()
      constraints[[1]] <- all_players$Price
      directions <- c("<=")
      rhs <- c(config$budget)
      
      # 2. Exactly roster_size players
      constraints[[2]] <- rep(1, n)
      directions <- c(directions, "==")
      rhs <- c(rhs, config$roster_size)
      
      const_mat <- do.call(rbind, constraints)
      
      # Solve
      result <- tryCatch({
        lpSolve::lp(
          direction = "max",
          objective.in = obj,
          const.mat = const_mat,
          const.dir = directions,
          const.rhs = rhs,
          all.bin = TRUE
        )
      }, error = function(e) {
        log_debug("LP optimization error:", e$message, level = "ERROR")
        NULL
      })
      
      if (!is.null(result) && result$status == 0) {
        selected_indices <- which(result$solution == 1)
        optimal_roster <- all_players[selected_indices, ]
        
        rv$current_roster <- optimal_roster
        
        # Set captain as highest projected player
        top_player <- optimal_roster %>%
          arrange(desc(weighted_proj)) %>%
          slice(1)
        rv$captain_id <- top_player$PlayerID
        
        showNotification(
          sprintf("Optimized! Total: %.1f pts, Salary: %.1fM",
                  sum(optimal_roster$weighted_proj),
                  sum(optimal_roster$Price)),
          type = "message"
        )
      } else {
        showNotification("Optimization failed - check constraints", type = "error")
      }
    })
    
    # =========================================================================
    # Transfer Analysis
    # =========================================================================
    
    # Update transfer dropdowns
    observe({
      roster <- current_roster_calc()
      pool <- player_pool()
      
      if (nrow(roster) > 0) {
        roster_choices <- setNames(roster$PlayerID, roster$full_name)
        updateSelectInput(session, "transfer_out", choices = roster_choices)
      }
      
      if (!is.null(pool) && nrow(pool) > 0) {
        # Limit to top 50 by projection for performance
        top_pool <- pool %>%
          head(50)
        pool_choices <- setNames(top_pool$PlayerID, 
                                 sprintf("%s (%.1fM)", top_pool$full_name, top_pool$Price))
        updateSelectInput(session, "transfer_in", choices = pool_choices)
      }
    })
    
    output$transfer_analysis_output <- renderUI({
      req(input$transfer_out, input$transfer_in)
      
      roster <- current_roster_calc()
      pool <- player_pool()
      
      player_out <- roster %>% filter(PlayerID == input$transfer_out)
      player_in <- pool %>% filter(PlayerID == input$transfer_in)
      
      req(nrow(player_out) == 1, nrow(player_in) == 1)
      
      weeks <- input$weeks_remaining %||% 32
      free_transfers <- input$free_transfers %||% 1
      
      # Calculate projections
      proj_diff <- player_in$weighted_proj - player_out$weighted_proj
      weekly_gain <- proj_diff / weeks
      salary_diff <- player_out$Price - player_in$Price
      
      # Determine transfer cost
      transfer_cost <- if (free_transfers > 0) 0 else GOLF_SCRAMBLE_CONFIG$transfer_penalty
      net_gain <- proj_diff + transfer_cost
      
      recommendation <- if (net_gain > 20) {
        list(text = "Recommended", class = "text-success")
      } else if (net_gain > 0) {
        list(text = "Marginal - Consider banking", class = "text-warning")
      } else {
        list(text = "Not recommended", class = "text-danger")
      }
      
      div(
        class = "transfer-analysis",
        style = "padding: 1rem; background: var(--bg-secondary); border-radius: 8px;",
        
        fluidRow(
          column(4,
                 div(
                   tags$strong("Projection Change"),
                   tags$p(
                     sprintf("%+.1f pts", proj_diff),
                     style = sprintf("font-size: 1.5rem; color: %s;",
                                     if (proj_diff > 0) "var(--accent-sage)" else "var(--accent-coral)")
                   ),
                   tags$small(sprintf("%.2f pts/week over %d weeks", weekly_gain, weeks))
                 )
          ),
          column(4,
                 div(
                   tags$strong("Salary Impact"),
                   tags$p(
                     sprintf("%+.1fM", salary_diff),
                     style = "font-size: 1.5rem;"
                   ),
                   tags$small(if (salary_diff > 0) "Freed up budget" else "Additional spend")
                 )
          ),
          column(4,
                 div(
                   tags$strong("Net Gain"),
                   tags$p(
                     sprintf("%+.1f pts", net_gain),
                     style = sprintf("font-size: 1.5rem; color: %s;",
                                     if (net_gain > 0) "var(--accent-sage)" else "var(--accent-coral)")
                   ),
                   tags$p(
                     class = recommendation$class,
                     style = "font-weight: 600;",
                     recommendation$text
                   ),
                   if (transfer_cost < 0) {
                     tags$small(sprintf("Includes %d point penalty (no free transfers)", 
                                        abs(transfer_cost)))
                   }
                 )
          )
        )
      )
    })
    
    # =========================================================================
    # Unmatched Players Debug Table
    # =========================================================================
    
    output$unmatched_table <- renderReactable({
      req(rv$merged_data)
      
      unmatched <- rv$merged_data %>%
        filter(is.na(Rank)) %>%
        select(
          Player = full_name,
          Price,
          name_normalized
        ) %>%
        head(50)
      
      if (nrow(unmatched) == 0) {
        return(reactable(
          data.frame(Message = "All players matched!"),
          theme = app_reactable_theme()
        ))
      }
      
      reactable(
        unmatched,
        theme = app_reactable_theme(),
        compact = TRUE,
        columns = list(
          Player = colDef(name = "Player", minWidth = 150),
          Price = colDef(name = "Price", format = colFormat(suffix = "M", digits = 1)),
          name_normalized = colDef(name = "Normalized Name")
        )
      )
    })
    
  })
}