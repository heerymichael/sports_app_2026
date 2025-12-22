# =============================================================================
# Module: Combined Navigation
# 
# Carousel-style sport navigation with centered sections
# =============================================================================

combined_nav_ui <- function(id) {
  ns <- NS(id)
  
  sports <- get_sports_config()
  
  # Build sport nav items (Tier 1) - will be arranged in carousel
  sport_items <- lapply(names(sports), function(sport_id) {
    sport <- sports[[sport_id]]
    
    tags$a(
      id = ns(paste0("nav_", sport_id)),
      class = paste0("sport-nav-item sport-nav-item--", sport_id),
      href = "#",
      `data-sport` = sport_id,
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
        ns("selected_sport"),
        sport_id
      ),
      tags$img(
        class = "sport-nav-icon",
        src = paste0("images/", sport$icon),
        alt = sport$name
      ),
      tags$span(class = "sport-nav-label", sport$name)
    )
  })
  
  # Navigation tray containing both tiers
  div(
    class = "nav-tray",
    
    # Tier 1: Sport navigation - carousel style with arrows
    div(
      class = "nav-tier-1",
      # Left arrow
      tags$button(
        class = "nav-arrow nav-arrow-left",
        type = "button",
        `aria-label` = "Previous sport",
        HTML("&#8249;")  # ‹ left chevron
      ),
      div(
        class = "nav-sports-carousel",
        div(
          class = "nav-sports-track",
          sport_items
        )
      ),
      # Right arrow
      tags$button(
        class = "nav-arrow nav-arrow-right",
        type = "button",
        `aria-label` = "Next sport",
        HTML("&#8250;")  # › right chevron
      )
    ),
    
    # Tier 2: Section navigation (dynamic based on sport) - centered
    div(
      class = "nav-tier-2",
      uiOutput(ns("section_nav"))
    )
  )
}

combined_nav_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("combined_nav_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    sports <- get_sports_config()
    first_sport <- names(sports)[1]
    first_section <- get_sport(first_sport)$default_section
    
    # Track selected sport and section using reactiveValues
    rv <- reactiveValues(
      sport = first_sport,
      section = first_section
    )
    
    # =========================================================================
    # SPORT SELECTION
    # =========================================================================
    
    observeEvent(input$selected_sport, {
      log_debug(">>> Sport selected:", input$selected_sport, level = "INFO")
      
      rv$sport <- input$selected_sport
      
      # Reset section to default for new sport
      sport_config <- get_sport(input$selected_sport)
      if (!is.null(sport_config)) {
        rv$section <- sport_config$default_section
      }
      
      # Update active class and carousel position via JS
      session$sendCustomMessage("updateSportNav", list(
        ns_prefix = ns(""),
        sport = input$selected_sport
      ))
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # SECTION NAVIGATION (Tier 2)
    # =========================================================================
    
    output$section_nav <- renderUI({
      sport_id <- rv$sport
      
      if (is.null(sport_id)) return(NULL)
      
      sections <- get_sport_sections(sport_id)
      
      log_debug(">>> Rendering section nav for:", sport_id, level = "DEBUG")
      
      if (length(sections) == 0) return(NULL)
      
      current_section <- rv$section
      
      # Build section nav items
      nav_items <- lapply(names(sections), function(section_id) {
        section <- sections[[section_id]]
        is_active <- identical(section_id, current_section)
        
        tags$a(
          id = ns(paste0("section_", section_id)),
          class = paste("section-nav-item", if (is_active) "active" else ""),
          href = "#",
          `data-section` = section_id,
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
            ns("selected_section"),
            section_id
          ),
          section$name
        )
      })
      
      div(
        class = "section-nav-container",
        `data-sport` = sport_id,
        nav_items
      )
    })
    
    # =========================================================================
    # SECTION SELECTION
    # =========================================================================
    
    observeEvent(input$selected_section, {
      log_debug(">>> Section selected:", input$selected_section, level = "INFO")
      rv$section <- input$selected_section
      
      # Update active class via JS
      session$sendCustomMessage("updateSectionNav", list(
        ns_prefix = ns(""),
        section = input$selected_section
      ))
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # INITIAL STATE
    # =========================================================================
    
    # Set initial active state for sports on first flush
    session$onFlushed(function() {
      session$sendCustomMessage("updateSportNav", list(
        ns_prefix = ns(""),
        sport = isolate(rv$sport)
      ))
    }, once = TRUE)
    
    # =========================================================================
    # RETURN VALUES
    # =========================================================================
    
    list(
      selected_sport = reactive({ rv$sport }),
      selected_section = reactive({ rv$section })
    )
  })
}