# =============================================================================
# Module: Section Navigation (Level 2)
# 
# Sub-navigation showing sections within a sport (Dashboard, Optimizer, etc.)
# =============================================================================

#' Section Navigation UI
#' @param id Module namespace ID
section_nav_ui <- function(id) {
  ns <- NS(id)
  
  # Container that will be populated dynamically based on selected sport
  uiOutput(ns("section_nav_container"))
}

#' Section Navigation Server
#' @param id Module namespace ID
#' @param selected_sport Reactive value containing current sport ID
#' @return Reactive value containing selected section ID
section_nav_server <- function(id, selected_sport) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track selected section
    selected_section <- reactiveVal(NULL)
    
    # Render section nav based on selected sport
    output$section_nav_container <- renderUI({
      req(selected_sport())
      
      sport_id <- selected_sport()
      sections <- get_sport_sections(sport_id)
      sport <- get_sport(sport_id)
      
      if (length(sections) == 0) return(NULL)
      
      # Build section nav items
      nav_items <- lapply(names(sections), function(section_id) {
        section <- sections[[section_id]]
        
        # Check if this is the default/active section
        is_default <- section_id == sport$default_section
        
        tags$a(
          id = ns(paste0("section_", section_id)),
          class = paste("section-nav-item", if (is_default) "active" else ""),
          href = "#",
          `data-section` = section_id,
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
            ns("selected_section"),
            section_id
          ),
          tags$span(class = "section-nav-label", section$name)
        )
      })
      
      div(
        class = "section-navbar",
        `data-sport` = sport_id,
        nav_items
      )
    })
    
    # Reset section to default when sport changes
    observeEvent(selected_sport(), {
      sport <- get_sport(selected_sport())
      if (!is.null(sport)) {
        selected_section(sport$default_section)
      }
    })
    
    # Update section on click
    observeEvent(input$selected_section, {
      selected_section(input$selected_section)
      
      # Update active class via JS
      session$sendCustomMessage("updateSectionNav", list(
        ns_prefix = ns(""),
        section = input$selected_section
      ))
    })
    
    # Return selected section as reactive
    return(selected_section)
  })
}