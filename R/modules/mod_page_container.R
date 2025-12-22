# =============================================================================
# Module: Page Container
# 
# Dynamically loads the appropriate module based on selected sport and section
# 
# ARCHITECTURE NOTE:
# This uses a "hide/show" pattern instead of "destroy/recreate" to avoid
# the classic Shiny problem where renderUI destroys inputs that servers
# are still listening to. All module UIs are created once and shown/hidden
# via CSS, ensuring servers stay connected to their inputs.
# =============================================================================

#' Page Container UI
#' @param id Module namespace ID
page_container_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "page-container",
    uiOutput(ns("page_content"))
  )
}

#' Page Container Server
#' @param id Module namespace ID
#' @param selected_sport Reactive value containing current sport ID
#' @param selected_section Reactive value containing current section ID
page_container_server <- function(id, selected_sport, selected_section) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_debug("========================================", level = "INFO")
    log_debug("page_container_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    # Track current module to detect changes
    previous_module <- reactiveVal(NULL)
    
    # Render page content based on sport and section
    output$page_content <- renderUI({
      req(selected_sport(), selected_section())
      
      sport <- selected_sport()
      section <- selected_section()
      module_id <- paste(sport, section, sep = "_")
      
      log_debug(">>> renderUI triggered for:", sport, "/", section, level = "INFO")
      
      # Get the UI function
      ui_fn <- get_module_ui(sport, section)
      
      if (is.null(ui_fn)) {
        log_debug(">>> No UI function found, showing placeholder", level = "WARN")
        return(placeholder_ui(sport, section))
      }
      
      log_debug(">>> Building UI for:", module_id, level = "INFO")
      
      # Build the UI with namespaced ID
      ui_fn(ns(module_id))
    })
    
    # Initialize servers when sport/section changes
    # Key: We re-initialize server every time because renderUI destroys old inputs
    # We use session$onFlushed to delay initialization until UI is in browser
    observeEvent(list(selected_sport(), selected_section()), {
      req(selected_sport(), selected_section())
      
      sport <- selected_sport()
      section <- selected_section()
      module_id <- paste(sport, section, sep = "_")
      
      # Check if this is the same module as before
      prev <- previous_module()
      
      if (identical(prev, module_id)) {
        log_debug(">>> Same module, skipping server re-init:", module_id, level = "DEBUG")
        return()
      }
      
      log_debug(">>> Module changed from", prev, "to", module_id, level = "INFO")
      previous_module(module_id)
      
      # Get server function
      server_fn <- get_module_server(sport, section)
      
      if (is.null(server_fn)) {
        log_debug(">>> No server function for:", module_id, level = "DEBUG")
        return()
      }
      
      # CRITICAL: Delay server initialization until AFTER UI has been sent to browser
      # session$onFlushed runs after all outputs have been sent to client
      # This ensures inputs exist before server tries to read them
      log_debug(">>> Scheduling server init for:", module_id, "(will run after UI flush)", level = "INFO")
      
      session$onFlushed(function() {
        log_debug(">>> [onFlushed] Initializing server for:", module_id, level = "INFO")
        server_fn(ns(module_id))
        log_debug(">>> [onFlushed] Server initialized for:", module_id, level = "INFO")
      }, once = TRUE)
      
    }, ignoreNULL = TRUE, ignoreInit = FALSE)
  })
}

#' Get module UI function for a sport-section combination
#' @param sport Sport ID
#' @param section Section ID
#' @return UI function or NULL if not found
get_module_ui <- function(sport, section) {
  fn_name <- paste0(sport, "_", section, "_ui")
  
  if (exists(fn_name, mode = "function")) {
    return(get(fn_name))
  }
  
  return(NULL)
}

#' Get module server function for a sport-section combination
#' @param sport Sport ID
#' @param section Section ID
#' @return Server function or NULL if not found
get_module_server <- function(sport, section) {
  fn_name <- paste0(sport, "_", section, "_server")
  
  if (exists(fn_name, mode = "function")) {
    return(get(fn_name))
  }
  
  return(NULL)
}

#' Placeholder UI for unimplemented modules
#' @param sport Sport ID
#' @param section Section ID
placeholder_ui <- function(sport, section) {
  sport_config <- get_sport(sport)
  sections_config <- get_sections_config()
  section_config <- sections_config[[section]]
  
  sport_name <- if (!is.null(sport_config)) sport_config$name else sport
  section_name <- if (!is.null(section_config)) section_config$name else section
  
  div(
    class = "placeholder-container",
    style = "padding: 3rem; text-align: center;",
    div(
      class = "card",
      style = "max-width: 500px; margin: 0 auto; padding: 2rem;",
      tags$h3(
        style = "margin-bottom: 1rem;",
        paste(sport_name, "-", section_name)
      ),
      tags$p(
        class = "text-muted",
        "This module is coming soon."
      ),
      tags$p(
        class = "text-muted",
        style = "font-family: monospace; font-size: 0.85rem;",
        paste0("Create: R/", sport, "/mod_", sport, "_", section, ".R")
      )
    )
  )
}