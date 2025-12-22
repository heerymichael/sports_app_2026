#' # =============================================================================
#' # Module: Sport Navigation (Level 1)
#' # 
#' # Top-level navigation showing sport icons (NFL, Soccer, Golf)
#' # =============================================================================
#' 
#' #' Sport Navigation UI
#' #' @param id Module namespace ID
#' sport_nav_ui <- function(id) {
#'   ns <- NS(id)
#'   
#'   sports <- get_sports_config()
#'   
#'   # Build nav items for each sport
#'   nav_items <- lapply(names(sports), function(sport_id) {
#'     sport <- sports[[sport_id]]
#'     
#'     tags$a(
#'       id = ns(paste0("nav_", sport_id)),
#'       class = paste0("sport-nav-item sport-nav-item--", sport_id),
#'       href = "#",
#'       onclick = sprintf(
#'         "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
#'         ns("selected_sport"),
#'         sport_id
#'       ),
#'       tags$img(
#'         class = "sport-nav-icon",
#'         src = paste0("images/", sport$icon),
#'         alt = sport$name
#'       ),
#'       tags$span(class = "sport-nav-label", sport$name)
#'     )
#'   })
#'   
#'   div(
#'     class = "sports-navbar",
#'     nav_items
#'   )
#' }
#' 
#' #' Sport Navigation Server
#' #' @param id Module namespace ID
#' #' @return Reactive value containing selected sport ID
#' sport_nav_server <- function(id) {
#'   moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'     
#'     sports <- get_sports_config()
#'     
#'     # Track selected sport - default to first sport
#'     selected <- reactiveVal(names(sports)[1])
#'     
#'     # Update on click
#'     observeEvent(input$selected_sport, {
#'       selected(input$selected_sport)
#'       
#'       # Update active class via JS
#'       session$sendCustomMessage("updateSportNav", list(
#'         ns_prefix = ns(""),
#'         sport = input$selected_sport
#'       ))
#'     })
#'     
#'     # Set initial active state
#'     observe({
#'       # Only run once on init
#'       isolate({
#'         session$sendCustomMessage("updateSportNav", list(
#'           ns_prefix = ns(""),
#'           sport = selected()
#'         ))
#'       })
#'     }) |> bindEvent(TRUE, once = TRUE)
#'     
#'     # Return selected sport as reactive
#'     return(selected)
#'   })
#' }