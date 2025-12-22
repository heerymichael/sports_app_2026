# =============================================================================
# NHL Ice Hockey Dashboard Module (Placeholder)
# =============================================================================

#' NHL Dashboard UI
#' @param id Module namespace ID
nhl_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "page-header",
      tags$h2("Ice Hockey"),
      tags$p(class = "text-muted", "NHL analytics and player projections")
    ),
    
    ui_card(
      title = "Coming Soon",
      color = "sage",
      
      div(
        style = "text-align: center; padding: 3rem;",
        
        # Hockey icon placeholder
        div(
          style = "font-size: 4rem; margin-bottom: 1.5rem;",
          icon("hockey-puck")
        ),
        
        tags$h3(
          style = "font-weight: 700; margin-bottom: 1rem;",
          "NHL Analytics"
        ),
        
        tags$p(
          style = "color: var(--text-muted); max-width: 500px; margin: 0 auto 2rem;",
          "Player projections, line combinations, goalie analysis, and DFS optimization tools are under development."
        ),
        
        div(
          style = "display: flex; justify-content: center; gap: 2rem; flex-wrap: wrap; margin-top: 2rem;",
          
          div(
            style = "text-align: center; padding: 1rem;",
            div(style = "font-size: 1.5rem; color: var(--accent-coral); margin-bottom: 0.5rem;", icon("users")),
            div(style = "font-weight: 600;", "Skater Projections"),
            div(style = "font-size: 0.85rem; color: var(--text-muted);", "Goals, assists, SOG")
          ),
          
          div(
            style = "text-align: center; padding: 1rem;",
            div(style = "font-size: 1.5rem; color: var(--accent-teal); margin-bottom: 0.5rem;", icon("shield-alt")),
            div(style = "font-weight: 600;", "Goalie Analysis"),
            div(style = "font-size: 0.85rem; color: var(--text-muted);", "Save %, GAA")
          ),
          
          div(
            style = "text-align: center; padding: 1rem;",
            div(style = "font-size: 1.5rem; color: var(--accent-sage); margin-bottom: 0.5rem;", icon("layer-group")),
            div(style = "font-weight: 600;", "Line Stacks"),
            div(style = "font-size: 0.85rem; color: var(--text-muted);", "PP1, top lines")
          )
        )
      )
    )
  )
}

#' NHL Dashboard Server
#' @param id Module namespace ID
nhl_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder - no server logic yet
    log_debug("NHL Dashboard module initialized (placeholder)", level = "INFO")
  })
}


#' NHL Projections UI (Placeholder)
#' @param id Module namespace ID
nhl_projections_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "page-header",
      tags$h2("NHL Projections"),
      tags$p(class = "text-muted", "Player and goalie projections")
    ),
    
    ui_card(
      title = "Under Development",
      color = "teal",
      
      div(
        style = "text-align: center; padding: 2rem; color: var(--text-muted);",
        icon("wrench", style = "font-size: 2rem; margin-bottom: 1rem;"),
        tags$p("NHL projection models coming soon.")
      )
    )
  )
}

#' NHL Projections Server
#' @param id Module namespace ID
nhl_projections_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    log_debug("NHL Projections module initialized (placeholder)", level = "INFO")
  })
}