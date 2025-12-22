# =============================================================================
# Module: NFL Dashboard
# 
# Main dashboard view for NFL analytics
# =============================================================================

#' NFL Dashboard UI
#' @param id Module namespace ID
nfl_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Page header
    div(
      class = "page-header",
      tags$h2("NFL Dashboard"),
      tags$p(class = "text-muted", "Fantasy football insights and player analysis")
    ),
    
    # Value boxes
    fluidRow(
      column(3, ui_value_box(
        value = textOutput(ns("players_count")),
        label = "Players Tracked",
        color = "teal"
      )),
      column(3, ui_value_box(
        value = textOutput(ns("avg_points")),
        label = "Avg Fantasy Pts",
        color = "coral"
      )),
      column(3, ui_value_box(
        value = textOutput(ns("prize_pool")),
        label = "Prize Pool",
        color = "sage"
      )),
      column(3, ui_value_box(
        value = textOutput(ns("current_week")),
        label = "Current Week",
        color = "yellow"
      ))
    ),
    
    tags$br(),
    
    # Main content
    fluidRow(
      column(
        4,
        ui_card(
          title = "Quick Filters",
          color = NFL_CARD_COLOR,
          selectInput(ns("position"), "Position",
                      choices = c("All", "QB", "RB", "WR", "TE", "K", "DEF")),
          selectInput(ns("week"), "Week",
                      choices = paste("Week", 15:1)),
          sliderInput(ns("salary"), "Salary Range ($)",
                      min = 3000, max = 10000, value = c(4000, 8000), step = 500),
          actionButton(ns("apply_btn"), "Apply Filters", class = "btn-primary")
        )
      ),
      column(
        8,
        ui_card(
          title = "Top Players",
          color = NFL_CARD_COLOR,
          tableOutput(ns("top_players"))
        ),
        ui_chart_card(
          title = "Points by Position",
          color = NFL_CARD_COLOR,
          plotOutput(ns("position_chart"), height = "300px")
        )
      )
    )
  )
}

#' NFL Dashboard Server
#' @param id Module namespace ID
nfl_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Value box outputs
    output$players_count <- renderText({ "324" })
    output$avg_points <- renderText({ "15.2" })
    output$prize_pool <- renderText({ "$52.1K" })
    output$current_week <- renderText({ "Week 15" })
    
    # Top players table
    output$top_players <- renderTable({
      data.frame(
        Player = c("Josh Allen", "Derrick Henry", "Ja'Marr Chase", "Travis Kelce"),
        Position = c("QB", "RB", "WR", "TE"),
        Team = c("BUF", "BAL", "CIN", "KC"),
        `Fantasy Pts` = c(24.5, 18.2, 22.1, 15.8),
        Salary = c("$8,200", "$7,800", "$8,000", "$6,500"),
        check.names = FALSE
      )
    })
    
    # Position chart
    output$position_chart <- renderPlot({
      colors <- c("#8FBCBB", "#D08770", "#A3BE8C", "#EBCB8B", "#A8C5D4")
      
      par(
        bg = "#FFFFFF",
        family = "sans",
        mar = c(5, 4, 2, 2),
        col.axis = "#3B3226",
        col.lab = "#3B3226"
      )
      
      positions <- c("QB", "RB", "WR", "TE", "K")
      values <- c(22.5, 15.8, 18.2, 12.4, 8.1)
      
      barplot(values, names.arg = positions, col = colors, border = "#3B3226",
              ylab = "Avg Points", lwd = 2)
    }, bg = "transparent")
    
    # Filter button
    observeEvent(input$apply_btn, {
      showNotification("Filters applied!", type = "message", duration = 2)
    })
  })
}