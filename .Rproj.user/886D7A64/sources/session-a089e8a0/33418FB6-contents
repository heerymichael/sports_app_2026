# =============================================================================
# NFL UI Helpers
# 
# Shared UI components for NFL modules including:
# - Player row rendering
# - Lineup slot display
# - Position badges
# - Value/projection badges
# - Lineup card rendering
# 
# Dependencies: htmltools, shiny
# =============================================================================

# =============================================================================
# POSITION BADGE
# =============================================================================

#' Create a styled position badge
#' 
#' @param position Position abbreviation (QB, RB, WR, TE, DST)
#' @param size Size variant: "small", "normal", "large"
#' @return HTML span element
create_position_badge <- function(position, size = "normal") {
  size_class <- switch(size,
                       "small" = "position-badge--sm",
                       "large" = "position-badge--lg",
                       "xs" = "position-badge--xs",
                       ""  # normal uses base class
  )
  tags$span(class = paste("position-badge", size_class), position)
}

# =============================================================================
# ADJUSTMENT BADGE
# =============================================================================

#' Create an adjustment percentage badge
#' 
#' @param adj_pct Adjustment percentage (positive or negative)
#' @param size Size variant: "small", "normal"
#' @return HTML span element or NULL if adj_pct is 0
create_adjustment_badge <- function(adj_pct, size = "normal") {
  if (is.null(adj_pct) || adj_pct == 0) return(NULL)
  
  is_positive <- adj_pct > 0
  bg_color <- if (is_positive) "rgba(139, 168, 134, 0.2)" else "rgba(232, 131, 121, 0.2)"
  text_color <- if (is_positive) "var(--accent-sage)" else "var(--accent-coral)"
  
  font_size <- if (size == "small") "0.65rem" else "0.75rem"
  padding <- if (size == "small") "0.1rem 0.25rem" else "0.15rem 0.35rem"
  
  tags$span(
    style = sprintf(
      "font-size: %s; padding: %s; border-radius: 3px; background: %s; color: %s; font-weight: 600;",
      font_size, padding, bg_color, text_color
    ),
    sprintf("%+.0f%%", adj_pct)
  )
}

# =============================================================================
# VALUE INDICATOR
# =============================================================================
#' Create a value indicator showing comparison to optimal
#' 
#' @param value Current value
#' @param optimal Optimal value to compare against
#' @param show_sign Whether to show + sign for positive values
#' @return HTML span element
create_value_indicator <- function(value, optimal = NULL, show_sign = TRUE) {
  
  if (is.null(optimal) || optimal == 0) {
    return(tags$span(style = "color: var(--text-muted);", "—"))
  }
  
  diff <- value - optimal
  is_good <- diff > -3  # Within 3 points of optimal
  
  color <- if (is_good) "var(--accent-sage)" else "var(--accent-coral)"
  format_str <- if (show_sign) "%+.1f" else "%.1f"
  
  tags$span(
    style = sprintf("font-weight: 600; color: %s;", color),
    sprintf(format_str, diff)
  )
}

# =============================================================================
# PLAYER ROW COMPONENTS
# =============================================================================

#' Create a compact player info cell (for tables)
#' 
#' @param player_name Player name
#' @param position Position
#' @param team Team abbreviation
#' @param headshot_url URL to headshot image
#' @param team_bg_color Background color for headshot circle
#' @param opponent Opponent team (optional)
#' @param home Is home game (optional)
#' @param show_matchup Whether to show matchup line
#' @return HTML div element
create_player_cell <- function(player_name, position, team, 
                               headshot_url = NULL, team_bg_color = NULL,
                               opponent = NULL, home = TRUE,
                               show_matchup = TRUE) {
  
  div(
    style = "display: flex; align-items: center; gap: 0.75rem;",
    
    # Headshot
    create_headshot_html(
      headshot_url, 
      team_bg_color, 
      "small", 
      position, 
      team
    ),
    
    # Name and matchup
    div(
      div(style = "font-weight: 600;", player_name),
      if (show_matchup) {
        create_matchup_html(team, opponent, home)
      }
    )
  )
}

# =============================================================================
# LINEUP SLOT DISPLAY
# =============================================================================

#' Create a lineup slot row for handbuild display
#' 
#' @param slot_name Internal slot name (e.g., "RB1")
#' @param label Display label (e.g., "RB")
#' @param player_data Data row for player in slot (NULL if empty)
#' @param qb_team QB's team for stack highlighting
#' @param adjustments List of player adjustments
#' @param ns Namespace function for action button IDs
#' @return HTML div element
create_lineup_slot_row <- function(slot_name, label, player_data, 
                                   qb_team = NULL, adjustments = list(), 
                                   ns = identity) {
  
  if (is.null(player_data)) {
    # Empty slot
    return(
      div(
        style = "display: flex; align-items: center; padding: 0.3rem 0.4rem; background: var(--bg-secondary); border-radius: 4px; margin-bottom: 0.3rem;",
        create_position_badge(label, "small"),
        tags$span(
          style = "flex: 1; padding-left: 0.5rem; color: var(--text-muted); font-style: italic; font-size: 0.8rem;",
          "Empty"
        )
      )
    )
  }
  
  # Check if player stacks with QB
  is_stack <- !is.null(qb_team) && player_data$team[1] == qb_team && slot_name != "QB"
  row_bg <- if (is_stack) "background: #FFF9E6;" else "background: white;"
  
  # Check for adjustment
  player_adj <- adjustments[[player_data$player[1]]]
  
  div(
    style = sprintf(
      "display: flex; align-items: center; padding: 0.3rem 0.4rem; %s border: 2px solid var(--text-primary); border-radius: 4px; margin-bottom: 0.3rem;",
      row_bg
    ),
    
    # Position badge
    create_position_badge(label, "small"),
    
    # Headshot
    div(
      style = "margin-left: 0.3rem;",
      create_headshot_html(
        player_data$headshot_url[1],
        player_data$team_bg_color[1],
        "small",
        player_data$position[1],
        player_data$team[1]
      )
    ),
    
    # Player info
    div(
      style = "flex: 1; padding-left: 0.3rem;",
      div(style = "font-weight: 600; font-size: 0.8rem; line-height: 1.2;", player_data$player[1]),
      create_matchup_html(player_data$team[1], player_data$opponent[1], player_data$home[1])
    ),
    
    # Adjustment badge if applicable
    create_adjustment_badge(player_adj, "small"),
    
    # Salary and projection
    div(
      style = "text-align: right; padding-right: 0.3rem;",
      div(style = "font-weight: 600; font-size: 0.75rem;", sprintf("$%.1f", player_data$salary[1])),
      div(style = "font-size: 0.65rem; color: var(--accent-coral); font-weight: 600;",
          sprintf("%.1f pts", player_data$blended[1]))
    ),
    
    # Remove button
    actionButton(
      ns(paste0("remove_", slot_name)),
      icon("times"),
      class = "btn-secondary",
      style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;"
    )
  )
}

# =============================================================================
# LINEUP CARD (for generated lineups)
# =============================================================================

#' Create a lineup card for display in grid
#' 
#' @param lineup Lineup data frame
#' @param lineup_num Lineup number for display
#' @param optimal_proj Optimal projection for comparison
#' @param player_data Full player data (for headshots lookup)
#' @param adjustments List of player adjustments
#' @param locked_players Character vector of locked player names
#' @return HTML div element
create_lineup_card <- function(lineup, lineup_num, optimal_proj = 0,
                               player_data = NULL, adjustments = list(),
                               locked_players = c()) {
  
  # Calculate lineup stats
  salary <- sum(lineup$salary)
  
  # Calculate adjusted projection
  adj_proj <- sum(sapply(1:nrow(lineup), function(j) {
    adj_pct <- adjustments[[lineup$player[j]]] %||% 0
    lineup$projection[j] * (1 + adj_pct / 100)
  }))
  
  vs_optimal <- if (optimal_proj > 0) adj_proj - optimal_proj else NA
  
  div(
    style = "background: white; border: 2px solid var(--text-primary); border-radius: 8px; padding: 1rem; box-shadow: 4px 4px 0 rgba(59,50,38,0.15);",
    
    # Header
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.75rem; padding-bottom: 0.5rem; border-bottom: 2px dashed var(--bg-secondary);",
      tags$span(
        style = "font-weight: 800; text-transform: uppercase; font-size: 0.85rem;",
        sprintf("Lineup %d", lineup_num)
      ),
      div(
        style = "display: flex; gap: 0.75rem; font-size: 0.85rem;",
        tags$span(style = "font-weight: 700; color: var(--accent-coral);", sprintf("%.1f", adj_proj)),
        tags$span(style = "color: var(--text-muted);", sprintf("$%.1f", salary)),
        if (!is.na(vs_optimal)) {
          create_value_indicator(adj_proj, optimal_proj)
        }
      )
    ),
    
    # Players grid
    div(
      style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 0.35rem; font-size: 0.8rem;",
      lapply(1:nrow(lineup), function(j) {
        # Get headshot info from player_data if available
        headshot_url <- lineup$headshot_url[j] %||% 
          "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png"
        team_bg <- lineup$team_bg_color[j] %||% "#E0E0E0"
        
        # Check if locked
        is_locked <- lineup$player[j] %in% locked_players
        opacity_style <- if (is_locked) "opacity: 0.5;" else ""
        
        # Check for adjustment
        player_adj <- adjustments[[lineup$player[j]]]
        
        div(
          style = sprintf(
            "display: flex; align-items: center; gap: 0.3rem; padding: 0.2rem 0.3rem; background: var(--bg-tertiary); border-radius: 4px; %s",
            opacity_style
          ),
          
          # Mini headshot
          div(
            style = sprintf(
              "width: 22px; height: 22px; border-radius: 50%%; background: %s; overflow: hidden; flex-shrink: 0;",
              team_bg
            ),
            if (lineup$position[j] == "DST") {
              tags$img(
                src = sprintf("nfl_logos/%s.webp", lineup$team[j]),
                style = "width: 100%; height: 100%; object-fit: contain;",
                onerror = "this.style.display='none'"
              )
            } else {
              tags$img(
                src = headshot_url,
                style = "width: 100%; height: 100%; object-fit: cover;",
                onerror = "this.src='https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png'"
              )
            }
          ),
          
          # Position badge
          tags$span(
            style = "background: var(--text-primary); color: white; padding: 0.1rem 0.2rem; border-radius: 3px; font-size: 0.6rem; font-weight: 700;",
            lineup$position[j]
          ),
          
          # Player name
          tags$span(
            style = "font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: 0.75rem;",
            lineup$player[j]
          ),
          
          # Adjustment badge
          if (!is.null(player_adj) && player_adj != 0) {
            tags$span(
              style = sprintf(
                "font-size: 0.55rem; padding: 0.05rem 0.15rem; border-radius: 2px; background: %s; color: %s;",
                if (player_adj > 0) "rgba(139, 168, 134, 0.3)" else "rgba(232, 131, 121, 0.3)",
                if (player_adj > 0) "var(--accent-sage)" else "var(--accent-coral)"
              ),
              sprintf("%+.0f%%", player_adj)
            )
          }
        )
      })
    )
  )
}

# =============================================================================
# SUMMARY STAT BOX
# =============================================================================

#' Create a summary stat box
#' 
#' @param label Label text
#' @param value Numeric value to display
#' @param format_str sprintf format string
#' @param color Text color (optional)
#' @param is_adjusted Whether this is an adjusted value
#' @return HTML div element
create_stat_box <- function(label, value, format_str = "%.1f", 
                            color = NULL, is_adjusted = FALSE) {
  
  label_text <- if (is_adjusted) paste0(label, " (Adj)") else label
  value_color <- color %||% "var(--text-primary)"
  
  div(
    style = "text-align: center;",
    div(
      style = "font-size: 0.75rem; text-transform: uppercase; color: var(--text-muted);",
      label_text
    ),
    div(
      style = sprintf("font-size: 1.25rem; font-weight: 700; color: %s;", value_color),
      if (is.na(value) || is.null(value)) "—" else sprintf(format_str, value)
    )
  )
}

# =============================================================================
# LINEUP SUMMARY BAR
# =============================================================================

#' Create a lineup summary stats bar
#' 
#' @param stats List with total_salary, total_projection, remaining_salary, filled_count
#' @param optimal_proj Optimal projection for comparison
#' @param has_adjustments Whether adjustments are active
#' @return HTML div element
create_lineup_summary <- function(stats, optimal_proj = 0, has_adjustments = FALSE) {
  
  div(
    style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 0.4rem; margin-bottom: 0.5rem; padding: 0.4rem; background: var(--bg-tertiary); border-radius: 6px;",
    
    # Salary
    div(
      style = "text-align: center;",
      div(style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted);", "Salary"),
      div(style = "font-weight: 700; font-size: 0.85rem;", sprintf("$%.1f", stats$total_salary))
    ),
    
    # Projection
    div(
      style = "text-align: center;",
      div(
        style = "font-size: 0.6rem; text-transform: uppercase; color: var(--text-muted);",
        if (has_adjustments) "Adj Proj" else "Projection"
      ),
      div(
        style = sprintf(
          "font-weight: 700; font-size: 0.85rem; color: %s;",
          if (has_adjustments) "var(--accent-coral)" else "var(--text-primary)"
        ),
        sprintf("%.1f", stats$adjusted_projection %||% stats$total_projection)
      )
    )
  )
}

# =============================================================================
# EXPORT MESSAGE
# =============================================================================

message("NFL UI helpers loaded: create_position_badge(), create_lineup_slot_row(), create_lineup_card()")