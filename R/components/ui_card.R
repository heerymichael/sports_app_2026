# =============================================================================
# Component: Card
# 
# Reusable styled card component
# =============================================================================

#' Create a styled card
#' @param ... Card content
#' @param title Optional card header title
#' @param color Optional accent color: "teal", "coral", "sage", "gold", "frost"
#' @param class Additional CSS classes
#' @return Shiny tag
ui_card <- function(..., title = NULL, color = NULL, class = NULL) {
  
  # Build class string
  card_class <- "card"
  
  if (!is.null(color)) {
    valid_colors <- c("teal", "coral", "sage", "gold", "frost")
    if (color %in% valid_colors) {
      card_class <- paste0(card_class, " card--", color)
    }
  }
  
  if (!is.null(class)) {
    card_class <- paste(card_class, class)
  }
  
  # Build card
  if (!is.null(title)) {
    div(
      class = card_class,
      div(class = "card-header", title),
      div(class = "card-body", ...)
    )
  } else {
    div(
      class = card_class,
      ...
    )
  }
}

#' Create a card with a chart container
#' @param ... Chart content (e.g., plotOutput)
#' @param title Optional card header title
#' @param color Optional accent color
#' @return Shiny tag
ui_chart_card <- function(..., title = NULL, color = NULL) {
  ui_card(
    title = title,
    color = color,
    div(class = "chart-container", ...)
  )
}