# =============================================================================
# Component: Value Box
# 
# Reusable styled value box component
# =============================================================================

#' Create a styled value box
#' @param value The main value to display
#' @param label Caption/label below the value
#' @param color Accent color. Preferred: "teal", "coral", "sage", "gold", "frost"
#'              Also accepts: "yellow" (alias for gold), "sky" (alias for frost)
#' @param icon Optional icon name (not currently implemented)
#' @return Shiny tag
ui_value_box <- function(value, label, color = "teal", icon = NULL) {
  
  # Valid colors - matches APP_COLORS naming
  # yellow/sky are aliases for backward compatibility
  valid_colors <- c("teal", "coral", "sage", "gold", "yellow", "frost", "sky")
  
  if (!(color %in% valid_colors)) {
    color <- "teal"
  }
  
  div(
    class = paste0("value-box value-box--", color),
    div(class = "value", value),
    div(class = "caption", label)
  )
}

#' Create a value box output (for use with renderUI)
#' @param outputId Output ID
#' @param color Accent color
#' @return Shiny UI output
value_box_output <- function(outputId, color = "teal") {
  
  valid_colors <- c("teal", "coral", "sage", "gold", "yellow", "frost", "sky")
  
  if (!(color %in% valid_colors)) {
    color <- "teal"
  }
  
  div(
    class = paste0("value-box value-box--", color),
    div(class = "value", textOutput(outputId)),
    div(class = "caption", textOutput(paste0(outputId, "_label")))
  )
}

#' Create a row of value boxes
#' @param ... Value box elements
#' @param cols Number of columns (default 4)
#' @return Shiny fluid row
ui_value_box_row <- function(..., cols = 4) {
  boxes <- list(...)
  col_width <- floor(12 / min(cols, length(boxes)))
  
  fluidRow(
    lapply(boxes, function(box) {
      column(col_width, box)
    })
  )
}