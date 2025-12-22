# =============================================================================
# R/config/app_themes.R
# Centralized Theme Configuration for Sports Analytics App
# =============================================================================

library(ggplot2)
library(reactable)
library(htmltools)

# =============================================================================
# COLOR PALETTE
# =============================================================================

APP_COLORS <- list(
  # Primary text colors
  primary = "#3B3226",
  secondary = "#5C4E3D",
  muted = "#7A7A7A",
  
  # Accent colors
  sage = "#A3BE8C",
  sage_dark = "#8FAF78",
  coral = "#D08770",
  coral_dark = "#BF7460",
  gold = "#EBCB8B",
  frost = "#A8C5D4",
  
  # Background colors
  bg_primary = "#FAF8F5",
  bg_secondary = "#F5F0EB",
  bg_card = "#FFFFFF",
  bg_tan = "#F9F7F4",
  
  # Borders and lines
  border = "#E5E9F0",
  border_dark = "#D8D0C4",
  grid = "#E5E9F0",
  
  # Chart-specific
  baseline = "#3B3226",
  highlight = "#E8F0E8"
)

# Named vectors for ggplot scales
COLORS_FOR_AGAINST <- c(
  "For" = APP_COLORS$sage,
  "Against" = APP_COLORS$coral
)

# =============================================================================
# FONT CONFIGURATION
# =============================================================================

APP_FONT <- "Plus Jakarta Sans"
APP_FONT_DISPLAY <- "Fjalla One"  # For axis labels in charts
APP_FONT_FALLBACK <- "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif"

get_app_font <- function() {
  tryCatch({
    if (requireNamespace("sysfonts", quietly = TRUE)) {
      if (APP_FONT %in% sysfonts::font_families()) {
        return(APP_FONT)
      }
    }
    return(APP_FONT_FALLBACK)
  }, error = function(e) {
    return(APP_FONT_FALLBACK)
  })
}

get_display_font <- function() {
  tryCatch({
    if (requireNamespace("sysfonts", quietly = TRUE)) {
      if (APP_FONT_DISPLAY %in% sysfonts::font_families()) {
        return(APP_FONT_DISPLAY)
      }
    }
    return(APP_FONT)  # Fall back to main app font
  }, error = function(e) {
    return(APP_FONT)
  })
}

# =============================================================================
# GGPLOT THEMES
# =============================================================================

theme_app <- function(base_size = 12) {
  font_family <- get_app_font()
  display_font <- get_display_font()
  
  theme_minimal(base_size = base_size, base_family = font_family) %+replace%
    theme(
      text = element_text(family = font_family, color = APP_COLORS$primary),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = APP_COLORS$grid, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text = element_text(
        family = display_font, 
        color = APP_COLORS$secondary, 
        size = base_size * 0.85
      ),
      axis.text.x = element_text(
        family = display_font,
        color = APP_COLORS$secondary,
        size = base_size * 0.85
      ),
      axis.text.y = element_text(
        family = display_font,
        color = APP_COLORS$secondary,
        size = base_size * 0.85
      ),
      axis.title = element_text(
        family = display_font,
        color = APP_COLORS$primary, 
        size = base_size, 
        face = "bold"
      ),
      axis.title.x = element_blank(),
      axis.line = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_blank(),
      legend.text = element_text(color = APP_COLORS$primary, size = base_size * 0.9, face = "bold"),
      legend.title = element_blank(),
      plot.caption = element_text(color = APP_COLORS$muted, size = base_size * 0.75, hjust = 0, margin = margin(t = 10)),
      strip.text = element_text(color = APP_COLORS$primary, size = base_size, face = "bold"),
      strip.background = element_blank(),
      plot.margin = margin(t = 5, r = 10, b = 10, l = 10)
    )
}

theme_app_timeseries <- function(base_size = 12) {
  display_font <- get_display_font()
  
  theme_app(base_size = base_size) %+replace%
    theme(
      axis.text.x.top = element_text(
        family = display_font,
        color = APP_COLORS$secondary, 
        size = base_size * 0.65, 
        lineheight = 0.9
      ),
      axis.text.x.bottom = element_blank()
    )
}

#' Team Performance Chart Theme
#' @description Specialized theme for game-by-game team performance charts
#' @param base_size Base font size
#' @param layout Either "side" for side-by-side or "stacked" for vertical facets
theme_team_performance <- function(base_size = 12, layout = "side") {
  display_font <- get_display_font()
  main_font <- get_app_font()
  
  # Panel spacing based on layout
  h_spacing <- if (layout == "side") unit(2, "lines") else unit(1, "lines")
  v_spacing <- if (layout == "stacked") unit(2.5, "lines") else unit(1, "lines")
  
  theme_app(base_size = base_size) %+replace%
    theme(
      # No axis titles
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      
      # Axis text: size 12, dark gray, Fjalla One font, NOT bold
      axis.text.x.top = element_text(
        family = display_font,
        color = APP_COLORS$secondary,
        size = 12,
        face = "plain",
        lineheight = 0.85,
        margin = margin(b = 8)
      ),
      axis.text.x.bottom = element_blank(),
      axis.text.y = element_text(
        family = display_font,
        color = APP_COLORS$secondary,
        size = 12,
        face = "plain"
      ),
      
      # Facet strip (team name): larger, bold, positioned outside/above
      strip.placement = "outside",
      strip.text = element_text(
        family = main_font,
        color = APP_COLORS$primary,
        size = 16,
        face = "bold",
        margin = margin(b = 6)
      ),
      strip.background = element_blank(),
      
      # Panel spacing for facets
      panel.spacing.x = h_spacing,
      panel.spacing.y = v_spacing,
      
      # Standard margins
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )
}

theme_app_scatter <- function(base_size = 12) {
  display_font <- get_display_font()
  
  theme_app(base_size = base_size) %+replace%
    theme(
      plot.background = element_rect(fill = APP_COLORS$bg_card, color = NA),
      panel.background = element_rect(fill = APP_COLORS$bg_card, color = NA),
      panel.grid.major.x = element_line(color = APP_COLORS$grid, linewidth = 0.5),
      panel.grid.major.y = element_line(color = APP_COLORS$grid, linewidth = 0.5),
      axis.title.x = element_text(
        family = display_font,
        color = APP_COLORS$secondary, 
        size = base_size * 0.9
      ),
      axis.title.y = element_text(
        family = display_font,
        color = APP_COLORS$secondary, 
        size = base_size * 0.9
      ),
      axis.text.x = element_text(
        family = display_font,
        color = APP_COLORS$secondary,
        size = base_size * 0.85
      ),
      axis.text.y = element_text(
        family = display_font,
        color = APP_COLORS$secondary,
        size = base_size * 0.85
      ),
      legend.position = "none"
    )
}

theme_app_bar <- function(base_size = 12) {
  display_font <- get_display_font()
  
  theme_app(base_size = base_size) %+replace%
    theme(
      plot.background = element_rect(fill = APP_COLORS$bg_card, color = NA),
      panel.background = element_rect(fill = APP_COLORS$bg_card, color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = APP_COLORS$grid, linewidth = 0.5),
      axis.text.y = element_text(
        family = display_font,
        color = APP_COLORS$secondary, 
        size = base_size * 0.8
      ),
      axis.text.x = element_text(
        family = display_font,
        color = APP_COLORS$secondary, 
        size = base_size * 0.8
      ),
      legend.position = "none"
    )
}

# =============================================================================
# REACTABLE THEMES
# =============================================================================

#' Standard reactable theme - clean, professional styling
#' @param compact Use compact padding (default TRUE)
app_reactable_theme <- function(compact = TRUE) {
  cell_pad <- if (compact) "6px 10px" else "10px 14px"
  header_pad <- if (compact) "8px 10px" else "10px 14px"
  font_sz <- if (compact) "13px" else "14px"
  
  reactable::reactableTheme(
    color = APP_COLORS$primary,
    backgroundColor = "white",
    borderColor = "#E8E8E8",
    stripedColor = APP_COLORS$bg_tan,
    highlightColor = "#F0F0F0",
    cellPadding = cell_pad,
    
    style = list(
      fontFamily = sprintf("'%s', %s", APP_FONT, APP_FONT_FALLBACK),
      fontSize = font_sz,
      borderRadius = "6px",
      boxShadow = "0 1px 3px rgba(0,0,0,0.04)"
    ),
    
    tableStyle = list(
      borderCollapse = "separate",
      borderSpacing = "0"
    ),
    
    headerStyle = list(
      background = "white",
      borderBottom = "2px solid #E0E0E0",
      color = APP_COLORS$muted,
      fontWeight = "600",
      fontSize = "10px",
      textTransform = "uppercase",
      letterSpacing = "0.5px",
      padding = header_pad
    ),
    
    cellStyle = list(
      borderBottom = "1px solid #F0F0F0"
    ),
    
    rowSelectedStyle = list(
      backgroundColor = APP_COLORS$highlight
    ),
    
    paginationStyle = list(
      borderTop = "1px solid #E8E8E8"
    ),
    
    searchInputStyle = list(
      borderRadius = "4px",
      border = "1px solid #E0E0E0",
      padding = "6px 10px"
    )
  )
}

#' Minimal theme for embedded tables
app_reactable_theme_minimal <- function() {
  reactable::reactableTheme(
    color = APP_COLORS$primary,
    backgroundColor = "transparent",
    borderColor = "transparent",
    stripedColor = "transparent",
    highlightColor = "rgba(163, 190, 140, 0.1)",
    cellPadding = "5px 8px",
    style = list(
      fontFamily = sprintf("'%s', %s", APP_FONT, APP_FONT_FALLBACK),
      fontSize = "12px"
    ),
    headerStyle = list(
      background = "transparent",
      color = APP_COLORS$muted,
      fontWeight = "600",
      fontSize = "9px",
      textTransform = "uppercase",
      letterSpacing = "0.5px",
      borderBottom = sprintf("1px solid %s", APP_COLORS$border),
      padding = "6px 8px"
    ),
    cellStyle = list(
      borderBottom = "none"
    )
  )
}

# =============================================================================
# CELL RENDERING HELPERS
# =============================================================================

get_heatmap_color <- function(value, max_val = 100, color = APP_COLORS$sage) {
  if (is.na(value) || is.null(value) || value == 0) return("transparent")
  norm_value <- min(1, max(0, value / max_val))
  opacity <- norm_value * 0.65
  rgb_vals <- col2rgb(color)
  sprintf("rgba(%d, %d, %d, %.2f)", rgb_vals[1], rgb_vals[2], rgb_vals[3], opacity)
}

create_heatmap_cell <- function(value, show_heatmap = TRUE, scale_factor = 1, format_string = "%.1f") {
  if (is.na(value) || is.null(value)) value <- 0
  bg_color <- if (show_heatmap) get_heatmap_color(value * scale_factor) else "transparent"
  div(
    style = sprintf("background: %s; padding: 4px 8px; border-radius: 4px; text-align: center; font-weight: 500;", bg_color),
    sprintf(format_string, value)
  )
}

create_team_cell <- function(team_name, logo_path = NULL, logo_size = 22) {
  logo_html <- if (!is.null(logo_path) && !is.na(logo_path) && logo_path != "") {
    tags$img(src = logo_path, style = sprintf("width: %dpx; height: %dpx; object-fit: contain; margin-right: 10px; vertical-align: middle;", logo_size, logo_size), onerror = "this.style.display='none';")
  } else NULL
  div(style = "display: flex; align-items: center;", logo_html, span(team_name, style = "font-weight: 600;"))
}

create_value_cell <- function(value, format_string = "%.2f", higher_is_better = TRUE, show_color = TRUE) {
  if (is.na(value) || is.null(value)) {
    return(div(style = sprintf("color: %s; text-align: center;", APP_COLORS$muted), "-"))
  }
  color <- APP_COLORS$primary
  if (show_color) {
    if (higher_is_better) {
      color <- if (value > 0) APP_COLORS$sage_dark else if (value < 0) APP_COLORS$coral_dark else APP_COLORS$primary
    } else {
      color <- if (value < 0) APP_COLORS$sage_dark else if (value > 0) APP_COLORS$coral_dark else APP_COLORS$primary
    }
  }
  div(style = sprintf("color: %s; text-align: center; font-weight: 600;", color), sprintf(format_string, value))
}

# =============================================================================
# GGPLOT SCALE FUNCTIONS
# =============================================================================

scale_color_for_against <- function(...) {
  scale_color_manual(values = COLORS_FOR_AGAINST, name = NULL, ...)
}

scale_fill_for_against <- function(...) {
  scale_fill_manual(values = COLORS_FOR_AGAINST, name = NULL, ...)
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

get_diverging_color <- function(value, positive_color = APP_COLORS$sage, negative_color = APP_COLORS$coral) {
  if (is.na(value) || is.null(value)) return(APP_COLORS$muted)
  if (value >= 0) return(positive_color)
  return(negative_color)
}

is_higher_better <- function(metric) {
  lower_is_better <- c("against", "conceded", "allowed", "lost")
  !any(sapply(lower_is_better, function(x) grepl(x, metric, ignore.case = TRUE)))
}

# =============================================================================
# NFL-SPECIFIC HEATMAP FUNCTIONS
# =============================================================================

#' Get diverging heatmap color for NFL value columns
#' 
#' Uses Coral Ã¢â€ â€™ White Ã¢â€ â€™ Teal diverging scale
#' Coral (#D08770) for below midpoint
#' White (#FFFFFF) at midpoint
#' Teal (#8FBCBB) for above midpoint
#' 
#' @param value Numeric value to color
#' @param midpoint Center point of the scale (default 1.0 for value columns, 0 for percentage)
#' @param min_val Minimum value in the scale (optional, for dynamic scaling)
#' @param max_val Maximum value in the scale (optional, for dynamic scaling)
#' @return CSS background-color string or empty string
get_diverging_heatmap_style <- function(value, midpoint = 1.0, min_val = NULL, max_val = NULL) {
  if (is.na(value) || is.null(value)) return("")
  
  # Coral: #D08770 = rgb(208, 135, 112)
  # White: #FFFFFF = rgb(255, 255, 255)
  # Teal:  #8FBCBB = rgb(143, 188, 187)
  
  # Set reasonable defaults if not provided
  if (is.null(min_val) || min_val >= midpoint) min_val <- midpoint - 0.5
  
  if (is.null(max_val) || max_val <= midpoint) max_val <- midpoint + 0.5
  
  if (value < midpoint) {
    # Below midpoint: interpolate coral -> white
    t <- (value - min_val) / (midpoint - min_val)
    t <- max(0, min(1, t))
    
    r <- round(208 + (255 - 208) * t)
    g <- round(135 + (255 - 135) * t)
    b <- round(112 + (255 - 112) * t)
  } else {
    # At or above midpoint: interpolate white -> teal
    t <- (value - midpoint) / (max_val - midpoint)
    t <- max(0, min(1, t))
    
    r <- round(255 + (143 - 255) * t)
    g <- round(255 + (188 - 255) * t)
    b <- round(255 + (187 - 255) * t)
  }
  
  sprintf("background-color: rgb(%d, %d, %d);", r, g, b)
}

#' Get diverging heatmap color (returns rgb string only)
#' 
#' @param value Numeric value to color
#' @param midpoint Center point of the scale
#' @param min_val Minimum value (optional)
#' @param max_val Maximum value (optional)
#' @return RGB color string
get_diverging_heatmap_color <- function(value, midpoint = 0, min_val = NULL, max_val = NULL) {
  if (is.na(value) || is.null(value)) return("")
  
  # Set reasonable defaults
  if (is.null(min_val) || min_val >= midpoint) min_val <- midpoint - 25
  if (is.null(max_val) || max_val <= midpoint) max_val <- midpoint + 25
  
  if (value < midpoint) {
    t <- (value - min_val) / (midpoint - min_val)
    t <- max(0, min(1, t))
    
    r <- round(208 + (255 - 208) * t)
    g <- round(135 + (255 - 135) * t)
    b <- round(112 + (255 - 112) * t)
  } else {
    t <- (value - midpoint) / (max_val - midpoint)
    t <- max(0, min(1, t))
    
    r <- round(255 + (143 - 255) * t)
    g <- round(255 + (188 - 255) * t)
    b <- round(255 + (187 - 255) * t)
  }
  
  sprintf("rgb(%d, %d, %d)", r, g, b)
}

#' Get sequential heatmap style for NFL projection columns
#' 
#' Uses White Ã¢â€ â€™ Teal Light sequential scale
#' 
#' @param value Numeric value
#' @param min_val Minimum value in range
#' @param max_val Maximum value in range
#' @return CSS background-color string
get_sequential_heatmap_style <- function(value, min_val, max_val) {
  if (is.na(value) || is.null(value)) return("")
  if (max_val == min_val) return("")
  
  # Normalize 0-1
  t <- (value - min_val) / (max_val - min_val)
  t <- max(0, min(1, t))
  
  # Interpolate white -> teal light (#A3D1D1 = rgb(163, 209, 209))
  r <- round(255 + (163 - 255) * t)
  g <- round(255 + (209 - 255) * t)
  b <- round(255 + (209 - 255) * t)
  
  sprintf("background-color: rgb(%d, %d, %d);", r, g, b)
}

#' Create a heatmap style function for a column of values
#' 
#' Returns a function that can be used with reactable cell styling
#' 
#' @param col_values Vector of all values in the column (for range calculation)
#' @param type "diverging" (for value columns with midpoint) or "sequential"
#' @param midpoint Midpoint for diverging scale (default 1.0)
#' @return Function that takes a value and returns CSS style string
create_nfl_heatmap_styler <- function(col_values, type = "sequential", midpoint = 1.0) {
  min_val <- min(col_values, na.rm = TRUE)
  max_val <- max(col_values, na.rm = TRUE)
  
  if (type == "diverging") {
    function(value) {
      get_diverging_heatmap_style(value, midpoint, min_val, max_val)
    }
  } else {
    function(value) {
      get_sequential_heatmap_style(value, min_val, max_val)
    }
  }
}

# =============================================================================
# REACTABLE COLUMN DEFINITIONS FOR NFL
# =============================================================================

#' Create standard NFL projection column definition
#' 
#' @param col_id Column ID in data
#' @param name Display name
#' @param format_str sprintf format string
#' @param heatmap_type "none", "sequential", or "diverging"
#' @param midpoint Midpoint for diverging scale
#' @param width Column width
#' @return reactable colDef object
nfl_projection_col <- function(col_id, name, format_str = "%.1f", 
                               heatmap_type = "sequential", midpoint = 1.0,
                               width = 80) {
  reactable::colDef(
    name = name,
    align = "center",
    width = width,
    cell = function(value) {
      if (is.na(value)) return("Ã¢â‚¬â€")
      sprintf(format_str, value)
    },
    style = function(value) {
      if (heatmap_type == "none" || is.na(value)) {
        list(fontWeight = 600)
      } else {
        # Note: actual styling needs column values passed in
        list(fontWeight = 600)
      }
    }
  )
}

message("App themes loaded: APP_COLORS, theme_app(), app_reactable_theme(), get_diverging_heatmap_style()")