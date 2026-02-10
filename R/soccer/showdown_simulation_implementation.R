################################################################################
# SOCCER SHOWDOWN - MATCH SIMULATION ENGINE
#
# Bivariate Poisson model for score and shot-on-target matrices.
#
# Theory:
#   Standard Poisson assumes goals are independent: P(h,a) = dpois(h,λh)*dpois(a,λa)
#   But bookmaker draw probabilities are typically HIGHER than independent Poisson
#   implies. The Holgate (1964) bivariate Poisson introduces a correlation
#   parameter ρ ≥ 0 that inflates diagonal (draw) probabilities:
#
#   X = Z1 + Z3,  Y = Z2 + Z3
#   Z1 ~ Poi(λ1), Z2 ~ Poi(λ2), Z3 ~ Poi(ρ)
#   where λ1 = μ_home - ρ,  λ2 = μ_away - ρ
#
#   The calibration step finds ρ such that P(draw) from the bivariate Poisson
#   matches the user's stated draw probability.
#
# Usage:
#   1. User adjusts odds → implied goals (μ_home, μ_away) via existing engine
#   2. calibrate_rho() finds ρ to match user's draw%
#   3. generate_score_matrix() produces 0-4+ bivariate probability grid
#   4. generate_sot_matrix() does the same for shots on target
#
# INSERT INTO: soccer_showdown_config.R
#   Location: Between predict_team_stats() and PLAYER PROJECTION ENGINE section
#   (after line 490, before line 492)
################################################################################


# =============================================================================
# ADDITION TO soccer_showdown_config.R
# Insert after predict_team_stats() function (line 490)
# =============================================================================

# =============================================================================
# BIVARIATE POISSON SIMULATION ENGINE
# Holgate (1964) formulation with draw-probability calibration
# =============================================================================

#' Bivariate Poisson probability mass function
#'
#' Computes P(X=x, Y=y) where (X,Y) follow a bivariate Poisson distribution
#' with marginal means mu_x, mu_y and covariance parameter rho.
#'
#' @param x Non-negative integer, home goals/shots
#' @param y Non-negative integer, away goals/shots
#' @param mu_x Expected value of X (home team)
#' @param mu_y Expected value of Y (away team)
#' @param rho Covariance parameter (0 = independent Poisson). Must be in [0, min(mu_x, mu_y))
#' @return Probability P(X=x, Y=y)
bpois_pmf <- function(x, y, mu_x, mu_y, rho = 0) {
  # Clamp rho to valid range
  rho <- max(0, min(rho, min(mu_x, mu_y) - 1e-6))
  
  # Independent Poisson shortcut
  if (rho < 1e-8) return(dpois(x, mu_x) * dpois(y, mu_y))
  
  l1 <- mu_x - rho  # independent home component
  
  l2 <- mu_y - rho  # independent away component
  l3 <- rho          # shared component
  
  k_max <- min(x, y)
  s <- 0
  for (k in 0:k_max) {
    s <- s + (l1^(x - k) * l2^(y - k) * l3^k) /
      (factorial(x - k) * factorial(y - k) * factorial(k))
  }
  
  exp(-(l1 + l2 + l3)) * s
}

#' Calibrate correlation parameter to match target draw probability
#'
#' Finds rho such that sum of P(k,k) for k=0..N equals the user's draw%.
#' If independent Poisson already produces a draw% >= target, returns 0
#' (bivariate Poisson can only increase draw probability).
#'
#' @param mu_home Expected home goals
#' @param mu_away Expected away goals
#' @param target_draw_pct Target draw probability (0-100 scale)
#' @param precision_goals Max goals to sum over for draw probability (default 10)
#' @return Optimal rho value (>= 0)
calibrate_rho <- function(mu_home, mu_away, target_draw_pct, precision_goals = 10) {
  target <- target_draw_pct / 100
  
  # Independent Poisson draw probability
  indep_draw <- sum(dpois(0:precision_goals, mu_home) * dpois(0:precision_goals, mu_away))
  
  # If independent Poisson already matches or exceeds target, no correlation needed
  if (indep_draw >= target - 0.002) return(0)
  
  # Maximum possible rho
  max_rho <- min(mu_home, mu_away) - 0.01
  if (max_rho <= 0.001) return(0)
  
  # Objective: P(draw | rho) - target = 0
  draw_prob_diff <- function(rho) {
    p_draw <- 0
    for (k in 0:precision_goals) {
      p_draw <- p_draw + bpois_pmf(k, k, mu_home, mu_away, rho)
    }
    p_draw - target
  }
  
  # Check if max rho can even reach the target
  max_draw <- tryCatch(draw_prob_diff(max_rho) + target, error = function(e) 0)
  if (max_draw < target) {
    # Can't reach target even with max correlation - use max
    return(max_rho)
  }
  
  tryCatch({
    result <- uniroot(draw_prob_diff, c(0, max_rho), tol = 0.0005)
    max(0, result$root)
  }, error = function(e) 0)
}

#' Generate bivariate Poisson probability matrix (0 to max_val+)
#'
#' Produces a (max_val+1) x (max_val+1) matrix where the last row/column
#' captures all overflow probability (i.e., "4+" means "4 or more").
#'
#' @param mu_x Expected value for row variable (home team)
#' @param mu_y Expected value for column variable (away team)
#' @param rho Correlation parameter (from calibrate_rho)
#' @param max_val Maximum explicit value; last bucket = "max_val+" (default 4)
#' @param precision Upper limit for internal computation (default 12)
#' @return Named list: matrix (probability grid), row_labels, col_labels,
#'         result_probs (home_win, draw, away_win percentages)
generate_bivariate_matrix <- function(mu_x, mu_y, rho = 0, max_val = 4, precision = 12) {
  # Compute full fine-grained grid
  n <- precision + 1
  full_grid <- matrix(0, nrow = n, ncol = n)
  
  for (i in 0:precision) {
    for (j in 0:precision) {
      full_grid[i + 1, j + 1] <- bpois_pmf(i, j, mu_x, mu_y, rho)
    }
  }
  
  # Aggregate into 0..max_val-1, max_val+ buckets
  display_n <- max_val + 1
  display_grid <- matrix(0, nrow = display_n, ncol = display_n)
  
  for (i in 1:display_n) {
    for (j in 1:display_n) {
      if (i < display_n && j < display_n) {
        # Exact cell (e.g., home=0, away=0)
        display_grid[i, j] <- full_grid[i, j]
      } else if (i == display_n && j < display_n) {
        # Row overflow: home=max_val+, away=j-1
        display_grid[i, j] <- sum(full_grid[display_n:n, j])
      } else if (i < display_n && j == display_n) {
        # Column overflow: home=i-1, away=max_val+
        display_grid[i, j] <- sum(full_grid[i, display_n:n])
      } else {
        # Corner: both overflow
        display_grid[i, j] <- sum(full_grid[display_n:n, display_n:n])
      }
    }
  }
  
  # Labels
  labels <- c(as.character(0:(max_val - 1)), paste0(max_val, "+"))
  
  # Calculate result probabilities from full grid
  home_win <- 0; draw <- 0; away_win <- 0
  for (i in 0:precision) {
    for (j in 0:precision) {
      p <- full_grid[i + 1, j + 1]
      if (i > j) home_win <- home_win + p
      else if (i == j) draw <- draw + p
      else away_win <- away_win + p
    }
  }
  
  list(
    matrix = display_grid,
    row_labels = labels,
    col_labels = labels,
    result_probs = list(
      home_win = round(home_win * 100, 1),
      draw = round(draw * 100, 1),
      away_win = round(away_win * 100, 1)
    )
  )
}

#' Generate score matrix from user's adjusted odds
#'
#' Convenience wrapper that calibrates rho and produces the goals matrix.
#'
#' @param mu_home Implied home goals (from calculate_implied_goals)
#' @param mu_away Implied away goals
#' @param draw_pct User's draw probability (0-100)
#' @param max_goals Grid cutoff (default 4 for "0-4+")
#' @return Output from generate_bivariate_matrix plus rho value
generate_score_matrix <- function(mu_home, mu_away, draw_pct, max_goals = 4) {
  rho <- calibrate_rho(mu_home, mu_away, draw_pct)
  
  result <- generate_bivariate_matrix(mu_home, mu_away, rho,
                                      max_val = max_goals, precision = 12)
  result$rho <- round(rho, 4)
  result$mu_home <- mu_home
  result$mu_away <- mu_away
  result
}

#' Generate shots-on-target matrix
#'
#' Uses predicted SoT from regression model as Poisson parameters.
#' SoT correlation is derived from goal correlation (scaled by conversion rate).
#'
#' @param sot_home Predicted home SoT (from predict_team_stats)
#' @param sot_away Predicted away SoT
#' @param goal_rho Goal correlation parameter (from calibrate_rho)
#' @param max_sot Grid cutoff (default 4 for "0-4+")
#' @return Output from generate_bivariate_matrix
generate_sot_matrix <- function(sot_home, sot_away, goal_rho = 0, max_sot = 4) {
  # SoT correlation: scale goal rho by shot-to-goal ratio
  # Goals are roughly 30% of SoT, so SoT correlation is weaker
  sot_rho <- goal_rho * 0.3
  sot_rho <- min(sot_rho, min(sot_home, sot_away) - 0.01)
  sot_rho <- max(0, sot_rho)
  
  result <- generate_bivariate_matrix(sot_home, sot_away, sot_rho,
                                      max_val = max_sot, precision = 15)
  result$sot_home <- sot_home
  result$sot_away <- sot_away
  result
}


################################################################################
# UI ADDITION TO mod_soccer_showdown.R
#
# Insert AFTER the "Your View" card closing (line 118-120)
# and BEFORE the "Confirmed Lineups" card (line 122)
#
# i.e., replace:
#   tags$br(),
#   
#   # =========================================================================
#   # LINEUP CONFIRMATION
#
# with the simulation card + tags$br() + the lineup confirmation header
################################################################################

# --- UI BLOCK TO INSERT ---
# (This goes between "Your View" closing and "Confirmed Lineups")

# tags$br(),
# 
# # =========================================================================
# # MATCH SIMULATION - Score & SoT Matrices
# # =========================================================================
# ui_card(
#   title = "Match Simulation",
#   color = APP_COLORS$sage,
#   
#   div(
#     style = "font-size: 0.85rem; color: var(--text-muted); margin-bottom: 1rem;",
#     "Bivariate Poisson simulation calibrated to your view. ",
#     "The correlation parameter (ρ) is fitted to match your draw probability."
#   ),
#   
#   fluidRow(
#     column(2,
#            selectInput(ns("sim_display_mode"), "Display Mode",
#                        choices = c("HTML Grid" = "html", "Heatmap Plot" = "plot"),
#                        selected = "html")
#     ),
#     column(10,
#            uiOutput(ns("sim_calibration_info"))
#     )
#   ),
#   
#   # Simulation results bar
#   uiOutput(ns("sim_result_bar")),
#   
#   tags$br(),
#   
#   fluidRow(
#     column(6,
#            tags$h4("Score Matrix", style = "text-align: center; font-weight: 600; margin-bottom: 0.5rem;"),
#            conditionalPanel(
#              condition = sprintf("input['%s'] == 'html'", ns("sim_display_mode")),
#              uiOutput(ns("score_matrix_html"))
#            ),
#            conditionalPanel(
#              condition = sprintf("input['%s'] == 'plot'", ns("sim_display_mode")),
#              plotOutput(ns("score_matrix_plot"), height = "350px")
#            )
#     ),
#     column(6,
#            tags$h4("Shots on Target Matrix", style = "text-align: center; font-weight: 600; margin-bottom: 0.5rem;"),
#            conditionalPanel(
#              condition = sprintf("input['%s'] == 'html'", ns("sim_display_mode")),
#              uiOutput(ns("sot_matrix_html"))
#            ),
#            conditionalPanel(
#              condition = sprintf("input['%s'] == 'plot'", ns("sim_display_mode")),
#              plotOutput(ns("sot_matrix_plot"), height = "350px")
#            )
#     )
#   )
# ),
# 
# tags$br(),


################################################################################
# SERVER ADDITIONS TO mod_soccer_showdown.R
#
# Insert AFTER the implied_stats_display renderUI (around line 640)
# and BEFORE the lineup display render functions
################################################################################

# --- SERVER BLOCK TO INSERT ---

# # =========================================================================
# # REACTIVE: Simulation matrices
# # =========================================================================
# simulation_results <- reactive({
#   preds <- tryCatch(team_predictions(), error = function(e) NULL)
#   if (is.null(preds)) return(NULL)
#   
#   # Get user's draw probability
#   hw <- input$home_win_pct %||% 50
#   dr <- input$draw_pct %||% 25
#   aw <- input$away_win_pct %||% 25
#   total_pct <- hw + dr + aw
#   if (total_pct == 0) total_pct <- 100
#   draw_pct_normalized <- (dr / total_pct) * 100
#   
#   # Score matrix
#   score <- generate_score_matrix(
#     mu_home = preds$home_goals,
#     mu_away = preds$away_goals,
#     draw_pct = draw_pct_normalized,
#     max_goals = 4
#   )
#   
#   # SoT matrix
#   sot <- generate_sot_matrix(
#     sot_home = preds$home_stats$sot,
#     sot_away = preds$away_stats$sot,
#     goal_rho = score$rho,
#     max_sot = 4
#   )
#   
#   list(score = score, sot = sot)
# })
# 
# # =========================================================================
# # RENDER: Calibration info bar
# # =========================================================================
# output$sim_calibration_info <- renderUI({
#   sim <- tryCatch(simulation_results(), error = function(e) NULL)
#   if (is.null(sim)) return(NULL)
#   
#   info <- rv$match_info
#   if (is.null(info)) return(NULL)
#   
#   home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
#   away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
#   
#   div(
#     style = "margin-top: 25px; padding: 0.5rem 0.75rem; background: var(--bg-secondary); border-radius: 6px; font-size: 0.82rem;",
#     sprintf(
#       "λ_home = %.2f  |  λ_away = %.2f  |  ρ = %.4f  |  Model: Bivariate Poisson (Holgate 1964)",
#       sim$score$mu_home, sim$score$mu_away, sim$score$rho
#     )
#   )
# })
# 
# # =========================================================================
# # RENDER: Simulation result probabilities bar
# # =========================================================================
# output$sim_result_bar <- renderUI({
#   sim <- tryCatch(simulation_results(), error = function(e) NULL)
#   if (is.null(sim)) return(NULL)
#   
#   info <- rv$match_info
#   if (is.null(info)) return(NULL)
#   
#   home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
#   away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
#   
#   rp <- sim$score$result_probs
#   
#   div(
#     style = "margin-bottom: 0.5rem;",
#     # Result probability bar (from simulation, not raw input)
#     div(
#       style = "display: flex; height: 32px; border-radius: 6px; overflow: hidden; border: 2px solid #333;",
#       div(style = sprintf("width: %.1f%%; background: %s; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.8rem; font-weight: 700;",
#                           rp$home_win, APP_COLORS$sage),
#           sprintf("%s %.1f%%", home_name, rp$home_win)),
#       div(style = sprintf("width: %.1f%%; background: #78909C; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.8rem; font-weight: 700;",
#                           rp$draw),
#           sprintf("Draw %.1f%%", rp$draw)),
#       div(style = sprintf("width: %.1f%%; background: %s; display: flex; align-items: center; justify-content: center; color: white; font-size: 0.8rem; font-weight: 700;",
#                           rp$away_win, APP_COLORS$coral),
#           sprintf("%s %.1f%%", away_name, rp$away_win))
#     ),
#     div(
#       style = "text-align: center; font-size: 0.75rem; color: var(--text-muted); margin-top: 0.25rem;",
#       "Simulated result probabilities (from bivariate Poisson)"
#     )
#   )
# })
# 
# # =========================================================================
# # HELPER: Render HTML heatmap grid
# # =========================================================================
# render_html_matrix <- function(sim_data, home_label, away_label, pct_fmt = "%.1f%%") {
#   mat <- sim_data$matrix
#   labels <- sim_data$row_labels
#   n <- length(labels)
#   
#   # Color scale: white -> light red -> deep red
#   # Based on probability value (higher = darker)
#   max_prob <- max(mat)
#   
#   cell_color <- function(prob) {
#     if (is.na(prob) || prob <= 0) return("background: #FAFAFA; color: #CCC;")
#     intensity <- min(1, prob / max(max_prob, 0.01))
#     # Interpolate: white (255,255,255) -> deep red (180, 40, 40)
#     r <- round(255 - intensity * 75)
#     g <- round(255 - intensity * 215)
#     b <- round(255 - intensity * 215)
#     text_color <- if (intensity > 0.4) "white" else "#333"
#     sprintf("background: rgb(%d,%d,%d); color: %s;", r, g, b, text_color)
#   }
#   
#   # Build table
#   # Header row: away team goals
#   header_cells <- paste0(
#     '<th style="padding: 0.4rem 0.6rem; text-align: center; font-weight: 700; font-size: 0.78rem; border-bottom: 2px solid #333;">',
#     labels, '</th>', collapse = "\n"
#   )
#   header_row <- paste0(
#     '<tr><th style="padding: 0.4rem; border-bottom: 2px solid #333; border-right: 2px solid #333;"></th>\n',
#     header_cells, '</tr>'
#   )
#   
#   # Data rows
#   data_rows <- sapply(1:n, function(i) {
#     row_cells <- sapply(1:n, function(j) {
#       prob <- mat[i, j]
#       style <- cell_color(prob)
#       sprintf(
#         '<td style="%s padding: 0.5rem 0.4rem; text-align: center; font-size: 0.82rem; font-weight: 500; min-width: 55px; border: 1px solid rgba(0,0,0,0.05);">%s</td>',
#         style, sprintf(pct_fmt, prob * 100)
#       )
#     })
#     paste0(
#       sprintf('<tr><td style="padding: 0.4rem 0.6rem; text-align: center; font-weight: 700; font-size: 0.78rem; border-right: 2px solid #333;">%s</td>\n', labels[i]),
#       paste(row_cells, collapse = "\n"),
#       '</tr>'
#     )
#   })
#   
#   # Away label (top)
#   away_header <- sprintf(
#     '<div style="text-align: center; font-weight: 700; font-size: 0.85rem; margin-bottom: 0.25rem; color: %s;">%s Goals Scored →</div>',
#     APP_COLORS$coral, away_label
#   )
#   
#   table_html <- sprintf(
#     '%s<div style="display: flex; align-items: flex-start; gap: 0.25rem;"><div style="writing-mode: vertical-rl; transform: rotate(180deg); font-weight: 700; font-size: 0.85rem; text-align: center; color: %s; padding-right: 0.25rem;">← %s Goals Scored</div><table style="border-collapse: collapse; border: 2px solid #333; border-radius: 6px; overflow: hidden;">%s\n%s</table></div>',
#     away_header,
#     APP_COLORS$sage, home_label,
#     header_row, paste(data_rows, collapse = "\n")
#   )
#   
#   HTML(table_html)
# }
# 
# # =========================================================================
# # HELPER: Render ggplot2 heatmap
# # =========================================================================
# render_ggplot_matrix <- function(sim_data, home_label, away_label, 
#                                   value_label = "Goals", fill_label = "Probability") {
#   mat <- sim_data$matrix
#   labels <- sim_data$row_labels
#   n <- length(labels)
#   
#   # Convert to long format for ggplot
#   df <- expand.grid(row = 1:n, col = 1:n)
#   df$prob <- as.vector(mat)
#   df$row_label <- factor(labels[df$row], levels = rev(labels))
#   df$col_label <- factor(labels[df$col], levels = labels)
#   df$label_text <- sprintf("%.1f%%", df$prob * 100)
#   
#   ggplot(df, aes(x = col_label, y = row_label, fill = prob)) +
#     geom_tile(color = "white", linewidth = 1.5) +
#     geom_text(aes(label = label_text),
#               size = 3.5, fontface = "bold",
#               color = ifelse(df$prob > max(df$prob) * 0.4, "white", "#333")) +
#     scale_fill_gradient(low = "#FFF5F5", high = "#B71C1C",
#                         labels = scales::percent,
#                         name = fill_label) +
#     labs(
#       x = sprintf("%s %s →", away_label, value_label),
#       y = sprintf("← %s %s", home_label, value_label)
#     ) +
#     theme_minimal(base_family = "Fjalla One") +
#     theme(
#       axis.text = element_text(size = 11, face = "bold"),
#       axis.title = element_text(size = 11, face = "bold"),
#       axis.title.y = element_text(angle = 90),
#       panel.grid = element_blank(),
#       legend.position = "none",
#       plot.margin = margin(10, 10, 10, 10)
#     )
# }
# 
# # =========================================================================
# # RENDER: Score matrix (HTML)
# # =========================================================================
# output$score_matrix_html <- renderUI({
#   sim <- tryCatch(simulation_results(), error = function(e) NULL)
#   if (is.null(sim)) return(div(style = "text-align: center; color: var(--text-muted); padding: 2rem;", 
#                                 "Load a match and adjust your view to generate simulation"))
#   
#   info <- rv$match_info
#   if (is.null(info)) return(NULL)
#   
#   home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
#   away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
#   
#   render_html_matrix(sim$score, home_name, away_name)
# })
# 
# # =========================================================================
# # RENDER: SoT matrix (HTML)
# # =========================================================================
# output$sot_matrix_html <- renderUI({
#   sim <- tryCatch(simulation_results(), error = function(e) NULL)
#   if (is.null(sim)) return(div(style = "text-align: center; color: var(--text-muted); padding: 2rem;", 
#                                 "Load a match and adjust your view to generate simulation"))
#   
#   info <- rv$match_info
#   if (is.null(info)) return(NULL)
#   
#   home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
#   away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
#   
#   render_html_matrix(sim$sot, home_name, away_name)
# })
# 
# # =========================================================================
# # RENDER: Score matrix (ggplot)
# # =========================================================================
# output$score_matrix_plot <- renderPlot({
#   sim <- tryCatch(simulation_results(), error = function(e) NULL)
#   if (is.null(sim)) return(NULL)
#   
#   info <- rv$match_info
#   if (is.null(info)) return(NULL)
#   
#   home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
#   away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
#   
#   render_ggplot_matrix(sim$score, home_name, away_name, "Goals", "P(Score)")
# }, bg = "transparent")
# 
# # =========================================================================
# # RENDER: SoT matrix (ggplot)
# # =========================================================================
# output$sot_matrix_plot <- renderPlot({
#   sim <- tryCatch(simulation_results(), error = function(e) NULL)
#   if (is.null(sim)) return(NULL)
#   
#   info <- rv$match_info
#   if (is.null(info)) return(NULL)
#   
#   home_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$home_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$home_abbrev], info$home_abbrev)
#   away_name <- ifelse(!is.na(TEAM_DISPLAY_NAMES[info$away_abbrev]),
#                       TEAM_DISPLAY_NAMES[info$away_abbrev], info$away_abbrev)
#   
#   render_ggplot_matrix(sim$sot, home_name, away_name, "SoT", "P(SoT)")
# }, bg = "transparent")