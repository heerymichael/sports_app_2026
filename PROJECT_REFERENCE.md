# Sports Analytics Dashboard - Reference Documentation

This document contains detailed examples, templates, and architecture documentation. For critical rules and quick reference, see **PROJECT_CONTEXT.md**.

---

## Design System Details

### Visual Style: "Stabilo Illustrated"
- **Aesthetic**: Flat vector illustration style inspired by Stabilo highlighter packaging
- **Background**: Off-white/cream (#ECEFF4)
- **Outlines**: Thick dark brown borders (#3B3226, 2-3px)
- **Shadows**: Simple offset drop shadows (no blur)
- **Corners**: Rounded geometric shapes
- **Palette**: Muted pastels (teal, sage, coral, yellow)

### Typography
- **Primary Font**: Plus Jakarta Sans (Google Fonts)
- **Weights**: 400 (body), 500 (medium), 600 (semibold), 700 (bold), 800 (display/nav)

### Sport-Specific Colors

| Sport  | Primary   | Light     | Icon File   |
|--------|-----------|-----------|-------------|
| NFL    | #D08770   | #E8B8A8   | nfl.png     |
| Soccer | #A3BE8C   | #C5D4B8   | soccer.png  |
| Golf   | #EBCB8B   | #F5E0B8   | golf.png    |

---

## Navigation Structure

### Level 1: Sports
- NFL, Soccer, Golf (Future: NBA, MLB, etc.)

### Level 2: Sections

**NFL**: Projections (default), Handbuild, Optimiser, FFPC Bestball

**Soccer**: Team Dashboard (default), Player Dashboard, Projections, Performance

**Golf**: Dashboard, Optimizer, Projections

### Header Styling
- **Background**: Full-width `--bg-secondary` (#E5E9F0)
- **No borders or shadows**
- **Sport nav icons**: NFL/Soccer 38Ã—38px, Golf 32Ã—32px
- **Labels**: Weight 800, uppercase, 1.5px letter-spacing, 0.85rem
- **States**: Active full opacity + colored underline, Inactive 0.6 opacity

---

## File Structure

```
sports_analytics/
â”œâ”€â”€ app.R                        # Main app entry point
â”œâ”€â”€ global.R                     # Global setup, source order
â”œâ”€â”€ app_themes.R                 # Centralized themes
â”œâ”€â”€ styles.css                   # All CSS
â”œâ”€â”€ app.js                       # JavaScript enhancements
â”‚
â”œâ”€â”€ R/
â”‚   â”œâ”€â”€ config/sports_config.R   # Sport definitions
â”‚   â”œâ”€â”€ utils/
â”‚   â”‚   â”œâ”€â”€ helpers.R            # log_debug(), formatting
â”‚   â”‚   â”œâ”€â”€ data_loader.R        # Load CSVs
â”‚   â”‚   â””â”€â”€ player_headshots.R   # NFL headshots
â”‚   â”œâ”€â”€ modules/
â”‚   â”‚   â”œâ”€â”€ mod_sport_nav.R
â”‚   â”‚   â”œâ”€â”€ mod_section_nav.R
â”‚   â”‚   â””â”€â”€ mod_page_container.R
â”‚   â”œâ”€â”€ components/
â”‚   â”‚   â”œâ”€â”€ ui_value_box.R
â”‚   â”‚   â””â”€â”€ ui_card.R
â”‚   â”œâ”€â”€ nfl/
â”‚   â”‚   â”œâ”€â”€ nfl_config.R
â”‚   â”‚   â”œâ”€â”€ nfl_optimizer.R
â”‚   â”‚   â”œâ”€â”€ nfl_ui_helpers.R
â”‚   â”‚   â””â”€â”€ mod_nfl_*.R
â”‚   â””â”€â”€ soccer/
â”‚       â”œâ”€â”€ soccer_config.R
â”‚       â”œâ”€â”€ soccer_cache.R
â”‚       â”œâ”€â”€ soccer_data_loader.R
â”‚       â”œâ”€â”€ soccer_transforms.R
â”‚       â””â”€â”€ mod_soccer_*.R
â”‚
â”œâ”€â”€ data/
â”‚   â”œâ”€â”€ cache/                   # Soccer RDS cache
â”‚   â”œâ”€â”€ projections/2025/
â”‚   â””â”€â”€ fanteam_salaries/2025/
â”‚
â””â”€â”€ www/
    â”œâ”€â”€ nfl_logos/               # {TEAM}.webp, {TEAM}.png
    â””â”€â”€ soccer_logos/{League}/   # Team SVGs
```

---

## Code Examples

### Theme Usage

```r
# Reactable with theme
reactable(
  data,
  theme = app_reactable_theme(),
  striped = TRUE,
  columns = list(...)
)

# ggplot with theme
ggplot(data, aes(x, y)) +
  geom_line(color = APP_COLORS$sage) +
  theme_app_timeseries()

# Heatmap styling
style = get_diverging_heatmap_style(value, midpoint = 1.0, min_val, max_val)
style = get_sequential_heatmap_style(value, min_val, max_val)
```

### CSS Class Usage

```r
# Position badges
tags$span(class = "position-badge position-badge--sm", "QB")

# Headshots
div(class = "player-headshot player-headshot--sm",
    style = sprintf("background-color: %s;", team_color),
    tags$img(src = headshot_url))

# Buttons
tags$button(class = "btn btn-refresh-subtle", "Refresh")
```

### req() vs return() Pattern

```r
# âŒ WRONG - Breaks reactive chain
observe({
  season <- input$season
  if (is.null(season) || season == "") return()
  # ... load data
})

# âœ… CORRECT - Maintains reactive dependency
observe({
  season <- input$season
  week <- input$week
  
  req(season, week)
  req(season != "", week != "")
  
  rv$player_data <- load_data(season, week)
})
```

### Dynamic Module Loading

```r
# In mod_page_container.R
observeEvent(list(selected_sport(), selected_section()), {
  # ... get module info ...
  
  session$onFlushed(function() {
    log_debug(">>> [onFlushed] Initializing server for:", module_id, level = "INFO")
    server_fn(ns(module_id))
  }, once = TRUE)
})
```

### Retry Mechanism for Input Timing

```r
load_attempts <- reactiveVal(0)

observe({
  season <- input$season
  
  if ((is.null(season) || season == "") && load_attempts() < 10) {
    load_attempts(load_attempts() + 1)
    invalidateLater(200, session)
    return()
  }
  
  load_attempts(0)
  req(season)
  # ... proceed
})
```

### Selectize with Team Logos

```r
selectizeInput(ns("team"), "Team",
  choices = c("All Teams" = "all"),
  options = list(
    render = I("{
      option: function(item, escape) {
        if (item.value === 'all') return '<div class=\"option\">' + escape(item.label) + '</div>';
        return '<div class=\"option\" style=\"display: flex; align-items: center; gap: 8px;\">' +
          '<img src=\"nfl_logos/' + escape(item.value) + '.webp\" style=\"width: 24px; height: 24px;\">' +
          '<span>' + escape(item.label) + '</span></div>';
      }
    }")
  )
)
```

### Dropdown Component Choice: selectizeInput vs pickerInput

**These are different libraries with different CSS requirements.**

| Component | Library | Use Case | CSS Section |
|-----------|---------|----------|-------------|
| `selectizeInput` | selectize.js | Single selection, custom rendering (logos) | `.selectize-*` |
| `pickerInput` | bootstrap-select | Multi-select (e.g., compare 2 teams) | `.bootstrap-select` |

**Why this matters:**
- They render completely different HTML structures
- Checkmarks, carets, and spacing need separate CSS rules
- Styling one does NOT affect the other

**Current usage:**
- **NFL**: `selectizeInput` for team filter (single select with logos)
- **Soccer Team Dashboard**: `pickerInput` for team comparison (multi-select, max 2)

**pickerInput with Team Logos (Soccer):**
```r
shinyWidgets::pickerInput(ns("team"), "Team(s)",
  choices = teams,
  selected = default_team,
  multiple = TRUE,
  options = shinyWidgets::pickerOptions(
    maxOptions = 2,
    noneSelectedText = "Select team(s)"
  )
)

# Update with logo content:
shinyWidgets::updatePickerInput(session, "team",
  choices = teams,
  selected = default_team,
  choicesOpt = list(content = team_content)  # HTML with <img> tags
)
```

---

## Module Template

```r
# =============================================================================
# Module: [Sport] [Section]
# =============================================================================

[sport]_[section]_ui <- function(id) {
  ns <- NS(id)
  log_debug("[sport]_[section]_ui() called", level = "INFO")
  
  tagList(
    ui_card(
      title = "Filters", color = "coral",
      fluidRow(
        column(4, selectInput(ns("filter1"), "Filter 1", choices = NULL)),
        column(4, selectInput(ns("filter2"), "Filter 2", choices = NULL))
      )
    ),
    uiOutput(ns("main_content"))
  )
}

[sport]_[section]_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialization logging
    log_debug("========================================", level = "INFO")
    log_debug("[sport]_[section]_server() initialized", level = "INFO")
    log_debug("========================================", level = "INFO")
    
    rv <- reactiveValues(data = NULL, initialized = FALSE)
    
    # Data loading with req()
    load_attempts <- reactiveVal(0)
    
    observe({
      filter1 <- input$filter1
      
      log_debug(">>> Data load observer triggered", level = "DEBUG")
      
      if ((is.null(filter1) || filter1 == "") && load_attempts() < 10) {
        load_attempts(load_attempts() + 1)
        invalidateLater(200, session)
        return()
      }
      
      load_attempts(0)
      req(filter1)
      
      log_debug(">>> DATA LOAD TRIGGERED", level = "INFO")
      
      tryCatch({
        rv$data <- load_data(filter1)
        log_debug(">>> Data loaded:", nrow(rv$data), "rows", level = "INFO")
      }, error = function(e) {
        log_debug(">>> Error:", e$message, level = "ERROR")
      })
    })
    
    # Output with theme
    output$main_content <- renderUI({
      req(rv$data)
      reactable(rv$data, theme = app_reactable_theme(), striped = TRUE)
    })
  })
}
```

---

## NFL Handbuild Architecture

### Features
1. **9-slot lineup**: QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX, DST
2. **Projection adjustments**: Percentage-based boost/dock per player
3. **Conditional stacking rules**: QB-specific team/opponent requirements
4. **Game stack option**: Concentrate players from specific game
5. **Lineup generation**: N lineups with variance, respecting rules

### Reactive Values Structure

```r
rv <- reactiveValues(
  player_data = NULL,
  optimal_lineup = NULL,
  adjusted_optimal_lineup = NULL,
  lineup_slots = list(...),           # 9 slots
  generated_lineups = NULL,
  projection_adjustments = list(),    # "Player Name" = percentage
  stacking_rules = list(),
  available_games = NULL
)
```

### Stacking Rule Structure

```r
rule <- list(
  id = "rule_1_1234",
  qbs = c("Patrick Mahomes", "Josh Allen"),
  same_team_min = 2,
  same_team_positions = c("WR", "TE"),
  opp_min = 1,
  opp_positions = c("WR")
)
```

### LP Optimization Constraints
- QB = 1, RB >= 2, WR >= 3, TE >= 1, DST = 1
- FLEX total >= 6 (RB + WR + TE)
- Total players = 9
- Salary <= cap

---

## Soccer Module Architecture

### Data Flow

```
Google Sheets â†’ soccer_data_loader.R (with caching) â†’ reactiveValues
                         â†“
              soccer_transforms.R (calculations)
                         â†“
        mod_soccer_team_dashboard.R  |  mod_soccer_player_dashboard.R
```

### Cache System

```r
# Check if cache is valid (< max_age_hours old)
is_cache_valid(cache_path, max_age_hours = 6)

# Load with automatic caching
data <- load_shooting_summary(force_refresh = FALSE)
```

---

## CSS Z-Index Reference

```css
/* Row stacking for dropdowns */
.row:first-of-type { z-index: 2; }
.row:nth-of-type(2) { z-index: 1; }

/* Active dropdown highest */
.selectize-control.dropdown-active { z-index: 10001 !important; }
.selectize-dropdown { z-index: 10000 !important; }

/* Cards allow overflow */
.card, .card-body { overflow: visible !important; }
```

---

## Debugging Output Format

```
========================================
SPORTS ANALYTICS APP - STARTUP
========================================
[21:30:00] [INFO] Loading packages...
[21:30:01] [INFO] Checking data directory...

>>> [onFlushed] Initializing server for: nfl_projections
[21:30:02] [INFO] nfl_projections_server() initialized
[21:30:02] [DEBUG] >>> Data load observer triggered
[21:30:02] [DEBUG] >>>   season: NULL
[21:30:02] [DEBUG] >>> Inputs not ready, scheduling retry
[21:30:02] [DEBUG] >>> Data load observer triggered
[21:30:02] [DEBUG] >>>   season: '2025'
[21:30:02] [INFO] >>> DATA LOAD TRIGGERED
[21:30:03] [INFO] >>> Data loaded: 142 players
```

---

## Heatmap Color Scales

### Diverging (Value Column)
- Below midpoint: Coral (#D08770) â†’ White
- At midpoint: White
- Above midpoint: White â†’ Teal (#8FBCBB)

### Sequential (Projection/Salary)
- Low: White
- High: Teal Light (#A3D1D1)

---

## Adding New Sports/Sections

### New Sport
1. Add config to `sports_config.R`
2. Create folder: `R/[sport]/`
3. Add icon: `www/images/[sport].png`
4. Create dashboard module minimum
5. Add color tokens to CSS if needed

### New Section
1. Add to sport config in `sports_config.R`
2. Create module: `R/[sport]/mod_[sport]_[section].R`
3. Register in section nav logic
4. Follow module template with logging

---

## Team Abbreviations

### NFL
ARI, ATL, BAL, BUF, CAR, CHI, CIN, CLE, DAL, DEN, DET, GB, HOU, IND, JAX, KC, LAC, LAR, LV, MIA, MIN, NE, NO, NYG, NYJ, PHI, PIT, SEA, SF, TB, TEN, WAS

### Soccer
Defined in `soccer_config.R` with `TEAM_ABBREVIATIONS` mapping.