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
- **Palette**: Muted pastels (teal, sage, coral, yellow, sky)

### Typography
- **Primary Font**: Plus Jakarta Sans (Google Fonts)
- **Display Font**: Fjalla One (chart axis labels)
- **Weights**: 400 (body), 500 (medium), 600 (semibold), 700 (bold), 800 (display/nav)
- **Fallback**: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif

### Sport-Specific Colors

| Sport | Primary | Light | Icon File |
|-------|---------|-------|-----------|
| Soccer | #A3BE8C (sage) | #C5D4B8 | soccer.png |
| Golf | #EBCB8B (gold) | #F5E0B8 | golf.png |
| NFL | #D08770 (coral) | #E8B8A8 | nfl.png |
| NHL | #A8C5D4 (frost) | #A3C1D9 | ice_hockey.png |
| Formula 1 | #E5383B | #F2A3A5 | f1.png |

---

## Navigation Structure

### Level 1: Sports (order in nav)
Soccer (default), Golf, NFL, NHL, Formula 1

### Level 2: Sections

**Soccer**: Player Stats (default), Match Ups, Handbuild, Shot Share, Betting

**Golf**: This Week (default), Season Long (management), Classic, Showdown

**NFL**: FT Playoffs (default), FFPC Bestball, Handbuild, Showdown, Projections

**NHL**: Dashboard (placeholder), Projections (default), Handbuild

**Formula 1**: Dashboard (placeholder), Projections (placeholder)

### Navigation Implementation
- Combined two-tier navigation in `mod_combined_nav.R`
- Sport nav icons: 38x38px (most sports), 32x32px (Golf - taller icon)
- Labels: Weight 800, uppercase, 1.5px letter-spacing, 0.85rem
- States: Active = full opacity + colored underline, Inactive = 0.6 opacity
- Background: Full-width `--bg-secondary` (#E5E9F0), no borders or shadows

### Archived Sections (files exist but NOT sourced)
- Soccer: team_dashboard, player_dashboard (FBref data no longer available)
- Soccer: fanteam_contests (superseded by matchups)
- NFL: dashboard (placeholder, never completed)
- NHL: dashboard (placeholder only)
- Navigation: mod_sport_nav.R, mod_section_nav.R (replaced by mod_combined_nav.R)

---

## File Structure

### Active Files (sourced in global.R)

```
app.R                              # Main app entry point
global.R                           # Global setup, source order
styles.css                         # All CSS
app.js                             # JavaScript enhancements

R/config/
  app_themes.R                     # Centralized themes, colors, heatmaps
  sports_config.R                  # Sport definitions, section navigation

R/utils/
  helpers.R                        # log_debug(), formatting utilities
  data_loader.R                    # Load CSVs, get_available_slates()
  player_headshots.R               # NFL headshot URL generation

R/components/
  ui_value_box.R                   # Styled value box component
  ui_card.R                        # Styled card component

R/modules/
  mod_combined_nav.R               # Combined sport + section navigation
  mod_page_container.R             # Dynamic module loading container

R/nfl/
  nfl_config.R                     # Teams, names, lineup constants
  nfl_optimizer.R                  # LP optimization, stacking rules
  nfl_ui_helpers.R                 # Shared UI components (badges, cells, cards)
  mod_nfl_projections.R            # Projections analysis
  mod_nfl_handbuild.R              # Manual lineup builder
  mod_nfl_showdown.R               # Single-game showdown (CPT/VICE/FLEX)
  mod_nfl_ffpc_bestball.R          # FFPC bestball draft board
  mod_nfl_fanteam_playoffs.R       # FanTeam playoff contests

R/soccer/
  soccer_config.R                  # Teams, logos, abbreviations, leagues
  soccer_cache.R                   # Three-tier cache management
  soccer_data_loader.R             # Google Sheets + Parquet loading
  soccer_transforms.R              # Statistical calculations
  soccer_fanteam_loader.R          # FanTeam salary CSV processing
  soccer_fanteam_matching.R        # Cross-source player name matching
  soccer_shot_share.R              # Shot/xG share analysis (Understat)
  mod_soccer_shot_share.R          # Shot share UI module
  soccer_betting_config.R          # 17-league betting config, Odds API keys
  soccer_betting_data.R            # Odds API + BBC table scraping
  mod_soccer_betting.R             # Betting odds UI module
  mod_soccer_player_stats.R        # Historical player analysis
  mod_soccer_matchups.R            # Weekly fixtures + FanTeam projections
  mod_soccer_handbuild.R           # FanTeam lineup builder

R/nhl/
  nhl_config.R                     # Teams, scoring rules, lineup config
  nhl_optimizer.R                  # LP optimization (centralized)
  mod_nhl_projections.R            # File upload + name reconciliation
  mod_nhl_handbuild.R              # Lineup builder

R/golf/
  golf_optimizer.R                 # Classic + Showdown LP optimization
  mod_golf_classic.R               # Full tournament DFS
  mod_golf_showdown.R              # Single-day DFS
  mod_golf_this_week.R             # Weekly projections comparison
  mod_golf_season_long.R           # Underdog Scramble season
  mod_golf_season_management.R     # 32+ gameweek roster management

www/
  nfl_logos/                       # {TEAM}.webp, {TEAM}.png
  soccer_logos/{League}/           # Team SVGs by league
  golf_logos/                      # Tournament/tour logos
  nhl_logos/                       # NHL team logos
  images/                          # Sport nav icons
```

### Archived Files (NOT sourced, retained for reference)
```
R/soccer/mod_soccer_team_dashboard.R      # Archived: FBref unavailable
R/soccer/mod_soccer_player_dashboard.R    # Archived: FBref unavailable
R/soccer/mod_soccer_fanteam_contests.R    # Superseded by mod_soccer_matchups.R
R/modules/mod_sport_nav.R                 # Replaced by mod_combined_nav.R
R/modules/mod_section_nav.R               # Replaced by mod_combined_nav.R
R/nfl/mod_nfl_dashboard.R                 # Placeholder, never completed
R/nhl/mod_nhl_dashboard.R                 # Placeholder only
```

### Data Pipeline Scripts (run offline, not sourced by app)
```
R/scrapers/
  fb_combined_scraper.R            # FBref player stats, shots, goals
  scrape_football_odds.R           # 5-season historical odds collection
  scrape_notonlyfpl.R              # FanTeam statistics (chromote)
  scrape_understat_python.R        # Understat via reticulate/Python
  scrape_understat.R               # Understat batch processing
  understat.R                      # Core Understat R functions

R/analysis/
  fanteam_regression_analysis.R    # FOS weight calibration
  fanteam_shots_calibration.R      # Shots/SOT/goals regression

R/utils/
  upload_to_drive.R                # Parquet upload to Google Drive
```

### Data Directories
```
data/
  projections/{year}/              # NFL projection CSVs (week_*_projections.csv)
  projections/{year}/showdown_*/   # NFL showdown CSVs
  fanteam_salaries/{year}/         # FanTeam salary export CSVs
  cache/*.parquet                  # Local Parquet cache (soccer)
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

# Heatmap styler for reactable column
styler <- create_nfl_heatmap_styler(data$projection, type = "sequential")
colDef(style = function(value) list(styler(value)))
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

# Cards
ui_card(title = "Filters", color = "coral", ...)  # NFL
ui_card(title = "Filters", color = "sage", ...)    # Soccer
ui_card(title = "Filters", color = "yellow", ...)  # Golf (use "yellow" not "gold")
ui_card(title = "Filters", color = "sky", ...)     # NHL (use "sky" not "frost")
```

### UI Patterns - Remove/Cancel Buttons

**Use `actionButton` with `class = "btn-secondary"` for remove/cancel/delete buttons.**

```r
# CORRECT - Use actionButton with btn-secondary
actionButton(
  ns(paste0("remove_", item_id)),
  icon("times"),
  class = "btn-secondary",
  style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;",
  onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                     ns("remove_item"), key)
)
```

**Note**: The `onclick` with `return false;` prevents the default Shiny action button behavior while still triggering our custom input value.

### Rule Display Cards Pattern

For displaying applied rules (locks, excludes, grouped adjustments, correlations), use full-width card rows:

```r
div(
  style = "display: flex; align-items: center; padding: 0.4rem 0.6rem;
           background: white; border: 2px solid var(--accent-sage); border-radius: 6px;",
  
  # Badge (colored background)
  div(
    style = "background: var(--accent-sage); color: white; padding: 0.2rem 0.5rem;
             border-radius: 4px; font-size: 0.75rem; font-weight: 700; margin-right: 0.75rem;",
    icon("lock"), " LOCKED"
  ),
  
  # Content (flex: 1)
  div(style = "flex: 1; font-weight: 600; font-size: 0.9rem;", player_name),
  
  # Remove button (actionButton)
  actionButton(ns("remove_btn"), icon("times"), class = "btn-secondary",
               style = "padding: 0.2rem 0.4rem; min-width: auto; font-size: 0.7rem;")
)
```

Border colors by rule type: Lock = `var(--accent-sage)`, Exclude = `var(--accent-coral)`, Boost = `var(--accent-sage)`, Dock = `var(--accent-coral)`, Correlation = `var(--accent-plum)`

### req() vs return() Pattern

```r
# WRONG - Breaks reactive chain
observe({
  season <- input$season
  if (is.null(season) || season == "") return()
  # ... load data
})

# CORRECT - Maintains reactive dependency
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
    
    # Data loading with req() and retry
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

## NFL Architecture

### Handbuild Module
- **9-slot lineup**: QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX, DST
- **Projection adjustments**: Percentage-based boost/dock per player
- **Conditional stacking rules**: QB-specific team/opponent requirements
- **Game stack option**: Concentrate players from specific game
- **Lineup generation**: N lineups with variance, respecting rules

### Showdown Module (mod_nfl_showdown.R)
- **Single-game format**: CPT (1.5x), VICE (1.2x), FLEX slots
- **Structure defined in** `NFL_SHOWDOWN_STRUCTURE`
- **Salary cap**: 85 (configurable via `NFL_SHOWDOWN_SALARY_CAP`)
- **Slate detection**: `get_available_showdown_slates(season, week)` scans for showdown CSV files

### FanTeam Playoffs Module (mod_nfl_fanteam_playoffs.R)
- **Playoff contest optimization**: Config in `FANTEAM_PLAYOFFS_CONFIG`
- **Round detection**: `get_available_playoff_rounds(season)` finds available data
- **Roster slots and salary cap** defined in config

### LP Optimization Constraints (Main Slate)
- QB = 1, RB >= 2, WR >= 3, TE >= 1, DST = 1
- FLEX total >= 6 (RB + WR + TE)
- Total players = 9, Salary <= cap

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

---

## Soccer Architecture

### Active Data Flow

```
Google Sheets --> soccer_data_loader.R (with 3-tier caching) --> reactiveValues
                         |
              soccer_transforms.R (calculations)
                         |
        mod_soccer_player_stats.R  |  mod_soccer_matchups.R  |  mod_soccer_handbuild.R

Understat --> soccer_shot_share.R --> mod_soccer_shot_share.R

Odds API + BBC --> soccer_betting_data.R --> mod_soccer_betting.R

FanTeam CSVs --> soccer_fanteam_loader.R --> soccer_fanteam_matching.R --> matchups/handbuild
```

### Three-Tier Caching System

```
1. Local Parquet (data/cache/*.parquet)     -- fastest, checked first
2. Google Drive Parquet                      -- medium speed, downloaded if local missing
3. Google Sheets                             -- slowest, fallback when no cache exists
```

Implemented across `soccer_cache.R`, `soccer_data_loader.R`, `soccer_shot_share.R`.

```r
# Check if cache is valid (< max_age_hours old)
is_cache_valid(cache_path, max_age_hours = 6)

# Load with automatic caching (checks Parquet first, falls back to Sheets)
data <- load_shooting_summary(force_refresh = FALSE)
```

### Empirical Coefficients (mod_soccer_matchups.R)

The matchups module uses `FANTEAM_COEFFICIENTS` derived from 5-season regression analysis of historical match data. These predict shots, SOT, and goals from win percentages and over/under totals.

```r
FANTEAM_COEFFICIENTS <- list(
  home_shots = list(intercept = 3.9941, win_pct = 0.150385, total_goals = 1.1583),
  away_shots = list(intercept = 4.4395, win_pct = 0.152701, total_goals = 0.8070),
  home_sot   = list(intercept = 0.2532, win_pct = 0.051147, total_goals = 0.7963),
  away_sot   = list(intercept = 0.5083, win_pct = 0.060438, total_goals = 0.5695),
  home_goals = list(intercept = -1.3634, win_pct = 0.024723, draw_pct = 0.020101, total_goals = 0.4762),
  away_goals = list(intercept = -0.5941, win_pct = 0.025267, draw_pct = 0.004321, total_goals = 0.3474)
)
```

### FanTeam Fantasy Scoring Weights

Empirical calibration from fanteam_regression_analysis.R revealed significant differences from theoretical assumptions:

| Position | Theo CS Weight | Theo GF Weight | Empirical CS | Empirical GF |
|----------|---------------|----------------|--------------|--------------|
| GK | 0.85 | 0.15 | 0.32 | 0.68 |
| DEF | 0.70 | 0.30 | 0.45 | 0.55 |
| MID | 0.15 | 0.85 | 0.14 | 0.86 |
| FWD | 0.10 | 0.90 | 0.45 | 0.55 |

**Key insight**: Goalkeeper scoring is 68% goal-weighted (not 15% as theoretically assumed). Forward clean sheet correlation is much stronger than expected (45% vs 10%).

### Betting Module (17 Leagues)

Configured in `soccer_betting_config.R` with league definitions including Odds API slugs, BBC URLs, and display properties. Data fetched via:
- **The Odds API**: Live match odds (h2h, totals)
- **BBC Sport**: League tables scraped via rvest

---

## Golf Architecture

### Classic Module (mod_golf_classic.R)
- **6-golfer lineup**: LP optimization via `optimize_golf_classic_lp()`
- **Multi-lineup generation** with variance: `generate_golf_classic_lineups()`
- **Data source**: Google Sheets (via googlesheets4)
- **Card color**: `GOLF_CARD_COLOR <- "gold"` (note: should use "yellow" for CSS match)

### Showdown Module (mod_golf_showdown.R)
- **Captain/Underdog multipliers**: CPT gets 1.5x scoring
- **LP optimization**: `optimize_golf_showdown_lp()`
- **Multi-lineup generation**: `generate_golf_showdown_lineups()`

### This Week Module (mod_golf_this_week.R)
- **Projections comparison**: Multiple projection sources for current tournament
- **Data loading**: `get_this_week_tournaments()` from Google Sheets

### Season Management Module (mod_golf_season_management.R)
- **Largest module in project**: 2,252 lines
- **32+ gameweek tracking**: Roster management across full season
- **Transfer planning**: Budget and swap optimization
- **Performance analytics**: Historical scoring trends

---

## NHL Architecture

### Configuration (nhl_config.R)
- **724 lines**: Teams, scoring rules, projection config, lineup structure
- **Position system**: C, LW, RW, D, G with flex positions

### Projections Module (mod_nhl_projections.R)
- **File upload**: Users upload projection CSVs
- **Name reconciliation**: stringi-based Unicode normalization for player matching
- **Position mapping**: Standardizes various position formats

### Handbuild Module (mod_nhl_handbuild.R)
- **1,407 lines**: Full lineup builder with embedded optimizer
- **Known issue**: Contains duplicate `optimize_nhl_lineup_lp()` that should use nhl_optimizer.R

### LP Optimization (nhl_optimizer.R)
- `optimize_nhl_lineup_lp()`: Single lineup generation
- `generate_nhl_lineups_with_variance()`: Multi-lineup with randomized projections

---

## Data Pipeline (Offline Scripts)

These scripts run outside the app to populate data sources:

### FBref Scraper (fb_combined_scraper.R)
- **1,288 lines**: Comprehensive player stats, shots, goals
- **Output**: Google Sheets + Parquet files
- **Note**: Handles current-season URL format differences

### Football Odds Scraper (scrape_football_odds.R)
- **744 lines**: 5-season historical odds collection
- **Source**: The Odds API historical endpoints
- **Output**: Data for regression analysis

### NotOnlyFPL Scraper (scrape_notonlyfpl.R)
- **1,108 lines**: FanTeam statistics extraction
- **Method**: chromote headless browser
- **Output**: Historical FanTeam scoring data

### Understat Pipeline
- **understat.R** (1,028 lines): Core R functions for Understat data
- **scrape_understat_python.R** (751 lines): Python-based extraction via reticulate
- **scrape_understat.R** (481 lines): Batch processing wrapper

### Analysis Scripts
- **fanteam_regression_analysis.R** (491 lines): FOS weight calibration
- **fanteam_shots_calibration.R** (794 lines): Shots/SOT/goals regression producing FANTEAM_COEFFICIENTS

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
- Below midpoint: Coral (#D08770) --> White
- At midpoint: White
- Above midpoint: White --> Teal (#8FBCBB)

### Sequential (Projection/Salary)
- Low: White
- High: Teal Light (#A3D1D1)

---

## Required Packages

Loaded in global.R:

| Package | Purpose |
|---------|---------|
| shiny | Core framework |
| tidyverse | Data manipulation (dplyr, ggplot2, etc.) |
| janitor | Column name cleaning |
| nflreadr | NFL player data |
| lpSolve | Linear programming optimization |
| googlesheets4 | Google Sheets API |
| shinyWidgets | pickerInput, other enhanced widgets |
| reactable | Styled interactive tables |
| zoo | Rolling averages |
| ggrepel | Non-overlapping chart labels |
| stringi | String normalization (NHL names) |
| httr | HTTP requests (Odds API) |
| jsonlite | JSON parsing |
| rvest | HTML scraping (BBC) |
| glue | URL construction |

Additional (used by specific modules): `arrow` (Parquet), `googledrive` (Drive API), `reticulate` (Python interop), `chromote` (headless browser), `sysfonts`/`showtext` (Google Fonts in ggplot).

---

## Adding New Sports/Sections

### New Sport
1. Add config to `sports_config.R` (both `get_sports_config()` and `get_sections_config()`)
2. Create folder: `R/[sport]/`
3. Add icon: `www/images/[sport].png`
4. Create at least one module (e.g., dashboard or projections)
5. Source in global.R in correct position
6. Add color tokens to CSS if needed (`.card--[color]`, `.value-box--[color]`)

### New Section
1. Add section ID to sport's `sections` vector in `get_sports_config()`
2. Add section definition to `get_sections_config()`
3. Create module: `R/[sport]/mod_[sport]_[section].R`
4. Source in global.R after sport dependencies
5. Follow module template with logging

---

## Team Abbreviations

### NFL
ARI, ATL, BAL, BUF, CAR, CHI, CIN, CLE, DAL, DEN, DET, GB, HOU, IND, JAX, KC, LAC, LAR, LV, MIA, MIN, NE, NO, NYG, NYJ, PHI, PIT, SEA, SF, TB, TEN, WAS

### Soccer
Defined in `soccer_config.R` with `TEAM_ABBREVIATIONS` mapping. Covers Premier League, La Liga, Bundesliga, Serie A, Ligue 1, Championship, and additional leagues.

### NHL
Defined in `nhl_config.R` with full 32-team mapping.

---

## Known Issues

1. **UTF-8 encoding corruption**: Special characters corrupted through# =============================================================================
# Soccer Ratings Engine
# 
# Ownership estimation and player grading for FanTeam DFS contests
#
# Features:
#   - Ownership estimation from salary, form, matchup quality, position
#   - Cash grades (reliability + matchup + value)
#   - GPP grades (ceiling + leverage + matchup)
#   - Captain grades (ceiling + captain leverage)
#   - Calibration against historical ownership data (GW20+)
#
# FanTeam Monster Scoring Rules (encoded for reference):
#   ALL:  1 Appearance, 1 60+min, 3 Assist, -1 Yellow, -3 Red,
#         -2 Own Goal / Pen Miss / Caused Pen / Caused scoring FK
#   GK:   0.5/Save, 4 CS(60+min), -1/2 goals conceded, 8 Goal, 1 SOT, 5 Pen Save
#   DEF:  4 CS(60+min), -1/2 goals conceded, 6 Goal, 0.6 SOT
#   MID:  1 CS(60+min), 1 Full Match, 5 Goal, 0.4 SOT
#   FWD:  1 Full Match, 4 Goal, 0.4 SOT
#   Captain: 2x all points
#   NOTE: Positive/Negative Impact (+/-0.3) excluded from modelling.
#         Depends on team winning during player's time on pitch and cannot
#         be reliably projected. Flagged for future calibration.
#
# Data Sources:
#   - Google Sheet: Historical ownership (OWNERSHIP_SHEET_ID)
#   - FanTeam salaries (local CSV via soccer_fanteam_loader.R)
#   - Odds report (local CSV via mod_soccer_matchups.R load_fanteam_odds())
#   - Player stats (Google Sheets via mod_soccer_player_stats.R)
#
# Depends on: helpers.R, soccer_config.R, soccer_fanteam_loader.R
# Sourced AFTER: soccer_transforms.R, BEFORE: mod_soccer_handbuild.R
# =============================================================================

# =============================================================================
# CONSTANTS
# =============================================================================

# Google Sheet for historical ownership data
OWNERSHIP_SHEET_ID <- "1uI0wkZVLBL9hgMDfoszuZYDx-n9P03BJPZ4vP1KAMjY"

# Rating tier definitions (checked top-down: first match wins)
RATING_TIERS <- list(
  elite = list(
    min_score = 85,
    cash    = "Cash Lock",
    gpp     = "Smash Play",
    captain = "Crown Him",
    emoji_cash = "\U0001F512",
    emoji_gpp  = "\U0001F680",
    emoji_cpt  = "\U0001F451",
    color   = "#2E7D32",
    bg      = "#E8F5E9"
  ),
  strong = list(
    min_score = 70,
    cash    = "Strong Play",
    gpp     = "Leverage Gold",
    captain = "Top Captain",
    emoji_cash = "\U0001F4AA",
    emoji_gpp  = "\U0001F3AF",
    emoji_cpt  = "\u2B50",
    color   = "#1565C0",
    bg      = "#E3F2FD"
  ),
  good = list(
    min_score = 55,
    cash    = "Solid",
    gpp     = "Sneaky Good",
    captain = "Live Longshot",
    emoji_cash = "\u2705",
    emoji_gpp  = "\U0001F50D",
    emoji_cpt  = "\U0001F4A1",
    color   = "#6A1B9A",
    bg      = "#F3E5F5"
  ),
  neutral = list(
    min_score = 40,
    cash    = "Playable",
    gpp     = "Meh",
    captain = "Risky Captain",
    emoji_cash = "\u2796",
    emoji_gpp  = "\U0001F937",
    emoji_cpt  = "\u2796",
    color   = "#7A7A7A",
    bg      = "#F5F5F5"
  ),
  weak = list(
    min_score = 25,
    cash    = "Thin Ice",
    gpp     = "Chalk Trap",
    captain = "Fade Captain",
    emoji_cash = "\u26A0\uFE0F",
    emoji_gpp  = "\U0001F480",
    emoji_cpt  = "\u274C",
    color   = "#E65100",
    bg      = "#FFF3E0"
  ),
  avoid = list(
    min_score = 0,
    cash    = "Hard Pass",
    gpp     = "Dead Money",
    captain = "No",
    emoji_cash = "\u274C",
    emoji_gpp  = "\u274C",
    emoji_cpt  = "\u274C",
    color   = "#C62828",
    bg      = "#FFEBEE"
  )
)

# =============================================================================
# DATA LOADING - Historical Ownership
# =============================================================================

#' Load historical ownership data from Google Sheet
#' @return Combined data frame across all available week_* tabs, or NULL
load_ownership_history <- function() {
  log_debug("========================================", level = "INFO")
  log_debug("load_ownership_history() called", level = "INFO")
  
  tryCatch({
    googlesheets4::gs4_deauth()
    
    # Discover available week sheets
    sheet_meta <- googlesheets4::gs4_get(OWNERSHIP_SHEET_ID)
    all_sheets <- sheet_meta$sheets$name
    week_sheets <- all_sheets[grepl("^week_\\d+$", all_sheets, ignore.case = TRUE)]
    
    log_debug("Found ownership sheets:", paste(week_sheets, collapse = ", "), level = "INFO")
    
    if (length(week_sheets) == 0) {
      log_debug("No ownership week sheets found", level = "WARN")
      return(NULL)
    }
    
    # Load each sheet
    all_data <- lapply(week_sheets, function(sn) {
      tryCatch({
        gw_num <- as.integer(gsub("\\D", "", sn))
        
        raw <- googlesheets4::read_sheet(
          OWNERSHIP_SHEET_ID, sheet = sn, col_types = "c"
        ) %>% janitor::clean_names()
        
        log_debug(sprintf("  %s: %d rows, cols: %s",
                          sn, nrow(raw), paste(names(raw), collapse = ", ")), level = "DEBUG")
        
        # Standardize column names (handle C%, Own%, etc.)
        col_map <- c(
          "own" = "own_pct", "own_percent" = "own_pct",
          "c"   = "captain_pct", "c_percent" = "captain_pct"
        )
        for (old in names(col_map)) {
          new <- col_map[old]
          if (old %in% names(raw) && !new %in% names(raw)) {
            names(raw)[names(raw) == old] <- new
          }
        }
        
        # Parse helper
        parse_num <- function(x) {
          x <- gsub("[^0-9.\\-]", "", as.character(x))
          x[x == "" | is.na(x)] <- NA_character_
          as.numeric(x)
        }
        
        raw <- raw %>% mutate(
          gameweek    = gw_num,
          price       = parse_num(price),
          own_pct     = if ("own_pct" %in% names(.)) parse_num(own_pct) else NA_real_,
          captain_pct = if ("captain_pct" %in% names(.)) parse_num(captain_pct) else NA_real_,
          form        = parse_num(form),
          score       = parse_num(score),
          pos = case_when(
            toupper(pos) %in% c("GK", "GKP", "GOALKEEPER") ~ "GK",
            toupper(pos) %in% c("DEF", "DEFENDER", "D")    ~ "DEF",
            toupper(pos) %in% c("MID", "MIDFIELDER", "M")  ~ "MID",
            toupper(pos) %in% c("FWD", "FOR", "FORWARD", "F", "ATT") ~ "FWD",
            TRUE ~ toupper(pos)
          )
        )
        
        available <- intersect(
          c("player", "pos", "team", "price", "own_pct", "captain_pct",
            "form", "score", "gameweek"),
          names(raw)
        )
        raw %>% select(all_of(available))
        
      }, error = function(e) {
        log_debug(sprintf("  Error loading %s: %s", sn, e$message), level = "WARN")
        NULL
      })
    })
    
    result <- bind_rows(Filter(Negate(is.null), all_data))
    log_debug(sprintf("Combined ownership: %d rows, %d weeks",
                      nrow(result), length(unique(result$gameweek))), level = "INFO")
    log_debug("========================================", level = "INFO")
    return(result)
    
  }, error = function(e) {
    log_debug("Error in load_ownership_history():", e$message, level = "ERROR")
    NULL
  })
}


# =============================================================================
# MATCHUP QUALITY
# =============================================================================

#' Calculate position-specific matchup quality (0-100) per player
#'
#' What matters by position (driven by the scoring rules):
#'   GK:  CS probability (4pts) + save volume (0.5/save) - goals conceded
#'   DEF: CS probability (4pts) + team attack for set-piece goals (6pts) - conceded
#'   MID: Team implied goals (5pts/goal) + SOT (0.4/SOT) + small CS (1pt)
#'   FWD: Team implied goals (4pts/goal) + SOT (0.4/SOT)
#'
#' @param players Data frame with at least: pos, team_normalized
#' @param matchup_context Data frame per team: team_normalized, implied_goals,
#'   implied_opp_goals, cs_prob
#' @return Input data with matchup_quality column added
calculate_matchup_quality <- function(players, matchup_context) {
  if (is.null(matchup_context) || nrow(matchup_context) == 0) {
    return(players %>% mutate(matchup_quality = 50))
  }
  
  # Baselines (EPL season averages for calibration)
  avg_goals <- 1.35
  avg_cs    <- 28
  
  team_mq <- matchup_context %>%
    mutate(
      # 0-100 scales centered on EPL average
      atk_score  = pmin(100, pmax(0, 50 + (implied_goals - avg_goals) * 30)),
      cs_score   = pmin(100, pmax(0, 50 + (cs_prob - avg_cs) * 1.8)),
      opp_score  = pmin(100, pmax(0, 50 + (implied_opp_goals - avg_goals) * 25)),
      # Save opportunity: opponent attacking volume
      save_opp   = pmin(100, pmax(0, 50 + (implied_opp_goals - avg_goals) * 20)),
      # Concede penalty: how badly does conceding hurt?
      concede_risk = pmin(100, pmax(0, 50 - (implied_opp_goals - avg_goals) * 25))
    ) %>%
    select(team_normalized, atk_score, cs_score, opp_score, save_opp, concede_risk)
  
  if (!"team_normalized" %in% names(players)) {
    return(players %>% mutate(matchup_quality = 50))
  }
  
  players %>%
    left_join(team_mq, by = "team_normalized") %>%
    mutate(
      matchup_quality = case_when(
        # GK: CS + saves volume - concede risk
        # A GK facing a weak attack gets high CS; a GK facing a strong attack
        # gets saves but concede risk. Best: weak-moderate opponent.
        pos == "GK"  ~ 0.40 * coalesce(cs_score, 50) +
                        0.30 * coalesce(save_opp, 50) +
                        0.30 * coalesce(concede_risk, 50),
        # DEF: CS dominant + some attack upside (set pieces)
        pos == "DEF" ~ 0.55 * coalesce(cs_score, 50) +
                        0.15 * coalesce(atk_score, 50) +
                        0.30 * coalesce(concede_risk, 50),
        # MID: Attack dominant + small CS
        pos == "MID" ~ 0.65 * coalesce(atk_score, 50) +
                        0.10 * coalesce(cs_score, 50) +
                        0.25 * coalesce(opp_score, 50),
        # FWD: Pure attack
        pos == "FWD" ~ 0.80 * coalesce(atk_score, 50) +
                        0.20 * coalesce(opp_score, 50),
        TRUE ~ 50
      ),
      matchup_quality = round(pmin(100, pmax(0, matchup_quality)), 1)
    ) %>%
    select(-any_of(c("atk_score", "cs_score", "opp_score", "save_opp", "concede_risk")))
}


# =============================================================================
# OWNERSHIP ESTIMATION
# =============================================================================

#' Estimate ownership for a slate of players
#'
#' Heuristic model: salary tier + form + matchup + position -> ownership %
#' Calibrated against historical data when available.
#'
#' @param players Data frame with: player, pos, salary. Optional: form, matchup_quality
#' @param historical Output of load_ownership_history() for calibration, or NULL
#' @return Input df with added: est_own_pct, est_captain_pct, own_bucket
estimate_ownership <- function(players, historical = NULL) {
  log_debug("estimate_ownership():", nrow(players), "players", level = "INFO")
  
  if (nrow(players) == 0 || !"salary" %in% names(players)) {
    return(players %>% mutate(est_own_pct = 5, est_captain_pct = 0, own_bucket = "Unknown"))
  }
  
  result <- players %>%
    mutate(
      # --- Salary attractiveness (non-linear: top-priced get disproportionate attention) ---
      sal_min = min(salary, na.rm = TRUE),
      sal_max = max(salary, na.rm = TRUE),
      sal_norm = (salary - sal_min) / pmax(1, sal_max - sal_min),
      sal_appeal = sal_norm ^ 1.3 * 40,
      
      # --- Form attractiveness ---
      form_safe = coalesce(as.numeric(form), 0),
      form_norm = form_safe / pmax(1, max(form_safe, na.rm = TRUE)),
      form_appeal = form_norm * 25,
      
      # --- Matchup attractiveness ---
      mq_safe = coalesce(matchup_quality, 50),
      mq_appeal = (mq_safe / 100) * 15,
      
      # --- Position base (FWDs/MIDs more popular in FanTeam ownership pools) ---
      pos_appeal = case_when(
        pos == "GK"  ~ 3,
        pos == "DEF" ~ 6,
        pos == "MID" ~ 10,
        pos == "FWD" ~ 12,
        TRUE ~ 5
      ),
      
      # --- Combined raw score ---
      raw_appeal = sal_appeal + form_appeal + mq_appeal + pos_appeal
    )
  
  # Distribute ownership with realistic power-law skew
  n_players <- nrow(result)
  target_avg <- 7.5  # Average ~7.5% per player
  
  result <- result %>%
    arrange(desc(raw_appeal)) %>%
    mutate(
      rank = row_number(),
      # Power-law skew: top players get disproportionate ownership
      skew = (n_players / rank) ^ 0.85,
      weighted = raw_appeal * skew,
      share = weighted / sum(weighted, na.rm = TRUE),
      est_own_pct = round(share * n_players * target_avg, 1),
      est_own_pct = pmin(60, pmax(0.2, est_own_pct))
    )
  
  # Captain ownership: very concentrated on MID/FWD
  result <- result %>%
    mutate(
      cpt_appeal = case_when(
        pos == "GK"  ~ raw_appeal * 0.05,
        pos == "DEF" ~ raw_appeal * 0.25,
        pos == "MID" ~ raw_appeal * 1.5,
        pos == "FWD" ~ raw_appeal * 1.6,
        TRUE ~ raw_appeal * 0.5
      ),
      cpt_share = cpt_appeal / sum(cpt_appeal, na.rm = TRUE),
      est_captain_pct = round(pmin(50, pmax(0, cpt_share * 100)), 1)
    )
  
  # Calibrate against historical if available
  if (!is.null(historical) && nrow(historical) > 0) {
    hist_bench <- tryCatch({
      historical %>%
        filter(!is.na(own_pct), own_pct > 0) %>%
        mutate(
          sal_bucket = cut(price, breaks = c(0, 6, 8, 10, 12, 15, Inf),
                           labels = c("budget", "low", "mid", "mid_high", "premium", "elite"))
        ) %>%
        group_by(pos, sal_bucket) %>%
        summarise(avg_own = mean(own_pct, na.rm = TRUE), .groups = "drop")
    }, error = function(e) NULL)
    
    if (!is.null(hist_bench) && nrow(hist_bench) > 0) {
      log_debug("Calibrating ownership against", nrow(hist_bench), "historical buckets", level = "INFO")
      result <- result %>%
        mutate(sal_bucket = cut(salary, breaks = c(0, 6, 8, 10, 12, 15, Inf),
                                labels = c("budget", "low", "mid", "mid_high", "premium", "elite"))) %>%
        left_join(hist_bench, by = c("pos", "sal_bucket")) %>%
        mutate(
          est_own_pct = if_else(!is.na(avg_own),
                                round(0.55 * est_own_pct + 0.45 * avg_own, 1),
                                est_own_pct)
        ) %>%
        select(-sal_bucket, -avg_own)
    }
  }
  
  # Assign buckets
  result <- result %>%
    mutate(
      own_bucket = case_when(
        est_own_pct >= 20 ~ "Chalk",
        est_own_pct >= 8  ~ "Popular",
        est_own_pct >= 2  ~ "Moderate",
        est_own_pct >= 0.5 ~ "Low",
        TRUE ~ "Ghost"
      )
    ) %>%
    select(-any_of(c(
      "sal_min", "sal_max", "sal_norm", "sal_appeal",
      "form_safe", "form_norm", "form_appeal",
      "mq_safe", "mq_appeal", "pos_appeal", "raw_appeal",
      "rank", "skew", "weighted", "share",
      "cpt_appeal", "cpt_share"
    )))
  
  log_debug(sprintf("Ownership: Chalk=%d, Popular=%d, Moderate=%d, Low=%d, Ghost=%d",
                    sum(result$own_bucket == "Chalk", na.rm = TRUE),
                    sum(result$own_bucket == "Popular", na.rm = TRUE),
                    sum(result$own_bucket == "Moderate", na.rm = TRUE),
                    sum(result$own_bucket == "Low", na.rm = TRUE),
                    sum(result$own_bucket == "Ghost", na.rm = TRUE)), level = "INFO")
  return(result)
}


# =============================================================================
# PLAYER RATINGS
# =============================================================================

#' Rate all players for Cash, GPP, and Captain
#'
#' Combines historical performance, matchup quality, salary value, form,
#' and ownership leverage into three composite scores, mapped to word grades.
#'
#' @param players Data frame with columns from salary loader + ownership + matchup.
#'   Required: player, pos, salary, est_own_pct, matchup_quality
#'   Optional: ppg, sortino, pts_floor, pts_ceiling, form, n_games, est_captain_pct
#' @return Input df with: cash_score, cash_rating, cash_color, cash_bg,
#'   gpp_score, gpp_rating, ..., captain_score, captain_rating, ...
rate_players <- function(players) {
  log_debug("rate_players():", nrow(players), "players", level = "INFO")
  
  # Ensure necessary columns exist with safe defaults
  result <- players %>%
    mutate(
      ppg_val     = coalesce(as.numeric(ppg), 0),
      sortino_val = coalesce(as.numeric(sortino), 0),
      floor_val   = coalesce(as.numeric(pts_floor), 0),
      ceil_val    = coalesce(as.numeric(pts_ceiling), 0),
      form_val    = coalesce(as.numeric(form), ppg_val),
      n_games_val = coalesce(as.numeric(n_games), 0),
      mq_val      = coalesce(matchup_quality, 50),
      own_val     = coalesce(est_own_pct, 5),
      cpt_own_val = coalesce(est_captain_pct, 0),
      # Confidence: shrink grades toward 50 for thin sample sizes
      conf        = pmin(1, n_games_val / 10)
    )
  
  # Position-relative percentiles
  result <- result %>%
    group_by(pos) %>%
    mutate(
      p_ppg     = percent_rank(ppg_val) * 100,
      p_sortino = percent_rank(sortino_val) * 100,
      p_floor   = percent_rank(floor_val) * 100,
      p_ceiling = percent_rank(ceil_val) * 100,
      p_form    = percent_rank(form_val) * 100,
      p_matchup = percent_rank(mq_val) * 100,
      p_value   = percent_rank(ppg_val / pmax(salary, 3)) * 100
    ) %>%
    ungroup()
  
  # ---- CASH SCORE ----
  # Cash = floor reliability + matchup quality + salary value + form
  # Position-aware weighting:
  #   GK/DEF: Matchup is dominant (CS probability is huge, 4pts)
  #   MID: Balanced (floor matters, matchup moderate, form important)
  #   FWD: Value + form dominant (no CS, goal + SOT driven)
  result <- result %>%
    mutate(
      cash_raw = case_when(
        pos == "GK"  ~ 0.25 * p_floor + 0.15 * p_sortino + 0.30 * p_matchup + 0.15 * p_value + 0.15 * p_form,
        pos == "DEF" ~ 0.25 * p_floor + 0.15 * p_sortino + 0.30 * p_matchup + 0.15 * p_value + 0.15 * p_form,
        pos == "MID" ~ 0.25 * p_floor + 0.15 * p_sortino + 0.25 * p_matchup + 0.15 * p_value + 0.20 * p_form,
        pos == "FWD" ~ 0.20 * p_floor + 0.15 * p_sortino + 0.20 * p_matchup + 0.20 * p_value + 0.25 * p_form,
        TRUE         ~ 0.25 * p_floor + 0.15 * p_sortino + 0.25 * p_matchup + 0.15 * p_value + 0.20 * p_form
      ),
      cash_score = round(pmin(100, pmax(0, conf * cash_raw + (1 - conf) * 50)), 1)
    )
  
  # ---- GPP SCORE ----
  # GPP = ceiling + leverage (good projection but low owned) + matchup + form
  # Leverage is THE key GPP concept: high projection rank vs low ownership rank
  result <- result %>%
    group_by(pos) %>%
    mutate(
      p_own = percent_rank(own_val) * 100,
      # Leverage: projection quality minus ownership pressure
      # High quality + low owned = positive leverage (Leverage Gold)
      # Low quality + high owned = negative leverage (Chalk Trap)
      leverage_raw = (0.50 * p_ceiling + 0.50 * p_form) - p_own,
      p_leverage = percent_rank(leverage_raw) * 100
    ) %>%
    ungroup() %>%
    mutate(
      gpp_raw = 0.25 * p_ceiling + 0.30 * p_leverage + 0.25 * p_matchup + 0.20 * p_form,
      gpp_score = round(pmin(100, pmax(0, conf * gpp_raw + (1 - conf) * 50)), 1)
    )
  
  # ---- CAPTAIN SCORE ----
  # Captain = ceiling (2x magnifies upside) + captain pool leverage + matchup
  # GKs are almost never good captains (capped upside, CS max 8pts before 2x)
  # Best captains: high-ceiling MID/FWD in good matchup with low captain ownership
  result <- result %>%
    group_by(pos) %>%
    mutate(
      p_cpt_own = percent_rank(cpt_own_val) * 100,
      cpt_leverage_raw = (0.60 * p_ceiling + 0.40 * p_form) - p_cpt_own,
      p_cpt_leverage = percent_rank(cpt_leverage_raw) * 100
    ) %>%
    ungroup() %>%
    mutate(
      # Position modifier: GKs penalized, DEFs moderate, MID/FWD natural captains
      pos_mod = case_when(
        pos == "GK"  ~ -20,
        pos == "DEF" ~ -5,
        pos == "MID" ~ 5,
        pos == "FWD" ~ 8,
        TRUE ~ 0
      ),
      cpt_raw = 0.35 * p_ceiling + 0.30 * p_cpt_leverage + 0.20 * p_matchup + 0.15 * p_form + pos_mod,
      captain_score = round(pmin(100, pmax(0, conf * cpt_raw + (1 - conf) * 35)), 1)
    )
  
  # ---- MAP TO LABELS + COLORS ----
  result <- result %>%
    mutate(
      cash_rating   = map_score_to_label(cash_score, "cash"),
      cash_color    = map_score_to_color(cash_score),
      cash_bg       = map_score_to_bg(cash_score),
      gpp_rating    = map_score_to_label(gpp_score, "gpp"),
      gpp_color     = map_score_to_color(gpp_score),
      gpp_bg        = map_score_to_bg(gpp_score),
      captain_rating = map_score_to_label(captain_score, "captain"),
      captain_color  = map_score_to_color(captain_score),
      captain_bg     = map_score_to_bg(captain_score)
    )
  
  # Clean working columns
  result <- result %>%
    select(-any_of(c(
      "ppg_val", "sortino_val", "floor_val", "ceil_val", "form_val",
      "n_games_val", "mq_val", "own_val", "cpt_own_val", "conf",
      "p_ppg", "p_sortino", "p_floor", "p_ceiling", "p_form",
      "p_matchup", "p_value", "p_own", "p_cpt_own",
      "leverage_raw", "p_leverage", "cpt_leverage_raw", "p_cpt_leverage",
      "pos_mod", "cash_raw", "gpp_raw", "cpt_raw"
    )))
  
  # Log distribution
  for (type in c("cash_rating", "gpp_rating", "captain_rating")) {
    dist <- table(result[[type]])
    log_debug(sprintf("  %s: %s", type,
                      paste(names(dist), dist, sep = "=", collapse = ", ")), level = "DEBUG")
  }
  
  return(result)
}


# =============================================================================
# LABEL / COLOR MAPPING HELPERS
# =============================================================================

#' Map score to word label
#' @param score Numeric vector (0-100)
#' @param type "cash", "gpp", or "captain"
map_score_to_label <- function(score, type = "cash") {
  sapply(score, function(s) {
    if (is.na(s)) return(NA_character_)
    for (tier in RATING_TIERS) {
      if (s >= tier$min_score) return(tier[[type]])
    }
    RATING_TIERS$avoid[[type]]
  })
}

#' Map score to hex color
map_score_to_color <- function(score) {
  sapply(score, function(s) {
    if (is.na(s)) return("#7A7A7A")
    for (tier in RATING_TIERS) {
      if (s >= tier$min_score) return(tier$color)
    }
    RATING_TIERS$avoid$color
  })
}

#' Map score to hex background color
map_score_to_bg <- function(score) {
  sapply(score, function(s) {
    if (is.na(s)) return("#F5F5F5")
    for (tier in RATING_TIERS) {
      if (s >= tier$min_score) return(tier$bg)
    }
    RATING_TIERS$avoid$bg
  })
}

#' Get emoji for a rating type and score
get_rating_emoji <- function(score, type = "cash") {
  emoji_key <- paste0("emoji_", switch(type, cash = "cash", gpp = "gpp", captain = "cpt", "cash"))
  sapply(score, function(s) {
    if (is.na(s)) return("")
    for (tier in RATING_TIERS) {
      if (s >= tier$min_score) return(tier[[emoji_key]])
    }
    RATING_TIERS$avoid[[emoji_key]]
  })
}


# =============================================================================
# RATING GUIDE HTML (for UI display in handbuild module)
# =============================================================================

#' Generate the rating methodology guide as HTML
#' @return shiny tagList suitable for rendering inside a ui_card
generate_rating_guide_html <- function() {
  
  tier_rows <- lapply(RATING_TIERS, function(tier) {
    tags$tr(
      style = sprintf("background: %s;", tier$bg),
      tags$td(style = "padding: 0.35rem 0.5rem; text-align: center; font-weight: 600; font-size: 0.75rem; color: var(--text-muted);",
              sprintf("%d+", tier$min_score)),
      tags$td(style = sprintf("padding: 0.35rem 0.5rem; font-weight: 700; font-size: 0.8rem; color: %s; white-space: nowrap;", tier$color),
              paste(tier$emoji_cash, tier$cash)),
      tags$td(style = sprintf("padding: 0.35rem 0.5rem; font-weight: 700; font-size: 0.8rem; color: %s; white-space: nowrap;", tier$color),
              paste(tier$emoji_gpp, tier$gpp)),
      tags$td(style = sprintf("padding: 0.35rem 0.5rem; font-weight: 700; font-size: 0.8rem; color: %s; white-space: nowrap;", tier$color),
              paste(tier$emoji_cpt, tier$captain))
    )
  })
  
  tagList(
    # Grade table
    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 2px solid var(--outline); border-radius: 6px; overflow: hidden;",
      tags$thead(
        tags$tr(
          style = "background: var(--text-primary); color: white;",
          tags$th(style = "padding: 0.4rem 0.5rem; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.5px;", "Score"),
          tags$th(style = "padding: 0.4rem 0.5rem; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.5px;", "Cash"),
          tags$th(style = "padding: 0.4rem 0.5rem; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.5px;", "GPP"),
          tags$th(style = "padding: 0.4rem 0.5rem; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.5px;", "Captain")
        )
      ),
      tags$tbody(tier_rows)
    ),
    
    # Methodology explanation
    tags$div(
      style = "margin-top: 0.75rem; padding: 0.6rem; background: var(--bg-secondary); border-radius: 6px;",
      tags$p(
        style = "font-size: 0.8rem; color: var(--text-secondary); margin: 0 0 0.5rem 0; line-height: 1.5;",
        tags$strong("How ratings work:"),
        " Each player gets a composite score (0-100) for Cash, GPP, and Captain suitability. ",
        "Scores combine historical performance (floor, ceiling, Sortino ratio), ",
        "this week's matchup quality (odds-derived), salary value, current form, ",
        "and estimated ownership (for GPP leverage). All grades are ", tags$strong("position-relative"),
        ": a 'Cash Lock' GK is elite among GKs."
      ),
      tags$p(
        style = "font-size: 0.75rem; color: var(--text-muted); margin: 0 0 0.3rem 0;",
        tags$strong("Cash:"), " Floor + Sortino (reliability) + matchup quality + salary value. ",
        "GK/DEF matchup-heavy (CS is worth 4pts). MID/FWD more form-driven."
      ),
      tags$p(
        style = "font-size: 0.75rem; color: var(--text-muted); margin: 0 0 0.3rem 0;",
        tags$strong("GPP:"), " Ceiling upside + leverage (good projection but low-owned) + matchup. ",
        "Rewards contrarian picks with genuine ceiling. Penalises chalk traps."
      ),
      tags$p(
        style = "font-size: 0.75rem; color: var(--text-muted); margin: 0 0 0.3rem 0;",
        tags$strong("Captain (2x):"), " Ceiling (magnified by 2x multiplier) + captain-pool leverage + matchup. ",
        "GKs heavily penalised (capped upside). MID/FWD natural captain picks."
      ),
      tags$p(
        style = "font-size: 0.75rem; color: var(--text-muted); margin: 0; font-style: italic;",
        "Ownership estimates calibrated against GW20+ actuals. Target accuracy: \u00B110pp. ",
        "Positive/Negative Impact scoring (\u00B10.3) excluded from analysis."
      )
    )
  )
}out codebase (e.g., arrows, checkmarks, R-squared symbols). Cosmetic only, does not affect functionality.

2. **Golf card color mismatch**: `GOLF_CARD_COLOR <- "gold"` generates CSS class `.card--gold` which has no CSS rule. Should use "yellow" for visible colored headers.

3. **NHL duplicate optimizer**: `mod_nhl_handbuild.R` contains its own `optimize_nhl_lineup_lp()` instead of using the centralized version in `nhl_optimizer.R`.

4. **worldfootballR package**: Broken/non-functional. Do not use as a data source. Use custom scrapers instead.