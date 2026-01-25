# =============================================================================
# Global Setup
# 
# Loads all modules, components, and configuration
# =============================================================================

cat("\n")
cat("========================================\n")
cat("SPORTS ANALYTICS APP - STARTUP\n")
cat("========================================\n")
cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Working directory:", getwd(), "\n")
cat("========================================\n\n")

# Load packages
cat("[STARTUP] Loading packages...\n")
library(shiny)
library(tidyverse)
library(janitor)
library(nflreadr)
library(lpSolve)
library(googlesheets4)
library(shinyWidgets)  # For pickerInput
library(reactable)     # For styled tables
library(zoo)           # For rolling averages
library(ggrepel)       # For chart labels
library(stringi)       # For string normalization (NHL name matching)
library(httr)          # For Odds API calls
library(jsonlite)      # For JSON parsing
library(rvest)         # For BBC scraping
library(glue)          # For URL construction
cat("[STARTUP] Packages loaded successfully\n\n")

# Load fonts for ggplot
cat("[STARTUP] Loading fonts...\n")
tryCatch({
  if (requireNamespace("sysfonts", quietly = TRUE) && 
      requireNamespace("showtext", quietly = TRUE)) {
    # Add Google Fonts
    sysfonts::font_add_google("Plus Jakarta Sans", "Plus Jakarta Sans")
    sysfonts::font_add_google("Fjalla One", "Fjalla One")
    showtext::showtext_auto()
    cat("[STARTUP] Fonts loaded: Plus Jakarta Sans, Fjalla One\n")
  } else {
    cat("[STARTUP] Font packages not available, using system fonts\n")
  }
}, error = function(e) {
  cat("[STARTUP] Font loading skipped:", e$message, "\n")
})

# -----------------------------------------------------------------------------
# Source Configuration
# -----------------------------------------------------------------------------

cat("[STARTUP] Loading configuration...\n")
source("R/config/app_themes.R")      # Centralized theme configuration (must be first)
source("R/config/sports_config.R")   # Uses APP_COLORS from app_themes.R
cat("[STARTUP] Configuration loaded\n")

# -----------------------------------------------------------------------------
# Source Utilities
# -----------------------------------------------------------------------------

cat("[STARTUP] Loading utilities...\n")
source("R/utils/helpers.R")
source("R/utils/player_headshots.R")
source("R/utils/data_loader.R")
cat("[STARTUP] Utilities loaded\n")

# Quick test: Check if data directory exists
cat("[STARTUP] Checking data directory structure...\n")

# Try new structure first
if (dir.exists("data/projections")) {
  cat("[STARTUP]   Found: data/projections/\n")
  proj_contents <- list.dirs("data/projections", recursive = FALSE, full.names = FALSE)
  cat("[STARTUP]   Year folders:", paste(proj_contents, collapse = ", "), "\n")
  if (length(proj_contents) > 0) {
    # Check first year folder for files
    first_year <- proj_contents[1]
    week_files <- list.files(paste0("data/projections/", first_year), pattern = "week_.*\\.csv")
    cat("[STARTUP]   Files in", first_year, ":", length(week_files), "projection files\n")
  }
} else if (dir.exists("projections")) {
  cat("[STARTUP]   Found: projections/ (flat structure)\n")
  week_files <- list.files("projections", pattern = "week_.*_projections\\.csv")
  cat("[STARTUP]   Projection files found:", length(week_files), "\n")
} else {
  cat("[STARTUP]   WARNING: No projection directory found!\n")
  cat("[STARTUP]   Looking for 'data/projections/' or 'projections/'\n")
  cat("[STARTUP]   Contents of working directory:\n")
  cat("[STARTUP]   ", paste(list.files("."), collapse = ", "), "\n")
}

# Check salary files
if (dir.exists("data/fanteam_salaries")) {
  cat("[STARTUP]   Found: data/fanteam_salaries/\n")
} else if (dir.exists("fanteam_salaries")) {
  cat("[STARTUP]   Found: fanteam_salaries/ (flat structure)\n")
} else {
  cat("[STARTUP]   WARNING: No salary directory found!\n")
}
cat("\n")

# -----------------------------------------------------------------------------
# Source Components
# -----------------------------------------------------------------------------

cat("[STARTUP] Loading components...\n")
source("R/components/ui_value_box.R")
source("R/components/ui_card.R")
cat("[STARTUP] Components loaded\n")

# -----------------------------------------------------------------------------
# Source Core Modules
# -----------------------------------------------------------------------------

cat("[STARTUP] Loading core modules...\n")
source("R/modules/mod_combined_nav.R")    # Combined two-tier navigation
source("R/modules/mod_page_container.R")
cat("[STARTUP] Core modules loaded\n")

# -----------------------------------------------------------------------------
# Source Sport Modules
# -----------------------------------------------------------------------------

cat("[STARTUP] Loading sport modules...\n")

# NFL (ORDER MATTERS - dependencies must be sourced first)
cat("[STARTUP]   Loading NFL modules...\n")
source("R/nfl/nfl_config.R")              # Team info, name corrections, lineup constants
cat("[STARTUP]     nfl_config.R loaded\n")
source("R/nfl/nfl_optimizer.R")           # LP optimization, stacking rules (depends on helpers.R)
cat("[STARTUP]     nfl_optimizer.R loaded\n")
source("R/nfl/nfl_ui_helpers.R")          # Shared UI components (depends on helpers.R)
cat("[STARTUP]     nfl_ui_helpers.R loaded\n")
source("R/nfl/mod_nfl_projections.R")     # Projections module
cat("[STARTUP]     mod_nfl_projections.R loaded\n")
source("R/nfl/mod_nfl_handbuild.R")       # Handbuild module (uses optimizer + ui_helpers)
cat("[STARTUP]     mod_nfl_handbuild.R loaded\n")
source("R/nfl/mod_nfl_showdown.R")        # Showdown single-game module
cat("[STARTUP]     mod_nfl_showdown.R loaded\n")
source("R/nfl/mod_nfl_ffpc_bestball.R")   # FFPC Bestball module
cat("[STARTUP]     mod_nfl_ffpc_bestball.R loaded\n")
source("R/nfl/mod_nfl_fanteam_playoffs.R") # Fanteam Playoffs module
cat("[STARTUP]     mod_nfl_fanteam_playoffs.R loaded\n")
# source("R/nfl/mod_nfl_dashboard.R")     # Dashboard (placeholder)
cat("[STARTUP]   NFL modules loaded\n")

# Soccer (ORDER MATTERS - dependencies must be sourced first)
cat("[STARTUP]   Loading Soccer modules...\n")
source("R/soccer/soccer_config.R")         # Team mappings, logos, sheet IDs
cat("[STARTUP]     soccer_config.R loaded\n")
source("R/soccer/soccer_cache.R")          # Cache management
cat("[STARTUP]     soccer_cache.R loaded\n")
source("R/soccer/soccer_data_loader.R")    # Google Sheets loading
cat("[STARTUP]     soccer_data_loader.R loaded\n")
source("R/soccer/soccer_transforms.R")     # Calculation functions
cat("[STARTUP]     soccer_transforms.R loaded\n")
source("R/soccer/soccer_fanteam_loader.R") # FanTeam data loading
cat("[STARTUP]     soccer_fanteam_loader.R loaded\n")
source("R/soccer/soccer_fanteam_matching.R") # Player name matching
cat("[STARTUP]     soccer_fanteam_matching.R loaded\n")

# Betting module (new)
source("R/soccer/soccer_betting_config.R")  # Betting config, league definitions
cat("[STARTUP]     soccer_betting_config.R loaded\n")
source("R/soccer/soccer_betting_data.R")    # Odds API, BBC scraper, data prep
cat("[STARTUP]     soccer_betting_data.R loaded\n")
source("R/soccer/mod_soccer_betting.R")     # Betting odds module
cat("[STARTUP]     mod_soccer_betting.R loaded\n")

# ARCHIVED: Team and Player dashboards (FBref data no longer available)
# source("R/soccer/mod_soccer_team_dashboard.R")    # Team dashboard module
# source("R/soccer/mod_soccer_player_dashboard.R")  # Player dashboard module

source("R/soccer/mod_soccer_fanteam_contests.R")  # FanTeam contests module
cat("[STARTUP]     mod_soccer_fanteam_contests.R loaded\n")
source("R/soccer/mod_soccer_player_stats.R")     # Player stats module
cat("[STARTUP]     mod_soccer_player_stats.R loaded\n")
cat("[STARTUP]   Soccer modules loaded\n")

# NHL (Ice Hockey)
cat("[STARTUP]   Loading NHL modules...\n")
source("R/nhl/nhl_config.R")              # Team info, scoring rules, lineup config
cat("[STARTUP]     nhl_config.R loaded\n")
source("R/nhl/mod_nhl_projections.R")     # Projections module
cat("[STARTUP]     mod_nhl_projections.R loaded\n")
source("R/nhl/mod_nhl_handbuild.R")       # Handbuild/lineup builder module
cat("[STARTUP]     mod_nhl_handbuild.R loaded\n")
cat("[STARTUP]   NHL modules loaded\n")

# Formula 1 - uses placeholder_ui (no module yet)
# source("R/f1/mod_f1_dashboard.R")

# Golf
cat("[STARTUP]   Loading Golf modules...\n")
# Note: golf_config.R removed - was duplicate of sports_config.R
source("R/golf/golf_optimizer.R")          # LP optimization
cat("[STARTUP]     golf_optimizer.R loaded\n")
source("R/golf/mod_golf_classic.R")        # Classic (full tournament) module
cat("[STARTUP]     mod_golf_classic.R loaded\n")
source("R/golf/mod_golf_showdown.R")       # Showdown (single day) module
cat("[STARTUP]     mod_golf_showdown.R loaded\n")
source("R/golf/mod_golf_this_week.R")      # This Week (projections comparison) module
cat("[STARTUP]     mod_golf_this_week.R loaded\n")
source("R/golf/mod_golf_season_long.R")    # Season long (Underdog Scramble) module
cat("[STARTUP]     mod_golf_season_long.R loaded\n")
source("R/golf/mod_golf_season_management.R")  # Season long management module
cat("[STARTUP]     mod_golf_season_management.R loaded\n")
cat("[STARTUP]   Golf modules loaded\n")

cat("[STARTUP] Sport modules loaded\n")

# -----------------------------------------------------------------------------
# Startup Complete
# -----------------------------------------------------------------------------
cat("\n")
cat("========================================\n")
cat("STARTUP COMPLETE\n")
cat("========================================\n\n")