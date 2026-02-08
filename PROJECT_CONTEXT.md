# Sports Analytics Dashboard - Project Context

## Overview

Multi-sport fantasy analytics platform (Soccer, Golf, NFL, NHL, Formula 1) with "Stabilo Illustrated" design system. R Shiny with modular architecture. Soccer is the default/first sport.

---

## CRITICAL RULES

These rules are non-negotiable. Violating them creates inconsistency and technical debt.

### Theming Rules

| # | Rule |
|---|------|
| 1 | **ALWAYS use `app_reactable_theme()`** for reactable tables |
| 2 | **ALWAYS use `theme_app*()`** functions for ggplot charts |
| 3 | **ALWAYS use `APP_COLORS`** for color references in R code |
| 4 | **ALWAYS use centralized heatmap functions** from app_themes.R |

### CSS Rules

| # | Rule |
|---|------|
| 5 | **NEVER create new CSS** when existing styles.css classes can be reused |
| 6 | **NEVER use inline styles** when a CSS class exists |
| 7 | **ALWAYS check styles.css** before adding any styling |

### Shiny Pattern Rules

| # | Rule |
|---|------|
| 8 | **ALWAYS use `req()`** for input validation, NEVER `return()` |
| 9 | **ALWAYS use `session$onFlushed()`** when initializing dynamic module servers |
| 10 | **ALWAYS use selectize** (not native selects) for dropdowns |

### Debugging Rules

| # | Rule |
|---|------|
| 11 | **ALWAYS use `log_debug()`** for all significant operations |
| 12 | **NEVER create a module** without initialization logging |
| 13 | **ALWAYS log** reactive triggers, data loading, and filtering |
| 14 | **ALWAYS wrap risky operations** in `tryCatch()` with error logging |

### Architecture Rules

| # | Rule |
|---|------|
| 15 | **Source files in dependency order** as defined in global.R (see Source Order below) |
| 16 | **Use sport-specific optimizer files** for LP optimization, don't duplicate LP code |
| 17 | **Use nfl_ui_helpers.R** components for consistent UI across NFL modules |
| 18 | **Use centralized name normalization** functions, don't scatter inline fixes |

### Source Order (from global.R)

**Config and Utilities** (always first):
app_themes.R --> sports_config.R --> helpers.R --> player_headshots.R --> data_loader.R --> ui_value_box.R --> ui_card.R --> mod_combined_nav.R --> mod_page_container.R

**NFL:** nfl_config.R --> nfl_optimizer.R --> nfl_ui_helpers.R --> mod_nfl_projections.R --> mod_nfl_handbuild.R --> mod_nfl_showdown.R --> mod_nfl_ffpc_bestball.R --> mod_nfl_fanteam_playoffs.R

**Soccer:** soccer_config.R --> soccer_cache.R --> soccer_data_loader.R --> soccer_transforms.R --> soccer_fanteam_loader.R --> soccer_fanteam_matching.R --> soccer_shot_share.R --> mod_soccer_shot_share.R --> soccer_betting_config.R --> soccer_betting_data.R --> mod_soccer_betting.R --> mod_soccer_player_stats.R --> mod_soccer_matchups.R --> mod_soccer_handbuild.R

**NHL:** nhl_config.R --> mod_nhl_projections.R --> mod_nhl_handbuild.R

**Golf:** golf_optimizer.R --> mod_golf_classic.R --> mod_golf_showdown.R --> mod_golf_this_week.R --> mod_golf_season_long.R --> mod_golf_season_management.R

---

## Quick Reference Tables

### Available Theme Functions (app_themes.R)

| Function | Use For |
|----------|---------|
| `app_reactable_theme(compact)` | All reactable tables |
| `app_reactable_theme_minimal()` | Embedded/minimal tables |
| `theme_app()` | Base ggplot theme |
| `theme_app_timeseries()` | Time series charts |
| `theme_app_scatter()` | Scatter plots |
| `theme_app_bar()` | Horizontal bar charts |
| `get_sequential_heatmap_style(value, min, max)` | White --> Teal heatmaps |
| `get_diverging_heatmap_style(value, mid, min, max)` | Coral <-- White --> Teal |
| `get_diverging_heatmap_color(value, mid, min, max)` | Returns rgb() string only |
| `create_nfl_heatmap_styler(col_values, type, midpoint)` | Returns styler function for reactable |
| `get_heatmap_color(value, max_val, color)` | Single-color opacity heatmap |
| `create_heatmap_cell(value, show_heatmap, scale_factor)` | Styled heatmap div |
| `create_team_cell(team_name, logo_path)` | Logo + name flex row |
| `create_value_cell(value, format, higher_is_better)` | Color-coded value |

### Available CSS Classes (styles.css)

| Component | Base Class | Variants |
|-----------|------------|----------|
| Card | `.card` | `--teal`, `--coral`, `--sage`, `--yellow`, `--sky` |
| Value Box | `.value-box` | `--teal`, `--coral`, `--sage`, `--yellow`, `--sky` |
| Position Badge | `.position-badge` | `--sm`, `--lg`, `--xs` |
| Player Headshot | `.player-headshot` | `--sm`, `--md`, `--xs`, `--mini` |
| Adjustment Badge | `.adjustment-badge` | `--positive`, `--negative`, `--sm` |
| Lineup Slot | `.lineup-slot` | `--empty`, `--filled`, `--stacked` |
| Lineup Card | `.lineup-card` | `--compact` |
| Stat Box | `.stat-mini` | `__value--primary`, `__value--success`, `__value--info` |
| Tables | `.data-table` | `.projections-table`, `.draft-board-table` |
| Buttons | `.btn-refresh-subtle` | `.btn-target-highlight` |
| Match Card | `.match-card` | `.match-card__header`, `.match-card__controls` |
| Prob Bar | `.prob-bar` | `.prob-bar__segment.home`, `.draw`, `.away` |
| Number Stepper | `.number-stepper` | `.stepper-btn` |
| Results Grid | `.results-grid` | `.results-grid__col`, `__header`, `__row` |
| FFPC Player Card | `.ffpc-player-card` | `--target` |

> **Note on ui_card colors:** ui_card.R accepts "gold" and "frost" as preferred names (with "yellow" and "sky" as aliases). However, CSS only defines `.card--yellow` and `.card--sky`. When using ui_card(), pass `color = "yellow"` or `color = "sky"` to get actual colored headers. The "gold" and "frost" names generate classes without CSS rules.

### APP_COLORS Palette (R code)

| Key | Hex | Use |
|-----|-----|-----|
| `primary` | #3B3226 | Headings, outlines |
| `secondary` | #5C4E3D | Body text |
| `muted` | #7A7A7A | Captions |
| `sage` | #A3BE8C | Success, Soccer |
| `sage_dark` | #8FAF78 | Positive values |
| `coral` | #D08770 | CTAs, NFL |
| `coral_dark` | #BF7460 | Negative values |
| `gold` | #EBCB8B | Warnings, Golf |
| `frost` | #A8C5D4 | Info, NHL |
| `bg_primary` | #FAF8F5 | Page background |
| `bg_secondary` | #F5F0EB | Secondary surfaces |
| `bg_card` | #FFFFFF | Card backgrounds |
| `bg_tan` | #F9F7F4 | Striped rows |
| `border` | #E5E9F0 | Light borders |
| `border_dark` | #D8D0C4 | Heavy borders |
| `grid` | #E5E9F0 | Chart gridlines |
| `baseline` | #3B3226 | Chart baselines |
| `highlight` | #E8F0E8 | Row highlight |

### CSS Variables (use in CSS, not hex codes)

| Variable | Use |
|----------|-----|
| `var(--accent-teal)` | Primary accent (#8FBCBB) |
| `var(--accent-coral)` | CTAs, NFL (#D08770) |
| `var(--accent-sage)` | Success, Soccer (#A3BE8C) |
| `var(--accent-yellow)` | Warnings, Golf (#EBCB8B) |
| `var(--accent-sky)` | Info, NHL (#A8C5D4) |
| `var(--accent-red)` | Errors (#BF616A) |
| `var(--accent-plum)` | Correlations (#9A8A9E) |
| `var(--text-primary)` | Headings (#3B3226) |
| `var(--text-secondary)` | Body text (#5C4E3D) |
| `var(--text-muted)` | Captions (#7A7A7A) |
| `var(--bg-primary)` | Page background (#ECEFF4) |
| `var(--bg-secondary)` | Navbar (#E5E9F0) |
| `var(--bg-tertiary)` | Cards (#F5F0EB) |

---

## Key Functions by Sport

### NFL

| File | Function | Purpose |
|------|----------|---------|
| nfl_config.R | `correct_player_names(names)` | Apply name corrections |
| nfl_optimizer.R | `optimize_lineup_lp(players, col, cap, locked, excluded)` | Single lineup LP optimization |
| nfl_optimizer.R | `generate_lineups_with_variance(...)` | Multiple lineup generation |
| nfl_optimizer.R | `check_stacking_rules(lineup, players, rules, ...)` | Validate stacking constraints |
| nfl_ui_helpers.R | `create_position_badge(pos, size)` | Position badge HTML |
| nfl_ui_helpers.R | `create_player_cell(name, pos, team, ...)` | Player info cell |
| nfl_ui_helpers.R | `create_lineup_card(lineup, num, ...)` | Generated lineup card |
| data_loader.R | `get_available_slates(season, week)` | Detect main/late slates |
| mod_nfl_showdown.R | `get_available_showdown_slates(season, week)` | Detect showdown CSVs |
| mod_nfl_showdown.R | `NFL_SHOWDOWN_STRUCTURE` | CPT/VICE/FLEX format config |
| mod_nfl_fanteam_playoffs.R | `FANTEAM_PLAYOFFS_CONFIG` | Playoff contest config |
| mod_nfl_fanteam_playoffs.R | `get_available_playoff_rounds(season)` | Detect available rounds |

### Soccer

| File | Function | Purpose |
|------|----------|---------|
| soccer_config.R | `get_team_abbreviation(name)` | 3-letter abbreviation |
| soccer_config.R | `normalize_team_names(names)` | Normalize team name variants |
| soccer_config.R | `get_soccer_team_logo(name)` | Logo path lookup |
| soccer_config.R | `get_league_logo(league_name)` | League logo path |
| soccer_config.R | `simplify_position(position)` | GK/DEF/MID/FWD simplification |
| soccer_config.R | `get_position_color(position)` | Position badge color |
| soccer_cache.R | `is_cache_valid(path, hours)` | Check cache age |
| soccer_data_loader.R | `load_shot_data(refresh)` | Load shot-level data |
| soccer_data_loader.R | `get_league_teams(data, league)` | Teams for dropdown |
| soccer_transforms.R | `calculate_team_stats(...)` | Single team stats |
| soccer_transforms.R | `calculate_all_team_stats(...)` | League comparison |
| soccer_fanteam_loader.R | `load_fanteam_soccer_salaries(gameweek)` | Load FanTeam salary CSVs |
| soccer_fanteam_loader.R | `load_fanteam_soccer_with_logos(gameweek)` | Salaries with team logos |
| soccer_fanteam_matching.R | `normalize_player_name(name)` | Standardize player names |
| soccer_fanteam_matching.R | `match_fanteam_to_fbref(ft, fbref, gw)` | Cross-source name matching |
| soccer_shot_share.R | `load_shots_for_share(force_refresh)` | Load Understat shot data |
| soccer_betting_data.R | `scrape_betting_bbc_table(url, league)` | BBC league table scraping |
| mod_soccer_matchups.R | `FANTEAM_COEFFICIENTS` | 5-season regression coefficients |

> **CRITICAL: Team Name Normalization**
>
> Data sources use inconsistent team names (e.g., "Manchester Utd" vs "Manchester United").
> **All team name comparisons MUST use normalized names** to ensure dropdowns, logos, and
> highlighting work correctly. Use `normalize_team_names()` for teams and
> `normalize_player_name()` for players.

> **CRITICAL: Player Name Matching**
>
> FanTeam, FBref, and projection files all use different player name formats.
> **Always use `match_fanteam_to_fbref()`** for cross-source matching. The centralized
> system in `soccer_fanteam_matching.R` handles Unicode normalization, accent stripping,
> and manual corrections via Google Sheets.

### Golf

| File | Function | Purpose |
|------|----------|---------|
| golf_optimizer.R | `optimize_golf_classic_lp(players, ...)` | 6-golfer LP optimization |
| golf_optimizer.R | `generate_golf_classic_lineups(players, ...)` | Multi-lineup generation |
| golf_optimizer.R | `optimize_golf_showdown_lp(players, ...)` | Showdown with CPT/Underdog multipliers |
| golf_optimizer.R | `generate_golf_showdown_lineups(players, ...)` | Multi-lineup showdown generation |
| mod_golf_season_management.R | Transfer planning, roster tracking | 32+ gameweek season management |
| mod_golf_this_week.R | `get_this_week_tournaments()` | Load current week from Sheets |

### NHL

| File | Function | Purpose |
|------|----------|---------|
| nhl_config.R | Team info, scoring rules, lineup config | 724 lines of configuration |
| nhl_optimizer.R | `optimize_nhl_lineup_lp(players, ...)` | Single lineup LP optimization |
| nhl_optimizer.R | `generate_nhl_lineups_with_variance(...)` | Multi-lineup generation |
| mod_nhl_projections.R | File upload, name reconciliation | Projection file processing |
| mod_nhl_handbuild.R | Lineup builder with optimizer | Manual + optimized lineups |

> **Known Issue:** mod_nhl_handbuild.R contains a duplicate `optimize_nhl_lineup_lp()` function
> that should be removed in favor of the centralized version in nhl_optimizer.R.

---

## Pre-Code Checklist

Before writing ANY code, verify:

### Theming
- [ ] Using `app_reactable_theme()` for tables?
- [ ] Using `theme_app*()` for ggplot?
- [ ] Using `APP_COLORS` for colors?

### CSS
- [ ] Checked styles.css for existing classes?
- [ ] Using CSS class instead of inline style?
- [ ] New style consistent across all 5 sport modules?

### Shiny Patterns
- [ ] Using `req()` not `return()` for validation?
- [ ] Using `session$onFlushed()` for dynamic modules?
- [ ] Using selectize for dropdowns?

### Debugging
- [ ] Module init logged with separators?
- [ ] All observers/reactives log triggers?
- [ ] Data loading logs row counts?
- [ ] Errors wrapped in tryCatch with logging?

---

## Common Mistakes to Avoid

| Wrong | Correct |
|-------|---------|
| `return()` in observer | `req()` for validation |
| `selectize = FALSE` | Default selectize |
| Custom reactable theme | `app_reactable_theme()` |
| Inline style for badge size | `class = "position-badge--sm"` |
| Hardcoded hex in R | `APP_COLORS$coral` |
| Hardcoded hex in CSS | `var(--accent-coral)` |
| No logging in module | `log_debug()` at init + key points |
| New LP optimization code | Use sport-specific optimizer file |
| Inline name corrections | Use centralized normalization functions |
| `ui_card(color = "gold")` | `ui_card(color = "yellow")` (CSS match) |

---

## File Locations

| What | Where |
|------|-------|
| Theme functions | `app_themes.R` |
| CSS styles | `styles.css` |
| JavaScript | `app.js` |
| NFL logos | `www/nfl_logos/{TEAM}.webp` |
| Soccer logos | `www/soccer_logos/{League}/{Team}.svg` |
| Golf logos | `www/golf_logos/` |
| NHL logos | `www/nhl_logos/` |
| Sport nav icons | `www/images/{sport}.png` |
| NFL projections | `data/projections/{year}/week_*_projections.csv` |
| NFL showdown | `data/projections/{year}/showdown_*` |
| FanTeam salaries | `data/fanteam_salaries/{year}/` |
| Soccer cache | `data/cache/*.parquet` (local), Google Drive (remote) |
| Golf data | Google Sheets (via googlesheets4) |
| NHL projections | Uploaded CSV files |
| Detailed docs | `PROJECT_REFERENCE.md` |

---

## Data Sources

| Source | Used By | Method |
|--------|---------|--------|
| Google Sheets | Golf, Soccer config | googlesheets4 package |
| Google Drive | Soccer cache (Parquet) | googledrive package |
| FBref | Soccer player/team stats | fb_combined_scraper.R (offline) |
| Understat | Soccer shot data | understat.R + Python scraper |
| The Odds API | Soccer betting (17 leagues) | httr + jsonlite |
| BBC Sport | Soccer league tables | rvest scraping |
| NotOnlyFPL | FanTeam historical stats | scrape_notonlyfpl.R (chromote) |
| FanTeam exports | Player salaries (CSV) | Local file upload |
| nflreadr | NFL player data | nflreadr package |

---

## Adding New Features

### New Module Checklist
1. Follow naming: `mod_[sport]_[section].R`
2. Source in correct dependency order in global.R
3. Use centralized themes (app_themes.R)
4. Use existing CSS classes (styles.css)
5. Include comprehensive logging
6. Use `req()` for input validation
7. Use `session$onFlushed()` if dynamic
8. Add section to sports_config.R

### New CSS Checklist
1. Search styles.css for existing class first
2. If truly new, add with clear section comment
3. Follow BEM naming: `.component`, `.component--variant`
4. Use CSS variables, not hex codes
5. Consider usage across all 5 sports

---

## Reference

For detailed examples, code templates, architecture diagrams, and data pipeline documentation, see **PROJECT_REFERENCE.md**.