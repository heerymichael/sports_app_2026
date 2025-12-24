# Sports Analytics Dashboard - Project Context

## Overview

Multi-sport fantasy analytics platform (NFL, Soccer, Golf) with "Stabilo Illustrated" design system. R Shiny with modular architecture.

---

## âš ï¸ CRITICAL RULES

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
| 15 | **Source NFL files in order**: config â†’ optimizer â†’ ui_helpers â†’ modules |
| 16 | **Source Soccer files in order**: config â†’ cache â†’ loader â†’ transforms â†’ modules |
| 17 | **Use nfl_optimizer.R** for lineup optimization, don't duplicate LP code |
| 18 | **Use nfl_ui_helpers.R** components for consistent UI across NFL modules |

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
| `get_sequential_heatmap_style(value, min, max)` | White â†’ Teal heatmaps |
| `get_diverging_heatmap_style(value, mid, min, max)` | Coral â† White â†’ Teal |

### Available CSS Classes (styles.css)

| Component | Base Class | Variants |
|-----------|------------|----------|
| Position Badge | `.position-badge` | `--sm`, `--lg`, `--xs` |
| Player Headshot | `.player-headshot` | `--sm`, `--md`, `--xs`, `--mini` |
| Adjustment Badge | `.adjustment-badge` | `--positive`, `--negative` |
| Lineup Slot | `.lineup-slot` | `--empty`, `--filled`, `--stacked` |
| Lineup Card | `.lineup-card` | `--compact` |
| Stat Box | `.stat-mini` | `__value--primary`, `__value--success` |
| Tables | `.data-table` | `.projections-table`, `.draft-board-table` |
| Buttons | `.btn-refresh-subtle` | â€” |

### APP_COLORS Palette (R code)

| Key | Hex | Use |
|-----|-----|-----|
| `primary` | #3B3226 | Headings, outlines |
| `secondary` | #5C4E3D | Body text |
| `muted` | #7A7A7A | Captions |
| `sage` | #A3BE8C | Success, Soccer |
| `coral` | #D08770 | CTAs, NFL |
| `gold` | #EBCB8B | Warnings, Golf |
| `frost` | #A8C5D4 | Info, links |
| `bg_primary` | #FAF8F5 | Page background |
| `bg_tan` | #F9F7F4 | Striped rows |

### CSS Variables (use in CSS, not hex codes)

| Variable | Use |
|----------|-----|
| `var(--accent-teal)` | Primary accent |
| `var(--accent-coral)` | CTAs, NFL |
| `var(--accent-sage)` | Success, Soccer |
| `var(--text-primary)` | Headings |
| `var(--bg-tertiary)` | Cards |

### NFL Key Functions

| File | Function | Purpose |
|------|----------|---------|
| nfl_config.R | `correct_player_names(names)` | Apply name corrections |
| nfl_optimizer.R | `optimize_lineup_lp(players, col, cap, locked, excluded)` | Single lineup optimization |
| nfl_optimizer.R | `generate_lineups_with_variance(...)` | Multiple lineup generation |
| nfl_optimizer.R | `check_stacking_rules(lineup, players, rules, ...)` | Validate stacking |
| nfl_ui_helpers.R | `create_position_badge(pos, size)` | Position badge |
| nfl_ui_helpers.R | `create_player_cell(name, pos, team, ...)` | Player info cell |
| nfl_ui_helpers.R | `create_lineup_card(lineup, num, ...)` | Generated lineup card |

### Soccer Key Functions

| File | Function | Purpose |
|------|----------|---------|
| soccer_config.R | `get_team_abbreviation(name)` | 3-letter abbreviation |
| soccer_config.R | `normalize_team_names(names)` | Normalize variants |
| soccer_config.R | `get_soccer_team_logo(name)` | Logo path |
| soccer_cache.R | `is_cache_valid(path, hours)` | Check cache validity |
| soccer_data_loader.R | `load_shot_data(refresh)` | Load shot-level data |
| soccer_data_loader.R | `get_league_teams(data, league)` | Get teams for dropdown |
| soccer_transforms.R | `calculate_team_stats(...)` | Single team stats |
| soccer_transforms.R | `calculate_all_team_stats(...)` | League comparison |

> **CRITICAL: Team Name Normalization**
> 
> FBref data contains inconsistent team names (e.g., "Manchester Utd" vs "Manchester United").
> **All team name comparisons MUST use normalized names** to ensure:
> - Dropdown selections match stats calculations
> - Team logos display correctly
> - Highlighting works in charts
> 
> Key pattern: `get_league_teams()` normalizes names before populating dropdowns,
> `calculate_all_team_stats()` uses normalized names internally, and
> `get_soccer_team_logo()` normalizes before lookup. This ensures consistency.

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
- [ ] New style consistent with NFL AND Soccer modules?

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

| âŒ Wrong | âœ… Correct |
|----------|-----------|
| `return()` in observer | `req()` for validation |
| `selectize = FALSE` | Default selectize |
| Custom reactable theme | `app_reactable_theme()` |
| Inline style for badge size | `class = "position-badge--sm"` |
| Hardcoded hex in R | `APP_COLORS$coral` |
| Hardcoded hex in CSS | `var(--accent-coral)` |
| No logging in module | `log_debug()` at init + key points |
| New LP optimization code | Use `nfl_optimizer.R` functions |

---

## File Locations

| What | Where |
|------|-------|
| Theme functions | `app_themes.R` |
| CSS styles | `styles.css` |
| NFL logos | `www/nfl_logos/{TEAM}.webp` |
| Soccer logos | `www/soccer_logos/{League}/{Team}.svg` |
| Projections data | `data/projections/{year}/week_*_projections.csv` |
| Soccer cache | `data/cache/*.rds` |
| Detailed docs | `PROJECT_REFERENCE.md` |

---

## Adding New Features

### New Module Checklist
1. Follow naming: `mod_[sport]_[section].R`
2. Source in correct dependency order
3. Use centralized themes (app_themes.R)
4. Use existing CSS classes (styles.css)
5. Include comprehensive logging
6. Use `req()` for input validation
7. Use `session$onFlushed()` if dynamic

### New CSS Checklist
1. Search styles.css for existing class first
2. If truly new, add with clear section comment
3. Follow BEM naming: `.component`, `.component--variant`
4. Use CSS variables, not hex codes
5. Consider both NFL and Soccer usage

---

## Reference

For detailed examples, code templates, and architecture diagrams, see **PROJECT_REFERENCE.md**.