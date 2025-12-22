################################################################################
# FBref COMBINED SCRAPER - UNIFIED PLAYER MATCH STATS
# 
# Scrapes and combines player data into a single sheet:
#   - Player Summary + Possession combined at scrape time
#   - Shots in separate sheet (different grain)
#   - Team Goals in separate sheet (different grain)
#
# Target Sheets:
#   - Player_Match_Stats: Combined player-level data (new!)
#   - Shot_Data: Individual shot records
#   - Team_Goals: Match scores
################################################################################

library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(googlesheets4)
library(chromote)

################################################################################
# CONFIGURATION
################################################################################

RATE_LIMIT_DELAY <- 10
LEAGUE_DELAY <- 15
BATCH_SIZE <- 20  # Write to Google Sheets every 20 matches
SAVE_LOCAL_BACKUP <- TRUE
BACKUP_DIR <- "fbref_backups"

LEAGUES_TO_SCRAPE <- c("Premier-League", "Serie-A", "La-Liga", "Bundesliga", "Championship")
SEASON <- "2025-2026"

# NEW SHEET STRUCTURE
SHEETS_CONFIG <- list(
  # NEW: Combined player match stats (Summary + Possession)
  player_match_stats = list(
    sheet_id = "12MXPMsuI4S7EiTPnVpaqx5-riXoK37cEtPo-MFOf1fA",
    sheet_name = "Player_Match_Stats"
  ),
  # Keep shots separate (shot-level grain)
  shots = list(
    sheet_id = "1oxQ6rk_B_r2QUNZxGssmDEopKqJD0otMt_r1hPOufT0",
    sheet_name = "Shot_Data"
  ),
  # Keep team goals separate (match-level grain)
  team_goals = list(
    sheet_id = "1gUCVxBFR3kwE259ZccWBLRn7iAlV0HpZL7OccbGsWuo",
    sheet_name = "Team_Goals"
  )
)

LEAGUE_CODES <- c(
  "Premier-League" = "9",
  "La-Liga" = "12",
  "Bundesliga" = "20",
  "Serie-A" = "11",
  "Ligue-1" = "13",
  "Champions-League" = "8",
  "Europa-League" = "19",
  "Championship" = "10",
  "MLS" = "22",
  "Liga-MX" = "31",
  "Eredivisie" = "23",
  "Primeira-Liga" = "32"
)

################################################################################
# COLUMN SCHEMA FOR COMBINED PLAYER STATS
################################################################################

SCHEMA_PLAYER_MATCH_STATS <- c(
  # Identifiers
  "league", "season", "match_date", "gameweek",
  # Match info
  "home_team", "away_team", "venue",
  # Player info
  "team", "player", "player_href", "shirtnumber", "nationality", "position", "age",
  # Playing time
  "minutes",
  # Goals & Assists
  "goals", "assists", "pens_made", "pens_att",
  # Shooting
  "shots", "shots_on_target", "xg", "npxg",
  # Passing
  "passes_completed", "passes", "passes_pct", "progressive_passes", "xg_assist",
  # Shot creation
  "sca", "gca",
  # Defense
  "tackles", "interceptions", "blocks",
  # Cards
  "cards_yellow", "cards_red",
  # Touches (from Possession)
  "touches", "touches_def_pen_area", "touches_def_3rd", "touches_mid_3rd",
  "touches_att_3rd", "touches_att_pen_area", "touches_live_ball",
  # Take-ons (from Possession)
  "take_ons", "take_ons_won", "take_ons_won_pct", "take_ons_tackled", "take_ons_tackled_pct",
  # Carries (from Possession)
  "carries", "carries_distance", "carries_progressive_distance", "progressive_carries",
  "carries_into_final_third", "carries_into_penalty_area",
  # Ball control (from Possession)
  "miscontrols", "dispossessed", "passes_received", "progressive_passes_received",
  # Reference
  "match_url"
)

SCHEMA_SHOTS <- c(
  "league", "season", "match_date", "gameweek", "home_team", "away_team",
  "minute", "player", "squad", "outcome", "distance", "body_part",
  "notes", "sca_1_player", "sca_1_type", "sca_2_player", "sca_2_type",
  "xg", "psxg", "match_url"
)

SCHEMA_TEAM_GOALS <- c(
  "league", "season", "gameweek", "date", "home_team", "away_team",
  "home_goals", "away_goals", "venue", "match_url"
)

################################################################################
# SCHEMA ENFORCEMENT
################################################################################

enforce_schema <- function(df, schema) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  # Add missing columns
  for (col in schema) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  # Order columns: schema first, then extras alphabetically
  extra_cols <- sort(setdiff(names(df), schema))
  final_order <- c(intersect(schema, names(df)), extra_cols)
  
  df <- df %>% 
    select(all_of(final_order)) %>%
    mutate(across(everything(), as.character))
  
  return(df)
}

################################################################################
# CACHED URL LOADING
################################################################################

load_all_existing_urls <- function() {
  message("\n========================================")
  message("Loading existing match URLs...")
  message("========================================")
  
  cache <- list()
  
  # Player Match Stats (combined sheet with league column)
  player_data <- tryCatch({
    read_sheet(
      SHEETS_CONFIG$player_match_stats$sheet_id,
      sheet = SHEETS_CONFIG$player_match_stats$sheet_name,
      col_types = "c"
    )
  }, error = function(e) {
    message(sprintf("  Note: Could not read Player_Match_Stats: %s", e$message))
    data.frame()
  })
  
  if (nrow(player_data) > 0 && "match_url" %in% names(player_data)) {
    if ("league" %in% names(player_data)) {
      for (league in LEAGUES_TO_SCRAPE) {
        cache[[paste0("player_", league)]] <- unique(
          player_data$match_url[player_data$league == league]
        )
      }
    } else {
      all_urls <- unique(player_data$match_url)
      for (league in LEAGUES_TO_SCRAPE) {
        cache[[paste0("player_", league)]] <- all_urls
      }
    }
    message(sprintf("  Player Match Stats: %d total rows", nrow(player_data)))
  } else {
    message("  Player Match Stats: Empty or new sheet")
    for (league in LEAGUES_TO_SCRAPE) {
      cache[[paste0("player_", league)]] <- character(0)
    }
  }
  
  # Shots
  shots_data <- tryCatch({
    read_sheet(
      SHEETS_CONFIG$shots$sheet_id,
      sheet = SHEETS_CONFIG$shots$sheet_name,
      col_types = "c"
    )
  }, error = function(e) data.frame())
  
  if (nrow(shots_data) > 0 && "match_url" %in% names(shots_data)) {
    if ("league" %in% names(shots_data)) {
      for (league in LEAGUES_TO_SCRAPE) {
        cache[[paste0("shots_", league)]] <- unique(
          shots_data$match_url[shots_data$league == league]
        )
      }
    } else {
      all_urls <- unique(shots_data$match_url)
      for (league in LEAGUES_TO_SCRAPE) {
        cache[[paste0("shots_", league)]] <- all_urls
      }
    }
  }
  message(sprintf("  Shots: %d total rows", nrow(shots_data)))
  
  # Team Goals
  goals_data <- tryCatch({
    read_sheet(
      SHEETS_CONFIG$team_goals$sheet_id,
      sheet = SHEETS_CONFIG$team_goals$sheet_name,
      col_types = "c"
    )
  }, error = function(e) data.frame())
  
  if (nrow(goals_data) > 0 && "match_url" %in% names(goals_data)) {
    if ("league" %in% names(goals_data)) {
      for (league in LEAGUES_TO_SCRAPE) {
        cache[[paste0("goals_", league)]] <- unique(
          goals_data$match_url[goals_data$league == league]
        )
      }
    } else {
      all_urls <- unique(goals_data$match_url)
      for (league in LEAGUES_TO_SCRAPE) {
        cache[[paste0("goals_", league)]] <- all_urls
      }
    }
  }
  message(sprintf("  Team Goals: %d total rows", nrow(goals_data)))
  
  message("========================================\n")
  return(cache)
}

get_existing_urls_for_league <- function(league, cache) {
  player_urls <- cache[[paste0("player_", league)]] %||% character(0)
  shots_urls <- cache[[paste0("shots_", league)]] %||% character(0)
  goals_urls <- cache[[paste0("goals_", league)]] %||% character(0)
  
  # Complete = in all three
  complete <- Reduce(intersect, list(player_urls, shots_urls, goals_urls))
  
  list(
    player = player_urls,
    shots = shots_urls,
    team_goals = goals_urls,
    complete = complete
  )
}

################################################################################
# LOCAL BACKUP
################################################################################

init_backup_dir <- function() {
  if (SAVE_LOCAL_BACKUP && !dir.exists(BACKUP_DIR)) {
    dir.create(BACKUP_DIR, recursive = TRUE)
    message(sprintf("✓ Created backup directory: %s", BACKUP_DIR))
  }
}

save_local_backup <- function(data, data_type, league) {
  if (!SAVE_LOCAL_BACKUP || is.null(data) || nrow(data) == 0) return(FALSE)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(BACKUP_DIR, sprintf("%s_%s_%s.csv", data_type, league, timestamp))
  
  tryCatch({
    write.csv(data, filename, row.names = FALSE)
    message(sprintf("    💾 Backup: %s", basename(filename)))
    return(TRUE)
  }, error = function(e) FALSE)
}

################################################################################
# CHROMOTE BROWSER
################################################################################

init_browser <- function() {
  message("Initializing headless browser...")
  
  tryCatch({
    system("pkill -f 'chrome.*--headless'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }, error = function(e) {})
  
  tryCatch({
    chromote::default_chromote_object()$close()
  }, error = function(e) {})
  
  Sys.sleep(2)
  b <- ChromoteSession$new()
  
  b$Network$setUserAgentOverride(
    userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  )
  
  message("✓ Browser initialized")
  return(b)
}

close_browser <- function(browser) {
  if (!is.null(browser)) {
    tryCatch({ browser$close() }, error = function(e) {})
  }
  tryCatch({
    system("pkill -f 'chrome.*--headless'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }, error = function(e) {})
}

is_browser_alive <- function(browser) {
  tryCatch({
    browser$Runtime$evaluate("1+1")
    return(TRUE)
  }, error = function(e) FALSE)
}

fetch_page <- function(url, browser, wait_time = 10, max_retries = 3) {
  
  for (attempt in 1:max_retries) {
    if (!is_browser_alive(browser)) {
      message("  ⚠️ Browser session lost, reinitializing...")
      tryCatch({ browser$close() }, error = function(e) {})
      Sys.sleep(2)
      browser <<- init_browser()
      Sys.sleep(2)
    }
    
    result <- tryCatch({
      browser$Page$navigate(url)
      Sys.sleep(wait_time)
      html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
      page <- read_html(html)
      
      title <- page %>% html_node("title") %>% html_text()
      if (!is.na(title) && grepl("Just a moment", title, ignore.case = TRUE)) {
        message(sprintf("  ⚠️ Cloudflare (attempt %d/%d)", attempt, max_retries))
        Sys.sleep(10)
        return(NULL)
      }
      return(page)
    }, error = function(e) {
      message(sprintf("  ⚠️ Fetch error: %s", e$message))
      return(NULL)
    })
    
    if (!is.null(result)) return(result)
    if (attempt < max_retries) Sys.sleep(5)
  }
  
  message("  ✗ Failed to fetch page")
  return(NULL)
}

################################################################################
# GOOGLE SHEETS WRITE
################################################################################

write_to_google_sheet <- function(data, sheet_id, sheet_name, schema = NULL, data_type = "records") {
  if (is.null(data) || nrow(data) == 0) return(FALSE)
  
  if (!is.null(schema)) {
    data <- enforce_schema(data, schema)
  }
  
  message(sprintf("  📤 Writing %d %s to %s...", nrow(data), data_type, sheet_name))
  
  tryCatch({
    sheet_info <- gs4_get(sheet_id)
    existing_sheets <- sheet_info$sheets$name
    
    if (!sheet_name %in% existing_sheets) {
      sheet_write(data, ss = sheet_id, sheet = sheet_name)
    } else {
      # Match existing column order
      existing_data <- tryCatch({
        read_sheet(sheet_id, sheet = sheet_name, n_max = 1)
      }, error = function(e) NULL)
      
      if (!is.null(existing_data) && ncol(existing_data) > 0) {
        existing_cols <- names(existing_data)
        for (col in existing_cols) {
          if (!col %in% names(data)) data[[col]] <- NA
        }
        new_cols <- setdiff(names(data), existing_cols)
        data <- data %>% select(all_of(c(existing_cols, new_cols)))
      }
      
      sheet_append(data, ss = sheet_id, sheet = sheet_name)
    }
    
    message(sprintf("    ✓ Write successful (%d rows)", nrow(data)))
    return(TRUE)
  }, error = function(e) {
    message(sprintf("    ✗ Error: %s", e$message))
    return(FALSE)
  })
}

################################################################################
# HELPER FUNCTIONS
################################################################################

get_league_url <- function(league, season) {
  league_code <- LEAGUE_CODES[league]
  sprintf("https://fbref.com/en/comps/%s/%s/%s-Stats", league_code, season, league)
}

polite_delay <- function() {
  Sys.sleep(RATE_LIMIT_DELAY)
}

################################################################################
# SCRAPING FUNCTIONS
################################################################################

scrape_league_match_urls <- function(league, season, browser) {
  
  message(sprintf("\n=== Getting Match URLs for %s %s ===", league, season))
  
  league_url <- get_league_url(league, season)
  polite_delay()
  
  tryCatch({
    page <- fetch_page(league_url, browser)
    if (is.null(page)) return(NULL)
    
    schedule_link <- page %>%
      html_nodes("a") %>%
      keep(~ grepl("Scores & Fixtures|Schedule", html_text(.))) %>%
      html_attr("href") %>%
      first()
    
    if (is.na(schedule_link)) {
      schedule_link <- page %>%
        html_nodes("div.filter a") %>%
        keep(~ grepl("Scores|Schedule|Fixtures", html_attr(., "href"))) %>%
        html_attr("href") %>%
        first()
    }
    
    if (is.na(schedule_link)) stop("Could not find schedule link")
    
    if (!grepl("^http", schedule_link)) {
      schedule_url <- paste0("https://fbref.com", schedule_link)
    } else {
      schedule_url <- schedule_link
    }
    
    polite_delay()
    schedule_page <- fetch_page(schedule_url, browser)
    if (is.null(schedule_page)) return(NULL)
    
    fixtures_table <- schedule_page %>% html_node("table.stats_table")
    if (is.null(fixtures_table)) stop("Could not find fixtures table")
    
    rows <- fixtures_table %>% html_nodes("tbody tr")
    
    match_data <- map_df(rows, function(row) {
      if (length(html_attrs(row)) > 0) {
        if ("class" %in% names(html_attrs(row))) {
          if (grepl("spacer", html_attr(row, "class"))) return(NULL)
        }
      }
      
      match_link <- row %>% html_node("td[data-stat='score'] a") %>% html_attr("href")
      if (is.na(match_link)) return(NULL)
      
      score_text <- row %>% html_node("td[data-stat='score'] a") %>% html_text(trim = TRUE)
      home_goals <- NA; away_goals <- NA
      if (!is.na(score_text) && score_text != "") {
        score_parts <- str_split(score_text, "[–\\-—]")[[1]]
        if (length(score_parts) == 2) {
          home_goals <- as.integer(str_trim(score_parts[1]))
          away_goals <- as.integer(str_trim(score_parts[2]))
        }
      }
      
      gameweek <- row %>% html_node("th[data-stat='gameweek']") %>% html_text(trim = TRUE)
      date <- row %>% html_node("td[data-stat='date']") %>% html_text(trim = TRUE)
      home_team <- row %>% html_node("td[data-stat='home_team']") %>% html_text(trim = TRUE)
      away_team <- row %>% html_node("td[data-stat='away_team']") %>% html_text(trim = TRUE)
      venue <- row %>% html_node("td[data-stat='venue']") %>% html_text(trim = TRUE)
      
      match_url <- if (!grepl("^http", match_link)) {
        paste0("https://fbref.com", match_link)
      } else match_link
      
      tibble(gameweek = gameweek, date = date, home_team = home_team, away_team = away_team,
             home_goals = home_goals, away_goals = away_goals, venue = venue, match_url = match_url)
    })
    
    match_data <- match_data %>% filter(!is.na(match_url))
    message(sprintf("✓ Found %d completed matches", nrow(match_data)))
    return(match_data)
    
  }, error = function(e) {
    message(sprintf("ERROR: %s", e$message))
    return(NULL)
  })
}

################################################################################
# DATA EXTRACTION - COMBINED PLAYER STATS
################################################################################

scrape_match_all_data <- function(match_url, match_info, league, browser, existing_urls) {
  
  polite_delay()
  
  need_player <- !(match_url %in% existing_urls$player)
  need_shots <- !(match_url %in% existing_urls$shots)
  
  data_needed <- c(
    if (need_player) "Player Stats" else NULL,
    if (need_shots) "Shots" else NULL
  )
  
  if (length(data_needed) == 0) {
    message("    ⏭️ All data exists - skipping")
    return(list(player_stats = NULL, shots = NULL, success = TRUE))
  }
  
  message(sprintf("  Collecting: %s", paste(data_needed, collapse = ", ")))
  
  page <- tryCatch({ fetch_page(match_url, browser) }, error = function(e) NULL)
  if (is.null(page)) {
    return(list(player_stats = NULL, shots = NULL, success = FALSE))
  }
  
  tryCatch({
    # Get match metadata
    match_date <- page %>%
      html_nodes("div.scorebox_meta div") %>%
      html_text() %>%
      .[1] %>%
      str_trim()
    
    teams <- page %>%
      html_nodes("div.scorebox div:nth-child(1) strong a") %>%
      html_text() %>%
      .[1:2]
    
    if (length(teams) < 2) {
      message("    Could not extract team names")
      return(list(player_stats = NULL, shots = NULL, success = FALSE))
    }
    
    home_team <- teams[1]
    away_team <- teams[2]
    
    results <- list(player_stats = NULL, shots = NULL, success = TRUE)
    
    if (need_player) {
      # Extract and combine Summary + Possession
      results$player_stats <- extract_combined_player_stats(
        page, home_team, away_team, match_date, match_url, match_info, league
      )
    }
    
    if (need_shots) {
      results$shots <- extract_shots(page, home_team, away_team, match_date, match_url, match_info, league)
    }
    
    counts <- c()
    if (!is.null(results$player_stats)) counts <- c(counts, sprintf("Players: %d", nrow(results$player_stats)))
    if (!is.null(results$shots)) counts <- c(counts, sprintf("Shots: %d", nrow(results$shots)))
    if (length(counts) > 0) message(sprintf("    ✓ %s", paste(counts, collapse = ", ")))
    
    return(results)
    
  }, error = function(e) {
    message(sprintf("    ✗ Parse error: %s", e$message))
    return(list(player_stats = NULL, shots = NULL, success = FALSE))
  })
}

extract_combined_player_stats <- function(page, home_team, away_team, match_date, match_url, match_info, league) {
  
  stats_tables <- page %>% html_nodes("table.stats_table")
  
  # Find Summary tables
  summary_home <- NULL; summary_away <- NULL
  for (tbl in stats_tables) {
    table_id <- html_attr(tbl, "id")
    if (!is.na(table_id) && grepl("stats_.*_summary", table_id)) {
      if (is.null(summary_home)) summary_home <- tbl
      else if (is.null(summary_away)) { summary_away <- tbl; break }
    }
  }
  
  # Find Possession tables
  poss_home <- NULL; poss_away <- NULL
  for (tbl in stats_tables) {
    table_id <- html_attr(tbl, "id")
    if (!is.na(table_id) && grepl("stats_.*_possession", table_id)) {
      if (is.null(poss_home)) poss_home <- tbl
      else if (is.null(poss_away)) { poss_away <- tbl; break }
    }
  }
  
  # Parse tables
  parse_player_table <- function(table, team_name) {
    if (is.null(table)) return(NULL)
    
    headers <- table %>%
      html_nodes("thead tr:last-child th") %>%
      html_attr("data-stat")
    headers[is.na(headers)] <- "player"
    
    rows <- table %>% html_nodes("tbody tr:not(.thead)")
    
    data_list <- lapply(rows, function(row) {
      cells <- row %>% html_nodes("th, td")
      values <- cells %>% html_text(trim = TRUE)
      if (length(values) == 0) return(NULL)
      
      player_link <- row %>% html_node("th a") %>% html_attr("href")
      
      n <- min(length(values), length(headers))
      values <- values[1:n]
      names(values) <- headers[1:n]
      
      df <- as.data.frame(t(values), stringsAsFactors = FALSE)
      df$player_href <- player_link
      return(df)
    })
    
    data_list <- data_list[!sapply(data_list, is.null)]
    if (length(data_list) > 0) {
      df <- bind_rows(data_list)
      df$team <- team_name
      return(df)
    }
    return(NULL)
  }
  
  # Parse Summary
  summary_home_df <- parse_player_table(summary_home, home_team)
  summary_away_df <- parse_player_table(summary_away, away_team)
  summary_df <- bind_rows(summary_home_df, summary_away_df)
  
  # Parse Possession
  poss_home_df <- parse_player_table(poss_home, home_team)
  poss_away_df <- parse_player_table(poss_away, away_team)
  poss_df <- bind_rows(poss_home_df, poss_away_df)
  
  # Combine Summary + Possession
  if (!is.null(summary_df) && nrow(summary_df) > 0) {
    summary_df$join_key <- paste(summary_df$player, summary_df$team, sep = "|||")
    
    if (!is.null(poss_df) && nrow(poss_df) > 0) {
      poss_df$join_key <- paste(poss_df$player, poss_df$team, sep = "|||")
      
      # Get possession columns not in summary
      summary_cols <- names(summary_df)
      poss_only_cols <- setdiff(names(poss_df), summary_cols)
      poss_only_cols <- c("join_key", poss_only_cols)
      
      poss_slim <- poss_df %>%
        select(any_of(poss_only_cols)) %>%
        distinct(join_key, .keep_all = TRUE)
      
      combined_df <- summary_df %>%
        left_join(poss_slim, by = "join_key", suffix = c("", "_poss"))
    } else {
      combined_df <- summary_df
    }
    
    combined_df <- combined_df %>% select(-join_key)
    
    # Add metadata
    combined_df$league <- league
    combined_df$season <- SEASON
    combined_df$match_date <- match_date
    combined_df$home_team <- home_team
    combined_df$away_team <- away_team
    combined_df$match_url <- match_url
    combined_df$gameweek <- match_info$gameweek
    combined_df$venue <- match_info$venue
    
    return(combined_df)
  }
  
  return(NULL)
}

extract_shots <- function(page, home_team, away_team, match_date, match_url, match_info, league) {
  
  shots_table <- page %>% html_node("table#shots_all")
  if (is.null(shots_table)) return(NULL)
  
  headers <- shots_table %>%
    html_nodes("thead tr:last-child th") %>%
    html_attr("data-stat")
  headers[is.na(headers)] <- "minute"
  
  rows <- shots_table %>% html_nodes("tbody tr:not(.thead)")
  
  shots_list <- lapply(rows, function(row) {
    cells <- row %>% html_nodes("th, td")
    values <- cells %>% html_text(trim = TRUE)
    if (length(values) == 0) return(NULL)
    
    n <- min(length(values), length(headers))
    values <- values[1:n]
    names(values) <- headers[1:n]
    
    as.data.frame(t(values), stringsAsFactors = FALSE)
  })
  
  shots_list <- shots_list[!sapply(shots_list, is.null)]
  if (length(shots_list) == 0) return(NULL)
  
  shots_df <- bind_rows(shots_list)
  shots_df$league <- league
  shots_df$season <- SEASON
  shots_df$match_date <- match_date
  shots_df$home_team <- home_team
  shots_df$away_team <- away_team
  shots_df$match_url <- match_url
  shots_df$gameweek <- match_info$gameweek
  
  return(shots_df)
}

################################################################################
# BATCH WRITE
################################################################################

flush_batch_to_sheets <- function(batch_data, league) {
  
  message("\n  === FLUSHING BATCH ===")
  
  # Write Combined Player Stats
  if (!is.null(batch_data$player_stats) && nrow(batch_data$player_stats) > 0) {
    write_to_google_sheet(
      batch_data$player_stats,
      SHEETS_CONFIG$player_match_stats$sheet_id,
      SHEETS_CONFIG$player_match_stats$sheet_name,
      SCHEMA_PLAYER_MATCH_STATS,
      "player stats"
    )
    save_local_backup(batch_data$player_stats, "player_stats", league)
  }
  
  # Write Shots
  if (!is.null(batch_data$shots) && nrow(batch_data$shots) > 0) {
    write_to_google_sheet(
      batch_data$shots,
      SHEETS_CONFIG$shots$sheet_id,
      SHEETS_CONFIG$shots$sheet_name,
      SCHEMA_SHOTS,
      "shots"
    )
    save_local_backup(batch_data$shots, "shots", league)
  }
  
  message("  === BATCH COMPLETE ===\n")
}

################################################################################
# MAIN WORKFLOW
################################################################################

process_league <- function(league, season, browser, url_cache) {
  
  message("\n################################################################################")
  message(sprintf("# Processing: %s %s", league, season))
  message("################################################################################")
  
  existing_urls <- get_existing_urls_for_league(league, url_cache)
  
  message(sprintf("  Existing - Player: %d, Shots: %d, Goals: %d",
                  length(existing_urls$player), length(existing_urls$shots),
                  length(existing_urls$team_goals)))
  
  all_matches <- scrape_league_match_urls(league, season, browser)
  
  if (is.null(all_matches) || nrow(all_matches) == 0) {
    message("✗ No matches found")
    return(list(matches_processed = 0, matches_failed = 0))
  }
  
  # Team Goals (from fixture list)
  new_team_goals <- all_matches %>%
    filter(!match_url %in% existing_urls$team_goals) %>%
    filter(!is.na(home_goals) & !is.na(away_goals))
  
  if (nrow(new_team_goals) > 0) {
    team_goals_df <- new_team_goals %>%
      mutate(league = league, season = season) %>%
      select(league, season, gameweek, date, home_team, away_team,
             home_goals, away_goals, venue, match_url)
    
    write_to_google_sheet(
      team_goals_df,
      SHEETS_CONFIG$team_goals$sheet_id,
      SHEETS_CONFIG$team_goals$sheet_name,
      SCHEMA_TEAM_GOALS,
      "team goals"
    )
    message(sprintf("✓ Wrote %d team goal records", nrow(team_goals_df)))
  }
  
  # Filter matches needing page visits
  new_matches <- all_matches %>%
    filter(!match_url %in% existing_urls$complete)
  
  if (nrow(new_matches) == 0) {
    message("✓ All matches up to date!")
    return(list(matches_processed = 0, matches_failed = 0,
                player_count = 0, shots_count = 0,
                team_goals_count = nrow(new_team_goals)))
  }
  
  message(sprintf("\n=== MATCHES TO PROCESS: %d ===", nrow(new_matches)))
  message(sprintf("Estimated time: ~%.1f minutes", (nrow(new_matches) * RATE_LIMIT_DELAY) / 60))
  
  # Process in batches
  batch_player <- list()
  batch_shots <- list()
  
  total_processed <- 0
  total_failed <- 0
  total_player <- 0
  total_shots <- 0
  batch_count <- 0
  
  for (i in 1:nrow(new_matches)) {
    match <- new_matches[i, ]
    message(sprintf("\n[%d/%d] %s vs %s", i, nrow(new_matches), match$home_team, match$away_team))
    
    data <- scrape_match_all_data(match$match_url, match, league, browser, existing_urls)
    
    if (isTRUE(data$success)) {
      if (!is.null(data$player_stats) && nrow(data$player_stats) > 0) {
        batch_player[[length(batch_player) + 1]] <- data$player_stats
      }
      if (!is.null(data$shots) && nrow(data$shots) > 0) {
        batch_shots[[length(batch_shots) + 1]] <- data$shots
      }
      
      total_processed <- total_processed + 1
      batch_count <- batch_count + 1
    } else {
      total_failed <- total_failed + 1
    }
    
    # Flush batch
    if (batch_count >= BATCH_SIZE || i == nrow(new_matches)) {
      batch_data <- list(
        player_stats = if (length(batch_player) > 0) bind_rows(batch_player) else data.frame(),
        shots = if (length(batch_shots) > 0) bind_rows(batch_shots) else data.frame()
      )
      
      if (nrow(batch_data$player_stats) > 0 || nrow(batch_data$shots) > 0) {
        flush_batch_to_sheets(batch_data, league)
        total_player <- total_player + nrow(batch_data$player_stats)
        total_shots <- total_shots + nrow(batch_data$shots)
      }
      
      batch_player <- list()
      batch_shots <- list()
      batch_count <- 0
      
      message(sprintf("  📊 Progress: %d/%d matches", i, nrow(new_matches)))
    }
  }
  
  message(sprintf("\n✓ %s COMPLETE: %d processed, %d failed", league, total_processed, total_failed))
  
  return(list(
    matches_processed = total_processed,
    matches_failed = total_failed,
    player_count = total_player,
    shots_count = total_shots,
    team_goals_count = nrow(new_team_goals)
  ))
}

################################################################################
# MAIN
################################################################################

main <- function() {
  
  message("================================================================================")
  message("       FBref COMBINED SCRAPER - UNIFIED PLAYER MATCH STATS")
  message("================================================================================")
  message(sprintf("Leagues: %s", paste(LEAGUES_TO_SCRAPE, collapse = ", ")))
  message(sprintf("Season: %s", SEASON))
  message("")
  message("📊 OUTPUT SHEETS:")
  message("   • Player_Match_Stats: Combined Summary + Possession (NEW!)")
  message("   • Shot_Data: Individual shots")
  message("   • Team_Goals: Match scores")
  message("")
  message(sprintf("⚙️ Settings: Batch %d, Rate limit %ds", BATCH_SIZE, RATE_LIMIT_DELAY))
  message("================================================================================")
  
  init_backup_dir()
  
  url_cache <- load_all_existing_urls()
  
  browser <- init_browser()
  
  message("\nAuthenticating with Google Sheets...")
  tryCatch({
    gs4_auth()
    message("✓ Authenticated")
  }, error = function(e) {
    close_browser(browser)
    stop("Auth failed: ", e$message)
  })
  
  all_results <- list()
  
  tryCatch({
    for (league in LEAGUES_TO_SCRAPE) {
      result <- tryCatch({
        process_league(league, SEASON, browser, url_cache)
      }, error = function(e) {
        message(sprintf("ERROR for %s: %s", league, e$message))
        NULL
      })
      
      if (!is.null(result)) all_results[[league]] <- result
      
      if (league != LEAGUES_TO_SCRAPE[length(LEAGUES_TO_SCRAPE)]) {
        message(sprintf("\n⏸ Pausing %ds...", LEAGUE_DELAY))
        Sys.sleep(LEAGUE_DELAY)
      }
    }
  }, finally = {
    close_browser(browser)
  })
  
  # Summary
  message("\n================================================================================")
  message("                              SUMMARY")
  message("================================================================================")
  
  for (league in names(all_results)) {
    r <- all_results[[league]]
    message(sprintf("✓ %s: %d matches, %d players, %d shots, %d goals",
                    league, r$matches_processed, r$player_count, r$shots_count, r$team_goals_count))
  }
  
  message("")
  message("Sheets:")
  message(sprintf("  Player Stats: https://docs.google.com/spreadsheets/d/%s/edit",
                  SHEETS_CONFIG$player_match_stats$sheet_id))
  message(sprintf("  Shots: https://docs.google.com/spreadsheets/d/%s/edit",
                  SHEETS_CONFIG$shots$sheet_id))
  message(sprintf("  Team Goals: https://docs.google.com/spreadsheets/d/%s/edit",
                  SHEETS_CONFIG$team_goals$sheet_id))
  message("================================================================================\n")
  
  return(all_results)
}

################################################################################
# RUN
################################################################################

result <- main()
