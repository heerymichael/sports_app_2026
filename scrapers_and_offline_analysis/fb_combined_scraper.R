################################################################################
# FBref COMBINED SCRAPER - UNIFIED PLAYER MATCH STATS
# 
# PRODUCTION VERSION - All fixes included:
#   - Fixed URL format for FBref (current season has no year in URL)
#   - Fixed score parsing regex (handles en-dashes, em-dashes)
#   - Fixed shot column names: xg_shot->xg, psxg_shot->psxg, team->squad
#   - Parquet-first loading for existing URLs (fast startup)
#   - Fixed write_to_google_sheet to handle empty sheets (writes headers)
#   - Exports to Parquet at end of scrape
#   - NON-HEADLESS browser to bypass Cloudflare
#
# Target Sheets:
#   - Player_Match_Stats: Combined player-level data
#   - Shot_Data: Individual shot records
#   - Team_Goals: Match scores
################################################################################
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(googlesheets4)
library(googledrive)
library(arrow)
library(chromote)

################################################################################
# CONFIGURATION
################################################################################
RATE_LIMIT_DELAY <- 10
LEAGUE_DELAY <- 15
BATCH_SIZE <- 20
SAVE_LOCAL_BACKUP <- TRUE
BACKUP_DIR <- "fbref_backups"
LEAGUES_TO_SCRAPE <- c("Premier-League", "Serie-A", "La-Liga", "Bundesliga", "Championship")
SEASON <- "2025-2026"

SHEETS_CONFIG <- list(
  player_match_stats = list(
    sheet_id = "12MXPMsuI4S7EiTPnVpaqx5-riXoK37cEtPo-MFOf1fA",
    sheet_name = "Player_Match_Stats"
  ),
  shots = list(
    sheet_id = "1oxQ6rk_B_r2QUNZxGssmDEopKqJD0otMt_r1hPOufT0",
    sheet_name = "Shot_Data"
  ),
  team_goals = list(
    sheet_id = "1gUCVxBFR3kwE259ZccWBLRn7iAlV0HpZL7OccbGsWuo",
    sheet_name = "Team_Goals"
  )
)

DRIVE_FOLDER_ID <- "1APlkMnjX3RjxPOzEnYWP5DYYCH_AcUM8"
EXPORT_PARQUET_TO_DRIVE <- TRUE

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
# COLUMN SCHEMAS
################################################################################
SCHEMA_PLAYER_MATCH_STATS <- c(
  "league", "season", "match_date", "gameweek",
  "home_team", "away_team", "venue",
  "team", "player", "player_href", "shirtnumber", "nationality", "position", "age",
  "minutes",
  "goals", "assists", "pens_made", "pens_att",
  "shots", "shots_on_target", "xg", "npxg",
  "passes_completed", "passes", "passes_pct", "progressive_passes", "xg_assist",
  "sca", "gca",
  "tackles", "interceptions", "blocks",
  "cards_yellow", "cards_red",
  "touches", "touches_def_pen_area", "touches_def_3rd", "touches_mid_3rd",
  "touches_att_3rd", "touches_att_pen_area", "touches_live_ball",
  "take_ons", "take_ons_won", "take_ons_won_pct", "take_ons_tackled", "take_ons_tackled_pct",
  "carries", "carries_distance", "carries_progressive_distance", "progressive_carries",
  "carries_into_final_third", "carries_into_penalty_area",
  "miscontrols", "dispossessed", "passes_received", "progressive_passes_received",
  "match_url",
  "touches_poss", "take_ons_poss", "take_ons_won_poss", "carries_poss", "progressive_carries_poss",
  "fouls", "fouled", "offsides", "crosses", "tackles_won", "own_goals", "pens_won", "pens_conceded"
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
  
  for (col in schema) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  df <- df %>% 
    select(all_of(schema)) %>%
    mutate(across(everything(), as.character))
  
  return(df)
}

################################################################################
# LOAD EXISTING URLS - PARQUET FIRST, SHEETS FALLBACK
################################################################################
load_parquet_from_drive <- function(data_type) {
  if (!requireNamespace("googledrive", quietly = TRUE) || 
      !requireNamespace("arrow", quietly = TRUE)) {
    return(NULL)
  }
  
  filenames <- list(
    player_match_stats = "player_match_stats.parquet",
    shots = "shots.parquet",
    team_goals = "team_goals.parquet"
  )
  
  filename <- filenames[[data_type]]
  if (is.null(filename)) return(NULL)
  
  tryCatch({
    googledrive::drive_deauth()
    
    files <- googledrive::drive_find(
      q = sprintf("'%s' in parents and name = '%s'", DRIVE_FOLDER_ID, filename),
      n_max = 1
    )
    
    if (nrow(files) == 0) {
      message(sprintf("  [WARN] Parquet not found on Drive: %s", filename))
      return(NULL)
    }
    
    temp_file <- tempfile(fileext = ".parquet")
    googledrive::drive_download(
      googledrive::as_id(files$id[1]),
      path = temp_file,
      overwrite = TRUE
    )
    
    data <- arrow::read_parquet(temp_file)
    unlink(temp_file)
    
    message(sprintf("  [OK] Loaded %s from Drive Parquet: %d rows", data_type, nrow(data)))
    return(as.data.frame(data))
    
  }, error = function(e) {
    message(sprintf("  [WARN] Drive Parquet load failed for %s: %s", data_type, e$message))
    return(NULL)
  })
}

load_all_existing_urls <- function() {
  message("\n========================================")
  message("Loading existing match URLs...")
  message("========================================")
  
  cache <- list()
  
  message("Attempting to load from Google Drive Parquet files...")
  
  player_data <- load_parquet_from_drive("player_match_stats")
  shots_data <- load_parquet_from_drive("shots")
  goals_data <- load_parquet_from_drive("team_goals")
  
  parquet_success <- !is.null(player_data) && !is.null(shots_data) && !is.null(goals_data)
  
  if (!parquet_success) {
    message("\n[INFO] Some Parquet files missing - falling back to Google Sheets (slower)...")
    
    if (is.null(player_data)) {
      message("  Loading Player_Match_Stats from Sheets...")
      player_data <- tryCatch({
        read_sheet(
          SHEETS_CONFIG$player_match_stats$sheet_id,
          sheet = SHEETS_CONFIG$player_match_stats$sheet_name,
          col_types = "c"
        )
      }, error = function(e) {
        message(sprintf("    [WARN] Could not read: %s", e$message))
        data.frame()
      })
    }
    
    if (is.null(shots_data)) {
      message("  Loading Shot_Data from Sheets...")
      shots_data <- tryCatch({
        read_sheet(
          SHEETS_CONFIG$shots$sheet_id,
          sheet = SHEETS_CONFIG$shots$sheet_name,
          col_types = "c"
        )
      }, error = function(e) data.frame())
    }
    
    if (is.null(goals_data)) {
      message("  Loading Team_Goals from Sheets...")
      goals_data <- tryCatch({
        read_sheet(
          SHEETS_CONFIG$team_goals$sheet_id,
          sheet = SHEETS_CONFIG$team_goals$sheet_name,
          col_types = "c"
        )
      }, error = function(e) data.frame())
    }
  } else {
    message("[OK] All data loaded from Parquet files (fast path)")
  }
  
  # Build URL cache from player data
  if (!is.null(player_data) && nrow(player_data) > 0 && "match_url" %in% names(player_data)) {
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
    message("  Player Match Stats: Empty or new")
    for (league in LEAGUES_TO_SCRAPE) {
      cache[[paste0("player_", league)]] <- character(0)
    }
  }
  
  # Build URL cache from shots data
  if (!is.null(shots_data) && nrow(shots_data) > 0 && "match_url" %in% names(shots_data)) {
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
    message(sprintf("  Shots: %d total rows", nrow(shots_data)))
  } else {
    message("  Shots: Empty or new")
    for (league in LEAGUES_TO_SCRAPE) {
      cache[[paste0("shots_", league)]] <- character(0)
    }
  }
  
  # Build URL cache from goals data
  if (!is.null(goals_data) && nrow(goals_data) > 0 && "match_url" %in% names(goals_data)) {
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
    message(sprintf("  Team Goals: %d total rows", nrow(goals_data)))
  } else {
    message("  Team Goals: Empty or new")
    for (league in LEAGUES_TO_SCRAPE) {
      cache[[paste0("goals_", league)]] <- character(0)
    }
  }
  
  message("========================================\n")
  return(cache)
}

get_existing_urls_for_league <- function(league, cache) {
  player_urls <- cache[[paste0("player_", league)]] %||% character(0)
  shots_urls <- cache[[paste0("shots_", league)]] %||% character(0)
  goals_urls <- cache[[paste0("goals_", league)]] %||% character(0)
  
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
    message(sprintf("[OK] Created backup directory: %s", BACKUP_DIR))
  }
}

save_local_backup <- function(data, data_type, league) {
  if (!SAVE_LOCAL_BACKUP || is.null(data) || nrow(data) == 0) return(FALSE)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(BACKUP_DIR, sprintf("%s_%s_%s.csv", data_type, league, timestamp))
  
  tryCatch({
    write.csv(data, filename, row.names = FALSE)
    message(sprintf("    [SAVE] Backup: %s", basename(filename)))
    return(TRUE)
  }, error = function(e) FALSE)
}

################################################################################
# CHROMOTE BROWSER - NON-HEADLESS FOR CLOUDFLARE BYPASS
################################################################################
# Global reference to chromote objects for cleanup
BROWSER_ENV <- new.env()

init_browser <- function() {
  message("Initializing browser (non-headless for Cloudflare bypass)...")
  
  # Kill any existing Chrome processes
  
  tryCatch({
    system("pkill -f 'chrome'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }, error = function(e) {})
  
  # Close any existing chromote sessions
  tryCatch({
    chromote::default_chromote_object()$close()
  }, error = function(e) {})
  
  Sys.sleep(2)
  
  # Create Chrome with anti-detection flags
  # chromote 0.2.0: Chrome$new() is non-headless by default
  chrome_args <- c(
    "--disable-blink-features=AutomationControlled",
    "--disable-infobars",
    "--disable-extensions",
    "--no-first-run",
    "--disable-default-apps",
    "--disable-popup-blocking",
    "--window-size=1920,1080"
  )
  
  tryCatch({
    chrome <- Chrome$new(args = chrome_args)
    BROWSER_ENV$chrome <- chrome
    
    chromote_obj <- Chromote$new(browser = chrome)
    BROWSER_ENV$chromote <- chromote_obj
    
    b <- ChromoteSession$new(parent = chromote_obj)
    
    # Set user agent
    b$Network$setUserAgentOverride(
      userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
    )
    
    # Hide webdriver property
    b$Page$addScriptToEvaluateOnNewDocument(
      source = "Object.defineProperty(navigator, 'webdriver', {get: () => undefined})"
    )
    
    message("[OK] Browser initialized (visible window)")
    return(b)
    
  }, error = function(e) {
    message(sprintf("[WARN] Non-headless failed: %s", e$message))
    message("[INFO] Falling back to default chromote...")
    
    # Fallback to standard chromote
    b <- ChromoteSession$new()
    b$Network$setUserAgentOverride(
      userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
    )
    
    message("[OK] Browser initialized (headless fallback)")
    return(b)
  })
}

close_browser <- function(browser) {
  if (!is.null(browser)) {
    tryCatch({ browser$close() }, error = function(e) {})
  }
  
  # Close chromote objects
  tryCatch({
    if (!is.null(BROWSER_ENV$chromote)) BROWSER_ENV$chromote$close()
  }, error = function(e) {})
  
  tryCatch({
    if (!is.null(BROWSER_ENV$chrome)) BROWSER_ENV$chrome$close()
  }, error = function(e) {})
  
  # Kill Chrome processes
  tryCatch({
    system("pkill -f 'chrome'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }, error = function(e) {})
  
  BROWSER_ENV$chrome <- NULL
  BROWSER_ENV$chromote <- NULL
}

is_browser_alive <- function(browser) {
  tryCatch({
    browser$Runtime$evaluate("1+1")
    return(TRUE)
  }, error = function(e) FALSE)
}

fetch_page <- function(url, browser, wait_time = 12, max_retries = 3) {
  
  for (attempt in 1:max_retries) {
    if (!is_browser_alive(browser)) {
      message("  [WARN] Browser session lost, reinitializing...")
      tryCatch({ close_browser(browser) }, error = function(e) {})
      Sys.sleep(2)
      browser <<- init_browser()
      Sys.sleep(2)
    }
    
    result <- tryCatch({
      browser$Page$navigate(url)
      Sys.sleep(wait_time)
      
      # Get page HTML
      html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
      page <- read_html(html)
      
      # Check for Cloudflare
      title <- page %>% html_node("title") %>% html_text()
      if (!is.na(title) && grepl("Just a moment|Checking your browser|Attention Required", title, ignore.case = TRUE)) {
        message(sprintf("  [WARN] Cloudflare challenge detected (attempt %d/%d)", attempt, max_retries))
        message("         Waiting 20s for challenge to resolve...")
        Sys.sleep(20)
        
        # Try to get page again after waiting
        html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
        page <- read_html(html)
        
        title <- page %>% html_node("title") %>% html_text()
        if (!is.na(title) && grepl("Just a moment|Checking your browser", title, ignore.case = TRUE)) {
          message("         Still blocked - will retry...")
          if (attempt < max_retries) Sys.sleep(10)
          next
        }
      }
      
      return(page)
      
    }, error = function(e) {
      message(sprintf("  [WARN] Fetch error: %s", e$message))
      return(NULL)
    })
    
    if (!is.null(result)) return(result)
    if (attempt < max_retries) Sys.sleep(5)
  }
  
  message("  [ERROR] Failed to fetch page after all retries")
  return(NULL)
}

################################################################################
# GOOGLE SHEETS WRITE
################################################################################
SHEETS_INITIALIZED <- new.env()

write_to_google_sheet <- function(data, sheet_id, sheet_name, schema = NULL, data_type = "records") {
  if (is.null(data) || nrow(data) == 0) return(FALSE)
  
  if (!is.null(schema)) {
    data <- enforce_schema(data, schema)
  }
  
  message(sprintf("  >> Writing %d %s to %s...", nrow(data), data_type, sheet_name))
  
  sheet_key <- paste0(sheet_id, "_", sheet_name)
  
  tryCatch({
    sheet_info <- gs4_get(sheet_id)
    existing_sheets <- sheet_info$sheets$name
    
    if (!sheet_name %in% existing_sheets) {
      sheet_write(data, ss = sheet_id, sheet = sheet_name)
      SHEETS_INITIALIZED[[sheet_key]] <- TRUE
      message(sprintf("    [OK] Created new sheet with headers (%d rows)", nrow(data)))
      
    } else if (!isTRUE(SHEETS_INITIALIZED[[sheet_key]])) {
      existing_data <- tryCatch({
        read_sheet(sheet_id, sheet = sheet_name, n_max = 1)
      }, error = function(e) NULL)
      
      if (is.null(existing_data)) {
        sheet_props <- sheet_info$sheets[sheet_info$sheets$name == sheet_name, ]
        grid_rows <- tryCatch({ sheet_props$grid_rows }, error = function(e) NA)
        
        if (!is.na(grid_rows) && grid_rows > 1) {
          sheet_append(data, ss = sheet_id, sheet = sheet_name)
          SHEETS_INITIALIZED[[sheet_key]] <- TRUE
        } else {
          sheet_write(data, ss = sheet_id, sheet = sheet_name)
          SHEETS_INITIALIZED[[sheet_key]] <- TRUE
        }
        
      } else if (ncol(existing_data) == 0 || nrow(existing_data) == 0) {
        sheet_write(data, ss = sheet_id, sheet = sheet_name)
        SHEETS_INITIALIZED[[sheet_key]] <- TRUE
        
      } else {
        existing_cols <- names(existing_data)
        for (col in existing_cols) {
          if (!col %in% names(data)) data[[col]] <- NA
        }
        new_cols <- setdiff(names(data), existing_cols)
        data <- data %>% select(all_of(c(existing_cols, new_cols)))
        
        sheet_append(data, ss = sheet_id, sheet = sheet_name)
        SHEETS_INITIALIZED[[sheet_key]] <- TRUE
      }
      
    } else {
      sheet_append(data, ss = sheet_id, sheet = sheet_name)
    }
    
    message(sprintf("    [OK] Write successful (%d rows)", nrow(data)))
    return(TRUE)
  }, error = function(e) {
    message(sprintf("    [ERROR] Error: %s", e$message))
    return(FALSE)
  })
}

################################################################################
# HELPER FUNCTIONS
################################################################################
get_league_url <- function(league, season, use_current = TRUE) {
  league_code <- LEAGUE_CODES[league]
  
  if (use_current) {
    sprintf("https://fbref.com/en/comps/%s/%s-Stats", league_code, league)
  } else {
    sprintf("https://fbref.com/en/comps/%s/%s/%s-%s-Stats", league_code, season, season, league)
  }
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
    if (is.null(page)) {
      message("  [ERROR] Failed to fetch league page")
      return(NULL)
    }
    
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
    if (is.null(schedule_page)) {
      message("  [ERROR] Failed to fetch schedule page")
      return(NULL)
    }
    
    fixtures_table <- schedule_page %>% html_node("table.stats_table")
    if (is.null(fixtures_table)) {
      stop("Could not find fixtures table")
    }
    
    rows <- fixtures_table %>% html_nodes("tbody tr")
    
    match_list <- list()
    
    for (row_idx in seq_along(rows)) {
      row <- rows[[row_idx]]
      
      tryCatch({
        if (length(html_attrs(row)) > 0) {
          if ("class" %in% names(html_attrs(row))) {
            if (grepl("spacer", html_attr(row, "class"))) next
          }
        }
        
        match_link <- row %>% html_node("td[data-stat='score'] a") %>% html_attr("href")
        if (is.na(match_link)) next
        
        score_text <- row %>% html_node("td[data-stat='score'] a") %>% html_text(trim = TRUE)
        home_goals <- NA_integer_
        away_goals <- NA_integer_
        
        if (!is.na(score_text) && score_text != "") {
          score_parts <- str_split(score_text, "[-–—]")[[1]]
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
        
        match_url_full <- if (!grepl("^http", match_link)) {
          paste0("https://fbref.com", match_link)
        } else {
          match_link
        }
        
        match_list[[length(match_list) + 1]] <- data.frame(
          gameweek = as.character(gameweek),
          date = as.character(date),
          home_team = as.character(home_team),
          away_team = as.character(away_team),
          home_goals = home_goals,
          away_goals = away_goals,
          venue = as.character(venue),
          match_url = as.character(match_url_full),
          stringsAsFactors = FALSE
        )
        
      }, error = function(e) {})
    }
    
    if (length(match_list) == 0) {
      message("  [ERROR] No matches parsed from fixtures table")
      return(NULL)
    }
    
    match_data <- bind_rows(match_list)
    match_data <- match_data %>% filter(!is.na(match_url))
    message(sprintf("[OK] Found %d completed matches", nrow(match_data)))
    return(match_data)
    
  }, error = function(e) {
    message(sprintf("ERROR: %s", e$message))
    return(NULL)
  })
}

################################################################################
# DATA EXTRACTION
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
    message("    [SKIP] All data exists")
    return(list(player_stats = NULL, shots = NULL, success = TRUE))
  }
  
  message(sprintf("  Collecting: %s", paste(data_needed, collapse = ", ")))
  
  page <- tryCatch({ fetch_page(match_url, browser) }, error = function(e) NULL)
  if (is.null(page)) {
    return(list(player_stats = NULL, shots = NULL, success = FALSE))
  }
  
  tryCatch({
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
    if (length(counts) > 0) message(sprintf("    [OK] %s", paste(counts, collapse = ", ")))
    
    return(results)
    
  }, error = function(e) {
    message(sprintf("    [ERROR] Parse error: %s", e$message))
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
      if (is.null(summary_home)) {
        summary_home <- tbl
      } else if (is.null(summary_away)) {
        summary_away <- tbl
        break
      }
    }
  }
  
  # Find Possession tables
  poss_home <- NULL; poss_away <- NULL
  for (tbl in stats_tables) {
    table_id <- html_attr(tbl, "id")
    if (!is.na(table_id) && grepl("stats_.*_possession", table_id)) {
      if (is.null(poss_home)) {
        poss_home <- tbl
      } else if (is.null(poss_away)) {
        poss_away <- tbl
        break
      }
    }
  }
  
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

################################################################################
# EXTRACT SHOTS - WITH COLUMN NAME FIXES
################################################################################
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
  
  # =========================================================================
  # FIX COLUMN NAMES - FBref uses different names than our schema
  # =========================================================================
  
  # FBref: xg_shot -> Our schema: xg
  if ("xg_shot" %in% names(shots_df) && !"xg" %in% names(shots_df)) {
    shots_df <- shots_df %>% rename(xg = xg_shot)
  }
  
  # FBref: psxg_shot -> Our schema: psxg
  if ("psxg_shot" %in% names(shots_df) && !"psxg" %in% names(shots_df)) {
    shots_df <- shots_df %>% rename(psxg = psxg_shot)
  }
  
  # FBref: team -> Our schema: squad
  if ("team" %in% names(shots_df) && (!"squad" %in% names(shots_df) || all(is.na(shots_df$squad)))) {
    shots_df$squad <- shots_df$team
  }
  
  # Add metadata
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
    message("[ERROR] No matches found")
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
    message(sprintf("[OK] Wrote %d team goal records", nrow(team_goals_df)))
  }
  
  # Filter matches needing page visits
  new_matches <- all_matches %>%
    filter(!match_url %in% existing_urls$complete)
  
  if (nrow(new_matches) == 0) {
    message("[OK] All matches up to date!")
    return(list(matches_processed = 0, matches_failed = 0,
                player_count = 0, shots_count = 0,
                team_goals_count = nrow(new_team_goals)))
  }
  
  message(sprintf("\n=== MATCHES TO PROCESS: %d ===", nrow(new_matches)))
  message(sprintf("Estimated time: ~%.1f minutes", (nrow(new_matches) * RATE_LIMIT_DELAY) / 60))
  
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
      
      message(sprintf("  >> Progress: %d/%d matches", i, nrow(new_matches)))
    }
  }
  
  message(sprintf("\n[OK] %s COMPLETE: %d processed, %d failed", league, total_processed, total_failed))
  
  return(list(
    matches_processed = total_processed,
    matches_failed = total_failed,
    player_count = total_player,
    shots_count = total_shots,
    team_goals_count = nrow(new_team_goals)
  ))
}

################################################################################
# PARQUET EXPORT TO GOOGLE DRIVE
################################################################################
upload_parquet_to_drive <- function(data, name, folder_id = DRIVE_FOLDER_ID) {
  message(sprintf("  Uploading %s.parquet...", name))
  
  if (any(sapply(data, is.list))) {
    data <- data %>%
      mutate(across(where(is.list), ~sapply(., function(x) if(length(x) == 0) NA else x[[1]])))
  }
  
  temp_file <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data, temp_file)
  
  file_size <- file.size(temp_file) / 1024 / 1024
  message(sprintf("    Size: %.2f MB (%d rows)", file_size, nrow(data)))
  
  existing <- drive_find(
    q = sprintf("'%s' in parents and name = '%s.parquet'", folder_id, name),
    n_max = 1
  )
  
  if (nrow(existing) > 0) {
    result <- drive_update(existing$id, media = temp_file)
    message(sprintf("    [OK] Updated existing file"))
  } else {
    result <- drive_upload(
      temp_file,
      path = as_id(folder_id),
      name = paste0(name, ".parquet"),
      overwrite = FALSE
    )
    drive_share(result$id, role = "reader", type = "anyone")
    message(sprintf("    [OK] Created new file (ID: %s)", result$id))
  }
  
  unlink(temp_file)
  return(result$id)
}

export_to_parquet <- function() {
  if (!EXPORT_PARQUET_TO_DRIVE) {
    message("\n[SKIP] Parquet export disabled (EXPORT_PARQUET_TO_DRIVE = FALSE)")
    return(NULL)
  }
  
  message("\n================================================================================")
  message("EXPORTING TO PARQUET (Google Drive - for fast Shiny app loading)")
  message("================================================================================\n")
  
  message("Authenticating with Google Drive...")
  drive_auth()
  message("[OK] Google Drive authenticated\n")
  
  file_ids <- list()
  
  tryCatch({
    message("Reading Player_Match_Stats from Google Sheets...")
    player_data <- read_sheet(
      SHEETS_CONFIG$player_match_stats$sheet_id,
      sheet = SHEETS_CONFIG$player_match_stats$sheet_name
    ) %>% as.data.frame()
    file_ids$player_match_stats <- upload_parquet_to_drive(player_data, "player_match_stats")
    
    message("\nReading Shot_Data from Google Sheets...")
    shot_data <- read_sheet(
      SHEETS_CONFIG$shots$sheet_id,
      sheet = SHEETS_CONFIG$shots$sheet_name
    ) %>% as.data.frame()
    file_ids$shots <- upload_parquet_to_drive(shot_data, "shots")
    
    message("\nReading Team_Goals from Google Sheets...")
    team_goals <- read_sheet(
      SHEETS_CONFIG$team_goals$sheet_id,
      sheet = SHEETS_CONFIG$team_goals$sheet_name
    ) %>% as.data.frame()
    file_ids$team_goals <- upload_parquet_to_drive(team_goals, "team_goals")
    
    message("\n================================================================================")
    message("[OK] PARQUET EXPORT COMPLETE")
    message("================================================================================")
    message(sprintf("Files at: https://drive.google.com/drive/folders/%s", DRIVE_FOLDER_ID))
    message("================================================================================\n")
    
  }, error = function(e) {
    message(sprintf("\n[ERROR] Parquet export failed: %s", e$message))
    message("Sheets data is still intact - you can retry export later\n")
  })
  
  return(file_ids)
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
  message(">> OUTPUT SHEETS:")
  message("    * Player_Match_Stats: Combined Summary + Possession")
  message("    * Shot_Data: Individual shots (xg, psxg, squad)")
  message("    * Team_Goals: Match scores")
  message("")
  message(sprintf("[CFG] Settings: Batch %d, Rate limit %ds", BATCH_SIZE, RATE_LIMIT_DELAY))
  message("================================================================================")
  
  init_backup_dir()
  
  url_cache <- load_all_existing_urls()
  
  browser <- init_browser()
  
  message("\nAuthenticating with Google Sheets...")
  tryCatch({
    gs4_auth()
    message("[OK] Authenticated")
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
        message(sprintf("\n[PAUSE] Pausing %ds...", LEAGUE_DELAY))
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
    message(sprintf("[OK] %s: %d matches, %d players, %d shots, %d goals",
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
  
  # Export to Parquet for fast Shiny app loading
  export_to_parquet()
  
  return(all_results)
}

################################################################################
# RUN
################################################################################
# To run ONLY the Parquet export (for existing data):
# gs4_auth(); export_to_parquet()

# To run the full scraper + Parquet export:
result <- main()