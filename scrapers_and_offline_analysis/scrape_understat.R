################################################################################
#
#                    UNDERSTAT BATCH SCRAPER
#
# Uses the understat.R module to scrape all match player data
# and save to Google Sheets with Parquet backup
#
# USAGE:
#   source("understat.R")        # Load the module first
#   source("understat_scraper.R")
#   
#   # Test
#   test_scraper()
#   
#   # Run EPL only
#   result <- run_epl_only()
#   
#   # Run all leagues
#   result <- main()
#
################################################################################

library(googlesheets4)
library(googledrive)
library(arrow)
library(dplyr)

# =============================================================================
# CONFIGURATION
# =============================================================================

BATCH_SIZE <- 25
SAVE_LOCAL_BACKUP <- TRUE
BACKUP_DIR <- "understat_backups"
SEASON <- 2025
SEASON_DISPLAY <- "2025-26"

LEAGUES_TO_SCRAPE <- c("EPL", "La liga", "Bundesliga", "Serie A", "Ligue 1")

SHEETS_CONFIG <- list(
  player_match_stats = list(
    sheet_id = NULL,
    sheet_name = "Understat_Player_Match"
  ),
  match_info = list(
    sheet_id = NULL,
    sheet_name = "Understat_Matches"
  )
)

DRIVE_FOLDER_ID <- "1APlkMnjX3RjxPOzEnYWP5DYYCH_AcUM8"
EXPORT_PARQUET_TO_DRIVE <- TRUE

# =============================================================================
# SCHEMAS
# =============================================================================

SCHEMA_PLAYER_MATCH <- c(
  "league", "season", "match_id", "match_date",
  "home_team", "away_team", "home_goals", "away_goals",
  "player_id", "player", "team_id", "home_away", "position", "positionOrder",
  "time_played", "goals", "own_goals", "shots", "xG",
  "assists", "key_passes", "xA", "xGChain", "xGBuildup",
  "yellow_card", "red_card", "roster_in", "roster_out",
  "match_url"
)

SCHEMA_MATCHES <- c(
  "league", "season", "match_id", "datetime",
  "home_team", "away_team", "home_goals", "away_goals",
  "home_xG", "away_xG", "match_url"
)

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

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

init_backup_dir <- function() {
  if (SAVE_LOCAL_BACKUP && !dir.exists(BACKUP_DIR)) {
    dir.create(BACKUP_DIR, recursive = TRUE)
    message(sprintf("[OK] Created backup directory: %s", BACKUP_DIR))
  }
}

save_local_backup <- function(data, data_type, league) {
  if (!SAVE_LOCAL_BACKUP || is.null(data) || nrow(data) == 0) return(FALSE)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  league_clean <- gsub(" ", "_", league)
  filename <- file.path(BACKUP_DIR, sprintf("%s_%s_%s.csv", data_type, league_clean, timestamp))
  
  tryCatch({
    write.csv(data, filename, row.names = FALSE, fileEncoding = "UTF-8")
    message(sprintf("    [SAVE] Backup: %s", basename(filename)))
    return(TRUE)
  }, error = function(e) FALSE)
}

# =============================================================================
# GOOGLE SHEETS FUNCTIONS
# =============================================================================

write_to_google_sheet <- function(data, sheet_id, sheet_name, schema, data_type) {
  if (is.null(data) || nrow(data) == 0) {
    message(sprintf("    [SKIP] No %s data to write", data_type))
    return(FALSE)
  }
  
  data <- enforce_schema(data, schema)
  
  message(sprintf("    Writing %d %s rows to Google Sheet...", nrow(data), data_type))
  
  tryCatch({
    sheets_info <- sheet_names(sheet_id)
    
    if (!sheet_name %in% sheets_info) {
      sheet_add(sheet_id, sheet = sheet_name)
      range_write(sheet_id, data.frame(t(schema), stringsAsFactors = FALSE), 
                  sheet = sheet_name, range = "A1", col_names = FALSE)
    }
    
    sheet_append(sheet_id, data, sheet = sheet_name)
    message(sprintf("    [OK] Wrote %d rows", nrow(data)))
    return(TRUE)
    
  }, error = function(e) {
    message(sprintf("    [ERROR] Failed to write: %s", e$message))
    return(FALSE)
  })
}

export_to_parquet <- function(data, filename) {
  if (is.null(data) || nrow(data) == 0) return(FALSE)
  
  local_path <- file.path(BACKUP_DIR, filename)
  
  tryCatch({
    write_parquet(data, local_path)
    message(sprintf("[OK] Saved Parquet: %s", local_path))
    
    if (EXPORT_PARQUET_TO_DRIVE) {
      drive_upload(
        local_path,
        path = as_id(DRIVE_FOLDER_ID),
        name = filename,
        overwrite = TRUE
      )
      message(sprintf("[OK] Uploaded to Drive: %s", filename))
    }
    
    return(TRUE)
  }, error = function(e) {
    message(sprintf("[WARN] Parquet export failed: %s", e$message))
    return(FALSE)
  })
}

# =============================================================================
# MAIN PROCESSING
# =============================================================================

load_existing_match_ids <- function(sheet_id, sheet_name, league) {
  if (is.null(sheet_id)) return(character(0))
  
  tryCatch({
    existing <- read_sheet(sheet_id, sheet = sheet_name, col_types = "c")
    
    if (nrow(existing) > 0 && "match_id" %in% names(existing) && "league" %in% names(existing)) {
      ids <- existing %>%
        filter(league == !!league) %>%
        pull(match_id) %>%
        unique()
      return(ids)
    }
    
    return(character(0))
    
  }, error = function(e) {
    message(sprintf("  [NOTE] Could not read existing data: %s", e$message))
    return(character(0))
  })
}

process_league <- function(league, season, sheet_id_players, sheet_id_matches) {
  
  message("\n================================================================================")
  message(sprintf("  Processing: %s %s", league, SEASON_DISPLAY))
  message("================================================================================")
  
  # Get existing match IDs
  existing_ids <- load_existing_match_ids(
    sheet_id_players, 
    SHEETS_CONFIG$player_match_stats$sheet_name, 
    league
  )
  message(sprintf("  Existing matches in sheet: %d", length(existing_ids)))
  
  # Get all matches for this league/season
  all_matches <- understat_league_match_results(league, season)
  
  if (is.null(all_matches) || nrow(all_matches) == 0) {
    message("  [ERROR] No matches found")
    return(list(processed = 0, failed = 0))
  }
  
  # Add match URL
  all_matches$match_url <- sprintf("https://understat.com/match/%s", all_matches$match_id)
  
  # Filter to new matches
  new_matches <- all_matches %>%
    filter(!as.character(match_id) %in% existing_ids)
  
  if (nrow(new_matches) == 0) {
    message("  [OK] All matches already scraped!")
    return(list(processed = 0, failed = 0))
  }
  
  message(sprintf("  New matches to scrape: %d", nrow(new_matches)))
  
  # Process in batches
  processed <- 0
  failed <- 0
  batch_players <- list()
  batch_matches <- list()
  
  for (i in seq_len(nrow(new_matches))) {
    match <- new_matches[i, ]
    
    message(sprintf("\n  [%d/%d] %s vs %s (%s)", 
                    i, nrow(new_matches),
                    match$home_team, match$away_team,
                    substr(match$datetime, 1, 10)))
    
    # Scrape player data
    player_data <- tryCatch({
      understat_match_players(match$match_url)
    }, error = function(e) {
      message(sprintf("    [ERROR] %s", e$message))
      NULL
    })
    
    if (!is.null(player_data) && nrow(player_data) > 0) {
      # Add match metadata
      player_data <- player_data %>%
        mutate(
          league = league,
          season = SEASON_DISPLAY,
          match_date = match$datetime,
          home_team = match$home_team,
          away_team = match$away_team,
          home_goals = as.character(match$home_goals),
          away_goals = as.character(match$away_goals),
          match_url = match$match_url
        )
      
      batch_players[[length(batch_players) + 1]] <- player_data
      
      # Prepare match info
      match_row <- match %>%
        mutate(
          league = league,
          season = SEASON_DISPLAY
        ) %>%
        select(league, season, match_id, datetime, 
               home_team, away_team, home_goals, away_goals,
               home_xG, away_xG, match_url)
      batch_matches[[length(batch_matches) + 1]] <- match_row
      
      processed <- processed + 1
      message(sprintf("    [OK] %d players extracted", nrow(player_data)))
    } else {
      failed <- failed + 1
      message("    [FAIL] No player data")
    }
    
    # Save batch
    if (length(batch_players) >= BATCH_SIZE || i == nrow(new_matches)) {
      if (length(batch_players) > 0) {
        message("\n  === SAVING BATCH ===")
        
        players_df <- bind_rows(batch_players)
        matches_df <- bind_rows(batch_matches)
        
        if (!is.null(sheet_id_players)) {
          write_to_google_sheet(
            players_df,
            sheet_id_players,
            SHEETS_CONFIG$player_match_stats$sheet_name,
            SCHEMA_PLAYER_MATCH,
            "player stats"
          )
        }
        
        if (!is.null(sheet_id_matches)) {
          write_to_google_sheet(
            matches_df,
            sheet_id_matches,
            SHEETS_CONFIG$match_info$sheet_name,
            SCHEMA_MATCHES,
            "match info"
          )
        }
        
        save_local_backup(players_df, "understat_players", league)
        
        batch_players <- list()
        batch_matches <- list()
      }
    }
  }
  
  message(sprintf("\n  League complete: %d processed, %d failed", processed, failed))
  return(list(processed = processed, failed = failed))
}

# =============================================================================
# MAIN ENTRY POINTS
# =============================================================================

main <- function(leagues = LEAGUES_TO_SCRAPE, 
                 sheet_id_players = NULL,
                 sheet_id_matches = NULL) {
  
  message("================================================================================")
  message("       UNDERSTAT BATCH SCRAPER")
  message("================================================================================")
  message(sprintf("Season: %s", SEASON_DISPLAY))
  message(sprintf("Leagues: %s", paste(leagues, collapse = ", ")))
  message(sprintf("Batch Size: %d matches", BATCH_SIZE))
  message("================================================================================\n")
  
  init_backup_dir()
  
  message("Authenticating with Google...")
  tryCatch({
    gs4_auth()
    if (EXPORT_PARQUET_TO_DRIVE) {
      drive_auth()
    }
    message("[OK] Authenticated\n")
  }, error = function(e) {
    stop("Authentication failed: ", e$message)
  })
  
  if (is.null(sheet_id_players)) {
    message("Creating new Google Sheet for player data...")
    sheet_id_players <- gs4_create(sprintf("Understat_Player_Stats_%s", SEASON_DISPLAY))
    message(sprintf("[OK] Created sheet ID: %s", sheet_id_players))
  }
  
  if (is.null(sheet_id_matches)) {
    message("Creating new Google Sheet for match data...")
    sheet_id_matches <- gs4_create(sprintf("Understat_Matches_%s", SEASON_DISPLAY))
    message(sprintf("[OK] Created sheet ID: %s", sheet_id_matches))
  }
  
  results <- list()
  
  for (league in leagues) {
    result <- tryCatch({
      process_league(league, SEASON, sheet_id_players, sheet_id_matches)
    }, error = function(e) {
      message(sprintf("  [ERROR] League failed: %s", e$message))
      list(processed = 0, failed = 0)
    })
    
    results[[league]] <- result
    
    if (league != leagues[length(leagues)]) {
      message("\n  Waiting before next league...")
      Sys.sleep(10)
    }
  }
  
  message("\n================================================================================")
  message("                              SCRAPE COMPLETE")
  message("================================================================================")
  
  total_processed <- sum(sapply(results, function(x) x$processed))
  total_failed <- sum(sapply(results, function(x) x$failed))
  
  for (league in names(results)) {
    r <- results[[league]]
    message(sprintf("  %s: %d processed, %d failed", league, r$processed, r$failed))
  }
  
  message(sprintf("\n  TOTAL: %d processed, %d failed", total_processed, total_failed))
  message("================================================================================")
  
  if (EXPORT_PARQUET_TO_DRIVE && total_processed > 0) {
    message("\nExporting combined data to Parquet...")
    
    tryCatch({
      all_data <- read_sheet(sheet_id_players, 
                             sheet = SHEETS_CONFIG$player_match_stats$sheet_name,
                             col_types = "c")
      export_to_parquet(all_data, "understat_player_match_stats.parquet")
    }, error = function(e) {
      message(sprintf("[WARN] Parquet export failed: %s", e$message))
    })
  }
  
  return(list(
    sheet_id_players = sheet_id_players,
    sheet_id_matches = sheet_id_matches,
    results = results
  ))
}

run_epl_only <- function(sheet_id_players = NULL, sheet_id_matches = NULL) {
  main(leagues = c("EPL"), 
       sheet_id_players = sheet_id_players,
       sheet_id_matches = sheet_id_matches)
}

# =============================================================================
# TEST FUNCTION
# =============================================================================

test_scraper <- function() {
  message("================================================================================")
  message("  TESTING SCRAPER (without Google Sheets)")
  message("================================================================================\n")
  
  # Test getting matches
  message("1. Getting EPL match results...")
  matches <- understat_league_match_results("EPL", SEASON)
  
  if (is.null(matches) || nrow(matches) == 0) {
    message("[ERROR] Could not get matches")
    return(NULL)
  }
  message(sprintf("   [OK] Found %d matches\n", nrow(matches)))
  
  # Test getting player data for first match
  test_url <- sprintf("https://understat.com/match/%s", matches$match_id[1])
  message(sprintf("2. Getting player data for: %s vs %s", 
                  matches$home_team[1], matches$away_team[1]))
  message(sprintf("   URL: %s", test_url))
  
  players <- understat_match_players(test_url)
  
  if (is.null(players) || nrow(players) == 0) {
    message("[ERROR] Could not get player data")
    return(list(matches = matches, players = NULL))
  }
  
  message(sprintf("   [OK] Found %d players\n", nrow(players)))
  
  # Show sample
  message("3. Sample player:")
  p <- players[1, ]
  message(sprintf("   Name: %s", p$player))
  message(sprintf("   Position: %s", p$position))
  message(sprintf("   Minutes: %d", p$time_played))
  message(sprintf("   Goals: %d, xG: %.3f", p$goals, p$xG))
  message(sprintf("   Assists: %d, xA: %.3f", p$assists, p$xA))
  
  message("\n================================================================================")
  message("  TEST PASSED!")
  message("================================================================================")
  
  return(list(matches = matches, players = players))
}

message("Understat scraper loaded. Run test_scraper() to verify, or main() to start scraping.")