################################################################################
#
#                    UNDERSTAT SCRAPER - FULL AUTOMATED
#
# Scrapes all leagues from Understat via Python understatapi package
# Writes to Google Sheets (incremental) AND Google Drive (parquet)
#
# USAGE:
#   source("scrape_understat_final.R")
#   # Script runs automatically on source
#
################################################################################

library(reticulate)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(purrr)
library(arrow)

# =============================================================================
# CONFIGURATION
# =============================================================================

SEASON <- "2025"
SEASON_DISPLAY <- "2025-26"

# All leagues to scrape
LEAGUES <- c(
  "EPL" = "EPL",
  "La_Liga" = "La Liga",
  "Bundesliga" = "Bundesliga", 
  "Serie_A" = "Serie A",
  "Ligue_1" = "Ligue 1"
)

# Google Sheet IDs
SHEETS <- list(
  match_results = "124WRVrrPyRu32pqzYrJLRCG5NiFaJYhBfd108OOUvhQ",
  player_stats = "1aQJe29RpMCpq14p_J2aInulpzHIjEleml2B8p56InI4",
  player_match_stats = "1IQFKATmpgLiK7aVbkfgATi_uJBKjt4xSRX065LukHYA",
  shots = "1QHXvdEOOJv2Bjiecvk7ZkOWp-THk4Pzy4IgyttxuPHo"
)

# Google Drive folder for parquet files
GDRIVE_FOLDER_ID <- "1APlkMnjX3RjxPOzEnYWP5DYYCH_AcUM8"

# Local temp directory for parquet (before upload)
PARQUET_TEMP_DIR <- tempdir()

# Rate limiting (seconds between API calls)
RATE_LIMIT <- 2

# =============================================================================
# PYTHON CLIENT
# =============================================================================

.understat_client <- NULL

.init_understat <- function() {
  if (is.null(.understat_client)) {
    if (!py_module_available("understatapi")) {
      message("Installing understatapi...")
      py_install("understatapi", pip = TRUE)
    }
    understatapi <- import("understatapi")
    .understat_client <<- understatapi$UnderstatClient()
  }
  return(.understat_client)
}

# =============================================================================
# ROBUST DATA CONVERSION
# =============================================================================

.to_dataframe_safe <- function(data) {
  if (length(data) == 0) return(data.frame())
  
  df_list <- lapply(seq_along(data), function(i) {
    record <- data[[i]]
    
    clean_record <- lapply(names(record), function(key) {
      val <- record[[key]]
      if (is.null(val) || length(val) == 0) {
        return(NA)
      } else if (is.list(val)) {
        return(as.character(val))
      } else {
        return(val)
      }
    })
    names(clean_record) <- names(record)
    
    tryCatch({
      as.data.frame(clean_record, stringsAsFactors = FALSE)
    }, error = function(e) {
      NULL
    })
  })
  
  df_list <- df_list[!sapply(df_list, is.null)]
  if (length(df_list) == 0) return(data.frame())
  
  bind_rows(df_list)
}

# =============================================================================
# DATA EXTRACTION HELPERS
# =============================================================================

.get_league_players <- function(league_code, season) {
  client <- .init_understat()
  Sys.sleep(RATE_LIMIT)
  
  tryCatch({
    data <- client$league(league = league_code)$get_player_data(season = season)
    df <- .to_dataframe_safe(data)
    
    if (nrow(df) > 0) {
      numeric_cols <- c("games", "time", "goals", "xG", "assists", "xA", 
                        "shots", "key_passes", "yellow_cards", "red_cards",
                        "npg", "npxG", "xGChain", "xGBuildup")
      for (col in numeric_cols) {
        if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
      }
    }
    return(df)
  }, error = function(e) {
    warning(sprintf("Failed to get players for %s: %s", league_code, e$message))
    return(data.frame())
  })
}

.flatten_match <- function(match) {
  goals_h <- if (!is.null(match$goals$h)) as.numeric(match$goals$h) else NA
  goals_a <- if (!is.null(match$goals$a)) as.numeric(match$goals$a) else NA
  xG_h <- if (!is.null(match$xG$h) && match$xG$h != "") as.numeric(match$xG$h) else NA
  xG_a <- if (!is.null(match$xG$a) && match$xG$a != "") as.numeric(match$xG$a) else NA
  forecast_w <- if (!is.null(match$forecast$w)) as.numeric(match$forecast$w) else NA
  forecast_d <- if (!is.null(match$forecast$d)) as.numeric(match$forecast$d) else NA
  forecast_l <- if (!is.null(match$forecast$l)) as.numeric(match$forecast$l) else NA
  
  list(
    id = match$id,
    isResult = match$isResult,
    datetime = match$datetime,
    h_id = match$h$id,
    h_title = match$h$title,
    h_short = match$h$short_title,
    a_id = match$a$id,
    a_title = match$a$title,
    a_short = match$a$short_title,
    goals_h = goals_h,
    goals_a = goals_a,
    xG_h = xG_h,
    xG_a = xG_a,
    forecast_w = forecast_w,
    forecast_d = forecast_d,
    forecast_l = forecast_l
  )
}

.get_league_matches <- function(league_code, season) {
  client <- .init_understat()
  Sys.sleep(RATE_LIMIT)
  
  tryCatch({
    data <- client$league(league = league_code)$get_match_data(season = season)
    
    if (length(data) == 0) return(data.frame())
    
    df_list <- lapply(data, function(m) {
      tryCatch({
        as.data.frame(.flatten_match(m), stringsAsFactors = FALSE)
      }, error = function(e) NULL)
    })
    
    df_list <- df_list[!sapply(df_list, is.null)]
    if (length(df_list) == 0) return(data.frame())
    
    bind_rows(df_list)
  }, error = function(e) {
    warning(sprintf("Failed to get matches for %s: %s", league_code, e$message))
    return(data.frame())
  })
}

.get_player_matches <- function(player_id) {
  client <- .init_understat()
  Sys.sleep(RATE_LIMIT)
  
  tryCatch({
    data <- client$player(player = as.character(player_id))$get_match_data()
    df <- .to_dataframe_safe(data)
    
    if (nrow(df) > 0) {
      numeric_cols <- c("time", "goals", "xG", "assists", "xA", "shots", 
                        "key_passes", "xGChain", "xGBuildup")
      for (col in numeric_cols) {
        if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
      }
      df$player_id <- player_id
    }
    return(df)
  }, error = function(e) {
    warning(sprintf("Failed to get matches for player %s: %s", player_id, e$message))
    return(data.frame())
  })
}

.get_match_shots <- function(match_id) {
  client <- .init_understat()
  Sys.sleep(RATE_LIMIT)
  
  tryCatch({
    data <- client$match(match = as.character(match_id))$get_shot_data()
    
    # Shots are split by h (home) and a (away) - combine them
    all_shots <- c(data$h, data$a)
    
    if (length(all_shots) == 0) return(data.frame())
    
    df <- .to_dataframe_safe(all_shots)
    
    if (nrow(df) > 0) {
      numeric_cols <- c("minute", "xG", "X", "Y")
      for (col in numeric_cols) {
        if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
      }
    }
    return(df)
  }, error = function(e) {
    warning(sprintf("Failed to get shots for match %s: %s", match_id, e$message))
    return(data.frame())
  })
}

# =============================================================================
# SHEET HELPERS
# =============================================================================

.read_existing <- function(sheet_id) {
  tryCatch({
    df <- read_sheet(sheet_id, sheet = 1)
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    return(as.data.frame(df))
  }, error = function(e) {
    message(sprintf("  Sheet empty or new: %s", e$message))
    return(data.frame())
  })
}

.get_existing_ids <- function(sheet_id, id_col = "id") {
  existing <- .read_existing(sheet_id)
  if (nrow(existing) == 0 || !id_col %in% names(existing)) {
    return(character(0))
  }
  return(as.character(existing[[id_col]]))
}

# =============================================================================
# GOOGLE DRIVE PARQUET HELPERS
# =============================================================================

.upload_parquet <- function(df, filename) {
  temp_path <- file.path(PARQUET_TEMP_DIR, filename)
  write_parquet(df, temp_path)
  
  existing <- drive_find(
    pattern = paste0("^", filename, "$"),
    q = sprintf("'%s' in parents", GDRIVE_FOLDER_ID),
    n_max = 1
  )
  
  if (nrow(existing) > 0) {
    drive_update(existing$id[1], media = temp_path)
    message(sprintf("[OK] Parquet updated on Drive: %s (%d rows)", filename, nrow(df)))
  } else {
    drive_upload(
      temp_path,
      path = as_id(GDRIVE_FOLDER_ID),
      name = filename,
      overwrite = FALSE
    )
    message(sprintf("[OK] Parquet uploaded to Drive: %s (%d rows)", filename, nrow(df)))
  }
  
  unlink(temp_path)
}

.read_parquet_from_drive <- function(filename) {
  existing <- drive_find(
    pattern = paste0("^", filename, "$"),
    q = sprintf("'%s' in parents", GDRIVE_FOLDER_ID),
    n_max = 1
  )
  
  if (nrow(existing) == 0) {
    return(data.frame())
  }
  
  temp_path <- file.path(PARQUET_TEMP_DIR, filename)
  drive_download(existing$id[1], path = temp_path, overwrite = TRUE)
  
  df <- read_parquet(temp_path)
  unlink(temp_path)
  return(as.data.frame(df))
}

# =============================================================================
# SCRAPER FUNCTIONS
# =============================================================================

scrape_player_stats <- function(season = SEASON) {
  message("\n========================================")
  message("  SCRAPING PLAYER STATS (overwrite)")
  message("========================================")
  
  all_data <- list()
  
  for (league_code in names(LEAGUES)) {
    league_name <- LEAGUES[[league_code]]
    message(sprintf("\n[%s] Fetching player stats...", league_name))
    
    df <- .get_league_players(league_code, season)
    
    if (nrow(df) > 0) {
      df$league <- league_name
      df$season <- SEASON_DISPLAY
      df$scrape_date <- Sys.Date()
      all_data[[league_code]] <- df
      message(sprintf("[%s] Got %d players", league_name, nrow(df)))
    }
  }
  
  if (length(all_data) > 0) {
    combined <- bind_rows(all_data)
    message(sprintf("\nTotal: %d players", nrow(combined)))
    
    col_order <- c(
      "id", "player_name", "team_title", "league", "season",
      "games", "time", "position",
      "goals", "assists", "shots", "key_passes",
      "xG", "xA", "npg", "npxG", "xGChain", "xGBuildup",
      "yellow_cards", "red_cards",
      "scrape_date"
    )
    existing_cols <- names(combined)
    ordered_cols <- c(col_order[col_order %in% existing_cols], 
                      setdiff(existing_cols, col_order))
    combined <- combined[, ordered_cols]
    
    message("Writing to Google Sheets (overwrite)...")
    write_sheet(combined, ss = SHEETS$player_stats, sheet = 1)
    message("[OK] Google Sheets done")
    
    .upload_parquet(combined, "player_stats.parquet")
    
    return(combined)
  }
  return(data.frame())
}

scrape_match_results <- function(season = SEASON) {
  message("\n========================================")
  message("  SCRAPING MATCH RESULTS (incremental)")
  message("========================================")
  
  message("Reading existing data...")
  existing_ids <- .get_existing_ids(SHEETS$match_results, "id")
  message(sprintf("Found %d existing matches in sheet", length(existing_ids)))
  
  all_data <- list()
  
  for (league_code in names(LEAGUES)) {
    league_name <- LEAGUES[[league_code]]
    message(sprintf("\n[%s] Fetching match results...", league_name))
    
    df <- .get_league_matches(league_code, season)
    
    if (nrow(df) > 0) {
      if ("isResult" %in% names(df)) {
        df <- df %>% filter(isResult == TRUE | isResult == "True" | isResult == "true")
      }
      
      if (nrow(df) > 0) {
        df$id <- as.character(df$id)
        new_df <- df %>% filter(!id %in% existing_ids)
        
        if (nrow(new_df) > 0) {
          new_df$league <- league_name
          new_df$season <- SEASON_DISPLAY
          new_df$scrape_date <- Sys.Date()
          all_data[[league_code]] <- new_df
          message(sprintf("[%s] Got %d NEW matches (skipped %d existing)", 
                          league_name, nrow(new_df), nrow(df) - nrow(new_df)))
        } else {
          message(sprintf("[%s] No new matches (all %d already in sheet)", league_name, nrow(df)))
        }
      } else {
        message(sprintf("[%s] No completed matches found", league_name))
      }
    } else {
      message(sprintf("[%s] No match data returned", league_name))
    }
  }
  
  if (length(all_data) > 0) {
    combined <- bind_rows(all_data)
    message(sprintf("\nTotal NEW: %d matches", nrow(combined)))
    
    col_order <- c(
      "id", "league", "season", "datetime", "isResult",
      "h_id", "h_title", "h_short",
      "a_id", "a_title", "a_short",
      "goals_h", "goals_a", "xG_h", "xG_a",
      "forecast_w", "forecast_d", "forecast_l",
      "scrape_date"
    )
    existing_cols <- names(combined)
    ordered_cols <- c(col_order[col_order %in% existing_cols], 
                      setdiff(existing_cols, col_order))
    combined <- combined[, ordered_cols]
    
    if (length(existing_ids) == 0) {
      message("Writing to Google Sheets (new sheet)...")
      write_sheet(combined, ss = SHEETS$match_results, sheet = 1)
    } else {
      message("Appending to Google Sheets...")
      sheet_append(ss = SHEETS$match_results, data = combined, sheet = 1)
    }
    message("[OK] Google Sheets done")
    
    existing_parquet <- .read_parquet_from_drive("match_results.parquet")
    if (nrow(existing_parquet) > 0) {
      combined_parquet <- bind_rows(existing_parquet, combined)
    } else {
      combined_parquet <- combined
    }
    .upload_parquet(combined_parquet, "match_results.parquet")
    
    return(combined)
  } else {
    message("\nNo new matches to add")
  }
  return(data.frame())
}

scrape_player_match_stats <- function(season = SEASON) {
  message("\n========================================")
  message("  SCRAPING PLAYER MATCH STATS (incremental)")
  message("========================================")
  message("This will take a while...")
  
  message("Reading existing data...")
  existing <- .read_existing(SHEETS$player_match_stats)
  
  existing_keys <- character(0)
  if (nrow(existing) > 0 && "player_id" %in% names(existing) && "id" %in% names(existing)) {
    existing_keys <- paste(existing$player_id, existing$id, sep = "_")
  }
  message(sprintf("Found %d existing player-match records", length(existing_keys)))
  
  all_data <- list()
  
  for (league_code in names(LEAGUES)) {
    league_name <- LEAGUES[[league_code]]
    message(sprintf("\n[%s] Getting player list...", league_name))
    
    players <- .get_league_players(league_code, season)
    if (nrow(players) == 0) next
    
    message(sprintf("[%s] Fetching match stats for %d players...", league_name, nrow(players)))
    
    league_data <- list()
    skipped <- 0
    added <- 0
    
    for (i in seq_len(nrow(players))) {
      player_id <- players$id[i]
      player_name <- players$player_name[i]
      
      if (i %% 50 == 0 || i == nrow(players)) {
        message(sprintf("  Progress: %d/%d (added: %d, skipped: %d)", 
                        i, nrow(players), added, skipped))
      }
      
      df <- .get_player_matches(player_id)
      
      if (nrow(df) > 0) {
        if ("season" %in% names(df)) {
          target_season <- as.numeric(gsub("-.*", "", SEASON_DISPLAY))
          df <- df %>% filter(as.numeric(season) == target_season)
        }
        
        if (nrow(df) > 0) {
          df$player_id <- player_id
          df$composite_key <- paste(df$player_id, df$id, sep = "_")
          
          new_df <- df %>% filter(!composite_key %in% existing_keys)
          
          if (nrow(new_df) > 0) {
            new_df$player_name <- player_name
            new_df$league <- league_name
            new_df$composite_key <- NULL
            league_data[[as.character(player_id)]] <- new_df
            added <- added + nrow(new_df)
          }
          skipped <- skipped + (nrow(df) - nrow(new_df))
        }
      }
    }
    
    if (length(league_data) > 0) {
      all_data[[league_code]] <- bind_rows(league_data)
      message(sprintf("[%s] Got %d NEW records for %d players", 
                      league_name, nrow(all_data[[league_code]]), length(league_data)))
    }
  }
  
  if (length(all_data) > 0) {
    combined <- bind_rows(all_data)
    combined$scrape_date <- Sys.Date()
    
    col_order <- c(
      "player_id", "player_name", "league", "season",
      "date", "id", "h_team", "a_team", "h_goals", "a_goals",
      "position", "time", "roster_id",
      "goals", "assists", "shots", "key_passes",
      "xG", "xA", "npg", "npxG", "xGChain", "xGBuildup",
      "scrape_date"
    )
    existing_cols <- names(combined)
    ordered_cols <- c(col_order[col_order %in% existing_cols], 
                      setdiff(existing_cols, col_order))
    combined <- combined[, ordered_cols]
    
    message(sprintf("\nTotal NEW: %d player-match records", nrow(combined)))
    
    if (length(existing_keys) == 0) {
      message("Writing to Google Sheets (new sheet)...")
      write_sheet(combined, ss = SHEETS$player_match_stats, sheet = 1)
    } else {
      message("Appending to Google Sheets...")
      sheet_append(ss = SHEETS$player_match_stats, data = combined, sheet = 1)
    }
    message("[OK] Google Sheets done")
    
    existing_parquet <- .read_parquet_from_drive("player_match_stats.parquet")
    if (nrow(existing_parquet) > 0) {
      combined_parquet <- bind_rows(existing_parquet, combined)
    } else {
      combined_parquet <- combined
    }
    .upload_parquet(combined_parquet, "player_match_stats.parquet")
    
    return(combined)
  } else {
    message("\nNo new player-match records to add")
  }
  return(data.frame())
}

scrape_shots <- function(season = SEASON) {
  message("\n========================================")
  message("  SCRAPING SHOT DATA (incremental)")
  message("========================================")
  message("This will take a while...")
  
  message("Reading existing data...")
  existing <- .read_existing(SHEETS$shots)
  
  existing_match_ids <- character(0)
  if (nrow(existing) > 0 && "match_id" %in% names(existing)) {
    existing_match_ids <- unique(as.character(existing$match_id))
  }
  message(sprintf("Found shots from %d existing matches", length(existing_match_ids)))
  
  all_data <- list()
  
  for (league_code in names(LEAGUES)) {
    league_name <- LEAGUES[[league_code]]
    message(sprintf("\n[%s] Getting match list...", league_name))
    
    matches <- .get_league_matches(league_code, season)
    if (nrow(matches) == 0) {
      message(sprintf("[%s] No matches found", league_name))
      next
    }
    
    if ("isResult" %in% names(matches)) {
      matches <- matches %>% filter(isResult == TRUE | isResult == "True" | isResult == "true")
    }
    
    if (nrow(matches) == 0) {
      message(sprintf("[%s] No completed matches", league_name))
      next
    }
    
    matches$id <- as.character(matches$id)
    new_matches <- matches %>% filter(!id %in% existing_match_ids)
    
    if (nrow(new_matches) == 0) {
      message(sprintf("[%s] No new matches to scrape (all %d already done)", 
                      league_name, nrow(matches)))
      next
    }
    
    message(sprintf("[%s] Fetching shots for %d NEW matches (skipping %d existing)...", 
                    league_name, nrow(new_matches), nrow(matches) - nrow(new_matches)))
    
    league_shots <- list()
    
    for (i in seq_len(nrow(new_matches))) {
      match_id <- new_matches$id[i]
      
      if (i %% 25 == 0 || i == nrow(new_matches)) {
        message(sprintf("  Progress: %d/%d", i, nrow(new_matches)))
      }
      
      df <- .get_match_shots(match_id)
      
      if (nrow(df) > 0) {
        df$league <- league_name
        df$match_id <- match_id
        league_shots[[as.character(match_id)]] <- df
      }
    }
    
    if (length(league_shots) > 0) {
      all_data[[league_code]] <- bind_rows(league_shots)
      message(sprintf("[%s] Got %d shots from %d matches", 
                      league_name, nrow(all_data[[league_code]]), length(league_shots)))
    }
  }
  
  if (length(all_data) > 0) {
    combined <- bind_rows(all_data)
    combined$season <- SEASON_DISPLAY
    combined$scrape_date <- Sys.Date()
    message(sprintf("\nTotal NEW: %d shots", nrow(combined)))
    
    col_order <- c(
      "id", "match_id", "league", "season",
      "h_team", "a_team", "h_goals", "a_goals", "h_a",
      "player_id", "player",
      "minute", "situation", "shotType", "result",
      "X", "Y", "xG",
      "lastAction",
      "scrape_date"
    )
    existing_cols <- names(combined)
    ordered_cols <- c(col_order[col_order %in% existing_cols], 
                      setdiff(existing_cols, col_order))
    combined <- combined[, ordered_cols]
    
    if (length(existing_match_ids) == 0) {
      message("Writing to Google Sheets (new sheet)...")
      write_sheet(combined, ss = SHEETS$shots, sheet = 1)
    } else {
      message("Appending to Google Sheets...")
      sheet_append(ss = SHEETS$shots, data = combined, sheet = 1)
    }
    message("[OK] Google Sheets done")
    
    existing_parquet <- .read_parquet_from_drive("shots.parquet")
    if (nrow(existing_parquet) > 0) {
      combined_parquet <- bind_rows(existing_parquet, combined)
    } else {
      combined_parquet <- combined
    }
    .upload_parquet(combined_parquet, "shots.parquet")
    
    return(combined)
  } else {
    message("\nNo new shots to add")
  }
  return(data.frame())
}

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

scrape_all <- function() {
  message("\n================================================================================")
  message("       UNDERSTAT SCRAPER - FULL AUTOMATED")
  message("================================================================================")
  message(sprintf("Season: %s", SEASON_DISPLAY))
  message(sprintf("Leagues: %s", paste(LEAGUES, collapse = ", ")))
  message(sprintf("Time: %s", Sys.time()))
  message("================================================================================")
  message("")
  message("Output:")
  message("  - Google Sheets (incremental)")
  message(sprintf("  - Parquet files: https://drive.google.com/drive/folders/%s", GDRIVE_FOLDER_ID))
  message("")
  message("Mode:")
  message("  - player_stats:       OVERWRITE (totals change)")
  message("  - match_results:      APPEND new matches only")
  message("  - player_match_stats: APPEND new records only")
  message("  - shots:              APPEND new matches only")
  message("")
  message("================================================================================\n")
  
  # Authenticate
  message("Authenticating with Google...")
  gs4_auth()
  drive_auth()
  message("[OK] Authenticated\n")
  
  results <- list()
  
  # Run all scrapers
  results$player_stats <- scrape_player_stats()
  results$match_results <- scrape_match_results()
  results$player_match_stats <- scrape_player_match_stats()
  results$shots <- scrape_shots()
  
  # Summary
  message("\n================================================================================")
  message("  COMPLETE")
  message("================================================================================")
  message(sprintf("Player stats:       %d rows (overwritten)", nrow(results$player_stats)))
  message(sprintf("Match results:      %d rows (new)", nrow(results$match_results)))
  message(sprintf("Player match stats: %d rows (new)", nrow(results$player_match_stats)))
  message(sprintf("Shots:              %d rows (new)", nrow(results$shots)))
  message("")
  message("Google Sheets:")
  message(sprintf("  Player stats:       https://docs.google.com/spreadsheets/d/%s", SHEETS$player_stats))
  message(sprintf("  Match results:      https://docs.google.com/spreadsheets/d/%s", SHEETS$match_results))
  message(sprintf("  Player match stats: https://docs.google.com/spreadsheets/d/%s", SHEETS$player_match_stats))
  message(sprintf("  Shots:              https://docs.google.com/spreadsheets/d/%s", SHEETS$shots))
  message("")
  message(sprintf("Parquet files: https://drive.google.com/drive/folders/%s", GDRIVE_FOLDER_ID))
  message("================================================================================")
  
  return(results)
}

# =============================================================================
# AUTO-RUN ON SOURCE
# =============================================================================

message("================================================================================")
message("       UNDERSTAT SCRAPER - STARTING AUTOMATICALLY")
message("================================================================================\n")

# Run the full scraper
results <- scrape_all()

message("\n[DONE] Scraping complete!")