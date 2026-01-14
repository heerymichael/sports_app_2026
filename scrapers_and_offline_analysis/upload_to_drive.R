# =============================================================================
# Upload Parquet Files to Google Drive
# 
# Run this AFTER your scraper completes to upload fast-loading Parquet files
# OR add to the end of fb_combined_scraper.R
# =============================================================================

library(arrow)
library(googledrive)
library(googlesheets4)

# Your Drive folder ID
DRIVE_FOLDER_ID <- "1APlkMnjX3RjxPOzEnYWP5DYYCH_AcUM8"

# Sheet IDs (same as scraper config)
SHEET_IDS <- list(
  player_match_stats = "12MXPMsuI4S7EiTPnVpaqx5-riXoK37cEtPo-MFOf1fA",
  shots = "1oxQ6rk_B_r2QUNZxGssmDEopKqJD0otMt_r1hPOufT0",
  team_goals = "1gUCVxBFR3kwE259ZccWBLRn7iAlV0HpZL7OccbGsWuo"
)

#' Upload data frame to Google Drive as Parquet
#' @param data Data frame to upload
#' @param name File name (without extension)
#' @param folder_id Google Drive folder ID
#' @return Google Drive file ID
upload_parquet_to_drive <- function(data, name, folder_id = DRIVE_FOLDER_ID) {
  message(sprintf("Uploading %s.parquet to Google Drive...", name))
  
  # Write to temp file
  temp_file <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data, temp_file)
  
  file_size <- file.size(temp_file) / 1024 / 1024
  message(sprintf("  File size: %.2f MB", file_size))
  
  # Check if file already exists
  existing <- drive_find(
    q = sprintf("'%s' in parents and name = '%s.parquet'", folder_id, name),
    n_max = 1
  )
  
  if (nrow(existing) > 0) {
    # Update existing file
    result <- drive_update(existing$id, media = temp_file)
    message(sprintf("  [OK] Updated existing file"))
  } else {
    # Upload new file
    result <- drive_upload(
      temp_file,
      path = as_id(folder_id),
      name = paste0(name, ".parquet"),
      overwrite = FALSE
    )
    message(sprintf("  [OK] Created new file"))
  }
  
  # Make file publicly accessible (anyone with link can view)
  drive_share(result$id, role = "reader", type = "anyone")
  message(sprintf("  [OK] File ID: %s", result$id))
  
  # Clean up
  unlink(temp_file)
  
  return(result$id)
}

#' Export all data from Google Sheets to Google Drive as Parquet
#' Call this after your scraper completes
export_sheets_to_parquet <- function() {
  message("\n================================================================================")
  message("EXPORTING GOOGLE SHEETS TO PARQUET (for fast app loading)")
  message("================================================================================\n")
  
  # Authenticate
  message("Authenticating...")
  gs4_auth()
  drive_auth()
  message("[OK] Authenticated\n")
  
  file_ids <- list()
  
  # 1. Player Match Stats
  message("Reading Player_Match_Stats from Google Sheets...")
  player_data <- read_sheet(
    SHEET_IDS$player_match_stats,
    sheet = "Player_Match_Stats"
  ) %>% as.data.frame()
  message(sprintf("  [OK] %d rows", nrow(player_data)))
  file_ids$player_match_stats <- upload_parquet_to_drive(player_data, "player_match_stats")
  
  # 2. Shot Data  
  message("\nReading Shot_Data from Google Sheets...")
  shot_data <- read_sheet(
    SHEET_IDS$shots,
    sheet = "Shot_Data"
  ) %>% as.data.frame()
  message(sprintf("  [OK] %d rows", nrow(shot_data)))
  file_ids$shots <- upload_parquet_to_drive(shot_data, "shots")
  
  # 3. Team Goals
  message("\nReading Team_Goals from Google Sheets...")
  team_goals <- read_sheet(
    SHEET_IDS$team_goals,
    sheet = "Team_Goals"
  ) %>% as.data.frame()
  message(sprintf("  [OK] %d rows", nrow(team_goals)))
  file_ids$team_goals <- upload_parquet_to_drive(team_goals, "team_goals")
  
  # Print config to paste into soccer_config.R
  message("\n================================================================================")
  message("DONE! Copy this into soccer_config.R:")
  message("================================================================================\n")
  
  cat(sprintf('SOCCER_DRIVE_IDS <- list(
  player_match_stats = "%s",
  shots = "%s",
  team_goals = "%s"
)\n', file_ids$player_match_stats, file_ids$shots, file_ids$team_goals))
  
  message("\n================================================================================")
  message("Files uploaded to: https://drive.google.com/drive/folders/", DRIVE_FOLDER_ID)
  message("================================================================================\n")
  
  return(file_ids)
}

# =============================================================================
# RUN THIS AFTER YOUR SCRAPER
# =============================================================================

# Option 1: Run standalone after scraper finishes
# export_sheets_to_parquet()

# Option 2: Add this line to the END of fb_combined_scraper.R:
# source("upload_to_drive.R"); export_sheets_to_parquet()