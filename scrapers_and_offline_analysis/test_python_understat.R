################################################################################
#           TEST PYTHON UNDERSTATAPI FROM R
################################################################################

# Install reticulate if needed
if (!require("reticulate", quietly = TRUE)) {
  install.packages("reticulate")
}
library(reticulate)

# Check if Python is available
message("1. Checking Python installation...")
py_available <- tryCatch({
  py_config()
  TRUE
}, error = function(e) {
  
  FALSE
})

if (!py_available) {
  message("   No Python found. Installing miniconda...")
  install_miniconda()
}

message(sprintf("   Python: %s", py_config()$python))

# Install understatapi
message("\n2. Installing understatapi...")
tryCatch({
  py_install("understatapi", pip = TRUE)
  message("   [OK] Installed")
}, error = function(e) {
  message(sprintf("   [WARN] %s", e$message))
})

# Test the API
message("\n3. Testing understatapi...")

tryCatch({
  # Import the module
  understat <- import("understatapi")
  
  # Create client and test
  client <- understat$UnderstatClient()
  
  # Try to get EPL player data for 2024
  message("   Fetching EPL 2024 player data...")
  player_data <- client$league(league = "EPL")$get_player_data(season = "2024")
  
  # Convert to R data frame
  df <- as.data.frame(do.call(rbind, lapply(player_data, as.data.frame)))
  
  message(sprintf("   [OK] Got %d players", nrow(df)))
  message("\n   Top 5 by xG:")
  
  df$xG <- as.numeric(df$xG)
  top5 <- head(df[order(-df$xG), c("player_name", "team_title", "xG")], 5)
  print(top5)
  
}, error = function(e) {
  message(sprintf("   [FAIL] %s", e$message))
  message("\n   understatapi is also broken by the December 2025 changes.")
})

message("\n================================================================================")
message("  TEST COMPLETE")
message("================================================================================")