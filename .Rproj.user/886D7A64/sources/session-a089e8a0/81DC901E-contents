# =============================================================================
# Player Headshots Helper
# Functions to get NFL player headshots and team colors
# =============================================================================

#' Get player headshots data from nflreadr
#' @return Tibble with player names, IDs, and headshot URLs
get_player_headshots <- function() {
  log_debug("get_player_headshots() called", level = "INFO")
  
  tryCatch({
    # Try current season first, then fall back to previous
    seasons_to_try <- c(2025, 2024)
    rosters <- NULL
    
    for (season in seasons_to_try) {
      log_debug("Trying to load rosters for season:", season, level = "INFO")
      
      rosters <- tryCatch({
        nflreadr::load_rosters(seasons = season)
      }, error = function(e) {
        log_debug("Failed to load season", season, ":", e$message, level = "DEBUG")
        NULL
      })
      
      if (!is.null(rosters) && nrow(rosters) > 0) {
        log_debug("Successfully loaded", nrow(rosters), "roster entries from season", season, level = "INFO")
        break
      }
    }
    
    # If still no data, try load_rosters() without season argument
    if (is.null(rosters) || nrow(rosters) == 0) {
      log_debug("Trying load_rosters() without season argument...", level = "INFO")
      rosters <- tryCatch({
        nflreadr::load_rosters()
      }, error = function(e) {
        log_debug("Failed:", e$message, level = "DEBUG")
        NULL
      })
    }
    
    if (is.null(rosters) || nrow(rosters) == 0) {
      log_debug("Could not load roster data from any season", level = "WARN")
      return(NULL)
    }
    
    log_debug("Roster columns available:", paste(names(rosters), collapse = ", "), level = "DEBUG")
    
    # Check which headshot column exists
    headshot_col <- if("headshot_url" %in% names(rosters)) {
      "headshot_url"
    } else if("headshot" %in% names(rosters)) {
      "headshot"
    } else if("headshot_nfl" %in% names(rosters)) {
      "headshot_nfl"
    } else if("espn_id" %in% names(rosters)) {
      # Build ESPN headshot URLs from espn_id
      log_debug("Building headshot URLs from espn_id", level = "INFO")
      rosters$headshot_url <- paste0(
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/",
        rosters$espn_id,
        ".png&w=350&h=254"
      )
      "headshot_url"
    } else {
      log_debug("No headshot column found. Available columns:", 
                paste(names(rosters), collapse = ", "), level = "WARN")
      return(NULL)
    }
    
    log_debug("Using headshot column:", headshot_col, level = "INFO")
    
    # Check for full_name column
    name_col <- if("full_name" %in% names(rosters)) {
      "full_name"
    } else if("player_name" %in% names(rosters)) {
      "player_name"
    } else if("name" %in% names(rosters)) {
      "name"
    } else {
      log_debug("No name column found in roster data", level = "WARN")
      return(NULL)
    }
    
    # Check for team column
    team_col <- if("team" %in% names(rosters)) {
      "team"
    } else if("team_abbr" %in% names(rosters)) {
      "team_abbr"
    } else {
      "team"
    }
    
    headshots <- rosters %>%
      select(
        player = all_of(name_col),
        team = any_of(team_col),
        headshot_url = all_of(headshot_col)
      ) %>%
      filter(!is.na(player), player != "") %>%
      distinct(player, .keep_all = TRUE)
    
    log_debug("Prepared headshots for", nrow(headshots), "unique players", level = "INFO")
    
    return(headshots)
    
  }, error = function(e) {
    log_debug("Error loading headshots:", e$message, level = "ERROR")
    return(NULL)
  })
}

#' NFL team colors (primary color for background fill)
nfl_team_colors <- tribble(
  ~team, ~color,
  "ARI", "#FFB3BA",
  "ATL", "#A50021",
  "BAL", "#241773",
  "BUF", "#00539B",
  "CAR", "#0085CA",
  "CHI", "#C83803",
  "CIN", "#FB4F14",
  "CLE", "#311D00",
  "DAL", "#041E42",
  "DEN", "#FB4F14",
  "DET", "#0076B6",
  "GB",  "#203731",
  "HOU", "#03202F",
  "IND", "#002C5F",
  "JAX", "#006778",
  "KC",  "#E31837",
  "LAC", "#0080C6",
  "LAR", "#003594",
  "LV",  "#000000",
  "MIA", "#008E97",
  "MIN", "#4F2683",
  "NE",  "#002244",
  "NO",  "#D3BC8D",
  "NYG", "#0B2265",
  "NYJ", "#125740",
  "PHI", "#004C54",
  "PIT", "#FFB612",
  "SF",  "#AA0000",
  "SEA", "#002244",
  "TB",  "#D50A0A",
  "TEN", "#0C2340",
  "WAS", "#5A1414"
)

# =============================================================================
# Headshot Name Corrections
# Maps YOUR projection/salary names -> nflreadr roster names
# Add entries here when headshots don't match due to naming differences
# =============================================================================
HEADSHOT_NAME_CORRECTIONS <- c(
  
  # Format: "Your Name" = "nflreadr Name"
  
  # Punctuation differences
  "Amon-Ra St Brown" = "Amon-Ra St. Brown",
  
  # Suffix differences (Jr, III, etc.)
  "Kenneth Walker" = "Kenneth Walker III",
  "Luther Burden" = "Luther Burden III",
  "Calvin Austin" = "Calvin Austin III",
  "Ollie Gordon" = "Ollie Gordon II",
  "Michael Pittman" = "Michael Pittman Jr.",
  "Marvin Harrison" = "Marvin Harrison Jr.",
  "Brian Robinson" = "Brian Robinson Jr.",
  "Odell Beckham" = "Odell Beckham Jr.",
  "Michael Thomas" = "Michael Thomas",
  "Travis Etienne" = "Travis Etienne Jr.",
  "Larry Rountree" = "Larry Rountree III",
  "Irv Smith" = "Irv Smith Jr.",
  "DJ Moore" = "D.J. Moore",
  "DJ Chark" = "D.J. Chark Jr.",
  "Harold Fannin" ~ "Harold Fannin Jr.",
  "Travis Etienne" ~ "Travis Etienne Jr.",
  "Cameron Ward" ~ "Cam Ward",
  "Tyrone Tracy" ~ "Tyrone Tracy Jr.",
  "Brian Thomas" ~ "Brian Thomas Jr.", 
  
  # Initial/name format differences
  "AJ Brown" = "A.J. Brown",
  "JK Dobbins" = "J.K. Dobbins",
  "TJ Hockenson" = "T.J. Hockenson",
  "DK Metcalf" = "D.K. Metcalf",
  "CJ Stroud" = "C.J. Stroud",
  "Demario Douglas" = "DeMario Douglas",
  "Kavontae Turpin" = "KaVontae Turpin",
  "Gabe Davis" = "Gabriel Davis",
  
  # Nickname differences
  "Marquise Brown" = "Marquise Brown",
  "Hollywood Brown" = "Marquise Brown"
)

#' Apply headshot name corrections to match nflreadr names
#' @param name Player name from your data
#' @return Corrected name for headshot matching
correct_name_for_headshot <- function(name) {
  if (name %in% names(HEADSHOT_NAME_CORRECTIONS)) {
    return(HEADSHOT_NAME_CORRECTIONS[[name]])
  }
  return(name)
}

#' Get lighter pastel version of team color for player card backgrounds
#' @param team Team abbreviation
#' @return Hex color code (lightened version)
get_team_bg_color <- function(team) {
  base_colors <- nfl_team_colors %>%
    deframe()
  
  base_color <- base_colors[team]
  
  # Return default gray if team not found or is NA
  if (is.na(team) || is.null(base_color) || is.na(base_color)) {
    return("#E0E0E0")
  }
  
  # Convert to RGB and lighten
  tryCatch({
    rgb_vals <- col2rgb(base_color)[,1]
    
    # Lighten by blending with white (70% lighter)
    lightened <- rgb_vals + (255 - rgb_vals) * 0.7
    
    return(rgb(lightened[1], lightened[2], lightened[3], maxColorValue = 255))
  }, error = function(e) {
    return("#E0E0E0")
  })
}

#' Join headshot data to player data
#' @param player_data Your player data frame with player names
#' @param headshots_data Headshot data from get_player_headshots()
#' @return Player data with headshot URLs and team colors added
add_headshot_info <- function(player_data, headshots_data = NULL) {
  log_debug("add_headshot_info() called with", nrow(player_data), "players", level = "INFO")
  
  if (is.null(headshots_data)) {
    log_debug("No headshots provided, fetching...", level = "INFO")
    headshots_data <- get_player_headshots()
  }
  
  if (is.null(headshots_data)) {
    log_debug("Could not get headshots, using defaults", level = "WARN")
    return(player_data %>%
             mutate(
               headshot_url = "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png",
               team_bg_color = "#E0E0E0"
             ))
  }
  
  # Apply name corrections for matching (keeps original display name)
  player_data_with_headshots <- player_data %>%
    mutate(player_match = sapply(player, correct_name_for_headshot)) %>%
    left_join(
      headshots_data %>% select(player, headshot_url) %>% rename(player_match = player), 
      by = "player_match"
    ) %>%
    select(-player_match) %>%
    rowwise() %>%
    mutate(
      team_bg_color = get_team_bg_color(team),
      headshot_url = if_else(
        is.na(headshot_url) | headshot_url == "",
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png",
        headshot_url
      )
    ) %>%
    ungroup()
  
  matched <- sum(!is.na(player_data_with_headshots$headshot_url) & 
                   player_data_with_headshots$headshot_url != "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png")
  log_debug("Matched headshots for", matched, "of", nrow(player_data_with_headshots), "players", level = "INFO")
  
  # Log unmatched players for debugging
  unmatched <- player_data_with_headshots %>%
    filter(headshot_url == "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png") %>%
    filter(position != "DST") %>%  # DST won't have headshots
    pull(player)
  
  if (length(unmatched) > 0 && length(unmatched) <= 20) {
    log_debug("Unmatched players:", paste(unmatched, collapse = ", "), level = "DEBUG")
  } else if (length(unmatched) > 20) {
    log_debug("Unmatched players:", length(unmatched), "(too many to list)", level = "DEBUG")
  }
  
  return(player_data_with_headshots)
}

#' Apply player name mapping for corrections
#' @param player_data Your player data
#' @param mapping_file Path to completed mapping CSV
#' @return Player data with corrected names
apply_player_mapping <- function(player_data, mapping_file = "data/player_name_mapping.csv") {
  log_debug("apply_player_mapping() checking for:", mapping_file, level = "DEBUG")
  
  if (!file.exists(mapping_file)) {
    log_debug("No mapping file found, using original names", level = "DEBUG")
    return(player_data)
  }
  
  mapping <- read_csv(mapping_file, show_col_types = FALSE) %>%
    filter(!is.na(correct_name), correct_name != "")
  
  if (nrow(mapping) == 0) {
    log_debug("Mapping file empty, using original names", level = "DEBUG")
    return(player_data)
  }
  
  log_debug("Applying", nrow(mapping), "name corrections", level = "INFO")
  
  player_data_corrected <- player_data %>%
    left_join(mapping %>% select(player, correct_name), by = "player") %>%
    mutate(player = if_else(!is.na(correct_name), correct_name, player)) %>%
    select(-correct_name)
  
  return(player_data_corrected)
}