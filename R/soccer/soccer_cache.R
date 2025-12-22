# =============================================================================
# Soccer Cache Management
# 
# Functions for caching soccer data to reduce Google Sheets API calls
# Extracted from soccer_data_loader.R for maintainability
# =============================================================================

# Note: CACHE_DIR and CACHE_MAX_AGE_HOURS are defined in soccer_config.R

# =============================================================================
# CACHE INFRASTRUCTURE
# =============================================================================

#' Ensure cache directory exists
ensure_cache_dir <- function() {
  if (!dir.exists(CACHE_DIR)) {
    dir.create(CACHE_DIR, recursive = TRUE)
    log_debug("Created cache directory:", CACHE_DIR, level = "INFO")
  }
}

#' Get cache file path for a dataset
#' @param dataset_name Name of the dataset (e.g., "shooting", "possession", "shots")
#' @return File path for the cached RDS file
get_cache_path <- function(dataset_name) {
  ensure_cache_dir()
  file.path(CACHE_DIR, paste0("soccer_", dataset_name, ".rds"))
}

#' Check if cache is valid (exists and not expired)
#' @param cache_path Path to cache file
#' @param max_age_hours Maximum age in hours before cache is considered stale
#' @return TRUE if cache is valid, FALSE otherwise
is_cache_valid <- function(cache_path, max_age_hours = CACHE_MAX_AGE_HOURS) {
  if (!file.exists(cache_path)) {
    log_debug("Cache file does not exist:", cache_path, level = "DEBUG")
    return(FALSE)
  }
  
  file_age_hours <- as.numeric(difftime(Sys.time(), file.mtime(cache_path), units = "hours"))
  is_valid <- file_age_hours < max_age_hours
  
  log_debug(sprintf("Cache age: %.1f hours, max: %d hours, valid: %s", 
                    file_age_hours, max_age_hours, is_valid), level = "DEBUG")
  
  return(is_valid)
}

#' Get cache age as human-readable string
#' @param cache_path Path to cache file
#' @return String like "5 minutes ago" or "2 hours ago"
get_cache_age_string <- function(cache_path) {
  if (!file.exists(cache_path)) return("never")
  
  age_mins <- as.numeric(difftime(Sys.time(), file.mtime(cache_path), units = "mins"))
  
  if (age_mins < 1) return("just now")
  if (age_mins < 60) return(sprintf("%.0f minutes ago", age_mins))
  if (age_mins < 1440) return(sprintf("%.1f hours ago", age_mins / 60))
  return(sprintf("%.1f days ago", age_mins / 1440))
}

#' Save data to cache
#' @param data Data frame to cache
#' @param dataset_name Name of the dataset
save_to_cache <- function(data, dataset_name) {
  cache_path <- get_cache_path(dataset_name)
  saveRDS(data, cache_path)
  log_debug("Saved to cache:", cache_path, level = "INFO")
}

#' Load data from cache
#' @param dataset_name Name of the dataset
#' @return Cached data frame or NULL if not found
load_from_cache <- function(dataset_name) {
  cache_path <- get_cache_path(dataset_name)
  
  if (file.exists(cache_path)) {
    log_debug("Loading from cache:", cache_path, level = "INFO")
    return(readRDS(cache_path))
  }
  
  return(NULL)
}

#' Get cache status for all soccer datasets
#' @return List with status for each dataset
get_cache_status <- function() {
  datasets <- c("shooting", "possession", "shots")
  
  status <- lapply(datasets, function(name) {
    path <- get_cache_path(name)
    list(
      name = name,
      path = path,
      exists = file.exists(path),
      valid = is_cache_valid(path),
      age = get_cache_age_string(path)
    )
  })
  
  names(status) <- datasets
  return(status)
}

#' Clear all soccer caches
#' @return Number of cache files deleted
clear_soccer_cache <- function() {
  log_debug("Clearing all soccer cache files...", level = "INFO")
  
  cache_files <- list.files(CACHE_DIR, pattern = "^soccer_.*\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    log_debug("No cache files to clear", level = "INFO")
    return(0)
  }
  
  deleted <- 0
  for (f in cache_files) {
    tryCatch({
      file.remove(f)
      log_debug("Deleted:", f, level = "DEBUG")
      deleted <- deleted + 1
    }, error = function(e) {
      log_debug("Failed to delete:", f, "-", e$message, level = "WARN")
    })
  }
  
  log_debug("Cleared", deleted, "cache files", level = "INFO")
  return(deleted)
}