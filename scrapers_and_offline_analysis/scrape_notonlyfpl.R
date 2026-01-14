################################################################################
# NotOnlyFPL FanTeam STATS SCRAPER (UPDATED)
# 
# Scrapes FanTeam player statistics from NotOnlyFPL:
#
# DATA SOURCES:
#   1. Stats Overall: https://www.notonlyfpl.co.uk/FanTeam/statsoverall
#      - Full season totals (with GW slider for filtering)
#      - Detailed stats: Goals, Assists, CS, Saves, SOT, etc.
#
#   2. Stats By GW: https://www.notonlyfpl.co.uk/FanTeam/statsbygw
#      - Wide format with GW columns (GW1, GW2, ... GW20)
#      - Shows fantasy points per player per gameweek
#
# OUTPUT SHEETS:
#   1. stats_overview    - Full season totals (replaced each run)
#   2. gameweek_detail   - Individual GW detailed stats (INCREMENTAL)
#                          ** KEY DATA FOR REGRESSION - has Goals, Assists, CS, etc. **
#   3. stats_by_gw       - Wide format GW points (replaced each run)
#
# USES CHROMOTE: Handles JavaScript-rendered pages
#
# Target Google Sheet:
#   https://docs.google.com/spreadsheets/d/1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg/
################################################################################

library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(googlesheets4)
library(chromote)
library(jsonlite)

# Helper for NULL coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

################################################################################
# CONFIGURATION
################################################################################

# Rate limit - be polite to the server
RATE_LIMIT_DELAY <- 3  # seconds between page requests
PAGE_LOAD_WAIT <- 5    # seconds to wait for page to render

# URLs to scrape
STATS_URL <- "https://www.notonlyfpl.co.uk/FanTeam/statsoverall"
STATS_BY_GW_URL <- "https://www.notonlyfpl.co.uk/FanTeam/statsbygw"

# Google Sheet configuration
GOOGLE_SHEET_ID <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"

# Worksheet names in the Google Sheet
SHEET_NAMES <- list(
  stats_overview = "stats_overview",
  gameweek_detail = "gameweek_detail",
  stats_by_gw = "stats_by_gw"
)

################################################################################
# CHROMOTE BROWSER FUNCTIONS
################################################################################

init_browser <- function() {
  message("Initializing headless browser...")
  
  # Kill any existing Chrome headless processes
  tryCatch({
    system("pkill -f 'chrome.*--headless'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }, error = function(e) {})
  
  
  # Close any existing chromote sessions
  tryCatch({
    chromote::default_chromote_object()$close()
  }, error = function(e) {})
  
  Sys.sleep(2)
  
  # Create new session
  b <- ChromoteSession$new()
  
  # Set user agent
  b$Network$setUserAgentOverride(
    userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  )
  
  message("✓ Browser initialized")
  return(b)
}

close_browser <- function(browser) {
  if (!is.null(browser)) {
    tryCatch({
      browser$close()
      message("✓ Browser closed")
    }, error = function(e) {
      message("Note: Browser already closed")
    })
  }
  
  # Also kill any lingering Chrome processes
  tryCatch({
    system("pkill -f 'chrome.*--headless'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }, error = function(e) {})
}

is_browser_alive <- function(browser) {
  tryCatch({
    browser$Runtime$evaluate("1+1")
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

fetch_page <- function(url, browser, wait_time = PAGE_LOAD_WAIT, max_retries = 3) {
  
  for (attempt in 1:max_retries) {
    
    if (!is_browser_alive(browser)) {
      message("  ⚠️  Browser session lost, reinitializing...")
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
      return(page)
      
    }, error = function(e) {
      message(sprintf("  ⚠️  Fetch error (attempt %d/%d): %s", attempt, max_retries, e$message))
      return(NULL)
    })
    
    if (!is.null(result)) {
      return(result)
    }
    
    if (attempt < max_retries) {
      message("  Retrying in 5 seconds...")
      Sys.sleep(5)
    }
  }
  
  stop(sprintf("Failed to fetch page after %d attempts: %s", max_retries, url))
}

################################################################################
# GOOGLE SHEETS FUNCTIONS
################################################################################

write_to_google_sheet <- function(data, sheet_id, sheet_name, replace = TRUE) {
  
  if (is.null(data) || nrow(data) == 0) {
    message(sprintf("  ⚠️  No data to write to %s", sheet_name))
    return(FALSE)
  }
  
  message(sprintf("  Writing %d rows to %s...", nrow(data), sheet_name))
  
  tryCatch({
    sheet_info <- gs4_get(sheet_id)
    existing_sheets <- sheet_info$sheets$name
    
    if (!sheet_name %in% existing_sheets) {
      # Create new sheet
      message(sprintf("    Creating new worksheet: %s", sheet_name))
      sheet_write(data, ss = sheet_id, sheet = sheet_name)
    } else if (replace) {
      # Clear and rewrite
      message(sprintf("    Clearing and replacing: %s", sheet_name))
      range_clear(ss = sheet_id, sheet = sheet_name)
      Sys.sleep(1)
      range_write(ss = sheet_id, sheet = sheet_name, data = data, range = "A1", col_names = TRUE)
    } else {
      # Append - but first check if columns match
      message(sprintf("    Appending to: %s", sheet_name))
      
      # Read existing headers
      existing_headers <- tryCatch({
        existing_data <- read_sheet(sheet_id, sheet = sheet_name, n_max = 1, col_types = "c")
        names(existing_data)
      }, error = function(e) NULL)
      
      # If columns don't match or sheet is empty, do full replace instead
      if (is.null(existing_headers) || !setequal(names(data), existing_headers)) {
        message("    Note: Column mismatch or empty sheet - doing full replace instead")
        range_clear(ss = sheet_id, sheet = sheet_name)
        Sys.sleep(1)
        range_write(ss = sheet_id, sheet = sheet_name, data = data, range = "A1", col_names = TRUE)
      } else {
        # Columns match - safe to append
        # Reorder columns to match existing sheet
        data <- data[, existing_headers]
        sheet_append(data, ss = sheet_id, sheet = sheet_name)
      }
    }
    
    message("    ✓ Write successful")
    return(TRUE)
    
  }, error = function(e) {
    message(sprintf("    ✗ Error: %s", e$message))
    return(FALSE)
  })
}

################################################################################
# TABLE SCRAPING FUNCTIONS
################################################################################

extract_table_from_page <- function(page) {
  # Find the table
  table <- page %>% html_node("table")
  
  if (is.null(table)) {
    return(NULL)
  }
  
  # Get headers
  headers <- table %>%
    html_nodes("thead th") %>%
    html_text(trim = TRUE)
  
  # If no thead, try first row
  if (length(headers) == 0) {
    headers <- table %>%
      html_node("tr:first-child") %>%
      html_nodes("th, td") %>%
      html_text(trim = TRUE)
  }
  
  # Get body rows
  rows <- table %>%
    html_nodes("tbody tr")
  
  if (length(rows) == 0) {
    # Try all rows except first
    rows <- table %>%
      html_nodes("tr")
    if (length(rows) > 1) {
      rows <- rows[-1]  # Skip header row
    }
  }
  
  # Extract row data
  row_data <- map(rows, function(row) {
    cells <- row %>%
      html_nodes("td") %>%
      html_text(trim = TRUE)
    
    # Skip empty rows
    if (length(cells) == 0 || all(cells == "")) {
      return(NULL)
    }
    
    return(cells)
  })
  
  # Remove NULL entries
  row_data <- row_data[!sapply(row_data, is.null)]
  
  if (length(row_data) == 0) {
    return(NULL)
  }
  
  return(list(headers = headers, rows = row_data))
}

get_pagination_info <- function(browser) {
  # Look for "Page X of Y" format used by NotOnlyFPL
  result <- tryCatch({
    info <- browser$Runtime$evaluate(
      "(() => {
        // Look for 'Page X of Y' text
        const body = document.body.innerText;
        const match = body.match(/Page\\s+(\\d+)\\s+of\\s+(\\d+)/i);
        if (match) {
          return { current: parseInt(match[1]), total: parseInt(match[2]) };
        }
        return null;
      })()"
    )$result$value
    
    return(info)
    
  }, error = function(e) {
    return(NULL)
  })
  
  return(result)
}

click_next_page <- function(browser) {
  # Click the "Next" button - specific to NotOnlyFPL's simple pagination
  result <- tryCatch({
    clicked <- browser$Runtime$evaluate(
      "(() => {
        // Find button with 'Next' text
        const buttons = document.querySelectorAll('button');
        for (const btn of buttons) {
          if (btn.textContent.trim() === 'Next' && !btn.disabled) {
            btn.click();
            return true;
          }
        }
        
        // Also try links or divs that act as buttons
        const allElements = document.querySelectorAll('a, div, span');
        for (const el of allElements) {
          if (el.textContent.trim() === 'Next' && 
              el.style.pointerEvents !== 'none' &&
              !el.classList.contains('disabled')) {
            el.click();
            return true;
          }
        }
        
        return false;
      })()"
    )$result$value
    
    return(isTRUE(clicked))
    
  }, error = function(e) {
    return(FALSE)
  })
  
  return(result)
}

is_last_page <- function(browser) {
  # Check if we're on the last page by comparing current vs total
  info <- get_pagination_info(browser)
  if (!is.null(info) && !is.null(info$current) && !is.null(info$total)) {
    return(info$current >= info$total)
  }
  return(FALSE)
}

################################################################################
# GAMEWEEK FILTERING FUNCTIONS
################################################################################

#' Get the maximum gameweek available from the slider
get_max_gameweek <- function(browser) {
  message("  Auto-detecting current max gameweek from slider...")
  
  result <- tryCatch({
    max_gw <- browser$Runtime$evaluate(
      "(() => {
        // PRIORITY: Check aria-valuemax on role='slider' elements (most reliable)
        const sliderElements = document.querySelectorAll('[role=\"slider\"]');
        for (const el of sliderElements) {
          const max = el.getAttribute('aria-valuemax');
          if (max) {
            const maxNum = parseInt(max);
            if (maxNum >= 1 && maxNum <= 38) return maxNum;
          }
        }
        
        // Fallback: Check other slider types
        const otherSliders = document.querySelectorAll('.rc-slider-handle, .MuiSlider-thumb, input[type=\"range\"]');
        for (const el of otherSliders) {
          const max = el.getAttribute('aria-valuemax') || el.getAttribute('max');
          if (max) {
            const maxNum = parseInt(max);
            if (maxNum >= 1 && maxNum <= 38) return maxNum;
          }
        }
        
        // Last resort: Parse 'Gameweek Range:' section text
        const gwLabel = Array.from(document.querySelectorAll('*')).find(
          el => el.textContent.includes('Gameweek Range')
        );
        if (gwLabel) {
          const parent = gwLabel.parentElement || gwLabel;
          const nums = parent.innerText.match(/\\b(\\d{1,2})\\b/g);
          if (nums) {
            const validNums = nums.map(n => parseInt(n)).filter(n => n >= 1 && n <= 38);
            if (validNums.length > 0) return Math.max(...validNums);
          }
        }
        
        return null;  // Return null if detection fails
      })()"
    )$result$value
    
    if (!is.null(max_gw) && max_gw >= 1 && max_gw <= 38) {
      message(sprintf("  ✓ Detected current max gameweek: %d", max_gw))
      return(as.integer(max_gw))
    } else {
      message("  ⚠️  Could not detect max gameweek from slider")
      return(NULL)
    }
    
  }, error = function(e) {
    message(sprintf("  ⚠️  Error detecting max gameweek: %s", e$message))
    return(NULL)
  })
  
  return(result)
}

#' Set the gameweek filter to a specific single gameweek
#' CRITICAL: Sets BOTH min and max handles to the same value for single-GW filtering
set_gameweek_filter <- function(browser, gameweek) {
  message(sprintf("  Setting filter to GW%d (both min AND max handles)...", gameweek))
  
  # Step 1: Get slider info
  slider_json <- browser$Runtime$evaluate(
    "JSON.stringify((() => {
      const sliders = document.querySelectorAll('[role=slider]');
      if (sliders.length < 2) return { error: 'Not enough sliders' };
      
      return {
        minVal: parseInt(sliders[0].getAttribute('aria-valuemin') || '1'),
        maxVal: parseInt(sliders[0].getAttribute('aria-valuemax') || '20'),
        leftVal: parseInt(sliders[0].getAttribute('aria-valuenow') || '1'),
        rightVal: parseInt(sliders[1].getAttribute('aria-valuenow') || '20'),
        leftX: sliders[0].getBoundingClientRect().x + 8,
        leftY: sliders[0].getBoundingClientRect().y + 8,
        rightX: sliders[1].getBoundingClientRect().x + 8,
        rightY: sliders[1].getBoundingClientRect().y + 8
      };
    })())"
  )$result$value
  
  slider_info <- jsonlite::fromJSON(slider_json)
  
  if (!is.null(slider_info$error)) {
    message(sprintf("    ⚠️  %s", slider_info$error))
    return(FALSE)
  }
  
  message(sprintf("    Detected: range %d-%d, current: left=%d, right=%d",
                  slider_info$minVal, slider_info$maxVal,
                  slider_info$leftVal, slider_info$rightVal))
  
  # Calculate target X position
  trackWidth <- slider_info$rightX - slider_info$leftX
  currentRange <- slider_info$rightVal - slider_info$leftVal
  
  if (currentRange > 0) {
    pixelPerUnit <- trackWidth / currentRange
    trackStartX <- slider_info$leftX - ((slider_info$leftVal - slider_info$minVal) * pixelPerUnit)
  } else {
    pixelPerUnit <- 33.3  # fallback
    trackStartX <- slider_info$leftX
  }
  
  targetX <- trackStartX + ((gameweek - slider_info$minVal) * pixelPerUnit)
  targetY <- slider_info$leftY
  
  message(sprintf("    Target X: %.0f (ppu=%.1f)", targetX, pixelPerUnit))
  
  # Step 2: Move RIGHT handle first using CDP Input (browser-level mouse events)
  message("    Moving RIGHT handle via CDP...")
  
  browser$Input$dispatchMouseEvent(
    type = "mousePressed",
    x = slider_info$rightX,
    y = slider_info$rightY,
    button = "left",
    clickCount = 1
  )
  Sys.sleep(0.1)
  
  browser$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = targetX,
    y = targetY,
    button = "left"
  )
  Sys.sleep(0.1)
  
  browser$Input$dispatchMouseEvent(
    type = "mouseReleased",
    x = targetX,
    y = targetY,
    button = "left"
  )
  Sys.sleep(0.3)
  
  # Get updated positions
  positions_json <- browser$Runtime$evaluate(
    "JSON.stringify({
      leftX: document.querySelectorAll('[role=slider]')[0]?.getBoundingClientRect().x + 8,
      leftY: document.querySelectorAll('[role=slider]')[0]?.getBoundingClientRect().y + 8,
      rightX: document.querySelectorAll('[role=slider]')[1]?.getBoundingClientRect().x + 8,
      rightVal: document.querySelectorAll('[role=slider]')[1]?.getAttribute('aria-valuenow')
    })"
  )$result$value
  positions <- jsonlite::fromJSON(positions_json)
  message(sprintf("    Right -> %s", positions$rightVal))
  
  # Step 3: Move LEFT handle to same position as right
  message("    Moving LEFT handle via CDP...")
  
  browser$Input$dispatchMouseEvent(
    type = "mousePressed",
    x = positions$leftX,
    y = positions$leftY,
    button = "left",
    clickCount = 1
  )
  Sys.sleep(0.1)
  
  browser$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = positions$rightX,
    y = positions$leftY,
    button = "left"
  )
  Sys.sleep(0.1)
  
  browser$Input$dispatchMouseEvent(
    type = "mouseReleased",
    x = positions$rightX,
    y = positions$leftY,
    button = "left"
  )
  Sys.sleep(0.5)
  
  # Verify final state
  verify_json <- browser$Runtime$evaluate(
    "JSON.stringify({
      left: document.querySelectorAll('[role=slider]')[0].getAttribute('aria-valuenow'),
      right: document.querySelectorAll('[role=slider]')[1].getAttribute('aria-valuenow')
    })"
  )$result$value
  verify <- jsonlite::fromJSON(verify_json)
  message(sprintf("    Final: left=%s, right=%s", verify$left, verify$right))
  
  # Check if both are at target
  if (as.integer(verify$left) == gameweek && as.integer(verify$right) == gameweek) {
    message(sprintf("    ✓ Both handles at GW%d", gameweek))
    Sys.sleep(1.5)  # Wait for table to update
    return(TRUE)
  } else {
    message(sprintf("    ⚠️  Handles not at target (expected %d)", gameweek))
    Sys.sleep(1.5)
    return(TRUE)  # Still try scraping
  }
}

#' Get existing gameweeks from the sheet
get_existing_gameweeks <- function(sheet_id, sheet_name) {
  tryCatch({
    # Read just the gameweek column
    existing_data <- read_sheet(sheet_id, sheet = sheet_name, col_types = "c")
    
    if (!"gameweek" %in% names(existing_data)) {
      message("  Note: No 'gameweek' column found - will scrape all GWs")
      return(integer(0))
    }
    
    gws <- as.integer(existing_data$gameweek)
    gws <- unique(gws[!is.na(gws)])
    
    message(sprintf("  Found existing GWs: %s", paste(sort(gws), collapse = ", ")))
    return(gws)
    
  }, error = function(e) {
    message(sprintf("  Note: Could not read existing data (%s)", e$message))
    return(integer(0))
  })
}

#' Scrape individual gameweek stats incrementally
#' Only scrapes GWs not already in the sheet
scrape_gameweek_stats_incremental <- function(url, browser, sheet_id, sheet_name) {
  
  message("\n=== Scraping Individual Gameweek Stats (Incremental) ===")
  message("  ** This contains detailed stats: Goals, Assists, CS, Saves, SOT, etc. **")
  
  # Navigate to stats page first
  Sys.sleep(RATE_LIMIT_DELAY)
  page <- fetch_page(url, browser)
  
  # Detect max gameweek
  max_gw <- get_max_gameweek(browser)
  
  if (is.null(max_gw)) {
    message("  ⚠️  Could not detect max GW - defaulting to 20")
    max_gw <- 20
  }
  
  # Get existing gameweeks
  existing_gws <- get_existing_gameweeks(sheet_id, sheet_name)
  
  # Determine which GWs to scrape
  all_gws <- 1:max_gw
  new_gws <- setdiff(all_gws, existing_gws)
  
  if (length(new_gws) == 0) {
    message("  ✓ All gameweeks already scraped (GW1 to GW", max_gw, ")")
    return(data.frame())
  }
  
  message(sprintf("  Will scrape GWs: %s", paste(sort(new_gws), collapse = ", ")))
  
  all_gw_data <- list()
  
  for (gw in sort(new_gws)) {
    message(sprintf("\n--- Gameweek %d ---", gw))
    
    # Set filter to this single GW
    if (!set_gameweek_filter(browser, gw)) {
      message(sprintf("  ⚠️  Skipping GW%d - could not set filter", gw))
      next
    }
    
    # Wait for table to update
    Sys.sleep(2)
    
    # Now scrape all pages for this GW
    gw_rows <- list()
    headers <- NULL
    page_num <- 1
    
    repeat {
      # Get current page HTML
      html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
      page <- read_html(html)
      
      # Extract table
      table_data <- extract_table_from_page(page)
      
      if (is.null(table_data)) {
        if (page_num == 1) {
          message("    ⚠️  No table found")
        }
        break
      }
      
      if (page_num == 1) {
        headers <- table_data$headers
        message(sprintf("    Headers: %s...", paste(headers[1:min(5, length(headers))], collapse = ", ")))
      }
      
      gw_rows <- c(gw_rows, table_data$rows)
      
      # Progress message every 5 pages or on page 1
      if (page_num == 1 || page_num %% 5 == 0) {
        message(sprintf("    Page %d: %d rows so far...", page_num, length(gw_rows)))
      }
      
      if (is_last_page(browser)) {
        message(sprintf("    Page %d (final): %d total rows", page_num, length(gw_rows)))
        break
      }
      if (!click_next_page(browser)) {
        message(sprintf("    Page %d (no more): %d total rows", page_num, length(gw_rows)))
        break
      }
      
      Sys.sleep(RATE_LIMIT_DELAY)
      page_num <- page_num + 1
      
      if (page_num > 60) break  # Safety limit
    }
    
    if (length(gw_rows) > 0 && !is.null(headers)) {
      # Build dataframe for this GW
      num_cols <- length(headers)
      cleaned_rows <- lapply(gw_rows, function(row) {
        if (length(row) < num_cols) {
          c(row, rep("", num_cols - length(row)))
        } else if (length(row) > num_cols) {
          row[1:num_cols]
        } else {
          row
        }
      })
      
      gw_df <- as.data.frame(do.call(rbind, cleaned_rows), stringsAsFactors = FALSE)
      names(gw_df) <- make.names(headers, unique = TRUE)
      gw_df$gameweek <- gw
      
      all_gw_data[[as.character(gw)]] <- gw_df
      message(sprintf("    ✓ GW%d: %d players", gw, nrow(gw_df)))
    }
    
    # Navigate back to stats page for next GW
    Sys.sleep(RATE_LIMIT_DELAY)
    fetch_page(url, browser)
  }
  
  # Combine all GW data
  if (length(all_gw_data) > 0) {
    combined <- bind_rows(all_gw_data)
    combined$scrape_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(sprintf("\n✓ Total: %d rows across %d gameweeks", nrow(combined), length(all_gw_data)))
    return(combined)
  }
  
  return(data.frame())
}

################################################################################
# PAGINATED TABLE SCRAPER (for full season stats)
################################################################################

scrape_paginated_table <- function(url, name, browser) {
  
  message(sprintf("\n=== Scraping %s ===", name))
  message(sprintf("URL: %s", url))
  
  # Navigate to page
  Sys.sleep(RATE_LIMIT_DELAY)
  page <- fetch_page(url, browser)
  
  # Get pagination info
  page_info <- get_pagination_info(browser)
  if (!is.null(page_info) && !is.null(page_info$total)) {
    message(sprintf("  Pagination detected: Page %d of %d", page_info$current, page_info$total))
  }
  
  all_rows <- list()
  headers <- NULL
  page_num <- 1
  
  repeat {
    message(sprintf("  Processing page %d...", page_num))
    
    # Re-fetch page content (important after navigation)
    html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html)
    
    # Extract current page
    table_data <- extract_table_from_page(page)
    
    if (is.null(table_data)) {
      if (page_num == 1) {
        message("  ✗ No table found on page")
        return(NULL)
      }
      break
    }
    
    # Store headers from first page
    if (page_num == 1) {
      headers <- table_data$headers
      message(sprintf("  Headers (%d cols): %s...", length(headers), 
                      paste(headers[1:min(5, length(headers))], collapse = ", ")))
    }
    
    # Add rows
    rows_added <- length(table_data$rows)
    all_rows <- c(all_rows, table_data$rows)
    message(sprintf("    Found %d rows (total: %d)", rows_added, length(all_rows)))
    
    # Check if we're on the last page
    if (is_last_page(browser)) {
      message("  Reached last page")
      break
    }
    
    # Try to go to next page
    if (!click_next_page(browser)) {
      message("  No more pages (Next button not found or disabled)")
      break
    }
    
    # Wait for new page to load
    Sys.sleep(RATE_LIMIT_DELAY)
    
    page_num <- page_num + 1
    
    # Safety limit (45 pages * ~10 rows = ~450 players, but allow extra)
    if (page_num > 100) {
      message("  ⚠️  Reached page limit of 100")
      break
    }
  }
  
  # Build dataframe
  if (length(all_rows) == 0) {
    message("  ✗ No data extracted")
    return(NULL)
  }
  
  # Ensure all rows have same length as headers
  num_cols <- length(headers)
  cleaned_rows <- lapply(all_rows, function(row) {
    if (length(row) < num_cols) {
      c(row, rep("", num_cols - length(row)))
    } else if (length(row) > num_cols) {
      row[1:num_cols]
    } else {
      row
    }
  })
  
  # Create dataframe
  df <- as.data.frame(do.call(rbind, cleaned_rows), stringsAsFactors = FALSE)
  
  # Clean up header names for column names
  clean_headers <- make.names(headers, unique = TRUE)
  names(df) <- clean_headers
  
  # Add scrape timestamp
  df$scrape_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  message(sprintf("✓ Extracted %d rows with %d columns", nrow(df), ncol(df)))
  
  return(df)
}

################################################################################
# STATS BY GAMEWEEK SCRAPER (wide format)
################################################################################

#' Scrape the Stats By Gameweek page (wide format with GW columns)
#' 
#' This page shows fantasy points per player per GW in columns (GW1, GW2, etc.)
#' Less detailed than gameweek_detail but useful for quick points lookup
#' 
#' @param browser Chromote browser session
#' @return Data frame with player stats by gameweek (wide format)
scrape_stats_by_gw <- function(browser) {
  
  message("\n=== Scraping Stats By Gameweek (Wide Format) ===")
  message(sprintf("URL: %s", STATS_BY_GW_URL))
  message("  Note: This shows fantasy points per GW only (not detailed stats)")
  
  # Navigate to page
  Sys.sleep(RATE_LIMIT_DELAY)
  page <- fetch_page(STATS_BY_GW_URL, browser, wait_time = PAGE_LOAD_WAIT)
  
  # Try to increase rows per page to minimize pagination
  tryCatch({
    message("  Attempting to show more rows...")
    browser$Runtime$evaluate(
      "(() => {
        // Look for rows per page selector and set to maximum
        const selects = document.querySelectorAll('select');
        for (let sel of selects) {
          const options = Array.from(sel.options);
          // Find option with highest value or 'All'
          let maxOpt = null;
          let maxVal = 0;
          for (let opt of options) {
            const val = parseInt(opt.value);
            if (opt.textContent.toLowerCase().includes('all')) {
              sel.value = opt.value;
              sel.dispatchEvent(new Event('change', { bubbles: true }));
              return 'set to all';
            }
            if (!isNaN(val) && val > maxVal) {
              maxVal = val;
              maxOpt = opt;
            }
          }
          if (maxOpt && maxVal >= 50) {
            sel.value = maxOpt.value;
            sel.dispatchEvent(new Event('change', { bubbles: true }));
            return 'set to ' + maxVal;
          }
        }
        return 'no selector found';
      })()"
    )
    Sys.sleep(3)  # Wait for table to reload
  }, error = function(e) {
    message("  Note: Could not modify rows per page")
  })
  
  # Now scrape the paginated table
  all_rows <- list()
  headers <- NULL
  page_num <- 1
  
  repeat {
    message(sprintf("  Processing page %d...", page_num))
    
    # Get current page HTML
    html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html)
    
    # Extract table
    table_data <- extract_table_from_page(page)
    
    if (is.null(table_data)) {
      if (page_num == 1) {
        message("  ✗ No table found on page")
        return(NULL)
      }
      break
    }
    
    # Store headers from first page
    if (page_num == 1) {
      headers <- table_data$headers
      gw_count <- sum(grepl("^GW\\d+$", headers, ignore.case = TRUE))
      message(sprintf("  Headers (%d cols, %d GW columns): %s...", 
                      length(headers), gw_count,
                      paste(headers[1:min(6, length(headers))], collapse = ", ")))
    }
    
    # Add rows
    rows_added <- length(table_data$rows)
    all_rows <- c(all_rows, table_data$rows)
    message(sprintf("    Found %d rows (total: %d)", rows_added, length(all_rows)))
    
    # Check if we're on the last page
    if (is_last_page(browser)) {
      message("  Reached last page")
      break
    }
    
    # Try to go to next page
    if (!click_next_page(browser)) {
      message("  No more pages")
      break
    }
    
    Sys.sleep(RATE_LIMIT_DELAY)
    page_num <- page_num + 1
    
    # Safety limit
    if (page_num > 60) {
      message("  ⚠️  Reached page limit of 60")
      break
    }
  }
  
  # Build dataframe
  if (length(all_rows) == 0) {
    message("  ✗ No data extracted")
    return(NULL)
  }
  
  # Ensure all rows have same length as headers
  num_cols <- length(headers)
  cleaned_rows <- lapply(all_rows, function(row) {
    if (length(row) < num_cols) {
      c(row, rep("", num_cols - length(row)))
    } else if (length(row) > num_cols) {
      row[1:num_cols]
    } else {
      row
    }
  })
  
  # Create dataframe
  df <- as.data.frame(do.call(rbind, cleaned_rows), stringsAsFactors = FALSE)
  
  # Clean up header names
  clean_headers <- make.names(headers, unique = TRUE)
  names(df) <- clean_headers
  
  # Add scrape timestamp
  df$scrape_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  message(sprintf("✓ Extracted %d rows with %d columns", nrow(df), ncol(df)))
  
  return(df)
}

################################################################################
# MAIN EXECUTION
################################################################################

main <- function() {
  
  message("================================================================================")
  message("          NotOnlyFPL FanTeam STATS SCRAPER (UPDATED)")
  message("================================================================================")
  message("")
  message("📊 DATA SOURCES:")
  message(sprintf("   Stats Overall:  %s", STATS_URL))
  message(sprintf("   Stats By GW:    %s", STATS_BY_GW_URL))
  message("")
  message("📝 TARGET GOOGLE SHEET:")
  message(sprintf("   https://docs.google.com/spreadsheets/d/%s/edit", GOOGLE_SHEET_ID))
  message("")
  message("⚠️  OUTPUT SHEETS:")
  message("   - stats_overview:   Full season totals (replaced each run)")
  message("   - gameweek_detail:  Individual GW detailed stats (INCREMENTAL)")
  message("                       ** KEY DATA: Goals, Assists, CS, Saves, SOT, etc. **")
  message("   - stats_by_gw:      Wide format GW points (replaced each run)")
  message("")
  message(sprintf("⚠️  RATE LIMIT: %d seconds between requests", RATE_LIMIT_DELAY))
  message("================================================================================")
  
  # Initialize browser
  browser <- init_browser()
  
  # Authenticate with Google Sheets
  message("\nAuthenticating with Google Sheets...")
  tryCatch({
    gs4_auth()
    message("✓ Authentication successful")
  }, error = function(e) {
    close_browser(browser)
    stop("Failed to authenticate with Google Sheets: ", e$message)
  })
  
  results <- list()
  
  tryCatch({
    
    # 1. Individual Gameweek Stats - DETAILED (incremental) - DO THIS FIRST
    #    THIS IS THE KEY DATA FOR REGRESSION - has Goals, Assists, CS, etc.
    results$gameweek_detail <- scrape_gameweek_stats_incremental(
      STATS_URL,
      browser,
      GOOGLE_SHEET_ID,
      SHEET_NAMES$gameweek_detail
    )
    
    # 2. Stats Overview - full season (slider at full range)
    results$stats_overview <- scrape_paginated_table(
      STATS_URL, 
      "Stats Overall (Full Season)",
      browser
    )
    
    # 3. Stats By Gameweek - wide format with GW points columns
    results$stats_by_gw <- scrape_stats_by_gw(browser)
    
  }, finally = {
    close_browser(browser)
  })
  
  # Write to Google Sheets
  message("\n=== Writing to Google Sheets ===")
  
  # Stats Overview - full replace
  if (!is.null(results$stats_overview)) {
    write_to_google_sheet(
      results$stats_overview,
      GOOGLE_SHEET_ID,
      SHEET_NAMES$stats_overview,
      replace = TRUE
    )
  }
  
  # Gameweek Detail - append new GWs only
  if (!is.null(results$gameweek_detail) && nrow(results$gameweek_detail) > 0) {
    write_to_google_sheet(
      results$gameweek_detail,
      GOOGLE_SHEET_ID,
      SHEET_NAMES$gameweek_detail,
      replace = FALSE
    )
  } else {
    message("  Gameweek Detail: No new data to write")
  }
  
  # Stats By Gameweek - full replace
  if (!is.null(results$stats_by_gw)) {
    write_to_google_sheet(
      results$stats_by_gw,
      GOOGLE_SHEET_ID,
      SHEET_NAMES$stats_by_gw,
      replace = TRUE
    )
  }
  
  # Summary
  message("\n")
  message("================================================================================")
  message("                              SUMMARY")
  message("================================================================================")
  
  if (!is.null(results$stats_overview)) {
    message(sprintf("✓ Stats Overview: %d rows (full season totals)", nrow(results$stats_overview)))
  } else {
    message("✗ Stats Overview: No data")
  }
  
  if (!is.null(results$gameweek_detail) && nrow(results$gameweek_detail) > 0) {
    gws_scraped <- unique(results$gameweek_detail$gameweek)
    message(sprintf("✓ Gameweek Detail: %d rows (GW %s) - DETAILED STATS", 
                    nrow(results$gameweek_detail),
                    paste(sort(gws_scraped), collapse = ", ")))
  } else {
    message("○ Gameweek Detail: Already up to date (or no new GWs)")
  }
  
  if (!is.null(results$stats_by_gw)) {
    gw_cols <- sum(grepl("^GW\\d+$", names(results$stats_by_gw), ignore.case = TRUE))
    message(sprintf("✓ Stats By Gameweek: %d players × %d gameweeks (wide format)", 
                    nrow(results$stats_by_gw), gw_cols))
  } else {
    message("✗ Stats By Gameweek: No data")
  }
  
  message("")
  message(sprintf("Data saved to: https://docs.google.com/spreadsheets/d/%s/edit", GOOGLE_SHEET_ID))
  message("================================================================================\n")
  
  return(results)
}

################################################################################
# RUN THE SCRAPER
################################################################################

result <- main()