################################################################################
# NotOnlyFPL FanTeam STATS SCRAPER
# 
# Scrapes the Stats Overall page from NotOnlyFPL:
#   https://www.notonlyfpl.co.uk/FanTeam/statsoverall
#
# OUTPUT SHEETS:
#   1. stats_overview - Full season totals (replaced each run)
#   2. gameweek_detail - Individual GW stats filtered via slider (INCREMENTAL)
#
# The Stats Overall page has a Gameweek Range slider. This script:
#   - First scrapes with full range for season totals
#   - Then filters to each individual GW to get per-week stats
#   - Only scrapes new GWs not already in the gameweek_detail sheet
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

# Helper for NULL coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

################################################################################
# CONFIGURATION
################################################################################

# Rate limit - be polite to the server
RATE_LIMIT_DELAY <- 3  # seconds between page requests
PAGE_LOAD_WAIT <- 5    # seconds to wait for page to render

# URL to scrape (Stats Overall page with GW slider)
STATS_URL <- "https://www.notonlyfpl.co.uk/FanTeam/statsoverall"

# For backwards compatibility
URLS <- list(
  stats_overview = STATS_URL
)

# Google Sheet configuration
GOOGLE_SHEET_ID <- "1EM_Xiqy5Kyvc-AlvpfLT7yjLl_7vcVbBgmj3GwNuIKg"

# Worksheet names in the Google Sheet
SHEET_NAMES <- list(
  stats_overview = "stats_overview",
  gameweek_detail = "gameweek_detail"
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
        // The slider shows badges with '1' and '17' (or current max)
        // We need to find the highest number displayed near 'Gameweek Range'
        
        // Method 1: Find the second badge/number indicator (the max value)
        // Look for elements that contain ONLY a number 1-38
        const candidates = [];
        const walker = document.createTreeWalker(
          document.body,
          NodeFilter.SHOW_TEXT,
          null,
          false
        );
        
        let node;
        while (node = walker.nextNode()) {
          const text = node.textContent.trim();
          // Check if it's just a number between 1-38
          if (/^\\d{1,2}$/.test(text)) {
            const num = parseInt(text);
            if (num >= 1 && num <= 38) {
              candidates.push(num);
            }
          }
        }
        
        // The max gameweek should be the highest small number found
        // Filter out things like years (2024, 2025) or large IDs
        if (candidates.length > 0) {
          const maxFound = Math.max(...candidates);
          if (maxFound >= 1 && maxFound <= 38) {
            return maxFound;
          }
        }
        
        // Method 2: Check aria-valuemax on any slider elements
        const sliderElements = document.querySelectorAll('[role=\"slider\"], .rc-slider-handle, input[type=\"range\"]');
        for (const el of sliderElements) {
          const max = el.getAttribute('aria-valuemax') || el.getAttribute('max');
          if (max) {
            const maxNum = parseInt(max);
            if (maxNum >= 1 && maxNum <= 38) return maxNum;
          }
        }
        
        // Method 3: Parse 'Gameweek Range:' section specifically
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
set_gameweek_filter <- function(browser, gameweek) {
  message(sprintf("  Setting filter to GW%d...", gameweek))
  
  # First, try to detect what kind of slider is on the page
  slider_info <- tryCatch({
    browser$Runtime$evaluate(
      "(() => {
        const info = {
          rangeInputs: document.querySelectorAll('input[type=\"range\"]').length,
          roleSliders: document.querySelectorAll('[role=\"slider\"]').length,
          rcSlider: document.querySelectorAll('.rc-slider-handle').length,
          muiSlider: document.querySelectorAll('.MuiSlider-thumb').length
        };
        return info;
      })()"
    )$result$value
  }, error = function(e) NULL)
  
  if (!is.null(slider_info)) {
    message(sprintf("    Slider detection: range=%d, role=%d, rc=%d, mui=%d",
                    slider_info$rangeInputs %||% 0, 
                    slider_info$roleSliders %||% 0,
                    slider_info$rcSlider %||% 0,
                    slider_info$muiSlider %||% 0))
  }
  
  result <- tryCatch({
    # Try to set the slider value
    success <- browser$Runtime$evaluate(
      sprintf("(() => {
        const targetGW = %d;
        let success = false;
        
        // APPROACH 1: RC-Slider (common React slider library)
        const rcHandles = document.querySelectorAll('.rc-slider-handle');
        if (rcHandles.length >= 2) {
          console.log('Using RC-Slider approach');
          // For a range slider, we need to set both handles to same value
          // RC-Slider stores value in aria-valuenow
          // We need to simulate mouse events
          
          // Get slider track to calculate positions
          const track = document.querySelector('.rc-slider-rail, .rc-slider');
          if (track) {
            const rect = track.getBoundingClientRect();
            const min = parseInt(rcHandles[0].getAttribute('aria-valuemin') || '1');
            const max = parseInt(rcHandles[0].getAttribute('aria-valuemax') || '17');
            const range = max - min;
            const pixelPerUnit = rect.width / range;
            const targetX = rect.left + ((targetGW - min) * pixelPerUnit);
            const targetY = rect.top + rect.height / 2;
            
            // Simulate drag for both handles
            for (const handle of rcHandles) {
              // Mouse down on handle
              handle.dispatchEvent(new MouseEvent('mousedown', {
                bubbles: true, clientX: handle.getBoundingClientRect().left, clientY: targetY
              }));
              
              // Mouse move to target
              document.dispatchEvent(new MouseEvent('mousemove', {
                bubbles: true, clientX: targetX, clientY: targetY
              }));
              
              // Mouse up
              document.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));
            }
            success = true;
          }
        }
        
        // APPROACH 2: Standard range inputs
        if (!success) {
          const sliders = document.querySelectorAll('input[type=\"range\"]');
          if (sliders.length >= 1) {
            console.log('Using range input approach');
            const nativeSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value').set;
            
            for (const slider of sliders) {
              nativeSetter.call(slider, targetGW);
              slider.dispatchEvent(new Event('input', { bubbles: true }));
              slider.dispatchEvent(new Event('change', { bubbles: true }));
            }
            success = true;
          }
        }
        
        // APPROACH 3: ARIA role sliders (keyboard navigation)
        if (!success) {
          const ariaSliders = document.querySelectorAll('[role=\"slider\"]');
          if (ariaSliders.length >= 1) {
            console.log('Using ARIA slider approach');
            for (const slider of ariaSliders) {
              const current = parseInt(slider.getAttribute('aria-valuenow') || '1');
              slider.focus();
              
              // Use arrow keys to adjust
              const diff = targetGW - current;
              const key = diff > 0 ? 'ArrowRight' : 'ArrowLeft';
              
              for (let i = 0; i < Math.abs(diff); i++) {
                slider.dispatchEvent(new KeyboardEvent('keydown', { 
                  key: key, code: key, keyCode: key === 'ArrowRight' ? 39 : 37, bubbles: true 
                }));
                slider.dispatchEvent(new KeyboardEvent('keyup', { 
                  key: key, code: key, keyCode: key === 'ArrowRight' ? 39 : 37, bubbles: true 
                }));
              }
            }
            success = true;
          }
        }
        
        return success;
      })()", gameweek)
    )$result$value
    
    if (!isTRUE(success)) {
      return(FALSE)
    }
    
    # Wait for table to update
    Sys.sleep(3)
    
    # VERIFY the filter actually worked by checking the page
    # Look for indication that we're viewing the right GW
    verification <- browser$Runtime$evaluate(
      sprintf("(() => {
        const targetGW = %d;
        
        // Check if aria-valuenow matches on sliders
        const sliders = document.querySelectorAll('[role=\"slider\"], .rc-slider-handle');
        for (const s of sliders) {
          const val = parseInt(s.getAttribute('aria-valuenow'));
          if (!isNaN(val) && val === targetGW) {
            return { verified: true, method: 'aria-valuenow' };
          }
        }
        
        // Check range inputs
        const rangeInputs = document.querySelectorAll('input[type=\"range\"]');
        for (const r of rangeInputs) {
          if (parseInt(r.value) === targetGW) {
            return { verified: true, method: 'range-input' };
          }
        }
        
        // Check for visual indicator showing the GW number
        const badges = document.querySelectorAll('.rc-slider-mark-text, [class*=\"badge\"], [class*=\"label\"]');
        for (const b of badges) {
          if (b.classList.contains('rc-slider-mark-text-active') && 
              parseInt(b.textContent) === targetGW) {
            return { verified: true, method: 'mark-text' };
          }
        }
        
        return { verified: false, method: 'none' };
      })()", gameweek)
    )$result$value
    
    if (!is.null(verification) && isTRUE(verification$verified)) {
      message(sprintf("    ✓ Filter verified via %s", verification$method))
      return(TRUE)
    } else {
      message("    ⚠️  Could not verify filter was set correctly")
      # Still return TRUE if we think we set it - table content will tell us
      return(TRUE)
    }
    
  }, error = function(e) {
    message(sprintf("    Error setting filter: %s", e$message))
    return(FALSE)
  })
  
  return(result)
}

#' Get already scraped gameweeks from Google Sheet
get_existing_gameweeks <- function(sheet_id, sheet_name) {
  tryCatch({
    # Check if sheet exists
    sheet_info <- gs4_get(sheet_id)
    if (!sheet_name %in% sheet_info$sheets$name) {
      message("  Sheet doesn't exist yet - will scrape all gameweeks")
      return(integer(0))
    }
    
    # Read just the gameweek column
    existing_data <- read_sheet(sheet_id, sheet = sheet_name, col_types = "c")
    
    if (nrow(existing_data) == 0 || !"gameweek" %in% names(existing_data)) {
      return(integer(0))
    }
    
    existing_gws <- unique(as.integer(existing_data$gameweek))
    existing_gws <- existing_gws[!is.na(existing_gws)]
    
    return(sort(existing_gws))
    
  }, error = function(e) {
    message(sprintf("  Note: Could not read existing data: %s", e$message))
    return(integer(0))
  })
}

#' Scrape gameweek stats one gameweek at a time (incremental)
scrape_gameweek_stats_incremental <- function(url, browser, sheet_id, sheet_name) {
  
  message("\n=== Scraping Gameweek Stats (Incremental) ===")
  message(sprintf("URL: %s", url))
  
  # Navigate to page first
  Sys.sleep(RATE_LIMIT_DELAY)
  page <- fetch_page(url, browser)
  
  # Detect max gameweek
  max_gw <- get_max_gameweek(browser)
  
  if (is.null(max_gw)) {
    message("  ✗ Could not detect max gameweek - cannot proceed with incremental scraping")
    message("  TIP: Check the page manually and update CURRENT_GAMEWEEK in config if needed")
    return(NULL)
  }
  
  message(sprintf("  Max gameweek to scrape: %d", max_gw))
  
  # Check which gameweeks already exist
  existing_gws <- get_existing_gameweeks(sheet_id, sheet_name)
  if (length(existing_gws) > 0) {
    message(sprintf("  Already scraped: GW %s", paste(existing_gws, collapse = ", ")))
  }
  
  # Determine which gameweeks to scrape
  all_gws <- 1:max_gw
  new_gws <- setdiff(all_gws, existing_gws)
  
  if (length(new_gws) == 0) {
    message("  ✓ All gameweeks already scraped - nothing to do")
    return(NULL)
  }
  
  message(sprintf("  Gameweeks to scrape: %s", paste(new_gws, collapse = ", ")))
  
  all_data <- list()
  
  for (gw in new_gws) {
    message(sprintf("\n--- Gameweek %d ---", gw))
    
    # Navigate fresh to the page for each gameweek
    Sys.sleep(RATE_LIMIT_DELAY)
    page <- fetch_page(url, browser)
    Sys.sleep(2)
    
    # Set the filter to this gameweek
    if (!set_gameweek_filter(browser, gw)) {
      message(sprintf("  ⚠️  Could not set filter for GW%d - skipping", gw))
      next
    }
    
    # Wait for table to update after filter
    Sys.sleep(3)
    
    # Scrape all pages for this gameweek
    gw_data <- scrape_current_table_all_pages(browser)
    
    if (!is.null(gw_data) && nrow(gw_data) > 0) {
      # Add gameweek column
      gw_data$gameweek <- gw
      all_data[[length(all_data) + 1]] <- gw_data
      message(sprintf("  ✓ GW%d: %d rows", gw, nrow(gw_data)))
    } else {
      message(sprintf("  ⚠️  GW%d: No data found", gw))
    }
  }
  
  if (length(all_data) == 0) {
    message("  No new data scraped")
    return(NULL)
  }
  
  # Combine all gameweek data
  combined_data <- bind_rows(all_data)
  combined_data$scrape_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  message(sprintf("\n✓ Total new data: %d rows across %d gameweeks", 
                  nrow(combined_data), length(all_data)))
  
  return(combined_data)
}

#' Scrape current filtered table across all pages (helper for gameweek scraping)
scrape_current_table_all_pages <- function(browser) {
  
  all_rows <- list()
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
        return(NULL)
      }
      break
    }
    
    # Store headers from first page
    if (page_num == 1) {
      headers <- table_data$headers
    }
    
    # Add rows
    all_rows <- c(all_rows, table_data$rows)
    
    # Check if last page
    if (is_last_page(browser)) {
      break
    }
    
    # Try next page
    if (!click_next_page(browser)) {
      break
    }
    
    Sys.sleep(RATE_LIMIT_DELAY)
    page_num <- page_num + 1
    
    if (page_num > 100) break
  }
  
  if (length(all_rows) == 0 || is.null(headers)) {
    return(NULL)
  }
  
  # Build dataframe
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
  
  df <- as.data.frame(do.call(rbind, cleaned_rows), stringsAsFactors = FALSE)
  names(df) <- make.names(headers, unique = TRUE)
  
  return(df)
}

set_rows_per_page <- function(browser, target_rows = 100) {
  # Try to increase rows per page to minimize pagination
  
  tryCatch({
    browser$Runtime$evaluate(
      sprintf("(() => {
        // MUI rows per page selector
        const rowsPerPage = document.querySelector('[aria-label=\"Rows per page:\"]');
        if (rowsPerPage) {
          rowsPerPage.click();
          setTimeout(() => {
            const options = document.querySelectorAll('[role=\"option\"]');
            let maxOption = null;
            let maxVal = 0;
            for (let opt of options) {
              const val = parseInt(opt.textContent);
              if (!isNaN(val) && val > maxVal && val <= %d) {
                maxVal = val;
                maxOption = opt;
              }
            }
            if (maxOption) maxOption.click();
          }, 500);
          return true;
        }
        
        // Generic select for rows per page
        const selects = document.querySelectorAll('select');
        for (let sel of selects) {
          if (sel.options) {
            for (let opt of sel.options) {
              if (parseInt(opt.value) >= 100 || opt.textContent.toLowerCase().includes('all')) {
                sel.value = opt.value;
                sel.dispatchEvent(new Event('change', { bubbles: true }));
                return true;
              }
            }
          }
        }
        
        return false;
      })()", target_rows)
    )
    
    Sys.sleep(2)  # Wait for table to update
    
  }, error = function(e) {
    message("  Note: Could not increase rows per page")
  })
}

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
# MAIN EXECUTION
################################################################################

main <- function() {
  
  message("================================================================================")
  message("          NotOnlyFPL FanTeam STATS SCRAPER")
  message("================================================================================")
  message("")
  message("📊 DATA SOURCE:")
  message(sprintf("   %s", STATS_URL))
  message("")
  message("📝 TARGET GOOGLE SHEET:")
  message(sprintf("   https://docs.google.com/spreadsheets/d/%s/edit", GOOGLE_SHEET_ID))
  message("")
  message("⚠️  MODE:")
  message("   - stats_overview: Full season totals (replaced each run)")
  message("   - gameweek_detail: Individual GW stats (INCREMENTAL - only new GWs)")
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
    
    # 1. Stats Overview - full season (slider at full range 1-17)
    results$stats_overview <- scrape_paginated_table(
      STATS_URL, 
      "Stats Overall (Full Season)",
      browser
    )
    
    # 2. Individual Gameweek Stats (incremental - filter by each GW)
    results$gameweek_detail <- scrape_gameweek_stats_incremental(
      STATS_URL,
      browser,
      GOOGLE_SHEET_ID,
      "gameweek_detail"
    )
    
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
      "gameweek_detail",
      replace = FALSE
    )
  } else {
    message("  Gameweek Detail: No new data to write")
  }
  
  # Summary
  message("\n")
  message("================================================================================")
  message("                              SUMMARY")
  message("================================================================================")
  
  if (!is.null(results$stats_overview)) {
    message(sprintf("✓ Stats Overview: %d rows (full season)", nrow(results$stats_overview)))
  } else {
    message("✗ Stats Overview: No data")
  }
  
  if (!is.null(results$gameweek_detail) && nrow(results$gameweek_detail) > 0) {
    gws_scraped <- unique(results$gameweek_detail$gameweek)
    message(sprintf("✓ Gameweek Detail: %d rows (GW %s)", 
                    nrow(results$gameweek_detail),
                    paste(sort(gws_scraped), collapse = ", ")))
  } else {
    message("○ Gameweek Detail: Already up to date (or no new GWs)")
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