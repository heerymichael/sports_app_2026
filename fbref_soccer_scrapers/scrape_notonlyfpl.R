################################################################################
# NotOnlyFPL FanTeam STATS SCRAPER
# 
# Scrapes paginated tables from NotOnlyFPL FanTeam stats pages:
#   1. Stats Overall (season totals) -> stats_overview worksheet
#   2. Stats by GW (gameweek breakdown) -> gameweek_stats worksheet
#
# USES CHROMOTE: Handles JavaScript-rendered pages
# FULL REPLACE: Clears and rewrites entire sheets each run
#
# Target Google Sheet:
#   https://docs.google.com/spreadsheets/d/1FQGMXsrjdgqo8KsHE1_ODd4KyB-h7SDWA2OpgVaOCLk/
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

# Rate limit - be polite to the server
RATE_LIMIT_DELAY <- 3  # seconds between page requests
PAGE_LOAD_WAIT <- 5    # seconds to wait for page to render

# URLs to scrape
URLS <- list(
  stats_overview = "https://www.notonlyfpl.co.uk/FanTeam/statsoverall",
  gameweek_stats = "https://www.notonlyfpl.co.uk/FanTeam/statsbygw"
)

# Google Sheet configuration
GOOGLE_SHEET_ID <- "1FQGMXsrjdgqo8KsHE1_ODd4KyB-h7SDWA2OpgVaOCLk"

# Worksheet names in the Google Sheet
SHEET_NAMES <- list(
  stats_overview = "stats_overview",
  gameweek_stats = "gameweek_stats"
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
      # Append
      sheet_append(data, ss = sheet_id, sheet = sheet_name)
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
  # Try to find pagination info from the page
  # Look for common patterns like "1-50 of 500"
  
  result <- tryCatch({
    info <- browser$Runtime$evaluate(
      "(() => {
        // Look for pagination text
        const elements = document.querySelectorAll('*');
        for (let el of elements) {
          const text = el.textContent;
          if (text && text.match(/\\d+[–-]\\d+\\s+of\\s+\\d+/i)) {
            return text.match(/\\d+[–-]\\d+\\s+of\\s+(\\d+)/i)[1];
          }
        }
        
        // Look for MUI pagination
        const muiPagination = document.querySelector('.MuiTablePagination-displayedRows');
        if (muiPagination) {
          const match = muiPagination.textContent.match(/of\\s+(\\d+)/i);
          if (match) return match[1];
        }
        
        return null;
      })()"
    )$result$value
    
    if (!is.null(result)) {
      return(as.integer(result))
    }
    return(NULL)
    
  }, error = function(e) {
    return(NULL)
  })
  
  return(result)
}

click_next_page <- function(browser) {
  # Try to click the next page button
  # Returns TRUE if successful, FALSE if no next page
  
  result <- tryCatch({
    clicked <- browser$Runtime$evaluate(
      "(() => {
        // MUI Next Button
        const muiNext = document.querySelector('button[aria-label=\"Go to next page\"]');
        if (muiNext && !muiNext.disabled) {
          muiNext.click();
          return true;
        }
        
        // Generic next button patterns
        const nextSelectors = [
          'button[aria-label=\"Next page\"]',
          'button[aria-label=\"next page\"]',
          '.pagination-next:not(.disabled)',
          'a[rel=\"next\"]',
          'button.next:not(:disabled)',
          '[class*=\"next\"]:not(.disabled)'
        ];
        
        for (const selector of nextSelectors) {
          const btn = document.querySelector(selector);
          if (btn && !btn.disabled && btn.offsetParent !== null) {
            btn.click();
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
  
  # Try to set max rows per page
  set_rows_per_page(browser, 500)
  
  # Re-fetch after potential rows change
  Sys.sleep(2)
  html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  # Get total count if available
  total_rows <- get_pagination_info(browser)
  if (!is.null(total_rows)) {
    message(sprintf("  Total rows detected: %d", total_rows))
  }
  
  all_rows <- list()
  headers <- NULL
  page_num <- 1
  
  repeat {
    message(sprintf("  Processing page %d...", page_num))
    
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
      message(sprintf("  Headers: %s", paste(headers, collapse = ", ")))
    }
    
    # Add rows
    rows_added <- length(table_data$rows)
    all_rows <- c(all_rows, table_data$rows)
    message(sprintf("  Found %d rows (total: %d)", rows_added, length(all_rows)))
    
    # Try to go to next page
    if (!click_next_page(browser)) {
      message("  No more pages")
      break
    }
    
    # Wait for new page to load
    Sys.sleep(RATE_LIMIT_DELAY)
    
    # Re-fetch page content
    html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html)
    
    page_num <- page_num + 1
    
    # Safety limit
    if (page_num > 50) {
      message("  ⚠️  Reached page limit of 50")
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
  message("📊 DATA SOURCES:")
  message(sprintf("   1. Stats Overall: %s", URLS$stats_overview))
  message(sprintf("   2. Stats by GW: %s", URLS$gameweek_stats))
  message("")
  message("📝 TARGET GOOGLE SHEET:")
  message(sprintf("   https://docs.google.com/spreadsheets/d/%s/edit", GOOGLE_SHEET_ID))
  message("")
  message("⚠️  MODE: Full replace (clears and rewrites sheets)")
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
  
  # Scrape both tables
  results <- list()
  
  tryCatch({
    
    # 1. Stats Overview
    results$stats_overview <- scrape_paginated_table(
      URLS$stats_overview, 
      "Stats Overall",
      browser
    )
    
    # 2. Gameweek Stats
    results$gameweek_stats <- scrape_paginated_table(
      URLS$gameweek_stats,
      "Stats by Gameweek",
      browser
    )
    
  }, finally = {
    close_browser(browser)
  })
  
  # Write to Google Sheets
  message("\n=== Writing to Google Sheets ===")
  
  if (!is.null(results$stats_overview)) {
    write_to_google_sheet(
      results$stats_overview,
      GOOGLE_SHEET_ID,
      SHEET_NAMES$stats_overview,
      replace = TRUE
    )
  }
  
  if (!is.null(results$gameweek_stats)) {
    write_to_google_sheet(
      results$gameweek_stats,
      GOOGLE_SHEET_ID,
      SHEET_NAMES$gameweek_stats,
      replace = TRUE
    )
  }
  
  # Summary
  message("\n")
  message("================================================================================")
  message("                              SUMMARY")
  message("================================================================================")
  
  if (!is.null(results$stats_overview)) {
    message(sprintf("✓ Stats Overview: %d rows", nrow(results$stats_overview)))
  } else {
    message("✗ Stats Overview: No data")
  }
  
  if (!is.null(results$gameweek_stats)) {
    message(sprintf("✓ Gameweek Stats: %d rows", nrow(results$gameweek_stats)))
  } else {
    message("✗ Gameweek Stats: No data")
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