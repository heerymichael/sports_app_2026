/* =============================================================================
   SPORTS ANALYTICS - JavaScript
   ============================================================================= */

$(document).ready(function() {
  
  // -------------------------------------------------------------------------
  // LOADING OVERLAY (with delay to prevent flicker on quick operations)
  // -------------------------------------------------------------------------
  
  const loadingOverlay = $(`
    <div id="app-loading" class="app-loading-overlay" style="display: none;">
      <div class="app-loading-container">
        <div class="app-loading-spinner"></div>
        <p class="app-loading-text">Loading...</p>
      </div>
    </div>
  `);
  
  $('body').append(loadingOverlay);
  
  const loadingStyles = `
    <style>
      .app-loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(143, 188, 187, 0.9);
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        backdrop-filter: blur(4px);
      }
      
      .app-loading-container {
        text-align: center;
        background-color: #FFFFFF;
        padding: 2rem 3rem;
        border: 3px solid #3B3226;
        border-radius: 16px;
        box-shadow: 8px 8px 0 rgba(59, 50, 38, 0.25);
      }
      
      .app-loading-spinner {
        width: 50px;
        height: 50px;
        margin: 0 auto 1rem;
        border: 4px solid #ECEFF4;
        border-top: 4px solid #D08770;
        border-radius: 50%;
        animation: app-spin 1s linear infinite;
      }
      
      .app-loading-text {
        font-family: 'Plus Jakarta Sans', sans-serif;
        font-weight: 700;
        font-size: 0.875rem;
        color: #3B3226;
        margin: 0;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      @keyframes app-spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      .app-fade-in {
        animation: app-fade-in 0.3s ease-out;
      }
      
      @keyframes app-fade-in {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .app-btn-press {
        transform: translate(2px, 2px) !important;
        box-shadow: none !important;
      }
    </style>
  `;
  
  $('head').append(loadingStyles);
  
  // -------------------------------------------------------------------------
  // LOADING STATE MANAGEMENT
  // Only show loading overlay for operations that take > 400ms
  // This prevents flicker on quick reactive updates
  // -------------------------------------------------------------------------
  
  let loadingTimer = null;
  let isCurrentlyBusy = false;
  const LOADING_DELAY_MS = 400; // Only show spinner if busy for longer than this
  
  $(document).on('shiny:busy', function() {
    isCurrentlyBusy = true;
    
    // Clear any existing timer
    if (loadingTimer) {
      clearTimeout(loadingTimer);
    }
    
    // Only show loading overlay if still busy after delay
    loadingTimer = setTimeout(function() {
      if (isCurrentlyBusy) {
        $('#app-loading').fadeIn(150);
      }
    }, LOADING_DELAY_MS);
  });
  
  $(document).on('shiny:idle', function() {
    isCurrentlyBusy = false;
    
    // Clear the timer if operation completed quickly
    if (loadingTimer) {
      clearTimeout(loadingTimer);
      loadingTimer = null;
    }
    
    // Hide loading overlay
    $('#app-loading').fadeOut(150);
  });
  
  // -------------------------------------------------------------------------
  // SPORT NAVIGATION (Level 1) - 3D ARC CAROUSEL
  // Continuous loop - items wrap around, selected always centered
  // -------------------------------------------------------------------------
  
  // Get sport order from items
  function getSportOrder($items) {
    return $items.map(function() {
      return $(this).attr('data-sport') || $(this).attr('id').replace(/.*nav_/, '');
    }).get();
  }
  
  // Position items in continuous arc around the active item
  function positionItemsAroundActive($track, activeIndex) {
    const $items = $track.find('.sport-nav-item');
    const itemCount = $items.length;
    
    if (itemCount === 0) return;
    
    // Settings
    const gap = 2; // Fixed gap between items in pixels
    const anglePerPosition = 12; // Rotation angle for 3D effect
    const depthFactor = 30; // How much items recede (translateZ reduction per position)
    
    // Measure all item widths
    const widths = [];
    $items.each(function() {
      widths.push($(this).outerWidth());
    });
    
    // Position each item relative to active
    $items.each(function(i) {
      // Calculate relative position with wrapping
      let relativePos = i - activeIndex;
      
      if (relativePos > Math.floor(itemCount / 2)) {
        relativePos -= itemCount;
      } else if (relativePos < -Math.floor(itemCount / 2)) {
        relativePos += itemCount;
      }
      
      // Calculate horizontal offset based on widths
      let xOffset = 0;
      if (relativePos !== 0) {
        const direction = relativePos > 0 ? 1 : -1;
        const steps = Math.abs(relativePos);
        
        // Start with half the active item's width
        xOffset = (widths[activeIndex] / 2) * direction;
        
        // Add widths of items between active and this one, plus gaps
        for (let step = 1; step <= steps; step++) {
          let idx = activeIndex + (step * direction);
          // Wrap index
          if (idx < 0) idx += itemCount;
          if (idx >= itemCount) idx -= itemCount;
          
          if (step === steps) {
            // For the target item, add half its width
            xOffset += (widths[idx] / 2 + gap) * direction;
          } else {
            // For items in between, add full width plus gap
            xOffset += (widths[idx] + gap) * direction;
          }
        }
      }
      
      const angle = relativePos * anglePerPosition;
      const zOffset = 200 - (Math.abs(relativePos) * depthFactor);
      
      $(this).css({
        'position': 'absolute',
        'left': '50%',
        'top': '50%',
        'transform': 'translateX(calc(-50% + ' + xOffset + 'px)) translateY(-50%) rotateY(' + angle + 'deg) translateZ(' + zOffset + 'px)',
        'transform-origin': 'center center',
        'transition': 'transform 0.5s cubic-bezier(0.25, 0.1, 0.25, 1), opacity 0.5s ease'
      });
      
      $(this).attr('data-relative-pos', relativePos);
      
      // Set depth based on position
      const absPos = Math.abs(relativePos);
      let depth;
      if (absPos === 0) {
        depth = 'front';
      } else if (absPos === 1) {
        depth = 'near';
      } else {
        depth = 'side';
      }
      $(this).attr('data-depth', depth);
    });
  }
  
  // Initialize the carousel
  function initCylinderCarousel() {
    const $track = $('.nav-sports-track, .nav-sports');
    const $items = $track.find('.sport-nav-item');
    const itemCount = $items.length;
    
    if (itemCount === 0) return;
    
    // Store item count
    $track.data('itemCount', itemCount);
    
    // Find initially active item
    let activeIndex = 0;
    $items.each(function(i) {
      if ($(this).hasClass('active')) {
        activeIndex = i;
      }
      $(this).attr('data-index', i);
    });
    
    $track.data('activeIndex', activeIndex);
    
    // Position items around active
    positionItemsAroundActive($track, activeIndex);
  }
  
  // Navigate to a specific sport
  function navigateToSport(sportId, nsPrefix) {
    const $track = $('.nav-sports-track, .nav-sports');
    const $items = $track.find('.sport-nav-item');
    const $target = $('#' + nsPrefix + 'nav_' + sportId);
    
    if ($target.length === 0) return;
    
    const targetIndex = parseInt($target.attr('data-index')) || 0;
    
    // Update active state
    $items.removeClass('active');
    $target.addClass('active');
    
    // Store new active index
    $track.data('activeIndex', targetIndex);
    
    // Reposition items around new active
    positionItemsAroundActive($track, targetIndex);
  }
  
  // Navigate by direction (-1 = left, +1 = right)
  function navigateByDirection(direction, nsPrefix) {
    const $track = $('.nav-sports-track, .nav-sports');
    const $items = $track.find('.sport-nav-item');
    const itemCount = $track.data('itemCount') || $items.length;
    const currentIndex = $track.data('activeIndex') || 0;
    
    // Calculate new index with wrapping
    let newIndex = (currentIndex + direction + itemCount) % itemCount;
    
    // Find the sport at that index
    const $newActive = $items.filter('[data-index="' + newIndex + '"]');
    if ($newActive.length) {
      const sportId = $newActive.attr('data-sport') || $newActive.attr('id').replace(/.*nav_/, '');
      
      // Trigger Shiny input update
      Shiny.setInputValue(nsPrefix + 'selected_sport', sportId, {priority: 'event'});
    }
  }
  
  // Initialize carousel when document is ready
  setTimeout(initCylinderCarousel, 100);
  
  // Reinitialize if Shiny re-renders the nav
  $(document).on('shiny:value', function(event) {
    if ($(event.target).find('.nav-sports-track, .nav-sports').length) {
      setTimeout(initCylinderCarousel, 100);
    }
  });
  
  // Arrow click handlers
  $(document).on('click', '.nav-arrow-left', function() {
    const nsPrefix = $(this).closest('.nav-tier-1').find('.sport-nav-item').first().attr('id').replace(/nav_.*/, '');
    navigateByDirection(-1, nsPrefix);
  });
  
  $(document).on('click', '.nav-arrow-right', function() {
    const nsPrefix = $(this).closest('.nav-tier-1').find('.sport-nav-item').first().attr('id').replace(/nav_.*/, '');
    navigateByDirection(1, nsPrefix);
  });
  
  Shiny.addCustomMessageHandler('updateSportNav', function(data) {
    const prefix = data.ns_prefix;
    const sport = data.sport;
    
    // Navigate to the selected sport
    navigateToSport(sport, prefix);
  });
  
  // -------------------------------------------------------------------------
  // SECTION NAVIGATION (Level 2)
  // -------------------------------------------------------------------------
  
  Shiny.addCustomMessageHandler('updateSectionNav', function(data) {
    const prefix = data.ns_prefix;
    const section = data.section;
    
    // Remove active from all section nav items in this namespace
    $('[id^="' + prefix + 'section_"]').removeClass('active');
    
    // Add active to selected
    $('#' + prefix + 'section_' + section).addClass('active');
  });
  
  // -------------------------------------------------------------------------
  // BUTTON INTERACTIONS
  // -------------------------------------------------------------------------
  
  $(document).on('mousedown', '.btn, button, .action-button', function() {
    if (!$(this).hasClass('nav-arrow')) {
      $(this).addClass('app-btn-press');
    }
  });
  
  $(document).on('mouseup mouseleave', '.btn, button, .action-button', function() {
    if (!$(this).hasClass('nav-arrow')) {
      $(this).removeClass('app-btn-press');
    }
  });
  
  // -------------------------------------------------------------------------
  // CARD ANIMATIONS
  // -------------------------------------------------------------------------
  
  $(document).on('shiny:value', function(event) {
    const $target = $(event.target);
    if ($target.closest('.card, .value-box').length) {
      $target.closest('.card, .value-box').addClass('app-fade-in');
    }
  });
  
  // -------------------------------------------------------------------------
  // SELECTIZE REINITIALIZATION FOR DYNAMIC CONTENT
  // -------------------------------------------------------------------------
  
  // When Shiny renders new UI content, selectize may need to be rebound
  $(document).on('shiny:value', function(event) {
    // Small delay to let DOM settle
    setTimeout(function() {
      // Find any uninitialized selectize inputs and trigger Shiny's binding
      $('.selectize-input').each(function() {
        var $input = $(this).closest('.shiny-input-container').find('select');
        if ($input.length && !$input.data('selectize-initialized')) {
          $input.data('selectize-initialized', true);
        }
      });
    }, 100);
  });
  
  // -------------------------------------------------------------------------
  // INITIALIZATION
  // -------------------------------------------------------------------------
  
  console.log('Sports Analytics JS initialized');
  
});