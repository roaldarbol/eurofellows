# Simple solution: Use filter_checkbox styled as a toggle
create_career_stage_filter <- function(shared_data) {
  
  tagList(
    # Standard crosstalk filter (will be styled with CSS)
    tags$div(
      class = "career-stage-wrapper",
      crosstalk::filter_checkbox(
        id = "career_stage_toggle",
        label = NULL,  # We'll create custom labels
        shared_data,
        ~career_stage,
        inline = TRUE
      )
    ),
    
    # JavaScript to handle the postdoc-specific filters visibility
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        
        function updatePostdocFiltersVisibility() {
          // Get the checkbox states
          const phdCheckbox = document.querySelector('input[value=\"PhD\"]');
          const postdocCheckbox = document.querySelector('input[value=\"Postdoc\"]');
          const advancedFilters = document.getElementById('postdoc-advanced-filters');
          
          if (phdCheckbox && postdocCheckbox && advancedFilters) {
            // Show postdoc filters only if Postdoc is checked
            if (postdocCheckbox.checked) {
              advancedFilters.style.display = 'block';
              advancedFilters.style.opacity = '1';
            } else {
              advancedFilters.style.display = 'none';
            }
          }
        }
        
        // Wait for crosstalk to initialize
        setTimeout(function() {
          // Find and attach listeners to the checkboxes
          const phdCheckbox = document.querySelector('input[value=\"PhD\"]');
          const postdocCheckbox = document.querySelector('input[value=\"Postdoc\"]');
          
          if (phdCheckbox && postdocCheckbox) {
            
            // Add change listeners
            phdCheckbox.addEventListener('change', updatePostdocFiltersVisibility);
            postdocCheckbox.addEventListener('change', updatePostdocFiltersVisibility);
            
            // Set initial state - default to Postdoc only
            phdCheckbox.checked = true;
            postdocCheckbox.checked = true;
            
            // Trigger change events to apply the filter
            postdocCheckbox.dispatchEvent(new Event('change'));
            
            // Update visibility
            updatePostdocFiltersVisibility();
            
            console.log('Career stage filter initialized with Postdoc selected');
          }
        }, 200);
      });
    "))
  )
}

create_compact_filters <- function(shared_fellowships) {
  
  tags$div(
    class = "filter-box compact-layout",
    
    # Header with career stage toggle
    tags$div(
      class = "filter-header",
      tags$div(
        class = "filter-title-section",
        tags$h4(class = "filter-title", 
                tags$span(class = "filter-icon", fa("filter", fill = "#3b82f6")), 
                "Filter Fellowships"
        )
      ),
      # tags$div(
      #   # class = "career-pills",
      #   create_career_stage_filter(shared_fellowships),
      #   # crosstalk::filter_checkbox(
      #   #   id = "career_stage",
      #   #   label = NULL, 
      #   #   shared_fellowships,
      #   #   ~career_stage,
      #   #   inline = TRUE
      #   #   # multiple = TRUE
      #   # )
      # )
    ),
    
    # Main filters in compact grid
    tags$div(
      class = "filters-grid-main",
      
      # Row 1: Core filters
      tags$div(
        class = "filter-row-triple",
        
        # Host countries
        tags$div(
          class = "filter-group compact",
          reactable_select_filter(
            "host_country",
            label =  tags$label(class = "filter-label",
                                tags$span(class = "filter-icon", fa("location-dot")), 
                                "Host country"),
            shared_fellowships,
            "host_country"
          )
        ),
        
        # Fields
        tags$div(
          class = "filter-group compact",
          reactable_select_filter(
            "academic_field", 
            label = tags$label(class = "filter-label", 
                               tags$span(class = "filter-icon", fa("microscope")), 
                               "Academic Field"
            ),
            shared_fellowships, 
            "academic_field"
          )
        ),
        
        tags$div(
          class = "filter-group compact",
          reactable_select_filter(
            "starting_grant",
            label = tags$label(class = "filter-label", fa("plane-departure"), "Starting Grant"),
            shared_fellowships,
            "starting_grant"
          )
        )
      ),
      
      # Row 2: Requirements and deadline
      tags$div(
        class = "filter-row-double",
        
        # Duration
        tags$div(
          class = "filter-group compact",
          crosstalk::filter_slider(
            id = "fellowship_duration",
            label = tags$label(class = "filter-label",
                               tags$span(class = "filter-icon", fa("far fa-clock")), 
                               "Duration (max)"
            ),
            sharedData = shared_fellowships,
            column = ~fellowship_duration,
            post = "y",
            step = 1,
            ticks = FALSE
          )
        ),
        
        # Deadline range
        tags$div(
          class = "filter-group deadline-range",
          crosstalk::filter_slider(
            id = "application_deadline",
            label = tags$label(class = "filter-label",
                               tags$span(class = "filter-icon", fa("calendar")), 
                               "Application Deadline"
            ),
            sharedData = shared_fellowships,
            column = ~application_deadline,
            round = TRUE,
            ticks = FALSE
          )
        )
      )
    ),
    
    hr(),
    
    # Postdoc-only filters (collapsible section)
    tags$div(
      id = "postdoc-advanced-filters",
      class = "filters-grid-main",
      # style = "display: block;", # Show by default for postdoc
      
      # tags$h5("Postdoc-specific"),
      tags$div(
        class = "filter-row-quad",
        
        # Requirements (compact dropdowns)
        tags$div(
          class = "filter-group compact",
          reactable_select_filter(
            "connection_country", 
            label = tags$label(class = "filter-label", fa("flag"), "Connection to Country"),
            shared_fellowships, 
            "connection_country",
            choices = c(
              "Not required",
              sort(setdiff(unique(data$connection_country), "Not required"))
            )
          )
        ),
        tags$div(
          class = "filter-group compact",
          reactable_select_filter(
            "requires_mobility",
            label = tags$label(class = "filter-label", fa("plane-departure"), "Mobility"),
            shared_fellowships,
            "requires_mobility"
          )
        ),
        tags$div(
          class = "filter-group compact",
          reactable_select_filter(
            "requires_phd",
            label = tags$label(class = "filter-label", fa("graduation-cap"), "PhD (by application deadline)"),
            shared_fellowships,
            "requires_phd" #~stringr::str_to_title(requires_phd)
          )
        ),
        tags$div(
          class = "filter-group compact",
          reactable_select_filter(
            "requires_publication",
            label = tags$label(class = "filter-label", fa("far fa-pen-to-square"), "Publication"),
            shared_fellowships,
            "requires_publication"#~stringr::str_to_title(requires_publication),
          )
        )
      ),
      tags$div(
        class = "filter-row-double",
        
        # Min years
        tags$div(
          class = "filter-group compact",
          crosstalk::filter_slider(
            id = "min-years-filter",
            label = tags$label(class = "filter-label",
                               tags$span(class = "filter-icon", fa("hourglass-start")), 
                               "Minimum years post-PhD"
            ),
            sharedData = shared_fellowships,
            column = ~minimum_years_post_phd,
            post = "y",
            step = 1,
            ticks = FALSE
          )
        ),
        
        # Max years
        tags$div(
          class = "filter-group compact",
          crosstalk::filter_slider(
            id = "max-years-filter",
            label = tags$label(class = "filter-label",
                               tags$span(class = "filter-icon", fa("hourglass-end")), 
                               "Maximum years post-PhD"
            ),
            sharedData = shared_fellowships,
            column = ~maximum_years_post_phd,
            post = "y",
            step = 1,
            ticks = FALSE
          )
        )
      )
      
      # tags$div(
      #   class = "experience-sliders",
      #   tags$div(
      #     id = "min-years-filter",
      #     class = "slider-container",
      #     crosstalk::filter_slider(
      #       id = "minimum_years_post_phd",
      #       "Minimum years post-PhD",
      #       sharedData = shared_fellowships,
      #       column = ~minimum_years_post_phd,
      #       post = "y",
      #       step = 1,
      #       ticks = FALSE
      #     )
      #   ),
      #   tags$div(
      #     id = "max-years-filter",
      #     class = "slider-container",
      #     crosstalk::filter_slider(
      #       id = "maximum_years_post_phd",
      #       "Maximum years post-PhD",
      #       sharedData = shared_fellowships,
      #       column = ~maximum_years_post_phd,
      #       post = "y",
      #       step = 1,
      #       ticks = FALSE
      #     )
      #   )
      # )
    ),
    
    # Slider toggle functionality
    # tags$script(HTML("
    #   document.addEventListener('DOMContentLoaded', function() {
    #     function bindSliderToggle(checkboxId, sliderId) {
    #       const checkbox = document.getElementById(checkboxId);
    #       const sliderDiv = document.getElementById(sliderId);
    #       
    #       if (checkbox && sliderDiv) {
    #         function updateSliderVisibility() {
    #           sliderDiv.style.display = checkbox.checked ? 'block' : 'none';
    #         }
    #         
    #         checkbox.addEventListener('change', updateSliderVisibility);
    #         updateSliderVisibility(); // Initial state
    #       }
    #     }
    #     
    #     bindSliderToggle('toggle-min-years', 'min-years-filter');
    #     bindSliderToggle('toggle-max-years', 'max-years-filter');
    #   });
    # "))
  )
}