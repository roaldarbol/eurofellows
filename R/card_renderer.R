# Enhanced Card rendering function for fellowship display
# NOTE: This version assumes CSS is loaded externally from style.css

#' Calculate days until deadline and return urgency styling
#' 
#' @param deadline_str String in format "YYYY-MM-DD"
#' @return List with days_left and urgency_class
calculate_urgency <- function(deadline_str) {
  # Handle NULL, NA, empty string, or missing values
  if (is.null(deadline_str) || length(deadline_str) == 0) {
    return(list(days_left = NA, urgency_class = "urgency-unknown", urgency_text = "Unknown"))
  }
  
  # Convert to character and check for problematic values
  deadline_str <- as.character(deadline_str)
  if (is.na(deadline_str) || deadline_str == "" || deadline_str == "NA" || deadline_str == "<NA>") {
    return(list(days_left = NA, urgency_class = "urgency-unknown", urgency_text = "Unknown"))
  }
  
  # Try to parse the date safely
  tryCatch({
    deadline_date <- as.Date(deadline_str)
    if (is.na(deadline_date)) {
      return(list(days_left = NA, urgency_class = "urgency-unknown", urgency_text = "Unknown"))
    }
  }, error = function(e) {
    return(list(days_left = NA, urgency_class = "urgency-unknown", urgency_text = "Unknown"))
  })
  today <- Sys.Date()
  days_left <- as.numeric(deadline_date - today)
  
  if (days_left < 0) {
    urgency_class <- "urgency-passed"
    urgency_text <- "Deadline passed"
  } else if (days_left < 30) {
    urgency_class <- "urgency-critical"
    urgency_text <- paste(days_left, "days left")
  } else if (days_left < 90) {
    urgency_class <- "urgency-warning"
    urgency_text <- paste(ceiling(days_left / 7), "weeks left")
  } else {
    urgency_class <- "urgency-normal"
    if (days_left < 365) {
      urgency_text <- paste(ceiling(days_left / 30), "months left")
    } else {
      urgency_text <- paste(ceiling(days_left / 365), "years left")
    }
  }
  
  list(days_left = days_left, urgency_class = urgency_class, urgency_text = urgency_text)
}

#' Format deadline display
#' 
#' @param deadline_str String in format "YYYY-MM-DD"
#' @return Formatted date string
format_deadline <- function(deadline_str) {
  # Handle NULL, NA, empty string, or missing values
  if (is.null(deadline_str) || length(deadline_str) == 0) {
    return("TBD")
  }
  
  # Convert to character and check for problematic values
  deadline_str <- as.character(deadline_str)
  if (is.na(deadline_str) || deadline_str == "" || deadline_str == "NA" || deadline_str == "<NA>") {
    return("TBD")
  }
  
  # Try to parse and format the date safely
  tryCatch({
    deadline_date <- as.Date(deadline_str)
    if (is.na(deadline_date)) {
      return("TBD")
    }
    format(deadline_date, "%b %d, %Y")
  }, error = function(e) {
    return("TBD")
  })
}

#' Create eligibility criteria badges
#' 
#' @param row Data row with eligibility information
#' @return HTML div with badges or NULL
create_eligibility_badges <- function(row) {
  badges <- c()
  
  # PhD experience requirements
  if (!is.na(row$minimum_years_post_phd) && row$minimum_years_post_phd != "") {
    badges <- c(badges, paste("Min", row$minimum_years_post_phd, "years post-PhD"))
  }
  
  if (!is.na(row$maximum_years_in_postdoc) && row$maximum_years_in_postdoc != "") {
    badges <- c(badges, paste("Max", row$maximum_years_in_postdoc, "years post-PhD"))
  }
  
  # Mobility requirements
  if (!is.na(row$requires_mobility) && row$requires_mobility) {
    badges <- c(badges, "Mobility required")
  }
  
  # PhD requirements
  if (!is.na(row$requires_phd) && row$requires_phd) {
    badges <- c(badges, "PhD required")
  }
  
  # Publication requirements
  if (!is.na(row$requires_publication) && row$requires_publication) {
    badges <- c(badges, "Publications required")
  }
  
  if (length(badges) == 0) return(NULL)
  
  badge_elements <- lapply(badges, function(badge) {
    # Use different icons for different types of requirements
    icon <- if (grepl("years", badge)) "🏆" 
    else if (grepl("Mobility", badge)) "✈️"
    else if (grepl("PhD", badge)) "🎓"
    else if (grepl("Publications", badge)) "📚"
    else "📋"
    
    tags$div(
      class = "special-note",
      tags$span(class = "detail-icon", icon),
      badge
    )
  })
  
  tags$div(
    class = "special-notes",
    do.call(tagList, badge_elements)
  )
}

#' Render a fellowship card
#' 
#' This function creates the HTML structure for each fellowship card
#' displayed in the reactable with modern styling and urgency indicators.
#' 
#' @param value The value from the Title column (not used directly)
#' @param index The row index in the data
#' @return HTML tags representing the fellowship card
render_fellowship_card <- function(value, index) {
  # Get the full row data (assumes 'data' is available in parent environment)
  row <- data[index, ]
  
  # Calculate deadline urgency
  urgency_info <- calculate_urgency(row$application_deadline)
  formatted_deadline <- format_deadline(row$application_deadline)
  
  # Card container with urgency-based styling
  tags$div(
    class = paste("fellowship-card", urgency_info$urgency_class),
    
    # Card header with title and funder
    tags$div(
      class = "card-header",
      tags$h3(
        class = "fellowship-title",
        row$fellowship_title
      ),
      tags$p(
        class = "fellowship-funder",
        row$fellowship_funder
      )
    ),
    
    # Key details grid
    tags$div(
      class = "card-details-grid",
      
      # Location
      tags$div(
        class = "detail-item",
        tags$span(class = "detail-icon location-icon", "📍"),
        tags$span(class = "detail-text", 
                  if(is.na(row$eligible_host_location) || row$eligible_host_location == "") "Location TBD" else row$eligible_host_location
        )
      ),
      
      # Duration
      tags$div(
        class = "detail-item",
        tags$span(class = "detail-icon duration-icon", "⏱️"),
        tags$span(class = "detail-text", 
                  if(is.na(row$fellowship_duration)) "Duration TBD" else paste(row$fellowship_duration, "years")
        )
      )
    ),
    
    # Deadline with urgency indicator
    tags$div(
      class = "deadline-section",
      tags$span(class = "detail-icon deadline-icon", "📅"),
      tags$span(class = "deadline-date", formatted_deadline),
      if (!is.na(urgency_info$days_left)) {
        tags$span(
          class = paste("urgency-badge", urgency_info$urgency_class),
          urgency_info$urgency_text
        )
      }
    ),
    
    # Academic fields
    if (!is.na(row$eligible_fields) && row$eligible_fields != "") {
      tags$div(
        class = "fields-section",
        tags$span(class = "detail-icon fields-icon", "🔬"),
        tags$span(class = "fields-text", row$eligible_fields)
      )
    },
    
    # Eligibility criteria as compact badges
    create_eligibility_badges(row),
    
    # Action button
    tags$div(
      class = "card-actions",
      tags$a(
        href = row$fellowship_url,
        target = "_blank",
        class = "learn-more-btn",
        "Learn More",
        tags$span(class = "external-icon", "↗")
      )
    ),
    
    # Attribution
    if (!is.na(row$contributor_name) && row$contributor_name != "") {
      tags$div(
        class = "card-attribution",
        paste("Added by", row$contributor_name)
      )
    }
  )
}