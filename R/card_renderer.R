# Card rendering function for fellowship display

#' Render a fellowship card
#' 
#' This function creates the HTML structure for each fellowship card
#' displayed in the reactable.
#' 
#' @param value The value from the Title column (not used directly)
#' @param index The row index in the data
#' @return HTML tags representing the fellowship card
render_fellowship_card <- function(value, index) {
  # Get the full row data (assumes 'data' is available in parent environment)
  row <- data[index, ]
  
  # Create card header with title and deadline
  header <- tags$div(
    class = "card-header",
    tags$a(
      href = row$fellowship_url,
      class = "card-title",
      tags$span(
        class = "title-text", 
        paste(row$fellowship_funder, row$fellowship_title, sep = " - ")
      )
    ),
    tags$span(
      class = "deadline-text",
      row$application_deadline,
      HTML(" \U0001F5D3")  # Calendar emoji
    )
  )
  
  # Helper function to create field lines
  field_line <- function(label, val) {
    if (is.na(val) || val == "") return(NULL)
    tags$div(
      class = "card-field",
      tags$b(paste0(label, ":")), 
      " ", 
      as.character(val)
    )
  }
  
  # # Create expandable description
  # description_section <- NULL
  # if (!is.na(row$Description) && row$Description != "") {
  #   description_section <- tags$details(
  #     tags$summary(
  #       class = "summary-label",
  #       "Show description"
  #     ),
  #     tags$p(
  #       style = "margin:4px 0 0 0;line-height:1.4;",
  #       as.character(row$Description)
  #     )
  #   )
  # }
  
  # Create apply button
  apply_button <- tags$div(
    class = "button-wrapper",
    tags$a(
      href = row$fellowship_url,
      target = "_blank",
      class = "apply-btn",
      "Find out more →"
    )
  )
  
  # Assemble the complete card
  tags$div(
    class = "card",
    header,
    field_line("Duration", paste(row$fellowship_duration, "years")),
    field_line("Eligible host countries", row$eligible_host_location),
    field_line("Eligible academic fields", row$eligible_fields),
    # description_section,
    apply_button
  )
}