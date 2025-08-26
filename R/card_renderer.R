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
      href = row$Url,
      class = "card-title",
      tags$span(
        class = "title-text", 
        paste(row$Funder, row$Title, sep = " - ")
      )
    ),
    tags$span(
      class = "deadline-text",
      row$Deadline,
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
  
  # Create expandable description
  description_section <- NULL
  if (!is.na(row$Description) && row$Description != "") {
    description_section <- tags$details(
      tags$summary(
        class = "summary-label",
        "Show description"
      ),
      tags$p(
        style = "margin:4px 0 0 0;line-height:1.4;",
        as.character(row$Description)
      )
    )
  }
  
  # Create apply button
  apply_button <- tags$div(
    class = "button-wrapper",
    tags$a(
      href = row$Url,
      target = "_blank",
      class = "apply-btn",
      "Find out more →"
    )
  )
  
  # Assemble the complete card
  tags$div(
    class = "card",
    header,
    field_line("Country", row$Country),
    field_line("Discipline", row$Discipline),
    field_line("Duration", row$Duration),
    description_section,
    apply_button
  )
}