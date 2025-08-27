# Filter functions for the fellowship dashboard

#' Radio button filter for single selection with default
#' 
#' @param id Unique identifier for the filter
#' @param label Label text for the filter (not displayed in current implementation)
#' @param shared_data SharedData object from crosstalk
#' @param group Column name to filter on
#' @param width CSS width specification
#' @param class CSS class for styling
#' @param default_value Default selected value
radio_filter <- function(id, label, shared_data, group, width = "100%", 
                         class = "filter-input", default_value = "Postdoc") {
  
  # Get unique values from the column
  values <- unique(shared_data$data()[[group]]) |>
    na.omit() |>
    sort()
  
  keys <- shared_data$key()
  
  # Create mapping of value to keys
  value_to_keys <- list()
  for (i in seq_along(shared_data$data()[[group]])) {
    val <- shared_data$data()[[group]][i]
    if (!is.na(val)) {
      value_to_keys[[val]] <- c(value_to_keys[[val]], keys[i])
    }
  }
  
  # JavaScript for radio button functionality
  script <- sprintf("
    window['__ct__%s'] = (function() {
      const handle = new window.crosstalk.FilterHandle('%s')
      const map = %s
      
      // Set default filter
      setTimeout(function() {
        handle.set(map['%s'] || [])
      }, 100)
      
      return {
        filter: function(value) {
          handle.set(map[value] || [])
        }
      }
    })()
  ", id, shared_data$groupName(), toJSON(value_to_keys, auto_unbox = FALSE), default_value)
  
  # Create radio button group
  div(
    class = class,
    tags$div(
      class = "radio-group",
      lapply(values, function(v) {
        option_id <- paste0(id, "_", gsub("[^A-Za-z0-9]", "_", v))
        tags$div(
          class = "radio-option",
          tags$input(
            type = "radio",
            id = option_id,
            name = id,
            value = v,
            checked = if(v == default_value) NA else NULL,
            onchange = sprintf("window['__ct__%s'].filter(this.value)", id)
          ),
          tags$label(`for` = option_id, v)
        )
      })
    ),
    tags$script(HTML(script))
  )
}

#' Multi-select filter for columns with comma-separated values
#' 
#' @param id Unique identifier for the filter
#' @param label Label text for the filter
#' @param shared_data SharedData object from crosstalk
#' @param group Column name to filter on
#' @param width CSS width specification
#' @param class CSS class for styling
#' @param custom_order Optional vector specifying custom ordering of options
multi_select_filter <- function(id, label, shared_data, group, width = "100%", 
                                class = "filter-input", custom_order = NULL, catchall_input = "any") {
  
  # Extract and process values from comma-separated strings
  raw_vals <- shared_data$data()[[group]]
  keys <- shared_data$key()
  
  split_vals <- strsplit(raw_vals, ",\\s*") |>
    lapply(trimws)
  
  # Build value-to-keys mapping
  value_to_keys <- list()
  for (i in seq_along(split_vals)) {
    for (v in split_vals[[i]]) {
      lc_v <- tolower(v)
      value_to_keys[[lc_v]] <- c(value_to_keys[[lc_v]], keys[i])
    }
  }
  
  # Create option list
  raw_pieces <- unique(unlist(strsplit(raw_vals, ",\\s*"))) |>
    trimws() |>
    (\(x) x[x != "" & x != catchall_input])()
  
  # Apply custom ordering if provided
  if (!is.null(custom_order)) {
    custom_items <- intersect(custom_order, raw_pieces)
    remaining_items <- setdiff(raw_pieces, custom_order) |> sort()
    raw_pieces <- c(custom_items, remaining_items)
  } else {
    raw_pieces <- sort(raw_pieces)
  }
  
  opts_df <- data.frame(
    display = raw_pieces,
    value = tolower(raw_pieces),
    stringsAsFactors = FALSE
  )
  
  # JavaScript for multi-select functionality
  script <- sprintf("
    window['__ct__%s'] = (function() {
      const handle = new window.crosstalk.FilterHandle('%s')
      const map = %s
      return {
        filter: function() {
          const sel = Array.from(document.getElementById('%s').selectedOptions)
                           .map(o => o.value)
          if (sel.length === 0 || sel.includes('')) {
            handle.clear()
            return
          }
          const allKeys = sel.reduce((acc, val) => {
            const ks = map[val] || []
            ks.forEach(k => { if (!acc.includes(k)) acc.push(k) })
            return acc
          }, [])
          handle.set(allKeys)
        }
      }
    })()
  ", id, shared_data$groupName(), toJSON(value_to_keys, auto_unbox = FALSE), id)
  
  # Create select element
  div(
    class = class,
    tags$label(`for` = id, label),
    tags$select(
      id = id,
      multiple = NA,
      size = 4,
      onchange = sprintf("window['__ct__%s'].filter()", id),
      style = sprintf("width: %s", validateCssUnit(width)),
      tags$option(value = "", catchall_input),
      lapply(seq_len(nrow(opts_df)), function(i) {
        tags$option(value = opts_df$value[i], opts_df$display[i])
      })
    ),
    tags$script(HTML(script))
  )
}

#' Search filter for text-based filtering
#' 
#' @param id Unique identifier for the filter
#' @param label Label text for the filter
#' @param shared_data SharedData object from crosstalk
#' @param group Column name to filter on
#' @param width CSS width specification
#' @param class CSS class for styling
search_filter <- function(id, label, shared_data, group, width = "100%", 
                          class = "filter-input") {
  
  values <- as.list(shared_data$data()[[group]])
  keys <- shared_data$key()
  values_by_key <- setNames(values, keys)
  
  script <- sprintf("
    window['__ct__%s'] = (function() {
      const handle = new window.crosstalk.FilterHandle('%s')
      const valuesByKey = %s
      return {
        filter: function(value) {
          if (!value) {
            handle.clear()
          } else {
            value = value.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&')
            const regex = new RegExp(value, 'i')
            const filtered = Object.keys(valuesByKey).filter(k => {
              const v = valuesByKey[k]
              if (Array.isArray(v)) {
                return v.some(item => regex.test(item))
              } else {
                return regex.test(v)
              }
            })
            handle.set(filtered)
          }
        }
      }
    })()
  ", id, shared_data$groupName(), toJSON(values_by_key))
  
  div(
    class = class,
    tags$label(`for` = id, label),
    tags$input(
      id = id,
      type = "search",
      oninput = sprintf("window['__ct__%s'].filter(this.value)", id),
      style = sprintf("width: %s", validateCssUnit(width))
    ),
    tags$script(HTML(script))
  )
}