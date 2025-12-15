#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import R6
#' @import duckdb
#' @noRd
app_server <- function(input, output, session) {
  # create the R6 store
  store <- DataStore$new()

  # refresh trigger that module can observe
  refresh_trigger <- shiny::reactiveVal(0)

  # call the table module and capture its return value (pass refresh trigger)
  table_mod <- mod_table_server("table", store, refresh = refresh_trigger)

  # track modified cells count
  modified_count <- shiny::reactiveVal(0)

  # render summary fields required by wireframe
  output$record_count <- shiny::renderText({
    paste("Records:", if (is.null(store$data)) 0 else nrow(store$data))
  })
  output$column_count <- shiny::renderText({
    paste("Columns:", if (is.null(store$data)) 0 else ncol(store$data))
  })
  output$avg_mpg <- shiny::renderText({
    if (is.null(store$data) || !"mpg" %in% colnames(store$data)) return("Avg MPG: N/A")
    paste0("Avg MPG: ", round(mean(store$data$mpg, na.rm = TRUE), 3))
  })
  output$avg_hp <- shiny::renderText({
    if (is.null(store$data) || !"hp" %in% colnames(store$data)) return("Avg HP: N/A")
    paste0("Avg HP: ", round(mean(store$data$hp, na.rm = TRUE), 2))
  })
  output$modified_cells <- shiny::renderText({
    paste("Modified cells:", modified_count())
  })

  # status message area
  output$status_message <- shiny::renderText({ "" })

  # increment modified counter whenever module reports a last_edit
  shiny::observeEvent(table_mod$last_edit(), {
    edit <- table_mod$last_edit()
    if (is.null(edit)) return()
    # increment modified counter only if update succeeded (DataStore returns success list or invisible(TRUE))
    modified_count(modified_count() + 1)
    output$status_message <- shiny::renderText({
      paste0("Last edit - row: ", edit$row, ", col: ", edit$col, " value: ", edit$value)
    })
    # update summary panel values
    output$record_count <- shiny::renderText({ paste("Records:", nrow(store$data)) })
    output$column_count <- shiny::renderText({ paste("Columns:", ncol(store$data)) })
    output$avg_mpg <- shiny::renderText({
      if (!"mpg" %in% colnames(store$data)) return("Avg MPG: N/A")
      paste0("Avg MPG: ", round(mean(store$data$mpg, na.rm = TRUE), 3))
    })
    output$avg_hp <- shiny::renderText({
      if (!"hp" %in% colnames(store$data)) return("Avg HP: N/A")
      paste0("Avg HP: ", round(mean(store$data$hp, na.rm = TRUE), 2))
    })
  })

  # revert button: revert store, reset modified counter and notify module via refresh_trigger
  shiny::observeEvent(input$revert_btn, {
    store$revert()
    modified_count(0)
    refresh_trigger(refresh_trigger() + 1)
    output$status_message <- shiny::renderText("Reverted changes")
    # refresh summary
    output$record_count <- shiny::renderText({ paste("Records:", nrow(store$data)) })
    output$column_count <- shiny::renderText({ paste("Columns:", ncol(store$data)) })
    output$avg_mpg <- shiny::renderText({
      if (!"mpg" %in% colnames(store$data)) return("Avg MPG: N/A")
      paste0("Avg MPG: ", round(mean(store$data$mpg, na.rm = TRUE), 3))
    })
    output$avg_hp <- shiny::renderText({
      if (!"hp" %in% colnames(store$data)) return("Avg HP: N/A")
      paste0("Avg HP: ", round(mean(store$data$hp, na.rm = TRUE), 2))
    })
  })

  # save button: placeholder behaviour (implement DB save if desired)
  shiny::observeEvent(input$save_btn, {
    # placeholder: no-op save, update status
    output$status_message <- shiny::renderText("Save requested — not implemented")
  })
}
