#' Module UI for editable table
#' @param id module id
#' @export
mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(ns("table"))
  )
}

#' Module server for editable table
#'
#' Expects an R6 DataStore instance passed as `store`.
#' Returns a list with reactive data: list(data = reactive(...), last_edit = reactive(...))
#'
#' @param id module id
#' @param store R6 DataStore instance (non-reactive). Module will maintain a reactive view of store$data.
#' @param refresh optional reactive trigger (e.g. reactiveVal) that when invalidated forces re-read from store
#' @export
mod_table_server <- function(id, store, refresh = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # reactive view of the store data
    rv <- shiny::reactiveVal(store$data)
    last_edit <- shiny::reactiveVal(NULL)

    # if caller provides a refresh reactive, update rv when it changes
    if (!is.null(refresh) && is.reactive(refresh)) {
      shiny::observeEvent(refresh(), {
        rv(store$data)
      }, ignoreNULL = FALSE)
    }

    # render DT table (editable)
    output$table <- DT::renderDataTable({
      df <- rv()
      if (is.null(df)) df <- data.frame()
      # Use DT::datatable for editing
      DT::datatable(
        df,
        rownames = FALSE,
        filter = 'top',
        selection = 'single',
        options = list(pageLength = 25, scrollX = TRUE),
        editable = TRUE
      )
    }, server = FALSE)

    # handle cell edits from DT
    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      # info has row (1-based), col (1-based), value
      if (is.null(info)) return()
      row <- info$row
      col <- info$col
      value <- info$value

      # Attempt to resolve column name from current rv data
      df <- rv()
      colname <- if (!is.null(df) && ncol(df) >= col) colnames(df)[col] else col

      res <- try(store$update_cell(row = row, col = colname, value = value), silent = TRUE)
      last_edit(list(row = row, col = colname, value = value, result = res))
      # refresh reactive view from store
      rv(store$data)
    }, ignoreNULL = TRUE)

    # expose reactive data and last_edit
    list(
      data = shiny::reactive(rv()),
      last_edit = shiny::reactive(last_edit())
    )
  })
}
