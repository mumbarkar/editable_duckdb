#' The application server-side
#' @noRd
app_server <- function(input, output, session) {

  store <- DataStore$new()

  table_mod <- mod_table_server("editable", store)

  # Listening to edits (increment behavior)
  shiny::observeEvent(table_mod$last_edit(), {
    edit <- table_mod$last_edit()
    if (!is.null(edit)) {
      message <- paste0("Main app saw edit: row=", edit$row,
                        " col=", edit$col,
                        " val=", edit$value)
      showNotification(message, type = "message")
    }
  })
}
