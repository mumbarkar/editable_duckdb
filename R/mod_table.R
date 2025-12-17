#' Module UI for editable table
#' @param id module id
#' @export
mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Bold styles for summary text
    tags$style(HTML(
      paste0(
        "#", ns('record_count'), ", ",
        "#", ns('column_count'), ", ",
        "#", ns('avg_mpg'), ", ",
        "#", ns('avg_hp'), ", ",
        "#", ns('modified_cells'), " { font-weight:bold; font-size:1.5em; }"
      )
    )),
    shiny::fluidPage(
      shiny::fluidRow(
        # Action buttons container
        shiny::div(
          style = "margin-bottom:12px; display:flex; justify-content:space-between;
                     border: 0px solid #ddd; border-radius: 8px; padding: 12px 16px; background: #fafafa;",
          shiny::actionButton(ns("save_btn"), "Save Changes", icon = shiny::icon("save"), class = "btn-primary"),
          shiny::actionButton(ns("revert_btn"), "Revert Changes", icon = shiny::icon("undo"), class = "btn-warning")
        ),
        # Left side (table)
        shiny::column(
          width = 9,
          # Table container with border
          shiny::div(
            class = "table-container",
            style = "min-height:600px; border: 1px solid #ddd; border-radius: 8px; padding: 12px;",
            shiny::h3("Data Table", style = "margin:0 0 10px 0;"),
            DT::DTOutput(ns("table"))
          )
        ),

        # Right side (summary)
        shiny::column(
          width = 3,
          shiny::div(
            style = "border: 1px solid #ccc; border-radius:8px; padding: 16px; background:#fdfdfd; min-height:600px;",
            shiny::h3("Summary", style = "margin:0 0 10px 0;"),

            # Inner cards
            shiny::div(
              style = "display:flex; flex-direction:column; gap:12px;",
              shiny::div(
                style = "border: 1px solid #ddd; border-radius: 8px; padding: 12px; background: #fff;",
                shiny::h5("Records", style = "margin:0 0 5px 0;"),
                shiny::textOutput(ns("record_count"))
              ),

              # Columns
              shiny::div(
                style = "border: 1px solid #ddd; border-radius: 8px; padding: 12px; background: #fff;",
                shiny::h5("Columns", style = "margin:0 0 5px 0;"),
                shiny::textOutput(ns("column_count"))
              ),

              # Avg MPG
              shiny::div(
                style = "border: 1px solid #ddd; border-radius: 8px; padding: 12px; background: #fff;",
                shiny::h5("Average MPG", style = "margin:0 0 5px 0;"),
                shiny::textOutput(ns("avg_mpg"))
              ),

              # Avg HP
              shiny::div(
                style = "border: 1px solid #ddd; border-radius: 8px; padding: 12px; background: #fff;",
                shiny::h5("Average HP", style = "margin:0 0 5px 0;"),
                shiny::textOutput(ns("avg_hp"))
              ),

              # Modified cells
              shiny::div(
                style = "border: 1px solid #ddd; border-radius: 8px; padding: 12px; background: #fff;",
                shiny::h5("Modified", style = "margin:0 0 5px 0;"),
                shiny::textOutput(ns("modified_cells"))
              ),

              # Status message
              shiny::div(style = "margin-top:8px;",
                         shiny::verbatimTextOutput(ns("status_message"), placeholder = TRUE)
              )
            )
          )
        )
      )
    )
  )
}


#' Module server for editable table
#' @param id module id
#' @param store R6 DataStore instance
#' @export
mod_table_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    rv_data <- reactiveVal(store$data)
    last_edit <- reactiveVal(NULL)
    edit_count <- reactiveVal(0)

    output$table <- DT::renderDT({
      DT::datatable(rv_data(), editable = "cell", options = list(pageLength = 10))
    })

    proxy <- DT::dataTableProxy(session$ns("table"))

    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      row <- info$row
      col <- info$col
      new_val <- DT::coerceValue(info$value, rv_data()[row, col])

      df <- rv_data()
      df[row, col] <- new_val
      rv_data(df)

      last_edit(list(row = row, col = col, value = new_val))
      edit_count(edit_count() + 1)

      DT::replaceData(proxy, df, resetPaging = FALSE)
    })

    observeEvent(input$revert_btn, {
      rv_data(store$original)
      edit_count(0)
      last_edit(NULL)
      DT::replaceData(proxy, store$original, resetPaging = TRUE)
    })

    observeEvent(input$save_btn, {
      store$data <- rv_data()
      # Write back to DuckDB
      tryCatch({
        store$save_to_db()
        showNotification("Saved to database!", type = "message")
      }, error = function(e) {
        showNotification("Save failed.", type = "error")
      })
    })

    output$record_count <- renderText(paste(nrow(rv_data())))
    output$column_count <- renderText(paste(ncol(rv_data())))
    output$avg_mpg <- renderText({
      if ("mpg" %in% colnames(rv_data())) {
        paste(round(mean(rv_data()$mpg, na.rm = TRUE), 3))
      }
    })
    output$avg_hp <- renderText({
      if ("hp" %in% colnames(rv_data())) {
        paste(round(mean(rv_data()$hp, na.rm = TRUE), 2))
      }
    })
    output$modified_cells <- renderText(paste(edit_count(), "cells"))

    output$status_message <- renderText({
      ed <- last_edit()
      if (is.null(ed)) ""
      else paste0("Last edit: row=", ed$row, " col=", ed$col, " val=", ed$value)
    })

    return(list(data = rv_data, last_edit = last_edit, edit_count = edit_count))
  })
}
