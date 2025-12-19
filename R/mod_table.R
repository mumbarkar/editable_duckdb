#' Create a reusable modal helper
#'
#' @param id ID
#' @param title Title of the modal
#' @param message Modal message
#' @param confirm_label Confirmation Label
#'
#' @returns A modal dialogue box
#' @export
#'
#' @examples
confirm_modal <- function(session, id, title, message, confirm_id,
                          confirm_label = "Confirm", danger = FALSE) {

  ns <- session$ns

  modalDialog(
    title = title,
    message,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        ns(paste0(id, "_", confirm_id)),
        confirm_label,
        class = if (danger) "btn-danger" else "btn-primary"
      )
    ),
    easyClose = FALSE
  )
}


# ===============================
# Helper: Summary block UI
# ===============================
summary_block <- function(ns, title, value_ui) {
  div(
    class = "mb-2 p-2 bg-light rounded",
    style = "border: 0.5px solid #e3e6ea;",
    div(
      class = "text-muted fw-semibold",
      style = "
        font-size: 0.7rem;
        letter-spacing: 0.4px;
        margin-bottom: 2px;",
      title
    ),
    div(
      style = "
        font-size: 1.3rem;
        font-weight: 600;
        line-height: 1.2;",
      value_ui
    )
  )
}


#' Module UI for Home / Editable table
#' @param id module id
#' @export
mod_table_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    div(
      class = "container-fluid py-4",
      style = "max-width: 1400px; margin: 0 auto;",

      # ---- HEADER ----
      div(
        class = "mb-4",
        h2("MTCars Dataset", class = "mb-2"),
        p(
          "Interactive data table with real-time editing",
          class = "text-muted",
          style = "font-size: 1.1rem;"
        )
      ),

      # ---- ACTION BUTTONS ----
      div(
        class = "d-flex justify-content-between mb-4",
        actionButton(
          ns("save_btn"),
          "Save Changes",
          icon = icon("save"),
          class = "btn-outline-secondary",
          style = "border-radius: 8px; padding: 8px 20px;"
        ),
        actionButton(
          ns("revert_btn"),
          "Revert Changes",
          icon = icon("rotate-left"),
          class = "btn-outline-danger",
          style = "border-radius: 8px; padding: 8px 20px;"
        )
      ),

      # ---- MAIN LAYOUT ----
      bslib::layout_columns(
        col_widths = c(8, 4),

        # ===== LEFT: TABLE =====
        bslib::card(
          class = "shadow-sm",
          style = "border-radius: 12px;",
          bslib::card_header(
            h5("Data Table", class = "mb-0 fw-bold")
          ),
          bslib::card_body(
            style = "padding: 0;",
            div(
              class = "table-container",
              style = "
                min-height:600px;
                border: 1px solid #e6e6e6;
                border-radius: 8px;
                padding: 12px;",
              hotwidgetOutput(ns("table"), width = '100%', height = '100%')
            )
          )
        ),

        # ===== RIGHT: SUMMARY =====
        bslib::card(
          class = "shadow-sm",
          style = "border-radius: 12px;",
          bslib::card_header(
            h5("Summary", class = "mb-0 fw-bold")
          ),
          bslib::card_body(

            summary_block(ns, "Records", textOutput(ns("record_count"), inline = TRUE)),
            summary_block(ns, "Columns", textOutput(ns("column_count"), inline = TRUE)),
            summary_block(ns, "Avg MPG", textOutput(ns("avg_mpg"), inline = TRUE)),
            summary_block(ns, "Avg HP", textOutput(ns("avg_hp"), inline = TRUE)),
            summary_block(ns, "Modified", textOutput(ns("modified_cells"), inline = TRUE)),

            div(
              class = "mt-3",
              verbatimTextOutput(ns("status_message"), placeholder = TRUE)
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
    pending_action <- shiny::reactiveVal(NULL)

    output$table <- renderHotwidget({
      hotwidget(rv_data(), options = list())
    })

    # handle single-cell edits coming from the hotwidget JS
    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      row <- as.integer(info$row)
      col <- as.integer(info$col)
      new_raw <- info$value

      # conservative server-side coercion based on current column class
      coerce_value <- function(val, col_idx) {
        if (is.null(val)) return(NA)
        cls <- class(store$data[[col_idx]])
        if (any(cls %in% c("numeric", "integer"))) {
          v <- suppressWarnings(as.numeric(val))
          if (is.na(v) && !is.na(val) && nzchar(as.character(val))) return(NA_real_)
          return(v)
        }
        if (any(cls %in% c("logical"))) {
          return(as.logical(val))
        }
        # default to character
        return(as.character(val))
      }

      coerced <- tryCatch(coerce_value(new_raw, col), error = function(e) new_raw)

      # apply to DataStore and update reactive view
      try({
        store$update_cell(row, col, coerced)
        rv_data(store$data)
        last_edit(list(row = row, col = col, value = coerced))
        edit_count(edit_count() + 1)
      }, silent = TRUE)
    })

    # ---- SAVE BUTTON ----
    observeEvent(input$save_btn, {
      # pending_action("save")

      shiny::showModal(
        confirm_modal(
          session = session,
          id = "save",
          title = "Confirm Save?",
          message = "Do you want to save changes to DuckDB?",
          confirm_id = "confirm",
          confirm_label = "Save",
          danger = TRUE
        )
      )
    })

    # ---- REVERT BUTTON ----
    observeEvent(input$revert_btn, {
      # pending_action("revert")

      shiny::showModal(
        confirm_modal(
          session = session,
          id = "revert",
          title = "Revert changes?",
          message = "This will discard all changes. Continue?",
          confirm_id = "confirm",
          confirm_label = "Revert",
          danger = TRUE
        )
      )
    })
    # observe({
    #   print(names(input))
    # })
    # ---- CONFIRM HANDLER ----
    observeEvent(input$save_confirm, {
      shiny::removeModal()
        tryCatch({
          store$data <- rv_data()
          store$save_to_db()
          shiny::showNotification("✅ Saved successfully!", type = "message")
        }, error = function(e) {
          shiny::showNotification(
            paste("❌ Save failed:", e$message),
            type = "error"
          )
        })
      })

    observeEvent(input$revert_confirm, {
      removeModal()
      edit_count(0)
      last_edit(NULL)
      rv_data(store$original)
      # hotwidget will re-render from rv_data(); show notification
      shiny::showNotification("↩ Changes reverted.", type = "warning")
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
