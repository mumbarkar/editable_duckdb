#' @title mod_table_ui
#' @description Module UI for Home / Editable table
#' @param id module id
#' @export
mod_table_ui <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    div(
      class = "container-fluid py-4",
      style = "max-width: 1400px; margin: 0 auto; background-color: #f5f5f7;",

      # Module header----
      div(
        class = "mb-4",
        h2("MTCars Dataset", class = "mb-2"),
        p(
          "Interactive data table with real-time editing",
          class = "text-muted",
          style = "font-size: 1.1rem;"
        )
      ),

      # ACtion buttons ----
      div(
        class = "d-flex justify-content-between mb-4",
        actionButton(
          ns("save_btn"),
          "Save Changes",
          icon = icon("save"),
          class = "btn-outline-secondary",
          style = "border-radius: 8px; padding: 8px 20px; background-color: white;"
        ),
        actionButton(
          ns("revert_btn"),
          "Revert Changes",
          icon = icon("rotate-left"),
          class = "btn-outline-danger",
          style = "border-radius: 8px; padding: 8px 20px; background-color: white;"
        )
      ),

      # Layout ----
      bslib::layout_columns(
        col_widths = c(8, 4),

        # ===== LEFT: TABLE =====
        bslib::card(
          class = "shadow-sm",
          style = "border-radius: 12px;",
          bslib::card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              h5("Data Table", class = "mb-0 fw-bold"),
              downloadButton(
                ns("download_btn"),
                label = "Download Data",
                class = "btn-sm btn-outline-primary",
                style = "padding: 6px 12px; border-radius: 6px;"
              )
            )
          ),
          bslib::card_body(
            style = "padding: 12px;",
            # Rows per page selector
            div(
              class = "mb-2 d-flex gap-2 align-items-center",
              style = "font-size: 0.9rem;",
              selectInput(
                ns("rows_per_page"),
                label = NULL,
                choices = c(10, 25, 50, 100),
                selected = 10,
                width = "70px"
              ),
              span("rows per page")
            ),
            # Table container
            div(
              class = "table-container mb-2",
              style = "
                height: 550px;
                overflow-y: auto;
                overflow-x: auto;
                border: 1px solid #e6e6e6;
                border-radius: 12px;
                padding: 12px;",
              hotwidgetOutput(ns("table"), width = '100%', height = '100%')
            ),
            # Page navigation at bottom right
            div(
              class = "d-flex justify-content-end gap-2 align-items-center",
              style = "font-size: 0.9rem;",
              actionButton(
                ns("prev_page"),
                label = "",
                icon = icon("chevron-left"),
                class = "btn-sm",
                style = "padding: 4px 8px;"
              ),
              span(
                textOutput(ns("page_info"), inline = TRUE),
                style = "min-width: 70px; text-align: center;"
              ),
              actionButton(
                ns("next_page"),
                label = "",
                icon = icon("chevron-right"),
                class = "btn-sm",
                style = "padding: 4px 8px;"
              )
            )
          )
        ),

        # Summary UI ----
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



#' @title mod_table_server
#' @description Module server for editable table
#' @param id module id
#' @param store R6 DataStore instance
#' @export
mod_table_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {

    # Define reactive values ----
    rv_data <- reactiveVal(store$data)
    last_edit <- reactiveVal(NULL)
    edit_count <- reactiveVal(0)
    pending_action <- shiny::reactiveVal(NULL)
    reset_edit_state <- function() {
      edit_count(0)
      last_edit(NULL)
    }

    # Pagination state ----
    current_page <- reactiveVal(1)
    rows_per_page <- reactive(as.integer(input$rows_per_page))

    # Calculate pagination parameters ----
    total_rows <- reactive(nrow(rv_data()))
    total_pages <- reactive(ceiling(total_rows() / rows_per_page()))
    start_row <- reactive((current_page() - 1) * rows_per_page())
    end_row <- reactive(min(start_row() + rows_per_page(), total_rows()))

    # Get paginated data for display ----
    paginated_data <- reactive({
      shiny::validate(need(nrow(rv_data()) > 0, 'No data available.'))
      df <- rv_data()
      start <- start_row() + 1  # 1-indexed
      end <- end_row()
      if (start <= nrow(df)) {
        df[start:end, ]
      } else {
        df[numeric(0), ]
      }
    })

    output$table <- renderHotwidget({
      # Inline validation
      shiny::validate(need(nrow(paginated_data()) > 0, 'No data available.'))
      hotwidget(
        paginated_data(),
        options = list(
          start_row = start_row(),
          rows_to_show = rows_per_page()
        )
      )
    })

    # Pagination display ----
    output$page_info <- renderText({
      req(total_rows() != 0)
      if (total_rows() == 0) {
        "No data"
      } else {
        paste0("Page ", current_page(), " of ", total_pages())
      }
    })

    # Previous page button ----
    observeEvent(input$prev_page, {
      if (current_page() > 1) {
        current_page(current_page() - 1)
      }
    })

    # Next page button ----
    observeEvent(input$next_page, {
      if (current_page() < total_pages()) {
        current_page(current_page() + 1)
      }
    })

    # Reset to page 1 when data changes ----
    observe({
      rv_data()  # dependency
      current_page(1)
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

    # Save button ----
    observeEvent(input$save_btn, {

      shiny::showModal(
        confirm_modal(
          session = session,
          id = "save",
          title = "Confirm Save?",
          message = "Do you want to save changes to DuckDB? \nOnce you save changes to DuckDB, you can't revert it.",
          confirm_id = "confirm",
          confirm_label = "Save",
          danger = FALSE
        )
      )
    })

    # Revert Button ----
    observeEvent(input$revert_btn, {

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

    # Confirm Handler ----
    observeEvent(input$save_confirm, {
      shiny::removeModal()
        tryCatch({
          store$data <- rv_data()
          store$save_to_db()
          shiny::showNotification("✅ Saved successfully!", type = "message")
          reset_edit_state()
        }, error = function(e) {
          shiny::showNotification(
            paste("❌ Save failed:", e$message),
            type = "error"
          )
        })
      })

    observeEvent(input$revert_confirm, {
      removeModal()
      rv_data(store$original)
      reset_edit_state()
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

    # Download button ----
    output$download_btn <- downloadHandler(
      filename = function() {
        paste0("mtcars_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(rv_data(), file, row.names = FALSE)
      }
    )

    return(list(data = rv_data, last_edit = last_edit, edit_count = edit_count))
  })
}
