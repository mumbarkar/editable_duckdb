#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Add external resources (golem helper)
    golem_add_external_resources(),

    # Title panel required by wireframe
    shiny::titlePanel("Data Explorer"),

    # Top header / hero area (wireframe)
    shiny::div(
      class = "app-header",
      shiny::tags$div(
        class = "container-fluid",
        shiny::tags$h1("MTCars Dataset", style = "margin-top:10px; margin-bottom:0;"),
        shiny::tags$p("Interactive data table with real-time editing", style = "margin-top:0; color: #555;")
      ),
      style = "padding: 12px 16px; background: #f8f9fb; border-bottom: 1px solid #e6e6e6;"
    ),

    # Main app UI matching wireframe: sidebar controls + large table area
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::wellPanel(
            shiny::h4("Controls"),
            shiny::actionButton("load_btn", "Load Data", icon = shiny::icon("download")),
            shiny::actionButton("revert_btn", "Revert Changes", icon = shiny::icon("undo")),
            shiny::actionButton("save_btn", "Save Changes", icon = shiny::icon("save")),
            shiny::tags$hr(),
            shiny::h5("Dataset summary"),
            # Wireframe-specific summary outputs
            shiny::textOutput("record_count"),
            shiny::textOutput("column_count"),
            shiny::textOutput("avg_mpg"),
            shiny::textOutput("avg_hp"),
            shiny::textOutput("modified_cells"),
            shiny::tags$hr(),
            shiny::verbatimTextOutput("status_message", placeholder = TRUE)
          ),
          # small footer / instructions
          shiny::tags$p("Edit cells directly in the table. Edits are applied to the DataStore.", style = "font-size:11px; color:#666;")
        ),

        shiny::column(
          width = 9,
          # area for the editable table (module)
          shiny::div(
            class = "table-container",
            mod_table_ui("table")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "atorus.takehome"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
