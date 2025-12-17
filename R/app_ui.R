#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DT
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::titlePanel("Data Explorer"),
    # Top header / hero area (wireframe)
    shiny::div(
      class = "app-header",
      shiny::tags$div(
        class = "container-fluid",
        shiny::tags$h1("MTCars Dataset", style = "margin-top:10px; margin-bottom:0;"),
        shiny::tags$p("Interactive data table with real-time editing", style = "margin-top:0; color: #555;")
      ),
      style = "padding: 12px 16px; background: #f8f9fb;"
    ),
    shiny::mainPanel(
      mod_table_ui("table")
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
