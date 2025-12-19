#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Application UI
    bslib::page_navbar(
      title = "Data Explorer",

      bslib::nav_spacer(),

      theme = bslib::bs_theme(
        version = 5,
        bg = "#ffffff",
        fg = "#333333",
        primary = "#0066cc",
        base_font = font_google("Inter"),
        font_scale = 0.9
      ),

      # Navigation items
      bslib::nav_panel(
        title = "Home",
        mod_table_ui("table_edit")
      ),

      # Analytics tab
      bslib::nav_panel(
        title = "Analytics",
        div(
          class = "container-fluid py-4",
          h3("Analytics Dashboard"),
          p("Analytics features coming soon...", class = "text-muted")
        )
      ),

      # Settings tab
      bslib::nav_panel(
        title = "Settings",
        div(
          class = "container-fluid py-4",
          h3("Settings"),
          p("Application settings coming soon...", class = "text-muted")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    'www',
    app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'atorus.takehome'
    ),
    # Handsontable is provided by the bundled htmlwidget dependency
    # Custom CSS for additional styling
    tags$style(HTML("
      .navbar-brand {
        font-weight: 600;
        font-size: 1.1rem;
      }

      .nav-link {
        font-weight: 500;
      }

      .btn-outline-secondary {
        border: 1px solid #dee2e6;
        color: #495057;
      }

      .btn-outline-secondary:hover {
        background-color: #f8f9fa;
        border-color: #dee2e6;
        color: #495057;
      }

      .btn-outline-danger {
        border: 1px solid #dc3545;
        color: #dc3545;
      }

      .btn-outline-danger:hover {
        background-color: #dc3545;
        border-color: #dc3545;
        color: white;
      }

      .card {
        border: 1px solid #e9ecef;
      }

      .card-header {
        padding: 1rem 1.25rem;
      }

      .card-body {
        padding: 1.25rem;
      }

      .bg-light {
        background-color: #f8f9fa !important;
      }
    "))
  )
}
