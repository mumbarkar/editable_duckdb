#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import R6
#' @import duckdb
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # Create new instance store from the parent DataStore R6 class and it will
  # inherit all the methods and attributes from the parent class
  store <- DataStore$new()
}
