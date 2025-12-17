#' DataStore R6 class
#'
#' an R6 class intended to connect to a bundled DuckDB DB (mtcars.duckdb) and
#' expose data, original, update_cell, revert, and summary methods.
#'
#' @importFrom DBI dbConnect dbReadTable dbListTables dbGetQuery dbDisconnect dbQuoteIdentifier
#' @importFrom duckdb duckdb
#' @export
DataStore <- R6::R6Class(
  "DataStore",
  public = list(
    con = NULL,
    data = NULL,
    original = NULL,
    tbl_name = NULL,

    initialize = function(db_path = NULL, table = "mtcars", read_only = TRUE) {
      db_file <- if (is.null(db_path)) {
        system.file("extdata", "mtcars.duckdb", package = "atorus.takehome")
      } else {
        db_path
      }
      self$tbl_name <- table

      df <- NULL

      if (nzchar(db_file)) {
        try({
          self$con <- DBI::dbConnect(
            duckdb::duckdb(),
            dbdir = db_file
          )

          tables <- tryCatch(DBI::dbListTables(self$con), error = function(e) character(0))

          if (table %in% tables) {
            df <- DBI::dbReadTable(self$con, table)
          }
          if (is.null(df) || nrow(df) == 0) {
            df <- datasets::mtcars
          }
        }, silent = TRUE)
      } else {
        df <- datasets::mtcars
      }

      self$data <- as.data.frame(df, stringsAsFactors = FALSE)
      self$original <- as.data.frame(df, stringsAsFactors = FALSE)
    },

    save_to_db = function() {
      if (is.null(self$con)) {
        stop("No DB connection to write to.")
      }
      # Overwrite the DuckDB table with the current data
      DBI::dbWriteTable(self$con, self$tbl_name, self$data, overwrite = TRUE)
      # Refresh the original snapshot
      self$original <- self$data
      invisible(TRUE)
    },

    # Method to update a single cell
    update_cell = function(row, col, value) {
      self$data[row, col] <- value
      invisible(TRUE)
    },

    # Method to revert back to original data
    save_snapshot = function() {
      self$original <- self$data
      invisible(TRUE)
    },

    # Method to generate simple summary
    summary = function() {
      if (is.null(self$data)) return("No data loaded")
      list(
        rows = nrow(self$data),
        cols = ncol(self$data),
        colnames = colnames(self$data)
        )
      },

    disconnect = function() {
      if (!is.null(self$con)) {
        try(DBI::dbDisconnect(self$con, shutdown = TRUE), silent = TRUE)
        self$con <- NULL
      }
      invisible(TRUE)
    },

    finalize = function() {
      self$disconnect()
    }
  )
)

