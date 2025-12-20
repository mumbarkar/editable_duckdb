#' DataStore R6 class
#'
#' @description
#' An R6 class intended to connect to a bundled DuckDB DB (mtcars.duckdb) and
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

    #' @title initialize
    #' @param db_path C character string of data base path
    #' @param table Character string of table name
    #' @param read_only Logical
    #'
    #' @returns
    #' @export
    #'
    #' @examples
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

      # Keep a copy of the original data-required for the Revert Changes button
      self$original <- as.data.frame(df, stringsAsFactors = FALSE)
    },

    #' @title save_to_db
    #' @description
    #' Method to overwrite the data with most recent changes back to database
    #' @returns
    #' @export
    #'
    #' @examples
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

    #' @title udpate_cell
    #' @description
    #' Method to update a single cell
    #'
    #' @param row
    #' @param col
    #' @param value
    #'
    #' @returns
    #' @export
    #'
    #' @examples
    update_cell = function(row, col, value) {
      self$data[row, col] <- value
      invisible(TRUE)
    },

    #' @title save_snapshot
    #' @description
    #' Method to revert back to original data
    #'
    #' @returns
    #' @export
    #'
    #' @examples
    save_snapshot = function() {
      self$original <- self$data
      invisible(TRUE)
    },

    #' @title summary
    #' @description
    #' Method to generate simple summary#'
    #' @returns
    #' @export
    #'
    #' @examples
    summary = function() {
      if (is.null(self$data)) return("No data loaded")
      list(
        rows = nrow(self$data),
        cols = ncol(self$data),
        colnames = colnames(self$data)
      )
    },

    #' @title disconnect
    #' @description
    #' Method for DB disconnect
    #'
    #' @returns
    #' @export
    #'
    #' @examples
    disconnect = function() {
      if (!is.null(self$con)) {
        try(DBI::dbDisconnect(self$con, shutdown = TRUE), silent = TRUE)
        self$con <- NULL
      }
      invisible(TRUE)
    }
  ),
  private = list(
    #' @title finalize
    #' @description
    #' Finalize method
    #'
    #' @returns
    #' @export
    #'
    #' @examples
    finalize = function() {
      self$disconnect()
    }
  )
)

