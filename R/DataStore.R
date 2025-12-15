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
    con = NULL,      # duckdb connection object (DBI connection)
    data = NULL,     # working data frame currently in use
    original = NULL, # original baseline snapshot of the data

    initialize = function(db_path = NULL, table = "mtcars", read_only = TRUE) {

      # Locate database file
      db_file <- if (is.null(db_path)) {
        system.file("extdata", "mtcars.duckdb", package = "atorus.takehome")
      } else {
        db_path
      }

      # Initialize the df
      df <- NULL

      # Check the db_file is not empty character
      if (nzchar(db_file)) {
        # Attempt to connect to duckdb and read the table; fall back to datasets::mtcars
        tryCatch({
          # Setting up DB connection
          self$con <- DBI::dbConnect(
            duckdb::duckdb(),
            dbdir = db_file
            )

          # Listing of tables from the existing database
          tables <- tryCatch(
            DBI::dbListTables(self$con),
            error = function(e) character(0)
            )

          # Checking the requested table is part of the DB collections or not
          if (table %in% tables) {
            df <- DBI::dbReadTable(
              conn = self$con,
              name = table
            )
          } else {
            df <- tryCatch(
              DBI::dbGetQuery(self$con,
                              statement = paste0("SELECT * FROM ", table)),
              error = function(e) NULL
            )
          }

          # Additional checks for safer side
          if (is.null(df) || nrow(df) == 0) {
            warning("Table '", table, "' not found or empty in duckdb file; falling back to datasets::mtcars")
            df <- datasets::mtcars
          }
        }, error = function(e) {
          warning("Failed connecting to duckdb: ", conditionMessage(e), "; falling back to datasets::mtcars")
          df <- datasets::mtcars
          if (!is.null(self$con)) {
            try(DBI::dbDisconnect(self$con, shutdown = TRUE), silent = TRUE)
            self$con <- NULL
          }
        })
      } else {
        warning("Bundled duckdb file not found -> using datasets::mtcars")
        df <- datasets::mtcars
      }

      # Store the result
      self$data <- df
      self$original <- df

      invisible(self)
    },

    # Method to update a single cell
    update_cell = function(row, col, value) {

      if (is.null(self$data)) return(list(success = FALSE, message = "No data loaded..."))

      # resolve column
      if (is.character(col)) {
        if (!(col %in% colnames(self$data))) return(list(success = FALSE, message = "Column not found..."))
        col_idx <- match(col, colnames(self$data))
      } else {
        col_idx <- as.integer(col)
        if (is.na(col_idx) || col_idx < 1 || col_idx > ncol(self$data)) return(list(success = FALSE, message = "Invalid column index..."))
      }

      row_idx <- as.integer(row)
      if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(self$data)) return(list(success = FALSE, message = "Invalid row index..."))

      # coerce to original type when possible
      target_col <- self$original[[col_idx]]
      new_value <- tryCatch({
        if (is.numeric(target_col)) {
          as.numeric(value)
        } else if (is.integer(target_col)) {
          as.integer(value)
        } else if (is.logical(target_col)) {
          as.logical(value)
        } else if (is.factor(target_col)) {
          # preserve factor levels where possible
          factor(value, levels = levels(target_col))
        } else {
          as.character(value)
        }
      }, warning = function(w) value, error = function(e) value)

      # assign in-memory
      self$data[row_idx, col_idx] <- new_value

      invisible(list(success = TRUE))
    },

    # Method to revert back to original data
    revert = function() {
      # PURPOSE:
      # --------
      # Reset the working data (self$data) back to the original snapshot.
      #
      # EXPECTED BEHAVIOR:
      # - self$data should become identical to self$original
      # - Does not affect the database or the connection

      if (!is.null(self$original)) {
        self$data <- self$original
        invisible(TRUE)
      } else {
        invisible(FALSE)
      }
    },

    # Method to generate simple summary
    summary = function() {
      # PURPOSE:
      # --------
      # Return a simple human-readable summary of the current dataset.
      #
      # EXPECTED BEHAVIOR:
      # - If no data has been loaded yet, return a message like:
      #       "No data loaded"
      # - Otherwise return something like:
      #       "Rows: X | Columns: Y"
      #
      # This is meant for display in the Shiny UI.

      if (is.null(self$data)) return("No data loaded")
      list(
        rows = nrow(self$data),
        cols = ncol(self$data),
        colnames = colnames(self$data),
        classes = vapply(self$data, function(x) paste(class(x), collapse = ","), character(1))
      )
    },

    # Method to disconnect the database if not being used
    disconnect = function() {
      if (!is.null(self$con)) {
        try(DBI::dbDisconnect(self$con, shutdown = TRUE), silent = TRUE)
        self$con <- NULL
      }
      invisible(TRUE)
    },

    # Method to cleanup callback
    finalize = function() {
      self$disconnect()
    }
  )
)
