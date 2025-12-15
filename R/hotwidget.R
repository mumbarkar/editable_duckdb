#' hotwidget htmlwidget wrapper
#'
#' @param data A data.frame to render in the widget (or `message` for backwards compatibility).
#' @param width,height Widget width/height.
#' @param options List of widget options (colClasses, readOnly, etc).
#' @import htmlwidgets
#' @export
hotwidget <- function(data = NULL, message = NULL, width = NULL, height = NULL, options = list()) {
  # Backwards compatibility: accept `message` as data
  if (is.null(data) && !is.null(message)) data <- message

  if (is.null(data)) data <- data.frame()

  # Ensure data is a data.frame
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Build metadata for JS (column names + classes)
  col_classes <- vapply(data, function(col) paste(class(col), collapse = ","), character(1))
  x <- list(
    data = unname(as.list(data)), # list of columns; JS will reconstruct rows
    nrow = nrow(data),
    ncol = ncol(data),
    colnames = colnames(data),
    colclasses = as.character(col_classes),
    options = options
  )

  htmlwidgets::createWidget(
    name = "hotwidget",
    x = x,
    width = width,
    height = height,
    package = "atorus.takehome"
  )
}

#' Shiny bindings for hotwidget
#'
#' Output and render functions for using hotwidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a hotwidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name hotwidget-shiny
#'
#' @export
hotwidgetOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'hotwidget', width, height, package = 'atorus.takehome')
}

#' @rdname hotwidget-shiny
#' @export
renderHotwidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, hotwidgetOutput, env, quoted = TRUE)
}
