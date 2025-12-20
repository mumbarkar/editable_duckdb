#' @title confirm_modal
#' @description
#' Create a reusable confirmation modal dialog
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
        class = if (danger) "btn-danger" else "btn-success"
      )
    ),
    easyClose = FALSE
  )
}


#' @title summary_block
#' @description
#' Summary value UI block
#'
#' @param ns Character, namespace
#' @param title Character, title of summary block
#' @param value_ui UI output code block
#'
#' @returns
#' A \code{shiny.tag} object containing the summary block UI.
#' @export
#'
#' @examples
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
