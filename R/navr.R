#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
navr <- function(message, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    message = message
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'navr',
    x,
    width = width,
    height = height,
    package = 'navr'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
navrOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'navr', width, height, package = 'navr')
}

#' Widget render function for use in Shiny
#'
#' @export
renderNavr <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, navrOutput, env, quoted = TRUE)
}
