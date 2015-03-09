#' Responsive Toolbar htmlwidget
#'
#' Utility htmlwidget to provide a responsive toolbar to a htmltools::tag,
#'  HTML element, or another htmlwidget.  The underlying JavaScript library is
#'  \href{http://responsive-nav.com/}{responsive-nav.js}, which is
#'  lightweight and dependency-free.
#'
#' @param selector \code{string} CSS selector of html element in which to build the toolbar.
#' @param taglist  \code{\link[htmltools]{tagList}} or \code{\link[htmltools]{HTML}}
#'   of items for the toolbar.  Generally, these
#'   will be \code{\link[htmltools]{tags$ul}} containing \code{\link[htmltools]{tags$li}}.
#' @param options  \code{list} of config options for responsive-nav.js.  See
#'  \href{https://github.com/viljamis/responsive-nav.js#usage-instructions}{Usage Instructions}
#'  for a discussion.  Options and their default values are listed below for reference.
#'  \itemize{
#'    \item animate: true // Boolean: Use CSS3 transitions, true or false
#'    \item transition: 284 // Integer: Speed of the transition, in milliseconds
#'    \item label: "Menu" // String: Label for the navigation toggle
#'    \item insert: "before" // String: Insert the toggle before or after the navigation
#'    \item customToggle: "" // Selector: Specify the ID of a custom toggle
#'    \item closeOnNavClick: false // Boolean: Close the navigation when one of the links are clicked
#'    \item openPos: "relative" // String: Position of the opened nav, relative or static
#'    \item navClass: "nav-collapse" // String: Default CSS class. If changed, you need to edit the CSS too!
#'    \item navActiveClass: "js-nav-active" // String: Class that is added to <html> element when nav is active
#'    \item jsClass: "js" // String: 'JS enabled' class which is added to <html> element
#'    \item init: function(){} // Function: Init callback
#'    \item open: function(){} // Function: Open callback
#'    \item close: function(){} // Function: Close callback
#'  }
#'  @param width \code{integer} width of the container in pixels.  Since this is generally expected
#'          to not be shown, \code{0} is the default.  However, if provided,
#'          \code{width} and \code{height} will be respected.
#'  @param height \code{integer} height of the container in pixels.  Since this is generally expected
#'          to not be shown, \code{0} is the default.  However, if provided,
#'          \code{width} and \code{height} will be respected.
#'
#' @import htmlwidgets
#'
#' @export
navr <- function(
  selector = "body"
  , taglist = tagList()
  , options = NULL  # configuration options
  , width = 0, height = 0
) {

  # check parameters to make sure they are of expected type
  if( !inherits(selector,"character") ) stop("selector should be a string",call.=F)
  if( !inherits(taglist,c("shiny.tag.list","html")) ) stop("taglist should be a tagList or HTML",call.=F)
  if( !(inherits(options,"list") || is.null(options)) ) stop("options should be a list or NULL",call.=F)

  # check taglist for length > 0
  if( length(taglist) == 0 ) warning("expect taglist parameter to be of length > 0", call.=F)

  # forward options using x
  x = list(
    selector = selector
    ,taglist = htmltools::browsable(as.character(taglist),F)
    ,options = options
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
