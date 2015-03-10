#' Easy Icons for \code{navr}
#'
#' Add \href{http://fontawesome.io/}{Font-Awesome} for styling
#'   our \code{navr} with beautiful, professional icons.  Please note
#'   that you'll already have these icons if using Shiny.
#'
#' @return \code{navr} htmlwidget with Font-Awesome dependencies attached.
#'
#' @import htmltools
#'
#' @export

add_font_awesome <- function( nav ){
  if(!inherits(nav,"htmlwidget")) stop("nav should be a htmlwidget.",call.=F)

  htmltools::attachDependencies(
    nav
    ,htmlDependency(
      name = "font-awesome"
      ,version = "4.3.0"
      ,src = c(file=system.file("htmlwidgets/lib/font-awesome",package="navr"))
      ,stylesheet = "font-awesome.css"
    )
  )
  return(nav)
}

#' Hover Effects for \code{navr}
#'
#' Add \href{http://ianlunn.github.io/Hover/}{Hover Effects} to
#'  our \code{navr}.  There are lots of hover effects.
#'
#' @param navr \code{navr} htmlwidget to which we will apply hover effects
#' @param effect \code{string} hover effect which we would like to apply.  For options,
#'          see \href{http://ianlunn.github.io/Hover/}{Hover Effects}.
#'
#' @return \code{navr} htmlwidget with hover effects applied and dependencies attached.
#'
#' @import htmltools XML
#'
#' @export
add_hover <- function( navr = NULL, effect = "grow" ){
  if(!inherits(nav,"htmlwidget")) stop("nav should be a htmlwidget.",call.=F)

  htmltools::attachDependencies(
    nav
    ,htmlDependency(
      name = "hover"
      ,version = "2.0.2"
      ,src = c(file=system.file("htmlwidgets/lib/hover/css",package="navr"))
      ,stylesheet = "hover.css"
    )
  )

  if(!requireNamespace(XML)){
    warning("Dependencies applied.  However, we need the XML package to apply the effect.")
  }

  xml <- XML::xmlParse(as.character(navr$taglist))

  XML::xpathSApply(
    xml
    ,"//li"
    ,function(li){
      XML::addAttributes(
        li
        ,"class" = sprintf(
          "%s %s"
          ,XML::xmlAttrs(li)[["class"]]
          ,paste0("hvr-",effect)
        )
      )
    }
  )

  nav$x$taglist = htmltools::HTML(XML::saveXML(xml,prefix=""))

  return(nav)
}
