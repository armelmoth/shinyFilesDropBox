#' Adds the content of www to sF/
#' 
#' @importFrom shiny addResourcePath
#' 
#' @noRd
#' 
# .onLoad <- function(...) {
#     addResourcePath('sF', system.file('www', package='shinyFilesDropBox'))
# }
addResourcePath('sF', '~/Documents/shinyFilesDropBox/inst/www')
#' Run a simple example app using the shinyFiles functionality
#' 
#' When the function is invoked a shiny app is started showing a very simple 
#' setup using shinyFiles. A button summons the dialog box allowing the user to
#' navigate the R installation directory. To showcase the restrictions parameter
#' the base package location has been hidden, and is thus inaccecible. A panel 
#' besides the button shows how the user selection is made accessible to the 
#' server after parsing with \code{\link{parseFilePaths}}.
#' 
#' @family shinyFilesDropBox
#' 
#' @importFrom shiny runApp
#' 
#' @export
#' 
shinyDropFilesExample <- function() {
    runApp(system.file('example', package='shinyFilesDropBox', mustWork=T), display.mode='showcase')
}

getDropSession <- function() {
    session <- shiny::getDefaultReactiveDomain()
    
    if (is.null(session)) {
        stop(paste(
            "could not find the Shiny session object. This usually happens when a",
            "shinyjs function is called from a context that wasn't set up by a Shiny session."
        ))
    }
    
    session
}
