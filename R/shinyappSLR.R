#' shinySLR
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{shinyhist()}
shinySLR <- function () {
  shiny::runApp(system.file("shinySLR",
                            package = "MATH4753GALEXFORCELAB13"),
                launch.browser = TRUE)
}
