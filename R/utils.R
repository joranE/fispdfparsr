#' Parse PDF File
#'
#' Wrapper for \code{\link[tabulizer]{extract_tables}}
#'
#' @param file character; file path
#' @param \dots further arguments to \code{\link[tabulizer]{extract_tables}}
#' @import tabulizer
parse_pdf <- function(file,...){
  tabulizer::extract_tables(file = file,...)
}
