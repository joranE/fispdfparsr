#' Parse World Cup Point PDFs
#'
#' @param file character; file path to PDF
#' @export
#' @examples
#' \dontrun{
#' stg <- parse_wc_pts_pdf(file = system.file("example_pdfs/wc_pts_example1.pdf",package = "fispdfparsr"))
#' }
parse_wc_pts_pdf <- function(file){
  pts_tbls <- parse_pdf(file = file,method = "lattice")

  result <- wc_pts_clean(tbls = pts_tbls)
  result
}
