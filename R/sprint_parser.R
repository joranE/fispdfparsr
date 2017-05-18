#' Parse Sprint Race PDFs
#'
#' Convert FIS sprint result PDFs into a format more
#' suitable for analysis. All times are converted to seconds.
#'
#' The PDF parser in the tabulizer package will sometimes miss blank columns,
#' for example if no one from QF3 ended up on the final heat, the parser will
#' simply skip that blank column. The function attemps to detect this and if it
#' does you will be prompted to supply an index for where to insert a blank
#' column so that the tables all line up correctly. You might need to have the
#' PDF open to look at while running this function so you can verify where to
#' add the column.
#'
#' @result A data.frame; specifically a \code[dplyr]{\link{tbl_df}}
#'
#' @param file character; file path to PDF (use the PDF for the final results
#' not the qualification results)
#' @param other arguments passsed to parse_pdf
#' @export
#' @import stringr
#' @import lubridate
#' @import dplyr
#' @examples
#' \dontrun{
#' spr <- parse_spr_pdf(file = system.file("example_pdfs/spr_example1.pdf",
#'                                          package = "fispdfparsr"))
#' }
parse_spr_pdf <- function(file = NULL,...){
  if (is.null(file)){
    stop("Must provide file path.")
  }

  #Read tables from final PDF
  spr_tbls <- parse_pdf(file = file,method = "matrix",...)

  result <- sprint_clean(tbls = spr_tbls)
  result
}
