#' Parse Stage Race PDFs
#'
#' Parses results from stage race standings PDFs, i.e. the PDFs with cumulative
#' results from multiple stages.
#'
#' @result A data.frame; specifically a \code[dplyr]{\link{tbl_df}}
#'
#' @section Warning: For PDFs that only contain results from 1-2 days of a stage
#' race, I have found that this function will garble the within stage rank
#' columns somewhat. In my limited testing, the results are typically repairable
#' with minimal work.
#'
#' @param file character; file path to PDF
#' @export
#' @examples
#' \dontrun{
#' pdf <- system.file("example_pdfs/stage_example1.pdf",package = "fispdfparsr")
#' stg <- parse_stage_pdf(file = pdf)
#' }
parse_stage_pdf <- function(file){
  stg_tbls <- parse_pdf(file = file,method = "lattice")

  result <- stage_clean(tbls = stg_tbls)
  result
}
