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
#' @param edit boolean; edit tables before passing them to be cleaned
#' @param \dots other arguments passsed to parse_pdf
#' @export
#' @import stringr
#' @import lubridate
#' @import dplyr
#' @examples
#' \dontrun{
#' spr <- parse_spr_pdf(file = system.file("example_pdfs/spr_example1.pdf",
#'                                          package = "fispdfparsr"))
#' }
parse_spr_pdf <- function(file = NULL,edit = FALSE,...){
  if (is.null(file)){
    stop("Must provide file path.")
  }

  #Read tables from final PDF
  spr_tbls <- parse_pdf(file = file,method = "decide",output = "matrix",...)

  if (edit){
    to_remove <- c()
    for (i in seq_along(spr_tbls)){
      yc_idx <- apply(spr_tbls[[i]],2,FUN = function(x) any(x == "YC") & all(x %in% c("","YC")))
      spr_tbls[[i]] <- spr_tbls[[i]][,!yc_idx]
      print(spr_tbls[[i]])
      cat("\n")
      choice <- readline(prompt = "Keep (1) or discard (2), edit (3) or browser (4)? ")
      if (choice == "1") next
      if (choice == "3") {
        spr_tbls[[i]] <- edit(spr_tbls[[i]])
      }
      if (choice == "2"){
        to_remove <- c(to_remove,i)
      }
      if (choice == "4"){
        browser()
      }
    }
    if (length(to_remove) > 0){
      spr_tbls <- spr_tbls[-to_remove]
    }
    choice <- readline(prompt = "Add additional tables from pages? (0 = skip) ")
    if (choice != "0"){
      pg <- str_split(string = choice,pattern = ",")[[1]] %>% as.integer()
      n <- length(spr_tbls)
      for (i in seq_along(pg)){
        spr_tbls[[n + i]] <- extract_areas(file = file,pages = pg[i])[[1]]
      }
    }
  }

  result <- sprint_clean(tbls = spr_tbls)
  result
}
