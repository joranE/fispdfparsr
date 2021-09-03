#' Parse Distance Race PDFs
#'
#' Convert FIS distance result PDFs into a format more
#' suitable for analysis. All times are converted to seconds.
#'
#' @result A data.frame; specifically a \code[dplyr]{\link{tbl_df}}
#'
#' @param file file path to PDF of distance results
#' @param race_distance numeric; race distance in km
#' @param long_mass boolean; flag for handling long mass start start races
#' @param edit boolean
#' @export
#' @import tidyr
#' @examples
#' \dontrun{
#' dst <- parse_dst_pdf(file = system.file("example_pdfs/dst_example1.pdf",
#'                                         package = "fispdfparsr"),15)
#' }
parse_dst_pdf <- function(file = NULL,race_distance,long_mass = FALSE,edit = FALSE,opa = FALSE,...){
  if (is.null(file)){
    stop("Must provide file path for race PDF.")
  }
  if (is.null(race_distance)){
    stop("Must provide race distance (in km).")
  }

  #Read tables from final PDF
  tbls <- parse_pdf(file = file,method = "stream",output = "matrix",...)

  if (edit){
    for (i in seq_along(tbls)){
      print(tbls[[i]])
      cat("\n")
      choice <- readline(prompt = "Edit (1), ok (2) or drop (3)? ")
      if (choice == "2") next
      else {
        if (choice == "3"){
          tbls <- tbls[-i]
        }else {
          tbls[[i]] <- edit(tbls[[i]])
        }
      }
    }
  }

  if (opa){
    result <- dst_clean_opa(tbls = tbls,race_distance = race_distance)
  }else {
    #Escape hatch of 30k/50k mass start races
    # that are more similar to stage races in that
    # they have bonus seconds at split
    if (long_mass){
      result <- dst_clean_mass(tbls = tbls,race_distance = race_distance)
    }else{
      result <- dst_clean(tbls = tbls,race_distance = race_distance)
    }
  }
  result
}
