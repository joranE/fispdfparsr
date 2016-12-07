#' Parse Stage Race PDFs
#'
#' Parses results from stage race standings PDFs, i.e. the PDFs with cumulative
#' results from multiple stages.
#'
#' @param file
#' @export
#' @examples
#' \dontrun{
#' stg <- parse_stage_pdf(file = system.file("example_pdfs/stage_example1.pdf",
#'                                            package = "fispdfparsr"))
#' }
parse_stage_pdf <- function(file){
  stg_tbls <- extract_tables(file = file)

  # Trim top three rows and process
  stg_tbls <- lapply(stg_tbls,function(x) x[-seq_len(3),,drop = FALSE])
  stg_tbls <- lapply(stg_tbls,function(x) {
    x <- apply(x,2,stringr::str_trim,side = "both")
    x <- as_data_frame(x)
    x})
  stg_tbls <- bind_rows(stg_tbls)
  stg_tbls$grp <- rep(seq_len(nrow(stg_tbls) / 3),each = 3)

  pick_row <- function(x) x[x != ""][1]

  stg_tbls <- stg_tbls %>%
    group_by(grp) %>%
    summarise_all(funs(pick_row(.))) %>%
    rename(overall_rank = V1,fisid = V2,name = V3,
           nation = V4,total_time = V5) %>%
    ungroup() %>%
    mutate(total_time = gsub(pattern = "[^0-9:.]",
                             replacement = "",
                             x = total_time),
           total_time = cumsum(convert_to_secs(total_time)))

  stg_tbls <- stg_tbls[!sapply(stg_tbls,function(x) all(is.na(x)))]

  stage_counter <- 1
  for (i in 7:ncol(stg_tbls)){
    #Bonus check
    if (any(grepl(pattern = "[",x = stg_tbls[[i]],fixed = TRUE))){
      colnames(stg_tbls)[i] <- paste0("stage",stage_counter,"_bonus")
      stg_tbls[[i]] <- as.integer(gsub(pattern = "\\[|\\]",
                                       replacement = "",
                                       x = stg_tbls[[i]]))
      next
    }
    #Time check
    if (any(grepl(pattern = ":",x = stg_tbls[[i]]))){
      colnames(stg_tbls)[i] <- paste0("stage",stage_counter,"_time")
      stg_tbls[[i]] <- convert_to_secs(stg_tbls[[i]])
      next
    }
    #Rank check
    if (all(grepl(pattern = "^=*[0-9]{1,3}\\.$",x = stg_tbls[[i]]))){
      colnames(stg_tbls)[i] <- paste0("stage",stage_counter,"_rank")
      stg_tbls[[i]] <- as.integer(gsub(pattern = "=|\\.",
                                       replacement = "",
                                       x = stg_tbls[[i]]))
      stage_counter <- stage_counter + 1
      next
    }
  }

  stg_tbls$grp <- NULL
  stg_tbls <- stg_tbls %>%
    mutate_at(vars(ends_with("bonus")),funs(if_else(is.na(.),0L,.)))
  stg_tbls
}
