#' Parse Stage Race PDFs
#'
#' Parses results from stage race standings PDFs, i.e. the PDFs with cumulative
#' results from multiple stages.
#'
#' @param file character; file path to PDF
#' @export
#' @examples
#' \dontrun{
#' pdf <- system.file("example_pdfs/stage_example1.pdf",package = "fispdfparsr")
#' stg <- parse_stage_pdf(file = pdf)
#' }
parse_stage_pdf <- function(file){
  stg_tbls <- extract_tables(file = file)

  #Remove race schedule tbl
  idx <- sapply(stg_tbls,function(x) {
    any(grepl("^[0-9]{1,2} [[:upper:]]{3} [0-9]{4}$",x))})
  stg_tbls <- stg_tbls[!idx]

  # Trim top three rows and process
  stg_tbls <- lapply(stg_tbls,function(x) x[-seq_len(3),,drop = FALSE])
  stg_tbls <- lapply(stg_tbls,function(x) {
    x <- apply(x,2,stringr::str_trim,side = "both")
    x <- as_data_frame(x)
    x})
  stg_tbls <- bind_rows(stg_tbls)
  stg_tbls$grp <- rep(seq_len(nrow(stg_tbls) / 3),each = 3)

  #Bonus seconds columns
  bonus_idx <- sapply(stg_tbls,function(x) any(grepl("^\\[[0-9]{1,2}\\]",x)))
  skip_idx <- sapply(stg_tbls,function(x) {
    all(grepl("^\\[[0-9]{1,2}\\]",x) | x == "")})
  insert_idx <- which(bonus_idx & !skip_idx)
  if (length(insert_idx) > 0){
    insert_at <- insert_idx + seq(0,length(insert_idx)-1,by = 1)
    for (i in seq_along(insert_idx)){
      bonus_col <- stringr::str_extract(string = stg_tbls[[insert_at[i]]],
                                        pattern = "^\\[[0-9]{1,2}\\]")
      stg_tbls <- tibble::add_column(stg_tbls,
                                     bonus_col,
                                     .before = insert_at[i])
      colnames(stg_tbls)[insert_at[i]] <- paste0("bonus_col",insert_at[i])
    }
    colnames(stg_tbls) <- c(paste0("V",seq_len(ncol(stg_tbls)-1)),"grp")
  }

  #Stage rank columns
  stg_rnk_idx <- sapply(stg_tbls,function(x) any(grepl("=*[0-9]{1,3}\\.$",x)))
  skip_idx <- sapply(stg_tbls,function(x) {
    all(grepl("^=*[0-9]{1,3}\\.$",x) | x == "")})
  insert_idx <- which(stg_rnk_idx & !skip_idx)
  if (length(insert_idx) > 0){
    insert_at <- insert_idx + seq(0,length(insert_idx)-1,by = 1)
    for (i in seq_along(insert_idx)){
      stg_rnk_col <- stringr::str_extract(string = stg_tbls[[insert_at[i]]],
                                        pattern = "=*[0-9]{1,3}\\.$")
      stg_tbls <- tibble::add_column(stg_tbls,
                                     stg_rnk_col,
                                     .after = insert_at[i])
      colnames(stg_tbls)[insert_at[i]+1] <- paste0("stg_rnk_col",insert_at[i])
    }
    colnames(stg_tbls) <- c(paste0("V",seq_len(ncol(stg_tbls)-1)),"grp")
  }

  pick_row <- function(x) x[!is.na(x) & x != ""][1]

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

  j <- c("bonus" = 0,"time" = 0,"rank" = 0)
  for (i in 7:ncol(stg_tbls)){
    #Bonus check
    if (any(grepl(pattern = "[",x = stg_tbls[[i]],fixed = TRUE))){
      j["bonus"] <- j["bonus"] + 1
      stg_num <- max(j)
      colnames(stg_tbls)[i] <- paste0("stage",stg_num,"_bonus")
      stg_tbls[[i]] <- as.integer(gsub(pattern = "\\[|\\]",
                                       replacement = "",
                                       x = stg_tbls[[i]]))
      next
    }
    #Time check
    if (any(grepl(pattern = ":",x = stg_tbls[[i]]))){
      j["time"] <- j["time"] + 1
      stg_num <- max(j)
      colnames(stg_tbls)[i] <- paste0("stage",stg_num,"_time")
      stg_tbls[[i]] <- convert_to_secs(stg_tbls[[i]])
      next
    }
    #Rank check
    if (all(grepl(pattern = "^=*[0-9]{1,3}\\.$",x = stg_tbls[[i]]))){
      j["rank"] <- j["rank"] + 1
      stg_num <- max(j)
      colnames(stg_tbls)[i] <- paste0("stage",stg_num,"_rank")
      stg_tbls[[i]] <- as.integer(gsub(pattern = "=|\\.",
                                       replacement = "",
                                       x = stg_tbls[[i]]))
      next
    }
  }

  stg_tbls$grp <- NULL
  stg_tbls <- stg_tbls %>%
    mutate_at(vars(ends_with("bonus")),funs(if_else(is.na(.),0L,.)))
  stg_tbls
}
