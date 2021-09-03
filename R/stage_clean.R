stage_clean <- function(tbls,...){
  #Remove race schedule tbl
  idx <- sapply(tbls,function(x) {
    any(grepl("^[0-9]{1,2} [[:upper:]]{3} [0-9]{4}$",x))})
  tbls <- tbls[!idx]

  # Trim top three rows and process
  tbls <- lapply(tbls,function(x) x[-seq_len(3),,drop = FALSE])
  tbls <- lapply(tbls,function(x) {
    x <- apply(x,2,stringr::str_trim,side = "both")
    x <- tibble:as_tibble(x,.name_repair = "minimal")
    x})
  tbls <- bind_rows(tbls)
  tbls$grp <- rep(seq_len(nrow(tbls) / 3),each = 3)

  #Bonus seconds columns
  bonus_idx <- sapply(tbls,function(x) any(grepl("^\\[[0-9]{1,2}\\]",x)))
  skip_idx <- sapply(tbls,function(x) {
    all(grepl("^\\[[0-9]{1,2}\\]",x) | x == "")})
  insert_idx <- which(bonus_idx & !skip_idx)
  if (length(insert_idx) > 0){
    insert_at <- insert_idx + seq(0,length(insert_idx)-1,by = 1)
    for (i in seq_along(insert_idx)){
      bonus_col <- stringr::str_extract(string = tbls[[insert_at[i]]],
                                        pattern = "^\\[[0-9]{1,2}\\]")
      tbls <- tibble::add_column(tbls,
                                 bonus_col,
                                 .before = insert_at[i])
      colnames(tbls)[insert_at[i]] <- paste0("bonus_col",insert_at[i])
    }
    colnames(tbls) <- c(paste0("V",seq_len(ncol(tbls)-1)),"grp")
  }

  #Stage rank columns
  stg_rnk_idx <- sapply(tbls,function(x) any(grepl("=*[0-9]{1,3}\\.$",x)))
  skip_idx <- sapply(tbls,function(x) {
    all(grepl("^=*[0-9]{1,3}\\.$",x) | x == "")})
  insert_idx <- which(stg_rnk_idx & !skip_idx)
  if (length(insert_idx) > 0){
    insert_at <- insert_idx + seq(0,length(insert_idx)-1,by = 1)
    for (i in seq_along(insert_idx)){
      stg_rnk_col <- stringr::str_extract(string = tbls[[insert_at[i]]],
                                          pattern = "=*[0-9]{1,3}\\.$")
      tbls <- tibble::add_column(tbls,
                                 stg_rnk_col,
                                 .after = insert_at[i])
      colnames(tbls)[insert_at[i]+1] <- paste0("stg_rnk_col",insert_at[i])
    }
    colnames(tbls) <- c(paste0("V",seq_len(ncol(tbls)-1)),"grp")
  }

  pick_row <- function(x) x[!is.na(x) & x != ""][1]

  tbls <- tbls %>%
    group_by(grp) %>%
    summarise_all(funs(pick_row(.))) %>%
    rename(overall_rank = V1,fisid = V2,name = V3,
           nation = V4,total_time = V5) %>%
    ungroup() %>%
    mutate(total_time = gsub(pattern = "[^0-9:.]",
                             replacement = "",
                             x = total_time),
           total_time = cumsum(convert_to_secs(total_time)))

  tbls <- tbls[!sapply(tbls,function(x) all(is.na(x)))]

  j <- c("bonus" = 0,"time" = 0,"rank" = 0)
  for (i in 7:ncol(tbls)){
    #Bonus check
    if (any(grepl(pattern = "[",x = tbls[[i]],fixed = TRUE))){
      j["bonus"] <- j["bonus"] + 1
      stg_num <- max(j)
      colnames(tbls)[i] <- paste0("stage",stg_num,"_bonus")
      tbls[[i]] <- as.integer(gsub(pattern = "\\[|\\]",
                                   replacement = "",
                                   x = tbls[[i]]))
      next
    }
    #Time check
    if (any(grepl(pattern = ":",x = tbls[[i]]))){
      j["time"] <- j["time"] + 1
      stg_num <- max(j)
      colnames(tbls)[i] <- paste0("stage",stg_num,"_time")
      tbls[[i]] <- convert_to_secs(tbls[[i]])
      next
    }
    #Rank check
    if (all(grepl(pattern = "^=*[0-9]{1,3}\\.$",x = tbls[[i]]))){
      j["rank"] <- j["rank"] + 1
      stg_num <- max(j)
      colnames(tbls)[i] <- paste0("stage",stg_num,"_rank")
      tbls[[i]] <- as.integer(gsub(pattern = "=|\\.",
                                   replacement = "",
                                   x = tbls[[i]]))
      next
    }
  }

  tbls$grp <- NULL
  tbls <- tbls %>%
    mutate_at(vars(ends_with("bonus")),funs(if_else(is.na(.),0L,.)))
  tbls
}
