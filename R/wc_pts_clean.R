wc_pts_clean <- function(tbls,...){
  #Remove race schedule tbl
  idx <- sapply(tbls,function(x) {
    any(grepl("^[0-9]{1,2} [[:upper:]]{3} [0-9]{4}$",x))})
  tbls <- tbls[!idx]

  fisid_name_noc <- function(x){
    all_blank <- apply(x,2,function(y) all(y == ""))
    x <- x[,!all_blank]
    id_cols <- which(grepl("FIS|NAME|NOC",x[1,]))
    mn <- min(id_cols) - 1
    mx <- max(id_cols) + 1
    ids <- apply(x[,id_cols,drop = FALSE],1,paste,collapse = "")
    x <- cbind(x[,seq_len(mn),drop = FALSE],
               matrix(ids,ncol = 1),
               x[,seq(mx,ncol(x)),drop = FALSE])
    x
  }
  tbls <- lapply(tbls,fisid_name_noc)

  # Trim top row and process
  tbls <- lapply(tbls,function(x) x[-1,,drop = FALSE])
  tbls <- lapply(tbls,function(x) {
    x <- apply(x,2,stringr::str_trim,side = "both")
    x <- tibble::as_tibble(x,.name_repair = "minimal")
    x})
  tbls <- bind_rows(tbls)
  tbls$grp <- rep(seq_len(nrow(tbls) / 3),each = 3)

  pick_row <- function(x) x[x != ""][1]

  tbls <- tbls %>%
    group_by(grp) %>%
    summarise_all(funs(pick_row(.)))

  #Clean up fisid,name,noc
  tbls <- tbls %>%
    rename(rank = V1,fnn = V2,total_pts = V3) %>%
    mutate(fisid = stringr::str_extract(pattern = "^[0-9]{7}",fnn),
           nation = stringr::str_extract(pattern = "[[:upper:]]{3}$",fnn),
           fnn = gsub(pattern = "^[0-9]{7}",replacement = "",x = fnn),
           fnn = gsub(pattern = "[[:upper:]]{3}$",replacement = "",x = fnn),
           fnn = stringr::str_trim(fnn,side = "both")) %>%
    select(grp,rank,fisid,name = fnn,nation,total_pts,everything()) %>%
    mutate(rank = as.integer(rank),
           total_pts = as.integer(total_pts)) %>%
    mutate_at(vars(starts_with("V")),funs(as.integer)) %>%
    mutate_at(vars(starts_with("V")),funs(if_else(is.na(.),0L,.)))

  col_idx <- grepl(pattern = "^V[0-9]{1,2}",colnames(tbls))
  colnames(tbls)[col_idx] <- paste0("race",seq_len(sum(col_idx)))

  all_zeros <- sapply(tbls,function(col) {is.integer(col) & all(col == 0L)})
  tbls <- tbls[!all_zeros]
  tbls
}
