tbl_clean <- function(x,cn){
  x <- x[-(1:2),]
  colnames(x) <- cn
  blank_cn <- sum(cn == "")
  colnames(x)[cn == ""] <- paste0("blank",seq_len(blank_cn))
  idx <- which(x[,1] != "")
  x <- x[sort(c(idx,idx-1)),]
  x <- tibble::as_tibble(x,.name_repair = fix_col_names)
  x[] <- lapply(x,stringr::str_trim,side = "both")

  if ("fis name" %in% colnames(x)){
    x <- x %>%
      tibble::add_column(name = x[["fis name"]],.after = "fis name") %>%
      rename(`fis code` = `fis name`)
  }
  if ("fis name nsa code" %in% colnames(x)){
    nsa <- stringr::str_extract(x[["fis name nsa code"]],"[A-Z]{3}$")
    x <- x %>%
      tibble::add_column(`nsa code` = nsa,.after = "fis name nsa code") %>%
      tibble::add_column(name = x[["fis name nsa code"]],.after = "fis name nsa code") %>%
      rename(`fis code` = `fis name nsa code`)
  }

  x[[4]][2:nrow(x)] <- x[[4]][1:(nrow(x) - 1)]
  x[[5]][2:nrow(x)] <- x[[5]][1:(nrow(x) - 1)]

  x[x[,1] != "",]
}

tbl_clean_mass <- function(x,cn){
  x <- x[-(1:5),]
  colnames(x) <- cn
  x <- tibble::as_tibble(x,.name_repair = "minimal")
  x$grp <- rep(seq_len(nrow(x)/3),each = 3)
  if ("fis name" %in% colnames(x)){
    x <- x %>%
      tibble::add_column(name = x[["fis name"]],.after = `fis name`) %>%
      rename(`fis code` = `fis name`)
  }
  x <- x %>%
    group_by(grp) %>%
    mutate_at(.vars = vars(-matches("^[0-9]")),
              .funs = list(~.[. != ""][1])) %>% #funs deprecated?
    mutate(val = c('split_rank','split_time','split_bonus'))
}

#' @export
dst_clean <- function(tbls,race_distance,...){
  #Ditch weather & legend tables
  weather_legend <- sapply(tbls,function(x) {
    any(grepl("revised|weather|legend|chief|delegate|did not finish|did not start",tolower(x[,1])))
  })
  tbls <- tbls[!weather_legend]

  tbl_head <- tbls[[1]][1:2,]
  cn <- apply(tbl_head,2,function(x) x[x != ""][1])
  cn <- gsub(pattern = "1ST|2ND|BONUS",replacement = "",x = cn)
  cn <- stringr::str_trim(tolower(cn),side = "both")
  cn[is.na(cn)] <- ""

  if (cn[3] == "fis"){
    cn[3] <- "fis code"
  }

  nc <- sapply(tbls,ncol)
  idx <- which(nc < length(cn))
  if (length(idx) > 0){
    n <- length(idx)
    tbls <- tbls[-idx]
    message("Removed ",n," tables with too few columns.")
  }

  tbls <- lapply(tbls,tbl_clean,cn = cn)

  tbls <- bind_rows(tbls) %>%
    select(-starts_with("blank")) %>%
    select(-contains("behind"))
  if (!any(grepl("^fis$",cn))){
    tbls$fis <- NA
  }

  if ("fis name" %in% colnames(tbls)){
    tbls <- tbls %>%
      mutate(name = NA_character_) %>%
      rename(`fis code` = `fis name`)
  }

  tbls <- tbls %>%
    ungroup() %>%
    rename_at(.vars = vars(contains("nsa")),.funs = function(x) "nation") %>%
    rename(fisid = `fis code`,fispoints = fis,
           finish_time = finish) %>%
    select(rank,bib,fisid,name,nation,fispoints,everything()) %>%
    gather(key = split,
           value = split_time,
           -rank,-bib,-fisid,-name,-nation,-fispoints) %>%
    mutate(split_time = gsub(pattern ="[[:space:]](.*)",
                             replacement = "",
                             x = split_time),
           split_time = gsub(pattern = "=.*$",
                             replacement = "",
                             x = split_time))
  tbls$split_time <- convert_to_secs(times = tbls$split_time)

  tbls <- tbls %>%
    filter(!grepl("points|climb",split) & !grepl("^V",split)) %>%
    mutate(split = if_else(grepl("finish",split),
                           paste0(race_distance,"km"),
                           split),
           split = gsub(pattern = "[[:alpha:]]",
                        replacement = "",
                        x = split),
           split = as.numeric(split),
           fispoints = as.numeric(fispoints)) %>%
    group_by(split) %>%
    mutate(split_rank = min_rank(split_time),
           split_time_back = split_time - min(split_time,na.rm = TRUE)) %>%
    ungroup() %>%
    rename(split_km = split,
           finish_rank = rank)

  #Ensure finish_rank is integer
  tbls <- tbls %>%
    mutate(finish_rank = as.integer(stringr::str_extract(finish_rank,"[0-9]+")))

  split_tbl <- table(tbls$split_km,useNA = "ifany")
  message("Split point count:")
  print(split_tbl)
  n <- nrow(filter(tbls,is.na(split_time)))
  if (n > 0){
    message(n," rows have missing split times.")
  }
  tbls
}

#' @export
dst_clean_mass <- function(tbls,race_distance){
  #Ditch weather & legend tables
  weather_legend <- sapply(tbls,function(x) {
    any(grepl("weather|legend|chief|delegate|did not finish|did not start",tolower(x[,1])))
  })
  tbls <- tbls[!weather_legend]

  tbl_head <- tbls[[1]][1:3,]
  cn <- apply(tbl_head,2,function(x) x[x != "" & !grepl("BONUS",x)][1])
  cn <- stringr::str_trim(tolower(cn),side = "both")
  cn[is.na(cn)] <- paste0("blank",seq_len(sum(is.na(cn))))

  tbls_ncol <- sapply(tbls,ncol)
  tbls <- tbls[tbls_ncol == length(cn)]

  tbls <- lapply(tbls,tbl_clean_mass,cn = cn)

  tbls <- bind_rows(tbls) %>%
    select(-starts_with("blank")) %>%
    select(-contains("behind"))
  if (!any(grepl("^fis$",cn))){
    tbls$fis <- NA
  }

  if ("fis name" %in% colnames(tbls)){
    tbls <- tbls %>%
      mutate(name = NA_character_) %>%
      rename(`fis code` = `fis name`)
  }

  tbls <- tbls %>%
    ungroup() %>%
    rename_at(.vars = vars(contains("nsa")),.funs = function(x) "nation") %>%
    rename(fisid = `fis code`,fispoints = fis,
           finish_time = finish) %>%
    select(rank,bib,fisid,name,nation,fispoints,everything())
  tbl_rank <- tbls %>%
    filter(val == 'split_rank') %>%
    gather(key = split_km,value = split_rank,matches("^[0-9]")) %>%
    ungroup() %>%
    select(-grp,-val,-finish_time)
  tbl_time <- tbls %>%
    filter(val == 'split_time') %>%
    gather(key = split_km,value = split_time,matches("^[0-9]")) %>%
    ungroup() %>%
    select(-grp,-val)
  tbl_time$split_time <- convert_to_secs(times = tbl_time$split_time)
  tbl_bonus <- tbls %>%
    filter(val == 'split_bonus') %>%
    gather(key = split_km,value = split_bonus,matches("^[0-9]")) %>%
    ungroup() %>%
    select(-grp,-val,-finish_time) %>%
    mutate(split_bonus = if_else(split_bonus == "",NA_character_,split_bonus),
           split_bonus = as.integer(split_bonus))

  #Add final split
  tbl_rank_final <- tbl_rank %>%
    select(rank,bib,fisid,name,nation,fispoints) %>%
    unique() %>%
    mutate(split_km = paste0(race_distance,"km"),
           split_rank = rank)
  tbl_rank <- bind_rows(tbl_rank,tbl_rank_final)

  tbl_time_final <- tbl_time %>%
    select(rank,bib,fisid,name,nation,fispoints,finish_time) %>%
    unique() %>%
    mutate(split_km = paste0(race_distance,"km")) %>%
    rename(split_time = finish_time) %>%
    mutate(split_time = convert_to_secs(times = split_time))
  tbl_time$finish_time <- NULL
  tbl_time <- bind_rows(tbl_time,tbl_time_final)

  tbl_out <- inner_join(tbl_rank,
                        tbl_time,
                        by = c('rank','bib','fisid','name','nation',
                               'fispoints','split_km')) %>%
    left_join(tbl_bonus,
              by = c('rank','bib','fisid','name','nation',
                     'fispoints','split_km')) %>%
    rename(finish_rank = rank,
           split_rank_bonus = split_rank) %>%
    mutate(split_km = gsub(pattern = "km$",replacement = "",x = split_km),
           split_km = as.numeric(split_km),
           fispoints = as.numeric(fispoints),
           split_bonus = if_else(is.na(split_bonus),0L,split_bonus))

  tbl_out <- tbl_out %>%
    group_by(split_km) %>%
    mutate(split_rank = min_rank(split_time),
           split_time_back = split_time - min(split_time,na.rm = TRUE),
           split_time_bonus = split_time - split_bonus,
           split_time_back_bonus = split_time_bonus - min(split_time_bonus,na.rm = TRUE)) %>%
    ungroup()

  #Ensure finish_rank is integer
  tbl_out <- tbl_out %>%
    mutate(finish_rank = as.integer(stringr::str_extract(finish_rank,"[0-9]+")))

  split_tbl <- table(tbl_out$split_km,useNA = "ifany")
  message("Split point count:")
  print(split_tbl)
  n <- nrow(filter(tbl_out,is.na(split_time)))
  if (n > 0){
    message(n," rows have missing split times.")
  }

  tbl_out
}

#' Alternate version for the PDF format typically seen in OPA Cups
#' @importFrom purrr keep
#' @export
dst_clean_opa <- function(tbls,race_distance){
  weather_legend <- sapply(tbls,function(x) {
    any(grepl("revised|weather|legend|chief|delegate|did not finish|did not start",tolower(x[,1])))
  })
  tbls <- tbls[!weather_legend]

  #Find where 'RESULTLIST' in header erroneously combined two columns
  for (i in seq_along(tbls)){
    top <- tbls[[i]][1,]
    if (any(grepl("RESULTLIST",top))){
      idx <- grepl("RESULTLIST",top)
      idx_int <- which(idx)
      to_split <- tbls[[i]][,idx][-1]
      to_fill <- do.call("rbind",str_split(to_split," "))
      tbls[[i]] <- tbls[[i]][-1,]
      tbls[[i]] <- cbind(tbls[[i]][,1:(idx_int-1)],to_fill,tbls[[i]][,(idx_int+1):ncol(tbls[[i]])])
    } else {
      tbls[[i]] <- tbls[[i]][-1,]
    }
  }

  if (tbls[[1]][1,1] == "# Bib FisCode"){
    tbls[[1]] <- tbls[[1]] %>%
      as.data.frame() %>%
      separate(col = 1,into = c("#","Bib","FisCode"),sep = " ") %>%
      as.matrix()
    colnames(tbls[[1]]) <- NULL
  }

  #Standardize on the number of cols of first "real" table
  nc <- ncol(tbls[[1]])
  tbls <- purrr::keep(.x = tbls,~ncol(.x) >= nc)
  tbls <- lapply(X = tbls,function(y) y[,1:nc])

  #Remaining first row in first table are our column names
  cn <- tbls[[1]][1,]
  # The symbol '#' represents rank
  rnk_idx <- which(grepl("#",cn))
  cn[rnk_idx] <- paste0("rank",rnk_idx)
  if (any(cn == "")){
    blank_idx <- which(cn == "")
    cn[blank_idx] <- paste0("X",blank_idx)
  }

  #Combine
  tbls <- lapply(tbls,function(y) y[-1,])
  tbls <- lapply(tbls,as.data.frame)
  tbls_bind <- bind_rows(tbls)

  colnames(tbls_bind) <- cn

  #Select columns we want to keep
  tbls_sub <- tbls_bind %>%
    mutate(nation = NA_character_,
           fispoints = NA_real_) %>%
    select(finish_rank = rank1,
           fisid = FisCode,
           name = Name,
           nation,
           fispoints,
           matches("^[0-9.,]+k$"),
           matches("Total",ignore.case = TRUE)) %>%
    mutate(finish_rank = stringr::str_extract(string = finish_rank,pattern = "[0-9]+")) %>%
    filter(finish_rank != "") %>%
    rename_with(.fn = ~paste0(race_distance,"k"),.cols = "Total") %>%
    mutate(finish_rank = as.integer(finish_rank))

  #Convert to long form
  tbls_sub_long <- tbls_sub %>%
    gather(key = "split_km",value = "split_time",matches("^[0-9.,]+k$")) %>%
    mutate(split_km = stringr::str_extract(string = split_km,pattern = "[0-9.,]+"),
           split_km = gsub(pattern = ",",replacement = ".",x = split_km,fixed = TRUE),
           split_km = as.numeric(split_km),
           split_time = convert_to_secs(split_time))

  #split_rank & split_time_back
  tbls_sub_long <- tbls_sub_long %>%
    group_by(split_km) %>%
    mutate(split_rank = min_rank(split_time),
           split_time_back = split_time - min(split_time,na.rm = TRUE))

}
