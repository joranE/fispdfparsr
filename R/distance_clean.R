dst_clean <- function(tbls,race_distance,...){
  #Ditch weather & legend tables
  weather_legend <- sapply(tbls,function(x) {
    any(grepl("weather|legend|chief|delegate|did not finish",tolower(x[,1])))
  })
  tbls <- tbls[!weather_legend]

  tbl_head <- tbls[[1]][1:2,]
  cn <- apply(tbl_head,2,function(x) x[x != ""][1])
  cn <- stringr::str_trim(tolower(cn),side = "both")
  cn[is.na(cn)] <- ""

  tbls <- lapply(tbls,function(x,cn){
    x <- x[-(1:2),]
    colnames(x) <- cn
    blank_cn <- sum(cn == "")
    colnames(x)[cn == ""] <- paste0("blank",seq_len(blank_cn))
    idx <- which(x[,1] != "")
    x <- x[sort(c(idx,idx-1)),]
    x <- as_data_frame(x)
    x[] <- lapply(x,stringr::str_trim,side = "both")
    x[[4]][2:nrow(x)] <- x[[4]][1:(nrow(x) - 1)]
    x[[5]][2:nrow(x)] <- x[[5]][1:(nrow(x) - 1)]
    x[x[,1] != "",]
  },cn = cn)

  tbls <- bind_rows(tbls) %>%
    select(-starts_with("blank")) %>%
    select(-contains("behind"))
  if (!any(grepl("^fis$",cn))){
    tbls$fis <- NA
  }
  tbls <- tbls %>%
    rename(fisid = `fis code`,nation = `nsa code`,
           fispoints = fis,finish_time = finish) %>%
    select(rank,bib,fisid,name,nation,fispoints,everything()) %>%
    gather(key = split,
           value = split_time,
           -rank,-bib,-fisid,-name,-nation,-fispoints) %>%
    mutate(split_time = gsub(pattern ="[[:space:]](.*)",
                             replacement = "",
                             x = split_time))
  tbls$split_time <- convert_to_secs(times = tbls$split_time)

  tbls <- tbls %>%
    mutate(split = if_else(split == 'finish_time',
                           paste0(race_distance,"km"),
                           split),
           split = gsub(pattern = "[[:alpha:]]",
                        replacement = "",
                        x = split),
           split = as.numeric(split),
           fispoints = as.numeric(fispoints)) %>%
    group_by(split) %>%
    mutate(split_rank = min_rank(split_time),
           split_time_back = split_time - min(split_time)) %>%
    ungroup() %>%
    rename(split_km = split,
           finish_rank = rank)
  tbls
}

dst_clean_mass <- function(tbls,race_distance){

  tbl_head <- tbls[[1]][1:3,]
  cn <- apply(tbl_head,2,function(x) x[x != "" & !grepl("BONUS",x)][1])
  cn <- stringr::str_trim(tolower(cn),side = "both")

  tbls <- lapply(tbls,function(x,cn){
    x <- x[-(1:5),]
    colnames(x) <- cn
    x <- as_data_frame(x)
    x$grp <- rep(seq_len(nrow(x)/3),each = 3)
    x <- x %>%
      group_by(grp) %>%
      mutate_at(.cols = vars(-matches("^[0-9]")),
                .funs = funs(.[. != ""][1])) %>%
      mutate(val = c('split_rank','split_time','split_bonus'))
  },cn = cn)

  tbls <- bind_rows(tbls) %>%
    select(-starts_with("blank")) %>%
    select(-contains("behind"))
  if (!any(grepl("^fis$",cn))){
    tbls$fis <- NA
  }
  tbls <- tbls %>%
    rename(fisid = `fis code`,nation = `nsa code`,
           fispoints = fis,finish_time = finish) %>%
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
           split_time_back = split_time - min(split_time),
           split_time_bonus = split_time - split_bonus,
           split_time_back_bonus = split_time_bonus - min(split_time_bonus)) %>%
    ungroup()
  tbl_out
}
