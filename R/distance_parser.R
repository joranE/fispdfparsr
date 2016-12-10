#' Parse Distance Race PDFs
#'
#' Convert FIS distance result PDFs into a format more
#' suitable for analysis. All times are converted to seconds.
#'
#' @result A data.frame; specifically a \code[dplyr]{\link{tbl_df}}
#'
#' @param file file path to PDF of distance results
#' @param race_distance numeric; race distance in km
#' @export
#' @import tidyr
#' @examples
#' \dontrun{
#' dst <- parse_dst_pdf(file = system.file("example_pdfs/dst_example1.pdf",
#'                                         package = "fispdfparsr"),15)
#' }
parse_dst_pdf <- function(file = NULL,race_distance = NULL){
  if (is.null(file)){
    stop("Must provide file path for race PDF.")
  }
  if (is.null(race_distance)){
    stop("Must provide race distance (in km).")
  }

  #Read tables from final PDF
  dst_tbls <- parse_pdf(file = file,method = "matrix",...)

  #Ditch weather & legend tables
  weather_legend <- sapply(dst_tbls,function(x) {
    any(grepl("weather|legend|chief|delegate|did not finish",tolower(x[,1])))
    })
  dst_tbls <- dst_tbls[!weather_legend]

  #Escape hatch of 30k/50k mass start races
  # that are more similar to stage races in that
  # they have bonus seconds at split
  if (race_distance %in% c(30,50)){
    result <- parse_dst_long_mass(dst_tbls = dst_tbls,
                                  race_distance = race_distance)
    return(result)
  }

  tbl_head <- dst_tbls[[1]][1:2,]
  cn <- apply(tbl_head,2,function(x) x[x != ""][1])
  cn <- stringr::str_trim(tolower(cn),side = "both")
  cn[is.na(cn)] <- ""

  dst_tbls <- lapply(dst_tbls,function(x,cn){
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

  dst <- bind_rows(dst_tbls) %>%
    select(-starts_with("blank")) %>%
    select(-contains("behind"))
  if (!any(grepl("^fis$",cn))){
    dst$fis <- NA
  }
  dst <- dst %>%
    rename(fisid = `fis code`,nation = `nsa code`,
           fispoints = fis,finish_time = finish) %>%
    select(rank,bib,fisid,name,nation,fispoints,everything()) %>%
    gather(key = split,
           value = split_time,
           -rank,-bib,-fisid,-name,-nation,-fispoints) %>%
    mutate(split_time = gsub(pattern ="[[:space:]](.*)",
                             replacement = "",
                             x = split_time))
  dst$split_time <- convert_to_secs(times = dst$split_time)

  dst <- dst %>%
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
  dst
}

parse_dst_long_mass <- function(dst_tbls,race_distance){

  tbl_head <- dst_tbls[[1]][1:3,]
  cn <- apply(tbl_head,2,function(x) x[x != "" & !grepl("BONUS",x)][1])
  cn <- stringr::str_trim(tolower(cn),side = "both")

  dst_tbls <- lapply(dst_tbls,function(x,cn){
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

  dst <- bind_rows(dst_tbls) %>%
    select(-starts_with("blank")) %>%
    select(-contains("behind"))
  if (!any(grepl("^fis$",cn))){
    dst$fis <- NA
  }
  dst <- dst %>%
    rename(fisid = `fis code`,nation = `nsa code`,
           fispoints = fis,finish_time = finish) %>%
    select(rank,bib,fisid,name,nation,fispoints,everything())
  dst_rank <- dst %>%
    filter(val == 'split_rank') %>%
    gather(key = split_km,value = split_rank,matches("^[0-9]")) %>%
    ungroup() %>%
    select(-grp,-val,-finish_time)
  dst_time <- dst %>%
    filter(val == 'split_time') %>%
    gather(key = split_km,value = split_time,matches("^[0-9]")) %>%
    ungroup() %>%
    select(-grp,-val)
  dst_time$split_time <- convert_to_secs(times = dst_time$split_time)
  dst_bonus <- dst %>%
    filter(val == 'split_bonus') %>%
    gather(key = split_km,value = split_bonus,matches("^[0-9]")) %>%
    ungroup() %>%
    select(-grp,-val,-finish_time) %>%
    mutate(split_bonus = if_else(split_bonus == "",NA_character_,split_bonus),
           split_bonus = as.integer(split_bonus))

  #Add final split
  dst_rank_final <- dst_rank %>%
    select(rank,bib,fisid,name,nation,fispoints) %>%
    unique() %>%
    mutate(split_km = paste0(race_distance,"km"),
           split_rank = rank)
  dst_rank <- bind_rows(dst_rank,dst_rank_final)

  dst_time_final <- dst_time %>%
    select(rank,bib,fisid,name,nation,fispoints,finish_time) %>%
    unique() %>%
    mutate(split_km = paste0(race_distance,"km")) %>%
    rename(split_time = finish_time) %>%
    mutate(split_time = convert_to_secs(times = split_time))
  dst_time$finish_time <- NULL
  dst_time <- bind_rows(dst_time,dst_time_final)

  dst_out <- inner_join(dst_rank,
                        dst_time,
                        by = c('rank','bib','fisid','name','nation',
                               'fispoints','split_km')) %>%
    left_join(dst_bonus,
               by = c('rank','bib','fisid','name','nation',
                      'fispoints','split_km')) %>%
    rename(finish_rank = rank,
           split_rank_bonus = split_rank) %>%
    mutate(split_km = gsub(pattern = "km$",replacement = "",x = split_km),
           split_km = as.numeric(split_km),
           fispoints = as.numeric(fispoints),
           split_bonus = if_else(is.na(split_bonus),0L,split_bonus))

  dst_out <- dst_out %>%
    group_by(split_km) %>%
    mutate(split_rank = min_rank(split_time),
           split_time_back = split_time - min(split_time),
           split_time_bonus = split_time - split_bonus,
           split_time_back_bonus = split_time_bonus - min(split_time_bonus)) %>%
    ungroup()
  dst_out
}
