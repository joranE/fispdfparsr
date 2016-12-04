#' Parse Distance Race PDFs
#'
#' Convert FIS distance result PDFs into a format more
#' suitable for analysis. All times are converted to seconds.
#'
#' @param race_pdf file path to PDF of distance results
#' @param race_distance numeric; race distance in km
#' @export
#' @examples
#' \dontrun{
#' dst <- parse_dst_df(final_pdf = system.file("inst/example_pdfs/men_dst.pdf",package = "fispdfparsr"))
#' }
parse_dst_pdf <- function(race_pdf = NULL,race_distance = NULL){
  if (is.null(final_pdf)){
    stop("Must provide file path for final_pdf.")
  }
  if (is.null(race_distance)){
    stop("Must provide race distance (in km).")
  }

  #Read tables from final PDF
  dst_tbls <- tabulizer::extract_tables(file = race_pdf,
                                        method = "matrix")

  #Ditch weather & legend tables
  weather_legend <- sapply(dst_tbls,function(x) any(grepl("weather|legend",tolower(x[,1]))))
  dst_tbls <- dst_tbls[!weather_legend]

  cn <- stringr::str_trim(tolower(dst_tbls[[1]][1,]),side = "both")

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
    select(-starts_with("blank"),-behind) %>%
    rename(fisid = `fis code`,nation = `nsa code`,
           fispoints = fis,finish_time = finish) %>%
    select(rank,bib,fisid,name,nation,fispoints,everything()) %>%
    gather(key = split,value = split_time,-rank,-bib,-fisid,-name,-nation,-fispoints) %>%
    mutate(split_time = gsub(pattern ="[[:space:]](.*)",replacement = "",x = split_time))
  colon_count <- 2 - stringr::str_count(dst$split_time,":")
  time_pad <- ifelse(colon_count == 1,"00:","00:00:")
  time_pad[is.na(time_pad)] <- ""
  dst$split_time <- lubridate::period_to_seconds(lubridate::hms(paste0(time_pad,dst$split_time)))

  dst <- dst %>%
    mutate(split = if_else(split == 'finish_time',paste0(race_distance,"km"),split),
           split = gsub(pattern = "km$",replacement = "",x = split),
           split = as.numeric(split)) %>%
    group_by(split) %>%
    mutate(split_rank = min_rank(split_time),
           split_time_back = split_time - min(split_time)) %>%
    ungroup()
  dst
}
