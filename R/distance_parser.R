#' Parse Distance Race PDFs
#'
#' Convert FIS distance result PDFs into a format more
#' suitable for analysis. All times are converted to seconds.
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
  dst_tbls <- tabulizer::extract_tables(file = file,
                                        method = "matrix")

  #Ditch weather & legend tables
  weather_legend <- sapply(dst_tbls,function(x) {
    any(grepl("weather|legend",tolower(x[,1])))
    })
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
    select(-starts_with("blank"),-behind)
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
    ungroup()
  dst
}
