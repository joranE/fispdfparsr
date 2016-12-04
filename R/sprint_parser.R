#' Parse Sprint Race PDFs
#'
#' Convert FIS sprint result PDFs into a format more
#' suitable for analysis. All times are converted to seconds.
#'
#' @param final_pdf file path to PDF of final sprint results
#' @export
#' @import tabulizer
#' @import stringr
#' @import lubridate
#' @import dplyr
#' @examples
#' \dontrun{
#' spr <- parse_spr_df(final_pdf = system.file("inst/example_pdfs/sprint_final.pdf",package = "fispdfparsr"))
#' }
parse_spr_pdf <- function(final_pdf = NULL){
  if (is.null(final_pdf)){
    stop("Must provide file path for final_pdf.")
  }

  #Read tables from final PDF
  spr_tbls <- tabulizer::extract_tables(file = final_pdf,
                             method = "matrix")

  #Ditch weather & legend tables
  weather_legend <- sapply(spr_tbls,function(x) any(grepl("weather|legend",tolower(x[,1]))))
  spr_tbls <- spr_tbls[!weather_legend]

  #Assumed maximal set of column names
  cn <- c('rank','bib','name','nation','qual_time','qual_rank',
          paste0("qf",1:5),paste0("sf",1:2),"final_heat")

  #Try to attach column names
  spr_tbls <- lapply(spr_tbls,function(x,cn) {
    nc <- ncol(x)
    if (nc < 6){
      cn <- cn[-2]
      colnames(x) <- cn[seq_len(nc)]
      return(as_data_frame(x))
    }else{
      colnames(x) <- cn[seq_len(nc)]
      return(as_data_frame(x))
    }
  },cn = cn)

  #Put tables from each round together and clean up times and ranks
  spr_df <- bind_rows(spr_tbls) %>%
    mutate_all(stringr::str_trim,side = "both") %>%
    mutate_all(na_if,y = "") %>%
    mutate(rank = as.integer(rank),
           bib = as.integer(bib),
           qual_time = period_to_seconds(hms(paste0("00:",qual_time))),
           qual_rank = as.integer(stringr::str_replace_all(qual_rank,"[^0-9]","")))

  #More fiddly cleanup and time parsing
  for (col_i in cn[grepl("^qf|^sf|^final",cn)]){
    spr_df <- separate_(spr_df,
                        col = col_i,
                        into = c(paste(col_i,"time",sep = "_"),paste(col_i,"rank",sep = "_")),
                        sep = "[(]")
    spr_df[[paste(col_i,"rank",sep = "_")]] <- as.integer(gsub(pattern = "[^0-9]",
                                                               replacement = "",
                                                               x = spr_df[[paste(col_i,"rank",sep = "_")]]))
    spr_df[[paste(col_i,"time",sep = "_")]] <- gsub(pattern = "[^0-9:\\.]",
                                                    replacement = "",
                                                    x = spr_df[[paste(col_i,"time",sep = "_")]])
    colon_count <- 2 - stringr::str_count(spr_df[[paste(col_i,"time",sep = "_")]],":")
    time_pad <- ifelse(colon_count == 1,"00:","00:00:")
    time_pad[is.na(time_pad)] <- ""
    spr_df[[paste(col_i,"time",sep = "_")]] <- lubridate::period_to_seconds(lubridate::hms(paste0(time_pad,spr_df[[paste(col_i,"time",sep = "_")]])))
  }

  #Gather times and ranks
  spr_times <- spr_df %>%
    select(rank,bib,name,nation,ends_with("_time")) %>%
    gather(key = round,value = times,ends_with("_time")) %>%
    mutate(round = gsub("_time","",round,fixed = TRUE)) %>%
    filter(!is.na(times))
  spr_ranks <- spr_df %>%
    select(rank,bib,name,nation,ends_with("_rank")) %>%
    gather(key = round,value = ranks,ends_with("_rank")) %>%
    mutate(round = gsub("_rank","",round,fixed = TRUE)) %>%
    filter(!is.na(ranks))

  #Recombine and convert time-backs into raw heat times
  spr <- full_join(spr_times,
                   spr_ranks,
                   by = c("rank","bib","name","nation","round"))
  spr <- split(spr,spr$round == "qual")
  spr[[1]] <- spr[[1]] %>%
    arrange(round,ranks) %>%
    group_by(round) %>%
    mutate(times = cumsum(times))
  spr <- bind_rows(spr)
  spr
}
