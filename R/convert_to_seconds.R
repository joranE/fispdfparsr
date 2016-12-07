#' Converts Times To Seconds
#'
#' Utility function that pads times with the appropriate number of copies
#' of "00:" to put it in hh:mm:ss.s format and then converts to seconds.
#'
#' @param times character vector of times
#' @export
convert_to_secs <- function(times){
  colon_count <- 2 - stringr::str_count(times,":")
  time_pad <- ifelse(colon_count == 0,"",
                     ifelse(colon_count == 1,"00:","00:00:"))
  time_pad[is.na(time_pad)] <- ""
  lubridate::period_to_seconds(lubridate::hms(paste0(time_pad,times)))
}
