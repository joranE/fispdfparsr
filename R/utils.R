#' Parse PDF File
#'
#' Wrapper for \code{\link[tabulizer]{extract_tables}}
#'
#' @param file character; file path
#' @param \dots further arguments to \code{\link[tabulizer]{extract_tables}}
#' @import tabulizer
parse_pdf <- function(file,...){
  tabulizer::extract_tables(file = file,...)
}

#' @export
dst_rank_change <- function(data,n = 5,top_n = Inf){
  split_ends <- range(data$split_km)
  start <- data %>%
    filter(split_km == split_ends[1] &
             split_rank <= top_n)
  end <- data %>%
    filter(split_km == split_ends[2] &
             split_rank <= top_n)
  gain <- data %>%
    filter(fisid %in% end$fisid) %>%
    arrange(fisid,name,split_km) %>%
    group_by(fisid,name) %>%
    summarise(rank_change = tail(split_rank,1) - head(split_rank,1),
              best = min(split_rank),
              worst = max(split_rank)) %>%
    ungroup() %>%
    mutate(idx = min_rank(rank_change)) %>%
    arrange(idx)
  loss <- data %>%
    filter(fisid %in% start$fisid) %>%
    arrange(fisid,name,split_km) %>%
    group_by(fisid,name) %>%
    summarise(rank_change = tail(split_rank,1) - head(split_rank,1),
              best = min(split_rank),
              worst = max(split_rank)) %>%
    ungroup() %>%
    mutate(idx = min_rank(rank_change)) %>%
    arrange(idx)
  result <- list(gain = head(gain,n),loss = tail(loss,n))
  result
}
