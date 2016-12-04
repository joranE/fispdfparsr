#' Plot Distance Split
#'
#' Various slope graphs for distance race splits.
#'
#' @param data data.frame, as returned by \code{\link{parse_dst_pdf}}
#' @param type character; one of "rank", "time" or "percent"; controls whether
#' the y axis is split rank, split time back or split percent back. Note that
#' both "time" and "percent" will usually lead to a fair amount of overplotting
#' of the athlete names due to space constraints.
#' @param nation_col character vector; if specified, highlight athletes from these nations. Use
#' the three letter codes, all caps. e.g. \code{nation_col = c("NOR","RUS")} You can only specify one of
#' \code{nation_col} or \code{name_col}.
#' @param name_col character vector; if specified, highlight these. Must spell out each athlete's
#' full name as it appears in \code{data}, exactly. e.g. \code{name_col = c("HOFFMAN Noah","HARVEY Alex")}
#' You can only specify one of \code{nation_col} or \code{name_col}.
#' @param offset.x numeric; amount to slide the names to the left/right
#' @examples
#' \dontrun{
#' require(ggplot2)
#' dst <- parse_dst_df(file = system.file("example_pdfs/dst_example1.pdf",package = "fispdfparsr"))
#' p <- dst_split_plot(data = dst,type = "percent",nation_col = c("USA","CAN"))
#' print(p)
#' }
#' @import ggplot2
#' @export
dst_split_plot <- function(data,type = c("rank","time","percent"),
                           nation_col = NULL,name_col = NULL,offset.x = 0.25){
  if (!is.null(nation_col) & !is.null(name_col)){
    stop("Please only specify one of nation_col or name_col.")
  }
  type <- match.arg(arg = type)
  #Add percent back column
  data <- data %>%
    group_by(split) %>%
    mutate(split_perc_back = round(100 * (split_time_back / min(split_time)),2)) %>%
    ungroup()

  ycol <- switch(type,rank = "split_rank",time = "split_time_back",percent = "split_perc_back")
  ylab <- switch(type,rank = "Split Rank",time = "Split Time Back (Sec)",percent = "Split % Back")
  y_ties_adj <- switch(type,rank = 1,time = 1,percent = 1)
  n_splits <- dplyr::n_distinct(data$split)

  left_labels <- filter(data,split == min(split))
  if (anyDuplicated(left_labels[[ycol]]) > 0){
    ties <- which(duplicated(left_labels[[ycol]]))
    left_labels[[ycol]][ties] <- left_labels[[ycol]][ties] + y_ties_adj
  }

  right_labels <- filter(data,split == max(split))
  if (anyDuplicated(right_labels[[ycol]]) > 0){
    ties <- which(duplicated(right_labels[[ycol]]))
    right_labels[[ycol]][ties] <- right_labels[[ycol]][ties] + y_ties_adj
  }

  line_layer <- geom_line()
  if (!is.null(nation_col)){
    data <- data %>%
      mutate(col = if_else(nation %in% nation_col,nation,NA_character_))
    line_layer <- geom_line(aes(color = col))
  }
  if (!is.null(name_col)){
    data <- data %>%
      mutate(col = if_else(name %in% name_col,nation,NA_character_))
    line_layer <- geom_line(aes(color = col))
  }

  x_breaks <- unique(data$split)
  left_labels$label_left <- min(x_breaks) - offset.x
  right_labels$label_right <- max(x_breaks) + offset.x
  xlim <- c(1,max(x_breaks) + 2)

  data$ycol_labs <- format(data[[ycol]],digits = 2)

  p <- ggplot(data = data,aes_string(x = "split",y = ycol,group = "name")) +
    line_layer +
    geom_point(size = 4,fill = "white",
               color = "white",shape = 21) +
    geom_text(aes_string(label = "ycol_labs"),size = 2) +
    geom_text(data = left_labels,
              mapping = aes_string(x = "label_left",y = ycol,
                                   label = "name"),
              hjust = 1,size = 2) +
    geom_text(data = right_labels,
              mapping = aes_string(x = "label_right",y = ycol,
                            label = "name"),
              hjust = 0,size = 2) +
    labs(x = "Split (km)",y = ylab) +
    scale_x_continuous(breaks = x_breaks,labels = x_breaks,limits = xlim) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "bottoms",
          legend.direction = "horizontal")
  p
}
