#' Plot Distance Split
#'
#' Various slope graphs for distance race splits.
#'
#' @param data data.frame, as returned by \code{\link{parse_dst_pdf}}
#' @param type character; one of "rank", "time" or "percent"; controls whether
#' the y axis is split rank, split time back or split percent back. Note that
#' both "time" and "percent" will usually lead to a fair amount of overplotting
#' of the athlete names due to space constraints.
#' @param nation_col character vector; if specified, highlight athletes from
#' these nations. Use the three letter codes, all caps. e.g.
#' \code{nation_col = c("NOR","RUS")}. You can only specify one of \code{nation_col}
#' or \code{name_col}.
#' @param name_col character vector; if specified, highlight these. Must spell
#' out each athlete's full name as it appears in \code{data}, exactly. e.g.
#' \code{name_col = c("HOFFMAN Noah","HARVEY Alex")} You can only specify one
#' of \code{nation_col} or \code{name_col}.
#' @param offset.x numeric; amount to slide the names to the left/right
#' @examples
#' \dontrun{
#' require(ggplot2)
#' dst <- parse_dst_pdf(file = system.file("example_pdfs/dst_example1.pdf",
#'                                        package = "fispdfparsr"))
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
    group_by(split_km) %>%
    mutate(split_perc_back = round(100 * (split_time_back / min(split_time,na.rm = TRUE)),2)) %>%
    ungroup()

  ycol <- switch(EXPR = type,
                 rank = "split_rank",
                 time = "split_time_back",
                 percent = "split_perc_back")
  ylab <- switch(EXPR = type,
                 rank = "Split Rank",
                 time = "Split Time Back (Sec)",
                 percent = "Split % Back")
  y_ties_adj <- switch(type,rank = 1,time = 5,percent = 0.005)
  n_splits <- dplyr::n_distinct(data$split_km)

  left_labels <- filter(data,split_km == min(split_km))
  if (anyDuplicated(left_labels[[ycol]]) > 0){
    ties <- which(duplicated(left_labels[[ycol]]))
    left_labels[[ycol]][ties] <- left_labels[[ycol]][ties] + y_ties_adj
  }

  right_labels <- filter(data,split_km == max(split_km))
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

  x_breaks <- unique(data$split_km)
  left_labels$label_left <- min(x_breaks) - offset.x
  right_labels$label_right <- max(x_breaks) + offset.x
  xlim <- c(1,max(x_breaks) + 2)

  data$ycol_labs <- format(data[[ycol]],digits = 2)

  p <- ggplot(data = data,
              aes_string(x = "split_km",y = ycol,group = "name")) +
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

#' Plot Distance Races Split Movement
#'
#' Highlight movement up and down by skiers throughout a distance race.
#'
#' @param data data.frame; as returned by \code{\link{parse_dst_pdf}}
#' @param n integer; number of athletes to highlight in each direction
#' @param top_n integer; focus on athletes moving into or out of the top_n
#' @param offset.x numeric; amount to slide the names to the left/right
#' @export
dst_move_plot <- function(data,n = 5,top_n = Inf,offset.x = 0.25){
  rc <- dst_rank_change(data = data,n = n,top_n = top_n)
  rc <- bind_rows(rc) %>%
    mutate(gain_loss = rep(c("Moved Up",'Moved Back'),each = n),
           col = rep(c('up','down'),each = n)) %>%
    select(fisid,name,gain_loss,col)

  ycol <- "split_rank"
  ylab <- "Split Rank"
  y_ties_adj <- 1
  n_splits <- dplyr::n_distinct(data$split_km)

  data_facet <- bind_rows(data,data) %>%
    mutate(gain_loss = rep(c("Moved Up","Moved Back"),each = nrow(data)))
  data_facet <- data_facet %>%
    left_join(rc,by = c('fisid','name','gain_loss'))

  left_labels <- filter(data,split_km == min(split_km))
  if (anyDuplicated(left_labels[[ycol]]) > 0){
    ties <- which(duplicated(left_labels[[ycol]]))
    left_labels[[ycol]][ties] <- left_labels[[ycol]][ties] + y_ties_adj
  }
  ll_facet <- bind_rows(left_labels,left_labels) %>%
    mutate(gain_loss = rep(c("Moved Up","Moved Back"),each = nrow(left_labels)))

  right_labels <- filter(data,split_km == max(split_km))
  if (anyDuplicated(right_labels[[ycol]]) > 0){
    ties <- which(duplicated(right_labels[[ycol]]))
    right_labels[[ycol]][ties] <- right_labels[[ycol]][ties] + y_ties_adj
  }
  rl_facet <- bind_rows(right_labels,right_labels) %>%
    mutate(gain_loss = rep(c("Moved Up","Moved Back"),each = nrow(right_labels)))

  x_breaks <- unique(data_facet$split_km)
  ll_facet$label_left <- min(x_breaks) - offset.x
  rl_facet$label_right <- max(x_breaks) + offset.x
  xlim <- c(1,max(x_breaks) + 2)

  data_facet$ycol_labs <- format(data_facet[[ycol]],digits = 2)
  data_facet$size <- if_else(is.na(data_facet$col),"normal","big")

  p <- ggplot(data = data_facet,
              aes_string(x = "split_km",y = ycol,group = "name")) +
    facet_wrap(~gain_loss,nrow = 1) +
    geom_line(aes(col = col,size = size),show.legend = FALSE) +
    geom_point(size = 4,fill = "white",
               color = "white",shape = 21) +
    geom_text(aes_string(label = "ycol_labs"),size = 2) +
    geom_text(data = ll_facet,
              mapping = aes_string(x = "label_left",y = ycol,
                                   label = "name"),
              hjust = 1,size = 2) +
    geom_text(data = rl_facet,
              mapping = aes_string(x = "label_right",y = ycol,
                                   label = "name"),
              hjust = 0,size = 2) +
    labs(x = "Split (km)",y = ylab) +
    scale_x_continuous(breaks = x_breaks,labels = x_breaks,limits = xlim) +
    scale_size_manual(values = c('normal' = 0.5,'big' = 1)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "bottoms",
          legend.direction = "horizontal")
  p
}
