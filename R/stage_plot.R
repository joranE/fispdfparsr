#' Stage Results Plot
#'
#' Plot bumps chart for stage race results.
#'
#' @param data data.frame; output from \code{\link{parse_stage_pdf}}
#' @param type character; one of "rank", "time" or "percent" to plot
#' overall rank, time back or percent back.
#' @param nation_col character vector of nations to highlight
#' @param name_col character vector of athletes to highlight
#' @param offset.x numeric; amount to adjust the athlete labels on the
#' left and right of the plot
#' @export
#' @examples
#' \dontrun{
#' pdf <- system.file("example_pdfs/stage_example2.pdf",package = "fispdfparsr")
#' stg <- parse_stage_pdf(file = pdf)
#' require(ggplot2)
#' p <- stage_plot(data = stg,type = "rank")
#' print(p)
#' }
stage_plot <- function(data,type = c("rank","time","percent"),
                       nation_col = NULL,name_col = NULL,offset.x = 0.05){
  if (!is.null(nation_col) & !is.null(name_col)){
    stop("Please only specify one of nation_col or name_col.")
  }
  type <- match.arg(arg = type)
  ycol <- switch(EXPR = type,
                 rank = "rank",
                 time = "back",
                 percent = "pb")
  ylab <- switch(EXPR = type,
                 rank = "Overall Rank",
                 time = "Overall Time Back",
                 percent = "Overall % Back")
  y_ties_adj <- switch(type,rank = 1,time = 1,percent = 1)

  #Stage times
  stg_times <- stg %>%
    select(fisid,name,nation,ends_with("time")) %>%
    select(-total_time) %>%
    gather(key = stage_time,value = time,ends_with("time")) %>%
    mutate(stage_time = gsub(pattern = "_time$",replacement = "",x = stage_time),
           stage_time = gsub(pattern = "^stage",replacement = "S",stage_time)) %>%
    rename(stage = stage_time) %>%
    arrange(fisid,name,nation,stage)

  #Stage bonuses
  stg_bonuses <- stg %>%
    select(fisid,name,nation,ends_with("bonus")) %>%
    gather(key = stage_bonus,value = bonus,ends_with("bonus")) %>%
    mutate(stage_bonus = gsub(pattern = "_bonus$",replacement = "",x = stage_bonus),
           stage_bonus = gsub(pattern = "^stage",replacement = "S",x = stage_bonus)) %>%
    rename(stage = stage_bonus) %>%
    arrange(fisid,name,nation,stage)

  bonus_stgs <- stg_bonuses %>%
    filter(bonus > 0) %>%
    select(stage) %>%
    unique() %>%
    arrange(stage)

  stg_time_bon <- left_join(stg_times,
                            stg_bonuses,
                            by = c('fisid','name','nation','stage')) %>%
    mutate(bonus = if_else(is.na(bonus),0L,bonus),
           time_with_bonus = time - bonus) %>%
    group_by(fisid,name,nation) %>%
    mutate(cum_time = cumsum(time),
           cum_bonus_time = cumsum(time_with_bonus)) %>%
    group_by(stage) %>%
    mutate(time_rank = min_rank(cum_time),
           bonus_rank = min_rank(cum_bonus_time),
           time_pb = 100 * (cum_time - min(cum_time)) / min(cum_time),
           bonus_pb = 100 * (cum_bonus_time - min(cum_bonus_time)) / min(cum_bonus_time),
           time_back = cum_time - min(cum_time),
           bonus_back = cum_bonus_time - min(cum_bonus_time)) %>%
    ungroup()

  col_sel <- (colnames(stg_time_bon) %in% c('fisid','name','nation','stage')) |
    (grepl(pattern = paste0(ycol,"$"),x = colnames(stg_time_bon)))
  stg_data <- stg_time_bon[,col_sel] %>%
    mutate(stage_bonus = paste(stage,"Bonus"))

  time_pts <- stg_data %>%
    select(fisid,name,nation,stage,starts_with("time"))
  cn_idx <- grepl(paste0(ycol,"$"),colnames(time_pts))
  colnames(time_pts)[cn_idx] <- "y"
  bonus_pts <- stg_data %>%
    select(fisid,name,nation,stage,stage_bonus,starts_with("bonus")) %>%
    semi_join(bonus_stgs,by = "stage") %>%
    select(-stage) %>%
    rename(stage = stage_bonus)
  cn_idx <- grepl(paste0(ycol,"$"),colnames(bonus_pts))
  colnames(bonus_pts)[cn_idx] <- "y"

  all_pts <- bind_rows(time_pts,bonus_pts) %>%
    arrange(stage,y)

  line_layer <- geom_line()
  if (!is.null(nation_col)){
    all_pts <- all_pts %>%
      mutate(col = if_else(nation %in% nation_col,nation,NA_character_))
    line_layer <- geom_line(aes(color = col))
  }
  if (!is.null(name_col)){
    all_pts <- all_pts %>%
      mutate(col = if_else(name %in% name_col,nation,NA_character_))
    line_layer <- geom_line(aes(color = col))
  }

  left_labels <- filter(all_pts,stage == min(stage))
  if (anyDuplicated(all_pts[["y"]]) > 0){
    ties <- which(duplicated(left_labels[["y"]]))
    left_labels[["y"]][ties] <- left_labels[["y"]][ties] + 1
  }

  right_labels <- filter(all_pts,stage == max(stage))
  if (anyDuplicated(right_labels[["y"]]) > 0){
    ties <- which(duplicated(right_labels[["y"]]))
    right_labels[["y"]][ties] <- right_labels[["y"]][ties] + 1
  }

  x_breaks <- unique(all_pts$stage)
  left_labels$label_left <- 1 - offset.x
  right_labels$label_right <- length(x_breaks) + offset.x
  xlim <- c(0,length(x_breaks) + 1)

  p <- ggplot(data = all_pts,aes(x = stage,y = y,group = name)) +
    line_layer +
    geom_point(size = 2,fill = "white",
               color = "white",shape = 21) +
    geom_text(data = left_labels,
              mapping = aes_string(x = "label_left",y = "y",
                                   label = "name"),
              hjust = 1,size = 2) +
    geom_text(data = right_labels,
              mapping = aes_string(x = "label_right",y = "y",
                                   label = "name"),
              hjust = 0,size = 2) +
    labs(x = "Stage",y = ylab) +
    scale_x_discrete(breaks = x_breaks,labels = x_breaks,expand = c(0,2)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1.25),
          legend.position = "bottoms",
          legend.direction = "horizontal")
  p
}

