#' Plot Sprint Heats
#'
#' Various graphs for sprint race heats.
#'
#' @param data data.frame, as returned by \code{\link{parse_spr_pdf}}
#' @param type character; one of "rank", "time" or "centered"; controls whether
#' the y axis is heat rank, heat time or the times centered by round.
#' @param nation_col character vector; if specified, highlight athletes from these nations. Use
#' the three letter codes, all caps. e.g. \code{nation_col = c("NOR","RUS")}
#' @param name_col character vector; if specified, highlight these. Must spell out each athlete's
#' full name as it appears in \code{data}, exactly. e.g. \code{name_col = c("HOFFMAN Noah","HARVEY Alex")}
#' @param offset.x numeric; amount to slide the names to the left/right
#' @examples
#' \dontrun{
#' require(ggplot2)
#' spr <- parse_spr_df(file = system.file("example_pdfs/spr_example1.pdf",package = "fispdfparsr"))
#' p <- spr_heat_plot(data = spr,type = "centered",nation_col = c("USA","CAN"))
#' print(p)
#' }
#' @import ggplot2
#' @export
spr_heat_plot <- function(data,type = c("rank","time","centered"),
                          nation_col = NULL,name_col = NULL,offset.x = 0.03){
  if (!is.null(nation_col) & !is.null(name_col)){
    stop("Please only specify one of nation_col or name_col.")
  }
  data <- filter(data,rank <= 30)
  type <- match.arg(arg = type)

  ycol <- switch(type,rank = "round_ranks",time = "times",centered = "centered_times")
  ylab <- switch(type,rank = "Round Rank",time = "Raw Times (Sec)",centered = "Centered Times By Round")
  y_ties_adj <- switch(type,rank = 1,time = 1,percent = 1)

  data$round_type <- dplyr::recode(data$round,
                                  qual = "Qualification",
                                  qf1 = "Quarterfinal",
                                  qf2 = "Quarterfinal",
                                  qf3 = "Quarterfinal",
                                  qf4 = "Quarterfinal",
                                  qf5 = "Quarterfinal",
                                  sf1 = "Semifinal",
                                  sf2 = "Semifinal",
                                  final_heat = "Final")
  data$round_type <- factor(data$round_type,
                           levels = c('Qualification','Quarterfinal',
                                      'Semifinal','Final'))
  data <- data %>%
    group_by(round_type) %>%
    mutate(centered_times = (times - mean(times,na.rm = TRUE)) / sd(times,na.rm = TRUE),
           round_ranks = min_rank(times)) %>%
    ungroup() %>%
    mutate(heat_label = stringr::str_extract(round,pattern = "[0-9]"))

  final_labs <- filter(data,round == "final_heat") %>%
    mutate(xpos = 4 + offset.x)
  semi_labs <- filter(data,grepl("^sf",round) & !name %in% final_labs$name) %>%
    mutate(xpos = 3 + offset.x)
  quart_labs <- filter(data,grepl("^qf",round) &
                         !name %in% c(final_labs$name,semi_labs$name)) %>%
    mutate(xpos = 2 + offset.x)

  line_layer <- geom_line(alpha = 0.5)
  if (!is.null(nation_col)){
    data <- data %>%
      mutate(col = if_else(nation %in% nation_col,nation,NA_character_))
    line_layer <- geom_line(aes(color = col),alpha = 0.5)
  }
  if (!is.null(name_col)){
    data <- data %>%
      mutate(col = if_else(name %in% name_col,nation,NA_character_))
    line_layer <- geom_line(aes(color = col),alpha = 0.5)
  }

  p <- ggplot(data = data,aes_string(x = "round_type",y = ycol,group = "name")) +
    line_layer +
    geom_point(shape = 21,color = "white",
               fill = "white",size = 4) +
    geom_text(aes(label = heat_label),size = 2) +
    geom_text(data = final_labs,
              aes_string(x = "xpos",y = ycol,label = "name"),
              hjust = 0,size = 2) +
    geom_text(data = semi_labs,
              aes_string(x = "xpos",y = ycol,label = "name"),
              hjust = 0,size = 2) +
    geom_text(data = quart_labs,
              aes_string(x = "xpos",y = ycol,label = "name"),
              hjust = 0,size = 2) +
    labs(x = "Round",y = ylab) +
  theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "bottoms",
          legend.direction = "horizontal")
  p
}
