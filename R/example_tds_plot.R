# pdfs <- list.files(path = "~/Desktop/scratch/fis_pdfs/tds16",full.names = TRUE)
#
# tds <- lapply(pdfs,FUN = parse_stage_pdf)
# tds[[1]]$V8 <- NULL
# colnames(tds[[1]])[8] <- "stage1_rank"
# tds[[1]]$stage1_rank <- as.integer(tds[[1]]$stage1_rank)
# tds[[2]]$V9 <- NULL
# colnames(tds[[2]])[8] <- "stage1_rank"
# tds[[2]]$stage1_rank <- as.integer(tds[[2]]$stage1_rank)
#
# tds_copy <- tds
# tds_copy <- bind_rows(tds_copy,.id = "idx")
# tds_copy <- tds_copy %>%
#   group_by(fisid) %>%
#   filter(idx == max(idx)) %>%
#   ungroup()
#
# p <- stage_plot(data = tds_copy,type = "rank")
