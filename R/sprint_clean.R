sprint_clean <- function(tbls,...){
  #Ditch weather, legend & race officials tables
  weather_legend <- sapply(tbls,function(x) {
    any(grepl("weather|legend|chief|delegate",tolower(x[,1])))
  })
  tbls <- tbls[!weather_legend]

  #Assumed maximal set of column names
  cn <- c('rank','bib','name','nation','qual_time','qual_rank',
          paste0("qf",1:5),paste0("sf",1:2),"final_heat")

  #First three tables should have 14, 13 and 11 columns
  act_cols <- sapply(tbls,ncol)[1:3]
  missing_cols <- act_cols < c(14,13,11)
  if (any(missing_cols)){
    #Hoo boy! Here we go...
    for (i in which(missing_cols)){
      nc <- ncol(tbls[[i]])
      cat("I think this section is missing a QF column.\n")
      print(tbls[[i]])
      cat("Insert a blank column after which column? (Enter 0 to skip)?\n")
      col_idx <- readline(prompt = "After column: ")
      col_idx <- as.integer(col_idx)
      if (is.na(col_idx)) stop("Invalid column choice.")
      if (col_idx == 0) next
      if (col_idx > (nc - 3) | col_idx < 6) stop("Column choice out of bounds.")
      else{
        left <- tbls[[i]][,seq_len(col_idx),drop = FALSE]
        right <- tbls[[i]][,(col_idx+1):nc,drop = FALSE]
        mid <- matrix(rep("",nrow(tbls[[i]])),ncol = 1)
        tbls[[i]] <- cbind(left,mid,right)
      }
    }
  }

  #Try to attach column names
  tbls <- lapply(tbls,function(x,cn) {
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
  spr_df <- bind_rows(tbls) %>%
    mutate_all(stringr::str_trim,side = "both") %>%
    mutate_all(na_if,y = "") %>%
    mutate(rank = as.integer(rank),
           bib = as.integer(bib),
           qual_time = period_to_seconds(hms(paste0("00:",qual_time))),
           qual_rank = as.integer(stringr::str_replace_all(qual_rank,"[^0-9]",""))) %>%
    filter(!is.na(rank))

  #More fiddly cleanup and time parsing
  for (col_i in cn[grepl("^qf|^sf|^final",cn)]){
    #Try to catch people relegated to last in a heat
    spr_df[[col_i]] <- gsub(pattern = "RAL",
                            replacement = "(6)",
                            x = spr_df[[col_i]],
                            fixed = TRUE)
    #Separate heat time & rank
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
    #Convert times to seconds
    spr_df[[paste(col_i,"time",sep = "_")]] <- convert_to_secs(times = spr_df[[paste(col_i,"time",sep = "_")]])
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
    mutate(times = if_else(ranks == 1,times,times + times[1]))
  spr <- bind_rows(spr)
  spr
}
