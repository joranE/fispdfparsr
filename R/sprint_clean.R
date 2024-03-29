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
      col_idx <- as.integer(unlist(strsplit(col_idx,",")))
      if (any(is.na(col_idx) | col_idx < 0 | col_idx > nc)) stop("Invalid column choice.")
      if (col_idx[1] == 0) next
      #if (col_idx > (nc - 3) | col_idx < 6) stop("Column choice out of bounds.")
      else{
        col_count <- col_idx
        for (j in seq_along(col_idx)){
          num_cols <- readline(prompt = sprintf("Number of columns to insert after col %s: ",col_idx[j]))
          num_cols <- as.integer(num_cols)
          left <- tbls[[i]][,seq_len(col_idx[j]),drop = FALSE]
          right <- tbls[[i]][,(col_idx[j]+1):ncol(tbls[[i]]),drop = FALSE]
          mid <- matrix(rep("",nrow(tbls[[i]]) * num_cols),ncol = num_cols)
          tbls[[i]] <- cbind(left,mid,right)
          if (j < length(col_idx)){
            col_idx[j+1] <- col_idx[j+1] + num_cols
          }
        }
      }
    }
  }

  for (i in seq_along(tbls)){
    nc <- ncol(tbls[[i]])
    col2_size <- max(nchar(stringr::str_trim(tbls[[i]][,2])))
    leading_digit <- grepl("^[0-9]",tbls[[i]][,2])

    if (col2_size > 3 & all(leading_digit)){
      left <- tbls[[i]][,1,drop = FALSE]

      if (all(tbls[[i]][,3] == "")) rt <- 4 else rt <- 3
      right = tbls[[i]][,rt:nc,drop = FALSE]
      bib <- stringr::str_extract(string = tbls[[i]][,2],pattern = "^[0-9]+")
      ath_name <- stringr::str_replace(string = tbls[[i]][,2],
                                       pattern = "^[0-9]+",
                                       replacement = "")
      ath_name <- stringr::str_trim(ath_name)
      tbls[[i]] <- cbind(left,cbind(bib,ath_name),right)
    }
  }

  #Try to attach column names
  tbls <- lapply(tbls,function(x,cn) {
    nc <- ncol(x)
    #Checking is col2 is actually names; for when bib
    # is all blank
    col2_size <- max(nchar(stringr::str_trim(x[,2])))
    if (col2_size > 3){
      cn <- cn[-2]
      colnames(x) <- cn[seq_len(nc)]
      return(tibble::as_tibble(x))
    }else{
      colnames(x) <- cn[seq_len(nc)]
      return(tibble::as_tibble(x))
    }
  },cn = cn)

  #Put tables from each round together and clean up times and ranks
  spr_df <- bind_rows(tbls) %>%
    mutate_all(stringr::str_trim,side = "both") %>%
    mutate_all(na_if,y = "") %>%
    mutate(rank = as.integer(rank),
           bib = as.integer(bib),
           qual_time = stringr::str_trim(gsub("\\(.*\\)","",qual_time)),
           qual_time = period_to_seconds(hms(paste0("00:",qual_time))),
           qual_rank = as.integer(stringr::str_replace_all(qual_rank,"[^0-9]",""))) %>%
    filter(!is.na(rank))

  if (any(is.na(spr_df$qual_rank))){
    spr_df <- mutate(spr_df,qual_rank = dplyr::coalesce(qual_rank,rank))
  }

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
