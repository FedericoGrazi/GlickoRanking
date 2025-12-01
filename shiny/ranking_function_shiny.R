
update_devs <- function(rat,cval = 16,minTourn = 2, time = weeks_passed(lubridate::year(Sys.Date()),lubridate::month(Sys.Date()),lubridate::date(Sys.Date())),rdmax = 250){
  rat$ratings <- rat$ratings[rat$rating$nTourn>minTourn,]
  cdevs <- rat$ratings$Deviation
  nlasttourn <- rat$ratings$LastTourn
  newdevs <- round(pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax))
  rat$ratings$Deviation <- newdevs
  out <- data.frame(giocatore = rat$ratings$giocatore,
                    Rating = rat$ratings$Rating,
                    `Last Dev` = cdevs,
                    `.` = "|",
                    Diffs = round(newdevs-cdevs),
                    `Today's Dev` = newdevs)
  
  return(out)
}

date_from_weeks <- function(weeks) {
  start_date <- as.Date("2023-04-15")
  target_date <- start_date + (weeks * 7)  # Convert weeks to days
  return(target_date)
}


order_history <- function(rat){
    output <- by(rat$history, rat$history$Category, function(x) x[order(x$Rating, decreasing = TRUE), ])
    return(output)
}

weeks_passed <- function(year, month, day, date = NULL) {
  if(is.null(date)) input_date <- as.Date(paste(year, month, day, sep = "-")) else input_date <- date
  reference_date <- as.Date("2023-04-15")
  weeks <- round(as.numeric(difftime(input_date, reference_date, units = "weeks")),0)
  return(weeks)
}

date_from_weeks <- function(weeks) {
  start_date <- as.Date("2023-04-15")
  target_date <- start_date + (weeks * 7)  # Convert weeks to days
  return(target_date)
}


find_player <- function(rat, player = "Lukas", type = c("history", "storico")){
  if(missing(type)) type <- "history"
  if(type == "history"){
    if(nrow(rat$history[str_detect(rownames(rat$history), player),]) ==0){
      print("Giocatore non trovato a questo torneo")
    }else{
      plain_hist <- rat$history[str_detect(rownames(rat$history), player),]
      matches <- match(rownames(plain_hist), rat$ratings$giocatore)
      plain_hist$NewRat <- rat$ratings$Rating[matches]
      plain_hist$PrevRat <- plain_hist$NewRat-plain_hist$Rating
      x <- plain_hist[,c(8,1,7,4,3,5)]
      x$`.` <- x$`,` <- "|" 
      x <- x[,c(7,1,2,3,8,4,5,6)]
     return(x)
    }
  }else if(type == "storico"){
    x <- rat$ratings[str_detect(rat$ratings$giocatore,player),]
   return(x)
  }
}

classifica <- function(rs, file, both = FALSE, oppScore = FALSE){
  if(oppScore){
    repeat {
      score_input <- readline(prompt = "Do you want opponent score for all phases or for the tournamet? (phs/gen): ")
      score_input <- tolower(score_input)  # Convert input to lowercase
      
      if (score_input %in% c("phs", "gen")) {
        scoreAnsw <- (score_input == "phs")
        break  
      } else {
        cat("Invalid input. Please enter 'all' or 'gen'.\n")
      }
    }
  }else{
    scoreAnsw = FALSE
  }
  dataset <- rs$data %>% 
    filter(!Weight %in% c(0.5,0.775)) %>% 
    arrange(cat)
  
  datasetG <- rs$data %>% arrange(cat)
  datasetG <- split(datasetG, datasetG$cat)
  
  
  dft <- read.csv(file) %>% 
    filter(!str_detect(Group.Bracket, "Group"),
           !str_detect(Group.Bracket, "Power"),
           !str_detect(Division, "Mix")) %>% 
    arrange(Division)
  
  dataset$Round <- dft$Round
  
  nc <- length(unique(dataset$cat))
  dataset <- split(dataset, dataset$cat)
  full <- list()
  for(kk in 1:nc){
    x <- dataset[[kk]]
    teams <- unique(c(x$Play1, x$Play2))
    cont <- x
    cont <- separate(cont, Play1, c("g1","g2"), " & ")
    cont <- separate(cont, Play2, c("g3","g4"), " & ")
    cont$all <- with(cont, paste(g1,g2,g3,g4, sep = " - "))
    
    gs <- with(cont, unique(c(g1,g2,g3,g4)))
    np <- length(gs)

    cont$g1 <- match(cont$g1,gs)
    cont$g2 <- match(cont$g2,gs)
    cont$g3 <- match(cont$g3,gs)
    cont$g4 <- match(cont$g4,gs)
    cont <- cont %>% filter(!Weight %in% c(0.5,0.775))
    
    res <- data.frame(giocatore = gs,Round = character(np), Score = double(np), Team = character(np))
    
    for(i in 1:np){
      gamePl <- which(str_detect(cont$all, gs[i]))
      lastG <- cont[max(gamePl),]
      res[i,2] <- lastG$Round
      if(i %in% c(lastG$g1, lastG$g2)) Score <- 1*(lastG$Score>0.5) else Score <- 1*(lastG$Score<0.5)
      if(lastG$Weight == 0.35) Score <- Score-32
      res[i,3] <- Score
      res[i, 4] <- teams[str_detect(teams,gs[i])]
    }
    
    if(any(res$Score< (-1))){
        lowrres <- 
          res %>% 
          filter(Score<0) %>% 
          mutate(Placement = as.double(ifelse(is.na(str_extract(Round, "(?<=P)\\d{1,2}")),1,str_extract(Round, "(?<=P)\\d{1,2}")))+32,
                 Score = Score + 32) %>% 
          mutate(Round = paste("P", Placement,": ", ifelse(is.na(str_extract(Round, "(?<=: ).*")), Round, str_extract(Round, "(?<=: ).*")), sep = "")) %>% 
          select(-Placement)
        res <- rbind(filter(res, Score>-1), lowrres)
    }
    
    fres <- 
      res %>% 
      mutate(Placement = as.double(ifelse(is.na(str_extract(Round, "(?<=P)\\d{1,2}")),1,str_extract(Round, "(?<=P)\\d{1,2}")))) %>% 
      mutate(
        Rank=case_when(
          str_detect(Round, "Final") ~ Placement +(1-Score),
          str_detect(Round, "Semi-final") ~ Placement + 3*(1-Score),
          str_detect(Round, "Quarter-final") ~ Placement + 7*(1-Score),
          str_detect(Round, "Round of 16") ~ Placement + 15*(1-Score),
          str_detect(Round, "Round of 32") ~ Placement + 31*(1-Score)),
        LastGame = paste(ifelse(Score == 1, "Won", "Lost"),Round, sep = " ")) %>% 
      arrange(Rank) %>%
      filter(duplicated(Team)) %>% 
      select( Team, Rank, LastGame )
    
    hist <- rs$history
    rat <- rs$ratings
    
    gfres <- separate(fres,Team, sep = " & ", into = c("p1", "p2"))
    gfres$Team <- fres$Team
    
    if(oppScore){

      xg<- datasetG[[kk]]
      teams <- unique(c(xg$Play1, xg$Play2))
      group <- xg
      group <- separate(group, Play1, c("g1","g2"), " & ")
      group <- separate(group, Play2, c("g3","g4"), " & ")
      
      gsg <- with(group, unique(c(g1,g2,g3,g4)))
      npg <- length(gsg)
      
      group$g1 <- match(group$g1,gsg)
      group$g2 <- match(group$g2,gsg)
      group$g3 <- match(group$g3,gsg)
      group$g4 <- match(group$g4,gsg)
      white1 <- group$g1
      white2 <- group$g2
      black1 <- group$g3
      black2 <- group$g4
      l1t <- double(npg)
      crats <- rs$ratings$Rating[match(gsg, rs$ratings$giocatore)]
      names(crats) <- gsg
        
      nameteams <- data.frame(G1 = c(group$g1,group$g3),G2 = c(group$g2,group$g4))
      sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
      split_cols <- strsplit(sq, " ")
      g1 <- as.numeric(sapply(split_cols, `[`, 1))
      g2 <- as.numeric(sapply(split_cols, `[`, 2))
      
      compagni <- data.frame(
        g1 = c(gsg[g1],gsg[g2]),
        g2 = c(gsg[g2],gsg[g1])
      )
      compagni <- compagni[order(compagni$g1),]
        
        # Unique Team's Rating
        cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
        
        df_scores <- data.frame(
          giocatore = gsg,
          groupOpp = 0,
          groupPlayed = 1,
          bracketOpp = 0,
          bracketPlayed = 1,
          placementOpp = 0,
          placementPlayed = 1
        )
        for(nk in 1:nrow(group)){
          cur_weight <- group$Weight[nk]
          if(cur_weight %in% c(0.5,0.775)){
            df_scores[group$g1[nk],2] <- df_scores[group$g1[nk],2] + mean(c(crats[group$g3[nk]],crats[group$g4[nk]]))
            df_scores[group$g1[nk],3] <- df_scores[group$g1[nk],3] + 1
            df_scores[group$g2[nk],2] <- df_scores[group$g2[nk],2] + mean(c(crats[group$g3[nk]],crats[group$g4[nk]]))
            df_scores[group$g2[nk],3] <- df_scores[group$g2[nk],3] + 1
            df_scores[group$g3[nk],2] <- df_scores[group$g3[nk],2] + mean(c(crats[group$g1[nk]],crats[group$g2[nk]]))
            df_scores[group$g3[nk],3] <- df_scores[group$g3[nk],3] + 1
            df_scores[group$g4[nk],2] <- df_scores[group$g4[nk],2] + mean(c(crats[group$g1[nk]],crats[group$g2[nk]]))
            df_scores[group$g4[nk],3] <- df_scores[group$g4[nk],3] + 1
          }else if(cur_weight %in% c(1,0.75)){
            df_scores[group$g1[nk],4] <- df_scores[group$g1[nk],4] + mean(c(crats[group$g3[nk]],crats[group$g4[nk]]))
            df_scores[group$g1[nk],5] <- df_scores[group$g1[nk],5] + 1
            df_scores[group$g2[nk],4] <- df_scores[group$g2[nk],4] + mean(c(crats[group$g3[nk]],crats[group$g4[nk]]))
            df_scores[group$g2[nk],5] <- df_scores[group$g2[nk],5] + 1
            df_scores[group$g3[nk],4] <- df_scores[group$g3[nk],4] + mean(c(crats[group$g1[nk]],crats[group$g2[nk]]))
            df_scores[group$g3[nk],5] <- df_scores[group$g3[nk],5] + 1
            df_scores[group$g4[nk],4] <- df_scores[group$g4[nk],4] + mean(c(crats[group$g1[nk]],crats[group$g2[nk]]))
            df_scores[group$g4[nk],5] <- df_scores[group$g4[nk],5] + 1
          }else{
            df_scores[group$g1[nk],6] <- df_scores[group$g1[nk],6] + mean(c(crats[group$g3[nk]],crats[group$g4[nk]]))
            df_scores[group$g1[nk],7] <- df_scores[group$g1[nk],7] + 1
            df_scores[group$g2[nk],6] <- df_scores[group$g2[nk],6] + mean(c(crats[group$g3[nk]],crats[group$g4[nk]]))
            df_scores[group$g2[nk],7] <- df_scores[group$g2[nk],7] + 1
            df_scores[group$g3[nk],6] <- df_scores[group$g3[nk],6] + mean(c(crats[group$g1[nk]],crats[group$g2[nk]]))
            df_scores[group$g3[nk],7] <- df_scores[group$g3[nk],7] + 1
            df_scores[group$g4[nk],6] <- df_scores[group$g4[nk],6] + mean(c(crats[group$g1[nk]],crats[group$g2[nk]]))
            df_scores[group$g4[nk],7] <- df_scores[group$g4[nk],7] + 1
            
          }
        }
          df_avg <- data.frame(
            giocatore = gsg,
            groupOpp = ifelse(df_scores$groupPlayed>1, df_scores$groupOpp/(df_scores$groupPlayed-1), 0),
            bracketOpp = ifelse(df_scores$bracketPlayed>1,df_scores$bracketOpp/(df_scores$bracketPlayed-1), 0),
            placementOpp = ifelse(df_scores$placementPlayed>1, df_scores$placementOpp/(df_scores$placementPlayed-1), 0)
          )
          df_avg[,-1] <- round(df_avg[,-1])
          
        
        if(!scoreAnsw) df_avg$Opp <- round((df_scores$groupOpp+df_scores$bracketOpp+df_scores$placementOpp)/(df_scores$groupPlayed+df_scores$bracketPlayed+df_scores$placementPlayed-3))
        
}
    
    if(both){
      gfres$TotRat <- paste(rat$Rating[match(gfres$p1, rat$giocatore)], " & ",rat$Rating[match(gfres$p2, rat$giocatore)], sep  = "")
      gfres$GainedRat<- paste("(",hist$Rating[match(gfres$p1, rownames(hist))], " & " ,hist$Rating[match(gfres$p2, rownames(hist))],")", sep = "")
      
      if(oppScore & scoreAnsw){ 
        gfres <- merge(gfres, df_avg, by.x = "p1", by.y = "giocatore")
        full[[kk]] <- gfres[order(gfres$Rank),c("Team", "TotRat", "GainedRat", "LastGame","groupOpp", "bracketOpp", "placementOpp", "Rank")]
      }else if(oppScore & !scoreAnsw){
        gfres <- merge(gfres, df_avg, by.x = "p1", by.y = "giocatore")
        full[[kk]] <- gfres[order(gfres$Rank),c("Team", "TotRat", "GainedRat", "Opp","LastGame", "Rank")]
      }else{
        full[[kk]] <- gfres[order(gfres$Rank),c("Team", "TotRat","GainedRat", "LastGame", "Rank")]
      }
      
    }else{
      gfres$TotRat <- round((rat$Rating[match(gfres$p1, rat$giocatore)]+rat$Rating[match(gfres$p2, rat$giocatore)])/2)
      gfres$GainedRat<- paste("(",round((hist$Rating[match(gfres$p1, rownames(hist))]+hist$Rating[match(gfres$p2, rownames(hist))])/2), ")", sep = "")

        if(oppScore & scoreAnsw){ 
          gfres <- merge(gfres, df_avg, by.x = "p1", by.y = "giocatore")
          full[[kk]] <- gfres[order(gfres$Rank),c("Team", "TotRat", "GainedRat", "LastGame","groupOpp", "bracketOpp", "placementOpp", "Rank")]
        }else if(oppScore & !scoreAnsw){
          gfres <- merge(gfres, df_avg, by.x = "p1", by.y = "giocatore")
          full[[kk]] <- gfres[order(gfres$Rank),c("Team", "TotRat", "GainedRat","Opp", "LastGame", "Rank")]
        }else{
          full[[kk]] <- gfres[order(gfres$Rank),c("Team", "TotRat","GainedRat", "LastGame", "Rank")]
        }
    }
  }

  names(full) <- names(dataset)
  return(full)
}

search_match <- function(athlete, person_to_look_for, arch) {
  athlete_result <- arch[[athlete]]
  output <- list()
  
  for (tournament in names(athlete_result)) {
    df <- athlete_result[[tournament]]
    truerows <- apply(df, 1, function(x) str_detect(paste0(x, collapse = " "),person_to_look_for))
    rows <- df[truerows,]
    
    if (nrow(rows) > 0) {
      output[[tournament]] <- rows
    }
  }
  
  return(output)
}


histry_merge <- function(list, giocatori){
  giocatori = sort(unique(giocatori))
  ng = length(giocatori)
  nt = ncol(sapply(list,dim))
  
  histry =
    array(NA,
          dim = c(ng,nt+1, 6),
          dimnames = list(giocatori,1:(nt+1),
                          c("Rating", "Deviation","Win", "Draw", "Loss", "W-D-L")))
  
  
  playfilt = list[[nt]]$giocatore[list[[nt]]$giocatore %in% giocatori]
  playfilt = playfilt[order(playfilt)]
  
  for(i in 2:(nt+1)){
    dflisti = as.data.frame(list[[i-1]])
    

    playmatch = na.omit(match(dflisti$giocatore,giocatori))
    histry[, i, 1][playmatch] <- dflisti$Rating[dflisti$giocatore %in% giocatori]
    histry[, i, 2][playmatch] <- dflisti$Deviation[dflisti$giocatore %in% giocatori]
    histry[, i, 3][playmatch] <- dflisti$Win[dflisti$giocatore %in% giocatori]
    histry[, i, 4][playmatch] <- dflisti$Draw[dflisti$giocatore %in% giocatori]
    histry[, i, 5][playmatch] <- dflisti$Loss[dflisti$giocatore %in% giocatori]
    
  }
  
  histry[,,"Rating"] = ifelse(is.na(histry[,,"Rating"]),1350.7654,histry[,,"Rating"])
  histry[,,"Win"] = ifelse(is.na(histry[,,"Win"]),0,histry[,,"Win"])
  histry[,,"Draw"] = ifelse(is.na(histry[,,"Draw"]),0,histry[,,"Draw"])
  histry[,,"Loss"] = ifelse(is.na(histry[,,"Loss"]),0,histry[,,"Loss"])

  # for(j in 2:(nt+1)){
  #   histry[,j,6] <- paste0(as.numeric(histry[,j,3]) - as.numeric(histry[,j-1,3]), "-",
  #                          as.numeric(histry[,j,4]) - as.numeric(histry[,j-1,4]), "-",
  #                          as.numeric(histry[,j,5]) - as.numeric(histry[,j-1,5]))
  #   histry[,j,6] <- ifelse( histry[,j,6] %in% c("NA-NA-NA", "0-0-0"), NA,  histry[,j,6])
  # }

  # histryfilt =
  #   array(NA,
  #         dim = c(length(playfilt),nt+1, 3),
  #         dimnames = list(playfilt,1:(nt+1),
  #                         c("Rating", "Deviation","Games")))
  # 
  # histryfilt[,,"Rating"] = histry[,,"Rating"][match(playfilt,rownames(histry[,,"Rating"])),]
  # 
  # histryfilt[,,"Deviation"] = histry[,,"Deviation"][match(playfilt,rownames(histry[,,"Deviation"])),]
  # 
  # histryfilt[,,"Games"] = histry[,,"Games"][match(playfilt,rownames(histry[,,"Games"])),]
  # 
  # 
  # histryfilt[,,"Rating"] <- as.numeric(histryfilt[,,"Rating"])
  
  return(histry)
  
}


opp_rat <- function(numplay, numrow, white1, white2, black1, black2, score, cdevsteams, cratsteams,dscore = double(2*numplay),weights,bval) { 

  l1t <- numeric(numplay)
  avg_opp <- numeric(numplay)
  
  # Create Empty Vectors
  for (k in 1:numplay) {
    l1t[k] <- 0
    avg_opp[k] <- 0
  }
  
  for (k in 1:numrow) {
    # sum of cumulative diffs when win
    l1t[white1[k]] <- l1t[white1[k]] + (cratsteams[black1[k]] - cratsteams[white1[k]]) * score
    l1t[white2[k]] <- l1t[white2[k]] + (cratsteams[black1[k]] - cratsteams[white1[k]]) * score
  
    avg_opp[white1[k]] <- avg_opp[white1[k]] + cratsteams[black1[k]] * score
    avg_opp[white2[k]] <- avg_opp[white2[k]] + cratsteams[black1[k]] * score
    
    # r-r_j
    l1t[black1[k]] <- l1t[black1[k]] + (cratsteams[white1[k]] - cratsteams[black1[k]]) * (1 - score)
    l1t[black2[k]] <- l1t[black2[k]] + (cratsteams[white1[k]] - cratsteams[black1[k]]) * (1 - score)
    
    avg_opp[black1[k]] <- avg_opp[black1[k]] + cratsteams[white1[k]] * (1 - score)
    avg_opp[black2[k]] <- avg_opp[black2[k]] + cratsteams[white1[k]] * (1 - score)
    
  }
  
  
  lout <- list(l1t,avg_opp)
  return(lout)
}


snake_seeding <- function(n, m) {
  if (n %% m != 0) {
    stop("Number of teams must be divisible by number of groups")
  }
  group_size <- n / m
  teams <- 1:n
  groups <- split(teams, rep(1:group_size, each = m))
  for (i in seq_along(groups)) {
    if (i %% 2 == 0) {
      groups[[i]] <- rev(groups[[i]])
    }
  }
  seeding <- unlist(groups)
  names(seeding) <- NULL
  return(seeding)
}


classifica <- function(rs,
                       role = c("classifica", "players"),
                       cols,
                       sel.pl) {
  
  d <- rs$data %>%
    mutate(across(
      contains("Play"),
      ~ stringi::stri_trans_general(str_to_title(.), "Latin-ASCII")
    ))
  
  
  g <- unique(c(d$Play1, d$Play2))
  
  
  ddi <- rs$data[, c("cat", "Time", "Play2", "Play1", "Score", "Weight")] %>%
    mutate(across(
      contains("Play"),
      ~ stringi::stri_trans_general(str_to_title(.), "Latin-ASCII")
    )) %>%
    rename(Play2 = Play1, Play1 = Play2)
  ddi$Score <- 1 - ddi$Score
  
  dd <- rbind(rs$data %>%
                mutate(across(
                  contains("Play"),
                  ~ stringi::stri_trans_general(str_to_title(.), "Latin-ASCII")
                )), ddi)
  
  dd <- separate(dd, Play2, into = c("o1", "o2"), sep = " & ")
  
  dd <- split(dd, dd$cat)
  
  nd <- length(dd)
  oppList <- vector("list", nd)
  names(oppList) <- names(dd)
  dpl <- lapply(dd, function(y)
    unique(c(y$Play1)))
  
  weight.val <- list(
    c(.5, .775, .4, 0.75 - 0.001, .65 , .7),
    c(1, .35, 1 - 0.002, .45, .8 - 0.001, 1 - 0.001, .75),
    c(.25, .25 - .001, .25 - .002, .35 - .001, .65 - .001, .6)
  )
  
  if (role == "classifica") {
    for (i in 1:nd) {
      g <- dpl[[i]]
      curdd <- dd[[i]]
      oppScore <- data.frame(
        "player" = g,
        "GroupOpp" = 0,
        "BracketOpp" = 0,
        "PlacementOpp" = 0
      )
      
      rownames(oppScore) <- g
      for (pl in g) {
        wh <- which(str_detect(curdd$Play1, pl))
        ddwh <- curdd[wh, ] %>%
          mutate(
            d1 = rs$ratings$Deviation[match(o1, rs$ratings$giocatore)],
            d2 = rs$ratings$Deviation[match(o2, rs$ratings$giocatore)],
            p1 = rs$ratings$Rating[match(o1, rs$ratings$giocatore)],
            p2 = rs$ratings$Rating[match(o2, rs$ratings$giocatore)]
          ) %>%
          rowwise() %>%
          mutate(avg = mean(c(p1, p2)), dev = sqrt((d1^2 + d2^2) / 2), ) %>% ungroup %>%
          rename(w = Weight)
        
        
        oppScore[pl, 2] <-
          ddwh %>%
          filter(w %in% weight.val[[1]]) %>%
          summarize(m = paste0(round(mean(avg)), " (", round(mean(dev)), ") - ", n(), " Games")) %>%
          pull(m)
        
        oppScore[pl, 3] <-
          ddwh %>%
          filter(w %in% weight.val[[2]]) %>%
          summarize(m = paste0(round(mean(avg)), " (", round(mean(dev)),  ") - ", n(), " Games")) %>%
          pull(m)
        
        oppScore[pl,4] <-
          ddwh %>%
          filter(w %in% weight.val[[3]]) %>%
          summarize(m = paste0(round(mean(avg)), " (", round(mean(dev)), ") - ", n(), " Games")) %>%
          pull(m)
        
        oppScore <-
          oppScore %>%
          mutate(across(contains("Opp"),~ifelse(str_detect(., "NaN"), "-", .) ))
      }
      rownames(oppScore) <- NULL
      oppList[[i]] <- oppScore
    }
    return(oppList)
  } else{
    dd <- do.call(rbind, dd)
    wh <- which(str_detect(dd$Play1, sel.pl))
    out <- dd[wh, ] %>%
      mutate(
        d1 = rs$ratings$Deviation[match(o1, rs$ratings$giocatore)],
        d2 = rs$ratings$Deviation[match(o2, rs$ratings$giocatore)],
        p1 = rs$ratings$Rating[match(o1, rs$ratings$giocatore)],
        p2 = rs$ratings$Rating[match(o2, rs$ratings$giocatore)]
      ) %>%
      rowwise() %>%
      mutate(avg = mean(c(p1, p2)), dev = sqrt((d1^2 + d2^2) / 2), ) %>% ungroup %>%
      rename(w = Weight) %>% 
      filter(w %in% weight.val[[cols]]) %>%
      transmute(
        Opponent1 = paste0(o1, " - ", p1, " (", d1, ")"),
        Opponent2 = paste0(o2, " - ", p2, " (", d2, ")"),
        Score = score_char(Score, w, "white")
      )
    return(out)
    
  }
  
}

make_groups <- function(mat, rat, ngroup = 2, pergroup = NULL, list = NULL, Adj = FALSE, View = FALSE){
  if(!is.null(list)) mat <- matrix(list, ncol = 2, byrow = T)
  if(!is.null(pergroup)) ngroup <- floor(nrow(mat)/pergroup) + (nrow(mat)%%pergroup)
  # pergroup <- floor(nrow(mat)/ngroup)+ (nrow(mat)%%ngroup)
  ratings <- rat$ratings
  
  df <- data.frame(
    Play = str_c(mat[,1], mat[,2], sep = " & "),
    Rat1 = ratings$Rating[match(mat[,1], ratings$giocatore)],
    Rat2 = ratings$Rating[match(mat[,2], ratings$giocatore)]
  )
  
  df$SumRat <- with(df,ifelse(is.na(Rat1),0,Rat1)+ifelse(is.na(Rat2),0,Rat2))
  df$AdjSumRat <- with(df,ifelse(is.na(Rat1),1000,Rat1)+ifelse(is.na(Rat2),1000,Rat2))
  
  if(Adj) df$Rank <- rank(-df$AdjSumRat) else df$Rank <- rank(-df$SumRat)
  df <- arrange(df,Rank)
  
  range <- 0:10
  cl.int.range <- (nrow(mat)+range)%%ngroup
  cl.int <- range[cl.int.range==0][which.min(abs(range[cl.int.range==0]))]
  Plays <- c(paste(df$Play, " (", df$SumRat, ")", sep = ""), rep(NA, cl.int))
  zigzag <- snake_seeding(nrow(mat)+cl.int, ngroup)
  
  df.groups <- data.frame(matrix(Plays[zigzag], ncol = ngroup, byrow = T))
  colnames(df.groups) <- paste("Group", 1:ngroup)
  
  if(View) View(df.groups) else return(df.groups)
  
}


summary_odds <- function(Player, type = c("Best Upset", "Best Bracket Upset", "Worst Lost", "Worst Bracket Lost", "Highest Rated Win", "Highest Rated Match"), perch){

  if("Best Upset" %in% type){
  BestUpset <- subset(perch[[Player]], Result==1)[which.min(subset(perch[[Player]], Result == 1)$PreGameOdds),]
  if(nrow(BestUpset)>0) BestUpset <- cbind(Type = "Best Upset",BestUpset)
  }else{BestUpset <-  NULL}
  
  if("Best Bracket Upset" %in% type){
  BestBracketUpset <-  subset(perch[[Player]], Result==1 & Weight == 1)[which.min(subset(perch[[Player]], Result==1 & Weight == 1)$PreGameOdds),]
  if(nrow(BestBracketUpset)>0) BestBracketUpset <- cbind(Type ="Best Bracket Upset",BestBracketUpset)
  }else{BestBracketUpset <-  NULL}
  
  if("Worst Lost" %in% type){
  WorstLost <- subset(perch[[Player]], Result==0)[which.max(subset(perch[[Player]], Result == 0)$PreGameOdds),]
  if(nrow(WorstLost)>0) WorstLost <-  cbind(Type ="Worst Lost",WorstLost)
  }else{WorstLost <-  NULL}
  
  if("Worst Bracket Lost" %in% type){
  WorstBracketLost <-  subset(perch[[Player]], Result==0& Weight == 1)[which.max(subset(perch[[Player]], Result == 0& Weight == 1)$PreGameOdds),]
  if(nrow(WorstBracketLost)>0) WorstBracketLost <- cbind(Type = "Worst Bracket Lost",WorstBracketLost)
  }else{WorstBracketLost <- NULL}
  
  if("Highest Rated Win" %in% type){
  HighestRatWin <- subset(perch[[Player]], Result==1)[which.max(subset(perch[[Player]], Result == 1)$oppRat),]
  if(nrow(HighestRatWin)>0) HighestRatWin <- cbind(Type = "Highest Rated Win ",HighestRatWin)
  }else{HighestRatWin <- NULL}
  
  if("Highest Rated Match" %in% type){
    HighestRatMat <- perch[[Player]][which.max(perch[[Player]]$oppRat),] 
    if(nrow(HighestRatMat)>0) HighestRatMat <- cbind(Type = "Highest Rated Match",HighestRatMat)
  }else{HighestRatMat <- NULL}
  
    merged_list <- 
      rbind(BestUpset, BestBracketUpset, WorstLost, WorstBracketLost, HighestRatWin, HighestRatMat)
  
    merged_list$PreGameOdds <- paste(round(merged_list$PreGameOdds,3)*100, "%", sep = "")
    
    return(merged_list)
}

summary_opponents <- function(Player, perch = perch, nmin = 2){
  pp <- perch[[Player]]
  pp <- tidyr::separate(pp, opp1, into = c("o1", "o2"), " & " )
  pp$o1 <- str_remove(str_remove(pp$o1, "\\("), "\\)")
  pp$o2 <- str_remove(str_remove(pp$o2, "\\("), "\\)")
  o <- str_remove(str_remove(unique(c(pp$o1, pp$o2)), "\\("), "\\)")
  pp.df <- data.frame(Giocatore = NULL, PartiteContro = NULL, Win = NULL, Draw = NULL, Loss = NULL, LastTour = NULL, LastRes = NULL)
  for(opp in o){
    cur.df <- pp[str_detect(paste(pp$o1, pp$o2, sep = " "),opp) ,]
    n <- nrow(cur.df)
    win = sum(cur.df$Result>0.5)
    draw = sum(cur.df$Result==0.5)
    loss = sum(cur.df$Result<0.5)
    lastT <- cur.df$Tournament[n]
    lastR <- cur.df$Score[n]
    pp.df <- rbind(pp.df, data.frame(Giocatore = opp, PartiteContro = n, Win = win, Draw = draw, Loss = loss, LastTour = lastT, LastRes = lastR))
  }
  
  pp.df <- pp.df[order(-pp.df$PartiteContro),]
  pp.df <- pp.df[pp.df$PartiteContro>nmin,]
  return(pp.df)
}

find_matchups <- function(Player, Opponent, perch, arch){
  
  
  perch_stat <- perch[[Player]][str_detect(perch[[Player]][,2], Opponent),c("opp1", "oppRat", "partn", "ownRat", "PreGameOdds", "Score", "Tournament")]
  names(perch_stat) <- c("Opponents",  "Opponent Rating", "Partner", "Team Rating","Pre-Game Odds", "Result", "Tournament")
  perch_stat$`Pre-Game Odds` <- paste(round(perch_stat$`Pre-Game Odds`,3)*100, "%", sep = "")
  full_points <- NULL
  for(i in unique(perch_stat[,"Tournament"])){
    cur_tourn <- arch[[Player]][[i]]
    opps <- paste(cur_tourn[,1], cur_tourn[,2], sep = " ")
    points <- as.numeric(cur_tourn[str_detect(opps, Opponent),4])
    full_points <- c(full_points, points)
  }
  perch_stat$Punti <- full_points

  return(perch_stat)
  
}

glicko_graphs <- function(player, second.player = NULL, ...) {
  nn <- list(...)
  arch <- nn$arch
  ratinghistr <- nn$ratinghistr
  eug <- nn$eug
  dft <- nn$dft 
  
  if ("nom" %in% names(nn)) nom <- nn$nom else nom <- 0.2 
  if ("size" %in% names(nn)) size <- nn$size else size <- 1
  if ("packages" %in% names(nn)) packages <- nn$packages else packages <- FALSE
  if ("nation" %in% names(nn)) nation <- nn$nation else nation <- TRUE
  
  
  
  plnat <- eug %>% filter(giocatore == player) %>% pull(Nazione)
  
  
  if(is.na(plnat) || plnat == "0" || length(plnat) == 0 | !nation){  
    natpl <- NULL
    nt <- 1
  } else {
    natpl <- eug %>% filter(Nazione == plnat) %>% pull(giocatore)
    nt <- length(natpl)
  }  
  
  
  p <- 
    ratinghistr %>%
    as_tibble() %>%
    rename_with(~paste0("tourn", .x), .cols = where(is.double)) %>%
    mutate(name = rownames(ratinghistr)) %>%
    pivot_longer(cols = starts_with("tourn"), names_to = "tourn", values_to = "Rating") %>%
    filter(name %in% c(natpl, player, second.player)) %>% 
    transmute(
      name = name,
      Rating = Rating,
      Tournament = str_replace(tourn, "tourn", ""),
      Date = as.Date(dft$date[match(Tournament, dft$TourName)])
    )
  
  
  tk <- unique(unlist(lapply(c(player, second.player), function(p) {
    if (!is.null(p) && p %in% names(arch)) names(arch[[p]]) else NULL
  })))
  
  
  aa <- arch[[player]]
  aa$Initialisation <- "ciao"
  list_of_aa = names(aa)
  if (second.player != "") {
    bb <- arch[[second.player]]
    bb$Initialisation <- "ciao"
    list_of_bb <- list(names(bb))  
  }  
  
  if(names(aa)[1] != dft$TourName[1])  p <- p[-1,]
  
  p_highlight <-
   p %>%
    filter(name %in% c(player, second.player)) %>%
    arrange(name) %>%
    group_by(name) %>%
    mutate(
      Tournament = case_when(
        is.na(lag(Rating)) & !is.na(Rating) ~ "Initialisation",
        Tournament == "V1" ~ "Initialisation",
        TRUE~Tournament),
      Date = coalesce(Date, as.Date("2023-01-01")),
      delta = ifelse(Tournament=="Initialisation", Rating, Rating - lag(Rating)),
      delta_str = ifelse(is.na(delta), "NA", 
                         ifelse(delta >= 0, paste0("+", round(delta, 1)), round(delta, 1)))
    ) %>%
    # select(-delta) %>% 
    filter(!duplicated(Tournament,fromLast = TRUE )) %>%
    ungroup() 
  
  p_highlight_1 <- 
    p_highlight %>% 
    filter(name == player) %>% 
    rowwise() %>%
    mutate(
      delta_str = {
        a <- aa[[Tournament]]
        if(!is.null(a) & Tournament != "Initialisation"){
          paste0(delta_str, " (", a$Score[nrow(a)+(ncol(a)-5)], ")")
        }else{
          delta_str
        }
      },
      partner = {
        a <- aa[[Tournament]]
        if(!is.null(a) & Tournament != "Initialisation"){
          if(ncol(a)==5){
            ifelse(
              length(unique(a$Partner))==1,
              unique(a$Partner),
              "Various"
            )
          }else{
             a[nrow(a)-1,2]
          }
        }else{
        ""
      }
      },
      tour_played = {
        matching_tours <- intersect(Tournament, list_of_aa)
        if (length(matching_tours) > 0) {
          paste(matching_tours, collapse = ", ")
        } else {
          ""
        }
      },
      colorefil = case_when(
        name == player ~ "indianred",
        name == second.player ~ "blue"
      )) %>% 
    mutate(
      tour_played = ifelse(is.na(tour_played) | !(tour_played %in% tk), NA_character_, tour_played),
      alphafil = "a",
      tooltip = paste0(
        Rating,
        "<br> Partner: ", partner,
        "<br> Rating: ", delta_str,
        "<br> ", tour_played
      )
    ) %>%
    ungroup() %>% 
    group_by(name, Date) %>%
    slice_max(order_by = abs(delta), n = 1, with_ties = T)
  
  if(second.player != ""){
    
    p_highlight_2 <- 
      p_highlight %>% 
      filter(name == second.player) %>% 
      rowwise() %>%
      mutate(
        delta_str = {
          b<- bb[[Tournament]]
          if(!is.null(b) & Tournament != "Initialisation"){
            paste0(delta_str, " (", b$Score[nrow(b)+(ncol(b)-5)], ")")
          }else{
            delta_str
          }
        },
        partner = {
          b <- bb[[Tournament]]
          if(!is.null(b) & Tournament != "Initialisation"){
            if(ncol(b)==5){
              ifelse(
                length(unique(b$Partner))==1,
                unique(b$Partner),
                "Various"
              )
            }else{
              b[nrow(b)-1,2]
            }
          }else{
            ""
          }
        },
        tour_played = {
          matching_tours <- intersect(Tournament, list_of_bb)
          if (length(matching_tours) > 0) {
            paste(matching_tours, collapse = ", ")
          } else {
            ""
          }
        },
        colorefil = case_when(
          name == player ~ "indianred",
          name == second.player ~ "blue"
        )) %>% 
      mutate(
        tour_played = ifelse(is.na(tour_played) | !(tour_played %in% tk), NA_character_, tour_played),
        alphafil = "a",
        tooltip = paste0(
          Rating,
          "<br> Partner: ", partner,
          "<br> Rating: ", delta_str,
          "<br> ", tour_played
        )
      ) %>%
      ungroup() %>% 
      group_by(name, Date) %>%
      slice_max(order_by = abs(delta), n = 1, with_ties = T)
    
    p_highlight <- rbind(p_highlight_1, p_highlight_2)
  }else{
    p_highlight <- p_highlight_1
    
  }
  
  
  
  p_background <- p %>%
    filter(!(name %in% c(player, second.player))) %>%
    mutate(
      colorefil = "grey20",
      alphafil = "b",
      tooltip = NA  # No tooltip
    )
  

  
  # Combine for full data
  
  
  title <- paste0(
    "<span style='color:indianred;'>", player, "</span>",
    "<span style='color:blue;'> ", second.player %||% "", "</span>"
  )
  
  # ggplot
  g <- ggplot() 
  
   if(nrow(p_background)> 0){
     g <-g+
       geom_line(data = p_background, 
              aes(x = Date, y = Rating, 
                  group = name, color = "grey80", alpha = alphafil), linewidth = size, show.legend = FALSE) 
  }
    
  g <- 
    g+  
    geom_line(data = p_highlight, 
              aes(x = Date, y = Rating,
                  group = name, color = colorefil, alpha = alphafil, text = tooltip), linewidth = size, show.legend = FALSE) +
    geom_point(data = p_highlight %>% filter(!is.na(tour_played)), 
               aes(x = Date, y = Rating,
                   group = name, color = colorefil, alpha = alphafil, text = tooltip) , show.legend = FALSE)+
    
    scale_color_identity() +
    scale_alpha_manual(values = c(a = 1, b = nom / log(nt))) +
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
    theme(
      plot.title = element_markdown(size = 16, hjust = 0, family = "Fira Sans Condensed"),
      plot.background = element_rect(fill = "grey10"),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey30", size = 0.2),
      panel.grid.minor = element_line(color = "grey30", size = 0.2),
      axis.ticks = element_blank(),
      legend.position = "none"
    ) +
    ggtitle(title)
  
  
    minR <- min(p_highlight$Rating)
    pdates <- as.Date(c("2025-3-01", "2025-5-1", "2025-7-1"))
  if(packages){
    g <- g+
      geom_vline(xintercept = as.numeric(pdates), col = "red", linetype = "dotted")+
      annotate(
        "text",
        x = pdates+10,
        y = minR, # top of the plot
        label = c("A", "B", "C"),
        vjust = 0,  # slightly above the top
        color = "red",
        size = 3
      )
      
  }
  
  
  # Convert to interactive Plotly
  ggplotly(g, tooltip = "text") %>%
    layout(
      plot_bgcolor = "#1a1a1a",
      paper_bgcolor = "#1a1a1a",
      font = list(color = "#ffffff"),
      hoverlabel = list(bgcolor = "#222", font = list(color = "white"))
    )
}



score_char <- function(score, weights, which = c("black","white")){
  if(which == "white"){
    sc <- dplyr::case_when(
      score == 1 & weights %in% c(.5)~ "Group Win (1:0)",
      score == .5 & weights %in% c(.5)~ "Group Draw (1:1)",
      score == 1 & weights %in% c(.775)~ "Group Win (2:0)",
      score == 0 & weights%in% c(.5)~ "Group Lost (0:1)",
      score == 0 & weights%in% c(.775)~ "Group Lost (0:2)",
      score == .5 & weights%in% c(.775)~ "Group Draw (1:1)",
      score == 1 & weights %in% c(1)~ "Bracket Win (2:0)",
      score == .75 & weights %in% c(1)~ "Bracket Win (2:1)",
      score == .5 & weights %in% c(1)~ "Draw (1:1)",
      score == .25 & weights %in% c(1)~ "Bracket Lost (1:2)",
      score == 0 & weights %in% c(1)~ "Bracket Lost (0:2)",
      score == 1 & weights %in% c(.75)~ "Third Place Win (2:0)",
      score == .75 & weights %in% c(.75)~ "Third Place Win (2:1)",
      score == .5 & weights %in% c(.75)~ "Third Place Draw (1:1)",
      score == .25 & weights %in% c(.75)~ "Third Place Lost (1:2)",
      score == 0 & weights %in% c(.75)~ "Third Place Lost (0:2)",
      score == 1 & weights %in% c(0.25,.6)~ "Placement Win (1:0)",
      score == .75 & weights %in% c(0.25,.6)~ "Placement Win (2:1)",
      score == .25 & weights %in% c(0.25,.6)~ "Placement Lost (1:2)",
      score == 0 & weights %in% c(0.25,.6) ~ "Placement Lost (0:1)",
      score == 1 & weights %in% c(0.35)~ "Lower Bracket Win (2:0)",
      score == .75 & weights %in% c(0.35)~ "Lower Bracket Win (2:1)",
      score == .25 & weights %in% c(0.35)~ "Lower Bracket Lost (1:2)",
      score == 0 & weights %in% c(0.35)~ "Lower Bracket Lost (1:2)",
      score == 1 & weights %in% c(0.4)~ "World's Group Win (1:0)",
      score == 0 & weights %in% c(0.4)~ "World's Group Lost (0:1)",
      score == 1 & weights %in% c(0.75-0.001)~ "World's Second Group Win (1:0)",
      score == 0 & weights %in% c(0.75-0.001)~ "World's Second Group Lost (0:1)",
      score == 1 & weights %in% c(1-0.001)~ "World's Gold Bracket Win (2:0)",
      score == .75 & weights %in% c(1-0.001)~ "World's Gold Bracket Win (2:1)",
      score == .25 & weights %in% c(1-0.001)~ "World's Gold Bracket Lost (1:2)",
      score == 0 & weights %in% c(1-0.001)~ "World's Gold Bracket Lost (0:2)",
      score == 1 & weights %in% c(0.65-0.001)~ "World's Gold Placement Win (1:0)",
      score == 0 & weights %in% c(0.65-0.001)~ "World's Gold Placement Lost (0:1)",
      score == 1 & weights %in% c(1-0.002)~ "World's Silver Bracket Win (2:0)",
      score == .75 & weights %in% c(1-0.002)~ "World's Silver Bracket Win (2:1)",
      score == .25 & weights %in% c(1-0.002)~ "World's Silver Bracket Lost (1:2)",
      score == 0 & weights %in% c(1-0.002)~ "World's Silver Bracket Lost (0:2)",
      score == 1 & weights %in% c(0.35-0.001)~ "World's Silver Placement Win (1:0)",
      score == 0 & weights %in% c(0.35-0.001)~ "World's Silver Placement Lost (0:1)",
      score == 1 & weights %in% c(.8-0.001)~ "World's Bronze Bracket Win (2:0)",
      score == .75 & weights %in% c(.8-0.001)~ "World's Bronze Bracket Win (2:1)",
      score == .25 & weights %in% c(.8-0.001)~ "World's Bronze Bracket Lost (1:2)",
      score == 0 & weights %in% c(.8-0.001)~ "World's Bronze Bracket Lost (0:2)",
      score == 1 & weights %in% c(0.25-0.001)~ "World's Bronze Placement Win (1:0)",
      score == 0 & weights %in% c(0.25-0.001)~ "World's Bronze Placement Lost (0:1)",
      score == 0 & weights %in% c(0.45,.65) ~ "Positioning Game Lost (0:1)",
      score == 1 & weights %in% c(0.45,.65) ~ "Positioning Game Win (1:0)",
      score == 0 & weights %in% c(0.7) ~ "Bundesliga Lost (0:2)",
      score == 1 & weights %in% c(0.7) ~ "Bundesliga Win (2:0)",
      score == .5 & weights %in% c(0.7) ~ "Bundesliga Draw (1:1)",
      TRUE ~"Game Not Identified"
    )
  }else{
    sc <- dplyr::case_when(
      score == 1 & weights %in% c(0.7) ~ "Bundesliga Lost (0:2)",
      score == 0 & weights %in% c(0.7) ~ "Bundesliga Win (2:0)",
      score == .5 & weights %in% c(0.7) ~ "Bundesliga Draw (1:1)",
      score == 1 & weights %in% c(.5)~ "Group Lost (0:1)",
      score == 1 & weights %in% c(.775)~ "Group Lost (0:2)",
      score == 0 & weights%in% c(.5)~ "Group Win (1:0)",
      score == 0 & weights%in% c(.775)~ "Group Win (2:0)",
      score == .5 & weights%in% c(.775)~ "Group Draw (1:1)",
      score == 0 & weights %in% c(1)~ "Bracket Win (2:0)",
      score == 1 & weights %in% c(1)~ "Bracket Lost (0:2)",
      score == .75 & weights %in% c(1)~ "Bracket Lost (1:2)",
      score == .25 & weights %in% c(1)~ "Bracket Win (2:1)",
      score == 0 & weights %in% c(.75)~ "Third Place Win (2:0)",
      score == .25 & weights %in% c(.75)~ "Third Place Win (2:1)",
      score == .5 & weights %in% c(.75)~ "Third Place Draw (1:1)",
      score == .75 & weights %in% c(.75)~ "Third Place Lost (1:2)",
      score == 1 & weights %in% c(.75)~ "Third Place Lost (0:2)",
      score == 0 & weights %in% c(0.25,.6)~ "Placement Win (1:0)",
      score == 0 & weights %in% c(0.25,.6)~ "Placement Win (1:0)",
      score == .5 & weights %in% c(1,.75)~ "Draw (1:1)",
      score == .25 & weights %in% c(0.25,.6)~ "Placement Win (2:1)",
      score == .75 & weights %in% c(0.25,.6)~ "Placement Lost (1:2)",
      score == 1 & weights %in% c(0.25,.6) ~ "Placement Lost (0:1)",
      score == 0 & weights %in% c(0.35)~ "Lower Bracket Win (2:0)",
      score == .25 & weights %in% c(0.35)~ "Lower Bracket Win (2:1)",
      score == .75 & weights %in% c(0.35)~ "Lower Bracket Lost (1:2)",
      score == 1 & weights %in% c(0.35) ~ "Lower Bracket Lost (0:1)",
      score == 1 & weights %in% c(0.4)~ "World's Group Lost (0:1)",
      score == 0 & weights %in% c(0.4)~ "World's Group Win (1:0)",
      score == 1 & weights %in% c(0.75-0.001)~ "World's Second Group Lost (0:1)",
      score == 0 & weights %in% c(0.75-0.001)~ "World's Second Group Win (1:0)",
      score == 0 & weights %in% c(1-0.001)~ "World's Gold Bracket Win (2:0)",
      score == .25 & weights %in% c(1-0.001)~ "World's Gold Bracket Win (2:1)",
      score == .75 & weights %in% c(1-0.001)~ "World's Gold Bracket Lost (1:2)",
      score == 1 & weights %in% c(1-0.001)~ "World's Gold Bracket Lost (0:2)",
      score == 0 & weights %in% c(0.65-0.001)~ "World's Gold Placement Win (1:0)",
      score == 1 & weights %in% c(0.65-0.001)~ "World's Gold Placement Lost (0:1)",
      score == 0 & weights %in% c(1-0.002)~ "World's Silver Bracket Win (2:0)",
      score == .25 & weights %in% c(1-0.002)~ "World's Silver Bracket Win (2:1)",
      score == .75 & weights %in% c(1-0.002)~ "World's Silver Bracket Lost (1:2)",
      score == 1 & weights %in% c(1-0.002)~ "World's Silver Bracket Lost (0:2)",
      score == 0 & weights %in% c(0.35-0.001)~ "World's Silver Placement Win (1:0)",
      score == 1 & weights %in% c(0.35-0.001)~ "World's Silver Placement Lost (0:1)",
      score == 0 & weights %in% c(.8-0.001)~ "World's Bronze Bracket Win (2:0)",
      score == .25 & weights %in% c(.8-0.001)~ "World's Bronze Bracket Win (2:1)",
      score == .75 & weights %in% c(.8-0.001)~ "World's Bronze Bracket Lost (1:2)",
      score == 1 & weights %in% c(.8-0.001)~ "World's Bronze Bracket Lost (0:2)",
      score == 0 & weights %in% c(0.25-0.001)~ "World's Bronze Placement Win (1:0)",
      score == 1 & weights %in% c(0.25-0.001)~ "World's Bronze Placement Lost (0:1)",
      score == 1 & weights %in% c(0.45,.65) ~ "Positioning Game Lost (0:1)",
      score == 0 & weights %in% c(0.45,.65) ~ "Positioning Game Win (1:0)",
      TRUE ~"Game Not Identified"
    )
  }
  return(sc)
  
}
