# Import Data ####
weeks_passed <- function(year, month, day, date = NULL, r = 1) {
  if(is.null(date)) input_date <- as.Date(paste(year, month, day, sep = "-")) else input_date <- date
  reference_date <- as.Date("2023-04-15")
  weeks <- round(as.numeric(difftime(input_date, reference_date, units = "weeks")),r)
  return(weeks)
}

date_from_weeks <- function(weeks) {
  start_date <- as.Date("2023-04-15")
  target_date <- start_date + (weeks * 7)  # Convert weeks to days
  return(target_date)
}

read_excel_correct3rd <- function(file){
  data <- read_excel(file)
  data$Weight <- ifelse(data$Weight == 0.75,.85,data$Weight)
  return(data)
}

from_Jannik_to_glicko <- function(xlsx= NULL, csv = NULL,typeGroups = c("single", "ar"), year, month, day){
  
  if(typeGroups == "single") WeigthGroups <- 0.5 else WeigthGroups <- 0.775
  
  
  if(is.null(xlsx)){
    data <- read.csv(xlsx)
    need.ord <- FALSE
    if(is.null(data$phase)){
        data <- read.csv(xlsx, sep = ";")
        need.ord <- TRUE
        if(is.null(data$phase)){
          message("Change something else please")
          break
        }
    }
  
    data <-  
      data %>% 
      mutate(across(contains("player_"), ~ str_remove_all(., '"[^"]*"\\s{2}'))) %>% 
      mutate(
        Weight = case_when(
          str_detect(phase, "Gruppenphase|Poules|Gruppi|Group") ~ WeigthGroups,
          str_detect(game_name, "P[0-9]*") ~ 0.25,
          str_detect(phase, "Lower|Middle") & !str_detect(game_name, "P") ~ 0.35,
          TRUE ~1
        ),
        Weight = ifelse(str_detect(phase, "Upper|Main|K.O.|KO|Tableau final") & str_detect(game_name, "P3-4"),0.75, Weight)
        # Weight = ifelse(str_detect(phase, "Upper|Main|K.O.|KO|Tableau final") & str_detect(game_name, "P3-4"),0.85, Weight)
      ) %>% 
      # mutate(r = str_remove_all(`result (team_a, team_b)`, "[()]")) %>% 
      mutate(r = str_remove_all(result..team_a..team_b., "[()]")) %>% 
      separate(r, into = c("resA", "resB"), sep = ", ") %>% 
      mutate(across(c(resA, resB, sets_played), as.numeric),
             res = resA/sets_played) %>% 
      mutate(Score = case_when(
                res %in% c(1,.5,0) ~ res,
               !res %in% c(1,.5,0) ~ 1.5 * (res  - 1/3)+ 0.25
             ),
             Play1 = paste(player_1_team_a, " & ", player_2_team_a, sep = ""),
             Play2 = paste(player_1_team_b, " & ", player_2_team_b, sep = ""),
             Time = weeks_passed(year, month, day), 
             )
  }else{
    data <- read_excel(xlsx)
    need.ord <- FALSE
    
    data <-  
      data %>% 
      mutate(across(contains("player_"), ~ str_remove_all(., '"[^"]*"\\s{2}'))) %>% 
      mutate(
        Weight = case_when(
          str_detect(phase, "Gruppenphase|Poules|Gruppi|Group") ~ WeigthGroups,
          str_detect(game_name, "P[0-9]*") ~ 0.25,
          str_detect(phase, "Lower|Middle") & !str_detect(game_name, "P") ~ 0.35,
          TRUE ~1
        ),
        Weight = ifelse(str_detect(phase, "Upper|Main|K.O.|KO|Tableau final") & str_detect(game_name, "P3-4"),0.75, Weight)
        # Weight = ifelse(str_detect(phase, "Upper|Main|K.O.|KO|Tableau final") & str_detect(game_name, "P3-4"),0.85, Weight)
      ) %>% 
      # mutate(r = str_remove_all(`result (team_a, team_b)`, "[()]")) %>% 
      mutate(r = str_remove_all(`result (team_a, team_b)`, "[()]")) %>% 
      separate(r, into = c("resA", "resB"), sep = ", ") %>% 
      mutate(across(c(resA, resB, sets_played), as.numeric),
             res = resA/sets_played) %>% 
      mutate(Score = case_when(
                res %in% c(1,.5,0) ~ res,
               !res %in% c(1,.5,0) ~ 1.5 * (res  - 1/3)+ 0.25
             ),
             Play1 = paste(player_1_team_a, " & ", player_2_team_a, sep = ""),
             Play2 = paste(player_1_team_b, " & ", player_2_team_b, sep = ""),
             Time = weeks_passed(year, month, day), 
             )
  }
  
  unique_div <- unique(data$division)
  new_div <- character(length(unique_div))
  for(i in 1:length(unique_div)){
    
    new_name <- readline(paste("Category `", unique_div[i],"`. How to rename it?(new_name/k): \n", sep = ""))
    new_div[i] <- ifelse(tolower(new_name) == "k", unique_div[i], new_name) 
    
  }
  names <- data.frame(old = unique_div, new = new_div)
  data$cat <- names$new[match(data$division, names$old)]
  
  data <- data %>% select(cat, Time, Play1, Play2, Score, Weight)
  
  custom_order <- c(0.5, 0.775, 1, 0.75, 0.35, 0.25)
  
  
  if(need.ord){
    data <- 
      data %>% 
      group_by(Weight) %>%
      arrange(-row_number()) %>% 
      ungroup()
    }
  
  data$Weight <- factor(data$Weight, levels = custom_order)
  
  
  data_ord <- data[order(data$cat, data$Weight),]
  data_ord$Weight <- as.numeric(as.character(data_ord$Weight))

  return(data_ord)
 
}


Bundes_names <- function(xlsx,cat = "1.Bundesliga", date, Weight = .7){
  df <- read_excel(xlsx)
  g <- unique(stringr::str_to_lower(c(df$g1, df$g2, df$g3, df$g4)))
  
  names_g <- sapply(g, \(c) get(dft[nrow(dft),1])$ratings$giocatore[str_detect(get(dft[nrow(dft),1])$ratings$giocatore,str_to_title(sub("^[A-Za-z]\\s+", "", c)))])
  
  for(gg in names(names_g)){
    n <- menu(c(names_g[[gg]], "NULL"), title =paste0("Which Name is Correct for ",gg,"?" ))
    if(c(names_g[[gg]], "NULL")[n]== "NULL") names_g[[gg]] <- readline("Which Name to Give? ") else names_g[[gg]] <- names_g[[gg]][n]
  }
  df$cat <- cat
  df$Time <- weeks_passed(date = date)
  df$Weight <-  Weight
  df$Play1 <- paste0(names_g[df$g1]," & ",names_g[df$g2])
  df$Play2 <- paste0(names_g[df$g3]," & ",names_g[df$g4])
  df <-df %>% 
    ungroup %>% 
    select(-g1:-g4, cat, Time, Play1, Play2, Score, Weight)
  
  return(df)
}
from_xl_to_glicko <- function(data, year, month, day){
  
  modified_data <-  
    data %>% 
    mutate(
      Play1 = paste(g1,"&",g2),
      Play2 = paste(g3,"&",g4),
      Time = weeks_passed(year,month,day),
      tot_set = w1+w2,
      Score = case_when(
             w1/tot_set == 2/3 ~ 0.75,
             w1/tot_set == 1/3 ~ 0.25,
             TRUE ~ w1/tot_set),
      Weight = case_when(
             # tot_set == 1 ~.75,
             tot_set == 1 ~.5,
             TRUE ~1),
      # Weight = ifelse(Round == "P9",.5, Weight)) %>% 
      Weight = ifelse(Round == "P9",.25, Weight)) %>% 
    dplyr::select(cat,Time, Play1,Play2,Score,Weight) %>% 
    as.data.frame()
  
  
  return(modified_data)
  
}

remove_third_player <- function(file= NULL, x){

  x <- read.csv(file)
  
  # Check for Third Column
  if("Team.A...Player.3" %in% names(x)){
    x <- x[,-which(names(x) == "Team.A...Player.3")]
  }
  if("Team.B...Player.3" %in% names(x)){
    x <- x[,-which(names(x) == "Team.B...Player.3")]
  }
  
  # Check for Duplicated Players
  if(sum(x$Team.A...Player.1 == x$Team.A...Player.2)>0 ){
    print(paste0("Remove Duplicated Player! Lines:",paste0(which(x$Team.A...Player.1 == x$Team.A...Player.2), collapse = "", sep = ",")))
    print(paste0("File: ",file))
  }
  return(x)
}


from_csv_to_glicko <- function(file, year, month, day, draws = NULL, mixed = F){

  x <- remove_third_player(file)
  sets <- matrix(c(x$Game.1...Score.team.A,x$Game.2...Score.team.A,x$Game.3...Score.team.A),ncol = 3, nrow = nrow(x))
  setwin <- matrix(c(
    ifelse(x$Game.1...Score.team.A>x$Game.1...Score.team.B,1,ifelse(x$Game.1...Score.team.A==x$Game.1...Score.team.B,.5,0)),
    ifelse(x$Game.2...Score.team.A>x$Game.2...Score.team.B,1,0),
    ifelse(x$Game.3...Score.team.A>x$Game.3...Score.team.B,1,0)),
    ncol = 3, nrow = nrow(x))
  
  df <- data.frame(
    cat = x$Division,
    g1 = x$Team.A...Player.1,
    g2 = x$Team.A...Player.2,
    g3 = x$Team.B...Player.1,
    g4 = x$Team.B...Player.2,
    Score = ifelse(x$Winning.team == x$Team.A,1,0),
    sets = apply(sets, 1, function(x) sum(!is.na(x))),
    placement = ifelse(str_detect(x$Round, "(P)[0-9]*"),1,0), # Placement
    Awins = apply(setwin,1, function(x) sum(x,na.rm = T)),
    PowerPool = ifelse(str_detect(x$Group.Bracket, "Power Pool"),1,0),
    SeedingRB = ifelse(str_detect(x$Group.Bracket, "(Seed)|(Seeding)"),1,0),
    PositioningRB = ifelse(str_detect(x$Stage, "(Positioning)|(Pos)"),1,0),
    Division1 = ifelse(str_detect(x$Group.Bracket,"(Division 1)|(Bracket A)"),1,0),
    Division2 = ifelse(str_detect(x$Group.Bracket,"(Division 2)|(Division 3)|(Division 4)|(Bracket B)|(Bracket C)|(Bracket D)"),1,0),
    IMP = ifelse(str_detect(x$Round,"IMP"),1,0),
    Stage = x$Stage,
    Match = x$Match.format,
    third = ifelse(str_detect(x$Round, "P3:"),1,0)
  )
  

  
 
  df$Weight <- ifelse(df$sets==1, .5,1)

  df$Weight[df$PowerPool==1] <- .775 # Power Pool Groups A/R
  df$Weight[df$SeedingRB==1 | df$PositioningRB] <- .45  # Power Pool Groups A/R
  df$Weight[df$Division2==1] <- .35 # Lower
  df$Weight[df$placement==1] <- .25 # Placement
  df$Weight[df$IMP==1] <- .6        # Placement Importanti
  df$Weight[df$Division1 == 1 & df$third == 1] <- .85        # Placement Importanti
  df$Score <- (df$Awins)/df$sets
  df$Score[df$Score == 2/3] <- 0.75
  df$Score[df$Score == 1/3] <- 0.25
  df <- df[,-which(names(df) %in% c("sets","Awins"))]
  
  df$Weight <- with(df, case_when(
    Stage == "Gold Groups" ~ 0.75-0.001,
    Stage == "Silver Groups" ~ 0.75-0.002,
    Stage == "Bronze Groups" ~ 0.75-0.003,
    Stage == "Gold Bracket" & Match == "Best of 3"  ~ 1-0.001,
    Stage == "Gold Bracket" & Match == "Single game"  ~ .65-0.001,
    Stage == "Silver Bracket" & Match == "Best of 3"  ~ 1-0.002,
    Stage == "Silver Bracket" & Match == "Single game"  ~ .35-0.001,
    Stage == "Bronze Bracket" & Match == "Best of 3"  ~ .8-0.001,
    Stage == "Bronze Bracket" & Match == "Single game"  ~ .25-0.001,
    TRUE ~ Weight
  ))
  
  dfout <- data.frame(
    cat = df$cat,
    Time = weeks_passed(year,month,day),
    Play1 = paste(df$g1,"&",df$g2),
    Play2 = paste(df$g3,"&",df$g4),
    Score = df$Score,
    Weight = df$Weight
  )
  

  if(mixed == F){
    dfout <- dfout[!str_detect(dfout$cat, "Mix"),]
  }else{
    dfout <- dfout[str_detect(dfout$cat, "Mix"),]
  }
  if(!is.null(draws)) dfout <- make_draws(dfout, cats = draws)
  
  return(dfout)
}

make_draws <- function(data, cats = NULL){
  if( "all" %in% cats ) cats <- unique(data$cat)
  df_draws <- data %>% 
    filter( cat %in% cats, 
            Weight == 0.5) %>% 
    mutate(match = paste(Play1, Play2, sep =" VS ")) %>% 
    group_by(match) %>%
    mutate(Score = mean(Score),
           Weight = 0.775) %>%
    ungroup %>% 
    filter(duplicated(match)) %>% 
    select(-match)
  
  df_draws_bracket <- data %>% 
    filter( cat %in% cats, 
            Weight != 0.5)
  
  df_nodraws <- data %>% 
    filter(!cat %in% cats)
  
  df_dr <- rbind(df_draws, df_draws_bracket)
  df <- rbind(df_dr, df_nodraws) %>% 
    arrange(cat)
  return(df)
}

worlds_obj <- function(rat, t = c("ind", "squad")){
  if(t == "ind"){
  x <- read.csv("data/Extra/Worlds/24_Worlds.csv")
  giocatori = sort(unique(c(x$Team.A...Player.1, x$Team.A...Player.2, x$Team.B...Player.1, x$Team.B...Player.2)))
  
  giocatori <- stringi::stri_trans_general(str_to_title(giocatori), "Latin-ASCII")
  rat$ratings$giocatore <- stringi::stri_trans_general(str_to_title(rat$ratings$giocatore), "Latin-ASCII")
  
  npadd <- giocatori[!(giocatori %in% rat$ratings$giocatore)]

  
  y <- data.frame(
    row = rep(1:nrow(x),4),
    division = rep(x$Division,4),
    stage = rep(x$Stage,4),
    pl = c(x$Team.A...Player.1, x$Team.A...Player.2, x$Team.B...Player.1, x$Team.B...Player.2)
  )
  y$pl <-  stringi::stri_trans_general(str_to_title(y$pl), "Latin-ASCII")
  y$ismatch <- match(y$pl, npadd)
  
  last.res <- foreach(i = 1:length(npadd), .combine = rbind) %do% max(y$row[!is.na(match(y$ismatch, i))])
  
  z <- data.frame(
    npadd,
    y[last.res,c(2,3)]
  )
  z$cat <- with(z, paste0(division, " ", stage, sep = ""))

  
  worlds_cat <- unique(z[,c("division","stage")])
  worlds_cat$cat <- with(worlds_cat, paste0(division," ", stage, sep = ""))
  worlds_cat <- worlds_cat[with(worlds_cat, order(cat)),]
  
  worlds_cat$initrats <- c(1250,1350,1300,1150,1300,1250)
  worlds_cat$initdev <- c(200,200,200,150,150,150)
  worlds_cat <- worlds_cat[,c(3,4,5)]
  Individual_Worlds <- data.frame(
    giocatore = npadd,
    Rating = as.numeric(worlds_cat$initrats[match(z$cat,worlds_cat$cat)]),
    Deviation = as.numeric(worlds_cat$initdev[match(z$cat,worlds_cat$cat)]),
    Games =0,
    Win =0,
    Draw = 0,
    Loss = 0,
    nTourn = 0,
    LastTourn =72,
    LastPen = 0,
    PeakElo = 0,
    Percentile = 0,
    PeakRank = Inf, PeakTime = NA)
  WorldsRat <- Individual_Worlds
  # data = list(dataWI1, dataWI2)
  out <- list(rat = WorldsRat)
  
  # new_worlds_rat <- rbind(new_worlds_rat,jenki)
  # save(new_worlds_rat, file = "D:/Personal Statistics/rcb/Ranking/data/WORLDS/Ratings_wordls.rda")
  
  
  }else if(t == "squad"){
  # FWANGO NAMES FOR SQUAD ##
  dwbs <- read_excel("data/Extra/Worlds/Data Worlds Bracket Squads.xlsx", sheet = "Full")
  dbsq <- dwbs %>% 
    group_by(Group, Tab) %>% 
    mutate(SS = sum(Score),
           Games = n(),
           NewScore = case_when(
             SS/Games == 1/3 ~ .25,
             SS/Games == 2/3 ~ .75,
             TRUE ~ SS/Games
           )) %>% 
    filter(!duplicated(Group)) %>% 
    mutate(Weight = case_when(
      str_detect(Round, "(Round)|(Final)") ~ 1,
      str_detect(Round, "(T5 - T8)|(5th and 6th)|(7th and 8th)") ~ .65,
      str_detect(Tab, "Groups 1.")~.4,
      str_detect(Tab, "Groups 2.")~.6,
      TRUE ~.3
    ))
  
  dbsq <-  dbsq %>% mutate(across(contains("g"), ~stringi::stri_trans_general(str_to_title(.), "Latin-ASCII")))
  
  names <- data.frame(names = sort(unique(c(dbsq$g1,dbsq$g2,dbsq$g3,dbsq$g4))))
  names$names <- stringi::stri_trans_general(str_to_title(names$names), "Latin-ASCII")
  
  nadd <- read_excel("data/Extra/Worlds/Data Names Worlds Squads (Revisione1).xlsx")
  nadd$names <- stringi::stri_trans_general(str_to_title(nadd$names), "Latin-ASCII")
  
  names$act <- nadd$names[match(names[,1], nadd$names)]
  names$fwagno <- nadd$V2[match(names[,1], nadd$names)]
  
  
  g1 <- names$fwagno[match(dbsq$g1, names$act)]
  g2 <- names$fwagno[match(dbsq$g2, names$act)]
  g3 <- names$fwagno[match(dbsq$g3, names$act)]
  g4 <- names$fwagno[match(dbsq$g4, names$act)]
  
  dataWSB <- data.frame(
    cat = dbsq$Type,
    Time = rep(72, times = nrow(dbsq)),
    Play1 = paste0(g1, " & ",g2),
    Play2 = paste0(g3, " & ",g4),
    Score = dbsq$NewScore,
    Weight = dbsq$Weight
  )
  
  SBgroups <- str_detect(dbsq$Tab,"0")
  dataWSB1 <- dataWSB[SBgroups,]
  dataWSB2 <- dataWSB[!SBgroups,]
  

  # INITVAL FOR SQUAD ##
  sqgiocatori = sort(unique(c(g1,g2,g3,g4)))
  sqgiocatori = stringi::stri_trans_general(str_to_title(sqgiocatori), "Latin-ASCII")
  
  
  
  sqnpadd <- sqgiocatori[!(sqgiocatori %in% stringi::stri_trans_general(str_to_title(rsWInd24b$ratings$giocatore), "Latin-ASCII") )]
  
  

  sqy <- data.frame(
    row = rep(1:nrow(dbsq),4),
    division = rep(dbsq$Type,4),
    stage = rep(dbsq$Round,4),
    pl = c(g1,g2,g3,g4)
  )
  sqy$ismatch <- match(sqy$pl, sqnpadd)
  sq.last.res <- foreach(i = 1:length(sqnpadd), .combine = rbind) %do% max(sqy$row[!is.na(match(sqy$ismatch, i))])
  
  sqz <- data.frame(
    sqnpadd,
    sqy[sq.last.res,c(2,3)]
  )
  sqz$cat <- with(sqz, paste0(division, " ", stage, sep = ""))
  
  squad_worlds_cat <- unique(sqz[,c("division","stage")])
  squad_worlds_cat$cat <- with(squad_worlds_cat, paste0(division," ", stage, sep = ""))
  squad_worlds_cat <- squad_worlds_cat[with(squad_worlds_cat, order(cat)),]
  
  positions <- c("Final", "3rd", paste(seq(from = 5,to = 20, by = 2), "th", sep = ""))
  rats <- c(1550,1550,1450,1450,1350,1350,1250, 1250, 1250, 1250)
  squad_worlds_cat$initrats <- foreach(i = 1:nrow(squad_worlds_cat), .combine = c) %do%{
    strn <- squad_worlds_cat$cat[i]
    if(sum(str_detect(strn, positions))>0){
      pos <- which(str_detect(strn, positions))
      rat <- rats[pos[which.max(pos)]]
    }else{
      rat <- 1150
    }
    rat
  }
  squad_worlds_cat$initdev <- rep(200, nrow(squad_worlds_cat))
  squad_worlds_cat <- squad_worlds_cat[,c(3,4,5)]
  
  Squad_Rat <- data.frame(
    giocatore = sqnpadd,
    Rating = as.numeric(squad_worlds_cat$initrats[match(sqz$cat,squad_worlds_cat$cat)]),
    Deviation = as.numeric(squad_worlds_cat$initdev[match(sqz$cat,squad_worlds_cat$cat)]),
    Games = 0,
    Win = 0,
    Draw = 0,
    Loss = 0,
    nTourn = 0,
    LastTourn =72,
    LastPen = 0,
    PeakElo = 0,Percentile = 0,
    PeakRank = Inf, PeakTime = NA
  )
  WorldsRat <- rbind(Squad_Rat)
  data = list( dataWSB1, dataWSB2)
  out <- list(rat = WorldsRat, data = data)
  
  }
  return(out)
}

extract_last_part <- function(info) {
  last_part <- tail(unlist(strsplit(gsub("[\n]+", "\n", info), "\n")), 1)
  return(trimws(last_part))  
}

divide_into_columns <- function(info) {
  cleaned_info <- gsub("[\n]+", "\n", info) 
  lines <- 
    lines <- trimws(lines)  
  
  
  lines <- lines[nzchar(lines)]
  
  num_columns <- 3
  df <- as.data.frame(matrix(lines, ncol = num_columns, byrow = TRUE), stringsAsFactors = FALSE)
  
  colnames(df) <- paste0("Column", 1:num_columns)
  
  return(df)
}

from_scraper_to_glicko <- function(games, names, typeGroups = c("single", "ar"), cat , year, month, day){
  if(!typeGroups %in%  c("single", "ar")){ 
    print("Wrong Format for Groups!")
    break()
  }
  if(typeGroups == "single") WeigthGroups <- 0.5 else WeigthGroups <- 0.775
  gm <- read.xlsx(games, sheetIndex = 1)[,3:6] %>% 
    filter(punti != "") %>% 
    rowwise %>% 
    mutate(g11 = extract_last_part(g11),
           p1 = unlist(str_extract_all(punti, "([0-9]{1,2}:[0-9]{1,2})"))[1],
           p2 = unlist(str_extract_all(punti, "([0-9]{1,2}:[0-9]{1,2})"))[2],
           p3 = unlist(str_extract_all(punti, "([0-9]{1,2}:[0-9]{1,2})"))[3],
           type = ifelse(str_extract(pl, "[A-Z]{1}") != "P3-4",str_extract(pl, "[A-Z]{1}"), "F")) %>% 
    mutate(
      Weight = case_when(
        type == "G" ~ WeigthGroups,
        type == "P" ~ .25,
        type == "L" ~ .35,
        TRUE ~ 1
      ),
      p1a = ifelse(as.double(unlist(str_extract_all(p1, "[0-9]+")))[1]>as.double(unlist(str_extract_all(p1, "[0-9]+")))[2],1,0),
      p2a = ifelse(as.double(unlist(str_extract_all(p2, "[0-9]+")))[1]>as.double(unlist(str_extract_all(p2, "[0-9]+")))[2],1,0),
      p3a = ifelse(as.double(unlist(str_extract_all(p3, "[0-9]+")))[1]>as.double(unlist(str_extract_all(p3, "[0-9]+")))[2],1,0)
    ) 
  
  if(typeGroups == "single"){ 
    gm <-  gm %>% 
      rowwise %>% 
      mutate(Score=ifelse(type == "G", p1a, mean(c(p1a,p2a,p3a), na.rm = T)))
  }else{
    gm <- gm %>% 
      rowwise %>% 
      mutate(Score=ifelse(type == "G", mean(c(p1a,p2a), na.rm =T),mean(c(p1a,p2a,p3a), na.rm = T)))
  }
  gm$Score <- case_when(
    gm$Score == 1/3 ~.25,
    gm$Score == 2/3 ~.75,
    TRUE ~ gm$Score
  )
  gm <- select(gm, g11,g22, Score, Weight)
  
  nm <- read.xlsx(names, sheetIndex = 1)[,4:6] 
  nm[,1] <- str_remove(nm[,1], "(\"[a-zA-Z]+\"\\s{2})")
  nm[,2] <- str_remove(nm[,2], "(\"[a-zA-Z]+\"\\s{2})")
  nm$Play <- paste(nm[,1], " & ", nm[,2], sep = "")
  
  gm$Play1 <- nm$Play[match(gm$g11, nm[,3])]
  gm$Play2 <- nm$Play[match(gm$g22, nm[,3])]
  gm$cat <- cat
  gm$Time <- weeks_passed(year, month ,day)
  
  gm <- select(gm, cat, Time, Play1, Play2, Score, Weight) %>% ungroup
  return(gm)
  
  
}

from_WebScrapBook_to_glicko <- function(df, names, typeGroups = c("single", "ar"), cat , year, month, day){
  if(!typeGroups %in%  c("single", "ar")){ 
    print("Wrong Format for Groups!")
    break()
  }
  if(typeGroups == "single") WeigthGroups <- 0.5 else WeigthGroups <- 0.775
  suppressMessages(df <- read_excel(df, col_names = F))
  nm <- read.xlsx(names, sheetIndex = 1)[,4:6]
  
  
  colnames(df)[1] <- "data"
  df$num <- str_detect(df$data, "[a-zA-Z]+")
  df$numlag <- lag(df$num)
  df$group <- ifelse(df$num ==TRUE & df$numlag == FALSE,1,0)
  df$group[1] <- 0
  df$id <- cumsum(df$group)
  df[df$num==FALSE,1] <- df[df$num==FALSE,1] %>% 
    rowwise() %>% 
    transmute(data = ifelse(as.double(str_extract(data, "[0-9]*"))>as.double(str_trim(str_extract(data, "\\s[0-9]{1,2}"))),1,0)) %>% 
    mutate(data = as.character(data))
  
  
  dfsplit <- split(df, df$id)
  gm <- do.call(rbind,lapply(dfsplit,function(x){
    type = x[1,1]
    g1 <- x[2,1]
    g2 <- x[3,1]
    Score <- mean(as.double(t(x[4:nrow(x),1])))
    Score <- case_when(
      Score == 1/3~0.25,
      Score == 2/3 ~0.75,
      TRUE ~Score)
    yy <- data.frame(type, g1,g2,Score)
    return(yy)
  }))
  gm <- gm[!str_detect(gm$data, "Cancelled"),]
  gm$Weight <- case_when(
    str_detect(gm$data, "Gr")~ WeigthGroups,
    str_detect(gm$data, "P")~ 0.25,
    str_detect(gm$data, "Third")~ 0.75,
    str_detect(gm$data, "IM")~ 0.6,
    str_detect(gm$data, "LB")~ 0.35,
    TRUE ~1
  )
  if(any(gm$Weight != WeigthGroups)){
    gm$id <- 1:nrow(gm)
    gmGroups <- gm %>% filter(Weight == WeigthGroups)
    gmBracket <- gm %>% filter(Weight != WeigthGroups)
    gmBracket <- gmBracket[order(-gmBracket$id),]
    gm <- rbind(gmGroups,gmBracket)
    gm <- select(gm, -id)
  }
  
  nm[,1] <- str_remove(nm[,1], "(\"[a-zA-Z]+\"\\s{2})")
  nm[,2] <- str_remove(nm[,2], "(\"[a-zA-Z]+\"\\s{2})")
  nm$Play <- paste(nm[,1], " & ", nm[,2], sep = "")
  
  Time <- weeks_passed(year, month ,day)
  
  out <- data.frame(cat = cat, Time = Time, Play1 = nm$Play[match(gm$data.1, nm$Squadra)],Play2 = nm$Play[match(gm$data.2, nm$Squadra)], Score = gm$Score, Weight = gm$Weight)
  return(out)
}

ranking_datasets <- function(data = NULL, flatten = F){
  # current_dir <- getwd()
  if(!exists("datasets")){
  
  current_dir <-"D:/Personal Statistics/rcb/Ranking/data"
  items <- list.files(current_dir, full.names = TRUE)
  folder_data <- list()
  
  for (item in items) {
    if (dir.exists(item) && basename(item) != "Extra") {
      files_in_folder <- list.files(item, full.names = TRUE)
      folder_files_data <- list()
      
      for (file in files_in_folder){
        if (grepl("\\.csv$", file, ignore.case = TRUE)) {
          
          file_name_without_ext <- file_path_sans_ext(basename(file))
          file_info <- strsplit(file_name_without_ext, "_")[[1]]
          
          tournament_name <- file_info[1]
          
          year <- as.numeric(file_info[2])
          month <- as.numeric(file_info[3])
          day <- as.numeric(file_info[4])
          
          if(length(file_info[-(1:4)])>0) categories <- file_info[-(1:4)] else categories = NULL
          
          if (grepl("Worlds", basename(file), ignore.case = TRUE)) {
            folder_files_data[[tournament_name]] <- from_worlds_to_glicko(file, year, month, day, categories)
          } else {
            folder_files_data[[tournament_name]] <- from_csv_to_glicko(file, year, month, day, categories)
          }
          
        }else if (grepl("\\.xlsx$", file, ignore.case = TRUE)){
          file_name_without_ext <- file_path_sans_ext(basename(file))
          file_info <- strsplit(file_name_without_ext, "_")[[1]]
          tournament_name <- file_info[1]
          
          # folder_files_data[[tournament_name]] <- read_excel(file)
          folder_files_data[[tournament_name]] <- read_excel_correct3rd(file)
        }
      }
      
      folder_data[[basename(item)]] <- folder_files_data
    }
  }
  if(flatten) folder_data <- purrr::flatten(folder_data)
  }
  
  return(folder_data)
}

from_JSON_to_glicko <- function(file_path){
  
  data <- fromJSON(file_path)
  
  TourName <- data$coreDetails$name
  
  names <- map_dfr(data$mass$teams, function(x){ 
    data.frame( 
      TeamName = x$coreDetails$name,
      Name1 = paste(data$mass$participants[[x$userIds[1]]]$coreDetails$firstName, data$mass$participants[[x$userIds[1]]]$coreDetails$lastName, sep = " " ),  
      Name2 = paste(data$mass$participants[[x$userIds[2]]]$coreDetails$firstName, data$mass$participants[[x$userIds[2]]]$coreDetails$lastName, sep = " " ),
      id1  = x$`_id`,
      # id2 = data$mass$participants[[x$userIds[2]]]$`_id`, 
      cat = x$registrations$trackId)
  })
  # names$cat <- str_extract(names$cat, "^[^-\\d]+(-[^-\\d]+)?") %>% str_replace_all("-", " ")
  names <- names[!grepl("mixte",names$cat),]
  catNames <- unique(names$cat)
  
  data$mass$results$tournament$tracks <- data$mass$results$tournament$tracks[which(!grepl("mixte", names(data$mass$results$tournament$tracks)))]
  
  games <- imap_dfr(data$mass$results$tournament$tracks, function(track,track_name) {
    map_dfr(track$games, function(game) {
      if(game$status == "Uninitialized"){ NULL }else{
        data.frame(
          Team1      = ifelse(game$result$finalScore$`0`>game$result$finalScore$`1`,game$participationUnits$units[[1]]$id,game$participationUnits$units[[2]]$id),
          Team2      = ifelse(game$result$finalScore$`0`>game$result$finalScore$`1`,game$participationUnits$units[[2]]$id,game$participationUnits$units[[1]]$id),
          Sets       = nrow(game$result$setScores),
          Score      = max(game$result$finalScore$`0`,game$result$finalScore$`1`),
          idgame     = game$id,
          Tournament = track_name,
          stringsAsFactors = FALSE
        )}
    })
  })
  
  
  
  # group_games <- map(data$mass$results$tournament$tracks, function(y) unlist(y$phases$groups[[1]]$stages[[1]]$groups[[1]]$games))
  year <- year(data$coreDetails$dateRange$start)
  if(year ==2023) condition <- (length(track$phases$groups)>1 & nrow(track$phases$groups[[1]])>1) else condition <- (length(track$phases$groups)>1 )
  
  
  bracket_games <- map_dfr(
    data$mass$results$tournament$tracks,
    function(track) {
      if(condition ){ #
        groups_list <- track$phases$groups[[2]]$stages[[1]]$groups
        
        map_dfr(seq_along(groups_list), function(i) {
          g <- groups_list[[i]][which(unlist(map(groups_list[[i]]$games, \(a) !is_empty(a)))),]
          
          
          w <- ifelse(g$weight == 0, 1, 0.25)
          if (i == length(groups_list)) w <- ifelse(g$weight == 1, 0.75, w)
          
          data.frame(
            w = w,
            Team1 = unlist(map(g$result, \(t) t$participationUnitId[1])),
            Team2 = unlist(map(g$result, \(t) t$participationUnitId[2])),
            idgame = unlist(g$games),
            Sets = unlist(map(g$result, \(r) sum(r$result$setsFor))),
            Score = unlist(map(g$result, \(r) r$result$setsFor[1]))
          )
        })}
    }
  )
  
  
  
  merged_games <- suppressMessages(left_join(games, bracket_games)) %>% 
    mutate(w = case_when(
      is.na(w) & Sets == 2 ~ .775,
      is.na(w) & Sets == 1 ~ .5,
      TRUE~w)
    ) %>% 
    arrange(Tournament, match(w, c(.5,.775,1,.75,.35,.25)))
  
  # (merged_games %>% filter(is.na(w), Tournament == "tournoi") %>% pull(idgame)) %in% group_games$tournoi
  
  month <- month(data$coreDetails$dateRange$start)
  day <- day(data$coreDetails$dateRange$start)
  dfout <- data.frame(
    cat = merged_games$Tournament,
    Time = weeks_passed(year, month, day),
    Play1 = paste0(names$Name1[match(merged_games$Team1, names$id1)]," & ",names$Name2[match(merged_games$Team1, names$id1)], sep = ""),
    Play2 = paste0(names$Name1[match(merged_games$Team2, names$id1)]," & ",names$Name2[match(merged_games$Team2, names$id1)], sep = ""),
    Score = with(merged_games, case_when(
      Score/Sets == 2/3 ~ .75,
      TRUE ~ Score/Sets
    )),
    Weight = merged_games$w
  )
  
  
  
  current_index <- 1  
  cat("Categorie Presenti: ", catNames, "\n", sep = ";")
  while (current_index <= length(unique(dfout$cat))) {
    cats <- unique(dfout$cat)[current_index]
    cat("Categoria:", cats, "\n")
    choice <- readline("Nuovo Nome: ")
    dfout$cat[dfout$cat == cats] <- choice
    current_index <- current_index+1
  }
  cat("Tournament Name:", TourName, "\n")
  saveFileName <- readline("Come Salvarlo?: ")
  write.xlsx(dfout, saveFileName, row.names = FALSE)
  return(dfout)
}

correct_names <- function(xlsx, pl, save_name){
  eu <- read.xlsx(xlsx, sheetIndex = 1)
  
  eu.pl <- unique(c(eu$g1, eu$g2, eu$g3, eu$g4))
  eu.pl.nodot <- gsub(".*?\\. ", "", eu.pl)
  
  matches <- sapply(eu.pl.nodot, function(p)  pl[which(grepl(p, pl, ignore.case = TRUE))])
  names(matches) <- eu.pl
  
  
  for(m in 1:length(matches)){
    curM <- c(matches[[m]], NULL)
    namM <- names(matches[m])
    
    if(length(curM)>0){
      choice <- menu(curM, title = paste0("Which surname is correct for ", namM, "?"))
      matches[[m]] <- curM[choice]
    }else{
      choice <- readline(paste0("What could be the name for ", namM, "?"))
      matches[[m]] <- choice
    }
  }
  
  ru <- 
    eu %>% 
    rename(W = Weight) %>% 
    rowwise() %>% 
    mutate(across(contains("g"),~matches[[.]]),
           Play1 = paste0(g1, " & ", g2),
           Play2 = paste0(g3, " & ", g4)) %>% 
    rename(Weight = W) %>% 
    ungroup() %>% 
    select(cat, Time, Play1, Play2, Score, Weight)
  write.xlsx(ru, file = save_name)
}

# Glicko Functions ####

glicko_ets <- function(x, status = NULL,rdmax = 300, cval = 14, lambda = 1/20, bval = 1/50, cat_to_unite = NULL, par = NULL, byrow = NULL){
 
  # Things to check 
  
  if(!is.null(status) && unique(date_from_weeks(x$Time))<max(status$LastTourn,na.rm = T)){ 
    print("Tournaments Not In Chronological Order!")
    break()
  }
  if(!is.null(par)){ 
    lambda <- par[1]
    bval <- par[2]
  }
  
  # Blank data used fro output
  data = x
  kstatus <- status
  
  # Start Data-Manipulation
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  giocatori <-stringi::stri_trans_general(str_to_title(giocatori), "Latin-ASCII")
  x <-  x %>% mutate(across(contains("Giocatore"), ~stringi::stri_trans_general(str_to_title(.), "Latin-ASCII")))
   
  # Va applicato anche a Giocatore1,...,Giocatore4
  
  np <- length(giocatori)
  nm <- length(unique(x$cat))
  
  # Assigning Division for Initialisation
  cat_values <- NULL
  for(i in seq_along(giocatori)) {
    giocatorei <- giocatori[i]
    cat_values[i] <- unique(x$cat[which(giocatorei == x$Giocatore1 | giocatorei == x$Giocatore2 | giocatorei == x$Giocatore3 | giocatorei == x$Giocatore4)])[1]
  }
  names(cat_values) <- giocatori
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  
  # Status
  if(!exists("initval")) load("R/Init.rda")
  if (!is.null(status)) {
    status$giocatore <- stringi::stri_trans_general(str_to_title(status$giocatore), "Latin-ASCII")
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           # Rating = rep(init[1], length(npadd)),
                           Rating = as.numeric(initval$initrats[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           # Deviation = rep(init[2], length(npadd)), 
                           Deviation = as.numeric(initval$initdev[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           Games = zv, Win = zv, Draw= zv, Loss = zv,nTourn = zv,
                           LastTourn =rep(date_from_weeks(time),length(npadd)),
                           LastPen = zv, PeakElo = zv, Percentile = zv, PeakRank = 1/zv, 
                           PeakTime = rep(date_from_weeks(time), length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Draw" %in% names(status))) 
      status <- cbind(status, Draw = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = date_from_weeks(time))
    if (!("LastPen" %in% names(status))) 
      status <- cbind(status, LastPen = 0)
    if (!("PeakElo" %in% names(status))) 
      status <- cbind(status, PeakElo = 0)
    if (!("Percentile" %in% names(status))) 
      status <- cbind(status, Percentile = 0)
    if (!("PeakRank" %in% names(status))) 
      status <- cbind(status, PeakRank = Inf, PeakTime = 0)
    if (!("PeakTime" %in% names(status))) 
      status <- cbind(status, PeakTime = date_from_weeks(time))
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win","Draw", "Loss", "nTourn","LastTourn", "LastPen", "PeakElo","Percentile", "PeakRank", "PeakTime")], npstatus)
    
    rinit <- ifelse(status[[2]]<700,700,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    ndraw <- status[[6]]
    nloss <- status[[7]]
    ntourn <- status[[8]]
    nlasttourn <- status[[9]]
    lastpen <- status[[10]]
    peakelo <- status[[11]]
    peakrank <- status[[13]]
    peaktime <- as.Date(status[[14]])
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    # rinit <- rep(init[1], length.out = np)
    # dinit <- rep(init[2], length.out = np)
    rinit <- as.numeric(initval$initrats[match(cat_values,initval$categories)])
    dinit <- as.numeric(initval$initdev[match(cat_values,initval$categories)])
    ngames <- nwin <-ndraw <-  nloss <- rep(0, length.out = np)
    ntourn <- rep(0, np)
    nlasttourn <- rep(time,np)
    lastpen <- rep(0, np)
    peakelo <- rep(0, np)
    peakrank <- rep(0, np)
    peaktime <- rep(date_from_weeks(time), np)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<-names(lastpen) <-  giocatori
  }
  
  # Players Active in this Event
  curplay <- match(giocatori, names(rinit))
  
  nlasttourn <- weeks_passed(date = nlasttourn)
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  ondraw <- ndraw[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  olastpen <- lastpen[-curplay]
  opeakelo <- peakelo[-curplay]
  opeakrank <- peakrank[-curplay]
  opeaktime <- peaktime[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  ndraw <- ndraw[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1 
  nlasttourn <- nlasttourn[curplay]
  nlastpen <- lastpen[curplay]
  npeakelo <- peakelo[curplay]
  npeakrank <- peakrank[curplay]
  
  
  npeaktime <- ifelse(is.na(peaktime[curplay]), time, weeks_passed( date = peaktime[curplay]))
  
  
  histry <- array(NA,
                  dim = c(np, 7),
                  dimnames = list(giocatori, 
                                  c("Rating", "Deviation", "Games", "Wins","Draws", "CatRating","Category")
                  ))
  histry<- as.data.frame(histry)
  
  # Costants
  qv <-  log(10)/400
  qip3 <- 3 * (qv/pi)^2
  
  # Dynamic Teammate Changing Scenatio 
  if(is.null(byrow)){
    nameteams <- data.frame(G1 = c(x$Giocatore1,x$Giocatore3),G2 = c(x$Giocatore2,x$Giocatore4))
    sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
    split_cols <- strsplit(sq, " ")
    g1 <- as.numeric(sapply(split_cols, `[`, 1))
    g2 <- as.numeric(sapply(split_cols, `[`, 2))
    
    cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
    cratsteams <- cratsteams[order(names(cratsteams))]
    
    cdevs <- pmin(pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn)),rdmax),rdmax)
    cdevsteams <- c(sqrt((cdevs[g1]^2+cdevs[g2]^2)/2),sqrt((cdevs[g2]^2+cdevs[g1]^2)/2)) # Actual

    cdevs_mid <- numeric(length(c(g1,g2)))
    for(i in 1:np){
      cdt <- cdevsteams[i]
      cdp1 <- cdevs[c(g1,g2)[i]]
      cdp2 <- cdevs[c(g2,g1)[i]]
      cdevs_mid[i] <- ifelse(cdt<80 & cdp1 <80 & cdp2<80, cdt, cdevs[c(g1,g2)[i]])
    }
    names(cdevs_mid) <- giocatori[c(g1,g2)]
    cdevsteams <- cdevsteams[order(names(cdevsteams))]
    cdevs_mid <- cdevs_mid[order(names(cdevs_mid))]
    
  }else if(byrow == "all"){
    cdevs_mid <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
    cratsteams <- crats
    byrow <- unique(x$cat)
  }else{
    cdevs <-  pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
    byrowPl <- giocatori[cat_values %in% byrow]
    
    nameteams <- data.frame(G1 = c(x$Giocatore1[!x$cat %in% byrow],x$Giocatore3[!x$cat %in% byrow]),G2 = c(x$Giocatore2[!x$cat %in% byrow],x$Giocatore4[!x$cat %in% byrow]))
    sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
    split_cols <- strsplit(sq, " ")
    g1 <- as.numeric(sapply(split_cols, `[`, 1))
    g2 <- as.numeric(sapply(split_cols, `[`, 2))
    
    cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
    cratsteams <- cratsteams[order(names(cratsteams))]
    
    cdevsteams <- c(sqrt((cdevs[g1]^2+cdevs[g2]^2)/2),sqrt((cdevs[g2]^2+cdevs[g1]^2)/2)) # Actual
    
    cdevs_mid <- numeric(length(giocatori)-length(byrowPl))
    for(i in 1:length(cdevs_mid)){
      cdt <- cdevsteams[i]
      cdp1 <- cdevs[c(g1,g2)[i]]
      cdp2 <- cdevs[c(g2,g1)[i]]
      cdevs_mid[i] <- ifelse(cdt<80 & cdp1 <80 & cdp2<80, cdt, cdevs[c(g1,g2)[i]])
    }
    names(cdevs_mid) <- c(giocatori[g1], giocatori[g2])
    
    cdevs_mid <- c(cdevs_mid, cdevs[byrowPl])
    cdevs_mid <- cdevs_mid[order(names(cdevs_mid))]
    
    cratsteams <- c(cratsteams, crats[byrowPl])
    cratsteams <- cratsteams[order(names(cratsteams))]
  }
  
  # Possible unions of categories
  if(!is.null(cat_to_unite)){
    cat_to_remove <- str_extract(cat_to_unite, ".*(?= ->)")
    cat_into_merge <- str_extract(cat_to_unite, "(?<=-> ).*")
    x$cat[which(x$cat == cat_to_remove)] <- cat_into_merge
    nm <- length(unique(x$cat))
  }
  x <- split(x, x$cat)
  players <- lapply(x, function(y) unique(c(y$Giocatore1, y$Giocatore2, y$Giocatore3, y$Giocatore4)))
  
  for (i in 1:nm) {
    traini <- x[[i]]
    nr <- nrow(traini)
    playi <- players[[i]]
    ngamesi <- tabulate(c(traini$Giocatore1,traini$Giocatore2,traini$Giocatore3,traini$Giocatore4), np)
    
    if(!is.null(byrow) & unique(traini$cat) %in% byrow) f <- "glicko_c_byrow" else f <- "glicko_c"
    
    dscores <- do.call(f, list(numplay = np, numrow = nr, white1 = traini$Giocatore1, white2 = traini$Giocatore2,
                          black1 = traini$Giocatore3,black2 = traini$Giocatore4, score = traini$Score,
                          cdevsteams = cdevs_mid, cratsteams =  cratsteams,weights=traini$Weight, bval = bval))
    
    cdscores <- dscores[[1]][playi]
    cdval <- dscores[[1]][(np + 1):(2 * np)][playi] # 1/d^2
    ascore <- dscores[[2]][playi]
    l1t <- dscores[[3]][playi]
    names(ascore) <- names(cdval) <- names(cdscores) <- names(l1t) <-  giocatori[playi]
    
    # Updated Values
    curupt <- qv/(1/(cdevs_mid[giocatori[playi]]^2)+ cdval) * cdscores + lambda * l1t/ngamesi[playi]
    crats[playi] <- crats[playi] + curupt 
    cdevs[playi] <- sqrt((1/cdevs[playi]^2 + cdval)^(-1))
    
    # Table of Games/Win/Draw/Loss
    trainiplw <- c(traini$Giocatore1[traini$Score > .5], traini$Giocatore2[traini$Score > .5],traini$Giocatore3[traini$Score < .5],traini$Giocatore4[traini$Score < .5])
    trainipll <- c(traini$Giocatore1[traini$Score < .5], traini$Giocatore2[traini$Score < .5],traini$Giocatore3[traini$Score > .5],traini$Giocatore4[traini$Score > .5])
    trainipld <- c(traini$Giocatore1[traini$Score == .5], traini$Giocatore2[traini$Score == .5],traini$Giocatore3[traini$Score == .5],traini$Giocatore4[traini$Score == .5])
    
    ngames <- ngames + ngamesi 
    nwin <- nwin + tabulate(trainiplw, np) 
    ndraw <- ndraw + + tabulate(trainipld, np)
    nloss <- nloss + tabulate(trainipll, np) 
    
    nlasttourn <- rep(time,length(nlasttourn))
    
    # Filling `history` list
    histry[playi, 1] <- round(curupt)
    histry[, 2] <- round(cdevs)
    histry[playi, 3] <- ngamesi[playi] 
    histry[playi, 4] <- tabulate(trainiplw, np)[playi] 
    histry[playi, 5] <- tabulate(trainipld, np)[playi]
    histry[playi, 6] <- round(lambda * l1t/ngamesi[playi])
    if(is.null(cat_to_unite)) histry[playi, 7] <- rep(unique(x[[i]]$cat), length(playi)) else histry[playi, 7] <- cat_values[playi] 
    
    npeakelo[playi] <- round(ifelse(crats[playi]>npeakelo[playi], crats[playi], npeakelo[playi]))
    currank <- rank(-c(crats, orats), ties.method = "min")
    npeakrank[playi] <- ifelse(currank[playi]<npeakrank[playi], currank[playi], npeakrank[playi])
    # npeaktime[playi] <- ifelse(currank[playi]<=npeakrank[playi],  time,npeaktime[playi])  # PeakTime Refers to Rank
    npeaktime[playi] <- ifelse(crats[playi]>npeakelo[playi],  time,npeaktime[playi])        # PeakTime Refers to Elo
    npeaktime[playi] <- as.Date(date_from_weeks(npeaktime[playi]))

    
  }
  
  
  player <- suppressWarnings(as.numeric(names(c(crats, orats))))
  if(any(is.na(player))) 
    player <- names(c(crats, orats))
  
  
  # Create `ratings` data frame
  dfout <- data.frame(
    giocatore = player,
    Rating = round(c(crats, orats)), 
    Deviation = round(c(cdevs, odevs),0),
    Games = c(ngames, ongames),
    Win = c(nwin, onwin),
    Draw = c(ndraw, ondraw),
    Loss = c(nloss, onloss),
    nTourn = c(ntourn, ontourn),
    LastTourn = c(nlasttourn,olasttourn),
    LastPen = c(nlastpen, olastpen),
    PeakElo = c(npeakelo, opeakelo),
    PeakRank = c(npeakrank, opeakrank),
    PeakTime = as.Date(c(npeaktime,opeaktime)),
    stringsAsFactors = FALSE)
  
  dfout$Percentile <- round(percent_rank(dfout$Rating)*100,0)

  
  
  
  dfout <- apply_pen(dfout, time)
  dfout$LastTourn <- date_from_weeks(dfout$LastTourn)
  
  # Remove NA players
  rownames(dfout) <- NULL
  if(sum(is.na(dfout$Rating))>0){ 
    cat(paste0("NA was detected and removed! Players: \n",paste0(dfout[which(is.na(dfout$Rating)),1],collapse = "\n"), 
               "\nNA count: ",sum(is.na(dfout$Rating))))
    dfout <- dfout[-which(is.na(dfout$Rating)),]
  }
  
  # Ordering and creating `par` list
  dfout <- dfout[order(dfout$Rating, decreasing = TRUE), ]
  rownames(dfout) <- 1:nrow(dfout)
  param = list(lambda = lambda, cval = cval, bval = bval, rdmax = rdmax, cat_to_unite = cat_to_unite, byrow = byrow)
  
  
  # Adding italian part to the dataset
  load("data/Extra/ita_players.rda")
  itadfout <- dfout[dfout$giocatore %in% ita_players,]
  rownames(itadfout) <- NULL
  itahistry <- histry[rownames(histry) %in% ita_players,]
  ita.lout <- list(ratings = itadfout,  history = itahistry, impr = NULL)
  
  # Output
  lout <- list(ratings = dfout, history = histry,param = param,data = data, ita = ita.lout, impr = NULL, type = "Glicko")
  
  # Adding `order_history()` directly to output
  lout$history <- order_history(lout)
  if(any(rownames(histry) %in% ita_players))  lout$ita$history <- order_history(lout$ita)
  
  
  
    status_blank <- list(ratings = kstatus, ita = list(ratings = kstatus[kstatus$giocatore %in% ita_players,]))
    tempLout <- lout
    lout$impr <- improvements(lout,status_blank,sort = "New")
  
  # if(any(rownames(histry) %in% ita_players)){
  #   tempLout$ratings <- tempLout$ratings[tempLout$ratings$giocatore %in% ita_players,]
  #   lout$ita$impr <- improvements(tempLout,status_blank, sort = "New", it = T)
  # }
  class(lout) <- "rating"
  return(lout)
}

glicko_c <- function(numplay, numrow, white1, white2, black1, black2, score, cdevsteams, cratsteams,dscore = double(2*numplay),weights,bval) { # Capire perchè 2*np
  escore <- numeric(numplay)
  ascore <- numeric(numplay)
  dval <- numeric(numplay)
  ptsupd <- numeric(numplay)
  l1t <- numeric(numplay)
  escorek <- 0
  qv <- (log(10)/400)
  qv2 <- (log(10)/400)^2
  qip3 <- 3 * (qv/pi)^2
  
  # Unique Team's gDevs
  gdevs <- 1/sqrt(1 + qip3 * cdevsteams)
  
  # Create Empty Vectors
  for (k in 1:numplay) {
    escore[k] <- 0
    ascore[k] <- 0
    dval[k] <- 0
    ptsupd[k] <- 0
    l1t[k] <- 0
  }
  
  for (k in 1:numrow) {
    # Cumulative Scores for each player
    ascore[white1[k]] <- ascore[white1[k]] + score[k]
    ascore[white2[k]] <- ascore[white2[k]] + score[k] 
    ascore[black1[k]] <- ascore[black1[k]] + 1 - score[k]
    ascore[black2[k]] <- ascore[black2[k]] + 1 - score[k] 
    
    ## WHITE 
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevs[black1[k]] * (cratsteams[white1[k]] - cratsteams[black1[k]] ))/400))
    escore[white1[k]] <- escore[white1[k]] + escorek
    escore[white2[k]] <- escore[white2[k]] + escorek
    
    # 1/d^2 - dval
    dval[white1[k]] <- dval[white1[k]] + qv2 * gdevs[black1[k]]^2 * escorek * (1 - escorek)
    dval[white2[k]] <- dval[white2[k]] + qv2 * gdevs[black1[k]]^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj)) - Exp gain
    dscore[white1[k]] <- dscore[white1[k]] +  gdevs[black1[k]] * weights[k] * (score[k] + bval - escorek)
    dscore[white2[k]] <- dscore[white2[k]] +  gdevs[black1[k]] * weights[k] * (score[k] + bval - escorek)
    
    # r-r_j
    l1t[white1[k]] <- l1t[white1[k]] + cratsteams[black1[k]] - cratsteams[white1[k]]
    l1t[white2[k]] <- l1t[white2[k]] + cratsteams[black1[k]] - cratsteams[white1[k]]
    
    ## BLACK
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevs[white1[k]] * (cratsteams[black1[k]] - cratsteams[white1[k]]))/400))
    escore[black1[k]] <- escore[black1[k]] + escorek
    escore[black2[k]] <- escore[black2[k]] + escorek
    
    # 1/d^2
    dval[black1[k]] <- dval[black1[k]] + qv2 * gdevs[white1[k]]^2 * escorek * (1 - escorek)
    dval[black2[k]] <- dval[black2[k]] + qv2 * gdevs[white1[k]]^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
    dscore[black1[k]] <- dscore[black1[k]] +  gdevs[white1[k]] * weights[k] * (1 - score[k] + bval - escorek)
    dscore[black2[k]] <- dscore[black2[k]] +  gdevs[white1[k]] * weights[k] * (1 - score[k] + bval - escorek)
    
    # r-r_j
    l1t[black1[k]] <- l1t[black1[k]] + cratsteams[white1[k]] - cratsteams[black1[k]]
    l1t[black2[k]] <- l1t[black2[k]] + cratsteams[white1[k]] - cratsteams[black2[k]]
    
    
  }
  
  dscore[(numplay+ 1):(2 * numplay)] <- dval
  
  lout <- list(dscore,ascore, l1t)
  return(lout)
}

glicko_c_byrow <- function(numplay, numrow, white1, white2, black1, black2, score, cdevsteams, cratsteams,dscore = double(2*numplay),weights,bval) { # Capire perchè 2*np
  escore <- numeric(numplay)
  ascore <- numeric(numplay)
  dval <- numeric(numplay)
  ptsupd <- numeric(numplay)
  l1t <- numeric(numplay)
  escorek <- 0
  qv <- (log(10)/400)
  qv2 <- (log(10)/400)^2
  qip3 <- 3 * (qv/pi)^2
  
  # Unique Team's gDevs
  
  # Create Empty Vectors
  for (k in 1:numplay) {
    escore[k] <- 0
    ascore[k] <- 0
    dval[k] <- 0
    ptsupd[k] <- 0
    l1t[k] <- 0
  }
  
  for (k in 1:numrow) {
    gdevswhite <- 1/sqrt(1 + qip3 * sqrt((cdevsteams[white1[k]]^2 + cdevsteams[white2[k]]^2)/2))
    gdevsblack <- 1/sqrt(1 + qip3 * sqrt((cdevsteams[black1[k]]^2 + cdevsteams[black2[k]]^2)/2))
    
    # Cumulative Scores for each player
    ascore[white1[k]] <- ascore[white1[k]] + score[k]
    ascore[white2[k]] <- ascore[white2[k]] + score[k] 
    ascore[black1[k]] <- ascore[black1[k]] + 1 - score[k]
    ascore[black2[k]] <- ascore[black2[k]] + 1 - score[k] 
    
    ## WHITE 
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevswhite * (mean(c(cratsteams[white1[k]],cratsteams[white2[k]])) - mean(c(cratsteams[black1[k]],cratsteams[black2[k]])) ))/400))
    escore[white1[k]] <- escore[white1[k]] + escorek
    escore[white2[k]] <- escore[white2[k]] + escorek
    
    # 1/d^2 - dval
    dval[white1[k]] <- dval[white1[k]] + qv2 * gdevsblack^2 * escorek * (1 - escorek)
    dval[white2[k]] <- dval[white2[k]] + qv2 * gdevsblack^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj)) - Exp gain
    dscore[white1[k]] <- dscore[white1[k]] + gdevsblack * weights[k] * (score[k] + bval - escorek)
    dscore[white2[k]] <- dscore[white2[k]] + gdevsblack * weights[k] * (score[k] + bval - escorek)
    
    # r-r_j
    l1t[white1[k]] <- l1t[white1[k]] + mean(c(cratsteams[black1[k]], cratsteams[black2[k]])) - mean(c(cratsteams[white1[k]],cratsteams[white2[k]]))
    l1t[white2[k]] <- l1t[white2[k]] + mean(c(cratsteams[black1[k]], cratsteams[black2[k]])) - mean(c(cratsteams[white1[k]],cratsteams[white2[k]]))
    
    ## BLACK
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevsblack * (mean(c(cratsteams[black1[k]], cratsteams[black2[k]])) - mean(c(cratsteams[white1[k]],cratsteams[white2[k]])) ))/400))
    escore[black1[k]] <- escore[black1[k]] + escorek
    escore[black2[k]] <- escore[black2[k]] + escorek
    
    # 1/d^2
    dval[black1[k]] <- dval[black1[k]] + qv2 * gdevswhite^2 * escorek * (1 - escorek)
    dval[black2[k]] <- dval[black2[k]] + qv2 * gdevswhite^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
    dscore[black1[k]] <- dscore[black1[k]] +  gdevswhite * weights[k] * (1 - score[k] + bval - escorek)
    dscore[black2[k]] <- dscore[black2[k]] +  gdevswhite * weights[k] * (1 - score[k] + bval - escorek)
    
    # r-r_j
    l1t[black1[k]] <- l1t[black1[k]] + mean(c(cratsteams[white1[k]],cratsteams[white2[k]])) - mean(c(cratsteams[black1[k]], cratsteams[black2[k]]))
    l1t[black2[k]] <- l1t[black2[k]] + mean(c(cratsteams[white1[k]],cratsteams[white2[k]])) - mean(c(cratsteams[black1[k]], cratsteams[black2[k]]))
    
    
  }
  
  dscore[(numplay+ 1):(2 * numplay)] <- dval
  
  lout <- list(dscore,ascore, l1t)
  return(lout)
}

apply_pen <- function(dfout, time){
  toBePen <- which(time-dfout$LastPen-dfout$LastTourn>42)
  dfout$Rating[toBePen] <- round(dfout$Rating[toBePen]*0.95)
  dfout$LastPen[toBePen] <- time
  return(dfout)
}

## Visualize ####

ratingtable <- function(rat, lastT =NULL, minDev = NULL, topw = "Chiara Pernigo", nTournMin = 1, Inc = FALSE, prevrat = FALSE, peakElo = NULL){
  library(foreach)
  elo_sesso <- read_excel("data/Extra/genere_elo_ir.xlsx") 
  
  final_ita <- rat$ita$ratings
  # final_ita$Deviation <- update_devs(rat$ita,minTourn = 0)$`Today.s.Dev`
  final_ita$Inc <-case_when(
    final_ita$Deviation <80 ~ "Bassa",
    final_ita$Deviation > 100 ~ "Alta",
      TRUE~"Media"
    )
  final_ita <-  final_ita %>% mutate_if(is.numeric, function(x) round(x,0))
  if(!is.null(minDev) & is.null(lastT)) ratingtable <-  merge(final_ita[final_ita$nTourn >nTournMin & final_ita$Deviation<minDev,],elo_sesso)
  if(!is.null(lastT) & is.null(minDev)) ratingtable <-  merge(final_ita[final_ita$nTourn >nTournMin & final_ita$LastTourn>lastT,],elo_sesso)
  if(!is.null(lastT) & !is.null(minDev)) ratingtable <-  merge(final_ita[final_ita$nTourn >nTournMin & final_ita$LastTourn>lastT & final_ita$Deviation<minDev,],elo_sesso)
  if(is.null(lastT) & is.null(minDev)) ratingtable <-  merge(final_ita[final_ita$nTourn >nTournMin,],elo_sesso)
  
  rat$history <- do.call(rbind,lapply(rat$history, function(x) x %>% mutate(giocatore = rownames(x))))
  ratingtable <-  ratingtable[order(ratingtable$gen,ratingtable$Rating,decreasing = T),]
  ratingtable$`Win%` = paste(round(ratingtable$Win/ratingtable$Games*100,0),"%",sep = "")
  ratingtable$Improv <- foreach(name = ratingtable$giocatore) %do% ifelse(name %in% rat$history$giocatore,rat$history$Rating[rat$history$giocatore == name],"-")
  
  if(prevrat){
    a <- rat$ita$impr
    a.unlist <- do.call(rbind, a)
    ratingtable$Posit <- a.unlist$Improv[match(ratingtable$giocatore, a.unlist$giocatore)]
    ratingtable$Posit <- ifelse(is.na(ratingtable$Posit), "=", ratingtable$Posit)
    ratingtable <- ratingtable[,c("Posit", "giocatore", "Rating", "Games", "Win", "Win%", "nTourn","Improv", "Inc", "club")] 
  }else{
  ratingtable <- ratingtable[,c("giocatore", "Rating", "Games", "Win","Draw", "Win%", "nTourn","Improv", "Inc", "club")] 
  }
  
  nm <- which(ratingtable$giocatore == topw)-1
  nf <- nrow(ratingtable)-nm
  rownames(ratingtable) <- c(paste("m",1:nm, sep = ""),paste("f",1:nf, sep = ""))
  
  if(!Inc) ratingtable <- ratingtable[,-which(colnames(ratingtable)== "Inc")] 
  
 
  return(ratingtable)
  
}

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

new_ita_players <- function(names){
  load("data/Extra/ita_players.rda")
  ita_players <<- c(ita_players, names)
  save(ita_players, file="data/Extra/ita_players.rda")
}

order_history <- function(rat){
    output <- by(rat$history, rat$history$Category, function(x) x[order(x$Rating, decreasing = TRUE), ])
    return(output)
}

improvements <- function(rat,prev.rat, sort = c("Gained","New"), it = FALSE){
  if(it){
    elo_sesso <- read_excel("data/Extra/genere_elo_ir.xlsx")[,c(1,2)]
    one_year_ago_week <- weeks_passed(year(today()), month(today()), day(today()))-52
    
    
    keepNames <- ratingtable(rat, nTournMin = 2 ,lastT = one_year_ago_week)$giocatore
    ratMerged <- merge(rat$ratings, elo_sesso)
    ratShow <-  
      ratMerged%>% 
      filter(giocatore %in% keepNames) %>% 
      group_by(gen) %>% 
      mutate(rank = rank(-Rating)) %>% 
      ungroup %>% 
      as.data.frame
    ratFull <- rbind(ratShow, mutate(filter(ratMerged, !giocatore %in% keepNames), rank = NA))
    
    
    prevKeepNames <- ratingtable(prev.rat, nTournMin = 2 ,lastT = one_year_ago_week)$giocatore
    prevRatMerged <- merge(prev.rat$ratings, elo_sesso)
    prevRatShow <-  
      prevRatMerged%>% 
      filter(giocatore %in% prevKeepNames) %>% 
      group_by(gen) %>% 
      mutate(rank = rank(-Rating)) %>% 
      ungroup %>% 
      as.data.frame
    prevRatFull <- rbind(prevRatShow, mutate(filter(prevRatMerged, !giocatore %in% prevKeepNames), rank = NA))
    
    rat$history <- do.call(rbind,lapply(rat$history, function(x) x %>% mutate(giocatore = rownames(x))))
    curPl <-  rat$history$giocatore
    curMatch <- match(curPl,prev.rat$ratings$giocatore)
    
    prevRatStatus <- na.omit(prev.rat$ratings[curMatch,1:3])
    prevRatStatus <- rbind(prevRatStatus,
                           data.frame(
                             giocatore = curPl[!curPl %in% prev.rat$ratings$giocatore],
                             Rating = rep("-", length(curPl[!curPl %in% prev.rat$ratings$giocatore])),
                             Deviation = rep("-", length(curPl[!curPl %in% prev.rat$ratings$giocatore])))
    )
    
    curMatchRat <- match(curPl,rat$ratings$giocatore)
    ratStatus <- rat$ratings[curMatchRat, 1:3]
    
    ratStatus <- ratStatus[order(ratStatus$giocatore),]
    prevRatStatus <- prevRatStatus[order(prevRatStatus$giocatore),]
    
    
    rankMatch <- match(ratStatus$giocatore, ratFull$giocatore)
    prevRankMatch <- match(prevRatStatus$giocatore, prevRatFull$giocatore)
    
    
    impr <- data.frame(
      giocatore = ratStatus$giocatore,
      PrevRat = prevRatStatus$Rating,
      GainedRat = rat$history$Rating[match(ratStatus$giocatore, rat$history$giocatore)],
      NewRat = ratStatus$Rating,
      `.` = "|",
      PrevRank = round(prevRatFull$rank[match(ratStatus$giocatore, prevRatFull$giocatore)]),
      NewRank = round(ratFull$rank[match(ratStatus$giocatore, ratFull$giocatore)])
    )
    impr$Improv <- with(impr, ifelse(PrevRank-NewRank ==0, "=",ifelse(PrevRank-NewRank>0, paste("+",sep = "", PrevRank-NewRank),PrevRank-NewRank)))
    impr$Improv <- with(impr, ifelse(is.na(PrevRank) & !is.na(NewRank), "!NEW!", Improv))
    impr$Improv <- with(impr, ifelse(is.na(PrevRank) & is.na(NewRank), "-", Improv))
    impr <- impr %>% relocate(Improv, .before = NewRank)
    impr$cat <- rat$history$Category[match(impr$giocatore, rat$history$giocatore)]
    impr$PrevRank <- ifelse(is.na(impr$PrevRank), "-", impr$PrevRank)
    impr$NewRank <- ifelse(is.na(impr$NewRank), "-", impr$NewRank)
    
    impr <- impr %>% filter(giocatore %in% elo_sesso$giocatore)
    
    if(missing(sort)) sort <- "New"
    if(sort == "Gained" )  out <- by(impr,impr$cat, function(x) x[order(x$GainedRat, decreasing = TRUE),-which(colnames(x) == "cat")])
    if(sort == "New" )  out <- by(impr,impr$cat, function(x) x[order(x$NewRat, decreasing = TRUE),-which(colnames(x) == "cat")])
    
    
  }else{
    status <- prev.rat$ratings
    
    rat$history <- do.call(rbind,lapply(rat$history, function(x) x %>% mutate(giocatore = rownames(x))))

    plays <- rat$history$giocatore
    matches <- match(plays,status$giocatore)
    
    prevrat <- status[na.omit(matches),1:3]
    prevrat <- rbind(prevrat,
                     data.frame(
                       giocatore = plays[!plays %in% status$giocatore],
                       Rating = rep("-", length(plays[!plays %in% status$giocatore])),
                       Deviation = rep("-", length(plays[!plays %in% status$giocatore])))
    )
    prevrat<- prevrat[match(plays,prevrat$giocatore),]
    
    
    prevrat$GainedRat <- rat$history$Rating
    prevrat$NewRat <- rat$ratings$Rating[match(plays, rat$ratings$giocatore)]
    prevrat$NewDev <- rat$history$Deviation
    prevrat$cat <- rat$history$Category
    prevrat$`.` <- "|"
    
    prevrat <- prevrat[,c("giocatore","Rating","GainedRat","NewRat",".","Deviation","NewDev","cat")] 
    if(missing(sort)) sort <- "New"
    if(sort == "Gained" )  out <- by(prevrat,prevrat$cat, function(x) x[order(x$GainedRat, decreasing = TRUE),-which(colnames(x) == "cat")])
    if(sort == "New" )  out <- by(prevrat,prevrat$cat, function(x) x[order(x$NewRat, decreasing = TRUE),-which(colnames(x) == "cat")])
    
  }
  return(out)
  
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

search_match <- function(athlete, person_to_look_for) {
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

## Graphs ####


rat_histr <- function(dft, week, arch){
  

  
  filtGioc <- get(dft[nrow(dft),1])$ratings %>%
    filter(nTourn > 1, LastTourn > week) %>%
    pull(giocatore)
  
  filtered_list <- lapply(dft$tornei, function(ds) {
    get(ds)$ratings %>% filter(giocatore %in% filtGioc)
  })
  
  
  histr <- histry_merge(
    list = filtered_list,
    giocatori = filtGioc
  )
  
  # nt <- ncol(histr[,,"De <- viation"])
  # ratinghistr = ifelse(histr[order(histr[,nt,"Deviation"],decreasing = T),1:nt,"Deviation"] == 0,
  #                      NA,
  #                      histr[order(histr[,nt,"Deviation"],decreasing = T),1:nt,"Deviation"])
  nt <- ncol(histr[,,"Rating"])
  ratinghistr = ifelse(histr[order(histr[,nt,"Rating"],decreasing = T),1:nt,"Rating"] == 1350.7654,
                       NA,
                       histr[order(histr[,nt,"Rating"],decreasing = T),1:nt,"Rating"])
  
  for(j in 1:nrow(ratinghistr)){
    for(i in 1:(nt-1)) {
      ratinghistr[j,i] = ifelse(is.na(ratinghistr[j,i]) & !is.na(ratinghistr[j,i+1]),as.integer(str_extract(arch[[rownames(ratinghistr)[j]]][[1]][nrow(arch[[rownames(ratinghistr)[j]]][[1]][]), 2], "[0-9]+")),ratinghistr[j,i])
      ratinghistr[j,i] = ifelse(is.na(ratinghistr[j,i]) & is.na(ratinghistr[j,i+1]),NA_integer_,ratinghistr[j,i])
    }
  }
  colnames(ratinghistr) <- c("",dft$TourName)
  
  save(ratinghistr, file = "D:/Personal Statistics/rcb/Ranking/data/Extra/RatingHistory.rda")
  return(ratinghistr)
}

glicko_graphs <- function(player, second.player = NULL, third.player = NULL, ...){
  library(ggtext)
  
  nn <- list(...)
  if("nom" %in% names(nn)) nom <- nn$nom else nom <-  .2 
  if("size" %in% names(nn)) size <- nn$size else size <-  1
  if("arch" %in% names(nn)) arch <- nn$arch else readRDS("shiny/www/Archive.RDS")
  if("dft" %in% names(nn)) dft <- nn$dft else dft <- order_datasets()
  if("eug" %in% names(nn)) eug <- nn$eug else readRDS("shiny/www/EU_dataset.RDS")
  if("ratinghistr" %in% names(nn)) ratinghistr <- nn$ratinghistr else ratinghistr <-  rat_histr(order_datasets(), 0, arch = arch)
  
  
  dft$date <- as.Date(sapply(dft$tornei, function(ds)
    date_from_weeks(unique(get(str_replace(ds, "rs", "")[1])$Time))
  ))
  
  gt <- names(arch[[player]])
  plnat <- eug %>% filter(giocatore == player) %>% pull(Nazione)
  
  if(is.na(plnat) || plnat == "0" || is_empty(plnat)){  
    natpl <- NULL
    nt = 1
  }else{
    natpl <- eug %>% filter(Nazione == plnat) %>% pull(giocatore)
    nt <- length(natpl)
  }  
  
  
  
  p <- ratinghistr%>%
    as_tibble %>%
    rename_if(is.double, function(x) paste("tourn",x, sep = "")) %>%
    mutate(name = rownames(ratinghistr)) %>%
    pivot_longer(cols = starts_with("tourn"), names_to = "tourn",values_to= "Rating") %>%
    filter(name %in% c(natpl,player, second.player, third.player)) %>% 
    mutate(Tournament = str_replace(tourn, "tourn", ""),
           Date = as.Date(dft$date[match(Tournament, dft$TourName)]),
           alphafil = ifelse(name %in% c(player, second.player, third.player),"a","b"),
           colorefil= ifelse(name == player, "indianred", "grey20")
    )
  
  col_vals <- c("white","indianred")
  
  if(!is.null(second.player)){
    p <- p %>% mutate(
      colorefil= case_when(
        name  == second.player ~ "blue",
        TRUE ~ colorefil
      ))
    
    vs <- " vs "
    col_vals <- c("blue", "white", "indianred")
  }else{
    vs <- NULL
  }
  
  if(!is.null(third.player)){
    p <- p %>% mutate(
      colorefil= case_when(
        name == third.player ~ "green4",
        TRUE ~ colorefil
      ))
    
    vs2 <- " vs "
    col_vals <- c("blue", "green4", "white", "indianred")
  }else{
    vs2 <- NULL
  }
  
  # p <- 
  p %>% 
    filter(Tournament!="V1") %>%
    ggplot(aes(x = Date, y = Rating, fill = name)) +
    geom_line(linewidth = size,aes(col = colorefil,alpha = alphafil),show.legend = F)+
    scale_color_manual(values = col_vals)+
    scale_alpha_manual(values = c(1,nom/log(nt)))+
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
    theme(plot.title = element_text(family = "Fira Sans Condensed"),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = c(0.815, 0.27)
    )+
    ggtitle(
      paste0(
        "<span style='color:indianred;'>", player, "</span>",
        vs,
        "<span style='color:blue;'>", second.player, "</span>",
        vs2,
        "<span style='color:green4;'>", third.player, "</span>"
      )
    ) +
    theme(
      plot.title = element_markdown(size = 16, hjust = 0)
    )
  
  
  
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
  return(histry)
  
}




## Others ####

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

output_opprats <- function(x, status = NULL,rdmax = 200, cval = 14,ita = T, lambda = 1/20){
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  np <- length(giocatori)
  nm <- length(unique(x$cat))
  
  cat_values <- NULL
  for (i in seq_along(giocatori)) {
    giocatorei <- giocatori[i]
    cat_values[i] <- unique(x$cat[which(giocatorei == x$Giocatore1 | giocatorei == x$Giocatore2 | giocatorei == x$Giocatore3 | giocatorei == x$Giocatore4)])
  }
  names(cat_values) <- giocatori
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  if(!exists("initval")) load("R/initval.rda")
  
  
  if (!is.null(status)) {
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           # Rating = rep(init[1], length(npadd)),
                           Rating = as.numeric(initval$initrats[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           # Deviation = rep(init[2], length(npadd)), 
                           Deviation = as.numeric(initval$initdev[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           Games = zv, Win = zv,  Loss = zv,nTourn = zv,
                           LastTourn = rep(time,length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = time)
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win", "Loss", "nTourn","LastTourn")], npstatus)
    
    rinit <- ifelse(status[[2]]<700,700,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    nloss <- status[[6]]
    ntourn <- status[[7]]
    nlasttourn <- status[[8]]
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    # rinit <- rep(init[1], length.out = np)
    # dinit <- rep(init[2], length.out = np)
    rinit <- as.numeric(initval$initrats[match(cat_values,initval$categories)])
    dinit <- as.numeric(initval$initdev[match(cat_values,initval$categories)])
    ngames <- nwin <- nloss <- rep(0, length.out = np)
    ntourn <- rep(0, np)
    nlasttourn <- rep(time,np)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<- giocatori
  }
  
  
  
  
  curplay <- match(giocatori, names(rinit))
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1 
  nlasttourn <- nlasttourn[curplay]
  
  
  histry <- array(NA,
                  dim = c(np, 6),
                  dimnames = list(giocatori, 
                                  c("Rating", "Deviation", "Games", "Wins","CatRating","Category")
                  ))
  histry<- as.data.frame(histry)
  
  qv <-  log(10)/400
  qip3 <- 3 * (qv/pi)^2
  
  nameteams <- data.frame(G1 = c(x$Giocatore1,x$Giocatore3),G2 = c(x$Giocatore2,x$Giocatore4))
  sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
  split_cols <- strsplit(sq, " ")
  g1 <- as.numeric(sapply(split_cols, `[`, 1))
  g2 <- as.numeric(sapply(split_cols, `[`, 2))
  
  # Unique Team's Rating
  cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
  cratsteams <- cratsteams[sort(names(cratsteams))]
  
  # Unique Team's Deviation
  # cdevs <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
  cdevs <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
  
  cdevsteams <- c(sqrt((cdevs[g1]^2+cdevs[g2]^2)/2),sqrt((cdevs[g2]^2+cdevs[g1]^2)/2))
  cdevsteams <- cdevsteams[sort(names(cdevsteams))]
  
  x <- split(x, x$cat)
  players <- lapply(x, function(y) unique(c(y$Giocatore1, y$Giocatore2, y$Giocatore3, y$Giocatore4)))
  
  for (i in 1:nm) {
    traini <- x[[i]]
    nr <- nrow(traini)
    
    playi <- players[[i]]
    
    ngamesi <- tabulate(c(traini$Giocatore1, traini$Giocatore2,traini$Giocatore3, traini$Giocatore4), np)
    
    dscores <- opp_rat(numplay = np, numrow = nr, white1 = traini$Giocatore1, white2 = traini$Giocatore2,
                        black1 = traini$Giocatore3,black2 = traini$Giocatore4, score = traini$Score,
                        cdevsteams = cdevsteams, cratsteams =  cratsteams,weights=traini$Weight, bval = bval)
    
    diffs <- dscores[[1]]
    sums <- dscores[[2]]
    
    names(diffs) <- names(sums) <- giocatori
    
    ngamesi <- tabulate(c(traini$Giocatore1,traini$Giocatore2,traini$Giocatore3,traini$Giocatore4), np)
    
    diffs <- diffs/ngamesi
    sums <- sums/ngamesi
  }
  lst <- list(diffs,sums)
  return(lst)
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


## EU functions ####

exportEU<- function(rat, nTournMin = 0, minGames = 0, lastT = 0){
  
  NatData <- read_excel( "shiny/www/NatDatabase.xlsx")[, c("giocatore", "Sesso", "Nazione")]
  NatData$giocatore <- stringi::stri_trans_general(str_to_title(NatData$giocatore), "Latin-ASCII")
  
  rat <- subset(rat$ratings, nTourn >nTournMin &  Games > minGames & LastTourn > lastT) 
  
  NatNA <- NatData[which(is.na(match(NatData$giocatore, rat$giocatore))),]
  rat <- left_join(rat, NatData, by = "giocatore")
  rat <- rat %>%  mutate(LastPen = as.Date(ifelse(LastPen == 0, NA_Date_, date_from_weeks(LastPen))))
  
  na_players <- rat[is.na(rat$Sesso) | is.na(rat$Nazione), ]
  
  if (nrow(na_players) == 0) {
    return(rat) 
  }
  
  current_index <- 1  
  
  while (current_index <= nrow(na_players)) {
    player_name <- na_players$giocatore[current_index]
    cat("Giocatore:", player_name, "\n")
    
    while (TRUE) {
      choice <- readline("Choose action (i/s/b/e): ")
      
      if (tolower(choice) == "i") {
        sesso <- readline("Sesso? (M/F): ")
        nazione <- readline("Nazione? ")
        
        rat$Sesso[rat$giocatore == player_name] <- toupper(sesso)
        rat$Nazione[rat$giocatore == player_name] <- toupper(nazione)
        current_index <- current_index + 1  
        break 
      } else if (tolower(choice) == "s") {
        current_index <- current_index + 1  
        break  
      } else if (tolower(choice) == "b") {
        if (current_index > 1) {
          current_index <- current_index - 1  
        } else {
          cat("No previous player to go back to.\n")
        }
        break  
      } else if (tolower(choice) == "e") {
        print(paste0("NA Player Left: ",sum(is.na(rat$Nazione)), sep = ""))
        NatDatabase <- rbind(rat[, c("giocatore", "Sesso",  "Nazione")],NatNA[, c("giocatore", "Sesso", "Nazione")])
        write.xlsx(NatDatabase, file ="shiny/www/NatDatabase.xlsx",row.names = FALSE)
        return(rat) 
      } else {
        cat("Invalid option. Please choose again.\n")
      }
    }
    # print(paste0("NA Player Left: ",sum(is.na(rat$Nazione)), sep = ""))
  }
  
  NatDatabase <- rbind(rat[, c("giocatore", "Sesso",  "Nazione")],NatNA[, c("giocatore", "Sesso", "Nazione")])
  write.xlsx(NatDatabase, file ="shiny/www/NatDatabase.xlsx")
  
  return(rat)
}

# Archive Function ####
archive <- function(keep = T, percentages = T){
  dft <- order_datasets()
  pb = txtProgressBar(min = 0, max = nrow(dft), initial = 0, style = 3) 
  setTxtProgressBar(pb,0)

  arch <- perch <- NULL
  for(t in 1:nrow(dft)){
    initval <- init_control(t, dft)
    obj_name <- dft$tornei[t]
    prev_obj <- dft$Status[t]   
    extra    <- dft$rbindStatus[t]
    
    
    
    ratings_in <- if(t == 1)  NULL else  get(prev_obj)$ratings
    
    if (extra != "")  ratings_in <- rbind(ratings_in, get(extra))
    
    if(all(unique(get(obj_name)$cat) %in% param$byrow) & !is.null(param$byrow)) param$byrow <-  "all" 
    
    
    if(keep)    arch  <-tryCatch(glicko_keep(get(obj_name), 
                                             status = ratings_in , 
                                             TourName = dft$TourName[t], 
                                             arch = arch, 
                                             initval = initval ),      error = function(e) message(sprintf("Error at Tournament %s",dft[t,1])))
    
    if(percentages) perch <- tryCatch(glicko_percentages(get(obj_name), 
                                                         status = ratings_in , 
                                                         TourName = dft$TourName[t], 
                                                         perch = perch, 
                                                         initval = initval),      error = function(e) message(sprintf("Error at Tournament %s",dft[t,1])))
    
    
    if(is.null(perch) & percentages){
      message(sprintf("Error at Tournament %s",dft[t,1]))
      break()
    }
    
    setTxtProgressBar(pb,t)
  }
  out <- list(arch, perch)
  return(out)
}

init_control <- function(t, dft, tornei = c("rsERSChmp23", "rsPadova24WS")){

    initval <- Init23
    initval <- rbind(initval, c(categories = "German Contender", initrats = 1300, initdev = 250))
    initval[initval$categories %in%  c("Open Pro", "Open Contender", "Pro German"),2:3] <- matrix(c(1600,1450, 1350,300,300,175),3,2) # Full Update
    init_23 <- initval
    
    initval <- Init24
    initval <- rbind(initval, c(categories = "German Contender", initrats = 1300, initdev = 250))
    initval <- rbind(initval, c(categories = "1.Bundesliga", initrats = 1200, initdev = 150))
    initval <- rbind(initval, c(categories = "1.Swissliga", initrats = 1200, initdev = 150))
    initval <- rbind(initval, c(categories = "2.Bundesliga", initrats = 1100, initdev = 150))
    initval[initval$categories %in%  c("Open Pro","Pro German" ,"Open Contender", "Open"),2:3] <- matrix(c(1600,1450,1350,1200,300,200,200,200),4,2) # Full Update
    init_24 <- initval
    
    initval <- rbind(initval, c(categories = "Gardens", initrats = 1050, initdev = 75))
    initval <- rbind(initval, c(categories = "Lega Roundnet A", initrats = 1200, initdev = 150))
    initval <- rbind(initval, c(categories = "Lega Roundnet B", initrats = 1000, initdev = 150))
    initval[initval$categories %in%  c("Open Contender", "Women's Contender"),2:3] <- matrix(c(1250,1150,200,200),2,2) 
    
    init_25 <- initval
    
  if(t <= which(dft$tornei == tornei[1])){
    initval <- init_23
  }else if(t <= which(dft$tornei == tornei[2])){
    initval <- init_24
  }else{
    initval <- init_25
  }
  
  return(initval)
}

glicko_keep <- function(rs, status = NULL, TourName = NULL, arch = arch, initval = initval){
  
  x <- as.data.frame(rs$data)
  lambda <- rs$param$lambda
  cval <- rs$param$cval
  bval <- rs$param$bval
  rdmax <- rs$param$rdmax
  cat_to_unite <- rs$param$cat_to_unite
  byrow <- rs$param$byrow
  
  # Start Data-Manipulation
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  giocatori <- stringi::stri_trans_general(str_to_title(giocatori), "Latin-ASCII")
  x <-  x %>% mutate(across(contains("Giocatore"), ~stringi::stri_trans_general(str_to_title(.), "Latin-ASCII")))
  
  # Va applicato anche a Giocatore1,...,Giocatore4
  
  np <- length(giocatori)
  nm <- length(unique(x$cat))
  
  # Assigning Division for Initialisation
  cat_values <- NULL
  for(i in seq_along(giocatori)) {
    giocatorei <- giocatori[i]
    cat_values[i] <- unique(x$cat[which(giocatorei == x$Giocatore1 | giocatorei == x$Giocatore2 | giocatorei == x$Giocatore3 | giocatorei == x$Giocatore4)])[1]
  }
  names(cat_values) <- giocatori
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  
  # Status
  if(!exists("initval")) load("R/initval.rda")
  if (!is.null(status)) {
    status$giocatore <- stringi::stri_trans_general(str_to_title(status$giocatore), "Latin-ASCII")
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           # Rating = rep(init[1], length(npadd)),
                           Rating = as.numeric(initval$initrats[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           # Deviation = rep(init[2], length(npadd)), 
                           Deviation = as.numeric(initval$initdev[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           Games = zv, Win = zv, Draw= zv, Loss = zv,nTourn = zv,
                           LastTourn =rep(date_from_weeks(time),length(npadd)),
                           LastPen = zv, PeakElo = zv, Percentile = zv, PeakRank = 1/zv, 
                           PeakTime = rep(date_from_weeks(time), length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Draw" %in% names(status))) 
      status <- cbind(status, Draw = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = date_from_weeks(time))
    if (!("LastPen" %in% names(status))) 
      status <- cbind(status, LastPen = 0)
    if (!("PeakElo" %in% names(status))) 
      status <- cbind(status, PeakElo = 0)
    if (!("Percentile" %in% names(status))) 
      status <- cbind(status, Percentile = 0)
    if (!("PeakRank" %in% names(status))) 
      status <- cbind(status, PeakRank = Inf, PeakTime = 0)
    if (!("PeakTime" %in% names(status))) 
      status <- cbind(status, PeakTime = date_from_weeks(time))
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win","Draw", "Loss", "nTourn","LastTourn", "LastPen", "PeakElo","Percentile", "PeakRank", "PeakTime")], npstatus)
    
    rinit <- ifelse(status[[2]]<700,700,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    ndraw <- status[[6]]
    nloss <- status[[7]]
    ntourn <- status[[8]]
    nlasttourn <- status[[9]]
    lastpen <- status[[10]]
    peakelo <- status[[11]]
    peakrank <- status[[13]]
    peaktime <- as.Date(status[[14]])
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    # rinit <- rep(init[1], length.out = np)
    # dinit <- rep(init[2], length.out = np)
    rinit <- as.numeric(initval$initrats[match(cat_values,initval$categories)])
    dinit <- as.numeric(initval$initdev[match(cat_values,initval$categories)])
    ngames <- nwin <-ndraw <-  nloss <- rep(0, length.out = np)
    ntourn <- rep(0, np)
    nlasttourn <- rep(time,np)
    lastpen <- rep(0, np)
    peakelo <- rep(0, np)
    peakrank <- rep(0, np)
    peaktime <- rep(date_from_weeks(time), np)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<-names(lastpen) <-  giocatori
  }
  
  # Players Active in this Event
  curplay <- match(giocatori, names(rinit))
  
  nlasttourn <- weeks_passed(date = nlasttourn)
  
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  ondraw <- ndraw[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  olastpen <- lastpen[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  ndraw <- ndraw[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1 
  nlasttourn <- nlasttourn[curplay]
  nlastpen <- lastpen[curplay]
  
  histry <- array(NA,
                  dim = c(np, 7),
                  dimnames = list(giocatori, 
                                  c("Rating", "Deviation", "Games", "Wins","Draws", "CatRating","Category")
                  ))
  histry<- as.data.frame(histry)
  
  # Costants
  qv <-  log(10)/400
  qip3 <- 3 * (qv/pi)^2
  
  # Dynamic Teammate Changing Scenatio 
  if(is.null(byrow)){
    nameteams <- data.frame(G1 = c(x$Giocatore1,x$Giocatore3),G2 = c(x$Giocatore2,x$Giocatore4))
    sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
    split_cols <- strsplit(sq, " ")
    g1 <- as.numeric(sapply(split_cols, `[`, 1))
    g2 <- as.numeric(sapply(split_cols, `[`, 2))
    
    cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
    cratsteams <- cratsteams[order(names(cratsteams))]
    
    cdevs <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn)),rdmax)
    cdevsteams <- c(sqrt((cdevs[g1]^2+cdevs[g2]^2)/2),sqrt((cdevs[g2]^2+cdevs[g1]^2)/2)) # Actual
    
    cdevs_mid <- numeric(length(c(g1,g2)))
    for(i in 1:np){
      cdt <- cdevsteams[i]
      cdp1 <- cdevs[c(g1,g2)[i]]
      cdp2 <- cdevs[c(g2,g1)[i]]
      cdevs_mid[i] <- ifelse(cdt<80 & cdp1 <80 & cdp2<80, cdt, cdevs[c(g1,g2)[i]])
    }
    names(cdevs_mid) <- giocatori[c(g1,g2)]
    cdevsteams <- cdevsteams[order(names(cdevsteams))]
    cdevs_mid <- cdevs_mid[order(names(cdevs_mid))]
    
    compagni <- data.frame(
      g1 = c(giocatori[g1],giocatori[g2]),
      g2 = c(giocatori[g2],giocatori[g1])
    )
    compagni <- compagni[order(compagni$g1),]
    
  }else if(all(unique(cat_values)%in% byrow)){
    cdevs_mid <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
    cratsteams <- crats
    byrow <- unique(x$cat)
    
    ## Non sappiamo cosa fare con compagni
  }else{
    cdevs <-  pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
    byrowPl <- giocatori[cat_values %in% byrow]
    
    
    nameteams <- data.frame(G1 = c(x$Giocatore1[!x$cat %in% byrow],x$Giocatore3[!x$cat %in% byrow]),G2 = c(x$Giocatore2[!x$cat %in% byrow],x$Giocatore4[!x$cat %in% byrow]))
    sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
    split_cols <- strsplit(sq, " ")
    g1 <- as.numeric(sapply(split_cols, `[`, 1))
    g2 <- as.numeric(sapply(split_cols, `[`, 2))
    
    compagni <- data.frame(
      g1 = c(giocatori[g1],giocatori[g2]),
      g2 = c(giocatori[g2],giocatori[g1])
    )
    compagni <- compagni[order(compagni$g1),]
    
    cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
    cratsteams <- cratsteams[order(names(cratsteams))]
    
    cdevsteams <- c(sqrt((cdevs[g1]^2+cdevs[g2]^2)/2),sqrt((cdevs[g2]^2+cdevs[g1]^2)/2)) # Actual
    
    cdevs_mid <- numeric(length(giocatori)-length(byrowPl))
    for(i in 1:length(cdevs_mid)){
      cdt <- cdevsteams[i]
      cdp1 <- cdevs[c(g1,g2)[i]]
      cdp2 <- cdevs[c(g2,g1)[i]]
      cdevs_mid[i] <- ifelse(cdt<80 & cdp1 <80 & cdp2<80, cdt, cdevs[c(g1,g2)[i]])
    }
    names(cdevs_mid) <- c(giocatori[g1], giocatori[g2])
    
    cdevs_mid <- c(cdevs_mid, cdevs[byrowPl])
    cdevs_mid <- cdevs_mid[order(names(cdevs_mid))]
    
    cratsteams <- c(cratsteams, crats[byrowPl])
    cratsteams <- cratsteams[order(names(cratsteams))]
  }
  
  # Possible unions of categories
  if(!is.null(cat_to_unite)){
    cat_to_remove <- str_extract(cat_to_unite, ".*(?= ->)")
    cat_into_merge <- str_extract(cat_to_unite, "(?<=-> ).*")
    x$cat[which(x$cat == cat_to_remove)] <- cat_into_merge
    nm <- length(unique(x$cat))
  }
  x <- split(x, x$cat)
  players <- lapply(x, function(y) unique(c(y$Giocatore1, y$Giocatore2, y$Giocatore3, y$Giocatore4)))
  
  keepl <- list()
  for (mm in 1:np) {
    keepl[[mm]] <- data.frame(opp1 = numeric(), opp2 = numeric(), Score = numeric(), Points = numeric())
    if(!is.null(byrow) & cat_values[mm] %in% byrow) keepl[[mm]] <- cbind(keepl[[mm]], Partner = character())
  }
  
  
  for (i in 1:nm) {
    traini <- x[[i]]
    nr <- nrow(traini)
    
    playi <- players[[i]]
    ngamesi <- tabulate(c(traini$Giocatore1,traini$Giocatore2,traini$Giocatore3,traini$Giocatore4), np)
    trainiplw <- c(traini$Giocatore1[traini$Score > .5], traini$Giocatore2[traini$Score > .5],traini$Giocatore3[traini$Score < .5],traini$Giocatore4[traini$Score < .5])
    
    if(!is.null(byrow) & unique(traini$cat) %in% byrow) f <- "glicko_c_keep_byrow" else f <- "glicko_c_keep"
    
    dscores <- do.call(f, list(numplay = np, numrow = nr, white1 = traini$Giocatore1, white2 = traini$Giocatore2,
                               black1 = traini$Giocatore3,black2 = traini$Giocatore4, score = traini$Score,
                               cdevsteams = cdevs_mid, cratsteams =  cratsteams,weights=traini$Weight, bval = bval,
                               keepl = keepl))
    
    
    cdscores <- dscores[[1]][playi]
    cdval <- dscores[[1]][(np + 1):(2 * np)][playi] # 1/d^2
    ascore <- dscores[[2]][playi]
    l1t <- dscores[[3]][playi]
    keepl <- dscores[[4]]
    
    names(ascore) <- names(cdval) <- names(cdscores) <- names(l1t) <-  giocatori[playi]
  names(keepl) <- giocatori
    for(jj in playi){
      rownames(keepl[[jj]]) <- NULL
      keepl[[jj]][,"Points"] <- round(qv/(1/cdevs_mid[giocatori[jj]]^2+ dscores[[1]][(np + 1):(2 * np)][jj]) * keepl[[jj]][,"Points"] + (lambda * dscores[[3]][jj]/ngamesi[jj])/(ngamesi[jj]))
      keepl[[jj]][,"opp1"] <- paste(giocatori[keepl[[jj]][,"opp1"]], " (", crats[keepl[[jj]][,"opp1"]], ")", sep = "")
      keepl[[jj]][,"opp2"] <- paste(giocatori[keepl[[jj]][,"opp2"]], " (", crats[keepl[[jj]][,"opp2"]], ")", sep = "")
      
      if(!is.null(byrow) & cat_values[giocatori[jj]] %in% byrow){
        keepl[[jj]][,"Partner"] <-  paste(giocatori[keepl[[jj]][,"Partner"]], " (", crats[keepl[[jj]][,"Partner"]], ")", sep = "")
        keepl[[jj]] <- rbind(keepl[[jj]], data.frame(
          opp1 = "Player:", 
          opp2 = paste(giocatori[jj], " (",crats[jj], ")", sep  =""), 
          Score = paste0(tabulate(trainiplw, np)[jj], "/", ngamesi[jj]), 
          Points =  sum(keepl[[jj]][,"Points"]),
          Partner = ""))
      # }else if(!is.null(byrow) & !cat_values[giocatori[jj]] %in% byrow){
      #   keepl[[jj]] <- rbind(keepl[[jj]], data.frame(opp1 = c("Squadra:", ""), opp2 = c(paste(compagni[jj,2], " (",crats[compagni[jj,2]] , ")", sep  =""),paste(compagni[jj,1], " (",crats[jj] , ")", sep  ="")), Score = c(paste0(tabulate(trainiplw, np)[jj], "/", ngamesi[jj]),""), Points = c(paste0("Tot:", sum(keepl[[jj]][,"Points"])),"")))
      }else{
        keepl[[jj]] <- rbind(keepl[[jj]], data.frame(
          opp1 = c("Squadra:", ""),
          opp2 = c(paste(compagni[giocatori[jj]== compagni[,1],2], " (",crats[compagni[giocatori[jj]== compagni[,1],2]] , ")", sep  =""),paste(compagni[giocatori[jj]== compagni[,1],1], " (",crats[jj] , ")", sep  ="")),
          Score = c(paste0(tabulate(trainiplw, np)[jj], "/", ngamesi[jj]),""), 
          Points = c(paste0("Tot:", sum(keepl[[jj]][,"Points"])),"")))
      }
    }
  }
  keepl$Tournament = TourName
  
  if(is.null(arch)){
    archout <- keepl
  }else if(!is.null(arch) & length(arch$Tournament) ==1){
    archout <- archappend(arch, keepl)
  }else{
    archout <- append_tournament(arch, keepl)
  }
  
  return(archout)
}

glicko_keep_control <- function(rs, status = NULL){
  
  x <- as.data.frame(rs$data)
  lambda <- rs$param$lambda
  cval <- rs$param$cval
  bval <- rs$param$bval
  rdmax <- rs$param$rdmax
  cat_to_unite <- rs$param$cat_to_unite
  byrow <- rs$param$byrow
  
  # Start Data-Manipulation
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  giocatori <- stringi::stri_trans_general(str_to_title(giocatori), "Latin-ASCII")
  x <-  x %>% mutate(across(contains("Giocatore"), ~stringi::stri_trans_general(str_to_title(.), "Latin-ASCII")))
  
  # Va applicato anche a Giocatore1,...,Giocatore4
  
  np <- length(giocatori)
  nm <- length(unique(x$cat))
  
  # Assigning Division for Initialisation
  cat_values <- NULL
  for(i in seq_along(giocatori)) {
    giocatorei <- giocatori[i]
    cat_values[i] <- unique(x$cat[which(giocatorei == x$Giocatore1 | giocatorei == x$Giocatore2 | giocatorei == x$Giocatore3 | giocatorei == x$Giocatore4)])[1]
  }
  names(cat_values) <- giocatori
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  
  # Status
  if (!is.null(status)) {
    status$giocatore <- stringi::stri_trans_general(str_to_title(status$giocatore), "Latin-ASCII")
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           # Rating = rep(init[1], length(npadd)),
                           Rating = as.numeric(initval$initrats[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           # Deviation = rep(init[2], length(npadd)), 
                           Deviation = as.numeric(initval$initdev[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           Games = zv, Win = zv, Draw= zv, Loss = zv,nTourn = zv,
                           LastTourn =rep(date_from_weeks(time),length(npadd)),
                           LastPen = zv, PeakElo = zv, Percentile = zv, PeakRank = 1/zv, 
                           PeakTime = rep(date_from_weeks(time), length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Draw" %in% names(status))) 
      status <- cbind(status, Draw = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = date_from_weeks(time))
    if (!("LastPen" %in% names(status))) 
      status <- cbind(status, LastPen = 0)
    if (!("PeakElo" %in% names(status))) 
      status <- cbind(status, PeakElo = 0)
    if (!("Percentile" %in% names(status))) 
      status <- cbind(status, Percentile = 0)
    if (!("PeakRank" %in% names(status))) 
      status <- cbind(status, PeakRank = Inf, PeakTime = 0)
    if (!("PeakTime" %in% names(status))) 
      status <- cbind(status, PeakTime = date_from_weeks(time))
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win","Draw", "Loss", "nTourn","LastTourn", "LastPen", "PeakElo","Percentile", "PeakRank", "PeakTime")], npstatus)
    
    rinit <- ifelse(status[[2]]<700,700,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    ndraw <- status[[6]]
    nloss <- status[[7]]
    ntourn <- status[[8]]
    nlasttourn <- status[[9]]
    lastpen <- status[[10]]
    peakelo <- status[[11]]
    peakrank <- status[[13]]
    peaktime <- as.Date(status[[14]])
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    # rinit <- rep(init[1], length.out = np)
    # dinit <- rep(init[2], length.out = np)
    rinit <- as.numeric(initval$initrats[match(cat_values,initval$categories)])
    dinit <- as.numeric(initval$initdev[match(cat_values,initval$categories)])
    ngames <- nwin <-ndraw <-  nloss <- rep(0, length.out = np)
    ntourn <- rep(0, np)
    nlasttourn <- rep(time,np)
    lastpen <- rep(0, np)
    peakelo <- rep(0, np)
    peakrank <- rep(0, np)
    peaktime <- rep(date_from_weeks(time), np)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<-names(lastpen) <-  giocatori
  }
  
  # Players Active in this Event
  curplay <- match(giocatori, names(rinit))
  
  nlasttourn <- weeks_passed(date = nlasttourn)
  
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  ondraw <- ndraw[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  olastpen <- lastpen[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  ndraw <- ndraw[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1 
  nlasttourn <- nlasttourn[curplay]
  nlastpen <- lastpen[curplay]
  
  histry <- array(NA,
                  dim = c(np, 7),
                  dimnames = list(giocatori, 
                                  c("Rating", "Deviation", "Games", "Wins","Draws", "CatRating","Category")
                  ))
  histry<- as.data.frame(histry)
  
  # Costants
  qv <-  log(10)/400
  qip3 <- 3 * (qv/pi)^2
  
  # Dynamic Teammate Changing Scenatio 
  if(is.null(byrow)){
    nameteams <- data.frame(G1 = c(x$Giocatore1,x$Giocatore3),G2 = c(x$Giocatore2,x$Giocatore4))
    sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
    split_cols <- strsplit(sq, " ")
    g1 <- as.numeric(sapply(split_cols, `[`, 1))
    g2 <- as.numeric(sapply(split_cols, `[`, 2))
    
    cratsteams <- c((crats[g1]+crats[g2])/2,(crats[g2]+crats[g1])/2)
    cratsteams <- cratsteams[order(names(cratsteams))]
    
    cdevs <- pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn)),rdmax)
    cdevsteams <- c(sqrt((cdevs[g1]^2+cdevs[g2]^2)/2),sqrt((cdevs[g2]^2+cdevs[g1]^2)/2)) # Actual
    
    cdevs_mid <- numeric(length(c(g1,g2)))
    for(i in 1:np){
      cdt <- cdevsteams[i]
      cdp1 <- cdevs[c(g1,g2)[i]]
      cdp2 <- cdevs[c(g2,g1)[i]]
      cdevs_mid[i] <- ifelse(cdt<80 & cdp1 <80 & cdp2<80, cdt, cdevs[c(g1,g2)[i]])
    }
    names(cdevs_mid) <- giocatori[c(g1,g2)]
    cdevsteams <- cdevsteams[order(names(cdevsteams))]
    cdevs_mid <- cdevs_mid[order(names(cdevs_mid))]
    
    compagni <- data.frame(
      g1 = c(giocatori[g1],giocatori[g2]),
      g2 = c(giocatori[g2],giocatori[g1])
    )
    compagni <- compagni[order(compagni$g1),]
    
  }else{
    cdevs <-  pmin(sqrt(cdevs^2+cval^2*(time - nlasttourn+1)),rdmax)
    byrowPl <- giocatori[cat_values %in% byrow]
    
    
    nameteams <- data.frame(G1 = c(x$Giocatore1[!x$cat %in% byrow],x$Giocatore3[!x$cat %in% byrow]),G2 = c(x$Giocatore2[!x$cat %in% byrow],x$Giocatore4[!x$cat %in% byrow]))
    sq <- unique(paste(nameteams$G1,nameteams$G2,sep = " "))
    split_cols <- strsplit(sq, " ")
    g1 <- as.numeric(sapply(split_cols, `[`, 1))
    g2 <- as.numeric(sapply(split_cols, `[`, 2))
    
    compagni <- data.frame(
      g1 = c(giocatori[g1],giocatori[g2]),
      g2 = c(giocatori[g2],giocatori[g1])
    )
    compagni <- compagni[order(compagni$g1),]
  }
  
  return(compagni$g1[which(duplicated(compagni$g1))])
}

glicko_c_keep <- function(numplay, numrow, white1, white2, black1, black2, score, cdevsteams, cratsteams,dscore = double(2*numplay),weights,bval,keepl) { # Capire perchè 2*np
  escore <- numeric(numplay)
  ascore <- numeric(numplay)
  dval <- numeric(numplay)
  ptsupd <- numeric(numplay)
  l1t <- numeric(numplay)
  escorek <- 0
  qv <- (log(10)/400)
  qv2 <- (log(10)/400)^2
  qip3 <- 3 * (qv/pi)^2
  
  # Unique Team's gDevs
  gdevs <- 1/sqrt(1 + qip3 * cdevsteams)
  
  # Create Empty Vectors
  for (k in 1:numplay) {
    escore[k] <- 0
    ascore[k] <- 0
    dval[k] <- 0
    ptsupd[k] <- 0
    l1t[k] <- 0
  }
  
  score_char_black <- score_char(score, weights, which = "black")
  score_char_white <- score_char(score, weights, which = "white")
  
  
  for (k in 1:numrow){
    # Cumulative Scores for each player
    ascore[white1[k]] <- ascore[white1[k]] + score[k]
    ascore[white2[k]] <- ascore[white2[k]] + score[k] 
    ascore[black1[k]] <- ascore[black1[k]] + 1 - score[k]
    ascore[black2[k]] <- ascore[black2[k]] + 1 - score[k] 
    
    ## WHITE 
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevs[black1[k]] * (cratsteams[white1[k]] - cratsteams[black1[k]] ))/400))
    escore[white1[k]] <- escore[white1[k]] + escorek
    escore[white2[k]] <- escore[white2[k]] + escorek
    
    # 1/d^2 - dval
    dval[white1[k]] <- dval[white1[k]] + qv2 * gdevs[black1[k]]^2 * escorek * (1 - escorek)
    dval[white2[k]] <- dval[white2[k]] + qv2 * gdevs[black1[k]]^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj)) - Exp gain
    dscore[white1[k]] <- dscore[white1[k]] +  gdevs[black1[k]] * weights[k] * (score[k] + bval - escorek)
    dscore[white2[k]] <- dscore[white2[k]] +  gdevs[black1[k]] * weights[k] * (score[k] + bval - escorek)
    
    # r-r_j
    l1t[white1[k]] <- l1t[white1[k]] + cratsteams[black1[k]] - cratsteams[white1[k]]
    l1t[white2[k]] <- l1t[white2[k]] + cratsteams[black1[k]] - cratsteams[white1[k]]
    
    keepl[[white1[k]]] <- rbind(keepl[[white1[k]]], data.frame(opp1 = black1[k],opp2 = black2[k],Score = score_char_white[k],Points = gdevs[black1[k]] * weights[k] * (score[k] + bval - escorek)  ))
    keepl[[white2[k]]] <- rbind(keepl[[white2[k]]], data.frame(opp1 = black1[k],opp2 = black2[k],Score =  score_char_white[k],Points = gdevs[black1[k]] * weights[k] * (score[k] + bval - escorek)  ))
    
    
    ## BLACK
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevs[white1[k]] * (cratsteams[black1[k]] - cratsteams[white1[k]]))/400))
    escore[black1[k]] <- escore[black1[k]] + escorek
    escore[black2[k]] <- escore[black2[k]] + escorek
    
    # 1/d^2
    dval[black1[k]] <- dval[black1[k]] + qv2 * gdevs[white1[k]]^2 * escorek * (1 - escorek)
    dval[black2[k]] <- dval[black2[k]] + qv2 * gdevs[white1[k]]^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
    dscore[black1[k]] <- dscore[black1[k]] +  gdevs[white1[k]] * weights[k] * (1 - score[k] + bval - escorek)
    dscore[black2[k]] <- dscore[black2[k]] +  gdevs[white1[k]] * weights[k] * (1 - score[k] + bval - escorek)
    
    # r-r_j
    l1t[black1[k]] <- l1t[black1[k]] + cratsteams[white1[k]] - cratsteams[black1[k]]
    l1t[black2[k]] <- l1t[black2[k]] + cratsteams[white1[k]] - cratsteams[black2[k]]
    
    keepl[[black1[k]]] <- rbind(keepl[[black1[k]]], data.frame(opp1 = white1[k],opp2 = white2[k],Score = score_char_black[k],Points = gdevs[white1[k]] * weights[k] * (1 - score[k] + bval - escorek)))
    keepl[[black2[k]]] <- rbind(keepl[[black2[k]]], data.frame(opp1 = white1[k],opp2 = white2[k],Score =  score_char_black[k],Points = gdevs[white1[k]] * weights[k] * (1 - score[k] + bval - escorek)))
    
    
  }
  
  
  dscore[(numplay+ 1):(2 * numplay)] <- dval
  
  lout <- list(dscore,ascore, l1t, keepl)
  return(lout)
}

glicko_c_keep_byrow <- function(numplay, numrow, white1, white2, black1, black2, score, cdevsteams, cratsteams,dscore = double(2*numplay),weights,bval,keepl) { # Capire perchè 2*np
  escore <- numeric(numplay)
  ascore <- numeric(numplay)
  dval <- numeric(numplay)
  ptsupd <- numeric(numplay)
  l1t <- numeric(numplay)
  escorek <- 0
  qv <- (log(10)/400)
  qv2 <- (log(10)/400)^2
  qip3 <- 3 * (qv/pi)^2
  
  # Unique Team's gDevs

  # Create Empty Vectors
  for (k in 1:numplay) {
    escore[k] <- 0
    ascore[k] <- 0
    dval[k] <- 0
    ptsupd[k] <- 0
    l1t[k] <- 0
  }
  
  score_char_black <- score_char(score, weights, which = "black")
  score_char_white <- score_char(score, weights, which = "white")
  
  
  for (k in 1:numrow){
    gdevswhite <- 1/sqrt(1 + qip3 * sqrt((cdevsteams[white1[k]]^2 + cdevsteams[white2[k]]^2)/2))
    gdevsblack <- 1/sqrt(1 + qip3 * sqrt((cdevsteams[black1[k]]^2 + cdevsteams[black2[k]]^2)/2))
    
    # Cumulative Scores for each player
    ascore[white1[k]] <- ascore[white1[k]] + score[k]
    ascore[white2[k]] <- ascore[white2[k]] + score[k] 
    ascore[black1[k]] <- ascore[black1[k]] + 1 - score[k]
    ascore[black2[k]] <- ascore[black2[k]] + 1 - score[k] 
    
    ## WHITE 
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevswhite * (mean(c(cratsteams[white1[k]],cratsteams[white2[k]])) - mean(c(cratsteams[black1[k]],cratsteams[black2[k]])) ))/400))
    escore[white1[k]] <- escore[white1[k]] + escorek
    escore[white2[k]] <- escore[white2[k]] + escorek
    
    # 1/d^2 - dval
    dval[white1[k]] <- dval[white1[k]] + qv2 * gdevsblack^2 * escorek * (1 - escorek)
    dval[white2[k]] <- dval[white2[k]] + qv2 * gdevsblack^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj)) - Exp gain
    dscore[white1[k]] <- dscore[white1[k]] + gdevsblack * weights[k] * (score[k] + bval - escorek)
    dscore[white2[k]] <- dscore[white2[k]] + gdevsblack * weights[k] * (score[k] + bval - escorek)
    
    # r-r_j
    l1t[white1[k]] <- l1t[white1[k]] + mean(c(cratsteams[black1[k]], cratsteams[black2[k]])) - mean(c(cratsteams[white1[k]],cratsteams[white2[k]]))
    l1t[white2[k]] <- l1t[white2[k]] + mean(c(cratsteams[black1[k]], cratsteams[black2[k]])) - mean(c(cratsteams[white1[k]],cratsteams[white2[k]]))
    
    
    keepl[[white1[k]]] <- rbind(keepl[[white1[k]]], data.frame(opp1 = black1[k],opp2 = black2[k],Score = score_char_white[k],Points = gdevsblack * weights[k] * (score[k] + bval - escorek), Partner = white2[k]))
    keepl[[white2[k]]] <- rbind(keepl[[white2[k]]], data.frame(opp1 = black1[k],opp2 = black2[k],Score = score_char_white[k],Points = gdevsblack * weights[k] * (score[k] + bval - escorek), Partner = white1[k]))
    
    
    ## BLACK
    # E(s|r,rj,RDj) - expected score
    escorek <- 1 / (1 + 10^(-(gdevsblack * (mean(c(cratsteams[black1[k]], cratsteams[black2[k]])) - mean(c(cratsteams[white1[k]],cratsteams[white2[k]])) ))/400))
    escore[black1[k]] <- escore[black1[k]] + escorek
    escore[black2[k]] <- escore[black2[k]] + escorek
    
    # 1/d^2
    dval[black1[k]] <- dval[black1[k]] + qv2 * gdevswhite^2 * escorek * (1 - escorek)
    dval[black2[k]] <- dval[black2[k]] + qv2 * gdevswhite^2 * escorek * (1 - escorek)
    
    # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
    dscore[black1[k]] <- dscore[black1[k]] +  gdevswhite * weights[k] * (1 - score[k] + bval - escorek)
    dscore[black2[k]] <- dscore[black2[k]] +  gdevswhite * weights[k] * (1 - score[k] + bval - escorek)
    
    # r-r_j
    l1t[black1[k]] <- l1t[black1[k]] + mean(c(cratsteams[white1[k]],cratsteams[white2[k]])) - mean(c(cratsteams[black1[k]], cratsteams[black2[k]]))
    l1t[black2[k]] <- l1t[black2[k]] + mean(c(cratsteams[white1[k]],cratsteams[white2[k]])) - mean(c(cratsteams[black1[k]], cratsteams[black2[k]]))
    
    
    keepl[[black1[k]]] <- rbind(keepl[[black1[k]]], data.frame(opp1 = white1[k],opp2 = white2[k],Score = score_char_black[k],Points = gdevswhite * weights[k] * (1 - score[k] + bval - escorek), Partner = black2[k]))
    keepl[[black2[k]]] <- rbind(keepl[[black2[k]]], data.frame(opp1 = white1[k],opp2 = white2[k],Score =  score_char_black[k],Points = gdevswhite * weights[k] * (1 - score[k] + bval - escorek), Partner = black1[k]))
    
    
  }
  
  
  dscore[(numplay+ 1):(2 * numplay)] <- dval
  
  lout <- list(dscore,ascore, l1t, keepl)
  return(lout)
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
      score == 1 & weights %in% c(.85)~ "Third Place Win (2:0)",
      score == .75 & weights %in% c(.85)~ "Third Place Win (2:1)",
      score == .5 & weights %in% c(.85)~ "Third Place Draw (1:1)",
      score == .25 & weights %in% c(.85)~ "Third Place Lost (1:2)",
      score == 0 & weights %in% c(.85)~ "Third Place Lost (0:2)",
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
      score == .25 & weights %in% c(0.7) ~ "Bundesliga Lost (1:2)",
      score == .75 & weights %in% c(0.7) ~ "Bundesliga Win (2:1)",
      score == 1 & weights %in% c(0.7) ~ "Bundesliga Win (2:0)",
      score == .5 & weights %in% c(0.7) ~ "Bundesliga Draw (1:1)",
      TRUE ~"Game Not Identified"
    )
  }else{
    sc <- dplyr::case_when(
      score == 1 & weights %in% c(0.7) ~ "Bundesliga Lost (0:2)",
      score == .75 & weights %in% c(0.7) ~ "Bundesliga Lost (1:2)",
      score == .25 & weights %in% c(0.7) ~ "Bundesliga Win (2:1)",
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
      score == 0 & weights %in% c(.85)~ "Third Place Win (2:0)",
      score == .25 & weights %in% c(.85)~ "Third Place Win (2:1)",
      score == .5 & weights %in% c(.85)~ "Third Place Draw (1:1)",
      score == .75 & weights %in% c(.85)~ "Third Place Lost (1:2)",
      score == 1 & weights %in% c(.85)~ "Third Place Lost (0:2)",
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

archappend <- function(arch,curKeepl){
  T1 <- arch
  T2 <- curKeepl
  result <- list()
  athletes <- unique(c(names(T1), names(T2)))
  for (athlete in athletes) {
    T1_df <- if (athlete %in% names(T1)) T1[[athlete]] else NULL
    T2_df <- if (athlete %in% names(T2)) T2[[athlete]] else NULL
    
    athlete_result <- list()
    if (!is.null(T1_df)) athlete_result[[T1$Tournament]] <- T1_df
    if (!is.null(T2_df)) athlete_result[[T2$Tournament]] <- T2_df
    
    if (length(athlete_result) > 0) {
      result[[athlete]] <- athlete_result
    }
  }
  return(result)
}

append_tournament <- function(result, new_tournament) {
  athletes <- unique(c(names(result), names(new_tournament)))

    for (athlete in athletes) {
    new_df <- if (athlete %in% names(new_tournament)) new_tournament[[athlete]] else NULL
    
    if (is.null(result[[athlete]])) {
      result[[athlete]] <- list()
    }
    
    if (!is.null(new_df)) {
      result[[athlete]][[new_tournament$Tournament]] <- new_df
    }
  }
  result <- result[athletes]
  
  return(result)
}

append_percentages <- function(result, new_tournament) {
  # load("D:/Personal Statistics/rcb/Ranking/data/Extra/ita_players.rda")
  # if(missing(ita_players)) print("ita_players.rda NOT FOUND")
  athletes <- unique(c(names(result), names(new_tournament)))
  # athletes <- intersect(athletes, ita_players)
  
  
  for (athlete in athletes) {
    new_df <- if (athlete %in% names(new_tournament)) new_tournament[[athlete]] else NULL
    if (is.null(result[[athlete]])) {
      result[[athlete]] <- list()
    }
    
    if (!is.null(new_df)) {
      new_df$Tournament <- new_tournament$Tournament
      result[[athlete]] <- rbind(result[[athlete]],new_df)
    }
  }
  result <- result[athletes]
  
  return(result)
}

order_datasets <- function(load = F){ 
  
  timeT <- sapply(datasets, function(x) unique(x$Time))
  sortNames <- names(sort(timeT))
  last_letter <- substr(sortNames, nchar(sortNames), nchar(sortNames))
  df <- data.frame(tornei = paste("rs",sortNames,sep = ""), rbindStatus = character(length(sortNames)), byrow = character(length(sortNames)), cat_to_unite = character(length(sortNames)))
  
  
  
  
  df$rbindStatus[df$tornei =="rsNRC23S"] <- "mb"
  df$rbindStatus[df$tornei =="rsPrague23E"] <- "dg"
  df$rbindStatus[df$tornei =="rsBolo24E"] <- "gpc"
  df$rbindStatus[df$tornei =="rsMunch24M"] <- "fs"
  df$rbindStatus[df$tornei =="rsWInd24a"] <- "WorldsRat"
  df$rbindStatus[df$tornei =="rsWSquad24a"] <- "SquadRat"
  df$rbindStatus[df$tornei =="rsBologna24WS"] <- "riwsbo"
  df$rbindStatus[df$tornei =="rsForli24WS"] <- "riwsrf"
  df$rbindStatus[df$tornei =="rsBucharest25E"] <- "sp"
  df$rbindStatus[df$tornei =="rsMontpellier23M"] <- "cm"
    
  df$Status = lag(df$tornei)
              
  # df$Status[1] <- NULL.ch
  
  # write.xlsx(df, file = "TournamentNames.xlsx")
  names<- xlsx::read.xlsx("D:/Personal Statistics/rcb/Ranking/data/Extra/TournamentNames.xlsx", sheetIndex = 1)
  df <- left_join(df, select(names, tornei, TourName, Nation), by = "tornei")
  
  if(sum(is.na(df$TourName))>0){
    current_index <- 1  
    naTourn <- df[which(is.na(df$TourName)),]
    while (current_index <= nrow(naTourn)) {
      Tourn <- naTourn$tornei[current_index]
      cat("Torneo:", Tourn, "\n")
      choice <- readline("Nome da dare al Torneo: ")
      naTourn$TourName[current_index] <- choice
      current_index <- current_index+1
    }
    df[which(is.na(df$TourName)),] <- naTourn
  }
  
  for(t in df$TourName[sapply(df$Nation, \(c) is.na(c))]){
    df$Nation[df$TourName==t] <- readline(paste0("Nation of Tournament ",t, "? " ))
  }
  write.xlsx(select(df, tornei, TourName, Nation), "Extra/TournamentNames.xlsx")
  
  df$date <- as.Date(sapply(df$tornei, function(ds)
    date_from_weeks(unique(get(str_replace(ds, "rs", "")[1])$Time))
  ))
  
  param <- readRDS("shiny/www/DataBaseOutput.RDS")
  df$param <- param$param[match(df$tornei, param$tornei)]
  
  
  for(t in df$TourName[sapply(df$param, \(c) is.null(c[[1]]))]){
    
    df$param[df$TourName==t] <- list(list(
      "lambda" = 0,
      "cval" = 14,
      "bval" = 0,
      "rdmax" = 300,
      "cat_to_unite" = NULL,
      "byrow" = NULL
    ))
  
    pars <- coda::multi.menu(c("lambda", "cval", "bval", "rdmax", "cat_to_unite", "byrow", "none"),title = paste0("Which parameter to change for ", t, "?"))
    if(all(pars != 7)){
      tmp <- df$param[df$TourName == t][[1]]
      for(p in pars){
        v  <- readline(paste0("Values to give to ", c("lambda", "cval", "bval", "rdmax", "cat_to_unite", "byrow")[p], ": "))
        if(p %in% 1:4){
          tmp[[p]]<- as.numeric(eval(parse(text = v)))
        }else if(p %in% 5:6 & v == "NULL"){ 
          tmp[p] <-list(NULL)
        }else{
          tmp[[p]] <-v
        }
      }
      df$param[df$TourName == t][[1]] <- tmp
      
    }
  }
      
  return(df)
}


get_par <- function(name) {
  if (grepl("[0-9]E$", name)) return(ETS)
  if (grepl("[0-9]M$", name)) return(RG)
  if (grepl("[0-9]S$", name)) return(Sanct)
  if (grepl("[0-9]WS$", name)) return(WS)
  return(Nat)
}


glicko_percentages <- function(rs, status = NULL, perch = perch,initval = initval, TourName = NULL){
  
  x <- as.data.frame(rs$data)
  lambda <- rs$param$lambda
  cval <- rs$param$cval
  bval <- rs$param$bval
  rdmax <- rs$param$rdmax
  byrow <- rs$param$byrow
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  
  giocatori <-stringi::stri_trans_general(str_to_title(giocatori), "Latin-ASCII")
  x <-  x %>% mutate(across(contains("Giocatore"), ~stringi::stri_trans_general(str_to_title(.), "Latin-ASCII")))
  
  
  np <- length(giocatori)
  nr <- nrow(x)
  nm <- length(unique(x$cat))
  
  cat_values <- NULL
  for(i in seq_along(giocatori)) {
    giocatorei <- giocatori[i]
    cat_values[i] <- unique(x$cat[which(giocatorei == x$Giocatore1 | giocatorei == x$Giocatore2 | giocatorei == x$Giocatore3 | giocatorei == x$Giocatore4)])
  }
  names(cat_values) <- giocatori
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  # Status
  if(!exists("initval")) load("R/initval.rda")
  if (!is.null(status)) {
    status$giocatore <- stringi::stri_trans_general(str_to_title(status$giocatore), "Latin-ASCII")
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           # Rating = rep(init[1], length(npadd)),
                           Rating = as.numeric(initval$initrats[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           # Deviation = rep(init[2], length(npadd)), 
                           Deviation = as.numeric(initval$initdev[match(cat_values[names(cat_values) %in% npadd],initval$categories)]),
                           Games = zv, Win = zv, Draw= zv, Loss = zv,nTourn = zv,
                           LastTourn =rep(date_from_weeks(time),length(npadd)),
                           LastPen = zv, PeakElo = zv, Percentile = zv, PeakRank = 1/zv, 
                           PeakTime = rep(date_from_weeks(time), length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Draw" %in% names(status))) 
      status <- cbind(status, Draw = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = date_from_weeks(time))
    if (!("LastPen" %in% names(status))) 
      status <- cbind(status, LastPen = 0)
    if (!("PeakElo" %in% names(status))) 
      status <- cbind(status, PeakElo = 0)
    if (!("Percentile" %in% names(status))) 
      status <- cbind(status, Percentile = 0)
    if (!("PeakRank" %in% names(status))) 
      status <- cbind(status, PeakRank = Inf, PeakTime = 0)
    if (!("PeakTime" %in% names(status))) 
      status <- cbind(status, PeakTime = date_from_weeks(time))
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win","Draw", "Loss", "nTourn","LastTourn", "LastPen", "PeakElo","Percentile", "PeakRank", "PeakTime")], npstatus)
    
    rinit <- ifelse(status[[2]]<700,700,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    ndraw <- status[[6]]
    nloss <- status[[7]]
    ntourn <- status[[8]]
    nlasttourn <- status[[9]]
    lastpen <- status[[10]]
    peakelo <- status[[11]]
    peakrank <- status[[13]]
    peaktime <- as.Date(status[[14]])
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    # rinit <- rep(init[1], length.out = np)
    # dinit <- rep(init[2], length.out = np)
    rinit <- as.numeric(initval$initrats[match(cat_values,initval$categories)])
    dinit <- as.numeric(initval$initdev[match(cat_values,initval$categories)])
    ngames <- nwin <-ndraw <-  nloss <- rep(0, length.out = np)
    ntourn <- rep(0, np)
    nlasttourn <- rep(time,np)
    lastpen <- rep(0, np)
    peakelo <- rep(0, np)
    peakrank <- rep(0, np)
    peaktime <- rep(date_from_weeks(time), np)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<-names(lastpen) <-  giocatori
  }
  
  # Players Active in this Event
  curplay <- match(giocatori, names(rinit))
  
  nlasttourn <- weeks_passed(date = nlasttourn)
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1 
  nlasttourn <- nlasttourn[curplay]
  
  
 
  
  keepl <- list()
  pregameodds <- actualscore <- numeric(np)
  result <- character(np)
  for (mm in 1:np) {
    keepl[[mm]] <- data.frame(partn = numeric(), opp1 = numeric(), opp2 = numeric(), PreGameOdds = numeric(), Result = numeric(),Weight = numeric(), Score = character(), Tournament = character(), ownRat = numeric())
    actualscore[mm] <- 0
    result[mm] <- ""
  }
  
  keepl <- glicko_c_percentges(nr = nr, np = np,white1 = x$Giocatore1, white2 = x$Giocatore2, black1 = x$Giocatore3, black2 = x$Giocatore4,
                               score = x$Score, w = x$Weight, 
                               score_char_black = score_char(x$Score, x$Weight, "black"),
                               score_char_white = score_char(x$Score, x$Weight, "white"),
                               crats = crats, giocatori = giocatori, keepl = keepl,
                               cdevsteam = cdevs)
  names(keepl) <- giocatori
  keepl$Tournament <- TourName
  
  if(is.null(perch)){
    perchout <- keepl
  }else if(!is.null(perch) & length(perch$Tournament) %in% 1){
    perchout <- archappend(perch, keepl)
    perchout <- lapply(perchout, function(x){
      s <- do.call(rbind,x)
      s <- cbind(s,Tournament = gsub("\\..*","",rownames(s)))
      rownames(s) <- 1:nrow(s)
      return(s)
    })
  }else{
    perchout <- append_percentages(perch, keepl)
  }
  
  return(perchout)
}

glicko_c_percentges <- function(np,nr,white1, white2, black1, black2, score, w, score_char_black, score_char_white, crats, giocatori, keepl,cdevsteams){

  qv <- (log(10)/400)
  qv2 <- (log(10)/400)^2
  qip3 <- 3 * (qv/pi)^2
  
  # Unique Team's gDevs

  for (k in 1:nr){
    
    gdevswhite <- 1/sqrt(1 + qip3 * sqrt((cdevsteams[white1[k]]^2 + cdevsteams[white2[k]]^2)/2))
    gdevsblack <- 1/sqrt(1 + qip3 * sqrt((cdevsteams[black1[k]]^2 + cdevsteams[black2[k]]^2)/2))
    
    pregameodds_white <- 1 / (1 + 10^(-((mean(c(crats[white1[k]],crats[white2[k]])) - mean(c(crats[black1[k]],crats[black2[k]])) ))/400*gdevsblack)) 
    pregameodds_black <- 1 / (1 + 10^(-((mean(c(crats[black1[k]],crats[black2[k]])) - mean(c(crats[white1[k]],crats[white2[k]])) ))/400*gdevswhite)) 
    
    keepl[[white1[k]]] <- rbind(keepl[[white1[k]]], data.frame(partn = white2[k], opp1 = black1[k], opp2 = black2[k],oppRat = crats[black1[k]]+crats[black2[k]], PreGameOdds = pregameodds_white, Result = score[k],Weight= w[k], Score = score_char_white[k], ownRat = crats[white1[k]]+crats[white2[k]]))
    keepl[[white2[k]]] <- rbind(keepl[[white2[k]]], data.frame(partn = white1[k], opp1 = black1[k], opp2 = black2[k],oppRat = crats[black1[k]]+crats[black2[k]], PreGameOdds = pregameodds_white, Result = score[k],Weight= w[k], Score = score_char_white[k],ownRat = crats[white1[k]]+crats[white2[k]]))
    keepl[[black1[k]]] <- rbind(keepl[[black1[k]]], data.frame(partn = black2[k], opp1 = white1[k], opp2 = white2[k],oppRat = crats[white1[k]]+crats[white2[k]], PreGameOdds = pregameodds_black, Result = 1-score[k],Weight= w[k], Score = score_char_black[k],ownRat = crats[black1[k]]+crats[black2[k]]))
    keepl[[black2[k]]] <- rbind(keepl[[black2[k]]], data.frame(partn = black1[k], opp1 = white1[k], opp2 = white2[k],oppRat = crats[white1[k]]+crats[white2[k]], PreGameOdds = pregameodds_black, Result = 1-score[k],Weight= w[k], Score = score_char_black[k],ownRat = crats[black1[k]]+crats[black2[k]]))
  }
  
  for(jj in 1:np){
    rownames(keepl[[jj]]) <- NULL
    keepl[[jj]][,"partn"] <- giocatori[keepl[[jj]][,"partn"]]
    keepl[[jj]][,"opp1"] <- paste0(giocatori[keepl[[jj]][,"opp1"]]," & ",giocatori[keepl[[jj]][,"opp2"]])
    keepl[[jj]][,"Result"] <- ifelse(keepl[[jj]][,"Result"]<.5,0,ifelse(keepl[[jj]][,"Result"]==0.5,.5,1))
    keepl[[jj]] <- keepl[[jj]][,-which(colnames(keepl[[jj]])=="opp2")] 
  }
  return(keepl)
}

## Visualize ####

summary_odds <- function(Player, type = c("Best Upset", "Best Bracket Upset", "Worst Lost", "Worst Bracket Lost", "Highest Rated Win", "Highest Rated Match")){
  
  if(!exists("perch")){
    print("Run glicko percentages!")
    break
  }
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

summary_opponents <- function(Player, nmin = 2){
  if(!exists("perch")) print("Please run R file: keep_percentages.R")
  pp <- perch[[Player]]
  pp <- tidyr::separate(pp, opp1, into = c("o1", "o2"), " & " )
  o <- unique(c(pp$o1, pp$o2))
  pp.df <- data.frame(Giocatore = NULL, PartiteContro = NULL, Win = NULL, Draw = NULL, Loss = NULL, LastTour = NULL, LastRes = NULL)
  for(opp in o){
    cur.df <- pp[str_detect(paste(pp$o1, pp$o2, sep = " "), opp),]
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

find_matchups <- function(Player, Opponent){
  if(!exists("perch")) print("Please run R file: keep_percentages.R")
  
  perch_stat <- perch[[Player]][str_detect(perch[[Player]][,2], Opponent),c(2,1,3,4, 7,8)]
  names(perch_stat) <- c("Opponents", "Partner", "Opponent Rating","Pre-Game Odds", "Result", "Tournament")
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

