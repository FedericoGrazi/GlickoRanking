library(readxl)
library(tidyverse)

# INITVAL FOR INDIVIDUAL ####
file = "D:/Personal Statistics/rcb/Ranking/data/WORLDS/24_Worlds.csv"
x <- read.csv(file)
giocatori = sort(unique(c(x$Team.A...Player.1, x$Team.A...Player.2, x$Team.B...Player.1, x$Team.B...Player.2)))
npadd <- giocatori[!(giocatori %in% rsBerlin24$ratings$giocatore)]
npadd <- npadd[!str_detect(npadd, "Jenki")]

  
y <- data.frame(
  row = rep(1:nrow(x),4),
  division = rep(x$Division,4),
  stage = rep(x$Stage,4),
  pl = c(x$Team.A...Player.1, x$Team.A...Player.2, x$Team.B...Player.1, x$Team.B...Player.2)
)
y$ismatch <- match(y$pl, npadd)
last.res <- foreach(i = 1:length(npadd), .combine = rbind) %do% max(y$row[!is.na(match(y$ismatch, i))])

z <- data.frame(
  npadd,
  y[last.res,c(2,3)]
)
z$cat <- with(z, paste0(division, " ", stage, sep = ""))

ajenki <- data.frame(giocatore = "Ali Jenki",Rating = 1600,Deviation = 150,Games = 0,Win = 0,Loss = 0,nTourn = 0,LastTourn = 72)
ojenki <- data.frame(giocatore = "Olivia Jenki",Rating = 1600,Deviation = 150,Games = 0,Win = 0,Loss = 0,nTourn = 0,LastTourn = 72)

# z[with(z, order(division, stage)),]
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
  Loss = 0,
  nTourn = 0,
  LastTourn = 72
  )


# new_worlds_rat <- rbind(new_worlds_rat,jenki)
# save(new_worlds_rat, file = "D:/Personal Statistics/rcb/Ranking/data/WORLDS/Ratings_wordls.rda")



# FWANGO NAMES FOR SQUAD ####
dwbs <- read_excel("D:/Personal Statistics/rcb/Ranking/data/WORLDS/Data Worlds Bracket Squads.xlsx", sheet = "Full")
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

names <- data.frame(names = sort(unique(c(dbsq$g1,dbsq$g2,dbsq$g3,dbsq$g4))))
nadd <- read_excel("D:/Personal Statistics/rcb/Ranking/data/WORLDS/Data Names Worlds Squads (Revisione1).xlsx")
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

# save(SBgroups, file = "D:/Personal Statistics/rcb/Ranking/data/WORLDS/Index Groups.rda")
# save(dataWSB, file = "D:/Personal Statistics/rcb/Ranking/data/WORLDS/Data Squad Battle.rda")

# INITVAL FOR SQUAD ####
sqgiocatori = sort(unique(c(g1,g2,g3,g4)))
sqnpadd <- sqgiocatori[!(sqgiocatori %in% rsWI2$ratings$giocatore)]


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

squad_worlds_cat$initrats <- c(rep(1250,5),rep(1150,5),rep(1350,3),rep(1550,3), rep(1150,9),rep(1200,3),rep(1350,2),rep(1200,3))
squad_worlds_cat$initdev <- rep(200, nrow(squad_worlds_cat))
squad_worlds_cat <- squad_worlds_cat[,c(3,4,5)]

Squad_Rat <- data.frame(
  giocatore = sqnpadd,
  Rating = as.numeric(squad_worlds_cat$initrats[match(sqz$cat,squad_worlds_cat$cat)]),
  Deviation = as.numeric(squad_worlds_cat$initdev[match(sqz$cat,squad_worlds_cat$cat)]),
  Games = 0,
  Win = 0,
  Loss = 0,
  nTourn = 0,
  LastTourn =72
)

save(new_squads_rat, file = "D:/Personal Statistics/rcb/Ranking/data/WORLDS/Ratings Squad Battle.rda")
  
# # WSB format ####
# dataNGB <- data.frame(
#   Round = paste0(dbsq$Tab," ", dbsq$Round),
#   Group = paste0(dbsq$`NGB A`,"vs",dbsq$`NGB B`),
#   Play1 = dbsq$`NGB A`,
#   Play2 = dbsq$`NGB B`,
#   Score = dataWSB$Score,
#   Weight = dataWSB$Weight,
#   type= dbsq$Type
# )
# 
# dataNGB <- 
#   dataNGB %>% 
#   group_by(Group, Round) %>% 
#   mutate(ScoreA = sum(Score),
#          ScoreB = sum(1-Score),
#          Score = ScoreA/(ScoreA+ScoreB)) %>%
#   filter(!duplicated(Group)) %>% 
#   ungroup %>% 
#   select(Play1,Play2,Score,Weight, type)
# 
# 
# a <- read_excel("C:/Users/Feder/Downloads/SQUADS_MATCHES.xlsx")
# 
# Nas <- NULL
# b <- a
# for(i in 1:ncol(a)){
#   squad <- data.frame(a[,i])
#   names(squad) <- "names"
#   values <- rsWSB2$ratings$Rating[match(squad$names,rsWSB2$ratings$giocatore)]
#   Nas <- c(Nas, squad$names[which(is.na(values))])
#   values <- ifelse(is.na(values), 1200, values)
#   b[,i] <- values
# }
# means <- apply(b, 2, mean)
# 
# dataNGB$NameA <- paste0(dataNGB$Play1," - ", dataNGB$type, sep = "")
# dataNGB$NameB <- paste0(dataNGB$Play2," - ", dataNGB$type, sep = "")
# 
# dataNGB <- dataNGB %>% select(NameA, NameB, Score, Weight)
# 
# # PERFECT ELO FORMAT ###
# # NGBdf <- data.frame(Player = names(means),
# #            Rating = as.double(round(means)),
# #            Games = rep(0, length(means)),
# #            Win = rep(0, length(means)),
# #            Draw = rep(0, length(means)),
# #            Loss = rep(0, length(means)),
# #            Lag = rep(0, length(means))
# #            )
# # rownames(NGBdf) <- NULL
# # NGBrat <- list(ratings = NGBdf, history = NULL, gamma = 0, kfac = 27, type = "Elo")
# # class(NGBrat) <- "rating"
# # 
# # dataNGB$Week <- as.numeric(1)
# # dataNGB <- dataNGB %>% select(Week,NameA, NameB, Score) %>% rename(HomeTeam = NameA, AwayTeam = NameB) %>% as.data.frame
# # 
# # NGBupdates <- elo(dataNGB, status = NGBrat$ratings, history = T)
# 
# # ELO GAME BY GAME ###
# 
# 
# ngbs <- names(means)
# numplay <- length(ngbs)
# numrow <- nrow(dataNGB)
# cratsteams <- means
# 
# dataNGB$NameA <- match(dataNGB$NameA,ngbs)
# dataNGB$NameB <- match(dataNGB$NameB,ngbs)
# kfac <- 27
# 
# white1 <- dataNGB$NameA
# black1 <- dataNGB$NameB
# weights <- dataNGB$Weight
# score <- dataNGB$Score
# 
# escore <- numeric(numplay)
# dscore <- numeric(numplay)
# escorek <- 0
# 
# K = 27
# 
# # Create Empty Vectors
# for (k in 1:numplay) {
#   escore[k] <- 0
#   dscore[k] <- 0
# }
# 
# for (k in 1:numrow) {
#   # E(s|r,rj) - expected score
#   escorek <- 1 / (1 + 10^(-( (cratsteams[white1[k]] - cratsteams[black1[k]] ))/400))
#   escore[white1[k]] <- escore[white1[k]] + escorek
# 
#   # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
#   dscore[white1[k]] <- dscore[white1[k]] +  kfac * weights[k] * (score[k] - escorek)
# 
#   # E(s|r,rj,RDj) - expected score
#   escorek <- 1 / (1 + 10^(-((cratsteams[black1[k]] - cratsteams[white1[k]]))/400))
#   escore[black1[k]] <- escore[black1[k]] + escorek
# 
#   
#   # \sum g(RD_j)(s_j-E(s|r,rj,RDj))
#   dscore[black1[k]] <- dscore[black1[k]] +  kfac * weights[k] * (1 - score[k] - escorek)
# }
# 
# names(dscore) <- ngbs
# dscore
# 
