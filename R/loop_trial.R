library(readxl)
library(xlsx)
library(tidyverse)
library(ggdark)
library(ggthemes)
library(tools)
library(foreach)
load("R/Init.rda")
source("R/ranking_function.R")

# Manual Entries ####

load("WORLDS/IndRat.rda")
load("WORLDS/SquadRat.rda")

# Needs to be run if a tournament pre 2024 Worlds is Run. !! TO BE DONE IN LOCAL !!
# WorldsOut <- worlds_obj(rsBang24, t = "ind"); WorldsRat <- WorldsOut$rat; save(WorldsRat,file =  "WORLDS/IndRat.rda")
# WorldsOut <- worlds_obj(rsBang24, t = "squad"); SquadRat <- WorldsOut$rat; save(SquadRat, file = "WORLDS/SquadRat.rda")

mb  = data.frame(giocatore = c("Marco Bortoloso", "Paolo Fusco"),Rating = 1350,Deviation = 200,Games = 0,Win = 0,Draw = 0,Loss = 0,nTourn = 0,LastTourn = date_from_weeks(-5), LastPen = 0, PeakElo = 0, Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))
ns  = data.frame(giocatore = c("Neri Sansone"),Rating = 1200,Deviation = 200,Games = 0,Win = 0,Draw = 0,Loss = 0,nTourn = 0,LastTourn = date_from_weeks(0), LastPen = 0, PeakElo = 0, Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))
dg  = data.frame(giocatore = c("Davide Greco", "omar zeni"),Rating = 1200,Deviation = 200,Games = 0,Win = 0,Draw = 0,Loss = 0,nTourn = 0,LastTourn = date_from_weeks(4), LastPen = 0, PeakElo = 0, Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))
fs  = data.frame(giocatore = "Filippo Struffi",Rating = 1200,Deviation = 200,Games = 0,Win = 0,Draw = 0,Loss = 0,nTourn = 0,LastTourn = date_from_weeks(62), LastPen = 0, PeakElo = 0, Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))
cm  = data.frame(giocatore = "Clark Marshall",Rating = 1750,Deviation = 215,Games = 0,Win = 0,Draw = 0,Loss = 0,nTourn = 0,LastTourn = date_from_weeks(10), LastPen = 0, PeakElo = 0, Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))
gpc  = data.frame(giocatore = c("Giacomo Colliva", "Filippo Fabbri"),Rating = 1200,Deviation = 200,Games = 0,Win = 0,Draw = 0,Loss = 0,nTourn = 0,LastTourn = date_from_weeks(52), LastPen = 0, PeakElo = 0, Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))
sp  = data.frame(giocatore = "Simone Paoletti",Rating = 1200,Deviation = 200,Games = 0,Win = 0,Draw = 0,Loss = 0,nTourn = 0,LastTourn = date_from_weeks(104), LastPen = 0, PeakElo = 0, Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))
riwsbo = data.frame(giocatore = c("Ivan Bolognesi", "Giovanni Zerbo", "Marco Groppetti", "Simone Stagni", "Emma Zanutta", "Arianna Esti", "Francesca Furlan", "Martina Boaretto"),
                    Rating = c(rep(1200,4), rep(1100,4)), # 1125, 1050 // 1200, 1100
                    Deviation = c(rep(100,4), rep(75,4)),
                    Games = 0,
                    Win = 0,
                    Draw = 0,
                    Loss = 0,
                    nTourn = 0,
                    LastTourn =date_from_weeks(83),
                    LastPen = 0, 
                    PeakElo = 0,
                    Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))

riwsrf = data.frame(giocatore = c("Samuele Violante", "Andrea Govoni", "Beatrice Toschi", "Alice Mastroilli", "Elvira Raimondo"),
                    Rating = c(rep(1200,2), rep(1100,3)), # 1175, 1050 // 1200, 1100
                    Deviation = c(rep(100,2), rep(75,3)),
                    Games = 0,
                    Win = 0,
                    Draw = 0,
                    Loss = 0,
                    nTourn = 0,
                    LastTourn = date_from_weeks(88), 
                    LastPen = 0,
                    PeakElo = 0,
                    Percentile = 0, PeakRank = Inf, PeakTime = as.Date(NA))


#           lambda,bval
# ETS    <- c(1/30,1/20)
# MAJ    <- c(1/20,1/30)
# RG     <- c(1/40,1/40)
# RF     <- c(1/40,1/50)
# Sanct  <- c(1/50,1/50)
# Nat    <- c(0,0)
# WS     <- c(0,0)
# Loop Trial ####


rm(datasets)
datasets <- ranking_datasets(flatten=T)
for(tour in names(datasets)) assign(tour, datasets[[tour]])
dft <- order_datasets()
saveRDS(dft, file = "shiny/www/DataBaseOutput.RDS")

pb <- txtProgressBar(min = 1, max = nrow(dft),style = 3)
for (i in seq_len(nrow(dft))) {
  obj_name <- sub("^rs", "", dft$tornei[i])
  prev_obj <- dft$Status[i]   
  extra    <- dft$rbindStatus[i]
  param    <- dft$param[[i]]
  
  initval <- init_control(i, dft)
  
  
  ratings_in <- if(i == 1)  NULL else  get(prev_obj)$ratings
  
  if (extra != "")  ratings_in <- rbind(ratings_in, get(extra))
  
  if(all(unique(get(obj_name)$cat) %in% param$byrow) & !is.null(param$byrow)) param$byrow <-  "all" 
  
  res <- glicko_ets(
    get(obj_name), 
    ratings_in,
    rdmax = param$rdmax,
    cval = param$cval,
    lambda = param$lambda,
    bval = param$bval,
    cat_to_unite = param$cat_to_unite,
    byrow = param$byrow
  )
  
  assign(paste0("rs", obj_name), res, envir = .GlobalEnv)
  setTxtProgressBar(pb, i)
}
# get(dft[nrow(dft),1])$ratings %>% View
# RSratingtable(get(dft[nrow(dft),1]))
# gk <- glicko_keep(get(dft[nrow(dft),1]), 
#                   get(dft[nrow(dft)-1,1])$ratings, 
#                   TourName = ".", 
#                   arch = NULL,
#                   initval = init_control(weeks_passed(date = today()), dft= dft))

source("R/run_archive.R")


"# trigger test" 
"# trigger test" 
"# trigger test" 
"# trigger test" 
"# trigger test" 
"# trigger test" 
"# trigger test" 
"# trigger test" 
