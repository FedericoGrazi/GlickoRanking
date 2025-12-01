library(lubridate)

eug <- exportEU(get(dft[nrow(dft),1]),
                nTournMin = 2, minGames = 14,
                lastT = weeks_passed(year(today()),
                                     month(today()),
                                     day(today()))-104)

saveRDS(eug, file = "D:/Personal Statistics/rcb/Ranking/shiny/www/EU_dataset.RDS")


w <- tolower(readline("Rerun everything? (y/n) "))
if(w == "y"){
  rch <- archive()
  arch <- rch[[1]]; perch <- rch[[2]]
}else{
  arch <- glicko_keep(get(dft[nrow(dft),1]), get(dft[nrow(dft)-1,1])$ratings, TourName =  dft$TourName[nrow(dft)], arch = readRDS("D:/Personal Statistics/rcb/Ranking/shiny/www/Archive.RDS"), initval = initval)
  perch <- glicko_percentages(get(dft[nrow(dft),1]), get(dft[nrow(dft)-1,1])$ratings, TourName = dft$TourName[nrow(dft)], perch = readRDS("D:/Personal Statistics/rcb/Ranking/shiny/www/Percentages.RDS"), initval = initval)

}

saveRDS(arch, file =  "D:/Personal Statistics/rcb/Ranking/shiny/www/Archive.RDS")
saveRDS(perch, file = "D:/Personal Statistics/rcb/Ranking/shiny/www/Percentages.RDS")
message("Saved Archive Files!")

ratinghistr <- rat_histr(dft,
                         0,
                         readRDS("D:/Personal Statistics/rcb/Ranking/shiny/www/Archive.RDS"))


saveRDS(ratinghistr, file = "D:/Personal Statistics/rcb/Ranking/shiny/www/RatingHistory.RDS")

message("Saved Rating History Chart")
rsList <- list()
for(t in 1:nrow(dft)){
  rsList[[dft$TourName[t]]] <- get(dft$tornei[t])
}
saveRDS(rsList, file = "D:/Personal Statistics/rcb/Ranking/shiny/www/rsList.RDS")
message("All Done!")
Sys.sleep(4)
cat("\014")
