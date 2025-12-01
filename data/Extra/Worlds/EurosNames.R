pl <- readRDS("D:/Personal Statistics/rcb/Ranking/shiny/www/LastRat.RDS")$giocatore
View(readRDS("D:/Personal Statistics/rcb/Ranking/shiny/www/LastRat.RDS"))

eu <- read.xlsx("Extra/Worlds/EUROS25.xlsx",colClasses =c("character", rep(c("character", rep("numeric",3)),2),"numeric"), sheetName = "Open",header = F, as.data.frame = T, )
colnames(eu) <- c("cat","Sq1", "p1", "p2", "p3", "Sq2", "W")
eu <- 
  eu %>% 
  separate(col = "Sq1", into = c("g1", "g2"), sep = "( / )|/" ) %>% 
  separate(col = "Sq2", into = c("g3", "g4"), sep = "( / )|/" ) %>% 
  mutate(across(contains("g"), ~stringi::stri_trans_general(str_to_title(gsub("\\*|\\n", "",.)), "Latin-ASCII")))
glimpse(eu)

eu.pl <- unique(c(eu$g1, eu$g2, eu$g3, eu$g4))
eu.pl.nodot <- gsub(".*?\\. ", "", eu.pl)

matches <- sapply(eu.pl.nodot, function(p)  pl[which(grepl(p, pl, ignore.case = TRUE))])
names(matches) <- eu.pl

for(m in 1:length(matches)){
  curM <- matches[[m]]
  namM <- names(matches[m])
  
  if(length(curM)>0){
    choice <- menu(curM, title = paste0("Which surname is correct for ", namM, "?"))
    matches[[m]] <- curM[choice]
  }else{
    choice <- readline(paste0("What could be the name for ", namM, "?"))
    matches[[m]] <- choice
  }
}

matches$`A. Vibenholm` <- "Andreas Christensen"
matches$`N. Vibenholm` <- "Nadia Vibenholm"
matches$Pin <- "Carina Kuo"
matches$Elde <- "Hanna Kristine Elde"
matches$Holte <- "Cecilie Holte"
matches$` Dahl` <- "Zakarias Dahl"
matches$`Dieltjes-Van Hummel` <- "Shauny Dieltjes-van Hummel"
matches$`Haugland Viland` <- "Filip Haugland Viland"
matches$`Mollerup Boge` <- "Bjarte Mollerup Boge"
matches$Thormodsaeter <- "Hakon Thormodsaeter"
matches$Ungerer <- "Thomas Ungerer"
matches$Chang <- "Minna Chang Hansen"
matches$Vaivilavicius <- "Kostas Vaivilavicius"



EUROS <- eu %>% 
  rowwise() %>% 
  mutate(across(contains("g"),~matches[[.]]),
         sets = sum(c(!is.na(p1),!is.na(p2),!is.na(p3)), na.rm = T),
         win = sum(c(p1,p2,p3), na.rm = T),
         Score = case_when(
           win/sets==1/3~.25,
           win/sets==2/3~.75,
           TRUE~win/sets
         ),
         Play1 = paste0(g1, " & ", g2),
         Play2 = paste0(g3, " & ", g4),
         Time = weeks_passed(2025,9,27)
         ) %>% 
  rename(Weight = W) %>% 
  select(cat, Time, Play1, Play2, Score, Weight)

write.xlsx(EUROS, file = "WORLDS/Europei25.xlsx")
            