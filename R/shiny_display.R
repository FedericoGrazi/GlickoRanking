library(rsconnect)



# GLICKO
rsconnect::setAccountInfo(name='federicograzi',
                          token='62CB10C8BCD7F7845A15990CDEFD38D6',
                          secret='ER5ayeZh3ba51nqX2E/4qrmRucx3rKMyEhNHIDmr')

rsconnect::deployApp("D:/Personal Statistics/rcb/Ranking/shiny/",
                     appName = "GlickoRanking")
