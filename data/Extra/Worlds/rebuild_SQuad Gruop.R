
## MENS ####
fgs <- read_excel("Extra/Worlds/Tutte le partite dello squad da PZ.xlsx", sheet = 1,col_names = c("t1", "t2", "l1", "l2"))
sgs <- read_excel("Extra/Worlds/Tutte le partite dello squad da PZ.xlsx", sheet = 2,col_names = c("t1", "t2", "l1", "l2"))
names <- read_excel("Extra/Worlds/Tutte le partite dello squad da PZ.xlsx", sheet = 3,col_names = c("n", paste0("s", 1:5)))

mn <- match(fgs$t1, names$n)
mn2 <- match(fgs$t2, names$n)
fDATA <- 
  data.frame(
    cat = "Men's Squad",
    Time = 72,
  Play1 = c(
    names$s1[mn],
    names$s2[mn],
    names$s3[mn],
    names$s4[mn],
    names$s5[mn]
    ),
  Play2 = c(
    names$s1[mn2],
    names$s2[mn2],
    names$s3[mn2],
    names$s4[mn2],
    names$s5[mn2]
  ),
  score = c(
    case_when(fgs$l1==1~0,fgs$l2== 1~0,TRUE~1),
    case_when(fgs$l1==2~0,fgs$l2== 2~0,TRUE~1),
    case_when(fgs$l1==3~0,fgs$l2== 3~0,TRUE~1),
    case_when(fgs$l1==4~0,fgs$l2== 4~0,TRUE~1),
    case_when(fgs$l1==5~0,fgs$l2== 5~0,TRUE~1)
  ),
  Weight = 0.4
)

smn <- match(sgs$t1, names$n)
smn2 <- match(sgs$t2, names$n)
sDATA <- 
  data.frame(
    cat = "Men's Squad",
    Time = 72,
  Play1 = c(
    names$s1[smn],
    names$s2[smn],
    names$s3[smn],
    names$s4[smn],
    names$s5[smn]
    ),
  Play2 = c(
    names$s1[smn2],
    names$s2[smn2],
    names$s3[smn2],
    names$s4[smn2],
    names$s5[smn2]
  ),
  score = c(
    case_when(sgs$l1==1~0,sgs$l2== 1~0,TRUE~1),
    case_when(sgs$l1==2~0,sgs$l2== 2~0,TRUE~1),
    case_when(sgs$l1==3~0,sgs$l2== 3~0,TRUE~1),
    case_when(sgs$l1==4~0,sgs$l2== 4~0,TRUE~1),
    case_when(sgs$l1==5~0,sgs$l2== 5~0,TRUE~1)
  ),
  Weight = 0.6
)


mens <- rbind(fDATA, sDATA)

## WOMENS ####
fgs <- read_excel("Extra/Worlds/Tutte le partite dello squad da PZ.xlsx", sheet = 4,col_names = c("t1", "t2", "l1", "l2"))
sgs <- read_excel("Extra/Worlds/Tutte le partite dello squad da PZ.xlsx", sheet = 4,col_names = c("t1", "t2", "l1", "l2"))
names <- read_excel("Extra/Worlds/Tutte le partite dello squad da PZ.xlsx", sheet = 6,col_names = c("n", paste0("s", 1:5)))

mn <- match(fgs$t1, names$n)
mn2 <- match(fgs$t2, names$n)
fDATA <- 
  data.frame(
    cat = "Women's Squad",
    Time = 72,
  Play1 = c(
    names$s1[mn],
    names$s2[mn],
    names$s3[mn],
    names$s4[mn],
    names$s5[mn]
    ),
  Play2 = c(
    names$s1[mn2],
    names$s2[mn2],
    names$s3[mn2],
    names$s4[mn2],
    names$s5[mn2]
  ),
  score = c(
    case_when(fgs$l1==1~0,fgs$l2== 1~0,TRUE~1),
    case_when(fgs$l1==2~0,fgs$l2== 2~0,TRUE~1),
    case_when(fgs$l1==3~0,fgs$l2== 3~0,TRUE~1),
    case_when(fgs$l1==4~0,fgs$l2== 4~0,TRUE~1),
    case_when(fgs$l1==5~0,fgs$l2== 5~0,TRUE~1)
  ),
  Weight = 0.4
)

smn <- match(sgs$t1, names$n)
smn2 <- match(sgs$t2, names$n)
sDATA <- 
  data.frame(
    cat = "Women's Squad",
    Time = 72,
  Play1 = c(
    names$s1[smn],
    names$s2[smn],
    names$s3[smn],
    names$s4[smn],
    names$s5[smn]
    ),
  Play2 = c(
    names$s1[smn2],
    names$s2[smn2],
    names$s3[smn2],
    names$s4[smn2],
    names$s5[smn2]
  ),
  score = c(
    case_when(sgs$l1==1~0,sgs$l2== 1~0,TRUE~1),
    case_when(sgs$l1==2~0,sgs$l2== 2~0,TRUE~1),
    case_when(sgs$l1==3~0,sgs$l2== 3~0,TRUE~1),
    case_when(sgs$l1==4~0,sgs$l2== 4~0,TRUE~1),
    case_when(sgs$l1==5~0,sgs$l2== 5~0,TRUE~1)
  ),
  Weight = 0.6
)


womens <- rbind(fDATA, sDATA)


xlsx::write.xlsx(rbind(mens,womens) %>% filter(!is.na(Play1), !is.na(Play2)), file = "WORLDS/WSquad24a.xlsx")
