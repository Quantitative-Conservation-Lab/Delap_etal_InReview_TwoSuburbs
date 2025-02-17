AMCR <- read.csv("results/occ.AMCR.csv")
AMGO <- read.csv("results/occ.AMGO.csv")
AMRO <- read.csv("results/occ.AMRO.csv")
ANHU <- read.csv("results/occ.ANHU.csv")

mod.sel <- paste(AMCR$w.sitetype,AMCR$w.year,AMCR$w.year2)

table.mod <- table(mod.sel)