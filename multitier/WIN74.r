#'# Two-phase wine experiment from Brien, May and Mayo (1987, Experiment 4)

#'## Intialize
library(dae)
options(width=110)
load("./data/WIN74.dat.rda")

#'## Shorten the names of the factors
WIN74.short.dat <- WIN74.dat 
names(WIN74.short.dat)[match(c("Occasions", "Sessions", "Judges", "Glasses", 
                               "Blocks", "Plots", "WineReps", "Treatments"),
                             names(WIN74.short.dat))] <- 
  c("Occ", "Sess", "Jud",  "Glass", "Blk", "Plot", "WReps", "Treat")

#'## Produce the anatomy of the design
WIN74.canon <- designAnatomy(list(eval=~  Occ/Jud/(Sess:Glass), 
                                  field=~ (Blk/Plot/WReps)*Occ,
                                  trt = ~ Treat*Occ), 
                             data=WIN74.short.dat)
summary(WIN74.canon, which.criteria =c("aefficiency", "order"))
