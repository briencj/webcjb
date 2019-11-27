#'# Two-phase wine experiment from Brien, May and Mayo (1987, Experiment 3)

#'## Intialize
library(dae)
options(width=110)
load("./data/WIN73.dat.rda")

#'## Shorten the names of the factors
WIN73.short.dat <- WIN73.dat 
names(WIN73.short.dat)[match(c("Occasions", "Sessions", "Judges", "Glasses", 
                                 "Blocks", "Plots", "WineReps", "Treatments"),
                               names(WIN73.short.dat))] <- 
  c("Occ", "Sess", "Jud",  "Glass", "Blk", "Plot", "WReps", "Treat")

#'## Produce the anatomy of the design
WIN73.canon <- designAnatomy(list(eval=~  ((Occ/Sess)*Jud)/Glass, 
                                  field=~ (Blk/Plot/WReps)*Occ,
                                  trt = ~ Treat*Occ), 
                             data=WIN73.short.dat)
summary(WIN73.canon, which.criteria =c("aefficiency", "order"))
