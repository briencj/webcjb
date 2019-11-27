#'# Two-phase wine experiment from Brien, May and Mayo (1987, Experiment 5)

#'## Intialize
library(dae)
options(width=110)
load("./data/WIN76.dat.rda")

#'## Shorten the names of the factors
WIN76.short.dat <- WIN76.dat 
names(WIN76.short.dat)[match(c("GlassType", "Sessions", "Judges", "Glasses", 
                               "Blocks", "Plots", "Halfplots", "WineReps", "RootStock"),
                             names(WIN76.short.dat))] <- 
  c("GType", "Sess", "Jud",  "Glass", "Blk", "Plot", "Hplot", "WReps", "RStock")

#'## Produce the anatomy of the design
WIN76.canon <- designAnatomy(list(eval=~  ((GType/Sess)*Jud)/Glass, 
                                  field=~ (Blk/Plot/Hplot/WReps)*GType,
                                  trt = ~ RStock*GType), 
                             data=WIN76.short.dat)
summary(WIN76.canon, which.criteria =c("aefficiency", "order"))
