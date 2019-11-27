#'# Two-phase wine experiment from Brien, May and Mayo (1987, Experiment 2)

#'## Intialize
library(dae)
options(width=110)
load("./data/WIN72.dat.rda")

#'## Produce the anatomy of the design
WIN72.canon <- designAnatomy(list(eval=~  ((Occasions/Sessions)*Judges)/Glasses, 
                                    field=~ (Treatments/WineReps/Bottles)*Occasions), 
                             data=WIN72.dat)
summary(WIN72.canon, which.criteria =c("aefficiency", "order"))
