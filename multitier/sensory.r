#'# Two-phase wine sensory experiment from Brien & Payne (1999)

#'## Intialize
library(dae)
options(width=110)
load("./data/sensory.dat.rda")

#'## Shorten names
sensory.short.dat <- sensory.dat 
names(sensory.short.dat)[match(c("Occasions", "Intervals", "Sittings",  "Positions", "Judges", 
                               "Squares", "Rows", "Columns", "Halfplots", "Trellis", "Method"),
                             names(sensory.short.dat))] <- 
  c("Occ", "Int", "Sit",  "Posn", "Jud", "Sqr", "Row", "Col", "Hplot", "Trel", "Meth")

#'## Produce the anatomy of the design
sensory.canon <- designAnatomy(list(eval=~ ((Occ/Int/Sit)*Jud)/Posn, 
                                    field=~ (Row*(Sqr/Col))/Hplot,
                                    treats=~ Trel*Meth), data=sensory.short.dat)
summary(sensory.canon, which.criteria =c("aefficiency", "order"))
