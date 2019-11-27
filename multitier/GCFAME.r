#'# Biodiversity experiment from Brien, Harch, Correll and Bailey (2011, Appendix G)

#'## Intialize
library(dae)
options(width=110)
load("./data/GCFAME.dat.rda")

#'## Shorten the names of the factors
GCFAME.short.dat <- GCFAME.dat 
names(GCFAME.short.dat)[match(c("Occasion", "Block", "Plot", "Sample", "Tillage"),
                             names(GCFAME.short.dat))] <- c("Occ", "Blk", "Plot", "Samp", "Till")


#'## Produce the anatomy of the design
GCFAME.canon <- designAnatomy(list(lab  = ~ Occ/Int1/Int2/Int3/Int4/Int5, 
                                  field = ~ ((Blk/Plot)*Depth)/Samp,
                                  trt   = ~ Till*Depth*Method), 
                             data=GCFAME.short.dat)
summary(GCFAME.canon, which.criteria =c("aefficiency", "order"))
