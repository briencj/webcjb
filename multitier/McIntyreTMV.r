#'# Produce the analtomy of the design for McIntyre's (1955) experiment on Tobacco Mosaic Virus

#'## Initialize
library(dae)
load("./data/McIntyreTMV.dat.rda")

#'## Produce the anatomy of the design
TMV.canon <- designAnatomy(formulae = list(assay = ~((Lot/DatPlant)*AssPosn)/HalfLeaf, 
                                           test = ~(Set/NicPlant)*Posn,
                                           trt = ~Treat),
                           data=McIntyreTMV.dat)
summary(TMV.canon, which.criteria=c("aeff", "order"))
