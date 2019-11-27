#'# Script for Jarrett & Ruggiero the two-phase microarray experiment
#'### Initialize
library(dae)
library(knitr)
#knitr::spin("MicroarrayJarrettRuggieroBIBDRandomized.v3.r")
options(width=120)

#'## Phase 1 - randomize treatments to samples
Treats <- factor(rep(c(1,2,4, 2,3,5, 3,4,6, 4,5,7,
                       5,6,1, 6,7,2, 7,1,3), each=2))
jr1.lay <- designRandomize(allocated = Treats,
                           recipient = list(Blocks=7, Plants=3, Samples=2),
                           nested.recipients = list(Plants = "Blocks",
                                                    Samples = c("Plants", "Blocks")),
                           seed = 13406)
jr1.canon <- designAnatomy(formulae = list(samples = ~Blocks/Plants/Samples,
                                           trts = ~Treats),
                           grandMean = TRUE, data = jr1.lay)
summary(jr1.canon, which.criteria =c("aefficiency", "order"))

#'## Phase 2 - assign and randomize samples (and treatments) to array-dyes
#'### Set up systematic layout for phase 2 design
ph2.sys <- fac.gen(list(Blocks=7, Plants=3, Samples=2))
ph2.sys$Plants <- factor(rep(c(1,2, 2,3, 3,1), times = 7))
ph2.sys <- merge(ph2.sys, jr1.lay, all = TRUE, sort = FALSE)
#'### Randomize systematic layout
jr2.lay <- designRandomize(allocated = ph2.sys,
                           recipient = list(Arrays = 21, Dyes = 2),
                           seed = 90508)
jr2.lay <- with(jr2.lay, jr2.lay[order(Blocks,Plants,Samples), ])
#'### Randomize samples to dyes
samp.lay <- designRandomize(allocated = jr1.lay$Samples,
                            recipient =  list(BP=21, S=2),
                            nested.recipients = list(S = "BP"),
                            seed = 87235)
jr2.lay$Samples <- samp.lay[[3]]
jr2.lay <- with(jr2.lay, jr2.lay[order(Arrays,Dyes), ])
jr2.lay

#'## Produce the anatomy of the two-phase design to check its properties
jr2.canon <- designAnatomy(formulae = list(arrdye = ~Dyes*Arrays,
                                           samples = ~Blocks/Plants/Samples,
                                           trts = ~Treats),
                           grandMean = TRUE, data = jr2.lay)
summary(jr2.canon, which.criteria =c("aefficiency", "order"))

#'### Produce the anatomy of the first- and second-phase units
jr2.units.canon <- designAnatomy(formulae = list(arrdye = ~Dyes*Arrays,
                                                 samples = ~Blocks/Plants/Samples),
                                 grandMean = TRUE, data = jr2.lay)
summary(jr2.units.canon)

#'### Produce the anatomy of the first-phase treatments and the second-phase units
jr2.trts.canon <- designAnatomy(formulae = list(arrdye = ~Dyes*Arrays,
                                                trts = ~Treats),
                                grandMean = TRUE, data = jr2.lay)
summary(jr2.trts.canon)
