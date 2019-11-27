#'# Script for a small wheat experiment with a Youden square in the lab phase

#'### Initialize
library(dae)
library(knitr)
#knitr::spin("McIntyeExample4Youden.v2.r")
options(width=105)

#'## Phase 1 - randomize lines to plots
Lines <- factor(rep(1:7, times = 6))
wht1.lay <- designRandomize(allocated = Lines,
                            recipient = list(Blocks=6, Plots=7),
                            nested.recipients = list(Plots = "Blocks"),
                            seed = 54744)
#'### Produce the anatomy of the field phase
wht1.canon <- designAnatomy(formulae = list(plots = ~Blocks/Plots,
                                            trts = ~Lines),
                            grandMean = TRUE, data = wht1.lay)
summary(wht1.canon, which.criteria =c("aefficiency", "order"))

#'## Phase 2 - assign and randomize plots (and lines) to analyses
#'### Set up systematic layout of phase 1 design for phase 2 randomization
ph2.sys <- fac.gen(list(Blocks=6, P1=7))
ph2.sys$P1 <- factor(designLatinSqrSys(7)[1:42]) # Get Youden square
#'### Randomize systematic layout
wht2.lay <- designRandomize(allocated = ph2.sys,
                            recipient = list(Runs = 6, Analyses = 7),
                            seed = 16455)
#'### Add the Lines and Plots factors from phase 1
wht2.lay <- merge(wht2.lay, 
                  within(wht1.lay, P1 <- Lines), 
                  sort = FALSE)
wht2.lay <- wht2.lay[c("Runs", "Analyses", "Blocks", "Plots", "P1", "Lines")]
wht2.lay

#'## Get the anatomy of the design to check its properties
wht2.canon <- designAnatomy(formulae = list(anls = ~Runs*Analyses,
                                            plots = ~Blocks/Plots,
                                            trts = ~Lines),
                            grandMean = TRUE, data = wht2.lay)
summary(wht2.canon, which.criteria =c("aefficiency", "order"))

#'### Produce the anatomy of the first- and second-phase units
wht2.units.canon <- designAnatomy(formulae = list(anls = ~Runs*Analyses,
                                                  plots = ~Blocks/Plots),
                                  grandMean = TRUE, data = wht2.lay)
summary(wht2.units.canon)

#'### Produce the anatomy of the first-phase treatments and the second-phase units
wht2.trts.canon <- designAnatomy(formulae = list(anls = ~Runs*Analyses,
                                                 trts = ~Lines),
                                 grandMean = TRUE, data = wht2.lay)
summary(wht2.trts.canon)
