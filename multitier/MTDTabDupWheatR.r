#'# Script for the two-phase duplicated wheat experiment
#'### Initialize
library(dae)
library(knitr)
#knitr::spin("MTDTabDupWheatR.r")
options(width = 100)

#'## Generate a layout for the field phase  - a randomized complete block design 
Lines  <- factor(rep(1:49, times=4))
field.lay <- designRandomize(allocated = Lines,
                             recipient = list(Blocks = 4, Plots = 49),
                             nested.recipients = list(Plots = "Blocks"),
                             seed = 54132)

#'## Phase 2 design - Form systematic design for allocating samples; 
#'### Set up two systematic 7 balanced lattice square designs 
lab.alloc <- fac.gen(list(Occasions = 2, Intervals = 4, Runs = 7, Times = 7))
lab.alloc$Lines <- factor(rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
                              19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 
                              35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 
                              1, 44, 38, 32, 26, 20, 14, 21, 8, 2, 45, 39, 33, 27, 34, 28, 15, 
                              9, 3, 46, 40, 47, 41, 35, 22, 16, 10, 4, 11, 5, 48, 42, 29, 23, 
                              17, 24, 18, 12, 6, 49, 36, 30, 37, 31, 25, 19, 13, 7, 43, 
                              1, 30, 10, 39, 19, 48, 28, 35, 8, 37, 17, 46, 26, 6, 13, 42, 15, 
                              44, 24, 4, 33, 40, 20, 49, 22, 2, 31, 11, 18, 47, 27, 7, 29, 9, 
                              38, 45, 25, 5, 34, 14, 36, 16, 23, 3, 32, 12, 41, 21, 43, 
                              1, 16, 31, 46, 12, 27, 42, 49, 8, 23, 38, 4, 19, 34, 41, 7, 15, 
                              30, 45, 11, 26, 33, 48, 14, 22, 37, 3, 18, 25, 40, 6, 21, 29, 44, 
                              10, 17, 32, 47, 13, 28, 36, 2, 9, 24, 39, 5, 20, 35, 43), 
                            times = 2))
#'### Randomize Samples to Occasions
samp.lay <- designRandomize(allocated = data.frame(Samples = factor(rep(1:2, times = 196))),
                            recipient = list(plots = 196, Occasions = 2),
                            nested.recipients = list(Occasions = "plots"),
                            seed = 7145)
samp.lay <- with(samp.lay, samp.lay[order(Occasions, plots),])
#'### Form design for allocating samples and lines
field.lay <- rbind(field.lay, field.lay)
field.lay <- within(field.lay, 
                    {
                      Samples <- samp.lay$Samples
                      Intervals <- Blocks
                      Occasions <- Samples
                    })
lab.alloc <- merge(lab.alloc, field.lay)


lab.alloc <- with(lab.alloc, lab.alloc[order(Occasions, Intervals, Runs,Times),])
lab.alloc <- lab.alloc[c("Blocks","Plots", "Samples", "Lines")] #Reduce columns in lab.alloc

#'## Generate a layout for Phase 2
lab.lay <- designRandomize(allocated = lab.alloc, 
                           recipient = list(Occasions = 2, Intervals = 4, Runs = 7, Times = 7),
                           nested.recipients = list(Intervals = "Occasions", 
                                                    Runs = c("Occasions", "Intervals"), 
                                                    Times = c("Occasions", "Intervals")),
                           seed = 646161)

#'## Produce the anatomy of the design
wheat.canon <- designAnatomy(formulae = list(analyses = ~ Occasions/Intervals/(Runs*Times), 
                                             samples = ~ Blocks/Plots/Samples,
                                             lines = ~Lines), 
                             data = lab.lay)
summary(wheat.canon, which.criteria =c("aefficiency", "order"))

