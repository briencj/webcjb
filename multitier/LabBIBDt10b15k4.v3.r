#'# Script for a small wheat experiment with BIBDs in the lab phase
#'## The lines are assigned to an RCBD in the first phase.
#'## Uses od 2.0.0 to search for an A-optimal design, 
#'##   based on the component ratios to BP + RS.
#'## For this the number of blocks in the RCBD must equal the number of reps of the lines in the BIBD
#'## Of course, the design cannot be balanced because no. plots = no. spots
library(dae)
library(knitr)
#knitr::spin("LabBIBDt10b15k4.v3.r")
library(od)
options(width = 105)

#'## Phase 1 - randomize lines to plots using an RCBD
RCBD.lay <- designRandomize(allocated = data.frame(Lines = factor(rep(1:10, times = 6))), 
                            recipient = list(Blocks = 6, Plots = 10),
                            nested.recipients = list(Plots = "Blocks"),
                            seed = 541064)

#'## Phase 2 - randomize plots to spots
#'### Input BIBD (C&C Plan 11.16) for assigning P1
BIBD.sys <- data.frame(P1 = factor(c(1,2,3,4, 1,2,5,6, 1,3,7,8, 1,4,9,10, 1,5,7,9, 
                                     1,6,8,10, 2,3,6,9, 2,4,7,10, 2,5,8,10, 2,7,8,9, 
                                     3,5,9,10, 3,6,7,10, 3,4,5,8, 4,5,6,7, 4,6,8,9)))
BIBD.sys <- cbind(fac.gen(list(Runs = 15, Spots = 4)), BIBD.sys)
#'### Input the BIBD (C&C Plan 11.6) for assigning Blocks to Runs, accounting for P1
#'### Each row is an incomplete block for a Run, in the positions of the P1 
#'### assigned to that Run. Each column has Blocks 1-6 for a P1
BlocksRuns <- fac.gen(list(Runs = 15, P1 = 10))
BlocksRuns$Blocks <- scan(n=150, text = '
3	2	4	1	0	0	0	0	0	0
4	6	0	0	5	1	0	0	0	0
2	0	5	0	0	0	3	6	0	0
5	0	0	4	0	0	0	0	6	2
1	0	0	0	4	0	5	0	3	0
6	0	0	0	0	2	0	3	0	1
0	3	1	0	0	5	0	0	2	0
0	1	0	6	0	0	2	0	0	4
0	5	0	0	3	0	0	4	0	6
0	4	0	0	0	0	1	2	5	0
0	0	3	0	6	0	0	0	1	5
0	0	2	0	0	4	6	0	0	3
0	0	6	5	2	0	0	1	0	0
0	0	0	3	1	6	4	0	0	0
0	0	0	2	0	3	0	5	4	0')
BlocksRuns <- with(BlocksRuns, BlocksRuns[Blocks != 0,])
BlocksRuns$Blocks <- factor(BlocksRuns$Blocks)
BIBD.sys <- merge(BIBD.sys, BlocksRuns)

BIBD.sys <- with(BIBD.sys, BIBD.sys[order(Runs, Spots),])
with(BIBD.sys, table(Blocks,Runs))

#'### Randomize the systematic layout
lab.lay <- designRandomize(allocated = BIBD.sys,
                           recipient = list(Runs = 15, Spots = 4),
                           nested.recipients = list(Spots = "Runs"),
                           seed = 89661)
#'### Add the Lines and Plots factors from phase 1
lab.lay <- merge(lab.lay, 
                 within(RCBD.lay, P1 <- Lines), 
                 sort = FALSE)
lab.lay <- lab.lay[c("Runs","Spots","Blocks","Plots", "P1", "Lines")]
lab.lay

#'## Produce the anatomy of the two-phase design to check its properties
lab.canon <- designAnatomy(formulae = list(spots = ~ Runs/Spots, 
                                           plots = ~ Blocks/Plots, 
                                           trt = ~ Lines),
                           grandMean = TRUE, data = lab.lay)
summary(lab.canon, which.criteria = c("ae", "xe", "ee", "ord"))

#'### Produce the anatomy of the field phase
field.canon <- designAnatomy(formulae = list(plots = ~ Blocks/Plots, 
                                             trt = ~ Lines),
                             grandMean = TRUE, data = lab.lay)
summary(field.canon, which.criteria = c("ae", "xe", "ee", "ord"))

#'### Produce the anatomy of the BIBD design
BIBD.canon <- designAnatomy(formulae = list(spots = ~ Runs/Spots, 
                                            trt = ~ Lines),
                            grandMean = TRUE, data = lab.lay)
summary(BIBD.canon, which.criteria = c("ae", "xe", "ee", "ord"))

#'### Produce the anatomy of the first- and second-phase units
units.canon <- designAnatomy(formulae = list(spots = ~ Runs/Spots, 
                                             plots = ~ Blocks/Plots),
                             grandMean = TRUE, data = lab.lay)
summary(units.canon, which.criteria = c("ae", "xe", "ee", "ord"))

#'## Use od to search for a first-phase-conditionally optimal design, 
#'## starting with the BIBD-based two-phase design
#'### Calculate ratios of the variance components (vc) to vc.BP + vc.RS
ratios <- c(0.5, 0.5, 10)
names(ratios) <- c("g.B", "g.BP", "g.R")
ratios["g.B"] <- ratios["g.B"] * ratios["g.BP"]
ratios <- ratios / (ratios["g.BP"] + 1)
#'### Set variance component ratios
lab.start<- od(fixed = ~ Lines, 
               random = ~ Blocks + Blocks:Plots + Runs, 
               residual = ~ Runs:Spots, 
               start.values = TRUE, data=lab.lay, 
               trace = FALSE)
vp.table <- lab.start$vparameters.table
vp.table$Value <- c(ratios, 1)
print(vp.table)
#'### Search for an A-optimal design
lab.od<- od(fixed = ~ Lines, 
            random = ~ Blocks + Blocks:Plots + Runs, 
            residual = ~ Runs:Spots, 
            permute = ~ Lines | Blocks + Blocks:Plots, 
            G.param = vp.table, 
            maxit=15, search = "tabu", 
            trace = FALSE, data=lab.lay)
lab.od.lay <- lab.od$design
lab.od.lay <- lab.od.lay[, -match("P1", names(lab.od.lay))] #Remove P1

#'### Re-randomize the design
lab.od.lay <- designRandomize(allocated = lab.od.lay[c("Blocks","Plots","Lines")],
                              recipient = list(Runs = 15, Spots = 4),
                              nested.recipients = list(Spots = "Runs"),
                              seed = 61321)

#'## Produce the anatomy of the od design to check its properties
lab.od.canon <- designAnatomy(formulae = list(spots = ~ Runs/Spots, 
                                              plots = ~ Blocks/Plots, 
                                              trt = ~ Lines),
                              grandMean = TRUE, data = lab.od.lay)
summary(lab.od.canon, which.criteria = c("ae", "xe", "ee", "ord"))

#'### Produce the anatomy of the field phase of the od design to check its properties
field.od.canon <- designAnatomy(formulae = list(plots = ~ Blocks/Plots, 
                                                trt = ~ Lines),
                                grandMean = TRUE, data = lab.od.lay)
summary(field.od.canon, which.criteria = c("ae", "xe", "ee", "ord"))

#'### Produce the anatomy of the first- and second-phase units of the od design
units.od.canon <- designAnatomy(formulae = list(spots = ~ Runs/Spots, 
                                                plots = ~ Blocks/Plots),
                                grandMean = TRUE, data = lab.od.lay)
summary(units.od.canon, which.criteria = c("ae", "xe", "ee", "ord"))
