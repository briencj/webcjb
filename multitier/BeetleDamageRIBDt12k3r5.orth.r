#'# Script to investigate different structure-orthogonal second-phase 
#'# designs for the beetle damage experiment.
#'## The basic design is a Resolved Incomplete Block Design (RIBD) for v=12, k=3 and r=5. 
#'## The blocks in the second phase are (i) made equivalent to first-phase blocks, 
#'## (ii) formed by subdivding or (iii) by combining first-phase blocks.

#'### Initialize
library(knitr)
#knitr::spin("BeetleDamageRIBDt12k3r5.orth.r")
library(dae)
options(width = 105)
opts_chunk$set("tidy" = FALSE, comment = NA) 

#'### A set of values for the gammas, the ratios of variance components (vc)
#'### g.RB is the ratio of vc.RB to vc.RBS
#'### g.RBS and g.OP are ratios of vc.RBS and vc.OP to vc.OPC
gammas <- c(0.5,0.5,0.5)
names(gammas) <- c("g.RB", "g.RBS", "g.OP")
print(gammas)

#'## Function to get AVPD for a design
#'### Have to ensure that supplied variance component values are ratios to vc.RBS + vc.OPC
getAVPD <- function(layout, gammas)
{
  #Calculate ratios of variance components (vc) to vc.BRC + vc.DA
  ratios <- c(gammas[["g.RB"]] * gammas[["g.RBS"]], gammas[["g.OP"]])
  ratios <- ratios / (gammas[["g.RBS"]] + 1)
  #Calculate the variance matrix of the predictions
  Vp <- mat.Vpredicts(target = ~ Damage - 1, 
                      fixed = ~ Reps + Occasions - 1, 
                      random = ~ Reps:Benches + Occasions:Plates - 1, 
                      G = as.list(ratios), 
                      design = layout)
  #Calculate Ameasures
  AVPD <- designAmeasures(Vp)
  return(AVPD)  
}

#'### Function to randomize the two-phase design and compute before and after AVPD values
getSummaryAVPD <- function(layout, gammas, randomize = TRUE)
{
  #Calculate Ameasures
  AVPD <- getAVPD(layout, gammas)
  
  #Randomize the final design and recalculate the AVPD
  if(randomize) 
  {
    unit.facs <- with(layout, list(Occasions = levels(Occasions), 
                                   Plates = levels(Plates), 
                                   Cells = levels(Cells)))
    layout <- with(layout, layout[order(Occasions, Plates, Cells), ])
    layout <- with(layout, layout[, !((colnames(layout) %in% 
                                         c("Occasions", "Plates", "Cells")))])
    layout <- designRandomize(allocated = layout,
                              recipient = unit.facs,
                              nested.recipients = list(Plates = "Occasions", 
                                                       Cells = c("Plates", 
                                                                 "Occasions")))
    
    AVPD.rand <- getAVPD(layout, gammas)
    AVPD <- c(as.vector(AVPD),as.vector(AVPD.rand))
    names(AVPD) <- c("final","post_rand")
  } else
  {
    AVPD <- as.vector(AVPD)
    names(AVPD) <- "final"
  }
  
  return(AVPD)
}  

#'### Function to get anatomies associated with a two-phase design
getSummary <- function(layout, gammas, randomize = TRUE)
{
  #'### Produce the anatomy for the two-phase design to check its properties
  beetle2.canon <- designAnatomy(formulae = list(cells = ~ Occasions/Plates/Cells, 
                                                 spots = ~ Reps/Benches/Spots,
                                                 trt = ~ Damage),
                                 grandMean = TRUE, data = layout)
  
  #'### Produce the anatomy of the first-phase design
  beetle2.ph1.canon <- designAnatomy(formulae = list(spots = ~ Reps/Benches/Spots,
                                                     trt = ~ Damage),
                                     grandMean = TRUE, data = layout)
  
  #'### Produce the anatomy of the cross-phase design
  beetle2.cross.canon <- designAnatomy(formulae = list(cells = ~ Occasions/Plates/Cells, 
                                                       trt = ~ Damage),
                                       grandMean = TRUE, data = layout)
  
  #'### Produce the anatomy of the first- and second-phase units
  beetle2.units.canon <- designAnatomy(formulae = list(cells = ~ Occasions/Plates/Cells, 
                                                       spots = ~ Reps/Benches/Spots),
                                       grandMean = TRUE, data = layout)
  
  AVPD <- getSummaryAVPD(layout, gammas, randomize = randomize)
  
  return(list(complete = beetle2.canon, 
              phase1 = beetle2.ph1.canon, 
              cross = beetle2.cross.canon, 
              units = beetle2.units.canon, 
              AVPD = AVPD))
}  


#'## Input the systematic RIBD
Damage <- factor(c(1,6,11, 2,7,12, 3,8,9, 4,5,10,
                   1,5,9, 3,7,11, 2,6,10, 4,8,12, 
                   1,8,10, 4,7,9, 2,5,11, 3,6,12,
                   1,5,12, 2,6,9, 3,7,10, 4,8,11, 
                   1,7,9, 3,5,11, 2,8,10, 4,6,12))

#'## Two-phase design with second phase units corresponding to first-phase units 
#'### Phase 1 - randomize treatments to spots for corresponding
beetle1.corr.lay <- designRandomize(allocated = Damage,
                                    recipient = list(Reps = 5, Benches =  4, Spots = 3),
                                    nested.recipients = list(Benches = "Reps", 
                                                             Spots =c("Benches", "Reps")),
                                    seed = 646441)
#'### Phase 2 - randomize spots (and treatments) to cells for corresponding spots and cells
beetle2.corr.lay <- designRandomize(allocated = beetle1.corr.lay,
                                    recipient = list(Occasions = 5, Plates = 4, Cells = 3),
                                    nested.recipients = list(Plates = "Occasions", 
                                                             Cells = c("Plates", 
                                                                       "Occasions")),
                                    seed = 529419)
#'### Get anatomies & AVPDs
beetle2.corr.summ <- getSummary(beetle2.corr.lay, gammas)
cat("\n#### Before- and after-randomzation AVPDs::\n")
(beetle2.corr.summ$AVPD)
cat("\n#### Two-phase anatomy:\n")
summary(beetle2.corr.summ$complete)
cat("\n#### First-phase anatomy:\n")
summary(beetle2.corr.summ$phase1)
cat("\n#### Second-phase anatomy:\n")
summary(beetle2.corr.summ$units)

#'## Two-phase design with first-phase blocks subdivided in second phase
#'### Randomize treatments to spots, after adding pseudofactors for the second phase
RIBD.sys <- fac.gen(list(Reps = 5, 2, S1 = 2, S2 = 3))
RIBD.sys$Damage <- Damage
beetle1.lay <- designRandomize(allocated = RIBD.sys[c("S1", "S2", "Damage")],
                               recipient = list(Reps = 5, Benches =  2, Spots = 6),
                               nested.recipients = list(Benches = "Reps", 
                                                        Spots =c("Benches", "Reps")),
                               seed = 921133)
#'### Reorder first phase and randomize the second phase
beetle1.sys <- with(beetle1.lay, beetle1.lay[order(Reps,Benches,S1,S2),])

#'### Produce the anatomy for the first phase design to check its properties
beetle1.canon <- designAnatomy(formulae = list(spots = ~ Reps/Benches/Spots,
                                               trt = ~ Damage),
                               grandMean = TRUE, data = beetle1.lay)
summary(beetle1.canon)

#'### Phase 2 - randomize spots (and treatments) to cells for subdivided
beetle2.div.lay <- designRandomize(allocated = beetle1.sys,
                                   recipient = list(Occasions = 5, Plates = 4, Cells = 3),
                                   nested.recipients = list(Plates = "Occasions", 
                                                            Cells = c("Plates", 
                                                                      "Occasions")),
                                   seed = 25253)
#'### Get anatomies & AVPDs
beetle2.div.summ <- getSummary(beetle2.div.lay, gammas)
cat("\n#### Before- and after-randomzation AVPDs::\n")
(beetle2.div.summ$AVPD)
cat("\n#### Two-phase anatomy:\n")
summary(beetle2.div.summ$complete)
cat("\n#### First-phase anatomy:\n")
summary(beetle2.div.summ$phase1)
cat("\n#### Second-phase anatomy:\n")
summary(beetle2.div.summ$units)


#'## Two-phase design with second phase blocks formed by combining first-phase blocks 
#'### Phase 1 - use the corresponding first phase
#'### Phase 2 - randomize spots (and treatments) to cells for combining
#'###  - pairs of blocks are assigned to a plate
beetle2.comb.lay <- designRandomize(allocated = beetle1.corr.lay, 
                                     recipient = list(Occasions = 5, Plates = 2, Cells = 6),
                                     nested.recipients = list(Plates = "Occasions", 
                                                              Cells = c("Plates", 
                                                                        "Occasions")),
                                     seed = 46414)
#'### Get anatomies & AVPDs
beetle2.comb.summ <- getSummary(beetle2.comb.lay, gammas)
cat("\n#### Before- and after-randomzation AVPDs::\n")
(beetle2.comb.summ$AVPD)
cat("\n#### Two-phase anatomy:\n")
summary(beetle2.comb.summ$complete)
cat("\n#### First-phase anatomy:\n")
summary(beetle2.comb.summ$phase1)
cat("\n#### Second-phase anatomy:\n")
summary(beetle2.comb.summ$units)


#'## Print the two-phase anatomies together 
#+ echo = FALSE
cat("\n#### Corresponding first- and second-phase units\n")
summary(beetle2.corr.summ$complete)
cat("\n#### Subdivided first-phase units\n")
summary(beetle2.div.summ$complete)
cat("\n#### Combined first-phase units\n")
summary(beetle2.comb.summ$complete)
