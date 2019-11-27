#'# Script for the large two-phase variety experiment with noncorresponding phases: 
#'# starting designs
#'### Initialize
library(knitr)
#knitr::spin("Variety.twoph.cols9.startdes.r")
library(dae)
options(width = 105)

#'# Set up a set of gammas for calculating average variance of pairwise differences (AVPD)
#'### g.B, g.BR, g.C and g.BC are the ratios of their varianc components (vc) to vc.BRC
#'### g.BRC, g.D and g.A are ratios of their vc to vc.DA
gammas <- list(g.B = 0.5, g.BR = 0.3, g.C = 0.25, g.BC = 0.05, g.BRC = 0.5, 
               g.D = 0.5, g.A = 0.5)

#'# Functions to calculate AVPD and decompositions for the starting designs
#'## Function to get AVPD for a starting design
#'### Have to ensure that supplied variance component values are ratios to vc.BRC + vc.DA
getAVPD <- function(layout, gammas)
{
  #Calculate ratios of variance components (vc) to vc.BRC + vc.DA
  ratios <- c(gammas[["g.BR"]] * gammas[["g.BRC"]], 
              gammas[["g.C"]] * gammas[["g.BRC"]], 
              gammas[["g.BC"]] * gammas[["g.BRC"]], 
              gammas[["g.D"]],
              gammas[["g.A"]])
  ratios <- ratios / (gammas[["g.BRC"]] + 1)
  #Calculate the variance matrix of the predictions
  Vp <- mat.Vpredicts(target = ~ Blocks + Lines -1, 
                      random = ~ Blocks:Rows + Columns/Blocks + Days + Analyses - 1, 
                      G = as.list(ratios), 
                      design = layout)
  #Calculate Ameasures
  AVPD <- designAmeasures(Vp)
  return(AVPD)  
}


#'## Function to compute summaries for a starting design, assuming random Days & Analyses
getSummary <- function(layout, gammas, randomize = TRUE)
{
  # Obtain the anatomy of the complete two-phase design
  ph2.canon <- designAnatomy(formulae = list(lab =  ~ Days*Analyses, 
                                             plot = ~ (Blocks/Rows)*Columns, 
                                             trt  = ~ Lines),
                             data = layout)
  
  #Calculate Ameasures
  AVPD <- getAVPD(layout, gammas)

  # Obtain the anatomy of the units of the two-phase design
  ph2units.canon <- designAnatomy(formulae = list(lab =  ~ Days*Analyses, 
                                                  plot = ~ (Blocks/Rows)*Columns),
                                  data = layout)
  
  # Produce the anatomy of the first-phase treatments and the second-phase units
  ph2trts.canon <- designAnatomy(formulae = list(lab =  ~ Days*Analyses, 
                                                 trt  = ~ Lines),
                                 data = layout)
  
  #Randomize the final design and recalculate the AVPD
  if(randomize) 
  {
    layout <- with(layout, layout[order(Days, Analyses), ])
    layout <- with(layout, layout[, !((colnames(layout) %in% c("Days","Analyses")))])
    layout <- designRandomize(allocated = layout,
                              recipient = list(Days = 50, Analyses = 9))
    
    #Calculate Ameasures for randomized design
    AVPD.rand <- getAVPD(layout, gammas)
    AVPD <- c(as.vector(AVPD),as.vector(AVPD.rand))
    names(AVPD) <- c("final","post_rand")
  } else
  {
    AVPD <- as.vector(AVPD)
    names(AVPD) <- "final"
  }
  
  return(list(complete = ph2.canon, units = ph2units.canon, treats = ph2trts.canon, 
              AVPD = AVPD))
}  

#'# Set up starting designs
#'## Phase 1 - randomize lines to plots
#'### Input the Resolved Row-Column Design (RRCD) for the first phase from CycDesigN
Lines <- scan(n = 450, text='
              3   101 92  85  121 132 130 70  74  45  94  143 47  100 125 93  11  63  57  73  8   48 
              99  13  131 4   75  95  117 21  88  53  126 15  102 31  26  29  55  7   24  56  129 10 
              91  41  140 146 20  54  98  44  116 103 136 128 84  149 52  58  23  135 87  105 35  50 
              76  9   120 77  42  19  90  33  108 147 34  59  124 12  28  150 32  18  97  148 14  5  
              139 86  127 72  2   144 141 115 96  138 142 49  65  104 25  80  145 64  118 109 107 119
              137 67  79  122 68  89  37  66  134 62  110 81  40  61  82  78  113 133 83  114 111 106
              43  71  39  60  16  22  38  17  69  6   51  36  27  46  123 112 30  1   148 141 81  79 
              5   41  92  29  143 27  117 144 50  55  21  11  61  147 145 45  87  51  42  31  52  83 
              112 84  89  102 116 125 67  33  126 53  14  95  127 121 69  128 13  124 114 48  109 115
              60  57  106 85  54  18  72  149 7   70  108 46  104 97  22  129 110 73  71  74  6   137
              24  32  118 96  9   23  44  10  8   17  122 135 107 111 86  49  59  25  75  130 82  90 
              131 101 62  138 26  2   58  120 37  78  38  30  34  132 4   47  113 142 28  119 123 77 
              15  133 64  91  140 146 68  16  103 20  76  94  39  88  99  93  12  19  134 43  80  36 
              56  35  65  98  1   136 139 63  40  105 100 150 3   66  138 5   44  75  39  35  33  144
              40  132 31  118 83  88  46  12  38  106 42  116 53  66  97  58  142 92  8   93  22  10 
              48  99  62  103 24  112 141 3   101 52  108 59  50  114 28  128 23  137 119 57  2   18 
              113 117 1   16  102 14  70  43  81  146 105 125 6   86  51  134 7   95  82  41  65  55 
              104 45  56  80  78  147 96  15  21  123 111 13  149 109 135 79  30  89  29  150 72  20 
              131 36  73  85  64  77  130 37  126 63  121 47  9   122 68  110 69  148 19  139 26  140
              145 61  107 115 127 27  100 133 84  87  76  34  129 74  17  4   136 91  71  11  60  90 
              143 94  54  25  67  49  124 32  98  120')
Lines <- factor(Lines)

#'### Randomize the first-phase design
ph1.lay <- designRandomize(allocated = Lines,
                           recipient = list(Blocks = 3, Rows = 10, Columns = 15),
                           nested.recipients = list(Rows = "Blocks"),
                           seed = 113111)

#'### Obtain the anatomy of the first phase design to check its properties
ph1.canon <- designAnatomy(formulae = list(plot = ~ (Blocks/Rows)*Columns, 
                                           trt  = ~ Lines),
                           data = ph1.lay)
summary(ph1.canon, which.criteria = c("ae","ee", "ord", "dfor"))

#'## (i) Ordered design used to assign plots (and lines) to analyses in phase 2
#'### Form two-phase design
ph2.ord.lay <- cbind(fac.gen(list(Days = 50, Analyses=9)),
                     ph1.lay)

#'### Obtain the summary of the ordered two-phase design, including its anatomy
ph2.ord.summ <- getSummary(ph2.ord.lay, gammas = gammas, randomize = TRUE)
print(ph2.ord.summ$AVPD)
print(summary(ph2.ord.summ$complete, which.criteria = c("ae","ee","order")))
print(summary(ph2.ord.summ$units, which.criteria = c("ae","ee","order")))
print(summary(ph2.RCD.summ$treats, which.criteria = c("ae","ee","order")))

#'## (ii) Randomization used to assign plots (and lines) to analyses in phase 2
#'### Randomize second-phase design
ph2.CRD.lay <- designRandomize(allocated = ph1.lay, 
                               recipient = list(Analyses=450),
                               seed = 85476)
ph2.CRD.lay <- cbind(fac.gen(list(Days = 50, Analyses=9)),
                     ph2.CRD.lay[, -match("Analyses", names(ph2.CRD.lay))])

#'### Obtain the summary of the randomized two-phase design, including its anatomy
ph2.CRD.summ <- getSummary(ph2.CRD.lay, gammas = gammas, randomize = TRUE)
print(ph2.CRD.summ$AVPD)
print(summary(ph2.CRD.summ$complete, which.criteria = c("ae","ee","order")))
print(summary(ph2.CRD.summ$units, which.criteria = c("ae","ee","order")))
print(summary(ph2.CRD.summ$treats, which.criteria = c("ae","ee","order")))

#'## (iii) RCD from CycDesign used to assign lines (and plots) in phase 2
#'### Input the Row-Column Design (RCD) from CycDesigN
Lines <- scan(n = 450, text='
              52  51  24 115 109 150  69  74 144 117   4  25 118  88  68  63  48 137
              102 106 125  75  12 110 144  61  71 132 124  59 113  82  57  61  80  33
              121  87  80  60 133  24  58 106  44  12  65 133 108  89 134  39  59 139
              82  73  66 111 112 142  47 130 120  49  46 138   6 149 106  51  98  81
              45  12 117  26  15 143   6  10 101 140  50 101  30  17 128  97  84  60
              76 141  96  67   4  90 107 136  99  11  35  29 139 103  32  90  97 131
              138 132  21  69  55  89 123   4  93 114 139  13 149 121  62 128 115 130
              32  13  51  55  76  88  33  91  42 108 148 119 125  67   8  73 129  38
              27  40   5 103  31  77  16  25  62  92   9 149  19  56  87 105 112  65
              22  23 110  14 127   5 118 119 146 146  53  36 120 124 136 121  85  91
              63 147  27  44  23  30  36  90  26  94  79 147  85 142  72 126 144   7
              129  33  41 141 150 117  54  17  86  86 130  79  16  96 122  78  32 113
              6  52  18  95 104 127  99  58  85  93  34 148  81 114 141 143  47  59
              147 122 102 105  81  18 129  20  31 110 101  43 126  78  70 114 145 116
              99  75  68 150  97  13   1 100  10  57  20  65  54 140  38  40  70  11
              25  83 123  70  34 104 133  92  75   1 137   3 102   2  50 142  55  41
              19   2 131 132  37 125  46 127 128  72  77 140  80 135  46 111  41  56
              136  26  47   1 116  39  79  37  24 115  64  95  96  38  83 135 138  63
              50  95 124  15  71  21  22  62  92  74 100  78  37  44  28 137 143  18
              16 109  84 148   7  45  57   9  48  64 146  10  28 126  56  29 108  21
              8  17  76  77  45 120  14   2 123  87  93  14  88  27  98  84 134  73
              145  67  94  48  29  69  19 122   5  60  28 112  11  61  53   8  23 107
              42 111  74  22  20   7  89  64   3 100 116  35  36 119 105  71  72  49
              34  58  54  42  68  35  31  30 145  66  94 104 134  49 113  91  40 109
              131 118 107  43  52  82  83  15  98  39 103  53  66  86   3   9  43 135')
Lines <- factor(Lines)

#'### Randomize RCD-based design for second phase
ph2.RCD.lay <- designRandomize(allocated = Lines, 
                               recipient = list(Days = 50, Analyses=9),
                               seed = 85476)
ph2.RCD.lay$Blocks <- fac.nested(ph2.RCD.lay$Lines)
ph2.RCD.lay <- merge(ph2.RCD.lay, ph1.lay, sort = FALSE)

#'### Obtain the summary of the RCD-based two-phase design, including its anatomy
ph2.RCD.summ <- getSummary(ph2.RCD.lay, gammas = gammas, randomize = TRUE)
print(ph2.RCD.summ$AVPD)
print(summary(ph2.RCD.summ$complete, which.criteria = c("ae","ee","order")))
print(summary(ph2.RCD.summ$units, which.criteria = c("ae","ee","order")))
print(summary(ph2.RCD.summ$treats, which.criteria = c("ae","ee","order")))
