##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                           Correlations in Numbers
#                                 Fragebogen

require(dplyr)
require(plyr)

##### FÜR FEHLERRATEN VON AY #####
# ## Dataframe
# 
# FAs_xy <- FAs_sum_All %>% dplyr::select(ID, Rew, Trialtype, Phase, Fehlerrate) %>%
#   filter(Trialtype == 3)
# 
# FAs_FR <- reshape2::dcast(FAs_xy, ID + Rew ~ Trialtype + Phase, value.var="Fehlerrate")
# 
# FAs_FR[is.na(FAs_FR)] <- 0.001
# 
# # FB Daten
# 
# Fragebogen_precorr <- select(Fragebogen_Skalen, ID, BI:DK)
# 
# Fragebogen_corr_FR <- merge(Fragebogen_precorr, FAs_FR, "ID")
# names(Fragebogen_corr_FR)[12:14] <- c("AY-Fehler B", "AY-Fehler nR", "AY-Fehler R")

## Korrelation

require(psych)
M_FR <- corr.test(Fragebogen_corr_FR[, -c(1,11)], adjust = 'fdr')
diag(M_FR$r) = NA
diag(M_FR$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_FR$r, p.mat = M_FR$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         addCoef.col = "black",
         # sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         # addgrid.col =  NA,
         insig = "p-value",
         sig.level = c(.001, .01, .05), pch.cex = 1.5,
         # pch.col = "white",
         # na.label.col = NA, 
         na.label = NA, 
         # cl.ratio = .25, cl.length = 11, cl.cex = 1
         )

##### FÜR RT VON HITS #####
## Dataframe

Hits_xy <- Hits_sum_All %>% dplyr::select(ID, Rew, Trialtype, Phase, m_RT)

Hits_RT <- reshape2::dcast(Hits_xy, ID + Rew ~ Trialtype + Phase, value.var="m_RT")

# FB Daten

Fragebogen_precorr <- select(Fragebogen_Skalen, ID, BI:DK)

Fragebogen_corr_RT <- merge(Fragebogen_precorr, Hits_RT, "ID")
names(Fragebogen_corr_RT)[12:23] <- c("AX-RT B", "AX-RT nR", "AX-RT R",
                                      "BX-RT B", "BX-RT nR", "BX-RT R",
                                      "AY-RT B", "AY-RT nR", "AY-RT R",
                                      "BY-RT B", "BY-RT nR", "BY-RT R")


## Korrelationen alles RT
require(psych)
M_RT <- corr.test(Fragebogen_corr_RT[, -c(1,11:23)], adjust = 'none')
diag(M_RT$r) = NA
diag(M_RT$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT$r, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1,
         # order = "hclust",
         addrect = 3,
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "p-value",
         #sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         #pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)


## Korrelationen für AX und AY

require(psych)
M_RT_A <- corr.test(Fragebogen_corr_RT[, -c(1,11, 15:17, 21:23)], adjust = 'fdr')
diag(M_RT_A$r) = NA
diag(M_RT_A$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT_A$r, p.mat = M_RT_A$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "p-value",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         # pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

## Korrelationen für BX und BY

require(psych)
M_RT_B <- corr.test(Fragebogen_corr_RT[, -c(1,11:14,18:20)], adjust = 'fdr')
diag(M_RT_B$r) = NA
diag(M_RT_B$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT_B$r, p.mat = M_RT_B$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "p-value",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         # pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

##### Korrelationen getrennt nach Rew-Gruppe ######
##### Fehlerraten #####

# dataset
FAs_xy <- FAs_sum_All %>% dplyr::select(ID, Rew, Trialtype, Phase, Fehlerrate) %>%
  filter(Trialtype == 3)

FAs_FR <- reshape2::dcast(FAs_xy, ID + Rew ~ Trialtype + Phase, value.var="Fehlerrate")

FAs_FR[is.na(FAs_FR)] <- 0.001

# FB Daten

Fragebogen_precorr <- select(Fragebogen_Skalen, ID, BI:DK)

Fragebogen_corr_FR <- merge(Fragebogen_precorr, FAs_FR, "ID")
names(Fragebogen_corr_FR)[12:14] <- c("AY-Fehler B", "AY-Fehler nR", "AY-Fehler R")

## split dataframe by Rew

Fragebogen_corr_FR_rew0 <- filter(Fragebogen_corr_FR, Rew == 0)
Fragebogen_corr_FR_rew1 <- filter(Fragebogen_corr_FR, Rew == 1)


##### Korrelation FR getrennt nach Rew ######

require(psych)
M_FR_rew1 <- corr.test(Fragebogen_corr_FR_rew1[, -c(1,11)], adjust = 'fdr')
diag(M_FR_rew1$r) = NA
diag(M_FR_rew1$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_FR_rew1$r, p.mat = M_FR$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)



require(psych)
M_FR_rew0 <- corr.test(Fragebogen_corr_FR_rew0[, -c(1,11)], adjust = 'fdr')
diag(M_FR_rew0$r) = NA
diag(M_FR_rew0$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_FR_rew0$r, p.mat = M_FR$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

####### Korrelationen für RT getrennt nach Rew #####

## Dataframe

Hits_xy <- Hits_sum_All %>% dplyr::select(ID, Rew, Trialtype, Phase, m_RT)

Hits_RT <- reshape2::dcast(Hits_xy, ID + Rew ~ Trialtype + Phase, value.var="m_RT")

# FB Daten

Fragebogen_precorr <- select(Fragebogen_Skalen, ID, BI:DK)

Fragebogen_corr_RT <- merge(Fragebogen_precorr, Hits_RT, "ID")
names(Fragebogen_corr_RT)[12:23] <- c("AX-RT B", "AX-RT nR", "AX-RT R",
                                      "BX-RT B", "BX-RT nR", "BX-RT R",
                                      "AY-RT B", "AY-RT nR", "AY-RT R",
                                      "BY-RT B", "BY-RT nR", "BY-RT R")

#### nach Rew trennen

Fragebogen_corr_RT_rew0 <- filter(Fragebogen_corr_RT, Rew == 0)
Fragebogen_corr_RT_rew1 <- filter(Fragebogen_corr_RT, Rew == 1)

##### Korrelationen nur FB nach Rew getrennt #####
require(psych)
M_FB_rew0 <- corr.test(Fragebogen_corr_RT_rew0[, -c(1,6,11:23)], adjust = 'none')
diag(M_FB_rew0$r) = NA
diag(M_FB_rew0$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_FB_rew0$r, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1,
         # order = "hclust",
         addrect = 3,
         type = 'upper',
         addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)


require(psych)
M_FB_rew1 <- corr.test(Fragebogen_corr_RT_rew1[, -c(1,6,11:23)], adjust = 'none')
diag(M_FB_rew1$r) = NA
diag(M_FB_rew1$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_FB_rew1$r, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1,
         # order = "hclust",
         addrect = 3,
         type = 'upper',
         addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

##### Korrelationen für AX und AY getrennt für Rew #####

require(psych)
M_RT_A_rew0 <- corr.test(Fragebogen_corr_RT_rew0[, -c(1,11, 15:17, 21:23)], adjust = 'fdr')
diag(M_RT_A_rew0$r) = NA
diag(M_RT_A_rew0$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT_A_rew0$r, p.mat = M_RT_A_rew0$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

require(psych)
M_RT_A_rew1 <- corr.test(Fragebogen_corr_RT_rew1[, -c(1,11, 15:17, 21:23)], adjust = 'fdr')
diag(M_RT_A_rew1$r) = NA
diag(M_RT_A_rew1$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT_A_rew1$r, p.mat = M_RT_A_rew1$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

##### Korrelationen für BX und BY getrennt für Rew #####

require(psych)
M_RT_B_rew0 <- corr.test(Fragebogen_corr_RT_rew0[, -c(1,11:14,18:20)], adjust = 'fdr')
diag(M_RT_B_rew0$r) = NA
diag(M_RT_B_rew0$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT_B_rew0$r, p.mat = M_RT_B_rew0$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

require(psych)
M_RT_B_rew1 <- corr.test(Fragebogen_corr_RT_rew1[, -c(1,11:14,18:20)], adjust = 'fdr')
diag(M_RT_B_rew1$r) = NA
diag(M_RT_B_rew1$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT_B_rew1$r, p.mat = M_RT_B_rew1$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1, 
         type = 'upper',
         #addCoef.col = "black", 
         #sig.level = 0.05, 
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5, 
         pch.col = "white", 
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

##### RT all getrennt nach Rew #####

require(psych)
M_RT_all_rew0 <- corr.test(Fragebogen_corr_RT_rew0[, -c(1,11)], adjust = 'fdr')
diag(M_RT_all_rew0$r) = NA
diag(M_RT_all_rew0$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT_all_rew0$r, p.mat = M_RT_all_rew0$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1,
         # order = "hclust",
         addrect = 3,
         type = 'upper',
         # addCoef.col = "black",
         # sig.level = 0.05,
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5,
         pch.col = "white",
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

require(psych)
M_RT_all_rew1 <- corr.test(Fragebogen_corr_RT_rew1[, -c(1,11)], adjust = 'fdr')
diag(M_RT_all_rew1$r) = NA
diag(M_RT_all_rew1$p) = NA

require(corrplot)
require(RColorBrewer)
corrplot(M_RT_all_rew1$r, p.mat = M_RT_all_rew1$p, 
         col = rev(colorRampPalette(brewer.pal(11,'RdBu'))(20)),
         method = "number", 
         number.cex = 1,
         # order = "hclust",
         addrect = 3,
         type = 'upper',
         # addCoef.col = "black",
         # sig.level = 0.05,
         tl.col = "black", 
         # tl.srt = 90,
         mar = c(1, 0, 2, 1),
         addgrid.col =  NA,
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = 1.5,
         pch.col = "white",
         na.label.col = NA, 
         na.label = NA, 
         cl.ratio = .25, cl.length = 11, cl.cex = 1)

