##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                        ANOVA FEHLERRATEN MAIN WITHIN

# Load nescessary packages
require(plyr)
require(dplyr)

require(car) # https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/

# https://cran.r-project.org/web/packages/emmeans/index.html

# ### die Daten müssen für die gemischte ANOVA umgebaut werden, muss angeben können für welche Variable der withinFaktor sich wiederholt --> brauche ID
# Baseline_RT_Zuordnung$Trialtype <- as.factor(Baseline_RT_Zuordnung$Trialtype)
# Baseline_RT_Zuordnung$BlockR <- as.factor(Baseline_RT_Zuordnung$BlockR)
# Baseline_RT_Zuordnung$Rew <- as.factor(Baseline_RT_Zuordnung$Rew)
# 
# # Vorrechnung für Baseline
# model1 <- lm(data=Baseline_RT_Zuordnung, m~Trialtype*Rew*Reactiontype)
# ANOVA_Baseline <- anova(model1)
# ANOVA_Baseline
# 
# # Vorrechnung für Blocks
# model2 <- lm(data=Block_RnR, m_RT~Trialtype*BlockR*Rew)   # m_FR~Trialtype*Phase*Rew
# ANOVA_Blocks <- anova(model2)
# ANOVA_Blocks

## as.factor umbauen
# Hits_sum_plot_All$Rew[Hits_sum_plot_All$Rew==0] <- "Verzögerte Belohnung"
# Hits_sum_plot_All$Rew[Hits_sum_plot_All$Rew==1] <- "Direkte Belohnung"      #### umbenannt, damit es im Plot erklärend darüber steht statt nur Zahlen

# Effekt oder Dummy Kodierung in FAs_sum2b z.B.     # https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-effect-coding/
FAs_sum2b$Trialtype <- as.factor(FAs_sum2b$Trialtype) 
contrasts(FAs_sum2b$Trialtype) <- contr.sum(4)        # sum macht dass es Effektcoding ist!!!!!
contrasts(FAs_sum2b$Trialtype) <- contr.treatment(4, base = 1)  # treatment macht dummycoding , base spezifiziert referenzlevel

##### ANOVA: Daten für jede Person einzeln gemittelt, nach Reactiontypes aufgeteilt (Rew, BlockR, m/ m_RT)   !!!!    ##########
##Datensatz:
# Baseline: Baseline-RT_Zuordnung nach Reactionstypes teilen
Baselinehits4anova<- filter(Baseline_RT_Zuordnung, Reactiontype== " hit")
Baselineincorrect4anova <-filter(Baseline_RT_Zuordnung, Reactiontype== " incorrect")

# Blocks:
Block_RT_Zuordnung4anova <- rbind(Block_perm0,Block_perm1)

Block_RT_Zuordnung4anova1 <- Block_RT_Zuordnung4anova%>%group_by(ID,Rew,BlockR,Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(m),
            sd_RT=sd(m),
            se_RT=sd(m)/sqrt(sum(!is.na(m))),
            n=sum(!is.na(m)))

# Aufteilung nach Reactiontype
Blockhits4anova <- filter(Block_RT_Zuordnung4anova1, Reactiontype== " hit")
Blockincorrect4anova <- filter(Block_RT_Zuordnung4anova1, Reactiontype== " incorrect")

############# ANOVA Model
model1.hit <- lm(data=Baselinehits4anova, m~Trialtype*Rew)
ANOVA_Baselinehits <- anova(model1.hit)
ANOVA_Baselinehits

model1.incorrect <- lm(data=Baselineincorrect4anova, m~Trialtype*Rew)
ANOVA_Baselineincorrect <- anova(model1.incorrect)
ANOVA_Baselineincorrect

####

model2.hit <- lm(data=Blockhits4anova, m_RT~Trialtype*BlockR*Rew)
ANOVA_Blockhit <- anova(model2.hit)
ANOVA_Blockhit

model2.incorrect <- lm(data=Blockincorrect4anova, m_RT~Trialtype*BlockR*Rew)
ANOVA_Blocksincorrect <- anova(model2.incorrect)
ANOVA_Blocksincorrect
