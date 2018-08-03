##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 August 2018 
#                        ANOVA FEHLERRATEN MAIN WITHIN

# Load nescessary packages
require(plyr)
require(dplyr)

### die Daten müssen für die gemischte ANOVA umgebaut werden, muss angeben können für welche Variable der withinFaktor sich wiederholt --> brauche ID
Baseline_RT_Zuordnung$Trialtype <- as.factor(Baseline_RT_Zuordnung$Trialtype)
Baseline_RT_Zuordnung$BlockR <- as.factor(Baseline_RT_Zuordnung$BlockR)
Baseline_RT_Zuordnung$Rew <- as.factor(Baseline_RT_Zuordnung$Rew)

# Vorrechnung für Baseline
model1 <- lm(data=Baseline_RT_Zuordnung, m~Trialtype*Rew*Reactiontype)
ANOVA_Baseline <- anova(model1)
ANOVA_Baseline

# Vorrechnung für Blocks
model2 <- lm(data=Block_RnR, m_RT~Trialtype*BlockR*Rew)
ANOVA_Blocks <- anova(model2)
ANOVA_Blocks




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
