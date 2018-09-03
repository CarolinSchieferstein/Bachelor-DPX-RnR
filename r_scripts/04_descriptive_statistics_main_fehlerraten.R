##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 July 2018 
#                       Compute Descriptive Statistics
#                                   Main
#                                Fehlerraten
#
# --------- BASELINE -----------
# Load necessary packages
require(plyr)
require(dplyr)

# Get the data
All_RT_selected <- read.table('~/Desktop/Bachelor/Daten/R_Frames/All_RT.txt', 
                              header = T)

# VP_Zuordnung einlesen

VP_Zuordnung <- read.table("Desktop/VP_Zuordnung.txt", header=TRUE)

## Zuordnung nach ID mit Daten mergen, dann nach perm aufteilen

colnames(VP_Zuordnung)[colnames(VP_Zuordnung)=="VP"] <- "ID"
RT_Zuordnung <- merge(VP_Zuordnung, All_RT_selected, by = "ID")

RT_Zuordnung$Phase <- "Baseline"

# Split dataset in corrects, incorrect and misses
Hits <- filter(RT_Zuordnung, Reactiontype == 'Correct')
FAs <- filter(RT_Zuordnung, Reactiontype == 'Incorrect')
Miss <- filter(RT_Zuordnung, Reactiontype == 'Miss')

# Save datasets for later use
write.table(Hits,
            '~/Desktop/Bachelor/Daten/R_Frames/Hits_RT.txt',
            row.names = F,
            sep = '\t')

write.table(FAs,
            '~/Desktop/Bachelor/Daten/R_Frames/FAs_RT.txt',
            row.names = F,
            sep = '\t')

write.table(Miss,
            '~/Desktop/Bachelor/Daten/R_Frames/Miss_RT.txt',
            row.names = F,
            sep = '\t')


# COMPUTE DESCRIPTIVE STATISTICS FOR BASELINE----

# Trialzahlen 
total_trials <- RT_Zuordnung %>% dplyr::group_by(ID, Trialtype, Rew, Phase) %>% dplyr::summarise(total=sum(!is.na(Trialnr)))

# Incorrects summary: mean und n
FAs_sum<- FAs %>% dplyr::group_by(ID, Trialtype, Rew, Phase) %>% dplyr::summarise(m_RT = mean(RT), n = sum(!is.na(RT)))
# FAs_sum mit Daten mit gezählten Trials zusammen
FAs_sum <- merge(FAs_sum, total_trials, c("ID", "Trialtype", "Rew", "Phase"))
# FEHLERRATE berechnen
FAs_sum <- mutate(FAs_sum, Fehlerrate = n/total)
# Mittlere Fehlerrate und SE
FAs_sum_grouped <- FAs_sum %>% dplyr::group_by(Trialtype) %>%
                        dplyr::summarise(m_FR=mean(Fehlerrate),
                                  se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))
# FEHLERRATE Plotten
require(ggplot2)
ggplot(FAs_sum_grouped, aes(x=Trialtype, y=m_FR)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3)


# ---------------- BLOCKS ----------------
# Load necessary packages
require(plyr)
require(dplyr)

# Get the data
All_RTB_selected <- read.table('~/Desktop/Bachelor/Daten/R_Frames/All_RTB.txt', 
                              header = T)
# VP_Zuordnung einlesen

VP_Zuordnung <- read.table("Desktop/VP_Zuordnung.txt", header=TRUE)

## Zuordnung nach ID mit Daten mergen, dann nach perm aufteilen

colnames(VP_Zuordnung)[colnames(VP_Zuordnung)=="VP"] <- "ID"
RTB_Prezuordnung <- merge(VP_Zuordnung, All_RTB_selected, by = "ID")
RTB_perm0 <- dplyr::filter(RTB_Prezuordnung, Perm==0)
RTB_perm1 <- dplyr::filter(RTB_Prezuordnung, Perm==1)

## Blöcke nach Rew - kein Rew kodieren
# Perm = 0 --> Block 1,3,5,7,9 nR // Block 2,4,6,8,10 R
RTB_perm0 <- dplyr::mutate(RTB_perm0, Phase= ifelse(Block%in%c(1,3,5,7,9),"noReward","Reward"))
# Perm1 --> Block 1,3,5,7,9 R // BLock 2,4,6,8,10 nR
RTB_perm1 <- dplyr::mutate(RTB_perm1, Phase= ifelse(Block%in%c(1,3,5,7,9),"Reward","noReward"))

RTB_Zuordnung <- rbind(RTB_perm0,RTB_perm1)

# Split dataset in corrects, incorrect and misses
Hits_B <- filter(RTB_Zuordnung, Reactiontype == 'Correct')
FAs_B <- filter(RTB_Zuordnung, Reactiontype == 'Incorrect')
Miss_B <- filter(RTB_Zuordnung, Reactiontype == 'Miss')

#Save datasets for later use
write.table(Hits_B,
            '~/Desktop/Bachelor/Daten/R_Frames/Hits_RTB.txt',
            row.names = F,
            sep = '\t')

write.table(FAs_B,
            '~/Desktop/Bachelor/Daten/R_Frames/FAs_RTB.txt',
            row.names = F,
            sep = '\t')

write.table(Miss_B,
            '~/Desktop/Bachelor/Daten/R_Frames/Miss_RTB.txt',
            row.names = F,
            sep = '\t')


# COMPUTE DESCRIPTIVE STATISTICS FOR BLOCKS----

# Trialzahlen 
total_trialsB <- RTB_Zuordnung%>%group_by(ID, Trialtype, Rew, Phase)%>% summarise(total=sum(!is.na(Trialnr)))

# Incorrects summary: mean und n
FAs_B_sum<- FAs_B %>% group_by(ID, Trialtype, Rew, Phase) %>% summarise(m_RT = mean(RT), n = sum(!is.na(RT)))
# FAs_B_sum mit Daten mit gezählten Trials zusammen
FAs_B_sum <- merge(FAs_B_sum, total_trialsB, c("ID", "Trialtype", "Rew", "Phase"))
# FEHLERRATE berechnen
FAs_B_sum <- mutate(FAs_B_sum, Fehlerrate = n/total)
# Mittlere Fehlerrate und SE
FAs_B_sum_grouped <- FAs_B_sum %>% group_by(Trialtype) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))
# FEHLERRATE Plotten
require(ggplot2)
ggplot(FAs_B_sum_grouped, aes(x=Trialtype, y=m_FR)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3)




# ------- BASELINE UND BLOCKS ZUSAMMEN -------

# Daten für Plot: Phase drin um es zu gruppieren, Fehlerraten m_FR und se_FR berechnen 
FAs_sum_plot <- FAs_sum %>% group_by(Trialtype, Rew, Phase) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))

FAs_B_sum_plot <- FAs_B_sum %>% group_by(Trialtype, Rew, Phase) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))


FAs_sum_plot_All <- rbind(FAs_sum_plot,FAs_B_sum_plot)
FAs_sum_plot_All$Rew[FAs_sum_plot_All$Rew==0] <- "Verzögerte Belohnung"
FAs_sum_plot_All$Rew[FAs_sum_plot_All$Rew==1] <- "Direkte Belohnung"      #### umbenannt, damit es im Plot erklärend darüber steht statt nur Zahlen



require(ggplot2)
ggplot(FAs_sum_plot_All, aes(x=Trialtype, y=m_FR, color = Phase, group = Phase)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), 
                colour="black", 
                width=.25,      # Breite der Querstriche am Ende der Fehlerbalken
                position=position_dodge(.25),
                size = .6) +
  geom_line(position=position_dodge(.25), size = 1) +
  geom_point(position=position_dodge(.25), size = 3) + 
  facet_wrap(~ Rew, ncol = 2, scales = 'free_y') +      # free_y --> y-Achse an beide teile einzeln
  coord_cartesian(ylim = c(0, .35)) +         # Range der Y-Achse
  theme_classic() +     # weißer Hintergrund etc., gibt viele einfach ausprobieren, welches man möchte
  labs(x = 'Trialtype', y = 'Mittlere Fehlerrate', title = "Mittlere Fehlerrate in der Haupttestung") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

# Save for later use
write.table(FAs_sum_plot_All, '~/Desktop/Bachelor/Daten/R_Frames/FAs_sum_plot_All.txt', row.names = F, sep = '\t')
