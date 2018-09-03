##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 August 2018
#                       Compute Descriptive Statistics
#                                   Main
#                              Reaktionszeiten
#
# --------- BASELINE -----------
# Load necessary packages
require(plyr)
require(dplyr)

# -------- Read in und Aufteilung/Ordnen bei 04 für Fehlerraten schon gemacht -> schon im Environment drin --------
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

# Hits rauslesen, group_by Trialtype , Rew und perm aufteilen, nicht nach Reactiontype, weil nur Hits drin! + summarise
Hits_sum <- Hits %>% group_by(Trialtype, Rew, Phase) %>%
                     summarise(m_RT = mean(RT),
                               sd_RT = sd(RT),
                               se_RT = sd(RT)/sqrt(sum(!is.na(RT))),
                               n = sum(!is.na(RT)))

require(ggplot2)
ggplot(Hits_sum, aes(x=Trialtype, y=m_RT)) + 
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3) +
  facet_wrap(~Rew)

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
All_RTB_Prezuordnung <- merge(VP_Zuordnung, All_RTB_selected, by = "ID")
All_RTB_perm0 <- dplyr::filter(RTB_Prezuordnung, Perm==0)
All_RTB_perm1 <- dplyr::filter(RTB_Prezuordnung, Perm==1)

## Blöcke nach Rew - kein Rew kodieren
# Perm = 0 --> Block 1,3,5,7,9 nR // Block 2,4,6,8,10 R
All_RTB_perm0 <- dplyr::mutate(All_RTB_perm0, Phase= ifelse(Block%in%c(1,3,5,7,9),"noReward","Reward"))
# Perm1 --> Block 1,3,5,7,9 R // BLock 2,4,6,8,10 nR
All_RTB_perm1 <- dplyr::mutate(All_RTB_perm1, Phase= ifelse(Block%in%c(1,3,5,7,9),"Reward","noReward"))

All_RTB_Zuordnung <- rbind(All_RTB_perm0,All_RTB_perm1)

# Split dataset in corrects, incorrect and misses
Hits_B <- filter(All_RTB_Zuordnung, Reactiontype == 'Correct')
FAs_B <- filter(All_RTB_Zuordnung, Reactiontype == 'Incorrect')
Miss_B <- filter(All_RTB_Zuordnung, Reactiontype == 'Miss')

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

# Hits rauslesen, group_by Trialtype , Rew und perm aufteilen, nicht nach Reactiontype, weil nur Hits drin! + summarise
Hits_sumB <- Hits_B %>% group_by(Trialtype, Rew, Phase) %>%
  summarise(m_RT = mean(RT),
            sd_RT = sd(RT),
            se_RT = sd(RT)/sqrt(sum(!is.na(RT))),
            n = sum(!is.na(RT)))

require(ggplot2)
ggplot(Hits_sumB, aes(x=Trialtype, y=m_RT, color = Phase, group = Phase)) + 
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3) +
  facet_wrap(~Rew)

# ---------------- BEIDES ZUSAMMEN ----------------
# Load necessary packages
require(plyr)
require(dplyr)

Hits_sum_plot <- Hits_sum %>% group_by(Trialtype, Rew, Phase)
Hits_sumB_plot <- Hits_sumB %>% group_by(Trialtype, Rew, Phase)

Hits_sum_plot_All <- rbind(Hits_sum_plot, Hits_sumB_plot)

Hits_sum_plot_All$Rew[Hits_sum_plot_All$Rew==0] <- "Verzögerte Belohnung"
Hits_sum_plot_All$Rew[Hits_sum_plot_All$Rew==1] <- "Direkte Belohnung"      #### umbenannt, damit es im Plot erklärend darüber steht statt nur Zahlen


require(ggplot2)
ggplot(Hits_sum_plot_All, aes(x=Trialtype, y=m_RT, color = Phase, group = Phase)) +
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT),
                colour="black",
                width=.25,      # Breite der Querstriche am Ende der Fehlerbalken
                position=position_dodge(.25),
                size = .6) +
  geom_line(position=position_dodge(.25), size = 1) +
  geom_point(position=position_dodge(.25), size = 3) +
  facet_wrap(~ Rew, ncol = 2, scales = 'free_y') +      # free_y --> y-Achse an beide teile einzeln
  coord_cartesian(ylim = c(200, 500)) +         # Range der Y-Achse
  theme_classic() +     # weißer Hintergrund etc., gibt viele einfach ausprobieren, welches man möchte
  labs(x = 'Trialtype', y = 'Mittlere Reaktionszeit', title = "Mittlere Reaktionszeit in der Haupttestung") +
  theme(strip.background = element_blank(),
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

# Save for later use
write.table(Hits_sum_plot_All, '~/Desktop/Bachelor/Daten/R_Frames/Hits_sum_plot_All.txt', row.names = F, sep = '\t')

