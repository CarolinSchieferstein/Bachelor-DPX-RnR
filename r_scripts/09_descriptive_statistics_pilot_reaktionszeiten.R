##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 August 2018
#                       Compute Descriptive Statistics
#                                   Pilot
#                              Reaktionszeiten
#
# --------- BASELINE -----------
# Load necessary packages
require(plyr)
require(dplyr)

# -------- Read in und Aufteilung/Ordnen bei 04 für Fehlerraten schon gemacht -> schon im Environment drin --------
# Get the data
All_Pilot1_selected <- read.table('~/Desktop/Bachelor/Daten/R_Frames/All_Pilot1.txt',
                              header = T)
All_Pilot2_selected <- read.table('~/Desktop/Bachelor/Daten/R_Frames/All_Pilot2.txt',
                                   header = T)

# VP_Zuordnung einfügen
Pilot1_Zuordnung <- mutate(All_Pilot1_selected, Phase = "Baseline", Rew = 0, perm = 0)
Pilot2_Zuordnung <- mutate(All_Pilot2_selected, Phase = "Baseline", Rew = 0, perm = 0)

# Split dataset in corrects, incorrect and misses
Hits1 <- filter(Pilot1_Zuordnung, Reactiontype == 'Correct')
FAs1 <- filter(Pilot1_Zuordnung, Reactiontype == 'Incorrect')
Miss1 <- filter(Pilot1_Zuordnung, Reactiontype == 'Miss')

Hits2 <- filter(Pilot2_Zuordnung, Reactiontype == 'Correct')
FAs2 <- filter(Pilot2_Zuordnung, Reactiontype == 'Incorrect')
Miss2 <- filter(Pilot2_Zuordnung, Reactiontype == 'Miss')

# Save datasets for later use
write.table(Hits1,
            '~/Desktop/Bachelor/Daten/R_Frames/Hits1.txt',
            row.names = F,
            sep = '\t')

write.table(FAs1,
            '~/Desktop/Bachelor/Daten/R_Frames/FAs1.txt',
            row.names = F,
            sep = '\t')

write.table(Miss1,
            '~/Desktop/Bachelor/Daten/R_Frames/Miss1.txt',
            row.names = F,
            sep = '\t')

write.table(Hits2,
            '~/Desktop/Bachelor/Daten/R_Frames/Hits2.txt',
            row.names = F,
            sep = '\t')

write.table(FAs2,
            '~/Desktop/Bachelor/Daten/R_Frames/FAs2.txt',
            row.names = F,
            sep = '\t')

write.table(Miss2,
            '~/Desktop/Bachelor/Daten/R_Frames/Miss2.txt',
            row.names = F,
            sep = '\t')


# COMPUTE DESCRIPTIVE STATISTICS FOR BASELINE----

# Hits rauslesen, group_by Trialtype , Rew und perm aufteilen, nicht nach Reactiontype, weil nur Hits drin! + summarise
Hits_sum1 <- Hits1 %>% dplyr::group_by(Trialtype, Rew, Phase) %>%
  dplyr::summarise(m_RT = mean(RT),
            sd_RT = sd(RT),
            se_RT = sd(RT)/sqrt(sum(!is.na(RT))),
            n = sum(!is.na(RT)))
Hits_sum2 <- Hits2 %>% dplyr::group_by(Trialtype, Rew, Phase) %>%
  dplyr::summarise(m_RT = mean(RT),
            sd_RT = sd(RT),
            se_RT = sd(RT)/sqrt(sum(!is.na(RT))),
            n = sum(!is.na(RT)))

require(ggplot2)
ggplot(Hits_sum1, aes(x=Trialtype, y=m_RT)) + 
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3) +
  facet_wrap(~Rew)

ggplot(Hits_sum2, aes(x=Trialtype, y=m_RT)) + 
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3) +
  facet_wrap(~Rew)

# ---------------- BLOCKS ----------------
# Load necessary packages
require(plyr)
require(dplyr)

# Get the data
All_Pilot1b_selected <- read.table('~/Desktop/Bachelor/Daten/R_Frames/All_Pilot1b.txt',
                                  header = T)
All_Pilot2b_selected <- read.table('~/Desktop/Bachelor/Daten/R_Frames/All_Pilot2b.txt',
                                  header = T)
# VP_Zuordnung einlesen

Pilot1b_Zuordnung <- mutate(All_Pilot1_selected, Rew = 0, perm = 0)
Pilot2b_Zuordnung <- mutate(All_Pilot2_selected, Rew = 0, perm = 0)

## Blöcke nach Rew - kein Rew kodieren
# Perm = 0 --> Block 1,3,5,7,9 nR // Block 2,4,6,8,10 R
Pilot1b_Zuordnung <- dplyr::mutate(Pilot1b_Zuordnung, Phase= ifelse(Block%in%c(1,3,5,7,9),"noReward","Reward"))
Pilot2b_Zuordnung <- dplyr::mutate(Pilot2b_Zuordnung, Phase= ifelse(Block%in%c(1,3,5,7,9),"noReward","Reward"))


# Split dataset in corrects, incorrect and misses
Hits1b <- filter(Pilot1b_Zuordnung, Reactiontype == 'Correct')
FAs1b<- filter(Pilot1b_Zuordnung, Reactiontype == 'Incorrect')
Miss1b <- filter(Pilot1b_Zuordnung, Reactiontype == 'Miss')

Hits2b <- filter(Pilot2b_Zuordnung, Reactiontype == 'Correct')
FAs2b<- filter(Pilot2b_Zuordnung, Reactiontype == 'Incorrect')
Miss2b <- filter(Pilot2b_Zuordnung, Reactiontype == 'Miss')

#Save datasets for later use
write.table(Hits1b,
            '~/Desktop/Bachelor/Daten/R_Frames/Hits1b.txt',
            row.names = F,
            sep = '\t')

write.table(FAs1b,
            '~/Desktop/Bachelor/Daten/R_Frames/FAs1b.txt',
            row.names = F,
            sep = '\t')

write.table(Miss1b,
            '~/Desktop/Bachelor/Daten/R_Frames/Miss1b.txt',
            row.names = F,
            sep = '\t')

write.table(Hits2b,
            '~/Desktop/Bachelor/Daten/R_Frames/Hits2b.txt',
            row.names = F,
            sep = '\t')

write.table(FAs2b,
            '~/Desktop/Bachelor/Daten/R_Frames/FAs2b.txt',
            row.names = F,
            sep = '\t')

write.table(Miss2b,
            '~/Desktop/Bachelor/Daten/R_Frames/Miss2b.txt',
            row.names = F,
            sep = '\t')

# COMPUTE DESCRIPTIVE STATISTICS FOR BLOCKS----

# Hits rauslesen, group_by Trialtype , Rew und perm aufteilen, nicht nach Reactiontype, weil nur Hits drin! + summarise
Hits1b_sum <- Hits1b %>% dplyr::group_by(Trialtype, Rew, Phase) %>%
  dplyr::summarise(m_RT = mean(RT),
            sd_RT = sd(RT),
            se_RT = sd(RT)/sqrt(sum(!is.na(RT))),
            n = sum(!is.na(RT)))

Hits2b_sum <- Hits2b %>% dplyr::group_by(Trialtype, Rew, Phase) %>%
  dplyr::summarise(m_RT = mean(RT),
            sd_RT = sd(RT),
            se_RT = sd(RT)/sqrt(sum(!is.na(RT))),
            n = sum(!is.na(RT)))

require(ggplot2)
ggplot(Hits1b_sum, aes(x=Trialtype, y=m_RT, color = Phase, group = Phase)) + 
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3) +
  facet_wrap(~Rew)

ggplot(Hits2b_sum, aes(x=Trialtype, y=m_RT, color = Phase, group = Phase)) + 
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3) +
  facet_wrap(~Rew)

# ---------------- BEIDES ZUSAMMEN ----------------
# Load necessary packages
require(plyr)
require(dplyr)

Hits1_sum_plot <- Hits_sum1 %>% dplyr::group_by(Trialtype, Rew, Phase)
Hits1b_sum_plot <- Hits1b_sum %>% dplyr::group_by(Trialtype, Rew, Phase)

Hits1_sum_plot_All <- rbind(Hits1_sum_plot, Hits1b_sum_plot)

Hits1_sum_plot_All$Rew[Hits1_sum_plot_All$Rew==0] <- "Verzögerte Belohnung"
Hits1_sum_plot_All$Rew[Hits1_sum_plot_All$Rew==1] <- "Direkte Belohnung"      #### umbenannt, damit es im Plot erklärend darüber steht statt nur Zahlen

Hits2_sum_plot <- Hits_sum2 %>% dplyr::group_by(Trialtype, Rew, Phase)
Hits2b_sum_plot <- Hits2b_sum %>% dplyr::group_by(Trialtype, Rew, Phase)

Hits2_sum_plot_All <- rbind(Hits2_sum_plot, Hits2b_sum_plot)

Hits2_sum_plot_All$Rew[Hits2_sum_plot_All$Rew==0] <- "Verzögerte Belohnung"
Hits2_sum_plot_All$Rew[Hits2_sum_plot_All$Rew==1] <- "Direkte Belohnung"      #### umbenannt, damit es im Plot erklärend darüber steht statt nur Zahlen

require(ggplot2)
ggplot(Hits1_sum_plot_All, aes(x=Trialtype, y=m_RT, color = Phase, group = Phase)) +
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
  labs(x = 'Trialtype', y = 'Mittlere Reaktionszeit', title = "Mittlere Reaktionszeit in Pilot 1") +
  theme(strip.background = element_blank(),
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

ggplot(Hits2_sum_plot_All, aes(x=Trialtype, y=m_RT, color = Phase, group = Phase)) +
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
  labs(x = 'Trialtype', y = 'Mittlere Reaktionszeit', title = "Mittlere Reaktionszeit in Pilot 2") +
  theme(strip.background = element_blank(),
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

# Save for later use
write.table(Hits1_sum_plot_All, '~/Desktop/Bachelor/Daten/R_Frames/Hits1_sum_plot_All.txt', row.names = F, sep = '\t')
write.table(Hits2_sum_plot_All, '~/Desktop/Bachelor/Daten/R_Frames/Hits2_sum_plot_All.txt', row.names = F, sep = '\t')

