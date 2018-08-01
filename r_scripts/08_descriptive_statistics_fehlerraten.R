##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 July 2018 
#                       Compute Descriptive Statistics
#                             FEHLERRATEN PILOTS
#
# --------- BASELINE -----------
# Load necessary packages
require(plyr)
require(dplyr)

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
            '~/Desktop/Bachelor/Daten/R_Frames/Hits_Pilot1.txt',
            row.names = F,
            sep = '\t')

write.table(FAs1,
            '~/Desktop/Bachelor/Daten/R_Frames/FAs_Pilot1.txt',
            row.names = F,
            sep = '\t')

write.table(Miss1,
            '~/Desktop/Bachelor/Daten/R_Frames/Miss_Pilot1.txt',
            row.names = F,
            sep = '\t')

write.table(Hits2,
            '~/Desktop/Bachelor/Daten/R_Frames/Hits_Pilot2.txt',
            row.names = F,
            sep = '\t')

write.table(FAs2,
            '~/Desktop/Bachelor/Daten/R_Frames/FAs_Pilot2.txt',
            row.names = F,
            sep = '\t')

write.table(Miss2,
            '~/Desktop/Bachelor/Daten/R_Frames/Miss_Pilot2.txt',
            row.names = F,
            sep = '\t')


# COMPUTE DESCRIPTIVE STATISTICS FOR BASELINE----

# Trialzahlen 
total_trials1 <- Pilot1_Zuordnung%>%group_by(ID, Trialtype, Rew, Phase)%>% summarise(total=sum(!is.na(Trialnr)))
total_trials2 <- Pilot2_Zuordnung%>%group_by(ID, Trialtype, Rew, Phase)%>% summarise(total=sum(!is.na(Trialnr)))

# Incorrects summary: mean und n
FAs_sum1 <- FAs1 %>% group_by(ID, Trialtype, Rew, Phase) %>% summarise(m_RT = mean(RT), n = sum(!is.na(RT)))
FAs_sum2 <- FAs2 %>% group_by(ID, Trialtype, Rew, Phase) %>% summarise(m_RT = mean(RT), n = sum(!is.na(RT)))
# FAs_sum mit Daten mit gezählten Trials zusammen
FAs_sum1 <- merge(FAs_sum1, total_trials1, c("ID", "Trialtype", "Rew", "Phase"))
FAs_sum2 <- merge(FAs_sum2, total_trials2, c("ID", "Trialtype", "Rew", "Phase"))
# FEHLERRATE berechnen
FAs_sum1 <- mutate(FAs_sum1, Fehlerrate = n/total)
FAs_sum2 <- mutate(FAs_sum2, Fehlerrate = n/total)
# Mittlere Fehlerrate und SE
FAs_sum1_grouped <- FAs_sum1 %>% group_by(Trialtype) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))
FAs_sum2_grouped <- FAs_sum2 %>% group_by(Trialtype) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))
# FEHLERRATE Plotten
require(ggplot2)

ggplot(FAs_sum1_grouped, aes(x=Trialtype, y=m_FR)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3)

ggplot(FAs_sum2_grouped, aes(x=Trialtype, y=m_FR)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3)


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

Pilot1b_Zuordnung <- mutate(All_Pilot1b_selected, Rew = 0, perm = 0)
Pilot2b_Zuordnung <- mutate(All_Pilot2b_selected, Rew = 0, perm = 0)

## Blöcke nach Rew - kein Rew kodieren
# Perm = 0 --> Block 1,3,5,7,9 nR // Block 2,4,6,8,10 R
Pilot1b_Zuordnung <- dplyr::mutate(Pilot1b_Zuordnung, Phase= ifelse(Block%in%c(1,3,5,7,9),"noReward","Reward"))
Pilot2b_Zuordnung <- dplyr::mutate(Pilot2b_Zuordnung, Phase= ifelse(Block%in%c(1,3,5,7,9),"noReward","Reward"))


# Split dataset in corrects, incorrect and misses
Hits1b <- filter(Pilot1b_Zuordnung, Reactiontype == 'Correct')
FAs1b <- filter(Pilot1b_Zuordnung, Reactiontype == 'Incorrect')
Miss1b <- filter(Pilot1b_Zuordnung, Reactiontype == 'Miss')

Hits2b <- filter(Pilot2b_Zuordnung, Reactiontype == 'Correct')
FAs2b <- filter(Pilot2b_Zuordnung, Reactiontype == 'Incorrect')
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

# Trialzahlen 
total_trials1b <- Pilot1b_Zuordnung%>%group_by(ID, Trialtype, Rew, Phase)%>% summarise(total=sum(!is.na(Trialnr)))
total_trials2b <- Pilot2b_Zuordnung%>%group_by(ID, Trialtype, Rew, Phase)%>% summarise(total=sum(!is.na(Trialnr)))

# Incorrects summary: mean und n
FAs_sum1b<- FAs1b %>% group_by(ID, Trialtype, Rew, Phase) %>% summarise(m_RT = mean(RT), n = sum(!is.na(RT)))
FAs_sum2b<- FAs2b %>% group_by(ID, Trialtype, Rew, Phase) %>% summarise(m_RT = mean(RT), n = sum(!is.na(RT)))
# FAs_B_sum mit Daten mit gezählten Trials zusammen
FAs_sum1b <- merge(FAs_sum1b, total_trials1b, c("ID", "Trialtype", "Rew", "Phase"))
FAs_sum2b <- merge(FAs_sum2b, total_trials2b, c("ID", "Trialtype", "Rew", "Phase"))
# FEHLERRATE berechnen
FAs_sum1b <- mutate(FAs_sum1b, Fehlerrate = n/total)
FAs_sum2b <- mutate(FAs_sum2b, Fehlerrate = n/total)
# Mittlere Fehlerrate und SE
FAs_sum1b_grouped <- FAs_sum1b %>% group_by(Trialtype) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))
FAs_sum2b_grouped <- FAs_sum2b %>% group_by(Trialtype) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))
# FEHLERRATE Plotten
require(ggplot2)

ggplot(FAs_sum1b_grouped, aes(x=Trialtype, y=m_FR)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3)

ggplot(FAs_sum2b_grouped, aes(x=Trialtype, y=m_FR)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), colour="black", width=.1, position=position_dodge(.5)) +
  geom_line(position=position_dodge(.5)) +
  geom_point(position=position_dodge(.5), size=3)


# ------- BASELINE UND BLOCKS ZUSAMMEN -------

## Für Pilot 1
# Daten für Plot: Phase drin um es zu gruppieren, Fehlerraten m_FR und se_FR berechnen 
FAs_sum1_plot <- FAs_sum1 %>% group_by(Trialtype, Rew, Phase) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))

FAs_sum1b_plot <- FAs_sum1b %>% group_by(Trialtype, Rew, Phase) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))


FAs_sum_plot1All <- rbind(FAs_sum1_plot,FAs_sum1b_plot)

FAs_sum_plot1All$Rew[FAs_sum_plot1All$Rew==0] <- "Verzögerte Belohnung"
FAs_sum_plot1All$Rew[FAs_sum_plot1All$Rew==1] <- "Direkte Belohnung"      #### umbenannt, damit es im Plot erklärend darüber steht statt nur Zahlen

require(ggplot2)
ggplot(FAs_sum_plot1All, aes(x=Trialtype, y=m_FR, color = Phase, group = Phase)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), 
                colour="black", 
                width=.25,      # Breite der Querstriche am Ende der Fehlerbalken
                position=position_dodge(.25),
                size = .6) +
  geom_line(position=position_dodge(.25), size = 1) +
  geom_point(position=position_dodge(.25), size = 3) + 
  facet_wrap(~ Rew, ncol = 2, scales = 'free_y') +      # free_y --> y-Achse an beide teile einzeln
  coord_cartesian(ylim = c(0, .5)) +         # Range der Y-Achse
  theme_classic() +     # weißer Hintergrund etc., gibt viele einfach ausprobieren, welches man möchte
  labs(x = 'Trialtype', y = 'Mittlere Fehlerrate', title = "Mittlere Fehlerrate in Pilotierung 1") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

# Save for later use
write.table(FAs_sum_plot1All, '~/Desktop/Bachelor/Daten/R_Frames/FAs_sum_plot1All.txt', row.names = F, sep = '\t')

## Für Pilot 2
# Daten für Plot: Phase drin um es zu gruppieren, Fehlerraten m_FR und se_FR berechnen 
FAs_sum2_plot <- FAs_sum2 %>% group_by(Trialtype, Rew, Phase) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))

FAs_sum2b_plot <- FAs_sum2b %>% group_by(Trialtype, Rew, Phase) %>%
  summarise(m_FR=mean(Fehlerrate),
            se_FR=sd(Fehlerrate)/sqrt(sum(!is.na(Fehlerrate))))


FAs_sum_plot2All <- rbind(FAs_sum2_plot,FAs_sum2b_plot)

FAs_sum_plot2All$Rew[FAs_sum_plot2All$Rew==0] <- "Verzögerte Belohnung"
FAs_sum_plot2All$Rew[FAs_sum_plot2All$Rew==1] <- "Direkte Belohnung"      #### umbenannt, damit es im Plot erklärend darüber steht statt nur Zahlen

require(ggplot2)
ggplot(FAs_sum_plot2All, aes(x=Trialtype, y=m_FR, color = Phase, group = Phase)) + 
  geom_errorbar(aes(ymin=m_FR-se_FR, ymax=m_FR+se_FR), 
                colour="black", 
                width=.25,      # Breite der Querstriche am Ende der Fehlerbalken
                position=position_dodge(.25),
                size = .6) +
  geom_line(position=position_dodge(.25), size = 1) +
  geom_point(position=position_dodge(.25), size = 3) + 
  facet_wrap(~ Rew, ncol = 2, scales = 'free_y') +      # free_y --> y-Achse an beide teile einzeln
  coord_cartesian(ylim = c(0, .5)) +         # Range der Y-Achse
  theme_classic() +     # weißer Hintergrund etc., gibt viele einfach ausprobieren, welches man möchte
  labs(x = 'Trialtype', y = 'Mittlere Fehlerrate', title = "Mittlere Fehlerrate in Pilotierung 2") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank())
## komische blaue Reward Linie, weil bei Trialtype 2 & 4 nur 1 Vp je 1 Fehler gemacht hat

# Save for later use
write.table(FAs_sum_plot2All, '~/Desktop/Bachelor/Daten/R_Frames/FAs_sum_plot2All.txt', row.names = F, sep = '\t')

## Für beide Pilotierungen zusammen
# nicht so sinnvoll einfach beide einzelnen Abbildungen nebeneinander packen

