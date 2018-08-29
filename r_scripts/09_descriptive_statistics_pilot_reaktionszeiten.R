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

Hits1_sum_plot <- Hits1_sum %>% dplyr::group_by(Trialtype, Rew, Phase)
Hits1b_sum_plot <- Hits1b_sum %>% dplyr::group_by(Trialtype, Rew, Phase)

Hits1_sum_plot_All <- rbind(Hits1_sum_plot, Hits1b_sum_plot)

Hits1_sum_plot_All$Rew[Hits1_sum_plot_All$Rew==0] <- "Verzögerte Belohnung"
Hits1_sum_plot_All$Rew[Hits1_sum_plot_All$Rew==1] <- "Direkte Belohnung"      #### umbenannt, damit es im Plot erklärend darüber steht statt nur Zahlen

Hits2_sum_plot <- Hits2_sum %>% dplyr::group_by(Trialtype, Rew, Phase)
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

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################

########## ALTE VERSION DES CODES ZUR SICHERHEIT UM ZUM RAUSKOPIEREN VON CODE ##########
#### BASELINE #####
# ## mean und Zahl nach miss/hit/incorrect
# All_RT_selected %>% group_by(Reactiontype)%>%summarise(m = mean(RT), n = sum(!is.na(RT)))
# ## mean, sd, se und Zahl nach miss/hit/incorrect bei versch. Trialtypes
# All_RT_selected%>%group_by(Trialtype, Reactiontype)%>%
#   summarise(m_RT=mean(RT),
#             sd_RT=sd(RT),
#             se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
#             n=sum(!is.na(RT))
#   )
# ##s.o. nach VP getrennt
# Baseline_descriptive <- All_RT_selected%>%group_by(ID, Trialtype, Reactiontype)%>%
#   summarise(m_RT=mean(RT),
#             sd_RT=sd(RT),
#             se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
#             n=sum(!is.na(RT))
#   )
# 
# ## mean RT und mean n über Personen pro Trialtype
# 
# Baseline_descriptiveAcross <- Baseline_descriptive%>%group_by(Trialtype, Reactiontype)%>%
#   summarise(M_RT=mean(m_RT),
#             M_N=mean(n))
# 
# Baseline_descriptiveAcrossWsdse <- Baseline_descriptive%>%group_by(Trialtype, Reactiontype)%>%
#   summarise(M_RT=mean(m_RT),
#             SD_RT=sd(m_RT),
#             SE_RT=sd(m_RT)/sqrt(sum(!is.na(m_RT))),
#             M_N=mean(n))
# 
# 
# ## plot trialtype, reactiontype and mean RT
# 
# Baseline_plot <- interaction.plot(Baseline_descriptiveAcrossWsdse$Trialtype, Baseline_descriptiveAcrossWsdse$Reactiontype, Baseline_descriptiveAcrossWsdse$M_RT, 
#                                   type = "b", col = c(2:6), 
#                                   pch = c(18, 24), 
#                                   xlab = "Trialtype", 
#                                   ylab="mean RT")

######### BLOCKS ###############
#  
# require(plyr)
# require(dplyr)
# 
# ## mean und Zahl nach miss/hit/incorrect
# Block_RT_selected <- Block_RT_selected %>% group_by(ID,Block,Trialtype,Reactiontype)%>%summarise(m = mean(RT), n = sum(!is.na(RT)))
# ## mean, sd, se und Zahl nach miss/hit/incorrect bei versch. Trialtypes
# Block_RT_selected%>%group_by(Trialtype, Reactiontype)%>%
#   summarise(m_RT=mean(RT),
#             sd_RT=sd(RT),
#             se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
#             n=sum(!is.na(RT))
#   )
# ##s.o. nach VP getrennt
# Block_descriptive <- Block_RT_selected%>%group_by(ID, Trialtype, Reactiontype)%>%
#   summarise(m_RT=mean(RT),
#             sd_RT=sd(RT),
#             se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
#             n=sum(!is.na(RT))
#   )
# 
# ## mean RT und mean n über Personen pro Trialtype
# 
# Block_descriptiveAcross <- Block_descriptive%>%group_by(Trialtype, Reactiontype)%>%
#   summarise(M_RT=mean(m_RT),
#             M_N=mean(n))
# 
# Block_descriptiveAcrossWsdse <- Block_descriptive%>%group_by(Trialtype, Reactiontype)%>%
#   summarise(M_RT=mean(m_RT),
#             SD_RT=sd(m_RT),
#             SE_RT=sd(m_RT)/sqrt(sum(!is.na(m_RT))),
#             M_N=mean(n))
# 
# ## plot trialtype, reactiontype and mean RT
# 
# Block_plot <- interaction.plot(Block_descriptiveAcrossWsdse$Trialtype, Block_descriptiveAcrossWsdse$Reactiontype, Block_descriptiveAcrossWsdse$M_RT, 
#                                type = "b", col = c(2:6), 
#                                pch = c(18, 24), 
#                                xlab = "Trialtype", 
#                                ylab="mean RT")
# 
# 
# ## VP_Zuordnung einlesen
# 
# VP_Zuordnung <- read.table("Desktop/VP_Zuordnung.txt", header=TRUE)
# 
# ## Zuordnung nach ID mit Daten mergen, dann nach perm aufteilen
# 
# colnames(VP_Zuordnung)[colnames(VP_Zuordnung)=="VP"] <- "ID"
# Block_RT_Zuordnung <- merge(VP_Zuordnung, Block_RT_selected, by = "ID")
# Block_perm0 <- dplyr::filter(Block_RT_Zuordnung, Perm==0)
# Block_perm1 <- dplyr::filter(Block_RT_Zuordnung, Perm==1)
# 
# ## Blöcke nach Rew - kein Rew kodieren
# 
# # Perm = 0 --> Block 1,3,5,7,9 nR // Block 2,4,6,8,10 R
# Block_perm0 <- dplyr::mutate(Block_perm0, BlockR= ifelse(Block%in%c(1,3,5,7,9),0,1))
# # Perm1 --> Block 1,3,5,7,9 R // BLock 2,4,6,8,10 nR
# Block_perm1 <- dplyr::mutate(Block_perm1, BlockR= ifelse(Block%in%c(1,3,5,7,9),1,0))
# 
# ## wieder zusammenpacken
# 
# Block_RnR <- rbind(Block_perm0,Block_perm1)
# 
# ## ordnen nach Rew BlockR(Block & Perm kombiniert), Trial-&Reactiontype, + mean, SD, SE, n
# Block_RnR <- Block_RnR%>%group_by(Rew,BlockR,Trialtype, Reactiontype)%>%
#   summarise(m_RT=mean(m),
#             sd_RT=sd(m),
#             se_RT=sd(m)/sqrt(sum(!is.na(m))),
#             n=sum(!is.na(m)))
# 
# Block_RnR$Trialtype <- as.factor(Block_RnR$Trialtype)
# Block_RnR$BlockR <- as.factor(Block_RnR$BlockR)
# Block_RnR$Rew <- as.factor(Block_RnR$Rew)
# 
# hits <- filter(Block_RnR, Reactiontype == " hit")
# hits <- as.data.frame(hits)
# 
# ## Abbildung für Blocks
# require(ggplot2)
# Block_RnR_Plot <- ggplot(hits, 
#                          aes(x = Trialtype, y = m_RT, color = BlockR)) + 
#   geom_point(size = 1, position = position_dodge(1)) + 
#   geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
#                 width=0.2, size=1, position = position_dodge(1)) +
#   coord_cartesian(ylim = c(150, 500)) + facet_wrap(~ Rew); Block_RnR_Plot
# 
# ## Plot für Baseline
# ## Baselinedaten grouped nach ID, Block, Trialtype & Reactiontype
# Baseline_RT <- All_RT_selected%>%group_by(ID, Block,Trialtype, Reactiontype)%>%
#   summarise(m=mean(RT),
#             n=sum(!is.na(RT))
#             )
# ## mit VP-Zuordnung nach ID mergen
# Baseline_RT_Zuordnung <- merge(VP_Zuordnung, Baseline_RT, by = "ID")
# 
# ## soll in Plot mit Blocks 3. Kategorie von BlockR sein -> BlockR = 99
# Baseline_RT_Zuordnung <- mutate(Baseline_RT_Zuordnung, BlockR = "Baseline" )
# Baseline_RT_Zuordnung <- select(Baseline_RT_Zuordnung, ID, Rew, BlockR, Trialtype:n )
# 
# ## ordnen nach Rew BlockR, Trial-&Reactiontype, + mean, SD, SE, n
# Baseline_RnR <- Baseline_RT_Zuordnung%>%group_by(Rew,BlockR,Trialtype, Reactiontype)%>%
#   summarise(m_RT=mean(m),
#             sd_RT=sd(m),
#             se_RT=sd(m)/sqrt(sum(!is.na(m))),
#             n=sum(!is.na(m))
#             )
# 
# Baseline_RnR$Trialtype <- as.factor(Baseline_RnR$Trialtype)
# Baseline_RnR$BlockR <- as.factor(Baseline_RnR$BlockR)
# Baseline_RnR$Rew <- as.factor(Baseline_RnR$Rew)
# 
# hits_Baseline <- filter(Baseline_RnR, Reactiontype == " hit")
# hits_Baseline <- as.data.frame(hits_Baseline)
# 
# ## Abbildung für Baseline
# require(ggplot2)
# Baseline_RnR_Plot <- ggplot(hits_Baseline, 
#                        aes(x = Trialtype, y = m_RT, color = BlockR)) + 
#   geom_point(size = 1, position = position_dodge(1)) + 
#   geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
#                 width=0.2, size=1, position = position_dodge(1)) +
#     coord_cartesian(ylim = c(150, 500)) + facet_wrap(~ Rew); Baseline_RnR_Plot
# 
###### BEIDES ZUSAMMEN#####
# hits_All <- rbind(hits, hits_Baseline)
# hits_All$BlockR <- recode(hits_All$BlockR, "0"="no Reward", "1"="Reward")
# hits_All$BlockR = factor(hits_All$BlockR,levels(hits_All$BlockR)[c(3,1,2)])
# hits_All$Rew <- recode(hits_All$Rew, "0"="Verzögerte Belohnung", "1"="Direkte Belohnung")
# 
# ## Abbildung für Blocks und Baseline zusammen
# require(ggplot2)
# All_RnR_Plot <- ggplot(hits_All, 
#                          aes(x = Trialtype, y = m_RT, fill = BlockR, shape = BlockR)) +
#   geom_bar(size = 2, position = position_dodge(1),  stat = 'identity') +
#   geom_errorbar(data=hits_All, mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT),
#                 width = 0.2, size=1, position = position_dodge(1), color='black') +
#       coord_cartesian(ylim = c(200, 500)) +
#   labs(x = 'Trialtype', y = 'mean RT', title = 'Descriptive RTs') +
#   theme(axis.text.x = element_text(color = 'black', size = 13),
#         axis.text.y = element_text(color = 'black', size = 13),
#         strip.text = element_text(color="black", size = 13)) +
#   facet_wrap(~ Rew) ; All_RnR_Plot