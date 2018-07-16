##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 July 2018 
#                       Compute Descriptive Statistics
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
total_trials <- RT_Zuordnung%>%group_by(ID, Trialtype, Rew, Phase)%>% summarise(total=sum(!is.na(Trialnr)))

# Incorrects summary: mean und n
FAs_sum<- FAs %>% group_by(ID, Trialtype, Rew, Phase) %>% summarise(m_RT = mean(RT), n = sum(!is.na(RT)))
# FAs_sum mit Daten mit gezählten Trials zusammen
FAs_sum <- merge(FAs_sum, total_trials, c("ID", "Trialtype", "Rew", "Phase"))
# FEHLERRATE berechnen
FAs_sum <- mutate(FAs_sum, Fehlerrate = n/total)
# Mittlere Fehlerrate und SE
FAs_sum_grouped <- FAs_sum %>% group_by(Trialtype) %>%
                        summarise(m_FR=mean(Fehlerrate),
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