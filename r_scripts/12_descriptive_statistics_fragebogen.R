##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                          Compute Descriptive Statistics
#                                  Fragebogen

# Load necessary packages
require(plyr)
require(dplyr)

# Get the data
Fragebogen <- read.table('~/Desktop/Bachelor/Daten/R_Frames/Fragebogen.txt', 
                              header = T)

# COMPUTE SCALE-SCORES

Fragebogen_Skalen <- mutate(Fragebogen, BI = X17 + X18 + X33 + X40 + X44 + X15 + X12 ,
                                        ZAP = X05 + X13 + X25 + X39 + X54 + X71 + X84 ,
                                        BR = X03 + X09 + X04 + X19 + X30 + X31 + X32 + X38 + X45 + X47 ,
                                        Imp = X29 + X35 + X36 + X48 + X53 + X57 + X68 + X70 ,
                                        BAS = BI + ZAP + BR + Imp,
                                        FFFS = X10 + X24 + X52 + X60 + X61 + X64 + X69 + X77 + X78 + X81 ,
                                        BIS = X01 + X02 + X07 + X08 + X11 + X21 + X23 + X28 + X37 + X41 + X42 + X55 + X56 + X62 + X65 + X66 + X74 + X75 + X76 + X79 + X80 + X82 + X83 ,
                                        Panik = X16 + X22 + X46 + X58 + X73 + X26 ,
                                        DK = X50 + X06 + X14 + X20 + X51 + X27 + X34 + X43)

Fragebogen_Skalen <- select(Fragebogen_Skalen, ID:Beruf, BI:DK, X01:X84, Dauergesamt) ## ordnen

# COMPUTE DESCRIPTIVE STATISTICS FOR QUESTIONNAIRE----

Fragebogen_sum_scalemeans<- Fragebogen_Skalen %>% dplyr::summarise(m_BI = mean(BI) ,
                                                            m_ZAP = mean(ZAP) ,
                                                            m_BR = mean(BR) ,
                                                            m_Imp = mean(Imp) ,
                                                            m_BAS = mean(BAS) ,
                                                            m_FFFS = mean(FFFS) ,
                                                            m_BIS = mean(BIS) ,
                                                            m_Panik = mean(Panik) ,
                                                            m_DK = mean(DK)
                                                            )



Fragebogen_sum_all<- Fragebogen_Skalen %>% dplyr::summarise(m_age = mean(age) ,
                                                 sd_age = sd(age) ,
                                                 m_sex = mean(sex) ,
                                                 sd_sex = sd(sex) ,
                                                 m_dauer = mean(Dauergesamt) ,
                                                 sd_dauer = sd(Dauergesamt) ,
                                                 m_BI = mean(BI) ,
                                                 sd_BI = sd(BI) ,
                                                 m_ZAP = mean(ZAP) ,
                                                 sd_ZAP = sd(ZAP) ,
                                                 m_BR = mean(BR) ,
                                                 sd_BR = sd(BR) ,
                                                 m_Imp = mean(Imp) ,
                                                 sd_Imp = sd(Imp) ,
                                                 m_BAS = mean(BAS) ,
                                                 sd_BAS = sd(BAS) ,
                                                 m_FFFS = mean(FFFS) ,
                                                 sd_FFFS = sd(FFFS) ,
                                                 m_BIS = mean(BIS) ,
                                                 sd_BIS = sd(BIS) ,
                                                 m_Panik = mean(Panik) ,
                                                 sd_Panik = sd(Panik) ,
                                                 m_DK = mean(DK) ,
                                                 sd_DK = sd(DK)
                                                 )


Fragebogen_sum_grouped<- Fragebogen_Skalen %>% dplyr::group_by(Rew, Perm) %>%
                                              dplyr::summarise(m_age = mean(age) ,
                                                       sd_age = sd(age) ,
                                                       m_sex = mean(sex) ,
                                                       sd_sex = sd(sex) ,
                                                       m_dauer = mean(Dauergesamt) ,
                                                       sd_dauer = sd(Dauergesamt) ,
                                                       m_BI = mean(BI) ,
                                                       sd_BI = sd(BI) ,
                                                       m_ZAP = mean(ZAP) ,
                                                       sd_ZAP = sd(ZAP) ,
                                                       m_BR = mean(BR) ,
                                                       sd_BR = sd(BR) ,
                                                       m_Imp = mean(Imp) ,
                                                       sd_Imp = sd(Imp) ,
                                                       m_BAS = mean(BAS) ,
                                                       sd_BAS = sd(BAS) ,
                                                       m_FFFS = mean(FFFS) ,
                                                       sd_FFFS = sd(FFFS) ,
                                                       m_BIS = mean(BIS) ,
                                                       sd_BIS = sd(BIS) ,
                                                       m_Panik = mean(Panik) ,
                                                       sd_Panik = sd(Panik) ,
                                                       m_DK = mean(DK) ,
                                                       sd_DK = sd(DK)
                                                       )
## Plot
# dafür Fragebogen_sum_all in Spalten statt Zeilen, dass eine Spalte jeweils BAS/BIS etc sagt und die zweite Spalte den Wert

Fragebogen_plot <- tidyr::gather(Fragebogen_sum_scalemeans)
Fragebogen_plot$key <-  as.factor(Fragebogen_plot$key)


# print(levels(Fragebogen_plot$key))  ## This will show the levels of x are "Levels: a b c d e"
## Reorder the levels:
## note, if x is not a factor use levels(factor(x))
Fragebogen_plot$key <- factor(Fragebogen_plot$key,levels(Fragebogen_plot$key)[c(2,9,4,7,1,3,6,5,8)])
# print(levels(Fragebogen_plot$key))

require(ggplot2)
# install.packages("viridis")
require(viridis)
ggplot(Fragebogen_plot, aes(x = key, y = value, fill = key)) + 
  geom_line(position = position_dodge(.5)) +
  coord_cartesian(ylim = c(10:90)) +
  scale_y_log10(breaks = seq(10,90,10)) +
  geom_bar(position = position_dodge(.5), size = 3, stat = "identity") + 
  scale_fill_viridis(option = "D", discrete = TRUE)

# Save for later use
write.table(Fragebogen_plot, '~/Desktop/Bachelor/Daten/R_Frames/Fragebogen_plot.txt', row.names = F, sep = '\t')

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