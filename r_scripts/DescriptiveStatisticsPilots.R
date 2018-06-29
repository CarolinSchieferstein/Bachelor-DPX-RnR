## Deskriptive Statistiken für Pilots

require(plyr)
require(dplyr)

## Daten einlesen von Runde 1 und 2 Baseline und Blocks
## Pilot1 Baseline
path1base <- c("/Users/carolin/Desktop/Bachelor/Daten/Pilotierung Runde 1 u 2/Logs_Pilot1") # <- location of files

paths1base <- dir(path = path1base, full.names = T, pattern = "rawdata.txt$")
names(paths1base) <- basename(paths1base)

## Pilot2 Baseline
path2base <- c("/Users/carolin/Desktop/Bachelor/Daten/Pilotierung Runde 1 u 2/Logs_Pilot2") # <- location of files

paths2base <- dir(path = path2base, full.names = T, pattern = "rawdata.txt$")
names(paths2base) <- basename(paths2base)

## Pilot1 Blocks
path1block <- c("/Users/carolin/Desktop/Bachelor/Daten/Pilotierung Runde 1 u 2/Logs_Pilot1") # <- location of files

paths1block <- dir(path = path1block, full.names = T, pattern = "block.txt$")
names(paths1block) <- basename(paths1block)

## Pilot2 Blocks
path2block <- c("/Users/carolin/Desktop/Bachelor/Daten/Pilotierung Runde 1 u 2/Logs_Pilot2") # <- location of files

paths2block <- dir(path = path2block, full.names = T, pattern = "block.txt$")
names(paths2block) <- basename(paths2block)

# ----- 2) Create data frame containing all files and observations

# **  RUN CODE to get dataframe with behavioral data form each individual
# **  The code cuts the dataframe down to a tidy version containing relevant variables 
# **  with the right attributues.

# Read in files
Pilot1base_RT <- plyr::ldply(paths1base, read.table, sep =",", dec = ".", header=F)
rm(paths1base)
Pilot1block_RT <- plyr::ldply(paths1block, read.table, sep =",", dec = ".", header=F)
rm(paths1block)
Pilot2base_RT <- plyr::ldply(paths2base, read.table, sep =",", dec = ".", header=F)
rm(paths2base)
Pilot2block_RT <- plyr::ldply(paths2block, read.table, sep =",", dec = ".", header=F)
rm(paths2block)

# Select columns with data, not names

Pilot1base_RT_selected <- select(Pilot1base_RT, .id, V2, V4, V6, V8,V10)
Pilot1block_RT_selected <- select(Pilot1block_RT, .id, V2, V4, V6, V8,V10)
Pilot2base_RT_selected <- select(Pilot2base_RT, .id, V2, V4, V6, V8,V10)
Pilot2block_RT_selected <- select(Pilot2block_RT, .id, V2, V4, V6, V8,V10)

## default names substitue with right names through vector of same length
names(Pilot1base_RT_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")
names(Pilot1block_RT_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")
names(Pilot2base_RT_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")
names(Pilot2block_RT_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")

Pilot1base_RT_selected$ID <- gsub(Pilot1base_RT_selected$ID, pattern = "rawdata.txt", replacement = "")
Pilot1block_RT_selected$ID <- gsub(Pilot1block_RT_selected$ID, pattern = "block.txt", replacement = "")
Pilot2base_RT_selected$ID <- gsub(Pilot2base_RT_selected$ID, pattern = "rawdata.txt", replacement = "")
Pilot2block_RT_selected$ID <- gsub(Pilot2block_RT_selected$ID, pattern = "block.txt", replacement = "")


## mean und Zahl nach miss/hit/incorrect
Pilot1base_RT_selected %>% group_by(Reactiontype)%>%summarise(m = mean(RT), n = sum(!is.na(RT)))
Pilot1block_RT_selected %>% group_by(Reactiontype)%>%summarise(m = mean(RT), n = sum(!is.na(RT)))
Pilot2base_RT_selected %>% group_by(Reactiontype)%>%summarise(m = mean(RT), n = sum(!is.na(RT)))
Pilot2block_RT_selected %>% group_by(Reactiontype)%>%summarise(m = mean(RT), n = sum(!is.na(RT)))
## mean, sd, se und Zahl nach miss/hit/incorrect bei versch. Trialtypes
Pilot1base_RT_selected%>%group_by(Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )
Pilot1block_RT_selected%>%group_by(Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )
Pilot2base_RT_selected%>%group_by(Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )
Pilot2block_RT_selected%>%group_by(Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )

##s.o. nach VP getrennt
Pilot1base_descriptive <- Pilot1base_RT_selected%>%group_by(ID, Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )
Pilot1block_descriptive <- Pilot1block_RT_selected%>%group_by(ID, Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )
Pilot2base_descriptive <- Pilot2base_RT_selected%>%group_by(ID, Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )
Pilot2block_descriptive <- Pilot2block_RT_selected%>%group_by(ID, Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )

## mean RT und mean n über Personen pro Trialtype

Pilot1base_descriptiveAcross <- Pilot1base_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            M_N=mean(n))
Pilot1block_descriptiveAcross <- Pilot1block_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            M_N=mean(n))
Pilot2base_descriptiveAcross <- Pilot2base_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            M_N=mean(n))
Pilot2block_descriptiveAcross <- Pilot2block_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            M_N=mean(n))

Pilot1base_descriptiveAcrossWsdse <- Pilot1base_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            SD_RT=sd(m_RT),
            SE_RT=sd(m_RT)/sqrt(sum(!is.na(m_RT))),
            M_N=mean(n))
Pilot1block_descriptiveAcrossWsdse <- Pilot1block_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            SD_RT=sd(m_RT),
            SE_RT=sd(m_RT)/sqrt(sum(!is.na(m_RT))),
            M_N=mean(n))
Pilot2base_descriptiveAcrossWsdse <- Pilot2base_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            SD_RT=sd(m_RT),
            SE_RT=sd(m_RT)/sqrt(sum(!is.na(m_RT))),
            M_N=mean(n))
Pilot2block_descriptiveAcrossWsdse <- Pilot2block_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            SD_RT=sd(m_RT),
            SE_RT=sd(m_RT)/sqrt(sum(!is.na(m_RT))),
            M_N=mean(n))


## plot trialtype, reactiontype and mean RT
par(mfrow=c(2,2))
Pilot1base_plot <- interaction.plot(Pilot1base_descriptiveAcrossWsdse$Trialtype, Pilot1base_descriptiveAcrossWsdse$Reactiontype, Pilot1base_descriptiveAcrossWsdse$M_RT, 
                                  type = "b", col = c(2:6), 
                                  pch = c(18, 24), 
                                  xlab = "Trialtype", 
                                  ylab="mean RT",
                                  ylim = c(0,600))
Pilot1block_plot <- interaction.plot(Pilot1block_descriptiveAcrossWsdse$Trialtype, Pilot1block_descriptiveAcrossWsdse$Reactiontype, Pilot1block_descriptiveAcrossWsdse$M_RT, 
                                type = "b", col = c(2:6), 
                                pch = c(18, 24), 
                                xlab = "Trialtype", 
                                ylab="mean RT",
                                ylim = c(0,600))
Pilot2base_plot <- interaction.plot(Pilot2base_descriptiveAcrossWsdse$Trialtype, Pilot2base_descriptiveAcrossWsdse$Reactiontype, Pilot2base_descriptiveAcrossWsdse$M_RT, 
                                type = "b", col = c(2:6), 
                                pch = c(18, 24), 
                                xlab = "Trialtype", 
                                ylab="mean RT",
                                ylim = c(0,600))
Pilot2block_plot <- interaction.plot(Pilot2block_descriptiveAcrossWsdse$Trialtype, Pilot2block_descriptiveAcrossWsdse$Reactiontype, Pilot2block_descriptiveAcrossWsdse$M_RT, 
                                type = "b", col = c(2:6), 
                                pch = c(18, 24), 
                                xlab = "Trialtype", 
                                ylab="mean RT",
                                ylim = c(0,600))

##############################################################################################################################################################
##############################################################################################################################################################

## Plots
## Grouped nach ID, Block, Trialtype & Reactiontype
Pilot1base_RT <- Pilot1base_RT_selected%>%group_by(ID, Block,Trialtype, Reactiontype)%>%
  summarise(m=mean(RT),
            n=sum(!is.na(RT))
  )
Pilot1block_RT <- Pilot1block_RT_selected%>%group_by(ID, Block,Trialtype, Reactiontype)%>%
  summarise(m=mean(RT),
            n=sum(!is.na(RT))
  )
Pilot2base_RT <- Pilot2base_RT_selected%>%group_by(ID, Block,Trialtype, Reactiontype)%>%
  summarise(m=mean(RT),
            n=sum(!is.na(RT))
  )
Pilot2block_RT <- Pilot2block_RT_selected%>%group_by(ID, Block,Trialtype, Reactiontype)%>%
  summarise(m=mean(RT),
            n=sum(!is.na(RT))
  )
## Blöcke nach Rew - kein Rew kodieren

# Perm = 0 --> Block 1,3,5,7,9 nR // Block 2,4,6,8,10 R
Pilot1_Block_RnR <- dplyr::mutate(Pilot1block_RT, BlockR= ifelse(Block%in%c(1,3,5,7,9),0,1))
Pilot2_Block_RnR <- dplyr::mutate(Pilot2block_RT, BlockR= ifelse(Block%in%c(1,3,5,7,9),0,1))

## kommt in Pilots nicht vor
# Perm = 1 --> Block 1,3,5,7,9 R // BLock 2,4,6,8,10 nR
#Block_perm1 <- dplyr::mutate(Block_perm1, BlockR= ifelse(Block%in%c(1,3,5,7,9),1,0))
## wieder zusammenpacken
##Block_RnR <- rbind(Block_perm0,Block_perm1)

## ordnen nach BlockR(Block & Perm kombiniert), Trial-&Reactiontype, + mean, SD, SE, n    # nicht nach Rew, in Pilots immer = 0
Pilot1_Block_RnR <- Pilot1_Block_RnR%>%group_by(BlockR,Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(m),
            sd_RT=sd(m),
            se_RT=sd(m)/sqrt(sum(!is.na(m))),
            n=sum(!is.na(m)))
Pilot2_Block_RnR <- Pilot2_Block_RnR%>%group_by(BlockR,Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(m),
            sd_RT=sd(m),
            se_RT=sd(m)/sqrt(sum(!is.na(m))),
            n=sum(!is.na(m)))

Pilot1_Block_RnR$Trialtype <- as.factor(Pilot1_Block_RnR$Trialtype)
Pilot1_Block_RnR$BlockR <- as.factor(Pilot1_Block_RnR$BlockR)
Pilot2_Block_RnR$Trialtype <- as.factor(Pilot2_Block_RnR$Trialtype)
Pilot2_Block_RnR$BlockR <- as.factor(Pilot2_Block_RnR$BlockR)

hits_Pilot1Block <- filter(Pilot1_Block_RnR, Reactiontype == " hit")
hits_Pilot1Block <- as.data.frame(hits_Pilot1Block)
hits_Pilot2Block <- filter(Pilot2_Block_RnR, Reactiontype == " hit")
hits_Pilot2Block <- as.data.frame(hits_Pilot2Block)

## Abbildung für Blocks
require(ggplot2)
Pilot1_Block_RnR_Plot <- ggplot(hits_Pilot1Block, 
                         aes(x = Trialtype, y = m_RT, color = BlockR)) + 
  geom_point(size = 1, position = position_dodge(1)) + 
  geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
                width=0.2, size=1, position = position_dodge(1)) +
  coord_cartesian(ylim = c(200, 500)); Pilot1_Block_RnR_Plot
Pilot2_Block_RnR_Plot <- ggplot(hits_Pilot2Block, 
                                aes(x = Trialtype, y = m_RT, color = BlockR)) + 
  geom_point(size = 1, position = position_dodge(1)) + 
  geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
                width=0.2, size=1, position = position_dodge(1)) +
  coord_cartesian(ylim = c(200, 500)); Pilot2_Block_RnR_Plot

################################
## Plot für Baseline

## soll in Plot mit Blocks 3. Kategorie von BlockR sein -> BlockR = Baseline
Pilot1base_BlockR <- mutate(Pilot1base_RT, BlockR = "Baseline" )
Pilot1base_BlockR <- select(Pilot1base_BlockR, ID, BlockR, Trialtype:n)
Pilot2base_BlockR <- mutate(Pilot2base_RT, BlockR = "Baseline" )
Pilot2base_BlockR <- select(Pilot2base_BlockR, ID, BlockR, Trialtype:n)

## ordnen nach Rew BlockR, Trial-&Reactiontype, + mean, SD, SE, n
Pilot1_Base_RnR <- Pilot1base_BlockR%>%group_by(BlockR,Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(m),
            sd_RT=sd(m),
            se_RT=sd(m)/sqrt(sum(!is.na(m))),
            n=sum(!is.na(m))
  )
Pilot2_Base_RnR <- Pilot2base_BlockR%>%group_by(BlockR,Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(m),
            sd_RT=sd(m),
            se_RT=sd(m)/sqrt(sum(!is.na(m))),
            n=sum(!is.na(m))
  )

Pilot1_Base_RnR$Trialtype <- as.factor(Pilot1_Base_RnR$Trialtype)
Pilot1_Base_RnR$BlockR <- as.factor(Pilot1_Base_RnR$BlockR)
Pilot2_Base_RnR$Trialtype <- as.factor(Pilot2_Base_RnR$Trialtype)
Pilot2_Base_RnR$BlockR <- as.factor(Pilot2_Base_RnR$BlockR)

hits_Pilot1Base <- filter(Pilot1_Base_RnR, Reactiontype == " hit")
hits_Pilot1Base <- as.data.frame(hits_Pilot1Base)
hits_Pilot2Base <- filter(Pilot1_Base_RnR, Reactiontype == " hit")
hits_Pilot2Base <- as.data.frame(hits_Pilot2Base)

## Abbildung für Baseline
require(ggplot2)
Pilot1_Base_RnR_Plot <- ggplot(hits_Pilot1Base, 
                            aes(x = Trialtype, y = m_RT, color = BlockR)) + 
  geom_point(size = 1, position = position_dodge(1)) + 
  geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
                width=0.2, size=1, position = position_dodge(1)) +
  coord_cartesian(ylim = c(200, 500)); Pilot1_Base_RnR_Plot

Pilot2_Base_RnR_Plot <- ggplot(hits_Pilot2Base, 
                               aes(x = Trialtype, y = m_RT, color = BlockR)) + 
  geom_point(size = 1, position = position_dodge(1)) + 
  geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
                width=0.2, size=1, position = position_dodge(1)) +
  coord_cartesian(ylim = c(200, 500)); Pilot2_Base_RnR_Plot

## Datensätze zusammenpacken
hits_AllPilot1 <- rbind(hits_Pilot1Base, hits_Pilot1Block)
hits_AllPilot1$BlockR <- recode(hits_AllPilot1$BlockR, "0"="no Reward", "1"="Reward")
hits_AllPilot1$BlockR = factor(hits_AllPilot1$BlockR,levels(hits_AllPilot1$BlockR)[c(3,1,2)])

hits_AllPilot2 <- rbind(hits_Pilot2Base, hits_Pilot2Block)
hits_AllPilot2$BlockR <- recode(hits_AllPilot2$BlockR, "0"="no Reward", "1"="Reward")
hits_AllPilot2$BlockR = factor(hits_AllPilot2$BlockR,levels(hits_AllPilot2$BlockR)[c(3,1,2)])

## Abbildung für Blocks und Baseline zusammen
require(ggplot2)
AllPilot1_RnR_Plot <- ggplot(hits_AllPilot1, 
                       aes(x = Trialtype, y = m_RT, fill = BlockR, shape = BlockR)) +
  geom_bar(size = 2, position = position_dodge(1),  stat = 'identity') +
  geom_errorbar(data=hits_AllPilot1, mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT),
                width = 0.2, size=1, position = position_dodge(1), color='black') +
  coord_cartesian(ylim = c(200, 500)) +
  labs(x = 'Trialtype', y = 'mean RT', title = 'Descriptive RTs Pilot1') +
  theme(axis.text.x = element_text(color = 'black', size = 13),
        axis.text.y = element_text(color = 'black', size = 13),
        strip.text = element_text(color="black", size = 13)) ; AllPilot1_RnR_Plot

AllPilot2_RnR_Plot <- ggplot(hits_AllPilot2, 
                             aes(x = Trialtype, y = m_RT, fill = BlockR, shape = BlockR)) +
  geom_bar(size = 2, position = position_dodge(1),  stat = 'identity') +
  geom_errorbar(data=hits_AllPilot2, mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT),
                width = 0.2, size=1, position = position_dodge(1), color='black') +
  coord_cartesian(ylim = c(200, 500)) +
  labs(x = 'Trialtype', y = 'mean RT', title = 'Descriptive RTs Pilot2') +
  theme(axis.text.x = element_text(color = 'black', size = 13),
        axis.text.y = element_text(color = 'black', size = 13),
        strip.text = element_text(color="black", size = 13)); AllPilot2_RnR_Plot

