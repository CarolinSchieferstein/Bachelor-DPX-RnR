## Deskriptive Statistiken für Baseline

require(plyr)
require(dplyr)

## mean und Zahl nach miss/hit/incorrect
All_RT_selected %>% group_by(Reactiontype)%>%summarise(m = mean(RT), n = sum(!is.na(RT)))
## mean, sd, se und Zahl nach miss/hit/incorrect bei versch. Trialtypes
All_RT_selected%>%group_by(Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )
##s.o. nach VP getrennt
Baseline_descriptive <- All_RT_selected%>%group_by(ID, Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )

## mean RT und mean n über Personen pro Trialtype

Baseline_descriptiveAcross <- Baseline_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            M_N=mean(n))

Baseline_descriptiveAcrossWsdse <- Baseline_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            SD_RT=sd(m_RT),
            SE_RT=sd(m_RT)/sqrt(sum(!is.na(m_RT))),
            M_N=mean(n))


## plot trialtype, reactiontype and mean RT

Baseline_plot <- interaction.plot(Baseline_descriptiveAcrossWsdse$Trialtype, Baseline_descriptiveAcrossWsdse$Reactiontype, Baseline_descriptiveAcrossWsdse$M_RT, 
                                  type = "b", col = c(2:6), 
                                  pch = c(18, 24), 
                                  xlab = "Trialtype", 
                                  ylab="mean RT")

########################
## Deskriptive Statistiken für Blocks

require(plyr)
require(dplyr)

## mean und Zahl nach miss/hit/incorrect
Block_RT_selected <- Block_RT_selected %>% group_by(ID,Block,Trialtype,Reactiontype)%>%summarise(m = mean(RT), n = sum(!is.na(RT)))
## mean, sd, se und Zahl nach miss/hit/incorrect bei versch. Trialtypes
Block_RT_selected%>%group_by(Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )
##s.o. nach VP getrennt
Block_descriptive <- Block_RT_selected%>%group_by(ID, Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(RT),
            sd_RT=sd(RT),
            se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
            n=sum(!is.na(RT))
  )

## mean RT und mean n über Personen pro Trialtype

Block_descriptiveAcross <- Block_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            M_N=mean(n))

Block_descriptiveAcrossWsdse <- Block_descriptive%>%group_by(Trialtype, Reactiontype)%>%
  summarise(M_RT=mean(m_RT),
            SD_RT=sd(m_RT),
            SE_RT=sd(m_RT)/sqrt(sum(!is.na(m_RT))),
            M_N=mean(n))

## plot trialtype, reactiontype and mean RT

Block_plot <- interaction.plot(Block_descriptiveAcrossWsdse$Trialtype, Block_descriptiveAcrossWsdse$Reactiontype, Block_descriptiveAcrossWsdse$M_RT, 
                               type = "b", col = c(2:6), 
                               pch = c(18, 24), 
                               xlab = "Trialtype", 
                               ylab="mean RT")


## VP_Zuordnung einlesen

VP_Zuordnung <- read.table("Desktop/VP_Zuordnung.txt", header=TRUE)

## Zuordnung nach ID mit Daten mergen, dann nach perm aufteilen

colnames(VP_Zuordnung)[colnames(VP_Zuordnung)=="VP"] <- "ID"
Block_RT_Zuordnung <- merge(VP_Zuordnung, Block_RT_selected, by = "ID")
Block_perm0 <- dplyr::filter(Block_RT_Zuordnung, Perm==0)
Block_perm1 <- dplyr::filter(Block_RT_Zuordnung, Perm==1)

## Blöcke nach Rew - kein Rew kodieren

# Perm = 0 --> Block 1,3,5,7,9 nR // Block 2,4,6,8,10 R
Block_perm0 <- dplyr::mutate(Block_perm0, BlockR= ifelse(Block%in%c(1,3,5,7,9),0,1))
# Perm1 --> Block 1,3,5,7,9 R // BLock 2,4,6,8,10 nR
Block_perm1 <- dplyr::mutate(Block_perm1, BlockR= ifelse(Block%in%c(1,3,5,7,9),1,0))

## wieder zusammenpacken

Block_RnR <- rbind(Block_perm0,Block_perm1)

## ordnen nach Rew BlockR(Block & Perm kombiniert), Trial-&Reactiontype, + mean, SD, SE, n
Block_RnR <- Block_RnR%>%group_by(Rew,BlockR,Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(m),
            sd_RT=sd(m),
            se_RT=sd(m)/sqrt(sum(!is.na(m))),
            n=sum(!is.na(m)))

Block_RnR$Trialtype <- as.factor(Block_RnR$Trialtype)
Block_RnR$BlockR <- as.factor(Block_RnR$BlockR)
Block_RnR$Rew <- as.factor(Block_RnR$Rew)

hits <- filter(Block_RnR, Reactiontype == " hit")
hits <- as.data.frame(hits)

## Abbildung für Blocks
require(ggplot2)
Block_RnR_Plot <- ggplot(hits, 
                         aes(x = Trialtype, y = m_RT, color = BlockR)) + 
  geom_point(size = 1, position = position_dodge(1)) + 
  geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
                width=0.2, size=1, position = position_dodge(1)) +
  coord_cartesian(ylim = c(150, 500)) + facet_wrap(~ Rew); Block_RnR_Plot

## Plot für Baseline
## Baselinedaten grouped nach ID, Block, Trialtype & Reactiontype
Baseline_RT <- All_RT_selected%>%group_by(ID, Block,Trialtype, Reactiontype)%>%
  summarise(m=mean(RT),
            n=sum(!is.na(RT))
            )
## mit VP-Zuordnung nach ID mergen
Baseline_RT_Zuordnung <- merge(VP_Zuordnung, Baseline_RT, by = "ID")

## soll in Plot mit Blocks 3. Kategorie von BlockR sein -> BlockR = 99
Baseline_RT_Zuordnung <- mutate(Baseline_RT_Zuordnung, BlockR = 99 )
Baseline_RT_Zuordnung <- select(Baseline_RT_Zuordnung, ID, Rew, BlockR, Trialtype:n )

## ordnen nach Rew BlockR, Trial-&Reactiontype, + mean, SD, SE, n
Baseline_RnR <- Baseline_RT_Zuordnung%>%group_by(Rew,BlockR,Trialtype, Reactiontype)%>%
  summarise(m_RT=mean(m),
            sd_RT=sd(m),
            se_RT=sd(m)/sqrt(sum(!is.na(m))),
            n=sum(!is.na(m))
            )

Baseline_RnR$Trialtype <- as.factor(Baseline_RnR$Trialtype)
Baseline_RnR$BlockR <- as.factor(Baseline_RnR$BlockR)
Baseline_RnR$Rew <- as.factor(Baseline_RnR$Rew)

hits_Baseline <- filter(Baseline_RnR, Reactiontype == " hit")
hits_Baseline <- as.data.frame(hits_Baseline)

## Abbildung für Baseline
require(ggplot2)
Baseline_RnR_Plot <- ggplot(hits_Baseline, 
                       aes(x = Trialtype, y = m_RT, color = BlockR)) + 
  geom_point(size = 1, position = position_dodge(1)) + 
  geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
                width=0.2, size=1, position = position_dodge(1)) +
    coord_cartesian(ylim = c(150, 500)) + facet_wrap(~ Rew); Baseline_RnR_Plot


## Abbildung für Blocks und Baseline zusammen
require(ggplot2)
All_RnR_Plot <- ggplot(hits, 
                         aes(x = Trialtype, y = m_RT, color = BlockR)) + 
  geom_point(size = 1, position = position_dodge(1)) + 
  geom_errorbar(mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), 
                width=0.2, size=1, position = position_dodge(1)) +
  geom_point(data=hits_Baseline,size = 1, position = position_dodge(1))+
  geom_errorbar(data=hits_Baseline, mapping = aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT),
                width=0.2, size=1, position = position_dodge(1)) +
  coord_cartesian(ylim = c(150, 500)) + facet_wrap(~ Rew); All_RnR_Plot
