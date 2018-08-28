##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                          ANOVA REAKTIONSZEITEN MAIN

# -- Helper functions ----
getPacks <- function( packs ) {
  
  # Check wich packages are not intalled and install them
  if ( sum(!packs %in% installed.packages()[, 'Package'])) {
    install.packages( packs[ which(!packs %in% installed.packages()[, 'Package']) ], 
                      dependencies = T)
  }
  
  # Require all packages
  sapply(packs, require, character.only =  T)
  
}

# -- Load necessary packages
pkgs <- c('dplyr', 'plyr', 
          'emmeans', 'car', 'sjstats', 'ggplot2')
getPacks(pkgs)
rm(pkgs)

# Dataframe entsprechend FAs_sum_All erstellen: Baseline und Blocks zusammenpacken
Hits$ID <- as.factor(Hits$ID)
Hits$Rew <- as.factor(Hits$Rew)
Hits$Perm <- as.factor(Hits$Perm)
Hits$Phase <- as.factor(Hits$Phase)
Hits$Trialtype <- as.factor(Hits$Trialtype)

Hits_B$ID <- as.factor(Hits_B$ID)
Hits_B$Rew <- as.factor(Hits_B$Rew)
Hits_B$Perm <- as.factor(Hits_B$Perm)
Hits_B$Phase <- as.factor(Hits_B$Phase)
Hits_B$Trialtype <- as.factor(Hits_B$Trialtype)

Hits_All <- rbind(Hits, Hits_B)

Hits_sum_All <- Hits_All %>% dplyr::group_by(ID, Trialtype, Rew, Phase) %>%
  dplyr::summarise(m_RT = mean(RT),
                   se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
                   n = sum(!is.na(RT)))

# Effektkodierung: Alle Fehlerraten mit Gesamtdurchschnitt vergleichen (sonst default = Dummy-Kodierung)
contrasts(Hits_sum_All$Trialtype) <- contr.sum(4); contrasts(Hits_sum_All$Trialtype)
contrasts(Hits_sum_All$Phase) <- contr.sum(3); contrasts(Hits_sum_All$Phase)

# Modell direkt auf Daten ohne summary
RT_mod_log <- lm(log(m_RT) ~ Trialtype*Rew*Phase, data = Hits_sum_All)
RT_mod_no_log <- lm(m_RT ~ Trialtype*Rew*Phase, data = Hits_sum_All)   # ausprobieren, ob Rechnung mit oder ohne log besser
hist(Hits_sum_All$m_RT)
hist(log(Hits_sum_All$m_RT))     # besser ohne log, weil auch normalverteilt und s.u.
sjstats::r2(RT_mod_log)
sjstats::r2(RT_mod_no_log)   # Modelfit für log und ohne log Modell -> ohne log besser
RT_a_mod<- car::Anova(RT_mod_no_log, type=3); RT_a_mod # Modell für ANOVA, type=3 -> Effekte unabhängig voneinander geprüft
eta_sq(RT_a_mod, partial=F)  # Effektgröße Eta-Quadrat (hat Konventionen)

emmeans::emmeans(RT_mod_no_log, pairwise ~ Trialtype | Rew , adjust = "fdr") # paarweise Vergleiche aller Stufen
emmeans::emmip(RT_mod_no_log, Rew ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Trialtypen",
       title = "Modellvorhersage Reaktionszeiten Haupttestung") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)
  



############### VON LINDA ##################
# #--------Merge behavioral and personality data---------
# Data_full<- merge (Data_card, Data_pers_full, by.x='VP', by.y = 'VP')
# 
# Data_full$Block<-as.factor(Data_full$Block)


#--------Data frame for frequency of cards---------

Data_sum <- Data_full %>% 
  dplyr::group_by(VP, Block, Card) %>% 
  dplyr::summarise(N=sum(!is.na(Card))) %>% 
  dplyr::filter(!Card==0)



#--------Alternative: bigger data set for Analysis with slopes------

Data_RT_all <- Data_card %>% 
  dplyr::group_by(VP, Block, Card) %>% 
  dplyr::summarise(RT_mean=mean(RT)) %>%
  dplyr::filter(!Card==0)

perso <- dplyr::select(Data_pers_score, BAS_Score, MAE_Score, BIS, FFFS, 
                       PE, AC, SP, BAS_Rew_Int, 
                       BAS_Rew_Reac, BAS_Goal_Drive, BAS_Impulsiv, VP)
Data_reg_all <- merge(Data_sum, perso, 'VP')
Data_reg_all <-merge(Data_reg_all, Data_RT_all)
Data_reg_all <-merge(Data_reg_all, Data_score)
Data_reg_all <- merge (Data_reg_all, Data_behav3)
Data_reg_all$VP <-as.factor(Data_reg_all$VP)  

Data_reg_all$RT_log = log(Data_reg_all$RT_mean)



#--------HLM RT by Block and VP----------

# Model 1
fm1 <- lmer(RT ~ Block + (1 | VP), data=Data_full, REML = F)
summary(fm1)
anova(fm1)

# Model 2
fm2 <- lmer(RT ~ Block + (1+ Block | VP), data=Data_full, REML = F)
summary(fm2)
anova(fm2)

# Model 3
fm3 <- lmer(RT ~ Block + (1 | VP/Block), data=Data_full, REML = F)
anova(fm3)
summary(fm3)

# Compare models to determine best fit
anova(fm1, fm2, fm3)

# plotting random effects
sjp.lmer(fm3, y.offset = .4)

sjp.lmer(fm3,
         facet.grid = F,
         sort.est = 'sort.all',
         y.offset = .4)

sjp.lmer(fm3, type = "ri.slope")

sjp.lmer(fm1, type = "rs.ri", vars = "Block", sample.n = 30)

sjp.lmer(fm3, 
         type = "rs.ri", 
         vars = "Block",
         facet.grid = FALSE)

#--------Correlations in Personality data----------------------


Data_pers_score_all<- Data_pers_score %>% dplyr::select(PE, AC, SP, MAE_Score, FFFS, BIS, BAS_Rew_Int, BAS_Rew_Reac, BAS_Goal_Drive, BAS_Impulsiv, BAS_Score)

matrix <- cor(Data_pers_score_all)
round(matrix,2)

matrix2<-rcorr(as.matrix(Data_pers_score_all))
matrix2

matrix2$r
matrix2$P

flattenCorrMatrix(matrix2$r, matrix2$P)
rcorr(matrix, type=c("pearson"), )
matrix$r


# Plot:  Insignificant correlation 
corrplot(matrix2$r, method=c("number"), type="full", order="original", 
         p.mat = matrix2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)

#Plot: full
corrplot(matrix2$r, method=c("number"), type="full", order="original", tl.col="black", tl.srt=45)



#--------Plot Personality Scores by Person------

mplot <-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
BIS<-c(55,64,50,66,49,59,39,49,82,38,65,62,33,47,50,47,60,50,52,74,62,80,48,74,62,59,46,64,47,51)
BAS<-c(103,101,100,87,77,84,96,80,70,103,88,85,81,88,98,91,105,76,91,84,86,91,85,87,95,89,70,97,70,94)
FFFS<-c(25,29,34,33,12,24,17,21,32,21,17,18,11,14,21,21,27,15,18,22,14,25,21,30,23,12,20,24,12,22)
MAE <-c(60,55,65,40,32,55,54,43,12,69,44,40,67,59,74,47,73,48,38,40,50,46,43,37,52,53,47,55,31,56)

plot(mplot, BIS, type="o", col="blue", pch="o", lty=1, ylim = c(0,120),ylab="Personality_Score", xlab="Person")
points(mplot, BAS, col="red", pch="*")
lines(mplot, BAS, col="red", lty=2)
points(mplot, FFFS, col="dark red", pch="+")
lines(mplot, FFFS, col="dark red", lty=3)
points(mplot, MAE, col="green", pch="#")
lines(mplot, MAE, col="green", lty=4)
legend(1,125,legend=c("BIS","BAS","FFFS","MAE"), col=c("blue","red","dark red", "green"),
       pch=c("o","*","+", "#"),lty=c(1,2,3,4), ncol=4)


rm(BAS,BIS,FFFS,MAE)

#Standardize values

BAS_z <- scale (Data_pers_score$BAS_Score)
BIS_z <- scale (Data_pers_score$BIS)
FFFS_z <- scale (Data_pers_score$FFFS)
MAE_z <- scale (Data_pers_score$MAE_Score)


show_col(viridis_pal()(20))

plot(mplot, BIS_z, type="o", col="#482878FF", pch="o", lty=1,lwd=2, ylim = c(-5,5),ylab="Personality_Score", xlab="Person")
points(mplot, BAS_z, col="#2D718EFF", pch="o")
lines(mplot, BAS_z, col="#2D718EFF", lty=1, lwd=2)
points(mplot, FFFS_z, col="#29AF7FFF", pch="o")
lines(mplot, FFFS_z, col="#29AF7FFF", lty=1, lwd=2)
points(mplot, MAE_z, col="#B8DE29FF", pch="o")
lines(mplot, MAE_z, col="#B8DE29FF", lty=1, lwd=2)
legend(1,5,legend=c("BIS","BAS","FFFS","MAE"), col=c("#482878FF","#2D718EFF","#29AF7FFF", "#B8DE29FF"),
       pch=c("o","o","o", "o"),lty=c(1,1,1,1), ncol=4)

#--------Correlation matrix including IGT Score------

Data_pers_score_all<- Data_pers_score %>% dplyr::select(PE, AC, SP, MAE_Score, FFFS, BIS, BAS_Rew_Int, BAS_Rew_Reac, BAS_Goal_Drive, BAS_Impulsiv, BAS_Score, IGT_Score_all) 

matrix2<-rcorr(as.matrix(Data_pers_score_all))

corrplot(matrix2$r, method=c("number"), type="full", order="original", 
         p.mat = matrix2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)                 

#--------HLM-Card by Block and Card-----------------------------

mod_card <- lm(N ~ Card*Block, data=Data_reg_all)
car::Anova(mod_card,type=3)
emmeans(mod_card, pairwise ~ Card, adjust='Bonferroni')


mod_card <- lmer(N ~ Card*Block + (1+Card|VP), data=Data_reg_all, REML = F)
anova(mod_card)
AIC(mod_card)
emmeans(mod_card, pairwise ~ Card |Block, adjust='Bonferroni')

visreg(mod_card,'Card', by='Block')

mf<-allEffects((mod_card))
plot(mf)

plot.emm():
  mod_card <- lmer(N ~ Card*Block + (1|VP/Card), data=Data_reg_all, REML= F)
AIC(mod_card)

emmip(mod_card, Block ~ Card)
emmip(mod_card, Block ~ Card | Block, CIs=T)

#--------Regression: IGT Score by Personality and Block -----

m1<-lm(IGT_Score ~ 
         Block*BAS_Score + 
         Block*BIS + 
         Block*FFFS + 
         Block*MAE_Score, data=Data_reg)
summary(m1)
car::Anova(m1, type=3)


m1_1<-lm(IGT_Score ~ Block + BAS_Score + BIS + 
           + FFFS + MAE_Score,
         data=Data_reg)
summary(m1_1)
car::Anova(m1_1, type=3)
visreg(m1_1)


m2<-lm(IGT_Score ~ 
         Block*BAS_Rew_Int +
         Block*BAS_Rew_Reac +
         Block*BAS_Goal_Drive +
         Block*BAS_Impulsiv, data=Data_reg)
summary(m2)
car::Anova(m2, type=3)

m2_1<-lm(IGT_Score ~ Block +
           BAS_Rew_Int +
           BAS_Rew_Reac +
           BAS_Goal_Drive +
           BAS_Impulsiv, data=Data_reg)
car::Anova(m2_1, type=3)
visreg(m2_1)

m3<-lm(IGT_Score ~   
         Block*PE + 
         Block*AC + 
         Block*SP, 
       data=Data_reg)
summary(m3)
car::Anova(m3,type=3)

m3_1<-lm(IGT_Score ~ Block + PE + AC + SP, 
         data=Data_reg)
car::Anova(m3_1,type=3)
visreg(m3_1)

#Score by RT and by Block only
m4<-lm(IGT_Score~RT_mean, data=Data_reg)
summary(m4)
car::Anova(m4,type=3)

m5<-lm(IGT_Score~Block, data=Data_reg)
summary(m5)
car::Anova(m5,type=3)

m5<-lmer(IGT_Score~Block + (1|VP), data=Data_reg)
anova(m5)
emmeans(m5, pairwise ~ Block, adjust='fdr')
visreg(m5)

#--------Regression: Payoff by Personality----------------------

cor.test(Data_reg$Payoff, Data_reg$IGT_Score)

m1<-lm(Payoff~Block*BAS_Score+
         Block*BIS+
         Block*FFFS+
         Block*MAE_Score, data=Data_reg)
car::Anova(m1,type=3)

m1_1<-lm(Payoff~Block+BAS_Score+BIS+FFFS+MAE_Score, data=Data_reg)
car::Anova(m1_1,type=3)


m2<-lm(Payoff~Block*BAS_Rew_Int+ 
         Block*BAS_Rew_Reac+ 
         Block*BAS_Goal_Drive+
         Block*BAS_Impulsiv, data=Data_reg)
car::Anova(m2,type=3)

m2_1<-lm(Payoff~Block+BAS_Rew_Int+BAS_Rew_Reac+BAS_Goal_Drive+BAS_Impulsiv, data=Data_reg)
car::Anova(m2_1,type=3)


m3<-lm(Payoff~Block*PE+
         Block*AC+
         Block*SP, data=Data_reg)
car::Anova(m3,type=3)

m3_1<-lm(Payoff~Block+PE+AC+SP, data=Data_reg)
car::Anova(m3_1,type=3)


#--------Regression: RT_mean by Personality and Block----------

m1<-lm(RT_mean ~ 
         Block*BAS_Score +
         Block*BIS + 
         Block*FFFS +
         Block*MAE_Score, data=Data_reg)
summary(m1)
car::Anova(m1,type=3)

m1_1<-lm(RT_mean ~ 
           Block + BAS_Score +
           BIS + 
           FFFS +
           MAE_Score, data=Data_reg)
car::Anova(m1_1,type=3)

visreg(m1_1)

m2<-lm(RT_mean ~ 
         Block*BAS_Rew_Int + 
         Block*BAS_Rew_Reac + 
         Block*BAS_Goal_Drive + 
         Block*BAS_Impulsiv, data=Data_reg)
summary(m2)
car::Anova(m2,type=3)

m2_1<-lm(RT_mean ~ 
           Block + BAS_Rew_Int + 
           BAS_Rew_Reac + 
           BAS_Goal_Drive + 
           BAS_Impulsiv, data=Data_reg)
car::Anova(m2_1,type=3)
visreg(m2_1)

m3<-lm(RT_mean ~ 
         Block*PE +
         Block*AC +
         Block*SP, data=Data_reg)
summary(m3)
car::Anova(m3, type=3)

m3_1<-lm(RT_mean ~ 
           Block  +
           PE + AC + SP, data=Data_reg)
car::Anova(m3_1, type=3)
visreg(m3_1)

m4<-lmer(RT_mean~Block + (1|VP), data = Data_reg, REML=F)
summary(m4)
anova(m4)
emmeans(m4, pairwise ~ Block, adjust='Bonferroni')

visreg(m4)

#--------Regression: IGT-Score by RT and Personality--------------

m1<-lm(IGT_Score ~ 
         RT_mean*BAS_Score + 
         RT_mean*BIS + 
         RT_mean*FFFS + 
         RT_mean*MAE_Score, data=Data_reg)
summary(m1)
car::Anova(m1,type=3)


m1_1<-lm(IGT_Score ~ RT_mean + BAS_Score + BIS + 
           + FFFS + MAE_Score,
         data=Data_reg)
summary(m_1_1)
car::Anova(m1_1,type=3)


m2<-lm(IGT_Score ~ 
         RT_mean*BAS_Rew_Int +
         RT_mean*BAS_Rew_Reac +
         RT_mean*BAS_Goal_Drive +
         RT_mean*BAS_Impulsiv, data=Data_reg)
summary(m2)
car::Anova(m2,type=3)

m2_2<-lm(IGT_Score ~ RT_mean +
           BAS_Rew_Int +
           BAS_Rew_Reac +
           BAS_Goal_Drive +
           BAS_Impulsiv, data=Data_reg)
car::Anova(m2_2,type=3)


m3<-lm(IGT_Score ~   
         RT_mean*PE + 
         RT_mean*AC + 
         RT_mean*SP, 
       data=Data_reg)
summary(m3)
car::Anova(m3,type=3)

m3_1<-lm(IGT_Score ~ RT_mean + PE + AC + SP, 
         data=Data_reg)

car::Anova(m3_1,type=3)

#--------HLM: IGT-Score by Personality and Block-----

m1<-lmer(IGT_Score ~ 
           Block*BAS_Score + 
           Block*BIS + 
           Block*FFFS + 
           Block*MAE_Score
         + (1|VP), data=Data_reg, REML = F)
anova(m1)

visreg(m1, xvar = 'MAE_Score', by='Block', line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion MAE und Block")

visreg(m1, xvar = 'BIS', by='Block', line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion BIS und Block")


m1_1<-lmer(IGT_Score ~ Block + BAS_Score + BIS + 
             + FFFS + MAE_Score + (1|VP),
           data=Data_reg)
anova(m1_1)


m2<-lmer(IGT_Score ~ 
           Block*BAS_Rew_Int +
           Block*BAS_Rew_Reac +
           Block*BAS_Goal_Drive +
           Block*BAS_Impulsiv + (1|VP), data=Data_reg, REML=F)
anova(m2)

visreg(m2, xvar = 'BAS_Goal_Drive', by='Block',line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion Goal-Drive und Block")

visreg(m2, xvar = 'BAS_Rew_Int', by='Block',line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion Reward Interest und Block")

visreg(m2,line=list(col='#2D718EFF'), main="Reward Interest")

m2_1<-lmer(IGT_Score ~ Block +
             BAS_Rew_Int +
             BAS_Rew_Reac +
             BAS_Goal_Drive +
             BAS_Impulsiv + (1|VP), data=Data_reg)
anova(m2_1)


m3<-lmer(IGT_Score ~   
           Block*PE + 
           Block*AC + 
           Block*SP+
           (1|VP), data=Data_reg, REML=F)
anova(m3)

visreg(m3, xvar = 'SP', by='Block',line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion Social Potency und Block")


m3_1<-lmer(IGT_Score ~ Block + PE + AC +  (1| VP), 
           data=Data_reg)
anova(m3_1)

#--------HLM: Payoff by Personality and Block-----

m1<-lmer(Payoff~Block*BAS_Score+
           Block*BIS+
           Block*FFFS+
           Block*MAE_Score+ (1|VP), data=Data_reg, REML = F)
anova(m1)
emmeans(m1, pairwise~Block|BIS, adjust='Bonferroni')
emmeans(m1, pairwise~Block|MAE_Score, adjust='Bonferroni')

visreg(m1, xvar = 'MAE_Score', by='Block',line=list(col='#20A386FF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion MAE und Block")

visreg(m1, xvar = 'BIS', by='Block',line=list(col='#20A386FF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion BIS und Block")


m1_1<-lmer(Payoff~Block+BAS_Score+BIS+FFFS+MAE_Score
           +(1|VP), data=Data_reg)
anova(m1_1)

m2<-lmer(Payoff~Block*BAS_Rew_Int+ 
           Block*BAS_Rew_Reac+ 
           Block*BAS_Goal_Drive+
           Block*BAS_Impulsiv+ (1|VP), data=Data_reg, REML=F)
anova(m2)
visreg(m2)

visreg(m2,line=list(col='#20A386FF'),main="Goal-Drive")

m2_1<-lmer(Payoff~Block+BAS_Rew_Int+BAS_Rew_Reac+
             BAS_Goal_Drive+BAS_Impulsiv + 
             (1|VP), data=Data_reg)
anova(m2_1)

m3<-lmer(Payoff~Block*PE+
           Block*AC+
           Block*SP+(1|VP), data=Data_reg, REML=F)
anova(m3)

visreg(m3, xvar = 'SP', by='Block',line=list(col='#20A386FF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion Social Potency und Block")

m3_1<-lmer(Payoff~Block+PE+AC+SP+
             (1|VP), data=Data_reg)
anova(m3_1)

#--------HLM: RT_mean by Personality and Block-------------------------------

m1<-lmer(RT_mean ~ 
           Block*BAS_Score +
           Block*BIS + 
           Block*FFFS +
           Block*MAE_Score + 
           (1|VP), data=Data_reg, REML=F)
anova(m1)

m1_1<-lmer(RT_mean ~ 
             Block + BAS_Score +
             BIS + 
             FFFS +
             MAE_Score + (1|VP), data=Data_reg)
anova(m1_1)

m2<-lmer(RT_mean ~ 
           Block*BAS_Rew_Int + 
           Block*BAS_Rew_Reac + 
           Block*BAS_Goal_Drive + 
           Block*BAS_Impulsiv + (1|VP), data=Data_reg, REML=F)
anova(m2)

m2_1<-lmer(RT_mean ~ 
             Block + BAS_Rew_Int + 
             BAS_Rew_Reac + 
             BAS_Goal_Drive + 
             BAS_Impulsiv + (1|VP), data=Data_reg)
anova(m2_1)


m3<-lmer(RT_mean ~ 
           Block*PE +
           Block*AC +
           Block*SP + (1|VP), data=Data_reg, REML=F)
anova(m3)

m3_1<-lmer(RT_mean ~ 
             Block  +
             PE + AC + SP + (1|VP), data=Data_reg)
anova(m3_1)

#--------HLM with intercept and slopes for IGT-Score and RT----------------
m1<-lmer(IGT_Score ~ 
           Block*BAS_Score + 
           Block*BIS + 
           Block*FFFS + 
           Block*MAE_Score
         + (1+Card|VP), data=Data_reg_all)
anova(m1)


m1_1<-lmer(IGT_Score ~ Block + BAS_Score + BIS + 
             + FFFS + MAE_Score + (1+Card|VP),
           data=Data_reg_all)
anova(m1_1)



m2<-lmer(IGT_Score ~ 
           Block*BAS_Rew_Int +
           Block*BAS_Rew_Reac +
           Block*BAS_Goal_Drive +
           Block*BAS_Impulsiv + 
           (1+Card|VP), data=Data_reg_all)
anova(m2)

m2_1<-lmer(IGT_Score ~ Block +
             BAS_Rew_Int +
             BAS_Rew_Reac +
             BAS_Goal_Drive +
             BAS_Impulsiv + (1+Card|VP), data=Data_reg_all)
anova(m2_1)


m3<-lmer(IGT_Score ~   
           Block*PE + 
           Block*AC + 
           Block*SP+
           (1+Card|VP), data=Data_reg_all)
anova(m3)

m3_1<-lmer(IGT_Score ~ Block + PE + AC +  (1+Card| VP), 
           data=Data_reg_all)
anova(m3_1)



m1<-lmer(RT_mean ~ 
           Block*BAS_Score +
           Block*BIS + 
           Block*FFFS +
           Block*MAE_Score + 
           (1+Card|VP), data=Data_reg_all)
anova(m1)

m1_1<-lmer(RT_mean ~ 
             Block + BAS_Score +
             BIS + 
             FFFS +
             MAE_Score + (1+Card|VP), data=Data_reg_all)
anova(m1_1)

m2<-lmer(RT_mean ~ 
           Block*BAS_Rew_Int + 
           Block*BAS_Rew_Reac + 
           Block*BAS_Goal_Drive + 
           Block*BAS_Impulsiv + (1|VP), data=Data_reg_all)
anova(m2)

m2_1<-lmer(RT_mean ~ 
             Block + BAS_Rew_Int + 
             BAS_Rew_Reac + 
             BAS_Goal_Drive + 
             BAS_Impulsiv + (1+Card|VP), data=Data_reg_all)
anova(m2_1)


m3<-lmer(RT_mean ~ 
           Block*PE +
           Block*AC +
           Block*SP + (1+Card|VP), data=Data_reg_all)
anova(m3)

m3_1<-lmer(RT_mean ~ 
             Block  +
             PE + AC + SP + (1+Card|VP), data=Data_reg_all)
anova(m3_1)

AIC(m1)

#--------RT after losses-------------------------

#RT mean
m1<-lmer(RT_mean ~  Cond * Card + Card * Block + Cond * Block+ 
           (1|VP), data = Data_loss_win, REML=F)
anova(m1)
car::qqPlot(resid(m1))

emmeans(m1, pairwise ~ Block | Cond, adjust='Bonferroni')
emmip(m1, ~ Cond | Block, CIs=T)
emmip(m1, ~ Card | Block, CIs=T)
emmip(m1, ~ Card| Cond, CIs=T)
emmip(m1, ~ Card | Block | Cond, CIs=T)

m1_1<-lmer(RT_mean ~  Cond  + Card +  Block+
             (1|VP), data = Data_loss_win, REML=F)
anova(m1_1)
car::qqPlot(resid(m1_1))

emmeans(m1_1, pairwise ~ Block | Cond, adjust='fdr')
emmeans(m1_1, pairwise ~ Card | Cond, adjust='fdr')
emmeans(m1_1, pairwise ~ Card | Block, adjust='fdr')
emmeans(m1_1, pairwise ~ Card, adjust='fdr')
emmeans(m1_1, pairwise ~ Block, adjust='fdr')
emmeans(m1_1, pairwise ~ Cond, adjust='fdr')
emmip(m1_1, ~ Cond | Block, CIs=T)
emmip(m1_1, ~ Card | Block, CIs=T)
emmip(m1_1, ~ Card| Cond, CIs=T)
emmip(m1_1, ~ Card | Block | Cond, CIs=T)

m1<-lmer(RT_mean ~ Cond*Block +(1|VP), data = Data_loss_win, REML=F)
anova(m1)
car::qqPlot(resid(m1))

emmeans(m1, pairwise ~ Block | Cond, adjust = 'fdr')
emmip(m1, ~ Block | Cond, CIs=T)


# HLM separate

Data_RT_loss <- Data_RT_loss %>% 
  dplyr::group_by(VP, Block, Card, Cond) %>% 
  dplyr::summarise(RT_mean=mean(RT))

m1_1<-lmer(RT_mean~Block + (1 |VP), data=Data_RT_loss, REML=F)
anova(m1_1)

emmeans(m1_1, pairwise ~ Block, adjust="fdr")
emmip(m1_1, ~ Block, CIs=T)


Data_RT_win <- Data_RT_win %>% 
  dplyr::group_by(VP, Block, Card, Cond) %>% 
  dplyr::summarise(RT_mean=mean(RT))

m2_1<-lmer(RT_mean~Block + (1|VP), data=Data_RT_win, REML=F)
anova(m2_1)

emmeans(m2_1, pairwise ~ Block, adjust= "fdr")
emmip(m2_1, ~ Block, CIs=T)

#--------RT Analysis for log transformed Data-----

#Regression
m1<-lm(RT_log ~ 
         Block*BAS_Score +
         Block*BIS + 
         Block*FFFS +
         Block*MAE_Score, data=Data_reg)
summary(m1)
car::Anova(m1,type=3)

m1_1<-lm(RT_log ~ 
           Block + BAS_Score +
           BIS + 
           FFFS +
           MAE_Score, data=Data_reg)
car::Anova(m1_1,type=3)


m2<-lm(RT_log ~ 
         Block*BAS_Rew_Int + 
         Block*BAS_Rew_Reac + 
         Block*BAS_Goal_Drive + 
         Block*BAS_Impulsiv, data=Data_reg)
summary(m2)
car::Anova(m2,type=3)

m2_1<-lm(RT_log ~ 
           Block + BAS_Rew_Int + 
           BAS_Rew_Reac + 
           BAS_Goal_Drive + 
           BAS_Impulsiv, data=Data_reg)
car::Anova(m2_1,type=3)
visreg(m2_1)

m3<-lm(RT_log ~ 
         Block*PE +
         Block*AC +
         Block*SP, data=Data_reg)
summary(m3)
car::Anova(m3, type=3)

m3_1<-lm(RT_log ~ 
           Block  +
           PE + AC + SP, data=Data_reg)
car::Anova(m3_1, type=3)



m4<-lmer(RT_log~Block + (1|VP), data = Data_reg, REML = F)
summary(m4)
anova(m4)
emmeans(m4, pairwise ~ Block, adjust='Bonferroni')

visreg(m4, xvar = 'RT_log', by='Block',line=list(col='#481568FF'),
       gg=T) + theme_bw()+ ggtitle("Reaktionszeit")+ theme(axis.title.x=element_blank(),
                                                           axis.text.x=element_blank(),
                                                           axis.ticks.x=element_blank())

#HLM with Intercept

m1<-lmer(RT_log ~ 
           Block*BAS_Score +
           Block*BIS + 
           Block*FFFS +
           Block*MAE_Score + (1|VP), data=Data_reg, REML = F)
anova(m1)

visreg(m1)
visreg(m1,line=list(col='#481568FF'),main="Block")

m1_1<-lmer(RT_log ~ 
             Block + BAS_Score +
             BIS + 
             FFFS +
             MAE_Score + (1|VP), data=Data_reg, REML = F)
anova(m1_1)

m2<-lmer(RT_log ~ 
           Block*BAS_Rew_Int + 
           Block*BAS_Rew_Reac + 
           Block*BAS_Goal_Drive + 
           Block*BAS_Impulsiv + (1|VP), data=Data_reg, REML = F)
anova(m2)

visreg(m2, xvar = 'BAS_Rew_Int', by='Block',line=list(col='#481568FF'),
       gg=T) + theme_bw()+ ggtitle("Interaktion Reward Interest und Block")

m2_1<-lmer(RT_log ~ 
             Block + BAS_Rew_Int + 
             BAS_Rew_Reac + 
             BAS_Goal_Drive + 
             BAS_Impulsiv + (1|VP), data=Data_reg, REML = F)
anova(m2_1)


m3<-lmer(RT_log ~ 
           Block*PE +
           Block*AC +
           Block*SP + (1|VP), data=Data_reg, REML = F)
anova(m3)


m3_1<-lmer(RT_log ~ 
             Block  +
             PE + AC + SP + (1|VP), data=Data_reg, REML=F)
anova(m3_1)

## HLM with Intercept and Slopes

m1<-lmer(RT_log ~ 
           Block*BAS_Score +
           Block*BIS + 
           Block*FFFS +
           Block*MAE_Score + 
           (1+Card|VP), data=Data_reg_all)
anova(m1)

m1_1<-lmer(RT_log ~ 
             Block + BAS_Score +
             BIS + 
             FFFS +
             MAE_Score + (1+Card|VP), data=Data_reg_all)
anova(m1_1)

m2<-lmer(RT_log ~ 
           Block*BAS_Rew_Int + 
           Block*BAS_Rew_Reac + 
           Block*BAS_Goal_Drive + 
           Block*BAS_Impulsiv + (1|VP), data=Data_reg_all)
anova(m2)

m2_1<-lmer(RT_log ~ 
             Block + BAS_Rew_Int + 
             BAS_Rew_Reac + 
             BAS_Goal_Drive + 
             BAS_Impulsiv + (1+Card|VP), data=Data_reg_all)
anova(m2_1)


m3<-lmer(RT_log ~ 
           Block*PE +
           Block*AC +
           Block*SP + (1+Card|VP), data=Data_reg_all)
anova(m3)

m3_1<-lmer(RT_log ~ 
             Block  +
             PE + AC + SP + (1+Card|VP), data=Data_reg_all)
anova(m3_1)

## RT after losses

Data_loss_win$RT_log = log(Data_loss_win$RT_mean)


m1<-lmer(RT_log ~ Cond*Block 
         +(1|VP), data = Data_loss_win, REML=F)
anova(m1)

emmeans(m1, pairwise ~ Block | Cond, adjust = 'fdr')

m1<-lmer(RT_log ~  Cond * Card + Card * Block + Cond * Block+ 
           (1|VP), data = Data_loss_win, REML=F)
anova(m1)

emmeans(m1, pairwise ~ Block | Cond, adjust='Bonferroni')
emmeans(m1, pairwise ~ Cond | Block, adjust='Bonferroni')
emmip(m1,~Cond|Block, CIs=T, engine ="ggplot",line=list(col='#481568FF')) + theme_bw() 
#ändert Farbe nicht, aber Code läuft
emmip(m1, ~Card|Cond|Block, CIs=T)+ theme_bw()
summary(m1)

#--------Poweranalysis-------------------

#IGT Score by Block in Data_reg
pwr.f2.test(u = 2, v = NULL , f2 = 0.02 , sig.level = 0.05, power = .8)

etasq(m1, anova=TRUE, partial =T) 

#IGT Score by Personality and Block in Data_reg
pwr.f2.test(u = 2, v =75 , f2 = 0.018, sig.level = 0.05)


#--------Visualisierung-------------------------------
stargazer(m1, out="m1.htm")

stargazer(m2, m3, type="html", intercept.bottom = FALSE, single.row = T,
          dep.var.labels = c("IGT_Score"), 
          covariate.labels = c("A"), out= "m2u3.htm")

visreg(m5, "IGT_Score", by= "Block")

visreg(m3, "IGT_Score", by= "MAE_Score", main= "MAE", ylab="IGT-Score", xlab="Extraversion")

visreg(m3, "IGT_Score",by="MAE_Score", cond=list("Block"="1"), main="Block1")

visreg(m3)
visreg(m2)
visreg(m1)


#--------Regression tables------------------

#IGT-Score
apa.reg.table(m1_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table11.doc", table.number = 1)
apa.reg.table(m2_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table12.doc", table.number = 2)
apa.reg.table(m3_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table13.doc", table.number = 3)
apa.aov.table(m1_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table11a.doc", table.number = 1)
apa.aov.table(m2_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table12a.doc", table.number = 2)
apa.aov.table(m3_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table13a.doc", table.number = 3)

#Payoff
apa.reg.table(m1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table21.doc", table.number = 1)
apa.reg.table(m2_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table22.doc", table.number = 2)
apa.reg.table(m3, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table23.doc", table.number = 3)
apa.aov.table(m1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table21a.doc", table.number = 1)
apa.aov.table(m2_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table22a.doc", table.number = 2)
apa.aov.table(m3, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table23a.doc", table.number = 3)

#RT_mean
apa.reg.table(m1_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table31.doc", table.number = 1)
apa.reg.table(m2_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table32.doc", table.number = 2)
apa.reg.table(m3_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table33.doc", table.number = 3)
apa.aov.table(m1_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table31a.doc", table.number = 1)
apa.aov.table(m2_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table32a.doc", table.number = 2)
apa.aov.table(m3_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table33a.doc", table.number = 3)

#--------Partial Correlation----------------

cor(Data_reg$IGT_Score, Data_reg$BAS_Score)
cor.test(Data_reg$IGT_Score, Data_reg$BAS_Score)
pcor.test(Data_reg$IGT_Score,Data_reg$BAS_Score,Data_reg[c("RT_mean")])

?apaTables






###OLD CODE-----------------------------------------------------------------
#--------Anova RT by Block-----------------------------------------------

#---- M1: RT by Block
Data_full$Block <- as.factor(Data_full$Block)
m1<-lm(RT~Block,data=Data_full)                            
summary(m1)
anova(m1)

# Post-hoc Test
mult1<- glht(m1,mcp(Block="Tukey"))             
summary(mult1)

summarise(group_by(Data_card,Block),mean(RT))   
visreg(m1)
#--------Anova Frequency Cards by Block and Card--------

#M3: N by Block and Card
m3<-lm(N~Block*Card, data=Data_sum)
anova(m3)
plot(m3)

# plot effects for M3
m3f<-allEffects((m3))
plot(m3f)

#--------Regression Analysis -----------------

#Total score by personality/RT in large data frame
#merge into large data set
Data_full2 <- merge (Data_score_all, Data_full, by.x = 'VP', by.y = 'VP')

m1<-lm(IGT_Score_all~Block*BAS_Score*BIS*MAE_Score, data=Data_full2)
summary(m4)

m2<-lm(IGT_Score_all~RT, data=Data_full2)
summary(m5)

m3<-lm(IGT_Score_all~BAS_Score, data=Data_full2)
summary(m6)

# Total Score in small data frame
m1<-lm(IGT_Score~Block,data=Data_score)                            
summary(m1)
anova(m1)

m2<-lm(IGT_Score_all~MAE_Score, data=Data_pers_score_all)
summary(m2)

m3<-lm(IGT_Score_all~BAS_Score, data=Data_pers_score_all)
summary(m3)


#Block 3 Score in small data frame
m1<-lm(IGT_Score~BIS,data=Data_pers_score3)                            
summary(m1)

m2<-lm(IGT_Score~MAE_Score, data=Data_pers_score3)
summary(m2)

m3<-lm(IGT_Score~BAS_Score, data=Data_pers_score3)
summary(m3)

#Split by Blocks in final data frame
Data_reg1<-dplyr::filter(Data_reg, Block==1)
Data_reg2<-dplyr::filter(Data_reg, Block==2)
Data_reg3<-dplyr::filter(Data_reg, Block==3)

#Score by Personality in Block1
m1<-lm(IGT_Score~BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg1)
summary(m1)
anova(m1)

m2<-lm(IGT_Score~BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg1)
summary(m2)
anova(m2)

m3<-lm(IGT_Score~MAE_Score*PE*AC*SP, data=Data_reg1)
summary(m3)
anova(m3)

#Score by Personality in Block2
m1<-lm(IGT_Score~BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg2)
summary(m1)
anova(m1)

m2<-lm(IGT_Score~BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg2)
summary(m2)
anova(m2)

m3<-lm(IGT_Score~MAE_Score*PE*AC*SP, data=Data_reg2)
summary(m3)
anova(m3)

#Score by Personality in Block3
m1<-lm(IGT_Score~BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg3)
summary(m1)
anova(m1)

m2<-lm(IGT_Score~BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg3)
summary(m2)
anova(m2)

m3<-lm(IGT_Score~MAE_Score*PE*AC*SP, data=Data_reg3)
summary(m3)
anova(m3)