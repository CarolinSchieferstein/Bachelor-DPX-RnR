##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                          ANOVA REAKTIONSZEITEN PILOTS

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
Hits1 <- select(Hits1, ID, Trialtype, Rew, Phase, perm, Block, Trialnr, RT, Reactiontype)
Hits2 <- select(Hits2, ID, Trialtype, Rew, Phase, perm, Block, Trialnr, RT, Reactiontype)

Hits1b <- select(Hits1b, ID, Trialtype, Rew, Phase, perm, Block, Trialnr, RT, Reactiontype)
Hits2b <- select(Hits2b, ID, Trialtype, Rew, Phase, perm, Block, Trialnr, RT, Reactiontype)

Hits1_All <- rbind(Hits1, Hits1b)
Hits2_All <- rbind(Hits2, Hits2b)

Hits_sum1_All <- Hits1_All %>% dplyr::group_by(ID, Trialtype, Rew, Phase) %>%
  dplyr::summarise(m_RT = mean(RT),
                   se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
                   n = sum(!is.na(RT)))

Hits_sum2_All <- Hits2_All %>% dplyr::group_by(ID, Trialtype, Rew, Phase) %>%
  dplyr::summarise(m_RT = mean(RT),
                   se_RT=sd(RT)/sqrt(sum(!is.na(RT))),
                   n = sum(!is.na(RT)))

Hits_sum1_All$ID <- as.factor(Hits_sum1_All$ID)
Hits_sum1_All$Rew <- as.factor(Hits_sum1_All$Rew)
Hits_sum1_All$Phase <- as.factor(Hits_sum1_All$Phase)
Hits_sum1_All$Trialtype <- as.factor(Hits_sum1_All$Trialtype)

Hits_sum2_All$ID <- as.factor(Hits_sum2_All$ID)
Hits_sum2_All$Rew <- as.factor(Hits_sum2_All$Rew)
Hits_sum2_All$Phase <- as.factor(Hits_sum2_All$Phase)
Hits_sum2_All$Trialtype <- as.factor(Hits_sum2_All$Trialtype)

# Effektkodierung: Alle Fehlerraten mit Gesamtdurchschnitt vergleichen (sonst default = Dummy-Kodierung)
contrasts(Hits_sum1_All$Trialtype) <- contr.sum(4); contrasts(Hits_sum1_All$Trialtype)
contrasts(Hits_sum1_All$Phase) <- contr.sum(3); contrasts(Hits_sum1_All$Phase)

contrasts(Hits_sum2_All$Trialtype) <- contr.sum(4); contrasts(Hits_sum2_All$Trialtype)
contrasts(Hits_sum2_All$Phase) <- contr.sum(3); contrasts(Hits_sum2_All$Phase)

##### -- FÜR PILOT 1 -- Modell direkt auf Daten ohne summary #####
####
RT_mod_log_P1 <- lm(log(m_RT) ~ Trialtype*Phase, data = Hits_sum1_All)
RT_mod_no_log_P1 <- lm(m_RT ~ Trialtype*Phase, data = Hits_sum1_All)   # ausprobieren, ob Rechnung mit oder ohne log besser
hist(Hits_sum1_All$m_RT)
hist(log(Hits_sum1_All$m_RT))     # besser ohne log, weil auch normalverteilt und s.u.
sjstats::r2(RT_mod_log_P1)
sjstats::r2(RT_mod_no_log_P1)   # Modelfit für log und ohne log Modell -> beides ähnlich aber NV für log besser!
RT_a_mod_P1<- car::Anova(RT_mod_log_P1, type=3); RT_a_mod_P1 # Modell für ANOVA, type=3 -> Effekte unabhängig voneinander geprüft
eta_sq(RT_a_mod_P1, partial=F)  # Effektgröße Eta-Quadrat (hat Konventionen)
car::qqPlot(resid(RT_mod_log_P1))

# Trialtypes über Phasen hinweg
emmeans::emmeans(RT_mod_log_P1, pairwise ~ Trialtype , adjust = "fdr") # paarweise Vergleiche aller Stufen
emmeans::emmip(RT_mod_log_P1, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Trialtypen",
       title = "Modellvorhersage Reaktionszeiten Pilot 1") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)

# Phasen über Trialtypes hinweg
emmeans::emmeans(RT_mod_log_P1, pairwise ~ Phase , adjust = "fdr") # paarweise Vergleiche aller Stufen
emmeans::emmip(RT_mod_log_P1, ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Phasen",
       title = "Modellvorhersage Reaktionszeiten Pilot 1") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)

##### Saving Plots #####

## T über P
# Open a pdf file
pdf("./Desktop/emmip_RT_P1_TP.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(RT_mod_log_P1, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Trialtypen",
       title = "Modellvorhersage Reaktionszeiten Pilot 1") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)
# Close the pdf file
dev.off()

## P über T
# Open a pdf file
pdf("./Desktop/emmip_RT_P1_PT.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(RT_mod_log_P1, ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Phasen",
       title = "Modellvorhersage Reaktionszeiten Pilot 1") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)
# Close the pdf file
dev.off()



##### -- FÜR PILOT 2 -- Modell direkt auf Daten ohne summary #####
####
RT_mod_log_P2 <- lm(log(m_RT) ~ Trialtype*Phase, data = Hits_sum2_All)
RT_mod_no_log_P2 <- lm(m_RT ~ Trialtype*Phase, data = Hits_sum2_All)   # ausprobieren, ob Rechnung mit oder ohne log besser
hist(Hits_sum2_All$m_RT)
hist(log(Hits_sum2_All$m_RT))     # besser ohne log, weil auch normalverteilt und s.u.
sjstats::r2(RT_mod_log_P2)
sjstats::r2(RT_mod_no_log_P2)   # Modelfit für log und ohne log Modell -> ohne log besser
RT_a_mod_P2 <- car::Anova(RT_mod_no_log_P2, type=3); RT_a_mod_P2 # Modell für ANOVA, type=3 -> Effekte unabhängig voneinander geprüft
eta_sq(RT_a_mod_P2, partial=F)  # Effektgröße Eta-Quadrat (hat Konventionen)
car::qqPlot(resid(RT_mod_log_P2))

# Trialtypes über Phasen hinweg
emmeans::emmeans(RT_mod_no_log_P2, pairwise ~ Trialtype , adjust = "fdr") # paarweise Vergleiche aller Stufen
emmeans::emmip(RT_mod_no_log_P2, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Trialtypen",
       title = "Modellvorhersage Reaktionszeiten Pilot 2") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)

# Phasen über Trialtypes hinweg
emmeans::emmeans(RT_mod_no_log_P2, pairwise ~ Phase , adjust = "fdr") # paarweise Vergleiche aller Stufen
emmeans::emmip(RT_mod_no_log_P2, ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Phasen",
       title = "Modellvorhersage Reaktionszeiten Pilot 2") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)

##### Saving Plots #####

## T über P
# Open a pdf file
pdf("./Desktop/emmip_RT_P2_TP.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(RT_mod_no_log_P2, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Trialtypen",
       title = "Modellvorhersage Reaktionszeiten Pilot 2") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)
# Close the pdf file
dev.off()

## P über T
# Open a pdf file
pdf("./Desktop/emmip_RT_P2_PT.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(RT_mod_no_log_P2, ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Phasen",
       title = "Modellvorhersage Reaktionszeiten Pilot 2") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)
# Close the pdf file
dev.off()


