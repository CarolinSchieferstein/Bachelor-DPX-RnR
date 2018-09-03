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
car::qqPlot(resid(RT_mod_no_log))

# Trialtypes über Phasen hinweg
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
  
# Phasen über Trialtypes hinweg
emmeans::emmeans(RT_mod_no_log, pairwise ~ Phase | Rew , adjust = "fdr") # paarweise Vergleiche aller Stufen
emmeans::emmip(RT_mod_no_log, Rew ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Phasen",
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

##### Saving Plots #####

## T über P
# Open a pdf file
pdf("./Desktop/emmip_RT_Main_TP.pdf", width = 10 , height = 10) 
# 2. Create a plot
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
# Close the pdf file
dev.off()

## P über T
# Open a pdf file
pdf("./Desktop/emmip_RT_Main_PT.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(RT_mod_no_log, Rew ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter RT nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Reaktionszeiten",
       x = "Phasen",
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
# Close the pdf file
dev.off()
