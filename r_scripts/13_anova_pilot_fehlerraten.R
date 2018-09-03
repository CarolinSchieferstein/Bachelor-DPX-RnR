##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                           ANOVA FEHLERRATEN PILOTS

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

# Benötigte Variablen als Faktoren aus beiden Pilots (FAs_sum1_All und FAs_sum2_All)
# mit Fehlerraten für jede VP einzeln und mit allen Phasen (Baseline + Blocks)
FAs_sum1_All <- rbind(FAs_sum1, FAs_sum1b)
FAs_sum2_All <- rbind(FAs_sum2, FAs_sum2b)

FAs_sum1_All$Trialtype <- as.factor(FAs_sum1_All$Trialtype)
FAs_sum2_All$Trialtype <- as.factor(FAs_sum2_All$Trialtype)

FAs_sum1_All$Phase <- as.factor(FAs_sum1_All$Phase)
FAs_sum2_All$Phase <- as.factor(FAs_sum2_All$Phase)

FAs_sum1_All$Rew <- as.factor(FAs_sum1_All$Rew)
FAs_sum2_All$Rew <- as.factor(FAs_sum2_All$Rew)

# Effektkodierung: Alle Fehlerraten mit Gesamtdurchschnitt vergleichen (sonst default = Dummy-Kodierung)
contrasts(FAs_sum1_All$Trialtype) <- contr.sum(4); contrasts(FAs_sum1_All$Trialtype) # Pilot1
contrasts(FAs_sum1_All$Phase) <- contr.sum(3); contrasts(FAs_sum1_All$Phase)

contrasts(FAs_sum2_All$Trialtype) <- contr.sum(4); contrasts(FAs_sum2_All$Trialtype) # Pilot2
contrasts(FAs_sum2_All$Phase) <- contr.sum(3); contrasts(FAs_sum2_All$Phase)

##### -- FÜR PILOT 1 -- Modell direkt auf Daten ohne summary #####
FR_mod_log_P1 <- lm(log(Fehlerrate) ~ Trialtype*Phase, data = FAs_sum1_All) # Rew immer = 0 (verzögert)
FR_mod_no_log_P1 <- lm(Fehlerrate ~ Trialtype*Phase, data = FAs_sum1_All)   # ausprobieren: mit oder ohne log besser?
hist(FAs_sum1_All$Fehlerrate)
hist(log(FAs_sum1_All$Fehlerrate))     # besser mit log, weil dann normalverteilt und s.u.
sjstats::r2(FR_mod_log_P1)
sjstats::r2(FR_mod_no_log_P1)   # Modelfit für log und nicht log Modell -> log besser
FR_a_mod_P1<- car::Anova(FR_mod_log_P1, type=3, singular.ok = TRUE); FR_a_mod_P1 # Modell für ANOVA, type=3 -> Effekte unabhängig
eta_sq(FR_a_mod_P1, partial=F)  # Effektgröße Eta-Quadrat (hat Konventionen)
car::qqPlot(resid(FR_mod_log_P1))

# Trialtypes über Phasen hinweg
emmeans::emmeans(FR_mod_log_P1, pairwise ~ Trialtype , adjust = "fdr") # paarweise Vergleiche
emmeans::emmip(FR_mod_log_P1, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Trialtypen",
       title = "Modellvorhersage Fehlerraten Pilot 1") + 
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
emmeans::emmeans(FR_mod_log_P1, pairwise ~ Phase , adjust = "fdr") # paarweise Vergleiche
emmeans::emmip(FR_mod_log_P1, ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Phasen",
       title = "Modellvorhersage Fehlerraten Pilot 1") + 
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
pdf("./Desktop/emmip_FR_P1_TP.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(FR_mod_log_P1, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Trialtypen",
       title = "Modellvorhersage Fehlerraten Pilot 1") + 
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
pdf("./Desktop/emmip_FR_P1_PT.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(FR_mod_log_P1, ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Phasen",
       title = "Modellvorhersage Fehlerraten Pilot 1") + 
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

###### -- FÜR PILOT 2 -- Modell direkt auf Daten ohne summary #####
FR_mod_log_P2 <- lm(log(Fehlerrate) ~ Trialtype*Phase, data = FAs_sum2_All) # Rew immer = 0 (verzögert)
FR_mod_no_log_P2 <- lm(Fehlerrate ~ Trialtype*Phase, data = FAs_sum2_All)   # ausprobieren: mit oder ohne log besser?
hist(FAs_sum2_All$Fehlerrate)
hist(log(FAs_sum2_All$Fehlerrate))     # besser mit log, weil dann normalverteilt und s.u.
sjstats::r2(FR_mod_log_P2)
sjstats::r2(FR_mod_no_log_P2)   # Modelfit für log und nicht log Modell -> log besser
FR_a_mod_P2 <- car::Anova(FR_mod_log_P2, type=3, singular.ok = TRUE); FR_a_mod_P2 # Modell für ANOVA, type=3->Effekte unabhängig
eta_sq(FR_a_mod_P2, partial=F)  # Effektgröße Eta-Quadrat (hat Konventionen)
car::qqPlot(resid(FR_mod_log_P2))

# Trialtypes über Phasen hinweg
emmeans::emmeans(FR_mod_log_P2, pairwise ~ Trialtype , adjust = "fdr") # paarweise Vergleiche
emmeans::emmip(FR_mod_log_P2, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Trialtypen",
       title = "Modellvorhersage Fehlerraten Pilot 2") + 
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
emmeans::emmeans(FR_mod_log_P2, pairwise ~ Phase , adjust = "fdr") # paarweise Vergleiche
emmeans::emmip(FR_mod_log_P2, ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Phasen",
       title = "Modellvorhersage Fehlerraten Pilot 2") + 
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
pdf("./Desktop/emmip_FR_P2_TP.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(FR_mod_log_P2, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Trialtypen",
       title = "Modellvorhersage Fehlerraten Pilot 2") + 
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
pdf("./Desktop/emmip_FR_P2_PT.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(FR_mod_log_P2, ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Phasen",
       title = "Modellvorhersage Fehlerraten Pilot 2") + 
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
