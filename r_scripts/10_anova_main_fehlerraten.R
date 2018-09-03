##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                           ANOVA FEHLERRATEN MAIN

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

# Benötigte Variablen als Faktoren aus FAs_sum_All mit Fehlerraten für jede VP einzeln und mit allen Phasen (Baseline + Blocks)
FAs_sum_All <- rbind(FAs_sum, FAs_B_sum)
FAs_sum_All$Trialtype <- as.factor(FAs_sum_All$Trialtype)
FAs_sum_All$Phase <- as.factor(FAs_sum_All$Phase)
FAs_sum_All$Rew <- as.factor(FAs_sum_All$Rew)

# Effektkodierung: Alle Fehlerraten mit Gesamtdurchschnitt vergleichen (sonst default = Dummy-Kodierung)
contrasts(FAs_sum_All$Trialtype) <- contr.sum(4); contrasts(FAs_sum_All$Trialtype)
contrasts(FAs_sum_All$Phase) <- contr.sum(3); contrasts(FAs_sum_All$Phase)

# Modell direkt auf Daten ohne summary
FR_mod_log <- lm(log(Fehlerrate) ~ Trialtype*Rew*Phase, data = FAs_sum_All)
FR_mod_no_log <- lm(Fehlerrate ~ Trialtype*Rew*Phase, data = FAs_sum_All)   # ausprobieren, ob Rechnung mit oder ohne log besser
hist(FAs_sum_All$Fehlerrate)
hist(log(FAs_sum_All$Fehlerrate))     # besser mit log, weil dann normalverteilt und s.u.
sjstats::r2(FR_mod_log)
sjstats::r2(FR_mod_no_log)   # Modelfit für log und nicht log Modell -> log besser
FR_a_mod<- car::Anova(FR_mod_log, type=3, singular.ok = TRUE); FR_a_mod # Modell für ANOVA, type=3 -> Effekte unabhängig geprüft
eta_sq(FR_a_mod, partial=F)  # Effektgröße Eta-Quadrat (hat Konventionen)
car::qqPlot(resid(FR_mod_log))

# Trialtypes über Phasen hinweg
emmeans::emmeans(FR_mod_log, pairwise ~ Trialtype | Rew , adjust = "fdr") # paarweise Vergleiche
emmeans::emmip(FR_mod_log, Rew ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Trialtypen",
       title = "Modellvorhersage Fehlerraten Haupttestung") + 
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 12),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  geom_line(position = position_dodge(.1), size = 1) +
  geom_point(position = position_dodge(.1), size = 3)
## bei Rew=1 gibt es keine Fehler bei BY, deshalb ist im Graph bei Trialtype 4 nichts verzeichnet
## auf daten basiert eine generalisierende Vorhersage und DIE wird dann auf sig getestet

# Phasen über Trialtypes hinweg
emmeans::emmeans(FR_mod_log, pairwise ~ Phase | Rew , adjust = "fdr") # paarweise Vergleiche
emmeans::emmip(FR_mod_log, Rew ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Phasen",
       title = "Modellvorhersage Fehlerraten Haupttestung") + 
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
pdf("./Desktop/emmip_FR_Main_TP.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(FR_mod_log, Rew ~ Trialtype, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Trialtypen",
       title = "Modellvorhersage Fehlerraten Haupttestung") + 
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
pdf("./Desktop/emmip_FR_Main_PT.pdf", width = 10 , height = 10) 
# 2. Create a plot
emmeans::emmip(FR_mod_log, Rew ~ Phase, type = "response", CIs = T) + # Vorhersage erwarteter FR nach Modell
  theme_bw() + 
  labs(y = "Vorhergesagte Fehlerraten",
       x = "Phasen",
       title = "Modellvorhersage Fehlerraten Haupttestung") + 
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
