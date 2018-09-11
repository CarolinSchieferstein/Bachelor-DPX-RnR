##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                          Compute Descriptive Statistics
#                                  Fragebogen

# Load necessary packages
require(plyr)
require(dplyr)

###### Get the data #####
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

##### COMPUTE DESCRIPTIVE STATISTICS FOR QUESTIONNAIRE----

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
##### Plot #####
# dafür Fragebogen_sum_scalemeans in Spalten statt Zeilen, dass eine Spalte jeweils BAS/BIS etc sagt und die zweite Spalte den Wert

Fragebogen_plot <- tidyr::gather(Fragebogen_sum_scalemeans)
Fragebogen_plot$key <- gsub(Fragebogen_plot$key,pattern = "m_", replacement = "")
Fragebogen_plot$key <-  as.factor(Fragebogen_plot$key)




## Reorder the levels:
Fragebogen_plot$key <- factor(Fragebogen_plot$key,levels(Fragebogen_plot$key)[c(2,9,4,7,1,3,6,5,8)])
print(levels(Fragebogen_plot$key))

require(ggplot2)
# install.packages("viridis")
require(viridis)
ggplot(Fragebogen_plot, aes(x = key, y = value, fill = key)) + 
  geom_line(position = position_dodge(.5)) +
  labs(y = "Skalenmittelwerte",
       x = "RST-PQ Skalen",
       title = "Mittelwerte der Skalen des RST-PQ") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 10),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  coord_cartesian(ylim = c(10:90)) +
  scale_y_log10(breaks = seq(10,90,10)) +
  geom_bar(position = position_dodge(.5), size = 3, stat = "identity") + 
  scale_fill_viridis(option = "D", discrete = TRUE)

# Save for later use
write.table(Fragebogen_plot, '~/Desktop/Bachelor/Daten/R_Frames/Fragebogen_plot.txt', row.names = F, sep = '\t')

##### Save Plot as PDF #####
# Open a pdf file
pdf("./Desktop/m_FB.pdf", width = 10 , height = 10) 
# 2. Create a plot
ggplot(Fragebogen_plot, aes(x = key, y = value, fill = key)) + 
  geom_line(position = position_dodge(.5)) +
  labs(y = "Skalenmittelwerte",
       x = "RST-PQ Skalen",
       title = "Mittelwerte der Skalen des RST-PQ") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 10),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  coord_cartesian(ylim = c(10:90)) +
  scale_y_log10(breaks = seq(10,90,10)) +
  geom_bar(position = position_dodge(.5), size = 3, stat = "identity") + 
  scale_fill_viridis(option = "D", discrete = TRUE)
# Close the pdf file
dev.off()

##### Balkendiagramm nach Rew getrennt ######
# ausgehend von Fragebogen_Skalen

Fragebogen_Skalen_rew0 <- filter(Fragebogen_Skalen, Rew == 0)
Fragebogen_Skalen_rew1 <- filter(Fragebogen_Skalen, Rew == 1)


Fragebogen_sum_scalemeans_rew0<- Fragebogen_Skalen_rew0 %>% dplyr::summarise(m_BI = mean(BI) ,
                                                                   m_ZAP = mean(ZAP) ,
                                                                   m_BR = mean(BR) ,
                                                                   m_Imp = mean(Imp) ,
                                                                   m_BAS = mean(BAS) ,
                                                                   m_FFFS = mean(FFFS) ,
                                                                   m_BIS = mean(BIS) ,
                                                                   m_Panik = mean(Panik) ,
                                                                   m_DK = mean(DK)
)

Fragebogen_sum_scalemeans_rew1<- Fragebogen_Skalen_rew1 %>% dplyr::summarise(m_BI = mean(BI) ,
                                                                             m_ZAP = mean(ZAP) ,
                                                                             m_BR = mean(BR) ,
                                                                             m_Imp = mean(Imp) ,
                                                                             m_BAS = mean(BAS) ,
                                                                             m_FFFS = mean(FFFS) ,
                                                                             m_BIS = mean(BIS) ,
                                                                             m_Panik = mean(Panik) ,
                                                                             m_DK = mean(DK)
)

##### Plot #####
# dafür Fragebogen_sum_scalemeans_rewX in Spalten statt Zeilen, dass eine Spalte jeweils BAS/BIS etc sagt und die zweite Spalte den Wert

Fragebogen_plot_rew0 <- tidyr::gather(Fragebogen_sum_scalemeans_rew0)
Fragebogen_plot_rew0$key <- gsub(Fragebogen_plot_rew0$key,pattern = "m_", replacement = "")
Fragebogen_plot_rew0$key <-  as.factor(Fragebogen_plot_rew0$key)

Fragebogen_plot_rew1 <- tidyr::gather(Fragebogen_sum_scalemeans_rew1)
Fragebogen_plot_rew1$key <- gsub(Fragebogen_plot_rew1$key,pattern = "m_", replacement = "")
Fragebogen_plot_rew1$key <-  as.factor(Fragebogen_plot_rew1$key)


## Reorder the levels:
Fragebogen_plot_rew0$key <- factor(Fragebogen_plot_rew0$key,levels(Fragebogen_plot_rew0$key)[c(2,9,4,7,1,3,6,5,8)])
print(levels(Fragebogen_plot_rew0$key))

Fragebogen_plot_rew1$key <- factor(Fragebogen_plot_rew1$key,levels(Fragebogen_plot_rew1$key)[c(2,9,4,7,1,3,6,5,8)])
print(levels(Fragebogen_plot_rew1$key))

require(ggplot2)
# install.packages("viridis")
require(viridis)
# Rew = 0
ggplot(Fragebogen_plot_rew0, aes(x = key, y = value, fill = key)) + 
  geom_line(position = position_dodge(.5)) +
  labs(y = "Skalenmittelwerte",
       x = "RST-PQ Skalen",
       title = "Mittelwerte der Skalen des RST-PQ für die Gruppe mit verzögerter Belohnung") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 10),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  coord_cartesian(ylim = c(10:90)) +
  scale_y_log10(breaks = seq(10,90,10)) +
  geom_bar(position = position_dodge(.5), size = 3, stat = "identity") + 
  scale_fill_viridis(option = "D", discrete = TRUE)

# Rew = 1
ggplot(Fragebogen_plot_rew1, aes(x = key, y = value, fill = key)) + 
  geom_line(position = position_dodge(.5)) +
  labs(y = "Skalenmittelwerte",
       x = "RST-PQ Skalen",
       title = "Mittelwerte der Skalen des RST-PQ für die Gruppe mit direkter Belohnung") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 10),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  coord_cartesian(ylim = c(10:90)) +
  scale_y_log10(breaks = seq(10,90,10)) +
  geom_bar(position = position_dodge(.5), size = 3, stat = "identity") + 
  scale_fill_viridis(option = "D", discrete = TRUE)

# Save for later use
write.table(Fragebogen_plot_rew0, '~/Desktop/Bachelor/Daten/R_Frames/Fragebogen_plot_rew0.txt', row.names = F, sep = '\t')
write.table(Fragebogen_plot_rew1, '~/Desktop/Bachelor/Daten/R_Frames/Fragebogen_plot_rew1.txt', row.names = F, sep = '\t')

###### Save Plots as PDF #####

# Open a pdf file
pdf("./Desktop/m_FB_rew0.pdf", width = 10 , height = 10) 
# 2. Create a plot
ggplot(Fragebogen_plot_rew0, aes(x = key, y = value, fill = key)) + 
  geom_line(position = position_dodge(.5)) +
  labs(y = "Skalenmittelwerte",
       x = "RST-PQ Skalen",
       title = "Mittelwerte der Skalen des RST-PQ für die Gruppe mit verzögerter Belohnung") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 10),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  coord_cartesian(ylim = c(10:90)) +
  scale_y_log10(breaks = seq(10,90,10)) +
  geom_bar(position = position_dodge(.5), size = 3, stat = "identity") + 
  scale_fill_viridis(option = "D", discrete = TRUE)
# Close the pdf file
dev.off()

# Open a pdf file
pdf("./Desktop/m_FB_rew1.pdf", width = 10 , height = 10) 
# 2. Create a plot
ggplot(Fragebogen_plot_rew1, aes(x = key, y = value, fill = key)) + 
  geom_line(position = position_dodge(.5)) +
  labs(y = "Skalenmittelwerte",
       x = "RST-PQ Skalen",
       title = "Mittelwerte der Skalen des RST-PQ für die Gruppe mit direkter Belohnung") +
  theme(strip.background = element_blank(), 
        strip.text= element_text(color= "black", size = 12),
        axis.text = element_text(color='black', size = 10),
        axis.title = element_text(color='black', size = 13),
        plot.title = element_text(hjust = .5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) + 
  coord_cartesian(ylim = c(10:90)) +
  scale_y_log10(breaks = seq(10,90,10)) +
  geom_bar(position = position_dodge(.5), size = 3, stat = "identity") + 
  scale_fill_viridis(option = "D", discrete = TRUE)
# Close the pdf file
dev.off()
