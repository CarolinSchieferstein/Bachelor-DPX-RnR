##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 July 2018 
#                           Read in Questionnaire data    

# Load nescessary packages
require(plyr)
require(dplyr)

fragebogen <- read.csv("/Users/carolin/Desktop/Bachelor/Daten/rdata_dpx_rnr_2018-02-21_18-30.csv")

fragebogen1 <- select(fragebogen, SD01:TIME_SUM)  ### relevante columns ausgewählt

names(fragebogen1) <- c("sex", "age", "Abschluss", "Beruf", "ID", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17",
                        "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", 
                        "42", "43", "44", "45", "46", "47", "48", "50", "51", "52", "53", "54", "55", "56", "57", "58", "60", "61", "62", "64", "65", "66", "68", "69",
                        "70", "71", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "Dauer1", "Dauer2", "Dauer3", "Dauer4", "Dauer5", "Dauergesamt")   ### columns umbenennen

fragebogen2 <- merge(fragebogen1,VP_Zuordnung, by = "ID") ### über VP-Zuordnung Rew und perm dazu