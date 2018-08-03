##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 August 2018 
#                          Read in Pilot Baseline data    

# Load nescessary packages
require(plyr)
require(dplyr)

# ----- 1) Read in the data ---------------------

## Verhaltensdaten
path1 <- c("Desktop/Bachelor/Daten/Pilotierung Runde 1 u 2/Logs_Pilot1") # <- location of files Pilot1
path2 <- c("Desktop/Bachelor/Daten/Pilotierung Runde 1 u 2/Logs_Pilot2") # <- location of files Pilot1

## Baselinedaten
paths1 <- dir(path = path1, full.names = T, pattern = "rawdata.txt$")
names(paths1) <- basename(paths1)
paths2 <- dir(path = path2, full.names = T, pattern = "rawdata.txt$")
names(paths2) <- basename(paths2)


# ----- 2) Create data frame containing all files and observations

# **  RUN CODE to get dataframe with behavioral data form each individual
# **  The code cuts the dataframe down to a tidy version containing relevant variables 
# **  with the right attributues.

# Read in files
All_Pilot1 <- plyr::ldply(paths1, read.table, sep =",", dec = ".", header=F)
rm(paths1)
All_Pilot2 <- plyr::ldply(paths2, read.table, sep =",", dec = ".", header=F)
rm(paths2)

# Select columns with data, not names

All_Pilot1_selected <- select(All_Pilot1, .id, V2, V4, V6, V8, V10)
All_Pilot2_selected <- select(All_Pilot2, .id, V2, V4, V6, V8, V10)

# Default names substitue with right names through vector of same length
names(All_Pilot1_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")
names(All_Pilot2_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")

# Get rid of suffix in ID column
All_Pilot1_selected$ID <- gsub(All_Pilot1_selected$ID, 
                           pattern = "rawdata.txt", 
                           replacement = "")
All_Pilot2_selected$ID <- gsub(All_Pilot2_selected$ID, 
                               pattern = "rawdata.txt", 
                               replacement = "")

# Get rid of blank space in Reaction Type column
levels(All_Pilot1_selected$Reactiontype)
All_Pilot1_selected$Reactiontype <- gsub(All_Pilot1_selected$Reactiontype, 
                                     pattern = " ", 
                                     replacement = "")
All_Pilot1_selected$Reactiontype <- as.factor(All_Pilot1_selected$Reactiontype)
All_Pilot1_selected$Reactiontype <- dplyr::recode(All_Pilot1_selected$Reactiontype, hit = 'Correct', incorrect = 'Incorrect', miss = 'Miss')

levels(All_Pilot2_selected$Reactiontype)
All_Pilot2_selected$Reactiontype <- gsub(All_Pilot2_selected$Reactiontype, 
                                         pattern = " ", 
                                         replacement = "")
All_Pilot2_selected$Reactiontype <- as.factor(All_Pilot2_selected$Reactiontype)
All_Pilot2_selected$Reactiontype <- dplyr::recode(All_Pilot2_selected$Reactiontype, hit = 'Correct', incorrect = 'Incorrect', miss = 'Miss')

# Save dataset for later use
write.table(All_Pilot1_selected, '~/Desktop/Bachelor/Daten/R_Frames/All_Pilot1.txt', row.names = F, sep = '\t')
write.table(All_Pilot2_selected, '~/Desktop/Bachelor/Daten/R_Frames/All_Pilot2.txt', row.names = F, sep = '\t')

