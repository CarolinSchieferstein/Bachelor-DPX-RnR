##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 August 2018 
#                           Read in Pilot Block data

# Load nescessary packages
require(plyr)
require(dplyr)

# ----- 1) Read in the data ---------------------

## Verhaltensdaten
path1b <- c("Desktop/Bachelor/Daten/Pilotierung Runde 1 u 2/Logs_Pilot1") # <- location of files Pilot1
path2b <- c("Desktop/Bachelor/Daten/Pilotierung Runde 1 u 2/Logs_Pilot2") # <- location of files Pilot2

## Blockdaten
paths1b <- dir(path = path1b, full.names = T, pattern = "block.txt$")
names(paths1b) <- basename(paths1b)
paths2b <- dir(path = path2b, full.names = T, pattern = "block.txt$")
names(paths2b) <- basename(paths2b)


# ----- 2) Create data frame containing all files and observations

# **  RUN CODE to get dataframe with behavioral data form each individual
# **  The code cuts the dataframe down to a tidy version containing relevant variables 
# **  with the right attributues.

# Read in files
All_Pilot1b <- plyr::ldply(paths1b, read.table, sep =",", dec = ".", header=F)
rm(paths1b)
All_Pilot2b <- plyr::ldply(paths2b, read.table, sep =",", dec = ".", header=F)
rm(paths2b)

# Select columns with data, not names

All_Pilot1b_selected <- select(All_Pilot1b, .id, V2, V4, V6, V8, V10)
All_Pilot2b_selected <- select(All_Pilot2b, .id, V2, V4, V6, V8, V10)

# Default names substitue with right names through vector of same length
names(All_Pilot1b_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")
names(All_Pilot2b_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")

# Get rid of suffix in ID column
All_Pilot1b_selected$ID <- gsub(All_Pilot1b_selected$ID, 
                            pattern = "block.txt", 
                            replacement = "")
All_Pilot2b_selected$ID <- gsub(All_Pilot2b_selected$ID, 
                                pattern = "block.txt", 
                                replacement = "")

# Get rid of blank space in Reaction Type column
levels(All_Pilot1b_selected$Reactiontype)
All_Pilot1b_selected$Reactiontype <- gsub(All_Pilot1b_selected$Reactiontype, 
                                      pattern = " ", 
                                      replacement = "")
All_Pilot1b_selected$Reactiontype <- as.factor(All_Pilot1b_selected$Reactiontype)
All_Pilot1b_selected$Reactiontype <- dplyr::recode(All_Pilot1b_selected$Reactiontype, hit = 'Correct', incorrect = 'Incorrect', miss = 'Miss')

levels(All_Pilot2b_selected$Reactiontype)
All_Pilot2b_selected$Reactiontype <- gsub(All_Pilot2b_selected$Reactiontype, 
                                          pattern = " ", 
                                          replacement = "")
All_Pilot2b_selected$Reactiontype <- as.factor(All_Pilot2b_selected$Reactiontype)
All_Pilot2b_selected$Reactiontype <- dplyr::recode(All_Pilot2b_selected$Reactiontype, hit = 'Correct', incorrect = 'Incorrect', miss = 'Miss')

# Save dataset for later use
write.table(All_Pilot1b_selected, '~/Desktop/Bachelor/Daten/R_Frames/All_Pilot1b.txt', row.names = F, sep = '\t')
write.table(All_Pilot2b_selected, '~/Desktop/Bachelor/Daten/R_Frames/All_Pilot2b.txt', row.names = F, sep = '\t')