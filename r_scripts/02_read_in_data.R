##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 July 2018 
#                          Read in Main Block data    

# Load nescessary packages
require(plyr)
require(dplyr)

# ----- 1) Read in the data ---------------------

## Verhaltensdaten
path <- c("Desktop/Bachelor/Daten/Logs_Caro") # <- location of files

## Blockdaten
paths <- dir(path = path, full.names = T, pattern = "block.txt$")
names(paths) <- basename(paths)

# ----- 2) Create data frame containing all files and observations

# **  RUN CODE to get dataframe with behavioral data form each individual
# **  The code cuts the dataframe down to a tidy version containing relevant variables 
# **  with the right attributues.

# Read in files
All_RTB <- plyr::ldply(paths, read.table, sep =",", dec = ".", header=F)
rm(paths)

# Select columns with data, not names

All_RTB_selected <- select(All_RTB, .id, V2, V4, V6, V8, V10)

# Default names substitue with right names through vector of same length
names(All_RTB_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")

# Get rid of suffix in ID column
All_RTB_selected$ID <- gsub(All_RTB_selected$ID, 
                           pattern = "block.txt", 
                           replacement = "")
# Get rid of blank space in Reaction Type column
levels(All_RTB_selected$Reactiontype)
All_RTB_selected$Reactiontype <- gsub(All_RTB_selected$Reactiontype, 
                                     pattern = " ", 
                                     replacement = "")
All_RTB_selected$Reactiontype <- as.factor(All_RTB_selected$Reactiontype)
All_RTB_selected$Reactiontype <- dplyr::recode(All_RTB_selected$Reactiontype, hit = 'Correct', incorrect = 'Incorrect', miss = 'Miss')

# Save dataset for later use
write.table(All_RTB_selected, '~/Desktop/Bachelor/Daten/R_Frames/All_RTB.txt', row.names = F, sep = '\t')

