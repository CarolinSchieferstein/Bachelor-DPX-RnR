##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 May 2018 
#                           Read in Baseline data    

# Load nescessary packages
require(plyr)
require(dplyr)

# ----- 1) Read in the data ---------------------

## Verhaltensdaten
path <- c("Desktop/Bachelor/Daten/Logs_Caro") # <- location of files

## Baselinedaten
paths <- dir(path = path, full.names = T, pattern = "rawdata.txt$")
names(paths) <- basename(paths)

## Daten Fragebogen
pathFB <- c("Desktop/Bachelor/Daten/rdata_dpx_rnr_2018-02-21_18-30.csv")

fb <- read.csv(pathFB,
               header = TRUE)

# ----- 2) Create data frame containing all files and observations

# **  RUN CODE to get dataframe with behavioral data form each individual
# **  The code cuts the dataframe down to a tidy version containing relevant variables 
# **  with the right attributues.

# Read in files
All_RT <- plyr::ldply(paths, read.table, sep =",", dec = ".", header=F)
rm(paths)

# Select columns with data, not names

All_RT_selected <- select(All_RT, .id, V2, V4, V6, V8, V10)

# Default names substitue with right names through vector of same length
names(All_RT_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")

# Get rid of suffix in ID column
All_RT_selected$ID <- gsub(All_RT_selected$ID, 
                           pattern = "rawdata.txt", 
                           replacement = "")
# Get rid of blank space in Reaction Type column
levels(All_RT_selected$Reactiontype)
All_RT_selected$Reactiontype <- gsub(All_RT_selected$Reactiontype, 
                                     pattern = " ", 
                                     replacement = "")
All_RT_selected$Reactiontype <- as.factor(All_RT_selected$Reactiontype)
All_RT_selected$Reactiontype <- dplyr::recode(All_RT_selected$Reactiontype, hit = 'Correct', incorrect = 'Incorrect', miss = 'Miss')

# Save dataset for later use
write.table(All_RT_selected, '~/Desktop/Bachelor/Daten/R_Frames/All_RT.txt', row.names = F, sep = '\t')
