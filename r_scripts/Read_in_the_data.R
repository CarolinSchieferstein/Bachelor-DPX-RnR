##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 May 2018 
#                                     


# Load nescessary packages
require(plyr)
require(dplyr)

# ----- 1) Read in the data ---------------------

## Verhaltensdaten
path <- c("Desktop/Bachelor/Daten/Logs_Caro") # <- location of files

##########
## Baselinedaten
paths <- dir(path = path, full.names = T, pattern = "rawdata.txt$")
names(paths) <- basename(paths)

##########
## Blockdaten
paths2 <- dir(path = path, full.names = T, pattern= "block.txt$")
names(paths2) <- basename(paths2)

###########
## FBdaten
pathFB <- c("Desktop/Bachelor/Daten/rdata_dpx_rnr_2018-02-21_18-30.csv")

fb <- read.csv(pathFB,header=TRUE)

# ----- 2) Create data frame containing all files and observations

# **  RUN CODE to get dataframe with behavioral data form each individual
# **  The code cuts the dataframe down to a tidy version containing relevant variables 
# **  with the right attributues.

# Read in files Baseline
All_RT <- plyr::ldply(paths, read.table, sep =",", dec = ".", header=F)
rm(paths)

# Select columns with data, not names

All_RT_selected <- select(All_RT, .id, V2, V4, V6, V8,V10)

## default names substitue with right names through vector of same length
names(All_RT_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")

All_RT_selected$ID <- gsub(All_RT_selected$ID, pattern = "rawdata.txt", replacement = "")

########################
# Read in files Blocks
Block_RT <- plyr::ldply(paths2, read.table, sep=",", dec = ".", header=F)

# Select columns with data, not names

Block_RT_selected <- select(Block_RT, .id, V2, V4, V6, V8,V10)

## default names substitue with right names through vector of same length
names(Block_RT_selected) <- c( "ID", "Block", "Trialnr", "Trialtype", "RT", "Reactiontype")

Block_RT_selected$ID <- gsub(Block_RT_selected$ID, pattern = "block.txt", replacement = "")
