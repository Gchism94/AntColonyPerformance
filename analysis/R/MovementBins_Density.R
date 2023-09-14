####################################################################################################################
## Author: GREG CHISM
## Date: JUNE 2022
## email: gchism@arizona.edu
## Project: Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication
## Title: Nest section bin functions, worker density in nest sections across nest shapes 
####################################################################################################################

#######DESCRIPTION OF THE SCRIPT##########
# This code does the following: 
# Binning each coordinate into eight even-area nest sections
# Data processing, analysis, and visualization for worker density in nest sections

##########################################################################################################
# INSTALL & LOAD REQUIRED PACKAGES
##########################################################################################################
# Download package with function to load multiple packaged at once
if (!require(pacman)) install.packages('pacman')

pacman::p_load(assertthat, # Loading required packages for code below. p_load() will download packages that aren't in system library
               data.table,
               ggpubr,
               here,
               lme4,
               lmerTest,
               MuMIn,
               tidyverse)

##########################################################################################################
# LOAD THE REQUIRED DATA SETS
##########################################################################################################

# DATASETS FROM ABCTracker_Movement.R
# Colony 5 Aggression
Colony5Aggn <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony5Aggn.csv"))

# Colony 5 Baseline
Colony5Pre <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony5Pre.csv"))

# Colony 6 Aggression
Colony6Aggn <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony6Aggn.csv"))

# Colony 6 Baseline
Colony6Pre <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony6Pre.csv"))

# Colony 7 Aggression
Colony7Aggn <- read_csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/tracking_standard/Colony7Aggn.csv")

# Colony 7 Baseline
Colony7Pre <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/tracking_standard/Colony7Pre.csv")

# Colony 8 Aggression
Colony8Aggn <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony8Aggn.csv"))

# Colony 8 Baseline
Colony8Pre <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony8Pre.csv"))

# Colony 9 Aggression
Colony9Aggn <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/tracking_standard/Colony9Aggn.csv")

# Colony 9 Baseline
Colony9Pre <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/tracking_standard/Colony9Pre.csv")

# Colony 11 Aggression
Colony11Aggn <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/tracking_standard/Colony11Aggn.csv")

# Colony 11 Baseline
Colony11Pre <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/tracking_standard/Colony11Pre.csv")

# Colony 13 Aggression
Colony13Aggn <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony13Aggn.csv"))

# Colony 13 Baseline
Colony13Pre <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony13Pre.csv"))

# Colony 17 Aggression
Colony17Aggn <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/tracking_standard/Colony17Aggn.csv")

# Colony 17 Baseline
Colony17Pre <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/tracking_standard/Colony17Pre.csv")

# Colony 18 Aggression
Colony18Aggn <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony18Aggn.csv"))

# Colony 18 Baseline
Colony18Pre <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony18Pre.csv"))

# Colony 20 Aggression
Colony20Aggn <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony20Aggn.csv"))

# Colony 20 Baseline
Colony20Pre <- read.csv(here("analysis", "data", "derived_data", "tracking_Standard", "Colony20Pre.csv"))

# REFERENCE DATA SETS
# BIN REFERENCE COORDINATES
BinCoordAssignMove <- read.csv(here("analysis", "data", "ref_data", "BinCoordAssignMove.csv"))

# BIN ASSIGNMENT FUNCTION
# The code below bins x and y coordinate colony data into eight even area nest sections
# To do this, a reference data set of bin coordinates is used and coordinates are run through a series of conditional statements
# Where each conditional statement checks whether the coordinate is in one of eight bins sequentially

# The code is set up such that you can run each colony sequentially, which was done to avoid errors in loops that can result in losing an entire set of data

# COLONY 5
# AGGRESSION ASSAY 
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony=="5" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "5" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony5BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "5" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony5BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "5" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
     dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony5AggnBinned <<- full_join(Colony5BinnedMoveTubeAggn, Colony5BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony5Aggn data set
CoordBinnedMoveAggn(Colony5Aggn)

# BASELINE ASSAY
CoordBinnedMovePre <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "5" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "5" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony5BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "5" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony5BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "5" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony5PreBinned <<- full_join(Colony5BinnedMoveTubePre, Colony5BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony5Pre data set
CoordBinnedMovePre(Colony5Pre)

# COLONY 6
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "6" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "6" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony6BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "6" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony6BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "6" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony6AggnBinned <<- full_join(Colony6BinnedMoveTubeAggn, Colony6BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony6Aggn data set
CoordBinnedMoveAggn(Colony6Aggn)


# BASELINE ASSAY
CoordBinnedMovePre <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "6" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "6" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony6BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "6" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony6BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "6" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony6PreBinned <<- full_join(Colony6BinnedMoveTubePre, Colony6BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony6Pre data set
CoordBinnedMovePre(Colony6Pre)

# COLONY 7
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "7" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "7" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony7BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "7" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony7BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "7" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony7AggnBinned <<- full_join(Colony7BinnedMoveTubeAggn, Colony7BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony7Aggn data set
CoordBinnedMoveAggn(Colony7Aggn)

# BASELINE ASSAY
CoordBinnedMovePre<-function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "7" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "7" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony7BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "7" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony7BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "7" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony7PreBinned <<- full_join(Colony7BinnedMoveTubePre, Colony7BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony7Pre data set
CoordBinnedMovePre(Colony7Pre)
  
# COLONY 8
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "8" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "8" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony8BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "8" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony8BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "8" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony8AggnBinned <<- full_join(Colony8BinnedMoveTubeAggn, Colony8BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony8Aggn data set
CoordBinnedMoveAggn(Colony8Aggn)

# BASELINE ASSAY
CoordBinnedMovePre<-function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "8" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "8" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony8BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "8" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony8BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "8" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony8PreBinned <<- full_join(Colony8BinnedMoveTubePre, Colony8BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony8Pre data set
CoordBinnedMovePre(Colony8Pre)

# COLONY 9
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "9" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "9" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony9BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "9" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony9BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "9" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony9AggnBinned <<- full_join(Colony9BinnedMoveTubeAggn, Colony9BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony9Aggn data set
CoordBinnedMoveAggn(Colony9Aggn)

# BASELINE ASSAY
CoordBinnedMovePre<-function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "9" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "9" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony9BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "9" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony9BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "9" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony9PreBinned <<- full_join(Colony9BinnedMoveTubePre, Colony9BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony9Pre data set
CoordBinnedMovePre(Colony9Pre)

# COLONY 11
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "11" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "11" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony11BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "11" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony11BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "11" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony11AggnBinned <<- full_join(Colony11BinnedMoveTubeAggn, Colony11BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony11Aggn data set
CoordBinnedMoveAggn(Colony11Aggn)

# BASELINE ASSAY
CoordBinnedMovePre<-function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "11" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "11" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony11BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "11" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony11BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "11" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony11PreBinned <<- full_join(Colony11BinnedMoveTubePre, Colony11BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony11Pre data set
CoordBinnedMovePre(Colony11Pre)

# COLONY 13
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "13" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "13" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony13BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "13" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony13BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "13" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony13AggnBinned <<- full_join(Colony13BinnedMoveTubeAggn, Colony13BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony13Aggn data set
CoordBinnedMoveAggn(Colony13Aggn)

# BASELINE ASSAY
CoordBinnedMovePre<-function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "13" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "13" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony13BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "13" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony13BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "13" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony13PreBinned <<- full_join(Colony13BinnedMoveTubePre, Colony13BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony13Pre data set
CoordBinnedMovePre(Colony13Pre)

# COLONY 17
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "17" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "17" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony17BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "17" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony17BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "17" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony17AggnBinned <<- full_join(Colony17BinnedMoveTubeAggn, Colony17BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony17Aggn data set
CoordBinnedMoveAggn(Colony17Aggn)

# BASELINE ASSAY
CoordBinnedMovePre<-function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "17" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "17" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony17BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "17" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony17BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "17" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony17PreBinned <<- full_join(Colony17BinnedMoveTubePre, Colony17BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony17Pre data set
CoordBinnedMovePre(Colony17Pre)

# COLONY 18
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "18" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "18" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony18BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "18" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony18BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "18" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony18AggnBinned <<- full_join(Colony18BinnedMoveTubeAggn, Colony18BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony18Aggn data set
CoordBinnedMoveAggn(Colony18Aggn)

# BASELINE ASSAY
CoordBinnedMovePre<-function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "18" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "18" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony18BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "18" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony18BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "18" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony18PreBinned <<- full_join(Colony18BinnedMoveTubePre, Colony18BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony18Pre data set
CoordBinnedMovePre(Colony18Pre)

# COLONY 20
# AGGRESSION
CoordBinnedMoveAggn <- function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "20" &  Nest == "Tube" & Trial == "Aggn") # Filter out tube nest aggression assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "20" & Nest == "Circle" & Trial == "Aggn") # Filter out circle nest aggression assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony20BinnedMoveTubeAggn <- data_table %>% # Data table to bin coordinates
    filter(Colony == "20" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony20BinnedMoveCircleAggn <- data_table %>%
    filter(Colony == "20" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony20AggnBinned <<- full_join(Colony20BinnedMoveTubeAggn, Colony20BinnedMoveCircleAggn)
}

# Run the CoordBinnedMoveAggn for the Colony20Aggn data set
CoordBinnedMoveAggn(Colony20Aggn)

# BASELINE ASSAY
CoordBinnedMovePre<-function(data_table){
  # REFERENCE COORDINATES
  # TUBE NEST
  BinCoordAssignMoveTube <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "20" &  Nest == "Tube" & Trial == "Pre") # Filter out tube nest baseline assay reference coordinates
  
  # CIRCLE NEST
  BinCoordAssignMoveCircle <- BinCoordAssignMove %>% # Reference data set for nest section bins
    filter(Colony == "20" & Nest == "Circle" & Trial == "Pre") # Filter out circle nest baseline assay reference coordinates
  
  # BINNING THE DATA
  # TUBE NEST
  Colony20BinnedMoveTubePre <- data_table %>% # Data table to bin coordinates
    filter(Colony == "20" & Nest == "Tube") %>% # Filter out the desired subset
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1, # Bin 1
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2, # Bin 2
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3, # Bin 3
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4, # Bin 4
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5, # Bin 5
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6, # Bin 6
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7, # Bin 7
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8, # Bin 7
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds", "AntLength.sec", "Bin")) # Select the desired columns
  
  # CIRCLE NEST
  Colony20BinnedMoveCirclePre <- data_table %>%
    filter(Colony == "20" & Nest == "Circle") %>%
    mutate(Bin = # Bin function, which qualifies whether each coordinate in within the reference coordinates for the bin
             if_else(Y <= BinCoordAssignMoveCircle$Y[1], 1, # Bin 1
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2], 2, # Bin 2
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3], 3, # Bin 3
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4], 4, # Bin 4
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5], 5, # Bin 5
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6], 6, # Bin 6
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7], 7, # Bin 7
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7], 8, # Bin 8
                                                                             0 # Else 0 (shouldn't happen)
                                                                     ))))))))) %>%
    dplyr::select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin")) # Select the desired columns
  
  # Join the two binned data sets
  Colony20PreBinned <<- full_join(Colony20BinnedMoveTubePre, Colony20BinnedMoveCirclePre)
}

# Run the CoordBinnedMovePre for the Colony20Pre data set
CoordBinnedMovePre(Colony20Pre)

# IF NEEDED: Remove unnecessary data sets to free up computer memory
rm("Colony5Aggn", "Colony5Pre", "Colony6Aggn", "Colony6Pre",
     "Colony7Aggn", "Colony7Pre", "Colony8Aggn", "Colony8Pre",
     "Colony9Aggn", "Colony9Pre", "Colony11Aggn", "Colony11Pre",
     "Colony13Aggn", "Colony13Pre", "Colony17Aggn", "Colony17Pre",
     "Colony18Aggn", "Colony18Pre", "Colony20Aggn", "Colony20Pre")

##########################################################################################################
# DATA PROCESSING, ANALYSES, VISUALIZATION: WORKER DENSITY IN NEST SECTIONS
# Producing the final data sets for each set of analyses and visualizations:
# Worker density in each nest section, for each assay and nest shape  
##########################################################################################################

# WORKER DENSITY
# AGGRESSION ASSAY
# Full join a subset of the aggression assay data sets (prevents too much computer memory use)

# AGGRESSION ASSAY 
# Subset 1
AggnAssayDensity <- Colony5AggnBinned %>%
    full_join(Colony6AggnBinned) %>%
    full_join(Colony7AggnBinned) %>%
    full_join(Colony8AggnBinned) %>%
    full_join(Colony9AggnBinned) %>%
    full_join(Colony13AggnBinned) %>% 
    drop_na() %>% # Drop NAs
    group_by(Colony, Nest, Seconds) %>% # Group by the Colony, Nest, and Seconds columns
    mutate(Count = n()) %>% # Count the number of rows in each group
    group_by(Colony, Nest, Seconds, Bin) %>% # New group: Colony, Nest, Seconds, Bins
    mutate(BinCount = n(), # Count the number of rows in each group
           PropWorkers = BinCount / Count, # Calculate the proportion of workers in each bin 
           AvgSpeed = mean(AntLength.sec)) %>% # Average speed in each bin
    ungroup() %>% # Ungroup the data
    dplyr::select(Colony, Nest, Trial, Seconds, Bin, PropWorkers, AvgSpeed) %>% # Select the desired columns
    distinct() # Remove duplicates

# Subset 2 
AggnAssayDensity1 <- Colony11AggnBinned %>%
  full_join(Colony17AggnBinned) %>%
  full_join(Colony18AggnBinned) %>%
  full_join(Colony20AggnBinned) %>%    
  drop_na() %>% # Drop NAs
  group_by(Colony, Nest, Seconds) %>% # Group by the Colony, Nest, and Seconds columns
  mutate(Count = n()) %>% # Count the number of rows in each group
  group_by(Colony, Nest, Seconds, Bin) %>% # New group: Colony, Nest, Seconds, Bins
  mutate(BinCount = n(), # Count the number of rows in each group
         PropWorkers = BinCount / Count, # Calculate the proportion of workers in each bin 
         AvgSpeed = mean(AntLength.sec)) %>% # Average speed in each bin
  ungroup() %>% # Ungroup the data
  dplyr::select(Colony, Nest, Trial, Seconds, Bin, PropWorkers, AvgSpeed) %>% # Select the desired columns
  distinct() # Remove duplicates

# BASELINE ASSAY 
# Subset 1
PreAssayDensity <- Colony5PreBinned %>%
  full_join(Colony6PreBinned) %>%
  full_join(Colony7PreBinned) %>%
  full_join(Colony8PreBinned) %>%
  full_join(Colony9PreBinned) %>%
  full_join(Colony13PreBinned) %>%  
  drop_na() %>% # Drop NAs
  group_by(Colony, Nest, Seconds) %>% # Group by the Colony, Nest, and Seconds columns
  mutate(Count = n()) %>% # Count the number of rows in each group
  group_by(Colony, Nest, Seconds, Bin) %>% # New group: Colony, Nest, Seconds, Bins
  mutate(BinCount = n(), # Count the number of rows in each group
         PropWorkers = BinCount / Count, # Calculate the proportion of workers in each bin 
         AvgSpeed = mean(AntLength.sec)) %>% # Average speed in each bin
  ungroup() %>% # Ungroup the data
  dplyr::select(Colony, Nest, Trial, Seconds, Bin, PropWorkers, AvgSpeed) %>% # Select the desired columns
  distinct() # Remove duplicates

# Subset 2 
PreAssayDensity1 <- Colony11PreBinned %>%
  full_join(Colony17PreBinned) %>%
  full_join(Colony18PreBinned) %>%
  full_join(Colony20PreBinned) %>%
  group_by(Colony,Nest,Seconds) %>% 
  drop_na() %>% # Drop NAs
  group_by(Colony, Nest, Seconds) %>% # Group by the Colony, Nest, and Seconds columns
  mutate(Count = n()) %>% # Count the number of rows in each group
  group_by(Colony, Nest, Seconds, Bin) %>% # New group: Colony, Nest, Seconds, Bins
  mutate(BinCount = n(), # Count the number of rows in each group
         PropWorkers = BinCount / Count, # Calculate the proportion of workers in each bin 
         AvgSpeed = mean(AntLength.sec)) %>% # Average speed in each bin
  ungroup() %>% # Ungroup the data
  dplyr::select(Colony, Nest, Trial, Seconds, Bin, PropWorkers, AvgSpeed) %>% # Select the desired columns
  distinct() # Remove duplicates

# Combine the worker density data sets
FullAssayDensity <- AggnAssayDensity %>%
  full_join(AggnAssayDensity1) %>%
  full_join(PreAssayDensity) %>%
  full_join(PreAssayDensity1)

# IF NEEDED: Remove unnecessary data sets to free up computer memory
rm("Colony5AggnBinned", "Colony5PreBinned", "Colony6AggnBinned", "Colony6PreBinned",
   "Colony7AggnBinned", "Colony7PreBinned", "Colony8AggnBinned", "Colony8PreBinned",
   "Colony9AggnBinned", "Colony9PreBinned", "Colony11AggnBinned", "Colony11PreBinned",
   "Colony13AggnBinned", "Colony13PreBinned", "Colony17AggnBinned", "Colony17PreBinned",
   "Colony18AggnBinned", "Colony18PreBinned", "Colony20AggnBinned", "Colony20PreBinned",
   "AggnAssayDensity", "AggnAssayDensity1", "PreAssayDensity", "PreAssayDensity1")

# Mean & standard deviation 
# Aggression assay
FullAssayDensity %>% 
  filter(Trial == "Aggn") %>%
  mutate(MeanProp = mean(PropWorkers), StdProp = sd(PropWorkers)) %>%
  ungroup() %>%
  dplyr::select(c(MeanProp, StdProp)) %>%
  distinct()

# Baseline assay
FullAssayDensity %>% 
  filter(Trial == "Pre") %>%
  mutate(MeanProp = mean(PropWorkers), StdProp = sd(PropWorkers)) %>%
  ungroup() %>%
  dplyr::select(c(MeanProp, StdProp)) %>%
  distinct()

# LINEAR MIXED EFFECTS MODEL
# RESPONSE VARIABLE 
# AvgSpeed - The average speed of workers in two-second bins over the 5-min video
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# Bin - Nest section, transformed to raw polynomial term because of a priori assumptions 
# RANDOM EFFECT
# (1|Colony) - Colony identification 
summary(lmer(AvgSpeed ~ PropWorkers * Nest * Trial + poly(Bin, degree = 2, raw = TRUE) + Seconds + (1|Colony), FullAssayDensity))

# Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(AvgSpeed ~ PropWorkers * Nest * Trial + poly(Bin, degree = 2, raw = TRUE) + Seconds + (1|Colony), FullAssayDensity))

# WORKER DENSITIES IN NEST SECTIONS

# Function to produce larger scatterplot legends
large_points <- function(data, params, size) {
  # Multiply by some number
  data$size <- data$size * 2
  draw_key_point(data = data, params = params, size = size)
}

# SCATTERPLOTS
# AGGRESSION ASSAY
# CIRCLE NEST
CircleAggn.Density <- ggplot(data = FullAssayDensity %>% filter(Trial == "Aggn" & Nest == "Circle") %>% drop_na(),
                           aes(x = PropWorkers, y = AvgSpeed)) +
  ggtitle("Circle Invader") +
  geom_point(key_glyph = large_points, 
             size = 2.5, 
             alpha = 0.15, 
             color = "blue", 
             shape = 16) +
  geom_smooth(method = 'lm', 
              se = FALSE, 
              linewidth = 1.5,
              color = "black") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"),
        legend.key = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(0, 1) +
  ylim(0, 0.1)

# TUBE NEST
TubeAggn.Density <- ggplot(data = FullAssayDensity %>% filter(Trial == "Aggn" & Nest == "Tube") %>% drop_na(),
                         aes(x = PropWorkers, y = AvgSpeed)) +
  ggtitle("Tube Invader") +
  geom_point(key_glyph = large_points, 
             size = 2.5, 
             alpha = 0.15, 
             color = "red", 
             shape = 17) +
  geom_smooth(method = 'lm', 
              se = FALSE, 
              linewidth = 1.5, 
              color = "black") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"),
        legend.key = element_blank()) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(0, 1) +
  ylim(0, 0.1) 

# BASELINE ASSAY
# CIRCLE NEST
CirclePre.Density <- ggplot(data = FullAssayDensity %>% filter(Trial == "Pre" & Nest == "Circle") %>% drop_na(), 
                            aes(x = PropWorkers, y = AvgSpeed)) +
  ggtitle("Circle Baseline") +
  geom_point(key_glyph = large_points, 
             size = 2.5, 
             alpha = 0.15, 
             color = "blue", 
             shape = 16) +
  geom_smooth(method = 'lm', 
              se = FALSE, 
              linewidth = 1.5, 
              color = "black") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size = 18, color = "black"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"),
        legend.key = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(0, 1) +
  ylim(0, 0.1) 

# TUBE NEST
TubePre.Density <- ggplot(data = FullAssayDensity %>% filter(Trial == "Pre" & Nest == "Tube") %>% drop_na(),
                        aes(x = PropWorkers, y = AvgSpeed)) +
  ggtitle("Tube Baseline") +
  geom_point(key_glyph = large_points, 
             size = 2.5, 
             alpha = 0.15, 
             color = "red", 
             shape = 17) +
  geom_smooth(method = 'lm', 
              se = FALSE, 
              linewidth = 1.5,
              color = "black") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"),
        legend.key = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  xlim(0, 1) +
  ylim(0, 0.1) 

# Compile the plots 
SpeedDensity <- ggarrange(CirclePre.Density, TubePre.Density, 
                        CircleAggn.Density, TubeAggn.Density,
                     labels = c("(a)", "(b)", "(c)", "(d)"),
                     font.label = list(size = 20, 
                                       face = "plain"),
                     ncol = 2, nrow = 2)

# Annotate the compiled plot and produce common x and y axes
SpeedDensityPlot <- annotate_figure(SpeedDensity,
                top = NULL,
                bottom = text_grob("Proportion of workers in nest sections", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Average ant-lengths / sec", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = "Fig8.pdf", plot = SpeedDensityPlot, width = 10.4, height = 10.4, units = "in")

