##########################################################################################################
## Author: GREG CHISM & ALANN RATHERY
## Date: JUNE 2022 
## email: gchism@arizona.edu
## Project: Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication
## Title: Worker interaction network matrix generation 
##########################################################################################################

#######DESCRIPTION OF THE SCRIPT##########
# This code takes raw ABCTracker output files and extracts adjacency matrix for direct and indirect worker interactions

# READ ME: This code has all of the matrix production code for every video in sequence, meaning if you run all of the code it will likely take several days to complete
# Instead, run one at a time, or only a small sequence
# NOTE: Colonies 11, 17, and 20 are especially large so the code will take a very long time for each of the associated videos

##########################################################################################################
# INSTALL & LOAD REQUIRED PACKAGES
##########################################################################################################
install.packages("pacman") # Download package with function to load multiple packaged at once


pacman::p_load(data.table, # Loading required packages for code below. p_load() will download packages that aren't in system library
               ggpubr,
               here,
               janitor,
               rbenchmark,
               Rcpp,
               tidyverse)

##########################################################################################################
# LOAD THE REQUIRED DATA SETS
##########################################################################################################

# ABCTRACKER RAW DATA FILES
# All raw ABCTracker outpus files needed for the code below 
# COLONY 5 
# INVADER ASSAY
# Import the data and change raw column headers to a more compatible format
Colony5CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony5_Circle_Aggn.csv"))

Colony5CircleAggnT <- setNames(Colony5CircleAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                     "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony5TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony5_Tube_Aggn.csv"))

Colony5TubeAggnT <- setNames(Colony5TubeAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                 "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))


# BASELINE ASSAY
Colony5CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony5_Circle_Pre.csv"))

Colony5CirclePreT <- setNames(Colony5CirclePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                                   "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony5TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony5_Tube_Pre.csv"))

Colony5TubePreT <- setNames(Colony5TubePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                               "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

# COLONY 6 
# INVADER ASSAY
Colony6CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony6_Circle_Aggn.csv"))

Colony6CircleAggnT <- setNames(Colony6CircleAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                     "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony6TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony6_Tube_Aggn.csv"))

Colony6TubeAggnT <- setNames(Colony6TubeAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                 "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

# BASELINE ASSAY
Colony6CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony6_Circle_Pre.csv"))

Colony6CirclePreT <- setNames(Colony6CirclePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                                   "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony6TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony6_Tube_Pre.csv"))

Colony6TubePreT <- setNames(Colony6TubePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                               "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

# COLONY 7 
# INVADER ASSAY
Colony7CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony7_Circle_Aggn.csv"))

Colony7CircleAggnT <- setNames(Colony7CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony7TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony7_Tube_Aggn.csv")) 

Colony7TubeAggnT <- setNames(Colony7TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# BASELINE ASSAY
Colony7CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony7_Circle_Pre.csv"))

Colony7CirclePreT <- setNames(Colony7CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony7TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony7_Tube_Pre.csv"))

Colony7TubePreT <- setNames(Colony7TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))


# COLONY 8
# INVADER ASSAY
Colony8CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony8_Circle_Aggn.csv")) 

Colony8CircleAggnT <- setNames(Colony8CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony8TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony8_Tube_Aggn.csv")) 

Colony8TubeAggnT <- setNames(Colony8TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# BASELINE ASSAY 
Colony8CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony8_Circle_Pre.csv")) 

Colony8CirclePreT <- setNames(Colony8CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony8TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony8_Tube_Pre.csv"))

Colony8TubePreT <- setNames(Colony8TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))


# COLONY 9
# INVADER ASSAY
Colony9CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony9_Circle_Aggn.csv"))

Colony9CircleAggnT <- setNames(Colony9CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony9TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony9_Tube_Aggn.csv"))

Colony9TubeAggnT <- setNames(Colony9TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))


# BASELINE ASSAY 
Colony9CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony9_Circle_Pre.csv"))

Colony9CirclePreT <- setNames(Colony9CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony9TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony9_Tube_Pre.csv"))

Colony9TubePreT <- setNames(Colony9TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))


# COLONY 11
# INVADER ASSAY
Colony11CircleAggnT  <- read_csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Raw_Data/tracking_raw/Colony11_Circle_Aggn.csv") %>%
  dplyr::select(-c("Tag", "Completed"))

Colony11CircleAggnT <- setNames(Colony11CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony11TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony11_Tube_Aggn.csv")) 

Colony11TubeAggnT <- setNames(Colony11TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))


# BASELINE ASSAY
Colony11CirclePreT <- read_csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Raw_Data/tracking_raw/Colony11_Circle_Pre.csv")

Colony11CirclePreT <- setNames(Colony11CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony11TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony11_Tube_Pre.csv"))

Colony11TubePreT <- setNames(Colony11TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))


# COLONY 13
# INVADER ASSAY
Colony13CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony13_Circle_Aggn.csv"))

Colony13CircleAggnT <- setNames(Colony13CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony13TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony13_Tube_Aggn.csv")) 

Colony13TubeAggnT <- setNames(Colony13TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# Pre assay
Colony13CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony13_Circle_Pre.csv"))

Colony13CirclePreT <- setNames(Colony13CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony13TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony13_Tube_Pre.csv"))

Colony13TubePreT <- setNames(Colony13TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))


# COLONY 17
# INVADER ASSAY
Colony17CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony17_Circle_Aggn.csv"))

Colony17CircleAggnT <- setNames(Colony17CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony17TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony17_Tube_Aggn.csv"))

Colony17TubeAggnT <- setNames(Colony17TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# BASELINE ASSAY
Colony17CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony17_Circle_Pre.csv"))

Colony17CirclePreT <- setNames(Colony17CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony17TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony17_Tube_Pre.csv"))

Colony17TubePreT <- setNames(Colony17TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# COLONY18
# INVADER ASSAY
Colony18CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony18_Circle_Aggn.csv")) 

Colony18CircleAggnT <- setNames(Colony18CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony18TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony18_Tube_Aggn.csv"))

Colony18TubeAggnT <- setNames(Colony18TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
# BASELINE ASSAY
Colony18CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony18_Circle_Pre.csv"))

Colony18CirclePreT <- setNames(Colony18CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony18TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony18_Tube_Pre.csv")) 

Colony18TubePreT <- setNames(Colony18TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# COLONY 20
# INVADER ASSAY
Colony20CircleAggnT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony20_Circle_Aggn.csv"))

Colony20CircleAggnT <- setNames(Colony20CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony20TubeAggnT<- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony20_Tube_Aggn.csv")) 

Colony20TubeAggnT <- setNames(Colony20TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# BASELINE ASSAY
Colony20CirclePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony20_Circle_Pre.csv")) 

Colony20CirclePreT <- setNames(Colony20CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony20TubePreT <- read_csv(here("analysis", "data", "raw_data", "tracker_data", "Colony20_Tube_Pre.csv"))

Colony20TubePreT <- setNames(Colony20TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

##########################################################################################################
# PROCESSING DATA, CONSTRUCTING, FILLING, AND EXPORTING MATRICES
# Processing ABCTracker raw data
# Constructing empty adjacency matrices
# Filling and exporting adjacency matrices
##########################################################################################################

# COLONY 5
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony5CirclePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony5CirclePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})


##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony5CirclePreR <- R
Colony5CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony5CirclePreR), Colony5CirclePreR), "Colony5CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony5CirclePreNR), Colony5CirclePreNR), "Colony5CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 5
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony5TubePreT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony5TubePreT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony5TubePreR <- R
Colony5TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony5TubePreR), Colony5TubePreR), "Colony5TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony5TubePreNR), Colony5TubePreNR), "Colony5TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 5 
# AGGRESSION ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony5CircleAggnT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony5CircleAggnT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)


##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony5CircleAggnR <- R
Colony5CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony5CircleAggnR), Colony5CircleAggnR), "Colony5CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony5CircleAggnNR), Colony5CircleAggnNR), "Colony5CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 5
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony5TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony5TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony5TubeAggnR <- R
Colony5TubeAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony5TubeAggnR), Colony5TubeAggnR), "Colony5TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony5TubeAggnNR), Colony5TubeAggnNR), "Colony5TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 6
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony6CirclePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony6CirclePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony6CirclePreR <- R
Colony6CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony6CirclePreR), Colony6CirclePreR), "Colony6CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony6CirclePreNR), Colony6CirclePreNR), "Colony6CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 6
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony6TubePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony6TubePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})


##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony6TubePreR <- R
Colony6TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony6TubePreR), Colony6TubePreR), "Colony6TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony6TubePreNR), Colony6TubePreNR), "Colony6TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 6
# AGGRESSION ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony6CircleAggnT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony6CircleAggnT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony6CircleAggnR <- R
Colony6CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony6CircleAggnR), Colony6CircleAggnR), "Colony6CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony6CircleAggnNR), Colony6CircleAggnNR), "Colony6CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 6
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony6TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony6TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony6TubeAggnR <- R
Colony6TubeAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony6TubeAggnR), Colony6TubeAggnR), "Colony6TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony6TubeAggnNR), Colony6TubeAggnNR), "Colony6TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 7
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony7CirclePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony7CirclePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony7CirclePreR <- R
Colony7CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony7CirclePreR), Colony7CirclePreR), "Colony7CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony7CirclePreNR), Colony7CirclePreNR), "Colony7CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 7
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony7TubePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony7TubePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony7TubePreR <- R
Colony7TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony7TubePreR), Colony7TubePreR), "Colony7TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony7TubePreNR), Colony7TubePreNR), "Colony7TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 7
# AGGRESSION ASSAY 
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony7CircleAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony7CircleAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony7CircleAggnR <- R
Colony7CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony7CircleAggnR), Colony7CircleAggnR), "Colony7CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony7CircleAggnNR), Colony7CircleAggnNR), "Colony7CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 7
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony7TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony7TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony7TubeAggnR <- R
Colony7TubeAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony7TubeAggnR), Colony7TubeAggnR), "Colony7TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony7TubeAggnNR), Colony7TubeAggnNR), "Colony7TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 8
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony8CirclePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony8CirclePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony8CirclePreR <- R
Colony8CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony8CirclePreR), Colony8CirclePreR), "Colony8CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony8CirclePreNR), Colony8CirclePreNR), "Colony8CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 8
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony8TubePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony8TubePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony8TubePreR <- R
Colony8TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony8TubePreR), Colony8TubePreR), "Colony8TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony8TubePreNR), Colony8TubePreNR), "Colony8TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 8
# AGGRESSION ASSAY 
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony8CircleAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony8CircleAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony8CircleAggnR <- R
Colony8CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony8CircleAggnR), Colony8CircleAggnR), "Colony8CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony8CircleAggnNR), Colony8CircleAggnNR), "Colony8CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 8 
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony8TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony8TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony8TubeAggnR <- R
Colony8TubeAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony8TubeAggnR), Colony8TubeAggnR), "Colony8TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony8TubeAggnNR), Colony8TubeAggnNR), "Colony8TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 9
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony9CirclePreT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony9CirclePreT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony9CirclePreR <- R
Colony9CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony9CirclePreR), Colony9CirclePreR), "Colony9CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony9CirclePreNR), Colony9CirclePreNR), "Colony9CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 9
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony9TubePreT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony9TubePreT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony9TubePreR <- R
Colony9TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony9TubePreR), Colony9TubePreR), "Colony9TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony9TubePreNR), Colony9TubePreNR), "Colony9TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 9
# AGGRESSION ASSAY 
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony9CircleAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony9CircleAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony9CircleAggnR <- R
Colony9CircleAggnNR <- NR

# Export the matrices
write.table(data.frame("ID" = rownames(Colony9CircleAggnR), Colony9CircleAggnR), "Colony9CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony9CircleAggnNR), Colony9CircleAggnNR), "Colony9CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 9
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony9TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony9TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony9TubeAggnR <- R
Colony9TubeAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony9TubeAggnR), Colony9TubeAggnR), "Colony9TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony9TubeAggnNR), Colony9TubeAggnNR), "Colony9TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 11
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony11CirclePreT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony11CirclePreT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony11CirclePreR <- R
Colony11CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony11CirclePreR), Colony11CirclePreR), "Colony11CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony11CirclePreNR), Colony11CirclePreNR), "Colony11CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 11
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony11TubePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony11TubePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony11TubePreR <- R
Colony11TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony11TubePreR), Colony11TubePreR), "Colony11TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony11TubePreNR), Colony11TubePreNR), "Colony11TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 11
# AGGRESSION ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony11CircleAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony11CircleAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony11CircleAggnR <- R
Colony11CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony11CircleAggnR), Colony11CircleAggnR), "Colony11CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony11CircleAggnNR), Colony11CircleAggnNR), "Colony11CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 11
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony11TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony11TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony11TubeAggnR <- R
Colony11TubeAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony11TubeAggnR), Colony11TubeAggnR), "Colony11TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony11TubeAggnNR), Colony11TubeAggnNR), "Colony11TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 13
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony13CirclePreT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony13CirclePreT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony13CirclePreR <- R
Colony13CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony13CirclePreR), Colony13CirclePreR), "Colony13CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony13CirclePreNR), Colony13CirclePreNR), "Colony13CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 13
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony13TubePreT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony13TubePreT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony13TubePreR <- R
Colony13TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony13TubePreR), Colony13TubePreR), "Colony13TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony13TubePreNR), Colony13TubePreNR), "Colony13TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 13
# AGGRESSION ASSAY 
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony13CircleAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony13CircleAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony13CircleAggnR <- R
Colony13CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony13CircleAggnR), Colony13CircleAggnR), "Colony13CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony13CircleAggnNR), Colony13CircleAggnNR), "Colony13CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 13
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

# NOTE: ID 1 in this data set does not produce a TRUE/FALSE for if(cond4 && cond5 | cond1 && cond5) and therefore causes an error
# This is because the individual doesn't interact with any individual and doesn't let the function work
# To counter this I manually added the column and row for this ID after it was completed
Colony13TubeAggnT <- Colony13TubeAggnT %>%
  filter(ID > 1)

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony13TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony13TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks

# RECIPROCAL MATRIX
Colony13TubeAggnR <- R

# Code to add a column and row for id_1, filled with 0s
# Matrix with 1 row and 61 columns, filled with 0s
Mat1 <- matrix(0, nrow = 61, ncol = 1)

# Change the column name to id_1
colnames(Mat1) = "id_1"

# Combine the two matrices by columns
R2 <- cbind(Mat1, Colony13TubeAggnR)

# Matrix with 62 rows and 1 column, filled with 0s
Mat2 <- matrix(0, nrow = 1, ncol = 62)

# Change the column names the same as matrix R2
colnames(Mat2) = colnames(R2)

# Change the row name to id_1
rownames(Mat2) = "id_1"

# Final reciprocal matrix
Colony13TubeAggnR <- rbind(Mat2, R2)

# NON-RECIPROCAL MATRIX
Colony13TubeAggnNR <- NR

# Code to add a column and row for id_1, filled with 0s
# Matrix with 1 row and 61 columns, filled with 0s
Mat1 <- matrix(0, nrow = 61, ncol = 1)

# Change the column name to id_1
colnames(Mat1) = "id_1"

# Combine the two matrices by columns
R2 <- cbind(Mat1, Colony13TubeAggnNR)

# Matrix with 62 rows and 1 column, filled with 0s
Mat2 <- matrix(0, nrow = 1, ncol = 62)

# Change the column names the same as matrix R2
colnames(Mat2) = colnames(R2)

# Change the row name to id_1
rownames(Mat2) = "id_1"

# Final non-reciprocal matrix
Colony13TubeAggnNR <- rbind(Mat2, R2)

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony13TubeAggnR), Colony13TubeAggnR), "Colony13TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony13TubeAggnNR), Colony13TubeAggnNR), "Colony13TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 17
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony17CirclePreT %>%
  filter(Frames < 7201) %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony17CirclePreT %>% # Original data
  filter(Frames < 7201) %>%
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony17CirclePreR <- R
Colony17CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony17CirclePreR), Colony17CirclePreR), "Colony17CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony17CirclePreNR), Colony17CirclePreNR), "Colony17CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# CIRCLE 17
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony17TubePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony17TubePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony17TubePreR <- R
Colony17TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony17TubePreR), Colony17TubePreR), "Colony17TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony17TubePreNR), Colony17TubePreNR), "Colony17TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 17
# AGGRESSION ASSAY 
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony17CircleAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony17CircleAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony17CircleAggnR <- R
Colony17CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony17CircleAggnR), Colony17CircleAggnR), "Colony17CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony17CircleAggnNR), Colony17CircleAggnNR), "Colony17CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 17
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony17TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony17TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony17TubeAggnR <- R
Colony17TubeAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony17TubeAggnR), Colony17TubeAggnR), "Colony17TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony17TubeAggnNR), Colony17TubeAggnNR), "Colony17TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 18 
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony18CirclePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony18CirclePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony18CirclePreR <- R
Colony18CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony18CirclePreR), Colony18CirclePreR), "Colony18CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony18CirclePreNR), Colony18CirclePreNR), "Colony18CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 18
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony18TubePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony18TubePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony18TubePreR <- R
Colony18TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony18TubePreR), Colony18TubePreR), "Colony18TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony18TubePreNR), Colony18TubePreNR), "Colony18TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 18
# AGGRESSION 
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony18CircleAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony18CircleAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony18CircleAggnR <- R
Colony18CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony18CircleAggnR), Colony18CircleAggnR), "Colony18CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony18CircleAggnNR), Colony18CircleAggnNR), "Colony18CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 18
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony18TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony18TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony18TubeAggnR <- R
Colony18TubeAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony18TubeAggnR), Colony18TubeAggnR), "Colony18TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony18TubeAggnNR), Colony18TubeAggnNR), "Colony18TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 20
# BASELINE ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony20CirclePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony20CirclePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony20CirclePreR <- R
Colony20CirclePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony20CirclePreR), Colony20CirclePreR), "Colony20CirclePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony20CirclePreNR), Colony20CirclePreNR), "Colony20CirclePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 20
# BASELINE ASSAY
# TUBE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony20TubePreT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony20TubePreT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony20TubePreR <- R
Colony20TubePreNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony20TubePreR), Colony20TubePreR), "Colony20TubePreRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony20TubePreNR), Colony20TubePreNR), "Colony20TubePreNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 20
# AGGRESSION ASSAY
# CIRCLE NEST
##########################################################################################################

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony20CircleAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony20CircleAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks
Colony20CircleAggnR <- R
Colony20CircleAggnNR <- NR

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony20CircleAggnR), Colony20CircleAggnR), "Colony20CircleAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony20CircleAggnNR), Colony20CircleAggnNR), "Colony20CircleAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")

# COLONY 20
# AGGRESSION ASSAY
# TUBE NEST
##########################################################################################################

# NOTE: ID 1 in this data set does not produce a TRUE/FALSE for if(cond4 && cond5 | cond1 && cond5) and therefore causes an error
# This is because the individual doesn't interact with any individual and doesn't let the function work
# To counter this I manually added the column and row for this ID after it was completed
Colony20TubeAggnT <- Colony20TubeAggnT %>%
  filter(ID > 1)

##########################################################################################################
# PROCESS IMPORTED DATA FOR THE MATRIX CODE
##########################################################################################################

# REFERENCES FOR LOOPS BELOW 
# Set references to be used for matrix column and row names 
DataReference <- Colony20TubeAggnT %>%
  # Arranging the data from smallest to largest values in the "ID" column
  arrange(ID) %>%
  # Only keep the ID column
  dplyr::select(ID) %>%
  # Remove duplicates
  distinct() %>%
  # Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var = 'obs')

# Joining the reference data set with the raw ABCTracker output
# This gives us the sequential numbers instead of the true ID values
# The matrix filling loops below use sequential ID numbers
data1 <- Colony20TubeAggnT %>% # Original data
  # Order the ID column smallest to largest
  arrange(ID) %>% 
  # Ungroup the data set
  ungroup() %>%
  # Left join the ID reference data set from above
  left_join(DataReference) %>%
  # Select the desired columns
  dplyr::select(-c(Interpolated, ID)) %>%
  # Rename the ID column as obs, this will allow us to change the matrix column and row names to the true ids
  rename(ID = obs)

##########################################################################################################
# FILL EACH ADJACENCY MATRIX WITH INTERACTIONS
# Here the nested for loops go through each ID and determine whether they interact with all other IDs
# This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
# This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
##########################################################################################################

# Data preparation and initial setup
unique_IDs <- unique(data1$ID)

# nbr_ID will be the number of individual IDs in a data set
nbr_ID <- length(unique_IDs)

# Create a vector that will contain all the value of box length to work on it
v <- tapply(data1$SizeLeng.px, data1$ID, function(x) x[1])

# Create a vector that will contain all the value of box width to work on it
g <- tapply(data1$SizeWidth.px, data1$ID, function(x) x[1])

# The distance reference for a proximity qualification in the matrix loop
# The distance is the average length of the box that represents each ant in the ABCTracker
distance <- 0.3 * mean(v)

# Create a vector that will contain all the values ant ids
A <- 1:nbr_ID

# Initialize empty matrices
NonR_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))
R_matrix <- matrix(0, nrow = nbr_ID, ncol = nbr_ID, dimnames = list(A))

# Loop through each unique ID
system.time({
  for (i in unique_IDs) {
    cat("starting ID ", i, " ")
    frames_id <- subset(data1, ID == i)
    
    for (f in frames_id$Frames) {
      frames_match <- subset(data1, Frames == f)
      
      headX_i <- frames_match$HeadX[frames_match$ID == i]
      headY_i <- frames_match$HeadY[frames_match$ID == i]
      middleX_i <- frames_match$X[frames_match$ID == i]
      middleY_i <- frames_match$Y[frames_match$ID == i]
      
      for (r in 1:nrow(frames_match)) {
        if (frames_match$ID[r] == i) next # Skip same ID
        
        headX_r <- frames_match$HeadX[r]
        headY_r <- frames_match$HeadY[r]
        middleX_r <- frames_match$X[r]
        middleY_r <- frames_match$Y[r]
        
        # Condition calculations
        dist_head <- sqrt((headX_r - headX_i)^2 + (headY_r - headY_i)^2)
        dist_middle_head <- sqrt((headX_r - middleX_i)^2 + (headY_r - middleY_i)^2)
        dist_middle <- sqrt((middleX_r - middleX_i)^2 + (middleY_r - middleY_i)^2)
        
        cond1 <- dist_head <= distance
        cond2 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] + 160 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] + 200
        cond3 <- frames_match$Orientation[r] >= frames_match$Orientation[frames_match$ID == i] - 200 && frames_match$Orientation[r] <= frames_match$Orientation[frames_match$ID == i] -160
        cond4 <- dist_middle_head <= 1.5 * mean(v)
        cond5 <- dist_middle > 0.5 * mean(g)
        
        # Update matrices based on conditions
        if ((cond4 && cond5) || (cond1 && cond5)) {
          if (cond1 && (cond2 || cond3)) {
            R_matrix [frames_match$ID[r], i] <- R_matrix [frames_match$ID[r], i] + 1
          } else {
            NonR_matrix [frames_match$ID[r], i] <- NonR_matrix [frames_match$ID[r], i] + 1
          }
        }
      }
    }
  }
})

##########################################################################################################
# MATRIX GENERATION TOWARDS STATIC NETWORKS 
# Create empty matrices to be filled interactions below
##########################################################################################################

# NON-RECIPROCAL MATRIX
# The matrix size is determined by the number of IDs
NonR_matrix <- matrix(data = NA, nrow = nbr_ID , ncol = nbr_ID , byrow = F, dimnames = list(A)) 

# Fill the matrix with zeros
NonR_matrix [is.na(NonR_matrix)] <- 0

# Row and column names are the sequential IDs
rownames(NonR_matrix) <- A
colnames(NonR_matrix) <- A

# RECIPROCAL MATRIX
# The matrix size is determined by the number of IDs
R_matrix <- matrix(data = NA, nrow = nbr_ID , ncol = nbr_ID , byrow = F, dimnames = list(A)) 

# Fill the matrix with zeros
R_matrix [is.na(R_matrix)] <- 0

# Row and column names are the sequential IDs
rownames(R_matrix) <- A
colnames(R_matrix) <- A

##########################################################################################################
# PROCESSING THE MATRICES & EXPORTING THEM 
# Process the matrices from the above code to add row and column names based on the true worker ID
# Export the matrices to be used for networks
##########################################################################################################

# Save the matrices to objects
R <- as.data.frame(R_matrix)
NR <- as.data.frame(NonR_matrix)

# Create a data set that is the true IDs of the ants
ColumnNamesMatrix <- DataReference$ID

# Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix

# Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("ID", colnames(R), sep = "_")

rownames(R) <- paste("ID", rownames(R), sep = "_")

# Make sure that all row and column names are the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)

# EXPORT FINAL MATRICES
# Export the matrices as .CSVs and add "ID" to the top left corner to name the rows, and for reference when constructing the networks

# RECIPROCAL MATRIX
Colony20TubeAggnR <- R

# Code to add a column and row for id_1, filled with 0s
# Matrix with 1 row and 61 columns, filled with 0s
Mat1 <- matrix(0, nrow = 61, ncol = 1)

# Change the column name to id_1
colnames(Mat1) = "id_1"

# Combine the two matrices by columns
R2 <- cbind(Mat1, Colony20TubeAggnR)

# Matrix with 62 rows and 1 column, filled with 0s
Mat2 <- matrix(0, nrow = 1, ncol = 62)

# Change the column names the same as matrix R2
colnames(Mat2) = colnames(R2)

# Change the row name to id_1
rownames(Mat2) = "id_1"

# Final reciprocal matrix
Colony20TubeAggnR <- rbind(Mat2, R2)

# NON-RECIPROCAL MATRIX
Colony20TubeAggnNR <- NR

# Code to add a column and row for id_1, filled with 0s
# Matrix with 1 row and 61 columns, filled with 0s
Mat1 <- matrix(0, nrow = 61, ncol = 1)

# Change the column name to id_1
colnames(Mat1) = "id_1"

# Combine the two matrices by columns
R2 <- cbind(Mat1, Colony20TubeAggnNR)

# Matrix with 62 rows and 1 column, filled with 0s
Mat2 <- matrix(0, nrow = 1, ncol = 62)

# Change the column names the same as matrix R2
colnames(Mat2) = colnames(R2)

# Change the row name to id_1
rownames(Mat2) = "id_1"

# Final non-reciprocal matrix
Colony20TubeAggnNR <- rbind(Mat2, R2)

# Export the matrices 
write.table(data.frame("ID" = rownames(Colony20TubeAggnR), Colony20TubeAggnR), "Colony20TubeAggnRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
write.table(data.frame("ID" = rownames(Colony20TubeAggnNR), Colony20TubeAggnNR), "Colony20TubeAggnNRMatrix.csv", row.names = FALSE, sep = ",", dec = ",")
